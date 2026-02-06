/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Batteries.Lean.NameMapAttribute
import SubVerso.Highlighting
import Dress.Highlighting
import Dress.Capture.State

/-!
# Info Tree Capture

This module provides functions for capturing SubVerso highlighting from
Lean's info trees during elaboration.

Info trees are ephemeral - they exist in `commandState.infoState.trees` during
elaboration but are discarded afterward. This module captures highlighting
while info trees are still available.
-/

open Lean Elab Command Term Meta
open SubVerso.Highlighting

namespace Dress.Capture

/-! ## Environment Extension for Captured Artifacts -/

/-- Environment extension storing captured highlighting for blueprint declarations.
    Keyed by declaration name, stores the Highlighted value captured during elaboration. -/
initialize dressedDeclExt : NameMapExtension Highlighted ←
  registerNameMapExtension Highlighted

/-- Get all captured highlighting for the current environment. -/
def getModuleHighlighting (env : Environment) : NameMap Highlighted :=
  dressedDeclExt.getState env

/-- Add captured highlighting for a declaration to the environment. -/
def addHighlighting (env : Environment) (declName : Name) (hl : Highlighted) : Environment :=
  dressedDeclExt.addEntry env (declName, hl)

/-! ## Module-Level Caches for Cross-Declaration Amortization -/

/-- Module-level suffix index. Built once per module on first `@[blueprint]` declaration,
    then reused for all subsequent declarations. Avoids the O(E) `env.constants.fold`
    per declaration where E can exceed 100K for mathlib-heavy projects. -/
initialize moduleSuffixIndexRef :
    IO.Ref (Option (SubVerso.Compat.HashMap String (Array Name))) ←
  IO.mkRef none

/-- Module-level expression/signature cache ref. Created on first use,
    passed to SubVerso's `highlightIncludingUnparsed` for cross-declaration sharing. -/
initialize moduleCacheRef : IO.Ref (Option (IO.Ref ModuleHighlightCache)) ←
  IO.mkRef none

/-- Get or build the module-level suffix index. Builds from `env.constants` on first call,
    returns cached index on subsequent calls. Avoids the O(E) fold per declaration. -/
def getOrBuildSuffixIndex (env : Environment) :
    IO (SubVerso.Compat.HashMap String (Array Name)) := do
  match ← moduleSuffixIndexRef.get with
  | some idx => return idx
  | none =>
    let idx := buildStandaloneSuffixIndex env
    moduleSuffixIndexRef.set (some idx)
    return idx

/-- Get or create the module-level highlight cache ref.
    Creates a fresh `IO.Ref ModuleHighlightCache` on first call. -/
def getOrCreateModuleCacheRef : IO (IO.Ref ModuleHighlightCache) := do
  match ← moduleCacheRef.get with
  | some ref => return ref
  | none =>
    let ref ← IO.mkRef (default : ModuleHighlightCache)
    moduleCacheRef.set (some ref)
    return ref

/-! ## Incremental Capture State -/

/-- Tracks statistics for the incremental highlighting pipeline within a module.
    Used for profiling and understanding cache effectiveness. -/
structure IncrementalCaptureState where
  /-- Total declarations processed in this module -/
  declsProcessed : Nat := 0
  /-- Declarations served from cache (type hash matched) -/
  declsFromCache : Nat := 0
  /-- Declarations that required fresh highlighting (cache miss or hash mismatch) -/
  declsReHighlighted : Nat := 0
deriving Inhabited, Repr

/-- Module-level incremental capture statistics. Reset at module boundaries. -/
initialize incrementalStateRef : IO.Ref IncrementalCaptureState ←
  IO.mkRef (default : IncrementalCaptureState)

/-- Get current incremental capture statistics. -/
def getIncrementalStats : IO IncrementalCaptureState :=
  incrementalStateRef.get

/-- Reset all module-level caches. Call at module boundaries to prevent stale
    data from one module leaking into the next.

    Clears:
    - `moduleSuffixIndexRef` (suffix index)
    - `moduleCacheRef` (highlight cache with type hashes)
    - `incrementalStateRef` (capture statistics) -/
def resetModuleCaches : IO Unit := do
  moduleSuffixIndexRef.set none
  moduleCacheRef.set none
  incrementalStateRef.set (default : IncrementalCaptureState)

/-! ## Core Capture Functions -/

/-- Capture SubVerso highlighting from info trees for a given syntax.

    This function:
    1. Takes the syntax, messages, and info trees from the current command state
    2. Calls SubVerso's `highlightIncludingUnparsed` function with module-level caches
    3. Returns the highlighted result or `none` on failure

    Must be called DURING elaboration while info trees are still available.

    Returns `none` if:
    - Info trees are empty
    - SubVerso highlighting fails
-/
def captureHighlightingFromInfoTrees
    (stx : Syntax)
    (messages : Array Message)
    (trees : PersistentArray InfoTree)
    (suppressedNamespaces : List Name := [])
    : TermElabM (Option Highlighted) := do
  if trees.isEmpty then
    return none
  try
    -- Get module-level caches for cross-declaration amortization
    let suffixIndex ← getOrBuildSuffixIndex (← getEnv)
    let cacheRef ← getOrCreateModuleCacheRef
    let hl ← highlightIncludingUnparsed stx messages trees suppressedNamespaces
      (prebuiltSuffixIndex? := some suffixIndex)
      (moduleCacheRef? := some cacheRef)
    return some hl
  catch _ =>
    -- SubVerso throws "not original, can't highlight: Lean.SourceInfo.synthetic" for some
    -- declarations (e.g., those with term-mode proofs or certain macro-generated syntax).
    -- Silently fail - highlighting is an optional enhancement.
    return none

/-- Lazy variant of `captureHighlightingFromInfoTrees` that checks the module-level
    cache before performing full highlighting.

    Uses `lazyHighlightIncludingUnparsed` to:
    - Return cached highlighting if the declaration's type hash is unchanged
    - Perform fresh highlighting on cache miss or type hash mismatch
    - Track incremental statistics for profiling

    Returns the `HighlightResult` indicating whether the result was cached, fresh, or skipped.
-/
def lazyCaptureHighlightingFromInfoTrees
    (stx : Syntax)
    (messages : Array Message)
    (trees : PersistentArray InfoTree)
    (declName : Name)
    (suppressedNamespaces : List Name := [])
    : TermElabM HighlightResult := do
  if trees.isEmpty then
    return HighlightResult.skipped
  try
    -- Get module-level caches for cross-declaration amortization
    let suffixIndex ← getOrBuildSuffixIndex (← getEnv)
    let cacheRef ← getOrCreateModuleCacheRef
    let result ← lazyHighlightIncludingUnparsed stx messages trees declName
      suppressedNamespaces
      (prebuiltSuffixIndex? := some suffixIndex)
      (moduleCacheRef? := some cacheRef)
    -- Update incremental statistics
    let stats ← incrementalStateRef.get
    let stats := { stats with declsProcessed := stats.declsProcessed + 1 }
    let stats := match result with
      | HighlightResult.cached _ => { stats with declsFromCache := stats.declsFromCache + 1 }
      | HighlightResult.fresh _ => { stats with declsReHighlighted := stats.declsReHighlighted + 1 }
      | HighlightResult.skipped => stats
    incrementalStateRef.set stats
    return result
  catch _ =>
    return HighlightResult.skipped

/-- Capture highlighting for a declaration using current command state.

    This is the main entry point for the `@[blueprint]` attribute handler.
    It extracts info trees from the current command state and captures highlighting.

    Must be called DURING elaboration while info trees are still available.
    Silently does nothing if:
    - Called outside elaboration context
    - Info trees are not available
    - SubVerso highlighting fails
-/
def captureHighlighting (declName : Name) (stx : Syntax) : CommandElabM Unit := do
  -- Check if highlighting is enabled
  unless blueprint.highlighting.get (← getOptions) do return

  try
    -- Get current info trees from command state
    let trees := (← get).infoState.trees
    if trees.isEmpty then
      trace[blueprint.debug] "No info trees available for {declName}"
      return

    -- Time: Message filtering
    let msgFilterStart ← IO.monoMsNow
    let allMessages := (← get).messages.toArray
    let fileMap := (← read).fileMap
    let messages := allMessages.filter fun msg =>
      !msg.isSilent &&
      stx.getRange?.any fun _ =>
        let msgStartPos := msg.pos
        -- Check if message position falls within syntax range
        match stx.getPos?, stx.getTailPos? with
        | some startPos, some endPos =>
          let stxStartLine := fileMap.toPosition startPos |>.line
          let stxEndLine := fileMap.toPosition endPos |>.line
          msgStartPos.line >= stxStartLine && msgStartPos.line <= stxEndLine
        | _, _ => false
    let msgFilterEnd ← IO.monoMsNow

    -- Time: SubVerso highlighting extraction (lazy/incremental path)
    let subversoStart ← IO.monoMsNow
    let result ← liftTermElabM do
      lazyCaptureHighlightingFromInfoTrees stx messages trees declName []
    let subversoEnd ← IO.monoMsNow

    -- Print timing breakdown for captureHighlighting internals (only when trace option enabled)
    trace[blueprint.timing] "  msgFilter: {msgFilterEnd - msgFilterStart}ms for {declName}"
    trace[blueprint.timing] "  subversoHighlight: {subversoEnd - subversoStart}ms for {declName}"
    if result.isCached then
      trace[blueprint.timing] "  (served from cache) for {declName}"

    match result.toHighlighted? with
    | some hl =>
      -- Store in environment extension
      modifyEnv fun env => addHighlighting env declName hl
      trace[blueprint] "Captured highlighting for {declName}"
    | none =>
      trace[blueprint.debug] "Highlighting capture returned none for {declName}"
  catch _ =>
    -- Silently fail - highlighting is optional enhancement
    trace[blueprint.debug] "Failed to capture highlighting for {declName}"

end Dress.Capture

-- Re-export at Dress namespace for backward compatibility
namespace Dress

abbrev dressedDeclExt := Capture.dressedDeclExt
abbrev getModuleHighlighting := Capture.getModuleHighlighting
abbrev addHighlighting := Capture.addHighlighting
abbrev captureHighlighting := Capture.captureHighlighting
abbrev resetModuleCaches := Capture.resetModuleCaches
abbrev getIncrementalStats := Capture.getIncrementalStats

end Dress
