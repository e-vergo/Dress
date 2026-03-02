/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Highlighting
import Dress.Highlighting
import Dress.HtmlRender
import Dress.Capture.State
import Dress.Capture.InfoTree
import Dress.Capture.Config
import Dress.Generate.Declaration
import Architect.Basic

/-!
# Elaboration Rules for All Declarations

This module provides `elab_rules` that intercept ALL declarations (theorem, def, lemma,
structure, class, instance, axiom, inductive, abbrev) and capture SubVerso highlighting
after elaboration while info trees are still available.

## Architecture

Every declaration is captured by default. The `@[blueprint]` attribute is now optional
metadata for titles, statements, manual status, etc. Declarations can opt out via
`@[noblueprint]`.

The `BLUEPRINT_CAPTURE` environment variable provides a global kill switch: set it to
`"false"` to disable all capture (for non-blueprint builds using the same toolchain).

## Capture flow

1. elab_rules intercept the declaration
2. Standard elaboration runs (via `elabCommandTopLevel`)
3. If the declaration has `@[blueprint]`, the Architect attribute handler already created
   a Node in `blueprintExt` -- we use that.
4. If the declaration has no `@[blueprint]`, we auto-create a minimal Node using the
   fully qualified name as the label and register it in `blueprintExt`.
5. SubVerso highlighting is captured and artifacts are written.
-/

open Lean Elab Command Term Meta
open SubVerso.Highlighting

namespace Dress.Capture

/-! ## Global Capture Control -/

/-- Check if blueprint capture is globally enabled.
    When `BLUEPRINT_CAPTURE` is set to `"false"` (case-insensitive), all capture is disabled.
    This allows the same toolchain to be used for non-blueprint builds. -/
def isCaptureEnabled : IO Bool := do
  let val ← IO.getEnv "BLUEPRINT_CAPTURE"
  match val with
  | some v => return v.toLower != "false"
  | none => return true

/-! ## Declaration Interception Helpers -/

/-- Extract declaration name from a declId syntax node. -/
def getDeclNameFromDeclId (declId : Syntax) : Option Name :=
  if declId.getKind == ``Lean.Parser.Command.declId then
    declId[0]?.map (·.getId)
  else if declId.isIdent then
    some declId.getId
  else
    none

/-- Check if we're currently inside the capture hook. -/
def inCaptureHookM : CommandElabM Bool := do
  blueprintCaptureHookRef.get

/-- Run an action with the capture hook flag set. -/
def withCaptureHookFlag (act : CommandElabM α) : CommandElabM α := do
  blueprintCaptureHookRef.set true
  try
    act
  finally
    blueprintCaptureHookRef.set false

/-- Determine the LaTeX environment for a declaration based on its kind in the environment. -/
private def inferLatexEnv (env : Environment) (name : Name) (hasProofPart : Bool) : String :=
  if isClass env name then "class"
  else if isStructure env name then "structure"
  else if Meta.isInstanceCore env name then "instance"
  else match env.find? name with
    | some (.defnInfo dv) => if dv.hints.isAbbrev then "abbrev" else "definition"
    | some (.inductInfo _) => "inductive"
    | some (.axiomInfo _) => "axiom"
    | _ => if hasProofPart then "theorem" else "definition"

/-- Check whether a resolved declaration name corresponds to a theorem
    (i.e., originally declared with `theorem` or `lemma`). -/
private def isTheoremDecl (env : Environment) (name : Name) : Bool :=
  match env.find? name with
  | some (.thmInfo _) => true
  | _ => false

/-- Create a minimal Architect.Node for an auto-captured declaration (no `@[blueprint]` tag).
    Uses the fully qualified name as the label. -/
private def mkAutoNode (env : Environment) (name : Name) : Architect.Node :=
  let hasProofPart := isTheoremDecl env name
  let latexEnv := inferLatexEnv env name hasProofPart
  let statement : Architect.NodePart := { text := "", latexEnv := latexEnv }
  let proof : Option Architect.NodePart :=
    if hasProofPart then some { text := "", latexEnv := "proof" } else none
  { name := name
    latexLabel := name.toString
    statement := statement
    proof := proof
    status := .notReady
    statusExplicit := false
    discussion := none
    title := none }

/-- Elaborate a declaration command and capture highlighting.
    This helper is called by the elab_rules below.

    For `@[blueprint]`-tagged declarations, the Node was already created by the Architect
    attribute handler. For untagged declarations, we auto-create a minimal Node and
    register it in `blueprintExt`.

    @param stx The full declaration syntax
    @param declId The declaration identifier syntax
    @param mods The declModifiers syntax (used to check for @[blueprint]/@[noblueprint]) -/
def elabDeclAndCaptureHighlighting (stx : Syntax) (declId : Syntax) (mods : Option Syntax := none)
    : CommandElabM Unit := do
  -- Run standard command elaboration with the flag set to prevent recursion
  withCaptureHookFlag do
    elabCommandTopLevel stx
    -- Capture highlighting immediately after elaboration, while info trees are still available
    if let some name := getDeclNameFromDeclId declId then
      -- Resolve the name with current namespace
      let ns ← getCurrNamespace
      let fullName := if ns.isAnonymous then name else ns ++ name
      let env ← getEnv
      let resolvedName := if env.contains fullName then fullName else name
      if env.contains resolvedName then
        -- Determine if @[blueprint] was present (Node already in blueprintExt)
        let hasBlueprint := mods.map hasBlueprintAttr |>.getD false

        -- If no @[blueprint], auto-create a Node and register it
        if !hasBlueprint then
          let node := mkAutoNode env resolvedName
          let env' := Architect.blueprintExt.addEntry env (resolvedName, node)
          let env'' := Architect.addLeanNameOfLatexLabel env' node.latexLabel resolvedName
          modifyEnv fun _ => env''
          trace[blueprint] "Auto-captured {resolvedName} (label={node.latexLabel})"

        -- Time the captureHighlighting operation
        let captureStart ← IO.monoMsNow
        captureHighlighting resolvedName stx
        let captureEnd ← IO.monoMsNow
        let captureTime := captureEnd - captureStart

        -- Write per-declaration artifacts
        let env ← getEnv
        match Architect.blueprintExt.find? env resolvedName with
        | some node =>
          try
            trace[blueprint] "Node for {node.latexLabel}: label={node.latexLabel}, statement.text={node.statement.text.take 50}, proof={node.proof.isSome}, latexEnv={node.statement.latexEnv}"

            -- Get highlighting from extension (just captured above)
            let highlighting := dressedDeclExt.find? env resolvedName

            -- Get source file path
            let file := (← read).fileName

            -- Get declaration location
            let location ← liftTermElabM do
              Lean.findDeclarationRanges? resolvedName

            -- Time the writeDeclarationArtifactsFromNode operation
            let writeStart ← IO.monoMsNow
            -- Write all artifacts (.tex, .html, .json) using the Node-based function
            Generate.writeDeclarationArtifactsFromNode resolvedName node highlighting (some file) (location.map (·.range))
            let writeEnd ← IO.monoMsNow
            let writeTime := writeEnd - writeStart

            -- Calculate and print total timing (only when trace option enabled)
            let totalTime := captureTime + writeTime
            trace[blueprint.timing] "captureHighlighting: {captureTime}ms for {resolvedName}"
            trace[blueprint.timing] "writeArtifacts: {writeTime}ms for {resolvedName}"
            trace[blueprint.timing] "TOTAL: {totalTime}ms for {resolvedName}"

            trace[blueprint] "Wrote artifacts for {resolvedName} with label {node.latexLabel}"
          catch e =>
            trace[blueprint.debug] "Failed to write declaration artifacts: {e.toMessageData}"
        | none =>
          trace[blueprint] "blueprintExt not populated for {resolvedName}"

/-- Common guard logic for all elab_rules.
    Returns `true` if capture should proceed, `false` if we should skip (throwUnsupportedSyntax). -/
private def shouldCapture (mods : Syntax) : CommandElabM Bool := do
  -- Check recursion guard
  if (← inCaptureHookM) then return false
  -- Check global capture enable
  unless (← isCaptureEnabled) do return false
  -- Check @[noblueprint] opt-out
  if hasNoblueprintAttr mods then return false
  return true

/-! ## Elaboration Rules

We intercept ALL declarations to capture highlighting and generate artifacts.
`@[blueprint]` is no longer required -- it provides optional metadata.
`@[noblueprint]` opts a declaration out of capture.
`BLUEPRINT_CAPTURE=false` disables all capture globally.

We use the `inCaptureHook` flag to prevent infinite recursion.
-/

-- Theorem/lemma and axiom declarations
-- Use high priority to run before built-in elaborators
@[command_elab Lean.Parser.Command.declaration]
def elabBlueprintDeclaration : CommandElab := fun stx => do
  -- Only handle declarations
  unless stx.getKind == ``Lean.Parser.Command.declaration do
    throwUnsupportedSyntax
  let decl := stx[1]
  let declKind := decl.getKind
  -- Handle theorem/lemma (both use Command.theorem kind) and axiom declarations
  unless declKind == ``Lean.Parser.Command.theorem ||
         declKind == ``Lean.Parser.Command.axiom do
    throwUnsupportedSyntax
  let mods := stx[0]
  unless (← shouldCapture mods) do throwUnsupportedSyntax
  let declId := decl[1]
  elabDeclAndCaptureHighlighting stx declId (some mods)

-- Definition declarations
elab_rules : command
  | `($mods:declModifiers def $declId:declId $_sig:optDeclSig $_val:declVal) => do
    unless (← shouldCapture mods) do throwUnsupportedSyntax
    elabDeclAndCaptureHighlighting (← getRef) declId (some mods)

-- Abbreviation declarations
elab_rules : command
  | `($mods:declModifiers abbrev $declId:declId $_sig:optDeclSig $_val:declVal) => do
    unless (← shouldCapture mods) do throwUnsupportedSyntax
    elabDeclAndCaptureHighlighting (← getRef) declId (some mods)

-- Structure declarations
elab_rules : command
  | `($mods:declModifiers structure $declId:declId $_*) => do
    unless (← shouldCapture mods) do throwUnsupportedSyntax
    elabDeclAndCaptureHighlighting (← getRef) declId (some mods)

-- Class declarations
elab_rules : command
  | `($mods:declModifiers class $declId:declId $_*) => do
    unless (← shouldCapture mods) do throwUnsupportedSyntax
    elabDeclAndCaptureHighlighting (← getRef) declId (some mods)

-- Inductive declarations
elab_rules : command
  | `($mods:declModifiers inductive $declId:declId $_*) => do
    unless (← shouldCapture mods) do throwUnsupportedSyntax
    elabDeclAndCaptureHighlighting (← getRef) declId (some mods)

-- Instance declarations
elab_rules : command
  | `($mods:declModifiers instance $[$_prio:namedPrio]? $declId:declId $_sig:declSig $_val:declVal) => do
    unless (← shouldCapture mods) do throwUnsupportedSyntax
    elabDeclAndCaptureHighlighting (← getRef) declId (some mods)

/-! ## Retroactive Annotation Support

The `#dressNodes` command writes dressed artifacts for all blueprint nodes in the
current environment that don't yet have per-declaration artifacts. This supports
retroactive `attribute [blueprint ...] Name` annotations, where the declaration
source code is already compiled and SubVerso highlighting is not available.

Usage: Place `#dressNodes` at the end of files containing retroactive blueprint
annotations. -/

/-- Scans the current environment for all blueprint nodes and writes dressed
    artifacts for any that don't already have per-declaration artifact directories.
    This is the primary mechanism for generating dressed artifacts from retroactive
    `attribute [blueprint ...]` annotations.

    Since the declaration source is already compiled, artifacts are written with
    `highlighting := none`, producing valid LaTeX and JSON without syntax-highlighted
    code display. -/
elab "#dressNodes" : command => do
  let env ← getEnv
  let entries := Architect.blueprintExt.getEntries env
  let buildDir : System.FilePath := ".lake" / "build"
  let rawModule := env.header.mainModule
  -- For standalone files, derive module name from filename (same logic as Declaration.lean)
  let moduleName ←
    if rawModule == `_stdin then do
      let fp : System.FilePath := (← read).fileName
      pure (Name.mkSimple (fp.fileStem.getD "standalone"))
    else pure rawModule
  let mut count : Nat := 0
  for (name, node) in entries do
    -- Skip imported declarations (only process nodes from the current module's env)
    let label := node.latexLabel
    let declDir := Dress.Paths.getDeclarationDir buildDir moduleName label
    -- Skip if artifacts already exist
    if ← declDir.pathExists then continue
    try
      let highlighting : Option Highlighted := none
      let file := (← read).fileName
      let location ← liftTermElabM do
        Lean.findDeclarationRanges? name
      Generate.writeDeclarationArtifactsFromNode name node highlighting (some file) (location.map (·.range))
      count := count + 1
    catch _ => pure ()
  if count > 0 then
    logInfo m!"Dressed {count} retroactive blueprint nodes"

end Dress.Capture

-- Re-export at Dress namespace for backward compatibility
namespace Dress

abbrev inCaptureHook := Capture.inCaptureHookM
/-- Run an action with the capture hook flag set.
    Re-export of `Capture.withCaptureHookFlag` for backward compatibility. -/
def withCaptureHookFlag (act : CommandElabM α) : CommandElabM α :=
  Capture.withCaptureHookFlag act
abbrev elabDeclAndCaptureHighlighting := Capture.elabDeclAndCaptureHighlighting
abbrev isCaptureEnabled := Capture.isCaptureEnabled

end Dress
