/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Batteries.Lean.NameMapAttribute
import SubVerso.Highlighting
import Dress.Highlighting

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

/-! ## Core Capture Functions -/

/-- Capture SubVerso highlighting from info trees for a given syntax.

    This function:
    1. Takes the syntax, messages, and info trees from the current command state
    2. Calls SubVerso's `highlightIncludingUnparsed` function
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
    let hl ← highlightIncludingUnparsed stx messages trees suppressedNamespaces
    return some hl
  catch _ =>
    -- SubVerso throws "not original, can't highlight: Lean.SourceInfo.synthetic" for some
    -- declarations (e.g., those with term-mode proofs or certain macro-generated syntax).
    -- Silently fail - highlighting is an optional enhancement.
    return none

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

    -- Get messages for this syntax range
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

    -- Run SubVerso highlighting in TermElabM
    let hl? ← liftTermElabM do
      captureHighlightingFromInfoTrees stx messages trees []

    match hl? with
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

end Dress
