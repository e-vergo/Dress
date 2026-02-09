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
# Elaboration Rules for Blueprint Declarations

This module provides `elab_rules` that intercept declarations with `@[blueprint]`
attributes and capture highlighting after elaboration while info trees are still available.

## Architecture

We use `elab_rules` to intercept declarations that have `@[blueprint]`.
After standard elaboration completes (but while info trees still exist), we capture
the highlighting and store it in the environment extension.

The key insight is that `elab_rules` run before the standard elaborators, and we can
call the standard elaborator explicitly, then capture highlighting afterward.
-/

open Lean Elab Command Term Meta
open SubVerso.Highlighting

namespace Dress.Capture

-- Note: `register_option blueprint.dress`, the `#dress` command, and `isDressEnabled`
-- were removed as part of the always-active Dress integration (#247).
-- Artifact writing is now unconditional for @[blueprint] declarations.
-- The BLUEPRINT_DRESS env var, blueprint.dress option, and .dress marker file
-- are no longer checked.

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

/-- Elaborate a declaration command and capture highlighting for blueprint declarations.
    This helper is called by the elab_rules below.

    Automatically captures SubVerso highlighting and writes dressed artifacts
    (`.tex`, `.html`, `.json`) for every `@[blueprint]` declaration.

    @param stx The full declaration syntax
    @param declId The declaration identifier syntax
    @param _mods The declModifiers syntax (optional, kept for backward compatibility but no longer used;
                 we now read from LeanArchitect's blueprintExt instead of re-parsing) -/
def elabDeclAndCaptureHighlighting (stx : Syntax) (declId : Syntax) (_mods : Option Syntax := none)
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
        -- Time the captureHighlighting operation
        let captureStart ← IO.monoMsNow
        captureHighlighting resolvedName stx
        let captureEnd ← IO.monoMsNow
        let captureTime := captureEnd - captureStart

        -- Write per-declaration artifacts for every @[blueprint] declaration.
        -- (The dress-mode gate was removed in #247 -- artifact writing is now unconditional.)
        let env ← getEnv
        match Architect.blueprintExt.find? env resolvedName with
        | some node =>
          try
            trace[blueprint] "Node for {node.latexLabel}: label={node.latexLabel}, statement.text={node.statement.text.take 50}, proof={node.proof.isSome}, latexEnv={node.statement.latexEnv}"

            -- Get highlighting from extension (just captured above)
            let highlighting := dressedDeclExt.getState env |>.find? resolvedName

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

/-! ## Elaboration Rules

We use `elab_rules` to intercept declarations with @[blueprint] attribute.
These intercept declarations and capture highlighting after elaboration.

We use scoped rules so they only apply when Dress is imported.
The rules check the `inCaptureHook` flag to prevent infinite recursion.
-/

-- Theorem declarations with @[blueprint]
-- Use high priority to run before built-in elaborators
@[command_elab Lean.Parser.Command.declaration]
def elabBlueprintDeclaration : CommandElab := fun stx => do
  -- Only handle declarations
  unless stx.getKind == ``Lean.Parser.Command.declaration do
    throwUnsupportedSyntax
  let decl := stx[1]
  let declKind := decl.getKind
  -- Handle theorem/lemma (both use Command.theorem kind)
  unless declKind == ``Lean.Parser.Command.theorem do
    throwUnsupportedSyntax
  if (← inCaptureHookM) then
    throwUnsupportedSyntax
  let mods := stx[0]
  let hasBlueprint := hasBlueprintAttr mods
  trace[blueprint.debug] "elabBlueprintDeclaration: hasBlueprint={hasBlueprint}"
  if hasBlueprint then
    let declId := decl[1]
    elabDeclAndCaptureHighlighting stx declId (some mods)
  else
    throwUnsupportedSyntax

-- Definition declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers def $declId:declId $_sig:optDeclSig $_val:declVal) => do
    if (← inCaptureHookM) then throwUnsupportedSyntax
    let hasBlueprint := hasBlueprintAttr mods
    trace[blueprint.debug] "elab_rules def: hasBlueprint={hasBlueprint}"
    if hasBlueprint then
      elabDeclAndCaptureHighlighting (← getRef) declId (some mods)
    else
      throwUnsupportedSyntax

-- Abbreviation declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers abbrev $declId:declId $_sig:optDeclSig $_val:declVal) => do
    if (← inCaptureHookM) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId (some mods)
    else
      throwUnsupportedSyntax

-- Structure declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers structure $declId:declId $_*) => do
    if (← inCaptureHookM) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId (some mods)
    else
      throwUnsupportedSyntax

-- Class declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers class $declId:declId $_*) => do
    if (← inCaptureHookM) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId (some mods)
    else
      throwUnsupportedSyntax

-- Inductive declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers inductive $declId:declId $_*) => do
    if (← inCaptureHookM) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId (some mods)
    else
      throwUnsupportedSyntax

-- Instance declarations with @[blueprint]
elab_rules : command
  | `($mods:declModifiers instance $[$_prio:namedPrio]? $declId:declId $_sig:declSig $_val:declVal) => do
    if (← inCaptureHookM) then throwUnsupportedSyntax
    if hasBlueprintAttr mods then
      elabDeclAndCaptureHighlighting (← getRef) declId (some mods)
    else
      throwUnsupportedSyntax

end Dress.Capture

-- Re-export at Dress namespace for backward compatibility
namespace Dress

abbrev inCaptureHook := Capture.inCaptureHookM
/-- Run an action with the capture hook flag set.
    Re-export of `Capture.withCaptureHookFlag` for backward compatibility. -/
def withCaptureHookFlag (act : CommandElabM α) : CommandElabM α :=
  Capture.withCaptureHookFlag act
abbrev elabDeclAndCaptureHighlighting := Capture.elabDeclAndCaptureHighlighting

end Dress
