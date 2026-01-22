/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Highlighting
import Dress.Core
import Dress.Base64
import Dress.HtmlRender
import Architect

/-!
# LaTeX Generation

This module provides functions for generating .tex files from blueprint declarations.

## Output Format

Generates LaTeX compatible with leanblueprint's theorem environments:
- `\begin{theorem}...\end{theorem}`, `\begin{definition}...`, etc.
- `\lean{name}` for declaration name
- `\leanposition{file|line|col|endLine|endCol}` for source position
- `\leansignaturesourcehtml{base64}` for syntax-highlighted signature
- `\leanproofsourcehtml{base64}` for syntax-highlighted proof body
- `\leanhoverdata{base64}` for hover tooltip data
- `\leanok` if no sorry
-/

open Lean Elab Command
open SubVerso.Highlighting

namespace Dress.Generate

/-- Merge two hover JSON objects. Both are `{"id": "html", ...}` format. -/
def mergeHoverJson (a b : String) : String :=
  if a == "{}" || a.isEmpty then b
  else if b == "{}" || b.isEmpty then a
  else
    -- Strip outer braces, concatenate with comma, re-wrap
    let aInner := (a.drop 1).dropEnd 1
    let bInner := (b.drop 1).dropEnd 1
    s!"\{{aInner}, {bInner}}"

/-- Determine the appropriate LaTeX environment for a declaration.
    Returns "theorem" for theorem-like declarations, "definition" otherwise. -/
def getDefaultLatexEnv (name : Name) : CommandElabM String := do
  let env ← getEnv
  let some info := env.find? name | return "theorem"
  match info with
  | .thmInfo _ => return "theorem"
  | .defnInfo _ => return "definition"
  | .opaqueInfo _ => return "definition"
  | .inductInfo _ => return "definition"
  | .ctorInfo _ => return "definition"
  | .recInfo _ => return "definition"
  | _ => return "theorem"

/-- Generate .tex content for a single @[blueprint] declaration from an Architect.Node.
    This function reads from the LeanArchitect environment extension instead of re-parsing.
    Uses `Architect.Node.inferUses` to compute dependencies and leanok status.

    @param name The fully qualified declaration name
    @param node The Architect.Node from blueprintExt
    @param highlighting Optional SubVerso highlighted code
    @param htmlCode Optional pre-rendered HTML (unused, kept for API compatibility)
    @param file Optional source file path
    @param location Optional declaration position range -/
def generateDeclarationTexFromNode (name : Name) (node : Architect.Node)
    (highlighting : Option Highlighted) (_htmlCode : Option String)
    (file : Option System.FilePath) (location : Option DeclarationRange)
    : CommandElabM String := do
  let latexLabel := node.latexLabel
  let defaultEnv ← getDefaultLatexEnv name
  let latexEnv := if node.statement.latexEnv.isEmpty then defaultEnv else node.statement.latexEnv

  -- Infer uses and leanok status from Architect
  let (statementUses, proofUses) ← node.inferUses

  let mut out := ""

  -- Begin environment with statement
  out := out ++ s!"\\begin\{{latexEnv}}\n"

  -- Label and lean name
  out := out ++ s!"\\label\{{latexLabel}}\n"
  out := out ++ s!"\\lean\{{name}}\n"

  -- Embed syntax highlighting data (base64-encoded HTML) with hover info
  -- Note: \leanposition is only emitted when highlighting succeeds, because leanblueprint
  -- expects \leansignaturesourcehtml to be present when \leanposition is present
  if let some hl := highlighting then
    -- Position info (only emit with highlighting to satisfy leanblueprint constraint)
    if let (some f, some loc) := (file, location) then
      let posStr := s!"{f}|{loc.pos.line}|{loc.pos.column}|{loc.endPos.line}|{loc.endPos.column}"
      out := out ++ s!"\\leanposition\{{posStr}}\n"
    -- Split into signature and proof body
    let hasProof := node.proof.isSome
    let (sigHl, bodyHl) := splitAtDefinitionAssign hl (splitAtAssign := hasProof)

    -- Render signature to HTML with hovers and base64 encode
    -- Use stateful renderer so proof body IDs continue from signature IDs
    let (sigHtml, sigHoverJson, sigState) := HtmlRender.renderHighlightedWithState sigHl
    let sigBase64 := Base64.encodeString sigHtml
    out := out ++ s!"\\leansignaturesourcehtml\{{sigBase64}}\n"

    -- Render proof body if present - WITH HOVERS
    -- Continue hover ID numbering from signature to avoid collisions
    let mut proofHoverJson := "{}"
    if let some proofHl := bodyHl then
      let (proofHtml, proofHovers, _) := HtmlRender.renderHighlightedWithState proofHl sigState
      proofHoverJson := proofHovers
      let proofBase64 := Base64.encodeString proofHtml
      out := out ++ s!"\\leanproofsourcehtml\{{proofBase64}}\n"

    -- Merge signature and proof body hover data
    let mergedHoverJson := mergeHoverJson sigHoverJson proofHoverJson
    let hoverBase64 := Base64.encodeString mergedHoverJson
    out := out ++ s!"\\leanhoverdata\{{hoverBase64}}\n"

  -- Uses (from inferred statement uses)
  unless statementUses.uses.isEmpty do
    out := out ++ s!"\\uses\{{",".intercalate statementUses.uses.toList}}\n"

  -- leanok if statement has no sorry
  if statementUses.leanOk then
    out := out ++ "\\leanok\n"

  -- Statement text (from node.statement.text)
  if !node.statement.text.isEmpty then
    out := out ++ node.statement.text.trimAscii.toString ++ "\n"

  out := out ++ s!"\\end\{{latexEnv}}\n"

  -- Proof section if present
  if let some proofPart := node.proof then
    out := out ++ "\\begin{proof}\n"
    -- Uses (from inferred proof uses)
    unless proofUses.uses.isEmpty do
      out := out ++ s!"\\uses\{{",".intercalate proofUses.uses.toList}}\n"
    -- Proof is leanok if proof has no sorry
    if proofUses.leanOk then
      out := out ++ "\\leanok\n"
    if !proofPart.text.isEmpty then
      out := out ++ proofPart.text.trimAscii.toString ++ "\n"
    out := out ++ "\\end{proof}\n"

  return out

end Dress.Generate

-- Re-export at Dress namespace for convenience
namespace Dress

abbrev getDefaultLatexEnv := Generate.getDefaultLatexEnv
abbrev generateDeclarationTexFromNode := Generate.generateDeclarationTexFromNode

end Dress
