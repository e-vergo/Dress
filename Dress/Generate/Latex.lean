/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Highlighting
import Dress.Core
import Dress.Base64
import Dress.HtmlRender
import Dress.Capture.Config

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

/-- Check if a declaration contains sorry (uses `sorryAx`).
    This is a simple check that examines the declaration's value expression. -/
def declarationHasSorry (name : Name) : CommandElabM Bool := do
  let env ← getEnv
  let some info := env.find? name | return true  -- If not found, assume sorry
  -- Check if the value expression contains sorryAx
  let checkExpr (e : Expr) : Bool := e.getUsedConstants.contains ``sorryAx
  match info with
  | .defnInfo v => return checkExpr v.value
  | .thmInfo v => return checkExpr v.value
  | .opaqueInfo v => return checkExpr v.value
  | _ => return false

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

/-- Generate .tex content for a single @[blueprint] declaration during dressing.
    Uses already-captured highlighting and parsed config.

    @param name The fully qualified declaration name
    @param config The parsed blueprint configuration
    @param highlighting Optional SubVerso highlighted code
    @param htmlCode Optional pre-rendered HTML (unused, kept for API compatibility)
    @param file Optional source file path
    @param location Optional declaration position range -/
def generateDeclarationTex (name : Name) (config : Capture.BlueprintConfig)
    (highlighting : Option Highlighted) (_htmlCode : Option String)
    (file : Option System.FilePath) (location : Option DeclarationRange)
    : CommandElabM String := do
  let latexLabel := config.latexLabel.getD name.toString
  let defaultEnv ← getDefaultLatexEnv name
  let latexEnv := config.latexEnv.getD defaultEnv

  let mut out := ""

  -- Begin environment with statement
  out := out ++ s!"\\begin\{{latexEnv}}\n"

  -- Label and lean name
  out := out ++ s!"\\label\{{latexLabel}}\n"
  out := out ++ s!"\\lean\{{name}}\n"

  -- Position info
  if let (some f, some loc) := (file, location) then
    let posStr := s!"{f}|{loc.pos.line}|{loc.pos.column}|{loc.endPos.line}|{loc.endPos.column}"
    out := out ++ s!"\\leanposition\{{posStr}}\n"

  -- Embed syntax highlighting data (base64-encoded HTML) with hover info
  if let some hl := highlighting then
    -- Split into signature and proof body
    let hasProof := config.proof.isSome
    let (sigHl, bodyHl) := splitAtDefinitionAssign hl (splitAtAssign := hasProof)

    -- Render signature to HTML with hovers and base64 encode
    let (sigHtml, sigHoverJson) := HtmlRender.renderHighlightedWithHovers sigHl
    let sigBase64 := Base64.encodeString sigHtml
    out := out ++ s!"\\leansignaturesourcehtml\{{sigBase64}}\n"

    -- Render proof body if present (hovers not used - separate ID space would conflict)
    if let some proofHl := bodyHl then
      let proofHtml := HtmlRender.renderHighlightedToHtml proofHl
      let proofBase64 := Base64.encodeString proofHtml
      out := out ++ s!"\\leanproofsourcehtml\{{proofBase64}}\n"

    -- Emit signature hover data as base64-encoded JSON
    -- (Signature hovers are most useful - variable types, constant docs)
    let hoverBase64 := Base64.encodeString sigHoverJson
    out := out ++ s!"\\leanhoverdata\{{hoverBase64}}\n"

  -- Uses (from config, not inferred)
  unless config.usesLabels.isEmpty do
    out := out ++ s!"\\uses\{{",".intercalate config.usesLabels.toList}}\n"

  -- Check for sorry
  let hasSorry ← declarationHasSorry name
  if !hasSorry then
    out := out ++ "\\leanok\n"

  -- Statement text (from config or empty)
  if let some stmt := config.statement then
    out := out ++ stmt.trimAscii.toString ++ "\n"

  out := out ++ s!"\\end\{{latexEnv}}\n"

  -- Proof section if present
  if let some proofText := config.proof then
    out := out ++ "\\begin{proof}\n"
    unless config.proofUsesLabels.isEmpty do
      out := out ++ s!"\\uses\{{",".intercalate config.proofUsesLabels.toList}}\n"
    -- Proof is leanok if main declaration is leanok
    if !hasSorry then
      out := out ++ "\\leanok\n"
    out := out ++ proofText.trimAscii.toString ++ "\n"
    out := out ++ "\\end{proof}\n"

  return out

/-- Write .tex file for a single declaration.

    Writes to `.lake/build/blueprint/module/{Module/Path}.artifacts/{sanitizedLabel}.tex`
    where the label is sanitized for filesystem use (`:` → `-`).

    @param moduleName The current module name
    @param latexLabel The LaTeX label for this declaration
    @param texContent The generated .tex content -/
def writeDeclarationTex (moduleName : Name) (latexLabel : String) (texContent : String) : IO Unit := do
  let buildDir : System.FilePath := ".lake" / "build"

  -- Sanitize label for filesystem (replace : with -)
  let sanitizedLabel := latexLabel.replace ":" "-"

  -- Build module path components
  let modulePathComponents := moduleName.components.map (·.toString)

  -- Write to .lake/build/blueprint/module/{Module/Path}.artifacts/{label}.tex
  let blueprintModulePath := modulePathComponents.foldl (init := buildDir / "blueprint" / "module")
    fun path component => path / component
  let blueprintArtifactsDir := blueprintModulePath.addExtension "artifacts"
  let blueprintTexPath := blueprintArtifactsDir / (sanitizedLabel ++ ".tex")
  IO.FS.createDirAll blueprintArtifactsDir
  IO.FS.writeFile blueprintTexPath texContent

  -- Also write to .lake/build/dressed/{Module/Path}.artifacts/{label}.tex
  let dressedModulePath := modulePathComponents.foldl (init := buildDir / "dressed")
    fun path component => path / component
  let dressedArtifactsDir := dressedModulePath.addExtension "artifacts"
  let dressedTexPath := dressedArtifactsDir / (sanitizedLabel ++ ".tex")
  IO.FS.createDirAll dressedArtifactsDir
  IO.FS.writeFile dressedTexPath texContent

end Dress.Generate

-- Re-export at Dress namespace for backward compatibility
namespace Dress

abbrev declarationHasSorry := Generate.declarationHasSorry
abbrev getDefaultLatexEnv := Generate.getDefaultLatexEnv
abbrev generateDeclarationTex := Generate.generateDeclarationTex
abbrev writeDeclarationTex := Generate.writeDeclarationTex

end Dress
