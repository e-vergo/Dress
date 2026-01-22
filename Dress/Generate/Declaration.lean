/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Highlighting
import Dress.Paths
import Dress.HtmlRender
import Dress.Base64
import Dress.Generate.Latex
import Architect.Basic

/-!
# Per-Declaration Artifact Generation

This module provides functions for writing per-declaration artifacts during elaboration.
Each @[blueprint] declaration gets its own subdirectory:
- `{sanitized-label}/decl.tex` - LaTeX content for the declaration
- `{sanitized-label}/decl.html` - Syntax-highlighted HTML
- `{sanitized-label}/decl.json` - JSON metadata with highlighting data

## Output Location

All artifacts are written to `.lake/build/dressed/{Module/Path}/{sanitized-label}/`.
-/

open Lean Elab Command
open SubVerso.Highlighting

namespace Dress.Generate

/-- Write all artifacts (.tex, .html, .json) for a single @[blueprint] declaration using Architect.Node.
    This function reads from the LeanArchitect environment extension instead of re-parsing.

    @param name The fully qualified declaration name
    @param node The Architect.Node from blueprintExt
    @param highlighting Optional SubVerso highlighted code
    @param file Optional source file path
    @param location Optional declaration position range -/
def writeDeclarationArtifactsFromNode (name : Name) (node : Architect.Node)
    (highlighting : Option Highlighted) (file : Option System.FilePath) (location : Option DeclarationRange)
    : CommandElabM Unit := do
  -- Get build directory and module name
  let buildDir : System.FilePath := ".lake" / "build"
  let env ← getEnv
  let moduleName := env.header.mainModule

  -- Use the LaTeX label from the node
  let label := node.latexLabel

  -- Get declaration directory and create it
  let declDir := Paths.getDeclarationDir buildDir moduleName label
  IO.FS.createDirAll declDir

  -- Write .tex file using the Node-based generator
  let texContent ← generateDeclarationTexFromNode name node highlighting none file location
  let texPath := Paths.getDeclarationTexPath buildDir moduleName label
  IO.FS.writeFile texPath texContent
  trace[blueprint.debug] "Wrote {texPath}"

  -- Write .html file if we have highlighting
  if let some hl := highlighting then
    let htmlContent := HtmlRender.renderHighlightedToHtml hl
    let htmlPath := Paths.getDeclarationHtmlPath buildDir moduleName label
    IO.FS.writeFile htmlPath htmlContent
    trace[blueprint.debug] "Wrote {htmlPath}"

  -- Write .json file with metadata
  let jsonPath := Paths.getDeclarationJsonPath buildDir moduleName label
  let jsonContent := match highlighting with
    | some hl =>
      let hlJson := (Lean.toJson hl).compress
      s!"\{\"name\": \"{name}\", \"label\": \"{label}\", \"highlighting\": {hlJson}}"
    | none =>
      s!"\{\"name\": \"{name}\", \"label\": \"{label}\"}"
  IO.FS.writeFile jsonPath jsonContent
  trace[blueprint.debug] "Wrote {jsonPath}"

end Dress.Generate

-- Re-export at Dress namespace for convenience
namespace Dress

abbrev writeDeclarationArtifactsFromNode := Generate.writeDeclarationArtifactsFromNode

end Dress
