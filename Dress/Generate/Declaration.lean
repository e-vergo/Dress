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
import Dress.Cache
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

## Caching

Artifacts are cached based on content hash. On cache hit, artifacts are restored
from `.lake/build/dressed/.decl_cache/{hash}/` instead of being regenerated.
-/

open Lean Elab Command
open SubVerso.Highlighting

namespace Dress.Generate

/-- Generate all artifacts (.tex, .html, .json) for a declaration (no caching).
    This is the core generation logic, called on cache miss. -/
private def generateArtifacts (name : Name) (node : Architect.Node)
    (highlighting : Option Highlighted) (file : Option System.FilePath)
    (location : Option DeclarationRange) (buildDir : System.FilePath)
    (moduleName : Name) (label : String)
    : CommandElabM Unit := do
  -- Time: Generate .tex content
  let texGenStart ← IO.monoMsNow
  let texContent ← generateDeclarationTexFromNode name node highlighting none file location
  let texGenEnd ← IO.monoMsNow

  -- Time: Write .tex file
  let texWriteStart ← IO.monoMsNow
  let texPath := Paths.getDeclarationTexPath buildDir moduleName label
  IO.FS.writeFile texPath texContent
  let texWriteEnd ← IO.monoMsNow
  trace[blueprint.debug] "Wrote {texPath}"

  -- Time: HTML rendering and writing
  let (htmlRenderTime, htmlWriteTime) ← if let some hl := highlighting then
    -- Time: Render HTML with hovers
    -- Rainbow bracket highlighting is applied automatically by Verso's toHtmlRainbow
    let renderStart ← IO.monoMsNow
    let (htmlContent, hoverJson) := HtmlRender.renderHighlightedWithHovers hl
    let renderEnd ← IO.monoMsNow

    -- Time: Write HTML and hovers
    let writeStart ← IO.monoMsNow
    let htmlPath := Paths.getDeclarationHtmlPath buildDir moduleName label
    IO.FS.writeFile htmlPath htmlContent
    let hoversPath := Paths.getDeclarationHoversPath buildDir moduleName label
    IO.FS.writeFile hoversPath hoverJson
    let writeEnd ← IO.monoMsNow

    trace[blueprint.debug] "Wrote {htmlPath}"
    trace[blueprint.debug] "Wrote {hoversPath}"
    pure (renderEnd - renderStart, writeEnd - writeStart)
  else
    -- No highlighting (retroactive annotation): generate plain-text signature from environment
    let writeStart ← IO.monoMsNow
    let sig ← liftTermElabM do
      let env ← getEnv
      match env.find? name with
      | some ci => do
        -- Check if this is a structure and show fields
        match Lean.getStructureInfo? env name with
        | some sinfo =>
          let fields ← sinfo.fieldInfo.mapM fun fi => do
            match env.find? fi.projFn with
            | some fci =>
              Lean.Meta.forallTelescopeReducing fci.type fun _ body => do
                let fmtF ← Lean.Meta.ppExpr body
                pure s!"  {fi.fieldName} : {fmtF}"
            | none => pure s!"  {fi.fieldName}"
          let fieldStr := "\n".intercalate fields.toList
          pure s!"structure {name} where\n{fieldStr}"
        | none =>
          let fmt ← Lean.Meta.ppExpr ci.type
          match ci with
          | .defnInfo _ => pure s!"def {name} : {fmt} := ..."
          | .thmInfo _ => pure s!"theorem {name} : {fmt}"
          | .axiomInfo _ => pure s!"axiom {name} : {fmt}"
          | .inductInfo _ => pure s!"inductive {name} : {fmt}"
          | _ => pure s!"{name} : {fmt}"
      | none => pure s!"-- {name} (declaration not found)"
    -- Escape HTML entities
    let htmlSig := sig.replace "&" "&amp;" |>.replace "<" "&lt;" |>.replace ">" "&gt;"
    let htmlContent := s!"<pre class=\"lean-code hl lean\"><code class=\"hl lean\">{htmlSig}</code></pre>"
    let htmlPath := Paths.getDeclarationHtmlPath buildDir moduleName label
    IO.FS.writeFile htmlPath htmlContent
    let hoversPath := Paths.getDeclarationHoversPath buildDir moduleName label
    IO.FS.writeFile hoversPath "{}"
    let writeEnd ← IO.monoMsNow
    trace[blueprint.debug] "Wrote {htmlPath} (plain-text signature)"
    trace[blueprint.debug] "Wrote {hoversPath} (empty hovers)"
    pure (0, writeEnd - writeStart)

  -- Time: Write .json file with metadata
  let jsonWriteStart ← IO.monoMsNow
  let jsonPath := Paths.getDeclarationJsonPath buildDir moduleName label
  let jsonContent := match highlighting with
    | some hl =>
      let hlJson := (Lean.toJson hl).compress
      s!"\{\"name\": \"{name}\", \"label\": \"{label}\", \"highlighting\": {hlJson}}"
    | none =>
      s!"\{\"name\": \"{name}\", \"label\": \"{label}\", \"plainText\": true}"
  IO.FS.writeFile jsonPath jsonContent
  let jsonWriteEnd ← IO.monoMsNow
  trace[blueprint.debug] "Wrote {jsonPath}"

  -- Time: Write manifest.entry
  let manifestWriteStart ← IO.monoMsNow
  let manifestEntryPath := Paths.getManifestEntryPath buildDir moduleName label
  let relativePath := Paths.getDeclarationRelativePath moduleName label
  let manifestEntryContent := s!"\{\"label\": \"{label}\", \"path\": \"{relativePath}\"}"
  IO.FS.writeFile manifestEntryPath manifestEntryContent
  let manifestWriteEnd ← IO.monoMsNow
  trace[blueprint.debug] "Wrote {manifestEntryPath}"

  -- Print sub-operation timing breakdown (only when trace option enabled)
  trace[blueprint.timing] "  texGen: {texGenEnd - texGenStart}ms for {name}"
  trace[blueprint.timing] "  texWrite: {texWriteEnd - texWriteStart}ms for {name}"
  trace[blueprint.timing] "  htmlRender: {htmlRenderTime}ms for {name}"
  trace[blueprint.timing] "  htmlWrite: {htmlWriteTime}ms for {name}"
  trace[blueprint.timing] "  jsonWrite: {jsonWriteEnd - jsonWriteStart}ms for {name}"
  trace[blueprint.timing] "  manifestWrite: {manifestWriteEnd - manifestWriteStart}ms for {name}"

/-- Write all artifacts (.tex, .html, .json) for a single @[blueprint] declaration using Architect.Node.
    This function reads from the LeanArchitect environment extension instead of re-parsing.

    Uses content-based caching: on cache hit, artifacts are restored from cache;
    on cache miss, artifacts are generated and saved to cache.

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
  let rawModule := env.header.mainModule
  -- For standalone files (not part of a Lake project), mainModule is `_stdin`.
  -- Derive a clean module name from the actual filename to avoid all standalone
  -- files colliding in `.lake/build/dressed/_stdin/`.
  let moduleName : Name :=
    if rawModule == `_stdin then
      match file with
      | some fp =>
        -- Extract stem: "/tmp/math/test.lean" -> "test"
        let stem := fp.fileStem.getD "standalone"
        .mkSimple stem
      | none => rawModule
    else rawModule

  -- Use the LaTeX label from the node
  let label := node.latexLabel

  -- Get declaration directory
  let declDir := Paths.getDeclarationDir buildDir moduleName label

  -- Compute content hash for caching
  let hash ← Cache.computeDeclarationHash name node highlighting file location

  -- Check cache
  if ← Cache.checkCache buildDir hash then
    -- Cache hit: restore from cache
    trace[blueprint.debug] "Cache hit for {name} (hash: {hash})"
    Cache.restoreFromCache buildDir hash declDir
    trace[blueprint.debug] "Restored {declDir} from cache"
  else
    -- Cache miss: generate artifacts
    trace[blueprint.debug] "Cache miss for {name} (hash: {hash})"
    IO.FS.createDirAll declDir
    generateArtifacts name node highlighting file location buildDir moduleName label
    -- Save to cache for future builds
    Cache.saveToCache buildDir hash declDir
    trace[blueprint.debug] "Saved {declDir} to cache"

end Dress.Generate

-- Re-export at Dress namespace for convenience
namespace Dress

abbrev writeDeclarationArtifactsFromNode := Generate.writeDeclarationArtifactsFromNode

end Dress
