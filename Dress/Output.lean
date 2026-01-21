/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Architect
import Dress.Content
import Dress.HtmlRender
import Dress.Hook
import Dress.Paths

open Lean

/-!
# Dress Output

This module provides LaTeX and JSON output functionality for Dress, extending
the base functionality from Architect with syntax highlighting support.

Dress extends `Architect.NodeWithPos` with additional fields for highlighted code,
so the `toLatex` function here emits additional macros like `\leansourcehtml{}`.
-/

-- Define functions in Architect namespace so dot notation works with Architect types
namespace Architect

variable {m} [Monad m] [MonadEnv m] [MonadError m]

private def isMathlibOk (name : Name) : m Bool := do
  let some modIdx := (← getEnv).getModuleIdxFor? name | return false
  let module := (← getEnv).allImportedModuleNames[modIdx]!
  return [`Init, `Lean, `Std, `Batteries, `Mathlib].any fun pre => pre.isPrefixOf module

/-- Convert a Dress.NodeWithPos to LaTeX, including syntax highlighting.
    This extends the base Architect output with highlighting macros. -/
def _root_.Dress.NodeWithPos.toLatex (node : Dress.NodeWithPos) : m Latex := do
  -- In the output, we merge the Lean nodes corresponding to the same LaTeX label.
  let env ← getEnv
  let allLeanNames := getLeanNamesOfLatexLabel env node.latexLabel
  let allNodes := allLeanNames.filterMap fun name => blueprintExt.find? env name

  let mut addLatex := ""
  addLatex := addLatex ++ "\\label{" ++ node.latexLabel ++ "}\n"

  addLatex := addLatex ++ "\\lean{" ++ ",".intercalate (allLeanNames.map toString).toList ++ "}\n"
  if allNodes.any (·.notReady) then
    addLatex := addLatex ++ "\\notready\n"
  if let some d := allNodes.findSome? (·.discussion) then
    addLatex := addLatex ++ "\\discussion{" ++ toString d ++ "}\n"
  if ← allNodes.allM (isMathlibOk ·.name) then
    addLatex := addLatex ++ "\\mathlibok\n"

  -- position string as annotation
  let posStr := match node.file, node.location with
    | some file, some location => s!"{file}:{location.range.pos.line}.{location.range.pos.column}-{location.range.endPos.line}.{location.range.endPos.column}"
    | _, _ => ""
  addLatex := addLatex ++ s!"% at {posStr}\n"

  -- Emit \leanposition and \leansource INSIDE the environment so plasTeX attaches them to the correct node
  -- leanposition uses selectionRange (signature only, excludes proof body)
  if let (some file, some location) := (node.file, node.location) then
    let positionStr := s!"{file}|{location.range.pos.line}|{location.range.pos.column}|{location.range.endPos.line}|{location.range.endPos.column}"
    addLatex := addLatex ++ "\\leanposition{" ++ positionStr ++ "}\n"
  -- Emit proof body position separately (for side-by-side proof display)
  if let (some file, some proofLoc) := (node.file, node.proofLocation) then
    let proofPosStr := s!"{file}|{proofLoc.pos.line}|{proofLoc.pos.column}|{proofLoc.endPos.line}|{proofLoc.endPos.column}"
    addLatex := addLatex ++ "\\leanproofposition{" ++ proofPosStr ++ "}\n"
  if let some hl := node.highlightedCode then
    -- JSON format (backward compatibility)
    let jsonStr := (toJson hl).compress
    let base64Json := Dress.stringToBase64 jsonStr
    addLatex := addLatex ++ "\\leansource{" ++ base64Json ++ "}\n"
    -- HTML format: use pre-rendered if available, otherwise render on demand
    let htmlStr := node.htmlCode.getD (Dress.HtmlRender.renderHighlightedToHtml hl)
    let base64Html := Dress.stringToBase64 htmlStr
    addLatex := addLatex ++ "\\leansourcehtml{" ++ base64Html ++ "}\n"
  -- Emit signature highlighting
  if let some hl := node.highlightedSignature then
    -- JSON format (backward compatibility)
    let jsonStr := (toJson hl).compress
    let base64Json := Dress.stringToBase64 jsonStr
    addLatex := addLatex ++ "\\leansignaturesource{" ++ base64Json ++ "}\n"
    -- HTML format: use pre-rendered if available, otherwise render on demand
    let htmlStr := node.htmlSignature.getD (Dress.HtmlRender.renderHighlightedToHtml hl)
    let base64Html := Dress.stringToBase64 htmlStr
    addLatex := addLatex ++ "\\leansignaturesourcehtml{" ++ base64Html ++ "}\n"
  -- Emit proof body highlighting
  if let some hl := node.highlightedProofBody then
    -- JSON format (backward compatibility)
    let jsonStr := (toJson hl).compress
    let base64Json := Dress.stringToBase64 jsonStr
    addLatex := addLatex ++ "\\leanproofsource{" ++ base64Json ++ "}\n"
    -- HTML format: use pre-rendered if available, otherwise render on demand
    let htmlStr := node.htmlProofBody.getD (Dress.HtmlRender.renderHighlightedToHtml hl)
    let base64Html := Dress.stringToBase64 htmlStr
    addLatex := addLatex ++ "\\leanproofsourcehtml{" ++ base64Html ++ "}\n"

  let inferredUsess ← allNodes.mapM (·.inferUses)
  let statementUses := InferredUses.merge (inferredUsess.map (·.1))
  let proofUses := InferredUses.merge (inferredUsess.map (·.2))

  let statementLatex ← node.statement.toLatex (allNodes.map (·.statement)) statementUses (allNodes.findSome? (·.title)) addLatex
  let mainContent ← match node.proof with
    | none => pure statementLatex
    | some proof =>
      let proofDocString := getProofDocString env node.name
      let proofLatex ← proof.toLatex (allNodes.filterMap (·.proof)) proofUses (defaultText := proofDocString)
      pure (statementLatex ++ proofLatex)

  return mainContent

def _root_.Dress.NodeWithPos.toLatexArtifact (node : Dress.NodeWithPos) : m LatexArtifact := do
  return { id := node.latexLabel, content := ← node.toLatex }

end Architect

-- Continue in Dress namespace for the rest
namespace Dress

-- Reuse types from Architect namespace
abbrev Latex := Architect.Latex
abbrev LatexArtifact := Architect.LatexArtifact
abbrev LatexOutput := Architect.LatexOutput
abbrev InferredUses := Architect.InferredUses

def BlueprintContent.toLatex : BlueprintContent → CoreM String
  | .node n => return "\\inputleannode{" ++ n.latexLabel ++ "}"
  | .modDoc d => return d.doc

def latexPreamble : CoreM String := do
  return "%%% This file is automatically generated by Dress. %%%

%%% Macro definitions for \\inputleannode, \\inputleanmodule %%%

\\makeatletter

% \\newleannode{name}{latex} defines a new Lean node
\\providecommand{\\newleannode}[2]{%
  \\expandafter\\gdef\\csname leannode@#1\\endcsname{#2}}
% \\inputleannode{name} inputs a Lean node
\\providecommand{\\inputleannode}[1]{%
  \\csname leannode@#1\\endcsname}

% \\newleanmodule{module}{latex} defines a new Lean module
\\providecommand{\\newleanmodule}[2]{%
  \\expandafter\\gdef\\csname leanmodule@#1\\endcsname{#2}}
% \\inputleanmodule{module} inputs a Lean module
\\providecommand{\\inputleanmodule}[1]{%
  \\csname leanmodule@#1\\endcsname}

\\makeatother

%%% Start of main content %%%"

private def dedupContentsByLatexLabel (contents : Array BlueprintContent) : Array BlueprintContent := Id.run do
  let mut seen : Std.HashSet String := ∅
  let mut result : Array BlueprintContent := #[]
  for content in contents do
    match content with
    | .node n =>
      if seen.contains n.latexLabel then
        continue
      seen := seen.insert n.latexLabel
      result := result.push content
    | .modDoc _ =>
      result := result.push content
  return result

/-- Convert a module to a header file and artifacts. The header file requires the path to the artifacts directory. -/
private def moduleToLatexOutputAux (module : Name) (contents : Array BlueprintContent) : CoreM Architect.LatexOutput := do
  -- First deduplicate contents by LaTeX label
  let contents' := dedupContentsByLatexLabel contents
  -- Artifact files
  let artifacts : Array Architect.LatexArtifact := ← contents'.filterMapM fun
    | .node n => n.toLatexArtifact
    | _ => pure none
  -- Header file
  let preamble ← latexPreamble
  let headerModuleLatex ← contents'.mapM BlueprintContent.toLatex
  let header (artifactsDir : System.FilePath) : String :=
    preamble ++ "\n\n" ++
      "\n\n".intercalate (artifacts.map fun ⟨id, _⟩ => "\\newleannode{" ++ id ++ "}{" ++ Architect.Latex.input (artifactsDir / id) ++ "}").toList ++ "\n\n" ++
      "\\newleanmodule{" ++ module.toString ++ "}{\n" ++ "\n\n".intercalate headerModuleLatex.toList ++ "\n}"
  return { header, artifacts }

/-- Convert imported module to LaTeX (header file, artifact files).
    Highlighted code is obtained from the provided map (from subverso-extract-mod),
    with fallback to the Hook mechanism during elaboration.
    Pre-rendered HTML is obtained from the htmlMap (from .html.json files). -/
def moduleToLatexOutput (module : Name)
    (highlightingMap : NameMap SubVerso.Highlighting.Highlighted := {})
    (htmlMap : NameMap String := {})
    : CoreM Architect.LatexOutput := do
  let contents ← getBlueprintContents module highlightingMap htmlMap
  moduleToLatexOutputAux module contents

/-- Convert current module to LaTeX (header file, artifact files).
    Highlighted code is captured during elaboration via the Hook mechanism. -/
def mainModuleToLatexOutput : CoreM Architect.LatexOutput := do
  let contents ← getMainModuleBlueprintContents
  moduleToLatexOutputAux (← getMainModule) contents

/-- Shows the blueprint LaTeX of the current module (`#show_blueprint`) or
a blueprint node (`#show_blueprint lean_name` or `#show_blueprint "latex_label"`). -/
syntax (name := show_blueprint) "#show_blueprint" (ppSpace (ident <|> str))? : command

open Elab Command in
@[command_elab show_blueprint] def elabShowBlueprint : CommandElab
  | `(command| #show_blueprint) => do
    let output ← liftCoreM mainModuleToLatexOutput
    output.artifacts.forM fun art => logInfo m!"LaTeX of node {art.id}:\n{art.content}"
    logInfo m!"LaTeX of current module:\n{output.header ""}"
  | `(command| #show_blueprint $id:ident) => do
    let name ← liftCoreM <| realizeGlobalConstNoOverloadWithInfo id
    let some node := Architect.blueprintExt.find? (← getEnv) name | throwError "{name} does not have @[blueprint] attribute"
    let art ← (← liftCoreM <| toDressNodeWithPos node).toLatexArtifact
    logInfo m!"{art.content}"
  | `(command| #show_blueprint $label:str) => do
    let env ← getEnv
    let names := Architect.getLeanNamesOfLatexLabel env label.getString
    if names.isEmpty then throwError "no @[blueprint] nodes with label {label}"
    for name in names do
      elabCommand <| ← `(command| #show_blueprint $(mkIdent name))
  | _ => throwUnsupportedSyntax

section ToJson

private def rangeToJson (range : DeclarationRange) : Json :=
  json% {
    "pos": $(range.pos),
    "endPos": $(range.endPos)
  }

private def locationToJson (location : DeclarationLocation) : Json :=
  json% {
    "module": $(location.module),
    "range": $(rangeToJson location.range)
  }

private def highlightedToJson (hl : SubVerso.Highlighting.Highlighted) : Json := toJson hl

def nodeWithPosToJson (node : NodeWithPos) : Json :=
  json% {
    "name": $(node.name),
    "latexLabel": $(node.latexLabel),
    "statement": $(node.statement),
    "proof": $(node.proof),
    "notReady": $(node.notReady),
    "discussion": $(node.discussion),
    "title": $(node.title),
    "hasLean": $(node.hasLean),
    "file": $(node.file),
    "location": $(node.location.map locationToJson),
    "highlightedCode": $(node.highlightedCode.map highlightedToJson)
  }

def BlueprintContent.toJson : BlueprintContent → Json
  | .node n => json% {"type": "node", "data": $(nodeWithPosToJson n)}
  | .modDoc d => json% {"type": "moduleDoc", "data": $(d.doc)}

def moduleToJson (module : Name) (highlightingMap : NameMap SubVerso.Highlighting.Highlighted := {})
    : CoreM Json := do
  return Json.arr <|
    (← getBlueprintContents module highlightingMap).map BlueprintContent.toJson

def mainModuleToJson : CoreM Json := do
  return Json.arr <|
    (← getMainModuleBlueprintContents).map BlueprintContent.toJson

/-- Shows the blueprint JSON of the current module (`#show_blueprint_json`) or
a single Lean declaration (`#show_blueprint_json name`). -/
syntax (name := show_blueprint_json) "#show_blueprint_json" (ppSpace (ident <|> str))? : command

open Elab Command in
@[command_elab show_blueprint_json] def elabShowBlueprintJson : CommandElab
  | `(command| #show_blueprint_json) => do
    let json ← liftCoreM mainModuleToJson
    logInfo m!"{json}"
  | `(command| #show_blueprint_json $id:ident) => do
    let name ← liftCoreM <| realizeGlobalConstNoOverloadWithInfo id
    let some node := Architect.blueprintExt.find? (← getEnv) name | throwError "{name} does not have @[blueprint] attribute"
    let json := nodeWithPosToJson (← liftCoreM <| toDressNodeWithPos node)
    logInfo m!"{json}"
  | `(command| #show_blueprint_json $label:str) => do
    let env ← getEnv
    let names := Architect.getLeanNamesOfLatexLabel env label.getString
    if names.isEmpty then throwError "no @[blueprint] nodes with label {label}"
    for name in names do
      elabCommand <| ← `(command| #show_blueprint_json $(mkIdent name))
  | _ => throwUnsupportedSyntax

end ToJson

open IO

def moduleToRelPath (module : Name) (ext : String) : System.FilePath :=
  modToFilePath "module" module ext

def libraryToRelPath (library : Name) (ext : String) : System.FilePath :=
  System.mkFilePath ["library", library.toString (escape := false)] |>.addExtension ext

/-- Write `latex` to the appropriate blueprint tex file. Returns the list of paths to auxiliary output files (note: the returned paths are currently discarded). -/
def outputLatexResults (basePath : System.FilePath) (module : Name) (latex : Architect.LatexOutput) : IO (Array System.FilePath) := do
  let filePath := basePath / moduleToRelPath module "tex"
  -- Use relative path for LaTeX \input{} commands
  -- plastex runs from blueprint/, tex files are in blueprint/src/
  -- so ../../ goes from blueprint/src/ to project root, then into .lake/build/dressed/
  let relativeModuleDir : System.FilePath :=
    System.FilePath.mk (Paths.getModuleDirForLatex module)
  -- Absolute path for writing artifact files (declaration subdirs in module dir)
  let moduleDressedDir := basePath / moduleToRelPath module "dressed"
  if let some d := filePath.parent then FS.createDirAll d
  FS.writeFile filePath (latex.header relativeModuleDir)

  latex.artifacts.mapM fun art => do
    let path := moduleDressedDir / art.id / (art.id ++ ".tex")
    if let some d := path.parent then FS.createDirAll d
    FS.writeFile path art.content
    return path

/-- Write `json` to the appropriate blueprint json file. -/
def outputJsonResults (basePath : System.FilePath) (module : Name) (json : Json) : IO Unit := do
  let filePath := basePath / moduleToRelPath module "json"
  if let some d := filePath.parent then FS.createDirAll d
  FS.writeFile filePath json.pretty

/-- Write to an appropriate index tex file that \inputs all modules in a library. -/
def outputLibraryLatex (basePath : System.FilePath) (library : Name) (modules : Array Name) : IO Unit := do
  FS.createDirAll basePath
  let latex : String := "\n\n".intercalate
    (modules.map fun mod => Architect.Latex.input (basePath / moduleToRelPath mod "tex")).toList
  let filePath := basePath / libraryToRelPath library "tex"
  if let some d := filePath.parent then FS.createDirAll d
  FS.writeFile filePath latex

/-- Write to an appropriate index json file containing paths to json files of all modules in a library. -/
def outputLibraryJson (basePath : System.FilePath) (library : Name) (modules : Array Name) : IO Unit := do
  FS.createDirAll basePath
  let json : Json := Json.mkObj [("modules", toJson (modules.map fun mod => moduleToRelPath mod "json"))]
  let content := json.pretty
  let filePath := basePath / libraryToRelPath library "json"
  if let some d := filePath.parent then FS.createDirAll d
  FS.writeFile filePath content

end Dress
