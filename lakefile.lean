import Lake
open System Lake DSL

package Dress where
  -- Lake handles linking for local dependencies automatically

@[default_target]
lean_lib Dress

@[default_target]
lean_exe extract_blueprint where
  root := `Main
  supportInterpreter := true

require LeanArchitect from git
  "https://github.com/e-vergo/LeanArchitect" @ "main"

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "v4.27.0-rc1"

require subverso from git
  "https://github.com/leanprover/subverso" @ "main"

require verso from git
  "https://github.com/leanprover/verso.git" @ "main"

/-! ## Path Helpers

Centralized path construction for dressed artifacts.
Mirrors `Dress.Paths` module for Lake facet use. -/

/-- Get the dressed directory for a module.
    Returns `.lake/build/dressed/{Module/Path}/` -/
def getModuleDressedDir (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  moduleName.components.foldl (init := buildDir / "dressed")
    fun path component => path / component.toString

/-- Get the path for a module's aggregated JSON file.
    Returns `.lake/build/dressed/{Module/Path}/module.json` -/
def getModuleJsonPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  getModuleDressedDir buildDir moduleName / "module.json"

/-- Get the path for a module's LaTeX header file.
    Returns `.lake/build/dressed/{Module/Path}/module.tex` -/
def getModuleTexPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  getModuleDressedDir buildDir moduleName / "module.tex"

/-- Sanitize a label for use as a filename.
    Replaces `:` with `-` for filesystem compatibility. -/
def sanitizeLabel (label : String) : String :=
  label.replace ":" "-"

/-- Get the declaration directory path relative to blueprint/src/ for LaTeX \input.
    Returns `../../.lake/build/dressed/{Module/Path}/{sanitized-label}`

    The `../../` prefix accounts for plastex running from `blueprint/` with
    tex files in `blueprint/src/`. -/
def getDeclarationDirForLatex (moduleName : Lean.Name) (label : String) : String :=
  let modulePathComponents := moduleName.components.map (·.toString)
  "../../.lake/build/dressed/" ++ "/".intercalate modulePathComponents ++ "/" ++ sanitizeLabel label

/-! ## JSON Parsing Helpers -/

/-- Parse a JSON string into a Json object. Returns none on failure. -/
def parseJson? (s : String) : Option Lean.Json :=
  match Lean.Json.parse s with
  | .ok j => some j
  | .error _ => none

/-- Extract a string field from a JSON object. -/
def getJsonString? (j : Lean.Json) (field : String) : Option String :=
  match j.getObjVal? field with
  | .ok (.str s) => some s
  | _ => none

/-! ## Dressed Facet

Aggregates per-declaration artifacts into module-level JSON. -/

/-- Facet that aggregates per-declaration artifacts into module.json.

    **Elaboration writes:** Per-declaration artifacts to `.lake/build/dressed/{Module/Path}/{sanitized-label}/`
    Each artifact file has format: `{"name": "...", "label": "...", "highlighting": {...}}`

    **This facet reads:** All decl.json files from declaration subdirectories in the module directory

    **This facet writes:** `.lake/build/dressed/{Module/Path}/module.json` with format:
    ```json
    {"DeclName": {"html": "...", "htmlBase64": "...", "jsonBase64": "..."}}
    ```

    Handles modules with no @[blueprint] declarations gracefully (empty module dir or no declaration subdirs).

    Cached by Lake - only rebuilds when module's olean changes. -/
module_facet dressed (mod : Module) : FilePath := do
  let ws ← getWorkspace
  let modJob ← mod.lean.fetch  -- Depend on lean facet (ensures elaboration ran)

  let buildDir := ws.root.buildDir
  let moduleDressedDir := getModuleDressedDir buildDir mod.name
  let moduleJsonPath := getModuleJsonPath buildDir mod.name

  modJob.mapM fun _leanArt => do
    -- Create module directory
    IO.FS.createDirAll moduleDressedDir

    -- Check if module dressed directory exists
    let moduleDirExists ← moduleDressedDir.pathExists
    if !moduleDirExists then
      -- No module dir - write empty JSON and return
      IO.FS.writeFile moduleJsonPath "{}"
      return moduleJsonPath

    -- Scan module directory for subdirectories containing decl.json
    -- Skip module.json and module.tex (not declaration dirs)
    let entries ← FilePath.readDir moduleDressedDir
    -- Filter to directories only (declaration subdirs)
    let mut declDirs : Array IO.FS.DirEntry := #[]
    for entry in entries do
      let entryPath := moduleDressedDir / entry.fileName
      if ← entryPath.isDir then
        let jsonPath := entryPath / "decl.json"
        if ← jsonPath.pathExists then
          declDirs := declDirs.push entry

    if Array.isEmpty declDirs then
      -- No declaration directories - write empty JSON
      IO.FS.writeFile moduleJsonPath "{}"
      return moduleJsonPath

    -- Parse each artifact JSON and build module.json
    let mut moduleEntries : Array (String × Lean.Json) := #[]

    for entry in declDirs do
      let filePath := moduleDressedDir / entry.fileName / "decl.json"
      let content ← IO.FS.readFile filePath

      -- Parse the artifact JSON
      if let some json := parseJson? content then
        if let some name := getJsonString? json "name" then
          -- Extract highlighting if present
          let highlightingJson := match json.getObjVal? "highlighting" with
            | .ok hl => hl
            | .error _ => Lean.Json.null

          -- Render HTML from highlighting (or empty if no highlighting)
          let html := if highlightingJson == Lean.Json.null then "" else
            -- We can't call HtmlRender here (not available in Lake), so store raw JSON
            -- The html field will be populated by consumers if needed
            ""
          let htmlBase64 := if html.isEmpty then "" else ""
          let jsonBase64 := "" -- Base64 encoding is done by consumers, not here

          -- Build artifact entry
          let artifactEntry := Lean.Json.mkObj [
            ("html", Lean.Json.str html),
            ("htmlBase64", Lean.Json.str htmlBase64),
            ("jsonBase64", Lean.Json.str jsonBase64),
            ("highlighting", highlightingJson)
          ]
          moduleEntries := moduleEntries.push (name, artifactEntry)

    -- Write module.json
    let moduleJson := Lean.Json.mkObj moduleEntries.toList
    IO.FS.writeFile moduleJsonPath moduleJson.compress

    return moduleJsonPath

/-! ## Blueprint Facet

Generates module.tex with LaTeX preamble and \newleannode entries. -/

/-- LaTeX preamble for module header files.
    Defines \newleannode, \inputleannode, \newleanmodule, \inputleanmodule macros. -/
def moduleHeaderPreamble : String :=
  "%%% This file is automatically generated by Dress. %%%

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

/-- Facet that generates module.tex from per-declaration artifacts.

    **Depends on:** `dressed` facet (ensures artifacts are aggregated)

    **Reads:** Per-declaration .json files from `.lake/build/dressed/{Module/Path}/{sanitized-label}/`

    **Writes:** `.lake/build/dressed/{Module/Path}/module.tex` with:
    - LaTeX preamble macros (\newleannode, \newleanmodule, etc.)
    - \newleannode entries for each declaration
    - Relative \input{} paths to .tex files in declaration subdirs

    Handles modules with no @[blueprint] declarations gracefully. -/
module_facet blueprint (mod : Module) : FilePath := do
  let ws ← getWorkspace
  let dressedJob ← fetch <| mod.facet `dressed  -- Depend on dressed facet

  let buildDir := ws.root.buildDir
  let moduleDressedDir := getModuleDressedDir buildDir mod.name
  let moduleTexPath := getModuleTexPath buildDir mod.name

  dressedJob.mapM fun _moduleJsonPath => do
    -- Check if module dressed directory exists
    let moduleDirExists ← moduleDressedDir.pathExists
    if !moduleDirExists then
      -- No module dir - write minimal .tex file
      IO.FS.writeFile moduleTexPath (moduleHeaderPreamble ++ "\n\n% No @[blueprint] declarations in this module.\n")
      return moduleTexPath

    -- Scan module directory for subdirectories containing decl.json
    -- Skip module.json and module.tex (not declaration dirs)
    let entries ← FilePath.readDir moduleDressedDir
    -- Filter to directories only (declaration subdirs)
    let mut declDirs : Array IO.FS.DirEntry := #[]
    for entry in entries do
      let entryPath := moduleDressedDir / entry.fileName
      if ← entryPath.isDir then
        let jsonPath := entryPath / "decl.json"
        if ← jsonPath.pathExists then
          declDirs := declDirs.push entry

    if Array.isEmpty declDirs then
      -- No declarations - write minimal .tex file
      IO.FS.writeFile moduleTexPath (moduleHeaderPreamble ++ "\n\n% No @[blueprint] declarations in this module.\n")
      return moduleTexPath

    -- Collect declaration labels from JSON files
    let mut declLabels : Array String := #[]

    for entry in declDirs do
      let filePath := moduleDressedDir / entry.fileName / "decl.json"
      let content ← IO.FS.readFile filePath
      if let some json := parseJson? content then
        if let some label := getJsonString? json "label" then
          declLabels := declLabels.push label

    -- Generate \newleannode entries
    let nodeEntries := declLabels.map fun label =>
      let inputPath := getDeclarationDirForLatex mod.name label ++ "/decl"
      s!"\\newleannode\{{label}}\{\\input\{{inputPath}}}"

    -- Generate \inputleannode entries for module content
    let inputEntries := declLabels.map fun label =>
      s!"\\inputleannode\{{label}}"

    let moduleContent := "\n\n".intercalate inputEntries.toList
    let moduleLatex := s!"\\newleanmodule\{{mod.name}}\{\n{moduleContent}\n}"

    let texContent := moduleHeaderPreamble ++ "\n\n" ++
      "\n\n".intercalate nodeEntries.toList ++ "\n\n" ++ moduleLatex

    IO.FS.writeFile moduleTexPath texContent
    return moduleTexPath

/-! ## Library and Package Facets -/

def buildLibraryBlueprint (lib : LeanLib) : FetchM (Job Unit) := do
  let mods ← (← lib.modules.fetch).await
  let moduleJobs := Job.collectArray <| ← mods.mapM (fetch <| ·.facet `blueprint)
  moduleJobs.mapM fun _ => pure ()

/-- A facet to build dressed artifacts and blueprint .tex for all modules in a library. -/
library_facet blueprint (lib : LeanLib) : Unit := do
  buildLibraryBlueprint lib

/-- A facet to build dressed artifacts and blueprint .tex for each library in a package. -/
package_facet blueprint (pkg : Package) : Unit := do
  let libJobs := Job.collectArray <| ← pkg.leanLibs.mapM (fetch <| ·.facet `blueprint)
  let _ ← libJobs.await
  return .nil

/-! ## Utility Script -/

open IO.Process in
/-- Run a command, print all outputs, and throw an error if it fails. -/
private def runCmd (cmd : String) (args : Array String) : ScriptM Unit := do
  let child ← spawn { cmd, args, stdout := .inherit, stderr := .inherit, stdin := .null }
  let exitCode ← child.wait
  if exitCode != 0 then
    throw <| IO.userError s!"Error running command {cmd} {args.toList}"

/-- Build the project with dressed artifact generation enabled.

    This creates a `.lake/build/.dress` marker file, then runs `lake build`.
    Hook.lean detects this marker and automatically exports dressed artifacts
    (highlighting, HTML, .tex) for all `@[blueprint]` declarations.

    Usage: `lake run dress` or `lake run dress MyLib`

    The dressed artifacts are written to:
    - `.lake/build/dressed/{Module/Path}/{sanitized-label}/decl.json` (per-declaration)
    - `.lake/build/dressed/{Module/Path}/{sanitized-label}/decl.tex` (per-declaration LaTeX)
    - `.lake/build/dressed/{Module/Path}/{sanitized-label}/decl.html` (per-declaration HTML)
    - `.lake/build/dressed/{Module/Path}/module.json` (aggregated by dressed facet)
    - `.lake/build/dressed/{Module/Path}/module.tex` (by blueprint facet) -/
script dress (args : List String) do
  let lake ← getLake
  -- Create marker file to signal dress mode to Hook.lean
  let markerFile : System.FilePath := ".lake" / "build" / ".dress"
  IO.FS.createDirAll markerFile.parent.get!
  IO.FS.writeFile markerFile "1"
  -- Run build
  let buildArgs := args.toArray
  runCmd lake.toString (#["build"] ++ buildArgs)
  -- Clean up marker file
  IO.FS.removeFile markerFile
  return 0
