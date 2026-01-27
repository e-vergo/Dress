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
  "https://github.com/e-vergo/LeanArchitect.git" @ "f0304c2"

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "v4.27.0"

require subverso from git
  "https://github.com/e-vergo/subverso.git" @ "tactic-hover-support"

require verso from git
  "https://github.com/leanprover/verso.git" @ "9137d01"

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

/-- Get the path for a library's index file.
    Returns `.lake/build/dressed/library/{LibName}.tex` -/
def getLibraryIndexPath (buildDir : System.FilePath) (libName : Lean.Name) : System.FilePath :=
  buildDir / "dressed" / "library" / (libName.toString (escape := false) ++ ".tex")

/-- Get the module tex path for use in LaTeX \input directives.
    Returns path relative to `blueprint/src/`: `../../.lake/build/dressed/{Module/Path}/module.tex` -/
def getModuleTexPathForLatex (moduleName : Lean.Name) : String :=
  let modulePathComponents := moduleName.components.map (·.toString)
  "../../.lake/build/dressed/" ++ "/".intercalate modulePathComponents ++ "/module.tex"

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

Generates module.tex with direct \input{} calls for each declaration. -/

/-- Header comment for module.tex files. -/
def moduleHeaderPreamble : String :=
  "%%% This file is automatically generated by Dress. %%%"

/-- Get the path for the global manifest file.
    Returns `.lake/build/dressed/manifest.json` -/
def getManifestPath (buildDir : System.FilePath) : System.FilePath :=
  buildDir / "dressed" / "manifest.json"

/-- Get the relative path from dressed/ to a declaration directory.
    Returns `{Module/Path}/{sanitized-label}` -/
def getDeclarationRelativePath (moduleName : Lean.Name) (label : String) : String :=
  let modulePathComponents := moduleName.components.map (·.toString)
  "/".intercalate modulePathComponents ++ "/" ++ sanitizeLabel label

/-- Facet that generates module.tex from per-declaration artifacts.

    **Depends on:** `dressed` facet (ensures artifacts are aggregated)

    **Reads:** Per-declaration .json files from `.lake/build/dressed/{Module/Path}/{sanitized-label}/`

    **Writes:** `.lake/build/dressed/{Module/Path}/module.tex` with:
    - Direct \input{} calls for each declaration's .tex file

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

    -- Generate direct \input{} calls for each declaration
    let inputEntries := declLabels.map fun label =>
      let inputPath := getDeclarationDirForLatex mod.name label ++ "/decl"
      s!"\\input\{{inputPath}}"

    let texContent := moduleHeaderPreamble ++ "\n\n" ++
      "\n\n".intercalate inputEntries.toList

    IO.FS.writeFile moduleTexPath texContent
    return moduleTexPath

/-! ## Library and Package Facets -/

/-- Recursively find all manifest.entry files in a directory -/
partial def findManifestEntries (dir : System.FilePath) : IO (Array System.FilePath) := do
  if !(← dir.pathExists) then return #[]
  let entries ← dir.readDir
  let mut results : Array System.FilePath := #[]
  for entry in entries do
    let path := entry.path
    if ← path.isDir then
      let subResults ← findManifestEntries path
      results := results ++ subResults
    else if path.fileName == some "manifest.entry" then
      results := results.push path
  return results

/-- Build all module blueprint facets, generate library index, and aggregate manifest.json.

    The library index file is written to `.lake/build/dressed/library/{LibName}.tex`
    and defines the `\inputleanmodule` macro.

    The manifest.json file maps labels to their module paths for Runway consumption.

    Consumer projects include this in their `blueprint/src/` with:
    `\input{../../.lake/build/dressed/library/MyLib.tex}` -/
def buildLibraryBlueprint (lib : LeanLib) : FetchM (Job Unit) := do
  let ws ← getWorkspace
  let mods ← (← lib.modules.fetch).await
  let moduleJobs := Job.collectArray <| ← mods.mapM (fetch <| ·.facet `blueprint)
  moduleJobs.mapM fun _ => do
    let buildDir := ws.root.buildDir
    let dressedDir := buildDir / "dressed"

    -- Aggregate manifest.entry files into manifest.json
    let manifestEntryFiles ← findManifestEntries dressedDir
    let mut manifestEntries : Array (String × Lean.Json) := #[]

    for entryPath in manifestEntryFiles do
      let content ← IO.FS.readFile entryPath
      if let some json := parseJson? content then
        if let some label := getJsonString? json "label" then
          if let some path := getJsonString? json "path" then
            manifestEntries := manifestEntries.push (label, Lean.Json.str path)

    let manifestJson := Lean.Json.mkObj manifestEntries.toList
    let manifestPath := getManifestPath buildDir
    IO.FS.writeFile manifestPath manifestJson.compress

    -- Generate library index file
    let libraryIndexPath := getLibraryIndexPath buildDir lib.name
    let libraryDir := libraryIndexPath.parent.get!

    -- Create dressed/library/ directory
    IO.FS.createDirAll libraryDir

    -- Generate path definitions for each module using simple \def
    let modulePathDefs := mods.map fun mod =>
      let moduleDotName := mod.name.toString (escape := false)
      let modulePath := getModuleTexPathForLatex mod.name
      s!"\\expandafter\\def\\csname leanmodulepath@{moduleDotName}\\endcsname\{{modulePath}}"

    -- Build library index content with macro definitions
    -- NOTE: \inputleannode is handled natively by Runway via manifest.json lookup.
    -- The \inputleanmodule macro is kept for LaTeX compatibility.
    let preamble := s!"%%% Library index for {lib.name} - generated by Dress %%%

%%% Macro definitions %%%
\\makeatletter

% \\inputleanmodule\{Module.Name} - inputs a Lean module's content
% Looks up the path via \\csname, then inputs it
\\providecommand\{\\inputleanmodule}[1]\{%
  \\edef\\@leanmodulepath\{\\csname leanmodulepath@#1\\endcsname}%
  \\input\{\\@leanmodulepath}}

\\makeatother

%%% Module path definitions %%%
"

    let indexContent := preamble ++ "\n".intercalate modulePathDefs.toList ++ "\n"

    IO.FS.writeFile libraryIndexPath indexContent

/-- A facet to build dressed artifacts and blueprint .tex for all modules in a library.
    Also generates a library index file at `.lake/build/dressed/library/{LibName}.tex`. -/
library_facet blueprint (lib : LeanLib) : Unit := do
  buildLibraryBlueprint lib

/-- A facet to build dressed artifacts and blueprint .tex for each library in a package. -/
package_facet blueprint (pkg : Package) : Unit := do
  let libJobs := Job.collectArray <| ← pkg.leanLibs.mapM (fetch <| ·.facet `blueprint)
  let _ ← libJobs.await
  return .nil

/-! ## Dependency Graph Facet

Generates `dep-graph.svg` and `dep-graph.json` from blueprint nodes. -/

/-- Get the path for the dependency graph SVG file.
    Returns `.lake/build/dressed/dep-graph.svg` -/
def getDepGraphSvgPath (buildDir : System.FilePath) : System.FilePath :=
  buildDir / "dressed" / "dep-graph.svg"

/-- Get the path for the dependency graph JSON file.
    Returns `.lake/build/dressed/dep-graph.json` -/
def getDepGraphJsonPath (buildDir : System.FilePath) : System.FilePath :=
  buildDir / "dressed" / "dep-graph.json"

/-- Build the dependency graph for a library by invoking the extract_blueprint executable.

    This facet depends on the `blueprint` facet to ensure all modules are compiled
    and blueprint nodes are available in the environment.

    Outputs:
    - `.lake/build/dressed/dep-graph.svg` - SVG visualization of the dependency graph
    - `.lake/build/dressed/dep-graph.json` - JSON data for interactive visualization -/
def buildLibraryDepGraph (lib : LeanLib) : FetchM (Job FilePath) := do
  let ws ← getWorkspace
  -- Depend on blueprint facet to ensure all modules are compiled
  let blueprintJob ← fetch <| lib.facet `blueprint
  let mods ← (← lib.modules.fetch).await
  let lake ← getLake

  blueprintJob.mapM fun _ => do
    let buildDir := ws.root.buildDir
    let svgPath := getDepGraphSvgPath buildDir
    -- Note: JSON path is generated by the executable alongside SVG
    let _jsonPath := getDepGraphJsonPath buildDir

    -- Create output directory
    IO.FS.createDirAll (buildDir / "dressed")

    -- Get module names as strings for the CLI
    let moduleNames := mods.map fun mod => mod.name.toString (escape := false)

    -- Find the extract_blueprint executable
    -- In the context of a downstream project using Dress, we need to find
    -- the executable from the Dress package
    let exePath := ws.root.buildDir / "bin" / "extract_blueprint"

    -- Check if we're in the Dress package itself or a downstream project
    let exeExists ← exePath.pathExists
    let exeToUse ← if exeExists then
        pure exePath
      else
        -- Try to find it in the packages
        let dressExePath := ws.root.lakeDir / "packages" / "Dress" / ".lake" / "build" / "bin" / "extract_blueprint"
        let dressExeExists ← dressExePath.pathExists
        if !dressExeExists then
          IO.eprintln s!"Warning: extract_blueprint executable not found. Skipping dep-graph generation."
          IO.eprintln s!"  Tried: {exePath}"
          IO.eprintln s!"  Tried: {dressExePath}"
          -- Create empty files as placeholders
          IO.FS.writeFile svgPath ""
          return svgPath
        pure dressExePath

    -- Run via `lake env` to ensure correct LEAN_PATH is set
    -- This allows the executable to import project modules
    let args := #["env", exeToUse.toString, "graph", "--build", buildDir.toString] ++ moduleNames
    let child ← IO.Process.spawn {
      cmd := lake.toString
      args := args
      stdout := .inherit
      stderr := .inherit
      stdin := .null
    }
    let exitCode ← child.wait
    if exitCode != 0 then
      IO.eprintln s!"Warning: dep-graph generation failed with exit code {exitCode}"

    return svgPath

/-- A facet to generate dependency graph (SVG and JSON) for all modules in a library.
    Depends on the `blueprint` facet. -/
library_facet depGraph (lib : LeanLib) : FilePath := do
  buildLibraryDepGraph lib

/-- A facet to build dependency graphs for each library in a package. -/
package_facet depGraph (pkg : Package) : Unit := do
  let libJobs := Job.collectArray <| ← pkg.leanLibs.mapM (fetch <| ·.facet `depGraph)
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
