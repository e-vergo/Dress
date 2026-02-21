/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Quickstart: Scaffold an SBS Blueprint Project

This module implements the `quickstart` subcommand for the Dress CLI.
Given an existing Lean project (with Dress already in `lakefile.toml`),
it scaffolds everything needed to make it a functioning SBS blueprint project:

1. Detects project info from `lakefile.toml` (project name, lean_lib names)
2. Scaffolds `runway.json`, CI workflow, and LaTeX blueprint stub
3. Injects `import Dress` into `.lean` source files that contain declarations
4. Prints a summary of all changes and next steps

This is a pure IO module -- it does not load a Lean environment.
-/

namespace Dress.Quickstart

-- ============================================================================
-- TOML Parsing (lightweight, line-by-line)
-- ============================================================================

/-- Strip surrounding double quotes from a string, if present. -/
private def unquote (s : String) : String :=
  if s.startsWith "\"" && s.endsWith "\"" && s.length >= 2 then
    ((s.drop 1).dropEnd 1).toString
  else s

/-- Extract the value from a `key = "value"` TOML line. Returns the unquoted value. -/
private def extractTomlValue (line : String) : Option String :=
  let parts := line.splitOn "="
  if parts.length >= 2 then
    let rhs := (String.intercalate "=" parts.tail!).trimAscii.toString
    some (unquote rhs)
  else
    none

/-- Extract the project name from `lakefile.toml`.
    Scans for `name = "..."` at the top level (before any `[[` section header). -/
def extractProjectName (lines : Array String) : Option String := do
  for line in lines do
    let trimmed := line.trimAscii.toString
    -- Stop at the first section header
    if trimmed.startsWith "[[" || trimmed.startsWith "[" then
      break
    if trimmed.startsWith "name" then
      if let some val := extractTomlValue trimmed then
        return val
  none

/-- Extract all `lean_lib` names from `lakefile.toml`. -/
def extractLeanLibs (lines : Array String) : Array String := Id.run do
  let mut libs : Array String := #[]
  let mut inLeanLib := false
  for line in lines do
    let trimmed := line.trimAscii.toString
    if trimmed.startsWith "[[" then
      inLeanLib := trimmed.startsWith "[[lean_lib]]"
    if inLeanLib && trimmed.startsWith "name" then
      if let some val := extractTomlValue trimmed then
        libs := libs.push val
  libs

-- ============================================================================
-- File Scaffolding
-- ============================================================================

/-- Write a file if it doesn't exist (or if force=true). Creates parent dirs.
    Returns true if written. -/
def scaffoldFile (path : System.FilePath) (content : String)
    (force dryRun : Bool) : IO Bool := do
  let fileExists ← path.pathExists
  if fileExists && !force then
    return false
  if dryRun then
    return true
  -- Create parent directories
  if let some parent := path.parent then
    IO.FS.createDirAll parent
  IO.FS.writeFile path content
  return true

-- ============================================================================
-- Asset Path Discovery
-- ============================================================================

/-- Search for `dress-blueprint-action/assets` by walking up from the project
    directory. Returns a relative path like `../../toolchain/dress-blueprint-action/assets`
    or falls back to `../dress-blueprint-action/assets` if not found. -/
partial def findAssetsDir (projectDir : System.FilePath) : IO String := do
  -- Resolve to absolute path for reliable parent traversal
  let absDir ← IO.Process.run { cmd := "pwd", cwd := projectDir }
  let absDir := absDir.trimAscii.toString
  -- Walk up to 5 levels looking for dress-blueprint-action/assets
  let mut current := System.FilePath.mk absDir
  let mut depth : Nat := 0
  while depth < 5 do
    -- Check common locations relative to current directory
    for candidate in #["toolchain/dress-blueprint-action/assets",
                       "dress-blueprint-action/assets"] do
      let full := current / candidate
      if ← full.pathExists then
        -- Build relative path: "../" * (depth) + candidate
        let dots := String.join (List.replicate depth "../")
        return dots ++ candidate
    current := current / ".."
    depth := depth + 1
  -- Fallback for standalone repos (CI overrides this anyway)
  return "../dress-blueprint-action/assets"

-- ============================================================================
-- Directory Walking
-- ============================================================================

/-- Recursively collect all `.lean` files under a directory. -/
partial def collectLeanFiles (dir : System.FilePath) : IO (Array System.FilePath) := do
  let mut result : Array System.FilePath := #[]
  if !(← dir.pathExists) then return result
  let entries ← dir.readDir
  for entry in entries do
    let info ← entry.path.metadata
    if info.type == .dir then
      let sub ← collectLeanFiles entry.path
      result := result ++ sub
    else if entry.fileName.endsWith ".lean" then
      result := result.push entry.path
  return result

-- ============================================================================
-- Git Remote Detection
-- ============================================================================

/-- Detect the GitHub URL from `git remote get-url origin`.
    Normalizes SSH URLs (git@github.com:owner/repo.git) to HTTPS.
    Returns none if git fails or the URL doesn't look like GitHub. -/
def detectGitHubUrl (projectDir : System.FilePath) : IO (Option String) := do
  let result ← IO.Process.output {
    cmd := "git", args := #["remote", "get-url", "origin"], cwd := projectDir
  }
  if result.exitCode != 0 then return none
  let url := result.stdout.trimAscii.toString
  -- Normalize git@github.com:owner/repo.git → https://github.com/owner/repo
  let httpsUrl := if url.startsWith "git@github.com:" then
    "https://github.com/" ++ (url.dropPrefix "git@github.com:").toString
  else
    url
  -- Strip trailing .git
  let clean := if httpsUrl.endsWith ".git" then
    (httpsUrl.dropSuffix ".git").toString
  else httpsUrl
  -- Only return if it looks like a GitHub URL
  if clean.startsWith "https://github.com/" then return some clean
  return none

-- ============================================================================
-- Template Generation
-- ============================================================================

/-- Generate `runway.json` content. -/
def mkRunwayJson (projectName title githubUrl baseUrl assetsDir : String) : String :=
  "{\n" ++
  "  \"title\": \"" ++ title ++ "\",\n" ++
  "  \"projectName\": \"" ++ projectName ++ "\",\n" ++
  "  \"githubUrl\": \"" ++ githubUrl ++ "\",\n" ++
  "  \"baseUrl\": \"" ++ baseUrl ++ "\",\n" ++
  "  \"docgen4Url\": null,\n" ++
  "  \"runwayDir\": \"runway\",\n" ++
  "  \"assetsDir\": \"" ++ assetsDir ++ "\"\n" ++
  "}\n"

/-- Generate CI workflow YAML content. -/
def mkCIWorkflow : String :=
  "name: Blueprint\n" ++
  "\n" ++
  "on:\n" ++
  "  push:\n" ++
  "    branches: [main]\n" ++
  "  workflow_dispatch:\n" ++
  "\n" ++
  "permissions:\n" ++
  "  contents: read\n" ++
  "  pages: write\n" ++
  "  id-token: write\n" ++
  "\n" ++
  "concurrency:\n" ++
  "  group: \"pages\"\n" ++
  "  cancel-in-progress: false\n" ++
  "\n" ++
  "jobs:\n" ++
  "  build:\n" ++
  "    runs-on: ubuntu-latest\n" ++
  "    steps:\n" ++
  "      - uses: actions/checkout@v4\n" ++
  "      - uses: e-vergo/dress-blueprint-action@main\n" ++
  "\n" ++
  "  deploy:\n" ++
  "    needs: build\n" ++
  "    runs-on: ubuntu-latest\n" ++
  "    environment:\n" ++
  "      name: github-pages\n" ++
  "      url: ${{ steps.deployment.outputs.page_url }}\n" ++
  "    steps:\n" ++
  "      - name: Deploy to GitHub Pages\n" ++
  "        id: deployment\n" ++
  "        uses: actions/deploy-pages@v4\n"

/-- A chapter entry: directory name (for chapter title) and module names within it. -/
structure ChapterEntry where
  name : String
  modules : Array String
  deriving Repr

/-- Scan the library directory and build chapter entries from its structure.
    Each subdirectory becomes a chapter; top-level .lean files go into "Main Results". -/
partial def scanChapters (libDir : System.FilePath) (libName : String) :
    IO (Array ChapterEntry) := do
  if !(← libDir.pathExists) then return #[]
  let entries ← libDir.readDir
  let mut chapters : Array ChapterEntry := #[]
  let mut topLevelModules : Array String := #[]
  -- Sort entries for deterministic output
  let sorted := entries.toList.toArray.qsort (·.fileName < ·.fileName)
  for entry in sorted do
    let info ← entry.path.metadata
    if info.type == .dir then
      -- Subdirectory → chapter with all .lean files inside
      let files ← collectLeanFiles entry.path
      let moduleNames : Array String := files.map fun f =>
        let rel := (f.toString.dropPrefix (libDir.toString ++ "/")).toString
        libName ++ "." ++ ((rel.dropSuffix ".lean").toString.replace "/" ".")
      let sortedModules := moduleNames.qsort (· < ·)
      if sortedModules.size > 0 then
        chapters := chapters.push { name := entry.fileName, modules := sortedModules }
    else if entry.fileName.endsWith ".lean" then
      -- Top-level file → collect for "Main Results"
      let moduleName := libName ++ "." ++ (entry.fileName.dropSuffix ".lean").toString
      topLevelModules := topLevelModules.push moduleName
  -- Add top-level modules as final chapter
  if topLevelModules.size > 0 then
    let sortedTop := topLevelModules.qsort (· < ·)
    chapters := chapters.push { name := "Main Results", modules := sortedTop }
  return chapters

/-- Generate `blueprint.tex` content with chapters mirroring repo structure. -/
def mkBlueprintTex (title libName : String) (chapters : Array ChapterEntry) : String :=
  let header :=
    "\\documentclass{article}\n" ++
    "\n" ++
    "\\input{../../.lake/build/dressed/library/" ++ libName ++ ".tex}\n" ++
    "\n" ++
    "\\usepackage{amsmath, amsthm, amssymb}\n" ++
    "\\usepackage{hyperref}\n" ++
    "\n" ++
    "\\theoremstyle{definition}\n" ++
    "\\newtheorem{definition}{Definition}[section]\n" ++
    "\\newtheorem{axiom}{Axiom}[section]\n" ++
    "\\newtheorem{theorem}{Theorem}[section]\n" ++
    "\\newtheorem{lemma}{Lemma}[section]\n" ++
    "\\newtheorem{corollary}{Corollary}[section]\n" ++
    "\n" ++
    "\\title{" ++ title ++ " Blueprint}\n" ++
    "\\author{}\n" ++
    "\n" ++
    "\\begin{document}\n" ++
    "\\maketitle\n"
  let chapterContent := chapters.foldl (init := "") fun acc ch =>
    acc ++ "\n\\chapter{" ++ ch.name ++ "}\n" ++
    ch.modules.foldl (init := "") fun acc2 m =>
      acc2 ++ "\\inputleanmodule{" ++ m ++ "}\n"
  let footer := "\n\\end{document}\n"
  header ++ chapterContent ++ footer

-- ============================================================================
-- Import Injection
-- ============================================================================

/-- Declaration keywords that indicate a file has meaningful declarations. -/
private def declKeywords : Array String :=
  #["theorem ", "def ", "lemma ", "structure ", "class ", "inductive ", "abbrev ", "axiom "]

/-- Check if a string contains a substring. -/
private def containsSubstr (haystack needle : String) : Bool :=
  (haystack.splitOn needle).length > 1

/-- Check if a file's content contains any declaration keywords. -/
private def hasDeclarations (content : String) : Bool :=
  declKeywords.any fun kw => containsSubstr content kw

/-- Check if a file already imports Dress. -/
private def hasDressImport (content : String) : Bool :=
  let lines := content.splitOn "\n"
  lines.any fun line =>
    let trimmed : String := line.trimAscii.toString
    trimmed == "import Dress" || trimmed.startsWith "import Dress."

/-- Find the line index after the last `import` statement.
    Returns 0 if no imports found. -/
private def findImportInsertionPoint (lines : Array String) : Nat := Id.run do
  let mut lastImportIdx : Option Nat := none
  for i in [:lines.size] do
    let trimmed : String := lines[i]!.trimAscii.toString
    if trimmed.startsWith "import " then
      lastImportIdx := some i
  match lastImportIdx with
  | some idx => idx + 1
  | none => 0

/-- Inject `import Dress` into a single file if it has declarations and doesn't
    already import Dress. Returns true if the file was modified. -/
def injectImport (path : System.FilePath) (dryRun : Bool) : IO Bool := do
  let content ← IO.FS.readFile path
  if hasDressImport content then
    return false
  if !hasDeclarations content then
    return false
  if dryRun then
    return true
  let lines := content.splitOn "\n" |>.toArray
  let insertIdx := findImportInsertionPoint lines
  let before := lines[:insertIdx].toArray
  let after := lines[insertIdx:].toArray
  let newLines := before ++ #["import Dress"] ++ after
  let output := String.intercalate "\n" newLines.toList
  IO.FS.writeFile path output
  return true

-- ============================================================================
-- Main Entry Point
-- ============================================================================

/-- Run the quickstart scaffolding process. -/
def runQuickstart (projectDir : System.FilePath) (githubUrl title baseUrl : Option String)
    (force dryRun : Bool) : IO UInt32 := do
  let lakefilePath := projectDir / "lakefile.toml"

  -- Verify lakefile.toml exists
  if !(← lakefilePath.pathExists) then
    IO.eprintln s!"Error: {lakefilePath} not found. Run this from a Lean project directory."
    return 1

  -- Parse lakefile.toml
  let lakefileContent ← IO.FS.readFile lakefilePath
  let lakefileLines := lakefileContent.splitOn "\n" |>.toArray

  -- Extract project name
  let some projectName := extractProjectName lakefileLines
    | IO.eprintln "Error: Could not find project name in lakefile.toml."
      return 1

  -- Extract lean_lib names
  let libs := extractLeanLibs lakefileLines
  let libName := libs[0]?.getD projectName

  -- Resolve optional parameters
  let title := title.getD projectName
  let detectedUrl ← detectGitHubUrl projectDir
  let githubUrl := githubUrl.orElse (fun _ => detectedUrl)
    |>.getD ("https://github.com/OWNER/" ++ projectName)
  let baseUrl := baseUrl.getD ("/" ++ projectName ++ "/")

  IO.println ("quickstart: Set up " ++ projectName ++ " as an SBS blueprint project")
  if dryRun then
    IO.println "(dry run -- no files will be written)"
  IO.println ""

  -- Discover assets path
  let assetsDir ← findAssetsDir projectDir

  -- Track created files for summary
  let mut created : Array String := #[]

  -- ========== Scaffold files ==========

  -- runway.json
  let runwayPath := projectDir / "runway.json"
  if ← scaffoldFile runwayPath (mkRunwayJson projectName title githubUrl baseUrl assetsDir) force dryRun then
    created := created.push "runway.json"

  -- .github/workflows/blueprint.yml
  let ciPath := projectDir / ".github" / "workflows" / "blueprint.yml"
  if ← scaffoldFile ciPath mkCIWorkflow force dryRun then
    created := created.push ".github/workflows/blueprint.yml"

  -- runway/src/blueprint.tex (auto-generated chapters from repo structure)
  let chapters ← scanChapters (projectDir / libName) libName
  let blueprintTexPath := projectDir / "runway" / "src" / "blueprint.tex"
  if ← scaffoldFile blueprintTexPath (mkBlueprintTex title libName chapters) force dryRun then
    created := created.push "runway/src/blueprint.tex"

  -- ========== Inject imports ==========

  let libDir := projectDir / libName
  let leanFiles ← collectLeanFiles libDir
  let mut injectedCount : Nat := 0
  let mut alreadyCount : Nat := 0

  for file in leanFiles do
    let content ← IO.FS.readFile file
    if hasDressImport content then
      alreadyCount := alreadyCount + 1
    else if hasDeclarations content then
      if ← injectImport file dryRun then
        injectedCount := injectedCount + 1

  -- ========== Print summary ==========

  IO.println ""
  if created.size > 0 then
    IO.println "Created:"
    for f in created do
      IO.println ("  " ++ f)
    IO.println ""

  if injectedCount > 0 || alreadyCount > 0 then
    IO.println "Imports:"
    IO.println ("  Added `import Dress` to " ++ toString injectedCount ++
      " files (" ++ toString alreadyCount ++ " already had it)")
    IO.println ""

  if created.isEmpty && injectedCount == 0 then
    IO.println "Nothing to do -- project already appears to be set up."
    IO.println ""

  IO.println "Next steps:"
  IO.println "  1. lake build"
  IO.println "  2. Optionally annotate declarations with @[blueprint] for enriched metadata"
  IO.println ("  3. lake exe extract_blueprint graph " ++ libName)
  IO.println "  4. git push (triggers CI to build and deploy site)"

  return 0

end Dress.Quickstart
