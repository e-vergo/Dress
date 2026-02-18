/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Graph.Build

/-!
# Auto-Tag: Automatic `@[blueprint]` Attribute Insertion

This module provides functionality to automatically add `@[blueprint]` attributes
to uncovered declarations in a Lean project. It uses `Graph.computeCoverage` to
discover which declarations lack annotations, then computes insertion points
using `findDeclarationRanges?` and writes the attributes into the source files.
-/

namespace Dress.AutoTag

open Lean

/-- A pending modification to add `@[blueprint ...]` to a source file.
    Can either insert new lines or replace an existing attribute line. -/
structure TagInsertion where
  /-- Path to the source file to modify -/
  filePath : System.FilePath
  /-- 0-indexed line number: insert BEFORE this line, or replace this line -/
  insertLine : Nat
  /-- The lines to insert or the replacement line -/
  textLines : Array String
  /-- Fully qualified declaration name (for reporting) -/
  declName : String
  /-- Declaration kind: "theorem", "def", etc. -/
  kind : String
  /-- Module the declaration belongs to -/
  moduleName : String
  /-- If true, replace the line at `insertLine` instead of inserting before it -/
  replaceMode : Bool := false
  /-- 0-indexed keyword line (theorem/def/etc.) used for dedup.
      Two declarations resolving to the same keyword line (e.g., a Prop-valued class
      and its auto-generated instance) should produce only one insertion. -/
  keywordLine : Nat := 0
  deriving Repr

/-- Determine the `@[blueprint ...]` attribute text based on declaration kind.
    Theorems and lemmas get `statement` and `proof` placeholders.
    Everything else gets a bare `@[blueprint]`.
    Returns an array of lines (without indentation — caller adds it). -/
def mkAttributeLines (kind : String) : Array String :=
  if kind == "theorem" || kind == "lemma" then
    #["@[blueprint", "  (statement := /--  -/)", "  (proof := /--  -/)]"]
  else
    #["@[blueprint]"]

/-- Declaration keyword tokens that start a declaration line.
    Used to find the actual keyword line when walking forward from
    the declaration range start. -/
private def declKeywords : Array String :=
  #["theorem", "def", "lemma", "structure", "class", "instance",
    "abbrev", "inductive", "opaque", "axiom"]

/-- Modifier prefixes that may appear before a declaration keyword. -/
private def modifierPrefixes : Array String :=
  #["noncomputable", "private", "protected", "unsafe", "partial", "scoped"]

/-- Strip modifier prefixes from the start of a string, returning the remainder.
    Recurses to handle multiple prefixes (e.g., "noncomputable private theorem"). -/
private partial def stripPrefixes (s : String) : String :=
  let found := modifierPrefixes.find? fun pfx =>
    s.startsWith pfx &&
    (s.length == pfx.length || (s.drop pfx.length).startsWith " ")
  match found with
  | some pfx =>
    let rest := (s.drop pfx.length).trimAsciiStart.toString
    stripPrefixes rest
  | none => s

/-- Strip leading `@[...]` attribute blocks from a string.
    Handles `@[simp] lemma foo` → `lemma foo`. -/
private partial def stripInlineAttrs (s : String) : String :=
  let trimmed := s.trimAsciiStart.toString
  if !trimmed.startsWith "@[" then trimmed
  else
    -- Find the closing `]` and strip the attribute block
    let parts := trimmed.splitOn "]"
    if parts.length >= 2 then
      let rest := (String.intercalate "]" parts.tail!).trimAsciiStart.toString
      stripInlineAttrs rest
    else
      trimmed

/-- Check if a line contains a declaration keyword (possibly after modifier prefixes
    and/or inline attributes like `@[simp]`).
    Returns true for lines like `theorem foo`, `@[simp] lemma foo`,
    `noncomputable @[simp] def bar`, etc. -/
private def isKeywordLine (line : String) : Bool :=
  let trimmed := line.trimAsciiStart.toString
  -- Try direct keyword match
  if declKeywords.any (fun kw => trimmed.startsWith kw) then true
  else
    -- Strip modifier prefixes and inline attributes, then check for keyword
    let stripped := stripPrefixes (stripInlineAttrs (stripPrefixes trimmed))
    declKeywords.any (fun kw => stripped.startsWith kw)

/-- Keywords that `@[blueprint]` can validly decorate.
    Excludes `notation`/`macro` (not declaration-level). -/
private def taggableKeywords : Array String :=
  #["theorem", "def", "lemma", "structure", "class",
    "abbrev", "inductive", "axiom", "instance", "opaque"]

/-- Extract the declaration keyword from a source line, stripping prefixes and inline attributes.
    Returns `none` if no known keyword is found. -/
private def getKeywordFromLine (line : String) : Option String :=
  let stripped := stripPrefixes (stripInlineAttrs (stripPrefixes (line.trimAsciiStart.toString)))
  declKeywords.find? (fun kw => stripped.startsWith kw)

/-- Check if a line is an attribute line (starts with `@[` after optional whitespace). -/
private def isAttributeLine (line : String) : Bool :=
  line.trimAsciiStart.toString.startsWith "@["

/-- Get the leading whitespace from a line by counting space/tab characters. -/
private def getIndentation (line : String) : String :=
  let chars := line.toList
  let wsCount := chars.takeWhile (fun c => c == ' ' || c == '\t') |>.length
  (line.take wsCount).toString

/-- Check if a line already contains an existing `@[blueprint` attribute. -/
private def hasBlueprintAttr (line : String) : Bool :=
  (line.splitOn "@[blueprint").length > 1

/-- Inject `blueprint` into an existing attribute line by replacing `@[` with `@[blueprint, `.
    For example: `@[simp]` → `@[blueprint, simp]`.
    Preserves leading whitespace. -/
private def injectBlueprintIntoAttrLine (line : String) : String :=
  let indent := getIndentation line
  let trimmed := line.trimAsciiStart.toString
  -- Replace the first `@[` with `@[blueprint, `
  if trimmed.startsWith "@[" then
    indent ++ "@[blueprint, " ++ (trimmed.drop 2)
  else
    line

/-- Find the insertion point for a declaration's `@[blueprint]` attribute.

    Algorithm:
    1. Get `DeclarationRanges` for the declaration
    2. Use `selectionRange.pos.line` (the declaration NAME line, not the full block)
    3. Walk backward from the name line to find the keyword line
    4. Walk further backward past existing attribute lines
    5. Guard: skip if an existing `@[blueprint]` is found nearby
    6. If existing attributes found: inject `blueprint` into the closest one
       (avoids @[blueprint] parser conflict with separate @[...] blocks)
    7. If no existing attributes: insert new @[blueprint] line(s) -/
def resolveInsertion (env : Environment) (name : Name) (kind : String) (moduleName : String)
    : CoreM (Option TagInsertion) := do
  -- Get declaration ranges
  let some ranges ← findDeclarationRanges? name
    | IO.eprintln s!"  Warning: No declaration ranges for {name}, skipping"
      return none

  -- Resolve module name to file path
  let module := match env.getModuleIdxFor? name with
    | some modIdx => env.allImportedModuleNames[modIdx]!
    | none => env.header.mainModule
  let some filePath ← (← getSrcSearchPath).findWithExt "lean" module
    | IO.eprintln s!"  Warning: Cannot resolve source file for module {module}, skipping {name}"
      return none

  -- Read the source file
  let content ← IO.FS.readFile filePath
  let lines := content.splitOn "\n" |>.toArray

  -- Use selectionRange (points to the declaration NAME, not the whole block)
  -- This gives us a unique line per declaration, unlike range.pos which can
  -- point to a shared section/namespace start.
  let nameLine := ranges.selectionRange.pos.line - 1  -- 1-based → 0-based

  if nameLine >= lines.size then
    IO.eprintln s!"  Warning: Declaration {name} selectionRange line {ranges.selectionRange.pos.line} exceeds file length {lines.size}, skipping"
    return none

  -- Walk backward from the name line to find the keyword line.
  -- The keyword is either on the same line as the name (e.g., "theorem foo")
  -- or on a nearby preceding line (e.g., after a line break in the signature).
  let mut keywordLine := nameLine
  -- First check if the name line itself is a keyword line
  if !isKeywordLine lines[nameLine]! then
    -- Walk backward (up to 5 lines) looking for the keyword
    for i in List.range (min 5 nameLine) do
      let checkIdx := nameLine - i - 1
      if isKeywordLine lines[checkIdx]! then
        keywordLine := checkIdx
        break

  -- Validate: the keyword on this line must be taggable (not instance, notation, etc.)
  -- This is a source-level guard that catches cases where the environment-level kind
  -- detection missed (e.g., instances not in instData, notation declarations).
  let actualKeyword := getKeywordFromLine lines[keywordLine]!
  match actualKeyword with
  | some kw =>
    unless taggableKeywords.contains kw do
      return none  -- Not a taggable keyword (e.g., instance, opaque)
  | none =>
    return none  -- Can't identify keyword — skip to be safe

  -- Use the actual source keyword rather than the environment-level kind.
  -- E.g., an `instance` may be classified as "theorem" in the environment,
  -- but we must generate the attribute form matching the source keyword.
  let actualKind := actualKeyword.getD kind

  -- Use DeclarationRanges.range.pos.line to find the full declaration start,
  -- which includes modifiers (noncomputable, private, etc.) and attributes
  -- (@[simp], @[to_additive /-- doc -/], etc.). This replaces the backward walk
  -- which broke on multi-line attribute blocks and didn't walk past modifiers.
  let rangeStart := ranges.range.pos.line - 1  -- 1-based → 0-based
  let rangeStart := if rangeStart < keywordLine then rangeStart else keywordLine

  -- Pre-scan: check for existing @[blueprint] in the declaration range
  for idx in [rangeStart:keywordLine + 1] do
    if hasBlueprintAttr lines[idx]! then
      return none

  -- Scan forward from rangeStart to keywordLine to find attribute lines for injection
  let mut closestAttrLine : Option Nat := none
  for idx in [rangeStart:keywordLine] do
    if isAttributeLine lines[idx]! then
      closestAttrLine := some idx  -- Last found = closest to keyword

  -- Fallback: backward scan from keyword when forward scan found nothing.
  -- Handles cases where range.pos.line doesn't reach multi-line attribute blocks
  -- (e.g., @[to_additive\n/-- doc -/] where range starts at the keyword line).
  if closestAttrLine.isNone then
    let mut checkIdx := keywordLine
    while checkIdx > 0 do
      checkIdx := checkIdx - 1
      let trimmed := lines[checkIdx]!.trimAsciiStart.toString
      if isAttributeLine lines[checkIdx]! then
        closestAttrLine := some checkIdx
        break
      -- Stop scanning at blank lines
      if trimmed == "" then break
      -- Continue past doc comments (/-- ... -/), closing brackets (]-only lines),
      -- attribute content lines (inside multi-line @[...]), and attribute parameter
      -- lines like `(statement := /--  -/)` inside multi-line @[blueprint ...] blocks
      unless trimmed.startsWith "/--" || trimmed.startsWith "--" ||
             trimmed.endsWith "-/" || trimmed.endsWith "-/]" ||
             trimmed.startsWith "#" || trimmed.startsWith "(" do
        break
    -- Also check backward-found attr range for existing @[blueprint]
    if let some attrLine := closestAttrLine then
      if attrLine < rangeStart then
        for idx in [attrLine:rangeStart] do
          if hasBlueprintAttr lines[idx]! then
            return none

  -- Determine indentation from the keyword line
  let indent := getIndentation lines[keywordLine]!

  -- Check for inline attributes on the keyword line itself (e.g., `@[simp] lemma foo`)
  let keywordLineContent := lines[keywordLine]!.trimAsciiStart.toString
  let hasInlineAttr := keywordLineContent.startsWith "@[" && closestAttrLine.isNone

  -- If existing attributes found (either above or inline), inject `blueprint` into them.
  -- This avoids the @[blueprint] parser conflict where separate @[blueprint]
  -- and @[simp] blocks cause "expected 'lemma'" errors.
  if hasInlineAttr then
    -- Inline attribute on keyword line: inject blueprint into it
    let modifiedLine := injectBlueprintIntoAttrLine lines[keywordLine]!
    return some {
      filePath
      insertLine := keywordLine
      textLines := #[modifiedLine]
      declName := name.toString
      kind := actualKind
      moduleName
      replaceMode := true
      keywordLine
    }
  if let some attrLine := closestAttrLine then
    let modifiedLine := injectBlueprintIntoAttrLine lines[attrLine]!
    return some {
      filePath
      insertLine := attrLine
      textLines := #[modifiedLine]
      declName := name.toString
      kind := actualKind
      moduleName
      replaceMode := true
      keywordLine
    }

  -- No existing attributes: insert new @[blueprint] line(s)
  -- Skip past any doc comment at rangeStart — Lean 4 requires doc comments
  -- BEFORE attributes, so inserting @[blueprint] before a /-- doc -/ is invalid.
  let mut insertPoint := rangeStart
  if insertPoint < keywordLine then
    let trimmedStart := lines[insertPoint]!.trimAsciiStart.toString
    if trimmedStart.startsWith "/--" then
      -- Multi-line doc comment: scan forward to find the closing -/
      while insertPoint < keywordLine do
        if (lines[insertPoint]!.splitOn "-/").length > 1 then
          insertPoint := insertPoint + 1  -- move past the closing line
          break
        insertPoint := insertPoint + 1
  let attrLines := mkAttributeLines actualKind |>.map (indent ++ ·)
  return some {
    filePath
    insertLine := insertPoint
    textLines := attrLines
    declName := name.toString
    kind := actualKind
    moduleName
    keywordLine
  }

/-- Apply a batch of insertions to a single file. Insertions must all target
    the same file and are applied bottom-up (highest line number first) so
    that earlier insertions don't invalidate later line numbers. -/
def applyInsertions (path : System.FilePath) (insertions : Array TagInsertion) : IO Unit := do
  let content ← IO.FS.readFile path
  let mut lines := content.splitOn "\n" |>.toArray

  -- Sort by line number descending (bottom-up)
  let sorted := insertions.qsort (fun a b => a.insertLine > b.insertLine)

  for ins in sorted do
    if ins.insertLine < lines.size then
      if ins.replaceMode then
        -- Replace the line at insertLine with the first textLine
        if let some replacement := ins.textLines[0]? then
          lines := lines.set! ins.insertLine replacement
      else
        -- Insert new lines before insertLine
        let before := lines[:ins.insertLine].toArray
        let after := lines[ins.insertLine:].toArray
        lines := before ++ ins.textLines ++ after

  -- Write back, joining with newlines
  let output := String.intercalate "\n" lines.toList
  IO.FS.writeFile path output

/-- Compute the set of module names that transitively import Dress or LeanArchitect,
    and therefore have access to `@[blueprint]`. Uses BFS over the reverse dependency graph. -/
private def blueprintCapableModules (env : Environment) : Std.HashMap String Unit := Id.run do
  let moduleNames := env.header.moduleNames
  let moduleData := env.header.moduleData
  -- Build reverse dependency map: imported module → modules that import it
  let mut reverseDeps : Std.HashMap String (Array String) := {}
  for idx in [:moduleData.size] do
    let modName := moduleNames[idx]!.toString
    for imp in moduleData[idx]!.imports do
      let impName := imp.module.toString
      let existing := reverseDeps.getD impName #[]
      reverseDeps := reverseDeps.insert impName (existing.push modName)
  -- Seed: modules providing @[blueprint]
  let mut capable : Std.HashMap String Unit := {}
  let mut queue : Array String := #[]
  for idx in [:moduleNames.size] do
    let name := moduleNames[idx]!.toString
    if name.startsWith "Dress" || name.startsWith "LeanArchitect" || name.startsWith "Architect" then
      capable := capable.insert name ()
      queue := queue.push name
  -- BFS: any module importing a capable module becomes capable
  let mut qi : Nat := 0
  while qi < queue.size do
    let current := queue[qi]!
    qi := qi + 1
    if let some deps := reverseDeps[current]? then
      for dep in deps do
        unless capable.contains dep do
          capable := capable.insert dep ()
          queue := queue.push dep
  capable

/-- Main entry point: discover uncovered declarations and compute insertions.

    Uses `Graph.computeCoverage` to find all declarations lacking `@[blueprint]`
    annotations, then resolves insertion points for each one.

    When `dryRun` is true, no files are modified -- only the insertion plan is returned. -/
def runAutoTag (env : Environment) (modules : Array Name) (dryRun : Bool)
    : CoreM (Array TagInsertion) := do
  -- Use existing coverage computation to find uncovered declarations
  let coverage ← Graph.computeCoverage env modules

  -- Compute which modules transitively import Dress/LeanArchitect.
  -- Declarations from non-capable modules (e.g., Mathlib patches) are skipped
  -- since @[blueprint] would be an unknown attribute in those files.
  let capableModules := blueprintCapableModules env

  IO.eprintln s!"  Found {coverage.uncovered.size} uncovered declarations (of {coverage.totalDeclarations} total, {coverage.coveredDeclarations} already covered)"

  -- Resolve insertion points for each uncovered declaration
  let mut insertions : Array TagInsertion := #[]
  for uncov in coverage.uncovered do
    -- Skip non-taggable declaration kinds at the environment level.
    -- other: notation, macro, and other non-standard declarations
    if uncov.kind == "other" then continue
    -- Skip declarations from modules that don't transitively import Dress/LeanArchitect.
    -- These files (e.g., Mathlib patches) don't have @[blueprint] in scope.
    unless capableModules.contains uncov.moduleName do continue
    let name := uncov.name.toName
    if let some ins ← resolveInsertion env name uncov.kind uncov.moduleName then
      insertions := insertions.push ins

  IO.eprintln s!"  Resolved {insertions.size} insertion points"

  -- Deduplicate insertions that target the same (file, keywordLine).
  -- Auto-generated declarations (from @[simps], @[to_additive], structure projections,
  -- Prop-valued class instances, etc.) may resolve to the same source declaration.
  -- Only one @[blueprint] should be emitted per declaration keyword line.
  -- We key on keywordLine (not insertLine) because two declarations sharing the same
  -- keyword line may have different insertLine values (e.g., a Prop-valued class and
  -- its auto-generated instance have different range.pos.line values).
  let mut deduped : Std.HashMap (String × Nat) TagInsertion := {}
  let mut skippedCount : Nat := 0
  for ins in insertions do
    let key := (ins.filePath.toString, ins.keywordLine)
    match deduped[key]? with
    | some _ =>
      skippedCount := skippedCount + 1
      -- Keep first seen; all insertions at same source line share the same keyword
    | none =>
      deduped := deduped.insert key ins
  insertions := deduped.values.toArray

  if skippedCount > 0 then
    IO.eprintln s!"  Deduplicated {skippedCount} insertions sharing same source line ({insertions.size} unique)"

  -- If not a dry run, apply the insertions grouped by file
  unless dryRun do
    -- Group insertions by file path
    let mut fileGroups : Std.HashMap String (Array TagInsertion) := {}
    for ins in insertions do
      let key := ins.filePath.toString
      let group := fileGroups.getD key #[]
      fileGroups := fileGroups.insert key (group.push ins)

    -- Apply insertions per file
    for (_, group) in fileGroups do
      if let some first := group[0]? then
        applyInsertions first.filePath group
        IO.eprintln s!"  Modified {first.filePath} ({group.size} insertions)"

  return insertions

end Dress.AutoTag
