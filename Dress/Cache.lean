/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Highlighting
import Architect.Basic
import Dress.Paths

/-!
# Per-Declaration Content Cache

This module provides caching for per-declaration artifacts to avoid redundant regeneration.
Each declaration's artifacts are cached based on a content hash derived from:
- Declaration fully qualified name
- `@[blueprint]` attribute fields (label, statement, proof, uses, etc.)
- Source file path + line range
- Highlighting data (if present)

## Cache Structure

```
.lake/build/dressed/.decl_cache/
└── {content_hash}/
    ├── decl.json
    ├── decl.tex
    ├── decl.html
    ├── decl.hovers.json
    └── manifest.entry
```

Cache hits copy from the hash directory to the target declaration directory,
avoiding expensive HTML rendering and file generation.
-/

open Lean
open SubVerso.Highlighting

namespace Dress.Cache

/-- Required artifact files for a complete cache entry -/
private def requiredFiles : List String := ["decl.tex", "decl.json", "manifest.entry"]

/-- Optional artifact files (may not exist for all declarations) -/
private def optionalFiles : List String := ["decl.html", "decl.hovers.json"]

/-- Compute a hash for declaration content used as cache key.
    Components: name, node JSON representation, file path, location range, highlighting JSON.
    Returns a 16-character hex string. -/
def computeDeclarationHash (name : Name) (node : Architect.Node)
    (highlighting : Option Highlighted) (file : Option System.FilePath)
    (location : Option DeclarationRange) : IO String := do
  -- Build canonical string representation for hashing
  let nodeJson := (Lean.toJson node).compress
  let highlightingStr := match highlighting with
    | some hl => (Lean.toJson hl).compress
    | none => "none"
  let fileStr := match file with
    | some f => f.toString
    | none => "none"
  let locationStr := match location with
    | some loc => s!"{loc.pos.line}:{loc.pos.column}-{loc.endPos.line}:{loc.endPos.column}"
    | none => "none"

  let combined := s!"{name}|{nodeJson}|{highlightingStr}|{fileStr}|{locationStr}"

  -- Use String.hash for a fast 64-bit hash, format as 16-char hex
  let hash := combined.hash
  return hexString hash
where
  /-- Convert UInt64 to 16-character hex string (iterative, no termination issues) -/
  hexString (n : UInt64) : String :=
    let hexDigits : Array Char := #['0', '1', '2', '3', '4', '5', '6', '7',
                                     '8', '9', 'a', 'b', 'c', 'd', 'e', 'f']
    -- Build array of 16 hex characters from least significant to most significant
    let chars := (List.range 16).foldl (init := (#[], n)) fun (acc, remaining) _ =>
      let digit := (remaining &&& 0xF).toNat
      let char := hexDigits[digit]!
      (acc.push char, remaining >>> 4)
    -- Reverse to get most significant first
    String.ofList chars.1.toList.reverse

/-- Get cache directory path for a given hash -/
def getCachePath (buildDir : System.FilePath) (hash : String) : System.FilePath :=
  buildDir / "dressed" / ".decl_cache" / hash

/-- Check if valid cache exists for this hash.
    Returns true only if all required files exist. -/
def checkCache (buildDir : System.FilePath) (hash : String) : IO Bool := do
  let cacheDir := getCachePath buildDir hash
  if !(← cacheDir.pathExists) then
    return false
  -- Check all required files exist
  for file in requiredFiles do
    if !(← (cacheDir / file).pathExists) then
      return false
  return true

/-- Copy a file if it exists, silently skip otherwise -/
private def copyIfExists (src : System.FilePath) (dst : System.FilePath) : IO Unit := do
  if ← src.pathExists then
    let contents ← IO.FS.readFile src
    IO.FS.writeFile dst contents

/-- Copy cached artifacts to target location -/
def restoreFromCache (buildDir : System.FilePath) (hash : String)
    (targetDir : System.FilePath) : IO Unit := do
  let cacheDir := getCachePath buildDir hash
  IO.FS.createDirAll targetDir
  -- Copy required files
  for file in requiredFiles do
    let src := cacheDir / file
    let dst := targetDir / file
    let contents ← IO.FS.readFile src
    IO.FS.writeFile dst contents
  -- Copy optional files if they exist
  for file in optionalFiles do
    copyIfExists (cacheDir / file) (targetDir / file)

/-- Save generated artifacts to cache -/
def saveToCache (buildDir : System.FilePath) (hash : String)
    (sourceDir : System.FilePath) : IO Unit := do
  let cacheDir := getCachePath buildDir hash
  IO.FS.createDirAll cacheDir
  -- Copy required files
  for file in requiredFiles do
    let src := sourceDir / file
    let dst := cacheDir / file
    if ← src.pathExists then
      let contents ← IO.FS.readFile src
      IO.FS.writeFile dst contents
  -- Copy optional files if they exist
  for file in optionalFiles do
    copyIfExists (sourceDir / file) (cacheDir / file)

end Dress.Cache
