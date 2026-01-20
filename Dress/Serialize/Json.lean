/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Highlighting
import SubVerso.Module

/-!
# JSON Serialization

This module provides JSON serialization functions for SubVerso highlighting data.

## Output Formats

- **SubVerso Module format**: Compatible with `subverso-extract-mod`, uses integer deduplication
- **Highlighted JSON**: Raw SubVerso `Highlighted` structure for individual declarations
-/

open Lean
open SubVerso.Highlighting
open SubVerso.Module

namespace Dress.Serialize

/-- Serialize a Highlighted value to JSON using SubVerso's deduplicated export format.
    This produces a compact JSON representation suitable for storage. -/
def highlightedToJson (hl : Highlighted) : Json :=
  hl.exportCode.toJson

/-- Serialize a NameMap of Highlighted values to JSON in SubVerso Module format.
    This format is compatible with `subverso-extract-mod` output. -/
def highlightingMapToModuleJson (highlighting : NameMap Highlighted) : Json :=
  let items : Array ModuleItem := highlighting.toList.foldl (init := #[]) fun acc (name, hl) =>
    acc.push {
      range := none  -- Range info not available at capture time
      kind := `blueprint
      defines := #[name]
      code := hl
    }
  let module : SubVerso.Module.Module := { items }
  module.toJson

/-- Write highlighted code to a JSON file atomically.
    Uses write-to-temp-then-rename for crash safety on POSIX systems. -/
def writeJsonAtomic (path : System.FilePath) (json : Json) : IO Unit := do
  -- Ensure parent directory exists
  if let some parent := path.parent then
    IO.FS.createDirAll parent

  -- Write to temp file first
  let tmpPath := path.withExtension "json.tmp"
  IO.FS.writeFile tmpPath json.compress

  -- Atomic rename (on POSIX systems)
  IO.FS.rename tmpPath path

/-- Load highlighting from a SubVerso Module format JSON file.
    Returns empty map if file doesn't exist or parsing fails. -/
def loadModuleJson (path : System.FilePath) : IO (NameMap Highlighted) := do
  if !(← path.pathExists) then
    return {}

  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return {}
  | .ok json =>
    match SubVerso.Module.Module.fromJson? json with
    | .error _ => return {}
    | .ok mod =>
      return mod.items.foldl (init := {}) fun acc item =>
        item.defines.foldl (init := acc) fun acc' name =>
          acc'.insert name item.code

end Dress.Serialize

-- Re-export at Dress namespace for backward compatibility
namespace Dress

abbrev serializeHighlightedToJson := Serialize.highlightedToJson
abbrev serializeHighlightingMapToJson := Serialize.highlightingMapToModuleJson
abbrev writeHighlightingJsonAtomic := Serialize.writeJsonAtomic
abbrev loadHighlightingFromJson := Serialize.loadModuleJson

end Dress
