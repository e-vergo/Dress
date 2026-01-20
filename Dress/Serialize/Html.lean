/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Highlighting
import Dress.HtmlRender
import Dress.Serialize.Json

/-!
# HTML Serialization

This module provides HTML serialization functions for SubVerso highlighting data.
It wraps the core rendering from HtmlRender.lean and adds file I/O.

## Output Format

Produces JSON maps of declaration name → HTML string for efficient loading.
-/

open Lean
open SubVerso.Highlighting

namespace Dress.Serialize

/-- Serialize a NameMap of Highlighted values to a JSON map of declaration name → HTML string.
    This format allows Output.lean to directly embed pre-rendered HTML. -/
def highlightingMapToHtmlJson (highlighting : NameMap Highlighted) : Json :=
  let entries : List (String × Json) := highlighting.toList.map fun (name, hl) =>
    (name.toString, Json.str (HtmlRender.renderHighlightedToHtml hl))
  Json.mkObj entries

/-- Write all captured module highlighting as HTML to a JSON map file. -/
def writeHighlightingHtml (path : System.FilePath) (highlighting : NameMap Highlighted) : IO Unit := do
  if highlighting.isEmpty then return
  writeJsonAtomic path (highlightingMapToHtmlJson highlighting)

/-- Load pre-rendered HTML highlighting from a JSON map file.
    Returns a NameMap of declaration name → HTML string.
    Returns empty map if file doesn't exist or parsing fails. -/
def loadHighlightingHtml (path : System.FilePath) : IO (NameMap String) := do
  if !(← path.pathExists) then
    return {}

  let contents ← IO.FS.readFile path
  match Json.parse contents with
  | .error _ => return {}
  | .ok json =>
    match json with
    | .obj kvs =>
      return kvs.toList.foldl (init := {}) fun acc (key, val) =>
        match val with
        | .str html => acc.insert key.toName html
        | _ => acc
    | _ => return {}

end Dress.Serialize

-- Re-export at Dress namespace for backward compatibility
namespace Dress

abbrev serializeHighlightingMapToHtmlJson := Serialize.highlightingMapToHtmlJson
abbrev loadHighlightingHtmlFromJson := Serialize.loadHighlightingHtml

end Dress
