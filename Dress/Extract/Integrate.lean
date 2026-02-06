/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Architect.Basic
import Dress.Extract.Delimiter

/-!
# Delimiter-to-Declaration Integration

Matches extracted delimiter blocks to `@[blueprint]` declarations by source position,
and provides enrichment functions to populate `NodePart.text` from delimiter content.

## Matching Algorithm

For each declaration (identified by byte offset in source), the nearest preceding
`DelimiterBlock` is matched. "Nearest preceding" means the block with the highest
`pos` that is still strictly less than the declaration's byte offset.

## Statement Source Policy

The `enrichNodeWithDelimiter` function respects a `statementSource` config:

- `"attribute"`: Use only the existing `statement.text` from `@[blueprint]` (default behavior).
- `"delimiters"`: Prefer delimiter content; fall back to `statement.text` if no delimiter found.
- `"both"`: Same as `"delimiters"` -- prefer delimiter content with attribute fallback.
-/

open Lean

namespace Dress.Extract

/-- A declaration's identity and position in source, used for matching against delimiters. -/
structure DeclPosition where
  /-- Byte offset in the source file where the declaration starts. -/
  pos : Nat
  /-- The fully qualified Lean name of the declaration. -/
  name : Name
  deriving Repr, Inhabited

/-- Match delimiter blocks to declarations by nearest-preceding-position.

    For each declaration, finds the `DelimiterBlock` with the highest `pos`
    that is strictly less than the declaration's `pos`. If multiple declarations
    share the same nearest delimiter, each gets the same content (this is unusual
    but handled gracefully).

    Both inputs are assumed sorted by position (ascending). `extractDelimiters`
    already returns sorted results; callers should sort `decls` similarly.

    Returns a `NameMap String` mapping declaration names to their matched
    delimiter content. Declarations with no preceding delimiter are absent
    from the map. -/
def matchDelimitersToDeclarations
    (blocks : Array DelimiterBlock)
    (decls : Array DeclPosition)
    : NameMap String := Id.run do
  if blocks.isEmpty || decls.isEmpty then return {}
  let mut result : NameMap String := {}
  -- For each declaration, binary search for the nearest preceding block.
  -- Since both arrays are sorted by position, we can also use a scan pointer,
  -- but binary search is simpler and correct for any ordering of decls.
  for decl in decls do
    -- Find the block with highest pos < decl.pos
    let mut bestIdx : Option Nat := none
    -- Linear scan is fine for prototype; blocks array is small (typically <100).
    for i in [:blocks.size] do
      if blocks[i]!.pos < decl.pos then
        bestIdx := some i
      else
        -- blocks are sorted, so once we pass decl.pos we can stop
        break
    if let some idx := bestIdx then
      result := result.insert decl.name blocks[idx]!.content
  return result

/-- Enrich an `Architect.Node` with delimiter-extracted TeX content.

    Applies statement source policy:
    - `"attribute"`: Ignore delimiter content entirely. Return node unchanged.
    - `"delimiters"` or `"both"`: Use delimiter content if available (`some`),
      otherwise keep existing `statement.text`.

    Any other `statementSource` value is treated as `"attribute"` (no-op).

    This is a pure function with no side effects. -/
def enrichNodeWithDelimiter
    (node : Architect.Node)
    (delimiterContent : Option String)
    (statementSource : String := "attribute")
    : Architect.Node :=
  match statementSource with
  | "delimiters" | "both" =>
    match delimiterContent with
    | some content =>
      { node with statement := { node.statement with text := content } }
    | none => node
  | _ => node

/-- Batch-enrich an array of nodes using a delimiter match map.

    Convenience wrapper that calls `enrichNodeWithDelimiter` for each node,
    looking up delimiter content by `node.name` in the provided `NameMap`. -/
def enrichNodesWithDelimiters
    (nodes : Array Architect.Node)
    (delimiterMap : NameMap String)
    (statementSource : String := "attribute")
    : Array Architect.Node :=
  nodes.map fun node =>
    enrichNodeWithDelimiter node (delimiterMap.find? node.name) statementSource

end Dress.Extract
