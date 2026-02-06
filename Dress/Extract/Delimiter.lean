/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

/-!
# TeX Delimiter Extraction

Extracts TeX content from Lean source files using delimiter conventions
ported from leanblueprint-extract.

## Delimiter Patterns

Two patterns are recognized:

* **Block delimiter**: open with slash-dash-percent-percent, close with
  percent-percent-dash-slash. These wrap multi-line TeX blocks inside
  Lean block comments.

* **Line delimiter**: double-dash-percent-percent to end of line.
  Single-line TeX content inside Lean line comments.

## Algorithm

1. Scan for block delimiters, recording their byte ranges
2. Scan for line delimiters, excluding any that fall inside block ranges
3. Sort all results by source position
-/

namespace Dress.Extract

/-- A TeX fragment extracted from a Lean source file. -/
structure DelimiterBlock where
  /-- Byte position in source where the delimiter starts. -/
  pos : Nat
  /-- Extracted TeX content (without delimiters, trimmed). -/
  content : String
  /-- Whether this was a block (multi-line) or line (single-line) delimiter. -/
  isBlock : Bool
  deriving Repr, Inhabited

/-! ## String Scanning Primitives

Low-level helpers for substring matching using `String.Pos.Raw` (byte-level positions).
These avoid deprecated `String.get` and `String.next` in favor of `String.Pos.Raw` methods. -/

open String.Pos

/-- Check if `source` contains `pattern` starting exactly at byte position `pos`. -/
def matchesAt (source : String) (pattern : String) (pos : Raw) : Bool := Id.run do
  let sourceEnd := source.endPos.offset
  let mut sPos := pos
  let mut pPos : Raw := ⟨0⟩
  let patternEnd : Raw := ⟨pattern.utf8ByteSize⟩
  while pPos < patternEnd && sPos < sourceEnd do
    if Raw.get source sPos != Raw.get pattern pPos then return false
    sPos := Raw.next source sPos
    pPos := Raw.next pattern pPos
  return pPos >= patternEnd

/-- Find the first occurrence of `pattern` in `source` at or after byte position `start`.
    Returns the byte position of the match, or `none` if not found. -/
def findSubstring (source : String) (pattern : String)
    (start : Raw := ⟨0⟩) : Option Raw := Id.run do
  let sourceEnd := source.endPos.offset
  let mut pos := start
  while pos < sourceEnd do
    if matchesAt source pattern pos then return some pos
    pos := Raw.next source pos
  return none

/-- Find the byte position of the next newline at or after `start`, or end-of-string. -/
def findLineEnd (source : String) (start : Raw) : Raw := Id.run do
  let sourceEnd := source.endPos.offset
  let mut pos := start
  while pos < sourceEnd do
    if Raw.get source pos == '\n' then return pos
    pos := Raw.next source pos
  return sourceEnd

/-- Check if a byte position falls strictly inside any of the given (start, end) ranges. -/
def insideAnyRange (pos : Nat) (ranges : Array (Nat × Nat)) : Bool :=
  ranges.any fun (start, stop) => pos > start && pos < stop

/-! ## Main Extraction -/

/-- The 4-byte opening delimiter for block TeX content. -/
private def blockOpen : String := .ofList ['/', '-', '%', '%']

/-- The 4-byte closing delimiter for block TeX content. -/
private def blockClose : String := .ofList ['%', '%', '-', '/']

/-- The 4-byte line delimiter for single-line TeX content. -/
private def lineMarker : String := .ofList ['-', '-', '%', '%']

/-- Extract all delimiter blocks from Lean source text, sorted by position.

    Scans for both block delimiters and line delimiters.
    Line delimiters that fall inside a block delimiter range are excluded to prevent
    double-counting.

    Unclosed block delimiters (no matching close) are silently skipped. -/
def extractDelimiters (source : String) : Array DelimiterBlock := Id.run do
  let mut result : Array DelimiterBlock := #[]

  let blockOpenLen := blockOpen.utf8ByteSize
  let blockCloseLen := blockClose.utf8ByteSize
  let lineMarkerLen := lineMarker.utf8ByteSize

  -- Track block ranges so line markers inside blocks are excluded
  let mut blockRanges : Array (Nat × Nat) := #[]

  -- Pass 1: Block delimiters
  let mut searchPos : Raw := ⟨0⟩
  while true do
    let some openPos := findSubstring source blockOpen searchPos | break
    let contentStart : Raw := ⟨openPos.byteIdx + blockOpenLen⟩
    let some closePos := findSubstring source blockClose contentStart | break
    let closeEnd := closePos.byteIdx + blockCloseLen
    let content := (Raw.extract source contentStart closePos).trimAscii.toString
    result := result.push { pos := openPos.byteIdx, content, isBlock := true }
    blockRanges := blockRanges.push (openPos.byteIdx, closeEnd)
    searchPos := ⟨closeEnd⟩

  -- Pass 2: Line delimiters, excluding those inside block ranges
  searchPos := ⟨0⟩
  while true do
    let some markerPos := findSubstring source lineMarker searchPos | break
    let contentStart : Raw := ⟨markerPos.byteIdx + lineMarkerLen⟩
    let lineEnd := findLineEnd source contentStart
    if !insideAnyRange markerPos.byteIdx blockRanges then
      let content := (Raw.extract source contentStart lineEnd).trimAscii.toString
      result := result.push { pos := markerPos.byteIdx, content, isBlock := false }
    searchPos := lineEnd

  -- Sort by source position to preserve document order
  result.qsort fun a b => a.pos < b.pos

end Dress.Extract
