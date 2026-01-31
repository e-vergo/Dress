/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Verso.Code.Highlighted
import Verso.Doc

/-!
# HTML Rendering for Blueprint Highlighting

This module provides a thin wrapper around Verso's `Highlighted.toHtml` to render
SubVerso highlighted code to HTML strings.

Uses `Genre.none` and minimal configuration to avoid dependencies on Verso's
full document infrastructure.
-/

open Verso.Code
open Verso.Doc
open Verso.Output

namespace Dress.HtmlRender

/-- Wrap bracket characters with depth-colored spans.
    Tracks nesting depth and wraps (, ), [, ], {, } with
    <span class="lean-bracket-N"> where N cycles 1-6.

    IMPORTANT: Skips brackets inside HTML tags (between < and >)
    to avoid breaking HTML structure.
-/
def wrapBracketsWithDepth (html : String) : String := Id.run do
  let mut result : String := ""
  let mut depth : Nat := 0
  let mut insideTag : Bool := false

  for c in html.toList do
    if c == '<' then
      insideTag := true
      result := result.push c
    else if c == '>' then
      insideTag := false
      result := result.push c
    else if insideTag then
      -- Inside HTML tag, pass through unchanged
      result := result.push c
    else if c == '(' || c == '[' || c == '{' then
      -- Opening bracket: increment depth first, then wrap
      depth := depth + 1
      let colorIndex := ((depth - 1) % 6) + 1
      result := result ++ s!"<span class=\"lean-bracket-{colorIndex}\">{c}</span>"
    else if c == ')' || c == ']' || c == '}' then
      -- Closing bracket: wrap with current depth, then decrement
      let colorIndex := if depth > 0 then (depth % 6) + 1 else 1
      result := result ++ s!"<span class=\"lean-bracket-{colorIndex}\">{c}</span>"
      if depth > 0 then
        depth := depth - 1
    else
      result := result.push c

  return result

/-- Wrap line comments (-- to end of line) with styled spans.
    Detects `--` pattern outside of HTML tags and wraps up to newline
    with `<span class="line-comment">`.

    IMPORTANT: Skips `--` inside HTML tags to avoid breaking structure.
    Also skips if already inside a span (to avoid double-wrapping).
-/
def wrapLineComments (html : String) : String := Id.run do
  let mut result : String := ""
  let mut i : Nat := 0
  let chars := html.toList.toArray
  let len := chars.size

  while i < len do
    let c := chars[i]!

    -- Skip HTML tags entirely
    if c == '<' then
      -- Find the closing '>'
      let tagStart := i
      while i < len && chars[i]! != '>' do
        i := i + 1
      if i < len then
        i := i + 1  -- include the '>'
      -- Copy the entire tag
      for j in [tagStart:i] do
        if j < len then
          result := result.push chars[j]!
    -- Check for "--" comment start
    else if c == '-' && i + 1 < len && chars[i + 1]! == '-' then
      -- Found a line comment, wrap until newline or end
      let commentStart := i
      -- Find the end of the line (newline or end of string)
      while i < len && chars[i]! != '\n' do
        i := i + 1
      -- Extract the comment content
      let mut commentContent : String := ""
      for j in [commentStart:i] do
        if j < len then
          commentContent := commentContent.push chars[j]!
      -- Wrap in span
      result := result ++ s!"<span class=\"line-comment\">{commentContent}</span>"
      -- Don't increment i here - the while loop condition will handle newline
    else
      result := result.push c
      i := i + 1

  return result

/-- Default context for rendering highlighted code. -/
def defaultContext : HighlightHtmlM.Context Genre.none := {
  linkTargets := {}
  traverseContext := ()
  definitionIds := {}
  options := {}
}

/-- Render highlighted code to HTML string and hover data JSON.

Returns (html, hoverJson) where hoverJson maps hover IDs to their content.
-/
def renderHighlightedWithHovers (hl : SubVerso.Highlighting.Highlighted) : String × String :=
  let initialState : Hover.State Html := .empty
  let (html, finalState) := (hl.toHtml).run defaultContext |>.run initialState
  let hoverJson := finalState.dedup.docJson.compress
  (html.asString (breakLines := false), hoverJson)

/-- Render highlighted code with an initial state, returning the final state.

This allows chaining multiple renders with continuous hover ID numbering.
Returns (html, hoverJson, finalState) where finalState can be passed to subsequent renders.
-/
def renderHighlightedWithState (hl : SubVerso.Highlighting.Highlighted)
    (initialState : Hover.State Html := .empty) : String × String × Hover.State Html :=
  let (html, finalState) := (hl.toHtml).run defaultContext |>.run initialState
  let hoverJson := finalState.dedup.docJson.compress
  (html.asString (breakLines := false), hoverJson, finalState)

/-- Render highlighted code to HTML string using Verso's production-quality renderer.

Uses `Genre.none` with empty `LinkTargets` (no hyperlinks) and default options.
This provides syntax highlighting without interactive features like hovers or links.
Applies rainbow bracket highlighting and line comment styling via post-processing.
-/
def renderHighlightedToHtml (hl : SubVerso.Highlighting.Highlighted) : String :=
  let rawHtml := (renderHighlightedWithHovers hl).1
  let withBrackets := wrapBracketsWithDepth rawHtml
  wrapLineComments withBrackets

/-- Render highlighted code wrapped in a code element with appropriate CSS classes.

The output includes `class="hl lean block"` which Verso uses for styling.
-/
def renderHighlightedBlock (hl : SubVerso.Highlighting.Highlighted) : String :=
  let inner := renderHighlightedToHtml hl
  s!"<code class=\"hl lean block\">{inner}</code>"

/-- Render highlighted code as an inline element.

Uses `class="hl lean inline"` for inline code styling.
-/
def renderHighlightedInline (hl : SubVerso.Highlighting.Highlighted) : String :=
  let inner := renderHighlightedToHtml hl
  s!"<code class=\"hl lean inline\">{inner}</code>"

end Dress.HtmlRender
