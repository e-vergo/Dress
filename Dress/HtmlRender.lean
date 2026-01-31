/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Verso.Code.Highlighted
import Verso.Doc

/-!
# HTML Rendering for Blueprint Highlighting

This module provides a thin wrapper around Verso's highlighting functions to render
SubVerso highlighted code to HTML strings with rainbow bracket matching.

Uses `Genre.none` and minimal configuration to avoid dependencies on Verso's
full document infrastructure.
-/

open Verso.Code
open Verso.Doc
open Verso.Output

namespace Dress.HtmlRender

/-- Default context for rendering highlighted code. -/
def defaultContext : HighlightHtmlM.Context Genre.none := {
  linkTargets := {}
  traverseContext := ()
  definitionIds := {}
  options := {}
}

/-- Render highlighted code to HTML string and hover data JSON with rainbow bracket matching.

Returns (html, hoverJson) where hoverJson maps hover IDs to their content.
Uses Verso's `toHtmlRainbow` for proper bracket pair matching with 6-color cycling.
-/
def renderHighlightedWithHovers (hl : SubVerso.Highlighting.Highlighted) : String × String :=
  let initialState : Hover.State Html := .empty
  let (html, finalState) := (hl.toHtmlRainbow).run defaultContext |>.run initialState
  let hoverJson := finalState.dedup.docJson.compress
  (html.asString (breakLines := false), hoverJson)

/-- Render highlighted code with an initial state, returning the final state.

This allows chaining multiple renders with continuous hover ID numbering.
Returns (html, hoverJson, finalState) where finalState can be passed to subsequent renders.
Uses Verso's `toHtmlRainbow` for proper bracket pair matching.
-/
def renderHighlightedWithState (hl : SubVerso.Highlighting.Highlighted)
    (initialState : Hover.State Html := .empty) : String × String × Hover.State Html :=
  let (html, finalState) := (hl.toHtmlRainbow).run defaultContext |>.run initialState
  let hoverJson := finalState.dedup.docJson.compress
  (html.asString (breakLines := false), hoverJson, finalState)

/-- Render highlighted code to HTML string using Verso's production-quality renderer.

Uses `Genre.none` with empty `LinkTargets` (no hyperlinks) and default options.
This provides syntax highlighting with rainbow bracket matching (6-color cycling,
unmatched brackets marked with error color).
-/
def renderHighlightedToHtml (hl : SubVerso.Highlighting.Highlighted) : String :=
  (renderHighlightedWithHovers hl).1

/-- Render highlighted code wrapped in a code element with appropriate CSS classes.

The output includes `class="hl lean block"` which Verso uses for styling.
Uses Verso's `blockHtmlRainbow` for proper bracket pair matching.
-/
def renderHighlightedBlock (hl : SubVerso.Highlighting.Highlighted) : String :=
  let initialState : Hover.State Html := .empty
  let (html, _) := (hl.blockHtmlRainbow "").run defaultContext |>.run initialState
  html.asString (breakLines := false)

/-- Render highlighted code as an inline element.

Uses `class="hl lean inline"` for inline code styling.
Uses Verso's `inlineHtmlRainbow` for proper bracket pair matching.
-/
def renderHighlightedInline (hl : SubVerso.Highlighting.Highlighted) : String :=
  let initialState : Hover.State Html := .empty
  let (html, _) := (hl.inlineHtmlRainbow none).run defaultContext |>.run initialState
  html.asString (breakLines := false)

end Dress.HtmlRender
