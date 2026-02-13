/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Svg.Core
import Dress.Svg.Style
import Dress.Svg.Shapes

/-!
# SVG Text Elements

Text rendering, labels, and annotations.
-/

namespace Dress.Svg

/-- Create a text element at a position -/
def text (x y : Float) (content : String) (style : Style := {}) : SvgElement :=
  .node "text" (#[("x", toString x), ("y", toString y)] ++ style.toAttrs) #[.text content]

/-- Create a centered text element -/
def centeredText (x y : Float) (content : String) (style : Style := {}) : SvgElement :=
  let centeredStyle := { style with textAnchor := some "middle", dominantBaseline := some "central" }
  text x y content centeredStyle

/-- Create a label with background rectangle -/
def label (x y : Float) (content : String) (padding : Float := 4)
    (bgColor : String := "white") (textStyle : Style := {}) : SvgElement :=
  -- Estimate text width (rough: 7px per character at default font size)
  let charWidth := textStyle.fontSize.getD 12.0 * 0.6
  let textWidth := content.length.toFloat * charWidth
  let textHeight := textStyle.fontSize.getD 12.0
  let bgRect := rect (x - textWidth / 2 - padding) (y - textHeight / 2 - padding)
    (textWidth + padding * 2) (textHeight + padding * 2) 3
    { fill := some bgColor, stroke := some "#ccc", strokeWidth := some 0.5 }
  let textEl := centeredText x y content textStyle
  .node "g" #[] #[bgRect, textEl]

end Dress.Svg
