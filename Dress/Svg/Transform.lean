/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Svg.Core

/-!
# SVG Transforms

Translate, rotate, scale, and compose transformations.
-/

namespace Dress.Svg

/-- Wrap elements in a group with a translate transform -/
def translate (dx dy : Float) (children : Array SvgElement) : SvgElement :=
  .node "g" #[("transform", s!"translate({dx}, {dy})")] children

/-- Wrap elements in a group with a rotation transform -/
def rotate (angle : Float) (cx cy : Float := 0) (children : Array SvgElement) : SvgElement :=
  .node "g" #[("transform", s!"rotate({angle}, {cx}, {cy})")] children

/-- Wrap elements in a group with a scale transform -/
def scale (sx : Float) (sy : Option Float := none) (children : Array SvgElement) : SvgElement :=
  let syVal := sy.getD sx
  .node "g" #[("transform", s!"scale({sx}, {syVal})")] children

/-- Wrap elements in a plain group -/
def group (attrs : Array (String × String) := #[]) (children : Array SvgElement) : SvgElement :=
  .node "g" attrs children

/-- Wrap elements in a group with a CSS class -/
def withClass (cls : String) (children : Array SvgElement) : SvgElement :=
  .node "g" #[("class", cls)] children

end Dress.Svg
