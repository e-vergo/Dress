/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Svg.Core
import Dress.Svg.Style

/-!
# SVG Shape Primitives

Circle, rectangle, line, path, ellipse, polygon, polyline.
-/

namespace Dress.Svg

/-- Create a circle element -/
def circle (cx cy r : Float) (style : Style := {}) : SvgElement :=
  .node "circle" (#[("cx", toString cx), ("cy", toString cy), ("r", toString r)] ++ style.toAttrs) #[]

/-- Create a rectangle element -/
def rect (x y width height : Float) (rx : Float := 0) (style : Style := {}) : SvgElement :=
  let baseAttrs := #[("x", toString x), ("y", toString y),
                      ("width", toString width), ("height", toString height)]
  let cornerAttrs := if rx > 0 then #[("rx", toString rx), ("ry", toString rx)] else #[]
  .node "rect" (baseAttrs ++ cornerAttrs ++ style.toAttrs) #[]

/-- Create a line element -/
def line (x1 y1 x2 y2 : Float) (style : Style := {}) : SvgElement :=
  .node "line" (#[("x1", toString x1), ("y1", toString y1),
                   ("x2", toString x2), ("y2", toString y2)] ++ style.toAttrs) #[]

/-- Create an ellipse element -/
def ellipse (cx cy rx ry : Float) (style : Style := {}) : SvgElement :=
  .node "ellipse" (#[("cx", toString cx), ("cy", toString cy),
                      ("rx", toString rx), ("ry", toString ry)] ++ style.toAttrs) #[]

/-- Create a polygon element -/
def polygon (points : Array (Float × Float)) (style : Style := {}) : SvgElement :=
  let pointsStr := points.map (fun (x, y) => s!"{x},{y}") |>.toList |> " ".intercalate
  .node "polygon" (#[("points", pointsStr)] ++ style.toAttrs) #[]

/-- Create a polyline element -/
def polyline (points : Array (Float × Float)) (style : Style := {}) : SvgElement :=
  let pointsStr := points.map (fun (x, y) => s!"{x},{y}") |>.toList |> " ".intercalate
  .node "polyline" (#[("points", pointsStr)] ++ style.toAttrs) #[]

/-- Create a path element from a path data string -/
def path (d : String) (style : Style := {}) : SvgElement :=
  .node "path" (#[("d", d)] ++ style.toAttrs) #[]

/-- Build SVG path data for a smooth curve through sampled points -/
def curvePath (points : Array (Float × Float)) : String := Id.run do
  if points.isEmpty then return ""
  let (x0, y0) := points[0]!
  let mut d := s!"M {x0} {y0}"
  -- Use line segments for simplicity (could upgrade to bezier)
  for i in [1:points.size] do
    let (x, y) := points[i]!
    d := d ++ s!" L {x} {y}"
  return d

/-- Create a dashed line -/
def dashedLine (x1 y1 x2 y2 : Float) (dashPattern : String := "4,3")
    (style : Style := {}) : SvgElement :=
  let dashStyle := { style with strokeDasharray := some dashPattern }
  line x1 y1 x2 y2 dashStyle

end Dress.Svg
