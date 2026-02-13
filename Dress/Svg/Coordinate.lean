/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Svg.Core
import Dress.Svg.Style
import Dress.Svg.Shapes
import Dress.Svg.Text

/-!
# SVG Coordinate System

Maps mathematical coordinates to SVG pixel coordinates.
Provides axes, grids, and tick marks.
-/

namespace Dress.Svg

/-- A coordinate system mapping math coordinates to SVG pixels -/
structure CoordSystem where
  /-- Mathematical X range -/
  xMin : Float
  xMax : Float
  /-- Mathematical Y range -/
  yMin : Float
  yMax : Float
  /-- SVG pixel dimensions -/
  svgWidth : Float
  svgHeight : Float
  /-- Padding around the plot area -/
  padding : Float := 40.0
  deriving Inhabited

namespace CoordSystem

/-- Plot area width in pixels -/
def plotWidth (cs : CoordSystem) : Float := cs.svgWidth - 2 * cs.padding

/-- Plot area height in pixels -/
def plotHeight (cs : CoordSystem) : Float := cs.svgHeight - 2 * cs.padding

/-- Convert math X to SVG X -/
def toSvgX (cs : CoordSystem) (mathX : Float) : Float :=
  cs.padding + (mathX - cs.xMin) / (cs.xMax - cs.xMin) * cs.plotWidth

/-- Convert math Y to SVG Y (flipped: math Y up, SVG Y down) -/
def toSvgY (cs : CoordSystem) (mathY : Float) : Float :=
  cs.padding + (cs.yMax - mathY) / (cs.yMax - cs.yMin) * cs.plotHeight

/-- Convert a math point to SVG coordinates -/
def toSvg (cs : CoordSystem) (p : Float × Float) : Float × Float :=
  (cs.toSvgX p.1, cs.toSvgY p.2)

/-- Generate X axis -/
def xAxis (cs : CoordSystem) (style : Style := { stroke := some "#333", strokeWidth := some 1.5 }) : SvgElement :=
  let y0 := cs.toSvgY 0
  line cs.padding y0 (cs.svgWidth - cs.padding) y0 style

/-- Generate Y axis -/
def yAxis (cs : CoordSystem) (style : Style := { stroke := some "#333", strokeWidth := some 1.5 }) : SvgElement :=
  let x0 := cs.toSvgX 0
  line x0 cs.padding x0 (cs.svgHeight - cs.padding) style

/-- Generate both axes -/
def axes (cs : CoordSystem) : Array SvgElement :=
  #[cs.xAxis, cs.yAxis]

/-- Generate tick marks on X axis -/
def xTicks (cs : CoordSystem) (values : Array Float) (tickSize : Float := 5)
    (style : Style := { stroke := some "#333", strokeWidth := some 1 }) : Array SvgElement := Id.run do
  let y0 := cs.toSvgY 0
  let mut ticks : Array SvgElement := #[]
  let labelStyle : Style := { fontSize := some 11, textAnchor := some "middle", fill := some "#333" }
  for v in values do
    let x := cs.toSvgX v
    ticks := ticks.push (line x (y0 - tickSize) x (y0 + tickSize) style)
    -- Label (skip 0 to avoid overlap with Y axis)
    if v != 0 then
      ticks := ticks.push (text x (y0 + tickSize + 14) (toString v) labelStyle)
  return ticks

/-- Generate tick marks on Y axis -/
def yTicks (cs : CoordSystem) (values : Array Float) (tickSize : Float := 5)
    (style : Style := { stroke := some "#333", strokeWidth := some 1 }) : Array SvgElement := Id.run do
  let x0 := cs.toSvgX 0
  let mut ticks : Array SvgElement := #[]
  let labelStyle : Style := { fontSize := some 11, textAnchor := some "end", fill := some "#333" }
  for v in values do
    let y := cs.toSvgY v
    ticks := ticks.push (line (x0 - tickSize) y (x0 + tickSize) y style)
    if v != 0 then
      ticks := ticks.push (text (x0 - tickSize - 4) (y + 4) (toString v) labelStyle)
  return ticks

/-- Generate a background grid -/
def grid (cs : CoordSystem) (xValues yValues : Array Float)
    (style : Style := { stroke := some "#e0e0e0", strokeWidth := some 0.5 }) : Array SvgElement := Id.run do
  let mut lines : Array SvgElement := #[]
  for x in xValues do
    let sx := cs.toSvgX x
    lines := lines.push (line sx cs.padding sx (cs.svgHeight - cs.padding) style)
  for y in yValues do
    let sy := cs.toSvgY y
    lines := lines.push (line cs.padding sy (cs.svgWidth - cs.padding) sy style)
  return lines

/-- Plot a function by sampling points -/
def plotFunction (cs : CoordSystem) (f : Float → Float) (numSamples : Nat := 200)
    (style : Style := { stroke := some "#2196F3", strokeWidth := some 2, fill := some "none" }) : SvgElement :=
  let step := (cs.xMax - cs.xMin) / numSamples.toFloat
  let points := Id.run do
    let mut pts : Array (Float × Float) := #[]
    for i in [0:numSamples + 1] do
      let x := cs.xMin + i.toFloat * step
      let y := f x
      pts := pts.push (cs.toSvgX x, cs.toSvgY y)
    return pts
  path (curvePath points) style

end CoordSystem

end Dress.Svg
