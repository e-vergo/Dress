/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Svg.Core

/-!
# SVG Styling

Fill, stroke, opacity, and other visual attributes.
-/

namespace Dress.Svg

/-- Visual style for SVG elements -/
structure Style where
  fill : Option String := none
  stroke : Option String := none
  strokeWidth : Option Float := none
  strokeDasharray : Option String := none
  opacity : Option Float := none
  fillOpacity : Option Float := none
  fontFamily : Option String := none
  fontSize : Option Float := none
  fontWeight : Option String := none
  textAnchor : Option String := none
  dominantBaseline : Option String := none
  deriving Inhabited

namespace Style

/-- Default style (no explicit attributes) -/
def default : Style := {}

/-- Convert style to SVG attribute pairs -/
def toAttrs (s : Style) : Array (String × String) := Id.run do
  let mut attrs : Array (String × String) := #[]
  if let some f := s.fill then attrs := attrs.push ("fill", f)
  if let some sk := s.stroke then attrs := attrs.push ("stroke", sk)
  if let some sw := s.strokeWidth then attrs := attrs.push ("stroke-width", toString sw)
  if let some sd := s.strokeDasharray then attrs := attrs.push ("stroke-dasharray", sd)
  if let some o := s.opacity then attrs := attrs.push ("opacity", toString o)
  if let some fo := s.fillOpacity then attrs := attrs.push ("fill-opacity", toString fo)
  if let some ff := s.fontFamily then attrs := attrs.push ("font-family", ff)
  if let some fs := s.fontSize then attrs := attrs.push ("font-size", toString fs)
  if let some fw := s.fontWeight then attrs := attrs.push ("font-weight", fw)
  if let some ta := s.textAnchor then attrs := attrs.push ("text-anchor", ta)
  if let some db := s.dominantBaseline then attrs := attrs.push ("dominant-baseline", db)
  return attrs

end Style

end Dress.Svg
