/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Graph.Layout

/-!
# SVG Generation

Generates SVG output from laid-out dependency graphs.
-/

namespace Dress.Graph.Svg

/-- SVG styling configuration -/
structure SvgConfig where
  /-- Background color -/
  backgroundColor : String := "#ffffff"
  /-- Node fill colors by status -/
  statedColor : String := "#f0f0f0"
  provedColor : String := "#90EE90"
  notReadyColor : String := "#FFB6C1"
  mathLibOkColor : String := "#87CEEB"
  /-- Node stroke color -/
  strokeColor : String := "#333333"
  /-- Node stroke width -/
  strokeWidth : Float := 1.5
  /-- Edge color -/
  edgeColor : String := "#555555"
  /-- Edge stroke width -/
  edgeWidth : Float := 2.0
  /-- Font family -/
  fontFamily : String := "Arial, sans-serif"
  /-- Font size -/
  fontSize : Float := 12.0
  /-- Text color -/
  textColor : String := "#333333"
  /-- Border radius for nodes -/
  borderRadius : Float := 5.0
  deriving Repr, Inhabited

/-- Get fill color for a node status -/
def getStatusColor (config : SvgConfig) : NodeStatus → String
  | .stated => config.statedColor
  | .proved => config.provedColor
  | .notReady => config.notReadyColor
  | .mathLibOk => config.mathLibOkColor

/-- Escape text for SVG -/
def escapeXml (s : String) : String :=
  s.replace "&" "&amp;"
   |>.replace "<" "&lt;"
   |>.replace ">" "&gt;"
   |>.replace "\"" "&quot;"
   |>.replace "'" "&apos;"

/-- Generate SVG for a single node -/
def renderNode (config : SvgConfig) (node : Layout.LayoutNode) : String :=
  let fillColor := getStatusColor config node.node.status
  let href := node.node.url
  let label := escapeXml node.node.label
  let textY := node.y + node.height / 2 + config.fontSize / 3

  s!"<a href=\"{href}\" target=\"_parent\">\n" ++
  s!"  <rect x=\"{node.x}\" y=\"{node.y}\" " ++
  s!"width=\"{node.width}\" height=\"{node.height}\" " ++
  s!"rx=\"{config.borderRadius}\" ry=\"{config.borderRadius}\" " ++
  s!"fill=\"{fillColor}\" stroke=\"{config.strokeColor}\" " ++
  s!"stroke-width=\"{config.strokeWidth}\"/>\n" ++
  s!"  <text x=\"{node.x + node.width / 2}\" y=\"{textY}\" " ++
  s!"text-anchor=\"middle\" font-family=\"{config.fontFamily}\" " ++
  s!"font-size=\"{config.fontSize}\" fill=\"{config.textColor}\">" ++
  s!"{label}</text>\n" ++
  "</a>\n"

/-- Generate SVG path for a bezier edge -/
def renderEdge (config : SvgConfig) (edge : Layout.LayoutEdge) : String :=
  if edge.points.size < 2 then ""
  else Id.run do
    let (x0, y0) := edge.points[0]!
    let mut path := s!"M {x0} {y0}"

    if edge.points.size >= 4 then
      -- Cubic bezier
      let (x1, y1) := edge.points[1]!
      let (x2, y2) := edge.points[2]!
      let (x3, y3) := edge.points[3]!
      path := path ++ s!" C {x1} {y1}, {x2} {y2}, {x3} {y3}"
    else
      -- Simple line
      for i in [1:edge.points.size] do
        let (x, y) := edge.points[i]!
        path := path ++ s!" L {x} {y}"

    return s!"<path d=\"{path}\" fill=\"none\" stroke=\"{config.edgeColor}\" " ++
      s!"stroke-width=\"{config.edgeWidth}\" marker-end=\"url(#arrowhead)\"/>\n"

/-- Generate SVG defs (arrowhead marker) -/
def renderDefs (config : SvgConfig) : String :=
  "<defs>\n" ++
  s!"  <marker id=\"arrowhead\" markerWidth=\"10\" markerHeight=\"7\" " ++
  s!"refX=\"9\" refY=\"3.5\" orient=\"auto\">\n" ++
  s!"    <polygon points=\"0 0, 10 3.5, 0 7\" fill=\"{config.edgeColor}\"/>\n" ++
  "  </marker>\n" ++
  "</defs>\n"

/-- Generate legend -/
def renderLegend (config : SvgConfig) (x y : Float) : String := Id.run do
  let items := #[
    ("Stated", config.statedColor),
    ("Proven", config.provedColor),
    ("Not Ready", config.notReadyColor),
    ("Mathlib", config.mathLibOkColor)
  ]
  let boxSize : Float := 15.0
  let gap : Float := 5.0
  let itemHeight : Float := 20.0

  let mut svg := s!"<g transform=\"translate({x}, {y})\">\n"
  for i in [0:items.size] do
    let (label, color) := items[i]!
    let itemY := i.toFloat * itemHeight
    svg := svg ++ s!"  <rect x=\"0\" y=\"{itemY}\" width=\"{boxSize}\" height=\"{boxSize}\" " ++
      s!"fill=\"{color}\" stroke=\"{config.strokeColor}\" stroke-width=\"1\"/>\n"
    svg := svg ++ s!"  <text x=\"{boxSize + gap}\" y=\"{itemY + boxSize - 3}\" " ++
      s!"font-family=\"{config.fontFamily}\" font-size=\"{config.fontSize - 2}\" " ++
      s!"fill=\"{config.textColor}\">{label}</text>\n"
  svg := svg ++ "</g>\n"
  return svg

/-- Generate complete SVG from a layout graph -/
def render (layout : Layout.LayoutGraph) (config : SvgConfig := {}) : String := Id.run do
  let legendWidth : Float := 100.0
  let totalWidth := layout.width + legendWidth

  let mut svg := s!"<svg xmlns=\"http://www.w3.org/2000/svg\" " ++
    s!"width=\"{totalWidth}\" height=\"{layout.height}\" " ++
    s!"viewBox=\"0 0 {totalWidth} {layout.height}\">\n"

  -- Background
  svg := svg ++ s!"<rect width=\"100%\" height=\"100%\" fill=\"{config.backgroundColor}\"/>\n"

  -- Defs
  svg := svg ++ renderDefs config

  -- Edges (render first so nodes appear on top)
  svg := svg ++ "<g class=\"edges\">\n"
  for edge in layout.edges do
    svg := svg ++ renderEdge config edge
  svg := svg ++ "</g>\n"

  -- Nodes
  svg := svg ++ "<g class=\"nodes\">\n"
  for node in layout.nodes do
    svg := svg ++ renderNode config node
  svg := svg ++ "</g>\n"

  -- Legend
  svg := svg ++ renderLegend config (layout.width + 10) 10

  svg := svg ++ "</svg>"
  return svg

/-- Generate SVG and write to file -/
def renderToFile (layout : Layout.LayoutGraph) (path : System.FilePath)
    (config : SvgConfig := {}) : IO Unit := do
  let content := render layout config
  IO.FS.writeFile path content

end Dress.Graph.Svg
