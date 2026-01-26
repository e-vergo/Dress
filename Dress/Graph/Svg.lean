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
  /-- Node fill colors by status (8 statuses) -/
  notReadyColor : String := "#F4A460"     -- Orange - not ready to formalize
  statedColor : String := "#FFD700"       -- Yellow - statement exists, no Lean
  readyColor : String := "#20B2AA"        -- Teal - ready to formalize
  sorryColor : String := "#8B0000"        -- Deep red - has sorryAx
  provenColor : String := "#90EE90"       -- Light green - formalized without sorry
  fullyProvenColor : String := "#228B22"  -- Dark green - this + all deps proven
  mathlibReadyColor : String := "#4169E1" -- Blue - ready to upstream
  inMathlibColor : String := "#191970"    -- Dark blue - already in Mathlib
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
  | .notReady => config.notReadyColor
  | .stated => config.statedColor
  | .ready => config.readyColor
  | .sorry => config.sorryColor
  | .proven => config.provenColor
  | .fullyProven => config.fullyProvenColor
  | .mathlibReady => config.mathlibReadyColor
  | .inMathlib => config.inMathlibColor

/-- Get text color based on node status - white for dark backgrounds -/
def getTextColor (config : SvgConfig) : NodeStatus → String
  | .sorry | .fullyProven | .mathlibReady | .inMathlib => "#ffffff"
  | _ => config.textColor

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
  let nodeId := escapeXml node.node.id
  let textY := node.y + node.height / 2 + config.fontSize / 3
  let cx := node.x + node.width / 2
  let cy := node.y + node.height / 2

  let shapeElement := match node.node.shape with
    | .ellipse =>
      -- Ellipse for theorems, lemmas, propositions
      let rx := node.width / 2
      let ry := node.height / 2
      s!"  <ellipse cx=\"{cx}\" cy=\"{cy}\" rx=\"{rx}\" ry=\"{ry}\" " ++
      s!"fill=\"{fillColor}\" stroke=\"{config.strokeColor}\" " ++
      s!"stroke-width=\"{config.strokeWidth}\"/>\n"
    | .box =>
      -- Rectangle for definitions, structures, classes
      s!"  <rect x=\"{node.x}\" y=\"{node.y}\" " ++
      s!"width=\"{node.width}\" height=\"{node.height}\" " ++
      s!"rx=\"{config.borderRadius}\" ry=\"{config.borderRadius}\" " ++
      s!"fill=\"{fillColor}\" stroke=\"{config.strokeColor}\" " ++
      s!"stroke-width=\"{config.strokeWidth}\"/>\n"

  -- Wrap in <g class="node"> with <title> for click handler compatibility
  s!"<g class=\"node\">\n" ++
  s!"  <title>{nodeId}</title>\n" ++
  s!"  <a href=\"{href}\" target=\"_parent\">\n" ++
  shapeElement ++
  let textColor := getTextColor config node.node.status
  s!"    <text x=\"{cx}\" y=\"{textY}\" " ++
  s!"text-anchor=\"middle\" font-family=\"{config.fontFamily}\" " ++
  s!"font-size=\"{config.fontSize}\" fill=\"{textColor}\">" ++
  s!"{label}</text>\n" ++
  s!"  </a>\n" ++
  s!"</g>\n"

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

    -- Add stroke-dasharray for dashed edges (statement dependencies)
    let dashAttr := match edge.style with
      | .dashed => " stroke-dasharray=\"5,3\""
      | .solid => ""

    return s!"<path d=\"{path}\" fill=\"none\" stroke=\"{config.edgeColor}\" " ++
      s!"stroke-width=\"{config.edgeWidth}\"{dashAttr} marker-end=\"url(#arrowhead)\"/>\n"

/-- Generate SVG defs (arrowhead marker) -/
def renderDefs (config : SvgConfig) : String :=
  "<defs>\n" ++
  s!"  <marker id=\"arrowhead\" markerWidth=\"10\" markerHeight=\"7\" " ++
  s!"refX=\"9\" refY=\"3.5\" orient=\"auto\">\n" ++
  s!"    <polygon points=\"0 0, 10 3.5, 0 7\" fill=\"{config.edgeColor}\"/>\n" ++
  "  </marker>\n" ++
  "</defs>\n"

/-- Generate enhanced legend with border and shape examples -/
def renderLegend (config : SvgConfig) (_x _y : Float) : String := Id.run do
  -- Fixed position at top-left
  let legendX : Float := 5.0
  let legendY : Float := 5.0

  -- Legend dimensions
  let boxSize : Float := 18.0
  let gap : Float := 8.0
  let itemHeight : Float := 26.0
  let fontSize : Float := 14.0
  let padding : Float := 15.0
  let sectionGap : Float := 20.0

  -- Color items - all 8 statuses
  let colorItems := #[
    ("Not Ready", config.notReadyColor),
    ("Stated", config.statedColor),
    ("Ready", config.readyColor),
    ("Sorry", config.sorryColor),
    ("Proven", config.provenColor),
    ("Fully Proven", config.fullyProvenColor),
    ("Mathlib Ready", config.mathlibReadyColor),
    ("In Mathlib", config.inMathlibColor)
  ]

  -- Shape items (ellipse for theorems, box for definitions)
  let shapeItems := #[
    ("Theorems/Lemmas", "ellipse"),
    ("Definitions", "box")
  ]

  -- Calculate legend height
  let colorSectionHeight := colorItems.size.toFloat * itemHeight
  let shapeSectionHeight := shapeItems.size.toFloat * itemHeight
  let totalHeight := colorSectionHeight + sectionGap + shapeSectionHeight + padding * 2 + itemHeight
  let legendWidth : Float := 180.0

  let mut svg := s!"<g class=\"legend\" transform=\"translate({legendX}, {legendY})\">\n"

  -- Background with border
  svg := svg ++ s!"  <rect x=\"0\" y=\"0\" width=\"{legendWidth}\" height=\"{totalHeight}\" " ++
    s!"fill=\"white\" fill-opacity=\"0.95\" stroke=\"{config.strokeColor}\" stroke-width=\"1\" rx=\"5\" ry=\"5\"/>\n"

  -- Title
  svg := svg ++ s!"  <text x=\"{padding}\" y=\"{padding + fontSize}\" " ++
    s!"font-family=\"{config.fontFamily}\" font-size=\"{fontSize + 2}\" font-weight=\"bold\" " ++
    s!"fill=\"{config.textColor}\">Legend</text>\n"

  let contentStartY := padding + itemHeight

  -- Color status section
  for i in [0:colorItems.size] do
    let (label, color) := colorItems[i]!
    let itemY := contentStartY + i.toFloat * itemHeight
    svg := svg ++ s!"  <rect x=\"{padding}\" y=\"{itemY}\" width=\"{boxSize}\" height=\"{boxSize}\" " ++
      s!"fill=\"{color}\" stroke=\"{config.strokeColor}\" stroke-width=\"1\" rx=\"3\" ry=\"3\"/>\n"
    svg := svg ++ s!"  <text x=\"{padding + boxSize + gap}\" y=\"{itemY + boxSize - 4}\" " ++
      s!"font-family=\"{config.fontFamily}\" font-size=\"{fontSize}\" " ++
      s!"fill=\"{config.textColor}\">{label}</text>\n"

  -- Shapes section (after color items + gap)
  let shapeStartY := contentStartY + colorItems.size.toFloat * itemHeight + sectionGap

  for i in [0:shapeItems.size] do
    let (label, shape) := shapeItems[i]!
    let itemY := shapeStartY + i.toFloat * itemHeight
    let cx := padding + boxSize / 2
    let cy := itemY + boxSize / 2

    if shape == "ellipse" then
      -- Mini ellipse for theorems
      let rx := boxSize / 2
      let ry := boxSize / 2.5
      svg := svg ++ s!"  <ellipse cx=\"{cx}\" cy=\"{cy}\" rx=\"{rx}\" ry=\"{ry}\" " ++
        s!"fill=\"{config.statedColor}\" stroke=\"{config.strokeColor}\" stroke-width=\"1\"/>\n"
    else
      -- Mini rectangle for definitions
      svg := svg ++ s!"  <rect x=\"{padding}\" y=\"{itemY}\" width=\"{boxSize}\" height=\"{boxSize}\" " ++
        s!"fill=\"{config.statedColor}\" stroke=\"{config.strokeColor}\" stroke-width=\"1\" rx=\"3\" ry=\"3\"/>\n"

    svg := svg ++ s!"  <text x=\"{padding + boxSize + gap}\" y=\"{itemY + boxSize - 4}\" " ++
      s!"font-family=\"{config.fontFamily}\" font-size=\"{fontSize}\" " ++
      s!"fill=\"{config.textColor}\">{label}</text>\n"

  svg := svg ++ "</g>\n"
  return svg

/-- Generate complete SVG from a layout graph -/
def render (layout : Layout.LayoutGraph) (config : SvgConfig := {}) : String := Id.run do
  -- No extra width for legend - it's positioned at top-left overlaying the graph
  let totalWidth := layout.width
  let totalHeight := layout.height

  let mut svg := s!"<svg xmlns=\"http://www.w3.org/2000/svg\" " ++
    s!"width=\"{totalWidth}\" height=\"{totalHeight}\" " ++
    s!"viewBox=\"0 0 {totalWidth} {totalHeight}\">\n"

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

  -- Legend is now rendered as static HTML outside the SVG viewport
  -- svg := svg ++ renderLegend config 20.0 20.0

  svg := svg ++ "</svg>"
  return svg

/-- Generate SVG and write to file -/
def renderToFile (layout : Layout.LayoutGraph) (path : System.FilePath)
    (config : SvgConfig := {}) : IO Unit := do
  let content := render layout config
  IO.FS.writeFile path content

end Dress.Graph.Svg
