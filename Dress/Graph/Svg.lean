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
  /-- Node fill colors by status (6 statuses) -/
  notReadyColor : String := "#E8820C"     -- Vivid orange - not ready to formalize
  readyColor : String := "#0097A7"        -- Deep teal/cyan - ready to formalize
  sorryColor : String := "#C62828"        -- Vivid red - has sorryAx
  provenColor : String := "#66BB6A"       -- Medium green - formalized without sorry
  fullyProvenColor : String := "#1B5E20"  -- Deep forest green - this + all deps proven
  mathlibReadyColor : String := "#42A5F5" -- Vivid blue - ready to upstream
  /-- Axiom color (overrides status for axiom nodes) -/
  axiomColor : String := "#7E57C2"       -- Vivid purple
  /-- Node stroke color -/
  strokeColor : String := "#000000"
  /-- Node stroke width -/
  strokeWidth : Float := 1.5
  /-- Edge color -/
  edgeColor : String := "#000000"
  /-- Edge stroke width -/
  edgeWidth : Float := 1.5
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
  | .ready => config.readyColor
  | .sorry => config.sorryColor
  | .proven => config.provenColor
  | .fullyProven => config.fullyProvenColor
  | .mathlibReady => config.mathlibReadyColor

/-- Get text color based on node status - white for dark backgrounds -/
def getTextColor (config : SvgConfig) : NodeStatus → String
  | .sorry | .fullyProven => "#ffffff"
  | .ready => "#ffffff"
  | _ => config.textColor

/-- Escape text for SVG -/
def escapeXml (s : String) : String :=
  s.replace "&" "&amp;"
   |>.replace "<" "&lt;"
   |>.replace ">" "&gt;"
   |>.replace "\"" "&quot;"
   |>.replace "'" "&apos;"

/-- Get CSS class name for a node status (for dark mode targeting) -/
def statusCssClass : NodeStatus → String
  | .notReady => "status-not-ready"
  | .ready => "status-ready"
  | .sorry => "status-sorry"
  | .proven => "status-proven"
  | .fullyProven => "status-fully-proven"
  | .mathlibReady => "status-mathlib-ready"

/-- Generate SVG for a single node -/
def renderNode (config : SvgConfig) (node : Layout.LayoutNode) : String :=
  let fillColor := if node.node.envType.toLower == "axiom"
    then config.axiomColor
    else getStatusColor config node.node.status
  let href := node.node.url
  let label := escapeXml node.node.label
  let nodeId := escapeXml node.node.id
  let textY := node.y + node.height / 2 + config.fontSize / 3
  let cx := node.x + node.width / 2
  let cy := node.y + node.height / 2

  -- CSS class for dark mode targeting
  let statusClass := if node.node.envType.toLower == "axiom"
    then "status-axiom"
    else statusCssClass node.node.status

  -- Dotted border for manually-tagged nodes (notReady, ready, mathlibReady, inMathlib)
  let strokeDash := if node.node.isManuallyTagged then " stroke-dasharray=\"2,2\"" else ""

  let shapeElement := match node.node.shape with
    | .ellipse =>
      -- Ellipse for theorems, lemmas, propositions
      let rx := node.width / 2
      let ry := node.height / 2
      s!"  <ellipse class=\"node-shape {statusClass}\" cx=\"{cx}\" cy=\"{cy}\" rx=\"{rx}\" ry=\"{ry}\" " ++
      s!"fill=\"{fillColor}\" stroke=\"{config.strokeColor}\" " ++
      s!"stroke-width=\"{config.strokeWidth}\"{strokeDash}/>\n"
    | .box =>
      -- Rectangle for definitions, structures, classes
      s!"  <rect class=\"node-shape {statusClass}\" x=\"{node.x}\" y=\"{node.y}\" " ++
      s!"width=\"{node.width}\" height=\"{node.height}\" " ++
      s!"rx=\"{config.borderRadius}\" ry=\"{config.borderRadius}\" " ++
      s!"fill=\"{fillColor}\" stroke=\"{config.strokeColor}\" " ++
      s!"stroke-width=\"{config.strokeWidth}\"{strokeDash}/>\n"
    | .diamond =>
      -- Diamond (rotated square) for axioms
      s!"  <polygon class=\"node-shape {statusClass}\" points=\"{cx},{node.y} {node.x + node.width},{cy} {cx},{node.y + node.height} {node.x},{cy}\" " ++
      s!"fill=\"{fillColor}\" stroke=\"{config.strokeColor}\" " ++
      s!"stroke-width=\"{config.strokeWidth}\"{strokeDash}/>\n"

  -- Wrap in <g class="node"> with <title> for click handler compatibility
  s!"<g class=\"node\">\n" ++
  s!"  <title>{nodeId}</title>\n" ++
  s!"  <a href=\"{href}\" target=\"_parent\">\n" ++
  shapeElement ++
  let textColor := if node.node.envType.toLower == "axiom"
    then "#ffffff"  -- white on purple
    else getTextColor config node.node.status
  s!"    <text class=\"node-text\" x=\"{cx}\" y=\"{textY}\" " ++
  s!"text-anchor=\"middle\" font-family=\"{config.fontFamily}\" " ++
  s!"font-size=\"{config.fontSize}\" fill=\"{textColor}\">" ++
  s!"{label}</text>\n" ++
  s!"  </a>\n" ++
  s!"</g>\n"

/-- Reverse cubic Bezier points array.
    For a cubic Bezier with format [start, cp1, cp2, end1, cp3, cp4, end2, ...],
    reversing means: swap control points order within each segment and reverse segment order.
    Result: arrow points in original direction while path follows layout routing. -/
def reverseBezierPoints (points : Array (Float × Float)) : Array (Float × Float) :=
  if points.size < 4 then
    -- Simple case: just reverse for line segments
    points.reverse
  else
    -- For cubic Bezier: [start, cp1, cp2, end1, cp3, cp4, end2, ...]
    -- Reversed: [end_n, cp_n2, cp_n1, end_{n-1}, ..., cp2, cp1, start]
    -- Basically reverse the whole array, then swap cp1/cp2 pairs
    let reversed := points.reverse
    -- The reversed array has format: [end_n, cp_n2, cp_n1, end_{n-1}, ...]
    -- We need to swap adjacent control point pairs (indices 1-2, 4-5, 7-8, ...)
    Id.run do
      let mut result := reversed
      let mut i := 1
      while i + 1 < result.size do
        -- Swap positions i and i+1
        let tmp := result[i]!
        result := result.set! i result[i+1]!
        result := result.set! (i+1) tmp
        i := i + 3  -- Skip to next pair of control points
      return result

/-- Generate SVG path for a bezier edge -/
def renderEdge (config : SvgConfig) (edge : Layout.LayoutEdge) : String :=
  if edge.points.size < 2 then ""
  else Id.run do
    -- For reversed edges (back-edges), reverse the points so the arrow
    -- points in the original direction (toward the original target)
    let points := if edge.isReversed then reverseBezierPoints edge.points else edge.points

    let (x0, y0) := points[0]!
    let mut path := s!"M {x0} {y0}"

    -- Points format from polylineToBezier: [start, cp1, cp2, end1, cp3, cp4, end2, ...]
    -- Each cubic Bezier segment needs 3 points after start (cp1, cp2, endpoint)
    -- So valid sizes are: 2 (line), 4 (1 cubic), 7 (2 cubics), 10 (3 cubics), etc.
    -- Pattern: 1 + 3*n points for n cubic segments

    if points.size >= 4 then
      -- Process cubic Bezier segments
      -- After the start point, consume groups of 3 points (cp1, cp2, endpoint)
      let mut i := 1
      while i + 2 < points.size do
        let (cp1x, cp1y) := points[i]!
        let (cp2x, cp2y) := points[i + 1]!
        let (ex, ey) := points[i + 2]!
        path := path ++ s!" C {cp1x} {cp1y}, {cp2x} {cp2y}, {ex} {ey}"
        i := i + 3
      -- Handle any remaining points as line segments (shouldn't happen with well-formed data)
      while i < points.size do
        let (x, y) := points[i]!
        path := path ++ s!" L {x} {y}"
        i := i + 1
    else
      -- Less than 4 points: use line segments
      for i in [1:points.size] do
        let (x, y) := points[i]!
        path := path ++ s!" L {x} {y}"

    -- Add stroke-dasharray for dashed edges (statement dependencies)
    let dashAttr := match edge.style with
      | .dashed => " stroke-dasharray=\"5,3\""
      | .solid => ""

    return s!"<path class=\"graph-edge\" data-from=\"{escapeXml edge.from_}\" data-to=\"{escapeXml edge.to}\" d=\"{path}\" fill=\"none\" stroke=\"{config.edgeColor}\" " ++
      s!"stroke-width=\"{config.edgeWidth}\"{dashAttr} marker-end=\"url(#arrowhead)\"/>\n"

/-- Generate SVG defs (arrowhead marker) -/
def renderDefs (config : SvgConfig) : String :=
  "<defs>\n" ++
  s!"  <marker id=\"arrowhead\" markerWidth=\"10\" markerHeight=\"7\" " ++
  s!"refX=\"9\" refY=\"3.5\" orient=\"auto\">\n" ++
  s!"    <polygon class=\"graph-arrowhead\" points=\"0 0, 10 3.5, 0 7\" fill=\"{config.edgeColor}\"/>\n" ++
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

  -- Color items - all 6 statuses
  let colorItems := #[
    ("Not Ready", config.notReadyColor),
    ("Ready", config.readyColor),
    ("Sorry", config.sorryColor),
    ("Proven", config.provenColor),
    ("Fully Proven", config.fullyProvenColor),
    ("Mathlib Ready", config.mathlibReadyColor),
    ("Axiom", config.axiomColor)
  ]

  -- Shape items (ellipse for theorems, box for definitions)
  let shapeItems := #[
    ("Theorems/Lemmas", "ellipse"),
    ("Definitions", "box"),
    ("Axioms", "diamond")
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
        s!"fill=\"{config.provenColor}\" stroke=\"{config.strokeColor}\" stroke-width=\"1\"/>\n"
    else if shape == "diamond" then
      -- Mini diamond for axioms
      svg := svg ++ s!"  <polygon points=\"{cx},{itemY} {padding + boxSize},{cy} {cx},{itemY + boxSize} {padding},{cy}\" " ++
        s!"fill=\"{config.axiomColor}\" stroke=\"{config.strokeColor}\" stroke-width=\"1\"/>\n"
    else
      -- Mini rectangle for definitions
      svg := svg ++ s!"  <rect x=\"{padding}\" y=\"{itemY}\" width=\"{boxSize}\" height=\"{boxSize}\" " ++
        s!"fill=\"{config.provenColor}\" stroke=\"{config.strokeColor}\" stroke-width=\"1\" rx=\"3\" ry=\"3\"/>\n"

    svg := svg ++ s!"  <text x=\"{padding + boxSize + gap}\" y=\"{itemY + boxSize - 4}\" " ++
      s!"font-family=\"{config.fontFamily}\" font-size=\"{fontSize}\" " ++
      s!"fill=\"{config.textColor}\">{label}</text>\n"

  svg := svg ++ "</g>\n"
  return svg

/-- Generate complete SVG from a layout graph -/
def render (layout : Layout.LayoutGraph) (config : SvgConfig := {}) : String := Id.run do
  -- Use the content bounding box for proper centering
  -- viewBox uses (minX, minY) as origin so content is properly positioned
  let totalWidth := layout.width
  let totalHeight := layout.height
  let viewBoxX := layout.minX
  let viewBoxY := layout.minY

  let mut svg := s!"<svg xmlns=\"http://www.w3.org/2000/svg\" " ++
    s!"width=\"{totalWidth}\" height=\"{totalHeight}\" " ++
    s!"viewBox=\"{viewBoxX} {viewBoxY} {totalWidth} {totalHeight}\">\n"

  -- Background
  svg := svg ++ s!"<rect class=\"graph-bg\" width=\"100%\" height=\"100%\" fill=\"{config.backgroundColor}\"/>\n"

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
