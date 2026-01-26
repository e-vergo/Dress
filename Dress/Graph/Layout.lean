/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Graph.Types

/-!
# Graph Layout Algorithm

Implements a simplified Sugiyama layered layout algorithm for
rendering dependency graphs.

The algorithm:
1. Assign layers (longest path from sources)
2. Order nodes within layers (barycenter heuristic)
3. Assign coordinates
-/

namespace Dress.Graph.Layout

/-- A positioned node -/
structure LayoutNode where
  /-- The original node -/
  node : Node
  /-- Horizontal position -/
  x : Float
  /-- Vertical position -/
  y : Float
  /-- Width -/
  width : Float
  /-- Height -/
  height : Float
  deriving Repr, Inhabited

/-- A positioned edge with control points -/
structure LayoutEdge where
  /-- Source node id -/
  from_ : String
  /-- Target node id -/
  to : String
  /-- Control points for bezier curve (x, y pairs) -/
  points : Array (Float × Float)
  /-- Edge style (solid or dashed) -/
  style : EdgeStyle := .solid
  deriving Repr, Inhabited

/-- The complete laid-out graph -/
structure LayoutGraph where
  /-- Positioned nodes -/
  nodes : Array LayoutNode
  /-- Positioned edges -/
  edges : Array LayoutEdge
  /-- Total width -/
  width : Float
  /-- Total height -/
  height : Float
  deriving Repr, Inhabited

/-- Layout configuration for left-to-right flow -/
structure LayoutConfig where
  /-- Minimum width of a node box -/
  nodeWidth : Float := 100.0
  /-- Height of a node box -/
  nodeHeight : Float := 40.0
  /-- Horizontal gap between layers (x-axis distance) -/
  layerGap : Float := 150.0
  /-- Vertical gap between nodes in the same layer (y-axis distance) -/
  nodeGap : Float := 60.0
  /-- Padding around the graph -/
  padding : Float := 20.0
  /-- Pixels per character for dynamic node width -/
  charWidth : Float := 8.0
  /-- Number of barycenter iterations for crossing reduction -/
  barycenterIterations : Nat := 4
  deriving Repr, Inhabited

/-- Compute node width based on label length -/
def computeNodeWidth (config : LayoutConfig) (label : String) : Float :=
  let labelWidth := label.length.toFloat * config.charWidth
  if labelWidth > config.nodeWidth then labelWidth else config.nodeWidth

/-- Layout state -/
structure LayoutState where
  /-- Node to layer assignment -/
  layers : Std.HashMap String Nat := {}
  /-- Nodes in each layer -/
  layerNodes : Array (Array String) := #[]
  /-- Node positions -/
  positions : Std.HashMap String (Float × Float) := {}
  /-- Dynamic node widths based on labels -/
  nodeWidths : Std.HashMap String Float := {}
  deriving Inhabited

/-- Layout monad -/
abbrev LayoutM := StateM LayoutState

namespace Algorithm

/-- Assign layers using longest path from sources -/
def assignLayers (g : Graph) : LayoutM Unit := do
  -- Find source nodes (no incoming edges)
  let sources := g.nodes.filter fun n =>
    g.edges.all fun e => e.to != n.id

  -- BFS to assign layers
  let mut queue := sources.map (·.id)
  let mut visited : Std.HashSet String := {}

  for src in sources.map (·.id) do
    modify fun s => { s with layers := s.layers.insert src 0 }
    visited := visited.insert src

  while !queue.isEmpty do
    let current := queue[0]!
    queue := queue.toSubarray.drop 1 |>.toArray

    let currentLayer := (← get).layers.get? current |>.getD 0

    -- Process outgoing edges
    for edge in g.edges do
      if edge.from_ == current then
        let targetLayer := currentLayer + 1
        let s ← get
        let existingLayer := s.layers.get? edge.to |>.getD 0
        if targetLayer > existingLayer then
          set { s with layers := s.layers.insert edge.to targetLayer }

        if !visited.contains edge.to then
          visited := visited.insert edge.to
          queue := queue.push edge.to

  -- Build layer arrays
  let s ← get
  let maxLayer := s.layers.fold (init := 0) fun acc _ layer => max acc layer

  let mut layerNodes : Array (Array String) := #[]
  for _ in [0:maxLayer + 1] do
    layerNodes := layerNodes.push #[]
  for (nodeId, layer) in s.layers.toArray do
    layerNodes := layerNodes.modify layer (·.push nodeId)

  set { s with layerNodes }

/-- Order a single layer by barycenter of neighbors in the reference layer
    forward=true means reference is previous layer (i-1), false means next layer (i+1) -/
def orderLayerByNeighbors (g : Graph) (layerNodes : Array (Array String))
    (layerIdx : Nat) (refLayerIdx : Nat) (forward : Bool) : Array String :=
  let layer := layerNodes[layerIdx]!
  let refLayer := layerNodes[refLayerIdx]!

  -- Calculate barycenter for each node based on neighbor positions
  let barycenters := layer.map fun nodeId =>
    let neighborEdges := if forward then
      -- Forward pass: look at incoming edges (neighbors in previous layer)
      g.edges.filter (·.to == nodeId)
    else
      -- Backward pass: look at outgoing edges (neighbors in next layer)
      g.edges.filter (·.from_ == nodeId)

    let refPositions := neighborEdges.filterMap fun e =>
      let neighborId := if forward then e.from_ else e.to
      refLayer.findIdx? (· == neighborId) |>.map (·.toFloat)

    let avg := if refPositions.isEmpty then
      -- Keep original position if no neighbors in reference layer
      layer.findIdx? (· == nodeId) |>.map (·.toFloat) |>.getD 0.0
    else
      refPositions.foldl (· + ·) 0.0 / refPositions.size.toFloat
    (nodeId, avg)

  -- Sort by barycenter
  barycenters.qsort (·.2 < ·.2) |>.map (·.1)

/-- Order nodes within layers using barycenter heuristic with multiple passes -/
def orderLayers (g : Graph) (iterations : Nat) : LayoutM Unit := do
  let s ← get
  let mut layerNodes := s.layerNodes

  -- Run multiple iterations alternating forward and backward passes
  for _iter in [0:iterations] do
    -- Forward pass: order each layer based on previous layer
    for i in [1:layerNodes.size] do
      let sorted := orderLayerByNeighbors g layerNodes i (i-1) true
      layerNodes := layerNodes.set! i sorted

    -- Backward pass: order each layer based on next layer
    if layerNodes.size > 1 then
      for i in List.range (layerNodes.size - 1) |>.reverse do
        let sorted := orderLayerByNeighbors g layerNodes i (i+1) false
        layerNodes := layerNodes.set! i sorted

  set { s with layerNodes }

/-- Assign coordinates to nodes (left-to-right flow)
    - Layer index determines x position (sources on left, sinks on right)
    - Node index within layer determines y position
    - Layers are centered vertically based on the tallest layer -/
def assignCoordinates (g : Graph) (config : LayoutConfig) : LayoutM Unit := do
  let s ← get
  let mut positions : Std.HashMap String (Float × Float) := {}
  let mut nodeWidths : Std.HashMap String Float := {}

  -- First, compute dynamic widths for all nodes
  for node in g.nodes do
    let width := computeNodeWidth config node.label
    nodeWidths := nodeWidths.insert node.id width

  -- Find the maximum layer height (for vertical centering)
  let maxNodesInLayer := s.layerNodes.foldl (fun acc layer => max acc layer.size) 0
  let totalMaxHeight := maxNodesInLayer.toFloat * (config.nodeHeight + config.nodeGap) - config.nodeGap

  -- Track cumulative x position (since widths vary)
  let mut currentX := config.padding

  -- Find max width per layer for proper spacing
  let layerMaxWidths := s.layerNodes.map fun layer =>
    layer.foldl (fun acc nodeId =>
      let w := nodeWidths.get? nodeId |>.getD config.nodeWidth
      max acc w) config.nodeWidth

  for layerIdx in [0:s.layerNodes.size] do
    let layer := s.layerNodes[layerIdx]!
    let layerWidth := layerMaxWidths[layerIdx]!

    -- Calculate vertical centering offset for this layer
    let layerHeight := layer.size.toFloat * (config.nodeHeight + config.nodeGap) - config.nodeGap
    let verticalOffset := (totalMaxHeight - layerHeight) / 2.0

    for nodeIdx in [0:layer.size] do
      let nodeId := layer[nodeIdx]!
      -- Center smaller layers vertically
      let y := config.padding + verticalOffset + nodeIdx.toFloat * (config.nodeHeight + config.nodeGap)
      positions := positions.insert nodeId (currentX, y)

    -- Move x position for next layer
    currentX := currentX + layerWidth + config.layerGap

  set { s with positions, nodeWidths }

/-- Create layout edges with control points (left-to-right flow)
    - Edges start from right edge of source node
    - Edges end at left edge of target node
    - Smooth bezier curves using offset-based control points -/
def createLayoutEdges (g : Graph) (config : LayoutConfig) : LayoutM (Array LayoutEdge) := do
  let s ← get
  let mut layoutEdges : Array LayoutEdge := #[]

  for edge in g.edges do
    match (s.positions.get? edge.from_, s.positions.get? edge.to) with
    | (some (x1, y1), some (x2, y2)) =>
      -- Get dynamic width for source node
      let sourceWidth := s.nodeWidths.get? edge.from_ |>.getD config.nodeWidth
      -- Start at right edge of source, vertically centered
      let startX := x1 + sourceWidth
      let startY := y1 + config.nodeHeight / 2
      -- End at left edge of target, vertically centered
      let endX := x2
      let endY := y2 + config.nodeHeight / 2

      -- Smooth bezier: offset-based control points for gradual curves
      -- Using 1/3 offset creates smooth S-curves that don't overlap nodes
      let offset := (endX - startX) / 3.0
      let points := #[
        (startX, startY),
        (startX + offset, startY),
        (endX - offset, endY),
        (endX, endY)
      ]
      layoutEdges := layoutEdges.push { from_ := edge.from_, to := edge.to, points, style := edge.style }
    | _ => pure ()

  return layoutEdges

end Algorithm

/-- Perform complete layout of a graph -/
def layout (g : Graph) (config : LayoutConfig := {}) : LayoutGraph := Id.run do
  let (_, state) := (do
    Algorithm.assignLayers g
    Algorithm.orderLayers g config.barycenterIterations
    Algorithm.assignCoordinates g config
  ).run {}

  -- Create layout nodes with dynamic widths
  let mut layoutNodes : Array LayoutNode := #[]
  for node in g.nodes do
    match state.positions.get? node.id with
    | some (x, y) =>
      let width := state.nodeWidths.get? node.id |>.getD (computeNodeWidth config node.label)
      layoutNodes := layoutNodes.push {
        node
        x, y
        width
        height := config.nodeHeight
      }
    | none => pure ()

  -- Create layout edges
  let (layoutEdges, _) := Algorithm.createLayoutEdges g config |>.run state

  -- Calculate dimensions
  let maxX := layoutNodes.foldl (fun acc n => max acc (n.x + n.width)) 0.0
  let maxY := layoutNodes.foldl (fun acc n => max acc (n.y + n.height)) 0.0

  return {
    nodes := layoutNodes
    edges := layoutEdges
    width := maxX + config.padding
    height := maxY + config.padding
  }

end Dress.Graph.Layout
