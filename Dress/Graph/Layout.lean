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

/-- Layout configuration -/
structure LayoutConfig where
  /-- Width of a node box -/
  nodeWidth : Float := 100.0
  /-- Height of a node box -/
  nodeHeight : Float := 40.0
  /-- Horizontal gap between nodes -/
  horizontalGap : Float := 50.0
  /-- Vertical gap between layers -/
  verticalGap : Float := 80.0
  /-- Padding around the graph -/
  padding : Float := 20.0
  deriving Repr, Inhabited

/-- Layout state -/
structure LayoutState where
  /-- Node to layer assignment -/
  layers : Std.HashMap String Nat := {}
  /-- Nodes in each layer -/
  layerNodes : Array (Array String) := #[]
  /-- Node positions -/
  positions : Std.HashMap String (Float × Float) := {}
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

/-- Order nodes within layers using barycenter heuristic -/
def orderLayers (g : Graph) : LayoutM Unit := do
  let s ← get

  -- Simple barycenter: order by average position of neighbors in previous layer
  let mut newLayerNodes := s.layerNodes

  for i in [1:s.layerNodes.size] do
    let layer := s.layerNodes[i]!
    let prevLayer := s.layerNodes[i-1]!

    -- Calculate barycenter for each node
    let barycenters := layer.map fun nodeId =>
      let inEdges := g.edges.filter (·.to == nodeId)
      let prevPositions := inEdges.filterMap fun e =>
        prevLayer.findIdx? (· == e.from_) |>.map (·.toFloat)
      let avg := if prevPositions.isEmpty then 0.0
        else prevPositions.foldl (· + ·) 0.0 / prevPositions.size.toFloat
      (nodeId, avg)

    -- Sort by barycenter
    let sorted := barycenters.qsort (·.2 < ·.2) |>.map (·.1)
    newLayerNodes := newLayerNodes.set! i sorted

  set { s with layerNodes := newLayerNodes }

/-- Assign coordinates to nodes -/
def assignCoordinates (config : LayoutConfig) : LayoutM Unit := do
  let s ← get
  let mut positions : Std.HashMap String (Float × Float) := {}

  for layerIdx in [0:s.layerNodes.size] do
    let layer := s.layerNodes[layerIdx]!
    let y := config.padding + layerIdx.toFloat * (config.nodeHeight + config.verticalGap)
    let startX := config.padding

    for nodeIdx in [0:layer.size] do
      let nodeId := layer[nodeIdx]!
      let x := startX + nodeIdx.toFloat * (config.nodeWidth + config.horizontalGap)
      positions := positions.insert nodeId (x, y)

  set { s with positions }

/-- Create layout edges with control points -/
def createLayoutEdges (g : Graph) (config : LayoutConfig) : LayoutM (Array LayoutEdge) := do
  let s ← get
  let mut layoutEdges : Array LayoutEdge := #[]

  for edge in g.edges do
    match (s.positions.get? edge.from_, s.positions.get? edge.to) with
    | (some (x1, y1), some (x2, y2)) =>
      -- Calculate control points for smooth bezier curve
      let startX := x1 + config.nodeWidth / 2
      let startY := y1 + config.nodeHeight
      let endX := x2 + config.nodeWidth / 2
      let endY := y2

      let midY := (startY + endY) / 2
      let points := #[
        (startX, startY),
        (startX, midY),
        (endX, midY),
        (endX, endY)
      ]
      layoutEdges := layoutEdges.push { from_ := edge.from_, to := edge.to, points }
    | _ => pure ()

  return layoutEdges

end Algorithm

/-- Perform complete layout of a graph -/
def layout (g : Graph) (config : LayoutConfig := {}) : LayoutGraph := Id.run do
  let (_, state) := (do
    Algorithm.assignLayers g
    Algorithm.orderLayers g
    Algorithm.assignCoordinates config
  ).run {}

  -- Create layout nodes
  let mut layoutNodes : Array LayoutNode := #[]
  for node in g.nodes do
    match state.positions.get? node.id with
    | some (x, y) =>
      layoutNodes := layoutNodes.push {
        node
        x, y
        width := config.nodeWidth
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
