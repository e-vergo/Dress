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

/-- Layout configuration for top-to-bottom flow -/
structure LayoutConfig where
  /-- Minimum width of a node box -/
  nodeWidth : Float := 100.0
  /-- Height of a node box -/
  nodeHeight : Float := 40.0
  /-- Vertical gap between layers (y-axis distance) -/
  layerGap : Float := 100.0
  /-- Horizontal gap between nodes in the same layer (x-axis distance) -/
  nodeGap : Float := 40.0
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

/-! ## Crossing Reduction Helpers -/

/-- Count edge crossings between two adjacent layers.
    An edge (u1, v1) crosses edge (u2, v2) if u1 < u2 and v1 > v2 (or vice versa).
    This counts inversions between edge endpoints. -/
def countCrossings (g : Graph) (layerNodes : Array (Array String))
    (layer1Idx layer2Idx : Nat) : Nat :=
  if layer1Idx < layerNodes.size then
    if layer2Idx < layerNodes.size then
      let layer1 := layerNodes[layer1Idx]!
      let layer2 := layerNodes[layer2Idx]!
      -- Get edges between these layers
      let edges := g.edges.filter fun e =>
        layer1.contains e.from_ && layer2.contains e.to
      -- For each pair of edges, check for crossing using fold
      Id.run do
        let mut crossings := 0
        for i in [0:edges.size] do
          for j in [i+1:edges.size] do
            let e1 := edges[i]!
            let e2 := edges[j]!
            let u1 := layer1.findIdx? (· == e1.from_) |>.getD 0
            let u2 := layer1.findIdx? (· == e2.from_) |>.getD 0
            let v1 := layer2.findIdx? (· == e1.to) |>.getD 0
            let v2 := layer2.findIdx? (· == e2.to) |>.getD 0
            -- Crossing if the order is inverted
            if (u1 < u2 && v1 > v2) || (u1 > u2 && v1 < v2) then
              crossings := crossings + 1
        return crossings
    else 0
  else 0

/-- Count total edge crossings in the entire graph -/
def countTotalCrossings (g : Graph) (layerNodes : Array (Array String)) : Nat := Id.run do
  let mut total := 0
  for i in [0:layerNodes.size - 1] do
    total := total + countCrossings g layerNodes i (i + 1)
  return total

/-- Swap two elements in an array -/
def swapArray (arr : Array α) (i j : Nat) : Array α :=
  if hi : i < arr.size then
    if hj : j < arr.size then
      let vi := arr[i]
      let vj := arr[j]
      arr.set! i vj |>.set! j vi
    else arr
  else arr

/-- Try swapping two adjacent nodes in a layer, return new layer if it reduces crossings -/
def trySwapAdjacent (g : Graph) (layerNodes : Array (Array String))
    (layerIdx : Nat) (i j : Nat) : Option (Array String) :=
  if layerIdx < layerNodes.size then
    let layer := layerNodes[layerIdx]!
    if i < layer.size then
      if j < layer.size then
        -- Compute crossings before swap
        let crossingsBefore := if layerIdx > 0 then
            countCrossings g layerNodes (layerIdx - 1) layerIdx
          else 0
        let crossingsAfter := if layerIdx + 1 < layerNodes.size then
            countCrossings g layerNodes layerIdx (layerIdx + 1)
          else 0
        let totalBefore := crossingsBefore + crossingsAfter

        -- Swap nodes
        let swapped := swapArray layer i j

        -- Update layerNodes temporarily for crossing count
        let newLayerNodes := layerNodes.set! layerIdx swapped

        -- Compute crossings after swap
        let crossingsBeforeNew := if layerIdx > 0 then
            countCrossings g newLayerNodes (layerIdx - 1) layerIdx
          else 0
        let crossingsAfterNew := if layerIdx + 1 < newLayerNodes.size then
            countCrossings g newLayerNodes layerIdx (layerIdx + 1)
          else 0
        let totalAfter := crossingsBeforeNew + crossingsAfterNew

        -- Keep swap if it reduces crossings
        if totalAfter < totalBefore then
          some swapped
        else
          none
      else none
    else none
  else none

/-- Transpose heuristic: repeatedly try swapping adjacent nodes to reduce crossings -/
def transposePass (g : Graph) (layerNodes : Array (Array String)) : Array (Array String) := Id.run do
  let mut result := layerNodes
  let mut improved := true
  let mut iterations := 0
  let maxIterations := 10  -- Prevent infinite loops

  while improved && iterations < maxIterations do
    improved := false
    iterations := iterations + 1
    for layerIdx in [0:result.size] do
      let layer := result[layerIdx]!
      for i in [0:layer.size - 1] do
        match trySwapAdjacent g result layerIdx i (i + 1) with
        | some newLayer =>
          result := result.set! layerIdx newLayer
          improved := true
        | none => pure ()

  return result

/-! ## Median Calculation -/

/-- Calculate median of a list of floats -/
def median (values : Array Float) : Float :=
  if values.isEmpty then 0.0
  else
    let sorted := values.qsort (· < ·)
    let mid := sorted.size / 2
    if sorted.size % 2 == 0 then
      -- Even number: average of two middle values
      (sorted[mid - 1]! + sorted[mid]!) / 2.0
    else
      sorted[mid]!

/-- Get the median position of neighbors in a reference layer.
    For DOT-style layout, we use median instead of mean for robustness. -/
def medianNeighborPosition (g : Graph) (layerNodes : Array (Array String))
    (nodeId : String) (layerIdx refLayerIdx : Nat) (forward : Bool) : Option Float :=
  if h1 : layerIdx < layerNodes.size then
    if h2 : refLayerIdx < layerNodes.size then
      let refLayer := layerNodes[refLayerIdx]

      -- Get edges connecting to reference layer
      let neighborEdges := if forward then
        g.edges.filter (·.to == nodeId)  -- Incoming edges
      else
        g.edges.filter (·.from_ == nodeId)  -- Outgoing edges

      let refPositions := neighborEdges.filterMap fun e =>
        let neighborId := if forward then e.from_ else e.to
        refLayer.findIdx? (· == neighborId) |>.map (·.toFloat)

      if refPositions.isEmpty then
        none
      else
        some (median refPositions)
    else none
  else none

/-- Order a single layer by median of neighbors in the reference layer -/
def orderLayerByMedian (g : Graph) (layerNodes : Array (Array String))
    (layerIdx : Nat) (refLayerIdx : Nat) (forward : Bool) : Array String :=
  if h : layerIdx < layerNodes.size then
    let layer := layerNodes[layerIdx]

    -- Calculate median position for each node
    let medians := layer.map fun nodeId =>
      let med := medianNeighborPosition g layerNodes nodeId layerIdx refLayerIdx forward
      match med with
      | some m => (nodeId, m)
      | none =>
        -- Keep original position if no neighbors
        let origIdx := layer.findIdx? (· == nodeId) |>.getD 0
        (nodeId, origIdx.toFloat)

    -- Sort by median
    medians.qsort (·.2 < ·.2) |>.map (·.1)
  else #[]

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

/-- Order nodes within layers using median heuristic with transpose optimization.
    DOT algorithm approach:
    1. Multiple iterations of forward/backward median ordering
    2. Transpose heuristic after each pass to further reduce crossings -/
def orderLayers (g : Graph) (iterations : Nat) : LayoutM Unit := do
  let s ← get
  let mut layerNodes := s.layerNodes

  -- Run multiple iterations alternating forward and backward passes
  for _iter in [0:iterations] do
    -- Forward pass: order each layer by median of previous layer neighbors
    for i in [1:layerNodes.size] do
      let sorted := orderLayerByMedian g layerNodes i (i-1) true
      layerNodes := layerNodes.set! i sorted

    -- Apply transpose heuristic after forward pass
    layerNodes := transposePass g layerNodes

    -- Backward pass: order each layer by median of next layer neighbors
    if layerNodes.size > 1 then
      for i in List.range (layerNodes.size - 1) |>.reverse do
        let sorted := orderLayerByMedian g layerNodes i (i+1) false
        layerNodes := layerNodes.set! i sorted

    -- Apply transpose heuristic after backward pass
    layerNodes := transposePass g layerNodes

  set { s with layerNodes }

/-! ## Median-Based Coordinate Assignment (for top-to-bottom layout) -/

/-- Get all neighbor X positions for a node (from all connected layers) -/
def getNeighborXPositions (g : Graph) (positions : Std.HashMap String (Float × Float))
    (nodeId : String) : Array Float := Id.run do
  let mut xPositions : Array Float := #[]

  -- Get neighbors from incoming edges
  for edge in g.edges do
    if edge.to == nodeId then
      match positions.get? edge.from_ with
      | some (x, _) => xPositions := xPositions.push x
      | none => pure ()
    if edge.from_ == nodeId then
      match positions.get? edge.to with
      | some (x, _) => xPositions := xPositions.push x
      | none => pure ()

  return xPositions

/-- Calculate ideal X position based on median of all neighbor X positions -/
def idealXPosition (g : Graph) (positions : Std.HashMap String (Float × Float))
    (nodeId : String) : Option Float :=
  let neighborXs := getNeighborXPositions g positions nodeId
  if neighborXs.isEmpty then
    none
  else
    -- Use median for robustness
    some (median neighborXs)

/-- Resolve overlaps within a layer by pushing nodes apart horizontally -/
def resolveOverlaps (layer : Array String) (positions : Std.HashMap String (Float × Float))
    (nodeWidths : Std.HashMap String Float) (config : LayoutConfig) : Std.HashMap String (Float × Float) := Id.run do
  let mut result := positions

  -- Sort nodes by X position
  let nodesWithX := layer.filterMap fun nodeId =>
    positions.get? nodeId |>.map fun (x, y) => (nodeId, x, y)

  let sorted := nodesWithX.qsort (fun a b => a.2.1 < b.2.1)

  -- Push overlapping nodes apart, tracking the actual X positions as we update
  let mut lastX : Float := -1000.0  -- Start with a very low value
  let mut lastWidth : Float := 0.0

  for i in [0:sorted.size] do
    let (currId, currX, currY) := sorted[i]!
    let currWidth := nodeWidths.get? currId |>.getD config.nodeWidth

    -- Get the actual current X (which may have been updated)
    let actualX := match result.get? currId with
      | some (x, _) => x
      | none => currX

    if i == 0 then
      lastX := actualX
      lastWidth := currWidth
    else
      let minSeparation := lastWidth + config.nodeGap
      let gap := actualX - lastX
      if gap < minSeparation then
        -- Push current node right
        let newX := lastX + minSeparation
        result := result.insert currId (newX, currY)
        lastX := newX
      else
        lastX := actualX
      lastWidth := currWidth

  return result

/-- Single pass of position refinement: move nodes toward neighbor medians -/
def refinePositionsPass (g : Graph) (layerNodes : Array (Array String))
    (positions : Std.HashMap String (Float × Float))
    (nodeWidths : Std.HashMap String Float) (config : LayoutConfig)
    (forward : Bool) : Std.HashMap String (Float × Float) := Id.run do
  let mut result := positions

  let layerIndices := if forward then
    List.range layerNodes.size
  else
    List.range layerNodes.size |>.reverse

  for layerIdx in layerIndices do
    let layer := layerNodes[layerIdx]!

    -- Calculate ideal positions for this layer
    for nodeId in layer do
      match (result.get? nodeId, idealXPosition g result nodeId) with
      | (some (_currentX, y), some idealX) =>
        -- Move toward ideal position (but keep Y fixed)
        result := result.insert nodeId (idealX, y)
      | _ => pure ()

    -- Resolve overlaps in this layer
    result := resolveOverlaps layer result nodeWidths config

  return result

/-- Iteratively refine positions to achieve organic layout -/
def refinePositions (g : Graph) (config : LayoutConfig) (refinementIterations : Nat) : LayoutM Unit := do
  let s ← get
  let mut positions := s.positions

  for _iter in [0:refinementIterations] do
    -- Forward pass
    positions := refinePositionsPass g s.layerNodes positions s.nodeWidths config true
    -- Backward pass
    positions := refinePositionsPass g s.layerNodes positions s.nodeWidths config false

  set { s with positions }

/-- Assign initial coordinates to nodes (grid-based starting point)
    Top-to-bottom layout:
    - Layer index determines y position (sources at top, sinks at bottom)
    - Node index within layer determines initial x position -/
def assignInitialCoordinates (g : Graph) (config : LayoutConfig) : LayoutM Unit := do
  let s ← get
  let mut positions : Std.HashMap String (Float × Float) := {}
  let mut nodeWidths : Std.HashMap String Float := {}

  -- First, compute dynamic widths for all nodes
  for node in g.nodes do
    let width := computeNodeWidth config node.label
    nodeWidths := nodeWidths.insert node.id width

  -- Find the maximum layer width (for horizontal centering)
  -- Calculate total width needed for each layer
  let layerTotalWidths := s.layerNodes.map fun layer =>
    let widthSum := layer.foldl (fun acc nodeId =>
      let w := nodeWidths.get? nodeId |>.getD config.nodeWidth
      acc + w) 0.0
    let gapsWidth := if layer.size > 1 then (layer.size - 1).toFloat * config.nodeGap else 0.0
    widthSum + gapsWidth

  let maxLayerWidth := layerTotalWidths.foldl max 0.0

  -- Track cumulative y position
  let mut currentY := config.padding

  for layerIdx in [0:s.layerNodes.size] do
    let layer := s.layerNodes[layerIdx]!
    let layerTotalWidth := layerTotalWidths[layerIdx]!

    -- Calculate horizontal centering offset for this layer
    let horizontalOffset := (maxLayerWidth - layerTotalWidth) / 2.0

    -- Place nodes in this layer horizontally
    let mut currentX := config.padding + horizontalOffset
    for nodeIdx in [0:layer.size] do
      let nodeId := layer[nodeIdx]!
      let nodeWidth := nodeWidths.get? nodeId |>.getD config.nodeWidth
      positions := positions.insert nodeId (currentX, currentY)
      currentX := currentX + nodeWidth + config.nodeGap

    -- Move y position for next layer
    currentY := currentY + config.nodeHeight + config.layerGap

  set { s with positions, nodeWidths }

/-- Assign coordinates using DOT-style algorithm:
    1. Initial grid-based placement
    2. Iterative refinement toward neighbor medians
    3. Overlap resolution -/
def assignCoordinates (g : Graph) (config : LayoutConfig) : LayoutM Unit := do
  -- Step 1: Initial grid-based placement
  assignInitialCoordinates g config

  -- Step 2: Refine positions toward neighbor medians (5 iterations)
  refinePositions g config 5

/-- Create layout edges with control points (top-to-bottom flow)
    - Edges start from bottom edge of source node
    - Edges end at top edge of target node
    - Smooth bezier curves using offset-based control points
    - Multiple edges to/from a node are distributed along the node boundary -/
def createLayoutEdges (g : Graph) (config : LayoutConfig) : LayoutM (Array LayoutEdge) := do
  let s ← get
  let mut layoutEdges : Array LayoutEdge := #[]

  -- Step 1: Count outgoing and incoming edges per node
  let mut outgoingCounts : Std.HashMap String Nat := {}
  let mut incomingCounts : Std.HashMap String Nat := {}

  for edge in g.edges do
    outgoingCounts := outgoingCounts.insert edge.from_
      ((outgoingCounts.get? edge.from_).getD 0 + 1)
    incomingCounts := incomingCounts.insert edge.to
      ((incomingCounts.get? edge.to).getD 0 + 1)

  -- Step 2: Track edge index per node as we process edges
  let mut outgoingIndex : Std.HashMap String Nat := {}
  let mut incomingIndex : Std.HashMap String Nat := {}

  for edge in g.edges do
    match (s.positions.get? edge.from_, s.positions.get? edge.to) with
    | (some (x1, y1), some (x2, y2)) =>
      -- Get dynamic width for source and target nodes
      let sourceWidth := s.nodeWidths.get? edge.from_ |>.getD config.nodeWidth
      let targetWidth := s.nodeWidths.get? edge.to |>.getD config.nodeWidth

      -- Get counts and current indices
      let outCount := outgoingCounts.get? edge.from_ |>.getD 1
      let inCount := incomingCounts.get? edge.to |>.getD 1
      let outIdx := outgoingIndex.get? edge.from_ |>.getD 0
      let inIdx := incomingIndex.get? edge.to |>.getD 0

      -- Calculate spread for source (bottom edge) - use 80% of node width
      let spreadWidth := sourceWidth * 0.8
      let outSpacing := if outCount > 1 then spreadWidth / (outCount.toFloat - 1) else 0.0
      let outOffsetX := if outCount > 1 then -spreadWidth/2 + outSpacing * outIdx.toFloat else 0.0

      -- Calculate spread for target (top edge) - use 80% of node width
      let inSpacing := if inCount > 1 then (targetWidth * 0.8) / (inCount.toFloat - 1) else 0.0
      let inOffsetX := if inCount > 1 then -(targetWidth * 0.8)/2 + inSpacing * inIdx.toFloat else 0.0

      -- Start at bottom edge of source, with distributed X offset
      let startX := x1 + sourceWidth / 2 + outOffsetX
      let startY := y1 + config.nodeHeight
      -- End at top edge of target, with distributed X offset
      let endX := x2 + targetWidth / 2 + inOffsetX
      let endY := y2

      -- Increment indices for next edge from/to these nodes
      outgoingIndex := outgoingIndex.insert edge.from_ (outIdx + 1)
      incomingIndex := incomingIndex.insert edge.to (inIdx + 1)

      -- Smooth bezier: offset-based control points for gradual curves
      -- Using 1/3 offset creates smooth S-curves that don't overlap nodes
      let offset := (endY - startY) / 3.0
      let points := #[
        (startX, startY),
        (startX, startY + offset),
        (endX, endY - offset),
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
