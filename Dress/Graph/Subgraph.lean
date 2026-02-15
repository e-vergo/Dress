/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Graph.Types

/-!
# Subgraph Extraction

Extracts subgraphs centered on a given node, following edges in a specified
direction up to a maximum depth. Used to pre-render per-node dependency
subgraphs at build time.

## Direction Semantics

Edge `(from_ = A, to = B)` means B depends on A (B uses A).
- **ancestors**: Follow edges backward (find what the center node depends on)
- **descendants**: Follow edges forward (find what depends on the center node)
- **both**: Follow edges in both directions
-/

namespace Dress.Graph

/-- Direction for subgraph extraction -/
inductive SubgraphDirection where
  | ancestors
  | descendants
  | both
  deriving Repr, Inhabited, BEq

/-- Parse a direction string. Defaults to `both` for unrecognized input. -/
def SubgraphDirection.fromString (s : String) : SubgraphDirection :=
  match s.toLower with
  | "ancestors" => .ancestors
  | "descendants" => .descendants
  | _ => .both

/-- Convert direction to filename-safe string -/
def SubgraphDirection.toString : SubgraphDirection → String
  | .ancestors => "ancestors"
  | .descendants => "descendants"
  | .both => "both"

/-- All direction values for iteration during generation -/
def SubgraphDirection.all : Array SubgraphDirection :=
  #[.ancestors, .descendants, .both]

/-- All depth values to pre-render (1 through 5) -/
def subgraphDepths : Array Nat := #[1, 2, 3, 4, 5]

/-- Maximum depth allowed for subgraph pre-rendering -/
def maxSubgraphDepth : Nat := 5

/-- Extract a subgraph centered on `centerId`, following edges in the given
    `direction` up to `depth` hops. Returns a new `Graph` containing only
    the reachable nodes and the edges between them.

    BFS explores layer by layer. At each depth level, all neighbors in the
    specified direction are added to the frontier.

    Accepts a precomputed `AdjIndex` for O(1) edge lookups instead of
    O(E) linear scans per node. -/
def extractSubgraph (graph : Graph) (adj : AdjIndex) (centerId : String) (depth : Nat)
    (direction : SubgraphDirection) : Graph := Id.run do
  -- Quick check: if center node doesn't exist, return empty graph
  match graph.getNode? centerId with
  | none => return { nodes := #[], edges := #[] }
  | some _ => pure ()

  -- BFS state
  let mut visited : Std.HashSet String := {}
  let mut frontier : Array String := #[centerId]
  visited := visited.insert centerId

  -- BFS layer by layer up to `depth`
  for _ in List.range depth do
    let mut nextFrontier : Array String := #[]
    for nodeId in frontier do
      -- Ancestors: follow edges where nodeId is `to` (nodeId depends on from_)
      if direction == .ancestors || direction == .both then
        for edge in adj.inEdges nodeId do
          if !visited.contains edge.from_ then
            visited := visited.insert edge.from_
            nextFrontier := nextFrontier.push edge.from_
      -- Descendants: follow edges where nodeId is `from_` (to depends on nodeId)
      if direction == .descendants || direction == .both then
        for edge in adj.outEdges nodeId do
          if !visited.contains edge.to then
            visited := visited.insert edge.to
            nextFrontier := nextFrontier.push edge.to
    frontier := nextFrontier

  -- Collect nodes and edges in the subgraph
  let subNodes := graph.nodes.filter (visited.contains ·.id)
  let subEdges := graph.edges.filter fun e =>
    visited.contains e.from_ && visited.contains e.to

  return { nodes := subNodes, edges := subEdges }

/-- Extract subgraphs at all depth levels from a single BFS traversal.
    Returns array of (depth, subgraph) pairs. Much more efficient than
    calling extractSubgraph separately for each depth because the BFS
    is performed once and subgraphs are materialized from the visited set. -/
def extractSubgraphsIncremental (graph : Graph) (adj : AdjIndex) (centerId : String)
    (maxDepth : Nat) (direction : SubgraphDirection)
    : Array (Nat × Graph) := Id.run do
  match graph.getNode? centerId with
  | none => return #[]
  | some _ => pure ()

  -- BFS recording depth per node
  let mut visited : Std.HashMap String Nat := {}
  let mut frontier : Array String := #[centerId]
  visited := visited.insert centerId 0
  let mut currentDepth : Nat := 0

  for _ in List.range maxDepth do
    currentDepth := currentDepth + 1
    let mut nextFrontier : Array String := #[]
    for nodeId in frontier do
      if direction == .ancestors || direction == .both then
        for edge in adj.inEdges nodeId do
          if !visited.contains edge.from_ then
            visited := visited.insert edge.from_ currentDepth
            nextFrontier := nextFrontier.push edge.from_
      if direction == .descendants || direction == .both then
        for edge in adj.outEdges nodeId do
          if !visited.contains edge.to then
            visited := visited.insert edge.to currentDepth
            nextFrontier := nextFrontier.push edge.to
    frontier := nextFrontier

  -- Materialize subgraphs at each requested depth level
  let mut results : Array (Nat × Graph) := #[]
  for depth in subgraphDepths do
    if depth ≤ maxDepth then
      let nodeSet := visited.fold (init := ({} : Std.HashSet String)) fun acc id d =>
        if d ≤ depth then acc.insert id else acc
      let subNodes := graph.nodes.filter (nodeSet.contains ·.id)
      let subEdges := graph.edges.filter fun e =>
        nodeSet.contains e.from_ && nodeSet.contains e.to
      results := results.push (depth, { nodes := subNodes, edges := subEdges })
  return results

/-- Compute the maximum meaningful depth from a node in a single direction
    (ancestors or descendants only). Uses BFS layer-by-layer up to maxSubgraphDepth
    and returns the deepest layer that had nodes.

    Accepts a precomputed `AdjIndex` for O(1) edge lookups. -/
private def computeMaxDepthSingle (adj : AdjIndex) (centerId : String)
    (useInEdges : Bool) : Nat := Id.run do
  let mut visited : Std.HashSet String := {}
  let mut frontier : Array String := #[centerId]
  visited := visited.insert centerId
  let mut depth : Nat := 0

  for _ in List.range maxSubgraphDepth do
    let mut nextFrontier : Array String := #[]
    for nodeId in frontier do
      if useInEdges then
        for edge in adj.inEdges nodeId do
          if !visited.contains edge.from_ then
            visited := visited.insert edge.from_
            nextFrontier := nextFrontier.push edge.from_
      else
        for edge in adj.outEdges nodeId do
          if !visited.contains edge.to then
            visited := visited.insert edge.to
            nextFrontier := nextFrontier.push edge.to
    if nextFrontier.isEmpty then
      return depth
    frontier := nextFrontier
    depth := depth + 1

  return depth

/-- Compute the maximum meaningful depth from a node in a given direction.
    For ancestors: longest path to a root (node with no incoming edges)
    For descendants: longest path to a leaf (node with no outgoing edges)
    For both: max of ancestors and descendants

    Accepts a precomputed `AdjIndex` for O(1) edge lookups. -/
def computeMaxDepth (graph : Graph) (adj : AdjIndex) (centerId : String)
    (direction : SubgraphDirection) : Nat :=
  match direction with
  | .ancestors => computeMaxDepthSingle adj centerId true
  | .descendants => computeMaxDepthSingle adj centerId false
  | .both =>
    let a := computeMaxDepthSingle adj centerId true
    let d := computeMaxDepthSingle adj centerId false
    Nat.max a d

/-- Structure holding per-node depth metadata for all three directions -/
structure NodeDepthInfo where
  ancestors : Nat
  descendants : Nat
  both : Nat
  deriving Repr, Inhabited

/-- Compute max depths for all nodes using topological sort.
    O(V + E) instead of O(V * (V + E)).

    Uses Kahn's algorithm for topological ordering, then:
    - Forward pass (reverse topo order): compute descendant depth
    - Backward pass (topo order): compute ancestor depth

    Accepts a precomputed `AdjIndex` for O(1) edge lookups. -/
def computeAllMaxDepths (graph : Graph) (adj : AdjIndex) (sanitize : String → String) :
    Std.HashMap String NodeDepthInfo := Id.run do
  -- Kahn's topological sort
  let mut inDegree : Std.HashMap String Nat := {}
  for node in graph.nodes do
    inDegree := inDegree.insert node.id 0
  for edge in graph.edges do
    let deg := inDegree.get? edge.to |>.getD 0
    inDegree := inDegree.insert edge.to (deg + 1)

  let mut queue : Array String := #[]
  for node in graph.nodes do
    if (inDegree.get? node.id |>.getD 0) == 0 then
      queue := queue.push node.id
  let mut topoOrder : Array String := #[]
  let mut qi : Nat := 0
  while qi < queue.size do
    let node := queue[qi]!
    qi := qi + 1
    topoOrder := topoOrder.push node
    for edge in adj.outEdges node do
      let newDeg := (inDegree.get? edge.to |>.getD 1) - 1
      inDegree := inDegree.insert edge.to newDeg
      if newDeg == 0 then
        queue := queue.push edge.to

  -- Forward pass (reverse topo order): descendant depth
  let mut descDepth : Std.HashMap String Nat := {}
  for nodeId in topoOrder.reverse do
    let mut maxChild : Nat := 0
    for edge in adj.outEdges nodeId do
      let childD := descDepth.get? edge.to |>.getD 0
      if childD + 1 > maxChild then maxChild := childD + 1
    descDepth := descDepth.insert nodeId maxChild

  -- Backward pass (topo order): ancestor depth
  let mut ancDepth : Std.HashMap String Nat := {}
  for nodeId in topoOrder do
    let mut maxParent : Nat := 0
    for edge in adj.inEdges nodeId do
      let parentD := ancDepth.get? edge.from_ |>.getD 0
      if parentD + 1 > maxParent then maxParent := parentD + 1
    ancDepth := ancDepth.insert nodeId maxParent

  -- Assemble, capped at maxSubgraphDepth
  let mut result : Std.HashMap String NodeDepthInfo := {}
  for node in graph.nodes do
    let a := min (ancDepth.get? node.id |>.getD 0) maxSubgraphDepth
    let d := min (descDepth.get? node.id |>.getD 0) maxSubgraphDepth
    result := result.insert (sanitize node.id) { ancestors := a, descendants := d, both := max a d }
  return result

/-- Serialize depth metadata to JSON string -/
def depthMetadataToJson (depths : Std.HashMap String NodeDepthInfo) : String := Id.run do
  let mut entries : Array String := #[]
  for (nodeId, info) in depths do
    let entry := s!"\"{nodeId}\": \{\"ancestors\": {info.ancestors}, \"descendants\": {info.descendants}, \"both\": {info.both}}"
    entries := entries.push entry
  return "{\n  " ++ ",\n  ".intercalate entries.toList ++ "\n}"

end Dress.Graph
