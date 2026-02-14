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

/-- Extract a subgraph centered on `centerId`, following edges in the given
    `direction` up to `depth` hops. Returns a new `Graph` containing only
    the reachable nodes and the edges between them.

    BFS explores layer by layer. At each depth level, all neighbors in the
    specified direction are added to the frontier. -/
def extractSubgraph (graph : Graph) (centerId : String) (depth : Nat)
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
        for edge in graph.inEdges nodeId do
          if !visited.contains edge.from_ then
            visited := visited.insert edge.from_
            nextFrontier := nextFrontier.push edge.from_
      -- Descendants: follow edges where nodeId is `from_` (to depends on nodeId)
      if direction == .descendants || direction == .both then
        for edge in graph.outEdges nodeId do
          if !visited.contains edge.to then
            visited := visited.insert edge.to
            nextFrontier := nextFrontier.push edge.to
    frontier := nextFrontier

  -- Collect nodes and edges in the subgraph
  let subNodes := graph.nodes.filter (visited.contains ·.id)
  let subEdges := graph.edges.filter fun e =>
    visited.contains e.from_ && visited.contains e.to

  return { nodes := subNodes, edges := subEdges }

/-- All direction values for iteration during generation -/
def SubgraphDirection.all : Array SubgraphDirection :=
  #[.ancestors, .descendants, .both]

/-- All depth values to pre-render (1 through 5) -/
def subgraphDepths : Array Nat := #[1, 2, 3, 4, 5]

/-- Maximum depth allowed for subgraph pre-rendering -/
def maxSubgraphDepth : Nat := 5

/-- Compute the maximum meaningful depth from a node in a single direction
    (ancestors or descendants only). Uses BFS layer-by-layer up to maxSubgraphDepth
    and returns the deepest layer that had nodes. -/
private def computeMaxDepthSingle (graph : Graph) (centerId : String)
    (useInEdges : Bool) : Nat := Id.run do
  let mut visited : Std.HashSet String := {}
  let mut frontier : Array String := #[centerId]
  visited := visited.insert centerId
  let mut depth : Nat := 0

  for _ in List.range maxSubgraphDepth do
    let mut nextFrontier : Array String := #[]
    for nodeId in frontier do
      if useInEdges then
        for edge in graph.inEdges nodeId do
          if !visited.contains edge.from_ then
            visited := visited.insert edge.from_
            nextFrontier := nextFrontier.push edge.from_
      else
        for edge in graph.outEdges nodeId do
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
    For both: max of ancestors and descendants -/
def computeMaxDepth (graph : Graph) (centerId : String) (direction : SubgraphDirection) : Nat :=
  match direction with
  | .ancestors => computeMaxDepthSingle graph centerId true
  | .descendants => computeMaxDepthSingle graph centerId false
  | .both =>
    let a := computeMaxDepthSingle graph centerId true
    let d := computeMaxDepthSingle graph centerId false
    Nat.max a d

/-- Structure holding per-node depth metadata for all three directions -/
structure NodeDepthInfo where
  ancestors : Nat
  descendants : Nat
  both : Nat
  deriving Repr, Inhabited

/-- Compute max depths for all nodes in the graph.
    Returns a map from sanitized node ID to depth info. -/
def computeAllMaxDepths (graph : Graph) (sanitize : String → String) :
    Std.HashMap String NodeDepthInfo := Id.run do
  let mut result : Std.HashMap String NodeDepthInfo := {}
  for node in graph.nodes do
    let ancestors := computeMaxDepth graph node.id .ancestors
    let descendants := computeMaxDepth graph node.id .descendants
    let both := Nat.max ancestors descendants
    let sanitizedId := sanitize node.id
    result := result.insert sanitizedId { ancestors, descendants, both }
  return result

/-- Serialize depth metadata to JSON string -/
def depthMetadataToJson (depths : Std.HashMap String NodeDepthInfo) : String := Id.run do
  let mut entries : Array String := #[]
  for (nodeId, info) in depths do
    let entry := s!"\"{nodeId}\": \{\"ancestors\": {info.ancestors}, \"descendants\": {info.descendants}, \"both\": {info.both}}"
    entries := entries.push entry
  return "{\n  " ++ ",\n  ".intercalate entries.toList ++ "\n}"

end Dress.Graph
