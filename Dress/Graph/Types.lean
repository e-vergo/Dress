/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Architect.Basic

/-!
# Dependency Graph Types

Core types for dependency graph construction and visualization.
-/

namespace Dress.Graph

/-- Node status for visualization coloring -/
inductive NodeStatus where
  | stated       -- Has statement, no Lean
  | proved       -- leanOk = true (has proof)
  | notReady     -- notReady = true
  | mathLibOk    -- Proved by Mathlib reference
  deriving Repr, Inhabited, BEq

/-- Node shape for visualization -/
inductive NodeShape where
  | box      -- definitions, abbrevs, structures, classes
  | ellipse  -- theorems, lemmas, propositions
  deriving Repr, Inhabited, BEq

/-- Edge style for visualization -/
inductive EdgeStyle where
  | solid   -- proof dependencies (direct usage in proof)
  | dashed  -- statement dependencies (uses clause)
  deriving Repr, Inhabited, BEq

/-- A node in the dependency graph -/
structure Node where
  /-- Unique identifier (label) -/
  id : String
  /-- Display label (theorem type + number) -/
  label : String
  /-- Environment type (theorem, lemma, etc.) -/
  envType : String
  /-- Current status -/
  status : NodeStatus
  /-- Node shape for rendering -/
  shape : NodeShape := .ellipse
  /-- URL to the node's section in the HTML -/
  url : String
  /-- Associated Lean declaration names -/
  leanDecls : Array Lean.Name
  deriving Repr, Inhabited

/-- An edge in the dependency graph -/
structure Edge where
  /-- Source node id -/
  from_ : String
  /-- Target node id -/
  to : String
  /-- Edge style (solid for proof deps, dashed for statement deps) -/
  style : EdgeStyle := .solid
  deriving Repr, Inhabited, BEq

/-- The complete dependency graph -/
structure Graph where
  /-- All nodes -/
  nodes : Array Node
  /-- All edges -/
  edges : Array Edge
  deriving Repr, Inhabited

/-- Get all node IDs -/
def Graph.nodeIds (g : Graph) : Array String :=
  g.nodes.map (·.id)

/-- Get node by ID -/
def Graph.getNode? (g : Graph) (id : String) : Option Node :=
  g.nodes.find? (·.id == id)

/-- Get outgoing edges from a node -/
def Graph.outEdges (g : Graph) (id : String) : Array Edge :=
  g.edges.filter (·.from_ == id)

/-- Get incoming edges to a node -/
def Graph.inEdges (g : Graph) (id : String) : Array Edge :=
  g.edges.filter (·.to == id)

/-- Compute transitive reduction of the graph -/
def Graph.transitiveReduction (g : Graph) : Graph := Id.run do
  -- Build adjacency sets for reachability
  let mut reachable : Std.HashMap String (Std.HashSet String) := {}
  for node in g.nodes do
    reachable := reachable.insert node.id {}

  -- Initialize direct edges
  for edge in g.edges do
    match reachable.get? edge.from_ with
    | some set => reachable := reachable.insert edge.from_ (set.insert edge.to)
    | none => pure ()

  -- Compute transitive closure (Floyd-Warshall style)
  for k in g.nodeIds do
    for i in g.nodeIds do
      for j in g.nodeIds do
        let iReachK := reachable.get? i |>.map (·.contains k) |>.getD false
        let kReachJ := reachable.get? k |>.map (·.contains j) |>.getD false
        if iReachK && kReachJ then
          match reachable.get? i with
          | some set => reachable := reachable.insert i (set.insert j)
          | none => pure ()

  -- Keep only edges that are not implied by transitivity
  let mut reducedEdges : Array Edge := #[]
  for edge in g.edges do
    let mut isTransitive := false
    -- Check if there's an intermediate node
    for mid in g.nodeIds do
      if mid != edge.from_ && mid != edge.to then
        let fromReachMid := reachable.get? edge.from_ |>.map (·.contains mid) |>.getD false
        let midReachTo := reachable.get? mid |>.map (·.contains edge.to) |>.getD false
        if fromReachMid && midReachTo then
          -- Check if the edge from_ -> mid is direct
          let directToMid := g.edges.any fun e => e.from_ == edge.from_ && e.to == mid
          if directToMid then
            isTransitive := true
            break
    if !isTransitive then
      reducedEdges := reducedEdges.push edge

  return { g with edges := reducedEdges }

end Dress.Graph
