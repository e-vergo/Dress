/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Architect.Basic

open Lean

/-!
# Dependency Graph Types

Core types for dependency graph construction and visualization.
-/

namespace Dress.Graph

-- Re-export NodeStatus from LeanArchitect for visualization
-- This ensures consistency between the blueprint attribute and graph rendering
export Architect (NodeStatus)

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
  /-- Whether this node's status was manually set via @[blueprint] attribute -/
  isManuallyTagged : Bool := false
  /-- Whether this is a key theorem -/
  keyTheorem : Bool := false
  /-- User message/notes -/
  message : Option String := none
  /-- Whether this is a priority item for dashboard display -/
  priorityItem : Bool := false
  /-- Blocked reason -/
  blocked : Option String := none
  /-- Potential issue description -/
  potentialIssue : Option String := none
  /-- Technical debt notes -/
  technicalDebt : Option String := none
  /-- Miscellaneous notes -/
  misc : Option String := none
  deriving Repr, Inhabited

/-- Counts of nodes by status -/
structure StatusCounts where
  notReady : Nat := 0
  stated : Nat := 0
  ready : Nat := 0
  hasSorry : Nat := 0
  proven : Nat := 0
  fullyProven : Nat := 0
  mathlibReady : Nat := 0
  inMathlib : Nat := 0
  total : Nat := 0
  deriving Repr, Inhabited, ToJson, FromJson

/-- An edge in the dependency graph -/
structure Edge where
  /-- Source node id -/
  from_ : String
  /-- Target node id -/
  to : String
  /-- Edge style (solid for proof deps, dashed for statement deps) -/
  style : EdgeStyle := .solid
  /-- Whether this edge was reversed during acyclic transformation (for back-edge handling) -/
  isReversed : Bool := false
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

/-- Compute status counts for all nodes in the graph -/
def Graph.computeStatusCounts (g : Graph) : StatusCounts := Id.run do
  let mut counts : StatusCounts := {}
  for node in g.nodes do
    counts := { counts with total := counts.total + 1 }
    match node.status with
    | .notReady => counts := { counts with notReady := counts.notReady + 1 }
    | .stated => counts := { counts with stated := counts.stated + 1 }
    | .ready => counts := { counts with ready := counts.ready + 1 }
    | .sorry => counts := { counts with hasSorry := counts.hasSorry + 1 }
    | .proven => counts := { counts with proven := counts.proven + 1 }
    | .fullyProven => counts := { counts with fullyProven := counts.fullyProven + 1 }
    | .mathlibReady => counts := { counts with mathlibReady := counts.mathlibReady + 1 }
    | .inMathlib => counts := { counts with inMathlib := counts.inMathlib + 1 }
  return counts

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
