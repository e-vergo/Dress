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
  | diamond  -- axioms
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
  /-- Source module name (e.g., `PrimeNumberTheoremAnd.Wiener`) -/
  moduleName : Lean.Name := .anonymous
  /-- Whether this node's status was manually set via @[blueprint] attribute -/
  isManuallyTagged : Bool := false
  /-- Whether this is a key declaration -/
  keyDeclaration : Bool := false
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
  wip : Nat := 0
  hasSorry : Nat := 0
  proven : Nat := 0
  fullyProven : Nat := 0
  axiom_ : Nat := 0
  mathlibReady : Nat := 0
  numAxioms : Nat := 0
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

/-- Precomputed adjacency index for O(1) edge lookups -/
structure AdjIndex where
  /-- Outgoing edges indexed by source node ID -/
  outgoing : Std.HashMap String (Array Edge)
  /-- Incoming edges indexed by target node ID -/
  incoming : Std.HashMap String (Array Edge)

/-- Build an adjacency index from a graph for fast edge lookups -/
def Graph.buildAdjIndex (g : Graph) : AdjIndex := Id.run do
  let mut outgoing : Std.HashMap String (Array Edge) := {}
  let mut incoming : Std.HashMap String (Array Edge) := {}
  for node in g.nodes do
    outgoing := outgoing.insert node.id #[]
    incoming := incoming.insert node.id #[]
  for edge in g.edges do
    let outArr := outgoing.get? edge.from_ |>.getD #[]
    outgoing := outgoing.insert edge.from_ (outArr.push edge)
    let inArr := incoming.get? edge.to |>.getD #[]
    incoming := incoming.insert edge.to (inArr.push edge)
  return { outgoing, incoming }

/-- O(1) outgoing edge lookup from adjacency index -/
def AdjIndex.outEdges (idx : AdjIndex) (id : String) : Array Edge :=
  idx.outgoing.get? id |>.getD #[]

/-- O(1) incoming edge lookup from adjacency index -/
def AdjIndex.inEdges (idx : AdjIndex) (id : String) : Array Edge :=
  idx.incoming.get? id |>.getD #[]

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
    -- Count axioms by envType
    if node.envType.toLower == "axiom" then
      counts := { counts with numAxioms := counts.numAxioms + 1 }
    match node.status with
    | .notReady => counts := { counts with notReady := counts.notReady + 1 }
    | .wip => counts := { counts with wip := counts.wip + 1 }
    | .axiom => counts := { counts with axiom_ := counts.axiom_ + 1 }
    | .sorry => counts := { counts with hasSorry := counts.hasSorry + 1 }
    | .proven => counts := { counts with proven := counts.proven + 1 }
    | .fullyProven => counts := { counts with fullyProven := counts.fullyProven + 1 }
    | .mathlibReady => counts := { counts with mathlibReady := counts.mathlibReady + 1 }
  return counts

/-- Result of a single soundness check -/
structure SoundnessResult where
  /-- Name of the check -/
  name : String
  /-- Whether the check passed -/
  passed : Bool
  /-- Optional detail message -/
  detail : String := ""
  /-- Optional URL to link to from dashboard -/
  url : String := ""
  deriving Repr, Inhabited, ToJson, FromJson

/-- A declaration that lacks a @[blueprint] annotation -/
structure UncoveredDecl where
  /-- Fully qualified Lean name -/
  name : String
  /-- Module the declaration belongs to -/
  moduleName : String
  /-- Kind of declaration (theorem, def, etc.) -/
  kind : String
  deriving Repr, Inhabited, ToJson, FromJson

/-- Blueprint coverage results for a project -/
structure CoverageResult where
  /-- Total number of eligible project-local declarations -/
  totalDeclarations : Nat
  /-- Number of declarations with @[blueprint] annotations -/
  coveredDeclarations : Nat
  /-- Coverage percentage (0.0 to 100.0) -/
  coveragePercent : Float
  /-- List of declarations missing @[blueprint] annotation -/
  uncovered : Array UncoveredDecl
  deriving Repr, Inhabited

/-- Results of graph validation checks -/
structure CheckResults where
  /-- Whether the graph is fully connected (single component) -/
  isConnected : Bool
  /-- Number of connected components -/
  numComponents : Nat
  /-- Size of each connected component -/
  componentSizes : Array Nat
  /-- Detected cycles in the graph (each cycle = array of node IDs) -/
  cycles : Array (Array String)
  /-- Whether all key declarations are fully proven (no sorry in dependency chain) -/
  kernelVerified : Option Bool := none
  /-- Results of project-specific soundness checks -/
  soundnessResults : Array SoundnessResult := #[]
  /-- Blueprint coverage results (what percentage of declarations have @[blueprint]) -/
  coverage : Option CoverageResult := none
  deriving Repr, Inhabited

/-- Compute transitive reduction of the graph.
    Uses DFS-based approach: for each edge (u,v), checks if v is reachable
    from u via an alternative path of length >= 2.
    O(E * (V + E)) worst case, but much faster in practice on sparse DAGs. -/
def Graph.transitiveReduction (g : Graph) : Graph := Id.run do
  -- Skip for very large graphs
  if g.nodes.size > 500 then return g

  let adj := g.buildAdjIndex
  let mut reducedEdges : Array Edge := #[]

  for edge in g.edges do
    -- DFS from edge.from_, following outgoing edges but skipping
    -- the direct edge to edge.to. If we reach edge.to, it's transitive.
    let mut reachable := false
    let mut stack : Array String := #[]
    let mut visited : Std.HashSet String := {}
    visited := visited.insert edge.from_
    for e in adj.outEdges edge.from_ do
      if e.to != edge.to then
        if !visited.contains e.to then
          stack := stack.push e.to
          visited := visited.insert e.to
    while !stack.isEmpty && !reachable do
      let node := stack.back!
      stack := stack.pop
      if node == edge.to then
        reachable := true
      else
        for e in adj.outEdges node do
          if !visited.contains e.to then
            visited := visited.insert e.to
            stack := stack.push e.to
    if !reachable then
      reducedEdges := reducedEdges.push edge

  return { g with edges := reducedEdges }

end Dress.Graph
