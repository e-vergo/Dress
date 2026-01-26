/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Dress.Graph.Types
import Dress.Core
import Architect.Basic
import Architect.Output

/-!
# Dependency Graph Construction

Builds dependency graphs from blueprint nodes captured during elaboration.
-/

namespace Dress.Graph

/-- Builder state for graph construction -/
structure BuilderState where
  /-- Collected nodes -/
  nodes : Array Node := #[]
  /-- Collected edges -/
  edges : Array Edge := #[]
  /-- Current theorem counters by type -/
  counters : Std.HashMap String Nat := {}
  /-- Label to node ID mapping for edge resolution -/
  labelToId : Std.HashMap String String := {}
  deriving Inhabited

/-- Builder monad -/
abbrev BuilderM := StateM BuilderState

namespace Builder

/-- Get next number for a theorem type -/
def nextNumber (envType : String) : BuilderM Nat := do
  let s ← get
  let n := s.counters.get? envType |>.getD 0
  set { s with counters := s.counters.insert envType (n + 1) }
  return n + 1

/-- Add a node to the graph -/
def addNode (node : Node) : BuilderM Unit := do
  modify fun s => { s with nodes := s.nodes.push node }

/-- Add an edge to the graph (if both endpoints exist) -/
def addEdge (from_ to : String) (style : EdgeStyle := .solid) : BuilderM Unit := do
  let s ← get
  -- Only add edge if both endpoints will exist
  if s.labelToId.contains from_ || s.labelToId.contains to then
    modify fun s => { s with edges := s.edges.push { from_, to, style } }
  else
    -- Try to add anyway - edge filtering happens later
    modify fun s => { s with edges := s.edges.push { from_, to, style } }

/-- Register a label -> node ID mapping -/
def registerLabel (label nodeId : String) : BuilderM Unit := do
  modify fun s => { s with labelToId := s.labelToId.insert label nodeId }

/-- Determine node status from Architect.Node.
    This computes the final visualization status based on:
    1. Manual status from @[blueprint] attribute
    2. Derived statuses (sorry, proven) based on leanOk

    Priority order (highest to lowest):
    1. If manual `inMathlib` flag → inMathlib
    2. If derived inMathlib (module prefix check) → inMathlib (handled elsewhere)
    3. If manual `mathlibReady` flag → mathlibReady
    4. If hasLean and all dependencies proven → fullyProven (TODO: graph traversal)
    5. If hasLean and no sorryAx → proven
    6. If hasLean but has sorryAx → sorry
    7. If manual `ready` flag → ready
    8. If manual `notReady` flag → notReady
    9. Otherwise → stated

    Note: The `leanOk` field (no sorryAx) would need to be passed in for proper
    sorry detection. For now we use hasLean as a proxy for proven status.
    The fullyProven computation requires graph traversal and is TODO.
-/
def getStatus (node : Architect.Node) (hasLean : Bool) (hasSorry : Bool := false) : NodeStatus :=
  -- Check for manual statuses first (highest priority)
  match node.status with
  | .inMathlib => .inMathlib
  | .mathlibReady => .mathlibReady
  | .ready =>
    -- Manual ready status is preserved - user explicitly marked this as "ready to formalize"
    -- even if there's placeholder Lean code with sorry
    .ready
  | .notReady => .notReady
  | .stated =>
    -- Default: derive from Lean presence
    if hasLean then
      if hasSorry then .sorry else .proven
    else
      .stated
  -- These are derived statuses that shouldn't be manually set but handle gracefully
  | .sorry => .sorry
  | .proven => .proven
  | .fullyProven => .fullyProven  -- TODO: would need graph computation

/-- Determine node shape from environment type -/
def getShape (envType : String) : NodeShape :=
  match envType.toLower with
  | "def" | "definition" | "abbrev" | "structure" | "class" | "instance" => .box
  | "theorem" | "lemma" | "proposition" | "corollary" | "example" => .ellipse
  | _ => .ellipse  -- default to ellipse for unknown types

/-- Process a single blueprint node -/
def processNode (dressNode : Dress.NodeWithPos) (hasSorry : Bool) : BuilderM Unit := do
  let node := dressNode.toNode
  let label := node.latexLabel
  let envType := node.statement.latexEnv

  -- Register this label
  registerLabel label label

  -- Determine if this is a manually tagged status
  -- Manual statuses are: notReady, ready, mathlibReady, inMathlib
  -- Derived statuses are: stated, sorry, proven, fullyProven
  let isManual := match node.status with
    | .notReady | .ready | .mathlibReady | .inMathlib => true
    | _ => false

  let num ← nextNumber envType
  let graphNode : Node := {
    id := label
    label := s!"{envType.capitalize} {num}"
    envType := envType
    status := getStatus node dressNode.hasLean hasSorry
    shape := getShape envType
    url := "#" ++ label
    leanDecls := #[node.name]
    isManuallyTagged := isManual
  }
  addNode graphNode

  -- Add edges from statement uses (label-based dependencies) - dashed style
  for dep in node.statement.usesLabels do
    addEdge dep label .dashed

  -- Add edges from proof uses - solid style
  if let some proof := node.proof then
    for dep in proof.usesLabels do
      addEdge dep label .solid

/-- Build graph from an array of Dress nodes with sorry information -/
def buildGraph (nodesWithSorry : Array (Dress.NodeWithPos × Bool)) : Graph :=
  let (_, state) := (nodesWithSorry.forM fun (node, hasSorry) => processNode node hasSorry).run {}
  -- Filter edges to only include those where both endpoints exist
  let validIds := state.nodes.map (·.id) |>.toList |> Std.HashSet.ofList
  let validEdges := state.edges.filter fun e =>
    validIds.contains e.from_ && validIds.contains e.to
  { nodes := state.nodes, edges := validEdges }

end Builder

/-- Build a dependency graph from Dress blueprint nodes (without sorry info - assumes no sorry) -/
def fromNodes (nodes : Array Dress.NodeWithPos) : Graph :=
  Builder.buildGraph (nodes.map fun n => (n, false))

/-- Build graph from the environment's blueprint extension -/
def fromEnvironment (env : Lean.Environment) : Lean.CoreM Graph := do
  let entries := Architect.blueprintExt.getState env |>.toList
  let nodesWithSorry ← entries.toArray.mapM fun (_, node) => do
    let dressNode ← Dress.toDressNodeWithPos node
    -- Infer uses to detect sorry (leanOk = false means has sorry)
    let (_, proofUses) ← node.inferUses
    let hasSorry := !proofUses.leanOk
    return (dressNode, hasSorry)
  return Builder.buildGraph nodesWithSorry

end Dress.Graph
