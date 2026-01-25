/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Dress.Graph.Types
import Dress.Core
import Architect.Basic

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
def addEdge (from_ to : String) : BuilderM Unit := do
  let s ← get
  -- Only add edge if both endpoints will exist
  if s.labelToId.contains from_ || s.labelToId.contains to then
    modify fun s => { s with edges := s.edges.push { from_, to } }
  else
    -- Try to add anyway - edge filtering happens later
    modify fun s => { s with edges := s.edges.push { from_, to } }

/-- Register a label -> node ID mapping -/
def registerLabel (label nodeId : String) : BuilderM Unit := do
  modify fun s => { s with labelToId := s.labelToId.insert label nodeId }

/-- Determine node status from Architect.Node -/
def getStatus (node : Architect.Node) (hasLean : Bool) : NodeStatus :=
  if node.notReady then .notReady
  else if node.proof.isSome && hasLean then .proved
  else if hasLean then .stated
  else .stated

/-- Process a single blueprint node -/
def processNode (dressNode : Dress.NodeWithPos) : BuilderM Unit := do
  let node := dressNode.toNode
  let label := node.latexLabel
  let envType := node.statement.latexEnv

  -- Register this label
  registerLabel label label

  let num ← nextNumber envType
  let graphNode : Node := {
    id := label
    label := s!"{envType.capitalize} {num}"
    envType := envType
    status := getStatus node dressNode.hasLean
    url := "#" ++ label
    leanDecls := #[node.name]
  }
  addNode graphNode

  -- Add edges from statement uses (label-based dependencies)
  for dep in node.statement.usesLabels do
    addEdge dep label

  -- Add edges from proof uses
  if let some proof := node.proof then
    for dep in proof.usesLabels do
      addEdge dep label

/-- Build graph from an array of Dress nodes -/
def buildGraph (nodes : Array Dress.NodeWithPos) : Graph :=
  let (_, state) := (nodes.forM processNode).run {}
  -- Filter edges to only include those where both endpoints exist
  let validIds := state.nodes.map (·.id) |>.toList |> Std.HashSet.ofList
  let validEdges := state.edges.filter fun e =>
    validIds.contains e.from_ && validIds.contains e.to
  { nodes := state.nodes, edges := validEdges }

end Builder

/-- Build a dependency graph from Dress blueprint nodes -/
def fromNodes (nodes : Array Dress.NodeWithPos) : Graph :=
  Builder.buildGraph nodes

/-- Build graph from the environment's blueprint extension -/
def fromEnvironment (env : Lean.Environment) : Lean.CoreM Graph := do
  let entries := Architect.blueprintExt.getEntries env
  let nodes ← entries.toArray.mapM fun (_, node) =>
    Dress.toDressNodeWithPos node
  return Builder.buildGraph nodes

end Dress.Graph
