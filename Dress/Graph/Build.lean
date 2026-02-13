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
  -- Only add edge if BOTH endpoints are known blueprint labels
  if s.labelToId.contains from_ && s.labelToId.contains to then
    modify fun s => { s with edges := s.edges.push { from_, to, style } }
  -- else: silently skip edges with unknown endpoints

/-- Register a label -> node ID mapping -/
def registerLabel (label nodeId : String) : BuilderM Unit := do
  modify fun s => { s with labelToId := s.labelToId.insert label nodeId }

/-- Determine node status from Architect.Node.
    This computes the final visualization status based on:
    1. Manual status from @[blueprint] attribute
    2. Derived statuses (sorry, proven) based on hasLean

    Priority order (highest to lowest):
    1. If manual `mathlibReady` flag → mathlibReady
    2. If manual `ready` flag → ready
    3. If hasLean and no sorryAx → proven (fullyProven computed post-graph)
    4. If hasLean but has sorryAx → sorry
    5. Default: notReady (no Lean code)

    Note: fullyProven is computed by `computeFullyProven` after graph construction
    by checking that all dependencies are proven/fullyProven.
-/
def getStatus (node : Architect.Node) (hasLean : Bool) (hasSorry : Bool := false) : NodeStatus :=
  match node.status with
  | .mathlibReady => .mathlibReady  -- Highest priority manual flag
  | .ready => .ready                 -- Manual ready flag
  | _ =>
    -- Auto-derive from Lean presence
    if hasLean then
      if hasSorry then .sorry else .proven
    else
      .notReady  -- No Lean = notReady

/-- Determine node shape from environment type -/
def getShape (envType : String) : NodeShape :=
  match envType.toLower with
  | "def" | "definition" | "abbrev" | "structure" | "class" | "instance" => .box
  | "theorem" | "lemma" | "proposition" | "corollary" | "example" => .ellipse
  | _ => .ellipse  -- default to ellipse for unknown types

/-- PASS 1: Register a node's label and create the graph node (no edges) -/
def registerNode (dressNode : Dress.NodeWithPos) (hasSorry : Bool) : BuilderM Unit := do
  let node := dressNode.toNode
  let label := node.latexLabel
  let envType := node.statement.latexEnv

  -- Register this label
  registerLabel label label

  -- Determine if this is a manually tagged status
  -- Manual statuses are: ready, mathlibReady
  -- Derived statuses are: notReady (default), sorry, proven, fullyProven
  let isManual := match node.status with
    | .ready | .mathlibReady => true
    | _ => false

  -- Priority: title > full qualified Lean name
  let displayLabel := match node.title with
    | some name => name
    | none => node.name.toString  -- Full qualified name like "SBSTest.Chapter2.square_nonneg"

  -- Extract module name from location if available
  let moduleName := match dressNode.location with
    | some loc => loc.module
    | none => .anonymous

  let graphNode : Node := {
    id := label
    label := displayLabel
    envType := envType
    status := getStatus node dressNode.hasLean hasSorry
    shape := getShape envType
    url := "#" ++ label
    leanDecls := #[node.name]
    moduleName := moduleName
    isManuallyTagged := isManual
    -- Propagate metadata fields from Architect.Node
    keyDeclaration := node.keyDeclaration
    message := node.message
    priorityItem := node.priorityItem
    blocked := node.blocked
    potentialIssue := node.potentialIssue
    technicalDebt := node.technicalDebt
    misc := node.misc
  }
  addNode graphNode

/-- PASS 2: Add edges for a node (all labels now registered) -/
def addNodeEdges (dressNode : Dress.NodeWithPos)
    (statementUses proofUses : Array String) : BuilderM Unit := do
  let node := dressNode.toNode
  let label := node.latexLabel

  -- Add edges from inferred statement uses - dashed style
  for dep in statementUses do
    addEdge dep label .dashed

  -- Add edges from inferred proof uses - solid style
  for dep in proofUses do
    addEdge dep label .solid

/-- Data for building a graph node: DressNode, hasSorry, statementUses, proofUses -/
structure NodeBuildData where
  node : Dress.NodeWithPos
  hasSorry : Bool
  statementUses : Array String
  proofUses : Array String

/-- Compute fullyProven status for all eligible nodes.
    A node is fullyProven if:
    1. It is `proven` status (not sorry, has Lean code)
    2. ALL its dependencies (ancestors) are `proven` or `fullyProven`

    Uses memoization for O(V+E) complexity.
-/
def computeFullyProven (g : Graph) : Graph := Id.run do
  -- Build dependency map: nodeId -> [dependency nodeIds]
  -- Edge semantics: (from_=A, to=B) means B depends on A (B uses A)
  let mut deps : Std.HashMap String (Array String) := {}
  for node in g.nodes do
    deps := deps.insert node.id #[]
  for edge in g.edges do
    -- edge.to depends on edge.from_ (edge.to uses edge.from_)
    match deps.get? edge.to with
    | some arr => deps := deps.insert edge.to (arr.push edge.from_)
    | none => pure ()

  -- Build node lookup
  let mut nodeMap : Std.HashMap String Node := {}
  for node in g.nodes do
    nodeMap := nodeMap.insert node.id node

  -- Memoization: nodeId -> Bool (is this node + all ancestors complete?)
  let mut memo : Std.HashMap String Bool := {}

  -- Check if status is "complete" (proven or fullyProven)
  let isComplete : NodeStatus → Bool
    | .proven | .fullyProven => true
    | _ => false

  -- Recursive check with memoization using iterative worklist algorithm
  for node in g.nodes do
    if memo.contains node.id then continue
    -- DFS to check all ancestors
    let mut stack : Array String := #[node.id]
    let mut visiting : Std.HashSet String := {}
    while !stack.isEmpty do
      let curr := stack.back!
      if memo.contains curr then
        stack := stack.pop
        continue
      -- Get node status
      match nodeMap.get? curr with
      | none =>
        memo := memo.insert curr false
        stack := stack.pop
      | some currNode =>
        if !isComplete currNode.status then
          memo := memo.insert curr false
          stack := stack.pop
        else
          -- Check if all deps are resolved
          let currDeps := deps.get? curr |>.getD #[]
          let unresolved := currDeps.filter (fun d => !memo.contains d)
          if unresolved.isEmpty then
            -- All deps resolved, compute our result
            let allDepsComplete := currDeps.all (fun d => memo.get? d |>.getD false)
            memo := memo.insert curr allDepsComplete
            stack := stack.pop
          else
            -- Push unresolved deps onto stack
            if visiting.contains curr then
              -- Cycle detected, mark as incomplete
              memo := memo.insert curr false
              stack := stack.pop
            else
              visiting := visiting.insert curr
              for dep in unresolved do
                stack := stack.push dep

  -- Update nodes: proven -> fullyProven where all ancestors complete
  let updatedNodes := g.nodes.map fun node =>
    if node.status == .proven then
      match memo.get? node.id with
      | some true => { node with status := .fullyProven }
      | _ => node
    else
      node

  { g with nodes := updatedNodes }

/-- Build graph from an array of node build data using two-pass processing.
    PASS 1: Register all labels and create nodes (so all labels exist)
    PASS 2: Add all edges (now back-edges work because targets are registered) -/
def buildGraph (nodesData : Array NodeBuildData) : Graph :=
  -- PASS 1: Register all labels and create nodes
  let (_, stateAfterNodes) := (nodesData.forM fun data =>
    registerNode data.node data.hasSorry).run {}
  -- PASS 2: Add all edges (now all labels are registered)
  let (_, state) := (nodesData.forM fun data =>
    addNodeEdges data.node data.statementUses data.proofUses).run stateAfterNodes
  -- Filter edges to only include those where both endpoints exist
  let validIds := state.nodes.map (·.id) |>.toList |> Std.HashSet.ofList
  let validEdges := state.edges.filter fun e =>
    validIds.contains e.from_ && validIds.contains e.to
  -- Deduplicate edges (keep first occurrence of each from/to pair)
  let uniqueEdges := Id.run do
    let mut seen : Std.HashSet (String × String) := {}
    let mut result : Array Edge := #[]
    for e in validEdges do
      let key := (e.from_, e.to)
      if !seen.contains key then
        seen := seen.insert key
        result := result.push e
    return result
  -- Return graph without artificial connectivity edges - disconnected components
  -- are now visible and can be detected via findComponents for the Checks feature
  let graph := { nodes := state.nodes, edges := uniqueEdges }
  computeFullyProven graph  -- Post-process to compute fullyProven

end Builder

/-- Find connected components using BFS -/
def findComponents (g : Graph) : Array (Array String) := Id.run do
  let mut visited : Std.HashSet String := {}
  let mut components : Array (Array String) := #[]

  for node in g.nodes do
    if !visited.contains node.id then
      -- BFS from this node
      let mut component : Array String := #[]
      let mut queue : Array String := #[node.id]
      let mut queueIdx := 0
      while h : queueIdx < queue.size do
        let curr := queue[queueIdx]
        queueIdx := queueIdx + 1
        if !visited.contains curr then
          visited := visited.insert curr
          component := component.push curr
          -- Add neighbors (both directions since graph is directed but we want connectivity)
          for edge in g.edges do
            if edge.from_ == curr && !visited.contains edge.to then
              queue := queue.push edge.to
            if edge.to == curr && !visited.contains edge.from_ then
              queue := queue.push edge.from_
      components := components.push component
  return components

-- NOTE: ensureConnected was removed to avoid silently adding artificial edges
-- that hide potential issues in the formalization. The graph now reflects true
-- connectivity. Use findComponents above for detecting disconnected components
-- (will be used for the Checks feature).

/-- Detect cycles in the graph using DFS.
    Returns array of cycles, where each cycle is an array of node IDs.
    Uses standard DFS with coloring: white (unvisited), gray (in stack), black (finished).
    When a gray node is reached from another gray node, a cycle is found. -/
def detectCycles (g : Graph) : Array (Array String) := Id.run do
  -- Color states: 0 = white (unvisited), 1 = gray (in current path), 2 = black (finished)
  let mut color : Std.HashMap String Nat := {}
  let mut parent : Std.HashMap String String := {}
  let mut cycles : Array (Array String) := #[]

  -- Initialize all nodes as white
  for node in g.nodes do
    color := color.insert node.id 0

  -- Build adjacency list for outgoing edges
  let mut adjList : Std.HashMap String (Array String) := {}
  for node in g.nodes do
    adjList := adjList.insert node.id #[]
  for edge in g.edges do
    match adjList.get? edge.from_ with
    | some neighbors => adjList := adjList.insert edge.from_ (neighbors.push edge.to)
    | none => pure ()

  -- DFS from each unvisited node
  for startNode in g.nodes do
    if (color.get? startNode.id |>.getD 0) == 0 then
      -- Stack contains (nodeId, neighborIndex)
      let mut stack : Array (String × Nat) := #[(startNode.id, 0)]
      color := color.insert startNode.id 1  -- Mark as gray

      while h : stack.size > 0 do
        let (curr, idx) := stack[stack.size - 1]'(by omega)
        let neighbors := adjList.get? curr |>.getD #[]

        if idx < neighbors.size then
          -- Update the index for current node
          stack := stack.set! (stack.size - 1) (curr, idx + 1)
          let neighbor := neighbors[idx]!

          match color.get? neighbor |>.getD 0 with
          | 0 => -- White: unvisited, push to stack
            parent := parent.insert neighbor curr
            color := color.insert neighbor 1  -- Mark as gray
            stack := stack.push (neighbor, 0)
          | 1 => -- Gray: back edge found, we have a cycle
            -- Reconstruct the cycle
            let mut cycle : Array String := #[neighbor]
            let mut node := curr
            -- Walk back through the stack until we reach the neighbor
            while node != neighbor do
              cycle := cycle.push node
              match parent.get? node with
              | some p => node := p
              | none => break  -- Safety: shouldn't happen
            cycle := cycle.push neighbor  -- Complete the cycle
            cycles := cycles.push cycle.reverse
          | _ => -- Black: already fully processed, skip
            pure ()
        else
          -- Done with all neighbors, mark as black and pop
          color := color.insert curr 2
          stack := stack.pop

  return cycles

/-- Compute check results for a graph -/
def computeCheckResults (g : Graph) : CheckResults :=
  let components := findComponents g
  let componentSizes := components.map (·.size)
  let cycles := detectCycles g
  let keyDecls := g.nodes.filter (·.keyDeclaration)
  let kernelVerified := if keyDecls.isEmpty then none
    else some (keyDecls.all (·.status == .fullyProven))
  { isConnected := components.size <= 1
    numComponents := components.size
    componentSizes := componentSizes
    cycles := cycles
    kernelVerified := kernelVerified }

/-- Build a dependency graph from Dress blueprint nodes (without inferred uses - uses explicit labels only) -/
def fromNodes (nodes : Array Dress.NodeWithPos) : Graph :=
  Builder.buildGraph (nodes.map fun n => {
    node := n,
    hasSorry := false,
    statementUses := n.toNode.statement.usesLabels,
    proofUses := n.toNode.proof.map (·.usesLabels) |>.getD #[]
  })

/-- Build graph from the environment's blueprint extension -/
def fromEnvironment (env : Lean.Environment) : Lean.CoreM Graph := do
  let entries := Architect.blueprintExt.getState env |>.toList
  let nodesData ← entries.toArray.mapM fun (_, node) => do
    let dressNode ← Dress.toDressNodeWithPos node
    -- Infer uses from actual Lean code dependencies
    let (statementUses, proofUses) ← node.inferUses
    return Builder.NodeBuildData.mk dressNode (!proofUses.leanOk) statementUses.uses proofUses.uses
  return Builder.buildGraph nodesData

end Dress.Graph
