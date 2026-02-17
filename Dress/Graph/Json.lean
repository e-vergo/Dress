/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Batteries.Lean.Json
import Dress.Graph.Layout
import Dress.Graph.Types

/-!
# JSON Serialization for Dependency Graphs

Generates D3.js-compatible JSON output from laid-out dependency graphs.

## Output Format

```json
{
  "nodes": [
    {
      "id": "thm-foo",
      "label": "Theorem 1",
      "envType": "theorem",
      "status": "proved",
      "url": "#thm-foo",
      "leanDecls": ["MyProject.foo"],
      "x": 100.0,
      "y": 50.0,
      "width": 100.0,
      "height": 40.0
    }
  ],
  "edges": [
    {
      "from": "thm-foo",
      "to": "lem-bar",
      "points": [[100.0, 90.0], [100.0, 110.0]]
    }
  ],
  "width": 500.0,
  "height": 300.0
}
```
-/

namespace Dress.Graph

open Lean

-- NodeStatus is re-exported from Architect.NodeStatus which already has ToJson instance

instance : ToJson NodeShape where
  toJson
    | .box => "box"
    | .ellipse => "ellipse"
    | .diamond => "diamond"

instance : ToJson EdgeStyle where
  toJson
    | .solid => "solid"
    | .dashed => "dashed"

/-- JSON instance for Node -/
instance : ToJson Node where
  toJson n := Json.mkObj [
    ("id", Json.str n.id),
    ("label", Json.str n.label),
    ("envType", Json.str n.envType),
    ("status", toJson n.status),
    ("shape", toJson n.shape),
    ("url", Json.str n.url),
    ("leanDecls", Json.arr (n.leanDecls.map (Json.str ∘ toString))),
    ("moduleName", Json.str n.moduleName.toString),
    ("keyDeclaration", toJson n.keyDeclaration),
    ("message", toJson n.message),
    ("priorityItem", toJson n.priorityItem),
    ("blocked", toJson n.blocked),
    ("potentialIssue", toJson n.potentialIssue),
    ("technicalDebt", toJson n.technicalDebt),
    ("misc", toJson n.misc)
  ]

/-- JSON instance for Edge -/
instance : ToJson Edge where
  toJson e := Json.mkObj [
    ("from", Json.str e.from_),
    ("to", Json.str e.to),
    ("style", toJson e.style)
  ]

/-- JSON instance for Graph (without layout) -/
instance : ToJson Graph where
  toJson g := Json.mkObj [
    ("nodes", Json.arr (g.nodes.map toJson)),
    ("edges", Json.arr (g.edges.map toJson))
  ]

instance : ToJson CoverageResult where
  toJson c := Json.mkObj [
    ("totalDeclarations", toJson c.totalDeclarations),
    ("coveredDeclarations", toJson c.coveredDeclarations),
    ("coveragePercent", toJson c.coveragePercent),
    ("uncovered", toJson c.uncovered)
  ]

instance : FromJson CoverageResult where
  fromJson? j := do
    let totalDeclarations ← j.getObjValAs? Nat "totalDeclarations"
    let coveredDeclarations ← j.getObjValAs? Nat "coveredDeclarations"
    let coveragePercent ← j.getObjValAs? Float "coveragePercent"
    let uncovered ← j.getObjValAs? (Array UncoveredDecl) "uncovered"
    return { totalDeclarations, coveredDeclarations, coveragePercent, uncovered }

/-- JSON instance for CheckResults -/
instance : ToJson CheckResults where
  toJson c := Json.mkObj [
    ("isConnected", .bool c.isConnected),
    ("numComponents", .num c.numComponents),
    ("componentSizes", toJson c.componentSizes),
    ("cycles", toJson c.cycles),
    ("kernelVerified", toJson c.kernelVerified),
    ("soundnessResults", toJson c.soundnessResults),
    ("coverage", toJson c.coverage),
    ("axiomTracking", toJson c.axiomTracking)
  ]

/-- FromJson instance for CheckResults (backward compatible with legacy manifests) -/
instance : FromJson CheckResults where
  fromJson? j := do
    let isConnected ← j.getObjValAs? Bool "isConnected"
    let numComponents ← j.getObjValAs? Nat "numComponents"
    let componentSizes ← j.getObjValAs? (Array Nat) "componentSizes"
    let cycles ← j.getObjValAs? (Array (Array String)) "cycles"
    -- Optional fields: absent or null → default
    let kernelVerified : Option Bool :=
      match j.getObjValAs? Bool "kernelVerified" with
      | .ok v => some v
      | .error _ => none
    let soundnessResults : Array SoundnessResult :=
      match j.getObjValAs? (Array SoundnessResult) "soundnessResults" with
      | .ok v => v
      | .error _ => #[]
    let coverage : Option CoverageResult :=
      match j.getObjValAs? CoverageResult "coverage" with
      | .ok v => some v
      | .error _ => none
    let axiomTracking : Option AxiomResult :=
      match j.getObjValAs? AxiomResult "axiomTracking" with
      | .ok v => some v
      | .error _ => none
    return {
      isConnected, numComponents, componentSizes, cycles,
      kernelVerified, soundnessResults, coverage, axiomTracking
    }

namespace Layout

/-- JSON instance for LayoutNode -/
instance : ToJson LayoutNode where
  toJson ln := Json.mkObj [
    ("id", Json.str ln.node.id),
    ("label", Json.str ln.node.label),
    ("envType", Json.str ln.node.envType),
    ("status", toJson ln.node.status),
    ("shape", toJson ln.node.shape),
    ("url", Json.str ln.node.url),
    ("leanDecls", Json.arr (ln.node.leanDecls.map (Json.str ∘ toString))),
    ("moduleName", Json.str ln.node.moduleName.toString),
    ("keyDeclaration", toJson ln.node.keyDeclaration),
    ("message", toJson ln.node.message),
    ("priorityItem", toJson ln.node.priorityItem),
    ("blocked", toJson ln.node.blocked),
    ("potentialIssue", toJson ln.node.potentialIssue),
    ("technicalDebt", toJson ln.node.technicalDebt),
    ("misc", toJson ln.node.misc),
    ("x", toJson ln.x),
    ("y", toJson ln.y),
    ("width", toJson ln.width),
    ("height", toJson ln.height)
  ]

/-- Convert a point to JSON array -/
private def pointToJson (p : Float × Float) : Json :=
  Json.arr #[toJson p.1, toJson p.2]

/-- JSON instance for LayoutEdge -/
instance : ToJson LayoutEdge where
  toJson le := Json.mkObj [
    ("from", Json.str le.from_),
    ("to", Json.str le.to),
    ("points", Json.arr (le.points.map pointToJson)),
    ("style", toJson le.style)
  ]

/-- JSON instance for LayoutGraph -/
instance : ToJson LayoutGraph where
  toJson lg := Id.run do
    -- Build adjacency lists from edges
    let mut predMap : Std.HashMap String (Array String) := {}
    let mut succMap : Std.HashMap String (Array String) := {}
    -- Initialize
    for node in lg.nodes do
      predMap := predMap.insert node.node.id #[]
      succMap := succMap.insert node.node.id #[]
    -- Populate: edge from_ -> to means "to depends on from_"
    for edge in lg.edges do
      match succMap.get? edge.from_ with
      | some arr => succMap := succMap.insert edge.from_ (arr.push edge.to)
      | none => pure ()
      match predMap.get? edge.to with
      | some arr => predMap := predMap.insert edge.to (arr.push edge.from_)
      | none => pure ()
    -- Build adjacency JSON
    let adjEntries := lg.nodes.map fun node =>
      (node.node.id, Json.mkObj [
        ("predecessors", toJson (predMap.get? node.node.id |>.getD #[])),
        ("successors", toJson (succMap.get? node.node.id |>.getD #[]))
      ])
    let adjacencyObj := Json.mkObj adjEntries.toList
    return Json.mkObj [
      ("nodes", Json.arr (lg.nodes.map toJson)),
      ("edges", Json.arr (lg.edges.map toJson)),
      ("adjacency", adjacencyObj),
      ("width", toJson lg.width),
      ("height", toJson lg.height)
    ]

end Layout

/-- Write a layout graph to a JSON file -/
def writeJsonFile (graph : Layout.LayoutGraph) (path : System.FilePath) : IO Unit := do
  let json := toJson graph
  let content := json.pretty
  IO.FS.writeFile path content

/-- Write a layout graph to a compact JSON file (no pretty printing) -/
def writeJsonFileCompact (graph : Layout.LayoutGraph) (path : System.FilePath) : IO Unit := do
  let json := toJson graph
  let content := json.compress
  IO.FS.writeFile path content

end Dress.Graph
