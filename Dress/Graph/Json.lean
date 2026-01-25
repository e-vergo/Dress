/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Batteries.Lean.Json
import Dress.Graph.Layout

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

/-- Convert NodeStatus to a string for JSON -/
def NodeStatus.toJsonString : NodeStatus → String
  | .stated => "stated"
  | .proved => "proved"
  | .notReady => "notReady"
  | .mathLibOk => "mathLibOk"

/-- JSON instance for NodeStatus -/
instance : ToJson NodeStatus where
  toJson s := Json.str s.toJsonString

/-- JSON instance for Node -/
instance : ToJson Node where
  toJson n := Json.mkObj [
    ("id", Json.str n.id),
    ("label", Json.str n.label),
    ("envType", Json.str n.envType),
    ("status", toJson n.status),
    ("url", Json.str n.url),
    ("leanDecls", Json.arr (n.leanDecls.map (Json.str ∘ toString)))
  ]

/-- JSON instance for Edge -/
instance : ToJson Edge where
  toJson e := Json.mkObj [
    ("from", Json.str e.from_),
    ("to", Json.str e.to)
  ]

/-- JSON instance for Graph (without layout) -/
instance : ToJson Graph where
  toJson g := Json.mkObj [
    ("nodes", Json.arr (g.nodes.map toJson)),
    ("edges", Json.arr (g.edges.map toJson))
  ]

namespace Layout

/-- JSON instance for LayoutNode -/
instance : ToJson LayoutNode where
  toJson ln := Json.mkObj [
    ("id", Json.str ln.node.id),
    ("label", Json.str ln.node.label),
    ("envType", Json.str ln.node.envType),
    ("status", toJson ln.node.status),
    ("url", Json.str ln.node.url),
    ("leanDecls", Json.arr (ln.node.leanDecls.map (Json.str ∘ toString))),
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
    ("points", Json.arr (le.points.map pointToJson))
  ]

/-- JSON instance for LayoutGraph -/
instance : ToJson LayoutGraph where
  toJson lg := Json.mkObj [
    ("nodes", Json.arr (lg.nodes.map toJson)),
    ("edges", Json.arr (lg.edges.map toJson)),
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
