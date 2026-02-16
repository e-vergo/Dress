# Graph

Dependency graph construction, validation, Sugiyama hierarchical layout, and output generation (SVG + JSON).

## Files

| File | Size | Purpose |
|------|------|---------|
| `Types.lean` | ~9 KB | Core types: `Node`, `Edge`, `Graph`, `StatusCounts`, `CheckResults`, `CoverageResult`, `AdjIndex` |
| `Build.lean` | ~19 KB | Graph construction from `Architect.Node`s, validation (`findComponents`, `detectCycles`), `computeFullyProven`, coverage |
| `Layout.lean` | ~59 KB | Sugiyama layout algorithm: layer assignment, crossing reduction, coordinate assignment, edge routing |
| `Svg.lean` | ~16 KB | SVG rendering with 7-status color model, node shapes, edge paths, legend |
| `Json.lean` | ~8 KB | JSON serialization for `Graph`, `LayoutGraph`, `CheckResults`, `CoverageResult` |
| `Subgraph.lean` | ~11 KB | BFS-based subgraph extraction with incremental depth levels and max-depth computation via topological sort |

## Graph Construction Pipeline

```
Architect.blueprintExt (environment)
    |
    v
Graph.fromEnvironment
    |-- For each blueprint node:
    |   |-- Node.inferUses (statement + proof dependency inference)
    |   |-- Builder.getStatus (manual flags + auto-derive from Lean)
    |   +-- Builder.getShape (envType -> box/ellipse/diamond)
    |
    +-- Builder.buildGraph (two-pass):
        |-- PASS 1: Register all labels, create nodes
        |-- PASS 2: Add edges (targets now exist for back-references)
        |-- Edge deduplication
        +-- computeFullyProven (post-process)
```

## Key Types

### Types.lean

- **`Node`** -- Graph node with `id`, `label`, `envType`, `status`, `shape`, `url`, `leanDecls`, `moduleName`, and all 8 metadata fields from LeanArchitect
- **`Edge`** -- Directed edge with `from_`, `to`, `style` (solid/dashed), `isReversed` flag
- **`Graph`** -- `Array Node` + `Array Edge`
- **`AdjIndex`** -- Precomputed O(1) adjacency index (`outgoing`/`incoming` HashMaps)
- **`NodeShape`** -- `box` (definitions), `ellipse` (theorems), `diamond` (axioms)
- **`EdgeStyle`** -- `solid` (proof dependencies), `dashed` (statement dependencies)
- **`StatusCounts`** -- Counts per status plus `numAxioms` and `total`
- **`CheckResults`** -- Connectivity, cycle detection, kernel verification, soundness, coverage
- **`CoverageResult`** -- Blueprint coverage: total declarations, covered count, percentage, uncovered list

### Build.lean

- **`BuilderState`** -- Accumulates nodes, edges, counters, and label-to-ID mapping during construction
- **`Builder.getStatus`** -- Status priority: mathlibReady > ready > explicit notReady > proven (if Lean, no sorry) > sorry (if Lean, has sorry) > notReady (default)
- **`Builder.registerNode`** (PASS 1) -- Creates graph node from `Dress.NodeWithPos`, registers label
- **`Builder.addNodeEdges`** (PASS 2) -- Adds statement edges (dashed) and proof edges (solid)
- **`computeFullyProven`** -- Iterative worklist algorithm with memoization. A node is upgraded to `fullyProven` if it is `proven` AND all ancestors are `proven` or `fullyProven`. O(V+E) via `AdjIndex`.
- **`findComponents`** -- BFS treating graph as undirected. O(V+E). Reports disconnected subgraphs.
- **`detectCycles`** -- DFS with white/gray/black coloring. O(V+E). Back-edge to gray node indicates cycle.
- **`computeCoverage`** -- Enumerates project-local declarations and checks `@[blueprint]` annotation coverage. Filters auto-generated names and projections.
- **`Graph.transitiveReduction`** -- DFS-based: for each edge (u,v), checks if v is reachable via alternative path. Skipped for >500 nodes.

## Sugiyama Layout Algorithm (Layout.lean)

### Algorithm Phases

1. **Acyclic transformation** -- DFS identifies back-edges, reverses them one at a time. Reversed edges get `isReversed := true` for correct arrow direction.

2. **Layer assignment** (`assignLayers`) -- Longest-path from sources. Nodes with no incoming edges go to layer 0; each node placed one layer above its highest dependency.

3. **Crossing reduction** (`orderLayers`) -- Median heuristic alternating forward/backward passes, followed by transpose pass that swaps adjacent nodes if it reduces crossings.

4. **Coordinate assignment** (`assignXCoordinates`) -- Grid-based initial placement centered by layer width, with iterative barycenter refinement pulling nodes toward connected neighbors. Overlap resolution prevents collisions.

5. **Edge routing** (`createLayoutEdges`) -- For small graphs: visibility graph from node corners/octant points, Dijkstra shortest path, Catmull-Rom to cubic Bezier conversion. For large graphs: simplified direct Bezier curves.

6. **Coordinate normalization** -- Shifts all coordinates so content starts at `(padding, padding)`. Required for proper SVG centering since JavaScript `getBBox()` expects viewBox origin at (0,0).

### Performance Thresholds (>100 nodes)

| Optimization | Normal | >100 nodes |
|--------------|--------|------------|
| Barycenter iterations | 4 | 2 |
| Transpose heuristic | Yes | Skipped |
| Visibility graph routing | Yes | Skipped (direct Bezier) |

These allow PNT (591 nodes) to render in ~15 seconds while maintaining quality for GCR (57 nodes) and SBS-Test (33 nodes).

### Layout Types

- **`LayoutNode`** -- `Node` + `x`, `y`, `width`, `height`
- **`LayoutEdge`** -- `from_`, `to`, control `points` array, `style`, `isReversed`
- **`LayoutGraph`** -- `Array LayoutNode` + `Array LayoutEdge` + `width`, `height`, `minX`, `minY`
- **`LayoutConfig`** -- `nodeWidth` (100), `nodeHeight` (40), `layerGap` (100), `nodeGap` (20), `padding` (20), `charWidth` (8), `barycenterIterations` (4)

## SVG Generation (Svg.lean)

### Status Color Model

| Status | Color | Hex |
|--------|-------|-----|
| notReady | Vivid Orange | #E8820C |
| wip | Deep Teal | #0097A7 |
| sorry | Vivid Red | #C62828 |
| proven | Medium Green | #66BB6A |
| fullyProven | Deep Forest Green | #1B5E20 |
| mathlibReady | Vivid Blue | #42A5F5 |
| axiom | Vivid Purple | #7E57C2 |

These are the canonical color values. CSS variables must match.

### Node Rendering

- **Ellipse** for theorems, lemmas, propositions
- **Rectangle** (rounded corners) for definitions, structures, classes
- **Diamond** for axioms
- Dotted border (`stroke-dasharray="2,2"`) for manually-tagged status nodes
- Base node highlighting (blue border + glow filter) for focal node in subgraph views

### Edge Rendering

- Cubic Bezier paths (`C` commands) from layout control points
- `stroke-dasharray="5,3"` for dashed edges (statement dependencies)
- `marker-end="url(#arrowhead)"` for directional arrows
- `reverseBezierPoints` handles reversed back-edges

## Subgraph Extraction (Subgraph.lean)

- **`extractSubgraph`** -- BFS from center node in specified direction (ancestors/descendants/both) up to depth limit
- **`extractSubgraphsIncremental`** -- Single BFS recording depth per node, then materializes subgraphs at each depth level. More efficient than separate calls.
- **`computeAllMaxDepths`** -- O(V+E) via Kahn's topological sort + forward/backward passes. Computes ancestor and descendant depth for every node.
- **`depthMetadataToJson`** -- Serializes per-node depth info for the JavaScript UI

## JSON Output (Json.lean)

`LayoutGraph` serializes to D3.js-compatible format with:
- `nodes` array with position (`x`, `y`, `width`, `height`) and all node metadata
- `edges` array with `from`, `to`, control `points`, and `style`
- `adjacency` object mapping each node ID to `predecessors` and `successors` arrays
- `width` and `height` of the bounding box

## Connection to Adjacent Stages

- **Upstream**: `Graph.fromEnvironment` reads from `Architect.blueprintExt`. Node status derived from `Architect.Node` fields.
- **Downstream**: `Main.lean` orchestrates the full pipeline: `fromEnvironment` -> `transitiveReduction` -> `layout` -> SVG/JSON/manifest output. Runway loads `manifest.json`, `dep-graph.svg`, `dep-graph.json`.
