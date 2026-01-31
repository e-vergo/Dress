# Dress

Artifact generation for Lean 4 mathematical blueprints. Transforms `@[blueprint]`-decorated declarations into syntax-highlighted HTML and LaTeX with interactive hovers, and builds dependency graphs with hierarchical layout.

## Overview

Dress is the artifact generation layer of the [Side-by-Side Blueprint](https://github.com/e-vergo/Side-By-Side-Blueprint) formalization documentation toolchain. It:

1. Intercepts `@[blueprint]` declarations during Lean elaboration
2. Captures syntax highlighting via [SubVerso](https://github.com/e-vergo/subverso) while info trees are still available
3. Renders HTML with rainbow bracket matching via [Verso](https://github.com/e-vergo/verso)
4. Generates LaTeX with embedded hover data
5. Builds dependency graphs with Sugiyama hierarchical layout
6. Validates graph structure (connectivity, cycles)
7. Computes status counts and `fullyProven` upgrades

Dress re-exports [LeanArchitect](https://github.com/e-vergo/LeanArchitect), so importing Dress provides the `@[blueprint]` attribute.

## Dependency Chain

```
SubVerso -> LeanArchitect -> Dress -> Runway
```

| Component | Role |
|-----------|------|
| [SubVerso](https://github.com/e-vergo/subverso) | Extracts syntax highlighting with O(1) indexed lookups via InfoTable |
| [LeanArchitect](https://github.com/e-vergo/LeanArchitect) | Defines `@[blueprint]` attribute with 8 metadata and 3 manual status options |
| Dress | Generates artifacts, computes statistics, validates graphs, performs Sugiyama layout |
| [Runway](https://github.com/e-vergo/Runway) | Consumes Dress output to produce the final website, dashboard, and paper/PDF |

## Installation

Add to `lakefile.lean`:

```lean
require Dress from git "https://github.com/e-vergo/Dress" @ "main"
```

For local development:

```lean
require Dress from ".." / "Dress"
```

## Usage

### 1. Mark Declarations

```lean
import Dress  -- Re-exports @[blueprint] from LeanArchitect

@[blueprint "thm:main"]
theorem mainTheorem : 2 + 2 = 4 := rfl

@[blueprint "thm:key" (keyDeclaration := true, message := "Central result")]
theorem keyTheorem : P := by sorry

@[blueprint "def:square" (title := "Square Function")]
def square (n : Nat) : Nat := n * n
```

### 2. Build with Artifact Generation

Enable dress mode via environment variable:

```bash
BLUEPRINT_DRESS=1 lake build
```

Or via marker file:

```bash
lake run dress
```

### 3. Generate Lake Facets

```bash
lake build :blueprint                   # All modules
lake build MyProject.MyModule:blueprint # Specific module
```

### 4. Generate Dependency Graph and Manifest

```bash
lake exe extract_blueprint graph Module1 Module2 Module3
```

## Build Pipeline

Dress operates in three phases during the build process.

### Phase 1: Per-Declaration Capture (During Elaboration)

When Lean compiles with `BLUEPRINT_DRESS=1`, the `elab_rules` in `Capture/ElabRules.lean` intercept each `@[blueprint]` declaration:

1. Standard elaboration runs first (the hook calls `elabCommandTopLevel`)
2. SubVerso highlighting is captured via `highlightIncludingUnparsed`
3. Code splitting separates signature from proof body at the `:=` boundary
4. Artifacts are written to `.lake/build/dressed/{Module/Path}/{sanitized-label}/`

Info trees are ephemeral and only exist during elaboration, so highlighting must be captured immediately. This accounts for 93-99% of build time.

### Phase 2: Lake Facet Aggregation

After compilation, Lake facets aggregate per-declaration artifacts:

| Facet | Level | Output |
|-------|-------|--------|
| `dressed` | Module | `module.json` aggregating all declarations |
| `blueprint` | Module | `module.tex` with `\input{}` directives |
| `blueprint` | Library | `library/{LibName}.tex` index with `\inputleanmodule` macro |
| `depGraph` | Library | `dep-graph.svg` and `dep-graph.json` |

### Phase 3: Manifest Generation

The `graph` subcommand performs final processing:

1. Loads modules and extracts blueprint nodes from the environment
2. Infers dependencies from Lean code via `Node.inferUses`
3. **Two-pass edge processing:**
   - PASS 1: Register all labels and create nodes (so back-references work)
   - PASS 2: Add all edges (now targets exist for back-edges)
   - Edge deduplication removes duplicate (from, to) pairs
4. Validates the graph (connectivity, cycle detection)
5. Computes status counts and upgrades nodes to `fullyProven`
6. Applies transitive reduction (skipped for >100 nodes)
7. Runs Sugiyama layout for hierarchical visualization
8. Writes `dep-graph.svg`, `dep-graph.json`, and `manifest.json`

## Artifact Format

### Per-Declaration Artifacts

Written to `.lake/build/dressed/{Module/Path}/{sanitized-label}/`:

| File | Description |
|------|-------------|
| `decl.tex` | LaTeX source for the declaration |
| `decl.html` | Pre-rendered HTML with hover spans and rainbow brackets via `toHtmlRainbow` |
| `decl.json` | Metadata: `{"name": "...", "label": "...", "highlighting": {...}}` |
| `decl.hovers.json` | Hover tooltip content for interactive display (JSON mapping IDs to content) |
| `manifest.entry` | Label-to-path mapping: `{"label": "...", "path": "..."}` |

Timing data is available via `trace[blueprint.timing]` for performance analysis.

### Module-Level Artifacts

| File | Location | Description |
|------|----------|-------------|
| `module.json` | `.lake/build/dressed/{Module/Path}/` | Aggregated declaration data |
| `module.tex` | `.lake/build/dressed/{Module/Path}/` | `\input{}` directives for each declaration |

### Library-Level Artifacts

| File | Location | Description |
|------|----------|-------------|
| `{LibName}.tex` | `.lake/build/dressed/library/` | Library index with `\inputleanmodule` macro |
| `dep-graph.svg` | `.lake/build/dressed/` | Dependency graph SVG visualization |
| `dep-graph.json` | `.lake/build/dressed/` | Layout data for interactive rendering |
| `manifest.json` | `.lake/build/dressed/` | Stats, validation, metadata for Runway |

## Manifest Schema

The `manifest.json` file contains precomputed data consumed by Runway:

```json
{
  "stats": {
    "notReady": 2,
    "ready": 3,
    "hasSorry": 2,
    "proven": 5,
    "fullyProven": 8,
    "mathlibReady": 1,
    "total": 21
  },
  "keyDeclarations": ["thm:main", "thm:secondary"],
  "messages": [
    {"id": "thm:main", "label": "Main Theorem", "message": "Central result"}
  ],
  "projectNotes": {
    "priority": [{"id": "lem:urgent", "label": "Urgent Lemma"}],
    "blocked": [{"id": "thm:blocked", "label": "...", "reason": "Waiting for PR"}],
    "potentialIssues": [{"id": "lem:issue", "label": "...", "issue": "..."}],
    "technicalDebt": [{"id": "def:ugly", "label": "...", "debt": "..."}],
    "misc": [{"id": "thm:note", "label": "...", "note": "..."}]
  },
  "nodes": {"thm:main": "#thm:main", "thm:secondary": "#thm:secondary"},
  "checks": {
    "isConnected": true,
    "numComponents": 1,
    "componentSizes": [21],
    "cycles": []
  }
}
```

Statistics are computed upstream in Dress. Runway loads `manifest.json` without recomputation, providing a soundness guarantee that displayed statistics match the actual graph state.

## 6-Status Model

Node status types are defined in LeanArchitect and re-exported by Dress:

| Status | Color | Hex | Source |
|--------|-------|-----|--------|
| `notReady` | Sandy Brown | #F4A460 | Default or manual `(notReady := true)` |
| `ready` | Light Sea Green | #20B2AA | Manual `(ready := true)` |
| `sorry` | Dark Red | #8B0000 | Auto-detected: proof contains `sorryAx` |
| `proven` | Light Green | #90EE90 | Auto-detected: complete proof |
| `fullyProven` | Forest Green | #228B22 | Auto-computed: all ancestors proven |
| `mathlibReady` | Light Blue | #87CEEB | Manual `(mathlibReady := true)` |

**Status priority** (manual flags take precedence):
1. `mathlibReady` (manual)
2. `ready` (manual)
3. `notReady` (manual, if explicitly set)
4. `fullyProven` (auto-computed)
5. `sorry` (auto-detected)
6. `proven` (auto-detected)
7. `notReady` (default)

### Node Shapes

| Shape | Used For |
|-------|----------|
| Rectangle (box) | def, abbrev, structure, class, instance |
| Ellipse | theorem, lemma, proposition, corollary, example |

### Edge Styles

| Style | Meaning |
|-------|---------|
| Solid | Proof dependency (used in proof body) |
| Dashed | Statement dependency (used in signature) |

## Graph Layout Algorithm

Dress implements a Sugiyama-style hierarchical layout algorithm in `Graph/Layout.lean` (~1500 lines).

### Algorithm Phases

#### 1. Acyclic Transformation

The graph is made acyclic before layout using the Graphviz approach:

1. DFS identifies back-edges (edges from a node to an ancestor in the DFS tree)
2. Back-edges are reversed **one at a time** with iteration until no cycles remain
3. Reversed edges are marked with `isReversed := true` for correct arrow direction in SVG
4. Safety bound: maximum iterations = `graph.edges.size + 1`

The `reverseBezierPoints` function handles reversing Bezier control points so arrows point correctly.

#### 2. Layer Assignment

Longest-path algorithm assigns nodes to vertical layers:

- Nodes with no incoming edges are placed at layer 0
- Each node is placed one layer above its highest dependency
- Produces valid topological ordering for acyclic graphs

#### 3. Crossing Reduction

Median heuristic minimizes edge crossings:

- For each layer, compute median position of connected nodes in adjacent layer
- Reorder layer by median values
- Alternate forward and backward passes
- Transpose pass: iteratively swap adjacent nodes if it reduces crossings

#### 4. Coordinate Assignment

Barycenter-based positioning with refinement:

- Initial placement on a grid centered by layer width
- Iterative refinement pulls nodes toward median of connected neighbors
- Overlap resolution prevents node collisions

#### 5. Edge Routing

For small graphs (<=100 nodes):

1. Build visibility graph from node corners (rectangles) or octant points (ellipses)
2. Use Liang-Barsky for rectangle intersection, parametric approach for ellipse intersection
3. Find shortest path using O(V^2) Dijkstra's algorithm
4. Convert polyline to smooth cubic Bezier curves via Catmull-Rom interpolation
5. Clip endpoints to node boundaries based on shape

For large graphs (>100 nodes), simplified direct Bezier curves are used with offset-based control points for gentle arcs.

#### 6. Coordinate Normalization

Final coordinates are normalized so content starts at (padding, padding) with viewBox origin at (0, 0):

```lean
let minX := nodes.foldl (fun acc n => min acc n.x) Float.inf
let minY := nodes.foldl (fun acc n => min acc n.y) Float.inf
let offsetX := padding - minX
let offsetY := padding - minY
-- All nodes shifted by (offsetX, offsetY)
```

This normalization is required for proper SVG centering because JavaScript's `getBBox()` expects the viewBox origin to be (0, 0).

### Performance Characteristics

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Layer assignment | O(V+E) | BFS from source nodes |
| Crossing reduction (barycenter) | O(n^2) normal, O(n) with iteration limit | Reduced to 2 iterations for >100 nodes |
| Transpose heuristic | O(L x N x swaps) | Skipped for >100 nodes |
| Edge routing (visibility graph) | O(V^2) per edge | Skipped for >100 nodes |
| Transitive reduction | O(n^3) Floyd-Warshall | Skipped for >100 nodes |

**>100 node optimizations** (automatic, triggered at `g.nodes.size > 100`):
- Max 2 barycenter iterations (vs 4) - in `orderLayers`
- Skip transpose heuristic - O(L x N x swaps) per call
- Skip visibility graph routing - O(V^2) per edge
- Skip transitive reduction - O(n^3) Floyd-Warshall
- Use simplified direct Bezier curves instead

These thresholds allow PNT (591 nodes) to render in ~15 seconds while maintaining quality for smaller graphs like GCR (57 nodes) and SBS-Test (33 nodes).

**Expected layout times:**

| Scale | Nodes | Layout Time |
|-------|-------|-------------|
| Small | <50 | <1s |
| Medium | 50-100 | 1-3s |
| Large | >100 | 5-20s |

## Validation Checks

### Connectivity (`findComponents`)

BFS-based component detection with O(V+E) complexity:

```lean
def findComponents (g : Graph) : Array (Array String)
```

- Treats the graph as undirected for connectivity purposes
- Single component indicates a fully connected graph
- Multiple components may indicate missing dependencies or orphaned declarations
- Results stored in `manifest.json` under `checks.numComponents` and `checks.componentSizes`

### Cycle Detection (`detectCycles`)

DFS with gray/black coloring (O(V+E) complexity):

```lean
def detectCycles (g : Graph) : Array (Array String)
```

- White nodes: unvisited
- Gray nodes: in the current DFS path
- Black nodes: fully processed
- Back-edge to gray node indicates cycle
- Returns array of detected cycles (each cycle is array of node IDs)
- Results stored in `manifest.json` under `checks.cycles`

### Fully Proven Computation (`computeFullyProven`)

Post-processing step with O(V+E) complexity via memoization:

```lean
def computeFullyProven (g : Graph) : Graph
```

Uses an iterative worklist algorithm instead of recursion:
1. Build dependency map: for each edge (A, B), B depends on A
2. For each node, DFS through dependencies using a stack
3. Memoize results to avoid recomputation
4. Detect and handle cycles (mark as incomplete)

A node is upgraded to `fullyProven` if:
1. Its status is `proven` (has Lean code without sorryAx)
2. All its ancestors are `proven` or `fullyProven`

This provides stronger verification guarantees than `proven` alone - it means the entire proof tree from axioms to this node is complete.

## Rainbow Bracket Highlighting

Dress uses Verso's `toHtmlRainbow` for bracket highlighting. The `HtmlRender.lean` module wraps Verso's highlighting functions using `Genre.none` to avoid Verso's full document infrastructure:

```lean
def renderHighlightedWithHovers (hl : Highlighted) : String × String :=
  let (html, finalState) := (hl.toHtmlRainbow).run defaultContext |>.run .empty
  let hoverJson := finalState.dedup.docJson.compress
  (html.asString (breakLines := false), hoverJson)
```

Additional rendering modes:
- `renderHighlightedBlock`: Wraps in `class="hl lean block"` using `blockHtmlRainbow`
- `renderHighlightedInline`: Wraps in `class="hl lean inline"` using `inlineHtmlRainbow`
- `renderHighlightedWithState`: Allows chaining renders with continuous hover ID numbering

The output uses CSS classes `lean-bracket-1` through `lean-bracket-6` for six distinct depth colors:

| Class | Light Mode | Dark Mode |
|-------|------------|-----------|
| `lean-bracket-1` | #d000ff | #e040ff |
| `lean-bracket-2` | #5126ff | #7156ff |
| `lean-bracket-3` | #0184BC | #01a4dc |
| `lean-bracket-4` | #4078F2 | #5098ff |
| `lean-bracket-5` | #50A14F | #70c16f |
| `lean-bracket-6` | #E45649 | #f47669 |

## Module Structure

```
Dress/
  Capture/
    ElabRules.lean     # elab_rules hooks for @[blueprint] declarations
    InfoTree.lean      # Environment extension for captured highlighting
    State.lean         # IO.Ref state for capture coordination
    Config.lean        # Blueprint configuration parsing

  Generate/
    Declaration.lean   # Per-declaration artifact writer
    Latex.lean         # LaTeX generation for declarations
    Module.lean        # Module-level utilities

  Graph/
    Types.lean         # Node, Edge, Graph, StatusCounts, CheckResults
    Build.lean         # Graph construction, validation, computeFullyProven
    Layout.lean        # Sugiyama algorithm, edge routing (~1500 lines)
    Json.lean          # JSON serialization for D3.js
    Svg.lean           # SVG generation

  Render/
    SideBySide.lean    # Side-by-side display rendering

  Serialize/
    Json.lean          # SubVerso JSON serialization
    Html.lean          # HTML serialization
    Artifacts.lean     # Dressed artifact format

  Base64.lean          # RFC 4648 Base64 encoding
  Content.lean         # BlueprintContent type for module contents
  Core.lean            # Core types, splitAtDefinitionAssign
  Highlighting.lean    # SubVerso highlighting integration
  HtmlRender.lean      # Verso HTML rendering wrapper
  Hook.lean            # Main entry point, re-exports
  Load.lean            # Loading nodes from environment
  Output.lean          # LaTeX and JSON output functions
  Paths.lean           # Path utilities for artifact locations
  Render.lean          # Rendering utilities
  SubVersoExtract.lean # SubVerso extraction utilities

Main.lean              # CLI executable (extract_blueprint)
lakefile.lean          # Package definition with Lake facets
```

## CLI Reference

The `extract_blueprint` executable provides three subcommands:

### graph (Primary)

Generate dependency graph and manifest:

```bash
lake exe extract_blueprint graph --build .lake/build Module1 Module2
```

Outputs `dep-graph.svg`, `dep-graph.json`, and `manifest.json` to `.lake/build/dressed/`.

### single (Deprecated)

Extract blueprint for a single module:

```bash
lake exe extract_blueprint single --build .lake/build MyModule
```

Use `lake build MyModule:blueprint` instead.

### index (Deprecated)

Collate modules into library index:

```bash
lake exe extract_blueprint index --build .lake/build MyLib Module1 Module2
```

Use `lake build :blueprint` instead.

## Integration with Runway

Dress artifacts are consumed by [Runway](https://github.com/e-vergo/Runway):

1. Build project with Dress (`BLUEPRINT_DRESS=1 lake build`)
2. Generate facets (`lake build :blueprint`)
3. Generate graph (`lake exe extract_blueprint graph ...`)
4. Configure `runway.json` with paths to artifacts
5. Generate site (`lake exe runway build runway.json`)

Runway loads:
- Per-declaration artifacts from `.lake/build/dressed/{Module/Path}/`
- `manifest.json` for dashboard data (precomputed, no recomputation)
- `dep-graph.json` and `dep-graph.svg` for visualization

## Dependencies

| Dependency | Purpose |
|------------|---------|
| [LeanArchitect](https://github.com/e-vergo/LeanArchitect) | `@[blueprint]` attribute definition |
| [SubVerso](https://github.com/e-vergo/subverso) | Syntax highlighting extraction with O(1) indexed lookups |
| [Verso](https://github.com/e-vergo/verso) | HTML rendering with rainbow bracket matching |
| [Cli](https://github.com/mhuisi/lean4-cli) | Command-line interface |

## Related Repositories

| Repository | Purpose |
|------------|---------|
| [Runway](https://github.com/e-vergo/Runway) | Site generator (downstream) |
| [LeanArchitect](https://github.com/e-vergo/LeanArchitect) | Blueprint attribute (upstream) |
| [SubVerso](https://github.com/e-vergo/subverso) | Syntax highlighting (upstream) |
| [SBS-Test](https://github.com/e-vergo/SBS-Test) | Minimal test project (33 nodes, all 6 statuses) |
| [dress-blueprint-action](https://github.com/e-vergo/dress-blueprint-action) | GitHub Actions CI solution |

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.
