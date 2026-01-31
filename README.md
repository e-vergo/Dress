# Dress

Artifact generation for Lean 4 mathematical blueprints. Transforms `@[blueprint]`-decorated declarations into syntax-highlighted HTML and LaTeX with interactive hovers, proof toggles, and dependency graph visualization.

## Overview

Dress is the artifact generation layer of the [Side-by-Side Blueprint](https://github.com/e-vergo/Side-By-Side-Blueprint) toolchain. It intercepts declarations during Lean elaboration to capture syntax highlighting via [SubVerso](https://github.com/e-vergo/subverso), renders HTML via [Verso](https://github.com/e-vergo/verso), and builds dependency graphs with the Sugiyama hierarchical layout algorithm.

Dress re-exports [LeanArchitect](https://github.com/e-vergo/LeanArchitect), so importing Dress provides the `@[blueprint]` attribute.

## Dependency Chain

```
SubVerso -> LeanArchitect -> Dress -> Runway
```

- **SubVerso**: Extracts syntax highlighting and semantic information during elaboration
- **LeanArchitect**: Defines the `@[blueprint]` attribute with 8 metadata and 3 manual status options
- **Dress**: Generates artifacts, computes statistics, validates graphs
- **Runway**: Consumes Dress output to produce the final website

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

## Build Phases

Dress operates in three distinct phases:

### Phase 1: Per-Declaration Capture (During Elaboration)

When Lean compiles with `BLUEPRINT_DRESS=1`, the `elab_rules` in `Capture/ElabRules.lean` intercept each `@[blueprint]` declaration:

1. **Standard elaboration** runs first (the hook calls `elabCommandTopLevel`)
2. **SubVerso highlighting** is captured via `highlightIncludingUnparsed`
3. **Code splitting** separates signature from proof body at the `:=` boundary
4. **Artifacts written** to `.lake/build/dressed/{Module/Path}/{sanitized-label}/`

Info trees are ephemeral and only exist during elaboration, so highlighting must be captured immediately.

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
3. Validates the graph (connectivity, cycle detection)
4. Computes status counts and upgrades nodes to `fullyProven`
5. Runs Sugiyama layout for hierarchical visualization
6. Writes `dep-graph.svg`, `dep-graph.json`, and `manifest.json`

## Artifact Format

### Per-Declaration Artifacts

Written to `.lake/build/dressed/{Module/Path}/{sanitized-label}/`:

| File | Description |
|------|-------------|
| `decl.tex` | LaTeX source for the declaration |
| `decl.html` | Pre-rendered HTML with hover spans and rainbow brackets |
| `decl.json` | Metadata including SubVerso highlighting data |
| `decl.hovers.json` | Hover tooltip content for interactive display |
| `manifest.entry` | Label-to-path mapping for aggregation |

### Module-Level Artifacts

| File | Location | Description |
|------|----------|-------------|
| `module.json` | `.lake/build/dressed/{Module/Path}/` | Aggregated declaration data |
| `module.tex` | `.lake/build/dressed/{Module/Path}/` | `\input{}` directives for each declaration |

### Library-Level Artifacts

| File | Location | Description |
|------|----------|-------------|
| `{LibName}.tex` | `.lake/build/dressed/library/` | Library index with `\inputleanmodule` macro |
| `dep-graph.svg` | `.lake/build/dressed/` | Dependency graph visualization |
| `dep-graph.json` | `.lake/build/dressed/` | Layout data for D3.js rendering |
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

## Rainbow Bracket Highlighting

Dress uses Verso's `toHtmlRainbow` for bracket highlighting. The `HtmlRender.lean` module wraps Verso's highlighting functions:

```lean
def renderHighlightedWithHovers (hl : Highlighted) : String × String :=
  let (html, finalState) := (hl.toHtmlRainbow).run defaultContext |>.run .empty
  let hoverJson := finalState.dedup.docJson.compress
  (html.asString (breakLines := false), hoverJson)
```

The output uses CSS classes `lean-bracket-1` through `lean-bracket-6` for six distinct depth colors:

| Class | Light Mode | Dark Mode |
|-------|------------|-----------|
| `lean-bracket-1` | #d000ff | #e040ff |
| `lean-bracket-2` | #5126ff | #7156ff |
| `lean-bracket-3` | #0184BC | #01a4dc |
| `lean-bracket-4` | #4078F2 | #5098ff |
| `lean-bracket-5` | #50A14F | #70c16f |
| `lean-bracket-6` | #E45649 | #f47669 |

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

## Graph Algorithms

### Layer Assignment (`Layout.lean`)

**Longest-path algorithm** assigns nodes to vertical layers:
- Nodes with no incoming edges are placed at layer 0
- Each node is placed one layer above its highest dependency
- Produces valid topological ordering for acyclic graphs

### Crossing Reduction

**Median heuristic** minimizes edge crossings:
- For each layer, compute median position of connected nodes in adjacent layer
- Reorder layer by median values
- Alternate forward and backward passes
- **Transpose pass**: iteratively swap adjacent nodes if it reduces crossings

### Coordinate Assignment

**Barycenter-based positioning** with refinement:
- Initial placement on a grid centered by layer width
- Iterative refinement pulls nodes toward median of connected neighbors
- Overlap resolution prevents node collisions

### Edge Routing

For small graphs (<=100 nodes):

1. **Visibility graph**: Collect vertices at obstacle corners/octants
2. **Dijkstra's algorithm**: Find shortest path around obstacles
3. **Bezier fitting**: Convert polyline to smooth cubic Bezier curves via Catmull-Rom interpolation

For large graphs (>100 nodes), simplified direct Bezier curves are used to avoid O(V^2) per-edge visibility graph construction.

### Cycle Handling

The graph is made acyclic before layout:

1. **DFS** identifies back-edges (edges that would create cycles)
2. **Back-edges are reversed** (Graphviz approach)
3. Layout proceeds on acyclic graph
4. Reversed edges are marked with `isReversed` flag for correct arrow direction in SVG

### Transitive Reduction

Removes redundant edges implied by transitivity. Uses Floyd-Warshall which is O(n^3), so it is **skipped for graphs with more than 100 nodes** to avoid multi-hour build times on large projects.

## Validation Checks

### Connectivity (`findComponents`)

BFS-based component detection:

```lean
def findComponents (g : Graph) : Array (Array String)
```

- Single component indicates a fully connected graph
- Multiple components may indicate missing dependencies or orphaned declarations

### Cycle Detection (`detectCycles`)

DFS with gray/black coloring:

```lean
def detectCycles (g : Graph) : Array (Array String)
```

- Gray nodes are in the current DFS path
- Back-edge to gray node indicates cycle
- Returns array of detected cycles (each cycle is array of node IDs)

### Fully Proven Computation (`computeFullyProven`)

Post-processing step with O(V+E) complexity via memoization:

```lean
def computeFullyProven (g : Graph) : Graph
```

A node is upgraded to `fullyProven` if:
1. Its status is `proven`
2. All its ancestors are `proven` or `fullyProven`

This provides stronger verification guarantees than `proven` alone.

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
    Layout.lean        # Sugiyama algorithm, edge routing (~1450 lines)
    Json.lean          # JSON serialization for D3.js
    Svg.lean           # SVG generation

  Render/
    SideBySide.lean    # Side-by-side display rendering

  Serialize/
    Json.lean          # SubVerso JSON serialization
    Html.lean          # HTML serialization
    Artifacts.lean     # Dressed artifact format

  Base64.lean          # RFC 4648 Base64 encoding
  Core.lean            # Core types, splitAtDefinitionAssign
  HtmlRender.lean      # Verso HTML rendering wrapper
  Hook.lean            # Main entry point, re-exports
  Paths.lean           # Path utilities for artifact locations

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
| [SubVerso](https://github.com/e-vergo/subverso) | Syntax highlighting extraction |
| [Verso](https://github.com/e-vergo/verso) | HTML rendering with rainbow brackets |
| [Cli](https://github.com/mhuisi/lean4-cli) | Command-line interface |

## Related Repositories

| Repository | Purpose |
|------------|---------|
| [Runway](https://github.com/e-vergo/Runway) | Site generator (downstream) |
| [LeanArchitect](https://github.com/e-vergo/LeanArchitect) | Blueprint attribute (upstream) |
| [SubVerso](https://github.com/e-vergo/subverso) | Syntax highlighting (upstream) |
| [SBS-Test](https://github.com/e-vergo/SBS-Test) | Minimal test project (16 nodes, all 6 statuses) |
| [dress-blueprint-action](https://github.com/e-vergo/dress-blueprint-action) | GitHub Actions CI solution |

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.
