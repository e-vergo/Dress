# Dress

![Lean](https://img.shields.io/badge/Lean-v4.28.0-blue)
![License](https://img.shields.io/badge/License-Apache%202.0-green)

Artifact generation for Lean 4 formalization documentation. Transforms `@[blueprint]`-decorated declarations into syntax-highlighted HTML with interactive hovers, and builds dependency graphs with hierarchical layout.

## Table of Contents

- [Overview](#overview)
- [lean4-dress Toolchain](#lean4-dress-toolchain)
- [Quick Start](#quick-start)
- [Dependency Chain](#dependency-chain)
- [Two-Phase Build Architecture](#two-phase-build-architecture)
- [Artifact Format](#artifact-format)
- [Manifest Schema](#manifest-schema)
- [7-Status Color Model](#7-status-color-model)
- [Graph Layout Algorithm](#graph-layout-algorithm)
- [Validation Checks](#validation-checks)
- [Rainbow Bracket Highlighting](#rainbow-bracket-highlighting)
- [SubVerso Integration](#subverso-integration)
- [Module Structure](#module-structure)
- [CLI Reference](#cli-reference)
- [Integration with Runway](#integration-with-runway)

## Overview

Dress is the artifact generation layer of the Side-by-Side Blueprint formalization documentation toolchain. It operates as the build-time phase, producing artifacts that Runway consumes for site generation.

**Part of the [Side-by-Side Blueprint](https://github.com/e-vergo/Side-By-Side-Blueprint) toolchain.** When used independently, Dress is available at [github.com/e-vergo/Dress](https://github.com/e-vergo/Dress).

**Core responsibilities:**

1. Intercept `@[blueprint]` declarations during Lean elaboration
2. Capture syntax highlighting via [SubVerso](https://github.com/e-vergo/subverso) while info trees are available (93-99% of build time)
3. Render HTML with rainbow bracket matching via [Verso](https://github.com/e-vergo/verso)
4. Generate declaration artifacts with embedded hover data
5. Build dependency graphs with Sugiyama hierarchical layout
6. Validate graph structure (connectivity, cycles)
7. Compute status counts and auto-upgrade nodes to `fullyProven`

**Why validation matters:** The Tao incident (January 2026) demonstrated that a proof can typecheck while proving something entirely different from what was intended. When Terence Tao reviewed the PNT+ blueprint graph, he noticed disconnected final theorems -- AI-provided proofs had satisfied trivial versions of statements. Dress's connectivity validation catches this class of errors automatically.

Dress re-exports [LeanArchitect](https://github.com/e-vergo/LeanArchitect), so importing Dress provides the `@[blueprint]` attribute.

## lean4-dress Toolchain

Dress is built into the [lean4-dress](https://github.com/e-vergo/lean4-dress) custom Lean 4.28 toolchain as a dynlib. When using the lean4-dress toolchain (the standard way to use Side-by-Side Blueprint), **Dress is auto-imported -- no explicit `import Dress` is needed.** The toolchain handles:

1. **Dynlib pre-loading:** Eight dylibs (including Dress and its dependencies) are loaded at startup via `loadDressDynlibsOnce`, making native symbols available before module import.
2. **Auto-import:** `processHeaderCore` in the toolchain injects `import Dress` into the import list when Dress oleans are findable, loading all environment extensions, parser categories, and elab_rules through the normal import system.
3. **Safety guards:** Auto-import is skipped during Dress rebuilds (`mainModule.getRoot == \`Dress`) to avoid circular dependencies.

[Mathlib.app](https://github.com/e-vergo/mathlib-app) ships the lean4-dress toolchain as a bundled macOS application where Dress is always active -- users write Lean, annotate with `@[blueprint]`, and everything works.

For standalone use without the lean4-dress toolchain, Dress can be added as a Lake dependency (see Quick Start below).

## Quick Start

### Using lean4-dress Toolchain (Recommended)

With the lean4-dress toolchain, Dress is auto-imported. Just annotate declarations:

```lean
@[blueprint "thm:main"]
theorem mainTheorem : 2 + 2 = 4 := rfl

@[blueprint "thm:key" (keyDeclaration := true, message := "Central result")]
theorem keyTheorem : P := by sorry
```

### Standalone Lake Dependency

In your `lakefile.toml`:

```toml
[[require]]
name = "Dress"
git = "https://github.com/e-vergo/Dress"
rev = "main"
```

For local development:

```toml
[[require]]
name = "Dress"
path = "../Dress"
```

Then import explicitly:

```lean
import Dress  -- Re-exports @[blueprint] from LeanArchitect

@[blueprint "thm:main"]
theorem mainTheorem : 2 + 2 = 4 := rfl
```

### Scaffold the Project

Use the `quickstart` subcommand to generate configuration and CI workflow automatically:

```bash
lake exe extract_blueprint quickstart
```

This creates an optional `runway.json` for CI/CD customization, a GitHub Actions workflow, and injects `import Dress` into your `.lean` files. Runway auto-derives config from the lakefile/directory, so the `runway.json` is not required for local builds. See [CLI Reference > quickstart](#quickstart) for flags and details.

### Mark Declarations

```lean
@[blueprint "thm:main"]
theorem mainTheorem : 2 + 2 = 4 := rfl

@[blueprint "thm:key" (keyDeclaration := true, message := "Central result")]
theorem keyTheorem : P := by sorry

@[blueprint "def:square" (title := "Square Function")]
def square (n : Nat) : Nat := n * n
```

To bootstrap annotations on an existing codebase, use `auto-tag` after building:

```bash
lake build
lake exe extract_blueprint auto-tag MyLib
```

See [CLI Reference > auto-tag](#auto-tag) for details.

### Build with Artifact Generation

```bash
# Build with artifact generation (automatic when Dress is imported)
lake build

# Generate Lake facets
lake build :blueprint

# Generate dependency graph and manifest
lake exe extract_blueprint graph MyProject.Module1 MyProject.Module2
```

Output is written to `.lake/build/dressed/`.

## Dependency Chain

```
SubVerso -> LeanArchitect -> Dress -> Runway
              |
              +-> Verso (genres use SubVerso for highlighting)
```

| Component | Location | Role |
|-----------|----------|------|
| [SubVerso](../../forks/subverso/) | `forks/subverso/` | Extracts syntax highlighting with O(1) indexed lookups via InfoTable |
| [LeanArchitect](../../forks/LeanArchitect/) | `forks/LeanArchitect/` | Defines `@[blueprint]` attribute with 8 metadata and 2 manual status options |
| **Dress** | `toolchain/Dress/` | Generates artifacts, computes statistics, validates graphs, performs Sugiyama layout |
| [Runway](../Runway/) | `toolchain/Runway/` | Consumes Dress output to produce the final website and dashboard |
| [Verso](../../forks/verso/) | `forks/verso/` | Document framework with SBSBlueprint and VersoPaper genres |

## Two-Phase Build Architecture

Dress operates in two distinct phases during the build process:

### Phase 1: Per-Declaration Capture (During Elaboration)

When Dress is imported, the `elab_rules` in `Capture/ElabRules.lean` unconditionally intercept each `@[blueprint]` declaration:

1. Standard elaboration runs first (the hook calls `elabCommandTopLevel`)
2. SubVerso highlighting is captured via `highlightIncludingUnparsed`
3. Code splitting separates signature from proof body at the `:=` boundary
4. Artifacts are written to `.lake/build/dressed/{Module/Path}/{sanitized-label}/`

**Why immediate capture?** Info trees are ephemeral and only exist during elaboration. They are discarded after elaboration completes, so highlighting must be captured immediately. This accounts for 93-99% of build time.

**Timing breakdown** (typical per-declaration):
- SubVerso highlighting: 800-6500ms (93-99%)
- HTML generation: <30ms (<1%)

### Phase 2: Lake Facet Aggregation

After compilation, Lake facets aggregate per-declaration artifacts:

| Facet | Level | Output |
|-------|-------|--------|
| `dressed` | Module | `module.json` aggregating all declarations |
| `depGraph` | Library | `dep-graph.svg` and `dep-graph.json` |

### Phase 3: Manifest Generation (CLI)

The `graph` subcommand performs final processing:

1. Loads modules and extracts blueprint nodes from the environment
2. Infers dependencies from Lean code via `Node.inferUses`
3. **Two-pass edge processing:**
   - PASS 1: Register all labels and create nodes (so back-references work)
   - PASS 2: Add all edges (now targets exist for back-edges)
   - Edge deduplication removes duplicate (from, to) pairs
4. Validates the graph (connectivity, cycle detection)
5. Computes status counts and upgrades nodes to `fullyProven`
6. Applies transitive reduction (skipped for >500 nodes -- O(E*(V+E)) DFS-based)
7. Runs Sugiyama layout for hierarchical visualization
8. Writes `dep-graph.svg`, `dep-graph.json`, and `manifest.json`

## Artifact Format

### Per-Declaration Artifacts

Written to `.lake/build/dressed/{Module/Path}/{sanitized-label}/`:

| File | Description |
|------|-------------|
| `decl.tex` | Structured declaration data with base64-encoded HTML, hover data, and metadata |
| `decl.html` | Pre-rendered HTML with hover spans and rainbow brackets via `toHtmlRainbow` |
| `decl.json` | Metadata: `{"name": "...", "label": "...", "highlighting": {...}}` |
| `decl.hovers.json` | Hover tooltip content for interactive display (JSON mapping IDs to content) |
| `manifest.entry` | Label-to-path mapping: `{"label": "...", "path": "..."}` |

The `decl.tex` file uses a structured format with commands like `\leansignaturesourcehtml{base64}`, `\leanproofsourcehtml{base64}`, and `\leanhoverdata{base64}` to embed syntax-highlighted HTML and hover data. Runway parses this format to extract content for side-by-side display.

Timing data is available via `trace[blueprint.timing]` for performance analysis.

### Module-Level Artifacts

| File | Location | Description |
|------|----------|-------------|
| `module.json` | `.lake/build/dressed/{Module/Path}/` | Aggregated declaration data |

### Library-Level Artifacts

| File | Location | Description |
|------|----------|-------------|
| `dep-graph.svg` | `.lake/build/dressed/` | Dependency graph SVG visualization |
| `dep-graph.json` | `.lake/build/dressed/` | Layout data for interactive rendering |
| `manifest.json` | `.lake/build/dressed/` | Stats, validation, metadata for Runway |
| `subgraphs/metadata.json` | `.lake/build/dressed/subgraphs/` | Per-node depth metadata for client-side subgraph rendering |

## Manifest Schema

The `manifest.json` file contains precomputed data consumed by Runway:

```json
{
  "stats": {
    "notReady": 2,
    "wip": 3,
    "hasSorry": 2,
    "proven": 5,
    "fullyProven": 8,
    "axiom": 1,
    "mathlibReady": 1,
    "total": 22
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

**Soundness guarantee:** Statistics are computed upstream in Dress. Runway loads `manifest.json` without recomputation, ensuring displayed statistics match the actual graph state.

## 7-Status Color Model

Node status types are defined in LeanArchitect and re-exported by Dress. The canonical hex values are defined in `Graph/Svg.lean`:

| Status | Color | Hex | Source |
|--------|-------|-----|--------|
| `notReady` | Vivid Orange | #E8820C | Default -- no Lean proof exists |
| `wip` | Deep Teal | #0097A7 | Manual `(wip := true)` |
| `sorry` | Vivid Red | #C62828 | Auto-detected: proof contains `sorryAx` |
| `proven` | Medium Green | #66BB6A | Auto-detected: complete proof |
| `fullyProven` | Deep Forest Green | #1B5E20 | Auto-computed: all ancestors proven |
| `axiom` | Vivid Purple | #7E57C2 | Structural: Lean `axiom` declaration (intentionally unproven) |
| `mathlibReady` | Vivid Blue | #42A5F5 | Manual `(mathlibReady := true)` |

**Status priority** (manual flags take precedence):
1. `mathlibReady` (manual)
2. `wip` (manual)
3. `notReady` (manual, if explicitly set)
4. `fullyProven` (auto-computed)
5. `axiom` (auto-detected for Lean `axiom` declarations)
6. `sorry` (auto-detected)
7. `proven` (auto-detected)
8. `notReady` (default)

**Note:** `axiom` detection is performed during graph construction in `Graph/Build.lean`, not at SVG render time.

**Color source of truth:** The hex values in `Graph/Svg.lean` are canonical. CSS variables in `common.css` must match these exactly.

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
| Transitive reduction | O(E*(V+E)) DFS-based | Skipped for >500 nodes |

**>100 node optimizations** (automatic, triggered at `g.nodes.size > 100`):
- Max 2 barycenter iterations (vs 4) - in `orderLayers`
- Skip transpose heuristic - O(L x N x swaps) per call
- Skip visibility graph routing - O(V^2) per edge
- Use simplified direct Bezier curves instead

**>500 node optimization**: transitive reduction (O(E*(V+E)) DFS-based) is skipped entirely.

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

**Motivation:** The Tao incident (January 2026) where disconnected final theorems were proven with AI-provided proofs that satisfied trivial versions of statements. Connectivity checks catch this class of errors.

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

This provides stronger verification guarantees than `proven` alone -- it means the entire proof tree from axioms to this node is complete.

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

## SubVerso Integration

Dress depends on a fork of SubVerso with O(1) indexed lookups via InfoTable:

| Field | Purpose | Complexity |
|-------|---------|------------|
| `infoByExactPos` | HashMap for exact syntax position (start, end) | O(1) |
| `termInfoByName` | HashMap for const/fvar by name | O(1) |
| `nameSuffixIndex` | HashMap for suffix-based lookups | O(1) |
| `allInfoSorted` | Sorted array for containment queries | O(n) worst |

Additional caches in `HighlightState`:
- `identKindCache`: Memoizes identifier classification by (position, name)
- `signatureCache`: Memoizes pretty-printed type signatures by constant name
- `hasTacticCache` / `childHasTacticCache`: Memoizes tactic info searches

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
    Latex.lean         # Structured declaration format generation (decl.tex)
    Module.lean        # Module-level utilities

  Graph/
    Types.lean         # Node, Edge, Graph, StatusCounts, CheckResults
    Build.lean         # Graph construction, validation, computeFullyProven
    Layout.lean        # Sugiyama algorithm, edge routing (~1500 lines)
    Json.lean          # JSON serialization
    Svg.lean           # SVG generation (canonical status colors)
    Subgraph.lean      # Per-node subgraph extraction and depth computation

  Render/
    SideBySide.lean    # Side-by-side display rendering

  Serialize/
    Json.lean          # SubVerso JSON serialization
    Html.lean          # HTML serialization
    Artifacts.lean     # Dressed artifact format

  Svg/
    Core.lean          # SVG element primitives
    Shapes.lean        # Rectangle, ellipse, path shapes
    Style.lean         # SVG styling utilities
    Text.lean          # SVG text rendering
    Transform.lean     # SVG coordinate transforms
    Coordinate.lean    # Coordinate system utilities

  Base64.lean          # RFC 4648 Base64 encoding
  Cache.lean           # Build cache for incremental compilation
  Content.lean         # BlueprintContent type for module contents
  Core.lean            # Core types, splitAtDefinitionAssign
  Highlighting.lean    # SubVerso highlighting integration
  HtmlRender.lean      # Verso HTML rendering wrapper
  Hook.lean            # Main entry point, re-exports
  Load.lean            # Loading nodes from environment
  Output.lean          # Output functions for declaration artifacts
  Paths.lean           # Path utilities for artifact locations
  Quickstart.lean      # Project scaffolding (quickstart command)
  Render.lean          # Rendering utilities
  SubVersoExtract.lean # SubVerso extraction utilities
  Svg.lean             # SVG barrel file

Main.lean              # CLI executable (extract_blueprint: graph, quickstart, auto-tag, single, index)
lakefile.lean          # Package definition with Lake facets
```

## CLI Reference

The `extract_blueprint` executable provides four subcommands:

### graph (Primary)

Generate dependency graph and manifest:

```bash
lake exe extract_blueprint graph --build .lake/build Module1 Module2
```

Outputs `dep-graph.svg`, `dep-graph.json`, `manifest.json`, and `subgraphs/metadata.json` to `.lake/build/dressed/`.

### quickstart

Scaffold an existing Lean project into an SBS blueprint project. Creates an optional `runway.json` for CI/CD, `.github/workflows/blueprint.yml`, and injects `import Dress` into `.lean` files containing declarations. Runway auto-derives config from lakefile/directory, so the `runway.json` is not required for local use.

```bash
# Basic usage (auto-detects GitHub URL and assets path)
lake exe extract_blueprint quickstart

# With explicit options
lake exe extract_blueprint quickstart --title "My Project" --github-url "https://github.com/user/repo"

# Preview without writing files
lake exe extract_blueprint quickstart --dry-run

# Overwrite existing files
lake exe extract_blueprint quickstart --force
```

**Flags:**

| Flag | Purpose |
|------|---------|
| `--title` | Project title for optional `runway.json` (default: project directory name) |
| `--github-url` | GitHub URL (default: auto-detected from `git remote get-url origin`, SSH normalized to HTTPS) |
| `--base-url` | Base URL for site (default: `/`) |
| `--dry-run` | Print what would be created without writing files |
| `--force` | Overwrite existing files (default: skip existing) |

Safe to re-run: existing files are skipped unless `--force` is passed. The `assetsDir` field in the generated `runway.json` is auto-discovered by walking up from the project directory looking for `dress-blueprint-action/assets`. Runway also auto-discovers assets at runtime without needing this field.

### auto-tag

Insert `@[blueprint]` attributes on declarations not yet annotated. Requires a compiled environment (`lake build` first).

```bash
lake exe extract_blueprint auto-tag MyLib
```

Scans all declarations in the given library and inserts `@[blueprint]` above each uncovered declaration. When a declaration already has an attribute block (e.g., `@[simp]`), injects `blueprint` into the existing block (`@[blueprint, simp]`) to avoid parser conflicts. Skips `instance` declarations (not valid blueprint targets).

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

1. Build project with Dress (`lake build`)
2. Generate graph (`lake exe extract_blueprint graph ...`)
3. Generate site (`lake exe runway build`) -- Runway auto-derives config from lakefile/directory

Runway loads:
- Per-declaration artifacts from `.lake/build/dressed/{Module/Path}/`
- `manifest.json` for dashboard data (precomputed, no recomputation)
- `dep-graph.json` and `dep-graph.svg` for visualization
- `subgraphs/metadata.json` for client-side subgraph rendering

## Environment Variable

| Variable | Purpose |
|----------|---------|
| `BLUEPRINT_CAPTURE=false` | Disable artifact writing during `lake build` (opt-out) |

Artifact generation is unconditional when Dress is imported -- the `elab_rules` in `Capture/ElabRules.lean` intercept `@[blueprint]` declarations and write artifacts immediately after elaboration. Set `BLUEPRINT_CAPTURE=false` to suppress artifact writing while keeping Dress loaded.

## Backwards Compatibility

JSON parsing handles legacy status values for compatibility with older manifests:
- `"stated"` maps to `.notReady`
- `"inMathlib"` maps to `.mathlibReady`

## Dependencies

| Dependency | Location | Purpose |
|------------|----------|---------|
| [LeanArchitect](../../forks/LeanArchitect/) | `forks/LeanArchitect/` | `@[blueprint]` attribute definition |
| [SubVerso](../../forks/subverso/) | `forks/subverso/` | Syntax highlighting extraction with O(1) indexed lookups |
| [Verso](../../forks/verso/) | `forks/verso/` | HTML rendering with rainbow bracket matching |
| Cli | External | Command-line interface (mhuisi/lean4-cli) |

## Related Repositories (Monorepo)

| Repository | Location | Purpose |
|------------|----------|---------|
| **Runway** | `toolchain/Runway/` | Site generator (downstream) |
| **LeanArchitect** | `forks/LeanArchitect/` | Blueprint attribute (upstream) |
| **SubVerso** | `forks/subverso/` | Syntax highlighting (upstream) |
| **Verso** | `forks/verso/` | Document framework (upstream) |
| **SBS-Test** | `toolchain/SBS-Test/` | Minimal test project (33 nodes, all 7 statuses) |
| **dress-blueprint-action** | `toolchain/dress-blueprint-action/` | GitHub Actions CI solution + CSS/JS assets |
| **GCR** | `showcase/General_Crystallographic_Restriction/` | Production example (57 nodes) |
| **PNT** | `showcase/PrimeNumberTheoremAnd/` | Large-scale integration (591 nodes) |

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.
