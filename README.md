# Dress

![Lean](https://img.shields.io/badge/Lean-v4.27.0-blue)
![License](https://img.shields.io/badge/License-Apache%202.0-green)

Artifact generation for Lean 4 mathematical blueprints. Transforms `@[blueprint]`-decorated declarations into richly-annotated LaTeX and HTML with syntax highlighting, interactive hovers, and proof toggling.

## Features

- **Declaration capture during elaboration** - Semantic highlighting via SubVerso, pre-rendered HTML via Verso
- **Rainbow bracket highlighting** - Depth-colored brackets for visual parsing clarity
- **Side-by-side rendering** - LaTeX statements with Lean code, proof toggle support
- **Dependency graph building** - Sugiyama hierarchical layout algorithm in pure Lean
- **Statistics computation** - Status counts for dashboard display (soundness guarantee)
- **Validation checks** - Connectivity analysis and cycle detection

## Build Phases

Dress operates in three distinct phases:

### Phase 1: Per-Declaration Capture (During Elaboration)

When Lean compiles a file with `BLUEPRINT_DRESS=1`, the `elab_rules` hook in `Capture/ElabRules.lean` fires after each `@[blueprint]` declaration:

1. **SubVerso highlighting capture** - Calls `highlightIncludingUnparsed` to extract semantic highlighting
2. **Info tree processing** - Info trees are **ephemeral** (only exist during elaboration) - must capture immediately
3. **Code splitting** - Splits at `:=` boundary to separate signature from proof body
4. **Artifact writing** - Writes per-declaration artifacts to `.lake/build/dressed/`

### Phase 2: Lake Facet Aggregation

After compilation completes, Lake facets aggregate the per-declaration artifacts:

- **`dressed` facet** - Scans `{label}/` directories, aggregates into `module.json`
- **`blueprint` facet** - Generates `module.tex` with `\input{}` paths

### Phase 3: Manifest Generation

The `graph` subcommand performs final processing:

1. **Graph construction** - Loads modules, extracts blueprint nodes, infers dependencies from Lean code
2. **Validation checks** - Connectivity analysis, cycle detection
3. **Status computation** - Counts nodes by status, computes `fullyProven` upgrades
4. **Layout calculation** - Sugiyama algorithm for hierarchical visualization
5. **Output** - Writes `dep-graph.svg`, `dep-graph.json`, and `manifest.json`

## Artifact Format

Per-declaration artifacts are written to `.lake/build/dressed/{Module/Path}/{label}/`:

| File | Description |
|------|-------------|
| `decl.tex` | LaTeX source for the declaration statement |
| `decl.html` | Pre-rendered HTML with hover spans and rainbow brackets |
| `decl.json` | SubVerso highlighting data (tokens, positions, types) |
| `decl.hovers.json` | Hover content data for interactive tooltips |

Additional module-level artifacts:

| File | Location | Description |
|------|----------|-------------|
| `module.json` | `.lake/build/dressed/{Module/Path}/` | Aggregated module artifacts |
| `module.tex` | `.lake/build/dressed/{Module/Path}/` | Module header with `\input{}` directives |
| `dep-graph.svg` | `.lake/build/dressed/` | Dependency graph visualization |
| `dep-graph.json` | `.lake/build/dressed/` | Layout data for D3.js rendering |
| `manifest.json` | `.lake/build/dressed/` | Stats, validation, key declarations, project notes |

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
    "priority": [{"id": "lem:helper", "label": "Helper Lemma"}],
    "blocked": [{"id": "thm:blocked", "label": "Blocked Theorem", "reason": "Waiting for mathlib PR"}],
    "potentialIssues": [{"id": "lem:issue", "label": "Issue Lemma", "issue": "May need refactoring"}],
    "technicalDebt": [{"id": "def:ugly", "label": "Ugly Definition", "debt": "Should use cleaner approach"}],
    "misc": [{"id": "thm:note", "label": "Note Theorem", "note": "Consider generalizing"}]
  },
  "nodes": {"thm:main": "chapter1.html#thm:main"},
  "checkResults": {
    "isConnected": true,
    "numComponents": 1,
    "componentSizes": [21],
    "cycles": []
  }
}
```

**Soundness guarantee**: Stats are computed upstream in Dress. Runway loads `manifest.json` without recomputation, ensuring the displayed statistics match the actual graph state.

## Rainbow Bracket Highlighting

The `wrapBracketsWithDepth` function in `HtmlRender.lean` adds visual depth cues to nested brackets:

```lean
def wrapBracketsWithDepth (html : String) : String
```

- Tracks nesting depth across `()`, `[]`, `{}`
- Wraps each bracket in `<span class="lean-bracket-N">` where N cycles 1-6
- Skips brackets inside HTML tags to preserve structure
- Opening brackets: increment depth first, then wrap
- Closing brackets: wrap with current depth, then decrement

CSS classes `lean-bracket-1` through `lean-bracket-6` provide six distinct colors for visual depth perception.

## Graph Layout Algorithm

Full Sugiyama hierarchical layout implemented in `Graph/Layout.lean` (~1450 lines):

### Layer Assignment

**Longest-path algorithm** assigns nodes to vertical layers:
- Nodes with no dependencies go to layer 0
- Each node is placed one layer above its highest dependency
- Produces a valid topological ordering

### Crossing Reduction

**Median heuristic** minimizes edge crossings:
- For each layer, compute median position of connected nodes
- Reorder layer by median values
- Iterate with transpose passes for refinement

### Coordinate Assignment

**Barycenter-based positioning**:
- Position nodes at average X of connected neighbors
- Apply overlap resolution to prevent collisions
- Iterate for convergence

### Edge Routing

**Visibility graph + Dijkstra**:
- Build visibility graph from node positions
- Find shortest path around obstacles
- Produces orthogonal-style routes

**Catmull-Rom to Bezier conversion**:
- Smooth edge paths with cubic Bezier curves
- Preserves control points for SVG path rendering

### Performance Optimization

For large graphs (>100 nodes):
- **Transitive reduction skipped** - O(n^3) algorithm causes 3+ hour hangs on 530-node graphs
- Simplified edge routing to reduce computation

## Validation Checks

### Connectivity Check (`findComponents`)

BFS-based component detection:

```lean
def findComponents (g : Graph) : Array (Array String)
```

- Returns array of components (each an array of node IDs)
- Single component = fully connected graph
- Multiple components may indicate:
  - Missing dependencies
  - Orphaned declarations
  - Logical gaps in proof structure (Tao-style errors)

### Cycle Detection (`detectCycles`)

DFS with gray/black node coloring:

```lean
def detectCycles (g : Graph) : Array (Array String)
```

- Gray nodes: currently being explored
- Black nodes: fully processed
- Back-edge to gray node = cycle detected
- Returns array of detected cycles

### Full Status Computation (`computeFullyProven`)

Post-processing step that upgrades `proven` nodes:

```lean
def computeFullyProven (g : Graph) : Graph
```

- O(V+E) complexity with memoization
- A node becomes `fullyProven` if:
  1. Its status is `proven`
  2. All its ancestors are either `proven` or `fullyProven`
- Automatically propagates through dependency chains

## 6-Status Model

Node status types defined in `LeanArchitect/Architect/Basic.lean`, used throughout the system:

| Status | Color | Hex | Source |
|--------|-------|-----|--------|
| `notReady` | Sandy Brown | #F4A460 | Default (no Lean code) or manual: `(notReady := true)` |
| `ready` | Light Sea Green | #20B2AA | Manual: `(ready := true)` |
| `sorry` | Dark Red | #8B0000 | Auto-detected: proof contains `sorryAx` |
| `proven` | Light Green | #90EE90 | Auto-detected: complete proof without sorry |
| `fullyProven` | Forest Green | #228B22 | Auto-computed: proven + all ancestors proven/fullyProven |
| `mathlibReady` | Light Blue | #87CEEB | Manual: `(mathlibReady := true)` |

**Priority order** (manual flags take precedence):
1. `mathlibReady` (manual) - highest priority
2. `ready` (manual)
3. `notReady` (manual, if explicitly set)
4. `fullyProven` (auto-computed)
5. `sorry` (auto-detected)
6. `proven` (auto-detected)
7. `notReady` (default)

### Node Shapes

| Shape | Used For |
|-------|----------|
| Box (rectangle) | Definitions, abbreviations, structures, classes |
| Ellipse | Theorems, lemmas, propositions |

### Edge Styles

| Style | Meaning |
|-------|---------|
| Solid | Proof dependency (used in proof body) |
| Dashed | Statement dependency (used in signature/statement) |

## Dependency Chain

```
SubVerso -> LeanArchitect -> Dress -> Runway
```

- **[SubVerso](https://github.com/e-vergo/subverso)**: Semantic highlighting extraction (fork with optimizations)
- **[LeanArchitect](https://github.com/e-vergo/LeanArchitect)**: `@[blueprint]` attribute with 8 metadata + 3 manual status options
- **Dress**: Artifact generation, graph building, stats computation
- **[Runway](https://github.com/e-vergo/Runway)**: Site generation consuming Dress outputs

## Installation

Add to `lakefile.toml`:

```toml
[[require]]
name = "Dress"
git = "https://github.com/e-vergo/Dress"
rev = "main"
```

Or for local development:

```toml
[[require]]
name = "Dress"
path = "../Dress"
```

## Usage

### 1. Import Dress in Lean files

```lean
import Dress  -- Re-exports LeanArchitect's @[blueprint] attribute

@[blueprint "thm:my-theorem"]
theorem myTheorem : 2 + 2 = 4 := rfl

@[blueprint (keyDeclaration := true, message := "Main result")]
theorem mainResult : P := by sorry

@[blueprint (title := "Square Non-negative", mathlibReady := true)]
theorem square_nonneg : ... := by ...
```

### 2. Build with dressing enabled

Using environment variable:

```bash
BLUEPRINT_DRESS=1 lake build
```

Or create marker file:

```bash
mkdir -p .lake/build
echo "1" > .lake/build/.dress
lake build
rm .lake/build/.dress
```

### 3. Generate Lake facets

```bash
lake build :blueprint                   # All modules
lake build MyProject.MyModule:blueprint # Specific module
```

### 4. Generate dependency graph and manifest

```bash
lake exe extract_blueprint graph MyProject.Module1 MyProject.Module2
```

## Module Structure

```
Dress/
  Capture/                  # Elaboration-time artifact capture
    ElabRules.lean          # elab_rules for @[blueprint] declarations
    InfoTree.lean           # Environment extension, info tree capture
    State.lean              # IO.Ref state management
    Config.lean             # Blueprint config parsing

  Generate/                 # LaTeX and artifact output
    Declaration.lean        # Per-declaration artifact writer
    Latex.lean              # Per-declaration LaTeX generation
    Module.lean             # Module-level utilities

  Graph/                    # Dependency graph construction
    Types.lean              # Node, Edge, Graph, StatusCounts, CheckResults
    Build.lean              # Graph construction, validation, computeFullyProven
    Layout.lean             # Sugiyama algorithm, edge routing (~1450 lines)
    Json.lean               # JSON serialization for D3.js
    Render.lean             # SVG generation

  Serialize/                # Output format generation
    Json.lean               # SubVerso JSON serialization
    Html.lean               # HTML serialization
    Artifacts.lean          # Dressed artifact format

  Base64.lean               # RFC 4648 Base64 encoding
  Core.lean                 # Core types, splitAtDefinitionAssign
  HtmlRender.lean           # Verso HTML rendering, wrapBracketsWithDepth
  Hook.lean                 # Main entry point, re-exports submodules
  Paths.lean                # Path utilities for artifact locations
```

## Lake Facets

| Facet | Level | Description |
|-------|-------|-------------|
| `dressed` | Module | Aggregate per-declaration artifacts into `module.json` |
| `blueprint` | Module/Library/Package | Generate `module.tex` with `\input{}` paths |
| `depGraph` | Library/Package | Generate dependency graph SVG and JSON |

```bash
lake build MyProject.MyModule:dressed   # Generate module.json
lake build MyProject.MyModule:blueprint # Generate module.tex
lake build :blueprint                   # All modules
lake build :depGraph                    # Dependency graph
```

## CLI Reference

The `graph` subcommand generates dependency graph and manifest:

```bash
lake exe extract_blueprint graph --build .lake/build Module1 Module2
```

The `single` and `index` subcommands are deprecated. Use Lake facets instead.

## Integration with Runway

Dress is designed to work with [Runway](https://github.com/e-vergo/Runway):

1. Build project with Dress to generate artifacts
2. Run `lake exe extract_blueprint graph` to generate dependency graph and manifest
3. Configure `runway.json` with paths to artifacts and assets
4. Run `lake exe runway build` to generate the interactive website

Runway consumes:
- Per-declaration artifacts from `.lake/build/dressed/{Module/Path}/`
- `manifest.json` for dashboard data (precomputed, no recomputation)
- `dep-graph.json` and `dep-graph.svg` for visualization

## Dependencies

- [LeanArchitect](https://github.com/e-vergo/LeanArchitect) - `@[blueprint]` attribute
- [SubVerso](https://github.com/e-vergo/subverso) - Semantic highlighting (fork with optimizations)
- [Verso](https://github.com/leanprover/verso) - HTML rendering
- [Cli](https://github.com/mhuisi/lean4-cli) - Command-line interface

## Related Repositories

| Repository | Purpose |
|------------|---------|
| [Runway](https://github.com/e-vergo/Runway) | Site generator (downstream) |
| [LeanArchitect](https://github.com/e-vergo/LeanArchitect) | Blueprint attribute (upstream) |
| [SubVerso](https://github.com/e-vergo/subverso) | Syntax highlighting (upstream) |
| [SBS-Test](https://github.com/e-vergo/SBS-Test) | Minimal test project (16 nodes, all 6 status colors) |
| [dress-blueprint-action](https://github.com/e-vergo/dress-blueprint-action) | GitHub Actions CI solution |

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.
