# Dress

Artifact generation for Lean 4 mathematical blueprints. Transforms `@[blueprint]`-decorated declarations into richly-annotated LaTeX and HTML with syntax highlighting, interactive hovers, and proof toggling.

## Features

- **Declaration capture during elaboration** - Semantic highlighting via SubVerso, pre-rendered HTML via Verso
- **Side-by-side rendering** - LaTeX statements with Lean code, proof toggle support
- **Dependency graph building** - Sugiyama hierarchical layout algorithm in pure Lean
- **Statistics computation** - Status counts for dashboard display (soundness guarantee)
- **Validation checks** - Connectivity analysis and cycle detection

## Output Files

| File | Location | Description |
|------|----------|-------------|
| `decl.tex` | `.lake/build/dressed/{Module/Path}/{label}/` | Per-declaration LaTeX |
| `decl.html` | `.lake/build/dressed/{Module/Path}/{label}/` | Per-declaration HTML with hover data |
| `decl.json` | `.lake/build/dressed/{Module/Path}/{label}/` | Per-declaration highlighting data |
| `module.json` | `.lake/build/dressed/{Module/Path}/` | Aggregated module artifacts |
| `module.tex` | `.lake/build/dressed/{Module/Path}/` | Module header with `\input{}` directives |
| `dep-graph.svg` | `.lake/build/dressed/` | Dependency graph visualization |
| `dep-graph.json` | `.lake/build/dressed/` | Layout data for D3.js rendering |
| `manifest.json` | `.lake/build/dressed/` | Stats, validation, key declarations, project notes |

## Dependency Chain

```
SubVerso -> LeanArchitect -> Dress -> Runway
```

- **SubVerso**: Semantic highlighting extraction
- **LeanArchitect**: `@[blueprint]` attribute with 8 metadata + 5 status options
- **Dress**: Artifact generation, graph building, stats computation
- **Runway**: Site generation consuming Dress outputs

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
```

### 2. Build with dressing enabled

Create marker file and build:

```bash
mkdir -p .lake/build
echo "1" > .lake/build/.dress
lake build
rm .lake/build/.dress
```

Or use the script:

```bash
lake run dress
```

### 3. Generate Lake facets

```bash
lake build :blueprint                   # All modules
lake build MyProject.MyModule:blueprint # Specific module
```

### 4. Generate dependency graph

```bash
lake exe extract_blueprint graph MyProject.Module1 MyProject.Module2
```

## Architecture

```
                   LEAN PROJECT
import Dress    <-- Re-exports LeanArchitect

@[blueprint]
theorem foo : ... := by ...

                            |
                            | lake build (with marker file)
                            v
                         DRESS

PHASE 1: Elaboration-time (per @[blueprint] declaration)
+-------------------------------------------------------+
| Capture/ElabRules.lean                                |
|   - elab_rules fires after each declaration           |
|   - Checks for .dress marker file                     |
|   - Calls SubVerso highlightIncludingUnparsed         |
|   - Info trees are EPHEMERAL - must capture now       |
|                                                       |
| Generate/Declaration.lean                             |
|   - Splits code at := boundary (signature vs proof)   |
|   - Writes: decl.tex, decl.html, decl.json            |
+-------------------------------------------------------+
                            |
                            v
PHASE 2: Lake facet (per module, after compilation)
+-------------------------------------------------------+
| dressed facet                                         |
|   - Scans {label}/ directories for decl.json          |
|   - Aggregates into module.json                       |
|                                                       |
| blueprint facet                                       |
|   - Generates module.tex with \input{} paths          |
+-------------------------------------------------------+
                            |
                            v
PHASE 3: Graph generation (lake exe extract_blueprint graph)
+-------------------------------------------------------+
| Graph/Build.lean                                      |
|   - Loads all modules, extracts blueprint nodes       |
|   - Infers dependencies from actual Lean code traces  |
|   - Computes validation checks                        |
|                                                       |
| Graph/Layout.lean                                     |
|   - Sugiyama algorithm for hierarchical layout        |
|   - Visibility graph + Dijkstra edge routing          |
|   - Bezier curve fitting for smooth edges             |
|                                                       |
| Graph/Types.lean                                      |
|   - Transitive reduction (skipped for >100 nodes)     |
|                                                       |
| Main.lean                                             |
|   - Writes dep-graph.svg, dep-graph.json              |
|   - Writes manifest.json (stats + validation)         |
+-------------------------------------------------------+
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
    Build.lean              # Graph construction, validation checks
    Layout.lean             # Sugiyama algorithm, edge routing (~1450 lines)
    Json.lean               # JSON serialization for D3.js
    Render.lean             # SVG generation

  Serialize/                # Output format generation
    Json.lean               # SubVerso JSON serialization
    Html.lean               # HTML serialization
    Artifacts.lean          # Dressed artifact format

  Base64.lean               # RFC 4648 Base64 encoding
  Core.lean                 # Core types, splitAtDefinitionAssign
  HtmlRender.lean           # Verso HTML rendering wrapper
  Hook.lean                 # Main entry point, re-exports submodules
  Paths.lean                # Path utilities for artifact locations
```

## Dependency Graph

### Layout Algorithm

Full Sugiyama hierarchical layout in pure Lean:

1. **Cycle removal**: DFS to find back edges, temporarily reverse
2. **Layer assignment**: Longest-path algorithm for vertical positioning
3. **Crossing reduction**: Median heuristic + transpose passes
4. **Coordinate assignment**: Barycenter-based positioning with overlap resolution
5. **Edge routing**: Visibility graph + Dijkstra shortest path
6. **Curve fitting**: Catmull-Rom to Bezier conversion

### Node Shapes

| Shape | Used For |
|-------|----------|
| Box (rectangle) | Definitions, abbreviations, structures, classes |
| Ellipse | Theorems, lemmas, propositions |

### Edge Styles

| Style | Meaning |
|-------|---------|
| Solid | Proof dependency (used in proof body) |
| Dashed | Statement dependency (used in signature) |

### Node Status (8 states)

| Status | Color | Source |
|--------|-------|--------|
| notReady | Red/Gray | Manual: `(notReady := true)` |
| stated | Light Blue | Default (no Lean code) |
| ready | Orange | Manual: `(ready := true)` |
| hasSorry | Yellow | Derived: proof contains sorry |
| proven | Light Green | Derived: complete proof |
| fullyProven | Dark Green | Manual: `(fullyProven := true)` |
| mathlibReady | Purple | Manual: `(mathlibReady := true)` |
| inMathlib | Dark Blue | Manual: `(mathlib := true)` |

## Validation Checks

### Connectivity Check

Detects disconnected components. A disconnected graph may indicate:
- Missing dependencies (though `inferUses` traces real Lean code dependencies)
- Orphaned declarations that should be connected
- Logical gaps in proof structure (Tao-style errors)

### Cycle Detection

Identifies circular dependencies using DFS with gray/black node coloring:
- Mutual dependencies that may cause proof issues
- Incorrectly specified dependencies

Results written to `manifest.json` under `checks` field.

## Manifest Schema

```json
{
  "stats": {
    "notReady": 0,
    "stated": 5,
    "ready": 3,
    "hasSorry": 2,
    "proven": 10,
    "fullyProven": 15,
    "mathlibReady": 0,
    "inMathlib": 0,
    "total": 35
  },
  "keyDeclarations": ["thm:main", "thm:secondary"],
  "messages": [
    {"id": "thm:main", "label": "Main Theorem", "message": "Central result"}
  ],
  "projectNotes": {
    "priority": [{"id": "lem:helper", "label": "Helper Lemma"}],
    "blocked": [{"id": "thm:blocked", "label": "...", "reason": "..."}],
    "potentialIssues": [{"id": "lem:issue", "label": "...", "issue": "..."}],
    "technicalDebt": [{"id": "def:ugly", "label": "...", "debt": "..."}],
    "misc": [{"id": "thm:note", "label": "...", "note": "..."}]
  },
  "nodes": {"thm:main": "chapter1.html#thm:main"},
  "checks": {
    "isConnected": true,
    "numComponents": 1,
    "componentSizes": [35],
    "cycles": []
  }
}
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

## Dependencies

- [LeanArchitect](https://github.com/e-vergo/LeanArchitect) - `@[blueprint]` attribute
- [SubVerso](https://github.com/e-vergo/subverso) - Semantic highlighting (fork with optimizations)
- [Verso](https://github.com/leanprover/verso) - HTML rendering
- [Cli](https://github.com/mhuisi/lean4-cli) - Command-line interface

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

## Related Repositories

- [Runway](https://github.com/e-vergo/Runway) - Site generator
- [LeanArchitect](https://github.com/e-vergo/LeanArchitect) - Blueprint attribute
- [SubVerso](https://github.com/e-vergo/subverso) - Syntax highlighting
- [SBS-Test](https://github.com/e-vergo/SBS-Test) - Minimal test project
- [dress-blueprint-action](https://github.com/e-vergo/dress-blueprint-action) - GitHub Actions CI

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.
