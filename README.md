# Dress

> **Experience Prototype**: This project is part of an experimental exploration of enhanced tooling for Lean mathematical blueprints. It integrates with forks of [SubVerso](https://github.com/e-vergo/subverso), [LeanArchitect](https://github.com/e-vergo/LeanArchitect), and [Runway](https://github.com/e-vergo/Runway) to enable side-by-side LaTeX/Lean display with interactive hover tooltips and proof toggling.

---

**Dress** is an artifact generator for Lean 4 mathematical blueprints. It transforms declarations marked with `@[blueprint]` (from [LeanArchitect](https://github.com/e-vergo/LeanArchitect)) into richly-annotated LaTeX and HTML output with syntax highlighting and interactive hover tooltips.

## Overview

Dress captures "dressed" artifacts during Lean elaboration:

- **Semantic highlighting** via [SubVerso](https://github.com/leanprover/subverso)
- **Pre-rendered HTML** via [Verso](https://github.com/leanprover/verso)
- **LaTeX fragments** with embedded base64 hover data
- **Source positions** and type signatures

Dress also builds the dependency graph and computes all dashboard metadata (statistics, validation checks, key declarations) upstream, guaranteeing soundness by coupling metadata generation to the build process.

## Pipeline Position

Dress sits in the middle of the Side-by-Side Blueprint toolchain:

```
LeanArchitect          Dress                   Runway
(metadata)      -->   (artifacts)      -->   (site generation)
                          |
                     manifest.json
                     (precomputed stats)
```

- **LeanArchitect**: Provides the `@[blueprint]` attribute with 7 metadata options
- **Dress**: Captures highlighting, writes artifacts, builds graph, computes stats
- **Runway**: Consumes artifacts and manifest.json to generate the final website

## Architecture

```
                     YOUR LEAN PROJECT
  import Dress    <-- Re-exports LeanArchitect

  @[blueprint]
  theorem foo : ... := by ...

                              |
                              | lake build (with marker file)
                              v
                           DRESS

  PHASE 1: Elaboration-time (per @[blueprint] declaration)
  +---------------------------------------------------------+
  | Capture/ElabRules.lean                                  |
  |   - elab_rules fires AFTER each declaration elaborates  |
  |   - Checks for .dress marker file                       |
  |   - Calls SubVerso's highlightIncludingUnparsed         |
  |   - Info trees are EPHEMERAL (only exist during elab)   |
  |   - MUST capture immediately or data is lost            |
  |                                                         |
  | Generate/Declaration.lean                               |
  |   - Splits code at := boundary (signature vs proof)     |
  |   - Writes per-declaration artifacts immediately        |
  |   - decl.tex, decl.html, decl.json                      |
  +---------------------------------------------------------+
                              |
                              v
  PHASE 2: Lake facet (per module, after compilation)
  +---------------------------------------------------------+
  | Lake facets: dressed, blueprint                         |
  |   - Scans {label}/ directories for decl.json files      |
  |   - Aggregates into module.json (Dress format)          |
  |   - Generates module.tex with \input{} paths            |
  +---------------------------------------------------------+
                              |
                              v
  PHASE 3: Graph generation (lake exe extract_blueprint graph)
  +---------------------------------------------------------+
  | Graph/Build.lean                                        |
  |   - Loads all modules, extracts @[blueprint] nodes      |
  |   - Infers dependencies from actual Lean code traces    |
  |   - Applies transitive reduction                        |
  |   - Computes validation checks (connectivity, cycles)   |
  |                                                         |
  | Graph/Layout.lean                                       |
  |   - Sugiyama algorithm for hierarchical layout          |
  |   - Visibility graph + Dijkstra edge routing            |
  |   - Bezier curve fitting for smooth edges               |
  |                                                         |
  | Main.lean                                               |
  |   - Writes dep-graph.svg, dep-graph.json                |
  |   - Writes manifest.json (stats + validation + notes)   |
  +---------------------------------------------------------+
                              |
                              v
                    OUTPUT ARTIFACTS

  .lake/build/dressed/{Module/Path}/
      {label}/decl.tex      <-- Per-declaration LaTeX
      {label}/decl.html     <-- Per-declaration HTML
      {label}/decl.json     <-- Per-declaration highlighting
      module.json           <-- Aggregated Dress format
      module.tex            <-- Module header (Lake facet)

  .lake/build/dressed/
      dep-graph.svg         <-- Dependency graph visualization
      dep-graph.json        <-- Layout data for D3.js
      manifest.json         <-- Stats, validation, project notes
```

## Module Structure

```
Dress/
  Capture/                    # Elaboration-time artifact capture
    State.lean                # IO.Ref state management
    InfoTree.lean             # Environment extension, info tree capture
    Config.lean               # Blueprint config parsing
    ElabRules.lean            # elab_rules for @[blueprint] declarations

  Serialize/                  # Output format generation
    Json.lean                 # SubVerso JSON serialization
    Html.lean                 # HTML serialization
    Artifacts.lean            # Dressed artifact format (html, htmlBase64, jsonBase64)

  Generate/                   # LaTeX and artifact output
    Latex.lean                # Per-declaration LaTeX generation
    Declaration.lean          # Per-declaration artifact writer (tex, html, json)
    Module.lean               # Module-level utilities

  Graph/                      # Dependency graph construction
    Types.lean                # Node, Edge, Graph, StatusCounts, CheckResults
    Build.lean                # Graph construction, validation checks
    Layout.lean               # Sugiyama algorithm, edge routing
    Json.lean                 # JSON serialization for D3.js
    Render.lean               # SVG generation

  Base64.lean                 # RFC 4648 Base64 encoding
  Core.lean                   # Core types (NodeWithPos, splitAtDefinitionAssign)
  Highlighting.lean           # Source highlighting utilities
  HtmlRender.lean             # Verso HTML rendering wrapper
  Hook.lean                   # Main entry point, re-exports submodules
  Paths.lean                  # Path utilities for artifact locations
  Output.lean                 # #show_blueprint commands
  Load.lean                   # Module loading utilities
  Content.lean                # Content extraction utilities
```

## Installation

Add Dress to your project's `lakefile.toml`:

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

### 1. Import Dress in your Lean files

```lean
import Dress  -- Re-exports LeanArchitect's @[blueprint] attribute

@[blueprint "thm:my-theorem"]
theorem myTheorem : 2 + 2 = 4 := rfl
```

### 2. Build with dressing enabled

Create a marker file to enable artifact generation during build:

```bash
mkdir -p .lake/build
echo "1" > .lake/build/.dress
lake build
rm .lake/build/.dress
```

Or use the `dress` script if running from the Dress package:

```bash
lake run dress
```

### 3. Generate library index (optional)

For leanblueprint integration, generate the library-level index:

```bash
lake build :blueprint
```

### 4. Generate dependency graph

Generate the SVG, JSON, and manifest files:

```bash
lake exe extract_blueprint graph MyProject.Module1 MyProject.Module2
```

## Output Files

### Per-Declaration Artifacts

All artifacts are written to `.lake/build/dressed/{Module/Path}/`:

```
.lake/build/dressed/MyProject/MyModule/
  thm-main/
    decl.tex       # LaTeX with \lean{}, \leanok, base64 data
    decl.html      # Syntax-highlighted HTML with hover data
    decl.json      # {"name": "...", "label": "...", "highlighting": {...}}
  lem-helper/
    decl.tex
    decl.html
    decl.json
  module.json        # Aggregated: {"DeclName": {"html", "htmlBase64", "jsonBase64"}}
  module.tex         # \newleannode entries + \input{} directives
```

| Path | Written By | Description |
|------|------------|-------------|
| `{label}/decl.tex` | Elaboration | Per-declaration LaTeX |
| `{label}/decl.html` | Elaboration | Per-declaration HTML with hover |
| `{label}/decl.json` | Elaboration | Per-declaration highlighting data |
| `module.json` | Lake facet | Aggregated Dress format |
| `module.tex` | Lake facet | Module header with `\input{}` paths |

### Graph Outputs

Written to `.lake/build/dressed/`:

| File | Description |
|------|-------------|
| `dep-graph.svg` | Standalone SVG visualization |
| `dep-graph.json` | Layout data for D3.js rendering |
| `manifest.json` | Stats, validation, key declarations, project notes |

## Dependency Graph

### Layout Algorithm

Dress implements a full Sugiyama hierarchical layout algorithm in pure Lean:

1. **Cycle removal**: DFS to find back edges, temporarily reverse to make acyclic
2. **Layer assignment**: Longest-path algorithm for vertical positioning
3. **Crossing reduction**: Median heuristic + transpose passes
4. **Coordinate assignment**: Barycenter-based positioning with overlap resolution
5. **Edge routing**: Visibility graph + Dijkstra shortest path
6. **Curve fitting**: Catmull-Rom to Bezier conversion for smooth edges

### Node Shapes

| Shape | Used For |
|-------|----------|
| **Box** (rectangle) | Definitions, abbreviations, structures, classes |
| **Ellipse** | Theorems, lemmas, propositions |

### Edge Styles

| Style | Meaning |
|-------|---------|
| **Solid** | Proof dependency (used in the proof body) |
| **Dashed** | Statement dependency (used in the statement/signature) |

### Node Status (8 states)

| Status | Meaning |
|--------|---------|
| `notReady` | Dependencies not satisfied |
| `stated` | Has LaTeX statement, no Lean implementation |
| `ready` | Dependencies satisfied, ready to prove |
| `hasSorry` | Contains sorry in proof |
| `proven` | Proved but dependencies may have sorry |
| `fullyProven` | Proved with all dependencies fully proven |
| `mathlibReady` | Ready for mathlib submission |
| `inMathlib` | Already in mathlib |

## Validation Checks

Dress computes validation checks that help catch errors in the dependency graph:

### Connectivity Check

Detects disconnected components in the graph. A disconnected graph may indicate:
- Missing `\uses{}` annotations (though `inferUses` traces real dependencies)
- Orphaned declarations that should be connected
- Logical gaps in the proof structure (Tao-style errors)

### Cycle Detection

Identifies circular dependencies using DFS with gray/black node coloring. Cycles indicate:
- Mutual dependencies that may cause proof issues
- Incorrectly specified dependencies

Results are written to `manifest.json` under the `checks` field.

## Dashboard Statistics

Dress computes status counts upstream during graph generation, providing a soundness guarantee (stats are coupled to the build):

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
  }
}
```

These statistics power the dashboard in Runway without any recomputation.

## Manifest.json Schema

The manifest.json file contains all dashboard metadata:

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
    {
      "id": "thm:main",
      "label": "Main Theorem",
      "message": "This is the central result"
    }
  ],
  "projectNotes": {
    "priority": [
      {"id": "lem:helper", "label": "Helper Lemma"}
    ],
    "blocked": [
      {"id": "thm:blocked", "label": "Blocked Theorem", "reason": "Waiting for mathlib PR"}
    ],
    "potentialIssues": [
      {"id": "lem:issue", "label": "Lemma with Issue", "issue": "May need stronger hypothesis"}
    ],
    "technicalDebt": [
      {"id": "def:ugly", "label": "Ugly Definition", "debt": "Should refactor to use simp"}
    ],
    "misc": [
      {"id": "thm:note", "label": "Some Theorem", "note": "Consider generalizing"}
    ]
  },
  "nodes": {
    "thm:main": "chapter1.html#thm:main",
    "lem:helper": "chapter2.html#lem:helper"
  },
  "checks": {
    "isConnected": true,
    "numComponents": 1,
    "componentSizes": [35],
    "cycles": []
  }
}
```

| Field | Description |
|-------|-------------|
| `stats` | Status counts for dashboard progress display |
| `keyDeclarations` | IDs of nodes marked with `keyTheorem := true` |
| `messages` | User notes from `message := "..."` attribute |
| `projectNotes` | Structured notes (priority, blocked, issues, debt, misc) |
| `nodes` | Mapping from node ID to URL path |
| `checks` | Validation results (connectivity, cycles) |

## Artifact Formats

### Per-declaration JSON (`{label}/decl.json`)

```json
{
  "name": "MyProject.MyModule.myTheorem",
  "label": "thm:my-theorem",
  "highlighting": { /* SubVerso Highlighted structure */ }
}
```

### Module JSON (`module.json` - aggregated by Lake facet)

```json
{
  "MyProject.MyModule.myTheorem": {
    "html": "<span class='hl'>...</span>",
    "htmlBase64": "PHNwYW4gY2xhc3M9...",
    "jsonBase64": "eyJraW5kIjoic2Vx..."
  }
}
```

### LaTeX Format

Generated `.tex` files use leanblueprint macros:

```latex
\begin{theorem}\label{thm:my-theorem}\lean{myTheorem}\leanok
\leansignaturesourcehtml{base64-encoded-signature-html}
\leanproofsourcehtml{base64-encoded-proof-html}
\leanhoverdata{base64-encoded-hover-json}
The statement text from @[blueprint (statement := ...)]
\end{theorem}
\begin{proof}\leanok
The proof text from @[blueprint (proof := ...)]
\end{proof}
```

### Signature/Proof Splitting

Dress automatically splits theorem code at the `:=` boundary:

- **Signature** (`\leansignaturesourcehtml`) - Everything up to and including `:=` plus the `by` keyword
- **Proof body** (`\leanproofsourcehtml`) - The tactic proof after `by`

This enables leanblueprint to render the signature always visible while the proof body can be toggled, synchronized with the LaTeX proof expansion.

### Dependency Graph JSON (`dep-graph.json`)

D3.js-compatible format with pre-computed layout:

```json
{
  "nodes": [
    {
      "id": "thm:main",
      "label": "Main Theorem",
      "envType": "theorem",
      "status": "fullyProven",
      "shape": "ellipse",
      "x": 150.0,
      "y": 50.0,
      "width": 120.0,
      "height": 40.0
    }
  ],
  "edges": [
    {
      "from": "lem:helper",
      "to": "thm:main",
      "style": "solid",
      "points": [[100.0, 100.0], [125.0, 75.0], [150.0, 50.0]]
    }
  ],
  "width": 800.0,
  "height": 600.0
}
```

## CLI Reference (Deprecated)

> **Deprecated**: The `extract_blueprint` CLI is deprecated for single/index commands. Use Lake facets instead:
> ```bash
> lake build :blueprint                   # Generate for all modules
> lake build MyProject.MyModule:blueprint # Generate for specific module
> ```

The `graph` subcommand is still the primary way to generate dependency graphs:

```bash
# Generate dependency graph (recommended)
lake exe extract_blueprint graph MyLib.Module1 MyLib.Module2

# Extract single module (DEPRECATED)
lake exe extract_blueprint single MyProject.MyModule

# Generate library index (DEPRECATED)
lake exe extract_blueprint index MyLibrary MyLib.Module1 MyLib.Module2
```

## Lake Facets

Dress defines Lake facets for build integration:

| Facet | Level | Description |
|-------|-------|-------------|
| `dressed` | Module | Aggregate per-declaration artifacts into `module.json` |
| `blueprint` | Module/Library/Package | Generate `module.tex` with `\input{}` paths |

Example usage:
```bash
lake build MyProject.MyModule:dressed   # Generate module.json
lake build MyProject.MyModule:blueprint # Generate module.tex
lake build :blueprint                   # Generate for all modules
```

## Dependencies

- [LeanArchitect](https://github.com/e-vergo/LeanArchitect) - `@[blueprint]` attribute and metadata
- [SubVerso](https://github.com/leanprover/subverso) - Semantic highlighting
- [Verso](https://github.com/leanprover/verso) - HTML rendering
- [Cli](https://github.com/mhuisi/lean4-cli) - Command-line interface

## Integration with Runway

Dress is designed to work with [Runway](https://github.com/e-vergo/Runway) for generating mathematical blueprint websites:

1. Build your project with Dress to generate artifacts
2. Run `lake exe extract_blueprint graph` to generate dependency graph and manifest
3. Configure `runway.json` with paths to artifacts and assets
4. Run `lake exe runway build` to generate the interactive website

Runway consumes:
- Per-declaration artifacts from `.lake/build/dressed/{Module/Path}/`
- `manifest.json` for dashboard data (precomputed, no recomputation)
- `dep-graph.json` and `dep-graph.svg` for visualization

## Integration with leanblueprint (Legacy)

Dress also works with the original [leanblueprint](https://github.com/PatrickMassot/leanblueprint):

1. Build your project with Dress to generate artifacts
2. Reference modules in your `blueprint.tex` with `\inputleanmodule{Module.Name}`
3. Run `leanblueprint web` to generate the interactive website

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.
