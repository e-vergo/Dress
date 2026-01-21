# Dress

**Dress** is an artifact generator for Lean 4 mathematical blueprints. It transforms declarations marked with `@[blueprint]` (from [LeanArchitect](https://github.com/e-vergo/LeanArchitect)) into richly-annotated LaTeX and HTML output with syntax highlighting and interactive hover tooltips.

## Overview

Dress captures "dressed" artifacts during Lean elaboration:

- **Semantic highlighting** via [SubVerso](https://github.com/leanprover/subverso)
- **Pre-rendered HTML** via [Verso](https://github.com/leanprover/verso)
- **LaTeX fragments** with embedded base64 hover data
- **Source positions** and type signatures

These artifacts integrate with [leanblueprint](https://github.com/PatrickMassot/leanblueprint) to produce interactive mathematical documentation where readers can hover over Lean code to see types and click to expand proofs.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                     YOUR LEAN PROJECT                               │
│  import Dress    ← Re-exports LeanArchitect                         │
│                                                                     │
│  @[blueprint]                                                       │
│  theorem foo : ... := by ...                                        │
└─────────────────────────────────────────────────────────────────────┘
                              │
                              │ lake build (with marker file)
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                         DRESS                                       │
│                                                                     │
│  Phase 1: Elaboration-time (per @[blueprint] declaration)           │
│    Capture/     ──► Intercepts elaboration, captures info trees     │
│    Generate/    ──► Writes per-declaration artifacts immediately    │
│                                                                     │
│  Phase 2: Lake facet (per module, after compilation)                │
│    lakefile     ──► Aggregates declarations into module-level files │
└─────────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    OUTPUT ARTIFACTS                                 │
│                                                                     │
│  .lake/build/dressed/{Module/Path}/                                 │
│      {label}/decl.tex      ← Per-declaration LaTeX                  │
│      {label}/decl.html     ← Per-declaration HTML                   │
│      {label}/decl.json     ← Per-declaration highlighting           │
│      module.json           ← Aggregated Dress format (Lake facet)   │
│      module.tex            ← Module header (Lake facet)             │
└─────────────────────────────────────────────────────────────────────┘
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

## Output Files

All artifacts are written to `.lake/build/dressed/{Module/Path}/`:

```
.lake/build/dressed/MyProject/MyModule/
├── thm-main/
│   ├── decl.tex       # LaTeX with \lean{}, \leanok, base64 data
│   ├── decl.html      # Syntax-highlighted HTML with hover data
│   └── decl.json      # {"name": "...", "label": "...", "highlighting": {...}}
├── lem-helper/
│   ├── decl.tex
│   ├── decl.html
│   └── decl.json
├── module.json        # Aggregated: {"DeclName": {"html", "htmlBase64", "jsonBase64"}}
└── module.tex         # \newleannode entries + \input{} directives
```

| Path | Written By | Description |
|------|------------|-------------|
| `{label}/decl.tex` | Elaboration | Per-declaration LaTeX |
| `{label}/decl.html` | Elaboration | Per-declaration HTML with hover |
| `{label}/decl.json` | Elaboration | Per-declaration highlighting data |
| `module.json` | Lake facet | Aggregated Dress format |
| `module.tex` | Lake facet | Module header with `\input{}` paths |

## CLI Reference

Dress provides the `extract_blueprint` executable:

```bash
# Extract single module
lake exe extract_blueprint single MyProject.MyModule

# Extract with pre-computed highlighting
lake exe extract_blueprint single --highlightedJson .lake/build/dressed/MyModule.json MyProject.MyModule

# Generate library index
lake exe extract_blueprint index MyLibrary "MyLib.Module1,MyLib.Module2"

# Output JSON instead of LaTeX
lake exe extract_blueprint single --json MyProject.MyModule
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

## How It Works

### Two-Phase Architecture

Info trees (containing type information for hover tooltips) are ephemeral—they exist only during elaboration. Dress uses a two-phase approach:

**Phase 1: Elaboration-time capture** (per `@[blueprint]` declaration)
1. `Capture/ElabRules.lean` registers `elab_rules` that fire after declarations
2. Checks for `.lake/build/.dress` marker file
3. Calls SubVerso's `highlightIncludingUnparsed` to capture highlighting
4. Immediately writes per-declaration artifacts (`decl.tex`, `decl.html`, `decl.json`)

**Phase 2: Lake facet aggregation** (per module, after compilation)
1. `dressed` facet scans declaration subdirectories for `decl.json` files
2. Parses each to extract name, label, and highlighting data
3. Aggregates into `module.json` (Dress format for leanblueprint)
4. `blueprint` facet generates `module.tex` with `\input{}` paths

### Artifact Formats

**Per-declaration JSON** (`{label}/decl.json`):
```json
{
  "name": "MyProject.MyModule.myTheorem",
  "label": "thm:my-theorem",
  "highlighting": { /* SubVerso Highlighted structure */ }
}
```

**Module JSON** (`module.json` - aggregated by Lake facet):
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

## Dependencies

- [LeanArchitect](https://github.com/e-vergo/LeanArchitect) - `@[blueprint]` attribute and metadata
- [SubVerso](https://github.com/leanprover/subverso) - Semantic highlighting
- [Verso](https://github.com/leanprover/verso) - HTML rendering
- [Cli](https://github.com/mhuisi/lean4-cli) - Command-line interface

## Integration with leanblueprint

Dress is designed to work with [leanblueprint](https://github.com/PatrickMassot/leanblueprint) for generating mathematical blueprint websites:

1. Build your project with Dress to generate artifacts
2. Reference modules in your `blueprint.tex` with `\inputleanmodule{Module.Name}`
3. Run `leanblueprint web` to generate the interactive website

## License

Apache 2.0 - see [LICENSE](LICENSE) for details.
