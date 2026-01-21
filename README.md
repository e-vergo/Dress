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
│  Capture/     ──► Intercepts elaboration of @[blueprint] decls      │
│       │           ElabRules.lean, InfoTree.lean, Config.lean        │
│       │                                                             │
│  Serialize/   ──► JSON and HTML output formats                      │
│       │           Json.lean, Html.lean, Artifacts.lean              │
│       │                                                             │
│  Generate/    ──► LaTeX with embedded base64 data                   │
│                   Latex.lean, Module.lean                           │
└─────────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────────┐
│                    OUTPUT ARTIFACTS                                 │
│                                                                     │
│  .lake/build/dressed/{Module/Path}.json     ← Dressed artifacts     │
│  .lake/build/blueprint/module/{Module}.tex  ← LaTeX fragments       │
│  .lake/build/blueprint/module/{Module}.artifacts/*.tex              │
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

  Generate/                   # LaTeX and module output
    Latex.lean                # Per-declaration LaTeX generation
    Module.lean               # Module-level output and export

  Base64.lean                 # RFC 4648 Base64 encoding
  Core.lean                   # Core types (NodeWithPos, splitAtDefinitionAssign)
  Highlighting.lean           # Source highlighting utilities
  HtmlRender.lean             # Verso HTML rendering wrapper
  Hook.lean                   # Main entry point, re-exports submodules
  Output.lean                 # #show_blueprint commands
  Load.lean                   # Module loading utilities
  SubVersoExtract.lean        # subverso-extract-mod integration
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

| Path | Description |
|------|-------------|
| `.lake/build/dressed/{Module/Path}.json` | Dressed artifacts with `html`, `htmlBase64`, `jsonBase64` |
| `.lake/build/dressed/{Module/Path}.subverso.json` | SubVerso Module format (for compatibility) |
| `.lake/build/blueprint/module/{Module/Path}.tex` | Per-module LaTeX with all declarations |
| `.lake/build/blueprint/module/{Module/Path}.artifacts/{label}.tex` | Individual declaration LaTeX files |
| `.lake/build/blueprint/library/{Library}.tex` | Library index with `\input{}` directives |

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

Dress defines several Lake facets for build integration:

| Facet | Level | Description |
|-------|-------|-------------|
| `dressed` | Module | Generate highlighting JSON |
| `blueprint` | Module/Library/Package | Generate LaTeX with highlighting |
| `blueprintPlain` | Module/Library/Package | Generate LaTeX without highlighting |
| `blueprintJson` | Module/Library/Package | Generate JSON output |
| `blueprintSafe` | Module/Library/Package | Generate with fallback on errors |

## How It Works

### Elaboration-Time Capture

Info trees (containing type information for hover tooltips) are ephemeral—they exist only during elaboration. Dress hooks into the elaboration process to capture this data before it's discarded:

1. **Hook registration**: `Capture/ElabRules.lean` registers `elab_rules` that fire after declarations
2. **Marker detection**: Checks for `.lake/build/.dress` file or `blueprint.dress` option
3. **Capture**: Calls SubVerso's `highlightIncludingUnparsed` on the declaration syntax
4. **Render**: Converts highlighting to HTML via Verso
5. **Export**: Writes JSON and LaTeX artifacts to `.lake/build/`

### Dressed Artifact Format

The `.json` files contain per-declaration entries:

```json
{
  "Module.declName": {
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
