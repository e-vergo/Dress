# Dress Module

Artifact generation for Lean 4 mathematical blueprints. Transforms `@[blueprint]`-decorated declarations into syntax-highlighted HTML, LaTeX with embedded hover data, and dependency graphs with hierarchical layout.

## Pipeline Overview

```
Capture --> Generate --> Graph --> Render
   |            |          |         |
   |            |          |         +-- SideBySide.lean (HTML grid rendering)
   |            |          +-- Layout, Build, Svg, Json, Subgraph, Types
   |            +-- Declaration.lean, Latex.lean, Module.lean
   +-- ElabRules.lean, InfoTree.lean, State.lean, Config.lean
```

Additionally, **Serialize/** handles JSON/HTML serialization of SubVerso highlighting data, and **Svg/** provides composable SVG primitives used outside the graph context.

## Module Map

| Directory | Purpose |
|-----------|---------|
| [`Capture/`](Capture/) | Intercepts `@[blueprint]` declarations during elaboration and captures SubVerso highlighting from info trees |
| [`Generate/`](Generate/) | Writes per-declaration artifacts (`.tex`, `.html`, `.json`) with content-based caching |
| [`Graph/`](Graph/) | Builds dependency graphs, runs Sugiyama layout, generates SVG and JSON output |
| [`Render/`](Render/) | Produces side-by-side HTML displays pairing LaTeX statements with Lean code |
| [`Serialize/`](Serialize/) | JSON and HTML serialization for SubVerso highlighting data |
| [`Svg/`](Svg/) | Composable SVG primitive library (shapes, text, transforms, coordinate systems) |

## Top-Level Files

| File | Purpose |
|------|---------|
| `Hook.lean` | Main entry point. Re-exports all submodules. Provides `writeModuleHighlightingJson`, `loadModuleHighlighting`, and other convenience functions |
| `Core.lean` | `NodeWithPos` type extending `Architect.Node` with position, highlighting, and HTML fields. Contains `splitAtDefinitionAssign` for separating signature from proof body |
| `Content.lean` | `BlueprintContent` inductive (`.node` or `.modDoc`) with declaration-range ordering. `getBlueprintContents` retrieves sorted blueprint contents for a module |
| `Highlighting.lean` | SubVerso highlighting integration. `highlightSource`, `highlightDeclaration` for re-elaboration with info tree context |
| `HtmlRender.lean` | Thin wrapper around Verso's `toHtmlRainbow`. Provides `renderHighlightedWithHovers`, `renderHighlightedBlock`, `renderHighlightedInline` |
| `Output.lean` | LaTeX and JSON output. `NodeWithPos.toLatex` emits `\leansignaturesourcehtml`, `\leanproofsourcehtml`, `\leanhoverdata` macros with base64-encoded content |
| `Load.lean` | `envOfImports`, `runEnvOfImports` for loading Lean environments. `latexOutputOfImportModule` and `jsonOfImportModule` for module-level output |
| `Paths.lean` | Centralized path construction for all artifact locations under `.lake/build/dressed/` |
| `SubVersoExtract.lean` | Interface to `subverso-extract-mod` CLI. `loadHighlightingWithFallback` prefers cached JSON, falls back to running the CLI |
| `Base64.lean` | RFC 4648 Base64 encoding for embedding HTML/JSON in LaTeX macros |
| `Cache.lean` | Content-hash-based caching for per-declaration artifacts. Stores cached artifacts in `.lake/build/dressed/.decl_cache/{hash}/` |
| `Render.lean` | Re-exports `Render.SideBySide` |
| `Svg.lean` | Re-exports all `Svg/` submodules |

## Key Entry Points

- **During elaboration**: `Capture.ElabRules` intercepts `@[blueprint]` declarations, calls `Capture.InfoTree.captureHighlighting`, then `Generate.Declaration.writeDeclarationArtifactsFromNode`
- **CLI (`extract_blueprint graph`)**: `Main.lean` loads modules via `Graph.fromEnvironment`, applies transitive reduction, runs `Layout.layout`, writes SVG/JSON/manifest
- **Downstream (Runway)**: Loads `manifest.json` for dashboard data, per-declaration artifacts for side-by-side rendering, `dep-graph.svg`/`dep-graph.json` for the dependency graph page

## Artifact Output Structure

```
.lake/build/dressed/
├── manifest.json                     # Stats, checks, dashboard metadata
├── dep-graph.svg                     # Full dependency graph SVG
├── dep-graph.json                    # Layout data with adjacency lists
├── subgraphs/                        # Pre-rendered per-node subgraph SVGs
│   ├── metadata.json                 # Max depth info per node
│   ├── cache-manifest.json           # Content hashes for incremental updates
│   └── {sanitized-label}/
│       ├── ancestors-1.svg ... ancestors-5.svg
│       ├── descendants-1.svg ... descendants-5.svg
│       └── both-1.svg ... both-5.svg
├── .decl_cache/                      # Content-hash cache
│   └── {hash}/
│       ├── decl.tex, decl.html, decl.json, decl.hovers.json, manifest.entry
└── {Module/Path}/
    ├── module.json                   # Module-level metadata
    ├── module.tex                    # Module-level LaTeX header
    └── {sanitized-label}/            # Per-declaration artifacts
        ├── decl.tex                  # LaTeX with highlighting macros
        ├── decl.html                 # Syntax-highlighted HTML
        ├── decl.json                 # JSON metadata with highlighting data
        ├── decl.hovers.json          # Hover tooltip content
        └── manifest.entry            # Label-to-path mapping
```

## Dependencies

| Package | Role |
|---------|------|
| LeanArchitect | `@[blueprint]` attribute, `Node`, `NodeStatus`, `inferUses` |
| SubVerso | Syntax highlighting extraction with O(1) indexed lookups |
| Verso | HTML rendering with rainbow bracket matching (`toHtmlRainbow`) |
| Cli | Command-line interface for `extract_blueprint` |
