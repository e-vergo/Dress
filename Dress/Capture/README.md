# Capture

Intercepts `@[blueprint]` declarations during Lean elaboration and captures SubVerso syntax highlighting while info trees are still available.

## Why Capture Exists

Info trees are ephemeral -- they exist in `commandState.infoState.trees` during elaboration but are discarded immediately afterward. SubVerso requires info trees to produce semantic syntax highlighting (identifier classification, type signatures, tactic info). This module captures highlighting data during the narrow window when info trees are available.

This capture accounts for 93-99% of total per-declaration build time. The remaining <1% covers TeX/HTML generation.

## Files

| File | Purpose |
|------|---------|
| `ElabRules.lean` | `elab_rules` and `@[command_elab]` hooks that intercept declarations with `@[blueprint]` attributes |
| `InfoTree.lean` | Environment extension (`dressedDeclExt`) for storing captured highlighting, plus core capture functions |
| `State.lean` | `IO.Ref Bool` (`blueprintCaptureHookRef`) preventing infinite recursion in elab hooks |
| `Config.lean` | `BlueprintConfig` parsing from `@[blueprint]` attribute syntax. `hasBlueprintAttr` checks if declModifiers contain blueprint |

## Data Flow

```
@[blueprint] declaration
    |
    v
ElabRules.elabBlueprintDeclaration (intercepts theorem/def/abbrev/structure/class/inductive/instance)
    |
    v
elabDeclAndCaptureHighlighting
    |-- withCaptureHookFlag (prevents recursion)
    |-- elabCommandTopLevel (standard elaboration)
    |-- captureHighlighting (while info trees exist)
    |       |
    |       +-- lazyCaptureHighlightingFromInfoTrees
    |               |-- getOrBuildSuffixIndex (cached per module, avoids O(E) fold)
    |               |-- getOrCreateModuleCacheRef (cross-declaration sharing)
    |               +-- lazyHighlightIncludingUnparsed (SubVerso, with caching)
    |
    +-- Generate.writeDeclarationArtifactsFromNode (writes .tex, .html, .json)
```

## Key Types and Functions

### ElabRules.lean

- **`elabBlueprintDeclaration`** -- `@[command_elab]` handler for theorem declarations. Checks for `@[blueprint]` attribute, prevents recursion, delegates to `elabDeclAndCaptureHighlighting`
- **`elab_rules`** -- Additional handlers for `def`, `abbrev`, `structure`, `class`, `inductive`, `instance` declarations
- **`elabDeclAndCaptureHighlighting`** -- Core function: runs standard elaboration, captures highlighting, writes artifacts. Includes timing traces via `trace[blueprint.timing]`
- **`#dressNodes`** -- Command for retroactive annotation support. Scans environment for blueprint nodes without artifacts and writes them (without highlighting, since elaboration has already completed)

### InfoTree.lean

- **`dressedDeclExt`** -- `NameMapExtension Highlighted` storing captured highlighting keyed by declaration name
- **`captureHighlighting`** -- Main entry point for the `@[blueprint]` handler. Extracts info trees from command state, filters messages to relevant range, calls SubVerso
- **`lazyCaptureHighlightingFromInfoTrees`** -- Incremental variant that checks module-level cache before performing full highlighting. Returns `HighlightResult` (cached, fresh, or skipped)
- **`captureHighlightingFromInfoTrees`** -- Non-lazy variant that always performs fresh highlighting
- **`moduleSuffixIndexRef`** -- Module-level suffix index cache. Built once per module on first `@[blueprint]` declaration to avoid O(E) `env.constants.fold` per declaration
- **`moduleCacheRef`** -- Module-level expression/signature cache passed to SubVerso for cross-declaration sharing
- **`IncrementalCaptureState`** -- Tracks declarations processed, cache hits, and re-highlights per module
- **`resetModuleCaches`** -- Clears suffix index, highlight cache, and statistics at module boundaries

### State.lean

- **`blueprintCaptureHookRef`** -- `IO.Ref Bool` flag. Set to `true` during capture to prevent `elab_rules` from recursively intercepting the `elabCommandTopLevel` call

### Config.lean

- **`BlueprintConfig`** -- Subset of LeanArchitect's config: `latexLabel`, `statement`, `proof`, `usesLabels`, `proofUsesLabels`, `latexEnv`, `trace`
- **`hasBlueprintAttr`** -- Checks `declModifiers` syntax for `@[blueprint]` or `@[Architect.blueprint]`
- **`extractBlueprintAttrSyntax`** -- Extracts the blueprint attribute syntax node from declModifiers
- **`parseBlueprintConfig`** -- Parses attribute syntax into `BlueprintConfig` (subset of full LeanArchitect parsing)

## Artifact Writing

Artifact writing is unconditional for all `@[blueprint]` declarations. The `BLUEPRINT_DRESS` environment variable gate was removed in issue #247 -- importing Dress is sufficient to enable artifact generation.

## Module-Level Caching

The capture module maintains three module-level caches to amortize costs across declarations within the same module:

1. **Suffix index** (`moduleSuffixIndexRef`): Built from `env.constants` on first use. Avoids O(E) fold per declaration where E can exceed 100K for mathlib-heavy projects.
2. **Highlight cache** (`moduleCacheRef`): Shared `IO.Ref ModuleHighlightCache` passed to SubVerso. Memoizes type signatures and expression renderings across declarations.
3. **Incremental stats** (`incrementalStateRef`): Tracks cache hit rate for profiling.

All three are cleared by `resetModuleCaches` at module boundaries.

## Connection to Adjacent Stages

- **Upstream**: LeanArchitect provides the `@[blueprint]` attribute definition and `blueprintExt` environment extension
- **Downstream**: `Generate.Declaration` writes artifacts; `Graph.Build` reads from `blueprintExt` to construct the dependency graph
