# Generate

Writes per-declaration artifacts (`.tex`, `.html`, `.json`) during elaboration, with content-based caching for incremental builds.

## Files

| File | Purpose |
|------|---------|
| `Declaration.lean` | Per-declaration artifact writer with content-hash caching |
| `Latex.lean` | LaTeX content generation for `@[blueprint]` declarations |
| `Module.lean` | Module-level path helpers for SubVerso and HTML JSON output |

## Artifact Output

Each `@[blueprint]` declaration produces a subdirectory at `.lake/build/dressed/{Module/Path}/{sanitized-label}/`:

| File | Content |
|------|---------|
| `decl.tex` | LaTeX with `\begin{theorem}...\end{theorem}`, `\lean{}`, `\leansignaturesourcehtml{}`, `\leanproofsourcehtml{}`, `\leanhoverdata{}`, `\leanok` |
| `decl.html` | Syntax-highlighted HTML with rainbow bracket matching (via Verso's `toHtmlRainbow`) |
| `decl.json` | JSON metadata: `{"name": "...", "label": "...", "highlighting": {...}}` or `{"plainText": true}` |
| `decl.hovers.json` | Hover tooltip content mapping IDs to rendered content |
| `manifest.entry` | Label-to-path mapping: `{"label": "...", "path": "..."}` |

## Key Functions

### Declaration.lean

- **`writeDeclarationArtifactsFromNode`** -- Main entry point called by `Capture.ElabRules`. Takes declaration name, `Architect.Node`, optional highlighting, file path, and location. Computes content hash, checks cache, generates or restores artifacts.
- **`generateArtifacts`** -- Core generation logic (called on cache miss). Generates `.tex` via `Latex.generateDeclarationTexFromNode`, renders HTML via `HtmlRender.renderHighlightedWithHovers`, writes all files. Includes per-operation timing traces.

### Latex.lean

- **`generateDeclarationTexFromNode`** -- Generates `.tex` content from an `Architect.Node`. Uses `Node.inferUses` for dependency computation and `splitAtDefinitionAssign` to separate signature from proof body.
- **`getDefaultLatexEnv`** -- Determines LaTeX environment name (`"theorem"` or `"definition"`) from the Lean constant info.

#### LaTeX Output Format

```latex
\begin{theorem}
\label{thm:main}
\lean{MyProject.mainTheorem}
\leanposition{file.lean|10|0|25|3}
\leansignaturesourcehtml{base64...}
\leanproofsourcehtml{base64...}
\leanhoverdata{base64...}
\leanabove{base64...}
\leanbelow{base64...}
\uses{lem:helper,def:square}
\leanok
Statement text here.
\end{theorem}
\begin{proof}
\uses{lem:helper}
\leanok
Proof text here.
\end{proof}
```

All HTML and hover data is base64-encoded (via `Base64.encodeString`) to safely embed in LaTeX.

### Module.lean

- **`getSubVersoOutputPath`** -- Returns `.lake/build/dressed/{Module/Path}.subverso.json`
- **`getHighlightingHtmlOutputPath`** -- Returns `.lake/build/dressed/{Module/Path}.html.json`

## Content-Based Caching

Artifacts are cached in `.lake/build/dressed/.decl_cache/{hash}/` based on a content hash computed from:

- Declaration fully qualified name
- `Architect.Node` JSON (includes label, statement, proof, uses, metadata)
- Source file path
- Declaration position range
- Highlighting data (if present)

The hash is a 16-character hex string from `String.hash` (64-bit). On cache hit, all artifact files are copied from the cache directory to the target directory, bypassing HTML rendering and file generation.

## Retroactive Annotation Support

When declarations are annotated retroactively via `attribute [blueprint ...] Name`, the source code is already compiled and SubVerso highlighting is unavailable. In this case:

- `highlighting` is `none`
- A plain-text signature is generated from the environment (`ppExpr` for type, field listing for structures)
- HTML wraps the plain text in `<pre class="lean-code hl lean">`
- Hover data is empty (`{}`)

The `#dressNodes` command in `Capture/ElabRules.lean` triggers this path for all blueprint nodes missing artifacts.

## Connection to Adjacent Stages

- **Upstream**: `Capture.ElabRules` calls `writeDeclarationArtifactsFromNode` after capturing highlighting
- **Downstream**: Lake facets aggregate per-declaration artifacts. `Graph.Build.fromEnvironment` reads from `blueprintExt` for graph construction. Runway loads artifacts for side-by-side rendering.
