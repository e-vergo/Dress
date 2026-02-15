# Render

Side-by-side HTML rendering that pairs LaTeX theorem statements with syntax-highlighted Lean code.

## Files

| File | Purpose |
|------|---------|
| `SideBySide.lean` | Core types (`SbsData`, `SbsVariant`) and rendering functions for the side-by-side display grid |

## Display Layout

The `renderSideBySide` function produces a CSS grid with 2 columns and up to 5 rows:

```
+---------------------------+---------------------------+
| Row 1: Above (LaTeX)      | Spacer                    |  (optional)
+---------------------------+---------------------------+
| Row 2: Heading            | Spacer                    |
+---------------------------+---------------------------+
| Row 3: Statement (LaTeX)  | Signature (Lean)          |
+---------------------------+---------------------------+
| Row 4: Proof (LaTeX)      | Proof Body (Lean)         |
+---------------------------+---------------------------+
| Row 5: Below (LaTeX)      | Spacer                    |  (optional)
+---------------------------+---------------------------+
```

Rows 1 and 5 collapse when `above`/`below` content is not present.

## Key Types

### SbsData

All data needed to render a single side-by-side display:

| Field | Type | Description |
|-------|------|-------------|
| `id` | `String` | HTML element ID |
| `label` | `String` | Display label (e.g., "4.1.1") |
| `displayNumber` | `Option String` | Optional custom display number |
| `envType` | `String` | "theorem", "lemma", "definition", etc. |
| `status` | `NodeStatus` | Status for indicator dot |
| `statementHtml` | `String` | LaTeX statement rendered to HTML |
| `proofHtml` | `Option String` | LaTeX proof rendered to HTML |
| `signatureHtml` | `Option String` | Lean signature with syntax highlighting |
| `proofBodyHtml` | `Option String` | Lean proof body with syntax highlighting |
| `hoverData` | `Option String` | JSON for hover tooltips |
| `declNames` | `Array Lean.Name` | Fallback declaration names |
| `above` | `Option String` | Narrative content above the block |
| `below` | `Option String` | Narrative content below the block |

### SbsVariant

Two rendering variants:

- **`blueprint`** -- Used on blueprint chapter pages. Status shown as a colored dot button. Standard CSS class prefixes.
- **`paper blueprintUrl`** -- Used on paper pages. Status shown as verification badge ("Verified"/"In Progress"/"Not Started"). Link back to blueprint page. Paper-specific CSS class prefixes.

## Key Functions

- **`renderSideBySide`** -- Main entry point. Assembles the complete grid from `SbsData` and `SbsVariant`.
- **`renderHeadingCellBlueprint`** -- Heading with status dot button, optional blueprint/paper links
- **`renderHeadingCellPaper`** -- Heading with verification badge, status dot, blueprint link
- **`renderStatementCell`** -- LaTeX statement wrapped in `{envType}_thmcontent`
- **`renderSignatureCell`** -- Lean signature in `<pre class="lean-code hl lean">` with hover data attribute
- **`renderProofToggle`** -- Collapsible proof section with expand/collapse arrow
- **`renderProofLeanCell`** -- Lean proof body in `<pre>` with hover data

## Status Indicators

| Function | Returns |
|----------|---------|
| `statusToColor` | Hex color for the status dot |
| `statusToCssClass` | CSS class name (e.g., `"status-proven"`) |
| `statusToDisplayString` | Human-readable label (e.g., `"Fully Proven"`) |
| `statusToBadgeClass` | Badge CSS class: `"verified"`, `"in-progress"`, `"not-started"` |
| `statusToBadgeLabel` | Badge text: `"Verified"`, `"In Progress"`, `"Not Started"` |
| `renderVerificationBadge` | Full badge HTML matching `.verification-badge` CSS |

## Hover Integration

When `hoverData` is present, the signature and proof `<pre>` elements get a `data-lean-hovers` attribute containing escaped JSON. The `verso-code.js` script reads this attribute to initialize Tippy.js tooltips for interactive type information on hover.

## Connection to Adjacent Stages

- **Upstream**: `Graph.Types` provides `NodeStatus` for status indicators. `SbsData` is populated by Runway from per-declaration artifacts and LaTeX rendering output.
- **Downstream**: Used by Runway's `Traverse.lean` and `Html/Render.lean` paths to produce blueprint chapter pages and paper pages respectively.
