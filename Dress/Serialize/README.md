# Serialize

JSON and HTML serialization for SubVerso highlighting data. Provides reading, writing, and format conversion between SubVerso's internal representation and Dress's artifact formats.

## Files

| File | Purpose |
|------|---------|
| `Json.lean` | SubVerso Module format JSON serialization. Atomic file writes. Loading from cached files. |
| `Html.lean` | HTML serialization: renders highlighting to HTML strings, stores as JSON map (name -> HTML) |
| `Artifacts.lean` | Full dressed artifact format with HTML, base64 HTML, and base64 JSON per declaration |

## Output Formats

### SubVerso Module Format (`Json.lean`)

Compatible with `subverso-extract-mod` output. Uses SubVerso's deduplicated export format for compact storage.

```json
{
  "items": [
    {
      "range": null,
      "kind": "blueprint",
      "defines": ["MyProject.mainTheorem"],
      "code": { ... }
    }
  ]
}
```

### HTML Map Format (`Html.lean`)

Maps declaration names to pre-rendered HTML strings. Used by `Output.lean` to embed HTML directly without re-rendering.

```json
{
  "MyProject.mainTheorem": "<span class=\"hl\">...</span>",
  "MyProject.helper": "<span class=\"hl\">...</span>"
}
```

### Dressed Artifact Format (`Artifacts.lean`)

Full format with multiple representations per declaration:

```json
{
  "MyProject.mainTheorem": {
    "html": "<pre>...</pre>",
    "htmlBase64": "PHByZT4uLi48L3ByZT4=",
    "jsonBase64": "eyJoaWdobGlnaHRlZCI6Li4ufQ=="
  }
}
```

## Key Functions

### Json.lean

- **`highlightedToJson`** -- Serializes a single `Highlighted` value using SubVerso's `exportCode` deduplication
- **`highlightingMapToModuleJson`** -- Converts `NameMap Highlighted` to SubVerso Module JSON format
- **`writeJsonAtomic`** -- Writes JSON to file via write-to-temp-then-rename for crash safety (POSIX atomic rename)
- **`loadModuleJson`** -- Loads `NameMap Highlighted` from a SubVerso Module format JSON file. Returns empty map on failure.

### Html.lean

- **`highlightingMapToHtmlJson`** -- Renders all declarations to HTML and produces a JSON map
- **`writeHighlightingHtml`** -- Writes HTML map to file (via atomic write)
- **`loadHighlightingHtml`** -- Loads `NameMap String` (name -> HTML string) from a JSON map file

### Artifacts.lean

- **`toDressedArtifactsJson`** -- Produces the full dressed format with `html`, `htmlBase64`, and `jsonBase64` fields
- **`writeDressedArtifacts`** -- Writes dressed artifacts to file

## Connection to Adjacent Stages

- **Upstream**: `HtmlRender.lean` provides `renderHighlightedToHtml` used by `Html.lean` and `Artifacts.lean`
- **Downstream**: `Hook.lean` uses `writeJsonAtomic` and `loadModuleJson`. `Output.lean` uses `loadHighlightingHtml` to get pre-rendered HTML for LaTeX macros. `SubVersoExtract.lean` uses the Module format for loading cached data.
