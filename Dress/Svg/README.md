# Svg

Composable SVG primitive library for generating SVG documents programmatically. This module provides a typed, tree-structured SVG API independent of the dependency graph rendering.

The dependency graph SVG generation lives in `Graph/Svg.lean` and uses string-based rendering directly. This `Svg/` module provides a separate, composable API that can be used for other SVG output needs (plots, diagrams, custom visualizations).

## Files

| File | Purpose |
|------|---------|
| `Core.lean` | `SvgElement` inductive type (`.node`, `.text`, `.raw`), rendering to string, `svgDocument` wrapper |
| `Style.lean` | `Style` structure with fill, stroke, opacity, font, and text-anchor attributes |
| `Shapes.lean` | Shape primitives: `circle`, `rect`, `line`, `ellipse`, `polygon`, `polyline`, `path`, `dashedLine`, `curvePath` |
| `Text.lean` | Text rendering: `text`, `centeredText`, `label` (text with background rectangle) |
| `Transform.lean` | Transform wrappers: `translate`, `rotate`, `scale`, `group`, `withClass` |
| `Coordinate.lean` | `CoordSystem` for math-to-SVG coordinate mapping, with axes, ticks, grids, and function plotting |

## SvgElement Type

The core type is an inductive with three constructors:

```
inductive SvgElement where
  | node (tag : String) (attrs : Array (String × String)) (children : Array SvgElement)
  | text (content : String)   -- auto-escaped
  | raw (content : String)    -- inserted verbatim
```

Elements compose via nesting. `render` produces indented SVG strings.

## Style Structure

```lean
structure Style where
  fill : Option String             -- e.g., "#ff0000"
  stroke : Option String           -- e.g., "#333"
  strokeWidth : Option Float       -- e.g., 1.5
  strokeDasharray : Option String  -- e.g., "4,3"
  opacity : Option Float           -- 0.0 to 1.0
  fillOpacity : Option Float
  fontFamily : Option String       -- e.g., "Arial, sans-serif"
  fontSize : Option Float          -- in pixels
  fontWeight : Option String       -- e.g., "bold"
  textAnchor : Option String       -- "start", "middle", "end"
  dominantBaseline : Option String  -- "central", "hanging", etc.
```

Converted to attribute pairs via `Style.toAttrs`. Only non-`none` fields produce attributes.

## Shape Primitives (Shapes.lean)

| Function | Parameters | Description |
|----------|-----------|-------------|
| `circle` | `cx cy r` | Circle at center with radius |
| `rect` | `x y width height` | Rectangle with optional corner radius (`rx`) |
| `line` | `x1 y1 x2 y2` | Straight line between two points |
| `ellipse` | `cx cy rx ry` | Ellipse at center with radii |
| `polygon` | `points` | Closed polygon from point array |
| `polyline` | `points` | Open polyline from point array |
| `path` | `d` | SVG path from data string |
| `dashedLine` | `x1 y1 x2 y2` | Line with dash pattern (default "4,3") |
| `curvePath` | `points` | Builds SVG path data string from sampled points |

## Coordinate System (Coordinate.lean)

`CoordSystem` maps mathematical coordinates to SVG pixel coordinates:

```lean
structure CoordSystem where
  xMin, xMax : Float    -- Mathematical X range
  yMin, yMax : Float    -- Mathematical Y range
  svgWidth : Float      -- SVG pixel width
  svgHeight : Float     -- SVG pixel height
  padding : Float       -- Padding around plot area (default 40)
```

Key functions:
- **`toSvgX`** / **`toSvgY`** -- Convert math coordinates to SVG pixels (Y axis is flipped)
- **`axes`** -- Generate X and Y axis lines
- **`xTicks`** / **`yTicks`** -- Tick marks with labels
- **`grid`** -- Background grid lines
- **`plotFunction`** -- Plot `Float -> Float` by sampling points and creating a path

## Connection to Graph Rendering

The `Graph/Svg.lean` module uses its own string-based SVG rendering (predating this composable API). The `Svg/` module provides a cleaner, reusable alternative for any SVG generation need beyond dependency graphs. The two modules share `escapeXml` functionality.
