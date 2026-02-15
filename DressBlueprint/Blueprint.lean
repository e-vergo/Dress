/-
Verso blueprint document for Dress.
Presents the artifact generation and dependency graph architecture through key declarations
using the SBSBlueprint genre.
-/
import SBSBlueprint
import DressBlueprint

open Verso.Genre.SBSBlueprint

set_option maxHeartbeats 800000

#doc (SBSBlueprint) "Dress Blueprint" =>

# Introduction

Dress is the artifact generation and dependency graph layout engine for the Side-by-Side
Blueprint toolchain. It intercepts `@[blueprint]` declarations during Lean elaboration,
captures syntax highlighting via SubVerso, generates per-declaration artifacts (HTML, JSON,
LaTeX), constructs the dependency graph with Sugiyama hierarchical layout, and renders
side-by-side displays pairing formal Lean code with informal LaTeX mathematics.

# Core Types

The foundational data structures that extend LeanArchitect's `Node` with positioning,
highlighting, and content representation.

## Node With Position

Enriches a LeanArchitect `Node` with source position and captured syntax highlighting,
forming the primary data unit flowing through the Dress pipeline:

:::leanNode "dr:node-with-pos"
:::

## Blueprint Content

The sum type representing everything Dress captures from a `@[blueprint]` declaration --
statement text, proof text, highlighting data, and ordering metadata:

:::leanNode "dr:blueprint-content"
:::

## Label Sanitization

Translates LaTeX labels (containing colons) into filesystem-safe paths:

:::leanNode "dr:paths-sanitize-label"
:::

# Elaboration-Time Capture

Dress hooks into Lean's elaboration pipeline to capture highlighting and artifacts
as declarations are processed.

## SubVerso Highlighting Capture

The core integration point with SubVerso: extracts syntax highlighting from Lean's
info trees during elaboration, producing the colored token streams used in all
HTML rendering:

:::leanNode "dr:capture-highlighting-from-info-trees"
:::

## Elaboration Orchestrator

Coordinates the full capture pipeline for a single `@[blueprint]` declaration --
elaboration, highlighting extraction, and artifact writing:

:::leanNode "dr:elab-decl-and-capture"
:::

## Environment Extension

The persistent storage for captured blueprint data, keyed by declaration name,
surviving across elaboration boundaries:

:::leanNode "dr:dressed-decl-ext"
:::

# Artifact Generation

Per-declaration artifacts written to `.lake/build/dressed/` during elaboration.

## Artifact Writer

Writes the complete artifact bundle (HTML, JSON, LaTeX) for a single declaration
to its directory under `.lake/build/dressed/{Module}/{label}/`:

:::leanNode "dr:write-decl-artifacts-from-node"
:::

## LaTeX Generation

Converts a positioned node into its LaTeX representation, producing the `.tex`
artifact that gets `\input{}` by the module's `module.tex`:

:::leanNode "dr:generate-decl-tex-from-node"
:::

# Dependency Graph

The graph data structures and construction algorithms that produce the
dependency visualization.

## Graph Structure

The complete dependency graph: nodes with edges, status counts, and
validation results:

:::leanNode "dr:graph"
:::

## Graph Node

A node in the dependency graph, carrying label, status, shape, and
connection metadata:

:::leanNode "dr:graph-node"
:::

## Graph Construction

Builds the full graph from the Lean environment by loading all
`@[blueprint]` declarations, inferring dependencies, and computing
derived statuses:

:::leanNode "dr:from-environment"
:::

## Coverage Analysis

Identifies declarations that have `@[blueprint]` annotations versus those
that do not, measuring formalization coverage:

:::leanNode "dr:compute-coverage"
:::

## Fully Proven Computation

The O(V+E) iterative worklist algorithm that upgrades `proven` nodes to
`fullyProven` when all ancestors are also proven:

:::leanNode "dr:compute-fully-proven"
:::

## Cycle Detection

DFS with gray/black coloring to detect dependency cycles, which would
indicate logical circularity:

:::leanNode "dr:detect-cycles"
:::

## Component Analysis

BFS traversal to find connected components, detecting disconnected
subgraphs that may indicate missing dependency edges:

:::leanNode "dr:find-components"
:::

# Hierarchical Layout

The Sugiyama algorithm implementation that transforms the abstract graph
into positioned nodes with routed edges.

## Layout Output

The positioned graph: nodes with (x,y) coordinates and edges with
Bezier control points, ready for SVG rendering:

:::leanNode "dr:layout-graph"
:::

## Main Layout Function

The top-level Sugiyama layout: layer assignment, crossing reduction,
coordinate assignment, and edge routing:

:::leanNode "dr:layout"
:::

## Layer Assignment

Kahn's algorithm variant that assigns nodes to horizontal layers
respecting edge direction:

:::leanNode "dr:assign-layers"
:::

## Crossing Reduction

Iterative barycenter heuristic that reorders nodes within layers
to minimize edge crossings:

:::leanNode "dr:order-layers"
:::

# SVG Generation

Rendering the positioned graph into scalable vector graphics.

## SVG Element

The composable SVG element type -- either a raw element with tag,
attributes, and children, or a text node:

:::leanNode "dr:svg-element"
:::

## SVG Rendering

Produces the complete SVG document from a positioned graph, including
node shapes, edge paths, labels, and the status legend:

:::leanNode "dr:svg-render"
:::

## Coordinate System

The mapping between graph coordinates and SVG viewport coordinates,
handling the axis inversion and scaling:

:::leanNode "dr:coord-system"
:::

## Status Colors

The canonical hex color values for the six-status model, serving as
the single source of truth that CSS must match:

:::leanNode "dr:svg-get-status-color"
:::

# Side-by-Side Rendering

The HTML generation that pairs formal Lean code with informal LaTeX
statements in the blueprint chapter pages.

## Side-by-Side Renderer

The main renderer that produces the two-column layout: LaTeX statement
on the left, highlighted Lean code on the right:

:::leanNode "dr:render-side-by-side"
:::

## SBS Data

The data bundle passed to the side-by-side renderer, containing all
artifacts needed to produce the display:

:::leanNode "dr:sbs-data"
:::

## Verification Badge

The status indicator rendered in theorem headers, showing the node's
current formalization status with appropriate color and icon:

:::leanNode "dr:render-verification-badge"
:::

# Output Pipeline

Module-level and library-level output aggregation.

## Node to LaTeX

Converts a single node into its LaTeX representation with dependency
annotations:

:::leanNode "dr:node-to-latex"
:::

## Module LaTeX Output

Aggregates all nodes in a module into a single LaTeX output with
a header preamble:

:::leanNode "dr:module-to-latex-output"
:::

## File Writing

Writes the complete LaTeX output for a module to disk:

:::leanNode "dr:output-latex-results"
:::

## Library Index

Generates the library-level index file that defines `\inputleanmodule`
for including module content in LaTeX documents:

:::leanNode "dr:output-library-latex"
:::

# Serialization

JSON serialization for inter-process communication between Dress and Runway.

## Highlighting to JSON

Serializes SubVerso highlighting data into the JSON format consumed
by the HTML rendering pipeline:

:::leanNode "dr:highlighted-to-json"
:::

## HTML JSON Output

Produces the combined HTML+JSON representation used for interactive
code displays with hover information:

:::leanNode "dr:highlighting-map-to-html-json"
:::
