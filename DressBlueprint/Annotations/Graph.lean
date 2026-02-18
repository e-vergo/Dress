/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress

open Dress

namespace DressBlueprint.Annotations

/-! # Graph Module: Core Types (Dress.Graph.Types)

Fundamental data structures for dependency graph construction and visualization.
These types form the backbone of the entire graph pipeline. -/

attribute [blueprint "dr:node-shape"
  (title := "Node Shape")
  (statement := /-- Inductive type for visual shape of graph nodes.
  Three variants: \texttt{box} for definitions/structures/classes,
  \texttt{ellipse} for theorems/lemmas, and \texttt{diamond} for axioms.
  Controls SVG rendering geometry. -/)
  (proof := /-- Simple three-variant inductive with derived \texttt{Repr},
  \texttt{Inhabited}, and \texttt{BEq} instances. -/)]
  Dress.Graph.NodeShape

attribute [blueprint "dr:edge-style"
  (title := "Edge Style")
  (statement := /-- Inductive type for visual style of graph edges.
  Two variants: \texttt{solid} for proof dependencies (direct usage in proof body)
  and \texttt{dashed} for statement dependencies (uses clause). -/)
  (proof := /-- Simple two-variant inductive with derived instances. -/)]
  Dress.Graph.EdgeStyle

attribute [blueprint "dr:graph-node"
  (title := "Graph Node")
  (keyDeclaration := true)
  (statement := /-- A node in the dependency graph, representing a single
  \texttt{@[blueprint]}-annotated declaration. Carries:
  \begin{itemize}
  \item Unique string identifier (the LaTeX label)
  \item Display label (title or qualified Lean name)
  \item Environment type (theorem, def, axiom, etc.)
  \item \texttt{NodeStatus} from the 7-status color model
  \item Shape for SVG rendering
  \item URL for HTML navigation
  \item Source module name
  \item All 8 metadata fields from \texttt{@[blueprint]} attribute
  \end{itemize} -/)
  (proof := /-- Structure with 16 fields. Default values for optional fields.
  Derives \texttt{Repr} and \texttt{Inhabited}. -/)
  (uses := ["dr:node-shape"])]
  Dress.Graph.Node

attribute [blueprint "dr:status-counts"
  (title := "Status Counts")
  (statement := /-- Aggregate counts of nodes by their \texttt{NodeStatus} value.
  Tracks all 7 status categories plus total node count.
  Used for dashboard statistics display. -/)
  (proof := /-- Structure with 8 \texttt{Nat} fields, all defaulting to 0.
  Derives \texttt{ToJson} and \texttt{FromJson} for manifest serialization. -/)]
  Dress.Graph.StatusCounts

attribute [blueprint "dr:graph-edge"
  (title := "Graph Edge")
  (statement := /-- An edge in the dependency graph connecting two nodes.
  Edge semantics: \texttt{from\_ = A, to = B} means B depends on A.
  Carries style (solid for proof deps, dashed for statement deps) and
  a flag indicating whether the edge was reversed during acyclic transformation. -/)
  (proof := /-- Structure with 4 fields. Default style is solid, default
  \texttt{isReversed} is false. Derives \texttt{BEq} for deduplication. -/)
  (uses := ["dr:edge-style"])]
  Dress.Graph.Edge

attribute [blueprint "dr:graph"
  (title := "Dependency Graph")
  (keyDeclaration := true)
  (statement := /-- The complete dependency graph: an array of nodes and an array
  of edges. This is the primary data structure produced by graph construction
  and consumed by layout, validation, SVG rendering, and JSON serialization. -/)
  (proof := /-- Simple structure with two \texttt{Array} fields.
  Derives \texttt{Repr} and \texttt{Inhabited}. -/)
  (uses := ["dr:graph-node", "dr:graph-edge"])]
  Dress.Graph.Graph

attribute [blueprint "dr:adj-index"
  (title := "Adjacency Index")
  (statement := /-- Precomputed adjacency index for $O(1)$ edge lookups.
  Stores outgoing and incoming edges indexed by node ID using
  \texttt{Std.HashMap}. Avoids $O(E)$ linear scans per node during
  BFS, DFS, and layout algorithms. -/)
  (proof := /-- Structure with two \texttt{Std.HashMap String (Array Edge)} fields:
  \texttt{outgoing} and \texttt{incoming}. -/)
  (uses := ["dr:graph-edge"])]
  Dress.Graph.AdjIndex

attribute [blueprint "dr:build-adj-index"
  (title := "Build Adjacency Index")
  (statement := /-- Constructs the precomputed adjacency index from a graph in $O(V + E)$ time.
  Initializes empty arrays for each node, then iterates edges once to populate
  both outgoing and incoming maps. -/)
  (proof := /-- Single pass over nodes (initialize) then single pass over edges
  (populate). Uses mutable \texttt{HashMap} updates in \texttt{Id.run do}. -/)
  (uses := ["dr:graph", "dr:adj-index"])]
  Dress.Graph.Graph.buildAdjIndex

attribute [blueprint "dr:adj-out-edges"
  (title := "Outgoing Edge Lookup")
  (statement := /-- $O(1)$ lookup of outgoing edges from a node via the adjacency index.
  Returns empty array if the node ID is not found. -/)
  (proof := /-- Direct \texttt{HashMap.get?} with \texttt{getD} fallback. -/)
  (uses := ["dr:adj-index"])]
  Dress.Graph.AdjIndex.outEdges

attribute [blueprint "dr:adj-in-edges"
  (title := "Incoming Edge Lookup")
  (statement := /-- $O(1)$ lookup of incoming edges to a node via the adjacency index.
  Returns empty array if the node ID is not found. -/)
  (proof := /-- Direct \texttt{HashMap.get?} with \texttt{getD} fallback. -/)
  (uses := ["dr:adj-index"])]
  Dress.Graph.AdjIndex.inEdges

attribute [blueprint "dr:graph-node-ids"
  (title := "Graph Node IDs")
  (statement := /-- Extracts an array of all node ID strings from the graph.
  Convenience accessor for iteration and lookup. -/)
  (proof := /-- Maps \texttt{Node.id} over \texttt{g.nodes}. -/)
  (uses := ["dr:graph"])]
  Dress.Graph.Graph.nodeIds

attribute [blueprint "dr:graph-get-node"
  (title := "Get Node by ID")
  (statement := /-- Looks up a node by its string ID. Returns \texttt{Option Node}.
  Linear scan over nodes array. -/)
  (proof := /-- Uses \texttt{Array.find?} with ID equality predicate. -/)
  (uses := ["dr:graph"])]
  Dress.Graph.Graph.getNode?

attribute [blueprint "dr:graph-out-edges"
  (title := "Graph Outgoing Edges (Linear)")
  (statement := /-- Gets outgoing edges from a node using $O(E)$ linear scan.
  For performance-sensitive code, prefer \texttt{AdjIndex.outEdges}. -/)
  (proof := /-- Filters \texttt{g.edges} by \texttt{from\_} field. -/)
  (uses := ["dr:graph"])]
  Dress.Graph.Graph.outEdges

attribute [blueprint "dr:graph-in-edges"
  (title := "Graph Incoming Edges (Linear)")
  (statement := /-- Gets incoming edges to a node using $O(E)$ linear scan.
  For performance-sensitive code, prefer \texttt{AdjIndex.inEdges}. -/)
  (proof := /-- Filters \texttt{g.edges} by \texttt{to} field. -/)
  (uses := ["dr:graph"])]
  Dress.Graph.Graph.inEdges

attribute [blueprint "dr:compute-status-counts"
  (title := "Compute Status Counts")
  (statement := /-- Computes aggregate \texttt{StatusCounts} from all nodes in the graph.
  Iterates once over nodes, incrementing the appropriate counter for each
  node's status. Also counts axioms by environment type string comparison. -/)
  (proof := /-- Single-pass fold with mutable \texttt{StatusCounts} accumulator.
  Pattern-matches on \texttt{NodeStatus} variants. -/)
  (uses := ["dr:graph", "dr:status-counts"])]
  Dress.Graph.Graph.computeStatusCounts

attribute [blueprint "dr:soundness-result"
  (title := "Soundness Result")
  (statement := /-- Result of a single soundness check. Carries the check name,
  pass/fail boolean, optional detail message, and optional URL for
  dashboard linking. Used in \texttt{CheckResults.soundnessResults}. -/)
  (proof := /-- Structure with 4 fields. Derives \texttt{ToJson} and \texttt{FromJson}. -/)]
  Dress.Graph.SoundnessResult

attribute [blueprint "dr:uncovered-decl"
  (title := "Uncovered Declaration")
  (statement := /-- A declaration that lacks a \texttt{@[blueprint]} annotation.
  Records the fully qualified name, module, and kind (theorem, def, etc.)
  for coverage reporting. -/)
  (proof := /-- Structure with 3 string fields. Derives \texttt{ToJson} and \texttt{FromJson}. -/)]
  Dress.Graph.UncoveredDecl

attribute [blueprint "dr:coverage-result"
  (title := "Coverage Result")
  (statement := /-- Blueprint coverage results for a project. Reports total eligible
  declarations, number covered by \texttt{@[blueprint]}, percentage, and
  the list of uncovered declarations. -/)
  (proof := /-- Structure with 4 fields including \texttt{Float} percentage
  and \texttt{Array UncoveredDecl}. -/)
  (uses := ["dr:uncovered-decl"])]
  Dress.Graph.CoverageResult

attribute [blueprint "dr:check-results"
  (title := "Check Results")
  (statement := /-- Results of graph validation checks. Includes:
  \begin{itemize}
  \item Connectivity: whether the graph is a single connected component
  \item Component sizes for disconnected graphs
  \item Detected cycles (each cycle as an array of node IDs)
  \item Kernel verification: whether all key declarations are fully proven
  \item Project-specific soundness results
  \item Blueprint coverage data
  \end{itemize}
  Serialized into \texttt{manifest.json} for dashboard display. -/)
  (proof := /-- Structure with 7 fields. Optional fields default to \texttt{none}
  or empty arrays for backward compatibility. -/)
  (uses := ["dr:soundness-result", "dr:coverage-result"])]
  Dress.Graph.CheckResults

attribute [blueprint "dr:transitive-reduction"
  (title := "Transitive Reduction")
  (statement := /-- Computes the transitive reduction of the graph using DFS.
  For each edge $(u, v)$, checks if $v$ is reachable from $u$ via an
  alternative path of length $\geq 2$. If so, the edge is transitive and removed.
  Skipped for graphs with $> 500$ nodes. Worst case $O(E \cdot (V + E))$
  but much faster on sparse DAGs. -/)
  (proof := /-- For each edge, runs DFS from source (skipping the direct edge to target).
  If target is reachable, the edge is transitive. Uses
  \texttt{AdjIndex} for $O(1)$ neighbor lookups and \texttt{HashSet}
  for visited tracking. -/)
  (uses := ["dr:graph", "dr:build-adj-index"])]
  Dress.Graph.Graph.transitiveReduction

/-! # Graph Module: Graph Construction (Dress.Graph.Build)

Two-pass graph construction from blueprint nodes, with validation and coverage analysis. -/

attribute [blueprint "dr:builder-state"
  (title := "Builder State")
  (statement := /-- Mutable state for graph construction. Accumulates nodes, edges,
  per-type counters (for auto-numbering), and a label-to-ID mapping
  used for edge resolution in the two-pass algorithm. -/)
  (proof := /-- Structure with 4 fields, all defaulting to empty.
  Derives \texttt{Inhabited}. -/)
  (uses := ["dr:graph-node", "dr:graph-edge"])]
  Dress.Graph.BuilderState

attribute [blueprint "dr:builder-m"
  (title := "Builder Monad")
  (statement := /-- The builder monad: \texttt{StateM BuilderState}.
  Threads mutable state through the two-pass graph construction algorithm. -/)
  (proof := /-- Type abbreviation for \texttt{StateM BuilderState}. -/)
  (uses := ["dr:builder-state"])]
  Dress.Graph.BuilderM

attribute [blueprint "dr:builder-next-number"
  (title := "Next Counter")
  (statement := /-- Gets and increments the counter for a given environment type string
  (e.g., ``theorem'', ``def''). Used for auto-numbering nodes like
  ``Theorem 1'', ``Definition 2''. -/)
  (proof := /-- Reads current counter from \texttt{HashMap}, increments, stores back. -/)
  (uses := ["dr:builder-m"])]
  Dress.Graph.Builder.nextNumber

attribute [blueprint "dr:builder-add-node"
  (title := "Add Node to Builder")
  (statement := /-- Appends a node to the builder state's node array.
  Called during Pass 1 of graph construction. -/)
  (proof := /-- \texttt{modify} pushing to \texttt{s.nodes}. -/)
  (uses := ["dr:builder-m"])]
  Dress.Graph.Builder.addNode

attribute [blueprint "dr:builder-add-edge"
  (title := "Add Edge to Builder")
  (statement := /-- Adds an edge to the builder state, but only if both endpoint labels
  are registered. Silently skips edges with unknown endpoints, which handles
  references to declarations outside the blueprint scope. -/)
  (proof := /-- Checks \texttt{labelToId.contains} for both endpoints before pushing. -/)
  (uses := ["dr:builder-m"])]
  Dress.Graph.Builder.addEdge

attribute [blueprint "dr:builder-register-label"
  (title := "Register Label")
  (statement := /-- Registers a label-to-node-ID mapping in the builder state.
  Called during Pass 1 so that Pass 2 can resolve edge endpoints. -/)
  (proof := /-- Inserts into \texttt{labelToId} HashMap. -/)
  (uses := ["dr:builder-m"])]
  Dress.Graph.Builder.registerLabel

attribute [blueprint "dr:builder-get-status"
  (title := "Determine Node Status")
  (statement := /-- Computes the final visualization status for a node based on:
  \begin{enumerate}
  \item Manual \texttt{mathlibReady} flag (highest priority)
  \item Manual \texttt{wip} flag
  \item Explicit \texttt{notReady} flag (overrides auto-derive)
  \item If has Lean code without \texttt{sorryAx} $\to$ \texttt{proven}
  \item If has Lean code with \texttt{sorryAx} $\to$ \texttt{sorry}
  \item Default: \texttt{notReady}
  \end{enumerate}
  Note: \texttt{fullyProven} is computed post-graph by \texttt{computeFullyProven}. -/)
  (proof := /-- Pattern match on \texttt{node.status} for manual flags, then
  conditional logic for auto-derivation from \texttt{hasLean} and \texttt{hasSorry}. -/)]
  Dress.Graph.Builder.getStatus

attribute [blueprint "dr:builder-get-shape"
  (title := "Determine Node Shape")
  (statement := /-- Maps environment type strings to visual \texttt{NodeShape}:
  \texttt{box} for definitions/structures/classes/instances,
  \texttt{ellipse} for theorems/lemmas/propositions,
  \texttt{diamond} for axioms. Defaults to ellipse for unknown types. -/)
  (proof := /-- Pattern match on lowercase environment type string. -/)
  (uses := ["dr:node-shape"])]
  Dress.Graph.Builder.getShape

attribute [blueprint "dr:builder-register-node"
  (title := "Register Node (Pass 1)")
  (statement := /-- Pass 1 of the two-pass algorithm: registers a node's label and creates
  the graph node without adding edges. Computes status, shape, display label
  (preferring title over qualified name), and propagates all 8 metadata fields
  from the \texttt{Architect.Node}. -/)
  (proof := /-- Extracts fields from \texttt{Dress.NodeWithPos}, calls
  \texttt{registerLabel} and \texttt{addNode}. -/)
  (uses := ["dr:builder-register-label", "dr:builder-add-node",
            "dr:builder-get-status", "dr:builder-get-shape"])]
  Dress.Graph.Builder.registerNode

attribute [blueprint "dr:builder-add-node-edges"
  (title := "Add Node Edges (Pass 2)")
  (statement := /-- Pass 2 of the two-pass algorithm: adds edges for a node.
  Statement uses become dashed edges; proof uses become solid edges.
  Both endpoint labels must be registered (enforced by \texttt{addEdge}). -/)
  (proof := /-- Iterates over \texttt{statementUses} and \texttt{proofUses} arrays,
  calling \texttt{addEdge} with appropriate style. -/)
  (uses := ["dr:builder-add-edge"])]
  Dress.Graph.Builder.addNodeEdges

attribute [blueprint "dr:node-build-data"
  (title := "Node Build Data")
  (statement := /-- Bundle of data needed to build a single graph node:
  the \texttt{Dress.NodeWithPos}, \texttt{hasSorry} flag,
  and arrays of statement and proof dependency labels. -/)
  (proof := /-- Simple 4-field structure. -/)]
  Dress.Graph.Builder.NodeBuildData

attribute [blueprint "dr:compute-fully-proven"
  (title := "Compute Fully Proven")
  (statement := /-- Upgrades eligible nodes from \texttt{proven} to \texttt{fullyProven}.
  A node qualifies if it is \texttt{proven} and ALL its transitive dependencies
  (ancestors) are \texttt{proven} or \texttt{fullyProven}. Uses memoization
  with precomputed \texttt{AdjIndex} for $O(V + E)$ complexity. -/)
  (proof := /-- Iterative worklist algorithm with DFS. Builds node lookup and
  memo \texttt{HashMap}. For each node, pushes unresolved dependencies
  onto the stack. Uses cycle detection via \texttt{visiting} set.
  Final pass maps nodes, upgrading status where memo indicates true. -/)
  (uses := ["dr:graph", "dr:build-adj-index"])]
  Dress.Graph.Builder.computeFullyProven

attribute [blueprint "dr:build-graph"
  (title := "Build Graph")
  (statement := /-- Constructs a dependency graph from an array of \texttt{NodeBuildData}
  using the two-pass algorithm:
  \begin{enumerate}
  \item Pass 1: Register all labels and create nodes
  \item Pass 2: Add all edges (back-references now resolve)
  \end{enumerate}
  Filters edges to valid endpoints, deduplicates by (from, to) pair,
  and post-processes with \texttt{computeFullyProven}. -/)
  (proof := /-- Runs \texttt{registerNode} for all data (Pass 1), then
  \texttt{addNodeEdges} for all data (Pass 2) using \texttt{StateM}.
  Edge validation via \texttt{HashSet}, deduplication via seen-set fold. -/)
  (uses := ["dr:builder-register-node", "dr:builder-add-node-edges",
            "dr:compute-fully-proven"])]
  Dress.Graph.Builder.buildGraph

attribute [blueprint "dr:find-components"
  (title := "Find Connected Components")
  (statement := /-- Finds connected components using BFS on the undirected view of the graph.
  Accepts a precomputed \texttt{AdjIndex} for $O(1)$ edge lookups.
  Returns an array of components, each being an array of node IDs.
  Total complexity is $O(V + E)$. -/)
  (proof := /-- Standard BFS with visited set. For each unvisited node, explores
  both outgoing and incoming edges (undirected connectivity). Collects
  component members into arrays. -/)
  (uses := ["dr:graph", "dr:adj-index"])]
  Dress.Graph.findComponents

attribute [blueprint "dr:detect-cycles"
  (title := "Detect Cycles")
  (statement := /-- Detects cycles in the directed graph using DFS with three-color marking.
  White (unvisited), gray (in current path), black (finished). When a gray node
  is reached from another gray node, a cycle is found and reconstructed.
  Returns array of cycles, each as an array of node IDs. $O(V + E)$. -/)
  (proof := /-- Iterative DFS with explicit stack of (nodeId, neighborIndex) pairs.
  Builds adjacency list, then processes each unvisited node. Back-edges
  (gray $\to$ gray) trigger cycle reconstruction by walking parent pointers. -/)
  (uses := ["dr:graph"])]
  Dress.Graph.detectCycles

attribute [blueprint "dr:compute-check-results"
  (title := "Compute Check Results")
  (statement := /-- Aggregates all graph validation results into a single
  \texttt{CheckResults} structure. Runs connectivity check (\texttt{findComponents}),
  cycle detection (\texttt{detectCycles}), and kernel verification
  (checks if all key declarations are \texttt{fullyProven}). -/)
  (proof := /-- Builds adjacency index, then calls component finder, cycle detector,
  and filters key declarations for kernel verification check. -/)
  (uses := ["dr:find-components", "dr:detect-cycles",
            "dr:check-results", "dr:build-adj-index"])]
  Dress.Graph.computeCheckResults

attribute [blueprint "dr:from-nodes"
  (title := "Graph from Nodes (Explicit Uses)")
  (statement := /-- Builds a dependency graph from \texttt{Dress.NodeWithPos} array
  using only explicit \texttt{uses} labels (no Lean-inferred dependencies).
  Statement uses come from \texttt{usesLabels}; proof uses from
  \texttt{proof.usesLabels}. -/)
  (proof := /-- Maps each node to \texttt{NodeBuildData} extracting explicit label arrays,
  then delegates to \texttt{buildGraph}. -/)
  (uses := ["dr:build-graph"])]
  Dress.Graph.fromNodes

attribute [blueprint "dr:from-environment"
  (title := "Graph from Environment")
  (keyDeclaration := true)
  (statement := /-- Builds a dependency graph from the Lean environment's blueprint extension.
  The primary entry point for graph construction in the build pipeline.
  For each blueprint-annotated declaration:
  \begin{itemize}
  \item Converts \texttt{Architect.Node} to \texttt{Dress.NodeWithPos}
  \item Overrides \texttt{envType} to ``axiom'' if the underlying constant is an axiom
  \item Infers statement and proof uses from actual Lean code dependencies
    via \texttt{Architect.Node.inferUses}
  \end{itemize}
  Runs in \texttt{CoreM} for environment access. -/)
  (proof := /-- Iterates blueprint extension state. For each entry, constructs
  \texttt{NodeBuildData} with inferred uses, then delegates to \texttt{buildGraph}. -/)
  (uses := ["dr:build-graph"])]
  Dress.Graph.fromEnvironment

attribute [blueprint "dr:compute-coverage"
  (title := "Compute Coverage")
  (keyDeclaration := true)
  (statement := /-- Computes blueprint coverage: what percentage of project-local declarations
  have \texttt{@[blueprint]} annotations. Enumerates all eligible declarations
  across modules matching specified project prefixes. Filters out:
  \begin{itemize}
  \item Auto-generated declarations (rec, casesOn, etc.)
  \item Structure projections
  \item Constructor and recursor infos
  \end{itemize}
  Returns \texttt{CoverageResult} with total, covered, percentage, and uncovered list. -/)
  (proof := /-- Iterates all loaded modules, filtering to project-local ones.
  For each module's constants, checks eligibility and blueprint annotation
  presence. Computes percentage as \texttt{Float} division. -/)
  (uses := ["dr:coverage-result", "dr:uncovered-decl"])]
  Dress.Graph.computeCoverage

/-! # Graph Module: JSON Serialization (Dress.Graph.Json)

ToJson and FromJson instances for graph types, producing D3.js-compatible output. -/

attribute [blueprint "dr:json-node"
  (title := "Node JSON Instance")
  (statement := /-- \texttt{ToJson} instance for \texttt{Node}. Serializes all 16 fields
  including the 8 metadata fields. Lean declaration names are converted
  to strings. Used in manifest JSON generation. -/)
  (proof := /-- Manual \texttt{Json.mkObj} with field-by-field serialization. -/)
  (uses := ["dr:graph-node"])]
  Dress.Graph.instToJsonNode

attribute [blueprint "dr:json-edge"
  (title := "Edge JSON Instance")
  (statement := /-- \texttt{ToJson} instance for \texttt{Edge}. Serializes \texttt{from},
  \texttt{to}, and \texttt{style} fields. Note: JSON field is ``from''
  (not ``from\_''). -/)
  (proof := /-- Manual \texttt{Json.mkObj} with 3 fields. -/)
  (uses := ["dr:graph-edge"])]
  Dress.Graph.instToJsonEdge

attribute [blueprint "dr:json-graph"
  (title := "Graph JSON Instance")
  (statement := /-- \texttt{ToJson} instance for \texttt{Graph} (without layout positions).
  Produces \texttt{\{"nodes": [...], "edges": [...]\}}. -/)
  (proof := /-- Maps \texttt{toJson} over node and edge arrays. -/)
  (uses := ["dr:graph", "dr:json-node", "dr:json-edge"])]
  Dress.Graph.instToJsonGraph

attribute [blueprint "dr:json-check-results-to"
  (title := "CheckResults ToJson")
  (statement := /-- \texttt{ToJson} instance for \texttt{CheckResults}. Serializes all
  validation fields including optional \texttt{kernelVerified},
  \texttt{soundnessResults}, and \texttt{coverage}. -/)
  (proof := /-- Manual \texttt{Json.mkObj} with 7 fields. -/)
  (uses := ["dr:check-results"])]
  Dress.Graph.instToJsonCheckResults

attribute [blueprint "dr:json-check-results-from"
  (title := "CheckResults FromJson")
  (statement := /-- \texttt{FromJson} instance for \texttt{CheckResults}. Backward compatible
  with legacy manifests: optional fields absent or null default to
  \texttt{none} or empty arrays. -/)
  (proof := /-- Parses required fields with \texttt{getObjValAs?}, then uses
  match/error fallback for optional fields. -/)
  (uses := ["dr:check-results"])]
  Dress.Graph.instFromJsonCheckResults

attribute [blueprint "dr:json-layout-node"
  (title := "LayoutNode JSON Instance")
  (statement := /-- \texttt{ToJson} instance for \texttt{LayoutNode}. Serializes all
  original node fields plus position (\texttt{x}, \texttt{y}) and
  dimensions (\texttt{width}, \texttt{height}). -/)
  (proof := /-- Manual \texttt{Json.mkObj} with 20+ fields combining
  node metadata and layout coordinates. -/)
  (uses := ["dr:layout-node"])]
  Dress.Graph.Layout.instToJsonLayoutNode

attribute [blueprint "dr:json-layout-edge"
  (title := "LayoutEdge JSON Instance")
  (statement := /-- \texttt{ToJson} instance for \texttt{LayoutEdge}. Serializes
  \texttt{from}, \texttt{to}, control \texttt{points} (as nested arrays),
  and \texttt{style}. Points are Float pairs for Bezier curve rendering. -/)
  (proof := /-- Manual \texttt{Json.mkObj} with point-to-JSON helper. -/)
  (uses := ["dr:layout-edge"])]
  Dress.Graph.Layout.instToJsonLayoutEdge

attribute [blueprint "dr:json-layout-graph"
  (title := "LayoutGraph JSON Instance")
  (statement := /-- \texttt{ToJson} instance for \texttt{LayoutGraph}. Produces the full
  D3.js-compatible manifest with nodes, edges, adjacency lists
  (predecessor/successor maps), and bounding box dimensions. The adjacency
  object enables $O(1)$ neighbor lookups in the JavaScript frontend. -/)
  (proof := /-- Builds predecessor/successor \texttt{HashMap}s from edges,
  then assembles JSON with nodes, edges, adjacency object, width, and height. -/)
  (uses := ["dr:layout-graph"])]
  Dress.Graph.Layout.instToJsonLayoutGraph

attribute [blueprint "dr:write-json-file"
  (title := "Write JSON File")
  (statement := /-- Writes a \texttt{LayoutGraph} to a JSON file with pretty-printing.
  Used for human-readable manifest output during development. -/)
  (proof := /-- Calls \texttt{toJson}, then \texttt{Json.pretty}, then \texttt{IO.FS.writeFile}. -/)
  (uses := ["dr:json-layout-graph"])]
  Dress.Graph.writeJsonFile

attribute [blueprint "dr:write-json-file-compact"
  (title := "Write Compact JSON File")
  (statement := /-- Writes a \texttt{LayoutGraph} to a compact JSON file (no whitespace).
  Used for production manifest output to minimize file size. -/)
  (proof := /-- Calls \texttt{toJson}, then \texttt{Json.compress}, then \texttt{IO.FS.writeFile}. -/)
  (uses := ["dr:json-layout-graph"])]
  Dress.Graph.writeJsonFileCompact

/-! # Graph Module: SVG Generation (Dress.Graph.Svg)

SVG rendering of laid-out dependency graphs. Contains the canonical status color definitions. -/

attribute [blueprint "dr:svg-config"
  (title := "SVG Configuration")
  (statement := /-- Configuration structure for SVG rendering. Defines the canonical
  status colors for the 7-status color model:
  \begin{itemize}
  \item \texttt{notReady}: Vivid orange (\texttt{\#E8820C})
  \item \texttt{wip}: Deep teal/cyan (\texttt{\#0097A7})
  \item \texttt{sorry}: Vivid red (\texttt{\#C62828})
  \item \texttt{proven}: Medium green (\texttt{\#66BB6A})
  \item \texttt{fullyProven}: Deep forest green (\texttt{\#1B5E20})
  \item \texttt{axiom}: Vivid purple (\texttt{\#7E57C2})
  \item \texttt{mathlibReady}: Vivid blue (\texttt{\#42A5F5})
  \end{itemize}
  Also configures stroke, edge, font, and border radius properties.
  This is the \textbf{source of truth} for status colors---CSS must match. -/)
  (proof := /-- Structure with 14 fields, all with sensible defaults.
  Derives \texttt{Repr} and \texttt{Inhabited}. -/)]
  Dress.Graph.Svg.SvgConfig

attribute [blueprint "dr:svg-get-status-color"
  (title := "Status Color Lookup")
  (statement := /-- Maps a \texttt{NodeStatus} to its hex color string from the
  \texttt{SvgConfig}. This function is the primary color dispatch
  used during SVG node rendering. -/)
  (proof := /-- Pattern match on all 7 \texttt{NodeStatus} variants,
  returning the corresponding config color field. -/)
  (uses := ["dr:svg-config"])]
  Dress.Graph.Svg.getStatusColor

attribute [blueprint "dr:svg-get-text-color"
  (title := "Text Color Lookup")
  (statement := /-- Determines text color based on node status. Returns white
  (\texttt{\#ffffff}) for dark backgrounds (\texttt{sorry}, \texttt{fullyProven},
  \texttt{wip}) and the default text color for light backgrounds. -/)
  (proof := /-- Pattern match on status variants with conditional white override. -/)
  (uses := ["dr:svg-config"])]
  Dress.Graph.Svg.getTextColor

attribute [blueprint "dr:svg-escape-xml"
  (title := "XML Escape")
  (statement := /-- Escapes text for safe inclusion in SVG/XML. Replaces the 5
  XML special characters: \texttt{\&}, \texttt{<}, \texttt{>},
  \texttt{"}, and \texttt{'}. -/)
  (proof := /-- Chain of \texttt{String.replace} calls. -/)]
  Dress.Graph.Svg.escapeXml

attribute [blueprint "dr:svg-status-css-class"
  (title := "Status CSS Class")
  (statement := /-- Maps a \texttt{NodeStatus} to a CSS class name for dark mode targeting.
  Classes follow the pattern \texttt{status-not-ready}, \texttt{status-proven}, etc.
  These classes are used in SVG \texttt{<g>} elements for theme-aware styling. -/)
  (proof := /-- Pattern match on all 7 variants returning string constants. -/)]
  Dress.Graph.Svg.statusCssClass

attribute [blueprint "dr:svg-render-node"
  (title := "Render SVG Node")
  (statement := /-- Generates SVG markup for a single positioned node. Renders the
  appropriate shape (ellipse, rect, or diamond polygon) with status-based
  fill color, wraps in a hyperlink \texttt{<a>} element, and adds a text
  label. Manually-tagged nodes get a dotted stroke. Axiom nodes override
  status color with purple. -/)
  (proof := /-- Pattern match on \texttt{NodeShape} for shape element generation.
  Computes center coordinates, text position, and CSS classes.
  Wraps everything in \texttt{<g class="node">} with \texttt{<title>} for
  click handler compatibility. -/)
  (uses := ["dr:svg-config", "dr:svg-get-status-color",
            "dr:svg-get-text-color", "dr:svg-escape-xml",
            "dr:svg-status-css-class", "dr:layout-node"])]
  Dress.Graph.Svg.renderNode

attribute [blueprint "dr:svg-reverse-bezier"
  (title := "Reverse Bezier Points")
  (statement := /-- Reverses cubic Bezier control points for back-edge rendering.
  For a cubic Bezier with format $[\text{start}, \text{cp1}, \text{cp2},
  \text{end}_1, \ldots]$, reversing requires swapping control point order
  within each segment and reversing segment order. The result makes the
  arrowhead point in the original dependency direction. -/)
  (proof := /-- Reverses the full array, then swaps adjacent control point pairs
  at indices $(1,2), (4,5), (7,8), \ldots$ using a stride-3 loop. -/)]
  Dress.Graph.Svg.reverseBezierPoints

attribute [blueprint "dr:svg-render-edge"
  (title := "Render SVG Edge")
  (statement := /-- Generates SVG path markup for a single positioned edge.
  Handles cubic Bezier curves (groups of 3 control points after start),
  dashed stroke for statement dependencies, and arrowhead markers.
  Reversed edges (back-edges) have their points reversed so the arrow
  points toward the original target. -/)
  (proof := /-- Builds SVG path string from points array. First point becomes
  \texttt{M} command; subsequent groups of 3 become \texttt{C} commands.
  Falls back to \texttt{L} line segments for remaining points. Adds
  \texttt{stroke-dasharray} for dashed edges. -/)
  (uses := ["dr:svg-config", "dr:svg-escape-xml",
            "dr:svg-reverse-bezier", "dr:layout-edge"])]
  Dress.Graph.Svg.renderEdge

attribute [blueprint "dr:svg-render-defs"
  (title := "Render SVG Defs")
  (statement := /-- Generates the SVG \texttt{<defs>} block containing the arrowhead
  marker definition. The marker is a small triangle polygon referenced
  by edges via \texttt{marker-end="url(\#arrowhead)"}. -/)
  (proof := /-- String interpolation producing SVG marker element. -/)
  (uses := ["dr:svg-config"])]
  Dress.Graph.Svg.renderDefs

attribute [blueprint "dr:svg-render-legend"
  (title := "Render SVG Legend")
  (statement := /-- Generates an SVG legend group showing all 7 status colors
  and 3 shape types (ellipse, box, diamond).
  Positioned at top-left with a white background box. Currently
  disabled in favor of static HTML legend outside the SVG viewport. -/)
  (proof := /-- Builds SVG \texttt{<g>} with background rect, color swatches,
  shape examples, and text labels using loops over item arrays. -/)
  (uses := ["dr:svg-config"])]
  Dress.Graph.Svg.renderLegend

attribute [blueprint "dr:svg-render"
  (title := "Render SVG")
  (keyDeclaration := true)
  (statement := /-- Generates the complete SVG document from a \texttt{LayoutGraph}.
  The SVG \texttt{viewBox} is set from the layout's bounding box
  (\texttt{minX}, \texttt{minY}, \texttt{width}, \texttt{height}).
  Renders in order: background rect, defs (arrowhead marker),
  edges group (rendered first so nodes appear on top), then nodes group. -/)
  (proof := /-- Iterative string building in \texttt{Id.run do}. Opens SVG element
  with dimensions, appends background, defs, then loops over edges and nodes
  calling their respective render functions. -/)
  (uses := ["dr:svg-config", "dr:svg-render-node", "dr:svg-render-edge",
            "dr:svg-render-defs", "dr:layout-graph"])]
  Dress.Graph.Svg.render

attribute [blueprint "dr:svg-render-to-file"
  (title := "Render SVG to File")
  (statement := /-- Generates SVG and writes it to a file path.
  Convenience wrapper combining \texttt{render} with \texttt{IO.FS.writeFile}. -/)
  (proof := /-- Calls \texttt{render} then \texttt{IO.FS.writeFile}. -/)
  (uses := ["dr:svg-render"])]
  Dress.Graph.Svg.renderToFile

/-! # Graph Module: Subgraph Extraction (Dress.Graph.Subgraph)

BFS-based subgraph extraction for per-node dependency views. -/

attribute [blueprint "dr:subgraph-direction"
  (title := "Subgraph Direction")
  (statement := /-- Direction for BFS-based subgraph extraction:
  \texttt{ancestors} (follow edges backward---what this node depends on),
  \texttt{descendants} (follow edges forward---what depends on this node),
  or \texttt{both}. -/)
  (proof := /-- Three-variant inductive with derived instances. -/)]
  Dress.Graph.SubgraphDirection

attribute [blueprint "dr:subgraph-direction-from-string"
  (title := "Parse Direction String")
  (statement := /-- Parses a direction string (case-insensitive).
  Returns \texttt{both} for unrecognized input. -/)
  (proof := /-- Pattern match on lowercase string. -/)
  (uses := ["dr:subgraph-direction"])]
  Dress.Graph.SubgraphDirection.fromString

attribute [blueprint "dr:subgraph-direction-to-string"
  (title := "Direction to String")
  (statement := /-- Converts direction to a filesystem-safe string for subgraph filenames. -/)
  (proof := /-- Pattern match returning string constants. -/)
  (uses := ["dr:subgraph-direction"])]
  Dress.Graph.SubgraphDirection.toString

attribute [blueprint "dr:subgraph-direction-all"
  (title := "All Directions")
  (statement := /-- Array of all three \texttt{SubgraphDirection} values,
  used for iterating during subgraph pre-rendering. -/)
  (proof := /-- Literal array of all 3 variants. -/)
  (uses := ["dr:subgraph-direction"])]
  Dress.Graph.SubgraphDirection.all

attribute [blueprint "dr:subgraph-depths"
  (title := "Subgraph Depths")
  (statement := /-- Array of depth values \texttt{[1, 2, 3, 4, 5]} to pre-render
  for each node in each direction. -/)
  (proof := /-- Literal array constant. -/)]
  Dress.Graph.subgraphDepths

attribute [blueprint "dr:max-subgraph-depth"
  (title := "Maximum Subgraph Depth")
  (statement := /-- Maximum depth allowed for subgraph pre-rendering: 5 hops.
  Limits BFS exploration to keep subgraphs manageable. -/)
  (proof := /-- Literal \texttt{Nat} constant. -/)]
  Dress.Graph.maxSubgraphDepth

attribute [blueprint "dr:extract-subgraph"
  (title := "Extract Subgraph")
  (keyDeclaration := true)
  (statement := /-- Extracts a subgraph centered on a given node ID, following edges
  in the specified direction up to a maximum depth. Returns a new
  \texttt{Graph} containing only reachable nodes and edges between them.
  BFS explores layer by layer. Accepts a precomputed \texttt{AdjIndex}
  for $O(1)$ edge lookups. -/)
  (proof := /-- BFS with \texttt{HashSet} visited tracking. For each depth level,
  expands frontier by following edges in the specified direction.
  After BFS completes, filters graph nodes and edges to the visited set. -/)
  (uses := ["dr:graph", "dr:adj-index", "dr:subgraph-direction"])]
  Dress.Graph.extractSubgraph

attribute [blueprint "dr:extract-subgraphs-incremental"
  (title := "Incremental Subgraph Extraction")
  (statement := /-- Extracts subgraphs at all depth levels from a single BFS traversal.
  Returns array of (depth, subgraph) pairs. Much more efficient than calling
  \texttt{extractSubgraph} separately for each depth because BFS is performed
  once and subgraphs are materialized from the visited-with-depth map. -/)
  (proof := /-- Single BFS recording depth per node in a \texttt{HashMap}.
  Materializes subgraphs by filtering the depth map at each requested level. -/)
  (uses := ["dr:graph", "dr:adj-index", "dr:subgraph-direction",
            "dr:subgraph-depths"])]
  Dress.Graph.extractSubgraphsIncremental

attribute [blueprint "dr:compute-max-depth"
  (title := "Compute Maximum Depth")
  (statement := /-- Computes the maximum meaningful depth from a node in a given direction.
  For \texttt{ancestors}: longest path to a root. For \texttt{descendants}:
  longest path to a leaf. For \texttt{both}: max of both.
  Capped at \texttt{maxSubgraphDepth}. -/)
  (proof := /-- BFS layer-by-layer up to \texttt{maxSubgraphDepth}, returning
  the deepest layer that had nodes. For \texttt{both}, takes max of
  two single-direction computations. -/)
  (uses := ["dr:adj-index", "dr:subgraph-direction", "dr:max-subgraph-depth"])]
  Dress.Graph.computeMaxDepth

attribute [blueprint "dr:node-depth-info"
  (title := "Node Depth Info")
  (statement := /-- Per-node depth metadata for all three directions (ancestors,
  descendants, both). Used to skip unnecessary subgraph extractions
  when the actual depth is less than the requested depth. -/)
  (proof := /-- Structure with 3 \texttt{Nat} fields. -/)
  (uses := ["dr:subgraph-direction"])]
  Dress.Graph.NodeDepthInfo

attribute [blueprint "dr:compute-all-max-depths"
  (title := "Compute All Max Depths")
  (statement := /-- Computes max depths for all nodes using topological sort in $O(V + E)$
  instead of $O(V \cdot (V + E))$ per-node BFS. Uses Kahn's algorithm for
  topological ordering, then:
  \begin{itemize}
  \item Forward pass (reverse topo order): compute descendant depth
  \item Backward pass (topo order): compute ancestor depth
  \end{itemize}
  Results are capped at \texttt{maxSubgraphDepth} and keyed by sanitized node ID. -/)
  (proof := /-- Kahn's topological sort, then two linear passes computing max
  child/parent depths. Assembles \texttt{NodeDepthInfo} for each node. -/)
  (uses := ["dr:graph", "dr:adj-index", "dr:node-depth-info",
            "dr:max-subgraph-depth"])]
  Dress.Graph.computeAllMaxDepths

attribute [blueprint "dr:depth-metadata-to-json"
  (title := "Depth Metadata to JSON")
  (statement := /-- Serializes the per-node depth metadata \texttt{HashMap} to a JSON string.
  Each entry maps a sanitized node ID to an object with
  \texttt{ancestors}, \texttt{descendants}, and \texttt{both} fields. -/)
  (proof := /-- Iterates HashMap entries, builds JSON object strings manually,
  joins with commas. -/)
  (uses := ["dr:node-depth-info"])]
  Dress.Graph.depthMetadataToJson

/-! # Graph Module: Layout Algorithm (Dress.Graph.Layout)

Sugiyama layered layout algorithm for dependency graphs.
This is the largest module (~1500 lines) with 6 algorithmic phases. -/

/-! ## Layout Types -/

attribute [blueprint "dr:layout-node"
  (title := "Layout Node")
  (statement := /-- A positioned node: wraps the original \texttt{Node} with
  layout coordinates (\texttt{x}, \texttt{y}) and computed dimensions
  (\texttt{width}, \texttt{height}). Width is dynamic based on label length. -/)
  (proof := /-- Structure with 5 fields. Derives \texttt{Repr} and \texttt{Inhabited}. -/)
  (uses := ["dr:graph-node"])]
  Dress.Graph.Layout.LayoutNode

attribute [blueprint "dr:layout-edge"
  (title := "Layout Edge")
  (statement := /-- A positioned edge: carries source/target IDs, an array of
  Bezier control points (Float pairs), edge style, and the
  \texttt{isReversed} flag for back-edge arrow direction handling. -/)
  (proof := /-- Structure with 5 fields. Default style is solid, default
  \texttt{isReversed} is false. -/)
  (uses := ["dr:edge-style"])]
  Dress.Graph.Layout.LayoutEdge

attribute [blueprint "dr:layout-graph"
  (title := "Layout Graph")
  (keyDeclaration := true)
  (statement := /-- The complete laid-out graph: positioned nodes, positioned edges,
  bounding box dimensions (\texttt{width}, \texttt{height}), and
  viewBox origin (\texttt{minX}, \texttt{minY}). After normalization,
  \texttt{minX} and \texttt{minY} are always 0. This is the final output
  of the layout algorithm, consumed by SVG rendering and JSON serialization. -/)
  (proof := /-- Structure with 6 fields. \texttt{minX} and \texttt{minY} default to 0. -/)
  (uses := ["dr:layout-node", "dr:layout-edge"])]
  Dress.Graph.Layout.LayoutGraph

attribute [blueprint "dr:layout-config"
  (title := "Layout Configuration")
  (statement := /-- Configuration for the Sugiyama layout algorithm. Parameters include
  node dimensions, layer gap (vertical spacing), node gap (horizontal spacing),
  padding, character width for dynamic sizing, and number of barycenter
  iterations for crossing reduction. -/)
  (proof := /-- Structure with 8 fields, all with sensible defaults. -/)]
  Dress.Graph.Layout.LayoutConfig

attribute [blueprint "dr:layout-graph-extract-subgraph"
  (title := "Extract Layout Subgraph")
  (statement := /-- Extracts a subgraph from a laid-out graph by filtering to given node IDs.
  Re-centers coordinates so subgraph content starts at (padding, padding).
  Computes viewBox from nodes only, matching the full graph's approach.
  Edges are filtered and shifted accordingly. -/)
  (proof := /-- Filters nodes and edges by ID set, computes bounding box,
  shifts all coordinates by offset, and recomputes viewBox dimensions. -/)
  (uses := ["dr:layout-graph", "dr:layout-config"])]
  Dress.Graph.Layout.LayoutGraph.extractSubgraph

attribute [blueprint "dr:compute-node-width"
  (title := "Compute Node Width")
  (statement := /-- Computes dynamic node width based on label length.
  Multiplies character count by \texttt{charWidth} config parameter,
  returns the maximum of that and the configured minimum \texttt{nodeWidth}. -/)
  (proof := /-- Simple max of label-width and config minimum. -/)
  (uses := ["dr:layout-config"])]
  Dress.Graph.Layout.computeNodeWidth

/-! ## Edge Routing: Boundary Intersection -/

attribute [blueprint "dr:intersect-line-ellipse"
  (title := "Line-Ellipse Intersection")
  (statement := /-- Computes the intersection point of a line from an ellipse center to
  a target point with the ellipse boundary. Uses parametric form:
  $(cx + t \cdot dx, cy + t \cdot dy)$ on the ellipse yields
  $t = 1 / \sqrt{dx^2/rx^2 + dy^2/ry^2}$. -/)
  (proof := /-- Direct parametric formula. Guards against zero denominator. -/)]
  Dress.Graph.Layout.intersectLineEllipse

attribute [blueprint "dr:intersect-line-rect"
  (title := "Line-Rectangle Intersection")
  (statement := /-- Computes the intersection point of a line from a rectangle center to
  a target point with the rectangle boundary. Finds the smallest positive $t$
  across all 4 edges. -/)
  (proof := /-- Computes $t$ for each edge (right, left, bottom, top),
  takes the minimum positive value. -/)]
  Dress.Graph.Layout.intersectLineRect

attribute [blueprint "dr:clip-to-node-boundary"
  (title := "Clip to Node Boundary")
  (statement := /-- Clips an edge endpoint to a node's boundary based on shape.
  Dispatches to \texttt{intersectLineEllipse} for ellipses/diamonds
  or \texttt{intersectLineRect} for boxes. This ensures edges terminate
  at the node border, not the center. -/)
  (proof := /-- Pattern match on \texttt{NodeShape}, then delegate. -/)
  (uses := ["dr:intersect-line-ellipse", "dr:intersect-line-rect",
            "dr:layout-node"])]
  Dress.Graph.Layout.clipToNodeBoundary

/-! ## Visibility Graph for Edge Routing -/

attribute [blueprint "dr:point"
  (title := "Point (2D)")
  (statement := /-- A 2D point with Float coordinates, used throughout edge routing.
  Has \texttt{BEq} for comparison and a \texttt{dist} function for
  Euclidean distance. -/)
  (proof := /-- Simple structure with \texttt{x} and \texttt{y} fields. -/)]
  Dress.Graph.Layout.Point

attribute [blueprint "dr:obstacle"
  (title := "Obstacle")
  (statement := /-- An obstacle (node bounding box with margin) for the visibility graph.
  Stores top-left position, dimensions, and shape for shape-aware
  collision detection. -/)
  (proof := /-- Structure with 5 fields: position, size, and shape. -/)
  (uses := ["dr:node-shape"])]
  Dress.Graph.Layout.Obstacle

attribute [blueprint "dr:rect-obstacle-vertices"
  (title := "Rectangle Obstacle Vertices")
  (statement := /-- Gets the 4 corner vertices of a rectangular obstacle expanded by a margin.
  These vertices serve as potential waypoints in the visibility graph. -/)
  (proof := /-- Computes expanded corners at $(x - m, y - m)$, $(x + w + m, y - m)$,
  $(x - m, y + h + m)$, $(x + w + m, y + h + m)$. -/)
  (uses := ["dr:point", "dr:obstacle"])]
  Dress.Graph.Layout.rectObstacleVertices

attribute [blueprint "dr:ellipse-obstacle-vertices"
  (title := "Ellipse Obstacle Vertices")
  (statement := /-- Gets 8 octant points of an elliptical obstacle expanded by a margin.
  Uses $\sqrt{2}/2 \approx 0.707$ for diagonal positions.
  More vertices than rectangles for smoother routing around curved shapes. -/)
  (proof := /-- Computes 8 points at 0, 45, 90, 135, 180, 225, 270, 315 degrees
  on the expanded ellipse. -/)
  (uses := ["dr:point", "dr:obstacle"])]
  Dress.Graph.Layout.ellipseObstacleVertices

attribute [blueprint "dr:segment-intersects-rect"
  (title := "Segment-Rectangle Intersection")
  (statement := /-- Checks if a line segment intersects a rectangle interior using the
  Liang-Barsky clipping algorithm. Returns true if any portion of the
  segment lies within the rectangle. -/)
  (proof := /-- Liang-Barsky: computes $p$ (direction toward boundary) and
  $q$ (signed distance) for all 4 edges. Maintains valid $t$ range
  $[t_0, t_1]$; intersection exists iff $t_0 \leq t_1$. -/)
  (uses := ["dr:point", "dr:obstacle"])]
  Dress.Graph.Layout.segmentIntersectsRect

attribute [blueprint "dr:segment-intersects-ellipse"
  (title := "Segment-Ellipse Intersection")
  (statement := /-- Checks if a line segment intersects an ellipse interior using
  parametric approach. Transforms to unit circle, then solves the
  quadratic $At^2 + Bt + C = 0$ for intersection parameters. -/)
  (proof := /-- Normalizes coordinates relative to ellipse center/radii.
  Solves quadratic and checks if any root $t \in [0, 1]$, or if
  the segment is entirely inside ($t_1 < 0$ and $t_2 > 1$). -/)
  (uses := ["dr:point", "dr:obstacle"])]
  Dress.Graph.Layout.segmentIntersectsEllipse

attribute [blueprint "dr:segment-intersects-obstacles"
  (title := "Segment-Obstacle Collision")
  (statement := /-- Checks if a line segment intersects any obstacle in the array,
  excluding specified indices (typically source and target nodes).
  Dispatches to shape-specific intersection tests. -/)
  (proof := /-- Iterates obstacles, skipping excluded indices. Pattern matches
  on shape for rect vs. ellipse test. Short-circuits on first hit. -/)
  (uses := ["dr:segment-intersects-rect", "dr:segment-intersects-ellipse"])]
  Dress.Graph.Layout.segmentIntersectsObstacles

attribute [blueprint "dr:collect-visibility-vertices"
  (title := "Collect Visibility Vertices")
  (statement := /-- Collects all potential waypoints for the visibility graph:
  source point, all obstacle corner/octant vertices, and target point.
  The margin parameter controls how far vertices are expanded from
  obstacle boundaries. -/)
  (proof := /-- Concatenates source, all per-obstacle vertices, and target. -/)
  (uses := ["dr:point", "dr:obstacle",
            "dr:rect-obstacle-vertices", "dr:ellipse-obstacle-vertices"])]
  Dress.Graph.Layout.collectVisibilityVertices

attribute [blueprint "dr:build-visibility-graph"
  (title := "Build Visibility Graph")
  (statement := /-- Constructs the visibility graph: for every pair of vertices,
  checks if they have unobstructed line of sight (no obstacle intersections).
  Returns undirected edges as $(i, j)$ index pairs. $O(V^2)$ pairwise checks. -/)
  (proof := /-- Nested loop over all vertex pairs. For each pair, calls
  \texttt{segmentIntersectsObstacles}. Adds both $(i,j)$ and $(j,i)$
  for undirected edges. -/)
  (uses := ["dr:point", "dr:obstacle", "dr:segment-intersects-obstacles"])]
  Dress.Graph.Layout.buildVisibilityGraph

/-! ## Dijkstra's Shortest Path -/

attribute [blueprint "dr:point-dist"
  (title := "Euclidean Distance")
  (statement := /-- Computes Euclidean distance between two 2D points:
  $\sqrt{(x_2 - x_1)^2 + (y_2 - y_1)^2}$. -/)
  (proof := /-- Direct formula using \texttt{Float.sqrt}. -/)
  (uses := ["dr:point"])]
  Dress.Graph.Layout.Point.dist

attribute [blueprint "dr:dijkstra"
  (title := "Dijkstra's Shortest Path")
  (statement := /-- Finds the shortest path between two vertices in the visibility graph
  using Dijkstra's algorithm. Returns an array of vertex indices from
  source to target. Uses $O(V^2)$ implementation (sufficient for small
  visibility graphs). Early termination when target is reached. -/)
  (proof := /-- Standard Dijkstra with arrays for dist, prev, and visited.
  Finds minimum-distance unvisited vertex each iteration, relaxes
  adjacent edges, then reconstructs path via prev pointers. -/)
  (uses := ["dr:point", "dr:point-dist"])]
  Dress.Graph.Layout.dijkstraShortestPath

attribute [blueprint "dr:get-path-points"
  (title := "Get Path Points")
  (statement := /-- Converts a path (array of vertex indices) to actual \texttt{Point}
  coordinates. Filters out invalid indices. -/)
  (proof := /-- Maps indices through \texttt{vertices[i]?} with \texttt{filterMap}. -/)
  (uses := ["dr:point"])]
  Dress.Graph.Layout.getPathPoints

/-! ## Bezier Curve Fitting -/

attribute [blueprint "dr:catmull-rom-to-bezier"
  (title := "Catmull-Rom to Bezier")
  (statement := /-- Converts 4 Catmull-Rom spline points to cubic Bezier control points.
  Given points $P_0, P_1, P_2, P_3$, the curve goes from $P_1$ to $P_2$.
  Control points are: $CP_1 = P_1 + (P_2 - P_0) \cdot t / 3$ and
  $CP_2 = P_2 - (P_3 - P_1) \cdot t / 3$ where $t$ is the tension parameter. -/)
  (proof := /-- Direct formula computing two control points from tangent vectors. -/)
  (uses := ["dr:point"])]
  Dress.Graph.Layout.catmullRomToBezier

attribute [blueprint "dr:polyline-to-bezier"
  (title := "Polyline to Bezier")
  (statement := /-- Converts a polyline (array of waypoints) to smooth Bezier control points
  using Catmull-Rom interpolation. Output format:
  $[\text{start}, \text{cp1}, \text{cp2}, \text{end}_1, \text{cp3},
  \text{cp4}, \text{end}_2, \ldots]$---suitable for SVG cubic Bezier paths.
  Edge segments use phantom points (reflection) for smooth endpoints. -/)
  (proof := /-- For each segment, computes 4 Catmull-Rom points (with phantom
  reflection at boundaries), converts to Bezier, and appends control
  points and endpoint. -/)
  (uses := ["dr:point", "dr:catmull-rom-to-bezier"])]
  Dress.Graph.Layout.polylineToBezier

attribute [blueprint "dr:bezier-to-svg-path"
  (title := "Bezier to SVG Path")
  (statement := /-- Converts Bezier control points to an SVG path data string.
  Format: \texttt{M x0 y0 C cp1x cp1y cp2x cp2y x1 y1 ...}
  Handles both straight lines (2 points) and multi-segment cubic curves. -/)
  (proof := /-- Builds path string: \texttt{M} for start, then groups of 3
  points become \texttt{C} cubic commands. -/)
  (uses := ["dr:point"])]
  Dress.Graph.Layout.bezierToSvgPath

attribute [blueprint "dr:smooth-corners"
  (title := "Smooth Corners")
  (statement := /-- Applies corner smoothing to a polyline: for each interior waypoint,
  creates a smooth corner by inserting start/control/end point triplets.
  The corner radius controls how far from the vertex the curve starts. -/)
  (proof := /-- For each interior point, computes vectors to previous and next
  points. Inserts start point (along incoming vector), control point
  (the original vertex), and end point (along outgoing vector). -/)
  (uses := ["dr:point"])]
  Dress.Graph.Layout.smoothCorners

attribute [blueprint "dr:point-to-tuple"
  (title := "Point to Tuple")
  (statement := /-- Converts a \texttt{Point} struct to a \texttt{Float $\times$ Float} tuple. -/)
  (proof := /-- Extracts \texttt{(p.x, p.y)}. -/)
  (uses := ["dr:point"])]
  Dress.Graph.Layout.Point.toTuple

attribute [blueprint "dr:point-from-tuple"
  (title := "Point from Tuple")
  (statement := /-- Converts a \texttt{Float $\times$ Float} tuple to a \texttt{Point} struct. -/)
  (proof := /-- Constructs \texttt{Point} from tuple components. -/)
  (uses := ["dr:point"])]
  Dress.Graph.Layout.Point.fromTuple

attribute [blueprint "dr:smooth-corners-to-svg"
  (title := "Smooth Corners to SVG Path")
  (statement := /-- Converts smoothed corners output to SVG path data string.
  Handles mixed line segments (\texttt{L}) and quadratic Bezier
  curves (\texttt{Q}) from the triplet pattern produced by
  \texttt{smoothCorners}. -/)
  (proof := /-- Walks the points array: groups of 3 become
  \texttt{L} + \texttt{Q} commands; remaining single points
  become \texttt{L} commands. -/)
  (uses := ["dr:point"])]
  Dress.Graph.Layout.smoothCornersToSvgPath

/-! ## Acyclic Graph Transformation -/

attribute [blueprint "dr:build-adjacency-list"
  (title := "Build Adjacency List")
  (statement := /-- Builds an adjacency list (outgoing edges only) from an edge array.
  Used internally by the DFS-based cycle detection. -/)
  (proof := /-- Single pass over edges, accumulating into a \texttt{HashMap}. -/)
  (uses := ["dr:graph-edge"])]
  Dress.Graph.Layout.buildAdjacencyList

attribute [blueprint "dr:dfs-state"
  (title := "DFS State")
  (statement := /-- State for DFS-based back-edge detection. Tracks fully processed nodes
  (\texttt{visited}), nodes in the current DFS path (\texttt{recursionStack}),
  and discovered back-edges. -/)
  (proof := /-- Structure with 3 fields, all defaulting to empty. -/)
  (uses := ["dr:graph-edge"])]
  Dress.Graph.Layout.DfsState

attribute [blueprint "dr:dfs-visit"
  (title := "DFS Visit")
  (statement := /-- Recursive DFS visit for back-edge detection. Adds the node to the
  recursion stack, visits all neighbors. If a neighbor is already in the
  recursion stack, that edge is a back-edge (creates a cycle).
  Marked \texttt{partial} due to recursive calls. -/)
  (proof := /-- Recursive function: marks node gray (recursion stack), iterates
  neighbors. Gray-to-gray edges are back-edges. After all neighbors,
  marks node black (visited). -/)
  (uses := ["dr:dfs-state"])]
  Dress.Graph.Layout.dfsVisit

attribute [blueprint "dr:find-back-edges"
  (title := "Find Back Edges")
  (statement := /-- Finds all back-edges in the graph using DFS. Back-edges point from
  a node to an ancestor in the DFS tree, indicating cycles. Handles
  disconnected components by starting DFS from each unvisited node. -/)
  (proof := /-- Builds adjacency list, then runs \texttt{dfsVisit} from
  each unvisited node. Collects back-edges from final DFS state. -/)
  (uses := ["dr:graph", "dr:build-adjacency-list", "dr:dfs-visit"])]
  Dress.Graph.Layout.findBackEdges

attribute [blueprint "dr:reverse-edge"
  (title := "Reverse Edge")
  (statement := /-- Reverses an edge by swapping \texttt{from\_} and \texttt{to},
  and setting \texttt{isReversed := true}. Used during acyclic transformation. -/)
  (proof := /-- Struct update swapping the two ID fields. -/)
  (uses := ["dr:graph-edge"])]
  Dress.Graph.Layout.reverseEdge

attribute [blueprint "dr:make-acyclic"
  (title := "Make Graph Acyclic")
  (statement := /-- Makes the graph acyclic by iteratively finding and reversing back-edges
  one at a time (Graphviz approach). Each iteration finds back-edges,
  reverses the first one, and re-checks. More stable than reversing all
  back-edges at once. Returns the acyclic graph and the list of reversed edges.
  Marked \texttt{partial} due to iterative back-edge detection. -/)
  (proof := /-- Loop with safety bound (\texttt{edges.size + 1}). Each iteration
  calls \texttt{findBackEdges}; if empty, done. Otherwise reverses the
  first back-edge by mapping over the edge array. -/)
  (uses := ["dr:graph", "dr:find-back-edges", "dr:reverse-edge"])]
  Dress.Graph.Layout.makeAcyclic

/-! ## Layout State Machine -/

attribute [blueprint "dr:layout-state"
  (title := "Layout State")
  (statement := /-- Mutable state for the layout algorithm. Tracks layer assignment
  per node, the ordered node arrays per layer, computed positions,
  and dynamic node widths. -/)
  (proof := /-- Structure with 4 \texttt{HashMap}/\texttt{Array} fields,
  all defaulting to empty. -/)]
  Dress.Graph.Layout.LayoutState

attribute [blueprint "dr:layout-monad"
  (title := "Layout Monad")
  (statement := /-- The layout monad: \texttt{StateM LayoutState}.
  Threads layout state through the Sugiyama algorithm phases. -/)
  (proof := /-- Type abbreviation for \texttt{StateM LayoutState}. -/)
  (uses := ["dr:layout-state"])]
  Dress.Graph.Layout.LayoutM

/-! ## Crossing Reduction -/

attribute [blueprint "dr:count-crossings"
  (title := "Count Edge Crossings")
  (statement := /-- Counts edge crossings between two adjacent layers. An edge
  $(u_1, v_1)$ crosses $(u_2, v_2)$ if $u_1 < u_2$ and $v_1 > v_2$
  (or vice versa). This counts inversions between edge endpoint positions. -/)
  (proof := /-- Nested loop over edge pairs. For each pair, checks if the
  relative order of source positions is opposite to the relative order
  of target positions. -/)
  (uses := ["dr:graph"])]
  Dress.Graph.Layout.Algorithm.countCrossings

attribute [blueprint "dr:count-total-crossings"
  (title := "Count Total Crossings")
  (statement := /-- Counts total edge crossings across all adjacent layer pairs.
  Sums \texttt{countCrossings} for each consecutive pair. -/)
  (proof := /-- Loop from layer 0 to $n-2$, summing crossings between
  layer $i$ and $i+1$. -/)
  (uses := ["dr:count-crossings"])]
  Dress.Graph.Layout.Algorithm.countTotalCrossings

attribute [blueprint "dr:swap-array"
  (title := "Swap Array Elements")
  (statement := /-- Swaps two elements in an array by index.
  Bounds-checked: returns original array if either index is out of range. -/)
  (proof := /-- Reads both values, then sets with \texttt{set!}. -/)]
  Dress.Graph.Layout.Algorithm.swapArray

attribute [blueprint "dr:try-swap-adjacent"
  (title := "Try Adjacent Swap")
  (statement := /-- Attempts to swap two adjacent nodes in a layer, accepting the swap
  only if it reduces the total edge crossings with neighboring layers.
  Returns \texttt{some newLayer} if improved, \texttt{none} otherwise. -/)
  (proof := /-- Computes crossings before swap (with layers above and below),
  performs the swap, recomputes crossings, and compares. -/)
  (uses := ["dr:count-crossings", "dr:swap-array"])]
  Dress.Graph.Layout.Algorithm.trySwapAdjacent

attribute [blueprint "dr:transpose-pass"
  (title := "Transpose Heuristic Pass")
  (statement := /-- Transpose heuristic: repeatedly tries swapping adjacent nodes in
  each layer to reduce crossings. Iterates until no improvement is found
  or a maximum of 10 iterations. This is a local optimization applied
  after each barycenter ordering pass. -/)
  (proof := /-- Outer loop with \texttt{improved} flag. Inner loops over all
  layers and all adjacent pairs. Each \texttt{trySwapAdjacent} success
  resets the improved flag. -/)
  (uses := ["dr:try-swap-adjacent"])]
  Dress.Graph.Layout.Algorithm.transposePass

/-! ## Median Ordering -/

attribute [blueprint "dr:median"
  (title := "Median of Floats")
  (statement := /-- Calculates the median of an array of \texttt{Float} values.
  Sorts the array, then returns the middle element (odd count) or
  average of two middle elements (even count). Returns 0 for empty arrays. -/)
  (proof := /-- Sorts with \texttt{qsort}, then indexes middle elements. -/)]
  Dress.Graph.Layout.Algorithm.median

attribute [blueprint "dr:median-neighbor-position"
  (title := "Median Neighbor Position")
  (statement := /-- Computes the median position of a node's neighbors in a reference layer.
  Uses the \texttt{AdjIndex} for $O(1)$ edge lookups. For DOT-style layout,
  median is preferred over mean for robustness against outliers. -/)
  (proof := /-- Gets edges from \texttt{AdjIndex} (incoming or outgoing based on
  direction), maps neighbor IDs to their positions in the reference layer,
  then computes median. Returns \texttt{none} if no neighbors found. -/)
  (uses := ["dr:adj-index", "dr:median"])]
  Dress.Graph.Layout.Algorithm.medianNeighborPosition

attribute [blueprint "dr:order-layer-by-median"
  (title := "Order Layer by Median")
  (statement := /-- Orders a single layer by median of neighbors in a reference layer.
  Nodes with no neighbors in the reference layer keep their original position.
  The result is a sorted array of node IDs. -/)
  (proof := /-- Computes median for each node in the layer, falls back to
  original index for unconnected nodes, sorts by median value. -/)
  (uses := ["dr:median-neighbor-position"])]
  Dress.Graph.Layout.Algorithm.orderLayerByMedian

attribute [blueprint "dr:assign-layers"
  (title := "Assign Layers (Kahn's Algorithm)")
  (statement := /-- Assigns nodes to layers using topological sort (Kahn's algorithm).
  Guarantees: for every edge $u \to v$, $\text{layer}[v] > \text{layer}[u]$.
  Source nodes (in-degree 0) are placed at layer 0.
  Each node is placed at the maximum layer of its predecessors plus 1,
  producing a longest-path layering. -/)
  (proof := /-- Computes in-degrees, seeds queue with sources at layer 0.
  Processes nodes in topological order, updating target layers to
  $\max(\text{existing}, \text{current} + 1)$. Builds layer arrays
  from the assignment map. -/)
  (uses := ["dr:graph", "dr:adj-index", "dr:layout-monad"])]
  Dress.Graph.Layout.Algorithm.assignLayers

attribute [blueprint "dr:order-layer-by-neighbors"
  (title := "Order Layer by Barycenter")
  (statement := /-- Orders a single layer by barycenter (mean) of neighbor positions in
  a reference layer. Alternative to median ordering, using mean instead
  for potentially different node placement. -/)
  (proof := /-- Computes average position of neighbors in reference layer for
  each node, falls back to original position for isolated nodes,
  sorts by barycenter. -/)
  (uses := ["dr:adj-index"])]
  Dress.Graph.Layout.Algorithm.orderLayerByNeighbors

attribute [blueprint "dr:order-layers"
  (title := "Order Layers")
  (statement := /-- Orders nodes within layers using the DOT algorithm approach:
  \begin{enumerate}
  \item Multiple iterations of forward/backward median ordering
  \item Transpose heuristic after each pass (skip for large graphs)
  \end{enumerate}
  \textbf{Performance optimization}: For graphs with $> 200$ nodes,
  reduces iterations to at most 2 and skips the transpose heuristic
  (which is $O(L \times N \times \text{swaps})$ per call). -/)
  (proof := /-- Alternates forward and backward passes. Forward: orders each
  layer by median of previous layer. Backward: orders by median of
  next layer. Transpose pass applied after each direction (except
  for large graphs). -/)
  (uses := ["dr:graph", "dr:adj-index", "dr:layout-monad",
            "dr:order-layer-by-median", "dr:transpose-pass"])]
  Dress.Graph.Layout.Algorithm.orderLayers

/-! ## Coordinate Assignment -/

attribute [blueprint "dr:get-neighbor-x-positions"
  (title := "Get Neighbor X Positions")
  (statement := /-- Collects X coordinates of all connected neighbors (both incoming
  and outgoing) for a given node. Uses \texttt{AdjIndex} for $O(1)$
  edge lookups. -/)
  (proof := /-- Iterates incoming and outgoing edges, collecting X from
  the positions map. -/)
  (uses := ["dr:adj-index"])]
  Dress.Graph.Layout.Algorithm.getNeighborXPositions

attribute [blueprint "dr:ideal-x-position"
  (title := "Ideal X Position")
  (statement := /-- Calculates the ideal horizontal position for a node as the median
  of all connected neighbor X positions. Returns \texttt{none} if
  the node has no positioned neighbors. -/)
  (proof := /-- Calls \texttt{getNeighborXPositions}, then \texttt{median}. -/)
  (uses := ["dr:get-neighbor-x-positions", "dr:median"])]
  Dress.Graph.Layout.Algorithm.idealXPosition

attribute [blueprint "dr:resolve-overlaps"
  (title := "Resolve Overlaps")
  (statement := /-- Resolves horizontal overlaps within a layer by pushing nodes apart.
  Sorts nodes by X position, then ensures each consecutive pair has
  at least \texttt{nodeWidth + nodeGap} separation. -/)
  (proof := /-- Sorts nodes by X, iterates maintaining \texttt{lastX} and
  \texttt{lastWidth}. If gap is too small, pushes current node right
  to \texttt{lastX + minSeparation}. -/)
  (uses := ["dr:layout-config"])]
  Dress.Graph.Layout.Algorithm.resolveOverlaps

attribute [blueprint "dr:node-degree"
  (title := "Node Degree")
  (statement := /-- Computes the degree (number of connected edges) for a node
  using the \texttt{AdjIndex}. Sum of incoming and outgoing edge counts. -/)
  (proof := /-- Adds sizes of outgoing and incoming edge arrays. -/)
  (uses := ["dr:adj-index"])]
  Dress.Graph.Layout.Algorithm.nodeDegree

attribute [blueprint "dr:refine-positions-pass"
  (title := "Refine Positions Pass")
  (statement := /-- Single pass of position refinement: moves nodes toward their
  neighbor medians. Low-degree nodes ($\leq 2$ edges) get full pull
  ($1.0$), higher-degree nodes get $0.85$ pull factor. Resolves
  overlaps after each layer. -/)
  (proof := /-- Iterates layers (forward or backward). For each node, interpolates
  between current and ideal X. Then calls \texttt{resolveOverlaps}. -/)
  (uses := ["dr:ideal-x-position", "dr:resolve-overlaps", "dr:node-degree"])]
  Dress.Graph.Layout.Algorithm.refinePositionsPass

attribute [blueprint "dr:refine-positions"
  (title := "Refine Positions")
  (statement := /-- Iteratively refines node positions to achieve organic layout.
  Runs at least 12 forward/backward passes, moving nodes toward
  neighbor medians and resolving overlaps. -/)
  (proof := /-- Loop of forward + backward \texttt{refinePositionsPass} calls,
  using at least $\max(\text{param}, 12)$ iterations. -/)
  (uses := ["dr:refine-positions-pass", "dr:layout-monad"])]
  Dress.Graph.Layout.Algorithm.refinePositions

attribute [blueprint "dr:assign-initial-coordinates"
  (title := "Assign Initial Coordinates")
  (statement := /-- Assigns initial grid-based coordinates to nodes.
  Top-to-bottom layout: layer index determines Y position (sources at top),
  node index within layer determines initial X position.
  Layers are horizontally centered relative to the widest layer.
  Node widths are computed dynamically based on label length. -/)
  (proof := /-- Computes per-layer total widths, finds maximum, then places
  each node with horizontal centering offset. Y position increments
  by \texttt{nodeHeight + layerGap} per layer. -/)
  (uses := ["dr:layout-config", "dr:compute-node-width", "dr:layout-monad"])]
  Dress.Graph.Layout.Algorithm.assignInitialCoordinates

attribute [blueprint "dr:assign-coordinates"
  (title := "Assign Coordinates")
  (statement := /-- Full coordinate assignment using the DOT-style algorithm:
  \begin{enumerate}
  \item Initial grid-based placement (centered layers)
  \item Iterative refinement toward neighbor medians (5 initial passes)
  \end{enumerate} -/)
  (proof := /-- Calls \texttt{assignInitialCoordinates} then
  \texttt{refinePositions} with 5 iterations. -/)
  (uses := ["dr:assign-initial-coordinates", "dr:refine-positions"])]
  Dress.Graph.Layout.Algorithm.assignCoordinates

/-! ## Edge Creation -/

attribute [blueprint "dr:create-layout-edges-simple"
  (title := "Create Layout Edges (Simple)")
  (statement := /-- Creates layout edges with simple Bezier control points, without
  obstacle avoidance. For each edge:
  \begin{enumerate}
  \item Computes center-to-center line
  \item Clips to source/target boundary
  \item Generates smooth Bezier with offset-based control points
  \end{enumerate}
  Used for large graphs ($> 100$ nodes) where visibility graph
  routing is too expensive. -/)
  (proof := /-- For each graph edge, looks up positions and shapes, builds
  temporary \texttt{LayoutNode} structs for clipping, computes
  perpendicular offset for gentle arcs, and assembles 4-point
  Bezier curves. -/)
  (uses := ["dr:clip-to-node-boundary", "dr:layout-monad"])]
  Dress.Graph.Layout.Algorithm.createLayoutEdgesSimple

attribute [blueprint "dr:create-layout-edges"
  (title := "Create Layout Edges")
  (statement := /-- Creates layout edges with obstacle-avoiding spline routing.
  Full pipeline: visibility graph $\to$ Dijkstra $\to$ Bezier fitting.
  \textbf{Performance optimization}: For graphs with $> 100$ nodes, falls back
  to \texttt{createLayoutEdgesSimple} (the $O(V^2)$ visibility graph per
  edge is prohibitive for 500+ nodes). For smaller graphs, computes
  obstacle vertices once (cached), then per-edge: builds visibility
  graph, runs Dijkstra, converts path to smooth Bezier, and clips
  endpoints to node boundaries. -/)
  (proof := /-- Gate check: $> 100$ nodes delegates to simple version.
  Otherwise: builds obstacle array from positioned nodes, pre-computes
  obstacle vertices once. Per-edge: concatenates source + cached vertices
  + target, builds visibility graph with exclusions, runs Dijkstra,
  converts polyline to Bezier, clips endpoints. -/)
  (uses := ["dr:create-layout-edges-simple", "dr:build-visibility-graph",
            "dr:dijkstra", "dr:polyline-to-bezier",
            "dr:clip-to-node-boundary", "dr:layout-monad"])]
  Dress.Graph.Layout.Algorithm.createLayoutEdges

/-! ## Main Layout Function -/

attribute [blueprint "dr:layout"
  (title := "Sugiyama Layout")
  (keyDeclaration := true)
  (statement := /-- The main layout function. Performs complete Sugiyama layered layout:
  \begin{enumerate}
  \item Make graph acyclic (reverse back-edges, Graphviz approach)
  \item Build adjacency index on acyclic graph
  \item Assign layers (Kahn's topological sort / longest path)
  \item Order nodes within layers (median heuristic + transpose)
  \item Assign coordinates (grid placement + iterative refinement)
  \item Route edges (visibility graph + Dijkstra, or simple Bezier for large graphs)
  \item Normalize coordinates to $(0, 0)$ origin
  \end{enumerate}
  Final coordinates are shifted so content starts at (\texttt{padding}, \texttt{padding})
  with viewBox origin at $(0, 0)$. This ensures proper SVG display and centering. -/)
  (proof := /-- Sequential pipeline: \texttt{makeAcyclic} $\to$ \texttt{buildAdjIndex}
  $\to$ run \texttt{LayoutM} monad (assignLayers, orderLayers, assignCoordinates)
  $\to$ compute content bounding box $\to$ normalize with offset
  $\to$ create layout nodes $\to$ create layout edges with normalized state
  $\to$ compute final viewBox dimensions. -/)
  (uses := ["dr:graph", "dr:layout-config", "dr:layout-graph",
            "dr:make-acyclic", "dr:build-adj-index",
            "dr:assign-layers", "dr:order-layers",
            "dr:assign-coordinates", "dr:create-layout-edges"])]
  Dress.Graph.Layout.layout

end DressBlueprint.Annotations

#dressNodes
