import Dress

open Dress

namespace DressBlueprint.Annotations

-- ============================================================================
-- === Dress.Output (LaTeX Output) ===
-- ============================================================================

-- ─── Core LaTeX Conversion ───

attribute [blueprint "dr:node-to-latex"
  (keyDeclaration := true)
  (title := "Node to LaTeX")
  (statement := /-- Converts a \texttt{Dress.NodeWithPos} to a complete LaTeX string for embedding
  in the blueprint document.

  This is the central LaTeX generation function for the Dress pipeline. For each node it:
  \begin{enumerate}
    \item Merges all Lean names sharing the same LaTeX label via
          \texttt{getLeanNamesOfLatexLabel}
    \item Emits LaTeX macros: \texttt{\textbackslash label}, \texttt{\textbackslash lean},
          \texttt{\textbackslash notready}, \texttt{\textbackslash discussion},
          \texttt{\textbackslash mathlibok}
    \item Emits source position annotations (\texttt{\textbackslash leanposition},
          \texttt{\textbackslash leanproofposition})
    \item Encodes SubVerso highlighted code as base64 in both JSON
          (\texttt{\textbackslash leansource}) and HTML
          (\texttt{\textbackslash leansourcehtml}) formats
    \item Separately emits signature-only and proof-body highlighting
    \item Encodes above/below narrative content as base64 LaTeX
    \item Infers statement and proof dependencies via \texttt{inferUses}
    \item Delegates to the \texttt{NodePart.toLatex} functions for the
          statement and optional proof environments
  \end{enumerate}

  \uses{dr:node-with-pos, dr:html-render-to-html, dr:split-at-definition-assign} -/)
  (proof := /-- A monadic computation building a mutable \texttt{addLatex} string that
  accumulates LaTeX macros. Each optional field (\texttt{highlightedCode},
  \texttt{highlightedSignature}, \texttt{highlightedProofBody}, \texttt{above},
  \texttt{below}) is conditionally appended. Base64 encoding uses
  \texttt{Dress.stringToBase64}. The final statement and proof LaTeX are produced
  by \texttt{NodePart.toLatex} from the Architect library. -/)
  (uses := ["dr:node-with-pos", "dr:html-render-to-html", "dr:split-at-definition-assign"])]
  Dress.NodeWithPos.toLatex

attribute [blueprint "dr:node-to-latex-artifact"
  (title := "Node to LaTeX Artifact")
  (statement := /-- Wraps a \texttt{Dress.NodeWithPos} as a \texttt{LatexArtifact} by calling
  \texttt{toLatex} and pairing the result with the node's LaTeX label as the artifact ID.

  A \texttt{LatexArtifact} is a \texttt{(id, content)} pair where the id determines
  the output filename in the dressed artifacts directory.

  \uses{dr:node-to-latex} -/)
  (proof := /-- Returns \texttt{\{ id := node.latexLabel, content := $\leftarrow$ node.toLatex \}}. -/)
  (uses := ["dr:node-to-latex"])]
  Dress.NodeWithPos.toLatexArtifact

-- ─── Content-Level LaTeX ───

attribute [blueprint "dr:blueprint-content-to-latex"
  (title := "Blueprint Content to LaTeX")
  (statement := /-- Converts a \texttt{BlueprintContent} element to a LaTeX string.

  For \texttt{.node} values, returns only the LaTeX label string (the full
  artifact is written separately). For \texttt{.modDoc} values, returns the
  raw documentation text for inline inclusion.

  \uses{la:blueprint-content} -/)
  (proof := /-- Pattern-matches on the two constructors of \texttt{BlueprintContent}. -/)
  (uses := ["la:blueprint-content"])]
  Dress.BlueprintContent.toLatex

attribute [blueprint "dr:latex-preamble"
  (title := "LaTeX Preamble")
  (statement := /-- Generates the standard preamble comment for Dress-generated LaTeX files.

  Returns a single-line comment indicating the file was automatically generated
  by Dress, serving as a warning against manual edits. -/)
  (proof := /-- Returns a constant string. -/)]
  Dress.latexPreamble

-- ─── Module-Level LaTeX Output ───

attribute [blueprint "dr:module-to-latex-output"
  (keyDeclaration := true)
  (title := "Module to LaTeX Output")
  (statement := /-- Converts an imported module's blueprint contents to a \texttt{LatexOutput}
  structure containing a header file and per-declaration artifacts.

  Takes an optional highlighting map (from \texttt{subverso-extract-mod}) and
  HTML map (from \texttt{.html.json} files) to enrich nodes with syntax
  highlighting. The header file contains \texttt{\textbackslash input\{\}}
  directives for each artifact.

  This is the primary entry point for generating LaTeX from previously compiled
  modules during the \texttt{blueprint} Lake facet.

  \uses{la:get-blueprint-contents, dr:node-to-latex-artifact, dr:latex-preamble} -/)
  (proof := /-- Calls \texttt{getBlueprintContents} to retrieve module contents, then
  delegates to \texttt{moduleToLatexOutputAux} which deduplicates by LaTeX label,
  converts nodes to artifacts, and assembles the header file. -/)
  (uses := ["la:get-blueprint-contents", "dr:node-to-latex-artifact", "dr:latex-preamble"])]
  Dress.moduleToLatexOutput

attribute [blueprint "dr:main-module-to-latex-output"
  (keyDeclaration := true)
  (title := "Main Module to LaTeX Output")
  (statement := /-- Converts the currently-being-elaborated module to a \texttt{LatexOutput}
  structure, using highlighting captured during elaboration via the Hook mechanism.

  Unlike \texttt{moduleToLatexOutput}, this operates on the current module's
  \texttt{blueprintExt} state rather than imported module data.

  \uses{la:get-main-module-blueprint-contents, dr:node-to-latex-artifact, dr:latex-preamble} -/)
  (proof := /-- Calls \texttt{getMainModuleBlueprintContents} for the current module, then
  delegates to the same \texttt{moduleToLatexOutputAux} helper. -/)
  (uses := ["la:get-main-module-blueprint-contents", "dr:node-to-latex-artifact", "dr:latex-preamble"])]
  Dress.mainModuleToLatexOutput

-- ─── Show Blueprint Command ───

attribute [blueprint "dr:elab-show-blueprint"
  (title := "Show Blueprint LaTeX Command")
  (statement := /-- Elaborator for the \texttt{\#show\_blueprint} command, which displays
  the LaTeX output of blueprint nodes in the Lean infoview.

  Three forms are supported:
  \begin{itemize}
    \item \texttt{\#show\_blueprint} --- shows LaTeX for all nodes in the current module
    \item \texttt{\#show\_blueprint ident} --- shows LaTeX for a specific Lean declaration
    \item \texttt{\#show\_blueprint "label"} --- shows LaTeX for all nodes with the given label
  \end{itemize}

  This is a development/debugging tool that does not write any files.

  \uses{dr:main-module-to-latex-output, dr:node-to-latex-artifact} -/)
  (proof := /-- Pattern-matches on three syntax variants. The ident form resolves via
  \texttt{realizeGlobalConstNoOverloadWithInfo} and looks up the node in
  \texttt{blueprintExt}. The label form uses \texttt{getLeanNamesOfLatexLabel}
  and recursively elaborates each name. -/)
  (uses := ["dr:main-module-to-latex-output", "dr:node-to-latex-artifact"])]
  Dress.elabShowBlueprint

-- ─── JSON Serialization ───

attribute [blueprint "dr:node-with-pos-to-json"
  (title := "NodeWithPos to JSON")
  (statement := /-- Serializes a \texttt{Dress.NodeWithPos} to a JSON object containing all
  node fields: \texttt{name}, \texttt{latexLabel}, \texttt{statement},
  \texttt{proof}, \texttt{notReady}, \texttt{discussion}, \texttt{title},
  \texttt{hasLean}, \texttt{file}, \texttt{location}, \texttt{highlightedCode},
  \texttt{above}, and \texttt{below}.

  Location data is serialized via helper functions that convert
  \texttt{DeclarationRange} and \texttt{DeclarationLocation} to nested JSON.
  Highlighted code is serialized via SubVerso's \texttt{ToJson} instance.

  \uses{dr:node-with-pos} -/)
  (proof := /-- Uses \texttt{json\%} quasi-quotation to build a JSON object literal,
  mapping each field. Optional fields use \texttt{Option.map} with
  private helper functions. -/)
  (uses := ["dr:node-with-pos"])]
  Dress.nodeWithPosToJson

attribute [blueprint "dr:blueprint-content-to-json"
  (title := "Blueprint Content to JSON")
  (statement := /-- Serializes a \texttt{BlueprintContent} element to JSON with a
  discriminated union format.

  Node values produce \texttt{\{"type": "node", "data": ...\}} with the node
  serialized via \texttt{nodeWithPosToJson}. Module doc values produce
  \texttt{\{"type": "moduleDoc", "data": ...\}} with the raw doc string.

  \uses{la:blueprint-content, dr:node-with-pos-to-json} -/)
  (proof := /-- Pattern-matches on the two constructors, using \texttt{json\%}
  quasi-quotation for each variant. -/)
  (uses := ["la:blueprint-content", "dr:node-with-pos-to-json"])]
  Dress.BlueprintContent.toJson

attribute [blueprint "dr:module-to-json"
  (keyDeclaration := true)
  (title := "Module to JSON")
  (statement := /-- Converts an imported module's blueprint contents to a JSON array.

  Each element is a \texttt{BlueprintContent.toJson} value, preserving the
  source-ordered interleaving of nodes and module docstrings.

  Takes an optional highlighting map for enriching nodes with syntax highlighting.

  \uses{la:get-blueprint-contents, dr:blueprint-content-to-json} -/)
  (proof := /-- Calls \texttt{getBlueprintContents}, then maps \texttt{BlueprintContent.toJson}
  over the result and wraps in \texttt{Json.arr}. -/)
  (uses := ["la:get-blueprint-contents", "dr:blueprint-content-to-json"])]
  Dress.moduleToJson

attribute [blueprint "dr:main-module-to-json"
  (keyDeclaration := true)
  (title := "Main Module to JSON")
  (statement := /-- Converts the currently-being-elaborated module's blueprint contents to a JSON
  array, using highlighting captured during elaboration.

  The counterpart of \texttt{moduleToJson} for the current module.

  \uses{la:get-main-module-blueprint-contents, dr:blueprint-content-to-json} -/)
  (proof := /-- Calls \texttt{getMainModuleBlueprintContents}, then maps
  \texttt{BlueprintContent.toJson} and wraps in \texttt{Json.arr}. -/)
  (uses := ["la:get-main-module-blueprint-contents", "dr:blueprint-content-to-json"])]
  Dress.mainModuleToJson

-- ─── Show Blueprint JSON Command ───

attribute [blueprint "dr:elab-show-blueprint-json"
  (title := "Show Blueprint JSON Command")
  (statement := /-- Elaborator for the \texttt{\#show\_blueprint\_json} command, which displays
  the JSON representation of blueprint nodes in the Lean infoview.

  Supports the same three forms as \texttt{\#show\_blueprint}: no argument
  (entire module), an identifier (single declaration), or a string (label lookup).

  \uses{dr:main-module-to-json, dr:node-with-pos-to-json} -/)
  (proof := /-- Pattern-matches on three syntax variants, mirroring \texttt{elabShowBlueprint}
  but calling JSON serialization functions instead of LaTeX generation. -/)
  (uses := ["dr:main-module-to-json", "dr:node-with-pos-to-json"])]
  Dress.elabShowBlueprintJson

-- ─── Path Helpers ───

attribute [blueprint "dr:module-to-rel-path"
  (title := "Module to Relative Path")
  (statement := /-- Computes a relative file path for a module's output file, given a file extension.

  Produces paths of the form \texttt{module/\{Module/Name\}.\{ext\}} which are used
  to construct output paths for both LaTeX and JSON module files within the
  dressed artifacts directory. -/)
  (proof := /-- Delegates to \texttt{modToFilePath} with the \texttt{"module"} prefix. -/)]
  Dress.moduleToRelPath

attribute [blueprint "dr:library-to-rel-path"
  (title := "Library to Relative Path")
  (statement := /-- Computes a relative file path for a library index file, given a file extension.

  Produces paths of the form \texttt{library/\{LibraryName\}.\{ext\}} which are used
  for aggregate index files that reference all modules in a library. -/)
  (proof := /-- Constructs \texttt{System.mkFilePath ["library", library.toString]} and appends
  the extension. -/)]
  Dress.libraryToRelPath

-- ─── File I/O ───

attribute [blueprint "dr:output-latex-results"
  (keyDeclaration := true)
  (title := "Output LaTeX Results")
  (statement := /-- Writes a module's \texttt{LatexOutput} to the filesystem, producing:
  \begin{itemize}
    \item A header \texttt{.tex} file containing \texttt{\textbackslash input\{\}} directives
          for each artifact, with relative paths computed for plasTeX compatibility
    \item Per-declaration artifact \texttt{.tex} files in subdirectories named by label
  \end{itemize}

  The relative path calculation accounts for plasTeX running from \texttt{blueprint/}
  with tex files in \texttt{blueprint/src/}, requiring \texttt{../../} to reach
  the project root and then into \texttt{.lake/build/dressed/}.

  Returns the array of written artifact file paths.

  \uses{dr:module-to-rel-path, dr:get-module-dressed-dir} -/)
  (proof := /-- Creates parent directories with \texttt{FS.createDirAll}, writes the header
  file via \texttt{FS.writeFile}, then maps over artifacts writing each to its
  own subdirectory. -/)
  (uses := ["dr:module-to-rel-path"])]
  Dress.outputLatexResults

attribute [blueprint "dr:output-json-results"
  (title := "Output JSON Results")
  (statement := /-- Writes a module's JSON representation to a \texttt{.json} file in the
  dressed artifacts directory.

  \uses{dr:module-to-rel-path} -/)
  (proof := /-- Creates parent directories and writes pretty-printed JSON via
  \texttt{FS.writeFile json.pretty}. -/)
  (uses := ["dr:module-to-rel-path"])]
  Dress.outputJsonResults

attribute [blueprint "dr:output-library-latex"
  (keyDeclaration := true)
  (title := "Output Library LaTeX Index")
  (statement := /-- Writes a library-level LaTeX index file that \texttt{\textbackslash input}s
  all module \texttt{.tex} files for a given library.

  This aggregation file is the entry point for including an entire library's
  blueprint content in a LaTeX document. Each module's header file is referenced
  via \texttt{Architect.Latex.input}.

  \uses{dr:library-to-rel-path, dr:module-to-rel-path} -/)
  (proof := /-- Intercalates \texttt{Architect.Latex.input} calls for each module path,
  separated by double newlines, and writes to the library index path. -/)
  (uses := ["dr:library-to-rel-path", "dr:module-to-rel-path"])]
  Dress.outputLibraryLatex

attribute [blueprint "dr:output-library-json"
  (title := "Output Library JSON Index")
  (statement := /-- Writes a library-level JSON index file containing an array of relative paths
  to all module \texttt{.json} files.

  The output is a JSON object with a single \texttt{"modules"} key whose value
  is the array of module JSON file paths.

  \uses{dr:library-to-rel-path, dr:module-to-rel-path} -/)
  (proof := /-- Constructs a JSON object with \texttt{Json.mkObj}, mapping each module
  name to its relative path via \texttt{moduleToRelPath}, then writes
  pretty-printed JSON. -/)
  (uses := ["dr:library-to-rel-path", "dr:module-to-rel-path"])]
  Dress.outputLibraryJson


-- ============================================================================
-- === Dress.Hook (Elaboration-Time Artifact Facade) ===
-- ============================================================================

-- ─── Path Helpers ───

attribute [blueprint "dr:get-highlighting-output-path"
  (title := "Highlighting Output Path")
  (statement := /-- Computes the output path for a module's SubVerso-compatible JSON file.

  Returns \texttt{.lake/build/dressed/\{Module/Path\}.subverso.json}. This uses the
  \texttt{.subverso.json} extension (distinct from the Dress \texttt{.json} format)
  to avoid overwriting the extended Dress format files. -/)
  (proof := /-- Delegates to \texttt{Generate.getSubVersoOutputPath}. -/)]
  Dress.getHighlightingOutputPath

-- ─── Write Helpers ───

attribute [blueprint "dr:write-module-highlighting-json"
  (title := "Write Module Highlighting JSON")
  (statement := /-- Writes all captured module-level highlighting data to a single SubVerso JSON file
  at \texttt{.lake/build/dressed/\{Module/Path\}.subverso.json}.

  Skips writing if the highlighting map is empty. The JSON format uses SubVerso's
  \texttt{Module} structure for compatibility with the \texttt{subverso-extract-mod}
  tooling.

  \uses{dr:get-highlighting-output-path} -/)
  (proof := /-- Guards on \texttt{highlighting.isEmpty}, then delegates to
  \texttt{Serialize.writeJsonAtomic} with data from
  \texttt{Serialize.highlightingMapToModuleJson}. -/)
  (uses := ["dr:get-highlighting-output-path"])]
  Dress.writeModuleHighlightingJson

attribute [blueprint "dr:write-module-highlighting-html"
  (title := "Write Module Highlighting HTML")
  (statement := /-- Writes all captured module-level highlighting as pre-rendered HTML to a JSON
  map file at \texttt{.lake/build/dressed/\{Module/Path\}.html.json}.

  The HTML map associates declaration names with their pre-rendered HTML strings,
  enabling downstream consumers (Runway) to use pre-rendered HTML instead of
  re-rendering SubVerso highlighted tokens at site generation time.

  \uses{dr:get-highlighting-output-path} -/)
  (proof := /-- Guards on \texttt{highlighting.isEmpty}, then delegates to
  \texttt{Serialize.writeHighlightingHtml}. -/)
  (uses := ["dr:get-highlighting-output-path"])]
  Dress.writeModuleHighlightingHtml

-- ─── Load Helpers ───

attribute [blueprint "dr:load-module-highlighting"
  (title := "Load Module Highlighting")
  (statement := /-- Loads SubVerso highlighting data for a module from the build cache.

  Reads from \texttt{.lake/build/dressed/\{Module/Path\}.subverso.json} and returns
  a \texttt{NameMap Highlighted} mapping declaration names to their highlighted
  representations.

  \uses{dr:get-highlighting-output-path} -/)
  (proof := /-- Delegates to \texttt{Serialize.loadModuleJson}. -/)
  (uses := ["dr:get-highlighting-output-path"])]
  Dress.loadModuleHighlighting

attribute [blueprint "dr:load-module-highlighting-html"
  (title := "Load Module Highlighting HTML")
  (statement := /-- Loads pre-rendered HTML highlighting for a module from the build cache.

  Reads from \texttt{.lake/build/dressed/\{Module/Path\}.html.json} and returns
  a \texttt{NameMap String} mapping declaration names to their HTML strings.

  \uses{dr:get-highlighting-output-path} -/)
  (proof := /-- Delegates to \texttt{Serialize.loadHighlightingHtml}. -/)
  (uses := ["dr:get-highlighting-output-path"])]
  Dress.loadModuleHighlightingHtml

attribute [blueprint "dr:write-highlighting-json"
  (title := "Write Declaration Highlighting JSON")
  (statement := /-- Writes highlighted code for a single declaration to a JSON file at
  \texttt{.lake/build/dressed/\{Module/Path\}/\{DeclName\}.json}.

  This is the per-declaration counterpart of \texttt{writeModuleHighlightingJson},
  used for fine-grained caching of individual declaration highlighting. -/)
  (proof := /-- Constructs the output path from module name components, then delegates
  to \texttt{Serialize.writeJsonAtomic} with \texttt{Serialize.highlightedToJson}. -/)]
  Dress.writeHighlightingJson


-- ============================================================================
-- === Dress.Highlighting (SubVerso Integration) ===
-- ============================================================================

-- ─── Configuration ───

attribute [blueprint "dr:get-suppressed-namespaces"
  (title := "Get Suppressed Namespaces")
  (statement := /-- Returns the list of namespaces whose declarations should be suppressed
  during SubVerso highlighting.

  Currently returns an empty list. This hook exists for future extensibility,
  following the same pattern used by SubVerso's own highlighting infrastructure. -/)
  (proof := /-- Returns the empty list. -/)]
  Dress.getSuppressedNamespaces

-- ─── Elaboration Wrapper ───

attribute [blueprint "dr:run-command"
  (title := "Run Command with Info Tree Context")
  (statement := /-- Executes a command elaboration action while capturing info trees with proper
  context for SubVerso highlighting.

  Wraps the action in \texttt{withInfoTreeContext} with an elaborator tag of
  \texttt{Dress.highlight}, ensuring that info trees produced during the action
  are properly annotated for SubVerso to process.

  This is the critical bridge between Lean's elaboration infrastructure and
  SubVerso's highlighting system---without this wrapper, info trees lack the
  context nodes SubVerso needs for semantic token classification. -/)
  (proof := /-- Runs the action through \texttt{withInfoTreeContext} with a
  \texttt{CommandInfo} node wrapper, converting to \texttt{IO} via
  \texttt{EIO.toIO'} and translating elaboration errors to \texttt{IO.userError}. -/)]
  Dress.runCommand

-- ─── Main Highlighting Functions ───

attribute [blueprint "dr:highlight-source"
  (keyDeclaration := true)
  (title := "Highlight Source Code")
  (statement := /-- Highlights complete Lean source code by parsing, elaborating, and running
  SubVerso on the resulting info trees.

  This follows the pattern used by the Lean~4 reference manual (Verso) to obtain
  properly contextualized info trees. The process:
  \begin{enumerate}
    \item Creates an input context and command state from the source string
    \item Iterates: parse a command, elaborate it via \texttt{runCommand}
          (which wraps in \texttt{withInfoTreeContext})
    \item After all commands are processed, runs SubVerso's
          \texttt{highlightIncludingUnparsed} on the accumulated info trees
    \item Returns the highlighted code and the message log
  \end{enumerate}

  The source should be complete Lean code with imports if needed.

  \uses{dr:run-command} -/)
  (proof := /-- A \texttt{repeat/break} loop parses and elaborates commands until a terminal
  command is reached. The highlighting phase runs in a fresh \texttt{TermElabM}
  context constructed with minimal state (\texttt{coreCtx}, \texttt{metaCtx},
  \texttt{termCtx}). -/)
  (uses := ["dr:run-command"])]
  Dress.highlightSource

attribute [blueprint "dr:highlight-declaration"
  (keyDeclaration := true)
  (title := "Highlight Declaration")
  (statement := /-- Highlights a single Lean declaration from source code, returning only the
  highlighted output (discarding the message log).

  This is the main entry point for blueprint highlighting. Takes complete Lean
  source and returns the SubVerso \texttt{Highlighted} representation.

  \uses{dr:highlight-source} -/)
  (proof := /-- Calls \texttt{highlightSource} and projects the first component of the
  result pair. -/)
  (uses := ["dr:highlight-source"])]
  Dress.highlightDeclaration

attribute [blueprint "dr:mk-minimal-source"
  (title := "Make Minimal Source")
  (statement := /-- Creates a minimal Lean source file containing import statements and the
  declaration text.

  Used to construct the source string that \texttt{highlightSource} requires:
  a self-contained Lean file that can be parsed and elaborated. Imports are
  rendered as \texttt{import \{name\}} lines joined by newlines, followed by
  the declaration text separated by a blank line. -/)
  (proof := /-- Maps import names to \texttt{"import \{n}"} strings, intercalates with
  newlines, and appends the declaration text with a separator. -/)]
  Dress.mkMinimalSource


-- ============================================================================
-- === Dress.HtmlRender (Verso HTML Rendering) ===
-- ============================================================================

-- ─── Rendering Context ───

attribute [blueprint "dr:html-render-context"
  (title := "Default HTML Render Context")
  (statement := /-- The default context for rendering SubVerso highlighted code to HTML via Verso.

  Configures \texttt{HighlightHtmlM} with:
  \begin{itemize}
    \item Empty \texttt{linkTargets} (no hyperlinks to external declarations)
    \item Unit \texttt{traverseContext} (no document traversal state)
    \item Empty \texttt{definitionIds} (no anchors for defined names)
    \item Default \texttt{options}
  \end{itemize}

  Uses \texttt{Genre.none} to avoid dependencies on Verso's full document
  infrastructure while still utilizing its production HTML renderer. -/)
  (proof := /-- A record literal with empty/default values for all context fields. -/)]
  Dress.HtmlRender.defaultContext

-- ─── Rendering Functions ───

attribute [blueprint "dr:html-render-with-hovers"
  (title := "Render Highlighted with Hovers")
  (statement := /-- Renders SubVerso highlighted code to an HTML string paired with hover tooltip
  JSON data.

  Returns \texttt{(html, hoverJson)} where:
  \begin{itemize}
    \item \texttt{html} is the rendered HTML string with rainbow bracket matching
          (6-color cycling via Verso's \texttt{toHtmlRainbow})
    \item \texttt{hoverJson} is a compressed JSON string mapping hover IDs to
          their tooltip content (type signatures, documentation)
  \end{itemize}

  The hover data enables interactive code display where users can see type
  information on mouseover.

  \uses{dr:html-render-context} -/)
  (proof := /-- Runs \texttt{hl.toHtmlRainbow} through the \texttt{HighlightHtmlM} monad
  with \texttt{defaultContext} and an empty \texttt{Hover.State}. Extracts the
  HTML string and deduplicates hover data via \texttt{finalState.dedup.docJson}. -/)
  (uses := ["dr:html-render-context"])]
  Dress.HtmlRender.renderHighlightedWithHovers

attribute [blueprint "dr:html-render-with-state"
  (title := "Render Highlighted with State")
  (statement := /-- Renders SubVerso highlighted code with an initial hover state, returning the
  final state for chaining.

  Returns \texttt{(html, hoverJson, finalState)} where \texttt{finalState} can be
  passed as \texttt{initialState} to subsequent renders, enabling continuous hover
  ID numbering across multiple code blocks on the same page.

  \uses{dr:html-render-context} -/)
  (proof := /-- Same as \texttt{renderHighlightedWithHovers} but accepts and returns the
  \texttt{Hover.State Html} for chaining. -/)
  (uses := ["dr:html-render-context"])]
  Dress.HtmlRender.renderHighlightedWithState

attribute [blueprint "dr:html-render-to-html"
  (keyDeclaration := true)
  (title := "Render Highlighted to HTML")
  (statement := /-- Renders SubVerso highlighted code to an HTML string, discarding hover data.

  This is the simplest rendering entry point, used when hover tooltips are not
  needed (e.g., for base64-encoded LaTeX embedding where interactivity is not
  available).

  Uses Verso's production renderer with rainbow bracket matching (6-color cycling,
  unmatched brackets marked with error color).

  \uses{dr:html-render-with-hovers} -/)
  (proof := /-- Projects the first component of \texttt{renderHighlightedWithHovers}. -/)
  (uses := ["dr:html-render-with-hovers"])]
  Dress.HtmlRender.renderHighlightedToHtml

attribute [blueprint "dr:html-render-block"
  (title := "Render Highlighted Block")
  (statement := /-- Renders highlighted code wrapped in a block-level code element with
  CSS class \texttt{hl lean block}.

  Uses Verso's \texttt{blockHtmlRainbow} which produces a \texttt{<pre>} element
  suitable for standalone code display with proper bracket pair matching. -/)
  (proof := /-- Runs \texttt{hl.blockHtmlRainbow ""} through the rendering monad. -/)]
  Dress.HtmlRender.renderHighlightedBlock

attribute [blueprint "dr:html-render-inline"
  (title := "Render Highlighted Inline")
  (statement := /-- Renders highlighted code as an inline element with CSS class
  \texttt{hl lean inline}.

  Uses Verso's \texttt{inlineHtmlRainbow} which produces a \texttt{<code>} element
  suitable for inline code within text, with proper bracket pair matching. -/)
  (proof := /-- Runs \texttt{hl.inlineHtmlRainbow none} through the rendering monad. -/)]
  Dress.HtmlRender.renderHighlightedInline


-- ============================================================================
-- === Dress.SubVersoExtract (External SubVerso CLI Integration) ===
-- ============================================================================

-- ─── Executable Discovery ───

attribute [blueprint "dr:default-extract-mod-path"
  (title := "Default Extract Mod Path")
  (statement := /-- The default relative path to the \texttt{subverso-extract-mod} executable
  from the project root:
  \texttt{.lake/packages/subverso/.lake/build/bin/subverso-extract-mod}.

  This is where Lake places the built executable after \texttt{lake build subverso}. -/)
  (proof := /-- A constant \texttt{FilePath} literal using the \texttt{/} path operator. -/)]
  Dress.SubVersoExtract.defaultExtractModPath

attribute [blueprint "dr:find-extract-mod-executable"
  (title := "Find Extract Mod Executable")
  (statement := /-- Searches for the \texttt{subverso-extract-mod} executable in multiple
  locations, returning the first found path.

  Search order:
  \begin{enumerate}
    \item Explicit path provided as argument
    \item Default location relative to the current working directory
    \item \texttt{LAKE\_HOME} environment variable path
  \end{enumerate}

  Returns \texttt{none} if the executable is not found in any location.

  \uses{dr:default-extract-mod-path} -/)
  (proof := /-- Sequentially checks \texttt{pathExists} for each candidate path,
  returning \texttt{some} on the first success. -/)
  (uses := ["dr:default-extract-mod-path"])]
  Dress.SubVersoExtract.findExtractModExecutable

-- ─── External Process Invocation ───

attribute [blueprint "dr:run-extract-mod"
  (title := "Run SubVerso Extract Mod")
  (statement := /-- Invokes the \texttt{subverso-extract-mod} CLI tool via \texttt{lake exe}
  to extract highlighted code for a Lean module.

  Uses \texttt{lake exe subverso-extract-mod} rather than calling the binary
  directly, ensuring the correct Lean toolchain and environment are used.

  Returns the raw JSON string output on success, or \texttt{none} on failure
  (with a warning logged to stderr). -/)
  (proof := /-- Spawns a child process via \texttt{IO.Process.spawn}, reads stdout
  to completion, checks the exit code, and returns the output string. -/)]
  Dress.SubVersoExtract.runExtractMod

attribute [blueprint "dr:extract-module-highlighting"
  (title := "Extract Module Highlighting")
  (statement := /-- Extracts module highlighting by calling \texttt{subverso-extract-mod} and
  parsing the JSON output into SubVerso's \texttt{ModuleItem} array.

  The JSON parsing uses SubVerso's \texttt{Module.fromJson?} instance to handle
  the deduplicated export format that SubVerso uses for efficient serialization.

  Returns \texttt{none} on any failure (subprocess error, JSON parse error,
  Module parse error), with warnings logged to stderr.

  \uses{dr:run-extract-mod} -/)
  (proof := /-- Chains \texttt{runExtractMod} with \texttt{Json.parse} and
  \texttt{Module.fromJson?}, returning \texttt{none} at each error point. -/)
  (uses := ["dr:run-extract-mod"])]
  Dress.SubVersoExtract.extractModuleHighlighting

-- ─── Highlighting Map Construction ───

attribute [blueprint "dr:build-highlighting-map"
  (title := "Build Highlighting Map")
  (statement := /-- Constructs a \texttt{NameMap Highlighted} from SubVerso's \texttt{ModuleItem}
  array.

  For each item, maps every name in the item's \texttt{defines} array to the
  item's \texttt{code} field. This handles the many-to-one relationship where
  a single source range can define multiple names (e.g., mutual definitions). -/)
  (proof := /-- A nested \texttt{foldl}: the outer fold iterates over items, the inner
  fold iterates over each item's \texttt{defines} array, inserting into the
  accumulator map. -/)]
  Dress.SubVersoExtract.buildHighlightingMap

attribute [blueprint "dr:load-highlighting-from-file"
  (title := "Load Highlighting from File")
  (statement := /-- Loads highlighting data from a pre-computed JSON file, handling both the
  \texttt{subverso-extract-mod} format and the Hook.lean elaboration-time format.

  Both formats use SubVerso's \texttt{Module} JSON structure. This is the fast
  path when highlighting has already been captured and cached to disk.

  Returns an empty map if the file does not exist or cannot be parsed.

  \uses{dr:build-highlighting-map} -/)
  (proof := /-- Checks \texttt{pathExists}, reads the file, parses via
  \texttt{Json.parse} and \texttt{Module.fromJson?}, then delegates to
  \texttt{buildHighlightingMap}. Returns empty maps at each error point. -/)
  (uses := ["dr:build-highlighting-map"])]
  Dress.SubVersoExtract.loadHighlightingFromFile

-- ─── Main Entry Points ───

attribute [blueprint "dr:extract-highlighting-map"
  (keyDeclaration := true)
  (title := "Extract Highlighting Map")
  (statement := /-- The main entry point for getting highlighted code for a module's declarations
  via the external \texttt{subverso-extract-mod} tool.

  Combines \texttt{extractModuleHighlighting} and \texttt{buildHighlightingMap}
  into a single call. Returns an empty map on failure (with warnings logged
  to stderr).

  This is the slow path that requires running the external tool; prefer
  \texttt{loadHighlightingWithFallback} when cached data may be available.

  \uses{dr:extract-module-highlighting, dr:build-highlighting-map} -/)
  (proof := /-- Calls \texttt{extractModuleHighlighting} with early return on
  \texttt{none}, then passes the result to \texttt{buildHighlightingMap}. -/)
  (uses := ["dr:extract-module-highlighting", "dr:build-highlighting-map"])]
  Dress.SubVersoExtract.extractHighlightingMap

attribute [blueprint "dr:get-highlighting-path"
  (title := "Get Highlighting Path")
  (statement := /-- Computes the path for a module's highlighting JSON file within the build
  directory.

  Returns \texttt{.lake/build/dressed/\{Module/Path\}.json}. This is the path
  where Hook.lean writes elaboration-time highlighting data. -/)
  (proof := /-- Folds module name components into subdirectories under
  \texttt{buildDir / "dressed"}, then applies \texttt{.withExtension "json"}. -/)]
  Dress.SubVersoExtract.getHighlightingPath

attribute [blueprint "dr:load-highlighting-with-fallback"
  (title := "Load Highlighting with Fallback")
  (statement := /-- Loads highlighting for a module with a two-tier fallback strategy:
  \begin{enumerate}
    \item First checks for a cached JSON file from Hook.lean elaboration-time capture
    \item Falls back to running \texttt{subverso-extract-mod} if no cached file exists
  \end{enumerate}

  The \texttt{buildDir} should be the Lake build directory (e.g., \texttt{.lake/build}).
  This is the recommended entry point for consumers that want highlighting data
  regardless of how it was captured.

  \uses{dr:get-highlighting-path, dr:load-highlighting-from-file, dr:extract-highlighting-map} -/)
  (proof := /-- Checks \texttt{pathExists} for the cached JSON path. If found, delegates
  to \texttt{loadHighlightingFromFile}. Otherwise falls back to
  \texttt{extractHighlightingMap}. -/)
  (uses := ["dr:get-highlighting-path", "dr:load-highlighting-from-file", "dr:extract-highlighting-map"])]
  Dress.SubVersoExtract.loadHighlightingWithFallback

attribute [blueprint "dr:get-declaration-highlighting"
  (title := "Get Declaration Highlighting")
  (statement := /-- Retrieves highlighted code for a specific declaration by name.

  Extracts the full module's highlighting and looks up the specific name in
  the resulting map. Returns \texttt{none} if the declaration is not found or
  if extraction fails.

  \uses{dr:extract-highlighting-map} -/)
  (proof := /-- Calls \texttt{extractHighlightingMap} and then \texttt{NameMap.find?}
  on the result. -/)
  (uses := ["dr:extract-highlighting-map"])]
  Dress.SubVersoExtract.getDeclarationHighlighting

attribute [blueprint "dr:get-declaration-range"
  (title := "Get Declaration Range")
  (statement := /-- Retrieves the source range for a specific declaration from a pre-extracted
  array of \texttt{ModuleItem}s.

  Returns \texttt{(startPos, endPos)} if the declaration is found in any item's
  \texttt{defines} array, or \texttt{none} otherwise. -/)
  (proof := /-- Uses \texttt{Array.findSome?} to locate the first item whose
  \texttt{defines} contains the declaration name, then extracts its range. -/)]
  Dress.SubVersoExtract.getDeclarationRange

attribute [blueprint "dr:get-all-declarations"
  (title := "Get All Declarations")
  (statement := /-- Extracts all declarations defined in a module along with their highlighted
  code from a pre-extracted array of \texttt{ModuleItem}s.

  Returns an array of \texttt{(Name, Highlighted)} pairs. Each item in the input
  may contribute multiple entries (one per name in its \texttt{defines} array),
  all sharing the same highlighted code. -/)
  (proof := /-- A nested \texttt{foldl}: the outer fold iterates over items, the inner
  fold pushes \texttt{(name, item.code)} for each name in \texttt{item.defines}. -/)]
  Dress.SubVersoExtract.getAllDeclarations

end DressBlueprint.Annotations

#dressNodes
