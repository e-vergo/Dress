/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress

/-!
# Blueprint Annotations: Core Types, Paths, Base64, Cache, Content, Load

Retroactive `@[blueprint]` annotations for the foundational Dress modules.
These modules define the core data model, filesystem path management,
encoding utilities, caching infrastructure, content aggregation, and
module loading.
-/

open Dress

namespace DressBlueprint.Annotations

/-! ## Core.lean: Core Types -/

attribute [blueprint "dr:node-with-pos"
  (keyDeclaration := true)
  (title := "NodeWithPos")
  (statement := /-- The central data type of Dress. \texttt{NodeWithPos} extends
  \texttt{Architect.Node} with elaboration-time metadata: source location,
  proof body location, SubVerso highlighted code (full, signature, and proof body),
  and pre-rendered HTML strings.

  Every \texttt{@[blueprint]} declaration is ultimately represented as a
  \texttt{NodeWithPos} for downstream consumption by the graph builder,
  LaTeX emitter, and side-by-side renderer. -/)
  (proof := /-- Defined as a \texttt{structure} extending \texttt{Architect.Node}
  with additional fields for \texttt{hasLean}, \texttt{location},
  \texttt{proofLocation}, \texttt{file}, \texttt{highlightedCode},
  \texttt{highlightedSignature}, \texttt{highlightedProofBody},
  \texttt{htmlCode}, \texttt{htmlSignature}, and \texttt{htmlProofBody}. -/)]
  Dress.NodeWithPos

attribute [blueprint "dr:split-at-definition-assign"
  (title := "Split at Definition Assign")
  (statement := /-- Splits highlighted code at the definition's \texttt{:=} token
  to separate the signature from the proof body.

  Uses bracket-depth tracking to find the correct \texttt{:=} (not one
  nested inside parentheses). Recognizes \texttt{def}, \texttt{theorem},
  \texttt{lemma}, \texttt{abbrev}, \texttt{instance}, \texttt{structure},
  \texttt{class}, and \texttt{inductive} keywords. Also includes a trailing
  \texttt{by} token when present.

  Always strips any prefix before the def keyword (e.g., the
  \texttt{@[blueprint ...]} attribute). -/)
  (proof := /-- Iterative two-pass algorithm over flattened token list.
  Pass~1 collects leaf nodes from the \texttt{Highlighted} tree.
  Pass~2 tracks bracket depth after the def keyword, splitting at the
  first depth-0 \texttt{:=}. -/)
  (uses := ["dr:node-with-pos"])]
  Dress.splitAtDefinitionAssign

attribute [blueprint "dr:to-dress-node-with-pos"
  (title := "Convert Node to NodeWithPos")
  (statement := /-- Converts an \texttt{Architect.Node} into a \texttt{Dress.NodeWithPos}
  by looking up source location, declaration ranges, highlighted code,
  and pre-rendered HTML from the Lean environment and provided maps.

  Highlighted code is looked up from \texttt{highlightingMap} (populated by
  SubVerso extraction). Pre-rendered HTML comes from \texttt{htmlMap}
  (populated from \texttt{.html.json} files written during elaboration).

  Uses \texttt{splitAtDefinitionAssign} to separate signature from proof body
  in the highlighted code.

  \uses{dr:node-with-pos, dr:split-at-definition-assign} -/)
  (proof := /-- Checks environment membership, resolves module index,
  extracts \texttt{DeclarationRanges}, computes proof body range from
  selection range vs.\ full range, looks up highlighting and HTML maps,
  and assembles the extended record. -/)
  (uses := ["dr:node-with-pos", "dr:split-at-definition-assign"])]
  Dress.toDressNodeWithPos

/-! ## Paths.lean: Centralized Path Management -/

attribute [blueprint "dr:paths-build-module-path"
  (title := "Build Module Path")
  (statement := /-- Constructs a filesystem path from a base directory and a
  \texttt{Lean.Name} by folding the name components as subdirectories.
  For example, \texttt{Dress.Graph.Build} becomes
  \texttt{baseDir/Dress/Graph/Build}. -/)
  (proof := /-- Left fold over \texttt{moduleName.components}, appending
  each component as a subdirectory. -/)]
  Dress.Paths.buildModulePath

attribute [blueprint "dr:paths-get-module-dressed-dir"
  (title := "Get Module Dressed Directory")
  (statement := /-- Returns the dressed artifact directory for a module:
  \texttt{.lake/build/dressed/\{Module/Path\}/}.

  \uses{dr:paths-build-module-path} -/)
  (proof := /-- Delegates to \texttt{buildModulePath} with
  \texttt{buildDir / "dressed"} as the base. -/)
  (uses := ["dr:paths-build-module-path"])]
  Dress.Paths.getModuleDressedDir

attribute [blueprint "dr:paths-get-module-json-path"
  (title := "Get Module JSON Path")
  (statement := /-- Returns the path for a module's aggregated JSON metadata:
  \texttt{.lake/build/dressed/\{Module/Path\}/module.json}.

  \uses{dr:paths-get-module-dressed-dir} -/)
  (proof := /-- Appends \texttt{module.json} to the module dressed directory. -/)
  (uses := ["dr:paths-get-module-dressed-dir"])]
  Dress.Paths.getModuleJsonPath

attribute [blueprint "dr:paths-get-module-tex-path"
  (title := "Get Module TeX Path")
  (statement := /-- Returns the path for a module's LaTeX header file:
  \texttt{.lake/build/dressed/\{Module/Path\}/module.tex}.

  \uses{dr:paths-get-module-dressed-dir} -/)
  (proof := /-- Appends \texttt{module.tex} to the module dressed directory. -/)
  (uses := ["dr:paths-get-module-dressed-dir"])]
  Dress.Paths.getModuleTexPath

attribute [blueprint "dr:paths-sanitize-label"
  (keyDeclaration := true)
  (title := "Sanitize Label")
  (statement := /-- Sanitizes a blueprint label for use as a directory or filename
  by replacing colons with hyphens.
  For example, \texttt{thm:main} becomes \texttt{thm-main}.

  This is the canonical normalization used throughout the toolchain
  for filesystem paths, CSS selectors, and JavaScript \texttt{querySelector}
  calls. -/)
  (proof := /-- Single \texttt{String.replace} call: \texttt{":"} to \texttt{"-"}. -/)]
  Dress.Paths.sanitizeLabel

attribute [blueprint "dr:paths-get-declaration-dir"
  (title := "Get Declaration Directory")
  (statement := /-- Returns the artifact directory for a single declaration:
  \texttt{.lake/build/dressed/\{Module/Path\}/\{sanitized-label\}/}.

  Each \texttt{@[blueprint]} declaration gets its own subdirectory containing
  \texttt{decl.tex}, \texttt{decl.html}, \texttt{decl.json},
  \texttt{decl.hovers.json}, and \texttt{manifest.entry}.

  \uses{dr:paths-get-module-dressed-dir, dr:paths-sanitize-label} -/)
  (proof := /-- Appends the sanitized label as a subdirectory of the module
  dressed directory. -/)
  (uses := ["dr:paths-get-module-dressed-dir", "dr:paths-sanitize-label"])]
  Dress.Paths.getDeclarationDir

attribute [blueprint "dr:paths-get-declaration-tex-path"
  (title := "Get Declaration TeX Path")
  (statement := /-- Returns the path for a declaration's LaTeX file:
  \texttt{.lake/build/dressed/\{Module/Path\}/\{sanitized-label\}/decl.tex}.

  \uses{dr:paths-get-declaration-dir} -/)
  (proof := /-- Appends \texttt{decl.tex} to the declaration directory. -/)
  (uses := ["dr:paths-get-declaration-dir"])]
  Dress.Paths.getDeclarationTexPath

attribute [blueprint "dr:paths-get-declaration-html-path"
  (title := "Get Declaration HTML Path")
  (statement := /-- Returns the path for a declaration's HTML file:
  \texttt{.lake/build/dressed/\{Module/Path\}/\{sanitized-label\}/decl.html}.

  \uses{dr:paths-get-declaration-dir} -/)
  (proof := /-- Appends \texttt{decl.html} to the declaration directory. -/)
  (uses := ["dr:paths-get-declaration-dir"])]
  Dress.Paths.getDeclarationHtmlPath

attribute [blueprint "dr:paths-get-declaration-json-path"
  (title := "Get Declaration JSON Path")
  (statement := /-- Returns the path for a declaration's JSON metadata:
  \texttt{.lake/build/dressed/\{Module/Path\}/\{sanitized-label\}/decl.json}.

  \uses{dr:paths-get-declaration-dir} -/)
  (proof := /-- Appends \texttt{decl.json} to the declaration directory. -/)
  (uses := ["dr:paths-get-declaration-dir"])]
  Dress.Paths.getDeclarationJsonPath

attribute [blueprint "dr:paths-get-declaration-hovers-path"
  (title := "Get Declaration Hovers Path")
  (statement := /-- Returns the path for a declaration's hover tooltip data:
  \texttt{.lake/build/dressed/\{Module/Path\}/\{sanitized-label\}/decl.hovers.json}.

  \uses{dr:paths-get-declaration-dir} -/)
  (proof := /-- Appends \texttt{decl.hovers.json} to the declaration directory. -/)
  (uses := ["dr:paths-get-declaration-dir"])]
  Dress.Paths.getDeclarationHoversPath

attribute [blueprint "dr:paths-get-declaration-dir-for-latex"
  (title := "Get Declaration Dir for LaTeX")
  (statement := /-- Returns the declaration directory path relative to
  \texttt{blueprint/src/} for use in LaTeX \texttt{\\input\{\}} directives:
  \texttt{../../.lake/build/dressed/\{Module/Path\}/\{sanitized-label\}}.

  The \texttt{../../} prefix accounts for plasTeX running from
  \texttt{blueprint/} with tex files in \texttt{blueprint/src/}.

  \uses{dr:paths-sanitize-label} -/)
  (proof := /-- String concatenation of the \texttt{../../.lake/build/dressed/}
  prefix, module path components joined by \texttt{/}, and the sanitized label. -/)
  (uses := ["dr:paths-sanitize-label"])]
  Dress.Paths.getDeclarationDirForLatex

attribute [blueprint "dr:paths-get-module-dir-for-latex"
  (title := "Get Module Dir for LaTeX")
  (statement := /-- Returns the module dressed directory path relative to
  \texttt{blueprint/src/} for use in LaTeX \texttt{\\input\{\}} directives:
  \texttt{../../.lake/build/dressed/\{Module/Path\}}. -/)
  (proof := /-- String concatenation of \texttt{../../.lake/build/dressed/}
  prefix and module path components joined by \texttt{/}. -/)]
  Dress.Paths.getModuleDirForLatex

attribute [blueprint "dr:paths-get-manifest-path"
  (title := "Get Manifest Path")
  (statement := /-- Returns the path for the global manifest file that maps
  labels to their per-module artifact paths:
  \texttt{.lake/build/dressed/manifest.json}.

  This manifest is aggregated from individual \texttt{manifest.entry} files
  in each declaration directory. -/)
  (proof := /-- Returns \texttt{buildDir / "dressed" / "manifest.json"}. -/)]
  Dress.Paths.getManifestPath

attribute [blueprint "dr:paths-get-manifest-entry-path"
  (title := "Get Manifest Entry Path")
  (statement := /-- Returns the path for a declaration's manifest entry file:
  \texttt{.lake/build/dressed/\{Module/Path\}/\{sanitized-label\}/manifest.entry}.

  Each declaration writes a small entry file that is later aggregated
  into the global \texttt{manifest.json}.

  \uses{dr:paths-get-declaration-dir} -/)
  (proof := /-- Appends \texttt{manifest.entry} to the declaration directory. -/)
  (uses := ["dr:paths-get-declaration-dir"])]
  Dress.Paths.getManifestEntryPath

attribute [blueprint "dr:paths-get-declaration-relative-path"
  (title := "Get Declaration Relative Path")
  (statement := /-- Returns the path from \texttt{dressed/} to a declaration directory:
  \texttt{\{Module/Path\}/\{sanitized-label\}} (no leading or trailing slashes).

  Used for manifest entries to provide paths relative to the
  \texttt{dressed/} directory.

  \uses{dr:paths-sanitize-label} -/)
  (proof := /-- String concatenation of module path components and
  sanitized label. -/)
  (uses := ["dr:paths-sanitize-label"])]
  Dress.Paths.getDeclarationRelativePath

/-! ## Base64.lean: RFC 4648 Encoding -/

attribute [blueprint "dr:base64-encode"
  (title := "Base64 Encode ByteArray")
  (statement := /-- Encodes a \texttt{ByteArray} to a Base64 string following
  RFC~4648.

  Used to embed pre-rendered HTML in LaTeX macros
  (\texttt{\\leansourcehtml\{base64\}}) and SubVerso JSON for compatibility.
  Processes input in 3-byte groups, producing 4 Base64 characters each,
  with \texttt{=} padding as needed. -/)
  (proof := /-- Iterative loop processing 3 bytes at a time. Each group
  is split into four 6-bit indices into the Base64 alphabet array.
  Padding with \texttt{=} for incomplete final groups. -/)]
  Dress.Base64.encode

attribute [blueprint "dr:base64-encode-string"
  (title := "Base64 Encode String")
  (statement := /-- Encodes a \texttt{String} to Base64 by first converting
  to UTF-8 bytes, then applying RFC~4648 encoding.

  Convenience wrapper used throughout Dress for embedding string content
  (HTML, JSON) into LaTeX macros.

  \uses{dr:base64-encode} -/)
  (proof := /-- Composes \texttt{String.toUTF8} with \texttt{Base64.encode}. -/)
  (uses := ["dr:base64-encode"])]
  Dress.Base64.encodeString

/-! ## Cache.lean: Per-Declaration Content Cache -/

attribute [blueprint "dr:cache-compute-declaration-hash"
  (title := "Compute Declaration Hash")
  (statement := /-- Computes a content-addressed hash for a declaration's artifacts
  as a 16-character hex string.

  Hash inputs: fully qualified name, node JSON, highlighting JSON,
  file path, and source location range. A cache hit means none of these
  have changed, so artifacts can be reused without re-rendering.

  The hash is used as a directory name under
  \texttt{.lake/build/dressed/.decl\_cache/}. -/)
  (proof := /-- Builds a canonical string by concatenating all hash inputs
  with pipe delimiters, then applies \texttt{String.hash} (64-bit)
  and formats as 16-character hex. -/)]
  Dress.Cache.computeDeclarationHash

attribute [blueprint "dr:cache-get-cache-path"
  (title := "Get Cache Path")
  (statement := /-- Returns the cache directory path for a given content hash:
  \texttt{.lake/build/dressed/.decl\_cache/\{hash\}}. -/)
  (proof := /-- Simple path concatenation:
  \texttt{buildDir / "dressed" / ".decl\_cache" / hash}. -/)]
  Dress.Cache.getCachePath

attribute [blueprint "dr:cache-check-cache"
  (title := "Check Cache Validity")
  (statement := /-- Checks whether a valid cache entry exists for the given hash.
  Returns \texttt{true} only if the cache directory exists and all required
  artifact files (\texttt{decl.tex}, \texttt{decl.json},
  \texttt{manifest.entry}) are present.

  \uses{dr:cache-get-cache-path} -/)
  (proof := /-- Checks directory existence, then iterates over the required
  files list verifying each path exists. Returns \texttt{false} on
  any missing file. -/)
  (uses := ["dr:cache-get-cache-path"])]
  Dress.Cache.checkCache

attribute [blueprint "dr:cache-restore-from-cache"
  (title := "Restore from Cache")
  (statement := /-- Copies cached artifacts from the hash-keyed cache directory
  to the target declaration directory.

  Copies all required files (\texttt{decl.tex}, \texttt{decl.json},
  \texttt{manifest.entry}) and optional files (\texttt{decl.html},
  \texttt{decl.hovers.json}) if they exist. Creates the target directory
  if needed.

  \uses{dr:cache-get-cache-path} -/)
  (proof := /-- Creates target directory, then copies required files
  unconditionally and optional files via \texttt{copyIfExists}. -/)
  (uses := ["dr:cache-get-cache-path"])]
  Dress.Cache.restoreFromCache

attribute [blueprint "dr:cache-save-to-cache"
  (title := "Save to Cache")
  (statement := /-- Saves generated artifacts from the source declaration directory
  to the hash-keyed cache directory for future reuse.

  Mirrors \texttt{restoreFromCache}: copies required and optional files
  from the source directory to the cache.

  \uses{dr:cache-get-cache-path} -/)
  (proof := /-- Creates cache directory, then copies required files
  (checking existence) and optional files via \texttt{copyIfExists}. -/)
  (uses := ["dr:cache-get-cache-path"])]
  Dress.Cache.saveToCache

/-! ## Content.lean: Blueprint Content Aggregation -/

attribute [blueprint "dr:blueprint-content"
  (keyDeclaration := true)
  (title := "BlueprintContent")
  (statement := /-- The union type representing items in a module's blueprint output.
  Either a \texttt{.node} (an \texttt{@[blueprint]} declaration with full
  metadata) or a \texttt{.modDoc} (a module docstring from
  \texttt{blueprint\_comment}).

  Analogous to doc-gen4's \texttt{ModuleMember}. Contents are sorted by
  declaration range to preserve source order.

  \uses{dr:node-with-pos} -/)
  (proof := /-- Inductive type with two constructors:
  \texttt{.node : NodeWithPos -> BlueprintContent} and
  \texttt{.modDoc : ModuleDoc -> BlueprintContent}. -/)
  (uses := ["dr:node-with-pos"])]
  Dress.BlueprintContent

attribute [blueprint "dr:blueprint-content-declaration-range"
  (title := "BlueprintContent Declaration Range")
  (statement := /-- Extracts the source declaration range from a
  \texttt{BlueprintContent} item.

  For nodes, uses the location's range. For module docs, uses the
  module doc's declaration range. Returns \texttt{none} for nodes
  without location information.

  \uses{dr:blueprint-content} -/)
  (proof := /-- Pattern match on the constructor, extracting the range
  from \texttt{node.location} or \texttt{modDoc.declarationRange}. -/)
  (uses := ["dr:blueprint-content"])]
  Dress.BlueprintContent.declarationRange

attribute [blueprint "dr:blueprint-content-order"
  (title := "BlueprintContent Ordering")
  (statement := /-- Defines a source-position ordering on \texttt{BlueprintContent}
  items based on their declaration ranges.

  Items with known positions are ordered by \texttt{Position.lt} on their
  start positions. Items with positions come before items without.

  \uses{dr:blueprint-content-declaration-range} -/)
  (proof := /-- Pattern match on both declaration ranges, comparing
  start positions when both are present. -/)
  (uses := ["dr:blueprint-content-declaration-range"])]
  Dress.BlueprintContent.order

attribute [blueprint "dr:get-main-module-blueprint-contents"
  (title := "Get Main Module Blueprint Contents")
  (statement := /-- Retrieves all blueprint contents from the \emph{current} module
  (the one being elaborated).

  Collects nodes from the \texttt{blueprintExt} environment extension and
  module docstrings from \texttt{getMainModuleBlueprintDoc}, converts nodes
  to \texttt{NodeWithPos} via \texttt{toDressNodeWithPos}, and sorts
  everything by source position.

  \uses{dr:blueprint-content, dr:to-dress-node-with-pos,
        dr:blueprint-content-order} -/)
  (proof := /-- Maps over \texttt{blueprintExt.getEntries}, converts each
  to a \texttt{BlueprintContent.node}, concatenates with module docs,
  and sorts with \texttt{qsort BlueprintContent.order}. -/)
  (uses := ["dr:blueprint-content", "dr:to-dress-node-with-pos",
            "dr:blueprint-content-order"])]
  Dress.getMainModuleBlueprintContents

attribute [blueprint "dr:get-blueprint-contents"
  (title := "Get Blueprint Contents of Imported Module")
  (statement := /-- Retrieves all blueprint contents from an \emph{imported} module,
  using externally-provided highlighting and HTML maps.

  Unlike \texttt{getMainModuleBlueprintContents}, this function takes
  a module name and maps for highlighted code and pre-rendered HTML,
  since info trees are no longer available for imported modules.

  \uses{dr:blueprint-content, dr:to-dress-node-with-pos,
        dr:blueprint-content-order} -/)
  (proof := /-- Looks up the module index, fetches entries from
  \texttt{blueprintExt.getModuleEntries}, converts to \texttt{NodeWithPos}
  with the provided maps, concatenates module docs, and sorts. -/)
  (uses := ["dr:blueprint-content", "dr:to-dress-node-with-pos",
            "dr:blueprint-content-order"])]
  Dress.getBlueprintContents

/-! ## Load.lean: Environment and Module Loading -/

attribute [blueprint "dr:env-of-imports"
  (title := "Environment of Imports")
  (statement := /-- Creates a Lean \texttt{Environment} by importing the specified
  modules. Enables initializer execution for modules that register
  parser aliases at initialization time.

  Based on \texttt{DocGen4.envOfImports}. The resulting environment
  contains all declarations and environment extensions from the
  imported module graph. -/)
  (proof := /-- Calls \texttt{enableInitializersExecution} (unsafe), then
  \texttt{importModules} with \texttt{leakEnv} and \texttt{loadExts}
  enabled. -/)]
  Dress.envOfImports

attribute [blueprint "dr:run-env-of-imports"
  (title := "Run CoreM with Imported Environment")
  (statement := /-- Runs a \texttt{CoreM} action in an environment constructed from
  the given imports, with custom options.

  Initializes the search path, creates the environment via
  \texttt{envOfImports}, configures the \texttt{Core.Context} with
  high heartbeat limit and disabled async elaboration, then executes
  the action.

  Based on \texttt{DocGen4.load}.

  \uses{dr:env-of-imports} -/)
  (proof := /-- Initializes search path via \texttt{findSysroot},
  builds environment, constructs \texttt{Core.Context} with
  \texttt{maxHeartbeats := 100000000} and \texttt{Elab.async := false},
  and runs the action via \texttt{CoreM.toIO}. -/)
  (uses := ["dr:env-of-imports"])]
  Dress.runEnvOfImports

attribute [blueprint "dr:latex-output-of-import-module"
  (title := "LaTeX Output of Imported Module")
  (statement := /-- Produces the complete LaTeX output for an imported module's
  blueprint content. This is the main entry point for the
  \texttt{extract\_blueprint} CLI when generating per-module artifacts.

  Loads highlighted code from the specified JSON path or from the
  standard Hook.lean output location (falling back to
  \texttt{subverso-extract-mod}). Also loads pre-rendered HTML
  from \texttt{.html.json} files.

  \uses{dr:run-env-of-imports, dr:get-blueprint-contents} -/)
  (proof := /-- Resolves the highlighting source (explicit path, Hook output,
  or SubVerso fallback), loads HTML map, then calls
  \texttt{runEnvOfImports} with \texttt{moduleToLatexOutput}. -/)
  (uses := ["dr:run-env-of-imports"])]
  Dress.latexOutputOfImportModule

attribute [blueprint "dr:json-of-import-module"
  (title := "JSON Output of Imported Module")
  (statement := /-- Produces the JSON representation of an imported module's
  blueprint content.

  Similar to \texttt{latexOutputOfImportModule} but emits JSON instead
  of LaTeX. Uses the same highlighting loading strategy.

  \uses{dr:run-env-of-imports} -/)
  (proof := /-- Resolves the highlighting source, then calls
  \texttt{runEnvOfImports} with \texttt{moduleToJson}. -/)
  (uses := ["dr:run-env-of-imports"])]
  Dress.jsonOfImportModule

end DressBlueprint.Annotations

#dressNodes
