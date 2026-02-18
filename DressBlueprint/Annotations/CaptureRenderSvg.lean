/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress

open Dress

namespace DressBlueprint.Annotations.CaptureRenderSvg

/-! # Capture Submodule -/

/-! ## Capture.State -- Recursion guard for elaboration hooks -/

attribute [blueprint "dr:capture-hook-ref"
  (title := "Blueprint Capture Hook Reference")
  (statement := /--
    \texttt{IO.Ref Bool} that tracks whether we are currently inside the
    blueprint capture hook. Prevents infinite recursion when
    \texttt{elab\_rules} call \texttt{elabCommandTopLevel}.
  -/)
  (proof := /--
    An \texttt{initialize} declaration creating a mutable \texttt{IO.Ref Bool}
    initialized to \texttt{false}. Set to \texttt{true} during capture hook
    execution, checked before re-entering the hook.
  -/)]
  Dress.Capture.blueprintCaptureHookRef

attribute [blueprint "dr:in-capture-hook"
  (title := "In Capture Hook Check")
  (statement := /--
    Checks whether execution is currently inside the blueprint capture hook
    by reading \texttt{blueprintCaptureHookRef}. Returns \texttt{true}
    if a capture is in progress.
  -/)
  (proof := /--
    Reads the \texttt{IO.Ref Bool} from \texttt{blueprintCaptureHookRef}.
  -/)
  (uses := ["dr:capture-hook-ref"])]
  Dress.Capture.inCaptureHook

/-! ## Capture.Config -- Blueprint attribute parsing -/

attribute [blueprint "dr:blueprint-config"
  (title := "Blueprint Config Structure")
  (statement := /--
    Simplified blueprint configuration parsed during elaboration.
    A subset of the full \texttt{Architect.Node} containing only the fields
    needed for \texttt{.tex} generation: \texttt{latexLabel}, \texttt{statement},
    \texttt{proof}, \texttt{usesLabels}, \texttt{proofUsesLabels},
    \texttt{latexEnv}, and \texttt{trace}.
  -/)
  (proof := /--
    A Lean structure with all-optional fields and a \texttt{trace} flag.
    Derives \texttt{Repr} and \texttt{Inhabited}.
  -/)]
  Dress.Capture.BlueprintConfig

attribute [blueprint "dr:has-blueprint-attr"
  (title := "Has Blueprint Attribute Check")
  (statement := /--
    Inspects \texttt{declModifiers} syntax to determine whether
    the declaration carries a \texttt{@[blueprint]} attribute.
    Walks the syntax tree: \texttt{declModifiers[1]} $\to$ \texttt{@[...]}
    $\to$ \texttt{attrInstance} list, checking each for
    \texttt{Architect.blueprint} kind.
  -/)
  (proof := /--
    Pure syntax inspection with no monadic effects. Returns \texttt{Bool}.
  -/)]
  Dress.Capture.hasBlueprintAttr

attribute [blueprint "dr:extract-blueprint-attr-syntax"
  (title := "Extract Blueprint Attribute Syntax")
  (statement := /--
    Extracts the full \texttt{@[blueprint ...]} attribute syntax node from
    \texttt{declModifiers}. Returns \texttt{Option Syntax} corresponding
    to the \texttt{attrInst[1]} part with kind \texttt{Architect.blueprint}.
  -/)
  (proof := /--
    Walks the same syntax structure as \texttt{hasBlueprintAttr} but
    uses \texttt{findSome?} to return the matching attribute node
    rather than a boolean.
  -/)
  (uses := ["dr:has-blueprint-attr"])]
  Dress.Capture.extractBlueprintAttrSyntax

attribute [blueprint "dr:parse-blueprint-config"
  (title := "Parse Blueprint Config")
  (statement := /--
    Parses a \texttt{BlueprintConfig} from the blueprint attribute syntax.
    Handles: optional trace flag (\texttt{?}), optional label string,
    \texttt{latexLabel}, \texttt{latexEnv}, \texttt{statement}, and
    \texttt{proof} options.
  -/)
  (proof := /--
    Runs in \texttt{CommandElabM}. Iterates over \texttt{blueprintOption}
    children, dispatching on inner option kind
    (\texttt{Architect.blueprintLatexLabelOption}, etc.).
    Uses \texttt{getDocStringText} for doc-comment--style statement/proof fields.
  -/)
  (uses := ["dr:blueprint-config", "dr:extract-blueprint-attr-syntax"])]
  Dress.Capture.parseBlueprintConfig

/-! ## Capture.InfoTree -- SubVerso highlighting from info trees -/

attribute [blueprint "dr:dressed-decl-ext"
  (title := "Dressed Declaration Extension")
  (statement := /--
    Persistent environment extension storing captured \texttt{Highlighted}
    values keyed by declaration name. This is the authoritative store of
    SubVerso highlighting captured during elaboration.
  -/)
  (proof := /--
    An \texttt{initialize} declaration using \texttt{registerNameMapExtension}
    for \texttt{Highlighted}.
  -/)]
  Dress.Capture.dressedDeclExt

attribute [blueprint "dr:get-module-highlighting"
  (title := "Get Module Highlighting")
  (statement := /--
    Retrieves all captured highlighting for the current environment
    as a \texttt{NameMap Highlighted}. Accessor for \texttt{dressedDeclExt}.
  -/)
  (proof := /--
    Calls \texttt{dressedDeclExt.getState env}.
  -/)
  (uses := ["dr:dressed-decl-ext"])]
  Dress.Capture.getModuleHighlighting

attribute [blueprint "dr:add-highlighting"
  (title := "Add Highlighting to Environment")
  (statement := /--
    Inserts a captured \texttt{Highlighted} value for a declaration
    into the environment extension via \texttt{dressedDeclExt.addEntry}.
  -/)
  (proof := /--
    Wraps \texttt{dressedDeclExt.addEntry env (declName, hl)}.
  -/)
  (uses := ["dr:dressed-decl-ext"])]
  Dress.Capture.addHighlighting

attribute [blueprint "dr:module-suffix-index-ref"
  (title := "Module Suffix Index Reference")
  (statement := /--
    Module-level suffix index cache. Built once per module on the first
    \texttt{@[blueprint]} declaration, then reused for all subsequent
    declarations. Avoids the $O(E)$ \texttt{env.constants.fold} per
    declaration where $E$ can exceed $100{,}000$ for mathlib-heavy projects.
  -/)
  (proof := /--
    An \texttt{initialize} declaration storing
    \texttt{IO.Ref (Option (HashMap String (Array Name)))},
    initially \texttt{none}.
  -/)]
  Dress.Capture.moduleSuffixIndexRef

attribute [blueprint "dr:module-cache-ref"
  (title := "Module Highlight Cache Reference")
  (statement := /--
    Module-level expression/signature cache. Created on first use and
    passed to SubVerso's \texttt{highlightIncludingUnparsed} for
    cross-declaration sharing of type signatures and expressions.
  -/)
  (proof := /--
    An \texttt{initialize} declaration storing
    \texttt{IO.Ref (Option (IO.Ref ModuleHighlightCache))},
    initially \texttt{none}.
  -/)]
  Dress.Capture.moduleCacheRef

attribute [blueprint "dr:get-or-build-suffix-index"
  (title := "Get or Build Suffix Index")
  (statement := /--
    Lazily builds the module-level suffix index. On first call, folds over
    \texttt{env.constants} to build a \texttt{HashMap}; on subsequent
    calls, returns the cached index.
  -/)
  (proof := /--
    Checks \texttt{moduleSuffixIndexRef}. On \texttt{none}, calls
    \texttt{buildStandaloneSuffixIndex} and caches the result.
  -/)
  (uses := ["dr:module-suffix-index-ref"])]
  Dress.Capture.getOrBuildSuffixIndex

attribute [blueprint "dr:get-or-create-module-cache-ref"
  (title := "Get or Create Module Cache Reference")
  (statement := /--
    Lazily creates the module-level highlight cache. On first call, allocates
    a fresh \texttt{IO.Ref ModuleHighlightCache}; on subsequent calls,
    returns the existing reference.
  -/)
  (proof := /--
    Checks \texttt{moduleCacheRef}. On \texttt{none}, calls
    \texttt{IO.mkRef default} and caches the new reference.
  -/)
  (uses := ["dr:module-cache-ref"])]
  Dress.Capture.getOrCreateModuleCacheRef

attribute [blueprint "dr:incremental-capture-state"
  (title := "Incremental Capture State")
  (statement := /--
    Tracks statistics for the incremental highlighting pipeline within a module:
    total declarations processed, cache hits, and cache misses.
    Used for profiling and understanding cache effectiveness.
  -/)
  (proof := /--
    A structure with three \texttt{Nat} fields, all defaulting to 0.
    Derives \texttt{Inhabited} and \texttt{Repr}.
  -/)]
  Dress.Capture.IncrementalCaptureState

attribute [blueprint "dr:incremental-state-ref"
  (title := "Incremental State Reference")
  (statement := /--
    Module-level mutable reference for incremental capture statistics.
    Reset at module boundaries by \texttt{resetModuleCaches}.
  -/)
  (proof := /--
    An \texttt{initialize} declaration creating \texttt{IO.Ref IncrementalCaptureState},
    initialized to default.
  -/)
  (uses := ["dr:incremental-capture-state"])]
  Dress.Capture.incrementalStateRef

attribute [blueprint "dr:get-incremental-stats"
  (title := "Get Incremental Stats")
  (statement := /--
    Returns the current incremental capture statistics
    (\texttt{IncrementalCaptureState}) for profiling.
  -/)
  (proof := /--
    Reads \texttt{incrementalStateRef}.
  -/)
  (uses := ["dr:incremental-state-ref"])]
  Dress.Capture.getIncrementalStats

attribute [blueprint "dr:reset-module-caches"
  (title := "Reset Module Caches")
  (statement := /--
    Clears all module-level caches at module boundaries:
    \texttt{moduleSuffixIndexRef}, \texttt{moduleCacheRef}, and
    \texttt{incrementalStateRef}. Prevents stale data from one
    module leaking into the next.
  -/)
  (proof := /--
    Sets each reference to its initial value (\texttt{none} or \texttt{default}).
  -/)
  (uses := ["dr:module-suffix-index-ref", "dr:module-cache-ref", "dr:incremental-state-ref"])]
  Dress.Capture.resetModuleCaches

attribute [blueprint "dr:capture-highlighting-from-info-trees"
  (keyDeclaration := true)
  (title := "Capture Highlighting from Info Trees")
  (statement := /--
    Core function that captures SubVerso highlighting from Lean's ephemeral
    info trees. Takes syntax, messages, and \texttt{PersistentArray InfoTree};
    calls SubVerso's \texttt{highlightIncludingUnparsed} with module-level
    caches for cross-declaration amortization.

    Must be called \emph{during} elaboration while info trees are still
    available. Returns \texttt{none} if trees are empty or highlighting fails.
  -/)
  (proof := /--
    Runs in \texttt{TermElabM}. Obtains the suffix index and cache ref via
    \texttt{getOrBuildSuffixIndex} and \texttt{getOrCreateModuleCacheRef},
    then passes them to \texttt{highlightIncludingUnparsed}. Catches
    all exceptions (SubVerso may throw for synthetic syntax).
  -/)
  (uses := ["dr:get-or-build-suffix-index", "dr:get-or-create-module-cache-ref"])]
  Dress.Capture.captureHighlightingFromInfoTrees

attribute [blueprint "dr:lazy-capture-highlighting"
  (title := "Lazy Capture Highlighting")
  (statement := /--
    Incremental variant of \texttt{captureHighlightingFromInfoTrees} that
    checks the module-level cache before performing full highlighting.
    Returns a \texttt{HighlightResult} indicating cached, fresh, or skipped.
    Updates \texttt{IncrementalCaptureState} for profiling.
  -/)
  (proof := /--
    Delegates to \texttt{lazyHighlightIncludingUnparsed} with module-level
    caches. On return, increments the appropriate counter in
    \texttt{incrementalStateRef}.
  -/)
  (uses := ["dr:capture-highlighting-from-info-trees", "dr:incremental-state-ref"])]
  Dress.Capture.lazyCaptureHighlightingFromInfoTrees

attribute [blueprint "dr:capture-highlighting"
  (title := "Capture Highlighting Entry Point")
  (statement := /--
    Main entry point for the \texttt{@[blueprint]} attribute handler.
    Extracts info trees from the current \texttt{CommandState}, filters
    messages to those within the declaration's syntax range, and delegates
    to \texttt{lazyCaptureHighlightingFromInfoTrees}. Stores the result
    in \texttt{dressedDeclExt}.

    Silently does nothing if highlighting is disabled, info trees are
    unavailable, or SubVerso fails.
  -/)
  (proof := /--
    Runs in \texttt{CommandElabM}. Checks \texttt{blueprint.highlighting}
    option, then extracts \texttt{infoState.trees} and filters messages
    by line range overlap with the declaration syntax. Calls
    \texttt{lazyCaptureHighlightingFromInfoTrees} via \texttt{liftTermElabM},
    then stores via \texttt{addHighlighting}.
  -/)
  (uses := ["dr:lazy-capture-highlighting", "dr:add-highlighting", "dr:dressed-decl-ext"])]
  Dress.Capture.captureHighlighting

/-! ## Capture.ElabRules -- Named helpers and command handler -/

attribute [blueprint "dr:get-decl-name-from-decl-id"
  (title := "Get Declaration Name from DeclId")
  (statement := /--
    Extracts the declaration name from a \texttt{declId} syntax node.
    Handles both the \texttt{Lean.Parser.Command.declId} composite form
    and plain identifier form.
  -/)
  (proof := /--
    Pattern-matches on \texttt{declId.getKind}: if it is
    \texttt{Command.declId}, extracts the first child's \texttt{getId};
    if it is an identifier, returns \texttt{declId.getId} directly.
  -/)]
  Dress.Capture.getDeclNameFromDeclId

attribute [blueprint "dr:in-capture-hook-m"
  (title := "In Capture Hook Check (CommandElabM)")
  (statement := /--
    Monadic version of the capture hook check for use in
    \texttt{CommandElabM}. Reads \texttt{blueprintCaptureHookRef}
    to determine if a capture is in progress.
  -/)
  (proof := /--
    Calls \texttt{blueprintCaptureHookRef.get} in \texttt{CommandElabM}.
  -/)
  (uses := ["dr:capture-hook-ref"])]
  Dress.Capture.inCaptureHookM

attribute [blueprint "dr:with-capture-hook-flag"
  (title := "With Capture Hook Flag")
  (statement := /--
    Runs an action with the capture hook flag set to \texttt{true},
    ensuring it is reset to \texttt{false} in a \texttt{finally} block.
    Used to guard against recursion when \texttt{elab\_rules} call
    \texttt{elabCommandTopLevel}.
  -/)
  (proof := /--
    Sets \texttt{blueprintCaptureHookRef} to \texttt{true}, runs the
    action, then resets to \texttt{false} in a \texttt{try/finally}.
  -/)
  (uses := ["dr:capture-hook-ref"])]
  Dress.Capture.withCaptureHookFlag

attribute [blueprint "dr:elab-decl-and-capture"
  (keyDeclaration := true)
  (title := "Elaborate Declaration and Capture Highlighting")
  (statement := /--
    The core helper called by all \texttt{elab\_rules}. Runs standard
    command elaboration under the capture hook flag, then:
    (1) resolves the declaration name with the current namespace,
    (2) calls \texttt{captureHighlighting},
    (3) looks up the \texttt{Architect.Node} from \texttt{blueprintExt},
    (4) writes all per-declaration artifacts via
    \texttt{writeDeclarationArtifactsFromNode}.
  -/)
  (proof := /--
    Runs in \texttt{CommandElabM}. Uses \texttt{withCaptureHookFlag} to
    call \texttt{elabCommandTopLevel}, then resolves the fully qualified name.
    On \texttt{blueprintExt} match, delegates artifact writing and logs
    timing for capture and write phases.
  -/)
  (uses := ["dr:with-capture-hook-flag", "dr:capture-highlighting",
            "dr:get-decl-name-from-decl-id", "dr:write-decl-artifacts-from-node"])]
  Dress.Capture.elabDeclAndCaptureHighlighting

attribute [blueprint "dr:elab-blueprint-declaration"
  (title := "Elaborate Blueprint Theorem Declaration")
  (statement := /--
    \texttt{@[command\_elab]} handler for theorem/lemma declarations with
    \texttt{@[blueprint]}. Checks for the blueprint attribute, guards
    against recursion via \texttt{inCaptureHookM}, then delegates to
    \texttt{elabDeclAndCaptureHighlighting}.
  -/)
  (proof := /--
    Registered as a \texttt{command\_elab} for
    \texttt{Lean.Parser.Command.declaration}. Filters to theorem kind,
    checks blueprint attribute, and delegates.
  -/)
  (uses := ["dr:elab-decl-and-capture", "dr:in-capture-hook-m", "dr:has-blueprint-attr"])]
  Dress.Capture.elabBlueprintDeclaration

/-! ## Generate Submodule -/

/-! ### Generate.Latex -- LaTeX file generation -/

attribute [blueprint "dr:get-default-latex-env"
  (title := "Get Default LaTeX Environment")
  (statement := /--
    Determines the appropriate \LaTeX{} environment name for a declaration
    by inspecting the environment's \texttt{ConstantInfo}. Returns
    \texttt{"theorem"} for \texttt{thmInfo} and \texttt{"definition"}
    for \texttt{defnInfo}, \texttt{inductInfo}, etc.
  -/)
  (proof := /--
    Runs in \texttt{CommandElabM}. Looks up the constant with
    \texttt{env.find?} and pattern-matches on the \texttt{ConstantInfo}
    constructor.
  -/)]
  Dress.Generate.getDefaultLatexEnv

attribute [blueprint "dr:generate-decl-tex-from-node"
  (title := "Generate Declaration TeX from Node")
  (statement := /--
    Generates \texttt{.tex} content for a single \texttt{@[blueprint]}
    declaration from an \texttt{Architect.Node}. Produces \LaTeX{} compatible
    with leanblueprint's theorem environments: \texttt{\\begin\{theorem\}},
    \texttt{\\label}, \texttt{\\lean}, \texttt{\\leanposition},
    \texttt{\\leansignaturesourcehtml} (base64), \texttt{\\leanproofsourcehtml}
    (base64), \texttt{\\leanhoverdata}, \texttt{\\uses}, \texttt{\\leanok},
    and above/below narrative content.
  -/)
  (proof := /--
    Runs in \texttt{CommandElabM}. Calls \texttt{Node.inferUses} for
    dependency inference. Splits highlighting at \texttt{:=} via
    \texttt{splitAtDefinitionAssign}, renders signature and proof body
    to HTML with stateful hover IDs via \texttt{renderHighlightedWithState},
    then base64-encodes all HTML content.
  -/)
  (uses := ["dr:get-default-latex-env"])]
  Dress.Generate.generateDeclarationTexFromNode

/-! ### Generate.Declaration -- Per-declaration artifact writing -/

attribute [blueprint "dr:write-decl-artifacts-from-node"
  (keyDeclaration := true)
  (title := "Write Declaration Artifacts from Node")
  (statement := /--
    Writes all artifacts (\texttt{.tex}, \texttt{.html}, \texttt{.json},
    \texttt{manifest.entry}) for a single \texttt{@[blueprint]} declaration.
    Uses content-based caching: on cache hit, artifacts are restored from
    \texttt{.lake/build/dressed/.decl\_cache/\{hash\}/}; on cache miss,
    artifacts are generated and saved to cache.
  -/)
  (proof := /--
    Runs in \texttt{CommandElabM}. Computes a content hash via
    \texttt{Cache.computeDeclarationHash}, checks the cache with
    \texttt{Cache.checkCache}, and either restores or generates artifacts.
    Generation delegates to the private \texttt{generateArtifacts} helper.
  -/)
  (uses := ["dr:generate-decl-tex-from-node"])]
  Dress.Generate.writeDeclarationArtifactsFromNode

/-! ### Generate.Module -- Module-level path helpers -/

attribute [blueprint "dr:get-subverso-output-path"
  (title := "Get SubVerso Output Path")
  (statement := /--
    Returns the output path for a module's SubVerso-compatible JSON file:
    \texttt{.lake/build/dressed/\{Module/Path\}.subverso.json}.
  -/)
  (proof := /--
    Folds module name components into the build directory path and
    appends the \texttt{.subverso.json} extension.
  -/)]
  Dress.Generate.getSubVersoOutputPath

attribute [blueprint "dr:get-highlighting-html-output-path"
  (title := "Get Highlighting HTML Output Path")
  (statement := /--
    Returns the output path for a module's HTML JSON file:
    \texttt{.lake/build/dressed/\{Module/Path\}.html.json}.
  -/)
  (proof := /--
    Folds module name components into the build directory path and
    appends the \texttt{.html.json} extension.
  -/)]
  Dress.Generate.getHighlightingHtmlOutputPath

/-! # Render Submodule -/

/-! ## Render.SideBySide -- Side-by-side LaTeX + Lean displays -/

attribute [blueprint "dr:sbs-data"
  (title := "Side-by-Side Data")
  (statement := /--
    Structure holding all data needed to render a side-by-side display:
    HTML element ID, display label, environment type, node status,
    LaTeX statement/proof HTML, Lean signature/proof-body HTML,
    hover data JSON, declaration names, and above/below narrative content.
  -/)
  (proof := /--
    A Lean structure with 12 fields. Derives \texttt{Repr} and
    \texttt{Inhabited}.
  -/)]
  Dress.Render.SbsData

attribute [blueprint "dr:sbs-variant"
  (title := "Side-by-Side Variant")
  (statement := /--
    Discriminates between rendering variants: \texttt{blueprint} (status
    character, standard CSS classes) and \texttt{paper} (verification badge,
    blueprint link, \texttt{paper-*} CSS classes).
  -/)
  (proof := /--
    An inductive type with two constructors: \texttt{blueprint} and
    \texttt{paper (blueprintUrl : Option String)}.
  -/)
  (uses := ["dr:sbs-data"])]
  Dress.Render.SbsVariant

attribute [blueprint "dr:escape-html"
  (title := "Escape HTML")
  (statement := /--
    Escapes HTML special characters (\texttt{\&}, \texttt{<}, \texttt{>},
    \texttt{"}, \texttt{'}) for safe inclusion in HTML attributes and content.
  -/)
  (proof := /--
    Sequential \texttt{String.replace} calls for each special character.
  -/)]
  Dress.Render.escapeHtml

attribute [blueprint "dr:status-to-color"
  (title := "Status to Color Hex")
  (statement := /--
    Maps each \texttt{NodeStatus} to its canonical color hex code.
    These are the 7-status color model values used in the side-by-side
    heading status dots.
  -/)
  (proof := /--
    Pattern match on \texttt{NodeStatus} returning hex strings.
  -/)]
  Dress.Render.statusToColor

attribute [blueprint "dr:status-to-display-string"
  (title := "Status to Display String")
  (statement := /--
    Maps each \texttt{NodeStatus} to a human-readable display string
    (e.g., \texttt{"Not Ready"}, \texttt{"Fully Proven"}).
  -/)
  (proof := /--
    Pattern match returning display strings.
  -/)]
  Dress.Render.statusToDisplayString

attribute [blueprint "dr:status-to-css-class"
  (title := "Status to CSS Class")
  (statement := /--
    Maps each \texttt{NodeStatus} to a CSS class name for the status
    indicator (e.g., \texttt{"status-not-ready"}, \texttt{"status-proven"}).
  -/)
  (proof := /--
    Pattern match returning CSS class strings.
  -/)]
  Dress.Render.statusToCssClass

attribute [blueprint "dr:capitalize"
  (title := "Capitalize String")
  (statement := /--
    Capitalizes the first letter of a string. Used for environment type
    display (e.g., \texttt{"theorem"} $\to$ \texttt{"Theorem"}).
  -/)
  (proof := /--
    Deconstructs the string to a char list, applies \texttt{Char.toUpper}
    to the head, and reconstructs.
  -/)]
  Dress.Render.capitalize

attribute [blueprint "dr:status-to-badge-class"
  (title := "Status to Badge CSS Class")
  (statement := /--
    Maps \texttt{NodeStatus} to a verification badge CSS class for paper mode:
    \texttt{"verified"}, \texttt{"in-progress"}, or \texttt{"not-started"}.
  -/)
  (proof := /--
    Pattern match grouping proven/fullyProven/mathlibReady as verified,
    sorry/wip as in-progress, notReady as not-started.
  -/)]
  Dress.Render.statusToBadgeClass

attribute [blueprint "dr:status-to-badge-label"
  (title := "Status to Badge Label")
  (statement := /--
    Maps \texttt{NodeStatus} to a verification badge label text:
    \texttt{"Verified"}, \texttt{"In Progress"}, or \texttt{"Not Started"}.
  -/)
  (proof := /--
    Same grouping as \texttt{statusToBadgeClass}.
  -/)
  (uses := ["dr:status-to-badge-class"])]
  Dress.Render.statusToBadgeLabel

attribute [blueprint "dr:status-to-badge-icon"
  (title := "Status to Badge Icon")
  (statement := /--
    Maps \texttt{NodeStatus} to a badge icon class suffix:
    \texttt{"check"}, \texttt{"half"}, or \texttt{"circle"}.
  -/)
  (proof := /--
    Same grouping as \texttt{statusToBadgeClass}.
  -/)
  (uses := ["dr:status-to-badge-class"])]
  Dress.Render.statusToBadgeIcon

attribute [blueprint "dr:render-verification-badge"
  (title := "Render Verification Badge")
  (statement := /--
    Renders an HTML verification badge for paper mode, producing markup
    matching \texttt{.verification-badge} CSS in \texttt{paper.css}.
    Combines badge class, icon, and label text.
  -/)
  (proof := /--
    Calls \texttt{statusToBadgeClass}, \texttt{statusToBadgeIcon}, and
    \texttt{statusToBadgeLabel}, then interpolates into an HTML
    \texttt{<span>} template.
  -/)
  (uses := ["dr:status-to-badge-class", "dr:status-to-badge-label", "dr:status-to-badge-icon"])]
  Dress.Render.renderVerificationBadge

attribute [blueprint "dr:render-proof-toggle"
  (title := "Render Proof Toggle")
  (statement := /--
    Renders the collapsible proof toggle (LaTeX proof). Always emits
    the \texttt{proof\_content} div when \texttt{proofHtml} is \texttt{some},
    even if empty, because the JS \texttt{slideToggle()} targets this
    sibling element.
  -/)
  (proof := /--
    Pattern matches on \texttt{proofHtml}; on \texttt{some}, returns
    the proof wrapper HTML with heading, expand arrow, and content div.
  -/)]
  Dress.Render.renderProofToggle

attribute [blueprint "dr:hover-data-attr"
  (title := "Hover Data Attribute")
  (statement := /--
    Builds the \texttt{data-lean-hovers} HTML attribute string from
    \texttt{SbsData.hoverData}, with proper HTML escaping.
  -/)
  (proof := /--
    Pattern matches on \texttt{hoverData}; on \texttt{some}, returns
    the escaped attribute string.
  -/)
  (uses := ["dr:sbs-data", "dr:escape-html"])]
  Dress.Render.hoverDataAttr

attribute [blueprint "dr:render-signature-cell"
  (title := "Render Signature Cell")
  (statement := /--
    Renders the Lean signature grid cell (row 2, col 2) with syntax-highlighted
    code or a fallback showing declaration names.
  -/)
  (proof := /--
    Checks \texttt{signatureHtml}; on \texttt{some}, wraps in
    \texttt{<code class="hl lean lean-signature">}. On \texttt{none},
    shows declaration name fallback.
  -/)
  (uses := ["dr:sbs-data", "dr:hover-data-attr"])]
  Dress.Render.renderSignatureCell

attribute [blueprint "dr:render-proof-lean-cell"
  (title := "Render Proof Lean Cell")
  (statement := /--
    Renders the Lean proof body grid cell (row 3, col 2) with
    syntax-highlighted proof code.
  -/)
  (proof := /--
    Pattern matches on \texttt{proofBodyHtml}; wraps in
    \texttt{<code class="hl lean lean-proof-body">} when present.
  -/)
  (uses := ["dr:sbs-data", "dr:hover-data-attr"])]
  Dress.Render.renderProofLeanCell

attribute [blueprint "dr:render-heading-cell-blueprint"
  (title := "Render Heading Cell (Blueprint)")
  (statement := /--
    Renders the heading grid cell for the blueprint variant. Includes
    environment type caption, display label, status dot button with
    color and tooltip, and optional blueprint/paper links.
  -/)
  (proof := /--
    Assembles HTML using \texttt{statusToColor}, \texttt{statusToCssClass},
    \texttt{statusToDisplayString}, and \texttt{capitalize}.
  -/)
  (uses := ["dr:sbs-data", "dr:status-to-color", "dr:status-to-css-class",
            "dr:status-to-display-string", "dr:capitalize", "dr:escape-html"])]
  Dress.Render.renderHeadingCellBlueprint

attribute [blueprint "dr:render-heading-cell-paper"
  (title := "Render Heading Cell (Paper)")
  (statement := /--
    Renders the heading grid cell for the paper variant. Includes
    environment type, display label, status dot, verification badge,
    and optional blueprint/paper links.
  -/)
  (proof := /--
    Delegates badge rendering to \texttt{renderVerificationBadge} and
    assembles the paper-specific HTML template.
  -/)
  (uses := ["dr:sbs-data", "dr:render-verification-badge", "dr:status-to-color",
            "dr:status-to-display-string", "dr:capitalize", "dr:escape-html"])]
  Dress.Render.renderHeadingCellPaper

attribute [blueprint "dr:render-heading-cell"
  (title := "Render Heading Cell (Dispatch)")
  (statement := /--
    Dispatches heading cell rendering based on \texttt{SbsVariant}:
    \texttt{blueprint} delegates to \texttt{renderHeadingCellBlueprint},
    \texttt{paper} delegates to \texttt{renderHeadingCellPaper}.
  -/)
  (proof := /--
    Pattern match on \texttt{SbsVariant} constructor.
  -/)
  (uses := ["dr:render-heading-cell-blueprint", "dr:render-heading-cell-paper", "dr:sbs-variant"])]
  Dress.Render.renderHeadingCell

attribute [blueprint "dr:render-statement-cell"
  (title := "Render Statement Cell")
  (statement := /--
    Renders the LaTeX statement grid cell (row 2, col 1) with
    the pre-rendered statement HTML wrapped in the environment-type
    content div.
  -/)
  (proof := /--
    Wraps \texttt{statementHtml} in a div with class
    \texttt{\{envType\}\_thmcontent}.
  -/)
  (uses := ["dr:sbs-data", "dr:escape-html"])]
  Dress.Render.renderStatementCell

attribute [blueprint "dr:render-proof-latex-cell"
  (title := "Render Proof LaTeX Cell")
  (statement := /--
    Renders the proof (LaTeX) grid cell (row 3, col 1) with the
    collapsible proof toggle.
  -/)
  (proof := /--
    Delegates to \texttt{renderProofToggle} and wraps in
    \texttt{sbs-proof-latex} div.
  -/)
  (uses := ["dr:sbs-data", "dr:render-proof-toggle"])]
  Dress.Render.renderProofLatexCell

attribute [blueprint "dr:render-side-by-side"
  (keyDeclaration := true)
  (title := "Render Side-by-Side Display")
  (statement := /--
    Main entry point for rendering a complete side-by-side display.
    Emits a 2-column $\times$ 5-row CSS grid:
    Row 1: above (LaTeX narrative) | spacer (collapses if absent);
    Row 2: heading | spacer;
    Row 3: statement | signature;
    Row 4: proof (LaTeX) | proof (Lean);
    Row 5: below (LaTeX narrative) | spacer (collapses if absent).

    Supports both blueprint and paper rendering variants via
    \texttt{SbsVariant}.
  -/)
  (proof := /--
    Selects container class based on variant, then calls each cell
    renderer: \texttt{renderHeadingCell}, \texttt{renderStatementCell},
    \texttt{renderSignatureCell}, \texttt{renderProofLatexCell},
    \texttt{renderProofLeanCell}. Conditionally emits above/below rows.
  -/)
  (uses := ["dr:sbs-data", "dr:sbs-variant", "dr:render-heading-cell",
            "dr:render-statement-cell", "dr:render-signature-cell",
            "dr:render-proof-latex-cell", "dr:render-proof-lean-cell",
            "dr:escape-html"])]
  Dress.Render.renderSideBySide

attribute [blueprint "dr:sbs-data-mk-prime"
  (title := "SbsData Convenience Constructor")
  (statement := /--
    Creates an \texttt{SbsData} from the minimal required fields:
    \texttt{id}, \texttt{label}, \texttt{envType}, \texttt{status},
    and \texttt{statementHtml}. All optional fields default.
  -/)
  (proof := /--
    Passes the five arguments to the structure constructor with
    remaining fields at their defaults.
  -/)
  (uses := ["dr:sbs-data"])]
  Dress.Render.SbsData.mk'

/-! # Serialize Submodule -/

/-! ## Serialize.Json -- JSON serialization -/

attribute [blueprint "dr:highlighted-to-json"
  (title := "Highlighted to JSON")
  (statement := /--
    Serializes a \texttt{Highlighted} value to JSON using SubVerso's
    deduplicated export format (\texttt{exportCode.toJson}).
  -/)
  (proof := /--
    Calls \texttt{hl.exportCode.toJson} for compact representation.
  -/)]
  Dress.Serialize.highlightedToJson

attribute [blueprint "dr:highlighting-map-to-module-json"
  (title := "Highlighting Map to Module JSON")
  (statement := /--
    Serializes a \texttt{NameMap Highlighted} to JSON in SubVerso Module
    format, compatible with \texttt{subverso-extract-mod} output.
    Each entry becomes a \texttt{ModuleItem} with kind \texttt{blueprint}.
  -/)
  (proof := /--
    Folds the name map into an array of \texttt{ModuleItem} values
    and serializes via \texttt{module.toJson}.
  -/)
  (uses := ["dr:highlighted-to-json"])]
  Dress.Serialize.highlightingMapToModuleJson

attribute [blueprint "dr:write-json-atomic"
  (title := "Write JSON Atomically")
  (statement := /--
    Writes JSON to a file atomically using a write-to-temp-then-rename
    pattern for crash safety on POSIX systems.
  -/)
  (proof := /--
    Creates parent directory, writes to a \texttt{.json.tmp} file,
    then calls \texttt{IO.FS.rename} for atomic replacement.
  -/)]
  Dress.Serialize.writeJsonAtomic

attribute [blueprint "dr:load-module-json"
  (title := "Load Module JSON")
  (statement := /--
    Loads highlighting from a SubVerso Module format JSON file.
    Returns a \texttt{NameMap Highlighted}, or an empty map if the
    file doesn't exist or parsing fails.
  -/)
  (proof := /--
    Reads the file, parses JSON, deserializes via
    \texttt{Module.fromJson?}, then folds items into a name map.
  -/)
  (uses := ["dr:highlighting-map-to-module-json"])]
  Dress.Serialize.loadModuleJson

/-! ## Serialize.Html -- HTML serialization -/

attribute [blueprint "dr:highlighting-map-to-html-json"
  (title := "Highlighting Map to HTML JSON")
  (statement := /--
    Serializes a \texttt{NameMap Highlighted} to a JSON map of
    declaration name $\to$ pre-rendered HTML string.
  -/)
  (proof := /--
    Maps each entry through \texttt{HtmlRender.renderHighlightedToHtml}
    and builds a JSON object.
  -/)]
  Dress.Serialize.highlightingMapToHtmlJson

attribute [blueprint "dr:write-highlighting-html"
  (title := "Write Highlighting HTML")
  (statement := /--
    Writes all captured module highlighting as pre-rendered HTML
    to a JSON map file.
  -/)
  (proof := /--
    Short-circuits on empty input. Delegates to
    \texttt{writeJsonAtomic} with \texttt{highlightingMapToHtmlJson}.
  -/)
  (uses := ["dr:highlighting-map-to-html-json", "dr:write-json-atomic"])]
  Dress.Serialize.writeHighlightingHtml

attribute [blueprint "dr:load-highlighting-html"
  (title := "Load Highlighting HTML")
  (statement := /--
    Loads pre-rendered HTML highlighting from a JSON map file.
    Returns a \texttt{NameMap String} (declaration name $\to$ HTML).
    Returns an empty map if the file doesn't exist or parsing fails.
  -/)
  (proof := /--
    Reads and parses JSON, then folds the object's key-value pairs
    into a \texttt{NameMap} using \texttt{key.toName}.
  -/)]
  Dress.Serialize.loadHighlightingHtml

/-! ## Serialize.Artifacts -- Full dressed artifact format -/

attribute [blueprint "dr:to-dressed-artifacts-json"
  (title := "To Dressed Artifacts JSON")
  (statement := /--
    Serializes a \texttt{NameMap Highlighted} to the full dressed artifact
    format: per declaration, produces \texttt{html}, \texttt{htmlBase64},
    and \texttt{jsonBase64} fields. This is the primary format consumed
    by leanblueprint.
  -/)
  (proof := /--
    Maps each entry through \texttt{renderHighlightedToHtml},
    base64-encodes the HTML and JSON representations, and builds
    a JSON object.
  -/)
  (uses := ["dr:highlighting-map-to-html-json"])]
  Dress.Serialize.toDressedArtifactsJson

attribute [blueprint "dr:write-dressed-artifacts"
  (title := "Write Dressed Artifacts")
  (statement := /--
    Writes all captured module dressed artifacts to a JSON file in the
    full artifact format (HTML + base64 HTML + base64 JSON per declaration).
  -/)
  (proof := /--
    Short-circuits on empty input. Delegates to \texttt{writeJsonAtomic}
    with \texttt{toDressedArtifactsJson}.
  -/)
  (uses := ["dr:to-dressed-artifacts-json", "dr:write-json-atomic"])]
  Dress.Serialize.writeDressedArtifacts

/-! # SVG Submodule -/

/-! ## Svg.Core -- SVG element types and rendering -/

attribute [blueprint "dr:escape-xml"
  (title := "Escape XML/SVG Text")
  (statement := /--
    Escapes text for safe inclusion in XML/SVG: replaces \texttt{\&},
    \texttt{<}, \texttt{>}, \texttt{"}, and \texttt{'} with their
    XML entity equivalents.
  -/)
  (proof := /--
    Sequential \texttt{String.replace} calls for each special character.
  -/)]
  Dress.Svg.escapeXml

attribute [blueprint "dr:svg-element"
  (keyDeclaration := true)
  (title := "SVG Element Type")
  (statement := /--
    Inductive type representing a composable SVG element. Three constructors:
    \texttt{node} (tag with attributes and children), \texttt{text}
    (escaped text content), and \texttt{raw} (pre-formatted content
    inserted verbatim).
  -/)
  (proof := /--
    An inductive type deriving \texttt{Inhabited}.
  -/)]
  Dress.Svg.SvgElement

attribute [blueprint "dr:svg-element-render"
  (title := "SVG Element Render")
  (statement := /--
    Renders an \texttt{SvgElement} to an indented XML string.
    Handles self-closing tags (no children), single-child inlining,
    and recursive multi-child rendering with proper indentation.
  -/)
  (proof := /--
    A \texttt{partial def} that pattern-matches on \texttt{SvgElement}
    constructors. For \texttt{node}, folds attributes into a string
    and recursively renders children with incremented indent.
  -/)
  (uses := ["dr:svg-element", "dr:escape-xml"])]
  Dress.Svg.SvgElement.render

attribute [blueprint "dr:svg-element-el"
  (title := "SVG Element Constructor")
  (statement := /--
    Convenience constructor for creating an \texttt{SvgElement.node}
    with tag, attributes, and children.
  -/)
  (proof := /--
    Wraps the \texttt{.node} constructor.
  -/)
  (uses := ["dr:svg-element"])]
  Dress.Svg.SvgElement.el

attribute [blueprint "dr:svg-element-txt"
  (title := "SVG Text Node Constructor")
  (statement := /--
    Convenience constructor for creating an \texttt{SvgElement.text} node.
  -/)
  (proof := /--
    Wraps the \texttt{.text} constructor.
  -/)
  (uses := ["dr:svg-element"])]
  Dress.Svg.SvgElement.txt

attribute [blueprint "dr:svg-document"
  (title := "SVG Document Renderer")
  (statement := /--
    Renders a complete SVG document with \texttt{xmlns}, \texttt{width},
    \texttt{height}, optional \texttt{viewBox}, and child elements.
  -/)
  (proof := /--
    Builds the SVG header string, maps children through
    \texttt{SvgElement.render} with indent 2, and assembles the
    document.
  -/)
  (uses := ["dr:svg-element", "dr:svg-element-render"])]
  Dress.Svg.svgDocument

/-! ## Svg.Style -- Visual styling -/

attribute [blueprint "dr:svg-style"
  (title := "SVG Style Structure")
  (statement := /--
    Structure holding visual style attributes for SVG elements: fill,
    stroke, stroke-width, stroke-dasharray, opacity, fill-opacity,
    font-family, font-size, font-weight, text-anchor, and
    dominant-baseline. All fields are optional.
  -/)
  (proof := /--
    A Lean structure with 11 optional fields, deriving \texttt{Inhabited}.
  -/)]
  Dress.Svg.Style

attribute [blueprint "dr:svg-style-to-attrs"
  (title := "Style to SVG Attributes")
  (statement := /--
    Converts a \texttt{Style} to an array of SVG attribute key-value pairs,
    emitting only the fields that are \texttt{some}.
  -/)
  (proof := /--
    Iteratively checks each optional field and pushes the corresponding
    attribute pair when present.
  -/)
  (uses := ["dr:svg-style"])]
  Dress.Svg.Style.toAttrs

/-! ## Svg.Shapes -- Shape primitives -/

attribute [blueprint "dr:svg-circle"
  (title := "SVG Circle")
  (statement := /--
    Creates an SVG \texttt{<circle>} element with center coordinates
    \texttt{(cx, cy)}, radius \texttt{r}, and optional style.
  -/)
  (proof := /--
    Constructs a \texttt{SvgElement.node} with tag \texttt{"circle"}
    and appropriate attributes.
  -/)
  (uses := ["dr:svg-element", "dr:svg-style-to-attrs"])]
  Dress.Svg.circle

attribute [blueprint "dr:svg-rect"
  (title := "SVG Rectangle")
  (statement := /--
    Creates an SVG \texttt{<rect>} element with position \texttt{(x, y)},
    dimensions \texttt{(width, height)}, optional corner radius \texttt{rx},
    and optional style.
  -/)
  (proof := /--
    Constructs a \texttt{SvgElement.node} with tag \texttt{"rect"}.
    Conditionally adds \texttt{rx}/\texttt{ry} attributes when
    \texttt{rx > 0}.
  -/)
  (uses := ["dr:svg-element", "dr:svg-style-to-attrs"])]
  Dress.Svg.rect

attribute [blueprint "dr:svg-line"
  (title := "SVG Line")
  (statement := /--
    Creates an SVG \texttt{<line>} element from \texttt{(x1, y1)} to
    \texttt{(x2, y2)} with optional style.
  -/)
  (proof := /--
    Constructs a \texttt{SvgElement.node} with tag \texttt{"line"}.
  -/)
  (uses := ["dr:svg-element", "dr:svg-style-to-attrs"])]
  Dress.Svg.line

attribute [blueprint "dr:svg-ellipse"
  (title := "SVG Ellipse")
  (statement := /--
    Creates an SVG \texttt{<ellipse>} element with center \texttt{(cx, cy)},
    radii \texttt{(rx, ry)}, and optional style.
  -/)
  (proof := /--
    Constructs a \texttt{SvgElement.node} with tag \texttt{"ellipse"}.
  -/)
  (uses := ["dr:svg-element", "dr:svg-style-to-attrs"])]
  Dress.Svg.ellipse

attribute [blueprint "dr:svg-polygon"
  (title := "SVG Polygon")
  (statement := /--
    Creates an SVG \texttt{<polygon>} element from an array of
    \texttt{(Float $\times$ Float)} points with optional style.
  -/)
  (proof := /--
    Formats points as a space-separated \texttt{"x,y"} string and
    constructs a \texttt{SvgElement.node} with tag \texttt{"polygon"}.
  -/)
  (uses := ["dr:svg-element", "dr:svg-style-to-attrs"])]
  Dress.Svg.polygon

attribute [blueprint "dr:svg-polyline"
  (title := "SVG Polyline")
  (statement := /--
    Creates an SVG \texttt{<polyline>} element from an array of points
    with optional style.
  -/)
  (proof := /--
    Same point formatting as \texttt{polygon} but uses tag
    \texttt{"polyline"}.
  -/)
  (uses := ["dr:svg-element", "dr:svg-polygon", "dr:svg-style-to-attrs"])]
  Dress.Svg.polyline

attribute [blueprint "dr:svg-path"
  (title := "SVG Path")
  (statement := /--
    Creates an SVG \texttt{<path>} element from a path data string
    \texttt{d} with optional style.
  -/)
  (proof := /--
    Constructs a \texttt{SvgElement.node} with tag \texttt{"path"} and
    the \texttt{d} attribute.
  -/)
  (uses := ["dr:svg-element", "dr:svg-style-to-attrs"])]
  Dress.Svg.path

attribute [blueprint "dr:svg-curve-path"
  (title := "SVG Curve Path Data")
  (statement := /--
    Builds SVG path data string for a smooth curve through sampled points.
    Starts with \texttt{M x0 y0} and appends \texttt{L x y} segments.
  -/)
  (proof := /--
    Iterates over points, building the path data string imperatively
    with \texttt{M} (move-to) for the first point and \texttt{L}
    (line-to) for subsequent points.
  -/)]
  Dress.Svg.curvePath

attribute [blueprint "dr:svg-dashed-line"
  (title := "SVG Dashed Line")
  (statement := /--
    Creates a dashed line element by augmenting the style with a
    \texttt{stroke-dasharray} pattern (default \texttt{"4,3"}).
  -/)
  (proof := /--
    Overrides the \texttt{strokeDasharray} field of the style and
    delegates to \texttt{line}.
  -/)
  (uses := ["dr:svg-line", "dr:svg-style"])]
  Dress.Svg.dashedLine

/-! ## Svg.Text -- Text elements and labels -/

attribute [blueprint "dr:svg-text"
  (title := "SVG Text Element")
  (statement := /--
    Creates an SVG \texttt{<text>} element at position \texttt{(x, y)}
    with the given content string and optional style.
  -/)
  (proof := /--
    Constructs a \texttt{SvgElement.node} with tag \texttt{"text"},
    position attributes, style, and a \texttt{SvgElement.text} child.
  -/)
  (uses := ["dr:svg-element", "dr:svg-style-to-attrs"])]
  Dress.Svg.text

attribute [blueprint "dr:svg-centered-text"
  (title := "SVG Centered Text")
  (statement := /--
    Creates a centered text element by augmenting the style with
    \texttt{text-anchor: middle} and \texttt{dominant-baseline: central}.
  -/)
  (proof := /--
    Overrides the \texttt{textAnchor} and \texttt{dominantBaseline}
    fields and delegates to \texttt{text}.
  -/)
  (uses := ["dr:svg-text", "dr:svg-style"])]
  Dress.Svg.centeredText

attribute [blueprint "dr:svg-label"
  (title := "SVG Label with Background")
  (statement := /--
    Creates a label element with a background rectangle and centered text.
    Estimates text width at approximately $0.6 \times$ font size per
    character.
  -/)
  (proof := /--
    Computes text dimensions, creates a background \texttt{rect} with
    padding and a \texttt{centeredText} element, then groups them in
    a \texttt{<g>} element.
  -/)
  (uses := ["dr:svg-rect", "dr:svg-centered-text", "dr:svg-element"])]
  Dress.Svg.label

/-! ## Svg.Transform -- Group transformations -/

attribute [blueprint "dr:svg-translate"
  (title := "SVG Translate Transform")
  (statement := /--
    Wraps child elements in a \texttt{<g>} with a
    \texttt{translate(dx, dy)} transform.
  -/)
  (proof := /--
    Constructs a \texttt{SvgElement.node} with tag \texttt{"g"} and
    a \texttt{transform} attribute.
  -/)
  (uses := ["dr:svg-element"])]
  Dress.Svg.translate

attribute [blueprint "dr:svg-rotate"
  (title := "SVG Rotate Transform")
  (statement := /--
    Wraps child elements in a \texttt{<g>} with a
    \texttt{rotate(angle, cx, cy)} transform.
  -/)
  (proof := /--
    Constructs a group with the rotation transform attribute.
    Center defaults to origin.
  -/)
  (uses := ["dr:svg-element"])]
  Dress.Svg.rotate

attribute [blueprint "dr:svg-scale"
  (title := "SVG Scale Transform")
  (statement := /--
    Wraps child elements in a \texttt{<g>} with a
    \texttt{scale(sx, sy)} transform. If \texttt{sy} is not provided,
    uses \texttt{sx} for uniform scaling.
  -/)
  (proof := /--
    Constructs a group with the scale transform attribute.
  -/)
  (uses := ["dr:svg-element"])]
  Dress.Svg.scale

attribute [blueprint "dr:svg-group"
  (title := "SVG Group")
  (statement := /--
    Wraps child elements in a plain \texttt{<g>} element with optional
    attributes.
  -/)
  (proof := /--
    Constructs a \texttt{SvgElement.node} with tag \texttt{"g"}.
  -/)
  (uses := ["dr:svg-element"])]
  Dress.Svg.group

attribute [blueprint "dr:svg-with-class"
  (title := "SVG Group with CSS Class")
  (statement := /--
    Wraps child elements in a \texttt{<g>} element with a
    \texttt{class} attribute for CSS styling.
  -/)
  (proof := /--
    Constructs a group with a single \texttt{("class", cls)} attribute.
  -/)
  (uses := ["dr:svg-element"])]
  Dress.Svg.withClass

/-! ## Svg.Coordinate -- Coordinate system and plotting -/

attribute [blueprint "dr:coord-system"
  (keyDeclaration := true)
  (title := "Coordinate System")
  (statement := /--
    Maps mathematical coordinates to SVG pixel coordinates. Defines
    the mathematical range (\texttt{xMin..xMax}, \texttt{yMin..yMax}),
    SVG pixel dimensions, and padding around the plot area.
  -/)
  (proof := /--
    A Lean structure with 7 fields (6 \texttt{Float} + 1 defaulting
    padding), deriving \texttt{Inhabited}.
  -/)]
  Dress.Svg.CoordSystem

attribute [blueprint "dr:coord-plot-width"
  (title := "Plot Width")
  (statement := /--
    Computes the plot area width in pixels:
    \texttt{svgWidth} $- 2 \times$ \texttt{padding}.
  -/)
  (proof := /--
    Simple arithmetic on structure fields.
  -/)
  (uses := ["dr:coord-system"])]
  Dress.Svg.CoordSystem.plotWidth

attribute [blueprint "dr:coord-plot-height"
  (title := "Plot Height")
  (statement := /--
    Computes the plot area height in pixels:
    \texttt{svgHeight} $- 2 \times$ \texttt{padding}.
  -/)
  (proof := /--
    Simple arithmetic on structure fields.
  -/)
  (uses := ["dr:coord-system"])]
  Dress.Svg.CoordSystem.plotHeight

attribute [blueprint "dr:coord-to-svg-x"
  (title := "Math X to SVG X")
  (statement := /--
    Converts a mathematical $x$-coordinate to an SVG pixel $x$-coordinate:
    $\texttt{padding} + \frac{x - x_{\min}}{x_{\max} - x_{\min}} \times \texttt{plotWidth}$.
  -/)
  (proof := /--
    Linear interpolation from the math range to the pixel range.
  -/)
  (uses := ["dr:coord-system", "dr:coord-plot-width"])]
  Dress.Svg.CoordSystem.toSvgX

attribute [blueprint "dr:coord-to-svg-y"
  (title := "Math Y to SVG Y")
  (statement := /--
    Converts a mathematical $y$-coordinate to an SVG pixel $y$-coordinate
    with vertical flip (math Y goes up, SVG Y goes down):
    $\texttt{padding} + \frac{y_{\max} - y}{y_{\max} - y_{\min}} \times \texttt{plotHeight}$.
  -/)
  (proof := /--
    Linear interpolation with inversion for the Y axis.
  -/)
  (uses := ["dr:coord-system", "dr:coord-plot-height"])]
  Dress.Svg.CoordSystem.toSvgY

attribute [blueprint "dr:coord-to-svg"
  (title := "Math Point to SVG Point")
  (statement := /--
    Converts a mathematical point $(x, y)$ to SVG pixel coordinates
    by applying \texttt{toSvgX} and \texttt{toSvgY}.
  -/)
  (proof := /--
    Pairs the results of \texttt{toSvgX} and \texttt{toSvgY}.
  -/)
  (uses := ["dr:coord-to-svg-x", "dr:coord-to-svg-y"])]
  Dress.Svg.CoordSystem.toSvg

attribute [blueprint "dr:coord-x-axis"
  (title := "X Axis Generator")
  (statement := /--
    Generates an SVG line element for the $x$-axis at $y = 0$
    in math coordinates, spanning the full plot width.
  -/)
  (proof := /--
    Computes the SVG $y$-coordinate of $y=0$ via \texttt{toSvgY} and
    draws a horizontal line.
  -/)
  (uses := ["dr:coord-system", "dr:coord-to-svg-y", "dr:svg-line"])]
  Dress.Svg.CoordSystem.xAxis

attribute [blueprint "dr:coord-y-axis"
  (title := "Y Axis Generator")
  (statement := /--
    Generates an SVG line element for the $y$-axis at $x = 0$
    in math coordinates, spanning the full plot height.
  -/)
  (proof := /--
    Computes the SVG $x$-coordinate of $x=0$ via \texttt{toSvgX} and
    draws a vertical line.
  -/)
  (uses := ["dr:coord-system", "dr:coord-to-svg-x", "dr:svg-line"])]
  Dress.Svg.CoordSystem.yAxis

attribute [blueprint "dr:coord-axes"
  (title := "Both Axes")
  (statement := /--
    Generates both the $x$-axis and $y$-axis as an array of two
    \texttt{SvgElement} values.
  -/)
  (proof := /--
    Returns \texttt{\#[cs.xAxis, cs.yAxis]}.
  -/)
  (uses := ["dr:coord-x-axis", "dr:coord-y-axis"])]
  Dress.Svg.CoordSystem.axes

attribute [blueprint "dr:coord-x-ticks"
  (title := "X Axis Tick Marks")
  (statement := /--
    Generates tick marks and labels on the $x$-axis for given values.
    Skips the label at $x = 0$ to avoid overlap with the $y$-axis.
  -/)
  (proof := /--
    Iterates over values, computing SVG coordinates and emitting
    vertical tick lines and text labels.
  -/)
  (uses := ["dr:coord-system", "dr:coord-to-svg-x", "dr:coord-to-svg-y",
            "dr:svg-line", "dr:svg-text"])]
  Dress.Svg.CoordSystem.xTicks

attribute [blueprint "dr:coord-y-ticks"
  (title := "Y Axis Tick Marks")
  (statement := /--
    Generates tick marks and labels on the $y$-axis for given values.
    Skips the label at $y = 0$ to avoid overlap with the $x$-axis.
  -/)
  (proof := /--
    Iterates over values, computing SVG coordinates and emitting
    horizontal tick lines and text labels.
  -/)
  (uses := ["dr:coord-system", "dr:coord-to-svg-x", "dr:coord-to-svg-y",
            "dr:svg-line", "dr:svg-text"])]
  Dress.Svg.CoordSystem.yTicks

attribute [blueprint "dr:coord-grid"
  (title := "Background Grid")
  (statement := /--
    Generates a background grid of light lines at specified $x$ and $y$
    values, providing visual reference for the plot area.
  -/)
  (proof := /--
    Iterates over $x$ and $y$ values, drawing vertical and horizontal
    lines spanning the full plot area.
  -/)
  (uses := ["dr:coord-system", "dr:coord-to-svg-x", "dr:coord-to-svg-y", "dr:svg-line"])]
  Dress.Svg.CoordSystem.grid

attribute [blueprint "dr:coord-plot-function"
  (title := "Plot Function")
  (statement := /--
    Plots a function $f : \mathbb{R} \to \mathbb{R}$ by sampling
    \texttt{numSamples} points across the $x$-range and connecting
    them with line segments via an SVG path.
  -/)
  (proof := /--
    Samples the function at evenly spaced $x$ values, converts each
    $(x, f(x))$ to SVG coordinates, then builds a path from the
    curve data.
  -/)
  (uses := ["dr:coord-system", "dr:coord-to-svg-x", "dr:coord-to-svg-y",
            "dr:svg-path", "dr:svg-curve-path"])]
  Dress.Svg.CoordSystem.plotFunction

end DressBlueprint.Annotations.CaptureRenderSvg

#dressNodes
