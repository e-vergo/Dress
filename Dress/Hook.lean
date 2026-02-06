/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Batteries.Lean.NameMapAttribute
import SubVerso.Highlighting
import SubVerso.Module
import Architect.Basic
import Dress.Core
import Dress.Highlighting
import Dress.HtmlRender
import Dress.Base64
import Dress.Capture.State
import Dress.Capture.InfoTree
import Dress.Capture.Config
import Dress.Capture.ElabRules
import Dress.Serialize.Json
import Dress.Serialize.Html
import Dress.Serialize.Artifacts
import Dress.Generate.Latex
import Dress.Generate.Module

/-!
# Elaboration-Time "Dressed" Artifact Generation

This module provides the core infrastructure for capturing rich metadata ("dressed" artifacts)
DURING Lean elaboration. Info trees (required for semantic highlighting) are ephemeral -
they exist in `commandState.infoState.trees` during elaboration but are discarded afterward.
This module captures this data while it's available.

## "Dressed" Artifacts

"Dressed" code = bare Lean source + rich metadata captured during elaboration:
- Semantic highlighting (SubVerso)
- Pre-rendered HTML (Verso)
- Type signatures
- Source positions
- Pre-computed base64 strings ready for TeX embedding

## Architecture

Artifacts are generated per-declaration during elaboration:
1. `@[blueprint]` declarations are intercepted by `Capture.ElabRules`
2. Highlighting is captured from info trees by `Capture.InfoTree`
3. Per-declaration artifacts are written by `Generate.Declaration`:
   - `.lake/build/dressed/{Module/Path}/artifacts/{label}.tex`
   - `.lake/build/dressed/{Module/Path}/artifacts/{label}.html`
   - `.lake/build/dressed/{Module/Path}/artifacts/{label}.json`
4. Lake facets aggregate these artifacts for downstream consumers

## Module Organization

This module re-exports functionality from the following submodules:
- `Capture.State`: IO.Ref state management
- `Capture.InfoTree`: Environment extension and info tree capture
- `Capture.Config`: Blueprint configuration parsing
- `Capture.ElabRules`: Elaboration rules for @[blueprint] declarations
- `Serialize.Json`: JSON serialization
- `Serialize.Html`: HTML serialization
- `Serialize.Artifacts`: Full dressed artifact format
- `Generate.Latex`: LaTeX generation for individual declarations
- `Generate.Declaration`: Per-declaration artifact writing

## Usage

Simply use `@[blueprint]` attributes on your declarations:

```lean
import Mathlib.Algebra.Group.Basic
import Dress

@[blueprint]
theorem my_theorem : ... := by
  ...
```

Run `lake build` to generate dressed artifacts. Artifact writing is automatic
for all `@[blueprint]` declarations -- no special environment variables or commands needed.
-/

open Lean Elab Command Term Meta
open SubVerso.Highlighting
open SubVerso.Module

namespace Dress

/-! ## Path Helpers (for backward compatibility) -/

/-- Get the output path for a module's SubVerso-compatible JSON file.
    Returns `.lake/build/dressed/{Module/Path}.subverso.json`

    This is separate from the Dress extended format (.json) to avoid overwriting. -/
def getHighlightingOutputPath (buildDir : System.FilePath) (moduleName : Name) : System.FilePath :=
  Generate.getSubVersoOutputPath buildDir moduleName

/-! ## Additional Serialization Helpers -/

/-- Write all captured module highlighting to a single JSON file.
    The file is written to `.lake/build/dressed/{Module/Path}.subverso.json`. -/
def writeModuleHighlightingJson (buildDir : System.FilePath) (moduleName : Name)
    (highlighting : NameMap Highlighted) : IO Unit := do
  if highlighting.isEmpty then return
  let path := getHighlightingOutputPath buildDir moduleName
  Serialize.writeJsonAtomic path (Serialize.highlightingMapToModuleJson highlighting)

/-- Write all captured module highlighting as HTML to a JSON map file.
    The file is written to `.lake/build/dressed/{Module/Path}.html.json`. -/
def writeModuleHighlightingHtml (buildDir : System.FilePath) (moduleName : Name)
    (highlighting : NameMap Highlighted) : IO Unit := do
  if highlighting.isEmpty then return
  let path := Generate.getHighlightingHtmlOutputPath buildDir moduleName
  Serialize.writeHighlightingHtml path highlighting

/-! ## Loading Captured Highlighting -/

/-- Load highlighting for a specific module from the build cache.
    Looks for `.lake/build/dressed/{Module/Path}.subverso.json`. -/
def loadModuleHighlighting (buildDir : System.FilePath) (moduleName : Name)
    : IO (NameMap Highlighted) := do
  let path := getHighlightingOutputPath buildDir moduleName
  Serialize.loadModuleJson path

/-- Load pre-rendered HTML highlighting for a specific module from the build cache.
    Looks for `.lake/build/dressed/{Module/Path}.html.json`. -/
def loadModuleHighlightingHtml (buildDir : System.FilePath) (moduleName : Name)
    : IO (NameMap String) := do
  let path := Generate.getHighlightingHtmlOutputPath buildDir moduleName
  Serialize.loadHighlightingHtml path

/-- Write all captured highlighting for a declaration to a JSON file.
    The file is written to `.lake/build/dressed/{Module/Path}/{DeclName}.json`. -/
def writeHighlightingJson (buildDir : System.FilePath) (moduleName : Name)
    (declName : Name) (hl : Highlighted) : IO Unit := do
  let moduleDir := moduleName.components.foldl (init := buildDir / "dressed")
    fun path component => path / component.toString
  let path := moduleDir / s!"{declName}.json"
  Serialize.writeJsonAtomic path (Serialize.highlightedToJson hl)

end Dress
