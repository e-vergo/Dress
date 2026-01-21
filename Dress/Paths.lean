/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Centralized Path Management

This module provides all path construction functions for Dress artifacts.
Centralizing paths here prevents duplication and ensures consistency.

## Output Directory Structure

All artifacts live under `.lake/build/dressed/`:

```
.lake/build/dressed/{Module/Path}/
├── module.json          # Module-level metadata
├── module.tex           # Module-level LaTeX header
└── artifacts/           # Per-declaration artifacts
    ├── {label}.tex      # Declaration LaTeX
    ├── {label}.html     # Declaration HTML
    └── {label}.json     # Declaration JSON
```
-/

namespace Dress.Paths

/-- Build a module path from a base directory and module name.
    Components are joined as subdirectories. -/
def buildModulePath (baseDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  moduleName.components.foldl (init := baseDir)
    fun path component => path / component.toString

/-- Get the dressed directory for a module.
    Returns `.lake/build/dressed/{Module/Path}/` -/
def getModuleDressedDir (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  buildModulePath (buildDir / "dressed") moduleName

/-- Get the artifacts directory for a module's per-declaration files.
    Returns `.lake/build/dressed/{Module/Path}/artifacts/` -/
def getModuleArtifactsDir (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  getModuleDressedDir buildDir moduleName / "artifacts"

/-- Get the path for a module's JSON metadata file.
    Returns `.lake/build/dressed/{Module/Path}/module.json` -/
def getModuleJsonPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  getModuleDressedDir buildDir moduleName / "module.json"

/-- Get the path for a module's LaTeX header file.
    Returns `.lake/build/dressed/{Module/Path}/module.tex` -/
def getModuleTexPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  getModuleDressedDir buildDir moduleName / "module.tex"

/-- Sanitize a label for use as a filename.
    Replaces `:` with `-` for filesystem compatibility. -/
def sanitizeLabel (label : String) : String :=
  label.replace ":" "-"

/-- Get the path for a declaration's .tex file.
    Returns `.lake/build/dressed/{Module/Path}/artifacts/{label}.tex` -/
def getDeclarationTexPath (buildDir : System.FilePath) (moduleName : Lean.Name) (label : String) : System.FilePath :=
  getModuleArtifactsDir buildDir moduleName / (sanitizeLabel label ++ ".tex")

/-- Get the path for a declaration's .html file.
    Returns `.lake/build/dressed/{Module/Path}/artifacts/{label}.html` -/
def getDeclarationHtmlPath (buildDir : System.FilePath) (moduleName : Lean.Name) (label : String) : System.FilePath :=
  getModuleArtifactsDir buildDir moduleName / (sanitizeLabel label ++ ".html")

/-- Get the path for a declaration's .json file.
    Returns `.lake/build/dressed/{Module/Path}/artifacts/{label}.json` -/
def getDeclarationJsonPath (buildDir : System.FilePath) (moduleName : Lean.Name) (label : String) : System.FilePath :=
  getModuleArtifactsDir buildDir moduleName / (sanitizeLabel label ++ ".json")

/-- Get the artifacts directory path relative to blueprint/src/ for LaTeX \input.
    Returns `../../.lake/build/dressed/{Module/Path}/artifacts`

    The `../../` prefix accounts for plastex running from `blueprint/` with
    tex files in `blueprint/src/`. -/
def getArtifactsDirForLatex (moduleName : Lean.Name) : String :=
  let modulePathComponents := moduleName.components.map (·.toString)
  "../../.lake/build/dressed/" ++ "/".intercalate modulePathComponents ++ "/artifacts"

end Dress.Paths
