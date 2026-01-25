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
├── module.json              # Module-level metadata
├── module.tex               # Module-level LaTeX header
└── {sanitized-label}/       # Per-declaration artifacts
    ├── decl.tex             # Declaration LaTeX
    ├── decl.html            # Declaration HTML
    └── decl.json            # Declaration JSON
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

/-- Get the path for a module's JSON metadata file.
    Returns `.lake/build/dressed/{Module/Path}/module.json` -/
def getModuleJsonPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  getModuleDressedDir buildDir moduleName / "module.json"

/-- Get the path for a module's LaTeX header file.
    Returns `.lake/build/dressed/{Module/Path}/module.tex` -/
def getModuleTexPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  getModuleDressedDir buildDir moduleName / "module.tex"

/-- Sanitize a label for use as a directory/filename.
    Replaces `:` with `-` for filesystem compatibility. -/
def sanitizeLabel (label : String) : String :=
  label.replace ":" "-"

/-- Get the directory for a declaration's artifacts.
    Returns `.lake/build/dressed/{Module/Path}/{sanitized-label}/` -/
def getDeclarationDir (buildDir : System.FilePath) (moduleName : Lean.Name) (label : String) : System.FilePath :=
  getModuleDressedDir buildDir moduleName / sanitizeLabel label

/-- Get the path for a declaration's .tex file.
    Returns `.lake/build/dressed/{Module/Path}/{sanitized-label}/decl.tex` -/
def getDeclarationTexPath (buildDir : System.FilePath) (moduleName : Lean.Name) (label : String) : System.FilePath :=
  getDeclarationDir buildDir moduleName label / "decl.tex"

/-- Get the path for a declaration's .html file.
    Returns `.lake/build/dressed/{Module/Path}/{sanitized-label}/decl.html` -/
def getDeclarationHtmlPath (buildDir : System.FilePath) (moduleName : Lean.Name) (label : String) : System.FilePath :=
  getDeclarationDir buildDir moduleName label / "decl.html"

/-- Get the path for a declaration's .json file.
    Returns `.lake/build/dressed/{Module/Path}/{sanitized-label}/decl.json` -/
def getDeclarationJsonPath (buildDir : System.FilePath) (moduleName : Lean.Name) (label : String) : System.FilePath :=
  getDeclarationDir buildDir moduleName label / "decl.json"

/-- Get the path for a declaration's hover data JSON file.
    Returns `.lake/build/dressed/{Module/Path}/{sanitized-label}/decl.hovers.json` -/
def getDeclarationHoversPath (buildDir : System.FilePath) (moduleName : Lean.Name) (label : String) : System.FilePath :=
  getDeclarationDir buildDir moduleName label / "decl.hovers.json"

/-- Get the declaration directory path relative to blueprint/src/ for LaTeX \input.
    Returns `../../.lake/build/dressed/{Module/Path}/{sanitized-label}`

    The `../../` prefix accounts for plastex running from `blueprint/` with
    tex files in `blueprint/src/`. -/
def getDeclarationDirForLatex (moduleName : Lean.Name) (label : String) : String :=
  let modulePathComponents := moduleName.components.map (·.toString)
  "../../.lake/build/dressed/" ++ "/".intercalate modulePathComponents ++ "/" ++ sanitizeLabel label

/-- Get the module dressed directory path relative to blueprint/src/ for LaTeX \input.
    Returns `../../.lake/build/dressed/{Module/Path}`

    The `../../` prefix accounts for plastex running from `blueprint/` with
    tex files in `blueprint/src/`.

    This is the module directory containing declaration subdirs; for specific declarations use `getDeclarationDirForLatex`. -/
def getModuleDirForLatex (moduleName : Lean.Name) : String :=
  let modulePathComponents := moduleName.components.map (·.toString)
  "../../.lake/build/dressed/" ++ "/".intercalate modulePathComponents

end Dress.Paths
