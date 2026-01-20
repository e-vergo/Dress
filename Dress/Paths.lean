/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Centralized Path Management

This module provides all path construction functions for Dress artifacts.
Centralizing paths here prevents duplication and ensures consistency.

## Output Directories

- `.lake/build/dressed/` - Dressed artifacts (highlighting JSON)
- `.lake/build/blueprint/module/` - LaTeX artifacts (.tex files)
- `.lake/build/blueprint/module/*.artifacts/` - Per-declaration .tex files
-/

namespace Dress.Paths

/-- Build a module path from a base directory and module name.
    Components are joined as subdirectories. -/
def buildModulePath (baseDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  moduleName.components.foldl (init := baseDir)
    fun path component => path / component.toString

/-- Get the output path for a module's dressed JSON file.
    Returns `.lake/build/dressed/{Module/Path}.json`

    This file contains the Dress extended format with:
    - `html`: Pre-rendered HTML string
    - `htmlBase64`: Base64-encoded HTML for TeX embedding
    - `jsonBase64`: Base64-encoded SubVerso JSON for compatibility -/
def getDressedOutputPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  (buildModulePath (buildDir / "dressed") moduleName).withExtension "json"

/-- Get the output path for a module's SubVerso-compatible JSON file.
    Returns `.lake/build/dressed/{Module/Path}.subverso.json`

    This file contains the standard SubVerso Module format for
    compatibility with other SubVerso-aware tools. -/
def getSubVersoOutputPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  (buildModulePath (buildDir / "dressed") moduleName).withExtension "subverso.json"

/-- Get the output path for a module's HTML map file.
    Returns `.lake/build/dressed/{Module/Path}.html.json` -/
def getHtmlMapOutputPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  (buildModulePath (buildDir / "dressed") moduleName).withExtension "html.json"

/-- Get the output path for a module's LaTeX header file.
    Returns `.lake/build/blueprint/module/{Module/Path}.tex` -/
def getModuleTexPath (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  (buildModulePath (buildDir / "blueprint" / "module") moduleName).withExtension "tex"

/-- Get the artifacts directory for a module's per-declaration .tex files.
    Returns `.lake/build/blueprint/module/{Module/Path}.artifacts/` -/
def getModuleArtifactsDir (buildDir : System.FilePath) (moduleName : Lean.Name) : System.FilePath :=
  (buildModulePath (buildDir / "blueprint" / "module") moduleName).withExtension "artifacts"

/-- Get the path for a declaration's .tex file within a module's artifacts directory.
    Returns `.lake/build/blueprint/module/{Module/Path}.artifacts/{label}.tex`

    The label is sanitized (`:` → `-`) for filesystem compatibility. -/
def getDeclTexPath (buildDir : System.FilePath) (moduleName : Lean.Name) (label : String) : System.FilePath :=
  let sanitizedLabel := label.replace ":" "-"
  getModuleArtifactsDir buildDir moduleName / (sanitizedLabel ++ ".tex")

/-- Get the artifacts directory path relative to blueprint/src/ for LaTeX \input.
    Returns `../../.lake/build/blueprint/module/{Module/Path}.artifacts`

    The `../../` prefix accounts for plastex running from `blueprint/` with
    tex files in `blueprint/src/`. -/
def getArtifactsDirForLatex (moduleName : Lean.Name) : String :=
  let modulePathComponents := moduleName.components.map (·.toString)
  "../../.lake/build/blueprint/module/" ++ "/".intercalate modulePathComponents ++ ".artifacts"

end Dress.Paths
