/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Highlighting
import Dress.Capture.InfoTree
import Dress.Serialize.Json
import Dress.Serialize.Html
import Dress.Paths

/-!
# Module-Level Output Generation

This module provides path helpers for module-level JSON output.
Per-declaration artifacts (.tex, .html, .json) are now written during elaboration
by `Generate.Declaration`. Lake facets aggregate these artifacts.

## Path Helpers

Provided for backward compatibility with consumers that need module paths:
- `getSubVersoOutputPath` - Path for SubVerso-compatible JSON
- `getHighlightingHtmlOutputPath` - Path for HTML map output
-/

open Lean Elab Command
open SubVerso.Highlighting

namespace Dress.Generate

/-- Get the output path for a module's SubVerso-compatible JSON file.
    Returns `.lake/build/dressed/{Module/Path}.subverso.json` -/
def getSubVersoOutputPath (buildDir : System.FilePath) (moduleName : Name) : System.FilePath :=
  let modulePath := moduleName.components.foldl (init := buildDir / "dressed")
    fun path component => path / component.toString
  modulePath.withExtension "subverso.json"

/-- Get the output path for a module's HTML JSON file.
    Returns `.lake/build/dressed/{Module/Path}.html.json` -/
def getHighlightingHtmlOutputPath (buildDir : System.FilePath) (moduleName : Name) : System.FilePath :=
  let modulePath := moduleName.components.foldl (init := buildDir / "dressed")
    fun path component => path / component.toString
  modulePath.withExtension "html.json"

end Dress.Generate
