/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress.Extract.Delimiter
import Dress.Extract.Integrate

/-!
# TeX Extraction from Lean Sources

Module root for the extraction subsystem. Re-exports:
- `Dress.Extract.Delimiter`: `extractDelimiters` for scanning `/-%%...%%-/` and `--%%` patterns
- `Dress.Extract.Integrate`: `matchDelimitersToDeclarations` and `enrichNodeWithDelimiter`
  for wiring extracted delimiters into the artifact pipeline
-/
