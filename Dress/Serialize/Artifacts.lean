/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import SubVerso.Highlighting
import Dress.Base64
import Dress.HtmlRender
import Dress.Serialize.Json

/-!
# Dressed Artifacts Serialization

This module provides the full dressed artifact format serialization.
This is the primary format consumed by leanblueprint.

## Output Format

Per declaration:
```json
{
  "html": "<pre>...</pre>",
  "htmlBase64": "PHByZT4uLi48L3ByZT4=",
  "jsonBase64": "eyJoaWdobGlnaHRlZCI6Li4ufQ=="
}
```
-/

open Lean
open SubVerso.Highlighting

namespace Dress.Serialize

/-- Serialize a NameMap of Highlighted values to the full dressed artifact format.
    Includes HTML, base64-encoded HTML, and base64-encoded JSON for fast TeX generation.

    Format per declaration:
    ```json
    {
      "html": "<pre>...</pre>",
      "htmlBase64": "PHByZT4uLi48L3ByZT4=",
      "jsonBase64": "eyJoaWdobGlnaHRlZCI6Li4ufQ=="
    }
    ```
-/
def toDressedArtifactsJson (highlighting : NameMap Highlighted) : Json :=
  let entries : List (String × Json) := highlighting.toList.map fun (name, hl) =>
    let html := HtmlRender.renderHighlightedToHtml hl
    let jsonStr := (toJson hl).compress
    let artifact := Json.mkObj [
      ("html", Json.str html),
      ("htmlBase64", Json.str (Base64.encodeString html)),
      ("jsonBase64", Json.str (Base64.encodeString jsonStr))
    ]
    (name.toString, artifact)
  Json.mkObj entries

/-- Write all captured module dressed artifacts to a JSON file. -/
def writeDressedArtifacts (path : System.FilePath) (highlighting : NameMap Highlighted) : IO Unit := do
  if highlighting.isEmpty then return
  writeJsonAtomic path (toDressedArtifactsJson highlighting)

end Dress.Serialize

-- Re-export at Dress namespace for backward compatibility
namespace Dress

abbrev serializeDressedArtifacts := Serialize.toDressedArtifactsJson
abbrev writeModuleDressedArtifacts (buildDir : System.FilePath) (moduleName : Name)
    (highlighting : NameMap Highlighted) : IO Unit := do
  if highlighting.isEmpty then return
  let modulePath := moduleName.components.foldl (init := buildDir / "dressed")
    fun path component => path / component.toString
  let path := modulePath.withExtension "json"
  Serialize.writeDressedArtifacts path highlighting

end Dress
