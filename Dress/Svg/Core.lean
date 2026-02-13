/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

/-!
# SVG Core Types

Core types and rendering for composable SVG generation.
-/

namespace Dress.Svg

/-- Escape text for XML/SVG -/
def escapeXml (s : String) : String :=
  s.replace "&" "&amp;"
   |>.replace "<" "&lt;"
   |>.replace ">" "&gt;"
   |>.replace "\"" "&quot;"
   |>.replace "'" "&apos;"

/-- An SVG element with tag, attributes, children, and optional text content -/
inductive SvgElement where
  | node (tag : String) (attrs : Array (String × String)) (children : Array SvgElement)
  | text (content : String)
  | raw (content : String)
  deriving Inhabited

namespace SvgElement

/-- Render an SVG element to string -/
partial def render (indent : Nat := 0) : SvgElement → String
  | .text content => escapeXml content
  | .raw content => content
  | .node tag attrs children =>
    let pad := "".pushn ' ' indent
    let attrStr := attrs.foldl (fun acc (k, v) => acc ++ s!" {k}=\"{v}\"") ""
    if children.isEmpty then
      s!"{pad}<{tag}{attrStr}/>"
    else if children.size == 1 then
      match children[0]! with
      | .text content => s!"{pad}<{tag}{attrStr}>{escapeXml content}</{tag}>"
      | .raw content => s!"{pad}<{tag}{attrStr}>{content}</{tag}>"
      | child => s!"{pad}<{tag}{attrStr}>\n{child.render (indent + 2)}\n{pad}</{tag}>"
    else
      let inner := children.map (·.render (indent + 2)) |>.toList |> "\n".intercalate
      s!"{pad}<{tag}{attrStr}>\n{inner}\n{pad}</{tag}>"

/-- Create an element node -/
def el (tag : String) (attrs : Array (String × String) := #[]) (children : Array SvgElement := #[]) : SvgElement :=
  .node tag attrs children

/-- Create a text node -/
def txt (s : String) : SvgElement := .text s

end SvgElement

/-- Render a complete SVG document -/
def svgDocument (width height : Float) (viewBox : Option (Float × Float × Float × Float) := none)
    (children : Array SvgElement) : String :=
  let vb := match viewBox with
    | some (x, y, w, h) => s!" viewBox=\"{x} {y} {w} {h}\""
    | none => ""
  let header := s!"<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{width}\" height=\"{height}\"{vb}>"
  let inner := children.map (·.render 2) |>.toList |> "\n".intercalate
  s!"{header}\n{inner}\n</svg>"

end Dress.Svg
