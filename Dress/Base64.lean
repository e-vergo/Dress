/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

/-!
# Base64 Encoding

RFC 4648 Base64 encoding for embedding binary data in LaTeX and JSON.

Used to embed:
- Pre-rendered HTML in LaTeX macros (`\leansourcehtml{base64}`)
- SubVerso JSON for compatibility
-/

namespace Dress.Base64

/-- Base64 encoding alphabet (RFC 4648). -/
private def base64Chars : String :=
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

/-- Array of base64 characters for efficient indexing. -/
private def base64Array : Array Char := base64Chars.toList.toArray

/-- Encode a ByteArray to base64 string.

    Processes input in 3-byte groups, producing 4 base64 characters each.
    Pads output with `=` characters as needed. -/
def encode (data : ByteArray) : String := Id.run do
  let mut result := ""
  let mut i := 0
  while i < data.size do
    let b0 := data.get! i
    let b1 := if i + 1 < data.size then data.get! (i + 1) else 0
    let b2 := if i + 2 < data.size then data.get! (i + 2) else 0

    let c0 := (b0 >>> 2) &&& 0x3F
    let c1 := ((b0 &&& 0x03) <<< 4) ||| ((b1 >>> 4) &&& 0x0F)
    let c2 := ((b1 &&& 0x0F) <<< 2) ||| ((b2 >>> 6) &&& 0x03)
    let c3 := b2 &&& 0x3F

    result := result.push (base64Array[c0.toNat]!)
    result := result.push (base64Array[c1.toNat]!)
    if i + 1 < data.size then
      result := result.push (base64Array[c2.toNat]!)
    else
      result := result.push '='
    if i + 2 < data.size then
      result := result.push (base64Array[c3.toNat]!)
    else
      result := result.push '='
    i := i + 3
  return result

/-- Encode a String to base64 (UTF-8 encoded). -/
def encodeString (s : String) : String :=
  encode s.toUTF8

end Dress.Base64

-- Re-export common functions at Dress namespace level
namespace Dress

/-- Encode a ByteArray to base64. -/
abbrev encodeBase64 := Base64.encode

/-- Encode a String to base64 (UTF-8 encoded). -/
abbrev stringToBase64 := Base64.encodeString

end Dress
