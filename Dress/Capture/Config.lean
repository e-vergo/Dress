/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Blueprint Config Parsing

This module parses `@[blueprint]` attribute syntax to extract configuration
needed for .tex file generation during elaboration.

This is a simplified version of the full Config in Architect/Attribute.lean,
containing only the fields needed during Dress elaboration.
-/

open Lean Elab Command

namespace Dress.Capture

/-- Blueprint configuration parsed during elaboration.
    This is a subset of the full Config in Attribute.lean, containing only
    the fields needed for .tex file generation during elaboration. -/
structure BlueprintConfig where
  /-- The LaTeX label to use for the node. -/
  latexLabel : Option String := none
  /-- The statement of the node in text. -/
  statement : Option String := none
  /-- The proof of the node in text. -/
  proof : Option String := none
  /-- Additional LaTeX labels of nodes that this node depends on. -/
  usesLabels : Array String := #[]
  /-- Additional LaTeX labels of nodes that the proof depends on. -/
  proofUsesLabels : Array String := #[]
  /-- The LaTeX environment to use for the node. -/
  latexEnv : Option String := none
  /-- Enable debugging. -/
  trace : Bool := false
deriving Repr, Inhabited

/-- Check if declModifiers contains a @[blueprint] attribute.

    The @[blueprint] attribute is parsed by `Lean.Parser.Attr.simple`, which has structure:
    - attrInst[0] = attrKind (scoped/local/global)
    - attrInst[1] = the attribute syntax with kind `Lean.Parser.Attr.simple`
    - attrInst[1][0] = the identifier (e.g., "blueprint")
    - attrInst[1][1] = optional priority/arguments

    We check if the identifier name is "blueprint". -/
def hasBlueprintAttr (mods : Syntax) : Bool :=
  -- declModifiers[1] is the optional attributes
  let attrs? := mods[1]?
  match attrs? with
  | none => false
  | some attrs =>
    if attrs.isNone then false
    else
      -- attrs is `Lean.Parser.Term.attributes` node: @[ attrInstance,* ]
      -- The actual content is in attrs[0] which is the @[...] node
      let attrsNode := attrs[0]!
      -- attrsNode[1] is the SepArray of attrInstance
      let attrInstances := attrsNode[1]!
      attrInstances.getArgs.any fun attrInst =>
        -- attrInst structure:
        --   attrInst[0] = attrKind (scoped/local/global)
        --   attrInst[1] = the actual attribute (kind = Lean.Parser.Attr.simple for simple attributes)
        -- For Attr.simple, attrInst[1][0] is the identifier
        let attrSyntax? := attrInst[1]?
        match attrSyntax? with
        | none => false
        | some attrSyntax =>
          -- Check if it's a simple attribute with name "blueprint"
          if attrSyntax.getKind == `Lean.Parser.Attr.simple then
            -- attrSyntax[0] is the identifier
            attrSyntax[0]?.map (·.getId == `blueprint) |>.getD false
          else
            -- For custom attribute syntax kinds (like Architect.blueprint)
            attrSyntax.getKind == `Architect.blueprint || attrSyntax.getKind == `blueprint


/-- Extract the @[blueprint ...] attribute syntax from declModifiers.

    Returns the full attribute syntax node (the `attrInst[1]` part) which has
    kind `Architect.blueprint` and contains the blueprint options.

    The syntax structure is:
    - declModifiers = docComment? attributes? visibility? ...
    - attributes = @[ attrInstance,* ]
    - attrInstance[0] = attrKind (scoped/local/global)
    - attrInstance[1] = the actual attribute (blueprint "?"? blueprintOptions)
-/
def extractBlueprintAttrSyntax (mods : Syntax) : Option Syntax :=
  let attrs? := mods[1]?
  match attrs? with
  | none => none
  | some attrs =>
    if attrs.isNone then none
    else
      let attrsNode := attrs[0]!
      let attrInstances := attrsNode[1]!
      attrInstances.getArgs.findSome? fun attrInst =>
        match attrInst[1]? with
        | some attr =>
          if attr.getKind == `Architect.blueprint || attr.getKind == `blueprint then
            some attr
          else
            none
        | none => none

/-- Parse BlueprintConfig from blueprint attribute syntax.

    The attribute syntax is: blueprint "?"? blueprintOptions
    where blueprintOptions = (ppSpace str)? (ppSpace blueprintOption)*

    This is a simplified version of elabBlueprintConfig from Attribute.lean,
    adapted to work in CommandElabM and parsing only the essential fields.
-/
def parseBlueprintConfig (attrStx : Syntax) : CommandElabM BlueprintConfig := do
  -- The attribute syntax is: blueprint "?"? blueprintOptions
  -- attrStx[0] = "blueprint"
  -- attrStx[1] = optional "?" token
  -- attrStx[2] = blueprintOptions node
  let mut config : BlueprintConfig := {}

  -- Check for trace flag (the "?" after blueprint)
  if let some traceOpt := attrStx[1]? then
    if !traceOpt.isNone then
      config := { config with trace := true }

  -- Get blueprintOptions node
  let optsNode := attrStx[2]?
  if optsNode.isNone then return config
  let optsNode := optsNode.get!

  -- blueprintOptions = (ppSpace str)? (ppSpace blueprintOption)*
  -- optsNode[0] = optional label string
  -- optsNode[1..] = blueprintOption nodes

  -- Check for the optional latex label string (first child)
  if let some labelOpt := optsNode[0]? then
    if !labelOpt.isNone then
      -- The label is wrapped in optional syntax
      if let some labelStr := labelOpt[0]? then
        if let some label := labelStr.isStrLit? then
          config := { config with latexLabel := some label }

  -- Parse the remaining options
  -- optsNode[1] contains all blueprintOption nodes as children
  -- (the * in the syntax creates a single node with multiple children)
  if let some optsWrapper := optsNode[1]? then
    if !optsWrapper.isNone then
      for j in [0:optsWrapper.getNumArgs] do
        let opt := optsWrapper[j]!
        -- blueprintOption = "(" innerOption ")"
        -- opt[0] = "(", opt[1] = innerOption, opt[2] = ")"
        if let some innerOpt := opt[1]? then
          let innerKind := innerOpt.getKind

          -- Parse based on inner option kind
          -- blueprintLatexLabelOption: "latexLabel" " := " str
          if innerKind == `Architect.blueprintLatexLabelOption then
            if let some strStx := innerOpt[2]? then
              if let some label := strStx.isStrLit? then
                config := { config with latexLabel := some label }
          -- blueprintLatexEnvOption: "latexEnv" " := " str
          else if innerKind == `Architect.blueprintLatexEnvOption then
            if let some strStx := innerOpt[2]? then
              if let some env := strStx.isStrLit? then
                config := { config with latexEnv := some env }
          -- blueprintStatementOption: "statement" " := " plainDocComment
          else if innerKind == `Architect.blueprintStatementOption then
            if let some docStx := innerOpt[2]? then
              let text ← liftCoreM <| getDocStringText ⟨docStx⟩
              if !text.isEmpty then
                config := { config with statement := some text.trimAscii.toString }
          -- blueprintProofOption: "proof" " := " plainDocComment
          else if innerKind == `Architect.blueprintProofOption then
            if let some docStx := innerOpt[2]? then
              let text ← liftCoreM <| getDocStringText ⟨docStx⟩
              if !text.isEmpty then
                config := { config with proof := some text.trimAscii.toString }
          -- For other options, skip (uses, proofUses require name resolution)

  return config

end Dress.Capture
