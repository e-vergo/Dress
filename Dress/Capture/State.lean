/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean

/-!
# Capture State Management

This module contains IO.Ref state for the blueprint elaboration hooks.
It's in a separate module to avoid the "cannot evaluate init in same module" issue.

## State Variables

- `blueprintCaptureHookRef`: Prevents recursion in elaboration hooks
- `texGeneratedDeclsRef`: Tracks declarations for module header generation
-/

namespace Dress.Capture

/-- IO.Ref to track if we're inside the blueprint capture hook to prevent recursion.
    Using IO.Ref to avoid infinite recursion when our elab_rules call elabCommandTopLevel. -/
initialize blueprintCaptureHookRef : IO.Ref Bool ← IO.mkRef false

/-- Track declarations processed for .tex generation in this module.
    Stores `(name, latexLabel)` pairs for later module header generation.
    Reset at module boundaries by the elaboration hook. -/
initialize texGeneratedDeclsRef : IO.Ref (Array (Lean.Name × String)) ← IO.mkRef #[]

/-- Check if we're currently inside the capture hook. -/
def inCaptureHook : IO Bool := blueprintCaptureHookRef.get

/-- Add a declaration to the tracking list for module header generation. -/
def trackDeclaration (name : Lean.Name) (latexLabel : String) : IO Unit := do
  texGeneratedDeclsRef.modify fun arr => arr.push (name, latexLabel)

/-- Get all tracked declarations for the current module. -/
def getTrackedDeclarations : IO (Array (Lean.Name × String)) :=
  texGeneratedDeclsRef.get

/-- Reset tracked declarations (called at module boundaries). -/
def resetTrackedDeclarations : IO Unit :=
  texGeneratedDeclsRef.set #[]

end Dress.Capture

-- Re-export at Dress namespace for backward compatibility
namespace Dress

abbrev blueprintCaptureHookRef := Capture.blueprintCaptureHookRef
abbrev texGeneratedDeclsRef := Capture.texGeneratedDeclsRef

end Dress
