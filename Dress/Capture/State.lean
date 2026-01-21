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
-/

namespace Dress.Capture

/-- IO.Ref to track if we're inside the blueprint capture hook to prevent recursion.
    Using IO.Ref to avoid infinite recursion when our elab_rules call elabCommandTopLevel. -/
initialize blueprintCaptureHookRef : IO.Ref Bool ← IO.mkRef false

/-- Check if we're currently inside the capture hook. -/
def inCaptureHook : IO Bool := blueprintCaptureHookRef.get

end Dress.Capture

-- Re-export at Dress namespace for backward compatibility
namespace Dress

abbrev blueprintCaptureHookRef := Capture.blueprintCaptureHookRef

end Dress
