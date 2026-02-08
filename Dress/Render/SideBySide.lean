/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Lean
import Dress.Graph.Types

/-!
# Side-by-Side Rendering

Core types and functions for rendering side-by-side LaTeX + Lean code displays.
This module provides a unified rendering API used by both blueprint pages and paper generation.
-/

namespace Dress.Render

open Dress.Graph

/-- Data needed for rendering a side-by-side display -/
structure SbsData where
  /-- HTML element ID -/
  id : String
  /-- Display label (e.g., "4.1.1") -/
  label : String
  /-- Optional custom display number -/
  displayNumber : Option String := none
  /-- Environment type ("theorem", "lemma", "definition", etc.) -/
  envType : String
  /-- Node status for status indicator -/
  status : NodeStatus
  /-- LaTeX statement (already rendered to HTML) -/
  statementHtml : String
  /-- LaTeX proof (already rendered to HTML) -/
  proofHtml : Option String := none
  /-- Lean signature with syntax highlighting -/
  signatureHtml : Option String := none
  /-- Lean proof body with syntax highlighting -/
  proofBodyHtml : Option String := none
  /-- JSON data for hover tooltips -/
  hoverData : Option String := none
  /-- Fallback declaration names -/
  declNames : Array Lean.Name := #[]
  /-- Content to display above the theorem block (decoded LaTeX) -/
  above : Option String := none
  /-- Content to display below the theorem block (decoded LaTeX) -/
  below : Option String := none
  deriving Repr, Inhabited

/-- Rendering variant for side-by-side displays -/
inductive SbsVariant where
  /-- Blueprint variant: status character, standard CSS classes -/
  | blueprint
  /-- Paper variant: verification badge, blueprint link, paper-* CSS classes -/
  | paper (blueprintUrl : Option String)
  deriving Repr, Inhabited

/-! ## Helper Functions -/

/-- Escape HTML special characters for safe inclusion in attributes -/
def escapeHtml (s : String) : String :=
  s.replace "&" "&amp;"
   |>.replace "<" "&lt;"
   |>.replace ">" "&gt;"
   |>.replace "\"" "&quot;"
   |>.replace "'" "&#39;"

/-- Convert NodeStatus to color hex code -/
def statusToColor : NodeStatus → String
  | .notReady => "#F4A460"       -- Sandy Brown
  | .ready => "#20B2AA"          -- Light Sea Green
  | .sorry => "#8B0000"          -- Dark Red
  | .proven => "#90EE90"         -- Light Green
  | .fullyProven => "#228B22"    -- Forest Green
  | .mathlibReady => "#87CEEB"   -- Light Blue

/-- Convert NodeStatus to display string -/
def statusToDisplayString : NodeStatus → String
  | .notReady => "Not Ready"
  | .ready => "Ready"
  | .sorry => "Has Sorry"
  | .proven => "Proven"
  | .fullyProven => "Fully Proven"
  | .mathlibReady => "Mathlib Ready"

/-- Convert NodeStatus to CSS class for status indicator (6 statuses) -/
def statusToCssClass : NodeStatus → String
  | .notReady => "status-not-ready"
  | .ready => "status-ready"
  | .sorry => "status-sorry"
  | .proven => "status-proven"
  | .fullyProven => "status-fully-proven"
  | .mathlibReady => "status-mathlib-ready"

/-- Capitalize the first letter of a string -/
def capitalize (s : String) : String :=
  match s.toList with
  | [] => s
  | c :: cs => String.ofList (c.toUpper :: cs)

/-- Convert NodeStatus to verification badge CSS class -/
def statusToBadgeClass : NodeStatus → String
  | .proven | .fullyProven | .mathlibReady => "verified"
  | .sorry | .ready => "in-progress"
  | .notReady => "not-started"

/-- Convert NodeStatus to verification badge label text -/
def statusToBadgeLabel : NodeStatus → String
  | .proven | .fullyProven | .mathlibReady => "Verified"
  | .sorry | .ready => "In Progress"
  | .notReady => "Not Started"

/-- Convert NodeStatus to badge icon class suffix -/
def statusToBadgeIcon : NodeStatus → String
  | .proven | .fullyProven | .mathlibReady => "check"
  | .sorry | .ready => "half"
  | .notReady => "circle"

/-- Render a verification badge for paper mode.
    Produces HTML matching `.verification-badge` CSS in paper.css. -/
def renderVerificationBadge (status : NodeStatus) : String :=
  let cls := statusToBadgeClass status
  let icon := statusToBadgeIcon status
  let label := statusToBadgeLabel status
  s!"<span class=\"verification-badge {cls}\" title=\"Formalization status\"><span class=\"badge-icon badge-icon-{icon}\"></span><span class=\"badge-text\">{label}</span></span>"

/-! ## Rendering Functions -/

/-- Render the collapsible proof toggle (LaTeX proof) -/
def renderProofToggle (proofHtml : Option String) : String :=
  match proofHtml with
  | none => ""
  | some proof =>
    s!"<div class=\"proof_wrapper proof_inline\">
  <div class=\"proof_heading\">
    <span class=\"proof_caption\">Proof</span>
    <span class=\"expand-proof\">\u25BC</span>
  </div>
  <div class=\"proof_content\"><p>{proof}</p></div>
</div>"

/-- Build the hover data attribute string if present -/
def hoverDataAttr (data : SbsData) : String :=
  match data.hoverData with
  | some json => s!" data-lean-hovers=\"{escapeHtml json}\""
  | none => ""

/-- Render the Lean signature grid cell (row 2, col 2) -/
def renderSignatureCell (data : SbsData) : String :=
  let hoverAttr := hoverDataAttr data
  let codeContent := match data.signatureHtml with
    | some sig => s!"<code class=\"hl lean lean-signature\">{sig}</code>"
    | none =>
      -- Fallback: show declaration names if no highlighted code
      let names := data.declNames.map (fun (n : Lean.Name) => n.toString) |>.toList |> String.intercalate ", "
      if names.isEmpty then
        "<code class=\"hl lean\">-- No Lean code available</code>"
      else
        s!"<code class=\"hl lean\">-- See: {escapeHtml names}</code>"
  s!"<div class=\"sbs-signature\">
  <pre class=\"lean-code hl lean\"{hoverAttr}>{codeContent}</pre>
</div>"

/-- Render the Lean proof body grid cell (row 3, col 2) -/
def renderProofLeanCell (data : SbsData) : String :=
  match data.proofBodyHtml with
  | some proof =>
    let hoverAttr := hoverDataAttr data
    s!"<div class=\"sbs-proof-lean\">
  <pre class=\"lean-code hl lean\"{hoverAttr}><code class=\"hl lean lean-proof-body\">{proof}</code></pre>
</div>"
  | none => "<div class=\"sbs-proof-lean\"></div>"

/-- Render the heading grid cell (row 1, col 1) for blueprint variant -/
def renderHeadingCellBlueprint (data : SbsData) : String :=
  let envType := escapeHtml data.envType
  let displayLabel := escapeHtml (data.displayNumber.getD data.label)
  let statusColor := statusToColor data.status
  let statusTitle := statusToDisplayString data.status
  s!"<div class=\"sbs-heading\"><div class=\"{envType}_thmheading\">
  <span class=\"{envType}_thmcaption\">{capitalize envType}</span>
  <span class=\"{envType}_thmlabel\">{displayLabel}</span>
  <div class=\"thm_header_extras {statusToCssClass data.status}\"><span class=\"status-dot header-status-dot\" style=\"background:{statusColor}\" title=\"Status: {statusTitle}\"></span></div>
</div></div>"

/-- Render the heading grid cell (row 1, col 1) for paper variant -/
def renderHeadingCellPaper (data : SbsData) (blueprintUrl : Option String) : String :=
  let envType := escapeHtml data.envType
  let displayLabel := escapeHtml (data.displayNumber.getD data.label)
  let blueprintLink := match blueprintUrl with
    | some url => s!" <a class=\"blueprint-link\" href=\"{escapeHtml url}\">[blueprint]</a>"
    | none => ""
  let badge := renderVerificationBadge data.status
  s!"<div class=\"sbs-heading\"><div class=\"paper-theorem-header\">
  <span class=\"paper-theorem-type\">{capitalize envType} {displayLabel}</span>
  {badge}{blueprintLink}
</div></div>"

/-- Render the heading grid cell based on variant -/
def renderHeadingCell (data : SbsData) (variant : SbsVariant) : String :=
  match variant with
  | .blueprint => renderHeadingCellBlueprint data
  | .paper blueprintUrl => renderHeadingCellPaper data blueprintUrl

/-- Render the statement grid cell (row 2, col 1) -/
def renderStatementCell (data : SbsData) : String :=
  let envType := escapeHtml data.envType
  s!"<div class=\"sbs-statement\"><div class=\"{envType}_thmcontent\"><p>{data.statementHtml}</p></div></div>"

/-- Render the proof (LaTeX) grid cell (row 3, col 1) -/
def renderProofLatexCell (data : SbsData) : String :=
  let proofToggle := renderProofToggle data.proofHtml
  if proofToggle.isEmpty then
    "<div class=\"sbs-proof-latex\"></div>"
  else
    s!"<div class=\"sbs-proof-latex\">{proofToggle}</div>"

/-- Main entry point: render complete side-by-side display.
    Emits a 2-column x 3-row grid:
    Row 1: heading | spacer
    Row 2: statement | signature
    Row 3: proof (LaTeX) | proof (Lean) -/
def renderSideBySide (data : SbsData) (variant : SbsVariant) : String :=
  -- Escape user-controlled values to prevent XSS
  let envType := escapeHtml data.envType

  -- Container class varies by variant - both use sbs-container for side-by-side layout
  let containerClass := match variant with
    | .blueprint => s!"{envType}_thmwrapper sbs-container theorem-style-{envType}"
    | .paper _ => s!"paper-theorem paper-{envType} sbs-container"

  -- Row 1: heading + spacer
  let headingCell := renderHeadingCell data variant
  let spacerCell := "<div class=\"sbs-heading-spacer\"></div>"

  -- Row 2: statement + signature
  let statementCell := renderStatementCell data
  let signatureCell := renderSignatureCell data

  -- Row 3: proof (LaTeX) + proof (Lean)
  let proofLatexCell := renderProofLatexCell data
  let proofLeanCell := renderProofLeanCell data

  -- Optional above content (raw LaTeX for MathJax processing)
  let aboveHtml := match data.above with
    | some content => s!"\n<div class=\"sbs-above-content\">{content}</div>"
    | none => ""

  -- Optional below content (raw LaTeX for MathJax processing)
  let belowHtml := match data.below with
    | some content => s!"\n<div class=\"sbs-below-content\">{content}</div>"
    | none => ""

  s!"{aboveHtml}<div id=\"{escapeHtml data.id}\" class=\"{containerClass}\">
{headingCell}
{spacerCell}
{statementCell}
{signatureCell}
{proofLatexCell}
{proofLeanCell}
</div>{belowHtml}"

/-! ## Convenience Constructors -/

/-- Create SbsData from minimal required fields -/
def SbsData.mk' (id label envType : String) (status : NodeStatus) (statementHtml : String) : SbsData :=
  { id, label, envType, status, statementHtml }

end Dress.Render
