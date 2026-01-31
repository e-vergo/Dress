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

/-! ## Rendering Functions -/

/-- Render the collapsible proof toggle (LaTeX proof) -/
def renderProofToggle (proofHtml : Option String) : String :=
  match proofHtml with
  | none => ""
  | some proof =>
    s!"<div class=\"proof_wrapper proof_inline\">
  <div class=\"proof_heading\">
    <span class=\"proof_caption\">Proof</span>
    <span class=\"expand-proof\">[show]</span>
  </div>
  <div class=\"proof_content\"><p>{proof}</p></div>
</div>"

/-- Render the Lean code column (right side) -/
def renderLeanColumn (data : SbsData) : String :=
  -- Handle all 4 cases for signature/proof combinations
  let codeContent := match data.signatureHtml, data.proofBodyHtml with
    | some sig, some proof =>
      s!"<code class=\"hl lean lean-signature\">{sig}</code><code class=\"hl lean lean-proof-body\">{proof}</code>"
    | some sig, none =>
      s!"<code class=\"hl lean lean-signature\">{sig}</code>"
    | none, some proof =>
      s!"<code class=\"hl lean lean-proof-body\">{proof}</code>"
    | none, none =>
      -- Fallback: show declaration names if no highlighted code
      let names := data.declNames.map (fun (n : Lean.Name) => n.toString) |>.toList |> String.intercalate ", "
      if names.isEmpty then
        "<code class=\"hl lean\">-- No Lean code available</code>"
      else
        s!"<code class=\"hl lean\">-- See: {escapeHtml names}</code>"

  -- Build the hover data attribute if present
  let hoverAttr := match data.hoverData with
    | some json => s!" data-lean-hovers=\"{escapeHtml json}\""
    | none => ""

  s!"<div class=\"sbs-lean-column\">
  <pre class=\"lean-code hl lean\"{hoverAttr}>
{codeContent}
  </pre>
</div>"

/-- Render the LaTeX column (left side) for blueprint variant -/
def renderLatexColumnBlueprint (data : SbsData) : String :=
  -- Escape user-controlled values to prevent XSS
  let envType := escapeHtml data.envType
  let displayLabel := escapeHtml (data.displayNumber.getD data.label)
  let statusColor := statusToColor data.status
  let statusTitle := statusToDisplayString data.status

  -- Heading with status dot (color indicates status)
  let heading := s!"<div class=\"{envType}_thmheading\">
  <span class=\"{envType}_thmcaption\">{capitalize envType}</span>
  <span class=\"{envType}_thmlabel\">{displayLabel}</span>
  <div class=\"thm_header_extras {statusToCssClass data.status}\"><span class=\"status-dot header-status-dot\" style=\"background:{statusColor}\" title=\"Status: {statusTitle}\"></span></div>
</div>"

  -- Statement content (statementHtml is pre-rendered LaTeX HTML, trusted)
  let statement := s!"<div class=\"{envType}_thmcontent\"><p>{data.statementHtml}</p></div>"

  -- Optional proof toggle
  let proofToggle := renderProofToggle data.proofHtml

  s!"<div class=\"sbs-latex-column\">
{heading}
{statement}
{proofToggle}
</div>"

/-- Render the LaTeX column (left side) for paper variant -/
def renderLatexColumnPaper (data : SbsData) (blueprintUrl : Option String) : String :=
  -- Escape user-controlled values to prevent XSS
  let envType := escapeHtml data.envType
  let displayLabel := escapeHtml (data.displayNumber.getD data.label)
  let statusColor := statusToColor data.status
  let statusTitle := statusToDisplayString data.status

  -- Blueprint link if URL provided
  let blueprintLink := match blueprintUrl with
    | some url => s!" <a class=\"blueprint-link\" href=\"{escapeHtml url}\">[blueprint]</a>"
    | none => ""

  -- Paper-style heading with status dot (no badge wrapper, just dot + link)
  let heading := s!"<div class=\"paper-theorem-header\">
  <span class=\"paper-theorem-type\">{capitalize envType} {displayLabel}</span>
  <span class=\"status-dot paper-status-dot\" style=\"background:{statusColor}\" title=\"Status: {statusTitle}\"></span>{blueprintLink}
</div>"

  -- Statement content (statementHtml is pre-rendered LaTeX HTML, trusted)
  let statement := s!"<div class=\"{envType}_thmcontent\"><p>{data.statementHtml}</p></div>"

  -- Optional proof toggle
  let proofToggle := renderProofToggle data.proofHtml

  s!"<div class=\"sbs-latex-column\">
{heading}
{statement}
{proofToggle}
</div>"

/-- Render the LaTeX column based on variant -/
def renderLatexColumn (data : SbsData) (variant : SbsVariant) : String :=
  match variant with
  | .blueprint => renderLatexColumnBlueprint data
  | .paper blueprintUrl => renderLatexColumnPaper data blueprintUrl

/-- Main entry point: render complete side-by-side display -/
def renderSideBySide (data : SbsData) (variant : SbsVariant) : String :=
  -- Escape user-controlled values to prevent XSS
  let envType := escapeHtml data.envType

  -- Container class varies by variant
  let containerClass := match variant with
    | .blueprint => s!"{envType}_thmwrapper sbs-container theorem-style-{envType}"
    | .paper _ => s!"paper-theorem paper-{envType}"  -- No sbs-container for paper (single column)

  let latexCol := renderLatexColumn data variant
  -- Paper mode: don't show Lean code column
  let leanCol := match variant with
    | .blueprint => renderLeanColumn data
    | .paper _ => ""

  s!"<div id=\"{escapeHtml data.id}\" class=\"{containerClass}\">
{latexCol}
{leanCol}
</div>"

/-! ## Convenience Constructors -/

/-- Create SbsData from minimal required fields -/
def SbsData.mk' (id label envType : String) (status : NodeStatus) (statementHtml : String) : SbsData :=
  { id, label, envType, status, statementHtml }

end Dress.Render
