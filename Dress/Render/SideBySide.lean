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

/-- Convert LaTeX list environments (itemize/enumerate) to HTML lists.
    This handles cases where docstrings contain LaTeX list syntax that
    would otherwise confuse MathJax. -/
def convertLatexLists (s : String) : String := Id.run do
  let mut result := s

  -- First pass: convert \begin{env} and \end{env}
  result := result.replace "\\begin{itemize}" "<ul>"
  result := result.replace "\\end{itemize}" "</ul>"
  result := result.replace "\\begin{enumerate}" "<ol>"
  result := result.replace "\\end{enumerate}" "</ol>"

  -- Second pass: convert \item to <li>...</li>
  -- We need to find each \item and wrap its content until the next \item or closing tag
  let chars := result.toList
  let mut output := ""
  let mut i := 0
  let mut inList := false
  let mut inListItem := false

  while i < chars.length do
    -- Check for <ul> or <ol> start
    if i + 3 < chars.length &&
       chars[i]! == '<' && (
         (chars[i+1]! == 'u' && chars[i+2]! == 'l' && chars[i+3]! == '>') ||
         (chars[i+1]! == 'o' && chars[i+2]! == 'l' && chars[i+3]! == '>')) then
      output := output ++ String.ofList (chars.drop i |>.take 4)
      i := i + 4
      inList := true
    -- Check for </ul> or </ol> end
    else if i + 4 < chars.length &&
            chars[i]! == '<' && chars[i+1]! == '/' && (
              (chars[i+2]! == 'u' && chars[i+3]! == 'l' && chars[i+4]! == '>') ||
              (chars[i+2]! == 'o' && chars[i+3]! == 'l' && chars[i+4]! == '>')) then
      if inListItem then
        output := output ++ "</li>"
        inListItem := false
      output := output ++ String.ofList (chars.drop i |>.take 5)
      i := i + 5
      inList := false
    -- Check for \item
    else if inList && i + 4 < chars.length &&
            chars[i]! == '\\' && chars[i+1]! == 'i' && chars[i+2]! == 't' &&
            chars[i+3]! == 'e' && chars[i+4]! == 'm' then
      if inListItem then
        output := output ++ "</li>"
      output := output ++ "<li>"
      i := i + 5
      inListItem := true
      -- Skip optional space/newline after \item
      while i < chars.length && (chars[i]! == ' ' || chars[i]! == '\n' || chars[i]! == '\r' || chars[i]! == '\t') do
        i := i + 1
    else
      output := output.push chars[i]!
      i := i + 1

  return output

/-- Convert NodeStatus to a status character for display (6 statuses) -/
def statusChar : NodeStatus → String
  | .notReady => "&#10008;"      -- Heavy ballot X (✗)
  | .ready => "&#9673;"          -- Fisheye / circled dot (◉)
  | .sorry => "&#9888;"          -- Warning sign (⚠)
  | .proven => "&#9680;"         -- Circle with left half black (◐)
  | .fullyProven => "&#10003;"   -- Check mark (✓)
  | .mathlibReady => "&#10004;"  -- Heavy check mark (✔)

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

/-- Convert NodeStatus to a badge class for paper variant (6 statuses) -/
def statusToBadgeClass : NodeStatus → String
  | .notReady => "not-started"
  | .ready => "not-started"
  | .sorry => "in-progress"
  | .proven => "in-progress"
  | .fullyProven => "verified"
  | .mathlibReady => "verified"

/-- Convert NodeStatus to display text for badge (6 statuses) -/
def statusToBadgeText : NodeStatus → String
  | .notReady => "Not Ready"
  | .ready => "Ready"
  | .sorry => "Has Sorry"
  | .proven => "Proven"
  | .fullyProven => "Verified"
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
  let envType := data.envType
  let displayLabel := data.displayNumber.getD data.label
  let statusIndicator := statusChar data.status
  let statusColor := statusToColor data.status
  let statusTitle := statusToDisplayString data.status

  -- Heading with status dot and status character
  let heading := s!"<div class=\"{envType}_thmheading\">
  <span class=\"{envType}_thmcaption\">{capitalize envType}</span>
  <span class=\"{envType}_thmlabel\">{displayLabel}</span>
  <div class=\"thm_header_extras {statusToCssClass data.status}\"><span class=\"status-dot header-status-dot\" style=\"background:{statusColor}\" title=\"Status: {statusTitle}\"></span>{statusIndicator}</div>
</div>"

  -- Statement content
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
  let envType := data.envType
  let displayLabel := data.displayNumber.getD data.label
  let badgeClass := statusToBadgeClass data.status
  let badgeText := statusToBadgeText data.status
  let badgeChar := statusChar data.status

  -- Blueprint link if URL provided
  let blueprintLink := match blueprintUrl with
    | some url => s!" <a class=\"blueprint-link\" href=\"{escapeHtml url}\">[blueprint]</a>"
    | none => ""

  -- Paper-style heading with verification badge
  let heading := s!"<div class=\"paper-theorem-header\">
  <span class=\"paper-theorem-type\">{capitalize envType} {displayLabel}</span>
  <span class=\"verification-badge {badgeClass}\">{badgeChar} {badgeText}</span>{blueprintLink}
</div>"

  -- Statement content (reuses same class for consistency)
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
  let envType := data.envType

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
