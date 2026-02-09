-- Main import file for Dress
-- Dress provides "dressing" (syntax highlighting, HTML rendering, .tex generation)
-- for @[blueprint] declarations from LeanArchitect.

-- Re-export Architect so users only need `import Dress`
import Architect

-- Core types and utilities
import Dress.Core
import Dress.Base64
import Dress.Highlighting
import Dress.HtmlRender

-- Capture: Elaboration-time artifact capture
import Dress.Capture.State
import Dress.Capture.InfoTree
import Dress.Capture.Config
import Dress.Capture.ElabRules

-- Serialize: Output format generation
import Dress.Serialize.Json
import Dress.Serialize.Html
import Dress.Serialize.Artifacts

-- Generate: LaTeX and module output
import Dress.Generate.Latex
import Dress.Generate.Module
import Dress.Generate.Declaration

-- Cache: Per-declaration content caching
import Dress.Cache

-- Main entry point (re-exports from submodules)
import Dress.Hook

-- Content and loading utilities
import Dress.Content
import Dress.SubVersoExtract
import Dress.Output
import Dress.Load

-- Graph: Dependency graph generation (SVG + JSON)
import Dress.Graph.Types
import Dress.Graph.Build
import Dress.Graph.Layout
import Dress.Graph.Svg
import Dress.Graph.Json

-- Render: Unified rendering for side-by-side displays
import Dress.Render

