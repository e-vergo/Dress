-- Main import file for Dress
-- Dress provides "dressing" (syntax highlighting, HTML rendering, .tex generation)
-- for @[blueprint] declarations from LeanArchitect.

-- Re-export Architect so users only need `import Dress`
import Architect

import Dress.Core
import Dress.HookState
import Dress.Highlighting
import Dress.HtmlRender
import Dress.Hook
import Dress.Content
import Dress.SubVersoExtract
import Dress.Output
import Dress.Load
