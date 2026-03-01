import Lake
open System Lake DSL

package Dress where
  -- Lake handles linking for local dependencies automatically

@[default_target]
lean_lib Dress

@[default_target]
lean_exe extract_blueprint where
  root := `Main
  supportInterpreter := true

lean_lib DressBlueprint

lean_exe «generate-blueprint-verso» where
  root := `GenerateBlueprint

require LeanArchitect from
  ".." / ".." / "forks" / "LeanArchitect"

require Cli from git
  "https://github.com/mhuisi/lean4-cli" @ "v4.28.0-rc1"

require subverso from git
  "https://github.com/e-vergo/subverso.git" @ "main"

-- Using forked verso with bracket/comment highlighting
require verso from git
  "https://github.com/e-vergo/verso.git" @ "main"
