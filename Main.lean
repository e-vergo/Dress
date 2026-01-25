/-
Copyright (c) 2025 Dress contributors. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
import Dress
import Lean
import Cli

/-!
This executable extracts the blueprint data from a module, or
collates the blueprint data from multiple modules into a LaTeX file.
It also generates dependency graph visualizations (SVG and JSON).
-/

open Lean Cli Dress

def outputBaseDir (buildDir : System.FilePath) : System.FilePath :=
  buildDir / "blueprint"

def outputDressedDir (buildDir : System.FilePath) : System.FilePath :=
  buildDir / "dressed"

def runSingleCmd (p : Parsed) : IO UInt32 := do
  IO.eprintln "Warning: `lake exe extract_blueprint` is deprecated. Use `lake build :blueprint` instead."
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let baseDir := outputBaseDir buildDir
  let module := p.positionalArg! "module" |>.as! String |>.toName
  let isJson := p.hasFlag "json"
  -- Get path to pre-computed highlighted JSON from Lake facet (if provided)
  let highlightedJsonPath := p.flag? "highlightedJson" |>.map (·.as! String)
  let options : LeanOptions ← match p.flag? "options" with
    | some o => IO.ofExcept (Json.parse (o.as! String) >>= fromJson?)
    | none => pure (∅ : LeanOptions)

  if isJson then
    let json ← jsonOfImportModule module options.toOptions highlightedJsonPath
    outputJsonResults baseDir module json
  else
    let latexOutput ← latexOutputOfImportModule module options.toOptions highlightedJsonPath
    discard <| outputLatexResults baseDir module latexOutput
  return 0

def runIndexCmd (p : Parsed) : IO UInt32 := do
  IO.eprintln "Warning: `lake exe extract_blueprint` is deprecated. Use `lake build :blueprint` instead."
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let baseDir := outputBaseDir buildDir
  let library := p.positionalArg! "library" |>.as! String |>.toName
  let modules := p.variableArgsAs! String |>.map (·.toName)
  let isJson := p.hasFlag "json"
  if isJson then
    outputLibraryJson baseDir library modules
  else
    outputLibraryLatex baseDir library modules
  return 0

/-- Generate dependency graph SVG and JSON from a list of modules.
    Loads all modules, extracts blueprint nodes, builds graph, runs layout,
    and writes output files to the dressed directory. -/
def runGraphCmd (p : Parsed) : IO UInt32 := do
  let buildDir := match p.flag? "build" with
    | some dir => dir.as! String
    | none => ".lake/build"
  let dressedDir := outputDressedDir buildDir
  let modules := p.variableArgsAs! String |>.map (·.toName)

  if modules.isEmpty then
    IO.eprintln "Error: At least one module must be specified."
    return 1

  -- Create output directory
  IO.FS.createDirAll dressedDir

  -- Load environment with all modules
  runEnvOfImports modules {} do
    -- Build graph from environment
    let graph ← Graph.fromEnvironment (← getEnv)

    -- Apply transitive reduction to simplify the graph
    let reducedGraph := graph.transitiveReduction

    -- Run Sugiyama layout
    let layoutConfig : Graph.Layout.LayoutConfig := {}
    let layoutGraph := Graph.Layout.layout reducedGraph layoutConfig

    -- Write SVG
    let svgPath := dressedDir / "dep-graph.svg"
    Graph.Svg.renderToFile layoutGraph svgPath

    -- Write JSON
    let jsonPath := dressedDir / "dep-graph.json"
    Graph.writeJsonFile layoutGraph jsonPath

    IO.println s!"Generated dependency graph:"
    IO.println s!"  SVG: {svgPath}"
    IO.println s!"  JSON: {jsonPath}"
    IO.println s!"  Nodes: {layoutGraph.nodes.size}"
    IO.println s!"  Edges: {layoutGraph.edges.size}"

  return 0

def singleCmd := `[Cli|
  single VIA runSingleCmd;
  "Only extract the blueprint for the module it was given, might contain broken \\input{}s unless all blueprint files are extracted."

  FLAGS:
    j, json; "Output JSON instead of LaTeX."
    h, highlight; "No-op (kept for backward compatibility)."
    b, build : String; "Build directory."
    o, options : String; "LeanOptions in JSON to pass to running the module."
    highlightedJson : String; "Path to pre-computed highlighted JSON from Lake facet."

  ARGS:
    module : String; "The module to extract the blueprint for."
]

def indexCmd := `[Cli|
  index VIA runIndexCmd;
  "Collates the LaTeX outputs of modules in a library from `single` into a LaTeX file with \\input{}s pointing to the modules."

  FLAGS:
    j, json; "Output JSON instead of LaTeX."
    b, build : String; "Build directory."

  ARGS:
    library : String; "The library to index."
    ...modules : String; "The modules in the library."
]

def graphCmd := `[Cli|
  graph VIA runGraphCmd;
  "Generate dependency graph visualization (SVG and JSON) from blueprint nodes in the specified modules."

  FLAGS:
    b, build : String; "Build directory (default: .lake/build)."

  ARGS:
    ...modules : String; "The modules to include in the dependency graph."
]

def blueprintCmd : Cmd := `[Cli|
  "Dress" NOOP;
  "A dressing generator for Lean 4 blueprint projects."

  SUBCOMMANDS:
    singleCmd;
    indexCmd;
    graphCmd
]

def main (args : List String) : IO UInt32 :=
  blueprintCmd.validate args
