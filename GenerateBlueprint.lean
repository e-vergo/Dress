/-
Generate HTML output from the Dress Blueprint document.
-/
import SBSBlueprint
import DressBlueprint.Blueprint

open Verso.Genre.SBSBlueprint.Main

def main : IO UInt32 :=
  sbsBlueprintMain (%doc DressBlueprint.Blueprint) (config := {
    outputDir := ".lake/build/verso",
    buildDir := ".lake/build",
    title := "Dress Blueprint",
    outputFileName := "blueprint_verso",
    verbose := true,
  })
