import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Util.Profile
--import Cli

namespace LeanTools.FindBug
def run' (args : List String) : IO UInt32 := do
  IO.println args
  match args with
  | inputFileName :: outputFileName :: rest_args =>

    pure 0
  | _ =>
    IO.println s!"Not enough arguments {args}"
    pure 1

-- def run (args : Cli.Parsed) : IO UInt32 := pure 0

section
/-
open Cli
def cmd := `[Cli|
  findbug VIA run; ["0.0.1"]
  "findbug attempts to create a small file which reproduces a bug found in a large development."
]
-/
end
/-
def minimize (contents : String) (outputFileName : String) (tmpFileName : String) : IO Bool := do


def run (args : List String) (opts : Options) (fileName : String) (mainModuleName : Name) : IO (Environment × Bool) := do
  let inputCtx := Parser.mkInputContext input fileName
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← processHeader header opts messages inputCtx
  let env := env.setMainModule mainModuleName
  let s ← IO.processCommands inputCtx parserState (Command.mkState env messages opts)
  for msg in s.commandState.messages.toList do
    IO.print (← msg.toString (includeEndPos := getPrintMessageEndPos opts))
  pure (s.commandState.env, !s.commandState.messages.hasErrors)

-/

end LeanTools.FindBug
