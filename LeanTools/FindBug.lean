import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Util.Profile
import Lean.Elab.Frontend
import Init.System.IO
-- import Cli

namespace LeanTools.FindBug

def runLean (input : String) (opts : Lean.Options) (fileName : String) : IO (Lean.Environment × Lean.MessageLog) := open Lean in do
  let inputCtx := Parser.mkInputContext input fileName
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← Elab.processHeader header opts messages inputCtx
  let s ← Elab.IO.processCommands inputCtx parserState (Elab.Command.mkState env messages opts)
  pure (s.commandState.env, s.commandState.messages)

def run' (args : List String) : IO UInt32 := do
  IO.println args
  match args with
  | inputFileName :: outputFileName :: rest_args =>
    Lean.initSearchPath Option.none
    let contents ← IO.FS.readFile inputFileName
    let (env, msgs) ← runLean contents Lean.Options.empty inputFileName
    for msg in msgs.toList do
      IO.print (← msg.toString (includeEndPos := true))
    pure 0
  | _ =>
    IO.println s!"Not enough arguments {args}"
    pure 1

-- def run (args : Cli.Parsed) : IO UInt32 := pure 0

section
/-
open Cli
def cmd := `[Cli|
  "find-bug" VIA run; ["0.0.1"]
  "find-bug attempts to create a small file which reproduces a bug found in a large development."
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
