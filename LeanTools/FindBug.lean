import Lean.Elab.Import
import Lean.Elab.Command
import Lean.Util.Profile
import Lean.Elab.Frontend
import Lean.Server.Snapshots
import Init.System.IO
-- import Cli

def List.takeOpt : Option Nat → List α → List α
  | Option.none, ls => ls
  | Option.some n, ls => ls.take n

namespace Lean.MessageSeverity
deriving instance DecidableEq for Lean.MessageSeverity
end Lean.MessageSeverity

namespace LeanTools.FindBug

structure Options where
  lean_path : List String := []
  init_search_path : Bool := true
  max_error_count : Option Nat := Option.none
  only_errors : Bool := true
  deriving Repr


structure InitialState extends Options where
  messages_to_match : List String
  message_filter : Lean.MessageLog → IO (List String)

structure State extends InitialState where
  contents : String

def Options.default : Options := { }

instance : ToString Options where
  toString (o : Options) := s!"{repr o}"

def parse_options : List String → Options × List String
  | [] => ({ }, [])
  | arg :: args =>
    match arg.splitOn "=" with
    | "--lean-path" :: rest =>
      let (o, args) := parse_options args
      ({o with lean_path := ("=".intercalate rest)::o.lean_path}, args)
    | ["--no-init-search-path"] =>
      let (o, args) := parse_options args
      ({o with init_search_path := false }, args)
    | ["--max-error-count", n] =>
      let (o, args) := parse_options args
      match n.toNat? with
      | Option.some n =>
        ({o with max_error_count := n}, args)
      | Option.none =>
        (o, arg::args)
    | _ =>
      let (o, args) := parse_options args
      (o, arg :: args)

def runLean (input : String) (opts : Lean.Options) (fileName : String) : IO (Lean.Environment × Lean.MessageLog) := open Lean in do
  let inputCtx := Parser.mkInputContext input fileName
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (env, messages) ← Elab.processHeader header opts messages inputCtx
  let s ← Elab.IO.processCommands inputCtx parserState (Elab.Command.mkState env messages opts)
  pure (s.commandState.env, s.commandState.messages)

def firstRun (opts : Options) (inputFileName : String) : IO (Sum UInt32 State) := do
  let contents ← IO.FS.readFile inputFileName
  let (env, msgs) ← runLean contents Lean.Options.empty inputFileName
  let check_message : Lean.Message → Bool :=
    if opts.only_errors
    then (λ msg => decide (msg.severity = Lean.MessageSeverity.error))
    else (λ msg => true)
  let filter_messages := λ (msgs : Lean.MessageLog) =>
      List.mapM (λ msg : Lean.Message => msg.data.toString) (List.filter check_message (msgs.toList.takeOpt opts.max_error_count))
  let st : State := { toOptions := opts
                    , message_filter := filter_messages
                    , messages_to_match := (← filter_messages msgs)
                    , contents := contents }
  for msg in st.messages_to_match do
    IO.println msg
  pure (Sum.inr st)

def run' (args : List String) : IO UInt32 := do
  IO.println args
  match args with
  | inputFileName :: outputFileName :: rest_args =>
    let (opts, rest_args) := parse_options rest_args
    IO.println opts
    IO.println rest_args
    let sp := match opts.lean_path with
      | [] => Option.none
      | _ => Option.some ([System.FilePath.searchPathSeparator].asString.intercalate opts.lean_path)
    if opts.init_search_path then Lean.initSearchPath sp else pure ()
    match (← firstRun opts inputFileName) with
    | Sum.inr st => pure 0
    | Sum.inl err => pure err
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
