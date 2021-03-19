import LeanTools.FindBug
import LeanTools.CommandLineColors
--import Cli
--open Cli
open LeanTools

/-
def run (args : Parsed) : IO UInt32 := pure 0

def cmd : Cmd := `[Cli|
  LeanTools VIA run; ["0.0.1"]
  "A collection of command-line tools for processing Lean files."

  FLAGS:

  SUBCOMMANDS:
    FindBug.cmd
]

def main (args : List String) : IO UInt32 := do
  cmd.validate! args
-/

structure Driver where
  name : String
  act : List String → IO UInt32

def known_drivers : List Driver :=
  [
    { name := "find-bug", act := FindBug.run' }
  ]

def usage : List String := []

def print_usage {A} (ret : A) : IO A :=
  pure ret

def main (args : List String) : IO UInt32 := do
  match args with
  | [] => print_usage 1
  | ["-h"] => print_usage 0
  | ["--help"] => print_usage 0
  | driver :: rest_args
    => match List.find? (λ d => driver = d.name) known_drivers with
       | Option.some d => d.act rest_args
       | Option.none => do
         let valid_drivers := List.map Driver.name known_drivers
         IO.println s!"{ANSI.red}Error{ANSI.reset}: Invalid driver {driver}, expected -h, --help, or one of: {valid_drivers}"
         pure 1
