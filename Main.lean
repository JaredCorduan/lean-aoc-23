import LeanAoc23


def run (n : Nat) (ab : parse.AorB) (lines : List String) : String :=
  match (n, ab) with
  | (1, parse.AorB.a) => toString $ Day01.a lines
  | (1, parse.AorB.b) => toString $ Day01.b lines
  | (2, parse.AorB.a) => toString $ Day02.a lines
  | (2, parse.AorB.b) => toString $ Day02.b lines
  | (3, parse.AorB.a) => toString $ Day03.a lines
  | (3, parse.AorB.b) => toString $ Day03.b lines
  | (4, parse.AorB.a) => toString $ Day04.a lines
  | (4, parse.AorB.b) => toString $ Day04.b lines
  | (5, parse.AorB.a) => toString $ Day05.a lines
  | (5, parse.AorB.b) => toString $ Day05.b lines
  | _ => "not implemented"

def main (args : List String) : IO UInt32 := do
  if let some (filename, n, ab) := parse.parse_args args
  then
    let ans <- (run n ab ∘ Array.toList) <$> IO.FS.lines ⟨filename⟩
    IO.println ans
    pure 0
  else
    IO.println "lean-aoc-23 [real/test] [num] [a/b]"
    pure 1
