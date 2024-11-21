
namespace parse

inductive RunType where
  | real : RunType
  | test : RunType

instance : ToString RunType where
  toString | RunType.real => "real"
           | RunType.test => "test"

def to_run_type (s : String) : Option RunType :=
  match s with
  | "real" => some RunType.real
  | "test" => some RunType.test
  | _ => none

inductive AorB where
  | a : AorB
  | b : AorB

instance : ToString AorB where
  toString | AorB.a => "a"
           | AorB.b => "b"

def to_aorb (s : String) : Option AorB :=
  match s with
  | "a" => some AorB.a
  | "b" => some AorB.b
  | _ => none

def make_filename (r: RunType) (ab : AorB) (n : Nat) : String :=
  let ns_tmp := toString n
  let ns := if String.length ns_tmp == 1 then "0" ++ ns_tmp else ns_tmp
  match r with
  | RunType.test => s!"files/{r}/{ns}{ab}.txt"
  | RunType.real => s!"files/{r}/{ns}.txt"

def parse_args (args : List String) : Option (String × Nat × AorB) :=
  match args with
  | runtype :: num :: ab :: _ =>
      match (to_run_type runtype, String.toNat? num, to_aorb ab) with
        | (some r, some x, some ab') => some (make_filename r ab' x, x, ab')
        | _ => none
  | _ => none

end parse
