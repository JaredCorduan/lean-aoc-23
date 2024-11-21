import LeanAoc23.Utils

namespace Day01

-- Part A

def just_digits(xs : String) : List Char :=
  List.filter Char.isDigit $ String.toList xs

def first_and_last(xs : List Char) : String :=
  Char.toString (List.head! xs) ++ Char.toString (List.getLast! xs)

def get_calibration (xs : String) : Nat :=
  String.toNat! $ first_and_last (just_digits xs)

def a (xs : List String) : Nat :=
  utils.sum $ Functor.map get_calibration xs


-- Part B

def update_minmax (n : Nat) ( p : Nat × Nat) : Nat × Nat :=
  match p with
  | (0, _) => (n, n)
  | (m, _) => (m, n)

def word_match (xs : List Char) (s : String) (n : Nat) (p : Nat × Nat) : Nat × Nat :=
  let x := List.length xs;
  let ys := String.toList s
  let y := List.length ys;
  if x >= y && (List.take y xs) == ys
    then update_minmax n p
    else p

def update_fold_func (acc : (Nat × Nat) × List Char) (c : Char) : (Nat × Nat) × List Char :=
  let rest := List.tail acc.snd;
  let f₁ := (update_minmax · acc.fst)
  let f₃ := (word_match acc.snd · · ·)
  let f₂ := (f₃ · · acc.fst)
  match c with
  | '1' => (f₁ 1, rest)
  | '2' => (f₁ 2, rest)
  | '3' => (f₁ 3, rest)
  | '4' => (f₁ 4, rest)
  | '5' => (f₁ 5, rest)
  | '6' => (f₁ 6, rest)
  | '7' => (f₁ 7, rest)
  | '8' => (f₁ 8, rest)
  | '9' => (f₁ 9, rest)
  | 'o' => (f₂ "one" 1,  rest)
  | 't' => (f₃ "two" 2
           (f₂ "three" 3), rest)
  | 'f' => (word_match acc.snd "four"  4
           (f₂ "five"  5), rest)
  | 's' => (word_match acc.snd "six"   6
           (f₂ "seven" 7), rest)
  | 'e' => (f₂ "eight" 8,  rest)
  | 'n' => (f₂ "nine"  9,  rest)
  | _ => (acc.fst, rest)

def update_fold_func₂ (s : String) : Nat :=
    let cs := String.toList s
    let x := List.foldl update_fold_func ((0, 0), cs) cs
    10*x.fst.fst + x.fst.snd

def b (lines : List String) : Nat :=
  utils.sum $ Functor.map update_fold_func₂ lines

end Day01
