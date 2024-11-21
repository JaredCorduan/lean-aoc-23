import LeanAoc23.Utils

namespace Day03

-- Part A

structure AOC3Num where
  val : Nat
  x   : Nat
  y   : Nat
  len : Nat
  deriving Repr

instance : ToString AOC3Num where
  toString s := s!"{s.val} ({s.x}, {s.y})"

def int_range (n : Nat) : List Int :=
  let f : Nat -> Int := λ a => a
  List.map f $ List.range n

def in_bounds  (m : Int)(p : Int × Int) :=
  p.fst >= 0 && p.fst < (m : Int) && p.snd >= 0 && p.snd < (m : Int)

def get_boundary (m : Nat) (n : AOC3Num) : Nat × List (Nat × Nat) :=
  let x : Int := n.x;
  let y : Int := n.y;
  let top := List.map (λ i => (x + i - 1, y-1)) (int_range $ n.len + 2)
  let bot := List.map (λ i => (x + i - 1, y+1)) (int_range $ n.len + 2)
  let full_box := (x -1, y) :: (x + n.len, y) :: (top ++ bot)
  let box := List.filter (in_bounds m) full_box
  (n.val, List.map (λ p => (Int.toNat p.fst, Int.toNat p.snd)) box)

structure AOC3State where
  num_chrs : List Char
  nums : List AOC3Num
  reading_num : Option (Nat × Nat)
  cursor_x : Nat
  cursor_y : Nat
  deriving Repr

def empty_aoc3state : AOC3State :=
  {num_chrs := [], nums := [], reading_num := none, cursor_x := 0, cursor_y := 0}

def parse_not_num (s : AOC3State) : AOC3State :=
    match s.reading_num with
    | none => {s with cursor_x := s.cursor_x + 1}
    | some (x, y) =>
        let n := String.toNat! (String.mk s.num_chrs)
        { num_chrs := []
        , nums := AOC3Num.mk n x y (List.length s.num_chrs):: s.nums
        , reading_num := none
        , cursor_x := s.cursor_x + 1
        , cursor_y := s.cursor_y
        }
def parse_num (s : AOC3State) (c : Char) : AOC3State :=
    match s.reading_num with
    | none => { num_chrs := [c]
              , nums := s.nums
              , reading_num := some (s.cursor_x, s.cursor_y)
              , cursor_x := s.cursor_x + 1
              , cursor_y := s.cursor_y
              }
    | some _ => {s with num_chrs := s.num_chrs ++ [c], cursor_x := s.cursor_x + 1}

def parse_any (s : AOC3State) (c : Char) : AOC3State :=
  if Char.isDigit c then parse_num s c else parse_not_num s

def parse_row (t : AOC3State ) (cs : List Char) : AOC3State :=
  let s := List.foldl parse_any t cs
  match s.reading_num with
  | none =>
     {empty_aoc3state with nums := s.nums, cursor_y := s.cursor_y + 1}
  | some (x, y) =>
     let n := String.toNat! (String.mk s.num_chrs)
     let l := List.length s.num_chrs
     {empty_aoc3state with nums := AOC3Num.mk n x y l :: s.nums, cursor_y := s.cursor_y + 1}

def is_symb (c : Char) : Bool := not (Char.isDigit c || c == '.')

def touches_symbol (map : List (List Char)) (s : Nat × List (Nat × Nat)) : Bool :=
  List.foldl (λ a p => a || is_symb (map[p.snd]![p.fst]!)) False s.snd

def a (lines : List String) : Nat :=
  let cs := Functor.map String.toList lines
  let state := List.foldl parse_row empty_aoc3state cs

  let m := List.length lines
  let boxes := Functor.map (get_boundary m) state.nums

  let part_numbers := List.filter (touches_symbol cs) boxes
  utils.sum $ Functor.map Prod.fst part_numbers

-- Part B

def BoundedNums := Nat × (List (Nat × Nat))
  deriving Inhabited

def gear_ratio (ns : List BoundedNums) (p : Nat × Nat) : Nat :=
  let adjacents := List.filter (λ n => List.elem p n.snd) ns
  if List.length adjacents == 2
    then List.foldl (· * · ) 1 $ List.map Prod.fst adjacents
    else 0

def is_star (cs : List (List Char)) (p : Nat × Nat) : Bool :=
  cs[p.snd]![p.fst]! == '*'

def to_pair (m : Nat) (p : Nat) : Nat × Nat := (p / m, p % m)

def b (lines : List String) : Nat :=
  let cs := Functor.map String.toList lines
  let state := List.foldl parse_row empty_aoc3state cs

  let m := List.length lines
  let boxes := Functor.map (get_boundary m) state.nums

  let coords := List.map (to_pair m) (List.range (m*m))
  let stars := List.filter (is_star cs) coords
  let gear_ratios := List.map (gear_ratio boxes) stars
  utils.sum $ gear_ratios

namespace Day03
