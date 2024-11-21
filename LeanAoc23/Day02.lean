import LeanAoc23.Utils

namespace Day02

-- Part A

def splitter (c : Char) : Bool :=
  match c with
  | ';' => True
  | ',' => True
  | _ => False

def parse_picks (s : String) : Nat × String :=
  let x := String.splitOn s " "
  (String.toNat! x[1]!, x[2]!)

def check_pick (r g b : Nat) (p : Nat × String) : Bool :=
  match p.snd with
  | "red" => p.fst <= r
  | "green" => p.fst <= g
  | "blue" => p.fst <= b
  | _ => panic "bad color"

def check_game (r g b : Nat) (game : String) : Nat × Bool :=
  let header := String.splitOn game ":"
  let game_num := String.toNat! (String.splitOn header[0]! " ")[1]!

  let ys := String.split header[1]! splitter
  let zs := Functor.map parse_picks ys
  let okay := List.foldl (λ a p => a && check_pick r g b p) True zs
  (game_num, okay)

def a (lines : List String) : Nat :=
  let xs := Functor.map (check_game 12 13 14) lines
  List.foldl (λ a p => if p.snd then a + p.fst else a) 0 xs

-- Part B

structure ColorMax where
  red : Nat
  green : Nat
  blue : Nat

def check_max (m : ColorMax) (p : Nat × String) : ColorMax :=
  match p.snd with
  | "red" => if p.fst > m.red then { m with red := p.fst } else m
  | "green" => if p.fst > m.green then { m with green := p.fst } else m
  | "blue" => if p.fst > m.blue then { m with blue := p.fst } else m
  | _ => m

def check_game₂ (game : String) : Nat :=
  let header := String.splitOn game ":"
  let xs := String.split header[1]! splitter
  let ys := Functor.map parse_picks xs
  let maximums := List.foldl check_max {red := 0, blue := 0, green := 0} ys

  maximums.red * maximums.green * maximums.blue

def b (lines : List String) : Nat :=
  utils.sum $ Functor.map check_game₂ lines

namespace Day02
