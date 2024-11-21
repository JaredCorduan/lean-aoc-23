import LeanAoc23.Utils

namespace Day04

-- Part A

structure Card where
  ws : List Nat
  xs : List Nat

instance : ToString Card where
  toString c := s!"winnig: {c.ws}\ngot: {c.xs}"

def parse_card (s : String) : Card :=
  let wx := String.splitOn s " | "

  let w1 := String.splitOn wx[0]! ": "
  let w2 := String.splitOn w1[1]! " "
  let w3 := List.filter (· != "") w2
  let w4 := List.map String.toNat! w3

  let x1 := String.splitOn wx[1]! " "
  let x2 := List.filter (· != "") x1
  let x3 := List.map String.toNat! x2
  {ws := w4, xs := x3}


def get_num_winners (c : Card) : Nat :=
  let winners := List.filter (λ z => List.elem z c.xs) c.ws
  List.length winners

def get_score (c : Card) : Nat :=
  let n := get_num_winners c
  if n > 0 then 2 ^ (n - 1) else 0

def a (lines : List String) : Nat :=
  let cards := List.map parse_card lines
  utils.sum $ List.map get_score cards

-- Part B


def redeem_one_number (as : List Nat) (p : Nat × Nat) (xs : List Nat) : List Nat :=
  let ys := utils.slice (p.fst+1) (p.snd) as
  List.foldl (λ a x => if x != p.fst then x::a else ys ++ x::a) [] xs

def b (lines : List String) : Nat :=
  let cards := List.map parse_card lines
  let scores := List.enum $ List.map get_num_winners cards

  let start := List.range (List.length cards)
  let all_cards := List.foldl (λ a p => redeem_one_number start p a) start scores
  List.length all_cards

namespace Day04
