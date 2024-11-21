namespace utils

class Monoid (α : Type) where
  sum : α → α → α
  iden : α

instance : Monoid Nat where
  sum (x y : Nat) := x + y
  iden := 0

def sum [Monoid α] : List α → α := List.foldl Monoid.sum Monoid.iden

def mapMaybe (f : α → Option β) : List α → List β
  | [] => []
  | x::xs =>
      match f x with
      | none    => mapMaybe f xs
      | some b  => b::(mapMaybe f xs)

-- slice n m of list xs is sublist starting at index n, of length m
def slice : Nat → Nat → List α → List α
  | _, _, [] => []
  | 0, m, xs => xs.take m
  | n+1, m, _ :: xs => slice n m xs

end utils
