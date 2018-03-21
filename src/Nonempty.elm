module Nonempty exposing (..)

type Nonempty a = Elem a | Cons a (Nonempty a)

fromList : a -> List a -> Nonempty a
fromList elem list =
  case list of
    [] -> Elem elem
    h::t -> Cons elem (fromList h t)

toList : Nonempty a -> List a
toList xs =
  case xs of
    Elem x -> [x]
    Cons h t -> h :: (toList t)