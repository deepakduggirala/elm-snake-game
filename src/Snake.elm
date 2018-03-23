module Snake exposing ( Snake, Point, coords, cons, head, isHeadOnTail, grow, adjacentList)

import Nonempty exposing (..)
import Math exposing (..)

type alias Point = (Float, Float)

type alias Snake = {
  body: List Point
  ,length: Float
}

head : Snake -> Maybe Point
head = coords 0

coords : Float -> Snake -> Maybe Point
coords l snake =
  case snake.body of
    [] -> Nothing
    [h] -> Just h
    h::h2::t -> Just <| pointAlongLine (fromList h2 t) h l

pointAlongLine : Nonempty Point -> Point -> Float -> Point
pointAlongLine data start l =
  case data of
    Elem h -> interpolate start h l
    Cons h t ->
      let
        d = distance start h
      in
      if l < d
      then interpolate start h l
      else pointAlongLine t h (l - d)
      
cons : Point -> Snake -> Snake
cons new_head snake =
  prune {
    snake
    | body = new_head :: snake.body
  }

prune : Snake -> Snake
prune snake =
  {
    snake
    | body =
      case snake.body of
        [] -> []
        h::t ->
          compress <|
          List.append (List.filter (\p -> (lineDistance (10^(-3)) t h p) <= snake.length) snake.body) (maybe2list (coords snake.length snake))
  }

lastMatch: (a -> Bool) -> List a -> (Maybe a, List a)
lastMatch pred xs =
  case xs of
    [] -> (Nothing, [])
    h::t ->
      if pred h
      then
        case lastMatch pred t of
          (Nothing, rest) -> (Just h, t)
          (Just match, rest) -> (Just match, rest)
      else
        case lastMatch pred t of
          (Nothing, rest) -> (Nothing, h::rest)
          (Just match, rest) -> (Just match, rest)


compress : List Point -> List Point
compress points =
  case points of
    [] -> []
    h::t->
      case t of
        [] -> [h]
        h2::t2 ->
          case lastMatch (sameSlopeAs h h2) t2 of
            (Nothing, rest) -> h :: (compress (h2::rest))
            (Just match, rest) -> h :: (compress (match::rest))

sameSlopeAs: Point -> Point -> Point -> Bool
sameSlopeAs p1 p2 p3 =
  slope p1 p2 == slope p1 p3

isHeadOnTail : Float -> Snake -> Bool
isHeadOnTail tolerance snake =
  case snake.body of
    [] -> True
    h::t ->
      case t of
        [] -> False
        h2::t2 ->
          case t2 of
            [] -> False
            h3::t3 ->
              List.foldr (||) False (List.map (\(p1,p2) -> is_between tolerance p1 h p2) (adjacentList t))

grow : Float -> Snake -> Snake
grow x snake = { snake | length = snake.length + x }

maybe2list: Maybe a -> List a
maybe2list ma =
  case ma of
    Nothing -> []
    Just a -> [a]

adjacentList : List a -> List (a,a)
adjacentList l =
  case l of
    [] -> []
    h::t ->
      case t of
        [] -> []
        h2::t2 -> (h,h2) :: (adjacentList (h2::t2))