module Snake exposing ( Snake, Point, coords, cons, head, isHeadOnTail, grow)

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
  {
    snake
    | body = List.filter (\pos -> (lineDistance snake.body new_head pos) <= snake.length) <| new_head :: snake.body
  }

isHeadOnTail : Snake -> Bool
isHeadOnTail snake =
  False

grow : Float -> Snake -> Snake
grow x snake = { snake | length = snake.length + x }