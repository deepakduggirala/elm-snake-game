module Snake exposing ( Snake, Point, coords, cons, head, isHeadOnTail )

import Nonempty exposing (..)

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
      
interpolate : Point -> Point -> Float -> Point  -- can return the point that lie on the same line but outside the segment
interpolate a b n =
  let
    d = distance a b
    ( xa, ya ) = a
    ( xb, yb ) = b
    l = n/d
    m = 1 - l
  in
    (
       xa * m + xb*l
      ,ya * m + yb*l
    )


cons : Point -> Snake -> Snake
cons new_head snake =
  {
    snake
    | body = List.filter (\pos -> (lineDistance snake.body new_head pos) <= snake.length) <| new_head :: snake.body
  }

lineDistance : List Point -> Point -> Point -> Float
lineDistance data start p =
  case data of
    [] -> distance start p
    h::t ->
      if is_between start p h
      then distance start p
      else lineDistance t h p + distance start h

distance : Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt <| (x2-x1) ^ 2 + (y2-y1) ^ 2

epsilon : Float
epsilon = 10^(-5)

is_between : Point -> Point -> Point -> Bool
is_between a c b = 
  let zero = (distance a c) + (distance c b) - (distance a b)
  in
    -epsilon < zero && zero < epsilon

isHeadOnTail : Snake -> Bool
isHeadOnTail snake =
  False