module Math exposing (..)

type alias Point =
  (Float, Float)

withInTol : Float -> Float -> Bool
withInTol tol zero =
    -tol < zero && zero < tol

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
  withInTol epsilon <| (distance a c) + (distance c b) - (distance a b)

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

slope : Point -> Point -> Float
slope (x1, y1) (x2, y2) =
  (y2-y1)/(x2-x1)