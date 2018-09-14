module Model exposing (Direction(..), Grid, Model)

import Random
import Snake exposing (Point, Snake)



-- MODEL


type alias Grid =
  { width : Int
  , height : Int
  , actualWidth : Int
  , actualHeight : Int
  }


type alias Model =
  { snake : Snake
  , speed : Float
  , attitude : Direction
  , dead : Bool
  , grid : Grid
  , food : Maybe Point
  }


type Direction
  = Up
  | Right
  | Down
  | Left