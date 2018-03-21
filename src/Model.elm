module Model exposing (..)

import Snake exposing (Snake, Point)
import Random

-- MODEL
type alias Grid =
  {
    width: Int,
    height: Int
  }

type alias Model = {
  snake: Snake
  ,speed: Float
  ,attitude: Direction
  ,dead: Bool
  ,grid: Grid
  ,food: Maybe Point
}

type Direction =
  Up
  | Right
  | Down
  | Left
  | UR
  | UL
  | DR
  | DL