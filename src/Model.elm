module Model exposing (..)

import Snake exposing (Snake, Point)

-- MODEL
type alias Position =
  {
    x: Int,
    y: Int
  }

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
}

type Direction =
  Up
  | Right
  | Down
  | Left