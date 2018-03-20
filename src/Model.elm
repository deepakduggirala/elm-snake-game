module Model exposing (..)

import Task exposing (Task)
import Msg exposing (..)

import Window

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
  attitude: Direction,
  body: List Position,
  dead: Bool,
  grid: Grid
}

type Direction =
  Up
  | Right
  | Down
  | Left


init : ( Model, Cmd Msg )
init =  ( Model Up (List.map (\i -> (Position (50-i) 50)) (List.range 0 29) ) False (Grid 0 0), Task.perform (\{width, height} -> Resize height width) Window.size )