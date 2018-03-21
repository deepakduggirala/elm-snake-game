module Init exposing (..)

import Task exposing (Task)
import Window

import Snake exposing (Snake, Point)
import Model exposing (..)
import Msg exposing (..)


speed: Float
speed = 0.025  -- grid units per millisecond

start: Point
start = (30, 30)

len: Float
len = 30 -- grid units

initSnake : Snake
initSnake = Snake [start, leftof start 1] len

leftof : Point -> Float -> Point
leftof (x,y) xd = (x-xd,y)

init : ( Model, Cmd Msg )
init =
  (
    Model initSnake speed Up False  (Grid 0 0)
    , Task.perform (\{width, height} -> Resize height width) Window.size
  )