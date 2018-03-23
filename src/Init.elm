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

foodInterval: Float
foodInterval = 5 -- seconds

foodRadius: Float
foodRadius = 2

foodTol: Float
foodTol = foodRadius + blockWidth/(2)

accel : Float
accel = 0.0025 -- speed per food

growth : Float
growth = 3

blockWidth: Float
blockWidth = 1

initSnake : Snake
initSnake = Snake.cons (leftof start 1) (Snake [start] len)

leftof : Point -> Float -> Point
leftof (x,y) xd = (x-xd,y)

init : ( Model, Cmd Msg )
init =
  (
    Model initSnake speed Up False  (Grid 0 0) Nothing
    , Task.perform (\{width, height} -> Resize height width) Window.size
  )