module Subscriptions exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Init exposing (..)

import Keyboard
import AnimationFrame
import Window
import Time exposing (Time)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dead of
    True -> Sub.none
    False ->
      Sub.batch [
        Keyboard.downs KeyCode
        , AnimationFrame.diffs Tick
        , Window.resizes (\{height, width} -> Resize height width)
        , Time.every (Init.foodInterval*Time.second) FoodTime
      ]