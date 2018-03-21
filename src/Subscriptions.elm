module Subscriptions exposing (..)

import Model exposing (..)
import Msg exposing (..)
import Init exposing (..)

import Keyboard
import AnimationFrame
import Window
import Time exposing (Time)
import Random

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

-- foo : Time -> Msg
-- foo t =
--   let
--     foodGen = Random.pair Random.float Random.float
--   in
--     Random.generate Food foodGen