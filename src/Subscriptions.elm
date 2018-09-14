module Subscriptions exposing (subscriptions)

import Init exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Time
import Browser.Events exposing (onKeyDown, onResize, onAnimationFrameDelta)
import Json.Decode as Decode


subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dead of
    True ->
      Sub.none

    False ->
      Sub.batch
        [ onKeyDown keyDecoder
        , onAnimationFrameDelta Tick
        , onResize (\h w -> Resize w h)
        , Time.every (1000*Init.foodInterval) FoodTime
        ]

keyDecoder : Decode.Decoder Msg
keyDecoder = Decode.map (KeyDirection << toDirection) (Decode.field "key" Decode.string)

toDirection : String -> Maybe Direction
toDirection k =
  case k of
    "ArrowLeft" ->
      Just Left
    "ArrowUp" ->
      Just Up
    "ArrowRight" ->
      Just Right
    "ArrowDown" ->
      Just Down
    "D" ->
      Just Right
    "d" ->
      Just Right
    "W" ->
      Just Up
    "w" ->
      Just Up
    "A" ->
      Just Left
    "a" ->
      Just Left
    "S" ->
      Just Down
    "s" ->
      Just Down
    _ ->
      Nothing