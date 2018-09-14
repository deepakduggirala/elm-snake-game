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
    "Q" ->
      Just UL
    "q" ->
      Just UL
    "W" ->
      Just UR
    "w" ->
      Just UR
    "A" ->
      Just DL
    "a" ->
      Just DL
    "S" ->
      Just DR
    "s" ->
      Just DR
    _ ->
      Nothing