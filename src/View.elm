module View exposing (block, discreteSnake, foo, foodBlock, getViewBox, range, score, view)

import Html exposing (br, button, div, h2, text)
import Html.Events exposing (onClick)
import Init
import Model exposing (..)
import Msg exposing (..)
import Snake exposing (Point, Snake)
import Svg exposing (circle, g, image, rect, svg)
import Svg.Attributes exposing (..)
import String


view : Model -> Html.Html Msg
view model =
  case model.dead of
    True ->
      div []
        [ h2 [] [ text "Game Over" ]
        , button [ onClick Restart ] [ text "Restart" ]
        ]

    False ->
      div []
        [ svg [ version "1.1", baseProfile "full", width "100vw", height "calc(100vh - 4px)", viewBox (getViewBox model.grid), preserveAspectRatio "none" ]
          [ g [ transform "matrix(1,0,0,-1,0,100)" ]
            (List.concat
              [ List.map block <| discreteSnake 1 model.snake
              , foodBlock model.food
              , score model
              ]
            )
          ]
        ]


block : Point -> Svg.Svg Msg
block ( cx, cy ) =
  let
    s =
      Init.blockWidth

    px =
      cx - s / 2

    py =
      cy - s / 2
  in
  rect [ x <| String.fromFloat px, y <| String.fromFloat py, width <| String.fromFloat s, height <| String.fromFloat s, fill "black" ] []


foodBlock : Maybe Point -> List (Svg.Svg Msg)
foodBlock mp =
  case mp of
    Nothing ->
      []

    Just ( px, py ) ->
      [ image [ x <| String.fromFloat px, y <| String.fromFloat py, width <| String.fromFloat Init.foodRadius, height <| String.fromFloat Init.foodRadius, xlinkHref "rat.svg" ] [] ]



-- [ circle [cx <| String.fromInt px, cy <| String.fromInt py, r <| String.fromInt Init.foodRadius, fill "red"] [] ]


score : Model -> List (Svg.Svg Msg)
score model =
  let
    s =
      truncate model.snake.length * 10

    { width, height } =
      model.grid
  in
  [ Svg.text_
    [ x <| String.fromInt (width - 2), y "4", textAnchor "end", transform "matrix(1,0,0,-1,0,100)", style "font-family: Times New Roman; font-size: 4; stroke: #000; stroke-width: 0.1; fill: #fff;" ]
    [ Svg.text <| String.fromInt s ]
  ]


discreteSnake : Float -> Snake -> List Point
discreteSnake res snake =
  foo <| List.map ((\b a -> Snake.coords a b) <| snake) (range 0 res snake.length)


foo : List (Maybe a) -> List a
foo mas =
  case mas of
    [] ->
      []

    mh :: mt ->
      case mh of
        Nothing ->
          []

        Just h ->
          h :: foo mt


range : Float -> Float -> Float -> List Float
range start step stop =
  let
    base =
      List.range 0 (truncate <| (stop - start) / step)
  in
  List.map (toFloat >> (*) step >> (+) start) base


getViewBox : Grid -> String
getViewBox { width, height } =
  "0 0 " ++ String.fromInt width ++ " 100"
