module View exposing (..)

import Html exposing (text, div, h2, br, button)
import Svg exposing (svg, rect, g, circle, image)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)

import Msg exposing (..)
import Model exposing (..)
import Snake exposing (Snake, Point)
import Init

view : Model -> Html.Html Msg
view model =
  case model.dead of
    True -> div []
      [
        h2 [] [text "Game Over"],
        button [ onClick Restart ] [ text "Restart" ]
      ]
    False ->
      div []
        [
          svg [ version "1.1", baseProfile "full", width "100vw", height "calc(100vh - 4px)", viewBox (getViewBox model.grid), preserveAspectRatio "none"]
            [
              g [ transform "matrix(1,0,0,-1,0,100)" ]
                (List.concat
                  [
                    List.map block <| discreteSnake 1 model.snake
                    ,foodBlock model.food
                    ,score model
                  ]
                )
            ]
        ]

block : Point -> Svg.Svg Msg
block (cx,cy) =
  let
    s = Init.blockWidth
    px = cx - s/2
    py = cy - s/2
  in
    rect [ x <| toString px, y <| toString py, width <| toString s, height <| toString s, fill "black"] []

foodBlock : Maybe Point -> List (Svg.Svg Msg)
foodBlock mp =
  case mp of
    Nothing -> []
    Just (px, py) ->
      [ image [x <| toString px, y <| toString py, width <| toString Init.foodRadius, height <| toString Init.foodRadius, xlinkHref "rat.svg"] [] ]
      -- [ circle [cx <| toString px, cy <| toString py, r <| toString Init.foodRadius, fill "red"] [] ]

score : Model -> List (Svg.Svg Msg)
score model =
  let
    s = (truncate model.snake.length)*10
    {width, height} = model.grid
  in
    [ Svg.text_
      [x <| toString (width-2), y "4", textAnchor "end", transform "matrix(1,0,0,-1,0,100)", style "font-family: Times New Roman; font-size: 4; stroke: #000; stroke-width: 0.1; fill: #fff;"]
      [ Svg.text <| toString s]
    ]

discreteSnake : Float -> Snake -> List Point
discreteSnake res snake =
  foo <| List.map (flip Snake.coords <| snake) (range 0 res snake.length)

foo : List (Maybe a) -> List a
foo mas =
  case mas of
    [] -> []
    mh::mt ->
      case mh of
        Nothing -> []
        Just h -> h :: foo mt


range : Float -> Float -> Float -> List Float
range start step stop =
  let
    base = List.range 0 (truncate <| (stop-start)/step)
  in
    List.map (toFloat >> (*) step >> (+) start) base


getViewBox : Grid -> String
getViewBox {width, height} = "0 0 " ++ toString width ++ " 100"