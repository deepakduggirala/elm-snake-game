module View exposing (..)

import Html exposing (text, div, h2, br, button)
import Svg exposing (svg, rect, g, circle)
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
              (case model.food of
                Nothing -> List.map block <| discreteSnake 1 model.snake
                Just p -> (foodBlock p) :: (List.map block <| discreteSnake 1 model.snake))
            ]
        ]

block : Point -> Html.Html Msg
block (cx,cy) =
  let
    s = Init.blockWidth
    px = cx - s/2
    py = cy - s/2
  in
    rect [ x <| toString px, y <| toString py, width <| toString s, height <| toString s, fill "black"] []

foodBlock : Point -> Html.Html Msg
foodBlock (px, py) =
  circle [cx <| toString px, cy <| toString py, r "1", fill "red"] []   

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