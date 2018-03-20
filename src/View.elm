module View exposing (..)

import Html exposing (text, div, h2, br, button)
import Svg exposing (svg, rect, g)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)

import Msg exposing (..)
import Model exposing (..)

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
              g [ transform "matrix(1,0,0,-1,0,100)" ] (List.map block model.body)
            ]
        ]

block : Position -> Html.Html Msg
block pos =
  rect [ x <| toString pos.x, y <| toString pos.y, width "1", height "1", fill "black"] []

getViewBox : Grid -> String
getViewBox {width, height} = "0 0 " ++ toString width ++ " 100"