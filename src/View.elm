module View exposing
    ( block
    ,  discreteSnake
       -- , foo

    , foodBlock
    , getViewBox
    , range
    , score
    , view
    )

import Grid exposing (GridPoint, scaleFrom, scalePointFrom)
import Html exposing (br, button, div, h2, text)
import Html.Events exposing (onClick)
import Init
import Model exposing (..)
import Msg exposing (..)
import Snake exposing (Snake, coords)
import String
import Svg exposing (circle, g, image, rect, svg)
import Svg.Attributes exposing (..)


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
                            [ List.map block <| discreteSnake Init.resolution model.snake model.length
                            , foodBlock <| Maybe.map (scalePointFrom Init.sf_grid_to_view) model.food
                            , score model
                            ]
                        )
                    ]
                ]


block : ( Float, Float ) -> Svg.Svg Msg
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


foodBlock : Maybe ( Float, Float ) -> List (Svg.Svg Msg)
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
            truncate (scaleFrom model.length) * 10

        { viewWidth } =
            model.grid
    in
    [ Svg.text_
        [ x <| String.fromInt (viewWidth - 2), y "4", textAnchor "end", transform "matrix(1,0,0,-1,0,100)", style "font-family: Times New Roman; font-size: 4; stroke: #000; stroke-width: 0.1; fill: #fff;" ]
        [ Svg.text <| String.fromInt s ]
    ]


discreteSnake : Int -> Snake -> Int -> List ( Float, Float )
discreteSnake res snake length =
    let
        gridPoints =
            List.map (coords snake) (range 0 res length)
    in
    List.map (scalePointFrom Init.sf_grid_to_view) gridPoints



-- foo : List (Maybe a) -> List a
-- foo mas =
--     case mas of
--         [] ->
--             []
--         mh :: mt ->
--             case mh of
--                 Nothing ->
--                     []
--                 Just h ->
--                     h :: foo mt


range : Int -> Int -> Int -> List Int
range start step stop =
    let
        base =
            List.range 0 ((stop - start) // step)

        r =
            List.map ((*) step >> (+) start) base
    in
    if modBy step (stop - start) == 0 then
        r

    else
        r ++ [ stop ]


getViewBox : Grid -> String
getViewBox { viewWidth, viewHeight } =
    "0 0 " ++ String.fromInt viewWidth ++ " " ++ String.fromInt viewHeight
