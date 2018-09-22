module Init exposing
    ( accel
    , blockWidth
    , foodInterval
    , foodRadius
    , foodTolerance
    , growth
    , idSnake
    , init
    , len
    , resolution
    , sf_grid_to_view
    , sf_view_to_grid
    , speed
    , start
    , viewResolution
    )

-- import Snake exposing (Point, Snake)

import Browser.Events as E
import Grid exposing (GridPoint, GridUnit, gridMax, scaleTo)
import Model exposing (..)
import Msg exposing (..)
import Snake2 exposing (Snake, initSnake, updateSnake)
import Task



-- assume grid is 100 x 100 (later scaled to resolution), Grid module will provide functions scaleTo and scaleFrom
-- to convert values to and from 100 x 100 grid from/to it's internal grid representation
-- 0.0025 per 100 grid units per millisecond


speed : GridUnit
speed =
    scaleTo 0.025



-- speed increase per food


accel : GridUnit
accel =
    scaleTo 0.0025


start : GridPoint
start =
    ( scaleTo 30, scaleTo 30 )


idSnake : Snake
idSnake =
    initSnake start



-- grid units


len : GridUnit
len =
    scaleTo 30



-- seconds


foodInterval : Float
foodInterval =
    5


blockWidth : Float
blockWidth =
    1


foodRadius : Float
foodRadius =
    2


foodTolerance : GridUnit
foodTolerance =
    scaleTo 2


growth : GridUnit
growth =
    scaleTo 3


sf_grid_to_view : Float
sf_grid_to_view =
    100 / toFloat gridMax


sf_view_to_grid : Float
sf_view_to_grid =
    1 / sf_grid_to_view


viewResolution : ( Int, Int ) -> ( Int, Int )
viewResolution ( sw, sh ) =
    let
        h =
            100
    in
    ( h * sw // sh, h )


s : Snake
s =
    Maybe.withDefault ( ( 0, 0 ), [] ) <| updateSnake ( scaleTo 60, scaleTo 30 ) len idSnake


resolution : GridUnit
resolution =
    scaleTo 1


init : ( Int, Int ) -> ( Model, Cmd Msg )
init ( width, height ) =
    ( Model s speed len False Up (Grid 0 0 0 0 width height) Nothing
    , Task.perform (\( w, h ) -> Resize h w) (Task.succeed ( width, height ))
    )
