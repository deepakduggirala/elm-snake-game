module Model exposing (Direction(..), Grid, Model)

import Grid exposing (GridPoint, GridUnit)
import Random
import Snake2 exposing (Snake)



-- MODEL


type alias Grid =
    { viewWidth : Int
    , viewHeight : Int
    , gridWidth : Int
    , gridHeight : Int
    , screenWidth : Int
    , screenHeight : Int
    }


type alias Model =
    { snake : Snake
    , speed : GridUnit
    , length : GridUnit
    , dead : Bool
    , attitude : Direction
    , grid : Grid
    , food : Maybe GridPoint
    }


type Direction
    = Up
    | Right
    | Down
    | Left
