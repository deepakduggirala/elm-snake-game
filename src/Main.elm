module Main exposing (main)

import Browser
import Init exposing (..)
import Model exposing (..)
import Msg exposing (..)
import Subscriptions exposing (..)
import Update exposing (..)
import View exposing (..)


main : Program ( Int, Int ) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
