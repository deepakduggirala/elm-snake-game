import Html

import Update exposing (..)
import Model exposing (..)
import Msg exposing (..)
import View exposing (..)
import Subscriptions exposing (..)
import Init exposing (..)




main : Program Never Model Msg
main =
  Html.program
    {
      init = init,
      update = update,
      view = view,
      subscriptions = subscriptions
    }