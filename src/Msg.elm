module Msg exposing (..)

import Time exposing (Time)

type Msg =
  KeyCode Int
  | Tick Time
  | Restart
  | Resize Int Int