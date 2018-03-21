module Msg exposing (..)

import Time exposing (Time)
import Snake exposing (Point)

type Msg =
  KeyCode Int
  | Tick Time
  | Restart
  | Resize Int Int
  | FoodTime Time
  | Food Point