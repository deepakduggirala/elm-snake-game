module Msg exposing (Msg(..))

import Snake exposing (Point)
import Time
import Model exposing (Direction)


type Msg
  = KeyDirection (Maybe Direction)
  | Tick Float
  | Restart
  | Resize Int Int
  | FoodTime Time.Posix
  | Food Point
