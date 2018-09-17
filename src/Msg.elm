module Msg exposing (Msg(..))

import Grid exposing (GridPoint)
import Model exposing (Direction)
import Time


type Msg
    = KeyDirection (Maybe Direction)
    | Tick Float
    | Restart
    | Resize Int Int
    | FoodTime Time.Posix
    | Food GridPoint
