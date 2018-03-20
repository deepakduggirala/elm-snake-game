module Update exposing (..)

import Keyboard

import Msg exposing (..)
import Model exposing (..)

toDirection : Keyboard.KeyCode -> Maybe Direction
toDirection k =
  case k of
    37 -> Just Left
    38 -> Just Up
    39 -> Just Right
    40 -> Just Down
    _ -> Nothing

isPerpendicular : Direction -> Direction -> Bool
isPerpendicular d1 d2 =
  if d1 == d2
  then False
  else let collapse = \d -> case d of
    Down -> Up
    Left -> Right
    _ -> d
  in
    collapse d1 /= collapse d2

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time -> ( { model | body = updateBody model.attitude model.body, dead = isDead model } , Cmd.none)
    KeyCode k -> let
      dir = toDirection k
    in
      case dir of
        Nothing -> (model, Cmd.none)
        Just d -> if isPerpendicular d model.attitude
          then ( { model | attitude = d, body = updateBody d model.body, dead = isDead model } , Cmd.none)
          else (model, Cmd.none)
    Restart -> init
    Resize h w -> ( { model | grid = Grid (w*100 // h) 100 }, Cmd.none )

updatePosition : Direction -> Position -> Position
updatePosition d pos =
  case d of
    Up -> Position pos.x (pos.y + 1)
    Right -> Position (pos.x + 1) pos.y
    Down -> Position pos.x (pos.y - 1)
    Left -> Position (pos.x - 1) pos.y

initList : List a -> List a
initList xs =
  case xs of
    [] -> []
    [x] -> []
    h::t -> h :: ( initList t )

updateBody : Direction -> List Position -> List Position
updateBody d body =
  case body of
    [] -> []
    h :: _ -> (updatePosition d h) :: (initList body)

isDead : Model -> Bool
isDead model =
  case model.body of
    [] -> True
    h :: t -> (isOnBorder model.grid h)  || isHeadOnTail h t

isOnBorder : Grid -> Position -> Bool
isOnBorder grid pos =
  not <|
  (0 < pos.x)
  && (pos.x < grid.width)
  && (0 < pos.y)
  && (pos.y < grid.height)

isHeadOnTail : Position -> List Position -> Bool
isHeadOnTail h t = not <| List.isEmpty <| List.filter ((==) h) t