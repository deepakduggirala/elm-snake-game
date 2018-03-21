module Update exposing (..)

import Keyboard
import Time

import Msg exposing (..)
import Model exposing (..)
import Snake exposing (Snake, Point)
import Init exposing (..)

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
  let collapse = \d -> case d of
    Down -> Up
    Left -> Right
    _ -> d
  in
    collapse d1 /= collapse d2

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick dt ->
      (
        { model |
          snake =
            let
              new_head = nextHead model.attitude (Snake.head model.snake) model.speed dt
            in
              Snake.cons new_head model.snake
          , dead = isDead model
        }
        , Cmd.none
      )
    KeyCode k ->
      let
        dir = toDirection k
      in
        case dir of
          Nothing -> (model, Cmd.none)
          Just d -> if isPerpendicular d model.attitude
            then ( { model | attitude = d }, Cmd.none )
            else (model, Cmd.none)
    Restart -> init
    Resize h w -> ( { model | grid = Grid (w*100 // h) 100 }, Cmd.none )

nextHead : Direction -> Maybe Point -> Float -> Time.Time -> Point
nextHead d mHead speed dt =
  case mHead of
    Nothing -> Init.start
    Just (x,y) ->
      let
        ds = Time.inMilliseconds dt * speed
      in
        case d of
          Up -> (,) x (y + ds)
          Right -> (,) (x + ds) y
          Down -> (,) x (y - ds)
          Left -> (,) (x - ds) y

isDead : Model -> Bool
isDead model =
  case Snake.coords 0 model.snake of
    Nothing -> True
    Just head ->
      insideGrid model.grid head
      || Snake.isHeadOnTail model.snake

insideGrid : Grid -> Point -> Bool
insideGrid grid (x,y) =
  not <|
  (0 < x)
  && (x < toFloat grid.width)
  && (0 < y)
  && (y < toFloat grid.height)

-- isHeadOnTail : Position -> List Position -> Bool
-- isHeadOnTail h t = not <| List.isEmpty <| List.filter ((==) h) t