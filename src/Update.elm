module Update exposing (..)

import Keyboard
import Time
import Random

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
    81 -> Just UL
    113 -> Just UL
    87 -> Just UR
    119 -> Just UR
    65 -> Just DL
    97 -> Just DL
    83 -> Just DR
    115 -> Just DR
    _ -> Nothing

notParallel : Direction -> Direction -> Bool
notParallel d1 d2 =
  let collapse = \d -> case d of
    Down -> Up
    Left -> Right
    DL -> UR
    UL -> DR
    _ -> d
  in
    collapse d1 /= collapse d2

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick dt ->
      let
        new_model =
        case nearFood model of
          False ->
            { model |
              snake = updateSnake model dt
              , dead = isDead model
            }
          True ->
            { model |
              food = Nothing
              ,snake = Snake.grow Init.growth (updateSnake model dt)
              ,speed = model.speed + Init.accel
              , dead = isDead model
            }
      in
        (new_model, Cmd.none)
    KeyCode k ->
      let
        dir = toDirection k
      in
        case dir of
          Nothing -> (model, Cmd.none)
          Just d -> if notParallel d model.attitude
            then ( { model | attitude = d }, Cmd.none )
            else (model, Cmd.none)
    Restart -> init
    Resize h w -> ( { model | grid = Grid (w*100 // h) 100 }, Cmd.none )
    FoodTime _ -> (model, Random.generate Food <| foodGenerator model.grid)
    Food f -> ( { model | food = Just f}, Cmd.none )

updateSnake : Model -> Time.Time -> Snake
updateSnake model dt =
  let
    new_head = nextHead model.attitude (Snake.head model.snake) model.speed dt
  in
    Snake.cons new_head model.snake

foodGenerator : Grid -> Random.Generator Point
foodGenerator {width, height} = Random.pair (Random.float 0 <| toFloat width) (Random.float 0 <| toFloat height)

nextHead : Direction -> Maybe Point -> Float -> Time.Time -> Point
nextHead d mHead speed dt =
  case mHead of
    Nothing -> Init.start
    Just (x,y) ->
      let
        ds = Time.inMilliseconds dt * speed
        s2 = sqrt 2
      in
        case d of
          Up -> (,) x (y + ds)
          Right -> (,) (x + ds) y
          Down -> (,) x (y - ds)
          Left -> (,) (x - ds) y
          UR -> (,) (x + ds/s2) (y + ds/s2)
          UL -> (,) (x - ds/s2) (y + ds/s2)
          DR -> (,) (x + ds/s2) (y - ds/s2)
          DL -> (,) (x - ds/s2) (y - ds/s2)

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

nearFood: Model -> Bool
nearFood model =
  case (Snake.head model.snake, model.food) of
    (Just h, Just f) -> Snake.withInTol 1 <| Snake.distance h f
    (_, _) -> False