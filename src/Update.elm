module Update exposing (foodGenerator, insideGrid, isDead, nearFood, nextHead, notParallel, update, updateSnake)

import Init exposing (..)
import Math
import Model exposing (..)
import Msg exposing (..)
import Random
import Snake exposing (Point, Snake)
import Time


notParallel : Direction -> Direction -> Bool
notParallel d1 d2 =
  let
    collapse =
      \d ->
        case d of
          Down ->
            Up

          Left ->
            Right
            
          _ ->
            d
  in
  collapse d1 /= collapse d2


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Tick dt ->
      let
        new_model =
          case nearFood model of
            False ->
              { model
                | snake = updateSnake model dt
                , dead = isDead model
              }

            True ->
              { model
                | food = Nothing
                , snake = Snake.grow Init.growth (updateSnake model dt)
                , speed = model.speed + Init.accel
                , dead = isDead model
              }
      in
      ( new_model, Cmd.none )

    KeyDirection k ->
      case k of
        Nothing ->
          ( model, Cmd.none )

        Just d ->
          if notParallel d model.attitude then
            ( { model | attitude = d }, Cmd.none )

          else
            ( model, Cmd.none )
          

    Restart ->
      init (model.grid.actualWidth, model.grid.actualHeight)

    Resize h w ->
      ( { model | grid = Grid (w * 100 // h) 100 w h}, Cmd.none )

    FoodTime _ ->
      ( model, Random.generate Food <| foodGenerator model.grid )

    Food f ->
      ( { model | food = Just f }, Cmd.none )


updateSnake : Model -> Float -> Snake
updateSnake model dt =
  let
    new_head =
      nextHead model.attitude (Snake.head model.snake) model.speed dt
  in
  Snake.cons new_head model.snake


foodGenerator : Grid -> Random.Generator Point
foodGenerator { width, height } =
  Random.pair (Random.float 0 <| toFloat width) (Random.float 0 <| toFloat height)


nextHead : Direction -> Maybe Point -> Float -> Float -> Point
nextHead d mHead speed dt =
  case mHead of
    Nothing ->
      Init.start

    Just ( x, y ) ->
      let
        ds =
          dt * speed

        s2 =
          sqrt 2
      in
      case d of
        Up ->
          ( x, y + ds )

        Right ->
          ( x + ds, y )

        Down ->
          ( x, y - ds )

        Left ->
          ( x - ds, y )


isDead : Model -> Bool
isDead model =
  case Snake.coords 0 model.snake of
    Nothing ->
      True

    Just head ->
      insideGrid model.grid head
        || Snake.isHeadOnTail 0.01 model.snake


insideGrid : Grid -> Point -> Bool
insideGrid grid ( x, y ) =
  not <|
    (0 < x)
      && (x < toFloat grid.width)
      && (0 < y)
      && (y < toFloat grid.height)


nearFood : Model -> Bool
nearFood model =
  case ( Snake.head model.snake, model.food ) of
    ( Just h, Just f ) ->
      Math.withInTol Init.foodTol <| Math.distance h f

    ( _, _ ) ->
      False
