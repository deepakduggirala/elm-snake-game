module Update exposing (foodGenerator, insideGrid, isDead, nearFood, nextHead, notParallel, update)

import Grid exposing (GridPoint, GridUnit, distance, gridResolution, origin, translate)
import Init exposing (..)
import Maybe
import Model exposing (..)
import Msg exposing (..)
import Random
import Snake2 exposing (Snake, coords, head, updateSnake)
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
                nf =
                    nearFood model

                new_head =
                    nextHead model.attitude (head model.snake) model.speed dt nf

                ( invalid, new_snake ) =
                    extractSnake <| updateSnake new_head model.length model.snake

                new_model =
                    case nf of
                        False ->
                            { model
                                | snake = new_snake
                                , dead = invalid || isDead model
                            }

                        True ->
                            { model
                                | food = Nothing
                                , snake = new_snake
                                , length = model.length + Init.growth
                                , speed = model.speed + Init.accel
                                , dead = invalid || isDead model
                            }
            in
            ( new_model, Cmd.none )

        KeyDirection k ->
            -- use maybe default and map
            case k of
                Nothing ->
                    ( model, Cmd.none )

                Just d ->
                    if notParallel d model.attitude then
                        ( { model | attitude = d }, Cmd.none )

                    else
                        ( model, Cmd.none )

        Restart ->
            init ( model.grid.screenWidth, model.grid.screenHeight )

        Resize h w ->
            let
                ( gw, gh ) =
                    gridResolution ( w, h )

                ( vw, vh ) =
                    viewResolution ( w, h )
            in
            ( { model | grid = Grid vw vh gw gh w h }, Cmd.none )

        FoodTime _ ->
            ( model, Random.generate Food <| foodGenerator model.grid )

        Food f ->
            ( { model | food = Just f }, Cmd.none )


extractSnake : Maybe Snake -> ( Bool, Snake )
extractSnake msnake =
    case msnake of
        Nothing ->
            ( True, Init.idSnake )

        Just s ->
            ( False, s )


foodGenerator : Grid -> Random.Generator GridPoint
foodGenerator { gridWidth, gridHeight } =
    Random.pair (Random.int 0 gridWidth) (Random.int 0 gridHeight)


nextHead : Direction -> GridPoint -> GridUnit -> Float -> Bool -> GridPoint
nextHead d h speed dt nf =
    let
        ds =
            if nf then
                (truncate dt * speed) + growth

            else
                truncate dt * speed
    in
    case d of
        Up ->
            translate h ( origin, ( 0, ds ) )

        Right ->
            translate h ( origin, ( ds, 0 ) )

        Down ->
            translate h ( origin, ( 0, -ds ) )

        Left ->
            translate h ( origin, ( -ds, 0 ) )


isDead : Model -> Bool
isDead model =
    insideGrid model.grid (head model.snake)


insideGrid : Grid -> GridPoint -> Bool
insideGrid grid ( x, y ) =
    not <|
        (0 < x)
            && (x < grid.gridWidth)
            && (0 < y)
            && (y < grid.gridHeight)


nearFood : Model -> Bool
nearFood model =
    case model.food of
        Nothing ->
            False

        Just f ->
            let
                d =
                    distance (head model.snake) f
            in
            d <= Init.foodTolerance
