import Html exposing (text, div, h2, br, button)
import Keyboard
import AnimationFrame
import Time exposing (Time)
import Svg exposing (svg, rect, g)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)
import Window
import Task exposing (Task)


-- MODEL
type alias Position =
  {
    x: Int,
    y: Int
  }

type alias Grid =
  {
    width: Int,
    height: Int
  }

type alias Model = {
  attitude: Direction,
  body: List Position,
  dead: Bool,
  grid: Grid
}

init : ( Model, Cmd Msg )
init =  ( Model Up (List.map (\i -> (Position (50-i) 50)) (List.range 0 29) ) False (Grid 0 0), Task.perform (\{width, height} -> Resize height width) Window.size )

type Msg =
  KeyCode Int
  | Tick Time
  | Restart
  | Resize Int Int

type Direction =
  Up
  | Right
  | Down
  | Left

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

view : Model -> Html.Html Msg
view model =
  case model.dead of
    True -> div []
      [
        h2 [] [text "Game Over"],
        button [ onClick Restart ] [ text "Restart" ]
      ]
    False ->
      div []  -- style "max-width: calc(100vw - 8px); max-height: calc(100vh - 8px);"
        [
          svg [ version "1.1", baseProfile "full", width "100vw", height "calc(100vh - 4px)", viewBox (getViewBox model.grid), preserveAspectRatio "none"]
            [
              g [ transform "matrix(1,0,0,-1,0,100)" ] (List.map block model.body)
            ]
        ]

block : Position -> Html.Html Msg
block pos =
  rect [ x <| toString pos.x, y <| toString pos.y, width "1", height "1", fill "black"] []

getViewBox : Grid -> String
getViewBox {width, height} = "0 0 " ++ toString width ++ " 100"

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dead of
    True -> Sub.none
    False ->
      Sub.batch [
        Keyboard.downs KeyCode
        , AnimationFrame.times Tick
        , Window.resizes (\{height, width} -> Resize height width)
      ]


main : Program Never Model Msg
main =
  Html.program
    {
      init = init,
      update = update,
      view = view,
      subscriptions = subscriptions
    }