import Html exposing (text, div, h2, br)
import Keyboard
import Char
import AnimationFrame
import Time exposing (Time)
import Svg exposing (svg, rect, g)
import Svg.Attributes exposing (..)

-- MODEL
type alias Position =
  {
    x: Int,
    y: Int
  }

type alias Model = Position

init : ( Model, Cmd Msg )
init = ( Position 100 100, Cmd.none )

type Msg =
  KeyCode Int
  | Tick Time

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


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time -> (model, Cmd.none)
    KeyCode k -> let
      dir = toDirection k
    in
      case dir of
        Nothing -> (model, Cmd.none)
        Just d -> (updatePosition d model, Cmd.none)

updatePosition : Direction -> Position -> Position
updatePosition d pos =
  case d of
    Up -> Position pos.x (pos.y + 1)
    Right -> Position (pos.x + 1) pos.y
    Down -> Position pos.x (pos.y - 1)
    Left -> Position (pos.x - 1) pos.y

view : Model -> Html.Html Msg
view model =
  div []
    [
      div [style "float: left"]
        [
          svg [ version "1.1", baseProfile "full", width "600",  height "600", viewBox "0 0 1000 1000", style "border: black 4px solid"]
            [
              g [ transform "matrix(1,0,0,-1,0,1000)" ]
                [
                  block model
                ]
            ]
        ],
      div [style "float: right"] [text <| toString model]
    ]

block : Position -> Html.Html Msg
block pos =
  rect [ x <| toString pos.x, y <| toString pos.y, width "10", height "10", fill "black", stroke "black"] []

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
      Keyboard.downs KeyCode
      -- AnimationFrame.times Tick
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