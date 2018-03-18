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

type alias Model = {
  attitude: Direction,
  body: List Position
}

init : ( Model, Cmd Msg )
init = ( Model Up (List.map (\i -> (Position (50-i) 50)) (List.range 0 29) ), Cmd.none )

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
    Tick time -> ({ model |
          body = updateBody model.attitude model.body,
          attitude = model.attitude
        }, Cmd.none)
    KeyCode k -> let
      dir = toDirection k
    in
      case dir of
        Nothing -> (model, Cmd.none)
        Just d -> ({ model |
          body = updateBody d model.body,
          attitude = d
        }, Cmd.none)

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

view : Model -> Html.Html Msg
view model =
  div []
    [
      div [style "float: left"]
        [
          svg [ version "1.1", baseProfile "full", width "600",  height "600", viewBox "0 0 100 100", style "border: black 4px solid"]
            [
              g [ transform "matrix(1,0,0,-1,0,100)" ] (List.map block model.body)
            ]
        ],
      div [style "float: right"] [text <| toString model]
    ]

block : Position -> Html.Html Msg
block pos =
  rect [ x <| toString pos.x, y <| toString pos.y, width "1", height "1", fill "black", stroke "black"] []

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [
      Keyboard.downs KeyCode,
      AnimationFrame.times Tick
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