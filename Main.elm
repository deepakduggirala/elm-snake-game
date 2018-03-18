import Html exposing (text, div, h2, br, button)
import Keyboard
import AnimationFrame
import Time exposing (Time)
import Svg exposing (svg, rect, g)
import Svg.Attributes exposing (..)
import Html.Events exposing (onClick)

-- MODEL
type alias Position =
  {
    x: Int,
    y: Int
  }

type alias Model = {
  attitude: Direction,
  body: List Position,
  dead: Bool
}

init : ( Model, Cmd Msg )
init = ( Model Up (List.map (\i -> (Position (50-i) 50)) (List.range 0 29) ) False, Cmd.none )

type Msg =
  KeyCode Int
  | Tick Time
  | Restart

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
    Tick time -> (Model model.attitude (updateBody model.attitude model.body) (isDead model), Cmd.none)
    KeyCode k -> let
      dir = toDirection k
    in
      case dir of
        Nothing -> (model, Cmd.none)
        Just d -> if isPerpendicular d model.attitude
          then (Model d (updateBody d model.body) (isDead model), Cmd.none)
          else (model, Cmd.none)
    Restart -> init

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
    h :: t -> (isOnBorder h)  || isHeadOnTail h t

isOnBorder : Position -> Bool
isOnBorder pos =
  not <|
  (0 < pos.x)
  && (pos.x < 100)
  && (0 < pos.y)
  && (pos.y < 100)

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
      div []
        [
          div [style "float: left"]
            [
              svg [ version "1.1", baseProfile "full", width "800",  height "800", viewBox "0 0 100 100", style "border: black 4px solid"]
                [
                  g [ transform "matrix(1,0,0,-1,0,100)" ] (List.map block model.body)
                ]
            ],
          div [style "float: right"] [text <| toString model]
        ]

block : Position -> Html.Html Msg
block pos =
  rect [ x <| toString pos.x, y <| toString pos.y, width "1", height "1", fill "black"] []

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.dead of
    True -> Sub.none
    False ->
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