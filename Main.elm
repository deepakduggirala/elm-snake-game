import Html exposing (text, div, h2, br)
import Keyboard
import Char

-- MODEL
type alias Model = String

init : ( Model, Cmd Msg )
init = ( "", Cmd.none )

type alias Msg = Maybe Direction

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
    Nothing -> ("Other", Cmd.none)
    Just dir -> (toString dir, Cmd.none)
    

view : Model -> Html.Html Msg
view model =
  div []
    [
      h2 [] [text "Direction"], 
      text model
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.downs toDirection


main : Program Never Model Msg
main =
  Html.program
    {
      init = init,
      update = update,
      view = view,
      subscriptions = subscriptions
    }