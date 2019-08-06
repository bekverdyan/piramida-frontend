module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  String

init : Model
init =
  "suro"

-- UPDATE

type Msg =
  Received

update : Msg -> Model -> Model
update msg model =
    case msg of
      Received ->
        model ++ "gago"


-- VIEW
view : Model -> Html Msg
view model =
  div []
  [text model]
