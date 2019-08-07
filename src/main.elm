module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, input, div, span, text)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { agents : List (Maybe Agent)
  , createdAgent : Maybe Agent
  }

init : Model
init =
  Model [] Nothing

type alias Agent =
  { id : String
  , name : String
  , level : Int
  , brokerId : String
  }

-- UPDATE

type Msg =
  CreateAgent String Int String

update : Msg -> Model -> Model
update msg model =
    case msg of
      CreateAgent name level brokerId ->
        { model
        | createdAgent = Just (createAgent name level brokerId)
        , agents = model.createdAgent :: model.agents }

createAgent : String -> Int -> String -> Agent
createAgent name level brokerId =
  { id = "gago"
  , name = name
  , level = level
  , brokerId = brokerId
  }

-- VIEW
view : Model -> Html Msg
view model =
  div [] [ div [] [text "gag"]
          , div [] [text "sur"]
          ]
