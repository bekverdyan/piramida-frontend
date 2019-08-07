module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, input, div, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model =
  { agents : List (Maybe Agent)
  , createdAgent : Maybe Agent
  , agentName : String
  , agentLevel : String
  , brokerId : String
  }

init : Model
init =
  Model [] Nothing "" "" ""

type alias Agent =
  { id : String
  , name : String
  , level : String
  , brokerId : String
  }

-- UPDATE

type Msg =
  CreateAgent

update : Msg -> Model -> Model
update msg model =
    case msg of
      CreateAgent ->
        { model
        | createdAgent = Just (createAgent model.agentName model.agentLevel model.brokerId)
        , agents = model.createdAgent :: model.agents }

createAgent : String -> String -> String -> Agent
createAgent name level brokerId =
  { id = "gago"
  , name = name
  , level = level
  , brokerId = brokerId
  }

-- VIEW
view : Model -> Html Msg
view model =
  div [ style "display" "table"]
  [ div [ style "display" "table-row"]
    [ div [ style "display" "table-cell"] [text "fafo"]
    , div [ style "display" "table-cell"]
      [ div [] [input [placeholder "Agent name", value model.agentName] []]
      , div [] [input [placeholder "Agent level", value model.agentLevel] []]
      , div [] [input [placeholder "Broker ID", value model.brokerId] []]
      , button [onClick CreateAgent] [text "Create"]
      ]
    ]
  ]

