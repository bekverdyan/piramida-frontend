module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, input, li, p, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { agents : List Agent
    , createdAgent : Agent
    , agentName : String
    , agentLevel : String
    , brokerId : String
    }


init : Model
init =
    Model [] { id = "", name = "", level = "", brokerId = "" } "" "" ""


type alias Agent =
    { id : String
    , name : String
    , level : String
    , brokerId : String
    }



-- UPDATE


type Msg
    = CreateAgent
    | NameInput String
    | LevelInput String
    | BrokerIDInput String


update : Msg -> Model -> Model
update msg model =
    case msg of
        NameInput name ->
            { model
                | agentName = name
            }

        LevelInput level ->
            { model
                | agentLevel = level
            }

        BrokerIDInput brokerId ->
            { model
                | brokerId = brokerId
            }

        CreateAgent ->
            { model
                | agents = createAgent model.agentName model.agentLevel model.brokerId :: model.agents
                , agentName = ""
                , agentLevel = ""
                , brokerId = ""
            }


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
    div [ style "display" "table" ]
        [ div [ style "display" "table-row" ]
            [ div [ style "display" "table-cell" ] [ ul [] (toHtmlFunction model.agents) ]
            , div [ style "display" "table-cell" ]
                [ div [] [ input [ placeholder "Agent name", value model.agentName, onInput NameInput ] [] ]
                , div []
                    [ input
                        [ placeholder "Agent level"
                        , value model.agentLevel
                        , onInput LevelInput
                        ]
                        []
                    ]
                , div []
                    [ input
                        [ placeholder "Broker ID"
                        , value model.brokerId
                        , onInput BrokerIDInput
                        ]
                        []
                    ]
                , button [ onClick CreateAgent ] [ text "Create" ]
                ]
            ]
        ]


toHtmlFunction : List Agent -> List (Html Msg)
toHtmlFunction agents =
    List.map myHtmlF agents


myHtmlF : Agent -> Html Msg
myHtmlF agent =
    li []
        [ span []
            [ text agent.name
            , text "     "
            , text agent.level
            , text "     "
            , text agent.brokerId
            ]
        ]
