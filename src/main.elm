module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, button, div, input, li, p, span, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { agents : List (Maybe Agent)
    , agentName : Maybe String
    , agentLevel : Maybe String
    , brokerId : Maybe String
    }


init : Model
init =
    Model [] Nothing Nothing Nothing


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
                | agentName = Just name
            }

        LevelInput level ->
            { model
                | agentLevel = Just level
            }

        BrokerIDInput brokerId ->
            { model
                | brokerId = Just brokerId
            }

        CreateAgent ->
            { model
                | agents =
                    if provided model.agentName && provided model.agentLevel && provided model.brokerId then
                        createAgent model.agentName model.agentLevel model.brokerId :: model.agents

                    else
                        model.agents
                , agentName = Nothing
                , agentLevel = Nothing
                , brokerId = Nothing
            }


provided : Maybe String -> Bool
provided agentDetail =
    case agentDetail of
        Just _ ->
            True

        Nothing ->
            False


pullInput : Maybe String -> String
pullInput userInput =
    case userInput of
        Just name ->
            name

        Nothing ->
            ""


from : String -> String -> String -> Agent
from name level brokerId =
    { id = "gg"
    , name = name
    , level = level
    , brokerId = brokerId
    }


createAgent : Maybe String -> Maybe String -> Maybe String -> Maybe Agent
createAgent name level brokerId =
    if provided name && provided level && provided brokerId then
        Just (from (pullInput name) (pullInput level) (pullInput brokerId))

    else
        Nothing



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "display" "table" ]
        [ div [ style "display" "table-row" ]
            [ div [ style "display" "table-cell" ] [ ul [] (toHtmlFunction model.agents) ]
            , div [ style "display" "table-cell" ]
                [ viewConverter "Agent name" model.agentName NameInput
                , viewConverter "Agent level" model.agentLevel LevelInput
                , viewConverter "Broker ID" model.brokerId BrokerIDInput
                , viewButton CreateAgent
                ]
            ]
        ]


viewButton : Msg -> Html Msg
viewButton toMsg =
    div [] [ button [ onClick toMsg ] [ text "Create" ] ]


viewConverter : String -> Maybe String -> (String -> Msg) -> Html Msg
viewConverter detailName agentDetail toMsg =
    div []
        [ input
            [ placeholder detailName
            , value (pullInput agentDetail)
            , onInput toMsg
            ]
            []
        ]



-- div []
--     [ span []
--         [ input
--             [ value convertible
--             , onInput toMsg
--             , style "border-color" (drawConvertible converted)
--             ]
--             []
--         , text measurment.convertibleSymbol
--         , span [ style "color" (drawConverted converted) ] [ text (toString converted) ]
--         , text measurment.convertedSymbol
--         ]
--     ]


toHtmlFunction : List (Maybe Agent) -> List (Html Msg)
toHtmlFunction agents =
    List.map myHtmlF agents


myHtmlF : Maybe Agent -> Html Msg
myHtmlF msg =
    case msg of
        Just agent ->
            li []
                [ span []
                    [ text agent.name
                    , text "     "
                    , text agent.level
                    , text "     "
                    , text agent.brokerId
                    ]
                ]

        Nothing ->
            li [] []
