module Main exposing (Model, Msg(..), init, main, update, view)

import Agent exposing (Agent, addAgent, createAgent, pullInput)
import Browser
import Html exposing (Html, button, div, input, span, table, td, text, th, thead, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Task
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = always Sub.none }



-- MODEL


type alias Model =
    { agents : List Agent
    , agentName : Maybe String
    , agentLevel : Maybe String
    , brokerId : Maybe String
    , currentTime : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [] Nothing Nothing Nothing ""
    , Cmd.none
    )



-- UPDATE


type Msg
    = CreateAgent
    | NameInput String
    | LevelInput String
    | BrokerIDInput String
    | RequestTime
    | ReceiveTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NameInput name ->
            ( { model
                | agentName = Just name
              }
            , Cmd.none
            )

        LevelInput level ->
            ( { model
                | agentLevel = Just level
              }
            , Cmd.none
            )

        BrokerIDInput brokerId ->
            ( { model
                | brokerId = Just brokerId
              }
            , Cmd.none
            )

        CreateAgent ->
            ( model
            , Task.perform ReceiveTime Time.now
            )

        RequestTime ->
            ( model, Task.perform ReceiveTime Time.now )

        ReceiveTime newTime ->
            let
                id =
                    String.fromInt (Time.toMillis Time.utc newTime)

                agent =
                    createAgent id model.agentName model.agentLevel model.brokerId
            in
            ( { model
                | agents = addAgent agent model.agents
                , agentName = Nothing
                , agentLevel = Nothing
                , brokerId = Nothing
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "display" "table" ]
        [ div [ style "display" "table-row" ]
            [ div [ style "display" "table-cell" ] [ viewTable model.agents ]
            , div [ style "display" "table-cell" ]
                [ viewAgentDetailInput "Agent name" model.agentName NameInput
                , viewAgentDetailInput "Agent level" model.agentLevel LevelInput
                , viewAgentDetailInput "Broker ID" model.brokerId BrokerIDInput
                , viewButton CreateAgent
                ]
            ]
        ]


viewTable : List Agent -> Html Msg
viewTable agents =
    table [] (viewTableHeader :: toTableRows agents)


viewTableHeader : Html Msg
viewTableHeader =
    thead []
        [ th [] [ text "ID" ]
        , th [] [ text "Name" ]
        , th [] [ text "Level" ]
        , th [] [ text "Broker ID" ]
        ]


toTableRows : List Agent -> List (Html Msg)
toTableRows agents =
    List.map toTableRow agents


toTableRow : Agent -> Html Msg
toTableRow agent =
    tr []
        [ td [] [ text agent.id ]
        , td [] [ text agent.name ]
        , td [] [ text agent.level ]
        , td [] [ text agent.brokerId ]
        ]


viewButton : Msg -> Html Msg
viewButton toMsg =
    div [] [ button [ onClick toMsg ] [ text "Create" ] ]


viewAgentDetailInput : String -> Maybe String -> (String -> Msg) -> Html Msg
viewAgentDetailInput detailName agentDetail toMsg =
    div []
        [ input
            [ placeholder detailName
            , value (pullInput agentDetail)
            , onInput toMsg
            ]
            []
        ]
