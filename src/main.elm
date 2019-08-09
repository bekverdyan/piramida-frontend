module Main exposing (Model, Msg(..), init, main, update, view)

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


addAgent : Maybe Agent -> List Agent -> List Agent
addAgent agent agents =
    case agent of
        Just validAgent ->
            validAgent :: agents

        _ ->
            agents


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


from : String -> String -> String -> String -> Agent
from id name level brokerId =
    { id = id
    , name = name
    , level = level
    , brokerId = brokerId
    }


createAgent : String -> Maybe String -> Maybe String -> Maybe String -> Maybe Agent
createAgent id name level brokerId =
    if provided name && provided level && provided brokerId then
        Just (from id (pullInput name) (pullInput level) (pullInput brokerId))

    else
        Nothing



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
