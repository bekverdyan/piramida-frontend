module Agent exposing (Agent, addAgent, createAgent, pullInput)


type alias Agent =
    { id : String
    , name : String
    , level : String
    , brokerId : String
    }


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
