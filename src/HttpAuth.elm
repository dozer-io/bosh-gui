effect module HttpAuth where { command = MyCmd, subscription = MySub } exposing (send, get, userClientInput, urlParser, updateClient, Client(..))

import Http
import HttpAuth.Basic as Basic
import HttpAuth.OAuth as OAuth
import HttpAuth.Util exposing (spawnTask, endWith)
import Navigation
import Task


-- COMMANDS


type MyCmd msg
    = Send Http.Request (Http.RawError -> msg) (Http.Response -> msg)
    | Get String (Http.RawError -> msg) (String -> msg)
    | UpdateClient Client


send : Http.Request -> (Http.RawError -> a) -> (Http.Response -> a) -> Cmd a
send request errorTagger responseTagger =
    command (Send request errorTagger responseTagger)


get : String -> (Http.RawError -> a) -> (String -> a) -> Cmd a
get url errorTagger responseTagger =
    command (Get url errorTagger responseTagger)


updateClient : Client -> Cmd msg
updateClient client =
    command (UpdateClient client)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        Send request errorTagger responseTagger ->
            Send request (errorTagger >> f) (responseTagger >> f)

        Get url errorTagger responseTagger ->
            Get url (errorTagger >> f) (responseTagger >> f)

        UpdateClient client ->
            UpdateClient client



-- SUBSCRIPTIONS


type MySub msg
    = UserClientInput (Client -> msg)


userClientInput : (Client -> a) -> Sub a
userClientInput tagger =
    subscription (UserClientInput tagger)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        UserClientInput tagger ->
            UserClientInput (tagger >> func)



-- MANAGER


type Client
    = OAuth OAuth.OAuthClient
    | Basic Basic.HttpBasicClient


type alias State msg =
    { queue : List (MyCmd msg)
    , subs : List (MySub msg)
    , client : Maybe Client
    , waitingForClientUpdate : Bool
    }


init : Task.Task Never (State msg)
init =
    Task.succeed <| State [] [] Nothing False


onEffects :
    Platform.Router msg (SelfMsg msg)
    -> List (MyCmd msg)
    -> List (MySub msg)
    -> State msg
    -> Task.Task Never (State msg)
onEffects router cmds subs state =
    let
        isQuable cmd =
            case cmd of
                UpdateClient _ ->
                    False

                _ ->
                    True

        ( queableCmds, nonQueableCmds ) =
            List.partition isQuable cmds

        runCmds runnableCmds =
            List.map (RunCmd >> toSelf) runnableCmds

        toSelf =
            Platform.sendToSelf router

        clientRequiresInput client =
            case client of
                OAuth client' ->
                    OAuth.clientRequiresInput client'

                Basic client' ->
                    Basic.clientRequiresInput client'

        ( cmds', queue ) =
            case state.client of
                Nothing ->
                    ( runCmds nonQueableCmds, state.queue ++ queableCmds )

                Just client ->
                    if clientRequiresInput client && not state.waitingForClientUpdate then
                        ( [ toSelf <| AskUserInput client ]
                            ++ (runCmds nonQueableCmds)
                        , state.queue ++ queableCmds
                        )
                    else
                        ( runCmds
                            <| nonQueableCmds
                            ++ queableCmds
                            ++ state.queue
                        , []
                        )
    in
        Task.sequence cmds' `endWith` { state | subs = subs, queue = queue }


type SelfMsg msg
    = RunCmd (MyCmd msg)
    | AskUserInput Client


onSelfMsg :
    Platform.Router msg (SelfMsg msg)
    -> SelfMsg msg
    -> State msg
    -> Task.Task Never (State msg)
onSelfMsg router selfMsg state =
    let
        toApp =
            Platform.sendToApp router

        toSelf =
            Platform.sendToSelf router

        noop =
            Task.succeed ()

        askUserInput client =
            Task.sequence
                <| List.map
                    (\mySub ->
                        case mySub of
                            UserClientInput tagger ->
                                toApp (tagger client)
                    )
                    state.subs

        authHeader =
            case state.client of
                Nothing ->
                    ( "", "" )

                Just client ->
                    case client of
                        OAuth client' ->
                            OAuth.authHeader client'

                        Basic client' ->
                            Basic.authHeader client'

        httpRequest method url =
            Http.Request method [ authHeader ] url Http.empty

        responseToString response =
            case response.value of
                Http.Text string ->
                    string

                _ ->
                    ""
    in
        case selfMsg of
            RunCmd cmd ->
                case cmd of
                    Send request errTagger succTagger ->
                        (spawnTask (toApp << errTagger) (toApp << succTagger)
                            <| Http.send Http.defaultSettings
                                { request
                                    | headers =
                                        request.headers
                                            ++ [ authHeader ]
                                }
                        )
                            `endWith` state

                    Get url errTagger succTagger ->
                        (spawnTask (toApp << errTagger)
                            (toApp << succTagger << responseToString)
                            <| Http.send Http.defaultSettings
                            <| httpRequest "GET" url
                        )
                            `endWith` state

                    UpdateClient client ->
                        let
                            queuedTasks =
                                Task.sequence <| List.map (toSelf << RunCmd) state.queue
                        in
                            queuedTasks
                                `endWith` { state
                                            | client = Just client
                                            , waitingForClientUpdate = False
                                          }

            AskUserInput client ->
                askUserInput client `endWith` { state | waitingForClientUpdate = True }


urlParser : Navigation.Parser String
urlParser =
    Navigation.makeParser (.hash >> OAuth.getTokenFromHash)
