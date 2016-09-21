effect module HttpAuth where { command = MyCmd, subscription = MySub } exposing (send, authRequests, urlParser, setToken)

import Http
import Task
import Process
import OAuth
import Navigation


-- COMMANDS


type MyCmd msg
    = Send Http.Request (Http.RawError -> msg) (Http.Response -> msg)
    | TokenTask (Task.Task String OAuth.Token)


send : Http.Request -> (Http.RawError -> a) -> (Http.Response -> a) -> Cmd a
send request errorTagger responseTagger =
    command (Send request errorTagger responseTagger)


setToken : Task.Task String OAuth.Token -> Cmd msg
setToken task =
    command (TokenTask task)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        Send request errorTagger responseTagger ->
            Send request (errorTagger >> f) (responseTagger >> f)

        TokenTask task ->
            TokenTask task



-- SUBSCRIPTIONS


type MySub msg
    = AuthRequest (String -> msg)


authRequests : (String -> a) -> Sub a
authRequests tagger =
    subscription (AuthRequest tagger)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        AuthRequest tagger ->
            AuthRequest (tagger >> func)



-- MANAGER


type alias State msg =
    { token : Maybe String
    , queue : List (MyCmd msg)
    , subs : List (MySub msg)
    }


init : Task.Task Never (State msg)
init =
    Task.succeed <| State Nothing [] []


onEffects :
    Platform.Router msg (SelfMsg msg)
    -> List (MyCmd msg)
    -> List (MySub msg)
    -> State msg
    -> Task.Task Never (State msg)
onEffects router cmds subs state =
    let
        ( queableCmds, nonQueableCmds ) =
            List.partition
                (\x ->
                    case x of
                        Send _ _ _ ->
                            True

                        _ ->
                            False
                )
                cmds

        runCmds runnableCmds =
            List.map (RunCmd >> toSelf) runnableCmds

        toSelf =
            Platform.sendToSelf router

        ( cmds', queue ) =
            case state.token of
                Nothing ->
                    if List.isEmpty queableCmds then
                        ( runCmds nonQueableCmds, state.queue )
                    else
                        ( (runCmds nonQueableCmds) ++ [ toSelf AuthRequired ]
                        , state.queue ++ queableCmds
                        )

                Just _ ->
                    ( runCmds <| nonQueableCmds ++ queableCmds ++ state.queue, [] )
    in
        Task.sequence cmds' `endWith` { state | subs = subs, queue = queue }


type SelfMsg msg
    = RunCmd (MyCmd msg)
    | AuthRequired
    | SetToken OAuth.Token


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

        runCmd cmd =
            case cmd of
                Send request errorTagger responseTagger ->
                    Process.spawn
                        <| Task.toResult
                            (Http.send Http.defaultSettings
                                { request | headers = [ ( "Authorization", Maybe.withDefault "" state.token ) ] }
                            )
                        `Task.andThen` \response ->
                                        case response of
                                            Ok succ ->
                                                toApp (responseTagger succ)

                                            Err rawError ->
                                                toApp (errorTagger rawError)

                TokenTask task ->
                    Process.spawn
                        <| Task.toResult task
                        `Task.andThen` \result ->
                                        case result of
                                            Ok token ->
                                                toSelf (SetToken token)

                                            Err _ ->
                                                noop
    in
        case selfMsg of
            SetToken (OAuth.Validated token) ->
                Task.succeed { state | token = Just token }

            RunCmd cmd ->
                runCmd cmd
                    `Task.andThen` \_ -> Task.succeed state

            AuthRequired ->
                Task.sequence
                    (List.map
                        (\mySub ->
                            case mySub of
                                AuthRequest tagger ->
                                    toApp
                                        (tagger
                                            <| OAuth.buildAuthUrl uaaAuthClient
                                        )
                        )
                        state.subs
                    )
                    `Task.andThen` \_ -> Task.succeed state


endWith : Task.Task a b -> output -> Task.Task a output
endWith task output =
    Task.map (\_ -> output) task


uaaAuthClient : OAuth.Client
uaaAuthClient =
    OAuth.newClient
        { endpointUrl = "http://localhost:8001/login/authorize"
        , validateUrl = ""
        }
        { clientId = "dozer-web"
        , scopes = [ "dozer_api.user", "bosh_api.user" ]
        , redirectUrl = "http://localhost:8080"
        }


urlParser : Navigation.Parser (Task.Task String OAuth.Token)
urlParser =
    OAuth.urlParser uaaAuthClient
