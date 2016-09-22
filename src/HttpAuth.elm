effect module HttpAuth where { command = MyCmd, subscription = MySub } exposing (send, authUrl, urlParser, setToken, configure, Config)

import Http
import Task
import Process
import OAuth
import Navigation
import String
import Dict


-- COMMANDS


type MyCmd msg
    = Send Http.Request (Http.RawError -> msg) (Http.Response -> msg)
    | TokenTask (Task.Task String OAuth.Token)
    | Configure Config


send : Http.Request -> (Http.RawError -> a) -> (Http.Response -> a) -> Cmd a
send request errorTagger responseTagger =
    command (Send request errorTagger responseTagger)


setToken : Task.Task String OAuth.Token -> Cmd msg
setToken task =
    command (TokenTask task)


configure : Config -> Cmd msg
configure config =
    command (Configure config)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f cmd =
    case cmd of
        Send request errorTagger responseTagger ->
            Send request (errorTagger >> f) (responseTagger >> f)

        TokenTask task ->
            TokenTask task

        Configure config ->
            Configure config



-- SUBSCRIPTIONS


type MySub msg
    = AuthUrl (Maybe String -> msg)


authUrl : (Maybe String -> a) -> Sub a
authUrl tagger =
    subscription (AuthUrl tagger)


subMap : (a -> b) -> MySub a -> MySub b
subMap func sub =
    case sub of
        AuthUrl tagger ->
            AuthUrl (tagger >> func)



-- MANAGER


type alias State msg =
    { token : Maybe String
    , queue : List (MyCmd msg)
    , subs : List (MySub msg)
    , config : Maybe Config
    }


type alias Config =
    { authUrl : String, appUrl : String }


init : Task.Task Never (State msg)
init =
    Task.succeed <| State Nothing [] [] Nothing


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
                        ( [ toSelf AuthRequired ]
                            ++ (runCmds nonQueableCmds)
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
    | SetConfig Config


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

        notifyAuthUrlChangeTasks url =
            Task.sequence
                <| List.map
                    (\mySub ->
                        case mySub of
                            AuthUrl tagger ->
                                toApp (tagger url)
                    )
                    state.subs

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

                Configure config ->
                    Process.spawn
                        <| toSelf
                        <| SetConfig config
    in
        case selfMsg of
            SetToken (OAuth.Validated token) ->
                if String.isEmpty token then
                    Task.succeed state
                else
                    (notifyAuthUrlChangeTasks Nothing)
                        `Task.andThen` \_ -> Task.succeed { state | token = Just token }

            RunCmd cmd ->
                runCmd cmd
                    `Task.andThen` \_ -> Task.succeed state

            AuthRequired ->
                case state.config of
                    Nothing ->
                        toSelf AuthRequired
                            `Task.andThen` \_ -> Task.succeed state

                    Just config ->
                        let
                            task =
                                case state.token of
                                    Nothing ->
                                        (notifyAuthUrlChangeTasks
                                            <| Just
                                            <| OAuth.buildAuthUrl
                                            <| uaaAuthClient config
                                        )

                                    Just _ ->
                                        notifyAuthUrlChangeTasks Nothing
                        in
                            task `Task.andThen` \_ -> Task.succeed state

            SetConfig config ->
                Task.succeed { state | config = Just config }


endWith : Task.Task a b -> output -> Task.Task a output
endWith task output =
    Task.map (\_ -> output) task


uaaAuthClient : Config -> OAuth.Client
uaaAuthClient config =
    OAuth.newClient
        { endpointUrl = config.authUrl
        , validateUrl = ""
        }
        { clientId = "dozer-web"
        , scopes = [ "dozer_api.user", "bosh_api.user" ]
        , redirectUrl = config.appUrl
        }


urlParser : Navigation.Parser (Task.Task String OAuth.Token)
urlParser =
    Navigation.makeParser (.hash >> getTokenFromHash >> validateToken)


getTokenFromHash : String -> String
getTokenFromHash s =
    let
        params =
            parseUrlParams s
    in
        Dict.get "access_token" params
            |> Maybe.withDefault ""


parseUrlParams : String -> Dict.Dict String String
parseUrlParams s =
    s
        |> String.dropLeft 1
        |> String.split "&"
        |> List.map parseSingleParam
        |> Dict.fromList


parseSingleParam : String -> ( String, String )
parseSingleParam p =
    let
        s =
            String.split "=" p
    in
        case s of
            [ s1, s2 ] ->
                ( s1, s2 )

            _ ->
                ( "", "" )


validateToken : String -> Task.Task String OAuth.Token
validateToken token =
    Task.succeed (OAuth.Validated token)
