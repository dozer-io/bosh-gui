effect module HttpAuth where { command = MyCmd, subscription = MySub } exposing (send, get, authUrl, urlParser, setToken, configure, Config)

import Dict
import Http
import Navigation
import OAuth
import Process
import String
import Task


-- COMMANDS


type MyCmd msg
    = Send Http.Request (Http.RawError -> msg) (Http.Response -> msg)
    | Get String (Http.RawError -> msg) (String -> msg)
    | TokenTask (Task.Task String OAuth.Token)
    | Configure Config


send : Http.Request -> (Http.RawError -> a) -> (Http.Response -> a) -> Cmd a
send request errorTagger responseTagger =
    command (Send request errorTagger responseTagger)


get : String -> (Http.RawError -> a) -> (String -> a) -> Cmd a
get url errorTagger responseTagger =
    command (Get url errorTagger responseTagger)


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

        Get url errorTagger responseTagger ->
            Get url (errorTagger >> f) (responseTagger >> f)

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

                        Get _ _ _ ->
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

        authHeader =
            ( "Authorization"
            , "bearer " ++ (Maybe.withDefault "" state.token)
            )

        httpRequest method url =
            Http.Request method [ authHeader ] url Http.empty

        handleResult error success result =
            case result of
                Ok value ->
                    success value

                Err err ->
                    error err

        spawnTask error success task =
            Process.spawn
                <| Task.andThen (Task.toResult task)
                <| handleResult error success

        responseToString response =
            case response.value of
                Http.Text string ->
                    string

                _ ->
                    ""

        runCmd cmd =
            case cmd of
                Send request errTagger succTagger ->
                    spawnTask (toApp << errTagger) (toApp << succTagger)
                        <| Http.send Http.defaultSettings
                            { request | headers = request.headers ++ [ authHeader ] }

                Get url errTagger succTagger ->
                    spawnTask (toApp << errTagger) (toApp << succTagger << responseToString)
                        <| Http.send Http.defaultSettings
                        <| httpRequest "GET" url

                TokenTask task ->
                    spawnTask (\_ -> noop) (toSelf << SetToken) task

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
        { clientId = "dozer-web-beta"
        , scopes = [ "dozer_api.user", "bosh_api.user", "bosh.*.admin" ]
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
