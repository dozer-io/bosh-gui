effect module HttpAuth where { command = MyCmd, subscription = MySub } exposing (send, authRequests)

-- import Dict

import Http
import Task
import Process


-- COMMANDS


type MyCmd msg
    = Send Http.Request (Http.RawError -> msg) (Http.Response -> msg)


send : Http.Request -> (Http.RawError -> a) -> (Http.Response -> a) -> Cmd a
send request errorTagger responseTagger =
    command (Send request errorTagger responseTagger)


cmdMap : (a -> b) -> MyCmd a -> MyCmd b
cmdMap f (Send request errorTagger responseTagger) =
    Send request (errorTagger >> f) (responseTagger >> f)



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
    { token :
        String
        -- , queue : List SelfMsg
    , subs : List (MySub msg)
    }


init : Task.Task Never (State msg)
init =
    Task.succeed <| State "foobar" []


onEffects :
    Platform.Router msg (SelfMsg msg)
    -> List (MyCmd msg)
    -> List (MySub msg)
    -> State msg
    -> Task.Task Never (State msg)
onEffects router cmds subs state =
    (Task.sequence <| List.map (RunCmd >> Platform.sendToSelf router) cmds)
        `endWith` { state | subs = state.subs ++ subs }


type SelfMsg msg
    = RunCmd (MyCmd msg)


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
                                { request | headers = [ ( "Authorization", state.token ) ] }
                            )
                        `Task.andThen` \response ->
                                        case response of
                                            Ok succ ->
                                                toApp (responseTagger succ)

                                            Err rawError ->
                                                toApp (errorTagger rawError)
    in
        case selfMsg of
            RunCmd cmd ->
                runCmd cmd
                    `Task.andThen` \_ -> Task.succeed state


endWith : Task.Task a b -> output -> Task.Task a output
endWith task output =
    Task.map (\_ -> output) task
