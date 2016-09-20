effect module HttpAuth where { command = MyCmd } exposing (send)

-- import Dict

import Http
import Task
import Process


type MyCmd msg
    = Send Http.Request (Http.RawError -> msg) (Http.Response -> msg)


send : Http.Request -> (Http.RawError -> a) -> (Http.Response -> a) -> Cmd a
send request errorTagger responseTagger =
    command (Send request errorTagger responseTagger)


cmdMap : (a -> b) -> (a -> b) -> MyCmd a -> MyCmd b
cmdMap f g (Send request errorTagger responseTagger) =
    Send request (errorTagger >> f) (responseTagger >> g)


type alias State =
    String


init : Task.Task Never State
init =
    Task.succeed "foobar"


onEffects : Platform.Router msg (SelfMsg msg) -> List (MyCmd msg) -> State -> Task.Task Never State
onEffects router cmds state =
    (Task.sequence <| List.map (RunCmd >> Platform.sendToSelf router) cmds)
        `endWith` state


type SelfMsg msg
    = RunCmd (MyCmd msg)


onSelfMsg : Platform.Router msg (SelfMsg msg) -> SelfMsg msg -> State -> Task.Task Never State
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
                                { request | headers = [ ( "Authorization", state ) ] }
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
