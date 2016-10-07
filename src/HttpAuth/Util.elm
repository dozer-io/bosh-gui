module HttpAuth.Util exposing (spawnTask, endWith)

import Process
import Task


spawnTask :
    (a -> Task.Task b c)
    -> (d -> Task.Task b c)
    -> Task.Task a d
    -> Task.Task e Process.Id
spawnTask error success task =
    let
        handleResult error success result =
            case result of
                Ok value ->
                    success value

                Err err ->
                    error err
    in
        Process.spawn
            <| Task.andThen (Task.toResult task)
            <| handleResult error success


endWith : Task.Task a b -> output -> Task.Task a output
endWith task output =
    Task.map (\_ -> output) task
