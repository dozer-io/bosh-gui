module TaskEventOutputTest exposing (..)

import ElmTestBDDStyle exposing (..)
import TaskEventOutput exposing (eventsToStages, Event, Stage, Task, State(..))
import Time exposing (second)


tests : Test
tests =
    let
        events =
            [ Event (2 * second) "Packages" [ "redis" ] "accept/a52b9ab" Started ""
            , Event (3 * second) "Packages" [ "redis" ] "accept/a52b9ab" Finished ""
            , Event (4 * second) "Packages" [ "redis" ] "redis/d1b927ac" Started ""
            , Event (4 * second) "Packages" [ "redis" ] "redis/d1b927ac" Failed "error"
            ]
    in
        describe "#eventsToStages"
            [ it "converts events into stages and tasks"
                <| expect (eventsToStages events)
                    toBe
                    [ Stage "Packages"
                        [ Task (2 * second) (1 * second) [ "redis" ] "accept/a52b9ab" Finished ""
                        , Task (4 * second) 0 [ "redis" ] "redis/d1b927ac" Failed "error"
                        ]
                    ]
            ]
