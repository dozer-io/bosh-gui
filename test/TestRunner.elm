module Main exposing (..)

import ElmTest exposing (..)
import TimeAgoTest
import TaskEventOutputTest


tests : Test
tests =
    suite "All tests"
        [ TimeAgoTest.tests
        , TaskEventOutputTest.tests
        ]


main : Program Never
main =
    runSuiteHtml tests
