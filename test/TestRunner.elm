module Main exposing (..)

import ElmTest exposing (..)
import TimeAgoTest


tests : Test
tests =
    TimeAgoTest.tests


main : Program Never
main =
    runSuite tests
