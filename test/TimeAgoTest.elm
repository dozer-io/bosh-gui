module TimeAgoTest exposing (..)

import ElmTestBDDStyle exposing (..)
import TimeAgo exposing (..)
import Time exposing (..)


tests : Test
tests =
    let
        now =
            1468832139 * second

        time ago =
            now - ago
    in
        describe "#timeAgo"
            [ it "returns just now"
                <| expect (timeAgo now (time 0)) toBe "just now"
            , it "returns just now"
                <| expect (timeAgo now (time (30 * second))) toBe "just now"
            , it "returns 1 minute ago"
                <| expect (timeAgo now (time (minute))) toBe "1 minute ago"
            , it "returns 2 minute ago"
                <| expect (timeAgo now (time (2 * minute))) toBe "2 minutes ago"
            , it "returns 1 hour ago"
                <| expect (timeAgo now (time (hour))) toBe "1 hour ago"
            , it "returns 2 hours ago"
                <| expect (timeAgo now (time (2 * hour))) toBe "2 hours ago"
            , it "returns 1 day ago"
                <| expect (timeAgo now (time (hour * 24))) toBe "1 day ago"
            , it "returns 2 days ago"
                <| expect (timeAgo now (time (hour * 24 * 2))) toBe "2 days ago"
            , it "returns 1 year ago"
                <| expect (timeAgo now (time (hour * 24 * 365))) toBe "1 year ago"
            , it "returns 2 years ago"
                <| expect (timeAgo now (time (hour * 24 * 365 * 2))) toBe "2 years ago"
            ]
