module TimeAgo exposing (timeAgo)

import Time exposing (Time, second, minute, hour)
import String.Extra exposing (pluralize)


type alias Range =
    { limit : Time
    , interval : Time
    , singular : String
    , plural : String
    }


day =
    hour * 24


month =
    day * 30


year =
    day * 365


ranges =
    [ Range (minute - second) second "second" "seconds"
    , Range (hour - second) minute "minute" "minutes"
    , Range (day - second) hour "hour" "hours"
    , Range (month - second) day "day" "days"
    , Range (year - second) month "month" "months"
    ]


timeAgo : Time -> Time -> String
timeAgo now time =
    let
        ago =
            now - time
    in
        if ago < (59 * second) then
            "just now"
        else
            case Maybe.oneOf <| List.map (applyRange ago) ranges of
                Just string ->
                    string

                Nothing ->
                    toAgoString "year" "years" ago year


applyRange : Time -> Range -> Maybe String
applyRange ago range =
    if ago < range.limit then
        Just <| toAgoString range.singular range.plural ago range.interval
    else
        Nothing


toAgoString : String -> String -> Time -> Time -> String
toAgoString singular plural ago interval =
    (pluralize singular plural <| floor <| ago / interval) ++ " ago"
