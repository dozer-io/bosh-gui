module TaskEventOutput exposing (..)

import Html exposing (..)


-- import Html.Events exposing (onClick)

import Platform.Cmd exposing (Cmd)
import Material


-- import Material.Button as Button exposing (..)
-- import Material.Icon as Icon
-- import Material.Color as Color exposing (color, Hue(..), Shade(..), background, white)
-- import Material.Options exposing (styled', nop, cs)

import Html.App as App


-- import Html.Attributes exposing (class, style)

import Json.Decode exposing (int, string, list, float, oneOf, null, Decoder, decodeString)
import Json.Decode.Pipeline exposing (decode, required, optional)
import List.Extra exposing (groupWhile)
import String
import Time exposing (..)


-- import TimeAgo exposing (timeAgo)

import Task exposing (perform)
import Http exposing (getString)


main : Program Never
main =
    App.program
        { init = init 65
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Mdl =
    Material.Model


type alias Model =
    { mdl : Mdl, stages : List Stage, loading : Bool, taskId : Int, now : Time }


type alias Stage =
    { name : String, tasks : List Task }


type alias Task =
    { start : Time
    , duration : Time
    , tags : List String
    , description : String
    , state : State
    , error : String
    }


type alias Event =
    { time : Time
    , stage : String
    , tags : List String
    , task : String
    , state : State
    , error : String
    }


type State
    = InProgress
    | Started
    | Finished
    | Failed
    | Unknown


decodeEvent : Decoder Event
decodeEvent =
    decode Event
        |> required "time" time
        |> required "stage" string
        |> required "tags" (list string)
        |> required "task" string
        |> required "state" state
        |> optional "error" string ""


state : Decoder State
state =
    let
        decodeState state =
            case state of
                "in_progress" ->
                    Json.Decode.succeed InProgress

                "started" ->
                    Json.Decode.succeed Started

                "finished" ->
                    Json.Decode.succeed Finished

                "failed" ->
                    Json.Decode.succeed Failed

                _ ->
                    Json.Decode.succeed Unknown
    in
        string `Json.Decode.andThen` decodeState


time : Decoder Time
time =
    float `Json.Decode.andThen` \second -> Json.Decode.succeed <| Time.second * second


init : Int -> ( Model, Cmd Msg )
init taskId =
    ( Model Material.model [] True taskId (second * 0)
    , Cmd.batch [ Task.perform Tick Tick Time.now, getTaskEventOutput taskId ]
    )



-- MESSAGE, UPDATE


type Msg
    = Mdl Material.Msg
    | Tick Time.Time
    | GetTaskEventOutputSucceed String
    | GetTaskEventOutputFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg ->
            Material.update Mdl msg model

        Tick now ->
            ( { model | now = now }, Cmd.none )

        GetTaskEventOutputFail _ ->
            ( model, getTaskEventOutput model.taskId )

        GetTaskEventOutputSucceed rawEvents ->
            let
                stages =
                    eventsToStages
                        <| Result.withDefault []
                        <| decodeEvents rawEvents
            in
                ( { model | stages = stages }, Cmd.none )


eventsToStages : List Event -> List Stage
eventsToStages events =
    let
        nullEvent =
            Event 0 "" [] "" Unknown ""

        firstEvent eventList =
            Maybe.withDefault nullEvent
                <| List.head eventList

        lastEvent eventList =
            Maybe.withDefault nullEvent
                <| Maybe.oneOf
                    [ List.head <| List.drop 1 eventList
                    , List.head eventList
                    ]

        firstTask taskList =
            Maybe.withDefault [ nullEvent ]
                <| List.head taskList

        groupByStage x y =
            (firstEvent x).stage == (firstEvent y).stage

        groupByTask x y =
            x.task == y.task

        toStage taskList =
            Stage (firstEvent <| firstTask taskList).stage
                <| List.map toTask taskList

        toTask eventList =
            let
                first =
                    firstEvent eventList

                last =
                    lastEvent eventList
            in
                Task first.time (last.time - first.time) first.tags first.task last.state last.error
    in
        List.map toStage
            <| groupWhile groupByStage
            <| groupWhile groupByTask events



-- VIEW


view : Model -> Html Msg
view model =
    let
        stageView stage =
            div []
                [ h3 [] [ text stage.name ]
                , ol [] <| List.map taskView stage.tasks
                ]

        taskView task =
            li [] [ text task.description ]
    in
        div [] <| List.map stageView model.stages



-- styled' li
--     [ cs "mdl-list__item mdl-list__item--three-line", selectedStyle model.selected ]
--     [ Html.Events.onClick Select ]
--     [ span [ class "mdl-list__item-primary-content" ]
--         [ i [ class "material-icons mdl-list__item-avatar" ]
--             [ text "person" ]
--         , span []
--             [ text <| "#" ++ (toString model.activity.id) ++ " " ++ model.activity.description
--             ]
--         , span [ class "mdl-list__item-text-body", style [ ( "whiteSpace", "nowrap" ) ] ]
--             [ text <| String.slice 0 65 model.activity.result
--             , br [] []
--             , b []
--                 [ text
--                     <| model.activity.state
--                     ++ " "
--                     ++ (timeAgo model.now model.activity.timestamp)
--                     ++ " by "
--                     ++ model.activity.user
--                 ]
--             ]
--         ]
--     , span [ class "mdl-list__item-secondary-action" ]
--         [ Button.render Mdl
--             [ 0 ]
--             model.mdl
--             [ Button.icon
--             , Button.ripple
--             , Button.onClick Select
--             ]
--             [ Icon.i "keyboard_arrow_right" ]
--         ]
--     ]
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.second) Tick



-- HTTP


getTaskEventOutput : Int -> Cmd Msg
getTaskEventOutput taskId =
    let
        url =
            "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000/tasks/"
                ++ toString taskId
                ++ "/output?type=event"
    in
        perform GetTaskEventOutputFail GetTaskEventOutputSucceed <| getString url



-- DECODE


decodeEvents : String -> Result String (List Event)
decodeEvents events =
    let
        combineResults =
            List.foldr (Result.map2 (::)) (Ok [])
    in
        combineResults
            <| List.map (decodeString decodeEvent)
            <| String.lines
            <| String.dropRight 1 events
