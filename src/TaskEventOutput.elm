module TaskEventOutput exposing (..)

import Html exposing (..)


-- import Html.Events exposing (onClick)

import Platform.Cmd exposing (Cmd)
import Material
import Material.Color as Color exposing (color, Hue(..), Shade(..), background, white)
import Material.Icon as Icon
import Material.Spinner as Loading
import Material.Options exposing (styled, nop, cs, css)
import Html.App as App
import Json.Decode exposing (int, string, list, float, oneOf, null, Decoder, decodeString)
import Json.Decode.Pipeline exposing (decode, required, optional)
import List.Extra exposing (groupWhile)
import String
import Time exposing (..)
import Erl
import Task exposing (perform)
import Http
import HttpAuth


main : Program Never
main =
    App.program
        { init = init "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000" 65 False
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Mdl =
    Material.Model


type alias Model =
    { mdl : Mdl
    , stages : List Stage
    , loading : Bool
    , taskId : Int
    , now : Time
    , endpoint : String
    , refresh : Bool
    }


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


init : String -> Int -> Bool -> ( Model, Cmd Msg )
init endpoint taskId refresh =
    ( Model Material.model [] True taskId (second * 0) endpoint refresh
    , Cmd.batch [ Task.perform Tick Tick Time.now, getTaskEventOutput endpoint taskId ]
    )



-- MESSAGE, UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | Tick Time.Time
    | GetTaskEventOutputSucceed String
    | GetTaskEventOutputFail Http.RawError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl message' ->
            Material.update message' model

        Tick now ->
            ( { model | now = now }, Cmd.none )

        GetTaskEventOutputFail _ ->
            ( model, getTaskEventOutput model.endpoint model.taskId )

        GetTaskEventOutputSucceed rawEvents ->
            let
                stages =
                    eventsToStages
                        <| Result.withDefault []
                        <| decodeEvents rawEvents
            in
                ( { model | stages = stages }
                , if model.refresh then
                    getTaskEventOutput model.endpoint model.taskId
                  else
                    Cmd.none
                )


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
                [ styled h5
                    [ cs "mdl-typography--headline"
                    , css "fontSize" "16px"
                    , css "padding-left" "40px"
                    , Color.text <| color Grey S600
                    ]
                    [ text stage.name ]
                , styled ol [ background white ]
                    <| List.map taskView stage.tasks
                ]

        taskView task =
            styled li
                [ cs "mdl-list__item" ]
                [ styled span
                    [ cs "mdl-list__item-primary-content" ]
                    [ taskState task.state
                    , text task.description
                    ]
                ]

        taskState state =
            case state of
                InProgress ->
                    Loading.spinner [ Loading.active True, Loading.singleColor True ]

                Started ->
                    Loading.spinner
                        [ Loading.active True
                        , Loading.singleColor True
                        , cs "material-icons mdl-list__item-icon"
                        ]

                Finished ->
                    Icon.view "done" [ Icon.size24, cs "material-icons mdl-list__item-icon" ]

                _ ->
                    Icon.view "error_outline" [ Icon.size24, cs "material-icons mdl-list__item-icon" ]
    in
        div [] <| List.map stageView model.stages



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.second) Tick



-- HTTP


getTaskEventOutput : String -> Int -> Cmd Msg
getTaskEventOutput endpoint taskId =
    let
        url =
            Erl.parse endpoint
                |> Erl.appendPathSegments [ "tasks", toString taskId, "output" ]
                |> Erl.addQuery "type" "event"
                |> Erl.toString
    in
        HttpAuth.get url GetTaskEventOutputFail GetTaskEventOutputSucceed



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
