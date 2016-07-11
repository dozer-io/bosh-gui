module Main exposing (..)

import Erl exposing (appendPathSegments)
import Html exposing (..)
import Html.App as App
import Http
import Json.Decode exposing (..)
import Platform.Cmd exposing (Cmd)
import Regex
import Task
import VM


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { deployment : Deployment
    , vms : List VM.Model
    , loading : Bool
    , taskUrl : String
    }


type alias Deployment =
    String


type alias TaskUrl =
    String


init : ( Model, Cmd Msg )
init =
    ( Model "cf-warden" [] True "", getVMsTask "cf-warden" )



-- ACTION, UPDATE


type Msg
    = GetVMsTaskFail Http.Error
    | GetTaskStateFail Http.Error
    | GetTaskResultFail Http.Error
    | GetVMsTaskSucceed TaskUrl
    | GetTaskStateSucceed String
    | GetTaskResultSucceed (List VM.VM)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetVMsTaskFail _ ->
            ( model, getVMsTask model.deployment )

        GetTaskStateFail _ ->
            ( model, getTaskState model.taskUrl )

        GetTaskResultFail _ ->
            ( model, getTaskResult model.taskUrl )

        GetVMsTaskSucceed taskUrl ->
            ( { model | taskUrl = taskUrl }, Cmd.none )

        GetTaskStateSucceed state ->
            case state of
                "done" ->
                    ( model, getTaskResult model.taskUrl )

                "running" ->
                    ( model, getTaskState model.taskUrl )

                "timeout" ->
                    ( model, getVMsTask model.deployment )

                "error" ->
                    ( model, getVMsTask model.deployment )

                _ ->
                    ( model, getVMsTask model.deployment )

        GetTaskResultSucceed vms ->
            ( { model | vms = List.map VM.init vms }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ (if model.loading then
            loading
           else
            text ""
          )
        , text model.taskUrl
        ]


loading : Html Msg
loading =
    h1 [] [ text "Loading" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getVMsTask : Deployment -> Cmd Msg
getVMsTask deployment =
    let
        url =
            "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000/deployments/"
                ++ deployment
                ++ "/vms?format=full"
    in
        Task.perform GetVMsTaskFail GetVMsTaskSucceed <| Http.get decodeVMsTask url


getTaskState : TaskUrl -> Cmd Msg
getTaskState taskUrl =
    Task.perform GetTaskStateFail GetTaskStateSucceed <| Http.get decodeTaskState taskUrl


getTaskResult : TaskUrl -> Cmd Msg
getTaskResult taskUrl =
    let
        url =
            Erl.toString
                <| appendPathSegments [ "output" ]
                <| Erl.parse taskUrl
    in
        Task.perform GetTaskResultFail GetTaskResultSucceed <| Http.get decodeVMsResult url



-- DECODE


decodeVMsTask : Decoder TaskUrl
decodeVMsTask =
    "location" := string


decodeTaskState : Decoder String
decodeTaskState =
    "state" := string


decodeVMsResult : Decoder (List VM.VM)
decodeVMsResult =
    let
        combineResults =
            List.foldr (Result.map2 (::)) (Ok [])
    in
        customDecoder (string)
            (\ndjson ->
                combineResults
                    <| List.map (decodeString decodeVM)
                    <| Regex.split Regex.All (Regex.regex "/n") ndjson
            )


decodeVM : Decoder VM.VM
decodeVM =
    object2 VM.VM
        ("vm_cid" := string)
        ("job_name" := string)



-- decodeDeployments : Decoder (List Deployment)
-- decodeDeployments =
--     Json.Decode.list decodeDeployment
-- decodeDeployment : Decoder Deployment
-- decodeDeployment =
--     "location" := string
-- decodeVMs : Decoder (List VM)
-- decodeVMs =
--     Json.Decode.list decodeVM
-- decodeVM : Decoder VM
-- decodeVM =
--     object1 VM
--         ("name" := string)
