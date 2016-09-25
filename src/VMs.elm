module VMs exposing (..)

import Common
import Erl exposing (appendPathSegments)
import Html exposing (..)
import Html.App as App
import Http
import HttpAuth
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (decode, required)
import Material
import Material.Options as Options
import Material.Table as Table
import Material.Typography as Typo
import Material.Toggles as Toggles
import Platform.Cmd exposing (Cmd)
import String


main : Program Never
main =
    App.program
        { init = init "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000" "cf-warden"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { mdl : Material.Model
    , deployment : Deployment
    , vms : List VM
    , loading : Bool
    , taskUrl : TaskUrl
    , endpoint : String
    , selected : Maybe String
    }


type alias VM =
    { vmCid : String
    , ips : List String
    , agentId : String
    , jobName : String
    , index : Int
    , jobState : String
    , resourcePool : String
    }


type alias Deployment =
    String


type alias TaskUrl =
    String


init : String -> Deployment -> ( Model, Cmd Msg )
init endpoint deployment =
    ( Model Material.model deployment [] True "" endpoint Nothing
    , getVMsTask endpoint deployment
    )



-- ACTION, UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | Select String
    | GetVMsTaskFail Http.RawError
    | GetTaskStateFail Http.RawError
    | GetTaskResultFail Http.RawError
    | GetVMsTaskSucceed TaskUrl
    | GetTaskStateSucceed String
    | GetTaskResultSucceed String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl action' ->
            Material.update action' model

        Select key ->
            ( { model | selected = Just key }, Cmd.none )

        GetVMsTaskFail _ ->
            ( model, getVMsTask model.endpoint model.deployment )

        GetTaskStateFail _ ->
            ( model, getTaskState model.taskUrl )

        GetTaskResultFail err ->
            ( model, getTaskResult model.taskUrl )

        GetVMsTaskSucceed string ->
            let
                taskUrl =
                    Result.withDefault ""
                        <| decodeString decodeVMsTask string
            in
                ( { model | taskUrl = taskUrl }, getTaskState taskUrl )

        GetTaskStateSucceed string ->
            let
                state =
                    Result.withDefault ""
                        <| decodeString decodeTaskState string
            in
                case state of
                    "done" ->
                        ( model, getTaskResult model.taskUrl )

                    "running" ->
                        ( model, getTaskState model.taskUrl )

                    "timeout" ->
                        ( model, getVMsTask model.endpoint model.deployment )

                    "error" ->
                        ( model, getVMsTask model.endpoint model.deployment )

                    _ ->
                        ( model, getVMsTask model.endpoint model.deployment )

        GetTaskResultSucceed rawVMs ->
            let
                newVMs =
                    Result.withDefault []
                        <| decodeVMsResult rawVMs
            in
                ( { model | vms = newVMs, loading = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Options.styled h2 [ Typo.title ] [ text model.deployment ]
        , if model.loading then
            Common.loaderText <| "Loading VMs for: " ++ model.deployment ++ "..."
          else
            Table.table [ Options.css "width" "100%" ]
                [ Table.thead []
                    [ Table.tr []
                        [ Table.th [] [ text "" ]
                        , Table.th [] [ text "VM" ]
                        , Table.th [] [ text "State" ]
                          --                        , Table.th [] [ text "VM Type" ]
                        , Table.th [] [ text "IPs" ]
                        ]
                    ]
                , Table.tbody []
                    <| List.indexedMap (viewVM model.selected model.mdl)
                        model.vms
                ]
        ]


viewVM : Maybe String -> Material.Model -> Int -> VM -> Html Msg
viewVM selected mdl idx vm =
    let
        key =
            vm.jobName ++ "/" ++ (toString vm.index)

        selected' =
            (Just key) == selected
    in
        Table.tr [ Table.selected `Options.when` selected' ]
            [ Table.td []
                [ Toggles.checkbox Mdl
                    [ idx ]
                    mdl
                    [ Toggles.onClick (Select key)
                    , Toggles.value selected'
                    ]
                    []
                ]
            , Table.td [] [ text key ]
            , Table.td [] [ text vm.jobState ]
              --        , Table.td [] [ text model.vm.jobState ]
            , Table.td [] [ text <| String.join ", " vm.ips ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getVMsTask : String -> Deployment -> Cmd Msg
getVMsTask endpoint deployment =
    let
        url =
            Erl.parse endpoint
                |> Erl.appendPathSegments [ "deployments", deployment, "vms" ]
                |> Erl.addQuery "format" "full"
                |> Erl.toString
    in
        HttpAuth.get url GetVMsTaskFail GetVMsTaskSucceed


getTaskState : TaskUrl -> Cmd Msg
getTaskState taskUrl =
    HttpAuth.get taskUrl GetTaskStateFail GetTaskStateSucceed


getTaskResult : TaskUrl -> Cmd Msg
getTaskResult taskUrl =
    let
        url =
            Erl.toString
                <| Erl.addQuery "type" "result"
                <| appendPathSegments [ "output" ]
                <| Erl.parse taskUrl
    in
        HttpAuth.get url GetTaskResultFail GetTaskResultSucceed



-- DECODE


decodeVM : Decoder VM
decodeVM =
    decode VM
        |> required "vm_cid" string
        |> required "ips" (list string)
        |> required "agent_id" string
        |> required "job_name" string
        |> required "index" int
        |> required "job_state" string
        |> required "resource_pool" string


decodeVMsTask : Decoder TaskUrl
decodeVMsTask =
    "location" := string


decodeTaskState : Decoder String
decodeTaskState =
    "state" := string


decodeVMsResult : String -> Result String (List VM)
decodeVMsResult vms =
    let
        combineResults =
            List.foldr (Result.map2 (::)) (Ok [])
    in
        combineResults
            <| List.map (decodeString decodeVM)
            <| String.lines
            <| String.dropRight 1 vms
