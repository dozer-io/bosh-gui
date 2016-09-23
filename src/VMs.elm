module VMs exposing (..)

import Erl exposing (appendPathSegments)
import Html exposing (..)
import Html.App as App
import Http
import HttpAuth
import Json.Decode exposing (..)
import Platform.Cmd exposing (Cmd)
import String
import VM
import List.Extra
import Material.List as Lists
import Material.Options as Options
import Material.Typography as Typo


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
    { deployment : Deployment
    , vms : List VM.Model
    , loading : Bool
    , taskUrl : TaskUrl
    , endpoint : String
    }


type alias Deployment =
    String


type alias TaskUrl =
    String


init : String -> Deployment -> ( Model, Cmd Msg )
init endpoint deployment =
    ( Model deployment [] True "" endpoint, getVMsTask endpoint deployment )



-- ACTION, UPDATE


type Msg
    = GetVMsTaskFail Http.RawError
    | GetTaskStateFail Http.RawError
    | GetTaskResultFail Http.RawError
    | GetVMsTaskSucceed TaskUrl
    | GetTaskStateSucceed String
    | GetTaskResultSucceed String
    | SubMsg Int VM.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                ( newVMs, cmds ) =
                    List.unzip
                        <| List.indexedMap createVM
                        <| Result.withDefault []
                        <| decodeVMsResult rawVMs
            in
                ( { model | vms = newVMs, loading = False }, Cmd.batch cmds )

        SubMsg id subMsg ->
            let
                maybeVM =
                    List.Extra.getAt id model.vms
            in
                case maybeVM of
                    Nothing ->
                        ( model, Cmd.none )

                    Just vm ->
                        let
                            ( newVM, cmds ) =
                                VM.update subMsg vm

                            maybeVMs =
                                List.Extra.setAt id newVM model.vms
                        in
                            case maybeVMs of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just vms ->
                                    ( { model | vms = vms }
                                    , Cmd.map (SubMsg id) cmds
                                    )


createVM : Int -> VM.VM -> ( VM.Model, Cmd Msg )
createVM id vm =
    let
        ( newVM, cmds ) =
            VM.init vm
    in
        ( newVM, Cmd.map (SubMsg id) cmds )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Options.styled h2 [ Typo.title ] [ text model.deployment ]
        , if model.loading then
            loading
          else
            Lists.ul []
                <| List.indexedMap viewVM model.vms
        ]


loading : Html Msg
loading =
    h1 [] [ text "Loading" ]


viewVM : Int -> VM.Model -> Html Msg
viewVM id model =
    App.map (SubMsg id) (VM.view model)



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


decodeVMsTask : Decoder TaskUrl
decodeVMsTask =
    "location" := string


decodeTaskState : Decoder String
decodeTaskState =
    "state" := string


decodeVMsResult : String -> Result String (List VM.VM)
decodeVMsResult vms =
    let
        combineResults =
            List.foldr (Result.map2 (::)) (Ok [])
    in
        combineResults
            <| List.map (decodeString VM.decodeVM)
            <| String.lines
            <| String.dropRight 1 vms
