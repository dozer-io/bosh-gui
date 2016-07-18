module VMs exposing (..)

import Erl exposing (appendPathSegments)
import Html exposing (..)
import Html.App as App
import Http
import Json.Decode exposing (..)
import Platform.Cmd exposing (Cmd)


-- import Material.Progress as Loading

import Material
import Task
import String
import VM
import List.Extra


main : Program Never
main =
    App.program
        { init = init "cf-warden"
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
    }


type alias Deployment =
    String


type alias TaskUrl =
    String


init : Deployment -> ( Model, Cmd Msg )
init deployment =
    ( Model deployment [] True "", getVMsTask deployment )



-- ACTION, UPDATE


type Msg
    = GetVMsTaskFail Http.Error
    | GetTaskStateFail Http.Error
    | GetTaskResultFail Http.Error
    | GetVMsTaskSucceed TaskUrl
    | GetTaskStateSucceed String
    | GetTaskResultSucceed String
    | SubMsg Int VM.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetVMsTaskFail _ ->
            ( model, getVMsTask model.deployment )

        GetTaskStateFail _ ->
            ( model, getTaskState model.taskUrl )

        GetTaskResultFail err ->
            ( model, getTaskResult model.taskUrl )

        GetVMsTaskSucceed taskUrl ->
            ( { model | taskUrl = taskUrl }, getTaskState taskUrl )

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

        GetTaskResultSucceed rawVMs ->
            let
                vms =
                    case decodeVMsResult rawVMs of
                        Err _ ->
                            []

                        Ok vms ->
                            vms

                ( newVMs, cmds ) =
                    List.unzip (List.indexedMap createVM vms)
            in
                ( { model | vms = newVMs, loading = False }
                , Cmd.batch cmds
                )

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
        [ h1 [] [ text model.deployment ]
        , if model.loading then
            loading
          else
            div []
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
                <| Erl.addQuery "type" "result"
                <| appendPathSegments [ "output" ]
                <| Erl.parse taskUrl
    in
        Task.perform GetTaskResultFail GetTaskResultSucceed
            <| Http.getString url



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
