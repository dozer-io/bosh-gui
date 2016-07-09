module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Http
import Json.Decode exposing (..)
import Platform.Cmd exposing (Cmd)
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
    | GetVMsTaskSucceed TaskUrl


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetVMsTaskFail _ ->
            ( model, getVMsTask model.deployment )

        GetVMsTaskSucceed taskUrl ->
            ( { model | taskUrl = taskUrl }, Cmd.none )



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



-- getTaskState : TaskUrl -> Cmd Msg
-- getTaskResult : TaskUrl -> Cmd Msg
-- DECODE


decodeVMsTask : Decoder TaskUrl
decodeVMsTask =
    "location" := string



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
