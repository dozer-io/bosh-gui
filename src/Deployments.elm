module Deployments exposing (..)

import Html exposing (..)
import Html.App as App
import Http
import HttpAuth
import Json.Decode exposing (..)
import List exposing (map)
import List.Extra exposing (getAt, setAt)
import Platform.Cmd exposing (Cmd)
import Task
import VMs
import Erl


main : Program Never
main =
    App.program
        { init = init "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { deployments : List VMs.Model
    , loading : Bool
    , endpoint : String
    }


type alias Deployment =
    String


init : String -> ( Model, Cmd Msg )
init endpoint =
    ( Model [] True endpoint, getDeployments endpoint )



-- ACTION, UPDATE


type Msg
    = GetDeployments
    | GetDeploymentsFail Http.RawError
    | GetDeploymentsSucceed String
    | SubMsg Int VMs.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        GetDeployments ->
            ( model, getDeployments model.endpoint )

        GetDeploymentsFail _ ->
            ( model, Cmd.none )

        GetDeploymentsSucceed string ->
            let
                deployments =
                    Result.withDefault []
                        <| decodeString decodeDeployments string

                createVMs id deployment =
                    let
                        ( vms, cmds ) =
                            VMs.init model.endpoint deployment
                    in
                        ( vms, Cmd.map (SubMsg id) cmds )

                ( newDeployments, cmds ) =
                    List.unzip (List.indexedMap createVMs deployments)
            in
                ( { model | loading = False, deployments = newDeployments }, Cmd.batch cmds )

        SubMsg id subMsg ->
            case getAt id model.deployments of
                Nothing ->
                    ( model, Cmd.none )

                Just deployment ->
                    let
                        ( newDeployment, cmds ) =
                            VMs.update subMsg deployment
                    in
                        case setAt id newDeployment model.deployments of
                            Nothing ->
                                ( model, Cmd.none )

                            Just deployments ->
                                ( { model | deployments = deployments }, Cmd.map (SubMsg id) cmds )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ (if model.loading then
            h1 [] [ text "Loading" ]
           else
            text ""
          )
        , div []
            <| List.indexedMap viewDeployment model.deployments
        ]


viewDeployment : Int -> VMs.Model -> Html Msg
viewDeployment id model =
    App.map (SubMsg id) (VMs.view model)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getDeployments : String -> Cmd Msg
getDeployments endpoint =
    let
        url =
            Erl.parse endpoint
                |> Erl.appendPathSegments [ "deployments" ]
                |> Erl.toString
    in
        HttpAuth.get url GetDeploymentsFail GetDeploymentsSucceed


decodeDeployments : Decoder (List Deployment)
decodeDeployments =
    Json.Decode.list decodeDeployment


decodeDeployment : Decoder Deployment
decodeDeployment =
    ("name" := string)
