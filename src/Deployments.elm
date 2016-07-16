module Deployments exposing (..)

import Html exposing (..)
import Html.App as App


-- import TimeTravel.Html.App as TimeTravel

import Http
import Json.Decode exposing (..)
import List exposing (map)
import List.Extra exposing (getAt, setAt)
import Platform.Cmd exposing (Cmd)
import Task
import VMs


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
    { deployments : List VMs.Model
    , loading : Bool
    }


type alias Deployment =
    String


init : ( Model, Cmd Msg )
init =
    ( Model [] True, getDeployments )



-- ACTION, UPDATE


type Msg
    = GetDeployments
    | GetDeploymentsFail Http.Error
    | GetDeploymentsSucceed (List Deployment)
    | SubMsg Int VMs.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        GetDeployments ->
            ( model, getDeployments )

        GetDeploymentsFail _ ->
            ( model, Cmd.none )

        GetDeploymentsSucceed deployments ->
            let
                createVMs id deployment =
                    let
                        ( vms, cmds ) =
                            VMs.init deployment
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


getDeployments : Cmd Msg
getDeployments =
    let
        url =
            "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000/deployments"
    in
        Task.perform GetDeploymentsFail GetDeploymentsSucceed
            <| Http.get decodeDeployments url


decodeDeployments : Decoder (List Deployment)
decodeDeployments =
    Json.Decode.list decodeDeployment


decodeDeployment : Decoder Deployment
decodeDeployment =
    ("name" := string)
