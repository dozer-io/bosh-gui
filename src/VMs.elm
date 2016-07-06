module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (..)
import List exposing (map)
import Material
import Platform.Cmd exposing (Cmd)
import Task


-- import Task.Extra exposing (parallel)
-- import Material.Scheme
-- import Material.Button as Button
-- import Material.Options exposing (css)


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
    { vms : List ( Deployment, List VM )
    , deployments : List Deployment
    , mdl : Material.Model
    }


type alias Deployment =
    String


type alias VM =
    { name : String }


init : ( Model, Cmd Msg )
init =
    ( Model [] [] Material.model, fetchDeployments )



-- ACTION, UPDATE


type Msg
    = FetchVMs
    | FetchVMsSucceed (List (List VM))
    | FetchDeploymentsSucceed (List Deployment)
    | FetchFail Http.Error
    | MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchVMs ->
            ( model, fetchDeployments )

        FetchDeploymentsSucceed deployments ->
            ( { model | deployments = deployments }, fetchVMs deployments )

        FetchVMsSucceed vms ->
            --            ( { model | vms = (Dict.insert deployment vms model.vms) }, Cmd.none )
            ( model, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )

        MDL action' ->
            Material.update MDL action' model



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    div []
        <| List.map deploymentList model.vms


deploymentList : ( String, List VM ) -> Html Msg
deploymentList namedVMs =
    div []
        [ h3 []
            [ text <| fst namedVMs
            , ul [ attribute "class" "mdl-list" ]
                (List.map vmListItem <| snd namedVMs)
            ]
        ]


vmListItem : VM -> Html Msg
vmListItem vm =
    li [ attribute "class" "mdl-list__item" ] [ text ("Name: " ++ vm.name) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


fetchDeployments : Cmd Msg
fetchDeployments =
    let
        url =
            "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000/deployments"
    in
        Task.perform FetchFail FetchDeploymentsSucceed <| Http.get decodeDeployments url


fetchVMs : List Deployment -> Cmd Msg
fetchVMs deployemnts =
    Task.perform FetchFail FetchVMsSucceed <| fetchVMsTasks deployemnts


fetchVMsTasks : List String -> Task.Task Http.Error (List (List VM))
fetchVMsTasks deployments =
    Task.sequence
        <| List.map fetchVMsTask deployments


fetchVMsTask : String -> Task.Task Http.Error (List VM)
fetchVMsTask deployment =
    let
        url =
            "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000/deployments/"
                ++ deployment
                ++ "/vms?format=full"
    in
        Http.get decodeVMs url


decodeDeployments : Decoder (List Deployment)
decodeDeployments =
    Json.Decode.list decodeDeployment


decodeDeployment : Decoder Deployment
decodeDeployment =
    "name" := string


decodeVMs : Decoder (List VM)
decodeVMs =
    Json.Decode.list decodeVM


decodeVM : Decoder VM
decodeVM =
    object1 VM
        ("name" := string)
