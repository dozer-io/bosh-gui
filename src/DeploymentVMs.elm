module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (..)
import List exposing (map)
import Platform.Cmd exposing (Cmd)
import Task


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
    , vms : List VM
    }


type alias Deployment =
    String


type alias VM =
    { name : String }


init : ( Model, Cmd Msg )
init =
    ( Model "cf-warden" [], fetchVMs "cf-warden" )



-- ACTION, UPDATE


type Msg
    = FetchVMs
    | FetchVMsSucceed (List VM)
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchVMs ->
            ( model, fetchVMs model.deployment )

        FetchVMsSucceed vms ->
            --            ( { model | vms = (Dict.insert deployment vms model.vms) }, Cmd.none )
            ( model, Cmd.none )

        FetchFail _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h3 []
            [ text model.deployment
            , ul [ attribute "class" "mdl-list" ]
                (List.map vmListItem model.vms)
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


fetchVMs : Deployment -> Cmd Msg
fetchVMs deployemnt =
    Task.perform FetchFail FetchVMsSucceed <| fetchVMsTask deployemnt


fetchVMsTask : String -> Task.Task Http.Error (List VM)
fetchVMsTask deployment =
    let
        url =
            "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000/deployments/"
                ++ deployment
                ++ "/vms?format=full"
    in
        Http.get decodeVMs url


decodeVMs : Decoder (List VM)
decodeVMs =
    Json.Decode.list decodeVM


decodeVM : Decoder VM
decodeVM =
    object1 VM
        ("name" := string)
