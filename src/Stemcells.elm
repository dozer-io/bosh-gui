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
    { stemcells : List Stemcell
    , mdl : Material.Model
    }


type alias Stemcell =
    { name : String
    , version : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] Material.model, fetchStemcells )



-- ACTION, UPDATE


type Msg
    = FetchStemcells
    | FetchSucceed (List Stemcell)
    | FetchFail Http.Error
    | MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchStemcells ->
            ( model, fetchStemcells )

        FetchSucceed stemcells ->
            ( { model | stemcells = stemcells }, Cmd.none )

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
        [ ul [ attribute "class" "mdl-list" ]
            (List.map stemcellListItem model.stemcells)
        ]


stemcellListItem : Stemcell -> Html Msg
stemcellListItem stemcell =
    li [ attribute "class" "mdl-list__item" ] [ text ("Name: " ++ stemcell.name) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


fetchStemcells : Cmd Msg
fetchStemcells =
    let
        url =
            "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000/stemcells"
    in
        Task.perform FetchFail
            FetchSucceed
            (Http.get decodeStemcells url)


decodeStemcells : Decoder (List Stemcell)
decodeStemcells =
    Json.Decode.list decodeStemcell


decodeStemcell : Decoder Stemcell
decodeStemcell =
    object2 Stemcell
        ("name" := string)
        ("version" := string)
