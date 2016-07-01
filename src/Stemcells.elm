module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (map)
import Http
import Task
import Platform.Cmd exposing (Cmd)
import Json.Decode exposing (..)
import Material


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
    ( Model [] Material.model, getStemcells )



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
            ( model, getStemcells )

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


getStemcells : Cmd Msg
getStemcells =
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
