module Stemcells exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Http
import Json.Decode exposing (..)
import List exposing (map)
import Material.Progress as Loading
import Material
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
    { stemcells : List Stemcell
    , loading : Bool
    , mdl : Material.Model
    }


type alias Stemcell =
    { name : String
    , version : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] True Material.model, getStemcells )



-- ACTION, UPDATE


type Msg
    = GetStemcells
    | GetSucceed (List Stemcell)
    | GetFail Http.Error
    | MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetStemcells ->
            ( model, getStemcells )

        GetSucceed stemcells ->
            ( { model | stemcells = stemcells, loading = False }, Cmd.none )

        GetFail _ ->
            ( model, Cmd.none )

        MDL msg ->
            Material.update MDL msg model



-- VIEW


type alias Mdl =
    Material.Model


view : Model -> Html Msg
view model =
    div []
        [ if model.loading then
            Loading.indeterminate
          else
            ul [ attribute "class" "mdl-list" ]
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
        Task.perform GetFail
            GetSucceed
            (Http.get decodeStemcells url)


decodeStemcells : Decoder (List Stemcell)
decodeStemcells =
    Json.Decode.list decodeStemcell


decodeStemcell : Decoder Stemcell
decodeStemcell =
    object2 Stemcell
        ("name" := string)
        ("version" := string)
