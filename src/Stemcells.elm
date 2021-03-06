module Stemcells exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Http
import HttpAuth
import Json.Decode exposing (..)
import List exposing (map)
import Material.Progress as Loading
import Material
import Platform.Cmd exposing (Cmd)
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
    { stemcells : List Stemcell
    , loading : Bool
    , endpoint : String
    , mdl : Material.Model
    }


type alias Stemcell =
    { name : String
    , version : String
    }


init : String -> ( Model, Cmd Msg )
init endpoint =
    ( Model [] True endpoint Material.model, getStemcells endpoint )



-- ACTION, UPDATE


type Msg
    = GetStemcells
    | GetSucceed String
    | GetFail Http.RawError
    | Mdl (Material.Msg Msg)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetStemcells ->
            ( model, getStemcells model.endpoint )

        GetSucceed string ->
            let
                stemcells =
                    Result.withDefault []
                        <| decodeString decodeStemcells string
            in
                ( { model | stemcells = stemcells, loading = False }, Cmd.none )

        GetFail _ ->
            ( model, Cmd.none )

        Mdl message' ->
            Material.update message' model



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


getStemcells : String -> Cmd Msg
getStemcells endpoint =
    let
        url =
            Erl.parse endpoint
                |> Erl.appendPathSegments [ "stemcells" ]
                |> Erl.toString
    in
        HttpAuth.get url GetFail GetSucceed


decodeStemcells : Decoder (List Stemcell)
decodeStemcells =
    Json.Decode.list decodeStemcell


decodeStemcell : Decoder Stemcell
decodeStemcell =
    object2 Stemcell
        ("name" := string)
        ("version" := string)
