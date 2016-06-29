module Main exposing (..)

import Html.App as App
import Html exposing (..)
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
    { info : Info
    , mdl : Material.Model
    }


type alias Info =
    { name : String
    , version : String
    , cpi : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model (Info "" "" "") Material.model, getInfo )



-- ACTION, UPDATE


type Msg
    = FetchInfo
    | FetchSucceed Info
    | FetchFail Http.Error
    | MDL Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchInfo ->
            ( model, getInfo )

        FetchSucceed info ->
            ( { model | info = info }, Cmd.none )

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
        [ h2 [] [ text model.info.name ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getInfo : Cmd Msg
getInfo =
    let
        url =
            "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000/info"
    in
        Task.perform FetchFail
            FetchSucceed
            (Http.get decodeInfo url)


decodeInfo : Decoder Info
decodeInfo =
    object3 Info
        ("name" := string)
        ("version" := string)
        ("cpi" := string)
