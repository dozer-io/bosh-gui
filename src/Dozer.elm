module Dozer exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Bosh
import Material
import Material.Layout as Layout
import Material.Options exposing (styled, cs, css, nop)
import Material.Color as Color exposing (background, white, primary, color, Hue(..), Shade(..))
import Material.Icon as Icon
import Material.Menu as Menu
import Time exposing (Time, second)
import Html.App as App
import HttpAuth
import HttpAuth.OAuth as OAuth
import HttpAuth.Part
import Http
import List.Extra exposing (getAt, setAt)
import Json.Decode exposing (string, list, null, succeed, oneOf, map, Decoder, customDecoder, decodeString)
import Json.Decode.Pipeline exposing (decode, required)
import Date
import Navigation
import Erl


type alias Flags =
    { apiUrl : String
    , authUrl : String
    , appUrl : String
    , oAuthClient : String
    , oAuthScopes : List String
    }


main : Program Flags
main =
    Navigation.programWithFlags (HttpAuth.urlParser)
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }


urlUpdate : String -> Model -> ( Model, Cmd Msg )
urlUpdate token model =
    let
        ( auth, cmd ) =
            HttpAuth.Part.urlUpdate token model.auth
    in
        ( { model | auth = auth }, cmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ HttpAuth.Part.subscriptions AuthMsg
        , Menu.subs Mdl model.mdl
        ]



-- MODEL


type alias Mdl =
    Material.Model


type State
    = Pending
    | Building
    | Starting
    | Running


type alias Director =
    { uuid : String
    , name : String
    , created_at : Time
    , updated_at : Maybe Time
    , state : State
    , endpoint : String
    }


type alias Model =
    { mdl : Mdl
    , selectedDirector : Maybe Int
    , directors : Maybe (List ( Director, Bosh.Model ))
    , apiUrl : String
    , auth : HttpAuth.Part.Model
    }


init : Flags -> String -> ( Model, Cmd Msg )
init flags token =
    let
        ( auth, cmd ) =
            HttpAuth.Part.init
                <| HttpAuth.OAuth
                <| OAuth.OAuthClient flags.authUrl
                    flags.oAuthClient
                    flags.oAuthScopes
                    flags.appUrl
                    token
    in
        ( { mdl = Material.model
          , selectedDirector = Nothing
          , directors = Nothing
          , apiUrl = flags.apiUrl
          , auth = auth
          }
        , Cmd.batch [ cmd, getDirectors flags.apiUrl ]
        )



-- MESSAGE, UPDATE


type Msg
    = SelectDirector Int
    | Mdl (Material.Msg Msg)
    | AuthMsg HttpAuth.Part.Msg
    | GetDirectorsSucceed String
    | GetDirectorsFail Http.RawError
    | SubMsg Int Bosh.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectDirector id ->
            ( { model | selectedDirector = Just id }, Cmd.none )

        AuthMsg message' ->
            let
                ( auth, cmd ) =
                    HttpAuth.Part.update message' model.auth
            in
                ( { model | auth = auth }, Cmd.map AuthMsg cmd )

        Mdl message' ->
            Material.update message' model

        GetDirectorsFail _ ->
            ( model, Cmd.none )

        GetDirectorsSucceed string ->
            let
                directors =
                    Result.withDefault []
                        <| decodeString decodeDirectors string

                toTuple id director =
                    let
                        ( model, cmd ) =
                            Bosh.init director.endpoint
                    in
                        ( ( director, model ), Cmd.map (SubMsg id) cmd )

                ( directors', cmds ) =
                    List.indexedMap toTuple directors
                        |> List.unzip
            in
                ( { model | directors = Just directors' }, Cmd.batch cmds )

        SubMsg id subMsg ->
            let
                directors =
                    Maybe.withDefault [] model.directors
            in
                case getAt id directors of
                    Nothing ->
                        ( model, Cmd.none )

                    Just director ->
                        let
                            ( newBosh, cmds ) =
                                Bosh.update subMsg <| snd director
                        in
                            case setAt id ( fst director, newBosh ) directors of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just directors' ->
                                    ( { model | directors = Just directors' }, Cmd.map (SubMsg id) cmds )



-- VIEW


view : Model -> Html Msg
view model =
    let
        selected =
            model.selectedDirector |> Maybe.withDefault -1

        directors =
            model.directors |> Maybe.withDefault []
    in
        case HttpAuth.Part.view model.auth of
            Just view ->
                App.map AuthMsg view

            Nothing ->
                Layout.render Mdl
                    model.mdl
                    [ Layout.fixedTabs
                    , Layout.fixedHeader
                    , Layout.selectedTab
                        <| case getAt selected directors of
                            Nothing ->
                                -1

                            Just director ->
                                (snd director).tab
                    , Layout.onSelectTab
                        (\id ->
                            SubMsg selected (Bosh.SelectTab id)
                        )
                    ]
                    { header =
                        [ Layout.row []
                            [ Layout.title []
                                [ text
                                    <| case getAt selected directors of
                                        Nothing ->
                                            ""

                                        Just director ->
                                            (fst director).name
                                ]
                            ]
                        ]
                    , drawer =
                        [ case model.directors of
                            Nothing ->
                                div [] [ text "loading" ]

                            Just directors ->
                                styled ol [ cs "mdl-list" ]
                                    <| List.indexedMap (viewDirectorListItem selected)
                                    <| fst
                                    <| List.unzip directors
                        ]
                    , tabs =
                        case model.selectedDirector of
                            Nothing ->
                                ( [], [] )

                            Just _ ->
                                Bosh.tabsView
                    , main =
                        [ case model.directors of
                            Nothing ->
                                div [] [ text "loading" ]

                            Just directors ->
                                case model.selectedDirector of
                                    Nothing ->
                                        div [] [ text "select a director" ]

                                    Just id ->
                                        viewBosh id directors
                        ]
                    }


viewDirectorListItem : Int -> Int -> Director -> Html Msg
viewDirectorListItem selectedId id director =
    styled li
        [ cs "mdl-list__item"
        , if selectedId == id then
            background <| color Blue S50
          else
            nop
        , Layout.onClick (SelectDirector id)
        ]
        [ styled span
            [ cs "mdl-list__item-primary-content" ]
            [ Icon.view "cloud_circle"
                [ Icon.size36
                , Color.text primary
                , cs "mdl-list__item-icon"
                , css "margin-top" "-12px"
                ]
            , span [] [ text director.name ]
            ]
        ]


viewBosh : Int -> List ( Director, Bosh.Model ) -> Html Msg
viewBosh id directors =
    case getAt id directors of
        Nothing ->
            div [] [ text "not yet loaded" ]

        Just director ->
            App.map (SubMsg id) <| Bosh.mainView <| snd director



-- HTTP


getDirectors : String -> Cmd Msg
getDirectors appUrl =
    let
        url =
            Erl.toString
                <| Erl.appendPathSegments [ "directors" ]
                <| Erl.parse appUrl
    in
        HttpAuth.get url GetDirectorsFail GetDirectorsSucceed


decodeDirectors : Decoder (List Director)
decodeDirectors =
    list decodeDirector


decodeDirector : Decoder Director
decodeDirector =
    decode Director
        |> required "uuid" string
        |> required "name" string
        |> required "created_at" time
        |> required "updated_at" maybeTime
        |> required "state" state
        |> required "endpoint" string


time : Decoder Time
time =
    customDecoder string
        <| \string' ->
            Date.fromString string'
                `Result.andThen` \date' ->
                                    Ok <| Date.toTime date'


maybeTime : Decoder (Maybe Time)
maybeTime =
    oneOf [ null Nothing, map Just time ]


state : Decoder State
state =
    let
        decodeState state =
            case state of
                "pending" ->
                    succeed Pending

                "building" ->
                    succeed Building

                "starting" ->
                    succeed Starting

                "Running" ->
                    succeed Running

                _ ->
                    succeed Pending
    in
        string `Json.Decode.andThen` decodeState
