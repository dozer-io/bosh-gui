module Dozer exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Bosh
import Material
import Material.Layout as Layout
import Material.Options exposing (styled, cs, css, nop)
import Material.Color as Color exposing (background, white, primary, color, Hue(..), Shade(..))
import Material.Icon as Icon
import Time exposing (Time, second)
import Html.App as App
import TimeTravel.Html.App as TimeTravel
import Http
import List.Extra exposing (getAt, setAt)
import Task
import Json.Decode exposing (string, list, null, succeed, oneOf, map, Decoder, customDecoder)
import Json.Decode.Pipeline exposing (decode, required)
import Date


main : Program Never
main =
    -- App.program
    TimeTravel.program
        { init = init
        , view = view
        , update = update
        , subscriptions =
            always Sub.none
            -- (Layout.subs Mdl)
        }



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
    { mdl : Material.Model
    , selectedDirector : Maybe Int
    , directors : Maybe (List ( Director, Bosh.Model ))
    }


init : ( Model, Cmd Msg )
init =
    ( { mdl = Material.model
      , selectedDirector = Nothing
      , directors = Nothing
      }
    , getDirectors
    )



-- MESSAGE, UPDATE


type Msg
    = SelectDirector Int
    | Mdl Material.Msg
    | GetDirectorsSucceed (List Director)
    | GetDirectorsFail Http.Error
    | SubMsg Int Bosh.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectDirector id ->
            ( { model | selectedDirector = Just id }, Cmd.none )

        Mdl msg ->
            Material.update Mdl msg model

        GetDirectorsFail _ ->
            ( model, Cmd.none )

        GetDirectorsSucceed directors ->
            let
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
        Layout.render Mdl
            model.mdl
            [ Layout.fixedTabs
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
            background <| color LightBlue S50
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


getDirectors : Cmd Msg
getDirectors =
    let
        url =
            "http://localhost:8001/dozer/directors"
    in
        Task.perform GetDirectorsFail GetDirectorsSucceed <| Http.get decodeDirectors url


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
