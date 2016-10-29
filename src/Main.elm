module Main exposing (..)

import Html.App as App
import Html exposing (..)
import Bosh
import Material
import Material.Layout as Layout
import Material.Menu as Menu
import Material.Options exposing (stylesheet)
import HttpAuth
import HttpAuth.Part
import HttpAuth.Basic as Basic


type alias Flags =
    { target : String }


main : Program Flags
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



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


type alias Model =
    { mdl : Mdl
    , bosh : Bosh.Model
    , auth : HttpAuth.Part.Model
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        ( auth, authCmd ) =
            HttpAuth.Part.init
                <| HttpAuth.Basic
                <| Basic.init flags.target

        ( bosh, boshCmd ) =
            Bosh.init flags.target
    in
        ( { mdl = Material.model
          , bosh = bosh
          , auth = auth
          }
        , Cmd.batch [ Cmd.map BoshMsg boshCmd, Cmd.map AuthMsg authCmd ]
        )



-- MESSAGE, UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | AuthMsg HttpAuth.Part.Msg
    | BoshMsg Bosh.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg' ->
            Material.update msg' model

        AuthMsg msg' ->
            let
                ( auth, cmd ) =
                    HttpAuth.Part.update msg' model.auth
            in
                ( { model | auth = auth }, Cmd.map AuthMsg cmd )

        BoshMsg msg' ->
            let
                ( bosh, cmd ) =
                    Bosh.update msg' model.bosh
            in
                ( { model | bosh = bosh }, Cmd.map BoshMsg cmd )



-- VIEW


view : Model -> Html Msg
view { bosh, auth, mdl } =
    case HttpAuth.Part.view auth of
        Just view ->
            App.map AuthMsg view

        Nothing ->
            Layout.render Mdl
                mdl
                [ Layout.fixedTabs
                , Layout.fixedHeader
                , Layout.selectedTab bosh.tab
                , Layout.onSelectTab (BoshMsg << Bosh.SelectTab)
                ]
                { header = []
                , drawer = []
                , tabs = Bosh.tabsView
                , main =
                    [ stylesheet "header.mdl-layout__header { min-height: 48px }"
                    , App.map BoshMsg
                        <| Bosh.mainView bosh
                    ]
                }
