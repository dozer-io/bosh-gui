module Bosh exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Html.App as App
import Material.Tabs as Tabs
import Material


main : Program Never
main =
    App.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Mdl =
    Material.Model


type alias Model =
    { mdl : Material.Model
    , tab : Int
    }


model : Model
model =
    { mdl = Material.model
    , tab = 0
    }



-- MESSAGE, UPDATE


type Msg
    = SelectTab Int
    | Mdl Material.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab idx ->
            ( { model | tab = idx }, Cmd.none )

        Mdl msg ->
            Material.update Mdl msg model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ Tabs.render Mdl
            [ 0 ]
            model.mdl
            [ Tabs.ripple
            , Tabs.onSelectTab SelectTab
            , Tabs.activeTab model.tab
            ]
            [ Tabs.label [] [ text "Tab One" ]
            , Tabs.textLabel [] "Tab Two"
            ]
            [ case model.tab of
                0 ->
                    div [] [ text "Content of tab one" ]

                1 ->
                    div [] [ text "Content of tab two" ]

                _ ->
                    div [] []
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
