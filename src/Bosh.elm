module Bosh exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Html.App as App
import Material.Tabs as Tabs
import Material
import Deployments


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Mdl =
    Material.Model


type alias Model =
    { mdl : Material.Model
    , tab : Int
    , deployments : Deployments.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( deployments, cmd ) =
            Deployments.init
    in
        ( { mdl = Material.model
          , tab = 0
          , deployments = deployments
          }
        , Cmd.map DeploymentsMsg cmd
        )



-- MESSAGE, UPDATE


type Msg
    = SelectTab Int
    | Mdl Material.Msg
    | DeploymentsMsg Deployments.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab idx ->
            case idx of
                0 ->
                    -- If we don't want to pre-load content we could use somthing like the code below
                    -- let
                    --     ( deployments, cmd ) =
                    --         Deployments.update Deployments.GetDeployments model.deployments
                    -- in
                    --     ( { model | tab = idx, deployments = deployments }, Cmd.map DeploymentsMsg cmd
                    --                        )
                    ( { model | tab = idx }, Cmd.none )

                1 ->
                    ( { model | tab = idx }, Cmd.none )

                _ ->
                    ( { model | tab = idx }, Cmd.none )

        DeploymentsMsg msg ->
            let
                ( deployments, cmd ) =
                    Deployments.update msg model.deployments
            in
                ( { model | deployments = deployments }, Cmd.map (DeploymentsMsg) cmd )

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
            [ Tabs.label [] [ text "VMs" ]
            , Tabs.textLabel [] "Tab Two"
            ]
            [ case model.tab of
                0 ->
                    div [] [ App.map DeploymentsMsg (Deployments.view model.deployments) ]

                1 ->
                    div [] [ text "Content of tab two" ]

                _ ->
                    div [] []
            ]
        ]
