module Bosh exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Html.App as App
import TimeTravel.Html.App as TimeTravel
import Material.Tabs as Tabs
import Material
import Deployments
import Stemcells


main : Program Never
main =
    -- App.program
    TimeTravel.program
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
    , stemcellsLoaded : Bool
    , stemcells : Stemcells.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( deployments, cmd ) =
            Deployments.init

        ( stemcells, _ ) =
            Stemcells.init
    in
        ( { mdl = Material.model
          , tab = 0
          , deployments = deployments
          , stemcellsLoaded = False
          , stemcells = stemcells
          }
        , Cmd.map DeploymentsMsg cmd
        )



-- MESSAGE, UPDATE


type Msg
    = SelectTab Int
    | Mdl Material.Msg
    | DeploymentsMsg Deployments.Msg
    | StemcellsMsg Stemcells.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab idx ->
            case idx of
                0 ->
                    ( { model | tab = idx }, Cmd.none )

                1 ->
                    if model.stemcellsLoaded then
                        ( { model | tab = idx }, Cmd.none )
                    else
                        let
                            ( stemcells, cmd ) =
                                Stemcells.update Stemcells.GetStemcells model.stemcells
                        in
                            ( { model | tab = idx, stemcells = stemcells, stemcellsLoaded = True }
                            , Cmd.map StemcellsMsg cmd
                            )

                _ ->
                    ( { model | tab = idx }, Cmd.none )

        DeploymentsMsg msg ->
            let
                ( deployments, cmd ) =
                    Deployments.update msg model.deployments
            in
                ( { model | deployments = deployments }, Cmd.map (DeploymentsMsg) cmd )

        StemcellsMsg msg ->
            let
                ( stemcells, cmd ) =
                    Stemcells.update msg model.stemcells
            in
                ( { model | stemcells = stemcells }, Cmd.map (StemcellsMsg) cmd )

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
            [ Tabs.textLabel [] "VMs"
            , Tabs.textLabel [] "Stemcells"
            ]
            [ case model.tab of
                0 ->
                    div [] [ App.map DeploymentsMsg (Deployments.view model.deployments) ]

                1 ->
                    div [] [ App.map StemcellsMsg (Stemcells.view model.stemcells) ]

                _ ->
                    div [] []
            ]
        ]
