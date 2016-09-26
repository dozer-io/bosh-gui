module Bosh exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Html.App as App
import TimeTravel.Html.App as TimeTravel
import Material.Layout as Layout
import Material.Options as Options
import Material
import Deployments
import Stemcells
import Activities


main : Program Never
main =
    -- App.program
    TimeTravel.program
        { init = init "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000"
        , view = view
        , update = update
        , subscriptions =
            always Sub.none
            -- (Layout.subs Mdl)
        }



-- MODEL


type alias Mdl =
    Material.Model


type alias Model =
    { mdl : Material.Model
    , tab : Int
    , deploymentsLoaded : Bool
    , deployments : Deployments.Model
    , stemcellsLoaded : Bool
    , stemcells : Stemcells.Model
    , activities : Activities.Model
    , endpoint : String
    }


init : String -> ( Model, Cmd Msg )
init endpoint =
    let
        ( deployments, _ ) =
            Deployments.init endpoint

        ( stemcells, _ ) =
            Stemcells.init endpoint

        ( activities, cmd ) =
            Activities.init endpoint

        layoutCmd =
            Layout.sub0 Mdl
    in
        ( { mdl = Material.model
          , tab = 0
          , deploymentsLoaded = False
          , deployments = deployments
          , stemcellsLoaded = False
          , stemcells = stemcells
          , activities = activities
          , endpoint = endpoint
          }
        , Cmd.batch [ Cmd.map ActivitiesMsg cmd, layoutCmd ]
        )



-- MESSAGE, UPDATE


type Msg
    = SelectTab Int
    | Mdl (Material.Msg Msg)
    | DeploymentsMsg Deployments.Msg
    | ActivitiesMsg Activities.Msg
    | StemcellsMsg Stemcells.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab idx ->
            case idx of
                0 ->
                    let
                        ( activities, cmd ) =
                            Activities.update Activities.GetActivities model.activities
                    in
                        ( { model | tab = idx, activities = activities }
                        , Cmd.map ActivitiesMsg cmd
                        )

                1 ->
                    if model.deploymentsLoaded then
                        ( { model | tab = idx }, Cmd.none )
                    else
                        let
                            ( deployments, cmd ) =
                                Deployments.update Deployments.GetDeployments model.deployments
                        in
                            ( { model | tab = idx, deployments = deployments, deploymentsLoaded = True }
                            , Cmd.map DeploymentsMsg cmd
                            )

                2 ->
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

        ActivitiesMsg msg ->
            let
                ( activities, cmd ) =
                    Activities.update msg model.activities
            in
                ( { model | activities = activities }, Cmd.map (ActivitiesMsg) cmd )

        Mdl message' ->
            Material.update message' model



-- VIEW


view : Model -> Html Msg
view model =
    Layout.render Mdl
        model.mdl
        [ Layout.fixedTabs
        , Layout.selectedTab model.tab
        , Layout.onSelectTab SelectTab
        ]
        { header = []
        , drawer = []
        , tabs = tabsView
        , main =
            [ stylesheet
            , mainView model
            ]
        }


tabsView : ( List (Html a), List (Options.Property () a) )
tabsView =
    ( [ text "Activities"
      , text "VMs"
      , text "Stemcells"
      ]
    , []
    )


mainView : Model -> Html Msg
mainView model =
    case model.tab of
        0 ->
            div [] [ App.map ActivitiesMsg (Activities.view model.activities) ]

        1 ->
            div [] [ App.map DeploymentsMsg (Deployments.view model.deployments) ]

        2 ->
            div [] [ App.map StemcellsMsg (Stemcells.view model.stemcells) ]

        _ ->
            div [] []


stylesheet : Html a
stylesheet =
    Options.stylesheet """
  header.mdl-layout__header { min-height: 0px; }
  a.mdl-layout__tab { color: rgba(255, 255, 255, .6); }
  a.mdl-layout__tab.is-active { color: rgb(255, 255, 255) !important;}
"""
