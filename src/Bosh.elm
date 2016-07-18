module Bosh exposing (..)

import Platform.Cmd exposing (Cmd, none)
import Html exposing (..)
import Html.App as App
import TimeTravel.Html.App as TimeTravel
import Material.Tabs as Tabs
import Material
import Deployments
import Stemcells
import Activities


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
    , deploymentsLoaded : Bool
    , deployments : Deployments.Model
    , stemcellsLoaded : Bool
    , stemcells : Stemcells.Model
    , activitiesLoaded : Bool
    , activities : Activities.Model
    }


init : ( Model, Cmd Msg )
init =
    let
        ( deployments, _ ) =
            Deployments.init

        ( stemcells, _ ) =
            Stemcells.init

        ( activities, cmd ) =
            Activities.init
    in
        ( { mdl = Material.model
          , tab = 0
          , deploymentsLoaded = False
          , deployments = deployments
          , stemcellsLoaded = False
          , stemcells = stemcells
          , activitiesLoaded = True
          , activities = activities
          }
        , Cmd.map ActivitiesMsg cmd
        )



-- MESSAGE, UPDATE


type Msg
    = SelectTab Int
    | Mdl Material.Msg
    | DeploymentsMsg Deployments.Msg
    | ActivitiesMsg Activities.Msg
    | StemcellsMsg Stemcells.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectTab idx ->
            case idx of
                0 ->
                    if model.activitiesLoaded then
                        ( { model | tab = idx }, Cmd.none )
                    else
                        let
                            ( activities, cmd ) =
                                Activities.update Activities.GetActivities model.activities
                        in
                            ( { model | tab = idx, activities = activities, activitiesLoaded = True }
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
            [ Tabs.textLabel [] "Activities"
            , Tabs.textLabel [] "VMs"
            , Tabs.textLabel [] "Stemcells"
            ]
            [ case model.tab of
                0 ->
                    div [] [ App.map ActivitiesMsg (Activities.view model.activities) ]

                1 ->
                    div [] [ App.map DeploymentsMsg (Deployments.view model.deployments) ]

                2 ->
                    div [] [ App.map StemcellsMsg (Stemcells.view model.stemcells) ]

                _ ->
                    div [] []
            ]
        ]
