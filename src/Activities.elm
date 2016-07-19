module Activities exposing (..)

import Material.Grid exposing (grid, cell, size, noSpacing, Device(..))
import Material.Options as Options exposing (css)
import Material.Elevation exposing (e3)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.App as App
import TimeTravel.Html.App as TimeTravel
import Http
import Json.Decode exposing (..)
import List exposing (map)
import List.Extra exposing (getAt, setAt)
import Platform.Cmd exposing (Cmd)
import Task
import Activity
import TaskEventOutput


main : Program Never
main =
    --    App.program
    TimeTravel.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { activities : List Activity.Model
    , taskEventOutput : Maybe TaskEventOutput.Model
    , loading : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] Nothing True, getActivities )



-- ACTION, UPDATE


type Msg
    = GetActivities
    | GetActivitiesFail Http.Error
    | GetActivitiesSucceed (List Activity.Activity)
    | SubMsgActivity Int Activity.Msg
    | SubMsgTaskEventOutput TaskEventOutput.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        GetActivities ->
            ( model, getActivities )

        GetActivitiesFail _ ->
            ( model, Cmd.none )

        GetActivitiesSucceed activities ->
            let
                createActivity id activity =
                    let
                        ( activityModel, cmd ) =
                            Activity.init activity
                    in
                        ( activityModel, Cmd.map (SubMsgActivity id) cmd )

                ( newActivities, cmds ) =
                    List.unzip (List.indexedMap createActivity activities)
            in
                ( { model | loading = False, activities = newActivities }, Cmd.batch cmds )

        SubMsgActivity id subMsg ->
            case subMsg of
                Activity.Select ->
                    let
                        ( activities, activitiesCmds ) =
                            List.unzip
                                <| List.indexedMap (selectById id) model.activities
                    in
                        case List.Extra.getAt id activities of
                            Nothing ->
                                ( model, Cmd.none )

                            Just selectedActivity ->
                                let
                                    ( taskEventOutput, taskEventOutputCmd ) =
                                        TaskEventOutput.init selectedActivity.activity.id
                                in
                                    ( { model
                                        | activities = activities
                                        , taskEventOutput = Just taskEventOutput
                                      }
                                    , Cmd.batch
                                        <| (Cmd.map SubMsgTaskEventOutput taskEventOutputCmd)
                                        :: activitiesCmds
                                    )

                _ ->
                    case getAt id model.activities of
                        Nothing ->
                            ( model, Cmd.none )

                        Just activity ->
                            let
                                ( newActivity, cmds ) =
                                    Activity.update subMsg activity
                            in
                                case setAt id newActivity model.activities of
                                    Nothing ->
                                        ( model, Cmd.none )

                                    Just activities ->
                                        ( { model | activities = activities }, Cmd.map (SubMsgActivity id) cmds )

        SubMsgTaskEventOutput subMsg ->
            case model.taskEventOutput of
                Just currentTaskEventOutput ->
                    let
                        ( taskEventOutput, cmd ) =
                            TaskEventOutput.update subMsg currentTaskEventOutput
                    in
                        ( { model | taskEventOutput = (Just taskEventOutput) }
                        , Cmd.map (SubMsgTaskEventOutput) cmd
                        )

                Nothing ->
                    ( model, Cmd.none )


selectById : Int -> Int -> Activity.Model -> ( Activity.Model, Cmd Msg )
selectById selectedId id activity =
    let
        msg =
            if selectedId == id then
                Activity.Select
            else
                Activity.DeSelect

        ( model, cmds ) =
            Activity.update msg activity
    in
        ( model, Cmd.map (SubMsgActivity id) cmds )



-- VIEW


view : Model -> Html Msg
view model =
    grid [ noSpacing ]
        [ cell [ size All 4, e3, css "display" "inline-flex" ]
            [ (if model.loading then
                h1 [] [ text "Loading" ]
               else
                text ""
              )
            , ul [ class "mdl-list", style [ ( "width", "100%" ) ] ]
                <| List.indexedMap viewActivity model.activities
            ]
        , cell [ size All 8 ]
            [ case model.taskEventOutput of
                Just taskEventOutput ->
                    App.map SubMsgTaskEventOutput <| TaskEventOutput.view taskEventOutput

                Nothing ->
                    p [] [ text "select a task from the list on the left side of your screen" ]
            ]
        ]


viewActivity : Int -> Activity.Model -> Html Msg
viewActivity id model =
    App.map (SubMsgActivity id) (Activity.view model)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- HTTP


getActivities : Cmd Msg
getActivities =
    let
        url =
            "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000/tasks?limit=20&verbose=0"
    in
        Task.perform GetActivitiesFail GetActivitiesSucceed
            <| Http.get decodeActivities url


decodeActivities : Decoder (List Activity.Activity)
decodeActivities =
    Json.Decode.list Activity.decodeActivity
