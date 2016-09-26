module Activities exposing (..)

import Material.Grid exposing (grid, cell, size, noSpacing, Device(..))
import Material.Options as Options exposing (css, cs)
import Material.Color as Color exposing (color, Hue(..), Shade(..), background)
import Material.Elevation exposing (e3)
import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.App as App
import TimeTravel.Html.App as TimeTravel
import Http
import HttpAuth
import Json.Decode exposing (..)
import List exposing (map)
import List.Extra exposing (getAt, setAt)
import Platform.Cmd exposing (Cmd)
import Activity
import TaskEventOutput
import Erl


main : Program Never
main =
    --    App.program
    TimeTravel.program
        { init = init "http://localhost:8001/bosh/00000000-0000-0000-0000-000000000000"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { activities : List Activity.Model
    , taskEventOutput : Maybe TaskEventOutput.Model
    , loading : Bool
    , endpoint : String
    }


init : String -> ( Model, Cmd Msg )
init endpoint =
    ( Model [] Nothing True endpoint, getActivities endpoint )



-- ACTION, UPDATE


type Msg
    = GetActivities
    | GetActivitiesFail Http.RawError
    | GetActivitiesSucceed String
    | SubMsgActivity Int Activity.Msg
    | SubMsgTaskEventOutput TaskEventOutput.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        GetActivities ->
            ( model, getActivities model.endpoint )

        GetActivitiesFail _ ->
            ( model, Cmd.none )

        GetActivitiesSucceed string ->
            let
                activities =
                    Result.withDefault []
                        <| decodeString decodeActivities string

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
                                    refresh =
                                        case selectedActivity.activity.state of
                                            "queued" ->
                                                True

                                            "processing" ->
                                                True

                                            _ ->
                                                False

                                    ( taskEventOutput, taskEventOutputCmd ) =
                                        TaskEventOutput.init model.endpoint selectedActivity.activity.id (Debug.log "refresh" refresh)
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
        [ cell [ size All 4, e3, css "display" "inline-flex", css "z-index" "1" ]
            [ (if model.loading then
                h1 [] [ text "Loading" ]
               else
                text ""
              )
            , ul
                [ class "mdl-list"
                , style [ ( "width", "100%" ), ( "paddingTop", "0px" ), ( "marginTop", "0px" ) ]
                ]
                <| List.indexedMap viewActivity model.activities
            ]
        , cell [ size All 8, cs "mdl-components__pages", background <| color Grey S100 ]
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


getActivities : String -> Cmd Msg
getActivities endpoint =
    let
        url =
            Erl.parse endpoint
                |> Erl.appendPathSegments [ "tasks" ]
                |> Erl.addQuery "limit" "20"
                |> Erl.addQuery "verbose" "1"
                |> Erl.toString
    in
        HttpAuth.get url GetActivitiesFail GetActivitiesSucceed


decodeActivities : Decoder (List Activity.Activity)
decodeActivities =
    Json.Decode.list Activity.decodeActivity
