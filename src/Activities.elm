module Activities exposing (..)

import Html exposing (..)
import Html.App as App
import TimeTravel.Html.App as TimeTravel
import Http
import Json.Decode exposing (..)
import List exposing (map)
import List.Extra exposing (getAt, setAt)
import Platform.Cmd exposing (Cmd)
import Task
import Activity


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
    , loading : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] True, getActivities )



-- ACTION, UPDATE


type Msg
    = GetActivities
    | GetActivitiesFail Http.Error
    | GetActivitiesSucceed (List Activity.Activity)
    | SubMsg Int Activity.Msg


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
                        ( activityModel, Cmd.map (SubMsg id) cmd )

                ( newActivities, cmds ) =
                    List.unzip (List.indexedMap createActivity activities)
            in
                ( { model | loading = False, activities = newActivities }, Cmd.batch cmds )

        SubMsg id subMsg ->
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
                                ( { model | activities = activities }, Cmd.map (SubMsg id) cmds )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ (if model.loading then
            h1 [] [ text "Loading" ]
           else
            text ""
          )
        , div []
            <| List.indexedMap viewActivity model.activities
        ]


viewActivity : Int -> Activity.Model -> Html Msg
viewActivity id model =
    App.map (SubMsg id) (Activity.view model)



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
