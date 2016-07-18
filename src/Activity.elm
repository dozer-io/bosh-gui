module Activity exposing (..)

import Html exposing (..)


-- import Html.Events exposing (onClick)

import Platform.Cmd exposing (Cmd)
import Material
import Html.App as App
import Html.Attributes exposing (class)
import Json.Decode exposing (int, string, list, float, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import Time exposing (..)
import TimeAgo exposing (timeAgo)
import Task exposing (perform)


main : Program Never
main =
    App.program
        { init = init sampleActivity
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Mdl =
    Material.Model


type alias Model =
    { mdl : Material.Model, activity : Activity, now : Time }


type alias Activity =
    { id : Int
    , state : String
    , description : String
    , timestamp : Time
    , result : String
    , user : String
    }


decodeActivity : Decoder Activity
decodeActivity =
    decode Activity
        |> required "id" int
        |> required "state" string
        |> required "description" string
        |> required "timestamp" time
        |> required "result" string
        |> required "user" string


time : Decoder Time
time =
    float `Json.Decode.andThen` \ms -> Json.Decode.succeed <| Time.millisecond * ms


sampleActivity : Activity
sampleActivity =
    Activity 1180
        "processing"
        "run errand acceptance_tests from deployment cf-warden"
        (second * 1447033291)
        "Created release 'redis/12'"
        "admin"


init : Activity -> ( Model, Cmd Msg )
init activity =
    ( Model Material.model activity (second * 0), Task.perform Tick Tick Time.now )



-- MESSAGE, UPDATE


type Msg
    = Mdl Material.Msg
    | Tick Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg ->
            Material.update Mdl msg model

        Tick now ->
            ( { model | now = now }, Cmd.none )



-- VIEW
-- <span class="mdl-list__item-primary-content">
--     <i class="material-icons mdl-list__item-avatar">person</i>
--     <span>Bryan Cranston</span>
--     <span class="mdl-list__item-sub-title">62 Episodes</span>
-- </span>
-- <span class="mdl-list__item-secondary-content">
--     <span class="mdl-list__item-secondary-info">Actor</span>
--     <a class="mdl-list__item-secondary-action" href="#"><i class="material-icons">star</i></a>
-- </span>


view : Model -> Html Msg
view model =
    li [ class "mdl-list__item mdl-list__item--two-line" ]
        [ span [ class "mdl-list__item-primary-content" ]
            [ i [ class "material-icons mdl-list__item-avatar" ]
                [ text "person" ]
            , span []
                [ text <| "#" ++ (toString model.activity.id) ++ " " ++ model.activity.description
                ]
            , span [ class "mdl-list__item-sub-title" ]
                [ text model.activity.result
                , b []
                    [ text
                        <| model.activity.state
                        ++ " "
                        ++ (timeAgo model.now model.activity.timestamp)
                        ++ " by "
                        ++ model.activity.user
                    ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.second * 30) Tick
