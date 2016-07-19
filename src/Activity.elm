module Activity exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)
import Material
import Material.Button as Button exposing (..)
import Material.Icon as Icon
import Material.Color as Color exposing (color, Hue(..), Shade(..), background, white)
import Material.Options exposing (styled, nop, cs)
import Html.App as App
import Html.Attributes exposing (class, style)
import Json.Decode exposing (int, string, list, float, oneOf, null, Decoder)
import Json.Decode.Pipeline exposing (decode, required)
import String
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
    { mdl : Material.Model, activity : Activity, now : Time, selected : Bool }


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
        |> required "result" (oneOf [ string, null "" ])
        |> required "user" string


time : Decoder Time
time =
    float `Json.Decode.andThen` \second -> Json.Decode.succeed <| Time.second * second


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
    ( Model Material.model activity (second * 0) False, Task.perform Tick Tick Time.now )



-- MESSAGE, UPDATE


type Msg
    = Mdl Material.Msg
    | Tick Time.Time
    | Select
    | DeSelect


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mdl msg ->
            Material.update Mdl msg model

        Tick now ->
            ( { model | now = now }, Cmd.none )

        Select ->
            ( { model | selected = True }, Cmd.none )

        DeSelect ->
            ( { model | selected = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    styled li
        [ cs "mdl-list__item mdl-list__item--three-line", selectedStyle model.selected ]
        [ span [ class "mdl-list__item-primary-content", Html.Events.onClick Select ]
            [ i [ class "material-icons mdl-list__item-avatar" ]
                [ text "person" ]
            , span []
                [ text <| "#" ++ (toString model.activity.id) ++ " " ++ model.activity.description
                ]
            , span [ class "mdl-list__item-text-body", style [ ( "whiteSpace", "nowrap" ) ] ]
                [ text <| String.slice 0 65 model.activity.result
                , br [] []
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
        , span [ class "mdl-list__item-secondary-action" ]
            [ Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.icon
                , Button.ripple
                , Button.onClick Select
                ]
                [ Icon.i "keyboard_arrow_right" ]
            ]
        ]


selectedStyle selected =
    if selected then
        background <| color LightBlue S50
    else
        nop



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.second * 30) Tick
