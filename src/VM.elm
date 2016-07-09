module Main exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { vm : VM
    , expanded : Bool
    }


type alias VM =
    { vmCid : String
    , ips : List String
    , agentId : String
    , jobName : String
    , index : Int
    , jobState : String
    , resourcePool : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (VM "b0e3f459-75ad-4cb6-745f-ee5f5ec6acd9"
            [ "10.244.8.22" ]
            "917f4529-4599-453b-a670-7368efb11683"
            "docker"
            1
            "running"
            "default"
        )
        False
    , Cmd.none
    )



-- MESSAGE, UPDATE


type Msg
    = ToggleExpanded


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleExpanded ->
            ( { model | expanded = not model.expanded }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ onClick ToggleExpanded ]
        [ h1 []
            [ text model.vm.jobName
            ]
        , p []
            [ text
                <| if model.expanded then
                    "expanded"
                   else
                    "collapsed"
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
