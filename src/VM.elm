module VM exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)
import Html.App as App
import Json.Decode exposing (int, string, list, Decoder)
import Json.Decode.Pipeline exposing (decode, required)


main : Program Never
main =
    App.program
        { init = init sampleVM
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


decodeVM : Decoder VM
decodeVM =
    decode VM
        |> required "vm_cid" string
        |> required "ips" (list string)
        |> required "agent_id" string
        |> required "job_name" string
        |> required "index" int
        |> required "job_state" string
        |> required "resource_pool" string


sampleVM : VM
sampleVM =
    VM "c1745718-3c3a-425a-a9ab-4233c56d565a"
        [ "10.0.0.5", "192.168.0.5" ]
        "c51ed5a6-227e-4ac2-aed8-842b061ae883"
        "nats"
        0
        "running"
        "default"


init : VM -> ( Model, Cmd Msg )
init vm =
    ( Model vm False, Cmd.none )



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
        [ h3 []
            [ text <| "#" ++ (toString model.vm.index) ++ " " ++ model.vm.jobName
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
