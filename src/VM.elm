module VM exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Platform.Cmd exposing (Cmd)


-- import Html.App as App
-- main : Program Never
-- main =
--     App.program
--         { init = init
--         , view = view
--         , update = update
--         , subscriptions = subscriptions
--         }
-- MODEL


type alias Model =
    { vm : VM
    , expanded : Bool
    }


type alias VM =
    { vmCid :
        String
        -- , ips : List String
        -- , agentId : String
    , jobName :
        String
        -- , index : Int
        -- , jobState : String
        -- , resourcePool : String
    }


init : VM -> Model
init vm =
    (Model vm False)



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
