module HttpAuth.Part exposing (urlUpdate, init, view, update, subscriptions, Msg, Model)

import HttpAuth.OAuth exposing (buildAuthUrl)
import HttpAuth exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href)


urlUpdate : String -> Model -> ( Model, Cmd a )
urlUpdate token model =
    let
        client =
            case model.client of
                OAuth client' ->
                    OAuth { client' | token = token }
    in
        ( { model | client = client }, updateClient client )



-- SUBSCRIPTIONS


subscriptions : (Msg -> a) -> Sub a
subscriptions tagger =
    userClientInput (tagger << UserInput)



-- MODEL


type alias Model =
    { client : Client
    , userInputRequired : Bool
    }


init : Client -> ( Model, Cmd a )
init client =
    ( Model client False, HttpAuth.updateClient client )



-- MESSAGE, UPDATE


type Msg
    = UserInput Client


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserInput client ->
            ( { model | userInputRequired = True }, Cmd.none )



-- VIEW


view : Model -> Maybe (Html Msg)
view model =
    if model.userInputRequired then
        case model.client of
            OAuth client' ->
                Just
                    <| div []
                        [ text "Please login"
                        , a [ href <| buildAuthUrl client' ] [ text "click here" ]
                        ]
    else
        Nothing
