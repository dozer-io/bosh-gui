module HttpAuth.Part exposing (urlUpdate, init, view, update, subscriptions, Msg, Model)

import HttpAuth.OAuth exposing (buildAuthUrl)
import HttpAuth.Front.Basic as FrontBasic
import HttpAuth exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.App as App


urlUpdate : String -> Model -> ( Model, Cmd a )
urlUpdate token model =
    let
        client =
            case model.client of
                OAuth client' ->
                    OAuth { client' | token = token }

                HttpAuth.Basic _ ->
                    client
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
    | BasicMsg FrontBasic.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UserInput client ->
            ( { model | userInputRequired = True }, Cmd.none )

        BasicMsg msg' ->
            case model.client of
                OAuth _ ->
                    ( model, Cmd.none )

                Basic client ->
                    let
                        ( client', cmd ) =
                            FrontBasic.update msg' client
                    in
                        ( { model | client = Basic client' }, Cmd.map BasicMsg cmd )



-- VIEW


view : Model -> Maybe (Html Msg)
view model =
    case model.client of
        OAuth client' ->
            if model.userInputRequired then
                Just
                    <| div []
                        [ text "Please login"
                        , a [ href <| buildAuthUrl client' ] [ text "click here" ]
                        ]
            else
                Nothing

        Basic client' ->
            case FrontBasic.view client' of
                Nothing ->
                    Nothing

                Just view ->
                    Just <| App.map BasicMsg view
