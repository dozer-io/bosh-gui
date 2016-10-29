module HttpAuth.Front.Basic exposing (view, update, Msg)

import Material
import Material.Grid exposing (grid, noSpacing, cell, size, offset, stretch, align, Align(..), Device(..))
import Material.Textfield as Textfield
import Material.Button as Button
import Material.Options exposing (css)
import Material.Card as Card
import Material.Color as Color
import Material.Typography exposing (right)
import Html as Html exposing (text, p)
import HttpAuth
import HttpAuth.Basic exposing (HttpBasicClient, clientRequiresInput)


-- MESSAGE, UPDATE


type Msg
    = Mdl (Material.Msg Msg)
    | ChangeUsername String
    | ChangePassword String
    | Submit


update : Msg -> HttpBasicClient -> ( HttpBasicClient, Cmd Msg )
update msg client =
    case msg of
        Mdl message' ->
            Material.update message' client

        ChangeUsername username ->
            ( { client | username = Just username }, Cmd.none )

        ChangePassword password ->
            ( { client | password = Just password }, Cmd.none )

        Submit ->
            let
                client' =
                    { client | requiresInput = False }
            in
                ( client', HttpAuth.updateClient <| HttpAuth.Basic client' )



-- VIEW


view : HttpBasicClient -> Maybe (Html.Html Msg)
view client =
    let
        username =
            Maybe.withDefault "" client.username

        password =
            Maybe.withDefault "" client.password
    in
        if client.requiresInput then
            Just
                <| grid
                    [ noSpacing
                    , css "position" "absolute"
                    , css "width" "100%"
                    , css "top" "0"
                    , css "bottom" "0"
                    , css "padding-bottom" "20%"
                    , Color.background Color.primary
                    ]
                    [ cell [ align Middle, size All 2, offset All 5 ]
                        [ Card.view []
                            [ Card.title []
                                [ Card.head [] [ text "Please Login" ] ]
                            , Card.text []
                                [ Textfield.render Mdl
                                    [ 0 ]
                                    client.mdl
                                    [ Textfield.label "Target"
                                    , Textfield.floatingLabel
                                    , Textfield.disabled
                                    , Textfield.value client.endpoint
                                    , Textfield.onInput ChangeUsername
                                    ]
                                , Textfield.render Mdl
                                    [ 1 ]
                                    client.mdl
                                    [ Textfield.label "Username"
                                    , Textfield.floatingLabel
                                    , Textfield.value username
                                    , Textfield.onInput ChangeUsername
                                    ]
                                , Textfield.render Mdl
                                    [ 2 ]
                                    client.mdl
                                    [ Textfield.label "Password"
                                    , Textfield.floatingLabel
                                    , Textfield.value password
                                    , Textfield.onInput ChangePassword
                                    ]
                                ]
                            , Card.actions [ Card.border, right ]
                                [ Button.render Mdl
                                    [ 0 ]
                                    client.mdl
                                    [ Button.primary
                                    , Button.ripple
                                    , Button.onClick Submit
                                    ]
                                    [ Html.text "Login" ]
                                ]
                            ]
                        ]
                    ]
        else
            Nothing
