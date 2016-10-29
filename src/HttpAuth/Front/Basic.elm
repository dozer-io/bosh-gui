module HttpAuth.Front.Basic exposing (view, update, Msg)

import Material
import Material.Grid exposing (grid, cell, size, Device(..))
import Material.Textfield as Textfield
import Material.Button as Button
import Html as Html
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
                <| grid []
                    [ cell [ size All 12 ]
                        [ Textfield.render Mdl
                            [ 0 ]
                            client.mdl
                            [ Textfield.label "Username"
                            , Textfield.floatingLabel
                            , Textfield.value username
                            , Textfield.onInput ChangeUsername
                            ]
                        , Textfield.render Mdl
                            [ 1 ]
                            client.mdl
                            [ Textfield.label "Password"
                            , Textfield.floatingLabel
                            , Textfield.value password
                            , Textfield.onInput ChangePassword
                            ]
                        ]
                    , cell [ size All 12 ]
                        [ Button.render Mdl
                            [ 0 ]
                            client.mdl
                            [ Button.raised
                            , Button.ripple
                            , Button.onClick Submit
                            ]
                            [ Html.text "Login" ]
                        ]
                    ]
        else
            Nothing
