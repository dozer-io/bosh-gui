module HttpAuth.OAuth exposing (OAuthClient, buildAuthUrl, getTokenFromHash, clientRequiresInput, authHeader)

import Http
import String
import Dict


type alias OAuthClient =
    { endpoint : String
    , clientId : String
    , scopes : List String
    , redirectUrl : String
    , token : String
    }


clientRequiresInput : OAuthClient -> Bool
clientRequiresInput client =
    --  TODO: use http://package.elm-lang.org/packages/simonh1000/elm-jwt/2.0.0
    --  to check if token has expired
    String.isEmpty client.token


authHeader : OAuthClient -> ( String, String )
authHeader { token } =
    ( "Authorization", "bearer " ++ token )


buildAuthUrl : OAuthClient -> String
buildAuthUrl client =
    Http.url client.endpoint
        [ ( "response_type", "token" )
        , ( "immediate", "true" )
        , ( "approval_prompt", "auto" )
        , ( "client_id", client.clientId )
        , ( "redirect_uri", client.redirectUrl )
        , ( "scope", String.join " " client.scopes )
        ]


getTokenFromHash : String -> String
getTokenFromHash s =
    let
        params =
            parseUrlParams s
    in
        Maybe.withDefault ""
            <| Dict.get "access_token" params


parseUrlParams : String -> Dict.Dict String String
parseUrlParams s =
    s
        |> String.dropLeft 1
        |> String.split "&"
        |> List.map parseSingleParam
        |> Dict.fromList


parseSingleParam : String -> ( String, String )
parseSingleParam p =
    let
        s =
            String.split "=" p
    in
        case s of
            [ s1, s2 ] ->
                ( s1, s2 )

            _ ->
                ( "", "" )
