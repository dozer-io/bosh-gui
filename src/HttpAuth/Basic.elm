module HttpAuth.Basic exposing (HttpBasicClient, clientRequiresInput, authHeader, init)

import Base64
import String
import Material


type alias HttpBasicClient =
    { endpoint : String
    , username : Maybe String
    , password : Maybe String
    , requiresInput : Bool
    , mdl : Material.Model
    }


init : String -> HttpBasicClient
init endpoint =
    { endpoint = endpoint
    , username = Nothing
    , password = Nothing
    , requiresInput = True
    , mdl = Material.model
    }


clientRequiresInput : HttpBasicClient -> Bool
clientRequiresInput { requiresInput } =
    requiresInput


authHeader : HttpBasicClient -> ( String, String )
authHeader client =
    let
        username =
            Maybe.withDefault "" client.username

        password =
            Maybe.withDefault "" client.password

        hash =
            Result.withDefault ""
                <| Base64.encode
                <| String.concat
                <| [ username, ":", password ]
    in
        ( "Authorization", "Basic " ++ hash )
