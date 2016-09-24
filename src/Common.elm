module Common exposing (..)

import Material.Options as Options
import Material.Spinner as Loading
import Material.Typography as Typo
import Html exposing (..)


loaderSpinner : String -> Html a
loaderSpinner desc =
    div []
        [ Options.styled p
            [ Options.center ]
            [ Loading.spinner
                [ Loading.active True
                , Loading.singleColor True
                ]
            ]
        , Options.styled p
            [ Options.center, Typo.caption ]
            [ text desc ]
        ]


loaderText : String -> Html a
loaderText desc =
    Options.styled p [ Typo.caption ] [ text desc ]
