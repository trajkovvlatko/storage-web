module UI exposing (h1, layout)

import Auth
import Gen.Route as Route exposing (Route)
import Html exposing (Html)
import Html.Attributes as Attr


viewLink : String -> Route -> Html msg
viewLink label route =
    Html.a [ Attr.href (Route.toHref route) ] [ Html.text label ]


authButtons : Maybe Auth.User -> Html msg
authButtons user =
    case user of
        Nothing ->
            Html.div []
                [ viewLink "Login" Route.Login
                ]

        Just _ ->
            Html.div []
                [ viewLink "Logout" Route.Home_
                ]


layout : Maybe Auth.User -> List (Html msg) -> List (Html msg)
layout user children =
    [ Html.div [ Attr.style "margin" "2rem" ]
        [ Html.header [ Attr.style "margin-bottom" "1rem" ]
            [ Html.strong [ Attr.style "margin-right" "1rem" ] [ viewLink "Home" Route.Home_ ]
            , authButtons user
            ]
        , Html.main_ [] children
        ]
    ]


h1 : String -> Html msg
h1 label =
    Html.h1 [] [ Html.text label ]
