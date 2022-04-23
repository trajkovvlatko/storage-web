module UI exposing (layout)

import Auth
import Gen.Route as Route exposing (Route)
import Html exposing (Html, a, div, h1, header, main_, strong, text)
import Html.Attributes as Attr exposing (href)
import Html.Events as Events


viewLink : String -> Route -> Html msg
viewLink label route =
    a [ href (Route.toHref route) ] [ text label ]


authButtons : Maybe Auth.User -> Html msg
authButtons user =
    case user of
        Nothing ->
            a [ href (Route.toHref Route.Login) ] [ text "Login" ]

        Just _ ->
            div []
                [ a [ href (Route.toHref Route.Home_) ] [ text "Home" ]
                , text " | "
                , a [ href (Route.toHref Route.Rooms) ] [ text "Rooms" ]
                , text " | "
                , a [ href (Route.toHref Route.Home_) ] [ text "Logout" ]
                ]


layout : Maybe Auth.User -> List (Html msg) -> List (Html msg)
layout user children =
    [ div [ Attr.style "margin" "2rem" ]
        [ header [ Attr.style "margin-bottom" "1rem" ]
            [ authButtons user
            ]
        , main_ [] children
        ]
    ]
