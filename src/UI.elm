module UI exposing (layout)

import Auth
import Gen.Route as Route exposing (Route)
import Html exposing (Html, a, div, header, main_, text)
import Html.Attributes as Attr exposing (href)


authButtons : Maybe Auth.User -> Html msg
authButtons user =
    case user of
        Nothing ->
            div []
                [ a [ href (Route.toHref Route.Login) ] [ text "Login" ]
                , text " | "
                , a [ href (Route.toHref Route.Register) ] [ text "Register" ]
                ]

        Just _ ->
            div []
                [ a [ href (Route.toHref Route.Home_) ] [ text "Home" ]
                , text " | "
                , a [ href (Route.toHref Route.Rooms) ] [ text "Browse" ]
                , text " | "
                , a [ href (Route.toHref Route.Colors) ] [ text "Colors" ]
                , text " | "
                , a [ href (Route.toHref Route.ItemTypes) ] [ text "Item types" ]
                , text " | "
                , a [ href (Route.toHref Route.Logout) ] [ text "Logout" ]
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
