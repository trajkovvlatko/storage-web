module Pages.Colors exposing (Model, Msg, page)

import Auth
import Const exposing (host)
import Domain.Color exposing (Color, Colors, colorDecoder, colorsDecoder)
import Gen.Route exposing (toHref)
import Html exposing (Html, a, button, div, h1, table, td, text, th, thead, tr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http exposing (header)
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import UI
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.protected.element <|
        \user ->
            { init = init shared.storage
            , update = update shared.storage
            , view = view user
            , subscriptions = \_ -> Sub.none
            }


type State
    = Failure
    | Loading
    | Done
    | DeleteFailure


type alias Model =
    { state : State
    , colors : Colors
    }



-- INIT


init : Storage -> ( Model, Cmd Msg )
init storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, colors = [] }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading, colors = [] }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/colors"
                , body = Http.emptyBody
                , expect = Http.expectJson GotResponse colorsDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error Colors)
    | Delete Int
    | Deleted (Result Http.Error Color)


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case storage.user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                Delete id ->
                    ( model
                    , Http.request
                        { method = "DELETE"
                        , headers = [ header "token" u.token ]
                        , url = host ++ "/colors/" ++ String.fromInt id
                        , body = Http.emptyBody
                        , expect = Http.expectJson Deleted colorDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                Deleted result ->
                    case result of
                        Ok response ->
                            ( { model | colors = List.filter (\x -> x.id /= response.id) model.colors }, Cmd.none )

                        Err _ ->
                            ( { model | state = DeleteFailure }, Cmd.none )

                GotResponse result ->
                    case result of
                        Ok response ->
                            ( { state = Done, colors = response }, Cmd.none )

                        Err _ ->
                            ( { state = Failure, colors = [] }, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Colors"
    , body =
        UI.layout (Just user)
            [ h1 [] [ text "Colors" ]
            , case model.state of
                Failure ->
                    div [] [ text "Failed to load colors." ]

                DeleteFailure ->
                    div [] [ text "Failed to delete a color." ]

                Loading ->
                    div [] [ text "Loading colors..." ]

                Done ->
                    table
                        []
                        (thead []
                            [ th [] [ text "ID" ]
                            , th [] [ text "Label" ]
                            , th [] [ text "" ]
                            ]
                            :: List.map colorRow model.colors
                        )
            , a [ href (Gen.Route.toHref Gen.Route.Colors__New) ] [ text "Add new color" ]
            ]
    }


colorRow : Color -> Html Msg
colorRow color =
    let
        editUrl =
            toHref (Gen.Route.Colors__ColorId___Edit { colorId = String.fromInt color.id })
    in
    tr []
        [ td [] [ text (String.fromInt color.id) ]
        , td [] [ text color.label ]
        , td [] [ a [ href editUrl ] [ text "Edit" ] ]
        , td [] [ button [ onClick (Delete color.id) ] [ text "Delete" ] ]
        ]
