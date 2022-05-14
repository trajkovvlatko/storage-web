module Pages.Colors.ColorId_.Edit exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.Color exposing (Color, colorDecoder)
import Gen.Params.Colors.ColorId_.Edit exposing (Params)
import Gen.Route
import Html exposing (button, div, form, input, label, text)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (header, multipartBody, stringPart)
import Page
import Request
import Shared
import Storage exposing (Storage)
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.element <|
        \user ->
            { init = init req shared.storage
            , update = update req shared.storage.user
            , view = view user
            , subscriptions = \_ -> Sub.none
            }



-- INIT


type State
    = Loading
    | Loaded
    | Failure


type alias Model =
    { state : State
    , color : Maybe Color
    }


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init req storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, color = Nothing }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading, color = Nothing }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/colors/" ++ req.params.colorId
                , body = Http.emptyBody
                , expect = Http.expectJson GotGetResponse colorDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = UpdatedLabel String
    | SubmittedColorForm
    | GotGetResponse (Result Http.Error Color)
    | GotPatchResponse (Result Http.Error Color)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedLabel label ->
                    case model.color of
                        Nothing ->
                            ( { model | color = Nothing }, Cmd.none )

                        Just r ->
                            ( { model | color = Just { id = r.id, label = label } }, Cmd.none )

                SubmittedColorForm ->
                    ( model
                    , case model.color of
                        Nothing ->
                            Cmd.none

                        Just r ->
                            Http.request
                                { method = "PATCH"
                                , headers = [ header "token" u.token ]
                                , url = host ++ "/colors/" ++ String.fromInt r.id
                                , body =
                                    multipartBody [ stringPart "label" r.label ]
                                , expect = Http.expectJson GotPatchResponse colorDecoder
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                    )

                GotGetResponse result ->
                    case result of
                        Ok r ->
                            ( { state = Loaded, color = Just r }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                GotPatchResponse result ->
                    case result of
                        Ok _ ->
                            ( model
                            , Request.pushRoute Gen.Route.Colors req
                            )

                        Err _ ->
                            ( model, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Edit color"
    , body =
        UI.layout (Just user)
            [ case model.state of
                Failure ->
                    div [] [ text "An error occured." ]

                Loading ->
                    div [] [ text "Updating..." ]

                Loaded ->
                    case model.color of
                        Nothing ->
                            button [ disabled True ] [ text "Update" ]

                        Just r ->
                            form [ onSubmit SubmittedColorForm ]
                                [ label []
                                    [ div []
                                        [ text "Label:"
                                        , input [ type_ "text", onInput UpdatedLabel, value r.label ] []
                                        ]
                                    ]
                                , button [ disabled (String.isEmpty r.label) ] [ text "Update" ]
                                ]
            ]
    }
