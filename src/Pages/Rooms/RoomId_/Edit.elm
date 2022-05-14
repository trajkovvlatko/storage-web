module Pages.Rooms.RoomId_.Edit exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.Room exposing (Room, roomDecoder)
import Gen.Params.Rooms.RoomId_.Edit exposing (Params)
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
    , room : Maybe Room
    }


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init req storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, room = Nothing }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading, room = Nothing }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/rooms/" ++ req.params.roomId
                , body = Http.emptyBody
                , expect = Http.expectJson GotGetResponse roomDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = UpdatedName String
    | SubmittedRoomForm
    | GotGetResponse (Result Http.Error Room)
    | GotPatchResponse (Result Http.Error Room)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedName name ->
                    case model.room of
                        Nothing ->
                            ( { model | room = Nothing }, Cmd.none )

                        Just r ->
                            ( { model | room = Just { id = r.id, name = name } }, Cmd.none )

                SubmittedRoomForm ->
                    ( model
                    , case model.room of
                        Nothing ->
                            Cmd.none

                        Just r ->
                            Http.request
                                { method = "PATCH"
                                , headers = [ header "token" u.token ]
                                , url = host ++ "/rooms/" ++ String.fromInt r.id
                                , body =
                                    multipartBody [ stringPart "name" r.name ]
                                , expect = Http.expectJson GotPatchResponse roomDecoder
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                    )

                GotGetResponse result ->
                    case result of
                        Ok r ->
                            ( { state = Loaded, room = Just r }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                GotPatchResponse result ->
                    case result of
                        Ok _ ->
                            ( model
                            , Request.pushRoute Gen.Route.Rooms req
                            )

                        Err _ ->
                            ( model, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Edit room"
    , body =
        UI.layout (Just user)
            [ case model.state of
                Failure ->
                    div [] [ text "An error occured." ]

                Loading ->
                    div [] [ text "Updating..." ]

                Loaded ->
                    case model.room of
                        Nothing ->
                            button [ disabled True ] [ text "Update" ]

                        Just r ->
                            form [ onSubmit SubmittedRoomForm ]
                                [ label []
                                    [ div []
                                        [ text "Name:"
                                        , input [ type_ "text", onInput UpdatedName, value r.name ] []
                                        ]
                                    ]
                                , button [ disabled (String.isEmpty r.name) ] [ text "Update" ]
                                ]
            ]
    }
