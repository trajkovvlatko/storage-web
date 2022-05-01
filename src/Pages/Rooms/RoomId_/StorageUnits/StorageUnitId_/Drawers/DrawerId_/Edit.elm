module Pages.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.DrawerId_.Edit exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.Drawer exposing (Drawer, drawerDecoder)
import Gen.Params.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.DrawerId_.Edit exposing (Params)
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
    , roomId : String
    , storageUnitId : String
    , drawer : Maybe Drawer
    }


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init req storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, drawer = Nothing, roomId = req.params.roomId, storageUnitId = req.params.storageUnitId }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading
              , drawer = Nothing
              , roomId = req.params.roomId
              , storageUnitId = req.params.storageUnitId
              }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/drawers/" ++ req.params.drawerId
                , body = Http.emptyBody
                , expect = Http.expectJson GotGetResponse drawerDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = UpdatedNote String
    | UpdatedLevel String
    | SubmittedDrawerForm
    | GotGetResponse (Result Http.Error Drawer)
    | GotPatchResponse (Result Http.Error Drawer)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedNote note ->
                    case model.drawer of
                        Nothing ->
                            ( { model | drawer = Nothing }, Cmd.none )

                        Just r ->
                            ( { model
                                | drawer =
                                    Just
                                        { id = r.id
                                        , storage_unit_id = r.storage_unit_id
                                        , note = note
                                        , level = r.level
                                        }
                              }
                            , Cmd.none
                            )

                UpdatedLevel level ->
                    case ( model.drawer, String.toInt level ) of
                        ( Just r, Just l ) ->
                            ( { model
                                | drawer =
                                    Just
                                        { id = r.id
                                        , storage_unit_id = r.storage_unit_id
                                        , note = r.note
                                        , level = l
                                        }
                              }
                            , Cmd.none
                            )

                        ( _, _ ) ->
                            ( { model | drawer = Nothing }, Cmd.none )

                SubmittedDrawerForm ->
                    ( model
                    , case model.drawer of
                        Nothing ->
                            Cmd.none

                        Just r ->
                            Http.request
                                { method = "PATCH"
                                , headers = [ header "token" u.token ]
                                , url = host ++ "/drawers/" ++ String.fromInt r.id
                                , body =
                                    multipartBody
                                        [ stringPart "note" r.note
                                        , stringPart "level" (String.fromInt r.level)
                                        ]
                                , expect = Http.expectJson GotPatchResponse drawerDecoder
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                    )

                GotGetResponse result ->
                    case result of
                        Ok s ->
                            ( { model | state = Loaded, drawer = Just s }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                GotPatchResponse result ->
                    case result of
                        Ok _ ->
                            ( model
                            , Request.pushRoute
                                (Gen.Route.Rooms__RoomId___StorageUnits__StorageUnitId___Drawers
                                    { roomId = model.roomId
                                    , storageUnitId = model.storageUnitId
                                    }
                                )
                                req
                            )

                        Err _ ->
                            ( model, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Homepage"
    , body =
        UI.layout (Just user)
            [ case model.state of
                Failure ->
                    div [] [ text "An error occured." ]

                Loading ->
                    div [] [ text "Updating..." ]

                Loaded ->
                    case model.drawer of
                        Nothing ->
                            button [ disabled True ] [ text "Update" ]

                        Just r ->
                            form [ onSubmit SubmittedDrawerForm ]
                                [ label []
                                    [ div []
                                        [ text "Note:"
                                        , input [ type_ "text", onInput UpdatedNote, value r.note ] []
                                        ]
                                    ]
                                , label []
                                    [ div []
                                        [ text "Level:"
                                        , input [ type_ "number", onInput UpdatedLevel, value (String.fromInt r.level) ] []
                                        ]
                                    ]
                                , button [ disabled (String.isEmpty r.note) ] [ text "Update" ]
                                ]
            ]
    }
