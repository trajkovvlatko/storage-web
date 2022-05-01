module Pages.Rooms.RoomId_.StorageUnits.StorageUnitId_.Edit exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.StorageUnit exposing (StorageUnit, storageUnitDecoder)
import Gen.Params.Rooms.RoomId_.StorageUnits.StorageUnitId_.Edit exposing (Params)
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
    , storageUnit : Maybe StorageUnit
    }


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init req storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, storageUnit = Nothing, roomId = req.params.roomId }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading, storageUnit = Nothing, roomId = req.params.roomId }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/storage_units/" ++ req.params.storageUnitId
                , body = Http.emptyBody
                , expect = Http.expectJson GotGetResponse storageUnitDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = UpdatedName String
    | SubmittedStorageUnitForm
    | GotGetResponse (Result Http.Error StorageUnit)
    | GotPatchResponse (Result Http.Error StorageUnit)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedName name ->
                    case model.storageUnit of
                        Nothing ->
                            ( { model | storageUnit = Nothing }, Cmd.none )

                        Just r ->
                            ( { model | storageUnit = Just { id = r.id, room_id = r.room_id, name = name } }, Cmd.none )

                SubmittedStorageUnitForm ->
                    ( model
                    , case model.storageUnit of
                        Nothing ->
                            Cmd.none

                        Just r ->
                            Http.request
                                { method = "PATCH"
                                , headers = [ header "token" u.token ]
                                , url = host ++ "/storage_units/" ++ String.fromInt r.id
                                , body =
                                    multipartBody [ stringPart "name" r.name ]
                                , expect = Http.expectJson GotPatchResponse storageUnitDecoder
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                    )

                GotGetResponse result ->
                    case result of
                        Ok s ->
                            ( { model | state = Loaded, storageUnit = Just s }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                GotPatchResponse result ->
                    case result of
                        Ok _ ->
                            ( model
                            , Request.pushRoute (Gen.Route.Rooms__RoomId___StorageUnits { roomId = model.roomId }) req
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
                    case model.storageUnit of
                        Nothing ->
                            button [ disabled True ] [ text "Update" ]

                        Just r ->
                            form [ onSubmit SubmittedStorageUnitForm ]
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
