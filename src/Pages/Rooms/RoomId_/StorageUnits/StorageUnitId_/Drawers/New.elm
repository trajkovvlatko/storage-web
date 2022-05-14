module Pages.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.New exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.Drawer exposing (Drawer, drawerDecoder)
import Gen.Params.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.New exposing (Params)
import Gen.Route
import Html exposing (button, div, form, input, label, text)
import Html.Attributes exposing (disabled, type_)
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
    = Pending
    | Loading
    | Failure


type alias Model =
    { state : State
    , note : String
    , level : String
    , roomId : String
    , storageUnitId : String
    }


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init { params } _ =
    ( { state = Pending, note = "", level = "", roomId = params.roomId, storageUnitId = params.storageUnitId }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdatedNote String
    | UpdatedLevel String
    | SubmittedDrawerForm
    | GotResponse (Result Http.Error Drawer)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedNote note ->
                    ( { model | note = note }, Cmd.none )

                UpdatedLevel level ->
                    ( { model | level = level }, Cmd.none )

                SubmittedDrawerForm ->
                    ( model
                    , Http.request
                        { method = "POST"
                        , headers = [ header "token" u.token ]
                        , url = host ++ "/drawers"
                        , body =
                            multipartBody
                                [ stringPart "note" model.note
                                , stringPart "level" model.level
                                , stringPart "storage_unit_id" model.storageUnitId
                                ]
                        , expect = Http.expectJson GotResponse drawerDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                GotResponse result ->
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
    { title = "Add new drawer"
    , body =
        UI.layout (Just user)
            [ form [ onSubmit SubmittedDrawerForm ]
                [ label []
                    [ div []
                        [ text "Note:"
                        , input [ type_ "text", onInput UpdatedNote ] []
                        ]
                    ]
                , label []
                    [ div []
                        [ text "Level:"
                        , input [ type_ "number", onInput UpdatedLevel ] []
                        ]
                    ]
                , case model.state of
                    Failure ->
                        div [] [ text "An error occured." ]

                    Loading ->
                        div [] [ text "Saving..." ]

                    Pending ->
                        button [ disabled (String.isEmpty model.note) ] [ text "Save" ]
                ]
            ]
    }
