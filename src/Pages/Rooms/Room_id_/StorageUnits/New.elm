module Pages.Rooms.Room_id_.StorageUnits.New exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.StorageUnit exposing (StorageUnit, storageUnitDecoder)
import Gen.Params.Rooms.Room_id_.StorageUnits.New exposing (Params)
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
    , name : String
    , roomId : String
    }


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init { params } _ =
    ( { state = Pending, name = "", roomId = params.room_id }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdatedName String
    | SubmittedStorageUnitForm
    | GotResponse (Result Http.Error StorageUnit)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedName name ->
                    ( { model | name = name }, Cmd.none )

                SubmittedStorageUnitForm ->
                    ( model
                    , Http.request
                        { method = "POST"
                        , headers = [ header "token" u.token ]
                        , url = host ++ "/storage_units"
                        , body =
                            multipartBody [ stringPart "name" model.name, stringPart "room_id" model.roomId ]
                        , expect = Http.expectJson GotResponse storageUnitDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                GotResponse result ->
                    case result of
                        Ok _ ->
                            ( model
                            , Request.pushRoute (Gen.Route.Rooms__Room_id___StorageUnits { room_id = model.roomId }) req
                            )

                        Err _ ->
                            ( model, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Homepage"
    , body =
        UI.layout (Just user)
            [ form [ onSubmit SubmittedStorageUnitForm ]
                [ label []
                    [ div []
                        [ text "Name:"
                        , input [ type_ "text", onInput UpdatedName ] []
                        ]
                    ]
                , case model.state of
                    Failure ->
                        div [] [ text "An error occured." ]

                    Loading ->
                        div [] [ text "Saving..." ]

                    Pending ->
                        button [ disabled (String.isEmpty model.name) ] [ text "Save" ]
                ]
            ]
    }
