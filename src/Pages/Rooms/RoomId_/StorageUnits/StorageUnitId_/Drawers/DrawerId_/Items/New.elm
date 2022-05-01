module Pages.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.DrawerId_.Items.New exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.Item exposing (Item, itemDecoder)
import Gen.Params.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.DrawerId_.Items exposing (Params)
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
    , colorId : String
    , itemTypeId : String
    , roomId : String
    , storageUnitId : String
    , drawerId : String
    }


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init { params } _ =
    ( { state = Pending
      , name = ""
      , colorId = ""
      , itemTypeId = ""
      , roomId = params.roomId
      , storageUnitId = params.storageUnitId
      , drawerId = params.drawerId
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdatedName String
    | UpdatedColorId String
    | UpdatedItemTypeId String
    | SubmittedItemForm
    | GotResponse (Result Http.Error Item)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedName name ->
                    ( { model | name = name }, Cmd.none )

                UpdatedColorId colorId ->
                    ( { model | colorId = colorId }, Cmd.none )

                UpdatedItemTypeId itemTypeId ->
                    ( { model | itemTypeId = itemTypeId }, Cmd.none )

                SubmittedItemForm ->
                    ( model
                    , Http.request
                        { method = "POST"
                        , headers = [ header "token" u.token ]
                        , url = host ++ "/items"
                        , body =
                            multipartBody
                                [ stringPart "name" model.name
                                , stringPart "color_id" model.colorId
                                , stringPart "item_type_id" model.itemTypeId
                                , stringPart "drawer_id" model.drawerId
                                ]
                        , expect = Http.expectJson GotResponse itemDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                GotResponse result ->
                    case result of
                        Ok _ ->
                            ( model
                            , Request.pushRoute
                                (Gen.Route.Rooms__RoomId___StorageUnits__StorageUnitId___Drawers__DrawerId___Items
                                    { roomId = model.roomId
                                    , storageUnitId = model.storageUnitId
                                    , drawerId = model.drawerId
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
            [ form [ onSubmit SubmittedItemForm ]
                [ label []
                    [ div []
                        [ text "Name:"
                        , input [ type_ "text", onInput UpdatedName ] []
                        ]
                    ]
                , label []
                    [ div []
                        [ text "Color id:"
                        , input [ type_ "number", onInput UpdatedColorId ] []
                        ]
                    ]
                , label []
                    [ div []
                        [ text "Item type id:"
                        , input [ type_ "number", onInput UpdatedItemTypeId ] []
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
