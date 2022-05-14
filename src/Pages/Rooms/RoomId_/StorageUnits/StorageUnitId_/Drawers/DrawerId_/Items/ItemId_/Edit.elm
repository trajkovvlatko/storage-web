module Pages.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.DrawerId_.Items.ItemId_.Edit exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.Color exposing (Color, Colors, colorOption, colorsDecoder, fetchColorsCmd)
import Domain.Item exposing (Item, itemDecoder)
import Domain.ItemType exposing (ItemType, ItemTypes, fetchItemTypesCmd, itemTypeOption, itemTypesDecoder)
import Gen.Params.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.DrawerId_.Items.ItemId_.Edit exposing (Params)
import Gen.Route
import Html exposing (Html, button, div, form, input, label, option, select, text)
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
    , drawerId : String
    , item : Maybe Item
    , colors : Colors
    , itemTypes : ItemTypes
    }


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init req storage =
    case storage.user of
        Nothing ->
            ( { state = Failure
              , roomId = req.params.roomId
              , storageUnitId = req.params.storageUnitId
              , drawerId = req.params.drawerId
              , item = Nothing
              , colors = []
              , itemTypes = []
              }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading
              , roomId = req.params.roomId
              , storageUnitId = req.params.storageUnitId
              , drawerId = req.params.drawerId
              , colors = []
              , itemTypes = []
              , item = Nothing
              }
            , Cmd.batch
                [ fetchColorsCmd storage (Http.expectJson GotColorsResponse colorsDecoder)
                , fetchItemTypesCmd storage (Http.expectJson GotItemTypesResponse itemTypesDecoder)
                , fetchItemCmd req user
                ]
            )


fetchItemCmd : Request.With Params -> User -> Cmd Msg
fetchItemCmd req user =
    Http.request
        { method = "GET"
        , headers = [ header "token" user.token ]
        , url = host ++ "/items/" ++ req.params.itemId
        , body = Http.emptyBody
        , expect = Http.expectJson GotGetResponse itemDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- UPDATE


type Msg
    = UpdatedName String
    | UpdatedColorId String
    | UpdatedItemTypeId String
    | SubmittedItemForm
    | GotGetResponse (Result Http.Error Item)
    | GotPatchResponse (Result Http.Error Item)
    | GotColorsResponse (Result Http.Error Colors)
    | GotItemTypesResponse (Result Http.Error ItemTypes)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedName name ->
                    case model.item of
                        Nothing ->
                            ( { model | item = Nothing }, Cmd.none )

                        Just r ->
                            ( { model
                                | item =
                                    Just
                                        { id = r.id
                                        , drawer_id = r.drawer_id
                                        , name = name
                                        , color_id = r.color_id
                                        , item_type_id = r.item_type_id
                                        }
                              }
                            , Cmd.none
                            )

                UpdatedColorId colorId ->
                    case ( model.item, String.toInt colorId ) of
                        ( Just r, Just l ) ->
                            ( { model
                                | item =
                                    Just
                                        { id = r.id
                                        , drawer_id = r.drawer_id
                                        , name = r.name
                                        , item_type_id = r.item_type_id
                                        , color_id = l
                                        }
                              }
                            , Cmd.none
                            )

                        ( _, _ ) ->
                            ( { model | item = Nothing }, Cmd.none )

                UpdatedItemTypeId itemTypeId ->
                    case ( model.item, String.toInt itemTypeId ) of
                        ( Just r, Just l ) ->
                            ( { model
                                | item =
                                    Just
                                        { id = r.id
                                        , drawer_id = r.drawer_id
                                        , name = r.name
                                        , color_id = r.color_id
                                        , item_type_id = l
                                        }
                              }
                            , Cmd.none
                            )

                        ( _, _ ) ->
                            ( { model | item = Nothing }, Cmd.none )

                SubmittedItemForm ->
                    ( model
                    , case model.item of
                        Nothing ->
                            Cmd.none

                        Just r ->
                            Http.request
                                { method = "PATCH"
                                , headers = [ header "token" u.token ]
                                , url = host ++ "/items/" ++ String.fromInt r.id
                                , body =
                                    multipartBody
                                        [ stringPart "name" r.name
                                        , stringPart "color_id" (String.fromInt r.color_id)
                                        , stringPart "item_type_id" (String.fromInt r.item_type_id)
                                        ]
                                , expect = Http.expectJson GotPatchResponse itemDecoder
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                    )

                GotGetResponse result ->
                    case result of
                        Ok s ->
                            ( { model | state = Loaded, item = Just s }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                GotPatchResponse result ->
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

                GotColorsResponse result ->
                    case result of
                        Ok colors ->
                            ( { model | colors = colors }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                GotItemTypesResponse result ->
                    case result of
                        Ok itemTypes ->
                            ( { model | itemTypes = itemTypes }, Cmd.none )

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
                    case model.item of
                        Nothing ->
                            button [ disabled True ] [ text "Update" ]

                        Just r ->
                            form [ onSubmit SubmittedItemForm ]
                                [ label []
                                    [ div []
                                        [ text "Name:"
                                        , input [ type_ "text", onInput UpdatedName, value r.name ] []
                                        ]
                                    ]
                                , case model.colors of
                                    [] ->
                                        div [] [ text "loading..." ]

                                    _ ->
                                        label []
                                            [ div []
                                                [ text "Color:"
                                                , select [ onInput UpdatedColorId ]
                                                    (List.map (\c -> colorOption c r.color_id) model.colors)
                                                ]
                                            ]
                                , case model.itemTypes of
                                    [] ->
                                        div [] [ text "loading..." ]

                                    _ ->
                                        label []
                                            [ div []
                                                [ text "Item type:"
                                                , select [ onInput UpdatedItemTypeId ]
                                                    (List.map (\i -> itemTypeOption i r.item_type_id) model.itemTypes)
                                                ]
                                            ]
                                , button [ disabled (String.isEmpty r.name) ] [ text "Update" ]
                                ]
            ]
    }
