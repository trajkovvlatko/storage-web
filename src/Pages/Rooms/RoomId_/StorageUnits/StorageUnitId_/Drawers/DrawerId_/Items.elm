module Pages.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.DrawerId_.Items exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.Item exposing (Item, Items, itemDecoder, itemsDecoder)
import Gen.Params.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers.DrawerId_.Items exposing (Params)
import Gen.Route exposing (toHref)
import Html exposing (Html, a, button, div, h1, table, td, text, th, thead, tr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http exposing (header)
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


type State
    = Failure
    | Loading
    | Done
    | DeleteFailure


type alias Model =
    { state : State
    , items : Items
    , roomId : String
    , storageUnitId : String
    , drawerId : String
    }



-- INIT


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init { params } storage =
    case storage.user of
        Nothing ->
            ( { state = Failure
              , items = []
              , roomId = params.roomId
              , storageUnitId = params.storageUnitId
              , drawerId = params.drawerId
              }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading
              , items = []
              , roomId = params.roomId
              , storageUnitId = params.storageUnitId
              , drawerId = params.drawerId
              }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/items?drawer_id=" ++ params.drawerId
                , body = Http.emptyBody
                , expect = Http.expectJson GotResponse itemsDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error Items)
    | Delete Int
    | Deleted (Result Http.Error Item)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update _ user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                Delete id ->
                    ( model
                    , Http.request
                        { method = "DELETE"
                        , headers = [ header "token" u.token ]
                        , url = host ++ "/items/" ++ String.fromInt id
                        , body = Http.emptyBody
                        , expect = Http.expectJson Deleted itemDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                Deleted result ->
                    case result of
                        Ok response ->
                            ( { model | items = List.filter (\x -> x.id /= response.id) model.items }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | state = DeleteFailure }, Cmd.none )

                GotResponse result ->
                    case result of
                        Ok response ->
                            ( { model | state = Done, items = response }, Cmd.none )

                        Err _ ->
                            ( { model | state = Failure, items = [] }, Cmd.none )



-- VIEW


newUrl : String -> String -> String -> String
newUrl roomId storageUnitId drawerId =
    toHref
        (Gen.Route.Rooms__RoomId___StorageUnits__StorageUnitId___Drawers__DrawerId___Items__New
            { roomId = roomId
            , storageUnitId = storageUnitId
            , drawerId = drawerId
            }
        )


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Items"
    , body =
        UI.layout (Just user)
            [ h1 [] [ text "Items" ]
            , case model.state of
                Failure ->
                    div [] [ text "Failed to load drawers." ]

                DeleteFailure ->
                    div [] [ text "Failed to delete a drawer." ]

                Loading ->
                    div [] [ text "Loading drawers..." ]

                Done ->
                    table
                        []
                        (thead []
                            [ th [] [ text "ID" ]
                            , th [] [ text "Name" ]
                            , th [] [ text "Color" ]
                            , th [] [ text "Item type" ]
                            , th [] [ text "" ]
                            , th [] [ text "" ]
                            ]
                            :: List.map (\row -> itemRow model row) model.items
                        )
            , a [ href (newUrl model.roomId model.storageUnitId model.drawerId) ] [ text "Add new item" ]
            ]
    }


itemRow : Model -> Item -> Html Msg
itemRow model item =
    let
        editUrl =
            toHref
                (Gen.Route.Rooms__RoomId___StorageUnits__StorageUnitId___Drawers__DrawerId___Items__ItemId___Edit
                    { roomId = model.roomId
                    , storageUnitId = model.storageUnitId
                    , drawerId = model.drawerId
                    , itemId = String.fromInt item.id
                    }
                )
    in
    tr []
        [ td [] [ text (String.fromInt item.id) ]
        , td [] [ text item.name ]
        , td [] [ text (String.fromInt item.color_id) ]
        , td [] [ text (String.fromInt item.item_type_id) ]
        , td [] [ a [ href editUrl ] [ text "Edit" ] ]
        , td [] [ button [ onClick (Delete item.id) ] [ text "Delete" ] ]
        ]
