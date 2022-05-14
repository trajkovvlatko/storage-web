module Pages.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.Drawer exposing (Drawer, Drawers, drawerDecoder, drawersDecoder)
import Gen.Params.Rooms.RoomId_.StorageUnits.StorageUnitId_.Drawers exposing (Params)
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
    , drawers : Drawers
    , roomId : String
    , storageUnitId : String
    }



-- INIT


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init { params } storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, drawers = [], roomId = params.roomId, storageUnitId = params.storageUnitId }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading, drawers = [], roomId = params.roomId, storageUnitId = params.storageUnitId }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/drawers?storage_unit_id=" ++ params.storageUnitId
                , body = Http.emptyBody
                , expect = Http.expectJson GotResponse drawersDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error Drawers)
    | Delete Int
    | Deleted (Result Http.Error Drawer)


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
                        , url = host ++ "/drawers/" ++ String.fromInt id
                        , body = Http.emptyBody
                        , expect = Http.expectJson Deleted drawerDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                Deleted result ->
                    case result of
                        Ok response ->
                            ( { model | drawers = List.filter (\x -> x.id /= response.id) model.drawers }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | state = DeleteFailure }, Cmd.none )

                GotResponse result ->
                    case result of
                        Ok response ->
                            ( { model | state = Done, drawers = response }, Cmd.none )

                        Err _ ->
                            ( { model | state = Failure, drawers = [] }, Cmd.none )



-- VIEW


newUrl : String -> String -> String
newUrl roomId storageUnitId =
    toHref
        (Gen.Route.Rooms__RoomId___StorageUnits__StorageUnitId___Drawers__New
            { roomId = roomId
            , storageUnitId = storageUnitId
            }
        )


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Drawers"
    , body =
        UI.layout (Just user)
            [ h1 [] [ text "Drawers" ]
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
                            , th [] [ text "Note" ]
                            , th [] [ text "Level" ]
                            , th [] [ text "" ]
                            , th [] [ text "" ]
                            ]
                            :: List.map (\row -> drawerRow model row) model.drawers
                        )
            , a [ href (newUrl model.roomId model.storageUnitId) ] [ text "Add new drawer" ]
            ]
    }


drawerRow : Model -> Drawer -> Html Msg
drawerRow model drawer =
    let
        params =
            { roomId = model.roomId
            , storageUnitId = model.storageUnitId
            , drawerId = String.fromInt drawer.id
            }

        itemsUrl =
            toHref (Gen.Route.Rooms__RoomId___StorageUnits__StorageUnitId___Drawers__DrawerId___Items params)

        editUrl =
            toHref
                (Gen.Route.Rooms__RoomId___StorageUnits__StorageUnitId___Drawers__DrawerId___Edit params)
    in
    tr []
        [ td [] [ text (String.fromInt drawer.id) ]
        , td [] [ text drawer.note ]
        , td [] [ text (String.fromInt drawer.level) ]
        , td [] [ a [ href itemsUrl ] [ text "Items" ] ]
        , td [] [ a [ href editUrl ] [ text "Edit" ] ]
        , td [] [ button [ onClick (Delete drawer.id) ] [ text "Delete" ] ]
        ]
