module Pages.Rooms exposing (Model, Msg, page)

import Auth
import Const exposing (host)
import Domain.Room exposing (Room, Rooms, roomDecoder, roomsDecoder)
import Gen.Route exposing (toHref)
import Html exposing (Html, a, button, div, h1, table, td, text, th, thead, tr)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http exposing (header)
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import UI
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.protected.element <|
        \user ->
            { init = init shared.storage
            , update = update shared.storage
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
    , rooms : Rooms
    }



-- INIT


init : Storage -> ( Model, Cmd Msg )
init storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, rooms = [] }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading, rooms = [] }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/rooms"
                , body = Http.emptyBody
                , expect = Http.expectJson GotResponse roomsDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error Rooms)
    | Delete Int
    | Deleted (Result Http.Error Room)


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case storage.user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                Delete id ->
                    ( model
                    , Http.request
                        { method = "DELETE"
                        , headers = [ header "token" u.token ]
                        , url = host ++ "/rooms/" ++ String.fromInt id
                        , body = Http.emptyBody
                        , expect = Http.expectJson Deleted roomDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                Deleted result ->
                    case result of
                        Ok response ->
                            ( { model | rooms = List.filter (\x -> x.id /= response.id) model.rooms }, Cmd.none )

                        Err _ ->
                            ( { model | state = DeleteFailure }, Cmd.none )

                GotResponse result ->
                    case result of
                        Ok response ->
                            ( { state = Done, rooms = response }, Cmd.none )

                        Err _ ->
                            ( { state = Failure, rooms = [] }, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Rooms"
    , body =
        UI.layout (Just user)
            [ h1 [] [ text "Rooms" ]
            , case model.state of
                Failure ->
                    div [] [ text "Failed to load rooms." ]

                DeleteFailure ->
                    div [] [ text "Failed to delete a room." ]

                Loading ->
                    div [] [ text "Loading rooms..." ]

                Done ->
                    table
                        []
                        (thead []
                            [ th [] [ text "ID" ]
                            , th [] [ text "Name" ]
                            , th [] [ text "" ]
                            , th [] [ text "" ]
                            ]
                            :: List.map roomRow model.rooms
                        )
            , a [ href (Gen.Route.toHref Gen.Route.Rooms__New) ] [ text "Add new room" ]
            ]
    }


roomRow : Room -> Html Msg
roomRow room =
    let
        editUrl =
            toHref (Gen.Route.Rooms__RoomId___Edit { roomId = String.fromInt room.id })

        storageUnitUrl =
            toHref
                (Gen.Route.Rooms__RoomId___StorageUnits { roomId = String.fromInt room.id })
    in
    tr []
        [ td [] [ text (String.fromInt room.id) ]
        , td [] [ text room.name ]
        , td []
            [ a [ href storageUnitUrl ] [ text "Storage units" ]
            ]
        , td [] [ a [ href editUrl ] [ text "Edit" ] ]
        , td [] [ button [ onClick (Delete room.id) ] [ text "Delete" ] ]
        ]
