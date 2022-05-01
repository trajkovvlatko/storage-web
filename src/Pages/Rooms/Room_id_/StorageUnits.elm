module Pages.Rooms.Room_id_.StorageUnits exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.StorageUnit exposing (StorageUnit, StorageUnits, storageUnitDecoder, storageUnitsDecoder)
import Gen.Params.Rooms.Room_id_.StorageUnits exposing (Params)
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
    , storageUnits : StorageUnits
    , roomId : String
    }



-- INIT


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init { params } storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, storageUnits = [], roomId = params.room_id }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading, storageUnits = [], roomId = params.room_id }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/storage_units?room_id=" ++ params.room_id
                , body = Http.emptyBody
                , expect = Http.expectJson GotResponse storageUnitsDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error StorageUnits)
    | Delete Int
    | Deleted (Result Http.Error StorageUnit)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
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
                        , url = host ++ "/storage_units/" ++ String.fromInt id
                        , body = Http.emptyBody
                        , expect = Http.expectJson Deleted storageUnitDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                Deleted result ->
                    case result of
                        Ok response ->
                            ( { model | storageUnits = List.filter (\x -> x.id /= response.id) model.storageUnits }
                            , Cmd.none
                            )

                        Err _ ->
                            ( { model | state = DeleteFailure }, Cmd.none )

                GotResponse result ->
                    case result of
                        Ok response ->
                            ( { model | state = Done, storageUnits = response }, Cmd.none )

                        Err _ ->
                            ( { model | state = Failure, storageUnits = [] }, Cmd.none )



-- VIEW


newUrl : String -> String
newUrl roomId =
    toHref (Gen.Route.Rooms__Room_id___StorageUnits__New { room_id = roomId })


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Homepage"
    , body =
        UI.layout (Just user)
            [ h1 [] [ text "Storage units" ]
            , case model.state of
                Failure ->
                    div [] [ text "Failed to load storage units." ]

                DeleteFailure ->
                    div [] [ text "Failed to delete a storage unit." ]

                Loading ->
                    div [] [ text "Loading storage units..." ]

                Done ->
                    table
                        []
                        (thead []
                            [ th [] [ text "ID" ]
                            , th [] [ text "Name" ]
                            , th [] [ text "" ]
                            , th [] [ text "" ]
                            ]
                            :: List.map storageUnitRow model.storageUnits
                        )
            , a [ href (newUrl model.roomId) ] [ text "Add new storage unit" ]
            ]
    }


storageUnitRow : StorageUnit -> Html Msg
storageUnitRow storageUnit =
    -- let
    --     editUrl =
    --         Gen.Route.toHref (Gen.Route.StorageUnits__Id___Edit { id = String.fromInt storageUnit.id })
    -- in
    tr []
        [ td [] [ text (String.fromInt storageUnit.id) ]
        , td [] [ text storageUnit.name ]
        , td [] [ a [ href ("/drawers/" ++ String.fromInt storageUnit.id) ] [ text "Drawers" ] ]

        -- , td [] [ a [ href editUrl ] [ text "Edit" ] ]
        , td [] [ button [ onClick (Delete storageUnit.id) ] [ text "Delete" ] ]
        ]
