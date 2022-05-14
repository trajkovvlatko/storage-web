module Pages.ItemTypes exposing (Model, Msg, page)

import Auth
import Const exposing (host)
import Domain.ItemType exposing (ItemType, ItemTypes, itemTypeDecoder, itemTypesDecoder)
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
    , itemTypes : ItemTypes
    }



-- INIT


init : Storage -> ( Model, Cmd Msg )
init storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, itemTypes = [] }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading, itemTypes = [] }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/item_types"
                , body = Http.emptyBody
                , expect = Http.expectJson GotResponse itemTypesDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error ItemTypes)
    | Delete Int
    | Deleted (Result Http.Error ItemType)


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
                        , url = host ++ "/item_types/" ++ String.fromInt id
                        , body = Http.emptyBody
                        , expect = Http.expectJson Deleted itemTypeDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                Deleted result ->
                    case result of
                        Ok response ->
                            ( { model | itemTypes = List.filter (\x -> x.id /= response.id) model.itemTypes }, Cmd.none )

                        Err _ ->
                            ( { model | state = DeleteFailure }, Cmd.none )

                GotResponse result ->
                    case result of
                        Ok response ->
                            ( { state = Done, itemTypes = response }, Cmd.none )

                        Err _ ->
                            ( { state = Failure, itemTypes = [] }, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "ItemTypes"
    , body =
        UI.layout (Just user)
            [ h1 [] [ text "ItemTypes" ]
            , case model.state of
                Failure ->
                    div [] [ text "Failed to load item types." ]

                DeleteFailure ->
                    div [] [ text "Failed to delete an item type." ]

                Loading ->
                    div [] [ text "Loading item types..." ]

                Done ->
                    table
                        []
                        (thead []
                            [ th [] [ text "ID" ]
                            , th [] [ text "Label" ]
                            , th [] [ text "" ]
                            ]
                            :: List.map itemTypeRow model.itemTypes
                        )
            , a [ href (Gen.Route.toHref Gen.Route.ItemTypes__New) ] [ text "Add new item type" ]
            ]
    }


itemTypeRow : ItemType -> Html Msg
itemTypeRow itemType =
    let
        editUrl =
            toHref (Gen.Route.ItemTypes__ItemTypeId___Edit { itemTypeId = String.fromInt itemType.id })
    in
    tr []
        [ td [] [ text (String.fromInt itemType.id) ]
        , td [] [ text itemType.label ]
        , td [] [ a [ href editUrl ] [ text "Edit" ] ]
        , td [] [ button [ onClick (Delete itemType.id) ] [ text "Delete" ] ]
        ]
