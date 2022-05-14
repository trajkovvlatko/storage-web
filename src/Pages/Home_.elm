module Pages.Home_ exposing (Model, Msg, page)

import Auth
import Const exposing (host)
import Domain.SearchResult exposing (SearchResult, SearchResults, searchResultsDecoder)
import Html exposing (Html, button, div, h1, input, table, td, text, th, thead, tr)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onClick, onInput)
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
            { init = init
            , update = update shared.storage
            , view = view user
            , subscriptions = \_ -> Sub.none
            }


type State
    = Idle
    | Loading
    | Failure
    | Done


type alias Model =
    { state : State
    , results : SearchResults
    , term : String
    }



-- INIT


defaultModel : Model
defaultModel =
    { term = "", results = [], state = Idle }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )



-- UPDATE


type Msg
    = UpdateSearchTerm String
    | RunSearch
    | GotResponse (Result Http.Error SearchResults)
    | ClickedLogout


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case storage.user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                ClickedLogout ->
                    ( model
                    , Storage.logout storage
                    )

                UpdateSearchTerm term ->
                    ( { model | term = term }, Cmd.none )

                RunSearch ->
                    ( { model | state = Loading }
                    , Http.request
                        { method = "GET"
                        , headers = [ header "token" u.token ]
                        , url = host ++ "/search/basic/`" ++ model.term
                        , body = Http.emptyBody
                        , expect = Http.expectJson GotResponse searchResultsDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                GotResponse result ->
                    case result of
                        Ok response ->
                            ( { model | state = Done, results = response }, Cmd.none )

                        Err _ ->
                            ( { model | state = Failure, results = [] }, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Search"
    , body =
        UI.layout (Just user)
            [ h1 [] [ text "Search" ]
            , input [ type_ "text", onInput UpdateSearchTerm, value model.term ] []
            , button [ disabled (String.isEmpty model.term), onClick RunSearch ] [ text "Search" ]
            , case model.state of
                Idle ->
                    div [] []

                Failure ->
                    div [] [ text "Failed to load results." ]

                Loading ->
                    div [] [ text "Loading..." ]

                Done ->
                    table
                        []
                        (thead []
                            [ th [] [ text "Name" ]
                            , th [] [ text "Item type" ]
                            , th [] [ text "Color" ]
                            , th [] [ text "Room" ]
                            , th [] [ text "Storage unit" ]
                            , th [] [ text "Drawer note" ]
                            , th [] [ text "Drawer level" ]
                            ]
                            :: List.map resultRow model.results
                        )
            , button [ onClick ClickedLogout ] [ text "Logout" ]
            ]
    }


resultRow : SearchResult -> Html Msg
resultRow searchResult =
    tr []
        [ td [] [ text searchResult.name ]
        , td [] [ text searchResult.itemType ]
        , td [] [ text searchResult.color ]
        , td [] [ text searchResult.roomName ]
        , td [] [ text searchResult.storageUnitName ]
        , td [] [ text searchResult.drawerNote ]
        , td [] [ text (String.fromInt searchResult.drawerLevel) ]
        ]
