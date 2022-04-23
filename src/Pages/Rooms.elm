module Pages.Rooms exposing (Model, Msg, page)

import Auth
import Const exposing (host)
import Gen.Params.Rooms exposing (Params)
import Html exposing (Html, a, div, h1, span, table, td, text, th, thead, tr)
import Html.Attributes exposing (href)
import Http exposing (header)
import Json.Decode as Decode exposing (Decoder, field, int, list, map, map2, string)
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


type alias Model =
    { state : State
    , rooms : Rooms
    }


type alias Room =
    { id : Int, name : String }


type alias Rooms =
    List Room


roomsResponseDecoder : Decoder Rooms
roomsResponseDecoder =
    list roomResponseDecoder


roomResponseDecoder : Decoder Room
roomResponseDecoder =
    map2 Room
        (field "id" int)
        (field "name" string)



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
                , expect = Http.expectJson GotResponse roomsResponseDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = GotResponse (Result Http.Error Rooms)


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        GotResponse result ->
            case result of
                Ok response ->
                    ( { state = Done, rooms = response }
                    , Cmd.none
                    )

                Err _ ->
                    ( { state = Failure, rooms = [] }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Homepage"
    , body =
        UI.layout (Just user)
            [ h1 [] [ text "Rooms" ]
            , case model.state of
                Failure ->
                    div [] [ text "Failed to load rooms." ]

                Loading ->
                    div [] [ text "Loading rooms..." ]

                Done ->
                    table
                        []
                        (thead []
                            [ th [] [ text "ID" ]
                            , th [] [ text "Name" ]
                            , th [] [ text "" ]
                            ]
                            :: List.map roomRow model.rooms
                        )
            ]
    }


roomRow : Room -> Html Msg
roomRow room =
    tr []
        [ td [] [ text (String.fromInt room.id) ]
        , td [] [ text room.name ]
        , td [] [ a [ href ("/storage_units/" ++ String.fromInt room.id) ] [ text "Storage units" ] ]
        ]
