module Pages.Rooms.New exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.Room exposing (Room, roomDecoder)
import Gen.Route
import Html exposing (button, div, form, input, label, text)
import Html.Attributes exposing (disabled, type_)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (header, multipartBody, stringPart)
import Page
import Request exposing (Request)
import Shared
import Storage exposing (Storage)
import UI
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.protected.element <|
        \user ->
            { init = init shared.storage
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
    }


init : Storage -> ( Model, Cmd Msg )
init _ =
    ( { state = Pending, name = "" }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdatedName String
    | SubmittedRoomForm
    | GotResponse (Result Http.Error Room)


update : Request -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedName name ->
                    ( { model | name = name }, Cmd.none )

                SubmittedRoomForm ->
                    ( model
                    , Http.request
                        { method = "POST"
                        , headers = [ header "token" u.token ]
                        , url = host ++ "/rooms"
                        , body =
                            multipartBody [ stringPart "name" model.name ]
                        , expect = Http.expectJson GotResponse roomDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                GotResponse result ->
                    case result of
                        Ok _ ->
                            ( model
                            , Request.pushRoute Gen.Route.Rooms req
                            )

                        Err _ ->
                            ( model, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Homepage"
    , body =
        UI.layout (Just user)
            [ form [ onSubmit SubmittedRoomForm ]
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
