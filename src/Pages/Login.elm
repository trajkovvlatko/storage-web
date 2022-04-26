module Pages.Login exposing (Model, Msg, page)

import Const exposing (host)
import Gen.Params.Login exposing (Params)
import Html exposing (button, div, form, input, label, text)
import Html.Attributes exposing (disabled, required, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (multipartBody, stringPart)
import Json.Decode as Decode exposing (Decoder, field, map, string)
import Page
import Request
import Shared
import Storage exposing (Storage)
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update shared.storage
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Credentials =
    { email : String, password : String }


type alias Model =
    { credentials : Credentials }


type alias Response =
    { token : String }


init : ( Model, Cmd Msg )
init =
    ( { credentials = { email = "email@email.com", password = "password" } }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdatedEmail String
    | UpdatedPassword String
    | SubmittedLoginForm
    | LoginResponse (Result Http.Error String)


loginResponseDecoder : Decoder String
loginResponseDecoder =
    field "token" string


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        UpdatedEmail email ->
            ( { model | credentials = { email = email, password = model.credentials.password } }
            , Cmd.none
            )

        UpdatedPassword password ->
            ( { model | credentials = { email = model.credentials.email, password = model.credentials.password } }
            , Cmd.none
            )

        SubmittedLoginForm ->
            ( model
            , Http.post
                { url = host ++ "/login"
                , body =
                    multipartBody
                        [ stringPart "email" model.credentials.email
                        , stringPart "password" model.credentials.password
                        ]
                , expect = Http.expectJson LoginResponse loginResponseDecoder
                }
            )

        LoginResponse result ->
            case result of
                Ok response ->
                    ( model
                    , Storage.login { token = response } storage
                    )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Login"
    , body =
        UI.layout Nothing
            [ form [ onSubmit SubmittedLoginForm ]
                [ label []
                    [ div []
                        [ text "Email:"
                        , input [ type_ "text", onInput UpdatedEmail ] []
                        ]
                    , div []
                        [ text "Password:"
                        , input [ type_ "password", onInput UpdatedPassword ] []
                        ]
                    ]
                , button
                    [ disabled (String.isEmpty model.credentials.email && String.isEmpty model.credentials.password) ]
                    [ text "Login" ]
                ]
            ]
    }
