module Pages.Login exposing (Model, Msg, page)

import Const exposing (host)
import Gen.Params.Login exposing (Params)
import Html exposing (button, div, form, input, label, text)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (multipartBody, stringPart)
import Json.Decode exposing (Decoder, field, string)
import Page
import Request
import Shared
import Storage exposing (Storage)
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update shared.storage
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- INIT


type State
    = Idle
    | Failure


type alias Credentials =
    { email : String, password : String }


type alias Model =
    { state : State, credentials : Credentials }


init : ( Model, Cmd Msg )
init =
    ( { state = Idle, credentials = { email = "", password = "" } }
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
            ( { model | credentials = { email = model.credentials.email, password = password } }
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
                    ( { model | state = Idle }
                    , Storage.login { token = response } storage
                    )

                Err _ ->
                    ( { model | state = Failure }, Cmd.none )


validate : Model -> Bool
validate { credentials } =
    not (String.isEmpty credentials.email) && not (String.isEmpty credentials.password)



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
                        , input [ type_ "text", onInput UpdatedEmail, value model.credentials.email ] []
                        ]
                    , div []
                        [ text "Password:"
                        , input [ type_ "password", onInput UpdatedPassword, value model.credentials.password ] []
                        ]
                    , case model.state of
                        Idle ->
                            div [] []

                        Failure ->
                            div [] [ text "Invalid login" ]
                    ]
                , button
                    [ disabled (not (validate model)) ]
                    [ text "Login" ]
                ]
            ]
    }
