module Pages.Register exposing (Model, Msg, page)

import Const exposing (host)
import Gen.Params.Register exposing (Params)
import Html exposing (button, div, form, input, label, text)
import Html.Attributes exposing (disabled, type_)
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
    { email : String, password : String, confirmPassword : String }


type alias Model =
    { state : State, credentials : Credentials }


init : ( Model, Cmd Msg )
init =
    ( { state = Idle, credentials = { email = "", password = "", confirmPassword = "" } }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdatedEmail String
    | UpdatedPassword String
    | UpdatedConfirmPassword String
    | SubmittedRegisterForm
    | RegisterResponse (Result Http.Error String)


registerResponseDecoder : Decoder String
registerResponseDecoder =
    field "token" string


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case msg of
        UpdatedEmail email ->
            ( { model
                | credentials =
                    { email = email
                    , password = model.credentials.password
                    , confirmPassword = model.credentials.confirmPassword
                    }
              }
            , Cmd.none
            )

        UpdatedPassword p ->
            ( { model
                | credentials =
                    { email = model.credentials.email
                    , password = p
                    , confirmPassword = model.credentials.confirmPassword
                    }
              }
            , Cmd.none
            )

        UpdatedConfirmPassword cp ->
            ( { model
                | credentials =
                    { email = model.credentials.email
                    , password = model.credentials.password
                    , confirmPassword = cp
                    }
              }
            , Cmd.none
            )

        SubmittedRegisterForm ->
            if validate model then
                ( { model | state = Idle }
                , Http.post
                    { url = host ++ "/register"
                    , body =
                        multipartBody
                            [ stringPart "email" model.credentials.email
                            , stringPart "password" model.credentials.password
                            ]
                    , expect = Http.expectJson RegisterResponse registerResponseDecoder
                    }
                )

            else
                ( { model | state = Failure }, Cmd.none )

        RegisterResponse result ->
            case result of
                Ok response ->
                    ( model
                    , Storage.login { token = response } storage
                    )

                Err _ ->
                    ( model, Cmd.none )


validate : Model -> Bool
validate { credentials } =
    not (String.isEmpty credentials.email)
        && not (String.isEmpty credentials.password)
        && not (String.isEmpty credentials.confirmPassword)
        && credentials.password
        == credentials.confirmPassword



-- VIEW


view : Model -> View Msg
view model =
    { title = "Register"
    , body =
        UI.layout Nothing
            [ form [ onSubmit SubmittedRegisterForm ]
                [ label []
                    [ div []
                        [ text "Email:"
                        , input [ type_ "text", onInput UpdatedEmail ] []
                        ]
                    , div []
                        [ text "Password:"
                        , input [ type_ "password", onInput UpdatedPassword ] []
                        ]
                    , div []
                        [ text "Confirm password:"
                        , input [ type_ "password", onInput UpdatedConfirmPassword ] []
                        ]
                    ]
                , button
                    [ disabled (not (validate model)) ]
                    [ text "Register" ]
                , if model.state == Failure then
                    div [] [ text "Invalid input." ]

                  else
                    div [] []
                ]
            ]
    }
