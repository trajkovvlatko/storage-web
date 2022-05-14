module Pages.ItemTypes.New exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.ItemType exposing (ItemType, itemTypeDecoder)
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
    , label : String
    }


init : Storage -> ( Model, Cmd Msg )
init _ =
    ( { state = Pending, label = "" }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UpdatedLabel String
    | SubmittedItemTypeForm
    | GotResponse (Result Http.Error ItemType)


update : Request -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedLabel label ->
                    ( { model | label = label }, Cmd.none )

                SubmittedItemTypeForm ->
                    ( model
                    , Http.request
                        { method = "POST"
                        , headers = [ header "token" u.token ]
                        , url = host ++ "/item_types"
                        , body =
                            multipartBody [ stringPart "label" model.label ]
                        , expect = Http.expectJson GotResponse itemTypeDecoder
                        , timeout = Nothing
                        , tracker = Nothing
                        }
                    )

                GotResponse result ->
                    case result of
                        Ok _ ->
                            ( model
                            , Request.pushRoute Gen.Route.ItemTypes req
                            )

                        Err _ ->
                            ( model, Cmd.none )



-- VIEW


view : Auth.User -> Model -> View Msg
view user model =
    { title = "Add new item type"
    , body =
        UI.layout (Just user)
            [ form [ onSubmit SubmittedItemTypeForm ]
                [ label []
                    [ div []
                        [ text "Label:"
                        , input [ type_ "text", onInput UpdatedLabel ] []
                        ]
                    ]
                , case model.state of
                    Failure ->
                        div [] [ text "An error occured." ]

                    Loading ->
                        div [] [ text "Saving..." ]

                    Pending ->
                        button [ disabled (String.isEmpty model.label) ] [ text "Save" ]
                ]
            ]
    }
