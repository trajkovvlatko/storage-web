module Pages.Logout exposing (Model, Msg, page)

import Auth
import Gen.Params.Logout
import Html exposing (div, h1, text)
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



-- INIT


type alias Model =
    {}


defaultModel : Model
defaultModel =
    {}


init : Storage -> ( Model, Cmd Msg )
init storage =
    update storage Logout defaultModel



-- UPDATE


type Msg
    = Logout


update : Storage -> Msg -> Model -> ( Model, Cmd Msg )
update storage msg model =
    case storage.user of
        Nothing ->
            ( model, Cmd.none )

        Just _ ->
            case msg of
                Logout ->
                    ( model
                    , Storage.logout storage
                    )



-- VIEW


view : Auth.User -> Model -> View Msg
view user _ =
    { title = "Logout"
    , body =
        UI.layout (Just user)
            [ h1 [] [ text "Logout" ]
            , div [] [ text "Logging out..." ]
            ]
    }
