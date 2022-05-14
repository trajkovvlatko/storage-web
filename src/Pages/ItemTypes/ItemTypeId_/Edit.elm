module Pages.ItemTypes.ItemTypeId_.Edit exposing (Model, Msg, page)

import Auth exposing (User)
import Const exposing (host)
import Domain.ItemType exposing (ItemType, itemTypeDecoder)
import Gen.Params.ItemTypes.ItemTypeId_.Edit exposing (Params)
import Gen.Route
import Html exposing (button, div, form, input, label, text)
import Html.Attributes exposing (disabled, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (header, multipartBody, stringPart)
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



-- INIT


type State
    = Loading
    | Loaded
    | Failure


type alias Model =
    { state : State
    , itemType : Maybe ItemType
    }


init : Request.With Params -> Storage -> ( Model, Cmd Msg )
init req storage =
    case storage.user of
        Nothing ->
            ( { state = Failure, itemType = Nothing }
            , Cmd.none
            )

        Just user ->
            ( { state = Loading, itemType = Nothing }
            , Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/item_types/" ++ req.params.itemTypeId
                , body = Http.emptyBody
                , expect = Http.expectJson GotGetResponse itemTypeDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )



-- UPDATE


type Msg
    = UpdatedLabel String
    | SubmittedItemTypeForm
    | GotGetResponse (Result Http.Error ItemType)
    | GotPatchResponse (Result Http.Error ItemType)


update : Request.With Params -> Maybe User -> Msg -> Model -> ( Model, Cmd Msg )
update req user msg model =
    case user of
        Nothing ->
            ( model, Cmd.none )

        Just u ->
            case msg of
                UpdatedLabel label ->
                    case model.itemType of
                        Nothing ->
                            ( { model | itemType = Nothing }, Cmd.none )

                        Just r ->
                            ( { model | itemType = Just { id = r.id, label = label } }, Cmd.none )

                SubmittedItemTypeForm ->
                    ( model
                    , case model.itemType of
                        Nothing ->
                            Cmd.none

                        Just r ->
                            Http.request
                                { method = "PATCH"
                                , headers = [ header "token" u.token ]
                                , url = host ++ "/item_types/" ++ String.fromInt r.id
                                , body =
                                    multipartBody [ stringPart "label" r.label ]
                                , expect = Http.expectJson GotPatchResponse itemTypeDecoder
                                , timeout = Nothing
                                , tracker = Nothing
                                }
                    )

                GotGetResponse result ->
                    case result of
                        Ok r ->
                            ( { state = Loaded, itemType = Just r }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                GotPatchResponse result ->
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
    { title = "Edit item type"
    , body =
        UI.layout (Just user)
            [ case model.state of
                Failure ->
                    div [] [ text "An error occured." ]

                Loading ->
                    div [] [ text "Updating..." ]

                Loaded ->
                    case model.itemType of
                        Nothing ->
                            button [ disabled True ] [ text "Update" ]

                        Just r ->
                            form [ onSubmit SubmittedItemTypeForm ]
                                [ label []
                                    [ div []
                                        [ text "Label:"
                                        , input [ type_ "text", onInput UpdatedLabel, value r.label ] []
                                        ]
                                    ]
                                , button [ disabled (String.isEmpty r.label) ] [ text "Update" ]
                                ]
            ]
    }
