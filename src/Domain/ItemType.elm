module Domain.ItemType exposing (ItemType, ItemTypes, fetchItemTypesCmd, itemTypeDecoder, itemTypeOption, itemTypesDecoder)

import Const exposing (host)
import Html exposing (Html, option, text)
import Html.Attributes exposing (value)
import Http exposing (Expect, header, multipartBody, stringPart)
import Json.Decode exposing (Decoder, field, int, list, map2, string)
import Storage exposing (Storage)


type alias ItemType =
    { id : Int, label : String }


type alias ItemTypes =
    List ItemType


itemTypesDecoder : Decoder ItemTypes
itemTypesDecoder =
    list itemTypeDecoder


itemTypeDecoder : Decoder ItemType
itemTypeDecoder =
    map2 ItemType
        (field "id" int)
        (field "label" string)


fetchItemTypesCmd : Storage -> Expect msg -> Cmd msg
fetchItemTypesCmd storage expect =
    case storage.user of
        Nothing ->
            Cmd.none

        Just user ->
            Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/item_types"
                , body = Http.emptyBody
                , expect = expect
                , timeout = Nothing
                , tracker = Nothing
                }


itemTypeOption : ItemType -> Html msg
itemTypeOption itemType =
    option [ value (String.fromInt itemType.id) ] [ text itemType.label ]
