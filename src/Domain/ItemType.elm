module Domain.ItemType exposing (ItemType, ItemTypes, itemTypeDecoder, itemTypesDecoder)

import Json.Decode exposing (Decoder, field, int, list, map2, string)


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
