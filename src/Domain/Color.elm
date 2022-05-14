module Domain.Color exposing (Color, Colors, colorDecoder, colorsDecoder)

import Json.Decode exposing (Decoder, field, int, list, map2, string)


type alias Color =
    { id : Int, label : String }


type alias Colors =
    List Color


colorsDecoder : Decoder Colors
colorsDecoder =
    list colorDecoder


colorDecoder : Decoder Color
colorDecoder =
    map2 Color
        (field "id" int)
        (field "label" string)
