module Domain.Color exposing (Color, Colors, colorDecoder, colorOption, colorsDecoder, fetchColorsCmd)

import Const exposing (host)
import Html exposing (Html, option, text)
import Html.Attributes exposing (selected, value)
import Http exposing (Expect, header)
import Json.Decode exposing (Decoder, field, int, list, map2, string)
import Storage exposing (Storage)


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


fetchColorsCmd : Storage -> Expect msg -> Cmd msg
fetchColorsCmd storage expect =
    case storage.user of
        Nothing ->
            Cmd.none

        Just user ->
            Http.request
                { method = "GET"
                , headers = [ header "token" user.token ]
                , url = host ++ "/colors"
                , body = Http.emptyBody
                , expect = expect
                , timeout = Nothing
                , tracker = Nothing
                }


colorOption : Color -> Int -> Html msg
colorOption color selectedColorId =
    option [ selected (color.id == selectedColorId), value (String.fromInt color.id) ] [ text color.label ]
