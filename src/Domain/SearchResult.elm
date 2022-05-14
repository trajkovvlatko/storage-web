module Domain.SearchResult exposing (SearchResult, SearchResults, searchResultDecoder, searchResultsDecoder)

import Json.Decode exposing (Decoder, field, int, list, map7, string)


type alias SearchResult =
    { name : String
    , drawerNote : String
    , drawerLevel : Int
    , storageUnitName : String
    , roomName : String
    , color : String
    , itemType : String
    }


type alias SearchResults =
    List SearchResult


searchResultsDecoder : Decoder SearchResults
searchResultsDecoder =
    list searchResultDecoder


searchResultDecoder : Decoder SearchResult
searchResultDecoder =
    map7 SearchResult
        (field "name" string)
        (field "drawer_note" string)
        (field "drawer_level" int)
        (field "storage_unit_name" string)
        (field "room_name" string)
        (field "color" string)
        (field "item_type" string)
