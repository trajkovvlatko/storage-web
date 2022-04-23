port module Storage exposing
    ( Storage
    , fromJson
    , load
    , login
    , logout
    )

import Domain.User as User exposing (User)
import Json.Decode as Json
import Json.Encode as Encode


type alias Storage =
    { user : Maybe User
    }


fromJson : Json.Value -> Storage
fromJson json =
    json
        |> Json.decodeValue decoder
        |> Result.withDefault init


init : Storage
init =
    { user = Nothing
    }


decoder : Json.Decoder Storage
decoder =
    Json.map Storage
        (Json.field "user" (Json.maybe User.decoder))


save : Storage -> Json.Value
save storage =
    Encode.object
        [ ( "user"
          , storage.user
                |> Maybe.map User.encode
                |> Maybe.withDefault Encode.null
          )
        ]



-- UPDATING STORAGE


login : User -> Storage -> Cmd msg
login user storage =
    saveToLocalStorage { storage | user = Just user }


logout : Storage -> Cmd msg
logout storage =
    saveToLocalStorage { storage | user = Nothing }



-- PORTS


saveToLocalStorage : Storage -> Cmd msg
saveToLocalStorage =
    save >> save_


port save_ : Json.Value -> Cmd msg


load : (Storage -> msg) -> Sub msg
load fromStorage =
    load_ (fromJson >> fromStorage)


port load_ : (Json.Value -> msg) -> Sub msg
