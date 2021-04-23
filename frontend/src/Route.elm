module Route exposing (Route(..), fromUrl)

import Url exposing (Url)
import Url.Parser as Parser


type Route
    = Root
    | OfflineRoute
    | OnlineRoute


fromUrl : Url -> Route
fromUrl =
    Parser.parse parser >> Maybe.withDefault Root


parser : Parser.Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map OfflineRoute (Parser.s "offline")
        , Parser.map OnlineRoute (Parser.s "online")
        ]
