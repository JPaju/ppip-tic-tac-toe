port module Ports exposing (connectionPort, receiveGameMessage, sendMark)

import Json.Decode as Decode
import Json.Encode as Encode


port sendMark : Encode.Value -> Cmd msg


port receiveGameMessage : (Decode.Value -> msg) -> Sub msg


port connectionPort : String -> Cmd msg
