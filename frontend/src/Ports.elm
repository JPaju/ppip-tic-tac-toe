port module Ports exposing (debugPort, receiveGameMessage, sendMark)

import Json.Decode as Decode
import Json.Encode as Encode


port sendMark : Encode.Value -> Cmd msg


port receiveGameMessage : (Decode.Value -> msg) -> Sub msg



-- TODO REMOVE


port debugPort : String -> Cmd msg
