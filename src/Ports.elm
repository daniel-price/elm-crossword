port module Ports exposing (messageReceiver, sendMessage)

import Json.Encode


port sendMessage : Json.Encode.Value -> Cmd msg


port messageReceiver : (Json.Encode.Value -> msg) -> Sub msg
