port module Ports exposing (writeMax)

import Json.Decode as D exposing (Value)
import Json.Encode as E
import SupPort


writeMax : Int -> Cmd msg
writeMax max =
    out "WriteMax" [ E.int max ]


out : String -> List Value -> Cmd msg
out =
    SupPort.out portsOut


port portsOut : Value -> Cmd msg
