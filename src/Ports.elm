port module Ports exposing (click, writeMax)

import Json.Decode as D exposing (Value)
import Json.Encode as E
import SupPort


click : String -> Cmd msg
click id =
    out "Click" [ E.string id ]


writeMax : Int -> Cmd msg
writeMax max =
    out "WriteMax" [ E.int max ]


out : String -> List Value -> Cmd msg
out =
    SupPort.out portsOut


port portsOut : Value -> Cmd msg
