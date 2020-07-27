module Type.Language exposing (Language(..), decoder, toString)

import Json.Decode as D exposing (Decoder)


type Language
    = English
    | Spanish


toString : Language -> String
toString l =
    case l of
        English ->
            "English"

        Spanish ->
            "Spanish"


decoder : Decoder Language
decoder =
    D.string
        |> D.andThen
            (\str ->
                case str of
                    "English" ->
                        D.succeed English

                    "Spanish" ->
                        D.succeed Spanish

                    _ ->
                        D.fail <| str ++ " is not a proper language."
            )
