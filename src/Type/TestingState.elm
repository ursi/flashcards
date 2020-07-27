module Type.TestingState exposing (TestingState(..), decoder, encode)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)


type TestingState
    = Side1 Int
    | Side2 Int
    | Pending
    | Completed


encode : TestingState -> Value
encode ts =
    case ts of
        Side1 n ->
            E.list identity [ E.string "Side1", E.int n ]

        Side2 n ->
            E.list identity [ E.string "Side2", E.int n ]

        Pending ->
            E.string "Pending"

        Completed ->
            E.string "Completed"


decoder : Decoder TestingState
decoder =
    D.oneOf
        [ D.string
            |> D.andThen
                (\str ->
                    case str of
                        "Pending" ->
                            D.succeed Pending

                        "Completed" ->
                            D.succeed Completed

                        _ ->
                            D.fail ""
                )
        , D.map2
            (\str i ->
                case str of
                    "Side1" ->
                        D.succeed <| Side1 i

                    "Side2" ->
                        D.succeed <| Side2 i

                    _ ->
                        D.fail "invalid testing state"
            )
            (D.index 0 D.string)
            (D.index 1 D.int)
            |> D.andThen identity
        ]
