port module Card exposing
    ( Card
    , currentlyTesting
    , decoder
    , encode
    , fill
    , new
    , nextState
    , updateCards
    , updateSides
    , write
    )

import Array exposing (Array)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E exposing (Value)
import SupPort
import Type.Language as Language exposing (Language(..))
import Type.TestingState as TestingState exposing (TestingState(..))


type alias Card =
    { sides : ( ( Language, String ), ( Language, String ) )
    , completed : Int
    , state : TestingState
    }


nextState : Card -> Card
nextState ({ completed, state } as card) =
    case state of
        Side1 n ->
            if n > 1 then
                { card | state = Side1 <| n - 1 }

            else
                { card | state = Side2 <| completed + 1 }

        Side2 n ->
            if n > 1 then
                { card | state = Side2 <| n - 1 }

            else
                { card
                    | completed = completed + 1
                    , state = Completed
                }

        Pending ->
            { card | state = Side1 <| completed + 1 }

        Completed ->
            { card | state = Pending }


encode : Card -> Value
encode card =
    let
        ( ( language1, side1 ), ( language2, side2 ) ) =
            card.sides
    in
    E.object
        [ ( "sides"
          , E.list (E.list E.string)
                [ [ Language.toString language1, side1 ]
                , [ Language.toString language2, side2 ]
                ]
          )
        , ( "completed", E.int card.completed )
        , ( "state", TestingState.encode card.state )
        ]


decoder : Decoder Card
decoder =
    D.map3 Card
        (D.field "sides" <|
            D.map2 Tuple.pair
                (D.index 0 <|
                    D.map2 Tuple.pair
                        (D.index 0 Language.decoder)
                        (D.index 1 D.string)
                )
                (D.index 1 <|
                    D.map2 Tuple.pair
                        (D.index 0 Language.decoder)
                        (D.index 1 D.string)
                )
        )
        (D.field "completed" D.int)
        (D.field "state" TestingState.decoder)


new : String -> String -> Card
new spanish english =
    Card
        ( ( Spanish, spanish ), ( English, english ) )
        0
        Pending


updateSides : Int -> String -> String -> Array Card -> Array Card
updateSides index side1 side2 cards =
    Array.get index cards
        |> Maybe.map
            (\card ->
                let
                    ( ( l1, _ ), ( l2, _ ) ) =
                        card.sides
                in
                Array.set index { card | sides = ( ( l1, side1 ), ( l2, side2 ) ) } cards
            )
        |> Maybe.withDefault cards


currentlyTesting : Array Card -> Int
currentlyTesting =
    Array.filter
        (\{ state } ->
            case state of
                Side1 _ ->
                    True

                Side2 _ ->
                    True

                _ ->
                    False
        )
        >> Array.length


fill : Int -> Array Card -> Array Card
fill max cards =
    fillHelper max (currentlyTesting cards) 0 cards


fillHelper : Int -> Int -> Int -> Array Card -> Array Card
fillHelper max current index cards =
    if max <= current || index == Array.length cards then
        cards

    else
        Array.get index cards
            |> Maybe.map
                (\card ->
                    case card.state of
                        Pending ->
                            fillHelper
                                max
                                (current + 1)
                                (index + 1)
                                (Array.set
                                    index
                                    (nextState card)
                                    cards
                                )

                        _ ->
                            fillHelper max current (index + 1) cards
                )
            |> Maybe.withDefault cards


updateCards : List ( Int, Card ) -> Array Card -> Array Card
updateCards newCards cards =
    case newCards of
        ( i, newCard ) :: tail ->
            updateCards tail (Array.set i newCard cards)

        [] ->
            cards



--PORTS


write : Array Card -> Cmd msg
write cards =
    out "Write" [ E.array encode cards ]


out : String -> List Value -> Cmd msg
out =
    SupPort.out cardOut


port cardOut : Value -> Cmd msg
