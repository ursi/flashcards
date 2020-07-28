module Main exposing (..)

import Array exposing (Array)
import Array.Extra as Array
import Browser exposing (Document)
import Browser.Dom as Dom
import Card exposing (Card)
import Css as C exposing (Declaration)
import Css.Global as CG
import File.Download as Download
import FoldIdentity as F
import Html.Attributes as A
import Html.Events as E
import Html.Styled as H exposing (Attribute, Html)
import Json.Decode as D exposing (Value)
import Json.Encode as En
import Ports exposing (click)
import PracticeQueue exposing (PracticeQueue)
import Random
import Random.List
import Task
import Type.TestingState as TS exposing (TestingState)
import Writer exposing (..)


todo =
    Debug.todo ""


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { cards : Array Card
    , side1 : String
    , side2 : String
    , tab : Tab
    , cardsTab : CardsTab
    , addingCard : Bool
    , editing : Maybe Int
    , editingSide1 : String
    , editingSide2 : String
    , max : Maybe Int
    , testingState : Maybe GlobalTestingState
    , practiceState : Maybe PracticeState
    , testingInput : String
    }


type Side
    = Side1
    | Side2


type alias PracticeState =
    { queue : PracticeQueue Card
    , showing : Bool
    }


type alias GlobalTestingState =
    { side1 : List ( Int, Card )
    , side2 : List ( Int, Card )
    , left : List ( Int, Card )
    , passed : List ( Int, Card )
    , side : Side
    , showing : Bool
    }


type Tab
    = Test
    | Practice
    | Cards


type CardsTab
    = All
    | Pending


init : Value -> ( Model, Cmd Msg )
init flags =
    pure
        { cards =
            D.decodeValue
                (D.field "cards" <| D.array Card.decoder)
                flags
                |> Result.withDefault Array.empty
        , side1 = ""
        , side2 = ""
        , tab = Cards
        , cardsTab = Pending
        , addingCard = False
        , editing = Nothing
        , editingSide1 = ""
        , editingSide2 = ""
        , max =
            D.decodeValue (D.map Just <| D.field "max" D.int) flags
                |> Result.withDefault Nothing
        , testingState = Nothing
        , practiceState = Nothing
        , testingInput = ""
        }



-- UPDATE


type Msg
    = UpdateSide1 String
    | UpdateSide2 String
    | AddCard
    | SetTab Tab
    | SetCardsTab CardsTab
    | SetAddingCard Bool
    | StartEditing Int String String
    | StopEditing
    | UpdateEditingSide1 String
    | UpdateEditingSide2 String
    | RemoveCard Int
    | UpdateMax (Maybe Int)
    | FillTesting
    | StartTesting
    | ShuffledTestingCardsReceived ( List ( Int, Card ), List ( Int, Card ) )
    | PassFailTesting PassFail
    | ShowBothSidesTesting
    | StartPractice
    | ShuffledPracticeCardsReceived (List Card)
    | PassPractice
    | FailPractice
    | ShowBothSidesPractice
    | Repractice Int
    | Export
    | UpdateTestingInput String
    | NoOp


type PassFail
    = Pass
    | Fail


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateTestingInput str ->
            pure { model | testingInput = format str }

        Export ->
            ( model, Download.string "flashcards.json" "application.json" <| En.encode 4 <| En.array Card.encode model.cards )

        Repractice i ->
            updateCards
                (\cards ->
                    Array.get i cards
                        |> Maybe.map Card.nextState
                        |> Maybe.map
                            (\c ->
                                cards
                                    |> Array.removeAt i
                                    |> Array.push c
                            )
                        |> Maybe.withDefault cards
                )
                model

        ShowBothSidesPractice ->
            pure
                { model
                    | practiceState =
                        model.practiceState
                            |> Maybe.map (\p -> { p | showing = True })
                }

        FailPractice ->
            updatePracticeQueue PracticeQueue.fail model

        PassPractice ->
            updatePracticeQueue PracticeQueue.pass model

        ShuffledPracticeCardsReceived cards ->
            pure
                { model
                    | practiceState =
                        PracticeQueue.fromList cards
                            |> Maybe.map (PracticeState >> (|>) False)
                }

        StartPractice ->
            tell
                (model.cards
                    |> Array.toList
                    |> List.filter
                        (\{ state } ->
                            case state of
                                TS.Side1 _ ->
                                    True

                                TS.Side2 _ ->
                                    True

                                _ ->
                                    False
                        )
                    |> Random.List.shuffle
                    |> Random.generate ShuffledPracticeCardsReceived
                )
                |> dure model

        ShowBothSidesTesting ->
            model
                |> ifTS
                    (\ts ->
                        pure
                            { model
                                | testingState = Just { ts | showing = True }
                            }
                    )

        PassFailTesting passFail ->
            model
                |> ifTS
                    (\testingState ->
                        let
                            partialModel =
                                { model
                                    | testingInput = ""
                                }
                        in
                                (case testingState.left of
                                    head :: tail ->
                                        let
                                            partialTS =
                                                { testingState
                                                    | passed =
                                                        if passFail == Pass then
                                                            head :: testingState.passed

                                                        else
                                                            testingState.passed
                                                    , showing = False
                                                }

                                            updateTestingState ts =
                                                pure { partialModel | testingState = Just ts }

                                            finish =
                                                let
                                                    passed =
                                                        partialTS.passed
                                                            |> List.map (Tuple.mapSecond Card.nextState)
                                                in
                                                pure { partialModel | testingState = Nothing }
                                                    |> bind (updateCards (Card.updateCards passed))
                                        in
                                        if List.isEmpty tail then
                                            if partialTS.side == Side1 then
                                                if List.isEmpty partialTS.side2 then
                                                    finish

                                                else
                                                    updateTestingState
                                                        { partialTS
                                                            | left = partialTS.side2
                                                            , side = Side2
                                                        }

                                            else
                                                finish

                                        else
                                            updateTestingState { partialTS | left = tail }

                                    [] ->
                                        pure partialModel
                                )
                    )

        ShuffledTestingCardsReceived ( side1, side2 ) ->
            let
                ( side, left ) =
                    if not <| List.isEmpty side1 then
                        ( Side1, side1 )

                    else
                        ( Side2, side2 )
            in
            pure
                { model
                    | testingState =
                        if List.isEmpty side1 && List.isEmpty side2 then
                            Nothing

                        else
                            Just
                                { side1 = side1
                                , side2 = side2
                                , left = left
                                , passed = []
                                , side = side
                                , showing = False
                                }
                }

        StartTesting ->
            let
                ( side1, side2 ) =
                    model.cards
                        |> Array.toIndexedList
                        |> List.foldl
                            (\( i, card ) acc ->
                                case card.state of
                                    TS.Side1 _ ->
                                        acc
                                            |> Tuple.mapFirst ((::) ( i, card ))

                                    TS.Side2 _ ->
                                        acc
                                            |> Tuple.mapSecond ((::) ( i, card ))

                                    _ ->
                                        acc
                            )
                            ( [], [] )
            in
            tell
                (Random.generate ShuffledTestingCardsReceived <|
                    Random.map2 Tuple.pair
                        (Random.List.shuffle side1)
                        (Random.List.shuffle side2)
                )
                |> dure model

        FillTesting ->
            case model.max of
                Just max ->
                    updateCards (Card.fill max) model

                Nothing ->
                    pure model

        UpdateMax mi ->
            (case mi of
                Just i ->
                    tell <| Ports.writeMax i

                Nothing ->
                    pure ()
            )
                |> dure { model | max = mi }

        RemoveCard i ->
            updateCards (Array.removeAt i) model

        UpdateEditingSide2 str ->
            pure { model | editingSide2 = str }

        UpdateEditingSide1 str ->
            pure { model | editingSide1 = format str }

        StopEditing ->
            case model.editing of
                Just i ->
                    updateCards
                        (Card.updateSides
                            i
                            model.editingSide1
                            model.editingSide2
                        )
                        model
                        |> map (\m -> { m | editing = Nothing })

                Nothing ->
                    pure model

        StartEditing i side1 side2 ->
            pure
                { model
                    | editing = Just i
                    , editingSide1 = side1
                    , editingSide2 = side2
                }

        SetAddingCard b ->
            pure { model | addingCard = b }

        SetCardsTab ct ->
            pure { model | cardsTab = ct }

        SetTab tab ->
            pure { model | tab = tab }

        AddCard ->
            let
                cards =
                    Array.push
                        (Card.new model.side1 model.side2)
                        model.cards
            in
            tell (Card.write cards)
                |> dell (focus side1Id)
                |> dure
                    { model
                        | cards = cards
                        , side1 = ""
                        , side2 = ""
                    }

        UpdateSide2 str ->
            pure { model | side2 = str }

        UpdateSide1 str ->
            pure { model | side1 = format str }

        NoOp ->
            pure model


focus : String -> Cmd Msg
focus id =
    Task.attempt (always NoOp) <| Dom.focus id


blur : String -> Cmd Msg
blur id =
    Task.attempt (always NoOp) <| Dom.blur id


format : String -> String
format =
    String.replace "a`" "á"
        >> String.replace "e`" "é"
        >> String.replace "i`" "í"
        >> String.replace "o`" "ó"
        >> String.replace "u`" "ú"
        >> String.replace "u:" "ü"
        >> String.replace "n~" "ñ"
        >> String.replace "A`" "Á"
        >> String.replace "E`" "É"
        >> String.replace "I`" "Í"
        >> String.replace "O`" "Ó"
        >> String.replace "U`" "Ú"
        >> String.replace "U:" "Ü"
        >> String.replace "N~" "Ñ"


updatePracticeQueue : (PracticeQueue Card -> Maybe (PracticeQueue Card)) -> Model -> ( Model, Cmd Msg )
updatePracticeQueue f model =
    tell (focus testInputId)
        |> dure
            { model
                | practiceState =
                    model.practiceState
                        |> Maybe.map (\p -> { p | showing = False })
                        |> Maybe.andThen
                            (\p ->
                                f p.queue
                                    |> Maybe.map (\q -> { p | queue = q })
                            )
                , testingInput = ""
            }


ifTS : (GlobalTestingState -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
ifTS f model =
    case model.testingState of
        Just ts ->
            f ts

        Nothing ->
            pure model


updateCards : (Array Card -> Array Card) -> Model -> ( Model, Cmd Msg )
updateCards f model =
    let
        newCards =
            f model.cards
    in
    ( { model | cards = newCards }, Card.write newCards )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = ""
    , body =
        [ header model
        , body model
        ]
            |> H.withStyles [ CG.body [ C.font "1.5rem sans-serif" ] ]
    }


header : Model -> Html Msg
header model =
    H.headerS
        [ C.display "grid"
        , C.grid "auto/auto-flow 1fr"
        ]
        []
        [ focusedTab (model.tab == Test) [ E.onClick <| SetTab Test ] [ H.text "Test" ]
        , focusedTab (model.tab == Practice) [ E.onClick <| SetTab Practice ] [ H.text "Practice" ]
        , focusedTab (model.tab == Cards) [ E.onClick <| SetTab Cards ] [ H.text "Cards" ]
        , H.buttonS
            [ C.position "absolute"
            , C.top ".5em"
            , C.right ".5em"
            ]
            [ E.onClick Export ]
            [ H.text "Export" ]
        ]


focusedTab : Bool -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
focusedTab bool =
    H.divS <|
        if bool then
            [ C.color "red" ]

        else
            []


body : Model -> Html Msg
body model =
    case model.tab of
        Test ->
            testTab model

        Practice ->
            practiceTab model

        Cards ->
            cardsTab model


practiceTab : Model -> Html Msg
practiceTab model =
    H.div []
        [ H.div []
            [ currentTestingHtml model
            , F.maybe idH
                (\_ ->
                    H.div [] [ H.button [ E.onClick StartPractice ] [ H.text "Start" ] ]
                )
                (F.bool <| model.practiceState == Nothing)
            ]
        , practiceCardsHtml model
        ]


practiceCardsHtml : { r | practiceState : Maybe PracticeState, testingInput : String } -> Html Msg
practiceCardsHtml { practiceState, testingInput } =
    practiceState
        |> F.maybe idH
            (\ps ->
                let
                    { queue, showing } =
                        ps

                    card =
                        PracticeQueue.selected queue

                    ( ( _, side1 ), ( _, side2 ) ) =
                        card.sides

                    ( primary, secondary ) =
                        case card.state of
                            TS.Side1 _ ->
                                ( side1, side2 )

                            TS.Side2 _ ->
                                ( side2, side1 )

                            _ ->
                                ( "uh oh, something went wrong", "" )
                in
                cardsHtml
                    { pass = PassPractice
                    , fail = FailPractice
                    , show = ShowBothSidesPractice
                    , showing = primary
                    , maybeShowing =
                        if showing then
                            Just secondary

                        else
                            Nothing
                    , testing = testingInput
                    }
            )


testTab : Model -> Html Msg
testTab model =
    H.div []
        [ H.div []
            [ currentTestingHtml model
            , F.maybe idH
                (\_ -> preTestingControls model)
                (F.bool <| model.testingState == Nothing)
            ]
        , testingCardsHtml model
        ]


testingCardsHtml : { r | testingState : Maybe GlobalTestingState, testingInput : String } -> Html Msg
testingCardsHtml { testingState, testingInput } =
    testingState
        |> F.maybe idH
            (\{ left, side, showing } ->
                case left of
                    ( _, card ) :: _ ->
                        let
                            ( ( _, side1 ), ( _, side2 ) ) =
                                card.sides

                            ( primary, secondary ) =
                                if side == Side1 then
                                    ( side1, side2 )

                                else
                                    ( side2, side1 )
                        in
                        cardsHtml
                            { pass = PassFailTesting Pass
                            , fail = PassFailTesting Fail
                            , show = ShowBothSidesTesting
                            , showing = primary
                            , maybeShowing =
                                if showing then
                                    Just secondary

                                else
                                    Nothing
                            , testing = testingInput
                            }

                    [] ->
                        idH
            )


cardsHtml : { pass : Msg, fail : Msg, show : Msg, showing : String, maybeShowing : Maybe String, testing : String } -> Html Msg
cardsHtml { pass, fail, show, showing, maybeShowing, testing } =
    H.div []
        [ H.divS
            [ C.display "grid"
            , C.grid "auto/auto-flow 1fr"
            , C.marginTop "3em"
            , C.textAlign "center"
            , C.whiteSpace "pre-wrap"
            ]
            []
            [ H.div [] [ H.text showing ]
            , F.maybe idH
                (\text -> H.div [] [ H.text text ])
                maybeShowing
            ]
        , H.divS
            [ C.display "grid"
            , C.justifyItems "center"
            , C.marginTop "1em"
            ]
            []
            [ H.divS
                [ C.display "grid"
                , C.grid "auto/auto-flow max-content"
                , C.columnGap "3em"
                ]
                []
              <|
                case maybeShowing of
                    Just _ ->
                        [ H.button [ E.onClick pass ] [ H.text "Pass" ]
                        , H.button [ E.onClick fail ] [ H.text "Fail" ]
                        ]

                    Nothing ->
                        [ H.button [ E.onClick show ] [ H.text "Show" ] ]
            ]
        , H.divS
            [ C.textAlign "center"
            , C.marginTop "1em"
            ]
            []
            [ cardInput
                [ A.value testing
                , A.attribute "spellcheck" "false"
                , E.onInput UpdateTestingInput
                ]
                []
            ]
        ]


preTestingControls : Model -> Html Msg
preTestingControls model =
    H.div []
        [ H.div []
            [ H.label []
                [ H.text "Max "
                , H.input
                    [ A.value <|
                        case model.max of
                            Just i ->
                                String.fromInt i

                            Nothing ->
                                ""
                    , E.onInput <| UpdateMax << String.toInt
                    ]
                    []
                ]
            , H.button [ E.onClick FillTesting ] [ H.text "Fill" ]
            ]
        , H.div [] [ H.button [ E.onClick StartTesting ] [ H.text "Start" ] ]
        ]


currentTestingHtml : { r | cards : Array Card } -> Html Msg
currentTestingHtml { cards } =
    H.div [] [ H.text <| "Current: " ++ String.fromInt (Card.currentlyTesting cards) ]


idH : Html Msg
idH =
    H.text ""


cardsTab : Model -> Html Msg
cardsTab model =
    H.div []
        [ H.divS
            [ C.display "grid"
            , C.gridTemplateColumns "1fr 1fr"
            ]
            []
            [ focusedTab (model.cardsTab == Pending) [ E.onClick <| SetCardsTab Pending ] [ H.text "Pending" ]
            , focusedTab (model.cardsTab == All) [ E.onClick <| SetCardsTab All ] [ H.text "All" ]
            ]
        , if model.addingCard then
            addingCardHtml model

          else
            H.div [] [ H.button [ E.onClick <| SetAddingCard True ] [ H.text "Add Cards" ] ]
        , case model.cardsTab of
            All ->
                model.cards
                    |> Array.toIndexedList
                    |> List.sortBy
                        (\( _, card ) ->
                            let
                                ( ( _, side1 ), _ ) =
                                    card.sides
                            in
                            side1
                        )
                    |> cardListHtml model

            Pending ->
                model.cards
                    |> Array.toIndexedList
                    |> List.filter
                        (\( _, card ) ->
                            case card.state of
                                TS.Pending ->
                                    True

                                _ ->
                                    False
                        )
                    |> cardListHtml model
        ]


cardListHtml : Model -> List ( Int, Card ) -> Html Msg
cardListHtml model =
    List.indexedMap
        (\row ( i, card ) ->
            let
                ( ( language1, side1 ), ( language2, side2 ) ) =
                    card.sides
            in
            H.divS
                [ C.display "contents"
                , C.children [ C.gridRow <| String.fromInt <| row + 1 ]
                ]
                []
                (model.editing
                    |> Maybe.andThen
                        (\index ->
                            if index == i then
                                Just
                                    [ cardInput
                                        [ A.value model.editingSide1
                                        , A.attribute "spellecheck" "false"
                                        , E.onInput UpdateEditingSide1
                                        ]
                                        []
                                    , cardInput
                                        [ A.value model.editingSide2
                                        , E.onInput UpdateEditingSide2
                                        ]
                                        []
                                    , H.button [ E.onClick StopEditing ] [ H.text "Save " ]
                                    ]

                            else
                                Nothing
                        )
                    |> Maybe.withDefault
                        [ H.div [] [ H.text side1 ]
                        , H.div [] [ H.text side2 ]
                        , H.button [ E.onClick <| StartEditing i side1 side2 ] [ H.text "Edit" ]
                        , H.button [ E.onDoubleClick <| RemoveCard i ] [ H.text "Remove" ]
                        , F.maybe idH
                            (\_ -> H.button [ E.onClick <| Repractice i ] [ H.text "Practice" ])
                            (F.bool <| card.state == TS.Completed)
                        ]
                )
        )
        >> H.divS
            [ C.display "grid"
            , C.columnGap "1em"
            , C.gridTemplateColumns "1fr 1fr repeat(3, max-content)"
            , C.textAlign "center"
            , tableBorder "black" "white" 2
            , C.whiteSpace "pre-wrap"
            ]
            []


cardInput : List (Attribute Msg) -> List (Html Msg) -> Html Msg
cardInput =
    H.textareaS [ C.fontSize "inherit" ]


side1Id =
    "side-1"


addingCardHtml : { r | side1 : String, side2 : String } -> Html Msg
addingCardHtml { side1, side2 } =
    H.div []
        [ cardInput
            [ A.id side1Id
            , A.value side1
            , A.attribute "spellcheck" "false"
            , E.onInput UpdateSide1
            ]
            []
        , cardInput
            [ A.value side2
            , E.onInput UpdateSide2
            ]
            []
        , H.button
            [ A.disabled (String.isEmpty side1 || String.isEmpty side2)
            , E.onClick AddCard
            ]
            [ H.text "Add" ]
        , H.button [ E.onClick <| SetAddingCard False ] [ H.text "Close" ]
        ]


tableBorder : String -> String -> Float -> Declaration
tableBorder borderColor backgroundColor width =
    C.batch
        [ C.background borderColor
        , C.borderJ [ C.px width, "solid", borderColor ]
        , C.gap <| C.px width
        , C.children [ C.child "div" [ C.background backgroundColor ] ]
        ]
