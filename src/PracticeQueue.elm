module PracticeQueue exposing (..)


type alias PracticeQueue a =
    ( List a, a, List a )


fromList : List a -> Maybe (PracticeQueue a)
fromList list =
    case list of
        head :: tail ->
            Just ( [], head, tail )

        [] ->
            Nothing


pass : PracticeQueue a -> Maybe (PracticeQueue a)
pass ( before, a, after ) =
    case after of
        head :: tail ->
            Just ( before, head, tail )

        [] ->
            fromList <| List.reverse before


fail : PracticeQueue a -> Maybe (PracticeQueue a)
fail ( before, a, after ) =
    case after of
        head :: tail ->
            Just ( a :: before, head, tail )

        [] ->
            a
                :: before
                |> List.reverse
                |> fromList


selected : PracticeQueue a -> a
selected ( _, a, _ ) =
    a
