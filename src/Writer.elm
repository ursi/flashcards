module Writer exposing
    ( bind
    , dell
    , dind
    , dure
    , map
    , pure
    , tell
    )


map : (a -> b) -> ( a, Cmd msg ) -> ( b, Cmd msg )
map f ( a, cmd ) =
    ( f a, cmd )


bind : (a -> ( model, Cmd msg )) -> ( a, Cmd msg ) -> ( model, Cmd msg )
bind f ( a, cmd ) =
    let
        ( model, newCmd ) =
            f a
    in
    ( model, Cmd.batch [ cmd, newCmd ] )


{-| discard + bind
-}
dind : ( model, Cmd msg ) -> ( (), Cmd msg ) -> ( model, Cmd msg )
dind w1 w2 =
    bind (\_ -> w1) w2


pure : model -> ( model, Cmd msg )
pure model =
    ( model, Cmd.none )


dure : model -> ( (), Cmd msg ) -> ( model, Cmd msg )
dure =
    dind << pure


tell : Cmd msg -> ( (), Cmd msg )
tell cmd =
    ( (), cmd )


dell : Cmd msg -> ( (), Cmd msg ) -> ( (), Cmd msg )
dell =
    dind << tell


when : Bool -> ( (), Cmd msg ) -> ( (), Cmd msg )
when cond w =
    if cond then
        w

    else
        pure ()
