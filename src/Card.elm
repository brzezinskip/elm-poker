module Card exposing (Cards, deal, deck, view)

import Html exposing (Html, li, text)


type alias Cards =
    List ( Face, Color )


type alias Hand =
    Cards


join : (a -> b -> Bool) -> List a -> List b -> List ( a, b )
join p xs ys =
    xs
        |> List.concatMap
            (\x ->
                ys
                    |> List.concatMap
                        (\y ->
                            if p x y then
                                [ ( x, y ) ]

                            else
                                []
                        )
            )


type Face
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | J
    | Q
    | K
    | A


faceToString : Face -> String
faceToString face =
    case face of
        Two ->
            "2"

        Three ->
            "3"

        Four ->
            "4"

        Five ->
            "5"

        Six ->
            "6"

        Seven ->
            "7"

        Eight ->
            "8"

        Nine ->
            "9"

        Ten ->
            "10"

        J ->
            "J"

        Q ->
            "Q"

        K ->
            "K"

        A ->
            "A"


type Color
    = Clubs
    | Diamonds
    | Hearts
    | Spades


colorToString : Color -> String
colorToString color =
    case color of
        Clubs ->
            "Clubs"

        Diamonds ->
            "Diamonds"

        Hearts ->
            "Hearts"

        Spades ->
            "Spades"


faces : List Face
faces =
    [ Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, J, Q, K, A ]


colors : List Color
colors =
    [ Clubs, Diamonds, Hearts, Spades ]


deck : Cards
deck =
    join (\_ _ -> True) faces colors


deal : Int -> Cards -> ( Hand, Cards )
deal howMany cards =
    ( List.take howMany cards, List.drop howMany cards )


view : ( Face, Color ) -> Html msg
view card =
    let
        ( face, color ) =
            Tuple.mapBoth faceToString colorToString card
    in
    li [] [ text (face ++ " " ++ color) ]
