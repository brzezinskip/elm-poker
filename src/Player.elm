module Player exposing
    ( Msg(..)
    , Player
    , actionToText
    , createPlayers
    , setBankroll
    , setBetSize
    , viewBankroll
    , viewPlayers
    )

import Card
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes
import Html.Events exposing (onClick, onInput)


type Msg
    = Call -- minimum required to continue playing a hand
    | Check -- to pass on betting. I there's no bet to player, there's nothing to call. If player doesn't want to bet, player can check. If there's no action from other players in the betting round
    | CheckRaise Int -- check current bet and raise by X  amount before moving to the next player
      -- then action comes back to player to call fold or raise
    | Fold -- player folds a hand when they don't want to participate
    | AllIn -- put everything you have into the pot
    | ChangedBetSize String


type alias Player =
    { id : Int
    , bankroll : Int
    , hand : Card.Cards
    , isPlaying : Bool
    , bet : Maybe Int
    }


createPlayer : Int -> Player
createPlayer id =
    { id = id
    , bankroll = 1000
    , hand = []
    , isPlaying = True
    , bet = Nothing
    }


createPlayers : Int -> List Player
createPlayers noOfPlayers =
    List.range 1 noOfPlayers |> List.map createPlayer


setBankroll : List Player -> Int -> (Int -> Int) -> List Player
setBankroll players currentPlayerId setter =
    List.map
        (\p ->
            if p.id == currentPlayerId then
                { p | bankroll = setter p.bankroll }

            else
                p
        )
        players


setBetSize : List Player -> Int -> String -> List Player
setBetSize players currentPlayerId amount =
    List.map
        (\p ->
            if p.id == currentPlayerId then
                { p
                    | bet =
                        if amount == "0" then
                            Nothing

                        else
                            String.toInt amount
                }

            else
                p
        )
        players


actionToText : Msg -> Html Msg
actionToText action =
    text <|
        case action of
            Call ->
                "Call"

            Check ->
                "Check"

            CheckRaise v ->
                "Raise by " ++ String.fromInt v

            Fold ->
                "Fold"

            AllIn ->
                "All In"

            ChangedBetSize _ ->
                ""


type alias ActionConfig =
    { disabled : Bool
    , action : Msg
    }


viewAction : ActionConfig -> Html Msg
viewAction { action, disabled } =
    let
        attrs =
            if disabled then
                []

            else
                [ onClick action ]
    in
    li [] [ button attrs [ actionToText action ] ]


viewBankroll : Player -> Html Msg
viewBankroll { bankroll } =
    li [] [ text ("coins: " ++ String.fromInt bankroll) ]


viewPlayerHand : Card.Cards -> Html Msg
viewPlayerHand cards =
    ul [] (List.map Card.view cards)


viewPlayerInput : Player -> Html Msg
viewPlayerInput { bankroll } =
    input
        [ onInput ChangedBetSize
        , Html.Attributes.max (String.fromInt bankroll)
        , Html.Attributes.type_ "number"
        ]
        [ text "bet size" ]


viewPlayer : Int -> Player -> Html Msg
viewPlayer currentPlayerId player =
    let
        availableActions =
            if currentPlayerId == player.id then
                [ { action = Call, disabled = False }
                , { action = Check, disabled = False }
                , { action = Fold, disabled = False }
                , { action = AllIn, disabled = False }
                ]
                    ++ (case player.bet of
                            Just n ->
                                [ { action = CheckRaise n, disabled = n >= player.bankroll } ]

                            Nothing ->
                                []
                       )

            else
                []

        betSize =
            if currentPlayerId == player.id then
                viewPlayerInput player

            else
                text ""
    in
    ul []
        (li [] [ text <| "Player no: " ++ String.fromInt player.id ]
            :: viewBankroll player
            :: viewPlayerHand player.hand
            :: betSize
            :: List.map viewAction availableActions
        )


viewPlayers : Int -> List Player -> Html Msg
viewPlayers currentPlayerId players =
    let
        participants =
            List.filter .isPlaying players
    in
    div [] (List.map (viewPlayer currentPlayerId) participants)
