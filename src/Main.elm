module Main exposing (main)

import Browser
import Card
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List.Extra as List
import Player
import Random
import Random.List as RandomList


type alias Players =
    List Player.Player


type alias Game =
    { pot : Int
    , currentPlayerId : Int
    , cards : Card.Cards
    , tableCards : Card.Cards
    , players : Players
    , passedTurns : Int
    }


type GameStatus
    = Settings
    | InProgress Game


type alias Model =
    GameStatus


type Msg
    = NoOp
    | GotShuffledDeck Card.Cards
    | StartGameClicked
    | PlayerAction Player.Msg


init : ( Model, Cmd Msg )
init =
    ( Settings, Cmd.none )


dealToPlayers : Card.Cards -> Players -> Players -> ( Players, Card.Cards )
dealToPlayers cards acc players =
    let
        ( playerCards, remainingCards ) =
            Card.deal 2 cards
    in
    case players of
        [] ->
            ( acc, cards )

        player :: rest ->
            dealToPlayers remainingCards ({ player | hand = playerCards } :: acc) rest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGameClicked ->
            ( model, Random.generate GotShuffledDeck (RandomList.shuffle Card.deck) )

        GotShuffledDeck shuffledDeck ->
            let
                -- dealer drops the first card from the top of stack before dealing cards
                withoutFirst =
                    List.drop 1 shuffledDeck

                -- Get first 3 cards to put them on the table and user remaining cards to deal to players
                ( tableCards, remainingCards ) =
                    withoutFirst |> Card.deal 3

                -- Deal two cards to each player, keep remaining cards and store them in the game so we can use them to deal to the table later
                ( players, cardsAfterDealt ) =
                    Player.createPlayers 4 |> dealToPlayers remainingCards []
            in
            ( InProgress
                { cards = cardsAfterDealt
                , tableCards = tableCards
                , players = players |> List.reverse
                , passedTurns = 0
                , currentPlayerId = 1
                , pot = 0
                }
            , Cmd.none
            )

        PlayerAction action ->
            case model of
                Settings ->
                    ( model, Cmd.none )

                InProgress game ->
                    let
                        newGame =
                            game |> setNextPlayer
                    in
                    case action of
                        Player.Call ->
                            ( InProgress newGame, Cmd.none )

                        Player.Check ->
                            ( InProgress
                                ({ game
                                    | players = Player.setBetSize game.players game.currentPlayerId "0"
                                 }
                                    |> setNextPlayer
                                )
                            , Cmd.none
                            )

                        Player.CheckRaise bet ->
                            ( { game | players = Player.setBankroll game.players game.currentPlayerId (\curr -> curr - bet) }
                                |> addToPot bet
                                |> setNextPlayer
                                |> InProgress
                            , Cmd.none
                            )

                        Player.Fold ->
                            ( exitHand game |> InProgress, Cmd.none )

                        Player.AllIn ->
                            ( { game | players = Player.setBankroll game.players game.currentPlayerId (\_ -> 0) }
                                |> addToPot 1000
                                |> setNextPlayer
                                |> InProgress
                            , Cmd.none
                            )

                        Player.ChangedBetSize bet ->
                            ( InProgress
                                { game
                                    | players = Player.setBetSize game.players game.currentPlayerId bet
                                }
                            , Cmd.none
                            )


addToPot : Int -> Game -> Game
addToPot value game =
    { game | pot = game.pot + value }


exitHand : Game -> Game
exitHand ({ currentPlayerId, players } as game) =
    { game
        | players =
            List.map
                (\p ->
                    if currentPlayerId == p.id then
                        { p | isPlaying = False }

                    else
                        p
                )
                players
    }
        |> setNextPlayer


setNextPlayer : Game -> Game
setNextPlayer game =
    let
        playing =
            List.filter .isPlaying game.players
    in
    { game
        | currentPlayerId =
            if
                (playing
                    |> List.reverse
                    |> List.head
                    |> Maybe.map .id
                )
                    == Just game.currentPlayerId
            then
                playing
                    |> List.head
                    |> Maybe.map .id
                    |> Maybe.withDefault 1

            else
                let
                    currentPlayerPosition =
                        List.findIndex (\p -> p.id == game.currentPlayerId) game.players
                            |> Maybe.withDefault 0

                    nextPlayer index =
                        case List.getAt (currentPlayerPosition + index) game.players of
                            Just player ->
                                if player.isPlaying then
                                    player.id

                                else
                                    nextPlayer (index + 1)

                            Nothing ->
                                1
                in
                nextPlayer 1
    }


view : Model -> Html Msg
view model =
    div []
        [ case model of
            Settings ->
                div [] [ button [ onClick StartGameClicked ] [ text "Start" ] ]

            InProgress game ->
                div
                    []
                    [ Html.map PlayerAction (Player.viewPlayers game.currentPlayerId game.players) ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \() -> init
        , update = update
        , subscriptions = subscriptions
        }
