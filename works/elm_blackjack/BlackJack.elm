module BlackJack exposing (..)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes as Attr
import Random exposing (Seed, generate)
import Random.List as RL exposing (shuffle)
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Events as Events
import Process
import Task


-- MAIN
main : Program Flags Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type GameState
  = Initial
  | Start
  | Win
  | Loss
  | Tie
  | InPlay

type alias Card 
  = (String, String)

unshuffledDeck : List Card
unshuffledDeck =
  let
    suits = ["D", "H", "C", "S"]
    values = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]
    applySuit s v =
      List.map (\y -> (v, y)) s
  in
    List.concat (List.map (\x -> applySuit suits x) values)

newDeck : Random.Generator (List Card)
newDeck =
  let
    suits = ["D", "H", "C", "S"]
    values = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"]
    applySuit s v =
      List.map (\y -> (v,y)) s
  in
    RL.shuffle (List.concat (List.map (\x -> applySuit suits x) values))

type alias Model =
  { dealerHand : List Card
  , playerHand : List Card
  , deck : List Card
  , discard : List Card
  , playerTurn : Bool
  , winCount : Int
  , lossCount : Int
  , tieCount : Int
  , state : GameState
  }

type alias Flags =
  ()

init : Flags -> (Model, Cmd Msg)
init () =
  let
    initModel =
      { dealerHand = [("gray", "_back"), ("gray", "_back")]
      , playerHand = [("gray", "_back"), ("gray", "_back")]
      , deck = []
      , discard = []
      , playerTurn = True
      , winCount = 0
      , lossCount = 0
      , tieCount = 0
      , state = Initial
      }
  in
    (initModel, Random.generate ReplaceDeck newDeck)
    --update Shuffle initModel


-- UPDATE

type Msg
  = Hit
  | Stand
  | NewGame
  | Shuffle
  | ReplaceDeck (List Card)
  | UpdateGameState
  | Reset

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Hit ->
      case model.playerTurn of
        True ->
          updateGameState (playerHit model)
        _ ->
          updateGameState (dealerHit model)
    Stand ->
      let
        newModel = replaceHiddenCard model
      in
      updateGameState newModel
      --updateGameState { model | playerTurn = False }
    NewGame ->
      updateGameState (startGame model)
    Shuffle ->
      (model, Random.generate ReplaceDeck (RL.shuffle model.deck))
      --update (Random.generate ReplaceDeck (RL.shuffle model.deck)) model
      --(model, generate ReplaceDeck (RL.shuffle model.deck))
    ReplaceDeck shuffled ->
      ({ model | deck = shuffled }, Cmd.none)
    UpdateGameState ->
      (updateGameState model)
    Reset ->
      init ()

replaceHiddenCard : Model -> Model
replaceHiddenCard model =
  case model.deck of
    [] ->
      { model | dealerHand = [] } --This should never happen
    c :: cs ->
      case model.dealerHand of
        [] ->
          model --This should never happen
        x :: xs ->
          { model 
          | dealerHand = c :: xs
          , deck = cs
          , playerTurn = False
          }


updateGameState : Model -> (Model, Cmd Msg)
updateGameState model =
  let
    newModel = checkForWinner model
  in
    case newModel.state of
      Initial ->
        ({ newModel | state = Start }, Cmd.none)
      Start ->
        ({ newModel | state = InPlay }, Cmd.none)
      Win ->
        ({ newModel | winCount = newModel.winCount + 1 }, Cmd.none)
      Loss ->
        ({ newModel | lossCount = newModel.lossCount + 1 }, Cmd.none)
      Tie ->
        ({ newModel | tieCount = newModel.tieCount + 1 }, Cmd.none)
      InPlay ->
        case newModel.playerTurn of
          True ->
            (newModel, Cmd.none)
          _ ->
            (newModel, dealerWait)
            --(update Hit newModel)
            --(newModel, Hit)

dealerWait : Cmd Msg
dealerWait =
  Process.sleep 1000
    |> Task.perform (\_ -> Hit)

checkForWinner : Model -> Model
checkForWinner model =
  let
    (playerTotal, _) = getHandState model.playerHand
    (dealerTotal, dSoft) = getHandState model.dealerHand
  in
    case (model.state, model.playerTurn) of
      (Start, _) ->
        if playerTotal == 21 && dealerTotal <= 11 then
          let
            newModel = replaceHiddenCard { model 
                                         | state = InPlay
                                         , playerTurn = False
                                         }
            --(newModel, _) =
              --(update Stand { model | state = InPlay, playerTurn = False })
          in
            case (getHandState newModel.dealerHand) of
              (21, _) ->
                { newModel | state = Tie }
              _ ->
                { newModel | state = Win }
          --{ model | state = InPlay, playerTurn = False }
          --{ model | state = Win }
        else
          { model | state = InPlay }
      (InPlay, True) ->
        if playerTotal > 21 then
          let
            newModel = (replaceHiddenCard { model | state = Loss })
          in
            newModel
          --{ model | state = Loss }
        --else if playerTotal > dealerTotal && dealerTotal >= 17 then
          --{ model | state = Win }
        else if playerTotal == 21 then
          let
            (newModel, _) =
              (update Stand { model | state = InPlay, playerTurn = False })
          in
            newModel
        else
          { model | state = InPlay }
      (InPlay, False) ->
        case (dealerTotal >= 17, dSoft) of
          (True, False) ->
            if dealerTotal > 21 && playerTotal <= 21 then
              { model | state = Win }
            else if dealerTotal > playerTotal then
              { model | state = Loss }
            else if dealerTotal == playerTotal then
              { model | state = Tie }
            else
              { model | state = Win }
          (True, True) ->
            if dealerTotal > playerTotal then
              { model | state = Loss }
            else if dealerTotal == 21 && dealerTotal == playerTotal then
              { model | state = Tie }
            else
              model
          (False, _) ->
            if dealerTotal > playerTotal then
              { model | state = Loss }
            else
              model
      _ ->
        Debug.todo "Something has gone wrong"
        

startGame : Model -> Model
startGame model =
  let
    checkFlipped card =
      case card of
        (_, "_back") -> True
        _            -> False
    (_, reshuffle) = 
      List.partition (\x -> checkFlipped x) (model.dealerHand ++ model.playerHand)
    newModel =
      { model
      | dealerHand = []
      , playerHand = []
      , deck = model.deck ++ reshuffle
      , playerTurn = True
      , state = Start
      }
    (newerModel, _) = (newModel, Random.generate ReplaceDeck newDeck)
  in
    case newerModel.deck of
      c1 :: c2 :: c3 :: cs ->
        { newerModel 
        | dealerHand = [("blue", "_back"), c2]
        , playerHand = [c1, c3]
        , deck = cs
        }
      _ ->
        {model | dealerHand = [("9", "D")], playerHand = [("9", "D")]}

playerHit : Model -> Model
playerHit model =
  case model.deck of
    [] ->
      model --This should never happen
    c :: cs ->
      { model | playerHand = model.playerHand ++ [c], deck = cs }

dealerHit : Model -> Model
dealerHit model =
  let
    (total, soft) = (getHandState model.dealerHand)
  in
    if (total < 17) || (soft && total - 10 < 17) then
      case model.deck of
        [] ->
          model --This should never happen
        c :: cs ->
          { model | dealerHand = model.dealerHand ++ [c], deck = cs }
    else
      model

shuffle : Model -> Cmd Msg
shuffle model =
  Random.generate ReplaceDeck (RL.shuffle model.deck)

getValue : Card -> Int
getValue c =
  case c of
    ("A", _)  -> 11
    ("2", _)  -> 2
    ("3", _)  -> 3
    ("4", _)  -> 4
    ("5", _)  -> 5
    ("6", _)  -> 6
    ("7", _)  -> 7
    ("8", _)  -> 8
    ("9", _)  -> 9
    ("10", _) -> 10
    ("J", _)  -> 10
    ("Q", _)  -> 10
    ("K", _)  -> 10
    _         -> 0


getHandState : List Card -> (Int, Bool)
getHandState cs =
  let
    (aces, rest) = List.partition (\x -> x > 10) (List.map (\x -> getValue x) cs)
    acesLen = List.length aces
    total = (List.sum rest) + acesLen
  in
    if (total + 10 <= 21 && acesLen > 0) then
      (total + 10, True)
    else
      (total, False)

--VIEW

view : Model -> Html Msg
view model =
  let
    buttonAttributes =
      [ Background.color (Element.rgb 255 255 205)
      , Border.width 5
      , Border.color (Element.rgb255 0 0 0)
      , Border.rounded 10
      , Element.padding 20
      ]

    hitButton =
      Input.button buttonAttributes
        { onPress =
            case (model.playerTurn, model.state) of
              (True, Start) ->
                Nothing
              (True, _) ->
                Just Hit
              _ ->
                Nothing
        , label = Element.text "Hit"
        }

    standButton =
      Input.button buttonAttributes
        { onPress =
            case (model.playerTurn, model.state) of
              (True, Start) ->
                Nothing
              (True, _) ->
                Just Stand
              _ ->
                Nothing
        , label = Element.text "Stand"
        }

    newGameButton =
      Input.button buttonAttributes
        { onPress =
            case model.state of
              Win ->
                Just NewGame
              Loss ->
                Just NewGame
              Tie ->
                Just NewGame
              Initial ->
                Just NewGame
              _ ->
                Nothing
        , label = Element.text "New Game"
        }

    resetButton =
      Input.button buttonAttributes
        { onPress = Just Reset
        , label = Element.text "Reset"
        }

    winCounter = 
      Element.el []
        (Element.text ("Wins: " ++ (String.fromInt model.winCount)))

    lossCounter =
      Element.el []
        (Element.text ("Losses: " ++ (String.fromInt model.lossCount)))

    tieCounter =
      Element.el []
        (Element.text ("Ties: " ++ (String.fromInt model.tieCount)))

    outcomeDisplay =
      let
        (color, outcomeText) =
          case model.state of
            Win  -> ((Element.rgb255 0 128 0), "You won!")
            Loss -> ((Element.rgb 1 0 0), "Sorry, you lost.")
            Tie  -> ((Element.rgb 0 0 1), "You and the dealer tied.")
            _    -> ((Element.rgb 0 0 0), " ")
      in
        Element.el 
          [ Font.color color
          , Element.centerX
          ]
          (Element.text outcomeText)

    winrateDisplay =
      let
        totalGames = model.winCount + model.lossCount + model.tieCount
      in
      Element.el []
        (Element.text ("Win Rate: " ++ (winrateCalc model.winCount totalGames)))

    dealerIndicator =
      let
        (dTotal, _) = getHandState model.dealerHand
      in
        Element.el []
          (Element.text ("Dealer Total: " ++ String.fromInt dTotal))

    playerIndicator =
      let
        (pTotal, _) = getHandState model.playerHand
      in
        Element.el []
          (Element.text ("Player Total: " ++ String.fromInt pTotal))
    
    dealerDisplay =
      let
        cardImages =  (List.map (\x -> getCardImage x) model.dealerHand)
      in
        Element.row
          [ Element.centerX, Element.centerY, Element.spacing 10 ]
          cardImages

    playerDisplay =
      let
        cardImages = (List.map (\x -> getCardImage x) model.playerHand)
      in
        Element.row
          [ Element.centerX, Element.centerY, Element.spacing 10 ]
          cardImages

  in
    Element.layout 
      [ Font.family
          [ Font.external
              { name = "Droid Sans"
              , url = "https://fonts.googleapis.com/css?family=Droid+Sans"
              }
          ]
      , Element.focused []
      ] <|
      Element.column
        [ Element.centerX, Element.centerY, Element.spacing 15 ]
        [ dealerIndicator
        , dealerDisplay
        , playerDisplay
        , playerIndicator
        , outcomeDisplay
        , Element.row
            [ Element.centerX, Element.centerY, Element.spacing 70]
            [ winCounter
            , lossCounter
            , tieCounter
            , winrateDisplay
            ]
        , Element.row
            [ Element.centerX, Element.centerY, Element.spacing 15 ]
            [ hitButton
            , standButton
            , newGameButton
            , resetButton
            ]

        ]

-- Images from http://acbl.mybigcommerce.com/52-playing-cards/
getCardImage : Card -> Element Msg
getCardImage (val, suit) =
  let
    desc = ((getValueName val) ++ " of " ++ (getSuitName suit))
    card = (val ++ suit ++ ".png")
  in
    Element.html
      (Html.div
        []
        [Html.img
          [ Attr.src ("/src/PNG/" ++ card)
          , Attr.alt desc
          , Attr.width 100
          ] 
          []
        ]
      )

getValueName : String -> String
getValueName v =
  case v of
    "A" -> "Ace"
    "J" -> "Jack"
    "Q" -> "Queen"
    "K" -> "King"
    _   -> v

getSuitName : String -> String
getSuitName s =
  case s of
    "D" -> "Diamonds"
    "C" -> "Clubs"
    "H" -> "Hearts"
    _   -> "Spades"

winrateCalc : Int -> Int -> String
winrateCalc gamesWon gamesPlayed =
  case (gamesPlayed == 0) of
    True ->
      "0%"
    _ ->
      let
        wins = toFloat gamesWon
        total = toFloat gamesPlayed
        winrate = round ((wins / total) * 100)
      in
        (String.fromInt winrate) ++ "%"


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
