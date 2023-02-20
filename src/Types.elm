module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type Route
    = Room RoomId
    | NotRoom
    | Reset
    | NotFound


type RoomId
    = RoomId Int


type alias FrontendModel =
    { key : Key
    , route : Route
    , userChoices : UserChoices
    , userName : String
    , gameStatus : GameStatus
    , startingCounterNumber : Int
    , players : Dict SessionId Player
    , opponent : Opponent
    , randomInt : Int
    , urlParamRandomNumber : Int
    }


type Opponent
    = Man
    | Machine


type alias Player =
    ( PlayerName, UserChoices )


type alias PlayerName =
    String


type UserChoices
    = Scissors
    | Rock
    | Paper


type GameStatus
    = OpponentStep
    | InvitedUser
    | NewUser
    | InvitedPlayerGamePending
    | GamePending
    | NotChoosen
    | TimerDone
    | PresentResults
    | FourOFour


type alias BackendModel =
    { players : Dict SessionId Player
    , opponent : Opponent
    }


type alias BotSessionId =
    String


botSessionId : BotSessionId
botSessionId =
    "123456789"


type alias DefaultSessionId =
    String


defaultSessionId : DefaultSessionId
defaultSessionId =
    "987654321"


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | TakeRandom Int
    | TakeRandomBigger Int
    | Tick Time.Posix
    | ChooseOpponent Opponent
    | SendOpponent Opponent
    | StoreName String
    | SendUserName String
    | StartGame
    | ChooseSign UserChoices
    | PlayAgainMan FrontendModel


type ToBackend
    = UserJoined PlayerName Opponent
    | TimeIsUp Player
    | ResetBeModel
    | FetchCurrentUser
    | SingnalPlayAgain
    | AnnounceResults


type BackendMsg
    = NoOp
    | SignalEnd


type ToFrontend
    = UserBecamePlayer (Dict SessionId Player)
    | ResetGame
    | SendCurrentPlayer (Dict SessionId Player)
    | BroadcastPlayAgain (Dict SessionId Player)
    | SendFinalResults (Dict SessionId Player)
    | SignalEndToFE
