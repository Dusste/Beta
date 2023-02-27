module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Time
import Url exposing (Url)


type Route
    = Room RoomParam
    | Home
    | Reset
    | GameOver
    | NotFound


type RoomParam
    = RoomParam Int


type alias BackendModel =
    { playersStructure : Dict RoomId RoomUnit
    , playersQueue : List ( ClientId, PlayerName )
    , roomId : Int
    }


type alias FrontendModel =
    { key : Key
    , route : Route
    , userChoices : UserChoices
    , userName : String
    , gameStatus : GameStatus
    , startingCounterNumber : Int
    , players : Dict ClientId PlayerFE
    , opponent : Opponent
    , randomInt : Int
    , urlParamRandomNumber : Int
    , standings : Dict ClientId GameResult
    }


type alias RoomUnit =
    { player1 : PlayerBE
    , player2 : PlayerBE
    }


type alias PlayerFE =
    ( RoomId, PlayerName, UserChoices )


defaultPlayerFE : PlayerFE
defaultPlayerFE =
    ( 1, "", Scissors )


type alias PlayerBE =
    { id : ClientId
    , playerName : PlayerName
    , userChoice : UserChoices
    }


playerBE : PlayerBE
playerBE =
    PlayerBE "" "" Scissors


type alias RoomId =
    Int


type Opponent
    = Man
    | Machine (Maybe GameResult)


type alias GameResult =
    { user : PlayerName
    , win : Int
    , lose : Int
    , tie : Int
    }


defaultGameResult : GameResult
defaultGameResult =
    { user = ""
    , win = 0
    , lose = 0
    , tie = 0
    }


type alias DefaultPlayerBEUnit =
    { player1 : PlayerBE
    , player2 : PlayerBE
    }


defaultPlayersUnit : DefaultPlayerBEUnit
defaultPlayersUnit =
    { player1 =
        playerBE
    , player2 =
        playerBE
    }


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


type alias BotSessionId =
    String


botClientId : BotSessionId
botClientId =
    "123456789"


type alias DefaultClientId =
    String


defaultClientId : DefaultClientId
defaultClientId =
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
    | PlayAgainMachine FrontendModel (Dict ClientId GameResult)


type ToBackend
    = UserJoined PlayerName
    | TimeIsUp PlayerFE
    | ResetBeModel RoomId
    | SignalPlayAgain RoomId
    | AnnounceResults RoomId
    | GameOverToBE RoomId


type BackendMsg
    = SignalGameStart
    | SignalEnd ClientId
    | InterruptGame SessionId ClientId
    | CheckRoomPlayers SessionId ClientId
    | NoOp


type ToFrontend
    = UserBecamePlayer (Dict ClientId PlayerFE) Bool
    | ResetOrOverGame
    | SendCurrentPlayer (Dict ClientId PlayerFE)
    | BroadcastPlayAgain (Dict ClientId PlayerFE)
    | SendFinalResults (Dict ClientId PlayerFE)
    | SignalEndToFE
