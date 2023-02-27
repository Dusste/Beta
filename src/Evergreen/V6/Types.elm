module Evergreen.V6.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type RoomParam
    = RoomParam Int


type Route
    = Room RoomParam
    | Home
    | Reset
    | GameOver
    | NotFound


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


type alias RoomId =
    Int


type alias PlayerName =
    String


type alias PlayerFE =
    ( RoomId, PlayerName, UserChoices )


type alias GameResult =
    { user : PlayerName
    , win : Int
    , lose : Int
    , tie : Int
    }


type Opponent
    = Man
    | Machine (Maybe GameResult)


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Route
    , userChoices : UserChoices
    , userName : String
    , gameStatus : GameStatus
    , startingCounterNumber : Int
    , players : Dict.Dict Lamdera.ClientId PlayerFE
    , opponent : Opponent
    , randomInt : Int
    , urlParamRandomNumber : Int
    , standings : Dict.Dict Lamdera.ClientId GameResult
    }


type alias PlayerBE =
    { id : Lamdera.ClientId
    , playerName : PlayerName
    , userChoice : UserChoices
    }


type alias RoomUnit =
    { player1 : PlayerBE
    , player2 : PlayerBE
    }


type alias BackendModel =
    { playersStructure : Dict.Dict RoomId RoomUnit
    , playersQueue : List ( Lamdera.ClientId, PlayerName )
    , roomId : Int
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Route
    , userChoices : UserChoices
    , userName : String
    , gameStatus : GameStatus
    , startingCounterNumber : Int
    , players : Dict.Dict Lamdera.ClientId PlayerFE
    , opponent : Opponent
    , randomInt : Int
    , urlParamRandomNumber : Int
    , standings : Dict.Dict Lamdera.ClientId GameResult
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
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
    | PlayAgainMachine FrontendModel (Dict.Dict Lamdera.ClientId GameResult)


type ToBackend
    = UserJoined PlayerName
    | TimeIsUp PlayerFE
    | ResetBeModel RoomId
    | SignalPlayAgain RoomId
    | AnnounceResults RoomId
    | GameOverToBE RoomId


type BackendMsg
    = SignalGameStart
    | SignalEnd Lamdera.ClientId
    | InterruptGame Lamdera.SessionId Lamdera.ClientId
    | CheckRoomPlayers Lamdera.SessionId Lamdera.ClientId
    | NoOp


type ToFrontend
    = UserBecamePlayer (Dict.Dict Lamdera.ClientId PlayerFE) Bool
    | ResetOrOverGame
    | SendCurrentPlayer (Dict.Dict Lamdera.ClientId PlayerFE)
    | BroadcastPlayAgain (Dict.Dict Lamdera.ClientId PlayerFE)
    | SendFinalResults (Dict.Dict Lamdera.ClientId PlayerFE)
    | SignalEndToFE
