module Evergreen.V4.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type RoomId
    = RoomId Int


type Route
    = Room RoomId
    | NotRoom
    | Reset


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
    | ChoosingDone
    | TimerDone


type alias PlayerName =
    String


type alias Player =
    ( PlayerName, UserChoices )


type Opponent
    = Man
    | Machine


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Route
    , userChoices : UserChoices
    , userName : String
    , gameStatus : GameStatus
    , startingCounterNumber : Int
    , players : Dict.Dict Lamdera.SessionId Player
    , opponent : Opponent
    , randomInt : Int
    , urlParamRandomNumber : Int
    }


type alias BackendModel =
    { players : Dict.Dict Lamdera.SessionId Player
    , opponent : Opponent
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , route : Route
    , userChoices : UserChoices
    , userName : String
    , gameStatus : GameStatus
    , startingCounterNumber : Int
    , players : Dict.Dict Lamdera.SessionId Player
    , opponent : Opponent
    , randomInt : Int
    , urlParamRandomNumber : Int
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


type ToBackend
    = UserJoined String Opponent Int
    | TimeIsUp Player
    | ResetBeModel
    | FetchCurrentUser
    | SingnalPlayAgain


type BackendMsg
    = NoOp


type ToFrontend
    = UserBecamePlayer (Dict.Dict Lamdera.SessionId Player)
    | UpdatePlayers (Dict.Dict Lamdera.SessionId Player)
    | RestGame
    | SendCurrentPlayer (Dict.Dict Lamdera.SessionId Player)
    | BroadcastPlayAgain (Dict.Dict Lamdera.SessionId Player)
