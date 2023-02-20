module Evergreen.V5.Types exposing (..)

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
    = UserBecamePlayer (Dict.Dict Lamdera.SessionId Player)
    | ResetGame
    | SendCurrentPlayer (Dict.Dict Lamdera.SessionId Player)
    | BroadcastPlayAgain (Dict.Dict Lamdera.SessionId Player)
    | SendFinalResults (Dict.Dict Lamdera.SessionId Player)
    | SignalEndToFE
