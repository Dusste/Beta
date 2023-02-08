module Evergreen.V2.Types exposing (..)

import Browser
import Browser.Navigation
import Time
import Url


type UserChoices
    = Scissors
    | Rock
    | Paper


type UserState
    = NewUser
    | GamePending
    | NotChoosen
    | ChoosingDone
    | TimerDone


type alias Player =
    ( String, UserChoices )


type Opponent
    = Man
    | Machine


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , userChoices : UserChoices
    , userName : String
    , userState : UserState
    , startingCounterNumber : Int
    , players : List Player
    , opponent : Opponent
    , randomInt : Int
    }


type alias BackendModel =
    { players : List Player
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ChooseSign UserChoices
    | StartGame
    | Tick Time.Posix
    | StoreName String
    | SendUserName String
    | InitiateReset
    | ChooseOpponent Opponent
    | TakeRandom Int


type ToBackend
    = UserJoined String Opponent Int
    | ShouldStartGame Int
    | TimeIsUp Player
    | ResetBeModel


type BackendMsg
    = NoOp


type ToFrontend
    = UserGreeting String
    | UserBecameClient Player
    | BeOrdersStart
    | UpdatePlayers (List Player)
    | RestGame
