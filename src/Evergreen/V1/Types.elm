module Evergreen.V1.Types exposing (..)

import Browser
import Browser.Navigation
import Time
import Url


type UserChoice
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
    ( String, UserChoice )


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , userChoice : UserChoice
    , userName : String
    , userState : UserState
    , startingCounterNumber : Int
    , players : List Player
    }


type alias BackendModel =
    { players : List Player
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | StoreScissors
    | StartGame
    | StoreRock
    | StorePaper
    | Tick Time.Posix
    | StoreName String
    | SendUserName String
    | InitiateReset


type ToBackend
    = UserJoined String
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
