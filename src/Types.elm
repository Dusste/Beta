module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Time
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , userChoices : UserChoices
    , userName : String
    , userState : UserState
    , startingCounterNumber : Int
    , players : List Player
    }


type alias Player =
    ( String, UserChoices )


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


type alias BackendModel =
    { players : List Player
    }



-- type Choices
--     = StoreRock
--     | StorePaper
--     | StoreScissors


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | ChooseSign UserChoices
    | StartGame
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
