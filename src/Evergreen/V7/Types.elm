module Evergreen.V7.Types exposing (..)

import Browser
import Browser.Dom
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


type Opponent
    = Man
    | Machine


type alias GameResult =
    { user : PlayerName
    , win : Int
    , lose : Int
    , tie : Int
    }


type Devices
    = DeviceMobile
    | DeviceTablet
    | DeviceSmallDesktop
    | DeviceDesktop


type Language
    = Eng
    | Srb


type ColorMode
    = Dark
    | Light
    | Blue


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
    , device : Devices
    , language : Language
    , openLanguageDropdown : Bool
    , openColorDropdown : Bool
    , colorMode : ColorMode
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
    , colorMode : ColorMode
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
    , device : Devices
    , language : Language
    , openLanguageDropdown : Bool
    , openColorDropdown : Bool
    , colorMode : ColorMode
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
    | PlayAgainMan FrontendModel (Dict.Dict Lamdera.ClientId GameResult)
    | PlayAgainMachine FrontendModel (Dict.Dict Lamdera.ClientId GameResult)
    | GotNewWidth Int
    | CheckDevice (Result String Browser.Dom.Viewport)
    | ChooseLanguage Language
    | OpenLanguageDropdown
    | OpenColorDropdown
    | ChooseDarkMode ColorMode


type ToBackend
    = UserJoined PlayerName
    | TimeIsUp PlayerFE
    | ResetBeModel RoomId
    | SignalPlayAgain RoomId (Dict.Dict Lamdera.ClientId GameResult)
    | AnnounceResults RoomId
    | GameOverToBE RoomId
    | StoreColorMode ColorMode


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
    | BroadcastPlayAgain (Dict.Dict Lamdera.ClientId GameResult)
    | SendFinalResults (Dict.Dict Lamdera.ClientId PlayerFE)
    | SignalEndToFE
    | InitialFeData ColorMode
