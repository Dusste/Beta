module Backend exposing (..)

import Dict
import Evergreen.Migrate.V2 exposing (backendMsg, toFrontend)
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, sendToBackend, sendToFrontend)
import Process
import Task
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { players = Dict.empty, opponent = Man }
    , Cmd.none
    )


actionBasedOnOpponent : ToFrontend -> ClientId -> Opponent -> Cmd backendMsg
actionBasedOnOpponent action clientId opponent =
    case opponent of
        Man ->
            broadcast action

        Machine ->
            sendToFrontend clientId action


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SignalEnd ->
            ( model, broadcast SignalEndToFE )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UserJoined name opponent ->
            let
                modifiedPlayers =
                    Dict.insert sessionId ( name, Scissors ) model.players
            in
            ( { model | players = modifiedPlayers, opponent = opponent }
            , broadcast <| UserBecamePlayer modifiedPlayers
            )

        TimeIsUp ( userName, userChoices ) ->
            let
                modifiedPlayers =
                    Dict.insert sessionId ( userName, userChoices ) model.players
            in
            ( { model | players = modifiedPlayers }
            , Process.sleep 2000
                |> Task.perform (\_ -> SignalEnd)
            )

        AnnounceResults ->
            ( model, actionBasedOnOpponent (SendFinalResults model.players) clientId model.opponent )

        ResetBeModel ->
            ( { model | players = Dict.empty }
            , actionBasedOnOpponent ResetGame clientId model.opponent
            )

        SingnalPlayAgain ->
            ( model, broadcast <| BroadcastPlayAgain model.players )

        FetchCurrentUser ->
            -- Invited player needs to see player that have initiated game
            ( model, broadcast <| SendCurrentPlayer model.players )
