module Backend exposing (..)

import Dict
import Evergreen.Migrate.V2 exposing (backendMsg, toFrontend)
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, sendToBackend, sendToFrontend)
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
    ( { players = Dict.empty, opponent = Machine }
    , Cmd.none
    )


actionBasedOnOpponent : ToFrontend -> String -> Opponent -> Cmd backendMsg
actionBasedOnOpponent action clientId opponent =
    case opponent of
        Man ->
            broadcast action

        Machine ->
            sendToFrontend clientId action


getRandomSignAndName : Int -> ( PlayerName, UserChoices )
getRandomSignAndName num =
    case num of
        1 ->
            ( "Bot22223127", Scissors )

        2 ->
            ( "Bot898dysag", Rock )

        3 ->
            ( "Bot113ds433", Paper )

        _ ->
            ( "Bot90090y3bf", Paper )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UserJoined name opponent randomInt ->
            case opponent of
                Machine ->
                    let
                        ( robotName, robotChoice ) =
                            getRandomSignAndName randomInt

                        usersBecamePlayers =
                            Dict.fromList
                                [ ( "123456789", ( robotName, robotChoice ) )
                                , ( sessionId, ( name, Scissors ) )
                                ]
                    in
                    ( { model
                        | players = usersBecamePlayers
                      }
                    , sendToFrontend clientId <| UserBecamePlayer usersBecamePlayers
                    )

                Man ->
                    let
                        modifiedPlayers =
                            Dict.insert sessionId ( name, Scissors ) model.players
                    in
                    ( { model | players = modifiedPlayers }
                    , broadcast <| UserBecamePlayer <| Dict.fromList [ ( sessionId, ( name, Scissors ) ) ]
                    )

        TimeIsUp ( userName, userChoices ) ->
            let
                -- @TODO should be update, not working properly
                modifiedPlayers =
                    Dict.insert sessionId ( userName, userChoices ) model.players
            in
            ( { model | players = modifiedPlayers }
            , actionBasedOnOpponent (UpdatePlayers modifiedPlayers) clientId model.opponent
            )

        ResetBeModel ->
            ( { model | players = Dict.empty }
            , actionBasedOnOpponent RestGame clientId model.opponent
            )

        SingnalPlayAgain ->
            ( model, broadcast <| BroadcastPlayAgain model.players )

        FetchCurrentUser ->
            -- Invited player needs to see player that have initiated game
            ( model, broadcast <| SendCurrentPlayer model.players )
