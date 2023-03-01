module Backend exposing (..)

import Dict exposing (Dict)
import Evergreen.Migrate.V2 exposing (backendMsg, toFrontend)
import Lamdera exposing (ClientId, SessionId, onConnect, onDisconnect, sendToFrontend)
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
        , subscriptions = subscriptions
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { playersStructure = Dict.empty, roomId = 1, playersQueue = [], colorMode = Blue }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        SignalGameStart ->
            case model.playersQueue of
                [ _ ] ->
                    ( model, Cmd.none )

                [] ->
                    ( model, Cmd.none )

                ( clientId1, playerName1 ) :: ( clientId2, playerName2 ) :: rest ->
                    let
                        updatedPlayersDict =
                            model.playersStructure
                                |> Dict.insert model.roomId
                                    { player1 = PlayerBE clientId1 playerName1 Scissors
                                    , player2 = PlayerBE clientId2 playerName2 Scissors
                                    }

                        pairOfPlayers =
                            Dict.fromList
                                [ ( clientId1, ( model.roomId, playerName1, Scissors ) )
                                , ( clientId2, ( model.roomId, playerName2, Scissors ) )
                                ]
                    in
                    ( { model | playersQueue = rest, playersStructure = updatedPlayersDict, roomId = model.roomId + 1 }
                    , Cmd.batch
                        [ sendToFrontend clientId1 <| UserBecamePlayer pairOfPlayers True
                        , sendToFrontend clientId2 <| UserBecamePlayer pairOfPlayers False
                        , sendToFrontend clientId1 <| SendCurrentPlayer (Dict.fromList [ ( clientId1, ( model.roomId, playerName1, Scissors ) ) ]) -- need function for converting PlayerBE -> PlayerFE
                        ]
                    )

        CheckRoomPlayers _ clientId ->
            -- Don't allow user to join if there are already 2 players in room or none
            if List.length model.playersQueue == 1 then
                ( model, sendToFrontend clientId <| InitialFeData model.colorMode )

            else
                ( model
                , Cmd.batch
                    [ sendToFrontend clientId ResetOrOverGame
                    , sendToFrontend clientId <| InitialFeData model.colorMode
                    ]
                )

        InterruptGame _ clientId ->
            let
                playersRoom =
                    model.playersStructure
                        |> Dict.filter
                            (\_ { player1, player2 } ->
                                List.member clientId [ player1.id, player2.id ]
                            )
                        |> Dict.toList
            in
            case playersRoom of
                [] ->
                    ( model, Cmd.none )

                ( _, { player1, player2 } ) :: _ ->
                    ( model
                    , Cmd.batch
                        [ sendToFrontend player1.id ResetOrOverGame
                        , sendToFrontend player2.id ResetOrOverGame
                        ]
                    )

        NoOp ->
            ( model, Cmd.none )

        SignalEnd clientId ->
            ( model, sendToFrontend clientId SignalEndToFE )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend _ clientId msg model =
    case msg of
        UserJoined name ->
            let
                populatePlayersList =
                    ( clientId, name ) :: model.playersQueue

                playersCount =
                    populatePlayersList
                        |> List.length

                divisibleByTwo =
                    modBy playersCount 2 == 0
            in
            ( { model | playersQueue = populatePlayersList }
            , if divisibleByTwo then
                -- @TODO find a way to dispatch BE cmd without process and task
                Process.sleep 50
                    |> Task.perform (\_ -> SignalGameStart)

              else
                Process.sleep 50
                    |> Task.perform (\_ -> NoOp)
            )

        TimeIsUp ( roomId, _, userChoices ) ->
            let
                updatedPlayersDict =
                    model.playersStructure
                        |> Dict.update roomId
                            (\maybeRoomUnit ->
                                maybeRoomUnit
                                    |> Maybe.andThen
                                        (\roomUnit ->
                                            let
                                                { player1, player2 } =
                                                    roomUnit
                                            in
                                            if player1.id == clientId then
                                                let
                                                    updatedRecord1 =
                                                        { player1 | userChoice = userChoices }
                                                in
                                                Just { player1 = updatedRecord1, player2 = player2 }

                                            else if player2.id == clientId then
                                                let
                                                    updatedRecord2 =
                                                        { player2 | userChoice = userChoices }
                                                in
                                                Just { player1 = player1, player2 = updatedRecord2 }

                                            else
                                                Just roomUnit
                                        )
                            )
            in
            ( { model | playersStructure = updatedPlayersDict }
            , Process.sleep 2000
                |> Task.perform (\_ -> SignalEnd clientId)
            )

        AnnounceResults roomId ->
            let
                { player1, player2 } =
                    model.playersStructure
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultPlayersUnit

                fromPlayerBEtoPlayerFE : List PlayerBE -> Dict ClientId PlayerFE
                fromPlayerBEtoPlayerFE listPlayers =
                    listPlayers
                        |> List.foldl
                            (\{ id, playerName, userChoice } sum ->
                                Dict.insert id ( roomId, playerName, userChoice ) sum
                            )
                            Dict.empty
            in
            ( model, sendToFrontend clientId <| SendFinalResults (fromPlayerBEtoPlayerFE [ player1, player2 ]) )

        ResetBeModel roomId ->
            let
                { player1, player2 } =
                    model.playersStructure
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultPlayersUnit

                updatedPlayersDict =
                    model.playersStructure
                        |> Dict.remove roomId

                -- updatedPlayersDict =
                --     model.playersStructure
                --         |> Dict.update roomId
                --             (\maybeRoomUnit ->
                --                 maybeRoomUnit
                --                     |> Maybe.andThen
                --                         (\_ ->
                --                             Just defaultPlayersUnit
                --                         )
                --             )
            in
            ( { model | playersStructure = updatedPlayersDict, roomId = model.roomId - 1 }
            , Cmd.batch
                [ sendToFrontend player1.id ResetOrOverGame
                , sendToFrontend player2.id ResetOrOverGame
                ]
            )

        GameOverToBE roomId ->
            let
                { player1, player2 } =
                    model.playersStructure
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultPlayersUnit
            in
            ( model
            , Cmd.batch
                [ sendToFrontend player1.id ResetOrOverGame
                , sendToFrontend player2.id ResetOrOverGame
                ]
            )

        SignalPlayAgain roomId gameResult ->
            let
                { player1, player2 } =
                    model.playersStructure
                        |> Dict.get roomId
                        |> Maybe.withDefault defaultPlayersUnit
            in
            ( model
            , Cmd.batch
                [ sendToFrontend player1.id <| BroadcastPlayAgain gameResult
                , sendToFrontend player2.id <| BroadcastPlayAgain gameResult
                ]
            )

        StoreColorMode colorMode ->
            ( { model | colorMode = colorMode }, Cmd.none )


subscriptions : BackendModel -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ onConnect CheckRoomPlayers
        , onDisconnect InterruptGame
        ]
