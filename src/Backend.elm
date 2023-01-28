module Backend exposing (..)

import Html
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
        , subscriptions = \m -> Sub.none
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { players = [] }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        UserJoined name ->
            ( { model | players = [] }
            , Cmd.batch
                [ broadcast <| UserBecameClient ( name, Scissors )
                , sendToFrontend clientId <| UserGreeting <| name
                ]
            )

        ShouldStartGame numOfPlayers ->
            ( model
            , if numOfPlayers == 2 then
                broadcast <| BeOrdersStart

              else
                Cmd.none
            )

        TimeIsUp ( userName, userChoices ) ->
            ( { model | players = ( userName, userChoices ) :: model.players }
            , broadcast <|
                UpdatePlayers <|
                    ( userName, userChoices )
                        :: model.players
            )

        ResetBeModel ->
            ( { model | players = [] }, broadcast RestGame )
