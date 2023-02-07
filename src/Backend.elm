module Backend exposing (..)

import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, sendToBackend, sendToFrontend)
import Random
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
    ( { players = [] }
    , Cmd.none
    )


getRandomSignAndName : Int -> ( String, UserChoices )
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
                    ( { model
                        | players =
                            [ getRandomSignAndName randomInt ]
                      }
                    , Cmd.batch
                        [ broadcast <| UserBecameClient <| getRandomSignAndName randomInt
                        , broadcast <| UserBecameClient ( name, Scissors )
                        , sendToFrontend clientId <| UserGreeting <| name
                        ]
                    )

                Man ->
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
