module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input as Input
import Helper exposing (..)
import Lamdera exposing (sendToBackend)
import Process
import Random
import Tailwind.Utilities exposing (border)
import Task
import Time
import Types exposing (..)
import Url exposing (Url)


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions =
            \m ->
                case m.gameStatus of
                    NotChoosen ->
                        Time.every 1000 Tick

                    _ ->
                        Sub.none
        , view = view
        }


initialModel : Nav.Key -> FrontendModel
initialModel key =
    { key = key
    , userChoices = Scissors
    , userName = ""
    , gameStatus = OpponentStep
    , startingCounterNumber = 5
    , players = Dict.empty
    , opponent = Machine Nothing
    , randomInt = 1
    , route = Home
    , urlParamRandomNumber = 1
    , standings = Dict.empty
    }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        initM =
            initialModel key
    in
    case parseUrl url of
        Room (RoomParam roomId) ->
            ( { initM
                | gameStatus = InvitedUser
                , randomInt = 1
                , route = Room (RoomParam roomId)
                , opponent = Man
                , urlParamRandomNumber = roomId
              }
            , Cmd.none
            )

        Home ->
            ( initialModel key
            , Cmd.batch
                [ Random.generate TakeRandom (Random.int 1 3)
                , Random.generate TakeRandomBigger (Random.int 911 1099)
                ]
            )

        NotFound ->
            ( { initM | gameStatus = FourOFour }, Cmd.none )

        _ ->
            ( initM, Cmd.none )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            case parseUrl url of
                Reset ->
                    let
                        ( roomId, _, _ ) =
                            -- Both players are in same room
                            model.players
                                |> Dict.toList
                                |> List.head
                                |> Maybe.withDefault ( defaultClientId, defaultPlayerFE )
                                |> Tuple.second
                    in
                    ( model, sendToBackend <| ResetBeModel roomId )

                Home ->
                    ( { model | gameStatus = OpponentStep }, Cmd.none )

                GameOver ->
                    let
                        ( roomId, _, _ ) =
                            -- Both players are in same room
                            model.players
                                |> Dict.toList
                                |> List.head
                                |> Maybe.withDefault ( defaultClientId, defaultPlayerFE )
                                |> Tuple.second
                    in
                    ( { model
                        | gameStatus = OpponentStep
                        , startingCounterNumber = 5
                        , players = Dict.empty
                        , standings = Dict.empty
                      }
                    , sendToBackend <| GameOverToBE roomId
                    )

                _ ->
                    ( model, Cmd.none )

        Tick _ ->
            let
                counterDone =
                    model.startingCounterNumber == 0
            in
            case model.opponent of
                Man ->
                    if counterDone then
                        let
                            ( roomId, _, _ ) =
                                -- Both players are in same room
                                model.players
                                    |> Dict.toList
                                    |> List.head
                                    |> Maybe.withDefault ( defaultClientId, defaultPlayerFE )
                                    |> Tuple.second
                        in
                        ( { model
                            | startingCounterNumber = 0
                            , gameStatus = TimerDone
                          }
                        , sendToBackend <| TimeIsUp ( roomId, model.userName, model.userChoices )
                        )

                    else
                        ( { model | startingCounterNumber = model.startingCounterNumber - 1 }, Cmd.none )

                Machine _ ->
                    if counterDone then
                        let
                            updatedPlayers =
                                Dict.insert defaultClientId ( invitedPlayerSecretNumber, model.userName, model.userChoices ) model.players
                        in
                        ( { model
                            | startingCounterNumber = 0
                            , players = updatedPlayers
                            , gameStatus = PresentResults
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | startingCounterNumber = model.startingCounterNumber - 1 }, Cmd.none )

        StartGame ->
            ( { model | gameStatus = NotChoosen, startingCounterNumber = 5 }, Cmd.none )

        ChooseSign choice ->
            ( { model | userChoices = choice }, Cmd.none )

        StoreName name ->
            ( { model | userName = name }, Cmd.none )

        SendUserName name ->
            let
                notValidName =
                    String.isEmpty name

                statusBasedOnValidation =
                    if notValidName then
                        NewUser

                    else
                        GamePending
            in
            case model.opponent of
                Man ->
                    ( { model
                        | gameStatus =
                            statusBasedOnValidation
                      }
                    , let
                        roomPath =
                            "/room/" ++ String.fromInt model.urlParamRandomNumber
                      in
                      if notValidName then
                        Cmd.none

                      else
                        Cmd.batch
                            [ sendToBackend <| UserJoined name
                            , Nav.pushUrl model.key roomPath
                            ]
                    )

                Machine _ ->
                    let
                        robotChoice =
                            getRandomSignAndName model.randomInt

                        usersBecamePlayers =
                            Dict.fromList
                                [ ( botClientId, ( invitedPlayerSecretNumber, "Bot", robotChoice ) )
                                , ( defaultClientId, ( invitedPlayerSecretNumber, model.userName, Scissors ) )
                                ]
                    in
                    ( { model
                        | gameStatus =
                            statusBasedOnValidation
                        , players = usersBecamePlayers
                      }
                    , if notValidName then
                        Cmd.none

                      else
                        Process.sleep 4000
                            |> Task.perform (\_ -> StartGame)
                    )

        ChooseOpponent opponentChoice ->
            ( { model | opponent = opponentChoice }, Cmd.none )

        SendOpponent opponentChoice ->
            ( { model | opponent = opponentChoice, gameStatus = NewUser }, Cmd.none )

        TakeRandom num ->
            ( { model | randomInt = num }, Cmd.none )

        TakeRandomBigger num ->
            ( { model | urlParamRandomNumber = num }, Cmd.none )

        PlayAgainMan oldModel ->
            let
                ( roomId, _, _ ) =
                    -- Both players are in same room
                    model.players
                        |> Dict.toList
                        |> List.head
                        |> Maybe.withDefault ( defaultClientId, defaultPlayerFE )
                        |> Tuple.second
            in
            ( { oldModel | gameStatus = NotChoosen }, sendToBackend <| SignalPlayAgain roomId )

        PlayAgainMachine oldModel gameResults ->
            let
                gameResultSum =
                    Dict.merge
                        Dict.insert
                        (\clientId oldStats newStats acc ->
                            Dict.insert clientId
                                { user = oldStats.user
                                , win = oldStats.win + newStats.win
                                , lose = oldStats.lose + newStats.lose
                                , tie = oldStats.tie + newStats.tie
                                }
                                acc
                        )
                        Dict.insert
                        model.standings
                        gameResults
                        Dict.empty
            in
            ( { oldModel | gameStatus = NotChoosen, startingCounterNumber = 5, standings = gameResultSum }, Random.generate TakeRandom (Random.int 1 3) )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UserBecamePlayer player isInvited ->
            let
                players =
                    Dict.union player model.players

                shouldStart =
                    Dict.size players == 2
            in
            ( { model
                | players = players
                , gameStatus =
                    if isInvited then
                        InvitedPlayerGamePending

                    else
                        GamePending
              }
            , if shouldStart then
                Process.sleep 4000
                    |> Task.perform (\_ -> StartGame)

              else
                Cmd.none
            )

        BroadcastPlayAgain players ->
            ( { model | players = players, gameStatus = NotChoosen, startingCounterNumber = 5 }, Cmd.none )

        SendCurrentPlayer players ->
            ( { model | players = players, gameStatus = InvitedPlayerGamePending }, Cmd.none )

        SignalEndToFE ->
            let
                ( roomId, _, _ ) =
                    -- Both players are in same room
                    model.players
                        |> Dict.toList
                        |> List.head
                        |> Maybe.withDefault ( defaultClientId, defaultPlayerFE )
                        |> Tuple.second
            in
            ( model, sendToBackend <| AnnounceResults roomId )

        SendFinalResults players ->
            ( { model | players = players, gameStatus = PresentResults }, Cmd.none )

        ResetOrOverGame ->
            ( { model
                | userChoices = Scissors
                , userName = ""
                , gameStatus = OpponentStep
                , startingCounterNumber = 5
                , players = Dict.empty
                , opponent = Machine Nothing
                , route = Home
              }
            , Nav.pushUrl model.key "/"
            )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout [ Background.color <| rgb255 1 150 324, Font.color <| rgb255 255 255 255, paddingXY 0 180 ] <|
            Element.column [ centerX ]
                [ Element.wrappedRow [] [ Element.paragraph [ Font.size <| Basics.round (scaled 4), paddingEach { top = 0, right = 0, left = 20, bottom = 30 } ] [ text "Dobrodošao u igru - Kamen Papir Makaze" ] ]
                , Element.column [ width fill, spacing 50, paddingXY 0 20, center, Background.color <| rgba255 25 105 162 0.3, Border.rounded 3 ]
                    [ case model.gameStatus of
                        OpponentStep ->
                            Element.column
                                [ spacing 20
                                , centerX
                                ]
                                [ Element.el []
                                    (Element.wrappedRow []
                                        [ Input.radio
                                            [ spacing 30
                                            , width Element.fill
                                            , Font.size <| Basics.round (scaled 4)
                                            ]
                                            { onChange = ChooseOpponent
                                            , label = Input.labelAbove [ Font.size <| Basics.round (scaled 4), center, Element.paddingXY 0 20 ] (text "Igram protiv")
                                            , selected = Just model.opponent
                                            , options =
                                                [ Input.optionWith Man (radioOption (Element.text "Čoveka"))
                                                , Input.optionWith (Machine Nothing) (radioOption (Element.text "Mašine"))
                                                ]
                                            }
                                        ]
                                    )
                                , Input.button
                                    [ padding 10
                                    , spacing 0
                                    , centerX
                                    , Background.color <| rgb255 17 75 123
                                    , Border.rounded 3
                                    , mouseOver <| [ Background.color <| rgb255 17 60 110 ]
                                    ]
                                    { label = Element.text "Pošalji"
                                    , onPress = Just <| SendOpponent model.opponent
                                    }
                                ]

                        InvitedUser ->
                            Element.column
                                [ spacing 20
                                , centerX
                                ]
                                [ Element.paragraph
                                    [ Font.size <| Basics.round (scaled 4) ]
                                    [ text <| "Pozvao te je drugar da odigrate igru" ]
                                , Input.text
                                    [ padding 10
                                    , spacing 30
                                    , Font.color <| rgb255 92 99 118
                                    ]
                                    { onChange = \a -> StoreName a
                                    , label = Input.labelAbove [ Font.size <| Basics.round (scaled 4), paddingXY 0 12 ] (text "Upiši svoje ime")
                                    , text = model.userName
                                    , placeholder = Nothing
                                    }
                                , Input.button
                                    [ padding 10
                                    , spacing 0
                                    , centerX
                                    , Background.color <| rgb255 17 75 123
                                    , Border.rounded 3
                                    , mouseOver <| [ Background.color <| rgb255 17 60 110 ]
                                    ]
                                    { label = Element.text "Pošalji"
                                    , onPress = Just <| SendUserName model.userName
                                    }
                                ]

                        InvitedPlayerGamePending ->
                            Element.column [ centerX, width <| fillPortion 500, center, spacing 50, Font.size <| Basics.round (scaled 4) ]
                                [ Element.paragraph []
                                    [ text "Zdravo "
                                    , Element.el [ Font.color <| rgb255 255 255 1 ] (text model.userName)
                                    ]
                                , Element.paragraph []
                                    (model.players
                                        |> Dict.toList
                                        |> List.filter
                                            (\( _, ( _, playerName, _ ) ) ->
                                                not <| String.contains playerName model.userName
                                            )
                                        |> List.map
                                            (\( _, ( _, name, _ ) ) ->
                                                Element.el [ center, Font.color <| rgb255 255 255 1 ] (text name)
                                            )
                                    )
                                , Element.paragraph [ center ]
                                    [ text <|
                                        if Dict.size model.players == 2 then
                                            "će igrati protiv tebe"

                                        else
                                            ""
                                    ]
                                ]

                        NewUser ->
                            Element.column
                                [ spacing 20
                                , centerX
                                ]
                                [ Input.text
                                    [ padding 10
                                    , spacing 30
                                    , Font.color <| rgb255 92 99 118
                                    ]
                                    { onChange = \a -> StoreName a
                                    , label = Input.labelAbove [ Font.size <| Basics.round (scaled 4), paddingXY 0 12 ] (text "Upiši svoje ime")
                                    , text = model.userName
                                    , placeholder = Nothing
                                    }
                                , Input.button
                                    [ padding 10
                                    , spacing 0
                                    , centerX
                                    , Background.color <| rgb255 17 75 123
                                    , Border.rounded 3
                                    , mouseOver <| [ Background.color <| rgb255 17 60 110 ]
                                    ]
                                    { label = Element.text "Pošalji"
                                    , onPress = Just <| SendUserName model.userName
                                    }
                                ]

                        GamePending ->
                            Element.column [ centerX, width <| fillPortion 500, center, spacing 50, Font.size <| Basics.round (scaled 4) ]
                                [ Element.paragraph []
                                    [ text "Zdravo "
                                    , Element.el [ Font.color <| rgb255 255 255 1 ] (text model.userName)
                                    , Element.paragraph []
                                        [ if model.opponent == Man then
                                            Element.el [ Font.color <| rgb255 255 255 255 ] (text ",kopiraj link i pošalji drugaru sa kojim želiš da igraš i sačekaj da dodje")

                                          else
                                            text ""
                                        ]
                                    ]
                                , Element.paragraph []
                                    (model.players
                                        |> Dict.toList
                                        |> List.filter
                                            (\( _, ( _, playerName, _ ) ) ->
                                                not <| String.contains playerName model.userName
                                            )
                                        |> List.map
                                            (\( _, ( _, name, _ ) ) ->
                                                Element.el [ center, Font.color <| rgb255 255 255 1 ] (text name)
                                            )
                                    )
                                , Element.paragraph [ center ]
                                    [ text <|
                                        if Dict.size model.players == 2 then
                                            "će igrati protiv tebe"

                                        else
                                            ""
                                    ]
                                ]

                        NotChoosen ->
                            Element.column [ centerX, width fill, center, Font.size <| Basics.round (scaled 3) ]
                                [ Element.paragraph [ center ] [ text <| model.userName ++ "," ]
                                , Element.wrappedRow [ centerX, paddingEach { top = 10, right = 0, left = 0, bottom = 50 } ]
                                    [ Input.radio
                                        [ paddingXY 0 50
                                        , spacing 20
                                        , width Element.fill
                                        ]
                                        { onChange = ChooseSign
                                        , label = Input.labelAbove [ Font.size <| Basics.round (scaled 4), center ] (text "izaberi svoj znak:")
                                        , selected = Just model.userChoices
                                        , options =
                                            [ Input.optionWith Rock (radioOption (Element.text "Kamen"))
                                            , Input.optionWith Scissors (radioOption (Element.text "Makaze"))
                                            , Input.optionWith Paper (radioOption (Element.text "Papir"))
                                            ]
                                        }
                                    ]
                                , Element.paragraph [ center ] [ text <| "Imate još: " ++ String.fromInt model.startingCounterNumber ++ " sekundi" ]
                                ]

                        FourOFour ->
                            Element.column [ centerX, Font.size <| Basics.round (scaled 3), spacing 20, center ]
                                [ Element.paragraph [ center ]
                                    [ text "Izgleda da si se izgubio :( Nema ništa na ovoj strani" ]
                                , link
                                    [ padding 10
                                    , spacing 0
                                    , centerX
                                    , Background.color <| rgb255 17 75 123
                                    , Border.rounded 3
                                    , Font.size <| Basics.round (scaled 3)
                                    , mouseOver <| [ Background.color <| rgb255 17 60 110 ]
                                    ]
                                    { url = "/"
                                    , label = Element.text "Vrati se na početak"
                                    }
                                ]

                        TimerDone ->
                            Element.column [ centerX, Font.size <| Basics.round (scaled 3), spacing 20, center ]
                                [ Element.paragraph [ center ]
                                    [ text "Imaćemo pobednika uskoro ..."
                                    ]
                                ]

                        PresentResults ->
                            Element.column [ centerX, Font.size <| Basics.round (scaled 3), spacing 20, center ]
                                [ Element.column [ width fill ]
                                    (model.players
                                        |> Dict.toList
                                        |> List.map
                                            (\( _, ( _, name, choice ) ) ->
                                                Element.paragraph [ padding 10 ] [ text <| "Učesnik " ++ name ++ " je izabrao " ++ choiceToString choice ]
                                            )
                                    )
                                , Element.paragraph []
                                    [ viewWinner
                                        model
                                    ]
                                ]
                    ]
                ]
        ]
    }


viewWinner : FrontendModel -> Element FrontendMsg
viewWinner model =
    let
        -- Dict.fromList [ ( "123456789", { lose = 1, tie = 0, user = "Bot", win = 0 } ), ( "987654321", { lose = 0, tie = 0, user = "dsa", win = 1 } ) ]
        calcPoints : Int -> Int -> String
        calcPoints win tie =
            let
                point =
                    if win > 0 then
                        win + 1

                    else if tie == 1 then
                        tie

                    else
                        win
            in
            point |> String.fromInt

        winnerDict =
            case determineWinner <| Dict.toList model.players of
                Just ( _, ( _, winnerName, _ ) ) ->
                    let
                        updatedStandings =
                            model.players
                                |> Dict.foldl
                                    (\currClientId ( _, playerName, _ ) sum ->
                                        if winnerName == playerName then
                                            Dict.insert currClientId { user = playerName, win = 1, lose = 0, tie = 0 } sum

                                        else
                                            Dict.insert currClientId { user = playerName, win = 0, lose = 1, tie = 0 } sum
                                    )
                                    Dict.empty
                    in
                    updatedStandings

                Nothing ->
                    let
                        updatedStandings =
                            model.players
                                |> Dict.foldl
                                    (\currClientId ( _, playerName, _ ) sum ->
                                        Dict.insert currClientId { user = playerName, win = 0, lose = 0, tie = 1 } sum
                                    )
                                    Dict.empty
                    in
                    updatedStandings
    in
    Element.column [ width fill, centerX, paddingXY 0 30, Font.size <| Basics.round (scaled 4), spacing 30 ]
        [ Element.paragraph
            []
            [ case determineWinner <| Dict.toList model.players of
                Just ( _, ( _, winnerName, winnerChoice ) ) ->
                    text <| "Pobednik je " ++ winnerName ++ " sa izborom " ++ choiceToString winnerChoice ++ "! Čestitamo !"

                Nothing ->
                    text "Nema pobednika, izabrali ste isti znak"
            ]

        -- @TODO reset game should be approved by second participant
        -- , link
        --     [ padding 10
        --     , spacing 0
        --     , centerX
        --     , Background.color <| rgb255 17 75 123
        --     , Border.rounded 3
        --     , Font.size <| Basics.round (scaled 3)
        --     , mouseOver <| [ Background.color <| rgb255 17 60 110 ]
        --     ]
        --     { url = "/reset"
        --     , label = Element.text "Resetuj Igru"
        --     }
        , Element.wrappedRow [ width fill, centerX, padding 30, Font.size <| Basics.round (scaled 4), spacing 30 ]
            [ Input.button
                [ padding 10
                , spacing 0
                , centerX
                , Background.color <| rgb255 17 75 123
                , Border.rounded 3
                , Font.size <| Basics.round (scaled 3)
                , mouseOver <| [ Background.color <| rgb255 17 60 110 ]
                ]
                { onPress =
                    if model.opponent == Man then
                        Just <| PlayAgainMan model

                    else
                        Just <| PlayAgainMachine model winnerDict
                , label = Element.text "Igraj ponovo"
                }
            , link
                [ padding 10
                , spacing 0
                , centerX
                , Background.color <| rgb255 17 75 123
                , Border.rounded 3
                , Font.size <| Basics.round (scaled 3)
                , mouseOver <| [ Background.color <| rgb255 17 60 110 ]
                ]
                { url = "/over"
                , label = Element.text "Izadji iz igre"
                }
            ]
        , Element.wrappedRow [ width fill, centerX, Font.size <| Basics.round (scaled 3) ]
            [ Element.table [ Border.width 1, Border.solid, Border.color <| rgb255 255 255 255, spacingXY 0 10, padding 10 ]
                { data =
                    Dict.merge
                        Dict.insert
                        (\clientId oldStats newStats acc ->
                            Dict.insert clientId
                                { user = oldStats.user
                                , win = oldStats.win + newStats.win
                                , lose = oldStats.lose + newStats.lose
                                , tie = oldStats.tie + newStats.tie
                                }
                                acc
                        )
                        Dict.insert
                        model.standings
                        winnerDict
                        Dict.empty
                        |> Dict.toList
                        |> List.map Tuple.second
                        |> List.sortBy .lose
                , columns =
                    [ { header = Element.el [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| rgb255 255 255 255, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text "Ime")
                      , width = fill
                      , view =
                            \person ->
                                Element.text person.user
                      }
                    , { header = Element.el [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| rgb255 255 255 255, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text "Pobeda")
                      , width = fill
                      , view =
                            \person ->
                                Element.text <| String.fromInt person.win
                      }
                    , { header = Element.el [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| rgb255 255 255 255, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text "Poraz")
                      , width = fill
                      , view =
                            \person ->
                                Element.text <| String.fromInt person.lose
                      }
                    , { header = Element.el [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| rgb255 255 255 255, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text "Nerešeno")
                      , width = fill
                      , view =
                            \person ->
                                Element.text <| String.fromInt person.tie
                      }
                    , { header = Element.el [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| rgb255 255 255 255, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text "Poeni")
                      , width = fill
                      , view =
                            \person ->
                                Element.text <|
                                    calcPoints person.win person.tie
                      }
                    ]
                }
            ]
        ]
