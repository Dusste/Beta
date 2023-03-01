module Frontend exposing (..)

import Ant.Icon
import Ant.Icons as Icons
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event
import Element.Font as Font exposing (center)
import Element.Input as Input
import Helper exposing (..)
import Lamdera exposing (sendToBackend)
import Process
import Random
import Task
import Time
import Types exposing (..)
import Url
import Util exposing (..)


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
                        Sub.batch [ Time.every 1000 Tick, Browser.Events.onResize (\w _ -> GotNewWidth w) ]

                    _ ->
                        Browser.Events.onResize (\w _ -> GotNewWidth w)
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
    , opponent = Machine
    , randomInt = 1
    , route = Home
    , urlParamRandomNumber = 1
    , standings = Dict.empty
    , device = DeviceDesktop
    , language = Eng
    , openLanguageDropdown = False
    , openColorDropdown = False
    , colorMode = Blue
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
            , Browser.Dom.getViewport |> Task.attempt (\x -> CheckDevice x)
            )

        Home ->
            ( initialModel key
            , Cmd.batch
                [ Random.generate TakeRandom (Random.int 1 3)
                , Random.generate TakeRandomBigger (Random.int 911 1099)
                , Browser.Dom.getViewport |> Task.attempt (\x -> CheckDevice x)
                ]
            )

        NotFound ->
            ( { initM | gameStatus = FourOFour }, Cmd.none )

        _ ->
            ( initM, Browser.Dom.getViewport |> Task.attempt (\x -> CheckDevice x) )


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
                    ( { model
                        | userName = ""
                        , route = Home
                        , gameStatus = OpponentStep
                        , startingCounterNumber = 5
                        , players = Dict.empty
                        , standings = Dict.empty
                      }
                    , case model.opponent of
                        Man ->
                            let
                                ( roomId, _, _ ) =
                                    -- Both players are in same room
                                    model.players
                                        |> Dict.toList
                                        |> List.head
                                        |> Maybe.withDefault ( defaultClientId, defaultPlayerFE )
                                        |> Tuple.second
                            in
                            Cmd.batch [ sendToBackend <| GameOverToBE roomId, Nav.reload ]

                        Machine ->
                            Nav.reload
                    )

                _ ->
                    ( model, Cmd.none )

        GotNewWidth width ->
            ( { model | device = fromWidthToDevice width }, Cmd.none )

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

                Machine ->
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

                Machine ->
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

        PlayAgainMan oldModel gameResults ->
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

                ( roomId, _, _ ) =
                    -- Both players are in same room
                    model.players
                        |> Dict.toList
                        |> List.head
                        |> Maybe.withDefault ( defaultClientId, defaultPlayerFE )
                        |> Tuple.second
            in
            ( { oldModel | gameStatus = NotChoosen, standings = gameResultSum }, sendToBackend <| SignalPlayAgain roomId gameResultSum )

        CheckDevice (Ok { scene }) ->
            let
                { width } =
                    scene
            in
            ( { model | device = fromWidthToDevice (round width) }, Cmd.none )

        CheckDevice (Err _) ->
            ( model, Cmd.none )

        ChooseLanguage lang ->
            ( { model | language = lang, openLanguageDropdown = not <| model.openLanguageDropdown }, Cmd.none )

        ChooseDarkMode colorMode ->
            ( { model | colorMode = colorMode, openColorDropdown = not <| model.openColorDropdown }, sendToBackend <| StoreColorMode colorMode )

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

        OpenLanguageDropdown ->
            ( { model | openLanguageDropdown = not <| model.openLanguageDropdown }, Cmd.none )

        OpenColorDropdown ->
            ( { model | openColorDropdown = not <| model.openColorDropdown }, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        InitialFeData colorMode ->
            ( { model | colorMode = colorMode }, Cmd.none )

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

        BroadcastPlayAgain gameResult ->
            ( { model | gameStatus = NotChoosen, startingCounterNumber = 5, standings = gameResult }, Cmd.none )

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
                , opponent = Machine
                , route = Home
              }
            , Nav.pushUrl model.key "/"
            )


viewColorMode : FrontendModel -> Translation -> ColorSet -> Element FrontendMsg
viewColorMode model translation colorScheme =
    Element.column
        [ padding 10, Font.size <| Basics.round (scaled 1), alignRight ]
        [ Input.button
            [ padding 4
            , spacing 0
            , centerX
            , if model.openColorDropdown then
                Background.color <| colorScheme.buttonSecondaryHover

              else
                Background.color <| colorScheme.buttonSecondary
            , Border.rounded 3
            , mouseOver <| [ Background.color <| colorScheme.buttonSecondaryHover ]
            ]
            { label =
                Element.paragraph []
                    [ text <| translation.pickColorScheme ++ " "
                    , Icons.downOutlined
                        [ Ant.Icon.width
                            12
                        , Ant.Icon.height 12
                        ]
                    ]
            , onPress = Just <| OpenColorDropdown
            }
        , if model.openColorDropdown then
            Element.column [ width fill, Background.color <| rgba255 25 105 162 0.3, spacingXY 0 4, Border.rounded 3, pointer ]
                [ Element.paragraph [ paddingXY 5 3, mouseOver <| [ Background.color <| colorScheme.buttonSecondaryHover ], Event.onClick <| ChooseDarkMode Light ]
                    [ text translation.light ]
                , Element.paragraph [ paddingXY 5 3, mouseOver <| [ Background.color <| colorScheme.buttonSecondaryHover ], Event.onClick <| ChooseDarkMode Dark ]
                    [ text translation.dark ]
                , Element.paragraph [ paddingXY 5 3, mouseOver <| [ Background.color <| colorScheme.buttonSecondaryHover ], Event.onClick <| ChooseDarkMode Blue ]
                    [ text translation.blue ]
                ]

          else
            text ""
        ]


viewLanguage : FrontendModel -> Translation -> ColorSet -> Element FrontendMsg
viewLanguage model translation colorScheme =
    Element.column
        [ padding 10, Font.size <| Basics.round (scaled 1) ]
        [ Input.button
            [ padding 4
            , spacing 0
            , centerX
            , if model.openLanguageDropdown then
                Background.color <| colorScheme.buttonSecondaryHover

              else
                Background.color <| colorScheme.buttonSecondary
            , Border.rounded 3
            , mouseOver <| [ Background.color <| colorScheme.buttonSecondaryHover ]
            ]
            { label =
                Element.paragraph []
                    [ text <| translation.pickLanguage ++ " "
                    , Icons.downOutlined
                        [ Ant.Icon.width
                            12
                        , Ant.Icon.height 12
                        ]
                    ]
            , onPress = Just <| OpenLanguageDropdown
            }
        , if model.openLanguageDropdown then
            Element.column [ width fill, Background.color <| rgba255 25 105 162 0.3, spacingXY 0 4, Border.rounded 3, pointer ]
                [ Element.paragraph [ paddingXY 5 3, mouseOver <| [ Background.color <| colorScheme.buttonSecondaryHover ], Event.onClick <| ChooseLanguage Eng ] [ text translation.english ]
                , Element.paragraph [ paddingXY 5 3, mouseOver <| [ Background.color <| colorScheme.buttonSecondaryHover ], Event.onClick <| ChooseLanguage Srb ] [ text translation.serbian ]
                ]

          else
            text ""
        ]


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ let
            translation =
                getTranslation model.language translations

            colorScheme =
                getColorMode model.colorMode colors
          in
          Element.layout [ Element.inFront <| viewLanguage model translation colorScheme, Element.inFront <| viewColorMode model translation colorScheme, Background.color <| colorScheme.background, Font.color <| colorScheme.font, paddingXY 0 180 ] <|
            Element.column [ centerX ]
                [ Element.wrappedRow []
                    [ Element.paragraph [ Font.size <| Basics.round (scaled 4), paddingEach { top = 0, right = 0, left = 20, bottom = 30 } ] [ text translation.heading ]
                    ]
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
                                            , label = Input.labelAbove [ Font.size <| Basics.round (scaled 4), center, Element.paddingXY 0 20 ] (text translation.playingAgainst)
                                            , selected = Just model.opponent
                                            , options =
                                                [ Input.optionWith Man (radioOption (Element.text translation.opponentMan) colorScheme)
                                                , Input.optionWith Machine (radioOption (Element.text translation.opponentMachine) colorScheme)
                                                ]
                                            }
                                        ]
                                    )
                                , Input.button
                                    [ padding 10
                                    , spacing 0
                                    , centerX
                                    , Background.color <| colorScheme.buttonPrimary
                                    , Border.rounded 3
                                    , mouseOver <| [ Background.color <| colorScheme.buttonPrimaryHover ]
                                    ]
                                    { label = Element.text translation.start
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
                                    [ text <| translation.friendInvited ]
                                , Input.text
                                    [ padding 10
                                    , spacing 30
                                    , Font.color <| rgb255 92 99 118
                                    ]
                                    { onChange = \a -> StoreName a
                                    , label = Input.labelAbove [ Font.size <| Basics.round (scaled 4), paddingXY 0 12 ] (text translation.addYourName)
                                    , text = model.userName
                                    , placeholder = Nothing
                                    }
                                , Input.button
                                    [ padding 10
                                    , spacing 0
                                    , centerX
                                    , Background.color <| colorScheme.buttonPrimary
                                    , Border.rounded 3
                                    , mouseOver <| [ Background.color <| colorScheme.buttonPrimaryHover ]
                                    ]
                                    { label = Element.text translation.send
                                    , onPress = Just <| SendUserName model.userName
                                    }
                                ]

                        InvitedPlayerGamePending ->
                            Element.column [ centerX, width <| fillPortion 500, center, spacing 50, Font.size <| Basics.round (scaled 4) ]
                                [ Element.paragraph []
                                    [ text <| translation.hello ++ " "
                                    , Element.el [ Font.color <| colorScheme.specialFont ] (text model.userName)
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
                                                Element.el [ center, Font.color <| colorScheme.specialFont ] (text name)
                                            )
                                    )
                                , Element.paragraph [ center ]
                                    [ text <|
                                        if Dict.size model.players == 2 then
                                            translation.willPlayAgainsYou

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
                                    , label = Input.labelAbove [ Font.size <| Basics.round (scaled 4), paddingXY 0 12 ] (text translation.addYourName)
                                    , text = model.userName
                                    , placeholder = Nothing
                                    }
                                , Input.button
                                    [ padding 10
                                    , spacing 0
                                    , centerX
                                    , Background.color <| colorScheme.buttonPrimary
                                    , Border.rounded 3
                                    , mouseOver <| [ Background.color <| colorScheme.buttonPrimaryHover ]
                                    ]
                                    { label = Element.text translation.send
                                    , onPress = Just <| SendUserName model.userName
                                    }
                                ]

                        GamePending ->
                            Element.column [ centerX, width <| fillPortion 500, center, spacing 50, Font.size <| Basics.round (scaled 4) ]
                                [ Element.paragraph []
                                    [ text <| translation.hello ++ " "
                                    , Element.el [ Font.color <| colorScheme.specialFont ] (text model.userName)
                                    , Element.paragraph []
                                        [ if model.opponent == Man then
                                            Element.el [ Font.color <| colorScheme.font ] (text translation.copyLinkAndSend)

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
                                                Element.el [ center, Font.color <| colorScheme.specialFont ] (text name)
                                            )
                                    )
                                , Element.paragraph [ center ]
                                    [ text <|
                                        if Dict.size model.players == 2 then
                                            translation.willPlayAgainsYou

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
                                        , label = Input.labelAbove [ Font.size <| Basics.round (scaled 3), center ] (text translation.pickYourSign)
                                        , selected = Just model.userChoices
                                        , options =
                                            [ Input.optionWith Rock (radioOption (Element.text translation.rock) colorScheme)
                                            , Input.optionWith Scissors (radioOption (Element.text translation.scissors) colorScheme)
                                            , Input.optionWith Paper (radioOption (Element.text translation.paper) colorScheme)
                                            ]
                                        }
                                    ]
                                , Element.paragraph [ center ] [ text <| "(" ++ translation.timeLeft ++ ": " ++ String.fromInt model.startingCounterNumber ++ ")" ]
                                ]

                        FourOFour ->
                            Element.column [ centerX, Font.size <| Basics.round (scaled 3), spacing 20, center ]
                                [ Element.paragraph [ center ]
                                    [ text translation.fourOfourText ]
                                , link
                                    [ padding 10
                                    , spacing 0
                                    , centerX
                                    , Background.color <| colorScheme.buttonPrimary
                                    , Border.rounded 3
                                    , Font.size <| Basics.round (scaled 3)
                                    , mouseOver <| [ Background.color <| colorScheme.buttonPrimaryHover ]
                                    ]
                                    { url = "/"
                                    , label = Element.text translation.backToBeginning
                                    }
                                ]

                        TimerDone ->
                            Element.column [ centerX, Font.size <| Basics.round (scaled 3), spacing 20, center ]
                                [ Element.paragraph [ center ]
                                    [ text translation.winnerAnnounceSoon
                                    ]
                                ]

                        PresentResults ->
                            Element.column [ centerX, Font.size <| Basics.round (scaled 3), spacing 20, center ]
                                [ Element.column [ width fill ]
                                    (model.players
                                        |> Dict.toList
                                        |> List.map
                                            (\( _, ( _, name, choice ) ) ->
                                                Element.paragraph [ padding 10 ] [ text <| translation.participant ++ " " ++ name ++ " " ++ translation.havePicked ++ " " ++ choiceToString translation choice ]
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
        calcPoints : Int -> Int -> String
        calcPoints win tie =
            (win * 2) + (tie * 1) |> String.fromInt

        winnerDict =
            -- Dict.fromList [ ( "123456789", { lose = 5, tie = 3, user = "Bot", win = 2 } ), ( "987654321", { lose = 0, tie = 5, user = "dsa", win = 7 } ) ]
            case determineWinner (Dict.toList model.players) translation of
                Just ( _, ( _, winnerName, _ ) ) ->
                    let
                        updatedStandings =
                            model.players
                                |> Dict.foldl
                                    (\currClientId ( _, playerName, _ ) sum ->
                                        if winnerName == playerName then
                                            Dict.insert currClientId { defaultGameResult | user = playerName, win = 1 } sum

                                        else
                                            Dict.insert currClientId { defaultGameResult | user = playerName, lose = 1 } sum
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
                                        Dict.insert currClientId { defaultGameResult | user = playerName, tie = 1 } sum
                                    )
                                    Dict.empty
                    in
                    updatedStandings

        translation =
            getTranslation model.language translations

        colorScheme =
            getColorMode model.colorMode colors

        sortedListResults =
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
    in
    Element.column [ width fill, centerX, paddingXY 0 30, Font.size <| Basics.round (scaled 4), spacing 30 ]
        [ Element.paragraph
            []
            [ case determineWinner (Dict.toList model.players) translation of
                Just ( _, ( _, winnerName, winnerChoice ) ) ->
                    text <|
                        translation.theWinnerIs
                            ++ " "
                            ++ winnerName
                            ++ " "
                            ++ translation.withChoice
                            ++ " "
                            ++ choiceToString translation winnerChoice
                            ++ "! "
                            ++ translation.congratulations

                Nothing ->
                    text translation.tieResult
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
                , Background.color <| colorScheme.buttonPrimary
                , Border.rounded 3
                , Font.size <| Basics.round (scaled 3)
                , mouseOver <| [ Background.color <| colorScheme.buttonPrimaryHover ]
                ]
                { onPress =
                    if model.opponent == Man then
                        Just <| PlayAgainMan model winnerDict

                    else
                        Just <| PlayAgainMachine model winnerDict
                , label = Element.text translation.playAgain
                }
            , link
                [ padding 10
                , spacing 0
                , centerX
                , Background.color <| colorScheme.buttonPrimary
                , Border.rounded 3
                , Font.size <| Basics.round (scaled 3)
                , mouseOver <| [ Background.color <| colorScheme.buttonPrimaryHover ]
                ]
                { url = "/over"
                , label = Element.text translation.exitGame
                }
            ]

        --  [ "Ime", "Pobeda", "Poraz", "Nerešeno", "Poeni" ]
        --                 |> List.map
        --                     (\s ->
        --                         Element.column [ width fill ] [ text <| s ]
        --                     )
        , Element.wrappedRow [ width fill, centerX, Font.size <| Basics.round (scaled 3) ]
            [ case model.device of
                DeviceMobile ->
                    Element.column [ width fill ]
                        [ Element.row [ width fill, paddingXY 20 10 ]
                            (sortedListResults
                                |> List.map
                                    (\r ->
                                        Element.column [ width fill ] [ text <| translation.name ++ ": " ++ r.user ]
                                    )
                            )
                        , Element.row [ width fill, paddingXY 20 10 ]
                            (sortedListResults
                                |> List.map
                                    (\r ->
                                        Element.column [ width fill ] [ text <| translation.win ++ ": " ++ String.fromInt r.win ]
                                    )
                            )
                        , Element.row [ width fill, paddingXY 20 10 ]
                            (sortedListResults
                                |> List.map
                                    (\r ->
                                        Element.column [ width fill ] [ text <| translation.lose ++ ": " ++ String.fromInt r.lose ]
                                    )
                            )
                        , Element.row [ width fill, paddingXY 20 10 ]
                            (sortedListResults
                                |> List.map
                                    (\r ->
                                        Element.column [ width fill ] [ text <| translation.tie ++ ": " ++ String.fromInt r.tie ]
                                    )
                            )
                        , Element.row [ width fill, paddingXY 20 10 ]
                            (sortedListResults
                                |> List.map
                                    (\r ->
                                        Element.column [ width fill ] [ text <| translation.points ++ ": " ++ calcPoints r.win r.tie ]
                                    )
                            )
                        ]

                _ ->
                    Element.table [ Border.width 1, Border.solid, Border.color <| colorScheme.font, spacingXY 0 10, padding 10 ]
                        { data =
                            sortedListResults
                        , columns =
                            [ { header = Element.el [ width fill, Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| colorScheme.font, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text translation.name)
                              , width = fill
                              , view =
                                    \person ->
                                        Element.text person.user
                              }
                            , { header = Element.el [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| colorScheme.font, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text translation.win)
                              , width = fill
                              , view =
                                    \person ->
                                        Element.text <| String.fromInt person.win
                              }
                            , { header = Element.el [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| colorScheme.font, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text translation.lose)
                              , width = fill
                              , view =
                                    \person ->
                                        Element.text <| String.fromInt person.lose
                              }
                            , { header = Element.el [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| colorScheme.font, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text translation.tie)
                              , width = fill
                              , view =
                                    \person ->
                                        Element.text <| String.fromInt person.tie
                              }
                            , { header = Element.el [ Border.widthEach { bottom = 1, left = 0, right = 0, top = 0 }, Border.solid, Border.color <| colorScheme.font, paddingEach { bottom = 3, left = 0, right = 0, top = 0 } ] (text translation.points)
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
