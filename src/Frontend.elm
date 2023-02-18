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
    , opponent = Machine
    , randomInt = 1
    , route = NotRoom
    , urlParamRandomNumber = 1
    }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    case parseUrl url of
        Reset ->
            ( initialModel key, sendToBackend ResetBeModel )

        Room id ->
            let
                initM =
                    initialModel key
            in
            ( { initM
                | gameStatus = InvitedUser
                , randomInt = invitedPlayerSecretNumber
                , route = Room id
                , opponent = Man
              }
            , Cmd.none
            )

        NotRoom ->
            ( initialModel key
            , Cmd.batch
                [ Random.generate TakeRandom (Random.int 1 3)
                , Random.generate TakeRandomBigger (Random.int 911 1099)
                ]
            )


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
                    ( model, sendToBackend ResetBeModel )

                _ ->
                    ( model, Cmd.none )

        Tick _ ->
            let
                counterDone =
                    model.startingCounterNumber == 0
            in
            ( { model
                | startingCounterNumber =
                    if counterDone then
                        0

                    else
                        model.startingCounterNumber - 1
                , gameStatus =
                    if counterDone then
                        TimerDone

                    else
                        NotChoosen
              }
            , if counterDone then
                sendToBackend <| TimeIsUp ( model.userName, model.userChoices )

              else
                Cmd.none
            )

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
                            [ sendToBackend <| UserJoined name model.opponent model.randomInt
                            , if model.randomInt == invitedPlayerSecretNumber then
                                sendToBackend FetchCurrentUser

                              else
                                Nav.pushUrl model.key roomPath
                            ]
                    )

                Machine ->
                    ( { model
                        | gameStatus =
                            statusBasedOnValidation
                      }
                    , if notValidName then
                        Cmd.none

                      else
                        sendToBackend <| UserJoined name model.opponent model.randomInt
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
            ( { oldModel | gameStatus = NotChoosen }, sendToBackend SingnalPlayAgain )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        UserBecamePlayer onePlayer ->
            let
                players =
                    Dict.union model.players onePlayer

                shouldStart =
                    Dict.size players == 2
            in
            ( { model
                | players = players
                , gameStatus =
                    case model.opponent of
                        Machine ->
                            if shouldStart then
                                GamePending

                            else
                                NewUser

                        Man ->
                            if model.randomInt == invitedPlayerSecretNumber then
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

        UpdatePlayers players ->
            ( { model | players = players }, Cmd.none )

        RestGame ->
            ( { model
                | userChoices = Scissors
                , userName = ""
                , gameStatus = OpponentStep
                , startingCounterNumber = 5
                , players = Dict.empty
                , opponent = Machine
                , route = NotRoom
              }
            , Nav.pushUrl model.key "/"
            )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Element.layout [ Background.color <| rgb255 1 150 324, Font.color <| rgb255 255 255 255, paddingXY 0 180 ] <|
            Element.column [ centerX ]
                [ Element.paragraph [ Font.size <| Basics.round (scaled 5), paddingEach { top = 0, right = 0, left = 20, bottom = 30 } ] [ text "Dobrodošao u igru - Kamen Papir Makaze" ]
                , Element.column [ width fill, spacing 100, padding 40, center, Background.color <| rgba255 25 105 162 0.3, Border.rounded 3 ]
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
                                                , Input.optionWith Machine (radioOption (Element.text "Mašine"))
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
                                            (\( _, ( playerName, _ ) ) ->
                                                not <| String.contains playerName model.userName
                                            )
                                        |> List.map
                                            (\( _, ( name, _ ) ) ->
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
                                            (\( _, ( playerName, _ ) ) ->
                                                not <| String.contains playerName model.userName
                                            )
                                        |> List.map
                                            (\( _, ( name, _ ) ) ->
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

                        -- TimerDone & ChoosingDone
                        _ ->
                            Element.column [ centerX, Font.size <| Basics.round (scaled 3), spacing 20, center ]
                                [ Element.column [ width fill ]
                                    (model.players
                                        |> Dict.toList
                                        |> List.map
                                            (\( _, ( name, choice ) ) ->
                                                Element.paragraph [ padding 10 ] [ text <| "Učesnik " ++ name ++ " je izabrao " ++ choiceToString choice ]
                                            )
                                    )
                                , Element.paragraph [ center ]
                                    [ viewWinner
                                        model
                                    ]
                                ]
                    ]
                ]
        ]
    }
