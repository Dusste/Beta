module Helper exposing (..)

import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Lamdera exposing (SessionId)
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


parseUrl : Url -> Route
parseUrl url =
    case Parser.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotRoom


roomIdParser : Parser (RoomId -> a) a
roomIdParser =
    Parser.custom "ROOMID" <|
        \roomId ->
            Maybe.map RoomId (String.toInt roomId)


matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ Parser.map NotRoom Parser.top
        , Parser.map Room (Parser.s "room" </> roomIdParser)
        , Parser.map Reset (Parser.s "reset")
        ]


invitedPlayerSecretNumber : Int
invitedPlayerSecretNumber =
    99999


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


radioOption : Element msg -> Input.OptionState -> Element msg
radioOption optionLabel status =
    Element.wrappedRow
        [ Element.spacing 10
        , Element.width Element.shrink
        ]
        [ Element.el
            [ Element.width (Element.px 20)
            , Element.height (Element.px 20)
            , Border.width <|
                case status of
                    Input.Idle ->
                        2

                    Input.Focused ->
                        2

                    Input.Selected ->
                        10
            , Border.color <|
                case status of
                    Input.Idle ->
                        Element.rgb 1 1 1

                    Input.Focused ->
                        Element.rgb 1 1 1

                    Input.Selected ->
                        Element.rgb 1 1 1
            ]
            Element.none
        , Element.el [ Element.width Element.fill ] optionLabel
        ]


viewWinner : FrontendModel -> Element FrontendMsg
viewWinner model =
    Element.column [ width fill, centerX, padding 30, Font.size <| Basics.round (scaled 4), spacing 30 ]
        [ Element.paragraph
            []
            [ case determineWinner <| Dict.toList model.players of
                Just ( _, ( winnerName, winnerChoice ) ) ->
                    text <| "Pobednik je " ++ winnerName ++ " sa izborom " ++ choiceToString winnerChoice ++ "! Čestitamo !"

                Nothing ->
                    text "Nema pobednika, izabrali ste isti znak"
            ]
        , case model.opponent of
            Man ->
                Element.row [ width fill, centerX, padding 30, Font.size <| Basics.round (scaled 4), spacing 30 ]
                    [ Input.button
                        [ padding 10
                        , spacing 0
                        , centerX
                        , Background.color <| rgb255 17 75 123
                        , Border.rounded 3
                        , Font.size <| Basics.round (scaled 3)
                        , mouseOver <| [ Background.color <| rgb255 17 60 110 ]
                        ]
                        { onPress = Just <| PlayAgainMan model
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
                        { url = "/reset"
                        , label = Element.text "Resetuj Igru"
                        }
                    ]

            Machine ->
                link
                    [ padding 10
                    , spacing 0
                    , centerX
                    , Background.color <| rgb255 17 75 123
                    , Border.rounded 3
                    , Font.size <| Basics.round (scaled 3)
                    , mouseOver <| [ Background.color <| rgb255 17 60 110 ]
                    ]
                    { url = "/reset"
                    , label = Element.text "Resetuj igru"
                    }
        ]


determineWinner : List ( SessionId, Player ) -> Maybe ( SessionId, Player )
determineWinner players =
    let
        ( _, listPlayers ) =
            List.unzip players

        ( _, listChoices ) =
            List.unzip listPlayers

        isSameChoice =
            checkIfSameChoice listChoices choiceToString

        sortedPlayers =
            List.sortWith
                (\( _, ( _, choiceA ) ) ( _, ( _, choiceB ) ) ->
                    compareChoices ( choiceA, choiceB )
                )
                players
    in
    case sortedPlayers of
        winner :: _ ->
            if isSameChoice then
                Nothing

            else
                Just winner

        _ ->
            Nothing


checkIfSameChoice : List UserChoices -> (UserChoices -> String) -> Bool
checkIfSameChoice list toString =
    case list of
        firstChoice :: secondChoice :: _ ->
            toString firstChoice == toString secondChoice

        _ ->
            False


choiceToString : UserChoices -> String
choiceToString choice =
    case choice of
        Scissors ->
            "Makaze"

        Rock ->
            "Kamen"

        Paper ->
            "Papir"


compareChoices : ( UserChoices, UserChoices ) -> Order
compareChoices tup =
    case tup of
        ( Scissors, Rock ) ->
            GT

        ( Rock, Paper ) ->
            GT

        ( Scissors, Paper ) ->
            LT

        ( Rock, Scissors ) ->
            LT

        ( Paper, Rock ) ->
            LT

        ( Paper, Scissors ) ->
            GT

        ( Scissors, Scissors ) ->
            EQ

        ( Rock, Rock ) ->
            EQ

        ( Paper, Paper ) ->
            EQ
