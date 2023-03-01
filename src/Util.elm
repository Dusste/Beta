module Util exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
import Helper exposing (..)
import Lamdera exposing (ClientId)
import Types exposing (..)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


parseUrl : Url -> Route
parseUrl url =
    case Parser.parse matchRoute url of
        Just route ->
            route

        Nothing ->
            NotFound


roomIdParser : Parser (RoomParam -> a) a
roomIdParser =
    Parser.custom "ROOMPARAM" <|
        \roomParam ->
            Maybe.map RoomParam (String.toInt roomParam)


matchRoute : Parser (Route -> a) a
matchRoute =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Room (Parser.s "room" </> roomIdParser)
        , Parser.map Reset (Parser.s "reset")
        , Parser.map GameOver (Parser.s "over")
        ]


invitedPlayerSecretNumber : Int
invitedPlayerSecretNumber =
    99999


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


fromWidthToDevice : Int -> Devices
fromWidthToDevice w =
    let
        isMobile =
            w >= 200 && w <= 450

        isTablet =
            w >= 451 && w <= 900

        isSmallDesktop =
            w >= 901 && w <= 1200
    in
    if isMobile then
        DeviceMobile

    else if isTablet then
        DeviceTablet

    else if isSmallDesktop then
        DeviceSmallDesktop

    else
        DeviceDesktop


radioOption : Element msg -> ColorSet -> Input.OptionState -> Element msg
radioOption optionLabel colorScheme status =
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
                        colorScheme.font

                    Input.Focused ->
                        colorScheme.font

                    Input.Selected ->
                        colorScheme.font
            ]
            Element.none
        , Element.el [ Element.width Element.fill ] optionLabel
        ]


determineWinner : List ( ClientId, PlayerFE ) -> Translation -> Maybe ( ClientId, PlayerFE )
determineWinner players translation =
    case players of
        ( _, ( _, _, userChoice1 ) ) :: ( _, ( _, _, userChoice2 ) ) :: _ ->
            let
                isSameChoice =
                    choiceToString translation userChoice1 == choiceToString translation userChoice2

                winnerFirst =
                    List.sortWith
                        (\( _, ( _, _, choiceA ) ) ( _, ( _, _, choiceB ) ) ->
                            compareChoices ( choiceA, choiceB )
                        )
                        players
            in
            case winnerFirst of
                winner :: _ ->
                    if isSameChoice then
                        Nothing

                    else
                        Just winner

                _ ->
                    Nothing

        _ ->
            Nothing


choiceToString : Translation -> UserChoices -> String
choiceToString translation choice =
    case choice of
        Scissors ->
            translation.scissors

        Rock ->
            translation.rock

        Paper ->
            translation.paper


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


getRandomSignAndName : Int -> UserChoices
getRandomSignAndName num =
    case num of
        1 ->
            Scissors

        2 ->
            Rock

        3 ->
            Paper

        _ ->
            Paper
