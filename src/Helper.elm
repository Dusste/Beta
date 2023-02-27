module Helper exposing (..)

import Element exposing (..)
import Element.Border as Border
import Element.Input as Input
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


determineWinner : List ( ClientId, PlayerFE ) -> Maybe ( ClientId, PlayerFE )
determineWinner players =
    case players of
        ( _, ( _, _, userChoice1 ) ) :: ( _, ( _, _, userChoice2 ) ) :: _ ->
            let
                isSameChoice =
                    choiceToString userChoice1 == choiceToString userChoice2

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
