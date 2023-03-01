module Helper exposing (..)

import Dict exposing (Dict)
import Element exposing (rgb255)
import Types exposing (..)


translations : Dict String Translation
translations =
    Dict.fromList
        [ ( "srb"
          , { heading = "Dobrodošao u igru - Kamen Papir Makaze"
            , playingAgainst = "Igram protiv"
            , opponentMan = "Čoveka"
            , opponentMachine = "Mašine"
            , addYourName = "Upiši svoje ime"
            , send = "Pošalji"
            , hello = "Zdravo"
            , friendInvited = "Pozvao te je drugar da odigrate igru"
            , copyLinkAndSend = ",kopiraj link i pošalji drugaru sa kojim želiš da igraš i sačekaj da dodje"
            , willPlayAgainsYou = "će igrati protiv tebe"
            , fourOfourText = "Izgleda da si se izgubio :( Nema ništa na ovoj strani"
            , pickYourSign = "izaberi svoj znak:"
            , backToBeginning = "Vrati se na početak"
            , timeLeft = "preostalo vreme u sekundama"
            , winnerAnnounceSoon = "Imaćemo pobednika uskoro ..."
            , rock = "Kamen"
            , paper = "Papir"
            , scissors = "Makaze"
            , seconds = "sekundi"
            , participant = "Učesnik"
            , havePicked = "je izabrao "
            , tieResult = "Nema pobednika, izabrali ste isti znak"
            , playAgain = "Igraj ponovo"
            , exitGame = "Izadji iz igre"
            , name = "Ime"
            , win = "Pobeda"
            , lose = "Poraz"
            , tie = "Nerešeno"
            , points = "Poeni"
            , theWinnerIs = "Pobednik je"
            , withChoice = "sa izborom"
            , congratulations = "Čestitamo !"
            , pickLanguage = "Izaberi jezik"
            , pickColorScheme = "Izaberi osnovne boje"
            , english = "Engleski"
            , serbian = "Srpski"
            , start = "Započni !"
            , light = "Svetla"
            , dark = "Tamna"
            , blue = "Plava"
            }
          )
        , ( "eng"
          , defaultTranslations
          )
        ]


colors : Dict String ColorSet
colors =
    Dict.fromList
        [ ( "light"
          , { background = rgb255 255 255 255
            , font = rgb255 0 0 0
            , specialFont = rgb255 76 175 80
            , buttonPrimary = rgb255 255 255 255
            , buttonPrimaryHover = rgb255 242 242 242
            , buttonSecondary = rgb255 255 255 255
            , buttonSecondaryHover = rgb255 242 242 242
            }
          )
        , ( "dark"
          , { background = rgb255 0 0 0
            , font = rgb255 255 255 255
            , specialFont = rgb255 255 255 1
            , buttonPrimary = rgb255 0 0 0
            , buttonPrimaryHover = rgb255 17 60 110
            , buttonSecondary = rgb255 0 0 0
            , buttonSecondaryHover = rgb255 17 60 110
            }
          )
        , ( "blue"
          , defaultColorSet
          )
        ]


defaultColorSet : ColorSet
defaultColorSet =
    { background = rgb255 1 150 324
    , font = rgb255 255 255 255
    , specialFont = rgb255 255 255 1
    , buttonPrimary = rgb255 17 75 123
    , buttonPrimaryHover = rgb255 17 60 110
    , buttonSecondary = rgb255 1 150 324
    , buttonSecondaryHover = rgb255 17 60 110
    }


defaultTranslations : Translation
defaultTranslations =
    { heading = "Welcome to the Rock, Paper, Scissors game"
    , playingAgainst = "Play against"
    , opponentMan = "Human"
    , opponentMachine = "Robot"
    , addYourName = "Write your name"
    , send = "Send"
    , hello = "Hello"
    , friendInvited = "Your friend wants to play with you"
    , copyLinkAndSend = ",copy the link and send it to friend you would like to play game with and wait for him/her"
    , willPlayAgainsYou = "will play against you"
    , fourOfourText = "You look puzzled :( Nothing on this page"
    , pickYourSign = "choose your sign:"
    , timeLeft = "time left in seconds"
    , winnerAnnounceSoon = "Winner will be announced soon ..."
    , backToBeginning = "Back to beginning"
    , rock = "Rock"
    , paper = "Paper"
    , scissors = "Scissors"
    , seconds = "second"
    , participant = "Participant"
    , havePicked = "has choosen"
    , tieResult = "It's a tie, you've chosen same sign"
    , playAgain = "Play again"
    , exitGame = "Exit"
    , name = "Name"
    , win = "Win"
    , lose = "Lose"
    , tie = "Tie"
    , points = "Points"
    , theWinnerIs = "Winner is"
    , withChoice = "with choice"
    , congratulations = "Congratulations !"
    , pickLanguage = "Language"
    , pickColorScheme = "Pick color mode"
    , english = "English"
    , serbian = "Serbian"
    , start = "Start !"
    , light = "Light"
    , dark = "Dark"
    , blue = "Blue"
    }


fromLanguageToString : Language -> String
fromLanguageToString lang =
    case lang of
        Eng ->
            "eng"

        Srb ->
            "srb"


fromColorToString : ColorMode -> String
fromColorToString color =
    case color of
        Dark ->
            "dark"

        Light ->
            "light"

        Blue ->
            "blue"


type alias ColorSet =
    { background : Element.Color
    , font : Element.Color
    , specialFont : Element.Color
    , buttonPrimary : Element.Color
    , buttonPrimaryHover : Element.Color
    , buttonSecondary : Element.Color
    , buttonSecondaryHover : Element.Color
    }


type alias Translation =
    { heading : String
    , playingAgainst : String
    , opponentMan : String
    , opponentMachine : String
    , addYourName : String
    , fourOfourText : String
    , send : String
    , hello : String
    , winnerAnnounceSoon : String
    , backToBeginning : String
    , friendInvited : String
    , copyLinkAndSend : String
    , willPlayAgainsYou : String
    , pickYourSign : String
    , timeLeft : String
    , rock : String
    , paper : String
    , scissors : String
    , seconds : String
    , participant : String
    , havePicked : String
    , tieResult : String
    , playAgain : String
    , exitGame : String
    , name : String
    , win : String
    , lose : String
    , tie : String
    , points : String
    , theWinnerIs : String
    , withChoice : String
    , congratulations : String
    , pickLanguage : String
    , english : String
    , serbian : String
    , start : String
    , pickColorScheme : String
    , light : String
    , dark : String
    , blue : String
    }


getTranslation : Language -> Dict String Translation -> Translation
getTranslation language translationsDict =
    translationsDict
        |> Dict.get (fromLanguageToString language)
        |> Maybe.withDefault defaultTranslations


getColorMode : ColorMode -> Dict String ColorSet -> ColorSet
getColorMode color colorsDict =
    colorsDict
        |> Dict.get (fromColorToString color)
        |> Maybe.withDefault defaultColorSet
