module SplashPage exposing (splashPage, splashText)

import Constants exposing (white)
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (class)
import Markdown exposing (defaultOptions)
import Messages exposing (Msg(..))


splashPage : Element Msg
splashPage =
    row [ width fill, moveUp 40 ]
        [ column [ width (fillPortion 3) ]
            [ image [ width (px 500)]
                { src = "img/receiver.png"
                , description = "The receiver panel"
                }
            , image [width (px 500)]
                { src = "img/calculator.png"
                , description = "The electronic calculator display"
                }
            , image [width (px 500)]
                { src = "img/map.png"
                , description = "Plots on a map"
                }
            ]
        , column [ width (fillPortion 5) ]
            [ paragraph
                [ Font.color white
                , padding 40
                , width fill
                ]
                [ html content ]
            ]
        ]


content : Html msg
content =
    Markdown.toHtmlWith { defaultOptions | smartypants = True }
        [ class "content" ]
        splashText

splashText =
        """
# Chain Home Receiver

## Demonstration mode

This seven minute loop covers basic training scenarios:

 - Single aircraft, including IFF

 - Two aircraft at same range, on same or different bearings

 - Three to six aircraft

 - Larger formations in different orientations
        """
