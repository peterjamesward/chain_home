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
    row [ width fill ]
        [ column [ width (fillPortion 3) ]
            [ image []
                { src = ""
                , description = "Place holder for picture 1"
                }
            , image []
                { src = ""
                , description = "Place holder for picture 2"
                }
            , image []
                { src = ""
                , description = "Place holder for picture 3"
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

This looping demonstration covers basic training scenarios:

 - Single aircraft, including IFF

 - Two aircraft at same range, on same or different bearings

 - Three to six aircraft

 -  Larger formations in different orientations

The full application allows you to track multiple raids and check your accuracy.

        """
