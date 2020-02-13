module Attr exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Constants exposing (..)


greenButton : List (Attribute msg)
greenButton =
    [ Background.color flatMidnightBlue
    , Border.color paletteLightGreen
    , Border.rounded 3
    , Border.width 2
    , Font.bold
    , Font.color paletteSand
    , paddingXY 20 6
    ]


greyButton : List (Attribute msg)
greyButton =
    [ Background.color paletteGrey
    , Border.color paletteLightGreen
    , Border.rounded 3
    , Border.width 2
    , Font.bold
    , Font.color paletteDarkGreen
    ]
