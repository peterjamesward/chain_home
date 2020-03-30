module Attr exposing (..)

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


greenButton : List (Attribute msg)
greenButton =
    [ Background.color flatMidnightBlue
    , Border.color paletteLightGreen
    , Border.rounded 5
    , Border.width 2
    , Font.color paletteLightGreen
    , paddingXY 20 6
    ]


greyButton : List (Attribute msg)
greyButton =
    [ Background.color paletteGrey
    , Border.color paletteDarkGreen
    , Border.rounded 5
    , Border.width 2
    , Font.color paletteDarkGreen
    ]
