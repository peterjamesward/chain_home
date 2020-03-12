module Attr exposing (..)

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Types exposing (OperatorMode(..))
import Utils exposing (choose)


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


promptSymbol model =
    inFront <|
        choose (model.operatorMode == Training)
            (el [ Background.color blue, Font.color white ] <| text "?")
            none
