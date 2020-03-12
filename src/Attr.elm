module Attr exposing (..)

import Constants exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onMouseEnter, onMouseLeave)
import Element.Font as Font
import Messages exposing (Msg(..))
import Types exposing (OperatorMode(..), Prompt)
import Utils exposing (choose, edges)


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


promptSymbol : Prompt -> Attribute Msg
promptSymbol prompt =
    onLeft <|
        (el
            [ Background.color blue
            , Font.color white
            , width (px 30)
            , height (px 30)
            , Border.rounded 15
            , Border.color white
            , Font.center
            , paddingEach { edges | top = 5 }
            , onMouseEnter (DisplayPrompt prompt)
            , onMouseLeave HidePrompt
            ]
         <|
            text "?"
        )
