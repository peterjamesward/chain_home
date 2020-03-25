module Utils exposing (..)

-- SVG requires a line to be expressed as a space separated string of pairs.

import Constants exposing (flatWetAsphalt, paletteDarkGreen, paletteSand, white)
import Element as E exposing (..)
import Element.Background as Background exposing (..)
import Element.Border as Border exposing (..)
import Element.Events exposing (onClick)
import Element.Font as Font exposing (..)
import Html.Attributes exposing (style)
import Messages exposing (Msg(..))


choose b o1 o2 =
    if b then
        o1

    else
        o2


removeFromList x xs =
    case xs of
        [] ->
            []

        a :: rest ->
            if a == x then
                rest

            else
                a :: removeFromList x rest


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


noise t =
    fractional (5000 * sin (toFloat t))


fractional x =
    x - toFloat (truncate x)


normalise theta =
    atan2 (sin theta) (cos theta)


disableSelection =
    [ htmlAttribute <| style "-webkit-user-select" "none"
    , htmlAttribute <| style "-webkit-touch-callout" "none"
    ]


commonStyles =
    [ E.width E.fill
    , E.centerX
    , E.spacingXY 20 0
    , E.paddingEach { top = 10, bottom = 10, left = 10, right = 10 }
    , Font.color paletteSand
    , Font.size 14
    , Font.family
        [ Font.typeface "monospace"
        , Font.sansSerif
        ]
    ]


helpButton : Element Msg
helpButton =
    el
        [ E.alignRight
        , E.alignTop
        , Border.color white
        , Border.width 1
        , Border.rounded 15
        , Background.color paletteDarkGreen
        , Font.color white
        , Font.size 12
        , Font.bold
        , Font.center
        , paddingEach { edges | top = 6 }
        , E.height (px 30)
        , E.width (px 30)
        , pointer
        , onClick ExplainModeToggle
        ]
    <|
        text "?"
