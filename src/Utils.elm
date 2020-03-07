module Utils exposing (..)

-- SVG requires a line to be expressed as a space separated string of pairs.

import Constants exposing (flatWetAsphalt, paletteSand)
import Element as E exposing (..)
import Element.Background as B
import Element.Border as Border exposing (..)
import Element.Font as Font exposing (..)
import Nixie exposing (nixieDisplay)
import String exposing (toInt)


stringifyPoint ( x, y ) =
    String.fromFloat x
        ++ ","
        ++ String.fromFloat y
        ++ " "


polyLineFromCoords coords =
    List.foldr (++) "" (List.map stringifyPoint coords)


choose b o1 o2 =
    if b then
        o1

    else
        o2


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


triangleWave t =
    toFloat (abs (abs (modBy 2000 t - 1000) - 1000)) / 1000.0


noise t =
    fractional (5000 * sin (toFloat t))


fractional x =
    x - toFloat (truncate x)


notNearlyEqual x1 x2 =
    (10 * abs x1 < abs x2) || (10 * abs x2 < abs x1)


numericDisplay label maybeValue =
    let
        value =
            truncate <| Maybe.withDefault 0.0 maybeValue
    in
    column commonStyles
        [ nixieDisplay 3 value
        , text label
        ]


bearingDisplay label maybeBearing =
    numericDisplay label <|
        case maybeBearing of
            Just m ->
                Just <| toFloat <| modBy 360 (truncate (m * 180 / pi))

            _ ->
                Nothing


commonStyles =
    [ E.width E.fill
    , E.centerX
    , E.spacingXY 20 0
    , E.paddingEach { top = 10, bottom = 10, left = 10, right = 10 }
    , Font.color paletteSand
    , B.color flatWetAsphalt
    , Font.size 14
    , Font.family
        [ Font.typeface "monospace"
        , Font.sansSerif
        ]
    ]
