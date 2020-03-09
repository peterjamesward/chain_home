module CalculatorDisplay exposing (calculator)

{-
   We attempt to mimic the output panel for the electronic calculator.
-}

import Array
import Constants exposing (..)
import Element exposing (..)
import Element.Font as Font
import Messages exposing (Msg)
import Nixie exposing (maybeNixieDisplay, nixieDisplay)
import Utils exposing (bearingDisplay, choose, edges)


type alias GridPosition =
    { gridSquareEast : Int -- 0 = station, +ve east, -ve west
    , gridSquareNorth : Int -- 0 = station, +ve north, -ve south
    , gridSquareOffsetEast : Int -- Two digits within square
    , gridSquareOffsetNorth : Int -- Two digits within square
    }


gridLettersList : List (List String)
gridLettersList =
    [ [ "Y", "Z", "V", "W", "X", "Y", "Z" ]
    , [ "D", "E", "A", "B", "C", "D", "E" ]
    , [ "J", "K", "F", "G", "H", "J", "K" ]
    , [ "O", "P", "L", "M", "N", "O", "P" ]
    , [ "T", "U", "Q", "R", "S", "T", "U" ]
    , [ "Y", "Z", "V", "W", "X", "Y", "Z" ]
    , [ "D", "E", "A", "B", "C", "D", "E" ]
    ]


gridLettersArray =
    Array.fromList <|
        List.map Array.fromList gridLettersList


gridPosition : Maybe Float -> Maybe Float -> Maybe GridPosition
gridPosition range bearing =
    -- Ideally, use RAF grid, but doesn't really matter.
    -- Each lettered square is 100km.
    -- Without much loss of generality, assume station is centred in centre square.
    let
        easting r b =
            truncate <| r * sin b + 50

        northing r b =
            truncate <| r * cos b + 50

        eastSquareBoundary r b =
            floor <| toFloat (easting r b) / 100.0

        northSquareBoundary r b =
            floor <| toFloat (northing r b) / 100.0
    in
    case ( range, bearing ) of
        ( Just r, Just b ) ->
            Just
                { gridSquareEast = eastSquareBoundary r b
                , gridSquareNorth = northSquareBoundary r b
                , gridSquareOffsetEast = easting r b - 100 * eastSquareBoundary r b
                , gridSquareOffsetNorth = northing r b - 100 * northSquareBoundary r b
                }

        ( _, _ ) ->
            Nothing


calculator : Maybe Float -> Maybe Float -> Maybe Float -> Maybe Int -> Maybe Bool -> Element Msg
calculator range bearing height strength friendly =
    let
        position =
            gridPosition range bearing
    in
    column [ centerX, centerY ]
        [ row []
            [ positionGridDisplay position
            , column []
                [ row []
                    [ column [ paddingEach { edges | bottom = 15 } ]
                        [ strengthDisplay strength
                        , friendlyDisplay friendly
                        ]
                    , column []
                        [ none
                        , none
                        ]
                    ]
                , heightGrid height
                ]
            ]
        , row []
            [ offsetDisplay <| Maybe.map .gridSquareOffsetEast position
            , offsetDisplay <| Maybe.map .gridSquareOffsetNorth position
            ]
        ]


positionGridDisplay : Maybe GridPosition -> Element Msg
positionGridDisplay position =
    let
        displayGridSquare : Int -> Int -> String -> Element Msg
        displayGridSquare north east letter =
            el
                [ Font.color <|
                    case position of
                        Just p ->
                            if p.gridSquareEast == east && p.gridSquareNorth == north then
                                vividGreen

                            else
                                flatWetAsphalt

                        _ ->
                            flatWetAsphalt
                , Font.family
                    [ Font.typeface "Courier New"
                    , Font.sansSerif
                    ]
                , Font.size 28
                , Font.bold
                ]
                (text letter)

        displayGridRow : Int -> List String -> Element Msg
        displayGridRow north letters =
            row [ spacing 18 ] <|
                List.map2 (displayGridSquare north)
                    (List.range -3 3)
                    letters
    in
    column [ centerX, padding 10, spacingXY 20 10 ] <|
        List.map2 displayGridRow
            (List.reverse <| List.range -3 3)
            gridLettersList


strengthDisplay : Maybe Int -> Element Msg
strengthDisplay strength =
    let
        makeIt label v =
            el
                [ Font.color <|
                    case strength of
                        Just n ->
                            choose (n == v)
                                raidStrengthIndicator
                                flatWetAsphalt

                        _ ->
                            flatWetAsphalt
                , Font.family
                    [ Font.typeface "Courier New"
                    , Font.sansSerif
                    ]
                , Font.size 28
                , Font.bold
                ]
                (text label)
    in
    row [ spacing 20, paddingEach { edges | left = 30 } ] <|
        List.map2 makeIt
            [ "1", "2", "3", "6", "9", "12", "18", "+" ]
            [ 1, 2, 3, 6, 9, 12, 18, 20 ]


friendlyDisplay : Maybe Bool -> Element Msg
friendlyDisplay friendly =
    el
        [ Font.color <|
            case friendly of
                Just _ ->
                    raidStrengthIndicator

                _ ->
                    flatWetAsphalt
        , Font.family
            [ Font.typeface "Courier New"
            , Font.sansSerif
            ]
        , Font.size 30
        , Font.bold
        , centerX
        ]
        (text "F")


heightGrid : Maybe Float -> Element Msg
heightGrid height =
    let
        lamp label low high =
            el
                [ width (px 30)
                , Font.center
                , centerX
                , Font.variant Font.tabularNumbers
                , Font.color <|
                    case height of
                        Just h ->
                            if h * 1000 >= low && h * 1000 < high then
                                flatSunflower

                            else
                                flatWetAsphalt

                        _ ->
                            flatWetAsphalt
                , Font.family
                    [ Font.typeface "Courier New"
                    , Font.sansSerif
                    ]
                , Font.size 22
                , Font.bold
                ]
                (text label)
    in
    -- Height is in '000 feet, sourced from config data!
    column [ spacingXY 30 15 ]
        [ row [ width fill, spaceEvenly, spacing 30 ] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ ".5-", ".5", "1.0", "1.5", "2.0", "2.5", "3.0", "3.5", "4.0", "4.5", "5.0", " 5.5" ]
                (List.range 0 11)
                (List.range 1 12)
        , row [ width fill, spaceEvenly, spacing 30 ] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ "6.0", "6.5", "7.0", "7.5", "8.0", "8.5", "9.0", "9.5", " 10", "10.5", " 11", "11.5" ]
                (List.range 12 23)
                (List.range 13 24)
        , row [ width fill, spaceEvenly, spacing 30 ] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ " 12", "12.5", " 13", "13.5", " 14", "14.5", " 15", "15.5", " 16", "16.5", " 17", "17.5" ]
                (List.range 24 35)
                (List.range 25 36)
        , row [ width fill, spaceEvenly, spacing 30 ] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ " 18", "18.5", " 19", "19.5", " 20", " 21", " 22", " 23", " 24", " 25", " 26", " 27" ]
                [ 36, 37, 38, 39, 40, 42, 44, 46, 48, 50, 52, 54 ]
                [ 37, 38, 39, 40, 42, 44, 46, 48, 50, 52, 54, 56 ]
        , row [ width fill, spaceEvenly, alignLeft, spacing 30 ] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ " 28", " 29", " 30", "30+" ]
                [ 56, 58, 60, 62, 64 ]
                [ 58, 60, 62, 64, 999 ]
        ]


digitDisplayLamp : Maybe Int -> Int -> Element Msg
digitDisplayLamp maybeN digit =
    el
        [ width (px 30)
        , Font.center
        , centerX
        , Font.variant Font.tabularNumbers
        , Font.color <|
            case maybeN of
                Just n ->
                    if n == digit then
                        vividGreen

                    else
                        flatWetAsphalt

                _ ->
                    flatWetAsphalt
        , Font.family
            [ Font.typeface "Courier New"
            , Font.sansSerif
            ]
        , Font.size 28
        , Font.bold
        ]
        (text <| String.fromInt digit)


significantDigit : Maybe Int -> Element Msg
significantDigit maybeN =
    column []
        [ row [] <| List.map (digitDisplayLamp maybeN) [ 1, 2, 3, 4, 5 ]
        , row [] <| List.map (digitDisplayLamp maybeN) [ 6, 7, 8, 9, 0 ]
        ]


offsetDisplay : Maybe Int -> Element Msg
offsetDisplay easting =
    row []
        [ significantDigit <| Maybe.map ((//) 10) easting
        , significantDigit <| Maybe.map (modBy 10) easting
        ]
