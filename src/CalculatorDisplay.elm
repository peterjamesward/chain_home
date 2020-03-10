module CalculatorDisplay exposing (calculator)

{-
   We attempt to mimic the output panel for the electronic calculator.
-}

import Array
import Constants exposing (..)
import Element exposing (..)
import Element.Border as Border
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
    -- Each lettered square is 100km. Range is in km.
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


calculator : Maybe Float -> Maybe Float -> Maybe Float -> Maybe Int -> Maybe Bool -> Maybe Bool -> Element Msg
calculator range bearing height strength plus friendly =
    let
        position =
            gridPosition range bearing
    in
    column [ centerX, width fill ]
        [ row [ spacing 10, padding 5 ]
            [ positionGridDisplay position
            , column [ spacing 10 ]
                [ row [ paddingEach { edges | left = 10, bottom = 10 }, spacing 10 ]
                    [ strengthDisplay strength
                    , maybeBoolDisplay "+" plus
                    , maybeBoolDisplay "F" friendly
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


bulbSize =
    40


buttonStyle : Bool -> Element.Color -> List (Attribute Msg)
buttonStyle enabled colour =
    [ Font.family
        [ Font.typeface "Courier New"
        , Font.sansSerif
        ]
    , Font.center
    , Font.bold
    , Font.color <| choose enabled colour flatWetAsphalt
    , Font.glow colour <| choose enabled 1.0 0.0
    , Border.glow colour <| choose enabled 1.0 0.0
    , Border.innerGlow colour <| choose enabled 1.0 0.0
    , Border.rounded (bulbSize // 2)
    , width (px bulbSize)
    , height (px bulbSize)
    , paddingEach { edges | top = bulbSize // 4 }

    --, centerX
    ]


positionGridDisplay : Maybe GridPosition -> Element Msg
positionGridDisplay position =
    let
        thisIsTheSquare e n =
            case position of
                Just p ->
                    p.gridSquareEast == e && p.gridSquareNorth == n

                _ ->
                    False

        displayGridSquare : Int -> Int -> String -> Element Msg
        displayGridSquare north east letter =
            el
                (Font.size 28
                    :: buttonStyle (thisIsTheSquare east north) vividGreen
                )
                (text letter)

        displayGridRow : Int -> List String -> Element Msg
        displayGridRow north letters =
            row [ spacing 0 ] <|
                List.map2 (displayGridSquare north)
                    (List.range -3 3)
                    letters
    in
    column [ centerX, padding 10, spacingXY 5 5 ] <|
        List.map2 displayGridRow
            (List.reverse <| List.range -3 3)
            gridLettersList


strengthDisplay : Maybe Int -> Element Msg
strengthDisplay strength =
    let
        thisIsTheSquare n =
            case strength of
                Just s ->
                    s == n

                _ ->
                    False

        makeIt label v =
            el
                (Font.size 24
                    :: buttonStyle (thisIsTheSquare v) raidStrengthIndicator
                )
                (text label)
    in
    row [ spacing 20, paddingEach { edges | left = 10 } ] <|
        List.map2 makeIt
            [ "1", "2", "3", "6", "9", "12", "18" ]
            [ 1, 2, 3, 6, 9, 12, 18 ]


maybeBoolDisplay : String -> Maybe Bool -> Element Msg
maybeBoolDisplay label b =
    let
        theChosenOne =
            case b of
                Just _ ->
                    True

                _ ->
                    False
    in
    el
        (Font.size 24
            :: buttonStyle theChosenOne raidStrengthIndicator
        )
        (text label)


heightGrid : Maybe Float -> Element Msg
heightGrid storedHeight =
    let
        theRightHeight low high =
            case storedHeight of
                Just h ->
                    if h * 1000 >= low && h * 1000 < high then
                        True

                    else
                        False

                _ ->
                    False

        lamp label low high =
            el
                (Font.size 24
                    :: spacingXY 10 0
                    :: buttonStyle (theRightHeight low high) flatSunflower
                )
                (text label)
    in
    -- Height is in '000 feet, sourced from config data!
    column [ width fill ]
        [ row [ width fill, spaceEvenly ] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ ".5-", ".5", "1.0", "1.5", "2.0", "2.5", "3.0", "3.5", "4.0", "4.5", "5.0", "5.5" ]
                (List.range 0 11)
                (List.range 1 12)
        , row [ width fill, spaceEvenly ] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ "6.0", "6.5", "7.0", "7.5", "8.0", "8.5", "9.0", "9.5", "10", "10.5", "11", "11.5" ]
                (List.range 12 23)
                (List.range 13 24)
        , row [ width fill, spaceEvenly ] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5", "16", "16.5", "17", "17.5" ]
                (List.range 24 35)
                (List.range 25 36)
        , row [ width fill, spaceEvenly ] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ "18", "18.5", "19", "19.5", "20", "21", "22", "23", "24", "25", "26", "27" ]
                [ 36, 37, 38, 39, 40, 42, 44, 46, 48, 50, 52, 54 ]
                [ 37, 38, 39, 40, 42, 44, 46, 48, 50, 52, 54, 56 ]
        , row [] <|
            List.map3
                (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                [ " 28", " 29", " 30", "30+" ]
                [ 56, 58, 60, 62 ]
                [ 58, 60, 62, 9999 ]
        ]


digitDisplayLamp : Maybe Int -> Int -> Element Msg
digitDisplayLamp maybeN digit =
    let
        thisIsTheDigit =
            case maybeN of
                Just n ->
                    n == digit

                _ ->
                    False
    in
    el
        (Font.size 24
            :: buttonStyle thisIsTheDigit vividGreen
        )
        (text <| String.fromInt digit)


significantDigit : Maybe Int -> Element Msg
significantDigit maybeN =
    column []
        [ row [] <| List.map (digitDisplayLamp maybeN) [ 1, 2, 3, 4, 5 ]
        , row [] <| List.map (digitDisplayLamp maybeN) [ 6, 7, 8, 9, 0 ]
        ]


offsetDisplay : Maybe Int -> Element Msg
offsetDisplay offset =
    row [ padding 5, spacing 10 ]
        [ significantDigit <| Maybe.map (\n -> n // 10) offset
        , significantDigit <| Maybe.map (modBy 10) offset
        ]
