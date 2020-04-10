module CalculatorDisplay exposing (calculator)

{-
   We attempt to mimic the output panel for the electronic calculator.
-}

import Constants exposing (..)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Grid exposing (GridPosition, gridLettersList, gridPosition)
import Messages exposing (Msg)
import Model exposing (Model)
import TrainingMode exposing (explanatoryText, tutorialTextBox)
import Types exposing (UiComponent(..))
import Utils exposing (choose, disableSelection, edges, helpButton)


withBorders widget =
    el
        [ Border.color lightCharcoal
        , Border.width 1
        ]
        widget


calculator : Model -> Element Msg
calculator model =
    case model.outputDevice.orientation of
        Landscape ->
            calculatorLandscape model

        Portrait ->
            calculatorPortrait model


calculatorLandscape : Model -> Element Msg
calculatorLandscape model =
    let
        position =
            gridPosition range bearing

        range =
            model.storedAzimuthRange

        bearing =
            model.storedAzimuth

        height =
            model.storedElevation

        strength =
            model.storedStrength

        plus =
            model.storedStrengthPlus

        friendly =
            model.storedFriendly
    in
    column
        [ centerX, centerY, moveDown 50 ]
        [ row [ spacing 10, padding 5, width fill ]
            [ positionGridDisplay model position
            , column [ spacing 10, alignLeft ]
                [ withBorders <|
                    row
                        [ paddingEach { edges | left = 10, bottom = 10 }
                        , spacing 10
                        , width fill
                        ]
                        [ strengthDisplay model strength
                        , maybeBoolDisplay "+" plus
                        , maybeBoolDisplay "F" friendly
                        , column []
                            [ none
                            , none
                            ]
                        ]
                , heightGrid model height
                ]
            , helpButton
            ]
        , el (explanatoryText model UiCalcOffset) <|
            row
                [ Border.color lightCharcoal
                , Border.width 1
                ]
                [ offsetDisplay <| Maybe.map .gridSquareOffsetEast position
                , offsetDisplay <| Maybe.map .gridSquareOffsetNorth position
                ]
        , row
            [ centerX
            , tutorialTextBox model [ centerX, moveDown 20 ]
            ]
            [ el [] none ]
        ]


calculatorPortrait : Model -> Element Msg
calculatorPortrait model =
    let
        position =
            gridPosition range bearing

        range =
            model.storedAzimuthRange

        bearing =
            model.storedAzimuth

        height =
            model.storedElevation

        strength =
            model.storedStrength

        plus =
            model.storedStrengthPlus

        friendly =
            model.storedFriendly
    in
    column
        ([ centerX
         , width fill
         , tutorialTextBox model
            [ alignTop
            , centerX
            ]
         ]
            ++ explanatoryText model UiCalculator
        )
        [ row [ centerX ]
            [ positionGridDisplay model
                position
            , helpButton
            ]
        , column
            (explanatoryText model UiCalcOffset ++ [ centerX ])
            [ offsetDisplay <| Maybe.map .gridSquareOffsetEast position
            , offsetDisplay <| Maybe.map .gridSquareOffsetNorth position
            ]
        , row [ centerX ]
            [ strengthDisplay model strength
            , maybeBoolDisplay "+" plus
            , maybeBoolDisplay "F" friendly
            ]
        , heightGrid model height
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
    ]
        ++ disableSelection


positionGridDisplay : Model -> Maybe GridPosition -> Element Msg
positionGridDisplay model position =
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
    el (explanatoryText model UiCalcGrid) <|
        column
            [ centerX
            , padding 10
            , spacingXY 5 5
            , Border.color lightCharcoal
            , Border.width 1
            ]
        <|
            List.map2 displayGridRow
                (List.reverse <| List.range -3 3)
                gridLettersList


strengthDisplay : Model -> Maybe Int -> Element Msg
strengthDisplay model strength =
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
    el (explanatoryText model UiCalcStrength) <|
        row
            [ spacing 20
            , paddingEach { edges | left = 10 }
            ]
        <|
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


heightGrid : Model -> Maybe Float -> Element Msg
heightGrid model storedHeight =
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
            paragraph
                (Font.size 24
                    :: spacingXY 10 0
                    :: buttonStyle (theRightHeight low high) flatSunflower
                )
                [ text label ]
    in
    -- Height is in '000 feet, sourced from config data!
    el (width fill :: explanatoryText model UiCalcHeight) <|
        column
            [ width fill
            , padding 20
            , centerX
            , Border.color lightCharcoal
            , Border.width 1
            ]
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
            , row [ alignLeft ] <|
                List.map3
                    (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                    [ "28", "29", "30", "30+" ]
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
    column
        [ Border.color lightCharcoal
        , Border.widthEach { edges | right = 1, left = 1 }
        , Border.dashed
        ]
        [ row [] <| List.map (digitDisplayLamp maybeN) [ 1, 2, 3, 4, 5 ]
        , row [] <| List.map (digitDisplayLamp maybeN) [ 6, 7, 8, 9, 0 ]
        ]


offsetDisplay : Maybe Int -> Element Msg
offsetDisplay offset =
    row
        [ padding 5
        , spacing 10
        ]
        [ significantDigit <| Maybe.map (\n -> n // 10) offset
        , significantDigit <| Maybe.map (modBy 10) offset
        ]
