module Calculator.View exposing (interpretCalculator, pressGonioNext, view)

{-
   Mimic the output panel for the electronic calculator.
-}

import Calculator.Model exposing (InputState(..), Model)
import Constants exposing (..)
import Element exposing (..)
import Element.Border as Border
import Element.Font as Font
import Grid exposing (GridPosition, gridLettersList, gridPosition, letterFromGrid)
import Messages exposing (Msg)
import Utils exposing (choose, disableSelection, edges, showExplanation)


withBorders widget =
    el
        [ Border.color lightCharcoal
        , Border.width 1
        ]
        widget


view : Device -> Model -> Element Msg
view device model =
    case device.orientation of
        Landscape ->
            calculatorLandscape model.explainMode model

        Portrait ->
            calculatorPortrait model.explainMode model


calculatorLandscape : Bool -> Model -> Element Msg
calculatorLandscape withExplanations model =
    let
        position =
            gridPosition model.storedAzimuthRange model.storedAzimuth
    in
    column
        [ centerX, centerY ]
        [ el [ height (px 100) ] none
        , row [ spacing 10, padding 5, centerX, centerY ]
            [ positionGridDisplay withExplanations position
            , column [ spacing 10, alignLeft, width fill ]
                [ withBorders <|
                    row
                        [ paddingEach { edges | left = 10, bottom = 10 }
                        , spacing 10
                        , width fill
                        ]
                        [ strengthDisplay withExplanations model.storedStrength
                        , maybeBoolDisplay "+" model.storedStrengthPlus
                        , maybeBoolDisplay "F" model.storedFriendly
                        , column []
                            [ none
                            , none
                            ]
                        ]
                , heightGrid withExplanations model.storedElevation
                ]
            , el [ width (px 40) ] none
            ]
        , row
            ([ Border.color lightCharcoal
             , Border.width 1
             , centerX
             ]
                ++ showExplanation withExplanations """The approximate position within the grid square"""
            )
            [ offsetDisplay <| Maybe.map .gridSquareOffsetEast position
            , offsetDisplay <| Maybe.map .gridSquareOffsetNorth position
            ]
        ]


calculatorPortrait : Bool -> Model -> Element Msg
calculatorPortrait withExplanations model =
    let
        position =
            gridPosition model.storedAzimuthRange model.storedAzimuth
    in
    column
        [ centerX
        , width fill
        ]
        [ row [ centerX ]
            [ positionGridDisplay withExplanations
                position
            ]
        , column
            (showExplanation withExplanations """The approximate position within the grid square"""
                ++ [ centerX ]
            )
            [ offsetDisplay <| Maybe.map .gridSquareOffsetEast position
            , offsetDisplay <| Maybe.map .gridSquareOffsetNorth position
            ]
        , row [ centerX ]
            [ strengthDisplay withExplanations model.storedStrength
            , maybeBoolDisplay "+" model.storedStrengthPlus
            , maybeBoolDisplay "F" model.storedFriendly
            ]
        , heightGrid withExplanations model.storedElevation
        ]


bulbSize =
    45


heightGridFontSize label =
    -- Slightly reduce font size for longer labels
    if String.length label > 3 then
        20

    else
        24


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


positionGridDisplay : Bool -> Maybe GridPosition -> Element Msg
positionGridDisplay withExplanations position =
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
    el (showExplanation withExplanations """The 100km map grid square containing the raid""") <|
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


strengthDisplay : Bool -> Maybe Int -> Element Msg
strengthDisplay withExplanations strength =
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
    el (showExplanation withExplanations """Estimate of number of planes in raid""") <|
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


heightGrid : Bool -> Maybe Float -> Element Msg
heightGrid withExplanations storedHeight =
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
                (Font.size (heightGridFontSize label)
                    :: spacingXY 0 0
                    :: padding 8
                    :: centerY
                    :: buttonStyle (theRightHeight low high) flatSunflower
                )
                [ text label ]
    in
    -- Height is in '000 feet, sourced from config data!
    el (width fill :: showExplanation withExplanations """The approximate height of the raid""") <|
        column
            [ padding 20
            , spacingXY 0 10
            , Border.color lightCharcoal
            , Border.width 1
            ]
            [ row [ spacing 20 ] <|
                List.map3
                    (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                    [ ".5-", ".5", "1.0", "1.5", "2.0", "2.5", "3.0", "3.5", "4.0", "4.5", "5.0", "5.5" ]
                    (List.range 0 11)
                    (List.range 1 12)
            , row [ spacing 20 ] <|
                List.map3
                    (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                    [ "6.0", "6.5", "7.0", "7.5", "8.0", "8.5", "9.0", "9.5", "10", "10.5", "11", "11.5" ]
                    (List.range 12 23)
                    (List.range 13 24)
            , row [ spacing 20 ] <|
                List.map3
                    (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                    [ "12", "12.5", "13", "13.5", "14", "14.5", "15", "15.5", "16", "16.5", "17", "17.5" ]
                    (List.range 24 35)
                    (List.range 25 36)
            , row [ spacing 20 ] <|
                List.map3
                    (\label low high -> lamp label (toFloat low * 500) (toFloat high * 500))
                    [ "18", "18.5", "19", "19.5", "20", "21", "22", "23", "24", "25", "26", "27" ]
                    [ 36, 37, 38, 39, 40, 42, 44, 46, 48, 50, 52, 54 ]
                    [ 37, 38, 39, 40, 42, 44, 46, 48, 50, 52, 54, 56 ]
            , row [ alignLeft, spacing 20 ] <|
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


interpretCalculator : Calculator.Model.Model -> String
interpretCalculator model =
    let
        gridPos =
            gridPosition model.storedAzimuthRange model.storedAzimuth

        letter =
            letterFromGrid gridPos

        twoDigits x =
            String.fromInt (x // 10) ++ String.fromInt (modBy 10 x)
    in
    "The raid grid position is "
        ++ (case ( gridPos, letter ) of
                ( Just grid, Just g ) ->
                    g
                        ++ twoDigits grid.gridSquareOffsetEast
                        ++ twoDigits grid.gridSquareOffsetNorth

                _ ->
                    "unknown"
           )
        ++ ", height "
        ++ (case model.storedElevation of
                Just h ->
                    if h >= 1 then
                        (String.fromInt <| truncate h)
                            ++ " thousand feet"

                    else
                        "below one thousand feet"

                Nothing ->
                    "unknown"
           )
        ++ ", strength "
        ++ (case model.storedStrengthPlus of
                Just True ->
                    "more than "

                _ ->
                    ""
           )
        ++ (case model.storedStrength of
                Just n ->
                    String.fromInt n

                _ ->
                    " unknown"
           )
        ++ (case model.storedFriendly of
                Just True ->
                    ", friendly."

                _ ->
                    "."
           )


pressGonioNext : Model -> Bool
pressGonioNext model =
    -- Support readouts that we need on the main receiver page.
    (model.inputState == BearingInput)
        || (model.inputState == HeightInput)
