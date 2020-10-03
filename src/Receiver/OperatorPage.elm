module Receiver.OperatorPage exposing (operatorPage, operatorPageWithTutorial)

import CRT_WebGL exposing (crt)
import Calculator.View exposing (pressGonioNext)
import Config exposing (groundRays)
import Constants exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Goniometer exposing (drawGoniometer)
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Messages exposing (..)
import Model exposing (ApplicationMode(..), Model)
import PushButtons exposing (..)
import Range exposing (drawRangeKnob)
import Svg exposing (polygon, svg)
import Svg.Attributes as S exposing (fontFamily, fontSize, points, stroke, strokeWidth, textAnchor, viewBox, x, x1, x2, y, y1, y2)
import Tutorials.Tutorials exposing (findTutorialSubject, highlightTutorialSubject)
import Tutorials.Views exposing (tutorialControls, tutorialText)
import Types exposing (..)
import Utils exposing (commonStyles, disableSelection, edges, helpButton, showExplanation)


clickableRangeKnob tutorialSubject model =
    el
        ([ htmlAttribute <| Pointer.onDown (\event -> RangeGrab event.pointer.offsetPos)
         , htmlAttribute <| Pointer.onMove (\event -> RangeMove event.pointer.offsetPos)
         , htmlAttribute <| Pointer.onUp (\event -> RangeRelease event.pointer.offsetPos)
         , htmlAttribute <| style "touch-action" "none"
         , width (px 200)
         , pointer
         ]
            ++ highlightTutorialSubject tutorialSubject UiRangeKnob
            ++ showExplanation model.explainModeReceiver """Range knob"""
        )
        (html <| drawRangeKnob model.rangeKnobAngle)


clickableGonioImage tutorialSubject model =
    let
        theta =
            model.goniometerAzimuth + model.station.lineOfShoot
    in
    el
        ([ htmlAttribute <| Pointer.onDown (\event -> GonioGrab event.pointer.offsetPos)
         , htmlAttribute <| Pointer.onMove (\event -> GonioMove event.pointer.offsetPos)
         , htmlAttribute <| Pointer.onUp (\event -> GonioRelease event.pointer.offsetPos)
         , htmlAttribute <| style "touch-action" "none"
         , width fill
         , pointer
         ]
            ++ highlightTutorialSubject tutorialSubject UiGoniometer
            ++ showExplanation model.explainModeReceiver """Goniometer"""
        )
        (html <| drawGoniometer theta)


rangeSlider model =
    Input.slider
        [ E.height (E.px 30)
        , E.width (E.px 610)

        -- Here is where we're creating/styling the "track"
        , E.behindContent
            (E.el
                [ E.width E.fill
                , E.height (E.px 2)
                , E.centerY
                , E.centerX
                , Background.color black
                , Border.rounded 2
                ]
                E.none
            )
        ]
        { onChange = AdjustRangeValue
        , label =
            Input.labelHidden "Range (miles)"
        , min = 0
        , max = 100.0
        , step = Nothing
        , value = model.rangeSlider
        , thumb =
            Input.thumb
                [ width (px 20)
                , height (px 80)
                , Border.width 0
                , Border.color white
                , inFront <|
                    html <|
                        svg
                            [ viewBox "0 0 16 50"
                            , S.width "100%"
                            , S.height "100%"
                            ]
                            [ polygon
                                [ points "2,20 14,20 14,35 8,48 2,35 2,20"
                                , stroke "honeydew"
                                , strokeWidth "1"
                                , S.fill "indianred"
                                ]
                                []
                            ]
                ]
        }


traceDependingOnMode : Model -> List Echo
traceDependingOnMode model =
    groundRays
        ++ (case ( model.goniometerMode, model.receiveAB ) of
                ( Azimuth, _ ) ->
                    model.azimuthModeTrace

                ( Elevation, True ) ->
                    model.elevation_A_trace

                ( Elevation, False ) ->
                    model.elevation_B_trace
           )


rangeTicks =
    let
        tickSize i =
            case ( modBy 10 i, modBy 5 i ) of
                ( 0, 0 ) ->
                    40

                ( _, 0 ) ->
                    20

                _ ->
                    10

        label i =
            if modBy 10 i == 0 then
                [ Svg.text_
                    [ x (String.fromFloat <| min 1950 <| max 15 (toFloat i * 19.7))
                    , y "100"
                    , S.fill "green"
                    , textAnchor "middle"
                    , fontFamily "monospace"
                    , fontSize "60"
                    ]
                    [ Svg.text <| String.fromInt i
                    ]
                ]

            else
                []

        tick i =
            [ Svg.line
                [ x1 <| String.fromInt (i * 20)
                , y1 "0"
                , x2 <| String.fromInt (i * 20)
                , y2 <| String.fromInt <| tickSize i
                , stroke "green"
                , strokeWidth "8"
                ]
                []
            ]
                ++ label i
    in
    svg
        [ viewBox "-5 -10 2010 200"
        , S.width "600px"
        , S.height "100px"
        ]
    <|
        List.concatMap tick (List.range 0 100)


rangeScale model =
    el
        ([ width (px 600)
         , E.above (rangeSlider model)
         ]
            ++ disableSelection
        )
    <|
        html rangeTicks


rangeSliderAndCRT tutorialSubject model trace =
    column
        ([ padding 5
         ]
            ++ highlightTutorialSubject tutorialSubject UiCRT
            ++ showExplanation model.explainModeReceiver """The operators "tube", or CRT"""
        )
        [ el
            [ inFront <|
                el
                    [ width fill
                    , paddingEach { edges | left = 20 }
                    ]
                    (rangeScale model)
            ]
            (E.html <| crt model.webGLtime trace)
        ]


modeSwitchPanel tutorialSubject model =
    column
        (commonStyles
            ++ highlightTutorialSubject tutorialSubject UiSwitchPanel
            ++ showExplanation model.explainModeReceiver """Mode switches"""
        )
        [ row (commonStyles ++ highlightTutorialSubject tutorialSubject UiClear)
            [ actionButtonLabelAbove "CLEAR" ResetInputState
            ]
        , row
            ([ Border.widthEach { edges | left = 1, right = 1, top = 1 }
             , Border.color paletteSand
             ]
                ++ commonStyles
            )
            [ column commonStyles
                [ indicatorLabelAbove "A" (model.goniometerMode == Azimuth && model.receiveAB)
                , el ([ E.centerX ] ++ disableSelection) <| text "D/F"
                , indicatorLabelBelow "B" (model.goniometerMode == Azimuth && not model.receiveAB)
                ]
            , column commonStyles
                [ el
                    ([ below (actionButtonNoLabel "A <> B" (SelectReceiveAntenna (not model.receiveAB)))
                     , centerX
                     ]
                        ++ disableSelection
                    )
                  <|
                    text "A <> B"
                ]
            , column commonStyles
                [ indicatorLabelAbove "A" (model.goniometerMode == Elevation && model.receiveAB)
                , el ([ E.centerX ] ++ disableSelection) <| text "HEIGHT"
                , indicatorLabelBelow "B" (model.goniometerMode == Elevation && not model.receiveAB)
                ]
            ]
        , row
            (Border.widthEach { edges | left = 1, right = 1, bottom = 1 }
                :: commonStyles
            )
            [ el ([ centerX, width <| fillPortion 1 ] ++ highlightTutorialSubject tutorialSubject UiSense) <|
                actionButtonLabelAboveWithIndicator "SENSE" model.reflector (EnableReflector (not model.reflector))
            , el ([ centerX, width <| fillPortion 1 ] ++ highlightTutorialSubject tutorialSubject UiHeight) <|
                actionButtonLabelAbove "HEIGHT" <|
                    SelectGoniometerMode
                        (case model.goniometerMode of
                            Elevation ->
                                Azimuth

                            Azimuth ->
                                Elevation
                        )
            ]
        , row (commonStyles ++ highlightTutorialSubject tutorialSubject UiOperatorPrompts)
            [ el [ centerX, width <| fillPortion 1 ] <|
                indicatorLabelBelow "PRESS\nGONIO" <|
                    pressGonioNext model.calculator
            , el [ centerX, width <| fillPortion 1 ] <|
                indicatorLabelBelow "PRESS\nRANGE" <|
                    not <|
                        pressGonioNext model.calculator
            ]
        ]


raidStrengthPanel tutorialSubject model =
    let
        strength =
            model.calculator.storedStrength
    in
    column
        [ Border.color paletteSand
        , Border.widthEach { edges | left = 1, right = 1, top = 1, bottom = 1 }
        , paddingEach { edges | left = 20, right = 20 }
        , centerX
        ]
        [ row
            (commonStyles
                ++ highlightTutorialSubject tutorialSubject UiRaidStrength
                ++ showExplanation model.explainModeReceiver """Raid strength entry buttons"""
            )
            [ column
                [ Font.size 18
                , Font.bold
                , alignRight
                ]
                [ raidStrengthButtonLabelLeft "1" (RaidStrength 1) (strength == Just 1)
                , raidStrengthButtonLabelLeft "2" (RaidStrength 2) (strength == Just 2)
                , raidStrengthButtonLabelLeft "3" (RaidStrength 3) (strength == Just 3)
                , raidStrengthButtonLabelLeft "6" (RaidStrength 6) (strength == Just 6)
                , raidStrengthButtonLabelLeft "9" (RaidStrength 9) (strength == Just 9)
                , raidStrengthButtonLabelLeft "12" (RaidStrength 12) (strength == Just 12)
                ]
            , el [ width (px 30) ] none
            , column
                [ Font.size 18
                , Font.bold
                ]
                [ raidStrengthButtonLabelRight "18" (RaidStrength 18) (strength == Just 18)
                , el [ height <| minimum 30 <| px 30 ] <| none
                , el [ height <| minimum 30 <| px 30 ] <| none
                , el [ height <| minimum 30 <| px 30 ] <| none
                , raidStrengthButtonLabelRight "+"
                    RaidStrengthPlus
                    (model.calculator.storedStrengthPlus == Just True)
                , raidStrengthButtonLabelRight "F"
                    RaidFriendly
                    (model.calculator.storedFriendly == Just True)
                ]
            ]
        , row commonStyles
            [ el ([ E.centerX ] ++ disableSelection) <| text "RAID STRENGTH"
            ]
        ]


operatorPageLandscape tutorialSubject model =
    row
        (centerX
            :: highlightTutorialSubject tutorialSubject UiOperatorPage
        )
        [ column [ width <| fillPortion 3, centerX ]
            [ row []
                [ el
                    []
                    (rangeSliderAndCRT tutorialSubject model <| traceDependingOnMode model)
                ]
            , row []
                [ clickableGonioImage tutorialSubject model
                , el (highlightTutorialSubject tutorialSubject UiGonioButton) <|
                    actionButtonLabelAbove "GONIO" StoreGoniometerSetting
                , clickableRangeKnob tutorialSubject model
                , el (highlightTutorialSubject tutorialSubject UIRangeButton) <|
                    actionButtonLabelAbove "RANGE" StoreRangeSetting
                ]

            --, debugModel model
            ]
        , column [ width <| fillPortion 2, centerX ]
            [ modeSwitchPanel tutorialSubject model
            , raidStrengthPanel tutorialSubject model
            ]
        ]


debugModel : Model -> Element Msg
debugModel model =
    case model.targets of
        [] ->
            text "No targets"

        t :: _ ->
            row [ spacing 10 ]
                [ text <| String.fromFloat t.height
                ]


operatorPagePortrait tutorialSubject model =
    column
        (highlightTutorialSubject tutorialSubject UiOperatorPage)
        [ row []
            [ el
                []
                (rangeSliderAndCRT tutorialSubject model <| traceDependingOnMode model)
            ]
        , row
            (highlightTutorialSubject tutorialSubject UiBothKnobs)
            [ clickableGonioImage tutorialSubject model
            , el (highlightTutorialSubject tutorialSubject UiGonioButton) <|
                actionButtonLabelAbove "GONIO" StoreGoniometerSetting
            , clickableRangeKnob tutorialSubject model
            , el (highlightTutorialSubject tutorialSubject UIRangeButton) <|
                actionButtonLabelAbove "RANGE" StoreRangeSetting
            , el [ alignBottom ] helpButton
            ]
        , row [ width <| fillPortion 2, centerX ]
            [ modeSwitchPanel tutorialSubject model
            , raidStrengthPanel tutorialSubject model
            ]
        ]


operatorPage model =
    let
        tutorialSubject =
            case model.applicationMode of
                TutorialMode tutorial ->
                    findTutorialSubject tutorial

                Model.KioskMode tutorial _ ->
                    findTutorialSubject tutorial

                _ ->
                    Nothing
    in
    case model.outputDevice.orientation of
        Landscape ->
            operatorPageLandscape tutorialSubject model

        Portrait ->
            operatorPagePortrait tutorialSubject model


operatorPageWithTutorial tutorial model =
    let
        rawPage =
            operatorPage model

        showArrows =
            case model.applicationMode of
                TutorialMode _ ->
                    True

                Model.KioskMode _ _ ->
                    False

                InteractiveMode ->
                    False
    in
    case tutorialText tutorial model of
        Just someText ->
            column [ width fill ]
                    [ rawPage
                    , tutorialControls showArrows someText
                    ]

        _ ->
            rawPage

