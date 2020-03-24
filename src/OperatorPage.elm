module OperatorPage exposing (operatorPage)

import CRT_WebGL exposing (crt)
import Config exposing (groundRays)
import Constants exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick, onMouseEnter)
import Element.Font as Font
import Element.Input as Input
import Goniometer exposing (drawGoniometer)
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Messages exposing (..)
import PushButtons exposing (..)
import Range exposing (drawRangeKnob)
import TrainingMode exposing (explanatoryText, tutorialTextBox)
import Types exposing (..)
import Utils exposing (choose, commonStyles, disableSelection, edges, helpButton)


clickableRangeKnob model tutorial =
    el
        ([ htmlAttribute <| Pointer.onDown (\event -> RangeGrab event.pointer.offsetPos)
         , htmlAttribute <| Pointer.onMove (\event -> RangeMove event.pointer.offsetPos)
         , htmlAttribute <| Pointer.onUp (\event -> RangeRelease event.pointer.offsetPos)
         , htmlAttribute <| style "touch-action" "none"
         , width (px 200)
         , pointer
         ]
            ++ explanatoryText model UiRangeKnob
        )
        (html <| drawRangeKnob model.rangeKnobAngle)


clickableGonioImage model tutorial =
    let
        theta =
            model.goniometerAzimuth + model.station.lineOfShoot
    in
    el
        ([ htmlAttribute <| Pointer.onDown (\event -> GonioGrab event.pointer.offsetPos)
         , htmlAttribute <| Pointer.onMove (\event -> GonioMove event.pointer.offsetPos)
         , htmlAttribute <| Pointer.onUp (\event -> GonioRelease event.pointer.offsetPos)
         , htmlAttribute <| style "touch-action" "none"
         , width (px 300)
         , pointer
         ]
            ++ explanatoryText model UiGoniometer
        )
        (html <| drawGoniometer theta)


rangeSlider model =
    Input.slider
        [ E.height (E.px 30)
        , E.width (E.px 560)
        , paddingEach { edges | left = 80 }
        , pointer

        -- Here is where we're creating/styling the "track"
        , E.behindContent
            (E.el
                [ E.width E.fill
                , E.height (E.px 2)
                , E.centerY
                , E.centerX
                , Background.color midGray
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
                [ E.width (E.px 16)
                , E.height (E.px 50)
                , Border.rounded 8
                , Border.width 4
                , Border.color paletteDarkGreen
                , Background.color white
                ]
        }


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


rangeScale model =
    row
        ([ width (px 600)
         , spaceEvenly
         , paddingEach { edges | top = 10, left = 40 }
         , Font.color beamGreen
         , Font.size 20
         , Font.family
            [ Font.typeface "Courier New"
            , Font.sansSerif
            ]
         ]
            ++ explanatoryText model UiRangeScale
        )
    <|
        List.map
            (\i ->
                el disableSelection <|
                    text (String.fromInt (10 * i))
            )
            (List.range 0 10)


rangeSliderAndCRT model trace =
    column
        ([ padding 5 ] ++ explanatoryText model UiCRT)
        [ none
        , el
            [ E.below (rangeScale model)
            , paddingEach { edges | left = 40 }
            ]
            (rangeSlider model)
        , el
            []
            (E.html <| crt model.webGLtime trace)
        ]


modeSwitchPanel model =
    column (commonStyles ++ explanatoryText model UiSwitchPanel)
        [ row (commonStyles ++ explanatoryText model UiClear)
            [ actionButtonLabelAbove "CLEAR" ResetInputState
            ]
        , row
            (Border.widthEach { edges | left = 1, right = 1, top = 1 }
                :: commonStyles
                ++ explanatoryText model UiAB
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
            [ el ([ centerX, width <| fillPortion 1 ] ++ explanatoryText model UiSense) <|
                actionButtonLabelAboveWithIndicator "SENSE" model.reflector (EnableReflector (not model.reflector))
            , el ([ centerX, width <| fillPortion 1 ] ++ explanatoryText model UiHeight) <|
                actionButtonLabelAbove "HEIGHT" (SelectGoniometerMode (model.goniometerMode == Elevation))
            ]
        , row (commonStyles ++ explanatoryText model UiOperatorPrompts)
            [ el [ centerX, width <| fillPortion 1 ] <|
                indicatorLabelBelow "PRESS\nGONIO" <|
                    (model.inputState == BearingInput)
                        || (model.inputState == HeightInput)
            , el [ centerX, width <| fillPortion 1 ] <|
                indicatorLabelBelow "PRESS\nRANGE" <|
                    (model.inputState == BearingRangeInput)
                        || (model.inputState == HeightRangeInput)
            ]
        ]


raidStrengthPanel model =
    column
        (Border.widthEach { edges | left = 1, right = 1, top = 1, bottom = 1 }
            :: paddingEach { edges | left = 20, right = 20 }
            :: (commonStyles ++ explanatoryText model UiRaidStrength)
        )
        [ row [ centerX ]
            [ column
                [ Font.size 18
                , Font.bold
                ]
                [ raidStrengthButtonLabelLeft "1" (RaidStrength 1) (model.storedStrength == Just 1)
                , raidStrengthButtonLabelLeft "2" (RaidStrength 2) (model.storedStrength == Just 2)
                , raidStrengthButtonLabelLeft "3" (RaidStrength 3) (model.storedStrength == Just 3)
                , raidStrengthButtonLabelLeft "6" (RaidStrength 6) (model.storedStrength == Just 6)
                , raidStrengthButtonLabelLeft "9" (RaidStrength 9) (model.storedStrength == Just 9)
                , raidStrengthButtonLabelLeft "12" (RaidStrength 12) (model.storedStrength == Just 12)
                ]
            , el [ width (px 30) ] none
            , column
                [ Font.size 18
                , Font.bold
                ]
                [ raidStrengthButtonLabelRight "18" (RaidStrength 18) (model.storedStrength == Just 18)
                , el [ height <| minimum 30 <| px 30 ] <| none
                , el [ height <| minimum 30 <| px 30 ] <| none
                , el [ height <| minimum 30 <| px 30 ] <| none
                , raidStrengthButtonLabelRight "+" RaidStrengthPlus (model.storedStrengthPlus == Just True)
                , raidStrengthButtonLabelRight "F" RaidFriendly (model.storedFriendly == Just True)
                ]
            ]
        , row commonStyles
            [ el ([ E.centerX ] ++ disableSelection) <| text "RAID STRENGTH"
            ]
        ]


operatorPageLandscape model =
    row
        ([ centerX
         , tutorialTextBox model
            [ moveUp 100
            , moveLeft 80
            , centerX
            , centerY
            ]
         ]
            ++ explanatoryText model UiOperatorPage
        )
        [ column [ width <| fillPortion 3, centerX ]
            [ row []
                [ el
                    []
                    (rangeSliderAndCRT model <| traceDependingOnMode model)
                ]
            , row []
                [ clickableGonioImage model UiRangeKnob
                , el (explanatoryText model UiGonioButton) <|
                    actionButtonLabelAbove "GONIO" StoreGoniometerSetting
                , clickableRangeKnob model UiGoniometer
                , el (explanatoryText model UIRangeButton) <|
                    actionButtonLabelAbove "RANGE" StoreRangeSetting
                ]
            , debugShowNewRaid model
            ]
        , column [ width <| fillPortion 2, centerX ]
            [ helpButton
            , modeSwitchPanel model
            , raidStrengthPanel model
            ]
        ]


debugShowNewRaid model =
    case model.newRaid of
        Just raid ->
            row [ spacing 10 ]
                [ text <| String.fromFloat raid.latitude
                , text <| String.fromFloat raid.longitude
                , text <| String.fromFloat raid.height
                ]

        _ ->
            none


operatorPagePortrait model =
    column
        ([ centerX
         , tutorialTextBox model
            [ moveUp 220
            , moveLeft 0
            , centerX
            , centerY
            ]
         ]
            ++ explanatoryText model UiOperatorPage
        )
        [ row []
            [ el
                []
                (rangeSliderAndCRT model <| traceDependingOnMode model)
            ]
        , row
            (explanatoryText model UiBothKnobs)
            [ clickableGonioImage model UiRangeKnob
            , el (explanatoryText model UiGonioButton) <|
                actionButtonLabelAbove "GONIO" StoreGoniometerSetting
            , clickableRangeKnob model UiGoniometer
            , el (explanatoryText model UIRangeButton) <|
                actionButtonLabelAbove "RANGE" StoreRangeSetting
            , el [ alignBottom ] helpButton
            ]
        , row [ width <| fillPortion 2, centerX ]
            [ modeSwitchPanel model
            , raidStrengthPanel model
            ]
        ]


operatorPage model =
    case model.outputDevice.orientation of
        Landscape ->
            operatorPageLandscape model

        Portrait ->
            operatorPagePortrait model
