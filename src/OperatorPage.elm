module OperatorPage exposing (operatorPage)

import CRT_WebGL exposing (crt)
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
import PushButtons exposing (..)
import Range exposing (drawRangeKnob)
import TrainingMode exposing (tutorialMode)
import Types exposing (..)
import Utils exposing (choose, commonStyles, disableSelection, edges)


clickableRangeKnob angle =
    el
        [ htmlAttribute <| Pointer.onDown (\event -> RangeGrab event.pointer.offsetPos)
        , htmlAttribute <| Pointer.onMove (\event -> RangeMove event.pointer.offsetPos)
        , htmlAttribute <| Pointer.onUp (\event -> RangeRelease event.pointer.offsetPos)
        , htmlAttribute <| style "touch-action" "none"
        , width (px 200)
        ]
        (html <| drawRangeKnob angle)


clickableGonioImage theta =
    el
        [ htmlAttribute <| Pointer.onDown (\event -> GonioGrab event.pointer.offsetPos)
        , htmlAttribute <| Pointer.onMove (\event -> GonioMove event.pointer.offsetPos)
        , htmlAttribute <| Pointer.onUp (\event -> GonioRelease event.pointer.offsetPos)
        , htmlAttribute <| style "touch-action" "none"
        , width (px 300)
        ]
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
    case ( model.goniometerMode, model.receiveAB ) of
        ( Azimuth, _ ) ->
            model.azimuthModeTrace

        ( Elevation, True ) ->
            model.elevation_A_trace

        ( Elevation, False ) ->
            model.elevation_B_trace


rangeScale =
    row
        [ width (px 600)
        , spaceEvenly
        , paddingEach { edges | top = 10, left = 40 }
        , Font.color beamGreen
        , Font.size 20
        , Font.family
            [ Font.typeface "Courier New"
            , Font.sansSerif
            ]
        ]
    <|
        List.map
            (\i ->
                el disableSelection <|
                    text (String.fromInt (10 * i))
            )
            (List.range 0 10)


rangeSliderAndCRT model trace =
    column
        [ padding 5 ]
        [ E.el
            [ E.below rangeScale
            , paddingEach { edges | left = 40 }
            ]
            (rangeSlider model)
        , el
            -- anchor for the 'inFromt'
            [ inFront <|
                el
                    ([ height (px 300), width fill ]
                        ++ tutorialMode model TutorialCRTTrace
                        ++ tutorialMode model TutorialIncomingRaid
                    )
                    (text "")
            ]
            (E.html <| crt model.webGLtime trace)
        ]


rangeKnob angle =
    E.row
        [ pointer
        , centerY
        , width fill
        ]
        [ clickableRangeKnob angle
        , actionButtonLabelAbove "RANGE" StoreRangeSetting
        ]


goniometer azimuth =
    E.row
        [ pointer
        , centerY
        , width fill
        ]
        [ clickableGonioImage azimuth
        , actionButtonLabelAbove "GONIO" StoreGoniometerSetting
        ]


modeSwitchPanel model =
    column commonStyles
        [ row commonStyles
            [ actionButtonLabelAbove "CLEAR" ResetInputState
            ]
        , row
            (Border.widthEach { edges | left = 1, right = 1, top = 1 }
                :: commonStyles
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
            [ el [ centerX, width <| fillPortion 1 ] <|
                actionButtonLabelAboveWithIndicator "SENSE" model.reflector (EnableReflector (not model.reflector))
            , el [ centerX, width <| fillPortion 1 ] <|
                actionButtonLabelAbove "HEIGHT" (SelectGoniometerMode (model.goniometerMode == Elevation))
            ]
        , row commonStyles
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


raidStrengthPanel =
    column
        (Border.widthEach { edges | left = 1, right = 1, top = 1, bottom = 1 }
            :: paddingEach { edges | left = 20, right = 20 }
            :: commonStyles
        )
        [ row [ centerX ]
            [ column
                [ Font.size 18
                , Font.bold
                ]
                [ actionButtonLabelLeft "1" (RaidStrength 1)
                , actionButtonLabelLeft "2" (RaidStrength 2)
                , actionButtonLabelLeft "3" (RaidStrength 3)
                , actionButtonLabelLeft "6" (RaidStrength 6)
                , actionButtonLabelLeft "9" (RaidStrength 9)
                , actionButtonLabelLeft "12" (RaidStrength 12)
                ]
            , el [ width (px 30) ] none
            , column
                [ Font.size 18
                , Font.bold
                ]
                [ actionButtonLabelRight "18" (RaidStrength 18)
                , el [ height <| minimum 30 <| px 30 ] <| none
                , el [ height <| minimum 30 <| px 30 ] <| none
                , el [ height <| minimum 30 <| px 30 ] <| none
                , actionButtonLabelRight "+" RaidStrengthPlus
                , actionButtonLabelRight "F" RaidFriendly
                ]
            ]
        , row commonStyles
            [ el ([ E.centerX ] ++ disableSelection) <| text "RAID STRENGTH"
            ]
        ]


operatorPageLandscape model =
    row (centerX :: tutorialMode model TutorialWelcome)
        [ column [ width <| fillPortion 3, centerX ]
            [ row (tutorialMode model TutorialDescribeCRT)
                -- This 'el' just an anchor for the 'inFront' element.
                [ el
                    [ inFront <|
                        el
                            (height (px 200) :: width fill :: tutorialMode model TutorialRangeScale)
                            (text "")
                    ]
                    (rangeSliderAndCRT model <| traceDependingOnMode model)
                ]
            , row []
                [ clickableGonioImage (model.goniometerAzimuth + model.station.lineOfShoot)
                , actionButtonLabelAbove "GONIO" StoreGoniometerSetting
                , clickableRangeKnob model.rangeKnobAngle
                , actionButtonLabelAbove "RANGE" StoreRangeSetting
                ]
            ]
        , column [ width <| fillPortion 2, centerX ]
            [ modeSwitchPanel model
            , raidStrengthPanel
            ]
        ]


operatorPagePortrait model =
    column
        commonStyles
        [ rangeSliderAndCRT model <| traceDependingOnMode model
        , goniometer (model.goniometerAzimuth + model.station.lineOfShoot)
        , rangeKnob model.rangeKnobAngle
        ]


operatorPage model =
    case model.outputDevice.orientation of
        Landscape ->
            operatorPageLandscape model

        Portrait ->
            operatorPagePortrait model
