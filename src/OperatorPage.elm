module OperatorPage exposing (operatorPage)

import CRT_WebGL exposing (crt)
import Constants exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Goniometer exposing (drawGoniometer)
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events.Extra.Pointer as Pointer
import Messages exposing (..)
import Nixie exposing (nixieDisplay)
import PushButtons exposing (..)
import Range exposing (drawRangeKnob)
import Types exposing (GoniometerMode(..), InputState(..))
import Utils exposing (commonStyles, edges)


clickableRangeKnob angle =
    el
        [ htmlAttribute <| Pointer.onDown (\event -> RangeGrab event.pointer.offsetPos)
        , htmlAttribute <| Pointer.onMove (\event -> RangeMove event.pointer.offsetPos)
        , htmlAttribute <| Pointer.onUp (\event -> RangeRelease event.pointer.offsetPos)
        , htmlAttribute <| style "touch-action" "none"
        ]
        (html <| drawRangeKnob angle)


clickableGonioImage theta =
    el
        [ htmlAttribute <| Pointer.onDown (\event -> GonioGrab event.pointer.offsetPos)
        , htmlAttribute <| Pointer.onMove (\event -> GonioMove event.pointer.offsetPos)
        , htmlAttribute <| Pointer.onUp (\event -> GonioRelease event.pointer.offsetPos)
        , htmlAttribute <| style "touch-action" "none"
        ]
        (html <| drawGoniometer theta)


rangeSlider model =
    Input.slider
        [ E.height (E.px 30)
        , E.width E.fill
        , E.centerX
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


rangeDisplay rangeValue =
    column [ E.centerX ]
        [ nixieDisplay 3 (truncate rangeValue)
        , el [ E.centerX ] (E.text "RANGE")
        ]


showMouseCoordinates model =
    let
        ( _, ( x, y ) ) =
            Maybe.withDefault ( 0, ( 0, 0 ) ) model.rangeDrag
    in
    column [ E.centerX ]
        [ el [ E.centerX ] (E.text "X")
        , nixieDisplay 4 (truncate x)
        , el [ E.centerX ] (E.text "Y")
        , nixieDisplay 4 (truncate y)
        ]


traceDependingOnMode model =
    case ( model.goniometerMode, model.receiveAB ) of
        ( Azimuth, _ ) ->
            model.azimuthModeTrace

        ( Elevation, True ) ->
            model.elevation_A_trace

        ( Elevation, False ) ->
            model.elevation_B_trace


rangeSliderAndCRT model =
    column commonStyles
        [ rangeSlider model
        , E.html <| crt model.time
        ]


rangeKnob angle =
    E.el
        [ pointer
        , width (px 200)
        , E.onRight <| actionButtonLabelAbove "RANGE" StoreRangeSetting
        ]
        (clickableRangeKnob angle)


goniometer azimuth =
    E.el
        [ pointer
        , width (px 300)
        , E.onRight <| actionButtonLabelAbove "GONIO" StoreGoniometerSetting
        ]
        (clickableGonioImage azimuth)


modeSwitchPanel model =
    column [ width fill ]
        [ el [ height (px 10) ] none
        , row commonStyles
            [ actionButtonLabelAbove "CLEAR" ResetInputState
            ]
        , row
            (Border.widthEach { edges | left = 1, right = 1, top = 1 }
                :: commonStyles
            )
            [ column commonStyles
                [ indicatorLabelAbove "A" (model.goniometerMode == Azimuth && model.receiveAB)
                , el [ centerX ] <| text "D/F"
                , indicatorLabelBelow "B" (model.goniometerMode == Azimuth && not model.receiveAB)
                ]
            , column commonStyles
                [ el
                    [ below (actionButtonNoLabel "A <> B" (SelectReceiveAntenna (not model.receiveAB)))
                    , centerX
                    ]
                  <|
                    text "A <> B"
                ]
            , column commonStyles
                [ indicatorLabelAbove "A" (model.goniometerMode == Elevation && model.receiveAB)
                , el [ centerX ] <| text "HEIGHT"
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
    column (Border.width 1 :: commonStyles)
        [ row commonStyles
            [ column
                (spacing 30
                    :: commonStyles
                )
                [ el [ alignRight, height <| minimum 30 <| fillPortion 1 ] <|
                    actionButtonLabelLeft "1" (RaidStrength 1)
                , el [ alignRight, height <| minimum 30 <| fillPortion 1 ] <|
                    actionButtonLabelLeft "2" (RaidStrength 2)
                , el [ alignRight, height <| minimum 30 <| fillPortion 1 ] <|
                    actionButtonLabelLeft "3" (RaidStrength 3)
                , el [ alignRight, height <| minimum 30 <| fillPortion 1 ] <|
                    actionButtonLabelLeft "6" (RaidStrength 6)
                , el [ alignRight, height <| minimum 30 <| fillPortion 1 ] <|
                    actionButtonLabelLeft "9" (RaidStrength 9)
                , el [ alignRight, height <| minimum 30 <| fillPortion 1 ] <|
                    actionButtonLabelLeft "12" (RaidStrength 12)
                ]
            , el [ width (px 30) ] none
            , column
                (spacing 30
                    :: commonStyles
                )
                [ el [ alignLeft, height <| minimum 30 <| fillPortion 1 ] <|
                    actionButtonLabelRight "18" (RaidStrength 18)
                , el [ height <| minimum 30 <| px 30 ] <| none
                , el [ height <| minimum 30 <| px 30 ] <| none
                , el [ height <| minimum 30 <| px 30 ] <| none
                , el [ alignLeft, height <| minimum 30 <| fillPortion 1 ] <|
                    actionButtonLabelRight "+" RaidStrengthPlus
                , el [ alignLeft, height <| minimum 30 <| fillPortion 1 ] <|
                    actionButtonLabelRight "F" RaidFriendly
                ]
            ]
        , row commonStyles
            [ el [ centerX ] <| text "RAID STRENGTH"
            ]
        ]


operatorPageLandscape model =
    column
        commonStyles
        [ row commonStyles
            [ el
                [ paddingEach { left = 10, top = 10, bottom = 0, right = 0 }
                , width <| fillPortion 4
                ]
              <|
                rangeSliderAndCRT model
            , el [ width <| fillPortion 2 ] <| modeSwitchPanel model
            ]
        , row commonStyles
            [ el [ width <| fillPortion 2 ] <| goniometer (model.goniometerAzimuth + model.station.lineOfShoot)
            , el [ width <| fillPortion 1 ] <| none
            , el [ width <| fillPortion 2 ] <| rangeKnob model.rangeKnobAngle
            , el [ width <| fillPortion 2 ] <| raidStrengthPanel
            ]
        , el [ width fill, height (px 100) ] none
        ]


operatorPagePortrait model =
    column
        commonStyles
        [ rangeSliderAndCRT model
        , goniometer (model.goniometerAzimuth + model.station.lineOfShoot)
        , rangeKnob model.rangeKnobAngle
        ]


operatorPage model =
    case model.outputDevice.orientation of
        Landscape ->
            operatorPageLandscape model

        Portrait ->
            operatorPagePortrait model
