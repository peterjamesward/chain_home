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
        , E.width (E.px 700)
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


rangeScale =
    row
        [ width fill
        , spaceEvenly
        , paddingEach { edges | top = 10, left = 5 }
        , Font.color beamGreen
        , Font.size 28
        , Font.family
            [ Font.typeface "Courier New"
            , Font.sansSerif
            ]
        ]
    <|
        List.map
            (\i ->
                text (String.fromInt (10 * i))
            )
            (List.range 0 10)


rangeSliderAndCRT model trace =
    column [ width fill, paddingEach {edges | left = 50, top = 10, right = 50} ]
        [ E.el
            [ E.below rangeScale
            , centerX
            ]
            (rangeSlider model)
        , E.html <| crt model.webGLtime trace
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
        , actionButtonLabelAbove "RANGE" StoreRangeSetting
        ]


modeSwitchPanel model =
    column commonStyles
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
                    , paddingEach {edges | bottom = 10}
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
            [ el [ centerX ] <| text "RAID STRENGTH"
            ]
        ]


operatorPageLandscape model =
    row commonStyles
        [ column commonStyles
            [ rangeSliderAndCRT model <| traceDependingOnMode model
            , row commonStyles
                [ goniometer (model.goniometerAzimuth + model.station.lineOfShoot)
                , el [ width (px 200) ] none
                , rangeKnob model.rangeKnobAngle
                ]
            ]
        , column commonStyles
            [ modeSwitchPanel model
            , raidStrengthPanel
            ]
        , el [ width fill, height (px 100) ] none
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
