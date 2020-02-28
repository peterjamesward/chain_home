module OperatorPage exposing (operatorPage)

import CRT exposing (crt)
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
import PushButtons exposing (actionButton, indicator)
import Range exposing (drawRangeKnob)
import Types exposing (GoniometerMode(..))


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


bearingDisplay bearing =
    column [ E.centerX ]
        [ nixieDisplay 3
            (modBy 360 <|
                truncate
                    (bearing
                        * 180
                        / pi
                    )
            )
        , el [ E.centerX ] (E.text "AZIMUTH")
        ]


commonStyles =
    [ E.width E.fill
    , E.spacing 20
    , E.centerX
    , E.centerY
    , Font.color paletteSand
    , Font.size 14
    , Font.family
        [ Font.typeface "monospace"
        , Font.sansSerif
        ]
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
        , E.html <| crt <| traceDependingOnMode model
        ]


rangeKnob angle =
    E.el [ pointer, width (px 200) ]
        (clickableRangeKnob angle)


goniometer azimuth =
    E.el [ pointer, width (px 300) ]
        (clickableGonioImage azimuth)


modeSwitchPanel model =
    column commonStyles
        [ el [ height (px 10) ] none
        , row commonStyles
            [ el [ width <| fillPortion 1 ] <|
                column commonStyles
                    [ indicator "A" (model.goniometerMode == Azimuth && model.receiveAB)
                    , el [ height (px 10) ] none
                    , el [ centerX ] <| text "D/F"
                    , indicator "B" (model.goniometerMode == Azimuth && not model.receiveAB)
                    ]
            , el [ width <| fillPortion 1 ] <|
                actionButton "A <> B" (SelectReceiveAntenna (not model.receiveAB))
            , el [ width <| fillPortion 1 ] <|
                column commonStyles
                    [ indicator "A" (model.goniometerMode == Elevation && model.receiveAB)
                    , el [ height (px 10) ] none
                    , el [ centerX ] <| text "HEIGHT"
                    , indicator "B" (model.goniometerMode == Elevation && not model.receiveAB)
                    ]
            ]
        , el [ height (px 10) ] none
        , row commonStyles
            [ column commonStyles
                [ indicator "ON" model.reflector
                , el [ height (px 5) ] none
                , actionButton "SENSE" (EnableReflector (not model.reflector))
                ]
            , el [ centerX, width <| fillPortion 1 ] <| none
            , column commonStyles
                [ el [ height (px 40) ] none
                , actionButton "HEIGHT" (SelectGoniometerMode (model.goniometerMode == Elevation))
                ]
            ]
        , el [ height (px 10) ] none
        , row commonStyles
            [ el [ centerX, width <| fillPortion 1 ] <| indicator "PRESS\nGONIO" False
            , el [ centerX, width <| fillPortion 1 ] <| indicator "PRESS\nRANGE" False
            ]
        ]


secondarySwitchPanel model =
    column commonStyles
        [ actionButton "CLEAR" DummyMessage
        , el [ height (px 40) ] none
        , actionButton "RANGE" DummyMessage
        ]


operatorPageLandscape model =
    column
        commonStyles
        [ row commonStyles
            [ el
                [ paddingEach { left = 10, top = 10, bottom = 0, right = 0 }
                , width <| fillPortion 6
                ]
              <|
                rangeSliderAndCRT model
            , el [ width <| fillPortion 2 ] <| modeSwitchPanel model
            , el [ width <| fillPortion 1 ] <| secondarySwitchPanel model
            ]
        , row commonStyles
            [ el [ width <| fillPortion 1 ] <| goniometer (model.goniometerAzimuth + model.station.lineOfShoot)
            , el [ width <| fillPortion 2 ] <| E.text "padding"
            , el [ width <| fillPortion 1 ] <| rangeKnob model.rangeKnobAngle
            , el [ width <| fillPortion 1 ] <| E.text "raid strength"
            ]
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
