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
import PushButtons exposing (actionButton, toggleSwitch)
import Range exposing (drawRangeKnob)
import Types exposing (GoniometerMode(..))


clickableRangeKnob =
    div
        [ Pointer.onDown (\event -> RangeGrab event.pointer.offsetPos)
        , Pointer.onMove (\event -> RangeMove event.pointer.offsetPos)
        , Pointer.onUp (\event -> RangeRelease event.pointer.offsetPos)
        , style "touch-action" "none"
        ]


clickableGonioImage theta =
    div
        [ Pointer.onDown (\event -> GonioGrab event.pointer.offsetPos)
        , Pointer.onMove (\event -> GonioMove event.pointer.offsetPos)
        , Pointer.onUp (\event -> GonioRelease event.pointer.offsetPos)
        , style "touch-action" "none"
        ]
        [ drawGoniometer theta ]


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
                , Border.width 1
                , Border.color <| E.rgb 0 0 0
                , Background.color <| E.rgb255 180 20 20
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
    [ E.spacing 20
    , E.padding 30
    , E.width E.fill
    , E.centerX
    , Font.color paletteSand
    , Font.size 14
    , Font.family
        [ Font.typeface "monospace"
        , Font.sansSerif
        ]
    ]


modeToggles model =
    row
        [ E.width <| fillPortion <| 3
        , E.spacing 20
        , E.padding 40
        , E.centerX
        ]
        [ toggleSwitch "MODE" "D/F" "HEIGHT" (model.goniometerMode == Azimuth) SelectGoniometerMode
        , toggleSwitch "SENSING" "ON" "OFF" model.reflector EnableReflector
        ]


actionButtons model =
    row
        [ E.width <| fillPortion <| 3
        , E.spacing 20
        , E.padding 40
        , E.centerX
        ]
        [ actionButton "STORE\nAZIMUTH" False StoreAzimuth
        , actionButton "STORE\nHEIGHT" False StoreElevation
        ]


traceDependingOnMode model =
    case ( model.goniometerMode, model.receiveAB ) of
        ( Azimuth, _ ) ->
            model.azimuthModeTrace

        ( Elevation, True ) ->
            model.elevation_A_trace

        ( Elevation, False ) ->
            model.elevation_B_trace


operatorPageLandscape model =
    let
        rangeSliderAndCRT =
            column
                [ width fill
                , paddingEach { bottom = 10, top = 10, left = 100, right = 100 }
                ]
                [ rangeSlider model
                , E.html <| crt <| traceDependingOnMode model
                ]

        goniometer =
            E.el
                [ E.width <| minimum 200 <| fillPortion 2
                , pointer
                ]
            <|
                E.html <|
                    clickableGonioImage <|
                        model.goniometerAzimuth
                            + model.station.lineOfShoot

        rangeKnob =
            E.el
                [ E.width <| maximum 200 <| fillPortion 2
                , pointer
                ]
            <|
                E.html <|
                    clickableRangeKnob <|
                        [ drawRangeKnob model.rangeKnobAngle ]
    in
    column
        commonStyles
        [ rangeSliderAndCRT
        , row
            [ E.centerX
            , E.spacing 50
            ]
            [ goniometer
            , column [ E.width <| minimum 200 <| fillPortion 2 ]
                [ modeToggles model
                , actionButtons model
                ]
            , rangeKnob
            ]
        ]



--operatorPagePortrait : Model -> Element Msg


operatorPagePortrait model =
    column
        commonStyles
        [ rangeSlider model
        , E.html <| crt <| traceDependingOnMode model
        , row [ E.width E.fill ]
            [ E.el [ pointer, E.width <| fillPortion 3 ] <|
                E.html <|
                    clickableGonioImage <|
                        model.goniometerAzimuth
                            + model.station.lineOfShoot
            , E.el [ pointer, E.width <| fillPortion 2 ] <|
                E.html <|
                    clickableRangeKnob <|
                        [ drawRangeKnob model.rangeKnobAngle ]
            ]
        , row [ E.width E.fill, spacing 10, padding 5 ]
            [ modeToggles model

            --, column [ E.width <| fillPortion 1 ]
            --    [ rangeDisplay model.rangeSlider
            --    , bearingDisplay (model.goniometerAzimuth + model.station.lineOfShoot)
            --    ]
            ]
        ]



--operatorPage : Model -> Element Msg


operatorPage model =
    case model.outputDevice.orientation of
        Landscape ->
            operatorPageLandscape model

        Portrait ->
            operatorPagePortrait model
