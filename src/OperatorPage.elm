module OperatorPage exposing (operatorPage)


import CRT exposing (crt)
import Constants exposing (..)
import Element as E exposing (Element, Orientation(..), centerX, column, el, fill, fillPortion, maximum, minimum, none, padding, pointer, row, spacing)
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
import PushButtons exposing (toggleSwitch)
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
        , el [ E.centerX ] (E.text "BEARING")
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
        ]
        [ toggleSwitch "MODE" "BEARING" "ELEVATION" (model.goniometerMode == Bearing) SelectGoniometerMode
        , toggleSwitch "HEIGHT DF" "HIGH (A)" "LOW (B)" False SelectReceiveAntenna
        , toggleSwitch "TRANSMITTER" "MAIN ARRAY" "GAP FILLER" model.transmitAB SelectTransmitAntenna
        , toggleSwitch "REFLECTOR" "ON" "OFF" model.reflector EnableReflector
        ]


--operatorPageLandscape : Model -> Element Msg
operatorPageLandscape model =
    column
        commonStyles
        [ row
            [ E.width E.fill
            ]
            [ E.el
                [ E.width <| fillPortion 1 ]
                none
            , column
                [ E.width <| E.fillPortion 6 ]
                [ rangeSlider model
                , E.html (crt model)
                ]
            , E.el
                [ E.width <| fillPortion 1 ]
                none
            ]
        , row
            [ E.centerX
            , E.spacing 50
            ]
            [ E.el
                [ E.width <| minimum 200 <| fillPortion 3
                , pointer
                ]
              <|
                E.html <|
                    clickableGonioImage <|
                        model.goniometerBearing
                            + model.station.lineOfShoot

            --, showMouseCoordinates model
            , modeToggles model
            , column [ E.width <| fillPortion 1 ]
                [ rangeDisplay model.rangeSlider
                , bearingDisplay (model.goniometerBearing + model.station.lineOfShoot)
                ]
            , column
                [ E.width <| minimum 100 <| fillPortion 2
                , pointer
                , centerX
                ]
                [ E.el
                    [ E.width <| maximum 200 <| fill
                    , pointer
                    ]
                  <|
                    E.html <|
                        clickableRangeKnob <|
                            [ drawRangeKnob model.rangeKnobAngle ]
                , E.el [ centerX ] (E.text "RANGE")
                ]
            ]
        ]


--operatorPagePortrait : Model -> Element Msg
operatorPagePortrait model =
    column
        commonStyles
        [ rangeSlider model
        , E.html (crt model)
        , row [ E.width E.fill ]
            [ E.el [ pointer, E.width <| fillPortion 3 ] <|
                E.html <|
                    clickableGonioImage <|
                        model.goniometerBearing
                            + model.station.lineOfShoot
            , E.el [ pointer, E.width <| fillPortion 2 ] <|
                E.html <|
                    clickableRangeKnob <|
                        [ drawRangeKnob model.rangeKnobAngle ]
            ]
        , row [ E.width E.fill, spacing 10, padding 5 ]
            [ modeToggles model
            , column [ E.width <| fillPortion 1 ]
                [ rangeDisplay model.rangeSlider
                , bearingDisplay (model.goniometerBearing + model.station.lineOfShoot)
                ]
            ]
        ]


--operatorPage : Model -> Element Msg
operatorPage model =
    case model.outputDevice.orientation of
        Landscape ->
            operatorPageLandscape model

        Portrait ->
            operatorPagePortrait model
