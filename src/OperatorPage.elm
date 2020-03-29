module OperatorPage exposing (operatorPage)

import CRT_WebGL exposing (crt)
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
import Model exposing (Model)
import PushButtons exposing (..)
import Range exposing (drawRangeKnob)
import Svg exposing (svg)
import Svg.Attributes as S exposing (fontFamily, fontSize, stroke, strokeWidth, textAnchor, viewBox, x, x1, x2, y, y1, y2)
import TrainingMode exposing (explanatoryText, tutorialTextBox)
import Types exposing (..)
import Utils exposing (commonStyles, disableSelection, edges, helpButton)


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
        , E.width (E.px 600)
        , moveRight 5
        , pointer

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
                [ E.width (E.px 12)
                , E.height (E.px 60)
                , Border.rounded 6
                , Border.width 4
                , Border.color paletteDarkGreen
                , Background.color white
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
        , S.width "102%"
        , S.height "100%"
        ]
    <|
        List.concatMap tick (List.range 0 100)


rangeScale model =
    el
        ([ width fill
         , centerX
         , paddingEach { edges | left = 10, right = 30, top = 10 }
         , E.above (rangeSlider model)
         ]
            ++ disableSelection
        )
    <|
        html rangeTicks


rangeSliderAndCRT model trace =
    column
        ([ padding 5 ] ++ explanatoryText model UiCRT)
        [ el
            [ inFront <|
                el
                    [ alignTop
                    , width fill
                    , paddingEach { edges | left = 20 }
                    ]
                    (rangeScale model)
            ]
            (E.html <| crt model.webGLtime trace)
        ]


modeSwitchPanel model =
    column (commonStyles ++ explanatoryText model UiSwitchPanel)
        [ row (commonStyles ++ explanatoryText model UiClear)
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
        [ Border.color paletteSand
        , Border.widthEach { edges | left = 1, right = 1, top = 1, bottom = 1 }
        , paddingEach { edges | left = 20, right = 20 }
        , centerX
        ]
        [ row (commonStyles ++ explanatoryText model UiRaidStrength)
            [ column
                [ Font.size 18
                , Font.bold
                , alignRight
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
            , debugModel model
            ]
        , column [ width <| fillPortion 2, centerX ]
            [ helpButton
            , modeSwitchPanel model
            , raidStrengthPanel model
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
