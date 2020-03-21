module Main exposing (main)

{-
   This Main module now based on the elm-ui example by Alex Korban
   in his book "Practical Elm"".
-}

import Attr exposing (..)
import Browser
import Browser.Events exposing (..)
import CalculatorDisplay exposing (calculator)
import Config exposing (..)
import Constants exposing (..)
import Echo exposing (..)
import Element as E exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Event exposing (..)
import Element.Font as Font
import Element.Input as Input
import ElevationCurves exposing (aElevationAdjustedEchoes, bElevationAdjustedEchoes)
import Goniometer exposing (goniometerTurnAngle)
import Html.Attributes exposing (style)
import Json.Decode as D exposing (..)
import Keys exposing (Keys, noKeys, updateKeys)
import LobeFunctions exposing (..)
import Messages exposing (..)
import Model exposing (..)
import OperatorPage exposing (operatorPage)
import Platform.Cmd exposing (Cmd)
import Range exposing (rangeTurnAngle)
import Receiver exposing (goniometerMix)
import Station exposing (..)
import Target exposing (..)
import Task
import Time
import TrainingMode exposing (..)
import Types exposing (..)
import Utils exposing (..)


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = InputPage
      , webGLtime = 0.0
      , modelTime = 0
      , startTime = 0
      , azimuthModeTrace = []
      , elevation_A_trace = []
      , elevation_B_trace = []
      , station = bawdsey
      , targets = []
      , movedTargets = []
      , polarTargets = []
      , echoes = []
      , skyline = []
      , goniometerAzimuth = degrees 40 -- relative to Line Of Shoot.
      , gonioOutput = []
      , keys = noKeys
      , gonioDrag = Nothing
      , activeConfigurations = []
      , rangeSlider = 50.0
      , outputDevice = { class = Desktop, orientation = Landscape }
      , rangeDrag = Nothing
      , rangeKnobAngle = 0.0
      , goniometerMode = Azimuth
      , transmitAntenna = transmitANoReflect
      , reflector = False
      , isMenuOpen = False
      , receiveAB = True
      , receiveAntenna = receiveHigh
      , inputState = BearingInput
      , storedAzimuth = Nothing
      , storedElevation = Nothing
      , storedAzimuthRange = Nothing
      , storedElevationRange = Nothing
      , storedStrength = Nothing
      , storedFriendly = Nothing
      , storedStrengthPlus = Nothing
      , tutorialStage = Nothing
      , tutorialScenario = Nothing
      , explainMode = False
      , tutorialsCompleted = []
      }
    , Task.perform SetStartTime Time.now
    )


swingGoniometer : Angle -> Keys -> Angle
swingGoniometer angle keys =
    -- Mouse tracking permits non-integer movements.
    case ( keys.gonioClock, keys.gonioAnti ) of
        ( True, True ) ->
            angle

        ( True, False ) ->
            angle + degrees 1.0

        ( False, True ) ->
            angle - degrees 1.0

        _ ->
            angle


slideRangeSlider : Range -> Keys -> Range
slideRangeSlider range keys =
    case ( keys.rangeLeft, keys.rangeRight ) of
        ( True, True ) ->
            range

        ( True, False ) ->
            Basics.max 0.0 (range - 0.5)

        ( False, True ) ->
            Basics.min 100.0 (range + 0.5)

        _ ->
            range


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 40 UpdateModel
        , onAnimationFrameDelta TimeDelta
        , onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        , onResize (\w h -> DeviceResize w h)
        ]


applyReceiver : Antenna -> List Echo -> List Echo
applyReceiver antenna rawEchoes =
    List.map
        (\e ->
            { e
                | amplitude =
                    e.amplitude
                        * antenna.verticalLobeFunction e.alpha
            }
        )
        rawEchoes


deriveModelAtTime : Model -> Int -> Model
deriveModelAtTime model timeNow =
    let
        t =
            timeNow - model.startTime

        targetsNow =
            -- Where are they, based on origin, bearing, speed, time.
            List.map (targetAtTime t) model.targets

        convertedTargets =
            -- Easier to work in polar coordinates here on.
            -- Filter removes raid beyond our range.
            List.filter (\tgt -> tgt.r < 100 * 1600) <|
                List.map (mapToPolar bawdsey) targetsNow

        echoSignals =
            -- Deduce echo based on transmitter characteristics.
            deriveEchoes convertedTargets model.transmitAntenna

        receiveSignals =
            -- Deduce inputs based on receiver characteristics.
            applyReceiver model.receiveAntenna echoSignals

        heightMode_A_Outputs =
            -- When we are in elevation mode, we need high and low receiver inputs to derive elevation.
            -- But we going to cheat here, since height finding is iffy at best. We know the
            -- elevation of each target, and we can hard-code the height response curves for A and B
            -- systems (Supervisor's Handbook chap 12). We then know the goniometer setting that
            -- will make a target "D/F out" -- we only need a suitable function to make it appear
            -- that we are actually using a goniometer.
            aElevationAdjustedEchoes model.goniometerAzimuth echoSignals

        heightMode_B_Outputs =
            bElevationAdjustedEchoes model.goniometerAzimuth echoSignals

        gonioOutput =
            -- 'Blend' X and Y inputs to find target's azimuth.
            goniometerMix model.goniometerAzimuth receiveSignals

        newRangeSliderPosition =
            slideRangeSlider model.rangeSlider model.keys

        newRangeKnobPosition =
            -- Map 0..100 onto -pi..+pi
            (newRangeSliderPosition - 50) * pi / 50
    in
    tutorialAutomation
        { model
            | modelTime = timeNow
            , movedTargets = targetsNow
            , polarTargets = convertedTargets
            , echoes = echoSignals
            , gonioOutput = gonioOutput
            , azimuthModeTrace = gonioOutput
            , goniometerAzimuth = swingGoniometer model.goniometerAzimuth model.keys
            , rangeSlider = newRangeSliderPosition
            , rangeKnobAngle = newRangeKnobPosition
            , elevation_A_trace = heightMode_A_Outputs
            , elevation_B_trace = heightMode_B_Outputs
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        cleanModel =
            exitTutorial model
    in
    case msg of
        TimeDelta dt ->
            ( { model | webGLtime = model.webGLtime + dt / 100.0 }
            , Cmd.none
            )

        SetStartTime time ->
            ( { model | startTime = Time.posixToMillis time }
            , Cmd.none
            )

        StartScenario ->
            ( { cleanModel
                | startTime = model.modelTime
                , webGLtime = 0.0
                , targets = getAllTargets model.activeConfigurations
                , currPage = OperatorPage
              }
            , Cmd.none
            )

        UpdateModel time ->
            ( deriveModelAtTime model (Time.posixToMillis time)
            , Cmd.none
            )

        ToggleLearnMenu ->
            ( { model
                | isMenuOpen = not model.isMenuOpen
              }
            , Cmd.none
            )

        ExplainModeToggle ->
            ( { model
                | explainMode = not model.explainMode
              }
            , Cmd.none
            )

        SetConfigStateMsg index newState ->
            ( { model
                | activeConfigurations =
                    updateConfig model.activeConfigurations index newState
              }
            , Cmd.none
            )

        DisplayReceiver ->
            ( { cleanModel
                | currPage = OperatorPage
                , tutorialStage = Nothing
                , isMenuOpen = False
              }
            , Cmd.none
            )

        DisplayConfiguration ->
            ( { cleanModel
                | currPage = InputPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        DisplayCalculator ->
            ( { cleanModel
                | currPage = OutputPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        DisplayTraining scenario ->
            ( tutorialStartScenario scenario model
            , Cmd.none
            )

        TutorialAdvance ->
            ( advanceTutorial model
            , Cmd.none
            )

        TutorialBack ->
            ( goBackInTutorial model
            , Cmd.none
            )

        KeyChanged isDown key ->
            ( { model | keys = updateKeys isDown key model.keys }
            , Cmd.none
            )

        GonioGrab offset ->
            ( { model
                | gonioDrag = Just ( model.goniometerAzimuth, offset )
              }
            , Cmd.none
            )

        GonioMove offset ->
            case model.gonioDrag of
                Nothing ->
                    ( model, Cmd.none )

                Just ( startAngle, startXY ) ->
                    let
                        newAngle =
                            goniometerTurnAngle startAngle startXY offset
                    in
                    ( { model
                        | goniometerAzimuth = newAngle
                        , gonioDrag = Just ( newAngle, offset )
                      }
                    , Cmd.none
                    )

        GonioRelease offset ->
            ( { model
                | gonioDrag = Nothing
              }
            , Cmd.none
            )

        RangeGrab offset ->
            ( { model
                | rangeDrag = Just ( model.rangeKnobAngle, offset )
              }
            , Cmd.none
            )

        RangeMove offset ->
            case model.rangeDrag of
                Nothing ->
                    ( model, Cmd.none )

                Just ( startAngle, startXY ) ->
                    let
                        stopKnob =
                            pi - 0.1

                        -- Prevent complete rotation.
                        newAngle =
                            clamp (0 - stopKnob) stopKnob <|
                                rangeTurnAngle startAngle startXY offset

                        newSliderPosition =
                            (pi + normalise newAngle) * 50 / pi
                    in
                    ( if abs (newSliderPosition - model.rangeSlider) < 50 then
                        -- Looks like a reasonable move
                        { model
                            | rangeSlider = newSliderPosition
                            , rangeKnobAngle = newAngle
                            , rangeDrag = Just ( newAngle, offset )
                        }

                      else
                        -- Looks like it's wrapped around
                        { model | rangeDrag = Just ( newAngle, offset ) }
                    , Cmd.none
                    )

        RangeRelease offset ->
            ( { model
                | rangeDrag = Nothing

                --, tutorialStage = rangeMayUpdateTutorial model
              }
            , Cmd.none
            )

        AdjustRangeValue newRange ->
            ( { model
                | rangeSlider = newRange
              }
            , Cmd.none
            )

        DeviceResize w h ->
            ( { model | outputDevice = classifyDevice { width = w, height = h } }
            , Cmd.none
            )

        SelectReceiveAntenna val ->
            ( { model
                | receiveAB = val
                , receiveAntenna = choose val receiveHigh receiveLow
              }
            , Cmd.none
            )

        EnableReflector val ->
            ( { model
                | reflector = val
                , transmitAntenna = selectTransmitAntenna True val
              }
            , Cmd.none
            )

        SelectGoniometerMode mode ->
            ( { model
                | goniometerMode = choose mode Azimuth Elevation
                , inputState = choose mode BearingInput HeightInput
              }
            , Cmd.none
            )

        StoreGoniometerSetting ->
            ( case model.inputState of
                BearingInput ->
                    { model
                        | storedAzimuth = Just (model.goniometerAzimuth + model.station.lineOfShoot)
                        , inputState = BearingRangeInput
                    }

                HeightInput ->
                    { model
                        | storedElevation = findTargetElevation model.targets model.polarTargets model.rangeSlider
                        , inputState = HeightRangeInput
                    }

                _ ->
                    model
            , Cmd.none
            )

        StoreRangeSetting ->
            ( case model.inputState of
                BearingRangeInput ->
                    { model
                        | storedAzimuthRange = Just (1.6 * model.rangeSlider)
                        , inputState = BearingInput
                    }

                HeightRangeInput ->
                    { model
                        | storedElevationRange = Just (1.6 * model.rangeSlider)
                        , inputState = HeightInput
                    }

                _ ->
                    model
            , Cmd.none
            )

        RaidStrength strength ->
            ( { model | storedStrength = Just strength }, Cmd.none )

        RaidStrengthPlus ->
            ( { model | storedStrengthPlus = Just True }, Cmd.none )

        RaidFriendly ->
            ( { model | storedFriendly = Just True }, Cmd.none )

        ResetInputState ->
            ( clearCalculator model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


selectTransmitAntenna ab reflect =
    case ( ab, reflect ) of
        ( True, True ) ->
            transmitAReflector

        ( False, True ) ->
            transmitBReflector

        ( True, False ) ->
            transmitANoReflect

        ( False, False ) ->
            transmitBNoReflect


view : Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                OperatorPage ->
                    operatorPage model

                InputPage ->
                    inputPage model

                OutputPage ->
                    calculatorPage model

                TrainingPage ->
                    operatorPage model
    in
    { title = "Chain Home receiver emulation"
    , body =
        [ layout
            [ Background.color flatMidnightBlue
            , width fill
            ]
          <|
            column [ E.width E.fill, spacingXY 0 20 ]
                [ navBar model
                , content
                ]
        ]
    }


navItem label action =
    el
        ([ pointer
         , Event.onClick action
         ]
            ++ disableSelection
        )
    <|
        text label


navBar : Model -> Element Msg
navBar model =
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color paletteSand
        , Font.color paletteLightGreen
        , spaceEvenly
        ]
        [ el
            (if model.isMenuOpen then
                [ below learningMenu ]

             else
                []
            )
          <|
            navItem "Learn" ToggleLearnMenu
        , navItem "Configuration" DisplayConfiguration
        , navItem "Operator" DisplayReceiver
        , navItem "Calculator" DisplayCalculator
        ]


learningMenu : Element Msg
learningMenu =
    column
        [ spacing 20
        , Background.color flatMidnightBlue
        , Border.color paletteLightGreen
        , Border.width 2
        , moveDown 10
        , padding 20
        , Font.color white
        ]
        [ navItem "Basic operation" (DisplayTraining ScenarioBasic)
        , navItem "2 aircraft together" (DisplayTraining ScenarioTwoTogether)
        , navItem "2 aircraft apart" (DisplayTraining ScenarioTwoSeparate)
        , navItem "3 to 6 strong raid" (DisplayTraining ScenarioThreeToSix)
        , navItem "Friendly (IFF)" (DisplayTraining ScenarioFriendly)
        ]



-- Show list of configurations with Checkboxes.
-- This will now be on its own page with elm-ui.


setConfig : TargetSelector -> Bool -> Msg
setConfig selector newState =
    SetConfigStateMsg selector.id newState


targetSelector : List TargetSelector -> Element Msg
targetSelector active =
    let
        display : TargetSelector -> Element Msg
        display g =
            Input.checkbox
                [ E.height (px 40)
                , Border.width 1
                , Border.rounded 5
                , Border.color lightCharcoal
                , padding 10
                ]
                { onChange = setConfig g
                , checked = g.active
                , label =
                    Input.labelRight
                        [ htmlAttribute <| style "-webkit-user-select" "none"
                        ]
                    <|
                        E.text g.description
                , icon = Input.defaultCheckbox
                }
    in
    column
        [ E.width (px 550)
        , spacingXY 0 10
        , paddingEach { edges | left = 100, top = 50 }
        , Font.color lightCharcoal
        ]
    <|
        List.map display active


calculatorPage : Model -> Element Msg
calculatorPage model =
    calculator
        model


inputPage : Model -> Element Msg
inputPage model =
    row
        [ E.width fill
        , Font.color lightCharcoal
        ]
        [ targetSelector model.activeConfigurations
        , column [ centerX, spacingXY 0 20 ]
            [ Input.button
                (Attr.greenButton ++ [ width (px 200), height (px 40), centerX ])
                { onPress = Just StartScenario
                , label = el [ centerX ] <| text "Go!"
                }
            ]
        ]


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


menuPanel : Model -> Element Msg
menuPanel model =
    let
        items =
            [ E.el [ pointer, Event.onClick DisplayConfiguration ] <| text "Configuration"
            , E.el [ pointer, Event.onClick DisplayReceiver ] <| text "Operator"
            , E.el [ pointer, Event.onClick DisplayCalculator ] <| text "Outputs"
            ]

        panel =
            column
                [ Background.color lightCharcoal
                , Border.widthEach { left = 1, right = 0, top = 0, bottom = 0 }
                , Border.color flatMidnightBlue
                , Border.shadow
                    { offset = ( 0, 0 )
                    , size = 1
                    , blur = 10
                    , color = flatWetAsphalt
                    }
                , Font.bold
                , Font.color paletteDarkGreen
                , Font.family [ Font.sansSerif ]
                , width <| fillPortion 1
                , height fill
                , paddingXY 20 20
                , spacingXY 0 40
                ]
                items

        overlay =
            el [ width <| fillPortion 4, height fill, Event.onClick ToggleLearnMenu ] none
    in
    if model.isMenuOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none
