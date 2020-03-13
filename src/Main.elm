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
import LobeFunctions exposing (..)
import Messages exposing (..)
import Model exposing (..)
import OperatorPage exposing (operatorPage)
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
      , targets = getAllTargets targetConfigurations
      , movedTargets = []
      , polarTargets = []
      , echoes = []
      , skyline = []
      , goniometerAzimuth = degrees 10 -- relative to Line Of Shoot.
      , gonioOutput = []
      , keys = noKeys
      , gonioDrag = Nothing
      , activeConfigurations = targetConfigurations
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
      , trainingScenario = Nothing
      }
    , Task.perform SetStartTime Time.now
    )


noKeys : Keys
noKeys =
    Keys False False False False


updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
    case key of
        "q" ->
            { keys | gonioAnti = isDown }

        "a" ->
            { keys | gonioClock = isDown }

        "," ->
            { keys | rangeLeft = isDown }

        "." ->
            { keys | rangeRight = isDown }

        _ ->
            keys


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

        gonioAzimuthOut =
            -- 'Blend' X and Y inputs to find target's azimuth.
            goniometerMix model.goniometerAzimuth receiveSignals
    in
    { model
        | modelTime = timeNow
        , targets = getAllTargets model.activeConfigurations
        , movedTargets = targetsNow
        , polarTargets = convertedTargets
        , echoes = echoSignals
        , gonioOutput = gonioAzimuthOut
        , azimuthModeTrace = groundRays ++ gonioAzimuthOut
        , goniometerAzimuth = swingGoniometer model.goniometerAzimuth model.keys
        , rangeSlider = slideRangeSlider model.rangeSlider model.keys
        , elevation_A_trace = groundRays ++ heightMode_A_Outputs
        , elevation_B_trace = groundRays ++ heightMode_B_Outputs
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
            ( { model
                | startTime = model.modelTime
                , webGLtime = 0.0
                , currPage = OperatorPage
              }
            , Cmd.none
            )

        UpdateModel time ->
            ( deriveModelAtTime model (Time.posixToMillis time)
            , Cmd.none
            )

        ToggleMenu ->
            ( { model
                | isMenuOpen = not model.isMenuOpen
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
            ( { model
                | currPage = OperatorPage
              }
            , Cmd.none
            )

        DisplayConfiguration ->
            ( { model
                | currPage = InputPage
              }
            , Cmd.none
            )

        DisplayCalculator ->
            ( { model
                | currPage = OutputPage
              }
            , Cmd.none
            )

        DisplayTraining ->
            ( { model
                | currPage = TrainingPage
                , trainingScenario = Just welcomePrompt
              }
            , Cmd.none
            )

        ScenarioAdvance ->
            ( { model
                | currPage = TrainingPage
                , trainingScenario = advanceScenario model.trainingScenario
              }
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
                        newAngle =
                            goniometerTurnAngle startAngle startXY offset

                        angleChange =
                            (newAngle - startAngle) |> sin |> asin |> (*) (60 / pi)

                        newRange =
                            max 0 <| min 100 <| (model.rangeSlider + angleChange)
                    in
                    ( if angleChange > 0 then
                        -- Turn clockwise, increase range
                        { model
                            | rangeSlider = newRange
                            , rangeDrag =
                                choose (newRange <= 100) (Just ( newAngle, offset )) Nothing
                            , rangeKnobAngle = newAngle
                        }

                      else if angleChange < 0 then
                        -- Turn anticlockwise, decrease range
                        { model
                            | rangeSlider = newRange
                            , rangeDrag =
                                choose (newRange >= 0) (Just ( newAngle, offset )) Nothing
                            , rangeKnobAngle = newAngle
                        }

                      else
                        model
                    , Cmd.none
                    )

        RangeRelease offset ->
            ( { model
                | rangeDrag = Nothing
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
            ( { model
                | storedAzimuthRange = Nothing
                , storedAzimuth = Nothing
                , storedElevation = Nothing
                , storedElevationRange = Nothing
                , inputState = BearingInput
                , goniometerMode = Azimuth
              }
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
                    trainingPage model
    in
    { title = "Chain Home receiver emulation"
    , body =
        [ layout
            [ Background.color flatMidnightBlue
            , width fill
            ]
          <|
            column [ E.width E.fill, spacingXY 0 20 ]
                [ navBar
                , content
                ]
        ]
    }


navBar : Element Msg
navBar =
    let
        navItem label action =
            el
                ([ pointer
                 , Event.onClick action
                 ]
                    ++ disableSelection
                )
            <|
                text label
    in
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color paletteSand
        , Font.color paletteLightGreen
        , spaceEvenly
        ]
        [ navItem "Introduction" DisplayTraining
        , navItem "Configuration" DisplayConfiguration
        , navItem "Operator" DisplayReceiver
        , navItem "Calculator" DisplayCalculator
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
        model.storedAzimuthRange
        model.storedAzimuth
        model.storedElevation
        model.storedStrength
        model.storedStrengthPlus
        model.storedFriendly



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
            el [ width <| fillPortion 4, height fill, Event.onClick ToggleMenu ] none
    in
    if model.isMenuOpen then
        row [ width fill, height fill ] [ overlay, panel ]

    else
        none
