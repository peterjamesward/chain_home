module Main exposing (main)

{-
   This Main module now based on the elm-ui example by Alex Korban
   in his book "Practical Elm"".
-}

import Attr exposing (..)
import BeamSmoother exposing (beamPath, scalePathToDisplay)
import Browser
import Browser.Events exposing (..)
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
import Goniometer exposing (drawGoniometer, goniometerTurnAngle)
import Json.Decode as D exposing (..)
import LobeFunctions exposing (..)
import Messages exposing (..)
import OperatorPage exposing (operatorPage)
import Receiver exposing (goniometerMix)
import Skyline exposing (EdgeSegment, deriveSkyline)
import Station exposing (..)
import Target exposing (..)
import Task
import Time exposing (..)
import Types exposing (..)
import Utils exposing (..)


type Page
    = InputPage
    | OperatorPage
    | OutputPage


type alias Model =
    { currPage : Page
    , zone : Time.Zone
    , startTime : Maybe Time.Posix
    , time : Time.Posix -- milliseconds from t-zero
    , azimuthModeTrace : List Point
    , elevation_A_trace : List Point
    , elevation_B_trace : List Point
    , station : Station
    , targets : List Target
    , movedTargets : List Target
    , polarTargets : List PolarTarget
    , echoes : List Echo
    , skyline : List Line
    , goniometerAzimuth : Angle
    , gonioOutput : List Echo
    , keys : Keys
    , gonioDrag : Maybe ( Angle, Point ) -- angle and mouse position when mouse down
    , activeConfigurations : List TargetSelector
    , rangeSlider : Range
    , outputDevice : Device
    , rangeDrag : Maybe ( Angle, Point )
    , rangeKnobAngle : Angle
    , goniometerMode : GoniometerMode
    , transmitAntenna : Antenna
    , reflector : Bool
    , isMenuOpen : Bool
    , receiveAB : Bool
    , receiveAntenna : Antenna
    , inputState : InputState
    , storedAzimuth : Maybe Float
    , storedElevation : Maybe Float
    , storedAzimuthRange : Maybe Float
    , storedElevationRange : Maybe Float
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { currPage = InputPage
      , zone = Time.utc
      , startTime = Nothing
      , time = Time.millisToPosix 0
      , azimuthModeTrace = beamPath []
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
      }
    , Task.perform AdjustTimeZone Time.here
    )


type alias Keys =
    -- Keep track of any significant keys' state, such as for adjusting goniometer or range slider.
    { gonioClock : Bool -- A
    , gonioAnti : Bool -- Q
    , rangeLeft : Bool -- left arrow
    , rangeRight : Bool -- right arrow
    }


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 100 Tick
        , onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        , onResize (\w h -> DeviceResize w h)
        ]



-- UPDATE


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


deriveModelAtTime : Model -> Time.Posix -> Model
deriveModelAtTime model t =
    let
        effectiveStartTime =
            -- TODO: Embellish this so we can vary the model speed.
            Maybe.withDefault model.time model.startTime

        targetsNow =
            -- Where are they, based on origin, bearing, speed, time.
            List.map (targetAtTime t effectiveStartTime) model.targets

        convertedTargets =
            -- Easier to work in polar coordinates here on.
            List.map (mapToPolar bawdsey) targetsNow

        echoSignals =
            -- Deduce echo based on transmitter characteristics.
            deriveEchoes convertedTargets model.transmitAntenna (Time.posixToMillis t)

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
                |> deriveSkyline (Time.posixToMillis model.time) (scaleWidthKilometers * 1000)
                |> beamPath
                |> scalePathToDisplay

        heightMode_B_Outputs =
            bElevationAdjustedEchoes model.goniometerAzimuth echoSignals
                |> deriveSkyline (Time.posixToMillis model.time) (scaleWidthKilometers * 1000)
                |> beamPath
                |> scalePathToDisplay

        gonioAzimuthOut =
            -- 'Blend' X and Y inputs to find target's azimuth.
            goniometerMix model.goniometerAzimuth receiveSignals

        newSkyline =
            -- Work out what to show on the CRT trace.
            deriveSkyline (Time.posixToMillis model.time) (scaleWidthKilometers * 1000) gonioAzimuthOut
    in
    { model
        | startTime =
            case model.startTime of
                --TODO: Get rid of this somehow.
                Nothing ->
                    Just t

                _ ->
                    model.startTime
        , time = t
        , targets = getAllTargets model.activeConfigurations
        , movedTargets = targetsNow
        , polarTargets = convertedTargets
        , echoes = echoSignals
        , skyline = newSkyline
        , gonioOutput = gonioAzimuthOut
        , azimuthModeTrace = scalePathToDisplay <| beamPath newSkyline
        , goniometerAzimuth = swingGoniometer model.goniometerAzimuth model.keys
        , rangeSlider = slideRangeSlider model.rangeSlider model.keys
        , elevation_A_trace = heightMode_A_Outputs
        , elevation_B_trace = heightMode_B_Outputs
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
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
                , isMenuOpen = False
              }
            , Cmd.none
            )

        DisplayConfiguration ->
            ( { model
                | currPage = InputPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        DisplayCalculator ->
            ( { model
                | currPage = OutputPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        Tick newTime ->
            ( deriveModelAtTime model newTime
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
                        | storedElevation = Just (model.goniometerAzimuth + model.station.lineOfShoot)
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
                        | storedAzimuthRange = Just model.rangeSlider
                        , inputState = BearingInput
                    }

                HeightRangeInput ->
                    { model
                        | storedElevationRange = Just model.rangeSlider
                        , inputState = HeightInput
                    }

                _ ->
                    model
            , Cmd.none
            )

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



-- VIEW


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
    in
    { title = "Chain Home emulation"
    , body =
        [ layout
            [ Background.color flatMidnightBlue
            , inFront <| menuPanel model
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
    row
        [ width fill
        , paddingXY 60 10
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color blue
        ]
        [ el
            [ alignLeft
            , Font.color paletteSand
            ]
          <|
            text "Chain Home"
        , Input.button
            (Attr.greenButton
                ++ [ padding 5, alignRight, width (px 80) ]
            )
            { onPress = Just ToggleMenu
            , label = el [ centerX ] <| text "Menu"
            }
        ]



-- Show list of configurations with Checkboxes.
-- This will now be on its own page with elm-ui.


setConfig : TargetSelector -> Bool -> Msg
setConfig selector newState =
    SetConfigStateMsg selector.id newState


targetSelector : List TargetSelector -> List (Element Msg)
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
                    Input.labelRight [] <|
                        E.text g.description
                , icon = Input.defaultCheckbox
                }
    in
    List.map display active


calculatorPage : Model -> Element Msg
calculatorPage model =
    row (padding 50 :: spacing 50 :: commonStyles)
        [ bearingDisplay "AZIMUTH" model.storedAzimuth
        , numericDisplay "RANGE (Azimuth)" model.storedAzimuthRange
        , bearingDisplay "ELEVATION" model.storedElevation
        , numericDisplay "RANGE (Elevation)" model.storedElevationRange
        ]


inputPage : Model -> Element Msg
inputPage model =
    column
        [ E.width (px 500)
        , spacingXY 0 10
        , centerX
        , Font.color lightCharcoal
        ]
    <|
        targetSelector model.activeConfigurations
            ++ [ Input.button
                    (Attr.greenButton ++ [ width (px 200), height (px 40), alignRight ])
                    { onPress = Just DisplayReceiver
                    , label = el [ centerX ] <| text "Go!"
                    }
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
