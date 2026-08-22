module Main exposing (main)

{-
   This Main module now based on the elm-ui example by Alex Korban
   in his book "Practical Elm"".
-}

import AboutPage exposing (aboutPage)
import Attr exposing (..)
import Browser
import Browser.Events exposing (..)
import Calculator.Model as Calculator exposing (..)
import Calculator.View
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
import Map.View exposing (mapPage)
import Messages exposing (..)
import Model exposing (..)
import Platform.Cmd exposing (Cmd)
import Random
import Range exposing (rangeTurnAngle)
import Receiver exposing (goniometerMix)
import Receiver.OperatorPage exposing (operatorPage)
import Spherical exposing (newPosition)
import SplashPage exposing (splashPage)
import Station exposing (..)
import Target exposing (..)
import Task
import Time
import Types exposing (..)
import Utils exposing (..)


init : Flags -> ( Model.Model, Cmd Msg )
init _ =
    ( { currPage = AboutPage
      , webGLtime = 0.0
      , modelTime = 0
      , startTime = 0
      , azimuthModeTrace = []
      , elevation_A_trace = []
      , elevation_B_trace = []
      , station = bawdsey
      , targets = []
      , inRangeTargets = []
      , echoes = []
      , skyline = []
      , goniometerAzimuth = degrees 40 -- relative to Line Of Shoot.
      , gonioOutput = []
      , keys = noKeys
      , gonioDrag = Nothing
      , rangeSlider = 50.0
      , outputDevice = { class = Desktop, orientation = Landscape }
      , rangeDrag = Nothing
      , rangeKnobAngle = 0.0
      , goniometerMode = Azimuth
      , transmitAntenna = transmitANoReflect
      , reflector = False
      , receiveAB = True
      , receiveAntenna = receiveHigh
      , explainModeMenu = False
      , explainModeReceiver = False
      , explainModeMap = False
      , timeForNextRaid = Nothing
      , storedPlots = []
      , gameMode = GameNone
      , isMenuOpen = False
      , calculator = Calculator.init
      , actualTraceVisibleOnMap = False
      , rangeCircleVisibleOnMap = False
      , fullScreenCRT = False
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


subscriptions : Model.Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.none
        , Time.every 40 UpdateModel
        , onAnimationFrameDelta TimeDelta
        , onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        , onResize (\w h -> DeviceResize w h)
        ]


deriveModelAtTime : Model.Model -> Int -> Model.Model
deriveModelAtTime model timeNow =
    let
        eastOfGreenwich t =
            t.longitude > 0.0

        withinRange t =
            t.rangeInMetres <= 160000

        targetsNow =
            -- Where are they, based on origin, bearing, speed, time.
            model.targets
                |> List.map (targetAtTime model.station timeNow)
                |> List.filter eastOfGreenwich
                |> List.filter withinRange

        echoSignals =
            -- Deduce echo based on transmitter characteristics.
            deriveEchoes model.station model.transmitAntenna targetsNow

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
            goniometerMix (model.goniometerAzimuth + model.station.lineOfShoot) receiveSignals

        newRangeSliderPosition =
            slideRangeSlider model.rangeSlider model.keys

        newRangeKnobPosition =
            -- Map 0..100 onto -pi..+pi
            (newRangeSliderPosition - 50) * degrees 175 / 50
    in
    { model
        | modelTime = timeNow
        , targets = targetsNow
        , inRangeTargets = targetsNow
        , echoes = echoSignals
        , gonioOutput = gonioOutput
        , azimuthModeTrace = gonioOutput
        , goniometerAzimuth = swingGoniometer model.goniometerAzimuth model.keys
        , rangeSlider = newRangeSliderPosition
        , rangeKnobAngle = newRangeKnobPosition
        , elevation_A_trace = heightMode_A_Outputs
        , elevation_B_trace = heightMode_B_Outputs
    }


update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
    let
        requestRandomRaid =
            Random.generate RandomRaidGenerated <|
                Random.pair (Random.float -(degrees 45) (degrees 45)) (Random.float 5 30)
    in
    case msg of
        ToggleFullScreenCRT ->
            ( { model | fullScreenCRT = not model.fullScreenCRT }
            , Cmd.none
            )

        TimeDelta dt ->
            ( { model | webGLtime = model.webGLtime + dt / 100.0 }
            , Cmd.none
            )

        SetStartTime time ->
            ( { model
                | startTime = Time.posixToMillis time
                , targets = []
              }
            , Cmd.none
            )

        ToggleMenu menuState ->
            ( { model | isMenuOpen = menuState }
            , Cmd.none
            )

        StartScenario gameMode ->
            ( { model
                | currPage = OperatorPage
                , gameMode = gameMode
                , targets = .targets <| sharonMode model.modelTime
              }
            , Cmd.none
              --requestRandomRaid
            )

        UpdateModel time ->
            let
                raidIsDue =
                    case model.timeForNextRaid of
                        Nothing ->
                            False

                        Just due ->
                            Time.posixToMillis time > due

                resetRaidDue m =
                    if raidIsDue then
                        { m | timeForNextRaid = Nothing }

                    else
                        m
            in
            ( deriveModelAtTime model (Time.posixToMillis time)
                |> resetRaidDue
            , if raidIsDue then
                requestRandomRaid

              else
                Cmd.none
            )

        ExplainModeToggle ->
            ( case model.currPage of
                InputPage ->
                    { model | explainModeMenu = not model.explainModeMenu }

                OperatorPage ->
                    { model | explainModeReceiver = not model.explainModeReceiver }

                CalculatorPage ->
                    { model | calculator = toggleExplainMode model.calculator }

                MapPage ->
                    { model | explainModeMap = not model.explainModeMap }

                _ ->
                    model
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
                | currPage = CalculatorPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        DisplayAboutPage ->
            ( { model
                | currPage = AboutPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        DisplayMapPage ->
            ( { model
                | currPage = MapPage
                , isMenuOpen = False
              }
            , Cmd.none
            )

        KeyChanged isDown key ->
            ( { model | keys = updateKeys isDown key model.keys }
            , Cmd.none
            )

        GonioGrab offset ->
            ( { model | gonioDrag = Just ( model.goniometerAzimuth, offset ) }
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

        GonioRelease _ ->
            ( { model | gonioDrag = Nothing }
            , Cmd.none
            )

        RangeGrab offset ->
            ( { model | rangeDrag = Just ( model.rangeKnobAngle, offset ) }
            , Cmd.none
            )

        RangeMove offset ->
            case model.rangeDrag of
                Nothing ->
                    ( model, Cmd.none )

                Just ( startAngle, startXY ) ->
                    let
                        stopKnob =
                            -- Don't let the knob go down to 6 o'clock as that's ambiguous.
                            degrees 175

                        -- Prevent complete rotation.
                        newAngle =
                            clamp (0 - stopKnob) stopKnob <|
                                rangeTurnAngle startAngle startXY offset

                        newSliderPosition =
                            -- Map accessible rotation ( -175 deg .. + 175 deg ) onto 0 .. 100
                            normalise newAngle * 50 / stopKnob + 50
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

        RangeRelease _ ->
            ( { model | rangeDrag = Nothing }
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
                | goniometerMode = mode
                , calculator =
                    setInputState
                        (case mode of
                            Azimuth ->
                                BearingInput

                            Elevation ->
                                HeightInput
                        )
                        model.calculator
              }
            , Cmd.none
            )

        --TODO: Perhaps these should be Calculator Messages.
        StoreGoniometerSetting ->
            ( { model
                | calculator =
                    case model.calculator.inputState of
                        BearingInput ->
                            storeAzimuth (model.goniometerAzimuth + model.station.lineOfShoot) <|
                                setInputState BearingRangeInput <|
                                    model.calculator

                        HeightInput ->
                            storeElevation (findTargetHeight model.targets model.rangeSlider) <|
                                setInputState HeightRangeInput <|
                                    model.calculator

                        _ ->
                            model.calculator
              }
            , Cmd.none
            )

        StoreRangeSetting ->
            ( (case model.calculator.inputState of
                BearingRangeInput ->
                    { model
                        | calculator =
                            storeAzimuthRange (1.6 * model.rangeSlider) <|
                                setInputState BearingInput model.calculator
                    }
                        |> recordCurrentTargetPositions

                HeightRangeInput ->
                    { model
                        | calculator =
                            storeElevationRange (1.6 * model.rangeSlider) <|
                                setInputState BearingInput model.calculator
                        , goniometerMode = Azimuth
                    }

                _ ->
                    model
              )
                |> saveNewPlot
            , Cmd.none
            )

        RaidStrength strength ->
            ( { model | calculator = storeStrength strength model.calculator }
            , Cmd.none
            )

        RaidStrengthPlus ->
            ( { model | calculator = storeStrengthPlus True model.calculator }
            , Cmd.none
            )

        RaidFriendly ->
            ( { model | calculator = storeFriendly True model.calculator }
            , Cmd.none
            )

        ResetInputState ->
            ( { model | calculator = Calculator.init }
            , Cmd.none
            )

        RandomRaidGenerated ( bearing, height ) ->
            ( makeNewTarget ( bearing, height ) model
            , Cmd.none
            )

        SetActualTraceVisible visible ->
            ( { model | actualTraceVisibleOnMap = visible }
            , Cmd.none
            )

        SetRangeCircleVisible visible ->
            ( { model | rangeCircleVisibleOnMap = visible }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


saveNewPlot : Model.Model -> Model.Model
saveNewPlot model =
    -- After RANGE is stored, if we have bearing and range, add to history.
    -- Note that the stored value is in km, here we convert to meters.
    let
        newPlot range theta =
            { range = range * 1000
            , bearing = theta
            , time = model.modelTime
            , plotType = UserPlot
            , strength = 1
            }
    in
    case ( model.calculator.storedAzimuth, model.calculator.storedAzimuthRange ) of
        ( Just theta, Just range ) ->
            { model | storedPlots = newPlot range theta :: model.storedPlots }

        _ ->
            model


makeNewTarget : ( Float, Float ) -> Model.Model -> Model.Model
makeNewTarget ( bearing, height ) model =
    -- Create raids along an arc about 100 miles away.
    -- This assumes 90degE line of shoot!
    let
        station =
            model.station

        ( newLat, newLong ) =
            newPosition
                ( station.latitude, station.longitude )
                159000
                (bearing + station.lineOfShoot)

        raidSequence =
            List.length model.targets

        raidStrength =
            max 1 <|
                min 99 <|
                    round <|
                        (pseudoRandom * toFloat (1 + raidSequence * raidSequence))

        pseudoRandom =
            fractional <| 5000 * sin (toFloat model.modelTime)

        hostileProforma : TargetProforma
        hostileProforma =
            { latitude = newLat
            , longitude = newLong
            , height = height
            , heading = station.lineOfShoot - pi + (0.5 - pseudoRandom) * degrees 15
            , speed = 200 + pseudoRandom * 100
            , strength = raidStrength
            , iff = Nothing
            }

        -- Mark some small raids as friendly.
        makeProforma =
            if pseudoRandom < 0.1 then
                { hostileProforma
                    | iff = Just (modBy 12 (round model.webGLtime))
                    , strength = 1
                }

            else
                hostileProforma

        newTarget =
            targetFromProforma
                model.station
                model.modelTime
                makeProforma
    in
    { model
        | targets =
            if List.length model.targets < 14 then
                newTarget :: model.targets

            else
                model.targets
        , timeForNextRaid =
            -- Even if we were full up, keep trying as some may go out of range.
            Just <| model.modelTime + (round <| 40000.0 + 20000.0 * pseudoRandom)
    }


view : Model.Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                OperatorPage ->
                    operatorPage model

                InputPage ->
                    inputPage model

                CalculatorPage ->
                    Calculator.View.view model.outputDevice model.calculator

                AboutPage ->
                    aboutPage

                SplashPage ->
                    splashPage

                MapPage ->
                    mapPage model
    in
    { title = "Chain Home receiver emulation"
    , body =
        [ layout
            [ Background.color flatMidnightBlue
            , width fill
            , htmlAttribute <| style "touch-action" "manipulation"
            ]
          <|
            column
                [ E.width E.fill ]
                [ navBar model
                , content
                ]
        ]
    }


navItem label action isActive =
    let
        commonStyles =
            [ pointer
            , Event.onClick action
            , padding 5
            , Border.color paletteSand
            , Border.width 2
            , Border.rounded 6
            , width fill
            ]

        activeStyles =
            if isActive then
                [ Background.color flatMidnightBlue
                , Border.widthEach { edges | top = 2 }
                , Font.color white
                ]

            else
                [ Font.color lightGray
                , Border.widthEach { edges | bottom = 2, left = 2, right = 2 }
                ]
    in
    row
        (commonStyles
            ++ disableSelection
            ++ activeStyles
        )
        [ el [ centerX ] <|
            text label
        , el
            [ E.height (px 30)
            ]
            none
        ]


navBar : Model.Model -> Element Msg
navBar model =
    let
        isActive x =
            model.currPage == x
    in
    row
        [ width fill
        , Background.color paletteGrey
        , paddingEach { edges | top = 5, bottom = 5 }
        , spaceEvenly
        ]
        [ navItem "About" DisplayAboutPage (isActive AboutPage)
        , navItem "Mode selection" DisplayConfiguration (isActive InputPage)
        , navItem "Receiver" DisplayReceiver (isActive OperatorPage)
        , navItem "Calculator" DisplayCalculator (isActive CalculatorPage)
        , navItem "Map" DisplayMapPage (isActive MapPage)
        ]


targetSelector : List Scenario -> Element Msg
targetSelector scenarios =
    let
        display : Scenario -> Element Msg
        display g =
            row
                [ spacing 10
                , width fill
                ]
                [ Input.button
                    Attr.greenButton
                    { onPress = Nothing
                    , label = el [ centerX ] <| text g.description
                    }
                ]
    in
    column
        [ Font.color lightCharcoal
        , spacing 20
        , width fill
        ]
    <|
        List.map
            display
            scenarios


inputPage : Model.Model -> Element Msg
inputPage model =
    row
        [ E.width fill
        , Font.color lightCharcoal
        , paddingEach { edges | top = 60, left = 60, right = 60 }
        , spacing 30
        ]
        [ column
            [ padding 20
            , width <| fillPortion 3
            , alignTop
            , spacing 20
            ]
            [ textHeading "Practice"
            , blurb "Explore common scenarios."
            , targetSelector (Config.scenarioList model.modelTime)
            ]
        , column
            [ padding 20
            , width <| fillPortion 3
            , spacing 20
            ]
            [ textHeading "Test your skills"
            , blurb "Plot raids as they appear. Expect it to become increasingly busy."
            , blurb "Check the Map page occasionally to see how well you are doing."
            , Input.button
                Attr.greenButton
                { onPress = Just <| StartScenario GameUnlimited
                , label = el [ centerX ] <| text "BEGIN NEW SESSION"
                }
            , el [ height (px 10) ] none
            ]
        ]


main : Program Flags Model.Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


recordCurrentTargetPositions : Model.Model -> Model.Model
recordCurrentTargetPositions model =
    { model | targets = List.map (recordCurrentTargetPosition model.modelTime) model.targets }
