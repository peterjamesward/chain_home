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
import Receiver.OperatorPage exposing (operatorPage, operatorPageWithTutorial)
import Spherical exposing (newPosition)
import SplashPage exposing (splashPage)
import Station exposing (..)
import Target exposing (..)
import Task
import Time
import Tutorials.ActionCodes exposing (TutorialScenario(..))
import Tutorials.Actions exposing (..)
import Tutorials.KioskModeTutorial exposing (kioskModeTutorial)
import Tutorials.Messages as TutorialMsg exposing (TutorialMsg(..))
import Tutorials.Tutorials exposing (tutorialAutomation)
import Tutorials.Update
import Tutorials.Views exposing (tutorialText, viewCalculatorInTutorial)
import Types exposing (..)
import Utils exposing (..)
import Zipper


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
      , activeConfigurations = availableTargetOptions
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
      , newRaid = Nothing
      , timeForNextRaid = Nothing
      , storedPlots = []
      , gameMode = GameNone
      , isMenuOpen = False
      , calculator = Calculator.init
      , actualTraceVisibleOnMap = False
      , rangeCircleVisibleOnMap = False
      , applicationMode = InteractiveMode
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


deriveModelAtTime : Model.Model -> Int -> Model.Model
deriveModelAtTime model timeNow =
    let
        targetsNow =
            -- Where are they, based on origin, bearing, speed, time.
            List.map (targetAtTime model.station timeNow) model.targets

        inRangeTargets =
            -- Easier to work in polar coordinates here on.
            -- Filter removes raid beyond our range.
            List.filter (\tgt -> tgt.rangeInMetres < 100 * 1600) targetsNow

        echoSignals =
            -- Deduce echo based on transmitter characteristics.
            deriveEchoes model.station model.transmitAntenna inRangeTargets

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

        ( newMode, actionCodeList ) =
            case model.applicationMode of
                InteractiveMode ->
                    ( InteractiveMode, [] )

                TutorialMode tutorial ->
                    let
                        ( newTut, actions ) =
                            tutorialAutomation tutorial
                    in
                    ( TutorialMode newTut, actions )

                Model.KioskMode tutorial ticks ->
                    let
                        ( newTut, actions ) =
                            tutorialAutomation tutorial
                    in
                    ( Model.KioskMode newTut ticks, actions )

        postTutorialModel =
            applyTutorialActions actionCodeList preTutorialModel

        preTutorialModel =
            { model
                | modelTime = timeNow
                , targets = targetsNow
                , inRangeTargets = inRangeTargets
                , echoes = echoSignals
                , gonioOutput = gonioOutput
                , azimuthModeTrace = gonioOutput
                , goniometerAzimuth = swingGoniometer model.goniometerAzimuth model.keys
                , rangeSlider = newRangeSliderPosition
                , rangeKnobAngle = newRangeKnobPosition
                , elevation_A_trace = heightMode_A_Outputs
                , elevation_B_trace = heightMode_B_Outputs
            }
    in
    --Wrapping this in automation allows the Tutorials to do anything.
    { postTutorialModel | applicationMode = newMode }


clearHistory : Model.Model -> Model.Model
clearHistory model =
    { model | storedPlots = [] }


kioskAutomation : Model.Model -> Model.Model
kioskAutomation model =
    let
        howLongTheStringIs tut =
            String.length <| Maybe.withDefault "" <| tutorialText tut model

        advance =
            let
                ( newModel, _ ) =
                    update (TutorialMsg TutorialAdvance) model
            in
            startOverIfAtEnd newModel

        startOverIfAtEnd m =
            case m.applicationMode of
                Model.KioskMode (Just _) _ ->
                    m

                Model.KioskMode Nothing _ ->
                    { m
                        | applicationMode = Model.KioskMode kioskModeTutorial m.modelTime
                        , currPage = SplashPage
                    }

                _ ->
                    m
    in
    -- KioskTimer being not Nothing forces the looping demo.
    case model.applicationMode of
        Model.KioskMode tut lastTime ->
            if model.modelTime - lastTime > howLongTheStringIs tut * 80 then
                advance

            else
                model

        _ ->
            model


update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
    let
        cleanModel =
            model |> actionExitAction |> actionClearTargets |> actionClearCalculator |> clearHistory

        requestRandomRaid =
            Random.generate RandomRaidGenerated <|
                Random.pair (Random.float -(degrees 45) (degrees 45)) (Random.float 5 30)
    in
    case msg of
        Messages.KioskMode ->
            ( { model
                | applicationMode = Model.KioskMode kioskModeTutorial model.modelTime
                , currPage = SplashPage
              }
            , Cmd.none
            )

        TimeDelta dt ->
            ( { model | webGLtime = model.webGLtime + dt / 100.0 }
            , Cmd.none
            )

        SetStartTime time ->
            ( { model
                | startTime = Time.posixToMillis time
              }
            , Cmd.none
            )

        ToggleMenu menuState ->
            ( { model | isMenuOpen = menuState }
            , Cmd.none
            )

        StartScenario gameMode ->
            ( { cleanModel
                | currPage = OperatorPage
                , gameMode = gameMode

                -- Pseudo randomness will suffice at least for now.
                , timeForNextRaid =
                    Just <| model.modelTime + truncate (60000 * abs (sin <| toFloat model.modelTime))
              }
            , requestRandomRaid
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
            ( deriveModelAtTime (kioskAutomation model) (Time.posixToMillis time)
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

        SetConfigStateMsg index newState ->
            ( setTutorialCompletedState index newState model
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

        TutorialMsg tutMsg ->
            ( case model.applicationMode of
                TutorialMode tut ->
                    let
                        ( ts, acts ) =
                            Tutorials.Update.update tutMsg tut

                        newModel =
                            applyTutorialActions acts model
                    in
                    { newModel
                        | applicationMode = TutorialMode ts
                    }

                Model.KioskMode tut ticks ->
                    let
                        ( ts, acts ) =
                            Tutorials.Update.update tutMsg tut

                        newModel =
                            applyTutorialActions acts model
                    in
                    { newModel
                        | applicationMode =
                            Model.KioskMode ts <|
                                if ts == tut then
                                    ticks

                                else
                                    model.modelTime
                    }

                InteractiveMode ->
                    let
                        ( ts, acts ) =
                            Tutorials.Update.update tutMsg Nothing

                        newModel =
                            applyTutorialActions acts model
                    in
                    { newModel
                        | applicationMode =
                            case ts of
                                Just x ->
                                    TutorialMode (Just x)

                                _ ->
                                    InteractiveMode
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

        AdjustRangeValue newRange ->
            ( model
              --| rangeSlider = newRange }
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
            ( actionClearCalculator model
            , Cmd.none
            )

        RandomRaidGenerated ( bearing, height ) ->
            ( model |> makeNewTarget ( bearing, height ) |> setNextRandomRaidTime
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
            }
    in
    case ( model.calculator.storedAzimuth, model.calculator.storedAzimuthRange ) of
        ( Just theta, Just range ) ->
            { model | storedPlots = newPlot range theta :: model.storedPlots }

        _ ->
            model


setNextRandomRaidTime : Model.Model -> Model.Model
setNextRandomRaidTime model =
    -- If there are raids remaining for our current level, line the next one up here.
    let
        activeRaidCount =
            List.length model.targets

        scheduleRaid =
            { model | timeForNextRaid = Just <| model.modelTime + truncate (120000 * abs (sin <| toFloat model.modelTime)) }

        dontScheduleRaid =
            { model | timeForNextRaid = Nothing }
    in
    case ( model.gameMode, activeRaidCount ) of
        ( GameNone, _ ) ->
            dontScheduleRaid

        ( GameSingleRaid, _ ) ->
            dontScheduleRaid

        ( GameThreeRaids, n ) ->
            if n < 3 then
                scheduleRaid

            else
                dontScheduleRaid

        ( GameUnlimited, _ ) ->
            scheduleRaid


makeNewTarget : ( Float, Float ) -> Model.Model -> Model.Model
makeNewTarget ( bearing, height ) model =
    -- Create raids along a line of longitude about 100 miles away.
    -- This assumes 90degE line of shoot!
    let
        station =
            model.station

        ( newLat, newLong ) =
            newPosition ( station.latitude, station.longitude ) 160000 (bearing + station.lineOfShoot)

        pseudoRandom =
            fractional <| 5000 * sin (toFloat model.modelTime)

        heading =
            -- Som pseudo randomness in raid heading, which we try to
            -- make interesting in as much as it will pass near the station.
            degrees 270 + pseudoRandom * degrees 30

        hostileSingle : TargetProforma
        hostileSingle =
            { latitude = newLat
            , longitude = newLong
            , height = height
            , heading = heading
            , speed = 300 -- Quicker testing!
            , strength = 1
            , iff = Nothing
            }

        raidIfSelected scenario raidType =
            raidType

        hostileMultiple n =
            if n > 1 then
                hostileSingle :: hostileMultiple (n - 1)

            else
                [ hostileSingle ]

        friendlyRaid =
            [ { hostileSingle | iff = Just (modBy 12 (round model.webGLtime)) } ]

        raidDistribution =
            case modBy model.modelTime 10 of
                0 ->
                    raidIfSelected ScenarioFriendly friendlyRaid

                1 ->
                    raidIfSelected ScenarioTwoTogether (hostileMultiple 2)

                2 ->
                    raidIfSelected ScenarioTwoTogether (hostileMultiple 2)

                3 ->
                    raidIfSelected ScenarioThreeToSix (hostileMultiple 3)

                4 ->
                    raidIfSelected ScenarioThreeToSix (hostileMultiple 4)

                5 ->
                    raidIfSelected ScenarioThreeToSix (hostileMultiple 5)

                9 ->
                    raidIfSelected ScenarioThreeToSix (hostileMultiple 16)

                _ ->
                    [ hostileSingle ]

        newTargets =
            List.map
                (targetFromProforma
                    model.station
                    model.modelTime
                )
                raidDistribution
    in
    { model
        | targets = newTargets ++ model.targets
        , newRaid = List.head newTargets
    }


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


view : Model.Model -> Browser.Document Msg
view model =
    let
        content =
            case model.currPage of
                OperatorPage ->
                    operatorPage model

                OperatorPageInTutorial ->
                    case model.applicationMode of
                        TutorialMode tut ->
                            operatorPageWithTutorial tut model

                        Model.KioskMode tut _ ->
                            operatorPageWithTutorial tut model

                        InteractiveMode ->
                            operatorPage model

                InputPage ->
                    inputPage model

                CalculatorPage ->
                    Calculator.View.view model.outputDevice model.calculator

                CalculatorInTutorial ->
                    viewCalculatorInTutorial model

                TrainingPage ->
                    operatorPage model

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
                [ E.width E.fill
                ]
                [ case model.applicationMode of
                    Model.KioskMode _ _ ->
                        row [ height (px 40) ]
                            []

                    _ ->
                        navBar model
                , content
                ]
        ]
    }


navItem model label action pageId =
    let
        activeStyles =
            if model.currPage == pageId then
                [ Background.color flatMidnightBlue
                , Border.color paletteSand
                , Border.width 2
                , Border.rounded 3
                , Border.widthEach { edges | top = 2 }
                , Font.color white
                , Font.bold
                , width fill
                , centerX
                ]

            else
                [ Font.color lightGray
                , width fill
                , Border.color paletteSand
                , Border.width 2
                , Border.rounded 3
                , Border.widthEach { edges | bottom = 2, left = 2, right = 2 }
                ]
    in
    el
        ([ pointer
         , Event.onClick action
         , padding 5
         ]
            ++ disableSelection
            ++ activeStyles
        )
    <|
        el [ centerX ] <|
            text label


navBar : Model.Model -> Element Msg
navBar model =
    row
        [ width fill
        , Background.color paletteGrey
        , paddingEach { edges | top = 5, bottom = 5 }
        , spaceEvenly
        ]
        [ navItem model "About" DisplayAboutPage AboutPage
        , navItem model "Mode selection" DisplayConfiguration InputPage
        , navItem model "Receiver" DisplayReceiver OperatorPage
        , navItem model "Calculator" DisplayCalculator CalculatorPage
        , navItem model "Map" DisplayMapPage MapPage
        ]



-- Show list of configurations with Checkboxes.
-- This will now be on its own page with elm-ui.
-- Entries are ticked if the relevant tutorial is done, but can be ticked (& un-ticked) manually.


setConfig : TargetSelector -> Bool -> Msg
setConfig selector newState =
    SetConfigStateMsg selector.id newState


setTutorialCompletedState : TutorialScenario -> Bool -> Model.Model -> Model.Model
setTutorialCompletedState scenario state model =
    -- Functionality removed 2020-09-28
    model


targetSelector : List TargetSelector -> Element Msg
targetSelector availableRaidTypes =
    let
        display : TargetSelector -> Element Msg
        display g =
            row
                [ spacing 10
                , width fill
                ]
                [ Input.button
                    Attr.greenButton
                    { onPress = Just <| TutorialMsg (DisplayTraining g.id)
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
            availableRaidTypes


inputPageLandscape : Model.Model -> Element Msg
inputPageLandscape model =
    let
        buttonAction scenario =
            Just (StartScenario scenario)

        heading txt =
            paragraph
                [ width fill
                , Font.color white
                , Font.size 24
                , centerX
                ]
                [ text txt ]
    in
    row
        [ E.width fill
        , Font.color lightCharcoal
        , paddingEach { edges | top = 60, left = 60, right = 60 }
        , spacing 30
        ]
        [ column
            ([ padding 20
             , width <| fillPortion 3
             , alignTop
             , spacing 20
             ]
                ++ showExplanation model.explainModeMenu explainRaidTypes
            )
            [ heading "Tutorials"
            , targetSelector model.activeConfigurations
            , heading "Demonstration mode"
            , text """Clicking the button below will put the application into
demonstration mode. It will loop constantly until reloaded."""
            , Input.button
                Attr.greyButton
                { onPress = Just Messages.KioskMode
                , label = el [ centerX ] <| text "Switch to demo mode"
                }
            ]
        , column
            ([ padding 20
             , width <| fillPortion 3
             , spacing 20
             ]
                ++ showExplanation model.explainModeMenu explainPlayLevels
            )
            [ heading "Test your skills"
            , Input.button
                Attr.greenButton
                { onPress = buttonAction GameSingleRaid
                , label = el [ centerX ] <| text "One practice raid"
                }
            , Input.button
                Attr.greenButton
                { onPress = buttonAction GameThreeRaids
                , label = el [ centerX ] <| text "Three practice raids"
                }
            , Input.button
                Attr.greenButton
                { onPress = buttonAction GameUnlimited
                , label = el [ centerX ] <| text "Unlimited raids"
                }
            ]
        , column [ padding 10, alignRight, alignTop, width <| fillPortion 1 ] [ helpButton ]
        ]


inputPagePortrait : Model.Model -> Element Msg
inputPagePortrait model =
    column
        [ E.width fill
        , Font.color lightCharcoal
        , paddingEach { edges | left = 50, right = 50, top = 40 }
        , spacing 20
        ]
        [ helpButton
        , targetSelector
            model.activeConfigurations
        , el
            (width fill
                :: showExplanation model.explainModeMenu explainPlayLevels
            )
            (text "Hello world.")
        , Input.button
            (Attr.greenButton ++ [ width (px 200), height (px 40), centerX ])
            { onPress = Just (StartScenario GameSingleRaid)
            , label = el [ centerX ] <| text "One practice raid"
            }
        , Input.button
            (Attr.greyButton ++ [ width (px 200), height (px 40), centerX ])
            { onPress = Just (StartScenario GameThreeRaids)
            , label = el [ centerX ] <| text "Three practice raids"
            }
        , Input.button
            (Attr.greyButton ++ [ width (px 200), height (px 40), centerX ])
            { onPress = Just (StartScenario GameUnlimited)
            , label = el [ centerX ] <| text "Unlimited raids"
            }
        ]


inputPage model =
    case model.outputDevice.orientation of
        Landscape ->
            inputPageLandscape model

        Portrait ->
            inputPagePortrait model


main : Program Flags Model.Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


explainRaidTypes =
    """
Click the tutorial buttons to learn how to recognise the common types of raids.
    """


explainPlayLevels : String
explainPlayLevels =
    """
Raids will come from different directions, at different heights, and on different headings.

You should make several plots for each raid so that Fighter Command can work out
where the raid is heading. You will be able to see how you perform by looking at the Map.
    """


recordCurrentTargetPositions : Model.Model -> Model.Model
recordCurrentTargetPositions model =
    { model | targets = List.map (recordCurrentTargetPosition model.modelTime) model.targets }
