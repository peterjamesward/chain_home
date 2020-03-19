module TrainingMode exposing (advanceTutorial, clearCalculator, exitTutorial, explanatoryText, goBackInTutorial, tutorialAutomation, tutorialStartScenario, tutorialTextBox)

import Config exposing (targetConfigurations, trainingMode)
import Constants exposing (blue, flatSunflower, white)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Grid exposing (tutorialInterpretCalculator)
import Keys exposing (Keys, updateKeys)
import Messages exposing (..)
import Model exposing (Model)
import Target exposing (findTargetElevation)
import Types exposing (..)



-- Better structure is that Tutorial~ information is local and
-- UI components don't know what will be displayed. This means that
-- we need a component identifier and we will use a type for that.


type alias TutorialEntry =
    { tutorialStep : TutorialStep -- The unique step identifier
    , uiComponent : UiComponent -- The UI component to be highlighted
    , entryActions : TutorialActionList -- Changes to the model so we can be idempotent
    , stateActions : TutorialActionList -- Things we must do whilst in this state.
    , exitActions : TutorialActionList -- Changes to the model, to make sure we exit cleanly
    , tutorialText :
        Model
        -> String -- What we display to the user.
    }


type alias Tutorial =
    List TutorialEntry


tutorialCloseStep : TutorialEntry
tutorialCloseStep =
    -- Intended to be final step in any tutorial.
    TutorialEntry
        TutorialDummy
        UiDummy
        [ tutorialExitAction ]
        noStateActions
        [ clearCalculator ]
        (static "No text.")


static : String -> (Model -> String)
static s =
    -- As we now allow derived tutorial strings, we need a way to declare a fixed string.
    \_ -> s


tutorialTBD : List TutorialEntry
tutorialTBD =
    [ TutorialEntry
        TutorialWelcome
        UiDummy
        noEntryActions
        noStateActions
        noExitActions
        (static
            """Ask Pete to write this tutorial.
        """
        )
    , tutorialCloseStep
    ]


tutorialBasic : List TutorialEntry
tutorialBasic =
    [ TutorialEntry
        TutorialWelcome
        UiCRT
        [ tutorialBearingMode, clearCalculator ]
        [ tutorialGoniometerSwinging ]
        noExitActions
        (static
            """The operator is turning the gonio, looking for any sign of a signal.
        Click ► to begin.
        """
        )
    , TutorialEntry
        TutorialIncomingRaid
        UiCRT
        [ setupTutorialRaid ]
        noStateActions
        noExitActions
        (static
            """The white V shape under the 100 on the 'tube' is a new raid.
        Click ► to see the operator start to examine the raid.
        """
        )
    , TutorialEntry
        TutorialAdjustRange
        UiRangeKnob
        noEntryActions
        [ chaseTheRaidRange True ]
        [ chaseTheRaidRange False ]
        (static
            """The operator turns the range knob until the pointer
        lines up with the left edge of the raid on the CRT.
        """
        )
    , TutorialEntry
        TutorialFindBearing
        UiGoniometer
        noEntryActions
        [ swingThatGoniometer True ]
        [ swingThatGoniometer False ]
        (static
            """The operator 'swings' the gonio until the V on the CRT vanishes.
        """
        )
    , TutorialEntry
        TutorialStoreBearing
        UiGonioButton
        noEntryActions
        noStateActions
        [ tutorialStoreBearing ]
        (static
            """Pressing the GONIO button stores the bearing in the calculator.
        """
        )
    , TutorialEntry
        TutorialStoreRange1
        UIRangeButton
        noEntryActions
        noStateActions
        [ tutorialStoreRange1 ]
        (static
            """Pressing the RANGE button stores the range in the calculator.
        """
        )
    , TutorialEntry
        TutorialHeightMode
        UiHeight
        [ tutorialHeightMode ]
        noStateActions
        noExitActions
        (static
            """The operator will now try to work out the height.
        """
        )
    , TutorialEntry
        TutorialFindElevation
        UiGoniometer
        noEntryActions
        [ tutorialSeekElevation True ]
        [ tutorialSeekElevation False ]
        (static
            """The operator swings the gonio again, to minimise the V.
        """
        )
    , TutorialEntry
        TutorialStoreElevation
        UiGonioButton
        noEntryActions
        noStateActions
        [ tutorialStoreElevation ]
        (static
            """The GONIO setting is stored, this gives the elevation.
        """
        )
    , TutorialEntry
        TutorialAdjustRangeForHeight
        UiRangeKnob
        noEntryActions
        [ chaseTheRaidRange True ]
        [ chaseTheRaidRange False ]
        (static
            """Adjust the range pointer because the raid has moved.
        """
        )
    , TutorialEntry
        TutorialStoreRange2
        UIRangeButton
        noEntryActions
        noStateActions
        [ tutorialStoreRange1 ]
        (static
            """Pressing the RANGE button stores the range in the calculator.
        """
        )
    , TutorialEntry
        TutorialStoreStrength
        UiRaidStrength
        [ tutorialStoreStrength 1 ]
        noStateActions
        noExitActions
        (static
            """Finally, the operator presses Raid Strength 1, because this small steady signal is one aircraft.
        """
        )
    , TutorialEntry
        TutorialShowCalculator
        UiCalculator
        [ tutorialShowCalculator ]
        noStateActions
        [ tutorialShowOperator ]
        tutorialInterpretCalculator
    , TutorialEntry
        TutorialEnded
        UiDummy
        noEntryActions
        noStateActions
        [ stopTutorialRaid ]
        --, clearCalculator ]
        (static """Choose more training or click on Operate.
        """)
    , tutorialCloseStep
    ]


uiExplanations : List ( UiComponent, String )
uiExplanations =
    [ ( UiCRT, """The operators screen, or CRT""" )
    , ( UiGoniometer, """Goniometer""" )
    , ( UiRangeKnob, """Range knob""" )
    , ( UiRangeScale, """Range scale (miles) and range indicator""" )
    , ( UiSwitchPanel, """Mode switches""" )
    , ( UiRaidStrength, """Raid strength entry buttons""" )
    , ( UiCalcStrength, """Estimate of number of planes in raid""" )
    , ( UiCalcGrid, """The 100km map grid square containing the raid""" )
    , ( UiCalcHeight, """The approximate height of the raid""" )
    , ( UiCalcOffset, """The approximate position within the grid square""" )
    ]


type alias TutorialAction =
    Model -> Model


type alias TutorialActionList =
    List TutorialAction


noEntryActions : TutorialActionList
noEntryActions =
    []


noExitActions : TutorialActionList
noExitActions =
    []


noStateActions : TutorialActionList
noStateActions =
    []


tutorialFromId id =
    case id of
        ScenarioBasic ->
            tutorialBasic

        ScenarioTwoTogether ->
            tutorialTBD

        ScenarioTwoSeparate ->
            tutorialTBD

        ScenarioThreeOrMore ->
            tutorialTBD

        ScenarioFriendly ->
            tutorialTBD


tutorialStartScenario id model =
    let
        firstStep =
            List.head <| tutorialFromId id

        setActiveTutorial s m =
            { m
                | tutorialScenario = Just id
                , tutorialStage = Just s.tutorialStep
                , currPage = OperatorPage
                , isMenuOpen = False
            }
    in
    case firstStep of
        Nothing ->
            model

        Just step ->
            setActiveTutorial step <|
                applyActions step.entryActions <|
                    exitTutorial model


lookupUiExplanation : UiComponent -> Maybe String
lookupUiExplanation ui =
    List.head <|
        List.filterMap
            (\( i, s ) ->
                if i == ui then
                    Just s

                else
                    Nothing
            )
            uiExplanations


findNextStep : Maybe Tutorial -> Maybe TutorialStep -> Maybe TutorialStep
findNextStep currentTutorial currentStep =
    let
        findNextStepHelper steps =
            case steps of
                [] ->
                    Nothing

                step1 :: step2 :: more ->
                    if Just step1.tutorialStep == currentStep then
                        Just step2.tutorialStep

                    else
                        findNextStepHelper (step2 :: more)

                _ :: more ->
                    findNextStepHelper more
    in
    case currentTutorial of
        Just t ->
            findNextStepHelper t

        _ ->
            Nothing


findPrevStep : Maybe Tutorial -> Maybe TutorialStep -> Maybe TutorialStep
findPrevStep currentutorial currentStep =
    let
        findPrevStepHelper steps =
            case steps of
                [] ->
                    Just TutorialWelcome

                step1 :: step2 :: more ->
                    if Just step2.tutorialStep == currentStep then
                        Just step1.tutorialStep

                    else
                        findPrevStepHelper (step2 :: more)

                _ :: more ->
                    findPrevStepHelper more
    in
    case currentutorial of
        Just t ->
            findPrevStepHelper t

        _ ->
            Nothing


advanceTutorial : Model -> Model
advanceTutorial current =
    let
        nextStepId t =
            findNextStep t current.tutorialStage

        currentStep t =
            findStep t current.tutorialStage

        maybeNextStep t =
            findStep t (nextStepId t)
    in
    case current.tutorialScenario of
        Nothing ->
            current

        Just scenarioId ->
            let
                t =
                    Just <| tutorialFromId scenarioId
            in
            case ( currentStep t, maybeNextStep t ) of
                ( Just thisStage, Just nextStep ) ->
                    applyActions nextStep.entryActions <|
                        applyActions thisStage.exitActions <|
                            { current
                                | tutorialStage = nextStepId t
                            }

                ( Just thisStep, Nothing ) ->
                    tutorialExitAction <|
                        applyActions thisStep.exitActions current

                _ ->
                    current


exitTutorial : Model -> Model
exitTutorial model =
    -- Maybe reliable way to exit cleanly is to fast forward
    -- so that all state exit actions are performed.
    case model.tutorialScenario of
        Just id ->
            -- Apply all stage exit actions in correct order
            applyActions (List.concatMap .exitActions <| tutorialFromId id) model

        _ ->
            tutorialExitAction model


applyActions : TutorialActionList -> Model -> Model
applyActions actions model =
    List.foldl (\a m -> a m) model actions


goBackInTutorial : Model -> Model
goBackInTutorial current =
    let
        prevStepId t =
            findPrevStep t current.tutorialStage

        currentStep t =
            findStep t current.tutorialStage

        maybePrevStep t =
            findStep t (prevStepId t)
    in
    case current.tutorialScenario of
        Just id ->
            let
                t =
                    Just <| tutorialFromId id
            in
            case ( currentStep t, maybePrevStep t ) of
                ( Just thisStage, Just prevStep ) ->
                    applyActions prevStep.entryActions <|
                        applyActions thisStage.exitActions <|
                            { current | tutorialStage = prevStepId t }

                ( Just thisStage, Nothing ) ->
                    applyActions thisStage.exitActions <|
                        { current | tutorialStage = Nothing }

                _ ->
                    current

        Nothing ->
            current


tutorialAutomation : TutorialAction
tutorialAutomation model =
    -- This is the hook where we need to keep track of control positions etc.
    -- I.E. not only on state transitions.
    let
        currentStep id =
            findStep (Just (tutorialFromId id)) model.tutorialStage
    in
    case ( model.tutorialScenario, model.tutorialStage ) of
        ( Just scenario, Just stage ) ->
            case currentStep scenario of
                Just step ->
                    applyActions step.stateActions model

                Nothing ->
                    model

        _ ->
            model


setupTutorialRaid : TutorialAction
setupTutorialRaid model =
    { model | targets = trainingMode }


stopTutorialRaid model =
    { model | targets = [] }


clearCalculator model =
    { model
        | storedAzimuth = Nothing
        , storedElevation = Nothing
        , storedAzimuthRange = Nothing
        , storedElevationRange = Nothing
        , storedStrength = Nothing
        , storedFriendly = Nothing
        , storedStrengthPlus = Nothing
    }


tutorialStoreBearing : TutorialAction
tutorialStoreBearing model =
    { model
        | storedAzimuth = Just (model.goniometerAzimuth + model.station.lineOfShoot)
        , inputState = BearingRangeInput
    }


tutorialStoreElevation : TutorialAction
tutorialStoreElevation model =
    -- Use the actual known elevation for target near range setting.
    { model
        | storedElevation = findTargetElevation model.targets model.polarTargets model.rangeSlider
        , inputState = HeightRangeInput
    }


tutorialStoreStrength : Int -> TutorialAction
tutorialStoreStrength strength model =
    { model | storedStrength = Just strength }


tutorialStoreRange1 : TutorialAction
tutorialStoreRange1 model =
    { model
        | storedAzimuthRange = Just (1.6 * model.rangeSlider)
        , inputState = BearingInput
    }


tutorialStoreRange2 : TutorialAction
tutorialStoreRange2 model =
    { model
        | storedElevationRange = Just (1.6 * model.rangeSlider)
        , inputState = HeightInput
    }


tutorialHeightMode : TutorialAction
tutorialHeightMode model =
    { model
        | goniometerMode = Elevation
        , inputState = HeightInput
    }


tutorialBearingMode : TutorialAction
tutorialBearingMode model =
    { model
        | goniometerMode = Azimuth
        , inputState = BearingInput
    }


tutorialShowCalculator : TutorialAction
tutorialShowCalculator model =
    { model | currPage = OutputPage }


tutorialShowOperator : TutorialAction
tutorialShowOperator model =
    { model | currPage = OperatorPage }


tutorialExitAction : TutorialAction
tutorialExitAction model =
    { model
        | tutorialStage = Nothing
        , tutorialScenario = Nothing
    }


tutorialGoniometerSwinging : TutorialAction
tutorialGoniometerSwinging model =
    { model | goniometerAzimuth = 1.2 * sin (toFloat model.modelTime / 1000) }


chaseTheRaidRange : Bool -> Model -> Model
chaseTheRaidRange active model =
    -- Use simulated key presses to mimic the operator tracking the raid
    let
        rangeInMetres =
            Maybe.withDefault 50000 <|
                List.head <|
                    List.map .r model.polarTargets

        rangeInMiles =
            toFloat <| floor <| rangeInMetres / 1600

        currentKeys =
            model.keys
    in
    { model
        | keys =
            { currentKeys
                | rangeLeft = active && model.rangeSlider > rangeInMiles + 1
                , rangeRight = active && model.rangeSlider < rangeInMiles - 1
            }
    }


swingThatGoniometer : Bool -> Model -> Model
swingThatGoniometer active model =
    -- Use simulated key presses to mimic the operator tracking the raid
    let
        targetBearing =
            Maybe.withDefault 0.0 <|
                List.head <|
                    List.map .theta model.polarTargets

        currentKeys =
            model.keys
    in
    { model
        | keys =
            { currentKeys
                | gonioClock = active && model.goniometerAzimuth < targetBearing - degrees 1
                , gonioAnti = active && model.goniometerAzimuth > targetBearing + degrees 1
            }
    }


tutorialSeekElevation : Bool -> Model -> Model
tutorialSeekElevation active model =
    -- Use simulated key presses to mimic the operator tracking the raid.
    -- Unlike bearing, we don't know which way to move, so just go clockwise and
    -- don't try for zero!
    let
        targetElevation =
            Maybe.withDefault 1.5 <|
                List.head <|
                    List.map .amplitude model.elevation_A_trace

        currentKeys =
            model.keys
    in
    { model
        | keys =
            { currentKeys
                | gonioClock = active && targetElevation > 0.2
            }
    }


findMatchingStep : TutorialScenario -> Maybe TutorialStep -> UiComponent -> Maybe TutorialEntry
findMatchingStep tutorialId tutorialStep uiComponent =
    let
        findHelper steps =
            case steps of
                [] ->
                    Nothing

                step1 :: more ->
                    if
                        Just step1.tutorialStep
                            == tutorialStep
                            && step1.uiComponent
                            == uiComponent
                    then
                        Just step1

                    else
                        findHelper more
    in
    findHelper <| tutorialFromId tutorialId


findStep : Maybe Tutorial -> Maybe TutorialStep -> Maybe TutorialEntry
findStep currentTutorial tutorialStep =
    let
        findHelper steps =
            case steps of
                [] ->
                    Nothing

                step1 :: more ->
                    if
                        Just step1.tutorialStep
                            == tutorialStep
                    then
                        Just step1

                    else
                        findHelper more
    in
    case currentTutorial of
        Just tut ->
            findHelper tut

        Nothing ->
            Nothing


explanatoryText : Model -> UiComponent -> List (Attribute Msg)
explanatoryText model uiComponent =
    let
        uiComponentDescription =
            lookupUiExplanation uiComponent
    in
    [ inFront <|
        case ( model.explainMode, uiComponentDescription ) of
            ( True, Just txt ) ->
                el
                    [ centerX
                    , centerY
                    , Background.color blue
                    , Border.color white
                    , Border.width 1
                    , Border.rounded 5
                    ]
                <|
                    paragraph
                        [ spacing 1
                        , Font.size 16
                        , Font.family [ Font.typeface "Helvetica" ]
                        , Font.color white
                        , padding 5
                        ]
                        [ text txt ]

            _ ->
                none
    ]


tutorialTextBox : Model -> List (Attribute Msg) -> Attribute Msg
tutorialTextBox model adjustments =
    -- Use a single central text box for all tutorial text.
    -- Second argument allows caller to finesse the position
    inFront <|
        case model.tutorialScenario of
            Nothing ->
                none

            Just scenario ->
                case findStep (Just (tutorialFromId scenario)) model.tutorialStage of
                    Nothing ->
                        none

                    Just step ->
                        el
                            ([ width (px 500)
                             , Border.color flatSunflower
                             , Border.width 2
                             , Border.rounded 5
                             , Font.center
                             , Font.color white
                             , Font.size 32
                             , Font.family [ Font.typeface "Helvetica" ]
                             ]
                                ++ adjustments
                            )
                        <|
                            row
                                [ width fill ]
                                [ el [ onClick TutorialBack, pointer ] <| text "◀︎"
                                , paragraph
                                    [ Background.color blue
                                    , spacing 4
                                    , padding 10
                                    , Font.size 16
                                    ]
                                    [ text (step.tutorialText model) ]
                                , el [ onClick TutorialAdvance, alignRight, pointer ] <|
                                    text "►"
                                ]
