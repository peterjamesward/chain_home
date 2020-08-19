module TrainingMode exposing (..)

import Browser
import Calculator.Model exposing (InputState(..), setInputState, storeAzimuth, storeAzimuthRange, storeElevation, storeFriendly, storeStrength, storeStrengthPlus)
import Calculator.View exposing (interpretCalculator)
import Config exposing (trainingMode, trainingMode2, trainingMode3, trainingMode3to6, trainingModeFriendlyOutbound)
import Constants exposing (blue, flatMidnightBlue, flatSunflower, flatWetAsphalt, paletteSand, white)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Keys exposing (Keys, updateKeys)
import Messages exposing (..)
import Model exposing (Model)
import Target exposing (findTargetHeight)
import Types exposing (..)
import Utils exposing (explainModeEnabledForCurrentPage)



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


tutorialCloseStep : TutorialScenario -> TutorialEntry
tutorialCloseStep scenario =
    -- Intended to be final step in any tutorial.
    TutorialEntry
        TutorialDummy
        UiDummy
        [ tutorialExitAction
        , clearCalculator
        , clearTargets
        , recordScenarioDone scenario
        ]
        noStateActions
        noExitActions
        (static "No text.")


static : String -> (Model -> String)
static s =
    -- As we now allow derived tutorial strings, we need a way to declare a fixed string.
    \_ -> s

--TODO: Move these remaining two into the page and lose this list.
uiExplanations : List ( UiComponent, String )
uiExplanations =
    [ ( UiConfigOptions, """Click "Learn" to understand each of the types of raid.
    As you complete each section, the box will be ticked and raids like
    that will appear when you click "Go!".
    You can tick or untick them anyway, if you like.""" )
    , ( UiGoButton, """Test yourself with a series of incoming raids.
                        This will only use the types of raid that are ticked.""" )
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
            tutorial2SameBearing

        ScenarioTwoSeparate ->
            tutorial2DifferentBearings

        ScenarioThreeToSix ->
            tutorial3to6

        ScenarioFriendly ->
            tutorialIFF


tutorialBasic : Tutorial
tutorialBasic =
    [ TutorialEntry
        TutorialWelcome
        UiDummy
        [ centraliseKnobs, clearTargets ]
        noStateActions
        noExitActions
        (static """This tutorial will give a basic overview of the receiver and its operation.
        Click the ► to go on, ◀︎ to go back.
        """)
    , TutorialEntry
        TutorialIntroduceTheTube
        UiCRT
        noEntryActions
        noStateActions
        noExitActions
        (static """The most obvious and most important feature is the "tube" or CRT, behind this text.
        It shows radio signals returned from aircraft within our 100 mile range over the North Sea.
        """)
    , TutorialEntry
        TutorialIntroduceTheTube2
        UiCRT
        noEntryActions
        noStateActions
        noExitActions
        (static """A returned signal will be a V-shaped dip in the line that we see jiggling across the tube.
        The more to the right, the further away the aircraft, with the scale in miles across the top.
        The two large dips near the left are caused by local features such as buildings or hills.
        The constant jiggling is just electrical noise - these are very sensitive receivers.
        """)
    , TutorialEntry
        TutorialJustSwinging
        UiGoniometer
        [ tutorialBearingMode, clearCalculator, clearTargets ]
        [ tutorialGoniometerSwinging ]
        noExitActions
        (static
            """The operator uses the goniometer ("gonio") to estimate the bearing of a incoming raid.
            The operator must turn the gonio, not let it sit in one position, or new raids could be missed.
             """
        )
    , TutorialEntry
        TutorialIncomingRaid
        UiCRT
        [ setupTutorialRaid ]
        [ stopGonioAwayFromRaidBearing ]
        noExitActions
        (static
            """The white V shape under the 100 on the 'tube' is a new raid.
            This simple and stable shape is always only one aircraft.
            The operator turns the Range knob until the Range Pointer points at the
            raid (ideally at the left hand edge) for an accurate range reading.
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
        TutorialIntroduceGonio
        UiGoniometer
        noEntryActions
        noStateActions
        noExitActions
        (static """The raid will keep moving, so the operator works quickly.
        The operator will turn the Gonio to make the V as small as possible.
        """)
    , TutorialEntry
        TutorialFindBearing
        UiGoniometer
        noEntryActions
        [ findBearingOfNumberedTarget True 0 ]
        [ findBearingOfNumberedTarget False 0 ]
        (static
            """The operator 'swings' the gonio until the V on the CRT vanishes.
            The next step is to load information into the calculator. The lights
            over to the right remind the operator which button to press.
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
            This will use the goniometer to compare signals received at aerials
            placed at different heights. Height finding is at best approximate and often impossible.
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
            The reading on the goniometer scale has no direct meaning, it is just input to the calculator.
        """
        )
    , TutorialEntry
        TutorialStoreElevation
        UiGonioButton
        noEntryActions
        noStateActions
        [ tutorialStoreElevation ]
        (static
            """The GONIO setting is stored, this allows the calculator to work out the elevation.
        """
        )
    , TutorialEntry
        TutorialAdjustRangeForHeight
        UiRangeKnob
        noEntryActions
        [ chaseTheRaidRange True ]
        [ chaseTheRaidRange False ]
        (static
            """The operator adjusts the range pointer because the raid has moved.
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
        TutorialLeaveHeightMode
        UiHeight
        [ tutorialEndHeightMode ]
        noStateActions
        noExitActions
        (static
            """Make sure you don't leave the receiver in Height mode.
        """
        )
    , TutorialEntry
        TutorialShowCalculator
        UiCalculator
        [ tutorialShowCalculator ]
        noStateActions
        [ tutorialShowOperator ]
        tutorialInterpretCalculator
    , tutorialCloseStep ScenarioBasic
    ]


tutorial2SameBearing : Tutorial
tutorial2SameBearing =
    [ TutorialEntry
        TutorialWelcome
        UiGoniometer
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
        [ setupTutorialRaid2SameBearing ]
        noStateActions
        noExitActions
        (static
            """The white V shape on the 'tube' is a new raid.
        The regular "beating" up and down is always two aircraft.
        Let's find their bearing and height.
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
        [ findBearingOfNumberedTarget True 0 ]
        [ findBearingOfNumberedTarget False 0 ]
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
        [ tutorialStoreStrength 2 ]
        noStateActions
        noExitActions
        (static
            """The operator presses Raid Strength 2, because this beating signal is two aircraft.
        """
        )
    , TutorialEntry
        TutorialLeaveHeightMode
        UiHeight
        [ tutorialEndHeightMode ]
        noStateActions
        noExitActions
        (static
            """Make sure you don't leave the receiver in Height mode.
        """
        )
    , TutorialEntry
        TutorialShowCalculator
        UiCalculator
        [ tutorialShowCalculator ]
        noStateActions
        [ tutorialShowOperator ]
        tutorialInterpretCalculator
    , tutorialCloseStep ScenarioTwoTogether
    ]


tutorial2DifferentBearings : Tutorial
tutorial2DifferentBearings =
    [ TutorialEntry
        TutorialWelcome
        UiGoniometer
        [ tutorialBearingMode, clearCalculator ]
        [ tutorialGoniometerSwinging ]
        noExitActions
        (static
            """The operator is turning the gonio, looking for a signal.
        Click ► to begin.
        """
        )
    , TutorialEntry
        TutorialIncomingRaid
        UiCRT
        [ setupTutorialRaid2DifferentBearings ]
        noStateActions
        noExitActions
        (static
            """The white V shape on the 'tube' is a new raid.
        The regular "beating" up and down is always two aircraft.
        Let's find their bearing and height.
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
        [ tutorialGoniometerSwinging ]
        noExitActions
        (static
            """The operator swings the gonio but the V will not disappear completely.
            This means that the two planes are not together - they are on different bearings.
        """
        )
    , TutorialEntry
        TutorialFindBearingPlaneA
        UiGoniometer
        noEntryActions
        [ findBearingOfNumberedTarget True 0 ]
        [ findBearingOfNumberedTarget False 0 ]
        (static
            """We can find the bearing where the V stops going up and down.
            This means we have located one of the aircraft.
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
        [ tutorialBearingMode ]
        (static
            """We don't try to find the height because we won't know which aircraft it is.
            The gonio can do bearing or height, not both at the same time.
        """
        )
    , TutorialEntry
        TutorialStoreStrength
        UiRaidStrength
        [ tutorialStoreStrength 1 ]
        noStateActions
        noExitActions
        (static
            """The operator presses Raid Strength 1 because the plot is for one aircraft.
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
        TutorialFindBearingPlaneB
        UiGoniometer
        noEntryActions
        [ findBearingOfNumberedTarget True 1 ]
        [ findBearingOfNumberedTarget False 1, tutorialStoreBearing ]
        (static
            """We find another bearing where the movement stops.
            This means we have located the other the aircraft.
            The rest of the steps are the same.
        """
        )
    , TutorialEntry
        TutorialShowCalculator2
        UiCalculator
        [ tutorialShowCalculator ]
        noStateActions
        [ tutorialShowOperator ]
        tutorialInterpretCalculator
    , tutorialCloseStep ScenarioTwoSeparate
    ]


tutorial3to6 : Tutorial
tutorial3to6 =
    [ TutorialEntry
        TutorialWelcome
        UiGoniometer
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
        [ setupTutorialRaid3to6 ]
        noStateActions
        noExitActions
        (static
            """The white V shape under the 100 on the 'tube' is a new raid.
        Note how it moves up and down in a complex pattern.
        This is typical of from 3 to 6 aircraft.
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
        [ findBearingOfNumberedTarget True 0 ]
        [ findBearingOfNumberedTarget False 0 ]
        (static
            """This is a close formation of aircraft so it will still D/F out.
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
        [ tutorialStoreStrength 3, tutorialStorePlus ]
        noStateActions
        noExitActions
        (static
            """The operator presses Raid Strength 3 and the + sign.
        """
        )
    , TutorialEntry
        TutorialLeaveHeightMode
        UiHeight
        [ tutorialEndHeightMode ]
        noStateActions
        noExitActions
        (static
            """Make sure you don't leave the receiver in Height mode.
        """
        )
    , TutorialEntry
        TutorialShowCalculator
        UiCalculator
        [ tutorialShowCalculator ]
        noStateActions
        [ tutorialShowOperator ]
        tutorialInterpretCalculator
    , tutorialCloseStep ScenarioThreeToSix
    ]


tutorialIFF : Tutorial
tutorialIFF =
    [ TutorialEntry
        TutorialWelcome
        UiGoniometer
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
        [ setupTutorialRaidFriendlyOutbound ]
        noStateActions
        noExitActions
        (static
            """The white 'V' shape is a new raid.
            Notice how the signal is bigger every 6 seconds.
            This is the IFF (Information Friend or Foe) signal that identifies
            a friendly aircraft.
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
        [ findBearingOfNumberedTarget True 0 ]
        [ findBearingOfNumberedTarget False 0 ]
        (static
            """Find the bearing as usual.
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
        [ tutorialStoreStrength 1, tutorialStoreFriendly ]
        noStateActions
        noExitActions
        (static
            """The operator presses Raid Strength 1 and the F sign to show it's friendly.
        """
        )
    , TutorialEntry
        TutorialLeaveHeightMode
        UiHeight
        [ tutorialEndHeightMode ]
        noStateActions
        noExitActions
        (static
            """Make sure you don't leave the receiver in Height mode.
        """
        )
    , TutorialEntry
        TutorialShowCalculator
        UiCalculator
        [ tutorialShowCalculator ]
        noStateActions
        [ tutorialShowOperator ]
        tutorialInterpretCalculator
    , tutorialCloseStep ScenarioFriendly
    ]


tutorialStartScenario id model =
    let
        firstStep =
            List.head <| tutorialFromId id

        setActiveTutorial s m =
            { m
                | tutorialScenario = Just id
                , tutorialStage = Just s.tutorialStep
                , currPage = OperatorPageInTutorial
                , startTime = model.modelTime
                , webGLtime = 0.0
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

                _ :: [] ->
                    Nothing

                step1 :: step2 :: more ->
                    if Just step1.tutorialStep == currentStep then
                        Just step2.tutorialStep

                    else
                        findNextStepHelper (step2 :: more)
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
    -- This used only when user is clicking on the advance button,
    -- should not be used when exiting in any other way.
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
    let
        clearAnyCurrentTutorial m =
            { m
                | tutorialStage = Nothing
                , tutorialScenario = Nothing
                , targets = []
            }
    in
    clearAnyCurrentTutorial <|
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
        ( Just scenario, Just _ ) ->
            case currentStep scenario of
                Just step ->
                    applyActions step.stateActions model

                Nothing ->
                    model

        _ ->
            model


setupTutorialRaid : TutorialAction
setupTutorialRaid model =
    { model
        | targets = trainingMode model.modelTime
    }


centraliseKnobs : TutorialAction
centraliseKnobs model =
    { model
        | goniometerAzimuth = 0
        , rangeSlider = 50
    }


setupTutorialRaid2SameBearing : TutorialAction
setupTutorialRaid2SameBearing model =
    { model | targets = trainingMode2 model.modelTime }


setupTutorialRaid2DifferentBearings : TutorialAction
setupTutorialRaid2DifferentBearings model =
    { model | targets = trainingMode3 model.modelTime }


setupTutorialRaid3to6 : TutorialAction
setupTutorialRaid3to6 model =
    { model | targets = trainingMode3to6 model.modelTime }


setupTutorialRaidFriendlyOutbound : TutorialAction
setupTutorialRaidFriendlyOutbound model =
    { model | targets = trainingModeFriendlyOutbound model.modelTime }


stopTutorialRaid model =
    { model | targets = [] }


clearCalculator model =
    { model
        | calculator = Calculator.Model.init
    }


recordScenarioDone : TutorialScenario -> Model -> Model
recordScenarioDone scenario model =
    -- Needs to be Set but defined classes not Comparable.
    -- Minor problem, in the scheme of things.
    { model
        | tutorialsCompleted =
            if List.member scenario model.tutorialsCompleted then
                model.tutorialsCompleted

            else
                scenario :: model.tutorialsCompleted
    }


tutorialStoreBearing : TutorialAction
tutorialStoreBearing model =
    { model
        | calculator =
            setInputState BearingRangeInput <|
                storeAzimuth (model.goniometerAzimuth + model.station.lineOfShoot)
                    model.calculator
    }


tutorialStoreElevation : TutorialAction
tutorialStoreElevation model =
    -- Use the actual known elevation for target near range setting.
    { model
        | calculator =
            setInputState HeightRangeInput <|
                storeElevation (findTargetHeight model.targets model.rangeSlider) model.calculator
    }


tutorialStoreStrength : Int -> TutorialAction
tutorialStoreStrength strength model =
    { model | calculator = storeStrength strength model.calculator }


tutorialStorePlus : TutorialAction
tutorialStorePlus model =
    { model | calculator = storeStrengthPlus True model.calculator }


tutorialStoreFriendly : TutorialAction
tutorialStoreFriendly model =
    { model | calculator = storeFriendly True model.calculator }


tutorialStoreRange1 : TutorialAction
tutorialStoreRange1 model =
    { model
        | calculator =
            storeAzimuthRange (1.6 * model.rangeSlider) <|
                setInputState BearingInput model.calculator
    }


tutorialHeightMode : TutorialAction
tutorialHeightMode model =
    { model
        | goniometerMode = Elevation
        , calculator = setInputState HeightInput model.calculator
    }


tutorialEndHeightMode : TutorialAction
tutorialEndHeightMode model =
    { model
        | goniometerMode = Azimuth
        , calculator = setInputState BearingInput model.calculator
    }


tutorialBearingMode : TutorialAction
tutorialBearingMode model =
    { model
        | goniometerMode = Azimuth
        , calculator = setInputState BearingInput model.calculator
    }


tutorialShowCalculator : TutorialAction
tutorialShowCalculator model =
    { model | currPage = CalculatorInTutorial }


tutorialShowOperator : TutorialAction
tutorialShowOperator model =
    { model | currPage = OperatorPage }


tutorialExitAction : TutorialAction
tutorialExitAction model =
    { model
        | tutorialStage = Nothing
        , tutorialScenario = Nothing
        , currPage = InputPage
    }


clearTargets : TutorialAction
clearTargets model =
    { model | targets = [] }


tutorialGoniometerSwinging : TutorialAction
tutorialGoniometerSwinging model =
    { model | goniometerAzimuth = 1.2 * sin (toFloat model.modelTime / 1000) }


stopGonioAwayFromRaidBearing : TutorialAction
stopGonioAwayFromRaidBearing model =
    if model.goniometerAzimuth >= degrees -50 && model.goniometerAzimuth <= degrees 0 then
        tutorialGoniometerSwinging model
        -- keep turning.

    else
        model



-- stop swinging, we are away from the target.


chaseTheRaidRange : Bool -> Model -> Model
chaseTheRaidRange active model =
    -- Use simulated key presses to mimic the operator tracking the raid
    let
        rangeInMetres =
            Maybe.withDefault 50000 <|
                List.head <|
                    List.map .rangeInMetres model.targets

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


findBearingOfNumberedTarget : Bool -> Int -> Model -> Model
findBearingOfNumberedTarget active index model =
    -- Use simulated key presses to mimic the operator tracking the raid
    -- Note index is zero based.
    let
        targetBearing =
            Maybe.withDefault 0.0 <|
                List.head <|
                    List.drop index <|
                        List.map .theta model.targets

        currentKeys =
            model.keys

        angleRelativeToLineOfShoot =
            model.goniometerAzimuth + model.station.lineOfShoot
    in
    { model
        | keys =
            { currentKeys
                | gonioClock = active && angleRelativeToLineOfShoot < targetBearing - degrees 1
                , gonioAnti = active && angleRelativeToLineOfShoot > targetBearing + degrees 1
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


--TODO: Factor out this ugly repeating search pattern
findTutorialSubject : Model -> Maybe UiComponent
findTutorialSubject model =
    let
        currentStep id =
            findStep (Just (tutorialFromId id)) model.tutorialStage
    in
    case ( model.tutorialScenario, model.tutorialStage ) of
        ( Just scenario, Just _ ) ->
            case currentStep scenario of
                Just step ->
                    Just step.uiComponent

                Nothing ->
                    Nothing

        _ -> Nothing


highlightTutorialSubject : Maybe UiComponent -> UiComponent -> List (Attribute Msg)
highlightTutorialSubject tutorialSubject uiComponent =
    -- Highlight the control when this control pertains to current state of active tutorial.
    if Just uiComponent == tutorialSubject then
        [ Border.color paletteSand
        , Border.width 2
        , Border.glow paletteSand 2.0
        , Border.innerGlow paletteSand 2.0
        , Border.rounded 5
        ]

    else
        [ Border.color flatMidnightBlue
        , Border.width 2
        , Border.glow flatMidnightBlue 2.0
        , Border.innerGlow flatMidnightBlue 2.0
        , Border.rounded 5
        ]


tutorialText : Model -> Maybe String
tutorialText model =
    let
        currentStep id =
            findStep (Just (tutorialFromId id)) model.tutorialStage
    in
    case ( model.tutorialScenario, model.tutorialStage ) of
        ( Just scenario, Just stage ) ->
            case currentStep scenario of
                Just step ->
                    Just ( step.tutorialText model)

                Nothing ->
                    Nothing

        _ -> Nothing


tutorialTextBox : Model -> List (Attribute Msg) -> Element Msg
tutorialTextBox model adjustments =
    -- Use a single central text box for all tutorial text.
    -- Second argument allows caller to finesse the position
    let
        theText =
            tutorialText model
    in
    case theText of
        Nothing ->
            none

        Just someText ->
            el
                (tutorialControlHolderStyles ++ adjustments)
            <|
                tutorialControls someText



{-
   TODO: All pages to move towards this (below) style. Probably.
    I think this means we have to ask the page to do the highlighting of the control
    but the controls remain in the common floating box.
-}


viewCalculatorInTutorial : Model -> Element Msg
viewCalculatorInTutorial model =
    let
        rawPage =
            Calculator.View.view model.outputDevice model.calculator

        calcText =
            Calculator.View.interpretCalculator model.calculator
    in
    el
        [ centerX
        , inFront <| tutorialControls calcText
        ]
        rawPage


tutorialControls someText =
    el tutorialControlHolderStyles <|
        row
            [ width fill, centerY ]
            [ el [ onClick TutorialBack, pointer ] <| text "◀︎"
            , paragraph
                [ Background.color blue
                , spacing 4
                , padding 10
                , Font.size 16
                ]
                [ text someText ]
            , el [ onClick TutorialAdvance, alignRight, pointer ] <|
                text "▶︎"
            ]


tutorialControlHolderStyles =
    [ width (px 500)
    , height (px 160)
    , centerY
    , centerX
    , Background.color blue
    , Border.color flatSunflower
    , Border.width 2
    , Border.rounded 5
    , Font.center
    , Font.color white
    , Font.size 32
    , Font.family [ Font.typeface "Helvetica" ]
    ]


tutorialInterpretCalculator : Model -> String
tutorialInterpretCalculator model =
    interpretCalculator model.calculator
