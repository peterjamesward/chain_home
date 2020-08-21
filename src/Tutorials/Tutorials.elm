module Tutorials.Tutorials exposing (..)

import Browser
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
import Model exposing (..)
import Target exposing (findTargetHeight)
import Tutorials.ActionCodes exposing (..)
import Tutorials.Model exposing (..)
import Types exposing (..)
import Zipper


tutorialCloseStep : TutorialScenario -> TutorialEntry
tutorialCloseStep scenario =
    -- Intended to be final step in any tutorial.
    TutorialEntry
        UiDummy
        [ ActionExitTutorial --> tutorialExitAction
        , ActionClearCalculator
        , ActionClearTargets
        , ActionRecordScenarioDone scenario
        ]
        noStateActions
        noExitActions
        (TextConstant "Hello world.")


noEntryActions : TutorialActionList
noEntryActions =
    [ ]


noExitActions : TutorialActionList
noExitActions =
    []


noStateActions : TutorialActionList
noStateActions =
    []


tutorialFromId : TutorialScenario -> Tutorial
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


tutorialBasic : Maybe (Zipper.Zipper TutorialEntry)
tutorialBasic =
    Zipper.fromList
        [ TutorialEntry
            UiDummy
            [ ActionInitialiseTutorial
            , ActionCentraliseKnobs
            , ActionClearTargets
            ]
            noStateActions
            noExitActions
            (TextConstant
                """This tutorial will give a basic overview of the receiver and its operation.
            Click the ► to go on, ◀︎ to go back."""
            )
        , TutorialEntry
            UiCRT
            noEntryActions
            noStateActions
            noExitActions
            (TextConstant """The most obvious and most important feature is the "tube" or CRT, behind this text.
        It shows radio signals returned from aircraft within our 100 mile range over the North Sea.
        """)
        , TutorialEntry
            UiCRT
            noEntryActions
            noStateActions
            noExitActions
            (TextConstant """A returned signal will be a V-shaped dip in the line that we see jiggling across the tube.
        The more to the right, the further away the aircraft, with the scale in miles across the top.
        The two large dips near the left are caused by local features such as buildings or hills.
        The constant jiggling is just electrical noise - these are very sensitive receivers.
        """)
        , TutorialEntry
            UiGoniometer
            [ ActionBearingMode
            , ActionClearCalculator
            , ActionClearTargets
            ]
            [ ActionSwingGoniometer ]
            --tutorialGoniometerSwinging ]
            noExitActions
            (TextConstant
                """The operator uses the goniometer ("gonio") to estimate the bearing of a incoming raid.
            The operator must turn the gonio, not let it sit in one position, or new raids could be missed.
             """
            )
        , TutorialEntry
            UiCRT
            [ ActionSetupRaid ]
            --setupTutorialRaid
            [ ActionStopGonioAwayFromRaidBearing ]
            noExitActions
            (TextConstant
                """The white V shape under the 100 on the 'tube' is a new raid.
            This simple and stable shape is always only one aircraft.
            The operator turns the Range knob until the Range Pointer points at the
            raid (ideally at the left hand edge) for an accurate range reading.
             """
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """The operator turns the range knob until the pointer
            lines up with the left edge of the raid on the CRT.
            """
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            noStateActions
            noExitActions
            (TextConstant
                """The raid will keep moving, so the operator works quickly.
            The operator will turn the Gonio to make the V as small as possible.
            """
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 0 ]
            [ ActionFindBearingOfNumberedTarget False 0 ]
            (TextConstant
                """The operator 'swings' the gonio until the V on the CRT vanishes.
            The next step is to load information into the calculator. The lights
            over to the right remind the operator which button to press."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Pressing the GONIO button stores the bearing in the calculator."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Pressing the RANGE button stores the range in the calculator."""
            )
        , TutorialEntry
            UiHeight
            [ ActionHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """The operator will now try to work out the height.
            This will use the goniometer to compare signals received at aerials
            placed at different heights. Height finding is at best approximate and often impossible."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSeekElevation True ]
            [ ActionSeekElevation False ]
            (TextConstant
                """The operator swings the gonio again, to minimise the V.
            The reading on the goniometer scale has no direct meaning, it is just input to the calculator."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreElevation ]
            (TextConstant
                """The GONIO setting is stored to work out the elevation."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """The operator adjusts the range pointer because the raid has moved."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Pressing the RANGE button stores the range in the calculator."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 1 ]
            noStateActions
            noExitActions
            (TextConstant
                """Finally, the operator presses Raid Strength 1, because this small steady signal is one aircraft. """
            )
        , TutorialEntry
            UiHeight
            [ ActionEndHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """Make sure you don't leave the receiver in Height mode."""
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , tutorialCloseStep ScenarioBasic
        ]


tutorial2SameBearing : Tutorial
tutorial2SameBearing =
    Zipper.fromList
        [ TutorialEntry
            UiGoniometer
            [ ActionInitialiseTutorial
            , ActionBearingMode
            , ActionClearCalculator
            ]
            [ ActionSwingGoniometer ]
            noExitActions
            (TextConstant
                """The operator is turning the gonio, looking for any sign of a signal.
            Click ► to begin."""
            )
        , TutorialEntry
            UiCRT
            [ ActionSetupRaid2SameBearing ]
            noStateActions
            noExitActions
            (TextConstant
                """The white V shape on the 'tube' is a new raid.
            The regular "beating" up and down is always two aircraft.
            Let's find their bearing and height.
            """
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """The operator turns the range knob until the pointer
            lines up with the left edge of the raid on the CRT.
            """
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 0 ]
            [ ActionFindBearingOfNumberedTarget False 0 ]
            (TextConstant
                """The operator 'swings' the gonio until the V on the CRT vanishes."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Pressing the GONIO button stores the bearing in the calculator."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Pressing the RANGE button stores the range in the calculator."""
            )
        , TutorialEntry
            UiHeight
            [ ActionHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """The operator will now try to work out the height."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSeekElevation True ]
            [ ActionSeekElevation False ]
            (TextConstant
                """The operator swings the gonio again, to minimise the V."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreElevation ]
            (TextConstant
                """The GONIO setting is stored to work out the elevation."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """Adjust the range pointer because the raid has moved."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Pressing the RANGE button stores the range in the calculator."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 2 ]
            noStateActions
            noExitActions
            (TextConstant
                """The operator presses Raid Strength 2, because this beating signal is two aircraft."""
            )
        , TutorialEntry
            UiHeight
            [ ActionEndHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """Remember not to leave the receiver in Height mode."""
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , tutorialCloseStep ScenarioTwoTogether
        ]


tutorial2DifferentBearings : Tutorial
tutorial2DifferentBearings =
    Zipper.fromList
        [ TutorialEntry
            UiGoniometer
            [ ActionInitialiseTutorial
            , ActionBearingMode
            , ActionClearCalculator
            ]
            [ ActionSwingGoniometer ]
            noExitActions
            (TextConstant
                """The operator is turning the gonio, looking for a signal.
            Click ► to begin."""
            )
        , TutorialEntry
            UiCRT
            [ ActionSetupRaid2DifferentBearings ]
            noStateActions
            noExitActions
            (TextConstant
                """The white V shape on the 'tube' is a new raid.
            The regular "beating" up and down is always two aircraft.
            Let's find their bearing and height.
            """
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """The operator turns the range knob until the pointer
            lines up with the left edge of the raid on the CRT."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSwingGoniometer ]
            noExitActions
            (TextConstant
                """The operator swings the gonio but the V will not disappear completely.
            This means that the two planes are not together - they are on different bearings."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 0 ]
            [ ActionFindBearingOfNumberedTarget False 0 ]
            (TextConstant
                """We can find the bearing where the V stops going up and down.
            This means we have located one of the aircraft."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Pressing the GONIO button stores the bearing in the calculator."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Pressing the RANGE button stores the range in the calculator."""
            )
        , TutorialEntry
            UiHeight
            [ ActionHeightMode ]
            noStateActions
            [ ActionBearingMode ]
            (TextConstant
                """We don't try to find the height because we won't know which aircraft it is.
            The gonio can do bearing or height, not both at the same time."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 1 ]
            noStateActions
            noExitActions
            (TextConstant
                """The operator presses Raid Strength 1 because the plot is for one aircraft."""
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 1 ]
            [ ActionFindBearingOfNumberedTarget False 1
            , ActionStoreBearing
            ]
            (TextConstant
                """We find another bearing where the movement stops.
            This means we have located the other the aircraft.
            The rest of the steps are the same."""
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , tutorialCloseStep ScenarioTwoSeparate
        ]


tutorial3to6 : Tutorial
tutorial3to6 =
    Zipper.fromList
        [ TutorialEntry
            UiGoniometer
            [ ActionInitialiseTutorial
            , ActionBearingMode
            , ActionClearCalculator
            ]
            [ ActionSwingGoniometer ]
            noExitActions
            (TextConstant
                """The operator is turning the gonio, looking for any sign of a signal.
            Click ► to begin."""
            )
        , TutorialEntry
            UiCRT
            [ ActionSetupRaid3to6 ]
            noStateActions
            noExitActions
            (TextConstant
                """The white V shape under the 100 on the 'tube' is a new raid.
            Note how it moves up and down in a complex pattern.
            This is typical of from 3 to 6 aircraft."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """The operator turns the range knob until the pointer
            lines up with the left edge of the raid on the CRT."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 0 ]
            [ ActionFindBearingOfNumberedTarget False 0 ]
            (TextConstant
                """This is a close formation of aircraft so it will still D/F out."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Pressing the GONIO button stores the bearing in the calculator."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Pressing the RANGE button stores the range in the calculator."""
            )
        , TutorialEntry
            UiHeight
            [ ActionHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """The operator will now try to work out the height."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSeekElevation True ]
            [ ActionSeekElevation False ]
            (TextConstant
                """The operator swings the gonio again, to minimise the V."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreElevation ]
            (TextConstant
                """The GONIO setting is stored to work out the elevation."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """Adjust the range pointer because the raid has moved."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Pressing the RANGE button stores the range in the calculator."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 3
            , ActionStorePlus
            ]
            noStateActions
            noExitActions
            (TextConstant
                """The operator presses Raid Strength 3 and the + sign."""
            )
        , TutorialEntry
            UiHeight
            [ ActionEndHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """Make sure you don't leave the receiver in Height mode."""
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , tutorialCloseStep ScenarioThreeToSix
        ]


tutorialIFF : Tutorial
tutorialIFF =
    Zipper.fromList
        [ TutorialEntry
            UiGoniometer
            [ ActionInitialiseTutorial
            , ActionBearingMode
            , ActionClearCalculator
            ]
            [ ActionSwingGoniometer ]
            noExitActions
            (TextConstant
                """The operator is turning the gonio, looking for any sign of a signal.
            Click ► to begin."""
            )
        , TutorialEntry
            UiCRT
            [ ActionSetupRaidFriendlyOutbound ]
            noStateActions
            noExitActions
            (TextConstant
                """The white 'V' shape is a new raid.
            Notice how the signal is bigger every 6 seconds.
            This is the IFF (Information Friend or Foe) signal that identifies
            a friendly aircraft."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """The operator turns the range knob until the pointer
            lines up with the left edge of the raid on the CRT."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 0 ]
            [ ActionFindBearingOfNumberedTarget False 0 ]
            (TextConstant
                """Find the bearing as usual."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Pressing the GONIO button stores the bearing in the calculator."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Pressing the RANGE button stores the range in the calculator."""
            )
        , TutorialEntry
            UiHeight
            [ ActionHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """The operator will now try to work out the height."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSeekElevation True ]
            [ ActionSeekElevation False ]
            (TextConstant
                """The operator swings the gonio again, to minimise the V."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreElevation ]
            (TextConstant
                """The GONIO setting is stored to work out the elevation."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """Adjust the range pointer because the raid has moved."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Pressing the RANGE button stores the range in the calculator."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 1
            , ActionStoreFriendly
            ]
            noStateActions
            noExitActions
            (TextConstant
                """The operator presses Raid Strength 1 and the F sign to show it's friendly."""
            )
        , TutorialEntry
            UiHeight
            [ ActionEndHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """Make sure you don't leave the receiver in Height mode."""
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , tutorialCloseStep ScenarioFriendly
        ]


tutorialStartScenario : TutorialScenario -> ( Tutorial, TutorialActionList )
tutorialStartScenario id =
    -- Remember, the caller performs any state entry actions.
    let
        activeTutorial =
            tutorialFromId id
    in
    case activeTutorial of
        Nothing ->
            ( Nothing, [] )

        Just tutorialZipper ->
            ( activeTutorial, (Zipper.current tutorialZipper).entryActions )


advanceTutorial : Tutorial -> ( Tutorial, TutorialActionList )
advanceTutorial activeTutorial =
    -- This used only when user clicks on the advance button,
    -- should not be used when exiting in any other way.
    case activeTutorial of
        Nothing ->
            ( Nothing, [] )

        Just tutorial ->
            let
                currentStage =
                    Zipper.current tutorial

                maybeNextStage =
                    Zipper.next tutorial
            in
            ( maybeNextStage
            , currentStage.exitActions
                ++ (case maybeNextStage of
                        Just nextStage ->
                            (Zipper.current nextStage).entryActions

                        Nothing ->
                            []
                   )
            )


remainingActions : Tutorial -> List TutorialActionCode
remainingActions z =
    case z of
        Nothing ->
            []

        Just tut ->
            (Zipper.current tut).exitActions ++ remainingActions (Zipper.next tut)


exitTutorial : Tutorial -> ( Tutorial, TutorialActionList )
exitTutorial activeTutorial =
    -- Maybe reliable way to exit cleanly is to fast forward
    -- so that all state exit actions are performed.
    ( Nothing, remainingActions activeTutorial )


goBackInTutorial : Tutorial -> ( Tutorial, TutorialActionList )
goBackInTutorial activeTutorial =
    case activeTutorial of
        Nothing ->
            ( Nothing, [] )

        Just tutorial ->
            let
                currentStage =
                    Zipper.current tutorial

                maybePrevStage =
                    Zipper.previous tutorial
            in
            ( maybePrevStage
            , currentStage.exitActions
                ++ (case maybePrevStage of
                        Just prevStage ->
                            (Zipper.current prevStage).entryActions

                        Nothing ->
                            []
                   )
            )


tutorialAutomation : Tutorial -> ( Tutorial, TutorialActionList )
tutorialAutomation activeTutorial =
    -- This is the hook where we need to keep track of control positions etc.
    -- I.E. not only on state transitions.
    -- Note the actions must be decoded and applied by the caller.
    case activeTutorial of
        Nothing ->
            ( activeTutorial, [] )

        Just tutorial ->
            ( activeTutorial, (Zipper.current tutorial).stateActions )


findTutorialSubject : Model -> Maybe UiComponent
findTutorialSubject model =
    -- This helper for UI pages to know which element to highlight.
    case model.tutorialActive of
        Just stage ->
            Just <| (Zipper.current stage).uiComponent

        Nothing ->
            Nothing


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
