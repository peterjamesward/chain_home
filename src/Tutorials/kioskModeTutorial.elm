module Tutorials.KioskModeTutorial exposing (kioskModeTutorial)

import Tutorials.ActionCodes exposing (TutorialActionCode(..), TutorialScenario(..), TutorialTextFunction(..))
import Tutorials.Actions exposing (..)
import Tutorials.Model exposing (TutorialEntry)
import Types exposing (UiComponent(..))
import Zipper



--TODO: Remove some steps to reduce repetition
--TODO: Combine multiple raids in one display
--TODO: Display context banner (e.g. "IFF") (could use actions for this)
--DONE: Clear calculator after each raid
--TODO: End with a more realistic picture on the tube
--DONE: Try with eCrisper.
--DONE: Allow varying times. (Proportional to text length.)
--DONE: Pressing range when in height mode automatically exits height mode.


kioskModeTutorial : Maybe (Zipper.Zipper TutorialEntry)
kioskModeTutorial =
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
                """This tutorial gives an overview of the receiver and its operation."""
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

            """)
        , TutorialEntry
            UiCRT
            noEntryActions
            noStateActions
            noExitActions
            (TextConstant """The two large dips near the left are caused by local features such as buildings or hills.
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
            The operator must "swing" the gonio, or new raids could be missed.
             """
            )
        , TutorialEntry
            UiCRT
            [ ActionSetupRaid ]
            --setupTutorialRaid
            [ ActionStopGonioAwayFromRaidBearing ]
            noExitActions
            (TextConstant
                """The V shape under the 100 on the tube is a new raid.
            This shape means there is only one aircraft.
             """
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionChaseTheRaidRange True ]
            [ ActionChaseTheRaidRange False ]
            (TextConstant
                """The operator turns the Range knob until the Range Pointer points at the
               raid (ideally at the left hand edge) for an accurate range reading.
            """
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 0 ]
            [ ActionFindBearingOfNumberedTarget False 0 ]
            (TextConstant
                """The operator turns the gonio to minimise the depth of the V."""
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
            The goniometer can compare signals received at aerials at different heights.
            Height finding is at best approximate and often impossible."""
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
                """The operator adjusts the range pointer because the raid has moved."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange
            , ActionEndHeightMode
            ]
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
            UiCalculator
            [ ActionShowCalculator
            ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , TutorialEntry
            UiCRT
            [ ActionSetupRaid2SameBearing
            , ActionClearCalculator
            ]
            noStateActions
            noExitActions
            (TextConstant
                """The V shape on the 'tube' is a new raid.
            The regular "beating" up and down is always two aircraft.
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
                """Return to Bearing finding mode."""
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , TutorialEntry
            UiCRT
            [ ActionSetupRaid2DifferentBearings
            , ActionClearCalculator
            ]
            noStateActions
            noExitActions
            (TextConstant
                """The white V shape on the 'tube' is a new raid.
            The regular "beating" up and down is always two aircraft.
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
        , TutorialEntry
            UiGoniometer
            [ ActionBearingMode
            , ActionClearCalculator
            , ActionClearTargets
            ]
            [ ActionSwingGoniometer ]
            noExitActions
            (TextConstant
                """The operator swings the gonio looking for a new raid.
             """
            )
        , TutorialEntry
            UiCRT
            [ ActionSetupRaid3to6
            , ActionClearCalculator
            ]
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
        , TutorialEntry
            UiCRT
            [ ActionSetupRaidFriendlyOutbound
            , ActionClearCalculator
            ]
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
        , tutorialCloseStep ScenarioKioskMode
        ]
