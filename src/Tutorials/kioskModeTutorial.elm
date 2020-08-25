module Tutorials.KioskModeTutorial exposing (kioskModeTutorial)

import Tutorials.ActionCodes exposing (TutorialActionCode(..), TutorialScenario(..), TutorialTextFunction(..))
import Tutorials.Actions exposing (..)
import Tutorials.Model exposing (TutorialEntry)
import Types exposing (UiComponent(..))
import Zipper



--TODO: Remove some steps to reduce repetition
--TODO: Combine multiple raids in one display
--DROP: Display context banner (e.g. "IFF") (could use actions for this)
--DONE: Clear calculator after each raid
--TODO: End with a more realistic picture on the tube
--DONE: Try with eCrisper.
--DONE: Allow varying times. (Proportional to text length.)
--DONE: Pressing range when in height mode automatically exits height mode.
--TODO: Include a splash screen in the tutorial cycle.
--TODO: Reset model/webgl time each cycle.


kioskModeTutorial : Maybe (Zipper.Zipper TutorialEntry)
kioskModeTutorial =
    Zipper.fromList
        [ TutorialEntry
            UiCRT
            [ ActionInitialiseTutorial
            , ActionCentraliseKnobs
            , ActionClearTargets
            ]
            noStateActions
            noExitActions
            (TextConstant """The "tube" shows radar signals returned from raids within 100 miles.""")
        , TutorialEntry
            UiCRT
            noEntryActions
            noStateActions
            noExitActions
            (TextConstant """The two large dips are caused by local buildings or hills.
            The jiggling is electrical noise in the sensitive receivers.
            """)
        , TutorialEntry
            UiGoniometer
            [ ActionBearingMode
            , ActionClearCalculator
            , ActionClearTargets
            ]
            [ ActionSwingGoniometer ]
            noExitActions
            (TextConstant
                """The operator "swings" the gonio, looking for new signals.
             """
            )
        , TutorialEntry
            UiCRT
            [ ActionSetupRaid
            , ActionSetupRaidFriendlyOutbound
            ]
            [ ActionStopGonioAwayFromRaidBearing ]
            noExitActions
            (TextConstant
                """Here are two raids, each is a single aircraft.
                One is friendly, shown by the pulsing IFF pattern.
             """
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionFindRangeOfNumberedTarget True 0 ]
            [ ActionFindRangeOfNumberedTarget False 0 ]
            (TextConstant
                """Turn the Range knob so the Range Pointer points at a raid.
            """
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 0 ]
            [ ActionFindBearingOfNumberedTarget False 0 ]
            (TextConstant
                """Turn the gonio to minimise the depth of the V."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Press the GONIO button to store the bearing in the calculator."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Press the RANGE button to store the range in the calculator."""
            )
        , TutorialEntry
            UiHeight
            [ ActionHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """Press the HEIGHT button to begin working out the height."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSeekElevationOfNumberedTarget True 0 ]
            [ ActionSeekElevationOfNumberedTarget False 0 ]
            (TextConstant
                """Turn the gonio again to minimise the V."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreElevation ]
            (TextConstant
                """Store the GONIO setting to work out the height."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionFindRangeOfNumberedTarget True 0 ]
            [ ActionFindRangeOfNumberedTarget False 0 ]
            (TextConstant
                """Adjusts the range pointer."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange
            , ActionEndHeightMode
            ]
            (TextConstant
                """Press the RANGE button to store the range."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 1
            , ActionStoreFriendly
            ]
            noStateActions
            noExitActions
            (TextConstant
                """Press Raid Strength 1 and 'F' to show one friendly aircraft. """
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator
            ]
            noStateActions
            [ ActionShowOperator
            , ActionClearCalculator
            ]
            TextInterpretCalculator
        , TutorialEntry
            UiClear
            [ ActionClearCalculator
            ]
            noStateActions
            noExitActions
            (TextConstant
                """Clear the calculator ready for the next raid."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionFindRangeOfNumberedTarget True 1 ]
            [ ActionFindRangeOfNumberedTarget False 1 ]
            (TextConstant
                """Turn the Range knob so the Range Pointer points at the other raid.
            """
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 1 ]
            [ ActionFindBearingOfNumberedTarget False 1 ]
            (TextConstant
                """Turn the gonio to minimise the depth of the V."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Press the GONIO button to store the bearing in the calculator."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Press the RANGE button to store the range in the calculator."""
            )
        , TutorialEntry
            UiHeight
            [ ActionHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """Press the HEIGHT button to begin working out the height."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSeekElevationOfNumberedTarget True 1 ]
            [ ActionSeekElevationOfNumberedTarget False 1 ]
            (TextConstant
                """Turn the gonio again to minimise the V."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreElevation ]
            (TextConstant
                """Store the GONIO setting to work out the height."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionFindRangeOfNumberedTarget True 1 ]
            [ ActionFindRangeOfNumberedTarget False 1 ]
            (TextConstant
                """Adjusts the range pointer."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange
            , ActionEndHeightMode
            ]
            (TextConstant
                """Press the RANGE button to store the range."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 1
            ]
            noStateActions
            noExitActions
            (TextConstant
                """Press Raid Strength 1 to indicate one aircraft. """
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator
            ]
            noStateActions
            [ ActionShowOperator
            , ActionClearCalculator
            ]
            TextInterpretCalculator
        , TutorialEntry
            UiCRT
            [ ActionClearTargets
            , ActionSetupRaid2SameBearing
            , ActionSetupRaid2DifferentBearings
            , ActionSetupRaid3to6
            , ActionClearCalculator
            ]
            noStateActions
            noExitActions
            (TextConstant
                """Here are three raids. The regular beats signify two aircraft.
                The irregular pattern signifies three to six aircraft.
            """
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionFindRangeOfNumberedTarget True 0 ]
            [ ActionFindRangeOfNumberedTarget False 0 ]
            (TextConstant
                """Turn the rang knob so the pointer aligns with a raid.
            """
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 0 ]
            [ ActionFindBearingOfNumberedTarget False 0 ]
            (TextConstant
                """Turn the gonio to shrink the V on the CRT vanishes."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Press the GONIO button to store the bearing."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Press the RANGE button to store the range."""
            )
        , TutorialEntry
            UiHeight
            [ ActionHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """Press HEIGHT button to work out the height."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSeekElevationOfNumberedTarget True 0 ]
            [ ActionSeekElevationOfNumberedTarget False 0 ]
            (TextConstant
                """Turn the gonio again, to minimise the V."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreElevation ]
            (TextConstant
                """Store GONIO setting to work out the elevation."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionFindRangeOfNumberedTarget True 0 ]
            [ ActionFindRangeOfNumberedTarget False 0 ]
            (TextConstant
                """Check the range pointer."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Store the range."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 2 ]
            noStateActions
            noExitActions
            (TextConstant
                """Press Raid Strength 2, because this beating signal is two aircraft."""
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , TutorialEntry
            UiRangeKnob
            [ ActionClearCalculator
            , ActionEndHeightMode
            ]
            [ ActionFindRangeOfNumberedTarget True 2 ]
            [ ActionFindRangeOfNumberedTarget False 2 ]
            (TextConstant
                """Move the range pointer to the next raid."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSwingGoniometer ]
            noExitActions
            (TextConstant
                """We can't make the 'V' disappear completely.
            The aircraft are at the same range, on different bearings."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 2 ]
            [ ActionFindBearingOfNumberedTarget False 2 ]
            (TextConstant
                """We find the bearing where the V stops going up and down.
            This means we have located one of the aircraft."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Store the bearing."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Store the range."""
            )
        , TutorialEntry
            UiHeight
            []
            noStateActions
            []
            (TextConstant
                """We don't try to find the height because we can't know which aircraft it is."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 1 ]
            noStateActions
            noExitActions
            (TextConstant
                """Press Raid Strength 1 for one aircraft."""
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
            [ ActionFindBearingOfNumberedTarget True 3 ]
            [ ActionFindBearingOfNumberedTarget False 3
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
            [ ActionShowOperator
            , ActionClearCalculator
            ]
            TextInterpretCalculator
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionFindRangeOfNumberedTarget True 4 ]
            [ ActionFindRangeOfNumberedTarget False 4 ]
            (TextConstant
                """Move the range pointer to the next raid."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionFindBearingOfNumberedTarget True 4 ]
            [ ActionFindBearingOfNumberedTarget False 4 ]
            (TextConstant
                """This is a close formation of aircraft so it will still D/F out."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreBearing ]
            (TextConstant
                """Store the bearing in the calculator."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Store the range in the calculator."""
            )
        , TutorialEntry
            UiHeight
            [ ActionHeightMode ]
            noStateActions
            noExitActions
            (TextConstant
                """Now work out the height."""
            )
        , TutorialEntry
            UiGoniometer
            noEntryActions
            [ ActionSeekElevationOfNumberedTarget True 4 ]
            [ ActionSeekElevationOfNumberedTarget False 4 ]
            (TextConstant
                """Turn the gonio again, to minimise the V."""
            )
        , TutorialEntry
            UiGonioButton
            noEntryActions
            noStateActions
            [ ActionStoreElevation ]
            (TextConstant
                """Store the goniometer setting."""
            )
        , TutorialEntry
            UiRangeKnob
            noEntryActions
            [ ActionFindRangeOfNumberedTarget True 4 ]
            [ ActionFindRangeOfNumberedTarget False 4 ]
            (TextConstant
                """Check the range."""
            )
        , TutorialEntry
            UIRangeButton
            noEntryActions
            noStateActions
            [ ActionStoreRange ]
            (TextConstant
                """Store the range."""
            )
        , TutorialEntry
            UiRaidStrength
            [ ActionStoreStrength 3
            , ActionStorePlus
            ]
            noStateActions
            noExitActions
            (TextConstant
                """Press Raid Strength 3 and the + sign."""
            )
        , TutorialEntry
            UiCalculator
            [ ActionShowCalculator ]
            noStateActions
            [ ActionShowOperator ]
            TextInterpretCalculator
        , tutorialCloseStep ScenarioKioskMode
        ]
