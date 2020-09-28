module Tutorials.KioskModeTutorial exposing (kioskModeTutorial)

import SplashPage exposing (splashText)
import Tutorials.ActionCodes exposing (TutorialActionCode(..), TutorialScenario(..), TutorialTextFunction(..))
import Tutorials.Actions exposing (..)
import Tutorials.Model exposing (TutorialEntry)
import Types exposing (UiComponent(..))
import Zipper


kioskModeTutorial : Maybe (Zipper.Zipper TutorialEntry)
kioskModeTutorial =
    Zipper.fromList <|
        demoIntro
            ++ demoOnes
            ++ demoTwosAndThrees
            ++ demoMassRaids
            ++ demoFinale


demoIntro =
    [ TutorialEntry
        UiDummy
        [ ActionShowSplashPage ]
        noStateActions
        noExitActions
        (TextConstant splashText)
    , TutorialEntry
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
        (TextConstant """The two dips are caused by local buildings or hills.
                              The jiggling is electrical noise in the sensitive receivers.
                              """)
    , TutorialEntry
        UiGoniometer
        [ ActionBearingMode
        , ActionClearCalculator
        , ActionClearTargets
        ]
        [ ActionSwingGoniometer ]
        []
        (TextConstant
            """It's important to "swing" the gonio when looking for new signals.
                               """
        )
    ]


demoOnes =
    [ TutorialEntry
        UiCRT
        [ ActionSetupRaid
        , ActionSetupRaidFriendlyOutbound
        ]
        [ ActionStopGonioAwayFromRaidBearing ]
        noExitActions
        (TextConstant
            """Here are two raids. Each is a single aircraft.
                                  The pulsing pattern is a friendly aircraft using IFF.
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
            """Store the GONIO setting."""
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
        [ ActionStoreRange
        , ActionEndHeightMode
        ]
        (TextConstant
            """Store the range."""
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
            """Turn the gonio to minimise the V."""
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
        [ ActionHeightMode ]
        noStateActions
        noExitActions
        (TextConstant
            """Press the HEIGHT button."""
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
            """Store the gonio setting."""
        )
    , TutorialEntry
        UiRangeKnob
        noEntryActions
        [ ActionFindRangeOfNumberedTarget True 1 ]
        [ ActionFindRangeOfNumberedTarget False 1 ]
        (TextConstant
            """Check the range."""
        )
    , TutorialEntry
        UIRangeButton
        noEntryActions
        noStateActions
        [ ActionStoreRange
        , ActionEndHeightMode
        ]
        (TextConstant
            """Store the range."""
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
    ]


demoTwosAndThrees =
    [ TutorialEntry
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
            """Here are three more raids. The regular beats signify two aircraft.
                                           The irregular pattern signifies three to six aircraft.
                                       """
        )
    , TutorialEntry
        UiRangeKnob
        noEntryActions
        [ ActionFindRangeOfNumberedTarget True 0 ]
        [ ActionFindRangeOfNumberedTarget False 0 ]
        (TextConstant
            """Turn the range knob so the pointer aligns with a raid.
                                       """
        )
    , TutorialEntry
        UiGoniometer
        noEntryActions
        [ ActionFindBearingOfNumberedTarget True 0 ]
        [ ActionFindBearingOfNumberedTarget False 0 ]
        (TextConstant
            """Turn the gonio to shrink the V."""
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
        [ ActionHeightMode ]
        noStateActions
        noExitActions
        (TextConstant
            """Press HEIGHT button."""
        )
    , TutorialEntry
        UiGoniometer
        noEntryActions
        [ ActionSeekElevationOfNumberedTarget True 0 ]
        [ ActionSeekElevationOfNumberedTarget False 0 ]
        (TextConstant
            """Turn the gonio minimise the V."""
        )
    , TutorialEntry
        UiGonioButton
        noEntryActions
        noStateActions
        [ ActionStoreElevation ]
        (TextConstant
            """Store the gonio setting."""
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
        [ ActionShowOperator
        , ActionEndHeightMode
        , ActionClearCalculator
        ]
        TextInterpretCalculator
    , TutorialEntry
        UiRangeKnob
        noEntryActions
        [ ActionFindRangeOfNumberedTarget True 1 ]
        [ ActionFindRangeOfNumberedTarget False 1 ]
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
        [ ActionFindBearingOfNumberedTarget True 2 ]
        [ ActionFindBearingOfNumberedTarget False 2 ]
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
        UiRangeKnob
        noEntryActions
        [ ActionFindRangeOfNumberedTarget True 3 ]
        [ ActionFindRangeOfNumberedTarget False 3 ]
        (TextConstant
            """Move the range pointer to the next raid."""
        )
    , TutorialEntry
        UiGoniometer
        noEntryActions
        [ ActionFindBearingOfNumberedTarget True 3 ]
        [ ActionFindBearingOfNumberedTarget False 3 ]
        (TextConstant
            """This close formation of aircraft will still "D/F out"."""
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
        [ ActionHeightMode ]
        noStateActions
        noExitActions
        (TextConstant
            """Now work out the height."""
        )
    , TutorialEntry
        UiGoniometer
        noEntryActions
        [ ActionSeekElevationOfNumberedTarget True 3 ]
        [ ActionSeekElevationOfNumberedTarget False 3 ]
        (TextConstant
            """Minimise the V."""
        )
    , TutorialEntry
        UiGonioButton
        noEntryActions
        noStateActions
        [ ActionStoreElevation ]
        (TextConstant
            """Store the gonio setting."""
        )
    , TutorialEntry
        UiRangeKnob
        noEntryActions
        [ ActionFindRangeOfNumberedTarget True 3 ]
        [ ActionFindRangeOfNumberedTarget False 3 ]
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
    ]


demoMassRaids =
    [ TutorialEntry
        UiCRT
        [ ActionClearTargets
        , ActionMassRaids
        , ActionCentraliseKnobs
        , ActionClearCalculator
        , ActionShowOperator
        , ActionEndHeightMode
        ]
        [ ActionSwingGoniometer ]
        [ ActionStopGonioAwayFromRaidBearing ]
        (TextConstant
            """
                Here are two larger raids.
            """
        )
    , TutorialEntry
        UiRangeKnob
        noEntryActions
        [ ActionFindRangeOfNumberedTarget True 0 ]
        [ ActionFindRangeOfNumberedTarget False 0 ]
        (TextConstant
            """Set the Range Pointer points to the nearest raid.
                The wider the shape on the tube, the more aircraft in the raid.
            """
        )
    , TutorialEntry
        UiGoniometer
        noEntryActions
        [ ActionFindBearingOfNumberedTarget True 0 ]
        [ ActionFindBearingOfNumberedTarget False 0 ]
        (TextConstant
            """Large raids may not D?F out completely."""
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
        [ ActionHeightMode ]
        noStateActions
        noExitActions
        (TextConstant
            """Press the HEIGHT button."""
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
            """Store the gonio setting."""
        )
    , TutorialEntry
        UiRangeKnob
        noEntryActions
        [ ActionFindRangeOfNumberedTarget True 0 ]
        [ ActionFindRangeOfNumberedTarget False 0 ]
        (TextConstant
            """Check the range."""
        )
    , TutorialEntry
        UIRangeButton
        noEntryActions
        noStateActions
        [ ActionStoreRange
        , ActionEndHeightMode
        ]
        (TextConstant
            """Store the range."""
        )
    , TutorialEntry
        UiRaidStrength
        [ ActionStoreStrength 12
        ]
        noStateActions
        noExitActions
        (TextConstant
            """Press Raid Strength 12 as an estimate of size. """
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
    ]


demoFinale =
    [ TutorialEntry
        UiCRT
        [ ActionClearCalculator
        , ActionClearTargets
        , ActionSetupRaid
        , ActionSetupRaid2DifferentBearings
        , ActionSetupRaid2SameBearing
        , ActionSetupRaid3to6
        , ActionSetupRaidFriendlyOutbound
        , ActionMassRaids
        , ActionCentraliseKnobs
        , ActionShowOperator
        ]
        [ ActionSwingGoniometer ]
        noExitActions
        (TextConstant
            """In the heat of battle, the operator must maintain concentration to track multiple raids.
                            A constant stream of reports must be passed to the control rooms so they can respond.
                            """
        )
    ]
