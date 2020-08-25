module Tutorials.ActionCodes exposing (..)


type TutorialTextFunction
    = TextConstant String
    | TextInterpretCalculator


type TutorialScenario
    = ScenarioBasic
    | ScenarioTwoTogether
    | ScenarioTwoSeparate
    | ScenarioThreeToSix
    | ScenarioFriendly
    | ScenarioKioskMode


type
    TutorialActionCode
    {- Long list of action codes here so the actual actions can be in a dictionary.
       I hope that this encoding breaks the import cycles and mutual dependencies
       between Model and Tutorial. Note that these are *Model* affecting functions,
       so perhaps the tutorial engine can return a list of actions and the next state,
       moving the final execution of model updates back in the Model code.
    -}
    = ActionBearingMode
    | ActionInitialiseTutorial
    | ActionCentraliseKnobs
    | ActionChaseTheRaidRange Bool
    | ActionClearCalculator
    | ActionClearTargets
    | ActionEndHeightMode
    | ActionExitTutorial
    | ActionFindBearingOfNumberedTarget Bool Int
    | ActionFindRangeOfNumberedTarget Bool Int
    | ActionHeightMode
    | ActionRecordScenarioDone TutorialScenario
    | ActionSeekElevation Bool
    | ActionSeekElevationOfNumberedTarget Bool Int
    | ActionSetupRaid
    | ActionSetupRaidFriendlyOutbound
    | ActionSetupRaid2DifferentBearings
    | ActionSetupRaid3to6
    | ActionSetupRaid2SameBearing
    | ActionShowCalculator
    | ActionShowOperator
    | ActionStopGonioAwayFromRaidBearing
    | ActionStoreBearing
    | ActionStoreElevation
    | ActionStoreFriendly
    | ActionStorePlus
    | ActionStoreRange
    | ActionStoreStrength Int
    | ActionSwingGoniometer
