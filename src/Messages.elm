module Messages exposing (..)

import Time exposing (..)
import Tutorials.ActionCodes exposing (TutorialScenario)
import Tutorials.Messages exposing (TutorialMsg)
import Types exposing (..)


type Msg
    = NoOp
    | TimeDelta Float -- WebGL animation time
    | UpdateModel Time.Posix -- Time for model updates
    | SetStartTime Time.Posix
    | StartScenario GameMode
    | SetConfigStateMsg TutorialScenario Bool
    | DisplayReceiver
    | DisplayConfiguration
    | DisplayCalculator
    | DisplayAboutPage
    | DisplayMapPage
    | KeyChanged Bool String
    | GonioGrab Point
    | GonioMove Point
    | GonioRelease Point
    | AdjustRangeValue Float
    | DeviceResize Int Int
    | RangeGrab Point
    | RangeMove Point
    | RangeRelease Point
    | SelectReceiveAntenna Bool
    | EnableReflector Bool
    | SelectGoniometerMode GoniometerMode
    | StoreGoniometerSetting
    | StoreRangeSetting
    | ResetInputState
    | RaidStrength Int
    | RaidStrengthPlus
    | RaidFriendly
    | ExplainModeToggle
    | RandomRaidGenerated ( Float, Float )
    | MapMessage
    | ToggleMenu Bool
    | TutorialMsg TutorialMsg
    | SetActualTraceVisible Bool
    | SetRangeCircleVisible Bool
