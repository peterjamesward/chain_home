module Messages exposing (..)

import Time exposing (..)
import Types exposing (..)


type Msg
    = NoOp
    | TimeDelta Float -- WebGL animation time
    | TimeTicker Time.Posix -- Time for model updates
    | SetStartTime Time.Posix
    | StartScenario Scenario
    | StartRandomRaids
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
    | RandomRaidGenerated ( Float, Float )
    | ToggleMenu Bool
    | SetActualTraceVisible Bool
    | SetRangeCircleVisible Bool
