module Messages exposing (..)

import Time exposing (..)
import Types exposing (..)


type Msg
    = NoOp
    | TimeDelta Float -- WebGL animation time
    | UpdateModel Time.Posix -- Time for model updates
    | SetStartTime Time.Posix
    | StartScenario
    | SetConfigStateMsg Int Bool
    | DisplayReceiver
    | DisplayConfiguration
    | DisplayCalculator
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
    | SelectGoniometerMode Bool
    | ToggleMenu
    | StoreGoniometerSetting
    | StoreRangeSetting
    | ResetInputState
    | RaidStrength Int
    | RaidStrengthPlus
    | RaidFriendly
    | SetOperatorMode OperatorMode
    | DummyMessage
