module Messages exposing (..)

import Time exposing (..)
import Types exposing (..)

type Msg
    = NoOp
    | SetConfigStateMsg Int Bool
    | DisplayReceiver
    | DisplayConfiguration
    | Tick Time.Posix
    | KeyChanged Bool String
    | GonioGrab Point
    | GonioMove Point
    | GonioRelease Point
    | AdjustRangeValue Float
    | DeviceResize Int Int
    | RangeGrab Point
    | RangeMove Point
    | RangeRelease Point
    | SelectTransmitAntenna Bool
    | SelectReceiveAntenna Bool
    | EnableReflector Bool
    | SelectGoniometerMode Bool

