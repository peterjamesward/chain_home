module Messages exposing (..)

import Time exposing (..)

type Msg
    = NoOp
    | SetConfigStateMsg Int Bool
    | DisplayReceiver
    | DisplayConfiguration
    | Tick Time.Posix
    | KeyChanged Bool String
    | GonioGrab ( Float, Float )
    | GonioMove ( Float, Float )
    | GonioRelease ( Float, Float )
    | AdjustRangeValue Float

