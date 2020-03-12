module Types exposing (..)


type alias Angle =
    Float


type alias Point =
    ( Float, Float )


type alias Line =
    ( Point, Point )

type alias Range =
    Float

type GoniometerMode
    = Azimuth
    | Elevation

type alias Antenna =
    { verticalLobeFunction : Float -> Float
    , horizontalLobeFunction : Float -> Float
    }

type InputState
    -- Inferred sequence of operator actions
    = BearingInput
    | BearingRangeInput
    | HeightInput
    | HeightRangeInput

type OperatorMode
    = Training  -- On-screen narrative and prompts TBD.
    | Experienced -- Absent above.