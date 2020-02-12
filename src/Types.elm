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
    = Bearing
    | Elevation

type alias Antenna =
    { verticalLobeFunction : Float -> Float
    , horizontalLobeFunction : Float -> Float
    }
