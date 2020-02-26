module Target exposing (..)

import Html exposing (..)
import Spherical exposing (..)
import Station exposing (Station)
import Time


type alias Target =
    { latitude : Float
    , longitude : Float
    , height : Float -- in thousands of feet
    , bearing : Float -- in degrees from North
    , speed : Float -- miles per hour (!)
    , iff : Bool
    }


type alias PolarTarget =
    { r : Float -- metres
    , theta : Float -- radians
    , alpha : Float -- radians, ignoring curvature for now
    , iff : Bool -- is this a pipsqueak equipped friendly?
    }


mapToPolar : Station -> Target -> PolarTarget
mapToPolar station target =
    -- Convert from Cartesian (and imperial) map coordinates to
    -- polar (and metric) relative to station position and line of shoot.
    -- Note that target height are specified in '000 feet.
    let
        stationPos =
            ( station.latitude, station.longitude )

        targetPos =
            ( target.latitude, target.longitude )

        rng =
            range stationPos targetPos

        heightInMetres =
            target.height * 304.8
    in
    { r = rng
    , theta = bearing stationPos targetPos - station.lineOfShoot
    , alpha = atan2 heightInMetres rng - asin (heightInMetres / meanRadius)
    , iff = target.iff
    }


targetAtTime : Time.Posix -> Time.Posix -> Target -> Target
targetAtTime t startTime target =
    -- Targets move! t in seconds to at least centisecond resolution please
    let
        tempusFugit =
            -- milliseconds elapsed
            Time.posixToMillis t - Time.posixToMillis startTime

        distanceTravelled =
            -- switch from mph to km/s
            toFloat tempusFugit * target.speed * 1609 / 3600000

        ( newLat, newLong ) =
            -- Use the spherical stuff.
            newPosition ( target.latitude, target.longitude )
                distanceTravelled
                target.bearing
    in
    { target
        | latitude = newLat
        , longitude = newLong
    }


viewPolarTarget p1 =
    [ Html.text "r "
    , Html.text <| String.fromFloat <| p1.r
    , Html.br [] []
    , Html.text "theta "
    , Html.text <| String.fromFloat <| (180 / pi) * p1.theta
    , Html.br [] []
    , Html.text "alpha "
    , Html.text <| String.fromFloat <| (180 / pi) * p1.alpha
    , Html.br [] []
    , Html.hr [] []
    ]
