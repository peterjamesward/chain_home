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
    , iff : Maybe Int -- the value at which t mod 12 triggers a return
    , iffActive : Bool -- pulsing now.
    }


type alias PolarTarget =
    { r : Float -- metres
    , theta : Float -- radians
    , alpha : Float -- radians, ignoring curvature for now
    , iff : Maybe Int -- pulsing; time when pulse started
    , iffActive : Bool -- pulsing now.
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
    , iffActive = target.iffActive
    }


targetAtTime : Int -> Target -> Target
targetAtTime t target =
    -- Targets move! t in seconds to at least centisecond resolution please
    let
        distanceTravelled =
            -- switch from mph to km/s
            toFloat t * target.speed * 1609 / 3600000

        ( newLat, newLong ) =
            -- Use the spherical stuff.
            newPosition ( target.latitude, target.longitude )
                distanceTravelled
                target.bearing
    in
    { target
        | latitude = newLat
        , longitude = newLong
        , iffActive =
            case target.iff of
                Nothing ->
                    False

                Just n ->
                    modBy 12 (t // 1000) == n && modBy 1000 t > 0 && modBy 1000 t < 500
    }


findTargetElevation : List Target -> List PolarTarget -> Float -> Maybe Float
findTargetElevation targets polarTargets range =
    -- "Cheat" function to avoid inverse height curves, looks to see
    -- if your range pointer is on a an echo, and uses that target's elevation.

    -- Any target will do today.
    Maybe.map .height <| List.head targets

{-
    List.map2 (\rect pol -> ( pol.r, rect.height ))
        targets
        polarTargets
        |> List.filterMap
            (\( r, h ) ->
                if abs (r - range) < 50000 then
                    Just h

                else
                    Nothing
            )
        |> List.minimum
-}


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
