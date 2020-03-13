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
    , tutorial : Bool -- highlight this raid on the CRT for tutorial mode.
    }


type alias PolarTarget =
    { r : Float -- metres
    , theta : Float -- radians
    , alpha : Float -- radians, ignoring curvature for now
    , iff : Maybe Int -- pulsing; time when pulse started
    , iffActive : Bool -- pulsing now.
    , tutorial : Bool
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
    , tutorial = target.tutorial
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
    -- Find target nearest to range pointer
    let
        pairsOfRangeAndHeights =
            List.map2 (\r1 p1 -> ( abs (p1.r - range * 1600), r1.height ))
                targets
                polarTargets

        minByFst ( r1, h1 ) ( r2, h2 ) =
            if r1 < r2 then
                ( r1, h1 )

            else
                ( r2, h2 )
    in
    case ( targets, polarTargets ) of
        ( [], [] ) ->
            Nothing

        ( [ r1 ], [ _ ] ) ->
            Just r1.height

        ( _, _ ) ->
            Just <| Tuple.second <| List.foldl minByFst ( 1000000, 0 ) pairsOfRangeAndHeights


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
