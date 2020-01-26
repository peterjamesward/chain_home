module Target exposing (..)

import Spherical exposing (..)
import Station exposing (Station)
import Html exposing (..)

type alias Target = { latitude   : Float
                     , longitude : Float
                     , height    : Float  -- in thousands of feet
                     , bearing   : Float -- in degrees from North
                     , speed     : Float   -- miles per hour (!)
                     , iff       : Bool }

type alias PolarTarget = { r     : Float -- metres
                         , theta : Float -- radians
                         , alpha : Float -- radians, ignoring curvature for now
                         , iff   : Bool -- is this a pipsqueak equipped friendly?
                         }

defaultPolarTarget = { r = 0, theta = 0, alpha = 0, iff = False }

-- Convert from Cartesian (and imperial) map coordinates to 
-- polar (and metric) relative to station position and line of shoot.

mapToPolar : Station -> Target -> PolarTarget
mapToPolar station target = 
  let stationPos = (station.latitude, station.longitude)
      targetPos = (target.latitude, target.longitude)
      rng = range stationPos targetPos
  in
     { r = rng
     , theta = (bearing stationPos targetPos) - station.lineOfShoot
     , alpha = atan2 (target.height * 304.8) rng
     , iff = target.iff
     }

-- Targets move! t in seconds to at least centisecond resolution please
targetAtTime : Int -> Int -> Target -> Target
targetAtTime t startTime target = 
  let tempusFugit = t - startTime  -- milliseconds elapsed
      distanceTravelled = (toFloat tempusFugit) * target.speed * 1609 / 3600000
      (newLat, newLong) = newPosition (target.latitude, target.longitude) 
                                       distanceTravelled target.bearing
  in 
    { target | latitude = newLat
             , longitude = newLong 
             }

viewPolarTarget p1 =   [ Html.text "r "
                        , Html.text <| String.fromFloat <| p1.r
                       , Html.br [] []
                       , Html.text "theta "
                       , Html.text <| String.fromFloat <| (108/pi) * p1.theta
                       , Html.br [] []
                       , Html.text "alpha "
                       , Html.text <| String.fromFloat <| (108/pi) * p1.alpha
                       , Html.br [] []
                       , Html.hr [] []
                       ]