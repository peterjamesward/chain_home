module Target exposing (..)

import Spherical exposing (..)

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

