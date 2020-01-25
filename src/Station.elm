module Station exposing (..)

import Target exposing (..)

type alias Station = { latitude    : Float
                     , longitude   : Float
                     , lineOfShoot : Float
                     }

-- Generate a bunch of nearby clutter. Also stress test.

stationClutter : Station -> List Target
stationClutter station = 
  List.map (\i -> { latitude = station.latitude + 0.01 * (sin (toFloat i))
                  , longitude = station.longitude + 0.01 * (sin (toFloat i))
                  , height = 0.1 * sqrt (toFloat i) 
                  , bearing = toFloat i
                  , speed = toFloat i * 0.001
                  , iff = False
                  })
    (List.range 1 100)


