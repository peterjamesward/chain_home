module Station exposing (Station, stationClutter)


type alias Station = { latitude    : Float
                     , longitude   : Float
                     , lineOfShoot : Float
                     }

-- Generate a bunch of nearby clutter. Also stress test.

--stationClutter : Station -> List Target
stationClutter station n = 
  List.map (\i -> { latitude = station.latitude + 0.0001 * (toFloat i)
                  , longitude = station.longitude + 0.0001 * (toFloat i)
                  , height =  1 / (1 + sqrt (toFloat i))
                  , bearing = toFloat i
                  , speed = 0.0
                  , iff = False
                  })
    (List.range 1 n)


