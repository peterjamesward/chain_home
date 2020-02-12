module Station exposing (Station, stationClutter)


type alias Station = { latitude    : Float
                     , longitude   : Float
                     , lineOfShoot : Float
                     }

-- Generate a bunch of nearby clutter. Also stress test.

--stationClutter : Station -> List Target
stationClutter station n = 
  List.map (\i -> { latitude = station.latitude + 0.0001 * toFloat (modBy 5 i)* toFloat (modBy 5 i)
                  , longitude = station.longitude + 0.0001 * toFloat (modBy 7 i)* toFloat (modBy 7 i)
                  , height =  1 / (100 + sqrt (toFloat i))
                  , bearing = toFloat i * 9
                  , speed = 1.0
                  , iff = False
                  })
    (List.range 1 n)


