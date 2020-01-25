module Config exposing (..)

-- Let's make ourselves a station and a target
-- Bawdsey, assuming LOS due East.
bawdsey = { longitude   = degrees 1.408614
          , latitude    = degrees 51.993661 
          , lineOfShoot = degrees 90.0 
          }  

bomber1 = { longitude = degrees 2.0
          , latitude  = degrees 52.0
          , height    = 30 -- ,000 ft
          , bearing   = degrees 280
          , speed     = 200.0 -- mph
          , iff       = False 
          }
 
bomber2 = { longitude = degrees 2.000
          , latitude  = degrees 52.05
          , height    = 30.1 -- ,000 ft
          , bearing   = degrees 280
          , speed     = 200.0 -- mph
          , iff       = False 
          }
 
bomber2A = { longitude = degrees 2.000
          , latitude  = degrees 52.05
          , height    = 30.1 -- ,000 ft
          , bearing   = degrees 280
          , speed     = 200.0 -- mph
          , iff       = False 
          }
 
bomber3 = { longitude = bawdsey.longitude + (degrees 0.8)
          , latitude  = bawdsey.latitude + (degrees 0.5)
          , height    = 20 -- ,000 ft
          , bearing   = degrees 270
          , speed     = 220 -- mph
          , iff       = False 
          }
 
bomber4 = { longitude = bawdsey.longitude + (degrees 0.8) 
          , latitude  = bawdsey.latitude - (degrees 0.5)
          , height    = 20 -- ,000 ft
          , bearing   = degrees 270
          , speed     = 220 -- mph
          , iff       = False 
          }
 
fighter1 = { longitude = degrees 1.8
           , latitude  = bawdsey.latitude - (degrees 0.1)
           , height    = 10 -- ,000 ft
           , bearing   = degrees 90
           , speed     = 400 -- mph
           , iff       = False 
           }

tenAbreast = List.map (\i ->
                { longitude = bawdsey.longitude + (degrees 0.7)
                , latitude  = bawdsey.latitude + (degrees (toFloat i-5) * 0.001)
                , height    = 25
                , bearing   = 270
                , speed     = 300
                , iff       = False
                }
            )
            <| List.range 1 10

tenAligned = List.map (\i ->
                { longitude = bawdsey.longitude + (degrees 0.9) + (degrees (toFloat i-5) * 0.004)
                , latitude  = bawdsey.latitude
                , height    = 25
                , bearing   = 270
                , speed     = 300
                , iff       = False
                }
            )
            <| List.range 1 10


targetsBaseline =   [ bomber1
                    , bomber2
                    --, bomber2A
                    --, bomber3
                    --, bomber4
                    --, fighter1 
                    ]
                    ++ tenAligned
                    ++ tenAbreast


