module Config exposing (..)

import Station exposing (Station)
import Target exposing (targetFromProforma)
import Types exposing (Echo, Target, TargetProforma)


groundRays : List Echo
groundRays =
    [ { sequence = 0
      , r = 8000 -- 8km
      , theta = 0 -- ignored as these are injected after D/F
      , alpha = 0
      , phase = 0
      , duration = 0
      , amplitude = 3
      , tutorial = False
      }
    , { sequence = 0
      , r = 10000 -- 10km
      , theta = 0 -- ignored as these are injected after D/F
      , alpha = 0
      , phase = 0
      , duration = 0
      , amplitude = 6
      , tutorial = False
      }
    ]


station =
    bawdsey


bawdsey : Station
bawdsey =
    -- Bawdsey, assuming LOS due East.
    { longitude = degrees 1.408614
    , latitude = degrees 51.993661
    , lineOfShoot = degrees 90.0
    , gridSquareEasting = 0.74
    , gridSquareNorthing = 0.64
    }


behindStation : TargetProforma
behindStation =
    -- This to test reflector function.
    { longitude = bawdsey.longitude - degrees 0.9
    , latitude = bawdsey.latitude + degrees 0.01
    , height = 30 -- ,000 ft
    , heading = degrees 180
    , speed = 200.0 -- mph
    , iff = Nothing
    , tutorial = False
    }


bomber1 : TargetProforma
bomber1 =
    { longitude = bawdsey.longitude + degrees 2.0
    , latitude = bawdsey.latitude + degrees 0.5
    , height = 20 -- ,000 ft
    , heading = degrees 250
    , speed = 200.0 -- mph
    , iff = Nothing
    , tutorial = False
    }


bomber2 : TargetProforma
bomber2 =
    -- 2 and 2A very close to look for "beating" effect.
    { longitude = bawdsey.longitude + degrees 1.2
    , latitude = degrees 52.05
    , height = 30.1 -- ,000 ft
    , heading = degrees 280
    , speed = 200.0 -- mph
    , iff = Nothing
    , tutorial = False
    }


bomber2A : TargetProforma
bomber2A =
    { longitude = bawdsey.longitude + degrees 1.2
    , latitude = degrees 52.04
    , height = 30.2 -- ,000 ft
    , heading = degrees 280
    , speed = 200.0 -- mph
    , iff = Nothing
    , tutorial = False
    }


bomber3 : TargetProforma
bomber3 =
    -- Try to get 3 and 4 at similar range but differing in azimuth.
    { longitude = bawdsey.longitude + degrees 1.608
    , latitude = bawdsey.latitude + degrees 0.3
    , height = 40 -- ,000 ft
    , heading = degrees 270
    , speed = 200 -- mph
    , iff = Nothing
    , tutorial = False
    }


bomber4 : TargetProforma
bomber4 =
    { longitude = bawdsey.longitude + degrees 1.6
    , latitude = bawdsey.latitude - degrees 0.3
    , height = 40 -- ,000 ft
    , heading = degrees 270
    , speed = 200 -- mph
    , iff = Nothing
    , tutorial = False
    }


fighter1 : TargetProforma
fighter1 =
    -- Starts behind and heads out pretty quick.
    { longitude = bawdsey.longitude + degrees 1.8
    , latitude = bawdsey.latitude + degrees 0.01
    , height = 10 -- ,000 ft
    , heading = degrees 270
    , speed = 300 -- mph
    , iff = Just 1
    , tutorial = False
    }


severalAbreast n =
    List.map
        (\i ->
            { longitude = bawdsey.longitude + degrees 2.1
            , latitude = bawdsey.latitude + degrees 0.3 + (degrees (toFloat i - 5) * 0.002)
            , height = 25
            , heading = degrees 275
            , speed = 200
            , iff = Nothing
            , tutorial = False
            }
        )
    <|
        List.range 1 n


severalAligned n =
    List.map
        (\i ->
            { longitude = bawdsey.longitude + degrees 1.9 + (degrees (toFloat i - 5) * 0.002)
            , latitude = bawdsey.latitude - degrees 0.2 + (degrees <| 0.05 * cos (toFloat i))
            , height = 25 + toFloat (modBy 7 (100 * i)) * 0.1
            , heading = degrees 265
            , speed = 250
            , iff = Nothing
            , tutorial = False
            }
        )
    <|
        List.range 1 n


trainingMode : Int -> List Target
trainingMode timeNow =
    List.map (targetFromProforma station timeNow)
        [ { bomber1 | tutorial = False } ]


trainingMode2 : Int -> List Target
trainingMode2 timeNow =
    -- Two planes same range same heading
    List.map (targetFromProforma station timeNow)
        [ { bomber2 | tutorial = False }
        , { bomber2A | tutorial = False }
        ]


trainingMode3 : Int -> List Target
trainingMode3 timeNow =
    -- Two planes same range different headings
    List.map (targetFromProforma station timeNow)
        [ { bomber3 | tutorial = False }
        , { bomber4 | tutorial = False }
        ]


trainingModeFriendlyOutbound : Int -> List Target
trainingModeFriendlyOutbound timeNow =
    List.map (targetFromProforma station timeNow)
        [ { fighter1 | tutorial = False } ]


trainingMode3to6 : Int -> List Target
trainingMode3to6 timeNow =
    let
        placeInTutorialMode t =
            { t | tutorial = False }
    in
    -- Four aircraft close together
    -- Wonder if it will work using them twice!
    List.map (targetFromProforma station timeNow) <|
        severalAligned 5


trainingMassRaids : Int -> List Target
trainingMassRaids timeNow =
    List.map (targetFromProforma station timeNow) <|
        (severalAligned 10 ++ severalAbreast 10)
