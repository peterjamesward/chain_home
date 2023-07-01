module Config exposing (..)

import Station exposing (Station)
import Target exposing (targetFromProforma)
import Types exposing (Echo, Target, TargetProforma)


groundRays : List Echo
groundRays =
    [ { sequence = 0
      , r = 5000
      , theta = 0 -- ignored as these are injected after D/F
      , alpha = 0
      , strength = 1
      , phase = 0
      , duration = 0
      , amplitude = 4
      }
    , { sequence = 0
      , r = 8000
      , theta = 0 -- ignored as these are injected after D/F
      , alpha = 0
      , strength = 1
      , phase = 0
      , duration = 0
      , amplitude = 10
      }
    , { sequence = 0
      , r = 500
      , theta = 0 -- ignored as these are injected after D/F
      , alpha = 0
      , strength = 1
      , phase = 0
      , duration = 10
      , amplitude = 30
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
    , strength = 1
    , iff = Nothing
    }


singleHostile : TargetProforma
singleHostile =
    { longitude = bawdsey.longitude + degrees 1.0
    , latitude = bawdsey.latitude
    , height = 20 -- ,000 ft
    , heading = degrees 250
    , speed = 200.0 -- mph
    , strength = 1
    , iff = Nothing
    }


pairHostile : TargetProforma
pairHostile =
    { longitude = bawdsey.longitude + degrees 1.25
    , latitude = bawdsey.latitude
    , height = 30.1 -- ,000 ft
    , heading = degrees 280
    , speed = 200.0 -- mph
    , strength = 2
    , iff = Nothing
    }


groupOf20 : TargetProforma
groupOf20 =
    -- Try to get 3 and 4 at similar range but differing in azimuth.
    { longitude = bawdsey.longitude + degrees 1.5
    , latitude = bawdsey.latitude
    , height = 20 -- ,000 ft
    , heading = degrees 270
    , speed = 200 -- mph
    , strength = 20
    , iff = Nothing
    }


incomingSingleFriendly : TargetProforma
incomingSingleFriendly =
    { longitude = bawdsey.longitude + degrees 1.8
    , latitude = bawdsey.latitude
    , height = 10 -- ,000 ft
    , heading = degrees 270
    , speed = 300 -- mph
    , strength = 1
    , iff = Just 1
    }


outgoingFriendlySection : TargetProforma
outgoingFriendlySection =
    { longitude = bawdsey.longitude + degrees 0.1
    , latitude = bawdsey.latitude
    , height = 15 -- ,000 ft
    , heading = degrees 70
    , speed = 300 -- mph
    , strength = 4
    , iff = Just 1
    }


outgoingFriendlySection1a : TargetProforma
outgoingFriendlySection1a =
    -- Split into two, so that one aircraft has IFF on.
    { longitude = bawdsey.longitude + degrees 0.4
    , latitude = bawdsey.latitude + degrees 0.5
    , height = 10 -- ,000 ft
    , heading = degrees 170
    , speed = 300 -- mph
    , strength = 4
    , iff = Nothing
    }


outgoingFriendlySection1b : TargetProforma
outgoingFriendlySection1b =
    -- Split into two, so that one aircraft has IFF on.
    { longitude = bawdsey.longitude + degrees 0.4
    , latitude = bawdsey.latitude + degrees 0.5
    , height = 10 -- ,000 ft
    , heading = degrees 170
    , speed = 300 -- mph
    , strength = 1
    , iff = Just 1
    }


outgoingFriendlySection2a : TargetProforma
outgoingFriendlySection2a =
    -- Split into two, so that one aircraft has IFF on.
    { longitude = bawdsey.longitude + degrees 0.6
    , latitude = bawdsey.latitude - degrees 0.4
    , height = 10 -- ,000 ft
    , heading = degrees 70
    , speed = 300 -- mph
    , strength = 3
    , iff = Nothing
    }


outgoingFriendlySection2b : TargetProforma
outgoingFriendlySection2b =
    -- Split into two, so that one aircraft has IFF on.
    { longitude = bawdsey.longitude + degrees 0.6
    , latitude = bawdsey.latitude - degrees 0.4
    , height = 12 -- ,000 ft
    , heading = degrees 70
    , speed = 300 -- mph
    , strength = 1
    , iff = Just 3
    }


largeGroup1 n =
    { longitude = bawdsey.longitude + degrees 1.8
    , latitude = bawdsey.latitude - degrees 0.3
    , height = 25
    , heading = degrees 275
    , speed = 200
    , strength = n
    , iff = Nothing
    }


largeGroup2 n =
    { longitude = bawdsey.longitude + degrees 1.2
    , latitude = bawdsey.latitude - degrees 1.0
    , height = 30
    , heading = degrees 265
    , speed = 250
    , strength = n
    , iff = Nothing
    }


sharonMode : Int -> List Target
sharonMode timeNow =
    List.map (targetFromProforma station timeNow)
        [ singleHostile
        , pairHostile
        , groupOf20
        , incomingSingleFriendly
        , outgoingFriendlySection
        , outgoingFriendlySection1a
        , outgoingFriendlySection1b
        , outgoingFriendlySection2a
        , outgoingFriendlySection2b
        , largeGroup1 10
        , largeGroup2 10
        ]


bomber4 : TargetProforma
bomber4 =
    { longitude = bawdsey.longitude + degrees 1.6
    , latitude = bawdsey.latitude - degrees 0.3
    , height = 40 -- ,000 ft
    , heading = degrees 270
    , speed = 200 -- mph
    , strength = 1
    , iff = Nothing
    }


trainingMode : Int -> List Target
trainingMode timeNow =
    List.map (targetFromProforma station timeNow)
        [ singleHostile ]


trainingMode2 : Int -> List Target
trainingMode2 timeNow =
    -- Two planes same range same heading
    List.map (targetFromProforma station timeNow)
        [ pairHostile ]


trainingMode3 : Int -> List Target
trainingMode3 timeNow =
    -- Two planes same range different headings
    List.map (targetFromProforma station timeNow)
        [ groupOf20
        , bomber4
        ]


trainingModeFriendlyOutbound : Int -> List Target
trainingModeFriendlyOutbound timeNow =
    List.map (targetFromProforma station timeNow)
        [ incomingSingleFriendly ]


trainingMode3to6 : Int -> List Target
trainingMode3to6 timeNow =
    -- Four aircraft close together
    -- Wonder if it will work using them twice!
    List.map (targetFromProforma station timeNow) <|
        [ largeGroup1 6 ]


trainingMassRaids : Int -> List Target
trainingMassRaids timeNow =
    List.map (targetFromProforma station timeNow) <|
        [ largeGroup2 12
        , largeGroup1 60
        ]
