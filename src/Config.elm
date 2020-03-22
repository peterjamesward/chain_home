module Config exposing (..)

import Types exposing (Target, TutorialScenario(..))


type alias TargetSelector =
    { id : TutorialScenario
    , active : Bool -- Whether this group is active (dynamic)
    , description : String -- e.g. "mass raid"
    }

availableTargetOptions : List TargetSelector
availableTargetOptions =
    [ TargetSelector ScenarioBasic False "One aircraft at a time"
    , TargetSelector ScenarioTwoTogether False "Two aircraft in close formation"
    , TargetSelector ScenarioTwoSeparate False "Two aircraft at the same range"
    , TargetSelector ScenarioThreeToSix False "Three to six planes in close formation"
    , TargetSelector ScenarioFriendly False "A solitary friendly aircraft"
    ]

groundRays =
    [ { sequence = 0
      , r = 5000
      , theta = 0 -- ignored as these are injected after D/F
      , alpha = 0
      , phase = 0
      , duration = 0
      , amplitude = 10.0
      , tutorial = False
      }
    , { sequence = 0
      , r = 8000
      , theta = 0 -- ignored as these are injected after D/F
      , alpha = 0
      , phase = 0
      , duration = 0
      , amplitude = 8.0
      , tutorial = False
      }
    ]


bawdsey =
    -- Bawdsey, assuming LOS due East.
    { longitude = degrees 1.408614
    , latitude = degrees 51.993661
    , lineOfShoot = degrees 90.0
    }


behindStation =
    -- This to test reflector function.
    { longitude = bawdsey.longitude - degrees 0.9
    , latitude = bawdsey.latitude + degrees 0.01
    , height = 30 -- ,000 ft
    , bearing = degrees 180
    , speed = 200.0 -- mph
    , iff = Nothing
    , iffActive = False
    , tutorial = False
    }


bomber1 =
    { longitude = bawdsey.longitude + degrees 1.2
    , latitude = bawdsey.latitude + degrees 0.5
    , height = 20 -- ,000 ft
    , bearing = degrees 250
    , speed = 200.0 -- mph
    , iff = Nothing
    , iffActive = False
    , tutorial = False
    }


bomber2 =
    -- 2 and 2A very close to look for "beating" effect.
    { longitude = degrees 2.5
    , latitude = degrees 52.05
    , height = 30.1 -- ,000 ft
    , bearing = degrees 280
    , speed = 200.0 -- mph
    , iff = Nothing
    , iffActive = False
    , tutorial = False
    }


bomber2A =
    { longitude = degrees 2.5
    , latitude = degrees 52.04
    , height = 30.2 -- ,000 ft
    , bearing = degrees 280
    , speed = 200.0 -- mph
    , iff = Nothing
    , iffActive = False
    , tutorial = False
    }


bomber3 =
    -- Try to get 3 and 4 at similar range but differing in azimuth.
    { longitude = bawdsey.longitude + degrees 1.1
    , latitude = bawdsey.latitude + degrees 0.3
    , height = 40 -- ,000 ft
    , bearing = degrees 270
    , speed = 200 -- mph
    , iff = Nothing
    , iffActive = False
    , tutorial = False
    }


bomber4 =
    { longitude = bawdsey.longitude + degrees 1.1
    , latitude = bawdsey.latitude - degrees 0.3
    , height = 40 -- ,000 ft
    , bearing = degrees 270
    , speed = 200 -- mph
    , iff = Nothing
    , iffActive = False
    , tutorial = False
    }


fighter1 =
    -- Starts behind and heads out pretty quick.
    { longitude = bawdsey.longitude + degrees 0.06
    , latitude = bawdsey.latitude + degrees 0.01
    , height = 10 -- ,000 ft
    , bearing = degrees 90
    , speed = 300 -- mph
    , iff = Just 1
    , iffActive = True
    , tutorial = False
    }


severalAbreast n =
    List.map
        (\i ->
            { longitude = bawdsey.longitude + degrees 1.17
            , latitude = bawdsey.latitude + degrees 0.3 + (degrees (toFloat i - 5) * 0.001)
            , height = 25
            , bearing = degrees 275
            , speed = 200
            , iff = Nothing
            , iffActive = False
            , tutorial = False
            }
        )
    <|
        List.range 1 n


severalAligned n =
    List.map
        (\i ->
            { longitude = bawdsey.longitude + degrees 1.17 + (degrees (toFloat i - 5) * 0.001)
            , latitude = bawdsey.latitude - degrees 0.2 + (degrees <| 0.05 * cos (toFloat i))
            , height = 25 + toFloat (modBy 7 (100 * i)) * 0.1
            , bearing = degrees 265
            , speed = 250
            , iff = Nothing
            , iffActive = False
            , tutorial = False
            }
        )
    <|
        List.range 1 n


trainingMode : List Target
trainingMode =
    [ { bomber1 | tutorial = True } ]


trainingMode2 : List Target
trainingMode2 =
    -- Two planes same range same bearing
    [ { bomber2 | tutorial = True }
    , { bomber2A | tutorial = True }
    ]


trainingMode3 : List Target
trainingMode3 =
    -- Two planes same range different bearings
    [ { bomber3 | tutorial = True }
    , { bomber4 | tutorial = True }
    ]


trainingModeFriendlyOutbound : List Target
trainingModeFriendlyOutbound =
    [ { fighter1 | tutorial = True } ]


trainingMode3to6 : List Target
trainingMode3to6 =
    let
        placeInTutorialMode t =
            { t | tutorial = True }
    in
    -- Four aircraft close together
    -- Wonder if it will work using them twice!
    List.map placeInTutorialMode <| severalAligned 5

