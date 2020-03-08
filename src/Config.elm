module Config exposing (..)

import Target exposing (Target)


type alias TargetSelector =
    { id : Int
    , description : String -- e.g. "mass raid"
    , targets : List Target -- The planes to be active when this group selected
    , active : Bool -- Whether this group is active (dynamic)
    }


groundRays =
    [ { sequence = 0
      , r = 5000
      , theta = 0 -- ignored as these are injected after D/F
      , alpha = 0
      , phase = 0
      , duration = 0
      , amplitude = 10.0
      }
    , { sequence = 0
      , r = 8000
      , theta = 0 -- ignored as these are injected after D/F
      , alpha = 0
      , phase = 0
      , duration = 0
      , amplitude = 8.0
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
    }


bomber1 =
    { longitude = bawdsey.longitude + degrees 0.9
    , latitude = bawdsey.latitude + degrees 1.0
    , height = 20 -- ,000 ft
    , bearing = degrees 250
    , speed = 200.0 -- mph
    , iff = Just 7
    , iffActive = False
    }


bomber2 =
    -- 2 and 2A very close to look for "beating" effect.
    { longitude = degrees 2.0
    , latitude = degrees 52.05
    , height = 30.1 -- ,000 ft
    , bearing = degrees 280
    , speed = 200.0 -- mph
    , iff = Nothing
    , iffActive = False
    }


bomber2A =
    { longitude = degrees 2.0
    , latitude = degrees 52.04
    , height = 30.2 -- ,000 ft
    , bearing = degrees 280
    , speed = 200.0 -- mph
    , iff = Nothing
    , iffActive = False
    }


bomber3 =
    -- Try to get 3 and 4 at similar range but differing in azimuth.
    { longitude = bawdsey.longitude + degrees 0.8
    , latitude = bawdsey.latitude + degrees 0.3
    , height = 40 -- ,000 ft
    , bearing = degrees 270
    , speed = 200 -- mph
    , iff = Nothing
    , iffActive = False
    }


bomber4 =
    { longitude = bawdsey.longitude + degrees 0.8001
    , latitude = bawdsey.latitude + degrees -0.3
    , height = 40 -- ,000 ft
    , bearing = degrees 270
    , speed = 200 -- mph
    , iff = Nothing
    , iffActive = False
    }


fighter1 =
    -- Starts behind and heads out pretty quick.
    { longitude = bawdsey.longitude - degrees 0.01
    , latitude = bawdsey.latitude + degrees 0.01
    , height = 10 -- ,000 ft
    , bearing = degrees 90
    , speed = 300 -- mph
    , iff = Just 1
    , iffActive = False
    }


tenAbreast =
    List.map
        (\i ->
            { longitude = bawdsey.longitude + degrees 0.8
            , latitude = bawdsey.latitude + degrees 0.3 + (degrees (toFloat i - 5) * 0.001)
            , height = 25
            , bearing = degrees 270
            , speed = 200
            , iff = Nothing
            , iffActive = False
            }
        )
    <|
        List.range 1 10


tenAligned =
    List.map
        (\i ->
            { longitude = bawdsey.longitude + degrees 1.0 + (degrees (toFloat i - 5) * 0.001)
            , latitude = bawdsey.latitude - degrees 0.2 + (degrees <| 0.05 * cos (toFloat i))
            , height = 25 + toFloat (modBy 7 (100 * i)) * 0.1
            , bearing = degrees 270
            , speed = 200
            , iff = Nothing
            , iffActive = False
            }
        )
    <|
        List.range 1 10



-- Some useful configurations for training.


outboundFriendly =
    TargetSelector 1 "One outbound friendly fighter" [ fighter1 ] False


loneBomber =
    TargetSelector 2 "One returning friendly" [ bomber1 ] False


twoCloseTargets =
    TargetSelector 3 "Two targets in close formation" [ bomber2, bomber2A ] False


twoDistantTargets =
    TargetSelector 4 "Two targets, same range, different bearings" [ bomber3, bomber4 ] False


tenWide =
    TargetSelector 5 "Ten targets, line abreast" tenAbreast False


tenDeep =
    TargetSelector 6 "Ten targets, line astern" tenAligned False


behindYou =
    TargetSelector 9 "One behind station (use Sense)" [ behindStation ] False


targetConfigurations =
    [ outboundFriendly
    , loneBomber
    , twoCloseTargets
    , twoDistantTargets
    , tenWide
    , tenDeep
    , behindYou
    ]


updateConfig : List TargetSelector -> Int -> Bool -> List TargetSelector
updateConfig activeConfigurations idx newState =
    let
        matchingIdx ts =
            if ts.id == idx then
                { ts | active = newState }

            else
                ts
    in
    List.map matchingIdx activeConfigurations


getAllTargets : List TargetSelector -> List Target
getAllTargets config =
    List.concatMap .targets <| List.filter .active config
