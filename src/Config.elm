module Config exposing (TargetSelector, bawdsey, getAllTargets, targetConfigurations, updateConfig)

import Station exposing (Station, stationClutter)
import Target exposing (Target)


type alias TargetSelector =
    { id : Int
    , description : String -- e.g. "mass raid"
    , targets : List Target -- The planes to be active when this group selected
    , active : Bool -- Whether this group is active (dynamic)
    }



-- Let's make ourselves a station and a target
-- Bawdsey, assuming LOS due East.


bawdsey =
    { longitude = degrees 1.408614
    , latitude = degrees 51.993661
    , lineOfShoot = degrees 90.0
    }


bomber1 =
    { longitude = bawdsey.longitude + degrees 0.9
    , latitude = bawdsey.latitude + degrees 1.0
    , height = 20 -- ,000 ft
    , bearing = degrees 250
    , speed = 200.0 -- mph
    , iff = False
    }


bomber2 =
    { longitude = degrees 2.0
    , latitude = degrees 52.05
    , height = 30.1 -- ,000 ft
    , bearing = degrees 280
    , speed = 200.0 -- mph
    , iff = False
    }


bomber2A =
    { longitude = degrees 2.0
    , latitude = degrees 52.05
    , height = 30.1 -- ,000 ft
    , bearing = degrees 280
    , speed = 200.0 -- mph
    , iff = False
    }


bomber3 =
    { longitude = bawdsey.longitude + degrees 0.8
    , latitude = bawdsey.latitude + degrees 0.35
    , height = 20 -- ,000 ft
    , bearing = degrees 270
    , speed = 200 -- mph
    , iff = False
    }


bomber4 =
    { longitude = bawdsey.longitude + degrees 0.8
    , latitude = bawdsey.latitude + degrees -0.33
    , height = 20 -- ,000 ft
    , bearing = degrees 270
    , speed = 200 -- mph
    , iff = False
    }


fighter1 =
    { longitude = degrees 1.8
    , latitude = bawdsey.latitude - degrees 0.1
    , height = 10 -- ,000 ft
    , bearing = degrees 90
    , speed = 400 -- mph
    , iff = False
    }


bomberInFormation baseLocation latIndex longIndex =
    { baseLocation
        | longitude = baseLocation.longitude + (degrees (toFloat longIndex - 5) * 0.01)
        , latitude = baseLocation.latitude + (degrees (toFloat latIndex - 5) * 0.01)
        , height = baseLocation.height + (toFloat (latIndex + longIndex) * 5)
        , speed = baseLocation.speed + toFloat (latIndex + longIndex)
    }


massRaid =
    let
        base =
            { longitude = bawdsey.longitude + degrees 0.9
            , latitude = bawdsey.latitude - degrees 1.0
            , height = 25
            , bearing = degrees 290
            , speed = 200
            , iff = False
            }
    in
    List.map
        (\i -> bomberInFormation base (i // 10) (modBy 10 i))
    <|
        List.range 1 100


tenAbreast =
    List.map
        (\i ->
            { longitude = bawdsey.longitude + degrees 0.8
            , latitude = bawdsey.latitude + (degrees (toFloat i - 5) * 0.001)
            , height = 25
            , bearing = degrees 270
            , speed = 200
            , iff = False
            }
        )
    <|
        List.range 1 10


tenAligned =
    List.map
        (\i ->
            { longitude = bawdsey.longitude + degrees 1.0 + (degrees (toFloat i - 5) * 0.002)
            , latitude = bawdsey.latitude + (degrees <| 0.05 * sin (toFloat i))
            , height = 25
            , bearing = degrees 270
            , speed = 200
            , iff = False
            }
        )
    <|
        List.range 1 10



-- Some useful configurations for training.


outboundFriendly =
    TargetSelector 1 "One outbound friendly fighter" [ fighter1 ] False


loneBomber =
    TargetSelector 2 "One target" [ bomber1 ] False


twoCloseTargets =
    TargetSelector 3 "Two targets in close formation" [ bomber2, bomber2A ] False


twoDistantTargets =
    TargetSelector 4 "Two targets, same range, different bearings" [ bomber3, bomber4 ] False


tenWide =
    TargetSelector 5 "Ten targets, line abreast" tenAbreast False


tenDeep =
    TargetSelector 6 "Ten targets, line astern" tenAligned False


massiveRaid =
    TargetSelector 7 "Formation of 100" massRaid False


nearbynoise =
    TargetSelector 8 "Artefacts local to station" (stationClutter bawdsey 20) False


targetConfigurations =
    [ outboundFriendly
    , loneBomber
    , twoCloseTargets
    , twoDistantTargets
    , tenWide
    , tenDeep
    , massiveRaid
    , nearbynoise
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
