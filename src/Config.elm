module Config exposing (TargetSelector, targetConfigurations, bawdsey, targetsBaseline, toggleConfig, getAllTargets)

import Target exposing (Target)

type alias TargetSelector =
  { id          : Int
  , description : String    -- e.g. "mass raid"
  , targets     : List Target   -- The planes to be active when this group selected
  , active      : Bool      -- Whether this group is active (dynamic)
  }

-- Let's make ourselves a station and a target
-- Bawdsey, assuming LOS due East.
bawdsey = { longitude   = degrees 1.408614
          , latitude    = degrees 51.993661 
          , lineOfShoot = degrees 90.0 
          }  

bomber1 = { longitude = bawdsey.longitude + (degrees 0.9)
          , latitude  = bawdsey.latitude + (degrees 0.0)
          , height    = 20 -- ,000 ft
          , bearing   = degrees 270
          , speed     = 0.0 -- mph
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
          , latitude  = degrees 52.06
          , height    = 30.1 -- ,000 ft
          , bearing   = degrees 280
          , speed     = 200.0 -- mph
          , iff       = False 
          }
 
bomber3 = { longitude = bawdsey.longitude + (degrees 0.8)
          , latitude  = bawdsey.latitude + (degrees 0.55)
          , height    = 20 -- ,000 ft
          , bearing   = degrees 270
          , speed     = 200 -- mph
          , iff       = False 
          }
 
bomber4 = { longitude = bawdsey.longitude + (degrees 0.8) 
          , latitude  = bawdsey.latitude + (degrees -0.5)
          , height    = 20 -- ,000 ft
          , bearing   = degrees 270
          , speed     = 200 -- mph
          , iff       = False 
          }
 
fighter1 = { longitude = degrees 1.8
           , latitude  = bawdsey.latitude - (degrees 0.1)
           , height    = 10 -- ,000 ft
           , bearing   = degrees 90
           , speed     = 400 -- mph
           , iff       = False 
           }

bomberInFormation baseLocation latIndex longIndex =
    { baseLocation  | longitude = baseLocation.longitude + (degrees (toFloat longIndex-5) * 0.003)
                    , latitude  = baseLocation.latitude + (degrees (toFloat latIndex-5) * 0.0011)
                    , height = baseLocation.height + (toFloat (latIndex + longIndex) * 5)
                    , speed = baseLocation.speed + toFloat (latIndex + longIndex)
    }

massRaid = 
    let base =  { longitude = bawdsey.longitude + (degrees 0.9)
                , latitude  = bawdsey.latitude
                , height    = 25
                , bearing   = degrees 270
                , speed     = 200
                , iff       = False
                }
    in 
        List.map 
        (\i -> bomberInFormation base (i // 10) (modBy 10 i))
        <| List.range 1 100

tenAbreast = List.map (\i ->
                { longitude = bawdsey.longitude + (degrees 0.8)
                , latitude  = bawdsey.latitude + (degrees (toFloat i-5) * 0.001)
                , height    = 25
                , bearing   = degrees 270
                , speed     = 200
                , iff       = False
                }
            )
            <| List.range 1 10

tenAligned = List.map (\i ->
                { longitude = bawdsey.longitude + (degrees 1.0) + (degrees (toFloat i-5) * 0.004)
                , latitude  = bawdsey.latitude + (degrees <| 0.05 * sin (toFloat i))
                , height    = 25
                , bearing   = degrees 270
                , speed     = 200
                , iff       = False
                }
            )
            <| List.range 1 10


targetsBaseline =   [ 
                    --  bomber1
                    --, bomber2
                    --, bomber2A
                    -- bomber3
                    --, bomber4
                    --, fighter1 
                    ]
                    ++ tenAligned
                    ++ tenAbreast
                    --++ massRaid

-- Some useful configurations for training.

outboundFriendly  = TargetSelector 1 "One outbound friendly fighter"  [fighter1] False 
twoCloseTargets   = TargetSelector 2 "Two targets very close"        [bomber2, bomber2A] False 
twoDistantTargets = TargetSelector 3 "Two targets, differnt bearings" [bomber3, bomber4] False 
tenWide           = TargetSelector 4 "Ten targets, line abreast"     tenAbreast False 
tenDeep           = TargetSelector 5 "Ten targets, line astern"     tenAligned False 
massiveRaid       = TargetSelector 6 "Formation of 100"       massRaid False 

targetConfigurations = [ outboundFriendly, twoCloseTargets, twoDistantTargets, tenWide, tenDeep, massiveRaid ]

toggleConfig : List TargetSelector -> Int -> List TargetSelector
toggleConfig activeConfigurations idx =
  let
    toggleMatchingIdx i ts =
      if ts.id == i then
        { ts | active = not ts.active }
      else
        ts
  in
    List.map (toggleMatchingIdx idx) activeConfigurations

getAllTargets : List TargetSelector -> List Target
getAllTargets config =
  List.concatMap (.targets) <| List.filter (.active) config


