module Target exposing (..)

type alias Target = { latitude   : Float
                     , longitude : Float
                     , height    : Float  -- in thousands of feet
                     , bearing   : Float -- in degrees from North
                     , speed     : Float   -- miles per hour (!)
                     , iff       : Bool }

type alias PolarTarget = { r     : Float -- metres
                         , theta : Float -- radians
                         , alpha : Float -- radians, ignoring curvature for now
                         , iff   : Bool -- is this a pipsqueak equipped friendly?
                         }

defaultPolarTarget = { r = 0, theta = 0, alpha = 0, iff = False }

