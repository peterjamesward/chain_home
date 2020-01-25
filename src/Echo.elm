module Echo exposing (..)

import Constants exposing (scaleWidthKilometers)

type alias Echo = { r         : Float 
                  , theta     : Float
                  , alpha     : Float
                  , phase     : Float
                  , duration  : Float
                  , amplitude : Float
                  }

defaultEcho = { r = 0, theta = 0, alpha = 0, phase = 0, amplitude = 0, duration = 0 }

dummyFinalEcho = { r = scaleWidthKilometers * 1000
                 , theta = 0
                 , alpha = 0
                 , phase = 0
                 , amplitude = 0
                 , duration = 0
                 }

dummyInitialEcho = { dummyFinalEcho | r = 0 }
      
