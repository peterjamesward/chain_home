module Echo exposing (Echo, deriveEchoes, viewEcho)

import Constants exposing (scaleWidthKilometers, pulseDuration, wavelength)
import Target exposing (PolarTarget)
import LobeFunctions exposing (..)
import Html exposing (..)

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
      
deriveEchoes : List PolarTarget -> Int -> List Echo
deriveEchoes targets time = 
  let 
      ph rng = asin <| sin <| rng * (toFloat time)/wavelength  --/wavelength / 1000)  -- clearer not cheaper
      --ph rng = 2.0 * pi * (rng - wavelength * (toFloat << truncate) (rng / wavelength))/wavelength
      echoFromTarget target = { r         = target.r
                              , theta     = target.theta
                              , alpha     = target.alpha
                              , phase     = ph target.r
                              , duration  = pulseDuration    -- microseconds
                              , amplitude = abs <| ( txHorizReflectedLobe target.theta )
                                                 * ( txHiVertOmniLobe target.alpha )
                              }
  in
      List.map echoFromTarget targets

-- This version uses the Echoes, but really this doesn't happen until
-- the echoes are received at the (two) antenna, where they become a voltage.

combineEchoes : List Echo -> Float

combineEchoes activeEchoes = 
  --Dict.foldl (\_ e acc -> e.amplitude + acc) 0.0 activeEchoes
--  100.0 * toFloat (Dict.size activeEchoes)  --OK!
  -- Treat amplitude and phase as vector, sum components, convert back, use amplitude.
  let --asPolar = Dict.map (\_ e -> (e.amplitude, e.phase) ) activeEchoes
      asRect  = List.map (\e -> fromPolar (e.amplitude, e.phase)) activeEchoes
      combinedAsRect = List.foldl (\(x, y) (xAcc, yAcc) -> (x + xAcc, y + yAcc) ) (0.0,0.0) asRect
      (mag, phase) = toPolar combinedAsRect
  in
      mag

-- Handy function fo debuggery.
viewEcho e = 
    [ Html.text "r "
    , Html.text <| String.fromFloat <| e.r
    , Html.br [] []
    , Html.text "theta "
    , Html.text <| String.fromFloat <| e.theta
    , Html.br [] []
    , Html.text "alpha "
    , Html.text <| String.fromFloat <| e.alpha
    , Html.br [] []
    , Html.text "phase "
    , Html.text <| String.fromFloat <| e.phase
    , Html.br [] []
    , Html.text "duration "
    , Html.text <| String.fromFloat <| e.duration
    , Html.br [] []
    , Html.text "amplitude "
    , Html.text <| String.fromFloat <| e.amplitude
    , Html.br [] []
    , Html.hr [] []
    ]