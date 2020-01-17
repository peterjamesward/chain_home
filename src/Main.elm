module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import List
import Tuple exposing (..)
import String
import Browser
import Html exposing (..)
import Task
import Time

-- This is dummy line for me to practise with floats and trig.
myLineData t= List.map (\x -> ( x, truncate <| ((*) 100) <| sin <| toFloat <| x * t ))
  (List.range 1 800)

-- SVG requires a line to be expressed as a space separated string of
-- integer pairs. So it is.
stringifyPoint (x, y) = (String.fromInt x )++ 
    "," ++ 
    (String.fromInt y) ++ " "

polyLineFromCoords coords = List.foldr (++) "" 
  (List.map stringifyPoint coords)

-- Some RDF lobe functions TO GO IN DIFFERENT NODULE
txHiVertReflectedLobe alpha = (1 - 6 * alpha) * abs (sin (24 * alpha))
txHiVertOmniLobe alpha = sin (7 * alpha)
txHorizReflectedLobe theta = (cos theta)^2
txHorizOmniLobe theta = cos theta

rxHorizLobe theta = cos theta
rxLoVertLobe alpha = sin (7 * alpha)
rxHiVertLobe alpha = (1 - 6 * alpha) * abs (sin (24 * alpha))

-- SOME DATA STRUCTURES - ALSO DON'T BELONG HERE
type alias Target = { latitude : Float
                    , longitude : Float
                    , height : Float  -- in thousands of feet
                    , bearing : Float -- in degrees
                    , speed : Float   -- degrees from North
                    , iff : Bool }

type alias PolarTarget = { r : Float -- metres
                         , theta : Float -- radians
                         , alpha : Float -- radians, ignoring curvature for now
                         , iff : Bool -- is this a pipsqueak equipped friendly?
                         }

type alias Station = { latitude : Float
                     , longitude : Float
                     , lineOfShoot : Float
                     }

type alias Echo = { t : Float -- timebase is range/2c
                  , theta : Float
                  , phase : Float
                  , duration : Float
                  }

type alias LineData = List Float

-- Let's make ourselves a station and a target
ch1 = { latitude = 1.408614
      ,longitude = 51.993661 
      ,lineOfShoot = 90.0 }  -- Bawdsey, assuming LOS due East.

bomber = { latitude = 2.0
         , longitude = 51.993661
         , height = 20
         , bearing = 270
         , speed = 200
         , iff = False }

-- Need some coordinate mangling

mapToPolar target = ...

bearing station target = ...

range station target = ...

-- MAIN
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL
type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , lineData : List (Int, Int)
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( { zone = Time.utc 
    , time = Time.millisToPosix 0
    , lineData = myLineData 0
    }
  , Task.perform AdjustTimeZone Time.here
  )

-- UPDATE
type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime
                , lineData = myLineData (Time.toMillis model.zone model.time)}
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone 
                , lineData = myLineData (Time.toSecond model.zone model.time)}
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 40 Tick

-- VIEW

view : Model -> Svg Msg
view m = svg
    [ viewBox "0 0 800 400"
    , width "800"
    , height "400"
    ]
    [ rect
        [ x "0"
        , y "0"
        , width "800"
        , height "400"
        , fill "black"
        , stroke "black"
        , strokeWidth "2"
        ]
        []
    , polyline
        [ points (polyLineFromCoords m.lineData)
        , fill "none"
        , stroke "lightgreen"
        , strokeWidth "2"
        ]
        []
    ]
