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
import Array exposing (..)

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
                     , bearing : Float -- in degrees from North
                     , speed : Float   -- miles per hour (!)
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

defaultPolarTarget = { r = 0, theta = 0, alpha = 0, iff = False }

-- Need some coordinate mangling
-- https://www.movable-type.co.uk/scripts/latlong.html

meanRadius = 6371000

-- Equirectangular approximation
range (φ1, λ1) (φ2, λ2) = 
  let 
      x = (λ2 - λ1) * cos((φ1 + φ2)/2)
      y = (λ2 - λ1) 
  in 
      sqrt (x*x + y*y) * meanRadius

bearing (φ1, λ1) (φ2, λ2) = 
  let y = (sin λ2 - λ1) * (cos φ2)
      x = (cos φ1) * (sin φ2) - 
          (sin φ1) * (cos φ2) * (cos (λ2 - λ1))
  in
      atan2 y x

-- Find new lat long after travellin d metres on given bearing.
newPosition (φ1, λ1) d θ =
  let  δ = d / meanRadius
       φ2 = asin ( sin φ1 * cos δ + cos φ1 * sin δ * cos θ )
       λ2 = λ1 + atan2 (sin θ * sin δ * cos φ1) (cos δ - sin φ1 * sin φ2)
  in
    (φ2, λ2)

-- Convert from Cartesian (and imperial) map coordinates to 
-- polar (and metric) relative to station position and line of shoot.

mapToPolar : Station -> Target -> PolarTarget
mapToPolar station target = 
  let stationPos = (station.latitude, station.longitude)
      targetPos = (target.latitude, target.longitude)
      rng = range stationPos targetPos
  in
     { r = rng
     , theta = (bearing stationPos targetPos) - station.lineOfShoot
     , alpha = atan2 (target.height * 304.8) rng
     , iff = target.iff
     }

-- Let's make ourselves a station and a target
-- Bawdsey, assuming LOS due East.
bawdsey = { longitude = degrees 1.408614
          , latitude = degrees 51.993661 
          , lineOfShoot = degrees 90.0 
          }  

bomber = { longitude = degrees 2.0
         , latitude = degrees 51.993661
         , height = 20 -- ,000 ft
         , bearing = degrees 270
         , speed = 200 -- mph
         , iff = False 
         }

-- Targets move! t in seconds to at least centisecond resolution please
targetAtTime : Int -> Int -> Target -> Target
targetAtTime t startTime target = 
  let tempusFugit = (t) - (startTime)
      distanceTravelled = (toFloat tempusFugit) * target.speed * 1609 / 3600000
      (newLat, newLong) = newPosition (target.latitude, target.longitude) 
                                       distanceTravelled target.bearing
  in 
    { target | latitude = newLat
             , longitude = newLong 
             }

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
  , startTime : Int -- millseconds from t-zero
  , time : Int
  , lineData : List (Int, Int)
  , station : Station
  , targets : List Target
  , movedTargets : List Target
  , polarTargets : List PolarTarget
  , echoes : List Echo
  }


init : () -> (Model, Cmd Msg)
init _ =
  let
    targetsBaseline = [ bomber ]
  in
    ( { zone = Time.utc 
      , startTime = 0
      , time = 0
      , lineData = myLineData 0
      , station = bawdsey
      , targets = targetsBaseline
      , movedTargets = []
      , polarTargets = []
      , echoes = [] 
      }
    , Task.perform AdjustTimeZone Time.here
    )

-- Starting point for our bucket filling function.
-- Initially, just try to show range info with no echoing.
bucketize : List PolarTarget -> List (Int, Int)
bucketize targets =
  let emptyLineArray = Array.repeat 1000 (toFloat 0)
      filledLineArray = List.foldl addSignal emptyLineArray targets
      asList = Array.toIndexedList filledLineArray
  in
      List.map (\(i,x)->(i,truncate x)) asList

-- For now, assume 100km range so 1000 buckets of 100m each, 
-- Use a 20 bucket pulse width. 
addSignal : PolarTarget -> Array Float -> Array Float
addSignal signal array =
  let index = truncate <| (signal.r) / 100  
      currentValue = Maybe.withDefault 0 (Array.get index array)
  in
      Array.set index (currentValue + 100) array

-- UPDATE
type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone

-- THIS IS IT. This is the place where it all comes together.
deriveModelAtTime : Model -> Int -> Model
deriveModelAtTime model t =
  let
    targetsNow = List.map (targetAtTime t model.startTime) model.targets
    convertedTargets = List.map (mapToPolar bawdsey) targetsNow
  in
      { model | startTime = if model.startTime == 0 then t else model.startTime 
              , time = t
              , movedTargets = targetsNow
              , polarTargets = convertedTargets
              , echoes = []
              , lineData = bucketize model.polarTargets 
      }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( deriveModelAtTime model (Time.posixToMillis newTime)
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 40 Tick

-- VIEW

view : Model -> Svg Msg
view m = 
{-  let t1 = Maybe.withDefault bomber (List.head m.movedTargets)
      p1 = Maybe.withDefault defaultPolarTarget (List.head m.polarTargets)
      info = [ Html.text "Start time "
             , Html.text <| String.fromInt m.startTime
             , Html.br [] []
             , Html.text "At time "
             , Html.text <| String.fromInt m.time
             , Html.br [] []
             , Html.text "Longitude (radians) "
             , Html.text <| String.fromFloat <| t1.longitude
             , Html.br [] []
             , Html.text "Latitude (radians) "
             , Html.text <| String.fromFloat <| t1.latitude
             , Html.br [] []
             , Html.text "r "
             , Html.text <| String.fromFloat <| p1.r
             , Html.br [] []
             , Html.text "theta "
             , Html.text <| String.fromFloat <| p1.theta
             , Html.br [] []
             , Html.text "alpha "
             , Html.text <| String.fromFloat <| p1.alpha
             , Html.br [] []
             ]
      lineInfo = List.concatMap
                   (\(i,x) -> [ Html.text <| String.fromInt i
                              , Html.text <| String.fromInt x
                              , Html.br [] []])
                   m.lineData
  in
    div [] lineInfo-}
  svg
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
        , strokeWidth "1"
        ]
        []
    ]
