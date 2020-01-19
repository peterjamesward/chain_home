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
import Dict exposing (..)

-- This is dummy line for me to practise with floats and trig.
myLineData t = [ (0.0, 0.0), (1000.0, 0.0)]

-- SVG requires a line to be expressed as a space separated string of pairs.
stringifyPoint (x, y) = (String.fromFloat x )++ 
    "," ++ 
    (String.fromFloat y) ++ " "

polyLineFromCoords coords = List.foldr (++) "" 
  (List.map stringifyPoint coords)

-- Some RDF lobe functions TO GO IN DIFFERENT NODULE
txHiVertReflectedLobe 𝛂 = (1 - 6 * 𝛂) * abs (sin (24 * 𝛂))
txHiVertOmniLobe 𝛂      = sin (7 * 𝛂)
txHorizReflectedLobe θ  = (cos θ)^2
txHorizOmniLobe θ       = cos θ

rxHorizLobe θ  = cos θ
rxLoVertLobe 𝛂 = sin (7 * 𝛂)
rxHiVertLobe 𝛂 = (1 - 6 * 𝛂) * abs (sin (24 * 𝛂))

-- SOME DATA STRUCTURES - ALSO DON'T BELONG HERE
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

type alias Station = { latitude    : Float
                     , longitude   : Float
                     , lineOfShoot : Float
                     }

type alias Echo = { r         : Float 
                  , theta     : Float
                  , phase     : Float
                  , duration  : Float
                  , amplitude : Float
                  }

type alias LineData = List (Float, Float)

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
bawdsey = { longitude   = degrees 1.408614
          , latitude    = degrees 51.993661 
          , lineOfShoot = degrees 90.0 
          }  

bomber1 = { longitude = degrees 2.0
          , latitude  = degrees 51.993661
          , height    = 20 -- ,000 ft
          , bearing   = degrees 270
          , speed     = 220 -- mph
          , iff       = False 
          }
 
bomber2 = { longitude = degrees 1.9
          , latitude  = degrees 51.993
          , height    = 20 -- ,000 ft
          , bearing   = degrees 275
          , speed     = 200 -- mph
          , iff       = False 
          }
 
-- Targets move! t in seconds to at least centisecond resolution please
targetAtTime : Int -> Int -> Target -> Target
targetAtTime t startTime target = 
  let tempusFugit = t - startTime  -- milliseconds elapsed
      distanceTravelled = (toFloat tempusFugit) * target.speed * 1609 / 3600000
      (newLat, newLong) = newPosition (target.latitude, target.longitude) 
                                       distanceTravelled target.bearing
  in 
    { target | latitude = newLat
             , longitude = newLong 
             }

-- Non-bucketized line generation.
{-There will be some set of echoes, and shall probably manage these as a set not a list.
We sort by range of leading edge of echo. Maybe we have one sort of both leading and
trailing edges.
We process the echoes in order of range, skipping to the next edge. Range will be fractional,
and we rely on SVG to draw nice lines. (We may try various quality options.) As we 
encounter leading edges, we add the echo to the active echo set and sum the echoes in 
the active set accounting for phase and magnitude. This gives deflection and is valid
until the next edge.
On a trailing edge, we remove the relevant echo from the active set and derive the
new deflection.
On each edge, we output a line segment. We can prepend these to a list.
We end with a line to (800,0). -}

type alias EdgeInfo = (Float, Float, Bool)  -- x coord of edge, leading edge,  and whether leading edge.

-- This version uses the polar targets, not the echoes, so we can see how the line looks.

combineSignals : Dict Float PolarTarget -> Float
combineSignals activeSignals = 100.0 * toFloat (size activeSignals)

processEdge : EdgeInfo -> (LineData, Dict Float PolarTarget, Dict Float PolarTarget) 
                       -> (LineData, Dict Float PolarTarget, Dict Float PolarTarget)
processEdge (position, leading, isLeading) (lineData, activeSignals, allSignals) = 
  let  signal = Maybe.withDefault defaultPolarTarget <| Dict.get position allSignals
       newActiveSignals = case isLeading of
                              True  -> (Dict.insert position signal activeSignals)
                              False -> (Dict.remove leading activeSignals)
       newX = position / 100.0
       newY = combineSignals newActiveSignals
       (prevX, prevY) = Maybe.withDefault (0.0, 0.0) <| List.head lineData
  in
       ( (newX, newY) 
         :: (newX, prevY) 
         :: lineData
       , newActiveSignals
       , allSignals )

deriveTrace : List PolarTarget -> LineData
deriveTrace signals =
  -- Let's put the targets in a dictionary, indexed by range (probably unique!).
  -- Then we need a sorted list of edges (front and back).
  -- Sorted list will index into the dictionary, for easy access to each target.
  let allSignals = List.foldl (\p a -> Dict.insert p.r p a) Dict.empty signals
      activeSignals = Dict.empty
      theLine = [(0.0,0.0)]
      leadingEdges = List.map (\p -> (p.r, p.r, True)) signals
      trailingEdges = List.map (\p -> (p.r + 200, p.r, False )) signals
      allEdgesSorted = List.sortBy (\(a,b,c) -> a) <| leadingEdges ++ trailingEdges
      extractLineData (line, _, _) = line
  in
      ((::) (1000.0, 0.0)) <| extractLineData <| List.foldl processEdge (theLine, activeSignals, allSignals) allEdgesSorted
      

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
  , lineData : List (Float, Float)
  , station : Station
  , targets : List Target
  , movedTargets : List Target
  , polarTargets : List PolarTarget
  , echoes : List Echo
  }


init : () -> (Model, Cmd Msg)
init _ =
  let
    targetsBaseline = [ bomber1, bomber2 ]
  in
    ( { zone = Time.utc 
      , startTime = 0
      , time = 0
      , lineData = myLineData 0.0
      , station = bawdsey
      , targets = targetsBaseline
      , movedTargets = []
      , polarTargets = []
      , echoes = [] 
      }
    , Task.perform AdjustTimeZone Time.here
    )

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
              , lineData = deriveTrace model.polarTargets 
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
  --let t1 = Maybe.withDefault bomber1 (List.head m.movedTargets)
  --    p1 = Maybe.withDefault defaultPolarTarget (List.head m.polarTargets)
  --    info = [ Html.text "Start time "
  --           , Html.text <| String.fromInt m.startTime
  --           , Html.br [] []
  --           , Html.text "At time "
  --           , Html.text <| String.fromInt m.time
  --           , Html.br [] []
  --           , Html.text "Longitude (radians) "
  --           , Html.text <| String.fromFloat <| t1.longitude
  --           , Html.br [] []
  --           , Html.text "Latitude (radians) "
  --           , Html.text <| String.fromFloat <| t1.latitude
  --           , Html.br [] []
  --           , Html.text "r "
  --           , Html.text <| String.fromFloat <| p1.r
  --           , Html.br [] []
  --           , Html.text "theta "
  --           , Html.text <| String.fromFloat <| p1.theta
  --           , Html.br [] []
  --           , Html.text "alpha "
  --           , Html.text <| String.fromFloat <| p1.alpha
  --           , Html.br [] []
  --           ]
  --    lineInfo = List.concatMap
  --                 (\(i,x) -> [ Html.text <| String.fromFloat i
  --                            , Html.text " "
  --                            , Html.text <| String.fromFloat x
  --                            , Html.br [] []])
  --                 m.lineData
  --in
  --  div [] lineInfo
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
