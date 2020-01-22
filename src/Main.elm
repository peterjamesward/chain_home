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

import Station exposing (..)
import Target exposing (..)

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
newPosition : (Float, Float) -> Float -> Float -> (Float, Float)
newPosition (φ1, λ1) d θ =
  let  δ = d / meanRadius
       φ2 = asin ( sin φ1 * cos δ + cos φ1 * sin δ * cos θ )
       λ2 = λ1 + atan2 (sin θ * sin δ * cos φ1) (cos δ - sin φ1 * sin φ2)
  in
    (φ2, λ2)

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



-- This is dummy line for me to practise with floats and trig.
myLineData t = [ (0.0, 0.0), (1000.0, 0.0)]

-- SVG requires a line to be expressed as a space separated string of pairs.
stringifyPoint (x, y) = (String.fromFloat x )++ 
    "," ++ 
    (String.fromFloat y) ++ " "

polyLineFromCoords coords = List.foldr (++) "" (List.map stringifyPoint coords)

-- Some RDF lobe functions TO GO IN DIFFERENT NODULE
txHiVertReflectedLobe alpha = (1 - 6 * alpha) * abs (sin (24 * alpha))
txHiVertOmniLobe alpha      = sin (7 * alpha)
txHorizReflectedLobe θ  = (cos θ)^2
txHorizOmniLobe θ       = cos θ

rxHorizLobe θ  = cos θ
rxLoVertLobe alpha = sin (7 * alpha)
rxHiVertLobe alpha = (1 - 6 * alpha) * abs (sin (24 * alpha))

lightSpeed = 300000000
frequency  = 20000000
wavelength = lightSpeed / frequency
pulseDuration = 40  -- microseconds 
pulseWidth = lightSpeed * pulseDuration / 10000000  -- TODO: scale seems wrong here.

-- SOME DATA STRUCTURES - ALSO DON'T BELONG HERE
type alias Echo = { r         : Float 
                  , theta     : Float
                  , alpha     : Float
                  , phase     : Float
                  , duration  : Float
                  , amplitude : Float
                  }

type alias LineData = List (Float, Float)

type alias EdgeSegment = ((Float, Float), (Float, Float))

defaultEcho = { r = 0, theta = 0, alpha = 0, phase = 0, amplitude = 0, duration = 0 }

beamSweepMax = 20 -- Maximum vertical displacement for one microsecond.
      
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

bomber1 = { longitude = degrees 2.0
          , latitude  = degrees 51.993661
          , height    = 20 -- ,000 ft
          , bearing   = degrees 270
          , speed     = 210 -- mph
          , iff       = False 
          }
 
bomber2 = { longitude = degrees 1.99
          , latitude  = degrees 51.993660
          , height    = 20 -- ,000 ft
          , bearing   = degrees 275
          , speed     = 200 -- mph
          , iff       = False 
          }
 
fighter1 = { longitude = degrees 1.4
           , latitude  = degrees 52.0
           , height    = 5 -- ,000 ft
           , bearing   = degrees 90
           , speed     = 400 -- mph
           , iff       = False 
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

combineEchoes : Dict Float Echo -> Float

combineEchoes activeEchoes = 
  --Dict.foldl (\_ e acc -> e.amplitude + acc) 0.0 activeEchoes
--  100.0 * toFloat (Dict.size activeEchoes)  --OK!
  -- Treat amplitude and phase as vector, sum components, convert back, use amplitude.
  let asPolar = Dict.map (\_ e -> (e.amplitude, e.phase) ) activeEchoes
      asRect  = Dict.map (\_ p -> fromPolar p)  asPolar
      combinedAsRect = Dict.foldl (\_ (x, y) (xAcc, yAcc) -> (x + xAcc, y + yAcc) ) (0.0,0.0) asRect
      combinedAsPolar = toPolar combinedAsRect
  in
      100.0 * first combinedAsPolar

processEdge : Float -> EdgeInfo 
                    -> (List EdgeSegment, Dict Float Echo, Dict Float Echo) 
                    -> (List EdgeSegment, Dict Float Echo, Dict Float Echo)
processEdge _ (position, leading, isLeading) (lineData, activeEchoes, allEchoes) = 
  let  echo = Maybe.withDefault defaultEcho <| Dict.get position allEchoes
       newActiveEchoes = case isLeading of
                              True  -> (Dict.insert position echo activeEchoes)
                              False -> (Dict.remove leading activeEchoes)
       newX = position / 100.0
       newY = combineEchoes newActiveEchoes
       (_,(prevX, prevY)) = Maybe.withDefault ((0.0, 0.0),(0.0, 0.0)) <| List.head lineData
  in
       ( ((newX, prevY), (newX, newY)) :: lineData
       , newActiveEchoes
       , allEchoes )

deriveTrace : Dict Float Echo -> List EdgeSegment
deriveTrace allEchoes =
  -- Let's put the targets in a dictionary, indexed by range (probably unique!).
  -- Then we need a sorted list of edges (front and back).
  -- Sorted list will index into the dictionary, for easy access to each target.
  let activeEchoes = Dict.empty
      theLine = [((0.0,0.0),(0.0,0.0))]
      leadingEdges = Dict.foldl (\r e d -> Dict.insert r (r, r, True) d) 
                                Dict.empty allEchoes
      trailingEdges = Dict.foldl (\r e d -> Dict.insert (r + pulseWidth) (r + pulseWidth, r, False ) d) 
                                 Dict.empty allEchoes 
      allEdges = Dict.union leadingEdges trailingEdges
      extractLineData (line, _, _) = line
  in
        extractLineData <| Dict.foldl processEdge (theLine, activeEchoes, allEchoes) allEdges

-- Real CH CRTs would not draw vertical lines - it takes time to build up the voltages
-- on the deflection plated. We shall simulate that here.
-- We shall not bother to limit acceleration, unless we have to.

-- Should be easier using Edges rather than line segment list...
-- How about we apply the maximum slope to each edge, moving its right hand X rightwards.
-- e.g. (100,0)->(100,100) becomes (100,0)->(102.5,100)
-- Given that we want to truncate edges that will now overlap.
-- Conceivably, some edges might disappear completely!

lineSmoother : List EdgeSegment -> LineData
lineSmoother rawEdges = 
  let edgeSloper ((x1,y1),(_,y2)) = ((x1,y1),(x1 + abs ((y2-y1)/beamSweepMax),y2))
      slopingEdges = List.map edgeSloper rawEdges
      -- Now going to try to make sure that the sloped edges cannot overlap.
      -- We do this by concentrating on the end points, not the edges.
      dummyLeadingEdge = ((0.0,0.0),(0.0,0.0))
      dummyTrailingEdge = ((1000.0,0.0),(1000.0,0.0))
      paddedEdges = dummyTrailingEdge :: slopingEdges ++ [dummyLeadingEdge]
      nonOverlappingPoints = List.map2 removeAnyOverlap paddedEdges 
                             <| Maybe.withDefault [] <| List.tail paddedEdges
      -- Remember edges are in 'reverse' order still.
      -- We require that e1x2 <= e2x1 with corresponding adjustment to e1y2.
      -- We do not need to return EdgeSegments now, we can return line segments.
      removeAnyOverlap  ((e2x1,e2y1),(e2x2,e2y2)) ((e1x1,e1y1),(e1x2,e1y2)) =
        if e2x1 >= e1x2 then 
          ((e1x1,e1y1),(e1x2,e1y2)) -- no overlap
        else if e2x1 < e1x1 then
          ((e2x1,e2y1),(e2x1,e2y1)) -- edge completely obscured
        else if e1y2 >= e1y1 then
          ((e1x1,e1y1),(e2x1,e1y1 + beamSweepMax * (e2x1-e1x1))) -- e1 truncated
        else
          ((e1x1,e1y1),(e2x1,e1y1 - beamSweepMax * (e2x1-e1x1))) -- e1 truncated
      toLineData (p1,p2) = [p2,p1]
      removeZeroEdges = List.filter (\((x1,_),(x2,_)) -> x2 > x1) 
  in
      (1000.0,0.0) :: List.concatMap toLineData (removeZeroEdges nonOverlappingPoints)

-- Deriving echoes is just applying the transmitter lobe function so
-- amplitude is function of ltheta and range. Later, IFF figures.
-- 22/01 Want to change this to be Dict r Echo; probably Dict r PolarTargets as well.
deriveEchoes : List PolarTarget -> Dict Float Echo
deriveEchoes targets = 
  let ph rng = 2.0 * pi * (rng - wavelength * (toFloat << truncate) (rng / wavelength))/wavelength
      echoFromTarget target = { r         = target.r
                              , theta     = target.theta
                              , alpha     = target.alpha
                              , phase     = ph target.r
                              , duration  = pulseDuration    -- microseconds
                              , amplitude = ( txHorizReflectedLobe target.theta )
                                            * ( txHiVertOmniLobe target.alpha )
                              }
      deriveEcho t d = Dict.insert t.r (echoFromTarget t) d
  in
      List.foldl deriveEcho Dict.empty targets


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
  , echoes : Dict Float Echo
  }


init : () -> (Model, Cmd Msg)
init _ =
  let
    targetsBaseline = [ bomber1, bomber2, fighter1 ] ++ (stationClutter bawdsey)
  in
    ( { zone = Time.utc 
      , startTime = 0
      , time = 0
      , lineData = lineSmoother []
      , station = bawdsey
      , targets = targetsBaseline
      , movedTargets = []
      , polarTargets = []
      , echoes = Dict.empty
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
    echoSignals = deriveEchoes convertedTargets
  in
      { model | startTime = if model.startTime == 0 then t else model.startTime 
              , time = t
              , movedTargets = targetsNow
              , polarTargets = convertedTargets
              , echoes       = echoSignals
              , lineData     = lineSmoother <| deriveTrace echoSignals 
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

type ViewMode = AsText | AsImage
viewMode = AsImage

view : Model -> Svg Msg
view m = case viewMode of
  AsText ->
    let t1 = Maybe.withDefault bomber1 (List.head m.movedTargets)
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
                     (\(i,x) -> [ Html.text <| String.fromFloat i
                                , Html.text " "
                                , Html.text <| String.fromFloat x
                                , Html.br [] []])
                     m.lineData
    in
      div [] lineInfo

  AsImage ->  
    svg
      [ viewBox "-10 -10 1020 420"
      , width "1020"
      , height "420"
      ]
      [ rect
          [ x "-10"
          , y "-10"
          , width "1020"
          , height "420"
          , fill "black"
          , stroke "black"
          , strokeWidth "3"
          , strokeLinejoin "round"
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
