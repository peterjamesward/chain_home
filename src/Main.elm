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
import Set exposing (..)

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

lightSpeed = 300000000.0
frequency  = 20000000.0
wavelength = lightSpeed / frequency
pulseDuration = 40.0  -- microseconds 
pulseWidth = lightSpeed * pulseDuration / 10000000.0  -- TODO: scale seems wrong here.

-- Some facts about our display
beamSweepMax = 0.001 -- Maximum vertical displacement for one microsecond.
beamSweepDown = 0.003 -- Visually, they seem to return the beam more slowly.
beamSweepUp   = -0.0005  
scaleWidthKilometers = 150
viewWidth = 1000
viewHeight = 400
strengthToHeightFactor = 100.0
rangeToXFactor = viewWidth / (scaleWidthKilometers * 1000.0)
rangeDeltaPerMicrosecond = scaleWidthKilometers * 2


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

dummyFinalEcho = { r = scaleWidthKilometers * 1000
                 , theta = 0
                 , alpha = 0
                 , phase = 0
                 , amplitude = 0
                 , duration = 0
                 }

dummyInitialEcho = { dummyFinalEcho | r = 0 }

addEchoToDict e d = Dict.insert e.r e d

-- A way to make sure that we start and end cleanly??
dummyEchoes = [dummyInitialEcho, dummyFinalEcho]
      
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
          , latitude  = degrees 52.0
          , height    = 30 -- ,000 ft
          , bearing   = degrees 280
          , speed     = 200.0 -- mph
          , iff       = False 
          }
 
bomber2 = { longitude = degrees 2.000
          , latitude  = degrees 52.05
          , height    = 30.2 -- ,000 ft
          , bearing   = degrees 280
          , speed     = 200.0 -- mph
          , iff       = False 
          }
 
bomber3 = { longitude = bawdsey.longitude + (degrees 0.8)
          , latitude  = bawdsey.latitude + (degrees 0.5)
          , height    = 20 -- ,000 ft
          , bearing   = degrees 270
          , speed     = 220 -- mph
          , iff       = False 
          }
 
bomber4 = { longitude = bawdsey.longitude + (degrees 0.8)
          , latitude  = bawdsey.latitude - (degrees 0.5)
          , height    = 20 -- ,000 ft
          , bearing   = degrees 270
          , speed     = 220 -- mph
          , iff       = False 
          }
 
fighter1 = { longitude = degrees 1.4
           , latitude  = degrees 52.0
           , height    = 5 -- ,000 ft
           , bearing   = degrees 90
           , speed     = 400 -- mph
           , iff       = False 
           }
 
-- Skyline algorithm works out the trace from combined echoes.

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

type alias EdgeInfo = (Float, Echo, Bool)  -- x coord of edge, leading edge,  and whether leading edge.

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

processEdge : EdgeInfo 
              -> (List EdgeSegment, List Echo)
              -> (List EdgeSegment, List Echo)
processEdge (p, echo, isLeading) (lineData, activeEchoes) = 
  let  newActiveEchoes = case isLeading of
                              True  -> echo :: activeEchoes
                              False -> List.filter ((/=) echo) activeEchoes
       newX = p
       newY = combineEchoes newActiveEchoes
       (_,(prevX, prevY)) = Maybe.withDefault dummyLeadingEdge <| List.head lineData
  in
       ( ((newX, prevY), (newX, newY)) :: lineData
       , newActiveEchoes
       )


deriveSkyline : List Echo -> List EdgeSegment
deriveSkyline allEchoes =
  -- Let's put the echoes in a dictionary, indexed by range (probably unique!).
  -- Then we need a sorted list of edges (front and back).
  -- Sorted list will index into the dictionary, for easy access to each target.
  let activeEchoes = []
      leadingEdges = List.map (\e-> (e.r, e, True) ) allEchoes
      trailingEdges = List.map (\e -> (e.r + pulseWidth, e, False )) allEchoes 
      allEdges = List.sortBy (\(r,_,_) -> r) (leadingEdges ++ trailingEdges)
      --extractLineData (line, _, _) = line
  in
      first <| List.foldl processEdge 
                                    ([], activeEchoes) 
                                    allEdges

-- Real CH CRTs would not draw vertical lines - it takes time to build up the voltages
-- on the deflection plated. We shall simulate that here.
-- We shall not bother to limit acceleration, unless we have to.

dummyLeadingEdge = ((0.0,0.0),(0.0,0.0))
dummyTrailingEdge = ((1000.0,0.0),(1000.0,0.0))

-- Latest attempt at a simple beam movement smoother, to simulate limitation on beam vertical slope.
-- The beam goes left to right, the edges go up and down.
-- When affected by an edge the beam can only go up or down at at fixed speed - beanSweepMax..
-- If the beam reaches its goal Y before the next edge, it will move horizontally only.
-- If the next edge comes before the beam reaches its goal, the new edge takes precedence
-- and its (far) end becomes the new goal for the beam.
-- When there are no more edges the beam returns to vertical zero.
-- The implementation is a fold with nuance.
-- As we encounter an edge, it becomes the new goal but we do not yet 'draw' anything, because
-- we know not yet the next edge.
-- So, the second part is to complete the processing of the previous edge.
-- If it had time to reach the goal from its previous position, 
--    we have a line segment from previous position to goal, AND
--    a horizontal segment to the start of the new egde.
-- If it had not time to reach the goal, 
--    we draw a line segment from its previous position as far as it went
-- In either case, we update the beam position and set the new goal from the new edge.


beamSmoothingFunction : EdgeSegment -> 
    (LineData,  EdgeSegment) -> 
    (LineData,  EdgeSegment) 
beamSmoothingFunction newEdge (lines, prevEdge) =
    let
        ((newX1, newY1),(newX2, newY2)) = newEdge
        ((prevX1, prevY1),(prevX2, prevY2)) = prevEdge
        (beamX, beamY) = Maybe.withDefault (0.0,0.0) <| List.head lines
        
        beamDirection = if (prevY2 > prevY1) then 1 else -1
        beamSpeed = if (prevY2 > prevY1) then beamSweepDown else beamSweepUp
        edgeInterval = (newX1 - prevX1)  -- in metres range (!)
        intervalNeededToReachGoal = abs (prevY2 - beamY) / (abs beamSpeed)

        newLines =  if (edgeInterval >= intervalNeededToReachGoal) then
                        -- reached goal, and we can work out when
                        [(newX1, prevY2), (beamX + intervalNeededToReachGoal, prevY2)]
                    else 
                        -- did not reach goal, record where it arrived
                        [(newX1, beamY + edgeInterval * beamDirection * beamSpeed)]
    in              
        ( newLines ++ lines, newEdge )

beamPath : List EdgeSegment -> LineData
beamPath edges =
    let
        (lines, _) = List.foldr beamSmoothingFunction 
                     ( [], dummyLeadingEdge ) 
                     edges
    in
        lines

scalePathToDisplay : LineData -> LineData
scalePathToDisplay unscaled =
    let scalePoint (x,y) = (viewWidth * x / scaleWidthKilometers / 1000, ( y) * strengthToHeightFactor)
    in  List.map scalePoint unscaled

-- Deriving echoes is just applying the transmitter lobe function so
-- amplitude is function of ltheta and range. Later, IFF figures.
-- Including time here is just experimental for visual effects.

-- 224/01 We're losing edges I think. Let's simplify even if at cost of performance.

deriveEchoes : List PolarTarget -> Int -> List Echo
deriveEchoes targets time = 
  let 
      ph rng = asin <| sin (2 * (toFloat time) * rng) --/wavelength / 1000)  -- clearer not cheaper
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
  , prevLine : List (Float, Float)
  , olderLine : List (Float, Float)
  , oldestLine : List (Float, Float)
  , station : Station
  , targets : List Target
  , movedTargets : List Target
  , polarTargets : List PolarTarget
  , echoes : List Echo
  , skyline : List ((Float, Float),(Float, Float))
  }


init : () -> (Model, Cmd Msg)
init _ =
  let
    targetsBaseline = [ bomber1
                      , bomber2
                      , bomber3
                      , bomber4
                      , fighter1 
                      ]
                      --++ (stationClutter bawdsey)
  in
    ( { zone = Time.utc 
      , startTime = 0
      , time = 0
      , lineData = beamPath []
      , prevLine = beamPath []
      , olderLine = beamPath []
      , oldestLine = beamPath []
      , station = bawdsey
      , targets = targetsBaseline
      , movedTargets = []
      , polarTargets = []
      , echoes = dummyEchoes
      , skyline = []
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
    echoSignals = deriveEchoes convertedTargets t
    skyline = deriveSkyline <| dummyEchoes ++ echoSignals
  in
      { model | startTime = if model.startTime == 0 then t else model.startTime 
              , time = t
              , movedTargets = targetsNow
              , polarTargets = convertedTargets
              , echoes       = echoSignals
              , skyline      = skyline
              , oldestLine   = model.olderLine
              , olderLine    = model.prevLine
              , prevLine     = model.lineData
              , lineData     = scalePathToDisplay <| beamPath skyline
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
  Time.every 100 Tick

-- VIEW

type ViewMode = AsText | AsImage
viewMode = AsImage

viewPolarTarget p1 =   [ Html.text "r "
                        , Html.text <| String.fromFloat <| p1.r
                       , Html.br [] []
                       , Html.text "theta "
                       , Html.text <| String.fromFloat <| p1.theta
                       , Html.br [] []
                       , Html.text "alpha "
                       , Html.text <| String.fromFloat <| p1.alpha
                       , Html.br [] []
                       , Html.hr [] []
                       ]

viewEcho e = [ Html.text "r "
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

viewEdge ((x1,y1),(x2,y2)) = [ Html.text "( "
                , Html.text <| String.fromFloat <| x1
                , Html.text " , "
                , Html.text <| String.fromFloat <| y1
                , Html.text " ), ( "
                , Html.text <| String.fromFloat <| x2
                , Html.text " , "
                , Html.text <| String.fromFloat <| y2
                , Html.text " )"
                , Html.br [] []
               , Html.hr [] []
               ]

viewLineSegment (x,y) = [ Html.text "( "
                , Html.text <| stringifyPoint (x,y)
                --, Html.text <| String.fromFloat <| x
                --, Html.text " , "
                --, Html.text <| String.fromFloat <| y
                , Html.text " )"
                , Html.br [] []
               , Html.hr [] []
               ]

view : Model -> Svg Msg
view m = 
  let svgPointList = (polyLineFromCoords m.lineData) in 
  case viewMode of
    AsText ->
      let polarInfo = List.concatMap viewPolarTarget m.polarTargets
          echoInfo = List.concatMap viewEcho m.echoes
          edgeInfo = List.concatMap viewEdge m.skyline
          lineInfo = List.concatMap viewLineSegment m.lineData
      in
        div [] ( [] 
                 ++ polarInfo 
                 ++ echoInfo 
                 --++ edgeInfo
                 --++ lineInfo
               )

    AsImage ->  
      svg
        [ viewBox "-10 -40 1020 450"
        , width "1020"
        , height "420"
        ]
        (List.append 
          [ rect
            [ x "-10"
            , y "-40"
            , width "1020"
            , height "450"
            , fill "black"
            , stroke "black"
            , strokeWidth "3"
            , strokeLinejoin "round"
            ]
            []
        , polyline
            [ points svgPointList
            , fill "none"
            , stroke "forestgreen"
            , opacity "60%"
            , strokeWidth "2.5"
            ]
            []
        , polyline
            [ points svgPointList
            , fill "none"
            , stroke "springgreen"
            , strokeWidth "0.8"
            ]
            []
        ] rangeScale)

rangeScale = List.map (\i -> Svg.text_ [ x (String.fromInt (i*50)), y "-10", fill "lightgreen", textAnchor "right" ] 
                                       [ Svg.text (String.fromInt (i*5)) ])
  (List.range 0 19)
