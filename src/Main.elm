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
import Echo exposing (..)
import Constants exposing (..)
import Config exposing (..)
import Skyline exposing (deriveSkyline, EdgeSegment, viewEdge, viewLineSegment)
import BeamSmoother exposing (beamPath)
import Utils exposing (..)

-- This is dummy line for me to practise with floats and trig.
myLineData t = [ (0.0, 0.0), (1000.0, 0.0)]

-- SOME DATA STRUCTURES - ALSO DON'T BELONG HERE

type alias LineData = List (Float, Float)

rangeScale = List.map (\i -> Svg.text_ [ x (String.fromInt (i*50)), y "-10", fill "lightgreen", textAnchor "right" ] 
                                       [ Svg.text (String.fromInt (i*5)) ])
  (List.range 0 19)

scalePathToDisplay : LineData -> LineData
scalePathToDisplay unscaled =
    let scalePoint (x,y) = (viewWidth * x / scaleWidthKilometers / 1000, ( y) * strengthToHeightFactor)
    in  List.map scalePoint unscaled

-- Deriving echoes is just applying the transmitter lobe function so
-- amplitude is function of ltheta and range. Later, IFF figures.
-- Including time here is just experimental for visual effects.

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
    ( { zone = Time.utc 
      , startTime = 0
      , time = 0
      , lineData = beamPath []
      , prevLine = beamPath []
      , olderLine = beamPath []
      , oldestLine = beamPath []
      , station = bawdsey
      , targets = targetsBaseline ++ (stationClutter bawdsey)
      , movedTargets = []
      , polarTargets = []
      , echoes = [ dummyFinalEcho ]
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
    skyline = deriveSkyline (scaleWidthKilometers * 1000) echoSignals
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
viewMode = AsText

crt m =
  let svgPointList = (polyLineFromCoords m.lineData) 
  in 
    svg [ viewBox "-10 -40 1020 450"
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


view : Model -> Svg Msg
view m = 
  --case viewMode of
  --  AsText ->
      let polarInfo = List.concatMap viewPolarTarget m.polarTargets
          echoInfo = List.concatMap viewEcho m.echoes
          edgeInfo = List.concatMap viewEdge m.skyline
          lineInfo = List.concatMap viewLineSegment m.lineData
      in
        (div []) <| List.concat [  [crt m]
                              --, [Html.hr [] []]
                              --, polarInfo 
                              --, echoInfo 
                              --, edgeInfo
                              --, lineInfo
                            ]

