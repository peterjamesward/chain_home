module Main exposing (..)

import Svg exposing (..)
import Svg.Attributes as S exposing (..)
import List
import Tuple exposing (..)
import String
import Browser
import Browser.Events as E
import Json.Decode as D
import Html exposing (..)
import Html.Attributes as H exposing (..)
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
import Receiver exposing (goniometerMix)
import Goniometer exposing (showGonio, showGonioImage)


-- This is dummy line for me to practise with floats and trig.
myLineData t = [ (0.0, 0.0), (1000.0, 0.0)]

-- SOME DATA STRUCTURES - ALSO DON'T BELONG HERE

type alias LineData = List (Float, Float)

rangeScale = List.map (\i -> Svg.text_ [ x (String.fromInt (i*50))
                                       , y "-10"
                                       , fill "lightgreen"
                                       , textAnchor "right" 
                                       ] 
                                       [ Svg.text (String.fromInt (i*5)) ])
  (List.range 0 19)

scalePathToDisplay : LineData -> LineData
scalePathToDisplay unscaled =
    let scalePoint (x,y) = (viewWidth * x / scaleWidthKilometers / 1000, ( y) * strengthToHeightFactor)
    in  List.map scalePoint unscaled

-- INTERACTIVITY - starting with a linear goniometer.

type alias Keys =
  { gonioClock : Bool -- W
  , gonioAnti  : Bool -- Q
  }

noKeys : Keys
noKeys =
  Keys False False

updateKeys : Bool -> String -> Keys -> Keys
updateKeys isDown key keys =
  case key of
    "q"  -> { keys | gonioAnti   = isDown }
    "a"  -> { keys | gonioClock  = isDown }
    _    -> keys


swingGoniometer : Float -> Keys -> Float
swingGoniometer angle keys = 
  if keys.gonioClock && keys.gonioAnti then
    angle
  else if keys.gonioClock then
    degrees <| Basics.min  90.0 (angle * 180/pi + 1.0)
  else if keys.gonioAnti then
    degrees <| Basics.max -90.0 (angle * 180/pi - 1.0)
  else
    angle

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
  { zone         : Time.Zone
  , startTime    : Int -- millseconds from t-zero
  , time         : Int
  , lineData     : List (Float, Float)
  , station      : Station
  , targets      : List Target
  , movedTargets : List Target
  , polarTargets : List PolarTarget
  , echoes       : List Echo
  , skyline      : List ((Float, Float),(Float, Float))
  , goniometer   : Float
  , gonioOutput  : List Echo
  , keys         : Keys
  }


init : () -> (Model, Cmd Msg)
init _ =
    ( { zone         = Time.utc 
      , startTime    = 0
      , time         = 0
      , lineData     = beamPath []
      , station      = bawdsey
      , targets      = targetsBaseline --++ (stationClutter bawdsey)
      , movedTargets = []
      , polarTargets = []
      , echoes       = []
      , skyline      = []
      , goniometer   = degrees 10 -- relative to Line Of Shoot.
      , gonioOutput  = []
      , keys         = noKeys
      }
    , Task.perform AdjustTimeZone Time.here
    )

-- UPDATE
type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | KeyChanged Bool String

-- THIS IS IT. This is the place where it all comes together.
deriveModelAtTime : Model -> Int -> Model
deriveModelAtTime model t =
  let
    targetsNow       = List.map (targetAtTime t model.startTime) model.targets
    convertedTargets = List.map (mapToPolar bawdsey) targetsNow
    echoSignals      = deriveEchoes convertedTargets t
    gonioOut         = goniometerMix model.goniometer echoSignals 
    newSkyline       = deriveSkyline (scaleWidthKilometers * 1000) gonioOut
  in
      { model | startTime    = if model.startTime == 0 then t else model.startTime 
              , time         = t
              , movedTargets = targetsNow
              , polarTargets = convertedTargets
              , echoes       = echoSignals
              , skyline      = newSkyline
              , gonioOutput  = gonioOut
              , lineData     = scalePathToDisplay <| beamPath newSkyline
              , goniometer   = swingGoniometer model.goniometer model.keys
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

    KeyChanged isDown key ->
      ( { model | keys = updateKeys isDown key model.keys }
      , Cmd.none
      )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =

  Sub.batch
    [ E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
    , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
    , Time.every 100 Tick
    ]


-- VIEW

type ViewMode = AsText | AsImage
viewMode = AsText

crt m =
  let svgPointList = (polyLineFromCoords m.lineData) 
  in 
    svg [ viewBox "-10 -40 1020 450"
        , S.width "1020"
        , S.height "420"
        ]
        (List.append 
          [ rect
            [ x "-10"
            , y "-40"
            , S.width "1020"
            , S.height "450"
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
      let 
          -- Mostly debugging info ...
          polarInfo = List.concatMap viewPolarTarget m.polarTargets
          echoInfo = List.concatMap viewEcho m.echoes
          gonioInfo = List.concatMap viewEcho m.gonioOutput
          edgeInfo = List.concatMap viewEdge m.skyline
          lineInfo = List.concatMap viewLineSegment m.lineData
          gonio = showGonio m
      in
        (div []) <| List.concat [ [gonio, Html.br [] [], showGonioImage m, crt m ]
                              , [Html.hr [] []]
                              --, polarInfo 
                              --, echoInfo 
                              --, gonioInfo
                              --, edgeInfo
                              --, lineInfo
                            ]

