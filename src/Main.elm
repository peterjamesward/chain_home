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
--import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch

import Station exposing (..)
import Target exposing (..)
import Echo exposing (..)
import Constants exposing (..)
import Config exposing (TargetSelector, targetConfigurations, bawdsey, targetsBaseline, toggleConfig, getAllTargets)
import Skyline exposing (deriveSkyline, EdgeSegment, viewEdge, viewLineSegment)
import BeamSmoother exposing (beamPath)
import Utils exposing (..)
import Receiver exposing (goniometerMix)
import Goniometer exposing (showGonio, clickableGonioImage)


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


-- When we have mouse tracking we can have non-integer movements.
swingGoniometer : Float -> Keys -> Float
swingGoniometer angle keys = 
  if keys.gonioClock && keys.gonioAnti then
    angle
  else if keys.gonioClock then
    angle + (degrees 1.0)
  else if keys.gonioAnti then
    angle - (degrees 1.0)
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
  , gonioDrag : Maybe (Float, (Float, Float))  -- angle and mouse position when mouse down
  , activeConfigurations : List TargetSelector
  }


init : () -> (Model, Cmd Msg)
init _ =
    ( { zone         = Time.utc 
      , startTime    = 0
      , time         = 0
      , lineData     = beamPath []
      , station      = bawdsey
      , targets      = getAllTargets targetConfigurations
      , movedTargets = []
      , polarTargets = []
      , echoes       = []
      , skyline      = []
      , goniometer   = degrees 10 -- relative to Line Of Shoot.
      , gonioOutput  = []
      , keys         = noKeys
      , gonioDrag = Nothing
      , activeConfigurations = targetConfigurations
      }
    , Task.perform AdjustTimeZone Time.here
    )

-- UPDATE
type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | KeyChanged Bool String
  | GonioGrab ( Float, Float )
  | GonioMove ( Float, Float )
  | GonioRelease ( Float, Float )
  | ToggleConfig Int

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

    GonioGrab offset ->
      ( { model | gonioDrag = Just (model.goniometer, offset)
        }
      , Cmd.none
      )

    GonioMove offset ->
      ( { model | goniometer = case model.gonioDrag of
                                  Nothing -> 
                                    model.goniometer

                                  Just (startAngle, startXY) ->
                                    goniometerTurnAngle startAngle startXY offset
        }
      , Cmd.none
      )

    GonioRelease offset ->
      ( { model | gonioDrag = Nothing 
        }
      , Cmd.none
      )

    ToggleConfig idx ->
        let newConfig = toggleConfig model.activeConfigurations idx 
        in
          ( { model | activeConfigurations = newConfig
                    , targets = getAllTargets newConfig
            }
          , Cmd.none
          )

goniometerTurnAngle : Float -> (Float, Float) -> (Float, Float) -> Float
goniometerTurnAngle startAngle (startX, startY) (newX, newY) =
  let
    (_, dragStartAngle) = toPolar (startX - 150, startY - 150) -- where on control was clicked
    (_, dragNowAngle) = toPolar (newX - 150,  newY - 150) -- where that point is now
  in 
    startAngle + dragNowAngle - dragStartAngle

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =

  Sub.batch
    [ E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
    , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
    , Time.every 50 Tick
    ]

{-
  DRAGGABLE Goniometer
  I have found this packages to be the solution to dragging for mouse and touch.
  https://package.elm-lang.org/packages/mpizenberg/elm-pointer-events/latest/
-}

clickableGonioImageStyles m = 
    let 
        styles = 
            [ H.width 300
            , H.max "300px"
            , Mouse.onDown (\event -> GonioGrab event.offsetPos)
            , Mouse.onMove (\event -> GonioMove event.offsetPos) 
            , Mouse.onUp (\event -> GonioRelease event.offsetPos) 
            , Touch.onStart (GonioGrab << touchCoordinates)
            , Touch.onMove (GonioMove << touchCoordinates)
            , Touch.onEnd (GonioRelease << touchCoordinates)    
            ]
    in
        div 
            styles
            (clickableGonioImage (m.goniometer + m.station.lineOfShoot))

revealMouse pos = Html.text <| stringifyPoint pos

touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )

-- VIEW


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

debugInfo : Model -> List (Html Msg)
debugInfo m = 
  let 
    polarInfo = List.concatMap viewPolarTarget m.polarTargets
    echoInfo = List.concatMap viewEcho m.echoes
    edgeInfo = List.concatMap viewEdge m.skyline
    lineInfo = List.concatMap viewLineSegment m.lineData
    theta = m.goniometer + m.station.lineOfShoot
  in
    [ Html.hr [] [] ]
    --++ polarInfo 
    --++ echoInfo 
    --++ edgeInfo
    --++ lineInfo

-- Show list of configurations with Checkboxes.
targetSelector active =
    let
        display g = div 
            [
                Mouse.onClick (\_ -> ToggleConfig g.id)
            ]
            [
                input [H.type_ "checkbox", checked g.active] []
            ,   Html.text g.description
            ]
    in
        ul []
        (List.map display active)
        
view : Model -> Html Msg
view m = 
  div [
  ] 
  [ 
  --  showGonio m
  --, Html.br [] []
    Html.text "Emulation of Chain Home RDF receiver by Pete Ward"
  , targetSelector m.activeConfigurations
  , clickableGonioImageStyles m
  , crt m 
  , div [] (debugInfo m)
  ]
                      
