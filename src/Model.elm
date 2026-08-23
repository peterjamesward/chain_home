module Model exposing (..)

import Calculator.Model
import Element exposing (Device)
import Keys exposing (Keys)
import Station exposing (Station)
import Types exposing (..)


type alias Model =
    { currPage : Page
    , webGLtime : Float -- now updated by the WebGL animation control.
    , modelTime : Int -- milliseconds of Posix time.
    , startTime : Int
    , azimuthModeTrace : List Echo
    , elevation_A_trace : List Echo
    , elevation_B_trace : List Echo
    , station : Station
    , targets : List Target
    , inRangeTargets : List Target
    , echoes : List Echo
    , skyline : List Line
    , goniometerAzimuth : Angle
    , gonioOutput : List Echo
    , keys : Keys
    , gonioDrag : Maybe ( Angle, Point ) -- angle and mouse position when mouse down
    , rangeSlider : Range
    , rangeDrag : Maybe ( Angle, Point )
    , rangeKnobAngle : Angle
    , goniometerMode : GoniometerMode
    , transmitAntenna : Antenna
    , reflector : Bool
    , receiveAB : Bool
    , receiveAntenna : Antenna
    , explainModeMenu : Bool
    , explainModeReceiver : Bool
    , explainModeMap : Bool
    , timeForNextRaid : Maybe Int
    , storedPlots : List RecordedPlot
    , isMenuOpen : Bool
    , calculator : Calculator.Model.Model
    , actualTraceVisibleOnMap : Bool
    , rangeCircleVisibleOnMap : Bool
    , gameMode : GameMode
    }
