module Model exposing (..)

import Calculator.Model
import Element exposing (Device)
import Keys exposing (Keys)
import Station exposing (Station)
import Tutorials.ActionCodes exposing (TutorialScenario(..))
import Tutorials.Model exposing (Tutorial)
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
    , activeConfigurations : List TargetSelector
    , rangeSlider : Range
    , outputDevice : Device
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
    , gameMode : GameMode
    , isMenuOpen : Bool
    , calculator : Calculator.Model.Model
    , actualTraceVisibleOnMap : Bool
    , rangeCircleVisibleOnMap : Bool
    , applicationMode : ApplicationMode
    , fullScreenCRT : Bool
    }


type ApplicationMode
    = InteractiveMode
    | TutorialMode Tutorial
    | KioskMode Tutorial Int


type alias TargetSelector =
    { id : TutorialScenario
    , active : Bool -- Whether this group is active (dynamic)
    , description : String -- e.g. "mass raid"
    }


availableTargetOptions : List TargetSelector
availableTargetOptions =
    [ TargetSelector ScenarioBasic False "One aircraft"
    , TargetSelector ScenarioTwoTogether False "Two aircraft in close formation"
    , TargetSelector ScenarioTwoSeparate False "Two aircraft at the same range"
    , TargetSelector ScenarioThreeToSix False "Three to six planes in formation"
    , TargetSelector ScenarioFriendly False "A friendly aircraft with IFF"
    , TargetSelector ScenarioSharon False "Sample mix"
    ]
