module Model exposing (Model)

import Config exposing (TargetSelector)
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
    , inputState : InputState
    , storedAzimuth : Maybe Float
    , storedElevation : Maybe Float
    , storedAzimuthRange : Maybe Float
    , storedElevationRange : Maybe Float
    , storedStrength : Maybe Int
    , storedFriendly : Maybe Bool
    , storedStrengthPlus : Maybe Bool
    , tutorialScenario : Maybe TutorialScenario
    , tutorialStage : Maybe TutorialStep
    , explainMode : Bool
    , tutorialsCompleted : List TutorialScenario -- would like to use Set but can't. Because Elm.
    , newRaid : Maybe Target
    , timeForNextRaid : Maybe Int
    , storedPlots : List (Int, Float, Float)
    , actualTraceVisibleOnMap : Bool
    , rangeCircleVisibleOnMap : Bool
    }
