module Model exposing (Model)

import Config exposing (TargetSelector)
import Echo exposing (Echo)
import Element exposing (Device)
import Keys exposing (Keys)
import Station exposing (Station)
import Types exposing (..)



type alias Model =
    { currPage : Page
    , webGLtime : Float -- now updated by the WebGL animation control.
    , modelTime : Int -- milliseconds
    , startTime : Int
    , azimuthModeTrace : List Echo
    , elevation_A_trace : List Echo
    , elevation_B_trace : List Echo
    , station : Station
    , targets : List Target
    , movedTargets : List Target
    , polarTargets : List PolarTarget
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
    , isMenuOpen : Bool -- use this for a dfropdown "Learn" menu.
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
    }
