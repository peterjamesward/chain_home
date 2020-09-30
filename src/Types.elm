module Types exposing (..)


type alias Angle =
    Float


type Page
    = InputPage
    | OperatorPage
    | OperatorPageInTutorial
    | CalculatorPage
    | CalculatorInTutorial
    | TrainingPage
    | AboutPage
    | MapPage
    | SplashPage


type alias Flags =
    ()


type alias Point =
    ( Float, Float )


type alias Line =
    ( Point, Point )


type alias Range =
    Float


type GoniometerMode
    = Azimuth
    | Elevation


type alias Antenna =
    { verticalLobeFunction : Float -> Float
    , horizontalLobeFunction : Float -> Float
    }



-- DEEP BREATH: moving to single Target type.


type alias TargetProforma =
    -- Use this in config / tutorials; not actual targets.
    { latitude : Float
    , longitude : Float
    , height : Float -- in thousands of feet
    , heading : Float -- in degrees from North
    , speed : Float -- miles per hour (!)
    , strength : Int
    , iff : Maybe Int -- the value at which t mod 12 triggers a return
    }


type alias Target =
    { startLatitude : Float
    , startLongitude : Float
    , latitude : Float -- position after movement
    , longitude : Float
    , height : Float -- in thousands of feet
    , strength : Int -- how many aircraft
    , heading : Float -- in radians from North
    , speed : Float -- miles per hour (!)
    , iff : Maybe Int -- the value at which t mod 12 triggers a return
    , iffActive : Bool -- pulsing now.
    , startTime : Int -- Each target needs it own time baseline.
    , rangeInMetres : Float -- metres
    , theta : Float -- azimuth radians 0 is North.
    , alpha : Float -- elevation in radians, ignoring curvature for now
    , positionHistory : List RecordedPlot -- the actual track of the raid, minute by minute.
    }


type alias Echo =
    { sequence : Int
    , r : Float
    , theta : Float
    , alpha : Float
    , strength : Int
    , phase : Float
    , duration : Float
    , amplitude : Float
    }


type UiComponent
    = UiOperatorPage
    | UiRangeScale
    | UiCRT
    | UiGoniometer
    | UiRangeKnob
    | UiBothKnobs
    | UiSwitchPanel
    | UiRaidStrength
    | UiOperatorPrompts
    | UiAB
    | UiHeight
    | UiSense
    | UiClear
    | UiCalcStrength
    | UiGonioButton
    | UIRangeButton
    | UiCalcGrid
    | UiCalcHeight
    | UiCalcOffset
    | UiCalculator
    | UiConfigOptions
    | UiGoButton
    | UiMapPage
    | UiDummy


type GameMode
    = GameSingleRaid
    | GameThreeRaids
    | GameUnlimited
    | GameNone


type PlotType
    = UserPlot -- where the user thinks a raid is
    | TimedPlot -- raid positions stored each minute
    | ActualPlot -- raid positions stored when user stores a position


type alias RecordedPlot =
    { plotType : PlotType
    , time : Int
    , range : Float
    , bearing : Float
    , strength : Int
    }
