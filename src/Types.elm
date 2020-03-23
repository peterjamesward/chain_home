module Types exposing (..)


type alias Angle =
    Float


type Page
    = InputPage
    | OperatorPage
    | OutputPage
    | TrainingPage


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


type alias Target =
    { latitude : Float
    , longitude : Float
    , height : Float -- in thousands of feet
    , bearing : Float -- in degrees from North
    , speed : Float -- miles per hour (!)
    , iff : Maybe Int -- the value at which t mod 12 triggers a return
    , iffActive : Bool -- pulsing now.
    , tutorial : Bool -- highlight this raid on the CRT for tutorial mode.
    , startTime : Int -- Each target needs it own time baseline.
    }


type alias PolarTarget =
    { r : Float -- metres
    , theta : Float -- radians
    , alpha : Float -- radians, ignoring curvature for now
    , iff : Maybe Int -- pulsing; time when pulse started
    , iffActive : Bool -- pulsing now.
    , tutorial : Bool
    }


type
    InputState
    -- Inferred sequence of operator actions
    = BearingInput
    | BearingRangeInput
    | HeightInput
    | HeightRangeInput


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
    | UiDummy


type TutorialScenario
    = ScenarioBasic
    | ScenarioTwoTogether
    | ScenarioTwoSeparate
    | ScenarioThreeToSix
    | ScenarioFriendly


type TutorialStep
    = TutorialWelcome
    | TutorialIncomingRaid
    | TutorialAdjustRange
    | TutorialFindBearing
    | TutorialFindBearingPlaneA
    | TutorialFindBearingPlaneB
    | TutorialStoreBearing
    | TutorialStoreRange1 -- relates to bearing
    | TutorialHeightMode
    | TutorialFindElevation
    | TutorialStoreElevation
    | TutorialAdjustRangeForHeight
    | TutorialStoreRange2 -- relates to height
    | TutorialStoreStrength
    | TutorialShowCalculator
    | TutorialShowCalculator2
    | TutorialEnded -- last thing we show
    | TutorialDummy -- not shown, if it occurs.
