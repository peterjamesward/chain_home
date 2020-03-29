module Types exposing (..)


type alias Angle =
    Float


type Page
    = InputPage
    | OperatorPage
    | OutputPage
    | TrainingPage
    | AboutPage
    | MapPage


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
    , iff : Maybe Int -- the value at which t mod 12 triggers a return
    , tutorial : Bool
    }


type alias Target =
    { startLatitude : Float
    , startLongitude : Float
    , latitude : Float -- position after movement
    , longitude : Float
    , height : Float -- in thousands of feet
    , heading : Float -- in radians from North
    , speed : Float -- miles per hour (!)
    , iff : Maybe Int -- the value at which t mod 12 triggers a return
    , iffActive : Bool -- pulsing now.
    , tutorial : Bool -- highlight this raid on the CRT for tutorial mode.
    , startTime : Int -- Each target needs it own time baseline.
    , rangeInMetres : Float -- metres
    , theta : Float -- azimuth radians 0 is North.
    , alpha : Float -- elevation in radians, ignoring curvature for now
    , positionHistory : List ( Int, Float, Float ) -- the actual track of the raid, minute by minute.
    }


type alias Echo =
    { sequence : Int
    , r : Float
    , theta : Float
    , alpha : Float
    , phase : Float
    , duration : Float
    , amplitude : Float
    , tutorial : Bool -- this raid needs to be highlighting in a tutorial.
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
    | UiMapPage
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
    | TutorialIntroduceTheTube
    | TutorialJustSwinging
    | TutorialIntroduceTheTube2
    | TutorialIntroduceGonio
