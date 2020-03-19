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
    | UiLeftSide
    | UiRightSide
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
    | UiDummy


type TutorialScenario
    = ScenarioBasic
    | ScenarioTwoTogether
    | ScenarioTwoSeparate
    | ScenarioThreeOrMore
    | ScenarioFriendly

type TutorialStep
    = TutorialWelcome
    | TutorialIncomingRaid
    | TutorialAdjustRange
    | TutorialFindBearing
    | TutorialStoreBearing
    | TutorialStoreRange1 -- relates to bearing
    | TutorialHeightMode
    | TutorialFindElevation
    | TutorialStoreElevation
    | TutorialAdjustRangeForHeight
    | TutorialStoreRange2 -- relates to height
    | TutorialStoreStrength
    | TutorialShowCalculator
    | TutorialEnded -- last thing we show
    | TutorialDummy -- not shown, if it occurs.
