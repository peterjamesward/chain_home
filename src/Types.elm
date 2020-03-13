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


type alias Keys =
    -- Keep track of any significant keys' state, such as for adjusting goniometer or range slider.
    { gonioClock : Bool -- A
    , gonioAnti : Bool -- Q
    , rangeLeft : Bool -- left arrow
    , rangeRight : Bool -- right arrow
    }


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


type Tutorial
    = TutorialWelcome
    | TutorialDescribeCRT
    | TutorialRangeScale
    | TutorialCRTTrace
    | TutorialIncomingRaid
    | TutorialDummy
