module Tutorials.Messages exposing (..)

import Tutorials.ActionCodes exposing (TutorialScenario)


type TutorialMsg
    = DisplayTraining TutorialScenario
    | TutorialAdvance
    | TutorialBack
