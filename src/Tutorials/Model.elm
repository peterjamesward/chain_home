module Tutorials.Model exposing (..)

import Tutorials.ActionCodes exposing (TutorialActionCode, TutorialTextFunction)
import Types exposing (UiComponent)
import Zipper exposing (Zipper)


type alias TutorialActionList =
    List TutorialActionCode


type alias TutorialEntry =
    { uiComponent : UiComponent -- The UI component to be highlighted
    , entryActions : TutorialActionList -- Changes to the model so we can be idempotent
    , stateActions : TutorialActionList -- Things we must do whilst in this state.
    , exitActions : TutorialActionList -- Changes to the model, to make sure we exit cleanly
    , tutorialText : TutorialTextFunction
    }


type alias Tutorial =
    Maybe (Zipper.Zipper TutorialEntry)
