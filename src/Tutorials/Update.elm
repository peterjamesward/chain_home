module Tutorials.Update exposing (..)

import Tutorials.Messages exposing (TutorialMsg(..))
import Tutorials.Model exposing (Tutorial, TutorialActionList)
import Tutorials.Tutorials exposing (advanceTutorial, goBackInTutorial, tutorialStartScenario)


update : TutorialMsg -> Tutorial -> ( Tutorial, TutorialActionList )
update tutMsg activeTutorial =
    case tutMsg of
        DisplayTraining scenario ->
            tutorialStartScenario scenario

        TutorialAdvance ->
            advanceTutorial activeTutorial

        TutorialBack ->
            goBackInTutorial activeTutorial
