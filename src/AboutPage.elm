module AboutPage exposing (..)

import Constants exposing (white)
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (class)
import Markdown exposing (defaultOptions)
import Messages exposing (Msg)


aboutPage : Element Msg
aboutPage =
    paragraph [ Font.color white, padding 20, width fill ]
        [ html content ]


content : Html msg
content =
    Markdown.toHtmlWith { defaultOptions | smartypants = True }
        [ class "content" ]
        """

## What is this?

This application aims to give an impression of being an operator
at a Chain Home station during the Battle of Britain in 1940.

Scientists and engineers developed radio technology rapidly to prepare for the war, and were constantly
improving it. By modern standards, it was primitive. Rather than the circular screen familiar today, with aircraft
conveniently labelled, the operator would spend hours studying the "tube" looking for faint
signals amongst the noise.

To help, we have included the "Electrical Calculator" that works out the position and the height
-- before that, some lengthy calculations were required.

## How should I use this?

We have picked five distinctive patterns of incoming aircraft. Under "Learn & Play", you can learn about how each of these
would appear on the tube, and what the operator would do to try to find the bearing and the height
so that this information can be passed on to Fighter Command.

You can then try to keep up with a series of incoming raids. The operator had to keep track of all
the raids and send frequent updates on their position to Fighter Command. Keeping
track of all the raids is not easy.

## Credits

 * Application code written by Peter Ward.
 * Historical research by Sharon Ward.
 * The application is written in Elm (elm-lang.org) and uses elm-ui and WebGL for the display.
 * Temporary map image from Google.

"""
