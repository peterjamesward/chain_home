module AboutPage exposing (..)

import Constants exposing (white)
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (class)
import Markdown
import Messages exposing (Msg)


aboutPage : Element Msg
aboutPage =
    el [ Font.color white, padding 20 ] <|
        html content


content : Html msg
content =
    Markdown.toHtml [ class "content" ] """

## What is this?

This application is meant to give an impression of what it would have been like to be an operator
at a Chain Home station during the Battle of Britain in 1940.

Radar technology had been developed rapidly to prepare for the war, and was constantly being updated.
By modern standards, it was primitive. Rather than the circular screen familiar today, with aircraft
conveniently labelled, the operator would spend hours studying the "tube" looking for faint
signals amongst the noise.

To help, we have included the "Electrical Calculator" that works out the position and the height
-- before that, some lengthy calculations were required.

## How should I use this?

We have picked five distinctive patterns of incoming aircraft. You can learn about how each of these
would appear on the tube, and what the operator would do to try to find the bearing and the height
so that this information can be passed on to Fighter Command.

You can then try to keep up with a series on incoming raids. The operator would keep track of
the raids and send frequent updates on their position to Fighter Command. Keeping
track of all the raids is not easy.

## Credits

Application code written by Peter Ward. Historical input by Sharon Ward.
The application is written in Elm (elm-lang.org) and uses elm-ui and WebGL for the display.

"""
