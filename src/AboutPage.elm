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

Scientists and engineers rapidly developed radio technology to prepare for  war, and were constantly
improving it. By modern standards it was primitive. The operator studied the "tube" looking for faint
signals amongst the noise.

## How should I use this?

We have picked five distinctive patterns of incoming aircraft. Under "Learn & Play", you can learn about how each of these
would appear on the tube, and what the operator would do to try to find the bearing and the height
so that this information can be passed on to Fighter Command.

You can then try to keep up with a series of incoming raids. The operator had to keep track of all
the raids and send frequent updates on their position to Fighter Command. Keeping
track of all the raids is not easy.

## Credits and licenses

 * Application code written by Peter Ward.
 * Historical research by Sharon Ward.
 * The application is written in Elm (Copyright © 2012-2020 Evan Czaplicki, BSD-3-Clause license)
 * elm-ui library (Copyright © 2018, Matthew Griffith, BSD-3-Clause license).
 * elm-webgl library (Copyright © 2014, John P Mayer, Jr, BSD-3-Clause license).
 * Packaging for iOS courtesy of Cordova (Apache License 2.0).

"""
