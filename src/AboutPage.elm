module AboutPage exposing (..)

import Constants exposing (white)
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (class)
import Markdown exposing (defaultOptions)
import Messages exposing (Msg(..))


aboutPage : Element Msg
aboutPage =
    column []
        [ paragraph
            [ Font.color white
            , padding 40
            , width fill
            ]
            [ html content ]
        ]


content : Html msg
content =
    Markdown.toHtmlWith { defaultOptions | smartypants = True }
        [ class "content" ]
        """

# _For demonstration only_

## What is this?

This application shows the work of an operator
at a Chain Home station during the Battle of Britain in 1940.

## Platforms supported

This has been tested on and works fine using Safari, Opera, Chrome and Firefox browsers on MacOS X,
and Chrome, Firefox and Microsoft Edge browsers on Windows 10. It does *not* work with Internet Explorer 10 on Windows 10.
It also works on iPad with iOS 13.4 or higher; the iPad Mini will work but the screen is too small to recommend.
Other tablets have not been tested; feedback would be welcome but there is NO SUPPORT.

## Credits and licenses

 * Application code written by Peter Ward.
 * Historical research by Sharon Ward.
 * The application is written in Elm (Copyright © 2012-2020 Evan Czaplicki, BSD-3-Clause license)

"""
