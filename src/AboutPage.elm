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
    paragraph
        [ Font.color white
        , padding 40
        , width fill
        ]
        [ html content ]


content : Html msg
content =
    Markdown.toHtmlWith { defaultOptions | smartypants = True }
        [ class "content" ]
        """
## What is this?

This application aims to give an impression of being an operator
at a Chain Home station during the Battle of Britain in 1940.

## How should I use this?

 __ [Click here for User Guide](img/User%20guide.pdf) __

Using the Menu, under **Learn & Play**, discover how different types of raid
would appear on the tube, and how the operator would find each raid's bearing, height
and strength to be passed on to Fighter Command.

You can then test your skills with a series of incoming raids.

**Receiver** is a mock-up of the main operator's control panel, slightly simplified here.

**Calculator** illustrates the output from the electrical calculator that derives the height and
summarises the information to be passed to Fighter Command.

**Map** allows you to see how well your plots correspond with the actual raid positions.
This of course would not have been available to the operator!

Each screen (other than this one) has a green **?** button which will provide some explanation of the elements,

## Platforms supported

This has been tested on and works fine using Safari, Chrome and Firefox browsers on MacOS X,
and Chrome, Firefox and Microsoft Edge browsers on Windows 10. It does not work with Internet Explorer 10 on Windows 10.
It also works on iPad with iOS 13.4 or higher; the iPad Mini will work but the screen is too small to work properly.
Other tablets have not been tested; feedback would be welcome but there is NO SUPPORT.

## Credits and licenses

 * Application code written by Peter Ward.
 * Historical research by Sharon Ward.
 * The application is written in Elm (Copyright © 2012-2020 Evan Czaplicki, BSD-3-Clause license)

"""
