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

# Apple Pie Recipe

  1. Invent the universe.
  2. Bake an apple pie.

"""
