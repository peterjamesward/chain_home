module Constants exposing (..)

import Element exposing (Color, rgb255)

-- DON"T use elm-format on this!!

lightSpeed = 300000000.0
frequency =  20000000.0
wavelength = lightSpeed / frequency

pulseDuration = 20.0 -- microseconds
pulseWidth = lightSpeed * pulseDuration / 10000000.0

scaleWidthKilometers = 160

rangeToXFactor = (scaleWidthKilometers * 1000.0)

blue = rgb255 52 101 164
black = rgb255 0 0 0
darkGrey = rgb255 51 51 51

flatMidnightBlue = rgb255 44 62 80
flatSunflower = rgb255 241 196 15
lightCharcoal = rgb255 136 138 133
green = rgb255 52 164 100
vividGreen = rgb255 36 200 33
white = rgb255 200 200 200
midGray =  rgb255 80 80 80
flatWetAsphalt = rgb255 52 73 94

-- These four colours from W3C 2018 palettes.
paletteGrey = rgb255 62 62 68
paletteLightGreen = rgb255 130 183 75
paletteDarkGreen = rgb255 64 93 39
paletteSand = rgb255 193 148 106
beamGreen = rgb255 0 (truncate (0.7 * 255)) 0
