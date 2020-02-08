module Constants exposing (..)

import Element exposing (Color, rgb255)


lightSpeed =
    300000000.0


frequency =
    20000000.0


wavelength =
    lightSpeed / frequency


pulseDuration =
    40.0



-- microseconds


pulseWidth =
    lightSpeed * pulseDuration / 10000000.0



-- TODO: scale seems wrong here.
-- Some facts about our display
-- higher numbers are steeper slops.


beamSweepIncreasingAmplitude =
    0.001


beamSweepDecreasingAmplitude =
    -0.0003

beamResponseTime = -- A proxy for the time taken to the beam to respond to any change in amplitude.
    1


scaleWidthKilometers =
    150


viewWidth =
    1000


viewHeight =
    400


strengthToHeightFactor =
    100.0


rangeToXFactor =
    viewWidth / (scaleWidthKilometers * 1000.0)


rangeDeltaPerMicrosecond =
    scaleWidthKilometers * 2


blue : Color
blue =
    rgb255 52 101 164


lightCharcoal : Color
lightCharcoal =
    rgb255 136 138 133


green : Color
green =
    rgb255 52 164 100


darkGreen : Color
darkGreen =
    rgb255 36 200 33


white : Color
white =
    rgb255 200 200 200

midGray : Color
midGray =  rgb255 80 80 80
