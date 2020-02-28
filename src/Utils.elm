module Utils exposing (..)

-- SVG requires a line to be expressed as a space separated string of pairs.
stringifyPoint (x, y) = (String.fromFloat x )++ 
    "," ++ 
    (String.fromFloat y) ++ " "

polyLineFromCoords coords = List.foldr (++) "" (List.map stringifyPoint coords)

choose b o1 o2 = if b then o1 else o2

triangleWave t =
    toFloat (abs (abs (modBy 2000 t - 1000) - 1000)) / 1000.0

noise t =
    fractional (5000 * sin (toFloat t))

fractional x =
    x - toFloat (truncate x)

notNearlyEqual x1 x2 =
    (10 * abs x1 < abs x2) || (10 * abs x2 < abs x1)
