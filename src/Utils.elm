module Utils exposing (..)

-- SVG requires a line to be expressed as a space separated string of pairs.
stringifyPoint (x, y) = (String.fromFloat x )++ 
    "," ++ 
    (String.fromFloat y) ++ " "

polyLineFromCoords coords = List.foldr (++) "" (List.map stringifyPoint coords)

