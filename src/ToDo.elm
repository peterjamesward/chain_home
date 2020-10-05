
--TODO: Show raid size on map, maybe by size of circle, maybe small raid plaque?

--TODO: Second station location

--TODO: Collect raid details from server

--TODO: Filter table

--DROP: Our shader does a lot of [sin|cos] x.2^n
-- and sin 2x == 2 . sin x . cos x, so there's probably a in sin x . cos x that would be 'cheaper'/
-- I know this is unnecessary optimisation, but might matter on some devices.
-- Also cos 2x == cos^2 x - sin^2 x.
-- More generally, from Wolfram Alpha:
-- sin (2^n)x == 1/2 i e^(-i 2^n) x - 1/2 i e^(i 2^n) x
-- cos (2^n)x == 1/2 e^(-i 2^n) x + 1/2 e^(i 2^n) x
-- So I could probably simplify the whole big raid field using Alpha.
-- Last part is false, because of time component, but may still have some legs.
-- Particularly if WebGL has a complex type, which I doubt, but we'll see... (it does not).
-- Even so, the time element is simply e^(ni). Oh, this is not worth it.