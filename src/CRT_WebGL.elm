module CRT_WebGL exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Messages exposing (Msg)
import Types exposing (Echo, Point)
import Utils exposing (choose)
import WebGL exposing (clearColor)


crt : Float -> List Echo -> Html Msg
crt time echoes =
    WebGL.toHtmlWith
        [ clearColor 0.02 0.02 0.02 0.0
        ]
        [ width 800
        , height 400
        , style "display" "block"
        , style "width" "640px"
        ]
        [ WebGL.entity vertexShader fragmentShader lineMesh (uniforms time echoes)
        ]


type alias Uniforms =
    { rotation : Mat4
    , perspective : Mat4
    , camera : Mat4
    , u_time : Float
    , lineJiggle : Float
    , lineSpikes : Float
    , numRaids : Int
    , raid0 : Vec3
    , raid1 : Vec3
    , raid2 : Vec3
    , raid3 : Vec3
    , raid4 : Vec3
    , raid5 : Vec3
    , raid6 : Vec3
    , raid7 : Vec3
    , raid8 : Vec3
    , raid9 : Vec3
    , raid10 : Vec3
    , raid11 : Vec3
    , raid12 : Vec3
    , raid13 : Vec3
    , raid14 : Vec3
    , raid15 : Vec3
     , raid16 : Vec3
     , raid17 : Vec3
     , raid18 : Vec3
     , raid19 : Vec3
     , raid20 : Vec3
     , raid21 : Vec3
     , raid22 : Vec3
     , raid23 : Vec3
     , raid24 : Vec3
     , raid25 : Vec3
     , raid26 : Vec3
     , raid27 : Vec3
     , raid28 : Vec3
     , raid29 : Vec3
     , raid30 : Vec3
     , raid31 : Vec3
    }


echoToVec : Array Echo -> Int -> Vec3
echoToVec echoes i =
    -- Echoes are massaged into the "raids" uniforms and the WebGL -1..+1 coordinates.
    -- We can use z to allow us to colour the raids differently in tutorial mode.
    case Array.get i echoes of
        Just echo ->
            vec3
                (echo.r / 80000 - 1.0)
                (echo.amplitude / 10.0)
                (choose echo.tutorial 1.0 0.0)

        _ ->
            vec3 -1.0 0.0 0.0


uniforms : Float -> List Echo -> Uniforms
uniforms time echoes =
    let
        echoArray =
            Array.fromList echoes
    in
    -- Apologies this is ch'ugly but the Elm GLSL parser does not accept array, for now.
    { rotation =
        Mat4.mul
            (Mat4.makeRotate (3 * 0.0) (vec3 0 1 0))
            (Mat4.makeRotate (2 * 0.0) (vec3 1 0 0))
    , perspective = Mat4.makePerspective 19 1 0.1 10
    , camera = Mat4.makeLookAt (vec3 0 0 6.5) (vec3 0 -0.5 0) (vec3 0 1 0)
    , u_time = time
    , lineJiggle = 0.03 -- 0.03 is OK.
    , lineSpikes = 0.2 -- 1.0 is OK.
    , numRaids = min 32 <| List.length echoes
    , raid0 = echoToVec echoArray 0
    , raid1 = echoToVec echoArray 1
    , raid2 = echoToVec echoArray 2
    , raid3 = echoToVec echoArray 3
    , raid4 = echoToVec echoArray 4
    , raid5 = echoToVec echoArray 5
    , raid6 = echoToVec echoArray 6
    , raid7 = echoToVec echoArray 7
    , raid8 = echoToVec echoArray 8
    , raid9 = echoToVec echoArray 9
    , raid10 = echoToVec echoArray 10
    , raid11 = echoToVec echoArray 11
    , raid12 = echoToVec echoArray 12
    , raid13 = echoToVec echoArray 13
    , raid14 = echoToVec echoArray 14
    , raid15 = echoToVec echoArray 15
     , raid16 = echoToVec echoArray 16
     , raid17 = echoToVec echoArray 17
     , raid18 = echoToVec echoArray 18
     , raid19 = echoToVec echoArray 19
     , raid20 = echoToVec echoArray 20
     , raid21 = echoToVec echoArray 21
     , raid22 = echoToVec echoArray 22
     , raid23 = echoToVec echoArray 23
     , raid24 = echoToVec echoArray 24
     , raid25 = echoToVec echoArray 25
     , raid26 = echoToVec echoArray 26
     , raid27 = echoToVec echoArray 27
     , raid28 = echoToVec echoArray 28
     , raid29 = echoToVec echoArray 29
     , raid30 = echoToVec echoArray 30
     , raid31 = echoToVec echoArray 31
    }


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


lineMesh : WebGL.Mesh Vertex
lineMesh =
    WebGL.triangles <| fatLineTwo 0.04 <| subDivideTheLine 1000


subDivideTheLine : Int -> List Point
subDivideTheLine n =
    let
        fraction i =
            toFloat i / toFloat n
    in
    List.range 1 n |> List.map (\i -> ( 2 * fraction i - 1, 0.0 ))


fatLineTwo : Float -> List Point -> List ( Vertex, Vertex, Vertex )
fatLineTwo fatness points =
    -- Fatness is amount we spread either side of the y axis.
    -- We make four triangles for each new line segment.
    -- Vertices on axis are green, outliers are black.
    let
        axisVertex x =
            Vertex (vec3 x 0.0 0.0) beamCentreGreen

        belowVertex x =
            Vertex (vec3 x (0.0 - fatness) 0.0) beamEdgeGreen

        aboveVertex x =
            Vertex (vec3 x (0.0 + fatness) 0.0) beamEdgeGreen

        v0 =
            axisVertex -1.0

        v1 =
            belowVertex -1.0

        v2 =
            aboveVertex -1.0

        addSegment ( x, _ ) ( ( prevAxis, prevBelow, prevAbove ), triangles ) =
            let
                vAxis =
                    axisVertex x

                vBelow =
                    belowVertex x

                vAbove =
                    aboveVertex x
            in
            ( ( vAxis, vBelow, vAbove )
            , [ ( prevAxis, prevBelow, vBelow )
              , ( prevAxis, vBelow, vAxis )
              , ( prevAxis, vAxis, vAbove )
              , ( prevAxis, vAbove, prevAbove )
              ]
                ++ triangles
            )

        ( _, segments ) =
            List.foldl addSegment ( ( v0, v1, v2 ), [] ) points
    in
    segments


beamCentreGreen =
    vec3 0 0.7 0


beamEdgeGreen =
    vec3 0 0 0.1


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3, stretch : Float }
vertexShader =
    {-
       Our vertex shader will have special cases built in to give convincing trace patterns
       for our supported formations. We will not support variable numbers of raids, as Elm
       does not support array Uniforms, so we will have a small number of presets that we
       basically turn on and off, just by setting zero amplitude.
       Hence the vertex shader is long and inelegant. It is what it is.
    -}
    [glsl|
        attribute vec3 position;
        attribute vec3 color;

        uniform mat4 perspective;
        uniform mat4 camera;
        uniform mat4 rotation;
        uniform float u_time;
        uniform float lineJiggle;
        uniform float lineSpikes;
        uniform int numRaids; // use this to vary sort load.
        uniform vec3 raid0;
        uniform vec3 raid1;
        uniform vec3 raid2;
        uniform vec3 raid3;
        uniform vec3 raid4;
        uniform vec3 raid5;
        uniform vec3 raid6;
        uniform vec3 raid7;
        uniform vec3 raid8;
        uniform vec3 raid9;
        uniform vec3 raid10;
        uniform vec3 raid11;
        uniform vec3 raid12;
        uniform vec3 raid13;
        uniform vec3 raid14;
        uniform vec3 raid15;
        uniform vec3 raid16;
        uniform vec3 raid17;
        uniform vec3 raid18;
        uniform vec3 raid19;
        uniform vec3 raid20;
        uniform vec3 raid21;
        uniform vec3 raid22;
        uniform vec3 raid23;
        uniform vec3 raid24;
        uniform vec3 raid25;
        uniform vec3 raid26;
        uniform vec3 raid27;
        uniform vec3 raid28;
        uniform vec3 raid29;
        uniform vec3 raid30;
        uniform vec3 raid31;


        varying vec3 vcolor;
        varying float stretch; // how much this line segment is pulled out, this weakens the illumination.

        float random (in float x) {
            return fract(sin(x) * 5000.0);
        }

        //  Function from Iñigo Quiles
        //  www.iquilezles.org/www/articles/functions/functions.htm
        // w = width, c = centre
        float cubicPulse( float c, float w, float x ) {
            x = abs(x - c); // NOTE 0 <= x <= +w
            if (x > w) return 0.0;
            x /= w; // 0 <= x <= +1 (width is actually 2w)
            return 1.0 - x * x * (3.0 - 2.0 * x);
        }

        // x adjustment to keep line thickness across cubic pulse.
        float gradient(float c, float w, float x) {
            float x1 = abs((x - c)/w); // 0 <= x1 <= 1
            if (x1 > 1.0) return 0.0;
            float gradient = x1 * (1.0 - x1);
            return gradient * sign(c - x);
        }

        // Different version of FBM.
        float turbulence( float p ) {

            float w = 100.0;
            float t = -0.5;

            for (float f = 1.0 ; f <= 10.0 ; f++ ){
                float power = pow( 2.0, f );
                t += random(p) / power ;
            }

          return t;

        }

        vec3 pulseShape(vec3 raid, float halfWidth) {
            // returns x = x, y = displacement, z = gradient
            float height = raid.y * cubicPulse(raid.x, halfWidth, position.x);
            float slope = raid.y * gradient(raid.x, halfWidth, position.x);
            float tutorialRaidIndicator = raid.z * height;

           // WARNING -- x value of return used for raid colour indication!!
            return vec3( tutorialRaidIndicator, height, slope );
        }

        // Kludged to try to look OK with multiple raids.
        float coefficient(int i) {
            return 1.0 - 2.0 * mod(float(i),2.0);
        }

        // Kludged to try to look OK with multiple raids.
        float periodicity(int i) {
            if (i == 0) return 0.0;
            if (i == 1) return 1.0/3.0;
            return 2.0 / float(i);
        }

        void main () {
            vec3 newPos = position;
            vec3 newColour = color;
            stretch = 0.0; // The amount by which the rendered segment should be dimmed.

            // Copy raids into array for easier handling, probably.
            vec3 raid[32]; // x = x, y = amplitude, z = 1.0 if tutorial (=> white).
            raid[0] = raid0;
            raid[1] = raid1;
            raid[2] = raid2;
            raid[3] = raid3;
            raid[4] = raid4;
            raid[5] = raid5;
            raid[6] = raid6;
            raid[7] = raid7;
            raid[8] = raid8;
            raid[9] = raid9;
            raid[10] = raid10;
            raid[11] = raid11;
            raid[12] = raid12;
            raid[13] = raid13;
            raid[14] = raid14;
            raid[15] = raid15;
            raid[16] = raid16;
            raid[17] = raid17;
            raid[18] = raid18;
            raid[19] = raid19;
            raid[20] = raid20;
            raid[21] = raid21;
            raid[22] = raid22;
            raid[23] = raid23;
            raid[24] = raid24;
            raid[25] = raid25;
            raid[26] = raid26;
            raid[27] = raid27;
            raid[28] = raid28;
            raid[29] = raid29;
            raid[30] = raid30;
            raid[31] = raid31;


            // Compute height and slope for each raid, at this x position.
            vec3 pulse[32];
            for (int i = 0; i < 16; i++) {
                pulse[i] = pulseShape(raid[i], 0.02);
            }

            // 2020-04-11 New attempt at simpler (= better) signal combination using
            // time-rotating vector on slightly different frequency for each raid.
            // Kind of what I'm emulating but mathematically better and avoiding
            // need to sort signals in decreasing strength.

            float cumulativeX = 0.0; // We will add in cartesian space
            float cumulativeY = 0.0;
            float cumulativeSlopeX = 0.0; // We will add in cartesian space
            float cumulativeSlopeY = 0.0;

            for (int i = 0; i < 32; i++) {
                // The pulse height becomes amplitude of the rotating vector.
                // Signal 'i' is taken to rotate at rate i in our pretend phase space.
                // This is experimental of course.
                // We do the same for the slope but separately.
                if (pulse[i].y > 0.0) { // optimisation!
                    float amplitude = pulse[i].y;
                    float phase = u_time * periodicity(i);
                    cumulativeX += amplitude * cos(phase);
                    cumulativeY += amplitude * sin(phase);
                    cumulativeSlopeX += pulse[i].z * sin(phase); // Derivative therefore swap sin & cos.
                    cumulativeSlopeY += pulse[i].z * cos(phase);

                    // We have used pulse.x to pass through the raid colour identifier!!
                    if (pulse[i].x > 0.0 && position.y == 0.0) {
                        newColour = vec3(1.0, 1.0, 1.0);
                    }
                }
            }

            // Now we reclaim the height and slope by converting back to polar.
            float height = sqrt(cumulativeX * cumulativeX + cumulativeY * cumulativeY);
            float slope = sqrt(cumulativeSlopeX * cumulativeSlopeX + cumulativeSlopeY * cumulativeSlopeY);

          // Additional "spiky" line noise.
          if (lineSpikes > 0.0) {
              if ( random( floor( (3.0 + position.x) * 97.0) * floor(u_time / 3.0)) < 0.01 )
              {
                 vec3 noise = vec3(position.x, 0.1, 0.0);
                 vec3 spike = pulseShape(noise, 0.05);
                 height += lineSpikes * spike.y;
                 //slope += lineSpikes * spike.z;
              }
          }

            newPos.y -= max(0.0, height);
            newPos.x += position.y * slope;
            stretch = abs(slope);

            // Lower the resolution of the x line to make the noise less noisy.
            float newx = floor((position.x) * 153.0);
            float newtime = floor(u_time / 2.0);

            // add time to the noise parameters so it's animated
            if (lineJiggle > 0.0) {
                float noise = turbulence( newx * newtime );
                float b = random( newx * newtime );
                float displacement = lineJiggle * noise + 0.01 * b;
                newPos.y += displacement;
            }

            // Where no signal, narrow the line a bit.
            if (height == 0.0) {
                newPos.y /= 2.0;
            }
            //if (newPos.y > 0.0) { newPos.y /= 2.0; }

            gl_Position = perspective * camera * rotation * vec4(newPos, 1.0);
            vcolor = newColour;
            stretch = abs(stretch);
        }
  |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3, stretch : Float }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        varying float stretch; // how much this line segment is pulled out, this weakens the illumination.

        void main () {
            // Constants are empirical.
            gl_FragColor = vec4(vcolor * (1.0 - 2.5 * stretch), 0.0);
        }
  |]
