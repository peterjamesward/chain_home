module CRT_WebGL exposing (..)

import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Messages exposing (Msg)
import Types exposing (Echo, Point)
import WebGL exposing (Mesh, Shader, clearColor)


crt : Bool -> Float -> List Echo -> Html Msg
crt fullScreen time echoes =
    let
        styles =
            if fullScreen then
                [ width 1800
                , height 800
                , style "display" "block"
                , style "width" "1100px"
                ]

            else
                [ width 1800
                , height 800
                , style "display" "block"
                , style "width" "640px"
                ]
    in
    WebGL.toHtmlWith
        [ clearColor 0.02 0.02 0.02 0.0 ]
        styles
        [ WebGL.entity
            vertexShader
            fragmentShader
            mesh
            (uniforms time echoes)
        ]



-- Leave a border around the CRT line, to accommodate the range scale


xLimit =
    1.0


yLimit =
    1.0


mesh : Mesh { position : Vec3 }
mesh =
    -- The mesh corners adjusted empirically to align with range scale.
    WebGL.triangles
        [ ( { position = vec3 -1 1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 -1 -1 0 }
          )
        , ( { position = vec3 -1 -1 0 }
          , { position = vec3 1 1 0 }
          , { position = vec3 1 -1 0 }
          )
        ]


type alias Uniforms =
    { iResolution : Vec3
    , iTime : Float
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
    }


normaliseEcho : Echo -> Vec3
normaliseEcho echo =
    vec3
        -- distance in metres, map to [0,1]
        (echo.r / 160000)
        -- empirically seems to be [0, 10], map to [0,1]
        (logBase 10 (1 + echo.amplitude) / 10.0)
        -- number of craft.
        (toFloat echo.strength)


echoToVec : Int -> Array Vec3 -> Vec3
echoToVec i echoes =
    -- With new graphics, keep this in world space, let the GPU sort it out.
    Array.get i echoes
        |> Maybe.withDefault
            (vec3 0.0 0.0 0.0)


uniforms : Float -> List Echo -> Uniforms
uniforms time echoes =
    let
        echoArray =
            Array.fromList <| List.map normaliseEcho echoes

        _ =
            Debug.log "Amplitudes" echoArray
    in
    -- Apologies this is chugly but the Elm GLSL parser does not accept array, for now.
    { iResolution = vec3 1600 800 0
    , iTime = time / 10
    , numRaids = min 16 <| List.length echoes
    , raid0 = echoToVec 0 echoArray
    , raid1 = echoToVec 1 echoArray
    , raid2 = echoToVec 2 echoArray
    , raid3 = echoToVec 3 echoArray
    , raid4 = echoToVec 4 echoArray
    , raid5 = echoToVec 5 echoArray
    , raid6 = echoToVec 6 echoArray
    , raid7 = echoToVec 7 echoArray
    , raid8 = echoToVec 8 echoArray
    , raid9 = echoToVec 9 echoArray
    , raid10 = echoToVec 10 echoArray
    , raid11 = echoToVec 11 echoArray
    , raid12 = echoToVec 12 echoArray
    , raid13 = echoToVec 13 echoArray
    , raid14 = echoToVec 14 echoArray
    , raid15 = echoToVec 15 echoArray
    }


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


vertexShader : Shader { position : Vec3 } Uniforms { vFragCoord : Vec2 }
vertexShader =
    [glsl|

        precision mediump float;
        attribute vec3 position;
        varying vec2 vFragCoord;
        uniform vec3 iResolution;
        void main () {
            gl_Position = vec4(position, 1.0);
            vFragCoord = (position.xy + 1.0) / 2.0 * iResolution.xy;
        }

  |]


fragmentShader : WebGL.Shader {} Uniforms { vFragCoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec2 vFragCoord;
        uniform vec3 iResolution;
        uniform float iTime;
        uniform int numRaids;
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

        vec3 COL2 = vec3(0.0,1.0,0.2);

        float random(in vec3 pos) {
            return fract(sin(dot(pos.xyz, vec3(70.9898, 78.233, 32.4355))) * 43758.5453123);
        }

        float raidContribution(vec3 raid, vec2 xy) {
            // x and y in [0,1],
            // raid.x is range in [0,1].
            // raid.y is calculated amplitude nominally in [0,1] ??

            // If x = range, return amplitude.
            // As x moves away from range, use smoothstep.
            float xDist = abs ( xy.x - raid.x );
            float amp = raid.y;

            // Increase factor here to get a narrower pulse!
            float xDistForCube = clamp(0.0, 1.0, xDist * 20.0);
            float smoothed = smoothstep(1.0, 0.0, xDistForCube);

            return amp * smoothed ;
        }

        // NOTE: Important to separate the notions of:
        // 1, The deflection of the beam caused by echoes.
        // 2, The brightness of a pixel caused by distance from beam.

        float deriveSignalFromFieldsAt(vec2 xy) {
            // Each raid has a TBD signature that contributes to deflection.
            float deflection = 0.01;
            deflection += raidContribution(raid0, xy);
            deflection += raidContribution(raid1, xy);
            deflection += raidContribution(raid2, xy);
            deflection += raidContribution(raid3, xy);
            deflection += raidContribution(raid4, xy);
            deflection += raidContribution(raid5, xy);
            deflection += raidContribution(raid6, xy);
            deflection += raidContribution(raid7, xy);
            deflection += raidContribution(raid8, xy);
            deflection += raidContribution(raid9, xy);
            deflection += raidContribution(raid10, xy);
            deflection += raidContribution(raid11, xy);
            deflection += raidContribution(raid12, xy);
            deflection += raidContribution(raid13, xy);
            deflection += raidContribution(raid14, xy);
            deflection += raidContribution(raid15, xy);
            deflection = clamp(1.0 - deflection, 0.0, 0.95);

            // Intensity decays rapidly away from derived deflection.
            // Increase factor to focus the beam
            float beamDistance = clamp(0.0,1.0,abs(deflection - xy.y) * 80.0);
            float intensity = smoothstep( 1.0, 0.0, beamDistance );

            // Add some simple random noise, strong near the beam.
            //float d2 = random(vec2(xy * 5. + iTime));
            float d3 = random(vec3(xy, iTime));
            float randomSpread = clamp(0.0,d3,pow(1.0 - abs(deflection - xy.y),16.0));

            return  intensity + randomSpread;
        }

        void mainImage( out vec4 fragColor, in vec2 fragCoord )
        {
            // uv should be [0,1] 0 being bottom left BTW.
        //    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.xy;
            vec2 uv = fragCoord.xy/iResolution.xy;

            float strength = deriveSignalFromFieldsAt(uv);

            fragColor = vec4(COL2 * strength,1.0);
        }

        void main() {
          mainImage(gl_FragColor, vFragCoord);
        }

  |]
