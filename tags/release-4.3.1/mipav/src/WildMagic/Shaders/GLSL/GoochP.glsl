//
// Vertex shader for Gooch shading
//
// Author: Randi Rost
//
// Copyright (c) 2002-2005 3Dlabs Inc. Ltd. 
//
// See 3Dlabs-License.txt for license information
//
// adapted to WildMagic Java 11/07 Alexandra Bokinsky.

// uniform mat4 WVPMatrix;
// uniform mat4 WVMatrix;
// uniform mat4 WVMatrixIT;
// uniform vec3  LightPosition;  // (0.0, 10.0, 4.0) 

 varying float NdotL;
 varying vec3  ReflectVec;
 varying vec3  ViewVec;

// void v_Gooch()
// {
//     vec3 ecPos      = vec3(WVMatrix * gl_Vertex);
//     vec3 tnorm      = normalize((WVMatrixIT * vec4(gl_Normal, 0.0)).xyz);
//     vec3 lightVec   = normalize(LightPosition - ecPos);
//     ReflectVec      = normalize(reflect(-lightVec, tnorm));
//     ViewVec         = normalize(-ecPos);
//     NdotL           = (dot(lightVec, tnorm) + 1.0) * 0.5;
//     gl_Position     = WVPMatrix * gl_Vertex;
// }

const vec3  SurfaceColor = vec3(0.75, 0.75, 0.75);
const vec3  WarmColor = vec3(0.6, 0.6, 0.0);
const vec3  CoolColor = vec3(0.0, 0.0, 0.6);
const float DiffuseWarm = 0.45;
const float DiffuseCool = 0.45;
// uniform vec3  SurfaceColor; // (0.75, 0.75, 0.75)
// uniform vec3  WarmColor;    // (0.6, 0.6, 0.0)
// uniform vec3  CoolColor;    // (0.0, 0.0, 0.6)
// uniform float DiffuseWarm;  // 0.45
// uniform float DiffuseCool;  // 0.45

void p_GoochP()
{
    vec3 kcool    = min(CoolColor + DiffuseCool * SurfaceColor, 1.0);
    vec3 kwarm    = min(WarmColor + DiffuseWarm * SurfaceColor, 1.0); 
    vec3 kfinal   = mix(kcool, kwarm, NdotL);

    vec3 nreflect = normalize(ReflectVec);
    vec3 nview    = normalize(ViewVec);

    float spec    = max(dot(nreflect, nview), 0.0);
    spec          = pow(spec, 32.0);

    gl_FragColor = vec4(min(kfinal + spec, 1.0), 1.0);
}
