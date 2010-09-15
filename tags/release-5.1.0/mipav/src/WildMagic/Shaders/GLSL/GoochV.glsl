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

uniform mat4 WVPMatrix;
uniform mat4 WVMatrix;
uniform mat4 WVMatrixIT;
uniform vec3  LightPosition;  // (0.0, 10.0, 4.0) 

varying float NdotL;
varying vec3  ReflectVec;
varying vec3  ViewVec;

void v_GoochV()
{
    vec3 ecPos      = vec3(WVMatrix * gl_Vertex);
    vec3 tnorm      = normalize((WVMatrixIT * vec4(gl_Normal, 0.0)).xyz);
    vec3 lightVec   = normalize(LightPosition - ecPos);
    ReflectVec      = normalize(reflect(-lightVec, tnorm));
    ViewVec         = normalize(-ecPos);
    NdotL           = (dot(lightVec, tnorm) + 1.0) * 0.5;
    gl_Position     = WVPMatrix * gl_Vertex;
}
