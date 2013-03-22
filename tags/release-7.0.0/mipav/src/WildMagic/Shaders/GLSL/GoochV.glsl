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
//uniform vec3  LightPosition;  // (0.0, 10.0, 4.0) 
in vec3  inPosition;  
in vec3  inNormal;  

out float NdotL;
out vec3  ReflectVec;
out vec3  ViewVec;

void v_GoochV()
{
    vec3 ecPos      = vec3(WVMatrix * vec4(inPosition, 1.0));
    vec3 tnorm      = normalize((WVMatrixIT * vec4(inNormal, 0.0)).xyz);
    vec3 LightPosition   = vec3(0.0, 10.0, 4.0);
    vec3 lightVec   = normalize(LightPosition - ecPos);
    ReflectVec      = normalize(reflect(-lightVec, tnorm));
    ViewVec         = normalize(-ecPos);
    NdotL           = (dot(lightVec, tnorm) + 1.0) * 0.5;
    gl_Position     = WVPMatrix * vec4(inPosition, 1.0);
}
