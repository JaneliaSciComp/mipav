//--------------------------------------------------------------------------------------
// Order Independent Transparency with Average Color
//
// Author: Louis Bavoil
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
//--------------------------------------------------------------------------------------

uniform mat4 WVPMatrix;
in vec3 inPosition;
in vec3 inNormal;
out vec3 varTexCoord;
void main(void)
{
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
    float diffuse = abs(normalize(inNormal).z);
    varTexCoord.xyz = vec3(inPosition.xy, diffuse);
}
