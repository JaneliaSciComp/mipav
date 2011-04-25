//--------------------------------------------------------------------------------------
// Order Independent Transparency with Average Color
//
// Author: Louis Bavoil
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
//--------------------------------------------------------------------------------------

uniform mat4 WVPMatrix;
void main(void)
{
    gl_Position = WVPMatrix * gl_Vertex;
    //float diffuse = abs(normalize(gl_NormalMatrix * gl_Normal).z);
    //float diffuse = abs(normalize(WVMatrix * vec4(gl_Normal,0)).z);
    float diffuse = abs(normalize(gl_Normal).z);
    gl_TexCoord[0].xyz = vec3(gl_Vertex.xy, diffuse);
}
