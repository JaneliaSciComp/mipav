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
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
