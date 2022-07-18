//--------------------------------------------------------------------------------------
// Order Independent Transparency with Average Color
//
// Author: Louis Bavoil
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
//--------------------------------------------------------------------------------------

void main(void)
{
    gl_Position = ftransform();
    float diffuse = abs(normalize(gl_Normal).z);
    gl_TexCoord[0].xyz = vec3(gl_Vertex.xy, diffuse);
}
