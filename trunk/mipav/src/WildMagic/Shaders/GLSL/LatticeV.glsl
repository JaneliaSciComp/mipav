//
// Vertex shader for testing the discard command
//
// Author: OGLSL implementation by Ian Nurse
//
// Copyright (C) 2002-2006  LightWork Design Ltd.
//          www.lightworkdesign.com
//
// See LightworkDesign-License.txt for license information
//

uniform mat4 WVPMatrix;
uniform vec4   MaterialDiffuse;
in vec3 inPosition;
in vec2 inTexcoord0;
out vec4 varColor;
out vec2 varTexCoord0;
void v_LatticeV()
{
    varTexCoord0  = inTexcoord0;
    gl_Position = WVPMatrix * vec4(inPosition, 1.0);
    varColor = MaterialDiffuse;
}
