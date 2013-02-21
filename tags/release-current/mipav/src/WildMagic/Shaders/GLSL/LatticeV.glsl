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
void v_LatticeV()
{
    gl_TexCoord[0]  = gl_MultiTexCoord0;
    gl_Position = WVPMatrix * gl_Vertex;
    gl_FrontColor = MaterialDiffuse;
}
