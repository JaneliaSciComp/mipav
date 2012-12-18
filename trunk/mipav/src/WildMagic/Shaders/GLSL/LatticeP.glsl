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

uniform vec2  Scale;
uniform vec2  Threshold;
in vec4 varColor;
in vec2 varTexCoord0;
out vec4 fragColor;
void p_LatticeP()
{
    float ss = fract(varTexCoord0.s * Scale.s);
    float tt = fract(varTexCoord0.t * Scale.t);

    if ((ss > Threshold.s) && (tt > Threshold.t)) discard;
    fragColor = varColor;
}
