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
void p_LatticeP()
{
    float ss = fract(gl_TexCoord[0].s * Scale.s);
    float tt = fract(gl_TexCoord[0].t * Scale.t);

    if ((ss > Threshold.s) && (tt > Threshold.t)) discard;
    gl_FragColor = gl_Color;
}
