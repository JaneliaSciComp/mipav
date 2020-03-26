//--------------------------------------------------------------------------------------
// Order Independent Transparency with Average Color
//
// Author: Louis Bavoil
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
//--------------------------------------------------------------------------------------

#extension ARB_draw_buffers : require

uniform float Alpha;

#define COLOR_FREQ 30.0
#define ALPHA_FREQ 30.0

void main(void)
{
	float xWorldPos = gl_TexCoord[0].x;
	float yWorldPos = gl_TexCoord[0].y;
	float diffuse = gl_TexCoord[0].z;

	vec4 color;
	float i = floor(xWorldPos * COLOR_FREQ);
	float j = floor(yWorldPos * ALPHA_FREQ);
	color.rgb = (fmod(i, 2.0) == 0) ? vec3(.4,.85,.0) : vec3(1.0);
	//color.a = (fmod(j, 2.0) == 0) ? Alpha : 0.2;
	color.a = Alpha;

	//color.rgb *= diffuse;

	gl_FragColor = vec4(color.rgb * color.a, color.a);
}
