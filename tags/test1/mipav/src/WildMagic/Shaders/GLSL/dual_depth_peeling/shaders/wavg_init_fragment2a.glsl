//--------------------------------------------------------------------------------------
// Order Independent Transparency with Average Color
//
// Author: Louis Bavoil
// Email: sdkfeedback@nvidia.com
//
// Copyright (c) NVIDIA Corporation. All rights reserved.
//--------------------------------------------------------------------------------------

uniform float Alpha;

#define COLOR_FREQ 30.0
#define ALPHA_FREQ 30.0

in vec3 varTexCoord;
#if __VERSION__ > 150
layout(location = 0) out vec4     outFragData0;
layout(location = 1) out vec4     outFragData1;
#else
out vec4     outFragData0;
out vec4     outFragData1;
#endif
void main(void)
{
	float xWorldPos = varTexCoord.x;
	float yWorldPos = varTexCoord.y;
	float diffuse = varTexCoord.z;

	vec4 color;
	float i = floor(xWorldPos * COLOR_FREQ);
	float j = floor(yWorldPos * ALPHA_FREQ);
	color.rgb = (mod(i, 2.0) == 0) ? vec3(.4,.85,.0) : vec3(1.0);
	color.a = Alpha;

	color.rgb *= diffuse;

	outFragData0 = vec4(color.rgb * color.a, color.a);
	outFragData1 = vec4(1.0);
}
