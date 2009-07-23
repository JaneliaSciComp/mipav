//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
uniform sampler3D imageA; 

uniform float ZSlice;
uniform vec2 Min;
uniform vec2 Scale;

uniform vec3 ImageSize;
uniform vec3 ImageSizeInv;

uniform mat4 InverseTransformMatrix;
uniform vec2 UseTransform;

void v_VolumeHistogram1DV()
{
    gl_FrontColor = vec4(0.0,0.0,0.0,1.0);
    vec3 texCoord = gl_MultiTexCoord0.xyz;
    texCoord.z = ZSlice;

    vec4 vol1 = texture3D(imageA, texCoord );
//     vol1.r = (vol1.r - Min.x) * Scale.x;

    vec4 vert = gl_Vertex;
    vert.x = (vol1.r - 0.5) * 2.0;
    vert.y = 0.0;

    gl_FrontColor.r = 1.0;

    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*vert;
}
//----------------------------------------------------------------------------
