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

void v_VolumeHistogram1D_MovingV()
{
    vec4 kPos = vec4(0.0,0.0,0.0,1.0);
    kPos.x = (gl_MultiTexCoord0.x * ImageSize.x);
    kPos.y = (gl_MultiTexCoord0.y * ImageSize.y);
    kPos.z = (ZSlice * ImageSize.z);

    kPos = InverseTransformMatrix*kPos;
    
    vec3 texCoord = gl_MultiTexCoord0.xyz;
    texCoord.x = kPos.x * ImageSizeInv.x;
    texCoord.y = kPos.y * ImageSizeInv.y;
    texCoord.z = kPos.z * ImageSizeInv.z;

    vec4 vert = gl_Vertex;

    gl_FrontColor = vec4(1.0,0.0,0.0,1.0);
    if ( (texCoord.x < 0.0) || (texCoord.x > 1.0) ||
         (texCoord.y < 0.0) || (texCoord.y > 1.0) ||
         (texCoord.z < 0.0) || (texCoord.z > 1.0)    )
    {
        gl_FrontColor.r = 0.0;
        gl_FrontColor.a = 0.0;
    }
    else
    {
        vec4 vol1 = texture3D(imageA, texCoord );
        vert.x = (vol1.r - 0.5) * 2.0;
        vert.y = 0.0;
    }

    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*vert;
}
//----------------------------------------------------------------------------
