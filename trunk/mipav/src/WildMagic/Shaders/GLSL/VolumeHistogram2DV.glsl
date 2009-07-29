//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
uniform sampler3D imageA; 
uniform sampler3D imageB; 

uniform float ZSlice;
uniform vec2 Min;
uniform vec2 Scale;
uniform vec2 AB;

uniform vec3 ImageSize;
uniform vec3 ImageSizeInv;

uniform mat4 InverseTransformMatrix;
uniform vec2 UseTransform;

void v_VolumeHistogram2DV()
{
    vec3 texCoord = gl_MultiTexCoord0.xyz;
    texCoord.z = ZSlice;

    vec4 vol1 = texture3D(imageA, texCoord );
//     vol1.r = (vol1.r - Min.x) * Scale.x;

    vec4 vert = gl_Vertex;
    vert.x = (vol1.r - 0.5) * 2.0;

    gl_FrontColor = vec4(1.0,0.0,0.0,1.0);

    vec4 vol2 = vec4(0.0);

    vec4 kPos = vec4(0.0,0.0,0.0,1.0);
    kPos.x = (gl_MultiTexCoord0.x * ImageSize.x);
    kPos.y = (gl_MultiTexCoord0.y * ImageSize.y);
    kPos.z = (ZSlice * ImageSize.z);
    
    kPos = InverseTransformMatrix*kPos;
    
    texCoord.x = kPos.x * ImageSizeInv.x;
    texCoord.y = kPos.y * ImageSizeInv.y;
    texCoord.z = kPos.z * ImageSizeInv.z;
    
//     if ( (texCoord.x < 0.0) || (texCoord.x > 1.0) ||
//          (texCoord.y < 0.0) || (texCoord.y > 1.0)    )
    if ( (texCoord.x < 0.0) || (texCoord.x > 1.0) ||
         (texCoord.y < 0.0) || (texCoord.y > 1.0) ||
         (texCoord.z < 0.0) || (texCoord.z > 1.0)    )
    {
        gl_FrontColor.r = 0.0;
        gl_FrontColor.a = 0.0;
    }
    else
    {
        vol2 = texture3D(imageB, texCoord );
        
        // LinearRescale:
        //             vol2.r = (AB.x * vol2.r) + AB.y;
        //             vol2.r = (vol2.r - Min.x) * Scale.x;
        vert.y = (vol2.r - 0.5) * 2.0;
    }
    
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*vert;
}
//----------------------------------------------------------------------------
