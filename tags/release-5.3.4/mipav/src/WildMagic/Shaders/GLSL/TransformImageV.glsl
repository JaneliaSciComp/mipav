//----------------------------------------------------------------------------
uniform float ZSlice;
uniform vec3 ImageSize;
uniform vec3 ImageSizeInv;

uniform mat4 InverseTransformMatrix;

void v_TransformImageV()
{
    vec3 texCoord = gl_MultiTexCoord0.xyz;
    texCoord.z = ZSlice;

    vec4 kPos = vec4(0.0,0.0,0.0,1.0);
    kPos.xyz = texCoord * ImageSize;
    kPos = InverseTransformMatrix*kPos;
    texCoord = kPos.xyz * ImageSizeInv;


    gl_TexCoord[0].xyz = texCoord.xyz;
    gl_TexCoord[0].w = gl_MultiTexCoord0.w;

    gl_Position = gl_Vertex;
    //gl_TexCoord[0] = gl_MultiTexCoord0;
}
//----------------------------------------------------------------------------
