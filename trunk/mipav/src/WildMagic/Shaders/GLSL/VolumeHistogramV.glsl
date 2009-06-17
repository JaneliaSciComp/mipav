//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
uniform sampler3D bVolumeImageA_TEXUNIT1; 
uniform sampler3D fVolumeImageA_GM_TEXUNIT5; 

uniform float ZSlice;
uniform float UseZSlice;
uniform float InvDataCount;
uniform vec2 Min;
uniform vec2 Max;
uniform vec2 Range;
uniform vec2 Scale;
uniform vec2 AB;

void v_VolumeHistogramV()
{

    vec3 texCoord = gl_MultiTexCoord0.xyz;
    if ( UseZSlice != 0.0 )
    {
        texCoord.z = ZSlice;
    }
    vec4 vol1 = texture3D(bVolumeImageA_TEXUNIT1, texCoord );
    vec4 vol2 = texture3D(fVolumeImageA_GM_TEXUNIT5, texCoord );
    
    // LinearRescale:
    vol2.r = (AB.x * vol2.r) + AB.y;

    vol1.r = (vol1.r - Min.x) * Scale.x;
    vol2.r = (vol2.r - Min.x) * Scale.x;
    
    vec4 vert = gl_Vertex;
    vert.x = (vol1.r * 2.0) - 1.0;
    vert.y = (vol2.r * 2.0) - 1.0;

//     gl_FrontColor.r = 0.00390625;
//     gl_FrontColor.g = 0.00390625;
//     gl_FrontColor.b = 0.00390625;
    gl_FrontColor.r = 1.0;
    gl_FrontColor.g = 1.0;
    gl_FrontColor.b = 1.0;
//     gl_FrontColor.r = InvDataCount;
//     gl_FrontColor.g = InvDataCount;
//     gl_FrontColor.b = InvDataCount;
    gl_FrontColor.a = 1.0;

    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*vert;
}
//----------------------------------------------------------------------------
