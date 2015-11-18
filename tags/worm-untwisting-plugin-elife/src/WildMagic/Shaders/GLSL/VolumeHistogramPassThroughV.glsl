//----------------------------------------------------------------------------
uniform sampler2D imageA; 

void v_VolumeHistogramPassThroughV()
{
    vec2 texCoord = gl_Vertex.xy;
    texCoord += 1.0;
    texCoord /= 2.0;
    gl_TexCoord[0].z = 0.0;
    gl_TexCoord[0].w = 0.0;
    gl_TexCoord[0].xy = texCoord.xy;

    //vec4 color = texture2D(imageA, gl_MultiTexCoord0.xy );
//     vec4 color = texture2D(imageA, texCoord );
//    gl_FrontColor = vec4(0.0,0.0,0.0,1.0);
//     gl_FrontColor.r = color.r;
//    gl_FrontColor = texture2D(imageA, texCoord );
    //gl_FrontColor.rg = texCoord.xy;
    gl_Position = gl_Vertex;
}
//----------------------------------------------------------------------------
