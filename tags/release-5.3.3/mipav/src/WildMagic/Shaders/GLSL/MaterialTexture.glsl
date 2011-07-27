//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
uniform vec4 MaterialDiffuse;
void v_MaterialTexture()
   
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;

    // Pass through the material diffuse color.
    gl_FrontColor = MaterialDiffuse;

    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
void p_MaterialTexture()
{
    // Add the material and texture colors.
    vec4 kBaseColor = texture2D(BaseSampler,gl_TexCoord[0]);
    gl_FragColor.rgb = clamp(kBaseColor.rgb + gl_Color.rgb, 0.0, 1.0);
    
    // Multiply the material and texture alphas.
    gl_FragColor.a = gl_Color.a*gl_Color.a;
}
//----------------------------------------------------------------------------
