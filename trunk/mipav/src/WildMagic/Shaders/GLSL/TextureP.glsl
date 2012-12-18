//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
in vec2 varTexCoord;
out vec4 fragColor;
void p_TextureP ()
{
    // Sample the texture image.
    fragColor = texture(BaseSampler,varTexCoord, 0.0);
}
//----------------------------------------------------------------------------
