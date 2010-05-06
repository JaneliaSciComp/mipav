//----------------------------------------------------------------------------
uniform sampler3D BaseSampler;
void p_TransformImageP ()
{
    gl_FragColor = texture3D(BaseSampler,gl_TexCoord[0].xyz);
}
//----------------------------------------------------------------------------
