//----------------------------------------------------------------------------
// Texture 0:
//   srcBlend = SBF_ONE
//   dstBlend = DBF_ZERO
//   output   = C0
// Texture 1:
//   srcBlend = SBF_ONE_MINUS_DST_COLOR
//   dstBlend = DBF_ONE
//   output   = (1-C0)*C1 + C0
//----------------------------------------------------------------------------
uniform sampler2D Sampler0;
uniform sampler2D Sampler1;
void p_T0s1d0T1s3d1()
{
    // Sample the texture images and multiply the results.
    vec4 kColor0 = texture2D(Sampler0,gl_TexCoord[0].xy);
    vec4 kColor1 = texture2D(Sampler1,gl_TexCoord[1].xy);
    gl_FragColor = (1.0 - kColor0)*kColor1 + kColor0;
}
//----------------------------------------------------------------------------
