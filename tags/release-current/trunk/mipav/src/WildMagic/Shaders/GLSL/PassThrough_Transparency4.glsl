//----------------------------------------------------------------------------
in vec4 varColor;
#if __VERSION__ > 150
layout(location = 0) out vec4     outFragData0;
layout(location = 1) out vec4     outFragData1;
#else
out vec4     outFragData0;
out vec4     outFragData1;
#endif
void p_PassThrough_Transparency4 ()
{
    outFragData0 = vec4(varColor.rgb * varColor.a, varColor.a);
    outFragData1 = vec4(1.0);
}
//----------------------------------------------------------------------------
