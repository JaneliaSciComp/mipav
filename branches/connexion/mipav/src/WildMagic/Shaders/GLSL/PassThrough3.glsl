in vec4 varColor;
out vec4     fragColor;
void p_PassThrough3 ()
{
    fragColor.xyz = varColor.xyz;
    fragColor.w = 1.0;
}
