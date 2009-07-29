//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform vec2 Step;
void p_ImageReduce_Sum_2D_to_1DA_y ()
{
    gl_FragColor = vec4(0.0);

    vec2 index0 = gl_TexCoord[0].xy;
    index0.y -= Step.y;

    vec2 index1 =  gl_TexCoord[0].xy;
    index1.y += Step.y;

    vec4 data0 = texture2D(BaseSampler, index0);
    vec4 data1 = texture2D(BaseSampler, index1);

    gl_FragColor.r = data0.r + data1.r;
}
//----------------------------------------------------------------------------
