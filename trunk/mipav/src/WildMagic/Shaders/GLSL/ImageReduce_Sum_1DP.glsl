//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform vec2 Step;
uniform float dLogN;
void p_ImageReduce_Sum_1DP ()
{
    gl_FragColor = vec4(0.0);
    // Sample the texture image.
    vec2 index0 = gl_TexCoord[0].xy;
    index0.x -= Step.x;
    index0.y = 0.0;

    vec2 index1 =  gl_TexCoord[0].xy;
    index1.x += Step.x;
    index1.y = 0.0;

    vec4 data0 = texture2D(BaseSampler, index0);
    vec4 data1 = texture2D(BaseSampler, index1);
    gl_FragColor.r = data0.r + data1.r;
}
//----------------------------------------------------------------------------
