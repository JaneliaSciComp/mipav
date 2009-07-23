//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform vec2 Step;
uniform float dLogN;
void p_ImageReduce_Sum_2DP ()
{
    gl_FragColor = vec4(0.0,0.0,0.0,1.0);

    vec2 index0 = gl_TexCoord[0].xy;
    index0.x -= Step.x;
    index0.y -= Step.y;
//     index0.y = 0.0;
//     vec4 data0X = texture2D(BaseSampler, index0);
//     index0.y = gl_TexCoord[0].y - Step.y;


    vec2 index1 =  gl_TexCoord[0].xy;
    index1.x += Step.x;
    index1.y += Step.y;
//     index1.y = 0.0;
//     vec4 data1X = texture2D(BaseSampler, index1);
//     index1.y = gl_TexCoord[0].y + Step.y;

    vec2 index2 = gl_TexCoord[0].xy;
    index2.x -= Step.x;
    index2.y += Step.y;

    vec2 index3 = gl_TexCoord[0].xy;
    index3.x += Step.x;
    index3.y -= Step.y;

    vec4 data0 = texture2D(BaseSampler, index0);
    vec4 data1 = texture2D(BaseSampler, index1);
    vec4 data2 = texture2D(BaseSampler, index2);
    vec4 data3 = texture2D(BaseSampler, index3);

//     gl_FragColor.r = data0X.r + data1X.r;
//     gl_FragColor.g = data0X.g + data1X.g;
    gl_FragColor = data0 + data1 + data2 + data3;
}
//----------------------------------------------------------------------------
