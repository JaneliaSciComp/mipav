//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform vec2 Step;
uniform float dLogN;
uniform float nVoxels;
void p_ImageReduce_Entropy_2DP ()
{
    gl_FragColor = vec4(0.0,0.0,0.0,1.0);

    vec2 index0 = gl_TexCoord[0].xy;
    index0.x -= Step.x;
    index0.y -= Step.y;

    vec2 index1 =  gl_TexCoord[0].xy;
    index1.x += Step.x;
    index1.y += Step.y;

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

//     vec4 dataAvg = data0 + data1 + data2 + data3;
//     if ( dataAvg.r > 0.0 )
//     {
//         gl_FragColor.r += (-dataAvg.r * (log(dataAvg.r) - dLogN));
//         gl_FragColor.g += dataAvg.r;
//     }
//     if ( dataAvg.b > 0.0 )
//     {
//         gl_FragColor.b += (-dataAvg.b * (log(dataAvg.b) - dLogN));
//     }



    if ( data0.r > 0.0 )
    {
        gl_FragColor.r += (-data0.r * (log(data0.r) - dLogN));
        gl_FragColor.g += data0.r;
    }
    if ( data1.r > 0.0 )
    {
        gl_FragColor.r += (-data1.r * (log(data1.r) - dLogN));
        gl_FragColor.g += data1.r;
    }
    if ( data2.r > 0.0 )
    {
        gl_FragColor.r += (-data2.r * (log(data2.r) - dLogN));
        gl_FragColor.g += data2.r;
    }
    if ( data3.r > 0.0 )
    {
        gl_FragColor.r += (-data3.r * (log(data3.r) - dLogN));
        gl_FragColor.g += data3.r;
    }


    if ( data0.b > 0.0 )
    {
        gl_FragColor.b += (-data0.b * (log(data0.b) - dLogN));
    }
    if ( data1.b > 0.0 )
    {
        gl_FragColor.b += (-data1.b * (log(data1.b) - dLogN));
    }
    if ( data2.b > 0.0 )
    {
        gl_FragColor.b += (-data2.b * (log(data2.b) - dLogN));
    }
    if ( data3.b > 0.0 )
    {
        gl_FragColor.b += (-data3.b * (log(data3.b) - dLogN));
    }
}
//----------------------------------------------------------------------------
