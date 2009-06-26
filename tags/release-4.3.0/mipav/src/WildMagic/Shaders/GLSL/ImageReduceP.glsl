//----------------------------------------------------------------------------
uniform sampler2D BaseSampler;
uniform vec2 Step;
void p_ImageReduceP ()
{
    gl_FragColor.r = 0.0;
    gl_FragColor.g = 0.0;
    gl_FragColor.b = 0.0;
    gl_FragColor.a = 1.0;
    // Sample the texture image.
    vec2 index0 = gl_TexCoord[0].xy;
    index0.x = ((index0.x - 0.5)*2.0)+0.5;
    index0.y = ((index0.y - 0.5)*2.0)+0.5;
    vec2 index1 = index0.xy;  index1.x += Step.x;
    vec2 index2 = index0.xy;  index2.y += Step.y;
    vec2 index3 = index0.xy;  index3.x += Step.x;  index3.y += Step.y;

    vec4 data0 = texture2D(BaseSampler, index0);
    vec4 data1 = texture2D(BaseSampler, index1);
    vec4 data2 = texture2D(BaseSampler, index2);
    vec4 data3 = texture2D(BaseSampler, index3);
    gl_FragColor.r = max( data0.r, data1.r );
    gl_FragColor.r = max( gl_FragColor.r, data2.r );
    gl_FragColor.r = max( gl_FragColor.r, data3.r );
    gl_FragColor.g = gl_FragColor.r;
    gl_FragColor.b = gl_FragColor.r;
}
//----------------------------------------------------------------------------
