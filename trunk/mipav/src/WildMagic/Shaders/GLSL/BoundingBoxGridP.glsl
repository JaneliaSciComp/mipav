//----------------------------------------------------------------------------
uniform vec3 volScale;
uniform vec3 origin;
uniform vec3 range;
uniform vec3 plane;
varying vec4 position;
void p_BoundingBoxGridP ()
{
    float marginSmall1 = 0.01;
    vec3 marginSmall2 = 0.99 * volScale;

    float marginWide1 = 0.05;
    vec3 marginWide2 = 0.95 * volScale;
    if ( (plane.x == -1) || (plane.x == 1) )
    {
        if ( (position.z > marginWide1) && (position.z < marginWide2.z) &&
             (position.y > marginWide1) && (position.y < marginWide2.y)
             )
        {
            discard;
        }
    }
    if ( (plane.y == -1) || (plane.y == 1) )
    {
        if ( (position.z > marginWide1) && (position.z < marginWide2.z) &&
             (position.x > marginWide1) && (position.x < marginWide2.x)
             )
        {
            discard;
        }
    }
    if ( (plane.z == -1) || (plane.z == 1) )
    {
        if ( (position.x > marginWide1) && (position.x < marginWide2.x) &&
             (position.y > marginWide1) && (position.y < marginWide2.y)
             )
        {
            discard;
        }
    }

    
    vec3 Color1 = vec3(1,0,0);
    vec3 Color2 = vec3(0,0,0);
    vec3 AvgColor = vec3(0.5,0,0);

    vec3 Frequency = range / 20.0;

    vec2 fw;
    vec2 fuzz;
    float fuzzMax;

    vec3 color;


    vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * Frequency  );
    vec2 checkPos;

    if ( (plane.x == -1) || (plane.x == 1) )
    {
        fw = fwidth(gl_TexCoord[0].yz);
        fuzz = fw * Frequency.yz * 2.0;
        fuzzMax = max(fuzz.s,fuzz.t);
        if ( (position.z < marginWide1) || (position.z > marginWide2.z) )
        {
            checkPos = vec2(gridPos.y);
        }
        if ( (position.y < marginWide1) || (position.y > marginWide2.y) )
        {
            checkPos = vec2(gridPos.z);
        }
        if ( (position.z < marginSmall1) || (position.z > marginSmall2.z) )
        {
            fuzzMax = 1;
        }
        if ( (position.y < marginSmall1) || (position.y > marginSmall2.y) )
        {
            fuzzMax = 1;
        }
    }
    if ( (plane.y == -1) || (plane.y == 1) )
    {
        fw = fwidth(gl_TexCoord[0].xz);
        fuzz = fw * Frequency.xz * 2.0;
        fuzzMax = max(fuzz.s,fuzz.t);
        if ( (position.z < marginWide1) || (position.z > marginWide2.z) )
        {
            checkPos = vec2(gridPos.x);
        }
        if ( (position.x < marginWide1) || (position.x > marginWide2.x) )
        {
            checkPos = vec2(gridPos.z);
        }
        if ( (position.z < marginSmall1) || (position.z > marginSmall2.z) )
        {
            fuzzMax = 1;
        }
        if ( (position.x < marginSmall1) || (position.x > marginSmall2.x) )
        {
            fuzzMax = 1;
        }
    }
    if ( (plane.z == -1) || (plane.z == 1) )
    {
        fw = fwidth(gl_TexCoord[0].xy);
        fuzz = fw * Frequency.xy * 2.0;
        fuzzMax = max(fuzz.s,fuzz.t);
        if ( (position.x < marginWide1) || (position.x > marginWide2.x) )
        {
            checkPos = vec2(gridPos.y);
        }
        if ( (position.y < marginWide1) || (position.y > marginWide2.y) )
        {
            checkPos = vec2(gridPos.x);
        }
        if ( (position.x < marginSmall1) || (position.x > marginSmall2.x) )
        {
            fuzzMax = 1;
        }
        if ( (position.y < marginSmall1) || (position.y > marginSmall2.y) )
        {
            fuzzMax = 1;
        }
    }

    if ( fuzzMax < 0.5 )
    {
        vec2 p = smoothstep(vec2(0.5), fuzz + vec2(0.5), checkPos) + 
            (1.0 - smoothstep(vec2(0.0), fuzz, checkPos) );
        color = mix( Color1, Color2, p.x * p.y +
                     (1.0 - p.x) * (1.0 - p.y) );
        color = mix( color, AvgColor, smoothstep( 0.125, 0.5, fuzzMax));
    }
    else
    {
        color = AvgColor;
    }
    gl_FragColor = vec4(color, 1.0);

}
