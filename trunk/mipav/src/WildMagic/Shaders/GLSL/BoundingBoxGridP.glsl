//----------------------------------------------------------------------------
uniform float METHOD;
uniform vec3 Color1;
uniform vec3 Color2;
uniform vec3 AvgColor;
uniform vec3 volScale;
uniform vec3 origin;
uniform vec3 range;
uniform vec3 plane;
varying vec4 position;
void p_BoundingBoxGridP ()
{
    float marginSmall1 = 0.01;
    vec3 marginSmall2 = 0.99 * volScale;

    float marginMedium1 = 0.025;
    vec3 marginMedium2 = 0.975 * volScale;

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


    if ( METHOD <= .10 )
    {
        vec3 FrequencySmall = range / 10.0;
        vec3 FrequencyLarge = range / 50.0;

        vec3 color;

        float sawtooth;
        float triangle;
        float dp;
        float edge;
        float square;
        float smoothE = 0.85;

        if ( (plane.x == -1) || (plane.x == 1) )
        {
            dp = length( vec2( dFdx(gl_TexCoord[0].y), dFdy(gl_TexCoord[0].z) ) );
            if ( (position.z < marginMedium1) || (position.z > marginMedium2.z) )
            {
                sawtooth = fract( gl_TexCoord[0].y * FrequencySmall.y );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencySmall.y * 2;
            }
            else if ( (position.z < marginWide1) || (position.z > marginWide2.z) )
            {
                sawtooth = fract( gl_TexCoord[0].y * FrequencyLarge.y );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencyLarge.y * 2;
                smoothE = 1.0 - 0.15/5.0;
            }
            if ( (position.y < marginMedium1) || (position.y > marginMedium2.y) )
            {
                sawtooth = fract( gl_TexCoord[0].z * FrequencySmall.z );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencySmall.z * 2;
            }
            else if ( (position.y < marginWide1) || (position.y > marginWide2.y) )
            {
                sawtooth = fract( gl_TexCoord[0].z * FrequencyLarge.z );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencyLarge.z * 2;
                smoothE = 1.0 - 0.15/5.0;
            }
            if ( (position.z < marginSmall1) || (position.z > marginSmall2.z) )
            {
                gl_FragColor = vec4( AvgColor, 1.0);
                return;
            }
            if ( (position.y < marginSmall1) || (position.y > marginSmall2.y) )
            {
                gl_FragColor = vec4( AvgColor, 1.0);
                return;
            }
        }
        if ( (plane.y == -1) || (plane.y == 1) )
        {
            dp = length( vec2( dFdx(gl_TexCoord[0].x), dFdy(gl_TexCoord[0].z) ) );
            if ( (position.z < marginMedium1) || (position.z > marginMedium2.z) )
            {
                sawtooth = fract( gl_TexCoord[0].x * FrequencySmall.x );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencySmall.x * 2;
            }
            else if ( (position.z < marginWide1) || (position.z > marginWide2.z) )
            {
                sawtooth = fract( gl_TexCoord[0].x * FrequencyLarge.x );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencyLarge.x * 2;
                smoothE = 1.0 - 0.15/5.0;
            }
            if ( (position.x < marginMedium1) || (position.x > marginMedium2.x) )
            {
                sawtooth = fract( gl_TexCoord[0].z * FrequencySmall.z );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencySmall.z * 2;
            }
            else if ( (position.x < marginWide1) || (position.x > marginWide2.x) )
            {
                sawtooth = fract( gl_TexCoord[0].z * FrequencyLarge.z );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencyLarge.z * 2;
                smoothE = 1.0 - 0.15/5.0;
            }
            if ( (position.z < marginSmall1) || (position.z > marginSmall2.z) )
            {
                gl_FragColor = vec4( AvgColor, 1.0);
                return;
            }
            if ( (position.x < marginSmall1) || (position.x > marginSmall2.x) )
            {
                gl_FragColor = vec4( AvgColor, 1.0);
                return;
            }
        }
        if ( (plane.z == -1) || (plane.z == 1) )
        {
            dp = length( vec2( dFdx(gl_TexCoord[0].x), dFdy(gl_TexCoord[0].y) ) );
            if ( (position.x < marginMedium1) || (position.x > marginMedium2.x) )
            {
                sawtooth = fract( gl_TexCoord[0].y * FrequencySmall.y );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencySmall.y * 2;
            }
            else if ( (position.x < marginWide1) || (position.x > marginWide2.x) )
            {
                sawtooth = fract( gl_TexCoord[0].y * FrequencyLarge.y );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencyLarge.y * 2;
                smoothE = 1.0 - 0.15/5.0;
            }
            if ( (position.y < marginMedium1) || (position.y > marginMedium2.y) )
            {
                sawtooth = fract( gl_TexCoord[0].x * FrequencySmall.x );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencySmall.x * 2;
            }
            else if ( (position.y < marginWide1) || (position.y > marginWide2.y) )
            {
                sawtooth = fract( gl_TexCoord[0].x * FrequencyLarge.x );
                triangle = abs( 2.0 * sawtooth - 1.0);
                edge = dp * FrequencyLarge.y * 2;
                smoothE = 1.0 - 0.15/5.0;
            }
            if ( (position.x < marginSmall1) || (position.x > marginSmall2.x) )
            {
                gl_FragColor = vec4( AvgColor, 1.0);
                return;
            }
            if ( (position.y < marginSmall1) || (position.y > marginSmall2.y) )
            {
                gl_FragColor = vec4( AvgColor, 1.0);
                return;
            }
        }

        color = mix( Color2, Color1,
                     0.85 * smoothstep(smoothE - edge, smoothE + edge, triangle ) );
        gl_FragColor = vec4( color, 1.0);
    }


    else
    {
        vec3 FrequencySmall = range / 20.0;
        vec3 FrequencyLarge = range / 100.0;

        vec2 fw;
        vec2 fuzz;
        float fuzzMax;

        vec3 color;


        vec2 checkPos;

        if ( (plane.x == -1) || (plane.x == 1) )
        {
            fw = fwidth(gl_TexCoord[0].yz);
            if ( (position.z < marginMedium1) || (position.z > marginMedium2.z) )
            {
                fuzz = fw * FrequencySmall.yz * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencySmall  );
                checkPos = vec2(gridPos.y);
            }
            else if ( (position.z < marginWide1) || (position.z > marginWide2.z) )
            {
                fuzz = fw * FrequencyLarge.yz * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencyLarge  );
                checkPos = vec2(gridPos.y);
            }
            if ( (position.y < marginMedium1) || (position.y > marginMedium2.y) )
            {
                fuzz = fw * FrequencySmall.yz * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencySmall  );
                checkPos = vec2(gridPos.z);
            }
            else if ( (position.y < marginWide1) || (position.y > marginWide2.y) )
            {
                fuzz = fw * FrequencyLarge.yz * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencyLarge  );
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
            if ( (position.z < marginMedium1) || (position.z > marginMedium2.z) )
            {
                fuzz = fw * FrequencySmall.xz * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencySmall  );
                checkPos = vec2(gridPos.x);
            }
            else if ( (position.z < marginWide1) || (position.z > marginWide2.z) )
            {
                fuzz = fw * FrequencyLarge.xz * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencyLarge  );
                checkPos = vec2(gridPos.x);
            }
            if ( (position.x < marginMedium1) || (position.x > marginMedium2.x) )
            {
                fuzz = fw * FrequencySmall.xz * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencySmall  );
                checkPos = vec2(gridPos.z);
            }
            else if ( (position.x < marginWide1) || (position.x > marginWide2.x) )
            {
                fuzz = fw * FrequencyLarge.xz * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencyLarge  );
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
            if ( (position.x < marginMedium1) || (position.x > marginMedium2.x) )
            {
                fuzz = fw * FrequencySmall.xy * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencySmall  );
                checkPos = vec2(gridPos.y);
            }
            else if ( (position.x < marginWide1) || (position.x > marginWide2.x) )
            {
                fuzz = fw * FrequencyLarge.xy * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencyLarge  );
                checkPos = vec2(gridPos.y);
            }
            if ( (position.y < marginMedium1) || (position.y > marginMedium2.y) )
            {
                fuzz = fw * FrequencySmall.xy * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencySmall  );
                checkPos = vec2(gridPos.x);
            }
            else if ( (position.y < marginWide1) || (position.y > marginWide2.y) )
            {
                fuzz = fw * FrequencyLarge.xy * 2.0;
                fuzzMax = max(fuzz.s,fuzz.t);
                vec3 gridPos = fract( origin + gl_TexCoord[0].xyz * FrequencyLarge  );
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
}
