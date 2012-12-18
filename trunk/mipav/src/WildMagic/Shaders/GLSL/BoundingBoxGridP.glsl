//----------------------------------------------------------------------------
uniform vec3 Color1;
uniform vec3 Color2;
uniform vec3 AvgColor;
uniform vec3 volScale;
uniform vec3 origin;
uniform vec3 range;
uniform vec3 plane;
in vec4 position;

in vec2 varTexCoord;
in vec4 varColor;
out vec4 fragColor;
void p_BoundingBoxGridP ()
{
    float marginSmall1 = 0.01;
    float marginSmall1Half = 0.005;
    vec3 marginSmall2 = 0.99 * volScale;
    vec3 marginSmall2Half = 0.995 * volScale;

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


    vec3 FrequencySmall = range / 20.0;
    vec3 FrequencyLarge = range / 100.0;
    
    vec2 fw;
    vec2 fuzz;
    float fuzzMax;
    float border = 0;
    vec3 color;
    
    vec2 checkPos;
    
    if ( (plane.x == -1) || (plane.x == 1) )
    {
        fw = fwidth(varTexCoord.yz);
        if ( (position.z < marginMedium1) || (position.z > marginMedium2.z) )
        {
            fuzz = fw * FrequencySmall.yz * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencySmall  );
            checkPos = vec2(gridPos.y);
        }
        else if ( (position.z < marginWide1) || (position.z > marginWide2.z) )
        {
            fuzz = fw * FrequencyLarge.yz * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencyLarge  );
            checkPos = vec2(gridPos.y);
        }
        if ( (position.y < marginMedium1) || (position.y > marginMedium2.y) )
        {
            fuzz = fw * FrequencySmall.yz * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencySmall  );
            checkPos = vec2(gridPos.z);
        }
        else if ( (position.y < marginWide1) || (position.y > marginWide2.y) )
        {
            fuzz = fw * FrequencyLarge.yz * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencyLarge  );
            checkPos = vec2(gridPos.z);
        }
        if ( position.z < marginSmall1 )
        {
            checkPos = 0.5 + fuzz;
        }
        if ( position.z > marginSmall2.z )
        {
            checkPos = 0.5 + fuzz;
        }
        if ( position.y < marginSmall1 )
        {
            checkPos = 0.5 + fuzz;
        }
        if ( position.y > marginSmall2.y )
        {
            checkPos = 0.5 + fuzz;
        }
    }
    if ( (plane.y == -1) || (plane.y == 1) )
    {
        fw = fwidth(varTexCoord.xz);

        if ( (position.z < marginMedium1) || (position.z > marginMedium2.z) )
        {
            fuzz = fw * FrequencySmall.xz * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencySmall  );
            checkPos = vec2(gridPos.x);
        }
        else if ( (position.z < marginWide1) || (position.z > marginWide2.z) )
        {
            fuzz = fw * FrequencyLarge.xz * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencyLarge  );
            checkPos = vec2(gridPos.x);
        }
        if ( (position.x < marginMedium1) || (position.x > marginMedium2.x) )
        {
            fuzz = fw * FrequencySmall.xz * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencySmall  );
            checkPos = vec2(gridPos.z);
        }
        else if ( (position.x < marginWide1) || (position.x > marginWide2.x) )
        {
            fuzz = fw * FrequencyLarge.xz * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencyLarge  );
            checkPos = vec2(gridPos.z);
        }
        if ( position.z < marginSmall1 )
        {
            checkPos = 0.5 + fuzz;
        }
        if ( position.z > marginSmall2.z ) 
        {
            checkPos = 0.5 + fuzz;
        }
        if ( position.x < marginSmall1 )
        {
            checkPos = 0.5 + fuzz;
        }
        if ( position.x > marginSmall2.x )
        {
            checkPos = 0.5 + fuzz;
        }
    }
    if ( (plane.z == -1) || (plane.z == 1) )
    {
        fw = fwidth(varTexCoord.xy);

        if ( (position.x < marginMedium1) || (position.x > marginMedium2.x) )
        {
            fuzz = fw * FrequencySmall.xy * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencySmall  );
            checkPos = vec2(gridPos.y);
        }
        else if ( (position.x < marginWide1) || (position.x > marginWide2.x) )
        {
            fuzz = fw * FrequencyLarge.xy * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencyLarge  );
            checkPos = vec2(gridPos.y);
        }
        if ( (position.y < marginMedium1) || (position.y > marginMedium2.y) )
        {
            fuzz = fw * FrequencySmall.xy * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencySmall  );
            checkPos = vec2(gridPos.x);
        }
        else if ( (position.y < marginWide1) || (position.y > marginWide2.y) )
        {
            fuzz = fw * FrequencyLarge.xy * 2.0;
            fuzzMax = max(fuzz.s,fuzz.t);
            vec3 gridPos = fract( origin + varTexCoord.xyz * FrequencyLarge  );
            checkPos = vec2(gridPos.x);
        }
        if ( position.x < marginSmall1 )
        {
            checkPos = 0.5 + fuzz;
        }
        if ( position.x > marginSmall2.x ) 
        {
            checkPos = 0.5 + fuzz;
        }
        if ( position.y < marginSmall1 ) 
        {
            checkPos = 0.5 + fuzz;
        }
        if ( position.y > marginSmall2.y )
        {
            checkPos = 0.5 + fuzz;
        }
    }

    if ( fuzzMax < 0.5 )
    {
        vec2 p = smoothstep(vec2(0.5), 2*fuzz + vec2(0.5), checkPos) + 
            (1.0 - smoothstep(vec2(0.0), 2*fuzz, checkPos) );
        color = mix( Color1, Color2, p.x * p.y +
                     (1.0 - p.x) * (1.0 - p.y) );
        color = mix( color, AvgColor, smoothstep( 0.125, 0.5, fuzzMax));
    }
    else
    {
        color = AvgColor;
    }
    fragColor = vec4(color, 1.0);
}
