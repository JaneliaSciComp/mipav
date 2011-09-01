//----------------------------------------------------------------------------
float computeX( float fY,
                float fInvY0MY1,
                vec4 LevLine )
{
    float x0 = LevLine.x;
    float y0 = LevLine.y;
    float x1 = LevLine.z;
    float y1 = LevLine.w;
    float t = (y0 - fY) * fInvY0MY1;
    float x = x0 - x0 * t + x1 * t;
    return x;
}
//----------------------------------------------------------------------------
float computeAlpha( float fX,
                    float fY,
                    vec2  fShift,
                    vec3  fInvY0MY1,
                    vec4  LevMidLine,
                    vec4  LevLeftLine,
                    vec4  LevRightLine )
{
    if ( (fY < LevLeftLine.y) || fY > LevLeftLine.w )
    {
        return 0.0;
    }
    float xMid = LevMidLine.x;
    float fShiftL = fShift.x;
    float fShiftR = fShift.y;
    if ( LevMidLine.y != LevMidLine.w )
    {
        xMid = computeX( fY, fInvY0MY1.y, LevMidLine );
    }
    float xLeft = computeX( fY, fInvY0MY1.x, LevLeftLine );
    float xRight = computeX( fY, fInvY0MY1.z, LevRightLine );
    
    float fAlpha = 0.0;
    if ( (fX > (xMid - fShiftL)) && (fX < (xMid + fShiftR)) )
    {
        fAlpha = 1.0;
    }
    if ( (fX <= (xMid-fShiftL)) && (fX >= xLeft) )
    {
        fAlpha = (fX - xLeft) / ((xMid-fShiftL) - xLeft);
    }
    if ( (fX >= (xMid+fShiftR)) && (fX <= xRight) )
    {
        fAlpha = (fX - xRight) / ((xMid+fShiftR) - xRight);
    }
    return fAlpha;
}
float computeAlphaCircle( float fX,
                    float fY,
                    vec4  Center,
                    vec4  MidLine,
                    float fRadius )
{
    
    float diffX = fX - Center.x;
    float diffY = fY - Center.y;
    float length = sqrt(diffX * diffX + diffY * diffY );
    if (  length >= fRadius )
    {
        return 0.0;
    }

    vec2 direction;
    direction.x = fX - MidLine.x;
    direction.y = fY - MidLine.y; 
    float lengthShade = sqrt(direction.x*direction.x + direction.y*direction.y);
    direction.x /= lengthShade;
    direction.y /= lengthShade;
    float fAlpha;
    if ( (MidLine.x == Center.x) && (MidLine.y == Center.y) )
    {
        fAlpha = max( 0.0, 1.0 - (lengthShade / fRadius) );
        return fAlpha;
    }
    
    vec2 diff;
    diff.x = MidLine.x - Center.x;
    diff.y = MidLine.y - Center.y;
    float a0 = (diff.x*diff.x + diff.y*diff.y) - fRadius*fRadius;
    float a1 = dot(direction,diff);
    float discr = sqrt(a1*a1 - a0);
    float t0 = -a1 - discr;
    float t1 = -a1 + discr;

    vec2 p;
    if ( a0 >= 0.0 )
    {
        p = MidLine.xy + t1 * direction.xy;
    }
    else if ( t0 > 0.0 )
    {
        p = MidLine.xy + t0 * direction.xy;
    }
    else
    {
        p = MidLine.xy + t1 * direction.xy;
    }
    diffX = p.x - MidLine.x;
    diffY = p.y - MidLine.y;
    length =  sqrt(diffX * diffX + diffY * diffY );

    fAlpha = max( 0.0, 1.0 - (lengthShade / length) );

//     float dirX = fX - MidLine.x;
//     float dirY = fY - MidLine.y;
//     float length = sqrt( dirX * dirX + dirY * dirY );
//     dirX /= length;
//     dirY /= length;

//     float length = sqrt( (fX - MidLine.x) * (fX - MidLine.x) +
//                          (fY - MidLine.y) * (fY - MidLine.y)   );
//     float fAlpha = max( 0.0, 1.0 - (length / fRadius) );

    return fAlpha;
}
//----------------------------------------------------------------------------
// uniform vec4 LevColor;
// uniform vec4 LevMidLine;
// uniform vec2 Center;
// uniform float Radius;
// uniform float UseColorMap;
// uniform sampler2D BaseSampler;
// uniform sampler1D ColorMap; 
// void main()
// {
//     vec4 kBase = texture2D(BaseSampler,gl_TexCoord[0].xy);
//     float fAlpha = computeAlphaCircle( gl_TexCoord[0].x,
//                                        gl_TexCoord[0].y,
//                                        Center.xy,
//                                        LevMidLine.xy,
//                                        Radius );
//     vec4 widgetColor = LevColor;
//     if ( UseColorMap != -1.0 )
//     {
//         widgetColor = texture1D(ColorMap, fAlpha );
//         widgetColor.a = LevColor.a;
//     }
//     fAlpha *= widgetColor.a;
//     gl_FragColor.r = widgetColor.r*fAlpha + (1.0 - fAlpha)*kBase.r;
//     gl_FragColor.g = widgetColor.g*fAlpha + (1.0 - fAlpha)*kBase.g;
//     gl_FragColor.b = widgetColor.b*fAlpha + (1.0 - fAlpha)*kBase.b;
//     gl_FragColor.a = 1.0;
// }
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
uniform vec4 LevColor;
uniform vec4 LevMidLine;
uniform sampler2D BaseSampler;
uniform sampler1D ColorMap; 
uniform float UseColorMap;
uniform vec4 Center;
uniform float Radius;
void main()
{
    vec4 kBase = texture2D(BaseSampler,gl_TexCoord[0].xy);
    //float fAlpha = computeAlpha( gl_TexCoord[0].x, gl_TexCoord[0].y, Shift, InvY0MY1,
    //LevMidLine, LevLeftLine, LevRightLine );
    float fAlpha = computeAlphaCircle( gl_TexCoord[0].x, gl_TexCoord[0].y,
                                       Center, LevMidLine, Radius );
    vec4 widgetColor = LevColor;
    if ( UseColorMap != -1.0 )
    {
        widgetColor = texture1D(ColorMap, fAlpha );
        widgetColor.a = LevColor.a;
    }
    fAlpha *= widgetColor.a;
    gl_FragColor.r = widgetColor.r*fAlpha + (1.0 - fAlpha)*kBase.r;
    gl_FragColor.g = widgetColor.g*fAlpha + (1.0 - fAlpha)*kBase.g;
    gl_FragColor.b = widgetColor.b*fAlpha + (1.0 - fAlpha)*kBase.b;
    gl_FragColor.a = 1.0;
}
//----------------------------------------------------------------------------
