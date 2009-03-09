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
// float computeX( float fY,
//                 vec4 LevLine )
// {
//     float x0 = LevLine.x;
//     float y0 = LevLine.y;
//     float x1 = LevLine.z;
//     float y1 = LevLine.w;
//     float t = (y0 - fY)/(y0 - y1);
//     float x = x0 - x0 * t + x1 * t;
//     return x;
// }
// //----------------------------------------------------------------------------
// float computeAlpha( float fX,
//                     float fY,
//                     vec4  LevMidLine,
//                     vec4  LevLeftLine,
//                     vec4  LevRightLine )
// {
//     if ( (fY < LevLeftLine.y) || fY > LevLeftLine.w )
//     {
//         return 0.0;
//     }
//     float xMid = LevMidLine.x;
//     float fShiftL = 0.0;
//     float fShiftR = 0.0;
//     float fShiftX = 0.0;
//     float fIncr = 0.0;
//     if ( LevMidLine.y != LevMidLine.w )
//     {
//         xMid = computeX( fY, LevMidLine );
//     }
//     else
//     {
//         fIncr = (LevMidLine.y - LevLeftLine.y)/(LevLeftLine.w-LevLeftLine.y);
//         fIncr = fIncr * (LevRightLine.x - LevLeftLine.x);
//         fShiftX = (LevMidLine.x - LevLeftLine.x)/(LevRightLine.x-LevLeftLine.x);
//         fShiftL = (fShiftX)*fIncr;
//         fShiftR = (1.0-fShiftX)*fIncr;
//     }
//     float xLeft = computeX( fY, LevLeftLine );
//     float xRight = computeX( fY, LevRightLine );
    
//     float fAlpha = 0.0;
//     if ( (fX > (xMid - fShiftL)) && (fX < (xMid + fShiftR)) )
//     {
//         fAlpha = 1.0;
//     }
//     if ( (fX <= (xMid-fShiftL)) && (fX >= xLeft) )
//     {
//         fAlpha = (fX - xLeft) / ((xMid-fShiftL) - xLeft);
//     }
//     else if ( (fX >= (xMid+fShiftR)) && (fX <= xRight) )
//     {
//         fAlpha = (fX - xRight) / ((xMid+fShiftR) - xRight);
//     }
//     return fAlpha;
// }
//----------------------------------------------------------------------------
uniform vec4 LevColor;
uniform vec2 Shift;
uniform vec3 InvY0MY1;
uniform vec4 LevMidLine;
uniform vec4 LevLeftLine;
uniform vec4 LevRightLine;
uniform sampler2D BaseSampler;
void main()
{
    vec4 kBase = texture2D(BaseSampler,gl_TexCoord[0].xy);
    float fAlpha = computeAlpha( gl_TexCoord[0].x, gl_TexCoord[0].y, Shift, InvY0MY1,
                                 LevMidLine, LevLeftLine, LevRightLine );
    fAlpha *= LevColor.a;
    gl_FragColor.r = LevColor.r*fAlpha + (1.0 - fAlpha)*kBase.r;
    gl_FragColor.g = LevColor.g*fAlpha + (1.0 - fAlpha)*kBase.g;
    gl_FragColor.b = LevColor.b*fAlpha + (1.0 - fAlpha)*kBase.b;
    gl_FragColor.a = 1.0;
}
//----------------------------------------------------------------------------
