uniform vec4    StepSize;
uniform sampler3D VolumeExtract;
void p_SurfaceExtract_P2()
{
    float gaussian[27];
    gaussian[0] = 0.014652;
    gaussian[1] = 0.058608;
    gaussian[2] = 0.014652;

    gaussian[3] = 0.058608;
    gaussian[4] = 0.095238;
    gaussian[5] = 0.058608;

    gaussian[6] = 0.014652;
    gaussian[7] = 0.058608;
    gaussian[8] = 0.014652;


    gaussian[9] = 0.058608;
    gaussian[10] = 0.095238;
    gaussian[11] = 0.058608;

    gaussian[12] = 0.095238;
    gaussian[13] = 0.150183;
    gaussian[14] = 0.095238;

    gaussian[15] = 0.058608;
    gaussian[16] = 0.095238;
    gaussian[17] = 0.058608;


    gaussian[18] = 0.014652;
    gaussian[19] = 0.058608;
    gaussian[20] = 0.014652;

    gaussian[21] = 0.058608;
    gaussian[22] = 0.095238;
    gaussian[23] = 0.058608;

    gaussian[24] = 0.014652;
    gaussian[25] = 0.058608;
    gaussian[26] = 0.014652;

    vec4 color4;
    vec4 colorAcc = (0.0);
    float norm = 0.0;
    vec3 index3;
    float stepX[3];    stepX[0] = -StepSize.x;    stepX[1] = 0.0;    stepX[2] = StepSize.x;
    float stepY[3];    stepY[0] = -StepSize.y;    stepY[1] = 0.0;    stepY[2] = StepSize.y;
    float stepZ[3];    stepZ[0] = -StepSize.z;    stepZ[1] = 0.0;    stepZ[2] = StepSize.z;
    int gIndex = 0;
    for ( int z = 0; z < 3; z++ )
    {
        for ( int y = 0; y < 3; y++ )
        {
            for ( int x = 0; x < 3; x++ )
            {
                index3.x = gl_TexCoord[0].x + stepX[x];
                index3.y = gl_TexCoord[0].y + stepY[y];
                index3.z = gl_TexCoord[0].z + stepZ[z];
                color4 = texture3D( VolumeExtract, index3 );
                gIndex = z*9+y*3+x;
                colorAcc.x += (color4.x * gaussian[gIndex]);
                colorAcc.y += (color4.y * gaussian[gIndex]);
                colorAcc.z += (color4.z * gaussian[gIndex]);
                colorAcc.w += (color4.w * gaussian[gIndex]);
                norm += gaussian[gIndex];
            }
        }
    }

    gl_FragColor.r = colorAcc.x/norm;
    gl_FragColor.g = colorAcc.y/norm;
    gl_FragColor.b = colorAcc.z/norm;
    gl_FragColor.a = colorAcc.w/norm;
}
//----------------------------------------------------------------------------
