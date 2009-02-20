uniform vec4    StepSize;
uniform sampler3D VolumeExtract;
void p_SurfaceExtract_P2()
{
    float gaussian[3][3][3];
    gaussian[0][0][0] = 0.014652;
    gaussian[0][0][1] = 0.058608;
    gaussian[0][0][2] = 0.014652;

    gaussian[0][1][0] = 0.058608;
    gaussian[0][1][1] = 0.095238;
    gaussian[0][1][2] = 0.058608;

    gaussian[0][2][0] = 0.014652;
    gaussian[0][2][1] = 0.058608;
    gaussian[0][2][2] = 0.014652;


    gaussian[1][0][0] = 0.058608;
    gaussian[1][0][1] = 0.095238;
    gaussian[1][0][2] = 0.058608;

    gaussian[1][1][0] = 0.095238;
    gaussian[1][1][1] = 0.150183;
    gaussian[1][1][2] = 0.095238;

    gaussian[1][2][0] = 0.058608;
    gaussian[1][2][1] = 0.095238;
    gaussian[1][2][2] = 0.058608;


    gaussian[2][0][0] = 0.014652;
    gaussian[2][0][1] = 0.058608;
    gaussian[2][0][2] = 0.014652;

    gaussian[2][1][0] = 0.058608;
    gaussian[2][1][1] = 0.095238;
    gaussian[2][1][2] = 0.058608;

    gaussian[2][2][0] = 0.014652;
    gaussian[2][2][1] = 0.058608;
    gaussian[2][2][2] = 0.014652;

    vec4 color4;
    vec4 colorAcc = (0.0);
    float norm = 0.0;
    vec3 index3;
    float stepX[3];    stepX[0] = -StepSize.x;    stepX[1] = 0.0;    stepX[2] = StepSize.x;
    float stepY[3];    stepY[0] = -StepSize.y;    stepY[1] = 0.0;    stepY[2] = StepSize.y;
    float stepZ[3];    stepZ[0] = -StepSize.z;    stepZ[1] = 0.0;    stepZ[2] = StepSize.z;
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
                colorAcc.x += (color4.x * gaussian[z][y][x]);
                colorAcc.y += (color4.y * gaussian[z][y][x]);
                colorAcc.z += (color4.z * gaussian[z][y][x]);
                colorAcc.w += (color4.w * gaussian[z][y][x]);
                norm += gaussian[z][y][x];
            }
        }
    }

    gl_FragColor.r = colorAcc.x/norm;
    gl_FragColor.g = colorAcc.y/norm;
    gl_FragColor.b = colorAcc.z/norm;
    gl_FragColor.a = colorAcc.w/norm;
}
//----------------------------------------------------------------------------
