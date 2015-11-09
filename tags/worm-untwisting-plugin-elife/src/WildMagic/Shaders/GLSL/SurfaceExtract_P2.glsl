uniform vec4    StepSize;
uniform sampler3D VolumeExtract;

in vec3 varTexCoord;
out vec4 fragColor;
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
    vec4 colorAcc = vec4(0.0);
    float norm = 1.406958;
    vec3 index3 = vec3(0.0);
    float stepX[3];    stepX[0] = -StepSize.x;    stepX[1] = 0.0;    stepX[2] = StepSize.x;
    float stepY[3];    stepY[0] = -StepSize.y;    stepY[1] = 0.0;    stepY[2] = StepSize.y;
    float stepZ[3];    stepZ[0] = -StepSize.z;    stepZ[1] = 0.0;    stepZ[2] = StepSize.z;

    // z = 0:
    index3.x = varTexCoord.x + stepX[0];
    index3.y = varTexCoord.y + stepY[0];
    index3.z = varTexCoord.z + stepZ[0];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[0]);
    colorAcc.y += (color4.y * gaussian[0]);
    colorAcc.z += (color4.z * gaussian[0]);
    colorAcc.w += (color4.w * gaussian[0]);

    index3.x = varTexCoord.x + stepX[1];
    index3.y = varTexCoord.y + stepY[0];
    index3.z = varTexCoord.z + stepZ[0];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[1]);
    colorAcc.y += (color4.y * gaussian[1]);
    colorAcc.z += (color4.z * gaussian[1]);
    colorAcc.w += (color4.w * gaussian[1]);

    index3.x = varTexCoord.x + stepX[2];
    index3.y = varTexCoord.y + stepY[0];
    index3.z = varTexCoord.z + stepZ[0];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[2]);
    colorAcc.y += (color4.y * gaussian[2]);
    colorAcc.z += (color4.z * gaussian[2]);
    colorAcc.w += (color4.w * gaussian[2]);

    index3.x = varTexCoord.x + stepX[0];
    index3.y = varTexCoord.y + stepY[1];
    index3.z = varTexCoord.z + stepZ[0];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[3]);
    colorAcc.y += (color4.y * gaussian[3]);
    colorAcc.z += (color4.z * gaussian[3]);
    colorAcc.w += (color4.w * gaussian[3]);

    index3.x = varTexCoord.x + stepX[1];
    index3.y = varTexCoord.y + stepY[1];
    index3.z = varTexCoord.z + stepZ[0];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[4]);
    colorAcc.y += (color4.y * gaussian[4]);
    colorAcc.z += (color4.z * gaussian[4]);
    colorAcc.w += (color4.w * gaussian[4]);

    index3.x = varTexCoord.x + stepX[2];
    index3.y = varTexCoord.y + stepY[1];
    index3.z = varTexCoord.z + stepZ[0];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[5]);
    colorAcc.y += (color4.y * gaussian[5]);
    colorAcc.z += (color4.z * gaussian[5]);
    colorAcc.w += (color4.w * gaussian[5]);

    index3.x = varTexCoord.x + stepX[0];
    index3.y = varTexCoord.y + stepY[2];
    index3.z = varTexCoord.z + stepZ[0];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[6]);
    colorAcc.y += (color4.y * gaussian[6]);
    colorAcc.z += (color4.z * gaussian[6]);
    colorAcc.w += (color4.w * gaussian[6]);

    index3.x = varTexCoord.x + stepX[1];
    index3.y = varTexCoord.y + stepY[2];
    index3.z = varTexCoord.z + stepZ[0];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[7]);
    colorAcc.y += (color4.y * gaussian[7]);
    colorAcc.z += (color4.z * gaussian[7]);
    colorAcc.w += (color4.w * gaussian[8]);

    index3.x = varTexCoord.x + stepX[2];
    index3.y = varTexCoord.y + stepY[2];
    index3.z = varTexCoord.z + stepZ[0];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[8]);
    colorAcc.y += (color4.y * gaussian[8]);
    colorAcc.z += (color4.z * gaussian[8]);
    colorAcc.w += (color4.w * gaussian[8]);

    // z = 1:
    index3.x = varTexCoord.x + stepX[0];
    index3.y = varTexCoord.y + stepY[0];
    index3.z = varTexCoord.z + stepZ[1];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[9]);
    colorAcc.y += (color4.y * gaussian[9]);
    colorAcc.z += (color4.z * gaussian[9]);
    colorAcc.w += (color4.w * gaussian[9]);

    index3.x = varTexCoord.x + stepX[1];
    index3.y = varTexCoord.y + stepY[0];
    index3.z = varTexCoord.z + stepZ[1];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[10]);
    colorAcc.y += (color4.y * gaussian[10]);
    colorAcc.z += (color4.z * gaussian[10]);
    colorAcc.w += (color4.w * gaussian[10]);

    index3.x = varTexCoord.x + stepX[2];
    index3.y = varTexCoord.y + stepY[0];
    index3.z = varTexCoord.z + stepZ[1];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[11]);
    colorAcc.y += (color4.y * gaussian[11]);
    colorAcc.z += (color4.z * gaussian[11]);
    colorAcc.w += (color4.w * gaussian[11]);

    index3.x = varTexCoord.x + stepX[0];
    index3.y = varTexCoord.y + stepY[1];
    index3.z = varTexCoord.z + stepZ[1];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[12]);
    colorAcc.y += (color4.y * gaussian[12]);
    colorAcc.z += (color4.z * gaussian[12]);
    colorAcc.w += (color4.w * gaussian[12]);

    index3.x = varTexCoord.x + stepX[1];
    index3.y = varTexCoord.y + stepY[1];
    index3.z = varTexCoord.z + stepZ[1];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[13]);
    colorAcc.y += (color4.y * gaussian[13]);
    colorAcc.z += (color4.z * gaussian[13]);
    colorAcc.w += (color4.w * gaussian[13]);

    index3.x = varTexCoord.x + stepX[2];
    index3.y = varTexCoord.y + stepY[1];
    index3.z = varTexCoord.z + stepZ[1];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[14]);
    colorAcc.y += (color4.y * gaussian[14]);
    colorAcc.z += (color4.z * gaussian[14]);
    colorAcc.w += (color4.w * gaussian[14]);

    index3.x = varTexCoord.x + stepX[0];
    index3.y = varTexCoord.y + stepY[2];
    index3.z = varTexCoord.z + stepZ[1];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[15]);
    colorAcc.y += (color4.y * gaussian[15]);
    colorAcc.z += (color4.z * gaussian[15]);
    colorAcc.w += (color4.w * gaussian[15]);

    index3.x = varTexCoord.x + stepX[1];
    index3.y = varTexCoord.y + stepY[2];
    index3.z = varTexCoord.z + stepZ[1];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[16]);
    colorAcc.y += (color4.y * gaussian[16]);
    colorAcc.z += (color4.z * gaussian[16]);
    colorAcc.w += (color4.w * gaussian[16]);

    index3.x = varTexCoord.x + stepX[2];
    index3.y = varTexCoord.y + stepY[2];
    index3.z = varTexCoord.z + stepZ[1];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[17]);
    colorAcc.y += (color4.y * gaussian[17]);
    colorAcc.z += (color4.z * gaussian[17]);
    colorAcc.w += (color4.w * gaussian[17]);



    // z = 2:
    index3.x = varTexCoord.x + stepX[0];
    index3.y = varTexCoord.y + stepY[0];
    index3.z = varTexCoord.z + stepZ[2];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[18]);
    colorAcc.y += (color4.y * gaussian[18]);
    colorAcc.z += (color4.z * gaussian[18]);
    colorAcc.w += (color4.w * gaussian[18]);

    index3.x = varTexCoord.x + stepX[1];
    index3.y = varTexCoord.y + stepY[0];
    index3.z = varTexCoord.z + stepZ[2];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[19]);
    colorAcc.y += (color4.y * gaussian[19]);
    colorAcc.z += (color4.z * gaussian[19]);
    colorAcc.w += (color4.w * gaussian[19]);

    index3.x = varTexCoord.x + stepX[2];
    index3.y = varTexCoord.y + stepY[0];
    index3.z = varTexCoord.z + stepZ[2];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[20]);
    colorAcc.y += (color4.y * gaussian[20]);
    colorAcc.z += (color4.z * gaussian[20]);
    colorAcc.w += (color4.w * gaussian[20]);

    index3.x = varTexCoord.x + stepX[0];
    index3.y = varTexCoord.y + stepY[1];
    index3.z = varTexCoord.z + stepZ[2];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[21]);
    colorAcc.y += (color4.y * gaussian[21]);
    colorAcc.z += (color4.z * gaussian[21]);
    colorAcc.w += (color4.w * gaussian[21]);

    index3.x = varTexCoord.x + stepX[1];
    index3.y = varTexCoord.y + stepY[1];
    index3.z = varTexCoord.z + stepZ[2];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[22]);
    colorAcc.y += (color4.y * gaussian[22]);
    colorAcc.z += (color4.z * gaussian[22]);
    colorAcc.w += (color4.w * gaussian[22]);

    index3.x = varTexCoord.x + stepX[2];
    index3.y = varTexCoord.y + stepY[1];
    index3.z = varTexCoord.z + stepZ[2];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[23]);
    colorAcc.y += (color4.y * gaussian[23]);
    colorAcc.z += (color4.z * gaussian[23]);
    colorAcc.w += (color4.w * gaussian[23]);

    index3.x = varTexCoord.x + stepX[0];
    index3.y = varTexCoord.y + stepY[2];
    index3.z = varTexCoord.z + stepZ[2];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[24]);
    colorAcc.y += (color4.y * gaussian[24]);
    colorAcc.z += (color4.z * gaussian[24]);
    colorAcc.w += (color4.w * gaussian[24]);

    index3.x = varTexCoord.x + stepX[1];
    index3.y = varTexCoord.y + stepY[2];
    index3.z = varTexCoord.z + stepZ[2];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[25]);
    colorAcc.y += (color4.y * gaussian[25]);
    colorAcc.z += (color4.z * gaussian[25]);
    colorAcc.w += (color4.w * gaussian[25]);

    index3.x = varTexCoord.x + stepX[2];
    index3.y = varTexCoord.y + stepY[2];
    index3.z = varTexCoord.z + stepZ[2];
    color4 = texture( VolumeExtract, index3 );
    colorAcc.x += (color4.x * gaussian[26]);
    colorAcc.y += (color4.y * gaussian[26]);
    colorAcc.z += (color4.z * gaussian[26]);
    colorAcc.w += (color4.w * gaussian[26]);



    fragColor.r = colorAcc.x/norm;
    fragColor.g = colorAcc.y/norm;
    fragColor.b = colorAcc.z/norm;
    fragColor.a = colorAcc.w/norm;
}
//----------------------------------------------------------------------------
