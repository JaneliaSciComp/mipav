uniform vec4    StepSize;
uniform sampler3D VolumeImageA;
uniform sampler1D OpacityMapA;

void p_SurfaceExtract()
{
    // texture-coordinate steps:
    vec3 index3;
    float stepX[3];    stepX[0] = -StepSize.x;    stepX[1] = 0.0;    stepX[2] = StepSize.x;
    float stepY[3];    stepY[0] = -StepSize.y;    stepY[1] = 0.0;    stepY[2] = StepSize.y;
    float stepZ[3];    stepZ[0] = -StepSize.z;    stepZ[1] = 0.0;    stepZ[2] = StepSize.z;

    // Look at the opacity values of all 8 corners of this voxel "Cube":
    vec3 cube[8];
    cube[ 0 ] = gl_TexCoord[0].xyz; // x,y,z
    cube[ 1 ] = gl_TexCoord[0].xyz; cube[1].x += StepSize.x; // x+1,y,z
    cube[ 2 ] = gl_TexCoord[0].xyz; cube[2].y += StepSize.y; // x,y+1,z
    cube[ 3 ] = gl_TexCoord[0].xyz; cube[3].x += StepSize.x; cube[3].y += StepSize.y; //x+1,y+1,z
    cube[ 4 ] = gl_TexCoord[0].xyz; cube[4].z += StepSize.z; //x,y,z+1
    cube[ 5 ] = gl_TexCoord[0].xyz; cube[5].x += StepSize.x; cube[5].z += StepSize.z; //x+1,y,z+1
    cube[ 6 ] = gl_TexCoord[0].xyz; cube[6].y += StepSize.y; cube[6].z += StepSize.z; //x,y+1,z+1 
    cube[ 7 ] = gl_TexCoord[0].xyz; cube[7].x += StepSize.x; cube[7].y += StepSize.y; cube[7].z += StepSize.z; //x+1,y+1,z+1

    vec4 color[8];
    float opacity = 0.0;
    int iShow = 8;
    for ( int i = 0; i < 8; i++ )
    {
        color[i] = texture3D(VolumeImageA,cube[i]);
        opacity = texture1D(OpacityMapA,color[i].r).r;
        if ( opacity <= 0.0 )
        {
            color[i].r = 0.0;
            color[i].g = 0.0;
            color[i].b = 0.0;
            iShow--;
        }
    }

    gl_FragColor.r = color[0].r;
    gl_FragColor.g = color[0].g;
    gl_FragColor.b = color[0].b;
    gl_FragColor.a = 0.0;
    // Alpha-value determines if voxel is part of surface:
    if ( iShow != 0.0 )
    {
        gl_FragColor.a = 1.0;
    }
}

