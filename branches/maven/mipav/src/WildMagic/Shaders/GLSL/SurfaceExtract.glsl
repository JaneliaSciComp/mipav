uniform vec4    StepSize;
uniform sampler3D bVolumeImageA; 
uniform sampler1D cColorMapA; 

in vec3 varTexCoord;
out vec4 fragColor;

void p_SurfaceExtract()
{
    // texture-coordinate steps:
    vec3 index3;
    float stepX[3];    stepX[0] = -StepSize.x;    stepX[1] = 0.0;    stepX[2] = StepSize.x;
    float stepY[3];    stepY[0] = -StepSize.y;    stepY[1] = 0.0;    stepY[2] = StepSize.y;
    float stepZ[3];    stepZ[0] = -StepSize.z;    stepZ[1] = 0.0;    stepZ[2] = StepSize.z;

    // Look at the opacity values of all 8 corners of this voxel "Cube":
    vec3 cube[8];
    cube[ 0 ] = varTexCoord.xyz; // x,y,z
    cube[ 1 ] = varTexCoord.xyz; cube[1].x += StepSize.x; // x+1,y,z
    cube[ 2 ] = varTexCoord.xyz; cube[2].y += StepSize.y; // x,y+1,z
    cube[ 3 ] = varTexCoord.xyz; cube[3].x += StepSize.x; cube[3].y += StepSize.y; //x+1,y+1,z
    cube[ 4 ] = varTexCoord.xyz; cube[4].z += StepSize.z; //x,y,z+1
    cube[ 5 ] = varTexCoord.xyz; cube[5].x += StepSize.x; cube[5].z += StepSize.z; //x+1,y,z+1
    cube[ 6 ] = varTexCoord.xyz; cube[6].y += StepSize.y; cube[6].z += StepSize.z; //x,y+1,z+1 
    cube[ 7 ] = varTexCoord.xyz; cube[7].x += StepSize.x; cube[7].y += StepSize.y; cube[7].z += StepSize.z; //x+1,y+1,z+1

    vec4 color[8];
    float opacity = 0.0;
    int iShow = 8;

    color[0] = texture(bVolumeImageA,cube[0]);
    opacity = texture(cColorMapA,color[0].r).a;
    if ( opacity <= 0.0 )
    {
        color[0].r = 0.0;
        color[0].g = 0.0;
        color[0].b = 0.0;
        iShow--;
    }

    color[1] = texture(bVolumeImageA,cube[1]);
    opacity = texture(cColorMapA,color[1].r).a;
    if ( opacity <= 0.0 )
    {
        color[1].r = 0.0;
        color[1].g = 0.0;
        color[1].b = 0.0;
        iShow--;
    }

    color[2] = texture(bVolumeImageA,cube[2]);
    opacity = texture(cColorMapA,color[2].r).a;
    if ( opacity <= 0.0 )
    {
        color[2].r = 0.0;
        color[2].g = 0.0;
        color[2].b = 0.0;
        iShow--;
    }

    color[3] = texture(bVolumeImageA,cube[3]);
    opacity = texture(cColorMapA,color[3].r).a;
    if ( opacity <= 0.0 )
    {
        color[3].r = 0.0;
        color[3].g = 0.0;
        color[3].b = 0.0;
        iShow--;
    }

    color[4] = texture(bVolumeImageA,cube[4]);
    opacity = texture(cColorMapA,color[4].r).a;
    if ( opacity <= 0.0 )
    {
        color[4].r = 0.0;
        color[4].g = 0.0;
        color[4].b = 0.0;
        iShow--;
    }


    color[5] = texture(bVolumeImageA,cube[5]);
    opacity = texture(cColorMapA,color[5].r).a;
    if ( opacity <= 0.0 )
    {
        color[5].r = 0.0;
        color[5].g = 0.0;
        color[5].b = 0.0;
        iShow--;
    }

    color[6] = texture(bVolumeImageA,cube[6]);
    opacity = texture(cColorMapA,color[6].r).a;
    if ( opacity <= 0.0 )
    {
        color[6].r = 0.0;
        color[6].g = 0.0;
        color[6].b = 0.0;
        iShow--;
    }

    color[7] = texture(bVolumeImageA,cube[7]);
    opacity = texture(cColorMapA,color[7].r).a;
    if ( opacity <= 0.0 )
    {
        color[7].r = 0.0;
        color[7].g = 0.0;
        color[7].b = 0.0;
        iShow--;
    }


    fragColor.r = color[0].r;
    fragColor.g = color[0].g;
    fragColor.b = color[0].b;
    fragColor.a = 0.0;
    // Alpha-value determines if voxel is part of surface:
    if ( iShow != 0.0 )
    {
        fragColor.a = 1.0;
    }
}

