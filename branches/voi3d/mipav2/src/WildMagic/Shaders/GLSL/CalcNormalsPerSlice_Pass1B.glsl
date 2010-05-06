//----------------------------------------------------------------------------
uniform vec4    StepSize;
uniform float     IsColor;
uniform sampler3D jVolumeImageB; 
void p_CalcNormalsPerSlice_Pass1B()
{
    vec3 indexX1 = gl_TexCoord[0].xyz; indexX1.y -= StepSize.y; indexX1.x -= StepSize.x;
    vec3 indexX2 = gl_TexCoord[0].xyz; indexX2.y -= StepSize.y; indexX2.x += StepSize.x;
    vec3 indexX3 = gl_TexCoord[0].xyz;                          indexX3.x -= StepSize.x;
    vec3 indexX4 = gl_TexCoord[0].xyz;                          indexX4.x += StepSize.x;
    vec3 indexX5 = gl_TexCoord[0].xyz; indexX5.y += StepSize.y; indexX5.x -= StepSize.x;
    vec3 indexX6 = gl_TexCoord[0].xyz; indexX6.y += StepSize.y; indexX6.x += StepSize.x;

    indexX1 = texture3D( jVolumeImageB, indexX1 ).xyz;
    indexX2 = texture3D( jVolumeImageB, indexX2 ).xyz;
    indexX3 = texture3D( jVolumeImageB, indexX3 ).xyz;
    indexX4 = texture3D( jVolumeImageB, indexX4 ).xyz;
    indexX5 = texture3D( jVolumeImageB, indexX5 ).xyz;
    indexX6 = texture3D( jVolumeImageB, indexX6 ).xyz;

    if ( IsColor == 1.0 )
    {
        indexX1.x = (indexX1.x + indexX1.y + indexX1.z)/3.0;
        indexX2.x = (indexX2.x + indexX2.y + indexX2.z)/3.0;
        indexX3.x = (indexX3.x + indexX3.y + indexX3.z)/3.0;
        indexX4.x = (indexX4.x + indexX4.y + indexX4.z)/3.0;
        indexX5.x = (indexX5.x + indexX5.y + indexX5.z)/3.0;
        indexX6.x = (indexX5.x + indexX6.y + indexX6.z)/3.0;
    }

    float fDX = 0.71 * (indexX1.x - indexX2.x) + 
                       (indexX3.x - indexX4.x) + 
                0.71 * (indexX5.x - indexX6.x);


    vec3 indexY1 = gl_TexCoord[0].xyz; indexY1.x -= StepSize.x; indexY1.y -= StepSize.y;
    vec3 indexY2 = gl_TexCoord[0].xyz; indexY2.x -= StepSize.x; indexY2.y += StepSize.y;
    vec3 indexY3 = gl_TexCoord[0].xyz;                          indexY3.y -= StepSize.y;
    vec3 indexY4 = gl_TexCoord[0].xyz;                          indexY4.y += StepSize.y;
    vec3 indexY5 = gl_TexCoord[0].xyz; indexY5.x += StepSize.x; indexY5.y -= StepSize.y;
    vec3 indexY6 = gl_TexCoord[0].xyz; indexY6.x += StepSize.x; indexY6.y += StepSize.y;

    indexY1 = texture3D( jVolumeImageB, indexY1 ).xyz;
    indexY2 = texture3D( jVolumeImageB, indexY2 ).xyz;
    indexY3 = texture3D( jVolumeImageB, indexY3 ).xyz;
    indexY4 = texture3D( jVolumeImageB, indexY4 ).xyz;
    indexY5 = texture3D( jVolumeImageB, indexY5 ).xyz;
    indexY6 = texture3D( jVolumeImageB, indexY6 ).xyz;

    if ( IsColor == 1.0 )
    {
        indexY1.x = (indexY1.x + indexY1.y + indexY1.z)/3.0;
        indexY2.x = (indexY2.x + indexY2.y + indexY2.z)/3.0;
        indexY3.x = (indexY3.x + indexY3.y + indexY3.z)/3.0;
        indexY4.x = (indexY4.x + indexY4.y + indexY4.z)/3.0;
        indexY5.x = (indexY5.x + indexY5.y + indexY5.z)/3.0;
        indexY6.x = (indexY5.x + indexY6.y + indexY6.z)/3.0;
    }

    float fDY = 0.71 * (indexY1.x - indexY2.x) + 
                       (indexY3.x - indexY4.x) + 
                0.71 * (indexY5.x - indexY6.x);


    vec3 indexZ1 = gl_TexCoord[0].xyz; indexZ1.x -= StepSize.x; indexZ1.z -= StepSize.z;
    vec3 indexZ2 = gl_TexCoord[0].xyz; indexZ2.x -= StepSize.x; indexZ2.z += StepSize.z;
    vec3 indexZ3 = gl_TexCoord[0].xyz;                          indexZ3.z -= StepSize.z;
    vec3 indexZ4 = gl_TexCoord[0].xyz;                          indexZ4.z += StepSize.z;
    vec3 indexZ5 = gl_TexCoord[0].xyz; indexZ5.x += StepSize.x; indexZ5.z -= StepSize.z;
    vec3 indexZ6 = gl_TexCoord[0].xyz; indexZ6.x += StepSize.x; indexZ6.z += StepSize.z;

    indexZ1 = texture3D( jVolumeImageB, indexZ1 ).xyz;
    indexZ2 = texture3D( jVolumeImageB, indexZ2 ).xyz;
    indexZ3 = texture3D( jVolumeImageB, indexZ3 ).xyz;
    indexZ4 = texture3D( jVolumeImageB, indexZ4 ).xyz;
    indexZ5 = texture3D( jVolumeImageB, indexZ5 ).xyz;
    indexZ6 = texture3D( jVolumeImageB, indexZ6 ).xyz;

    if ( IsColor == 1.0 )
    {
        indexZ1.x = (indexZ1.x + indexZ1.y + indexZ1.z)/3.0;
        indexZ2.x = (indexZ2.x + indexZ2.y + indexZ2.z)/3.0;
        indexZ3.x = (indexZ3.x + indexZ3.y + indexZ3.z)/3.0;
        indexZ4.x = (indexZ4.x + indexZ4.y + indexZ4.z)/3.0;
        indexZ5.x = (indexZ5.x + indexZ5.y + indexZ5.z)/3.0;
        indexZ6.x = (indexZ5.x + indexZ6.y + indexZ6.z)/3.0;
    }

    float fDZ = 0.71 * (indexZ1.x - indexZ2.x) + 
                       (indexZ3.x - indexZ4.x) + 
                0.71 * (indexZ5.x - indexZ6.x);

    gl_FragColor.r = 0.5 + fDX / 8.0;
    gl_FragColor.g = 0.5 + fDY / 8.0;
    gl_FragColor.b = 0.5 + fDZ / 8.0;
}
//----------------------------------------------------------------------------
