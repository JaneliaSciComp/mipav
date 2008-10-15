//----------------------------------------------------------------------------
uniform mat4 WVPMatrix;
void v_CalcNormalsPerSlice_Pass2()
{
    // Transform the position from model space to clip space.
    gl_Position = WVPMatrix*gl_Vertex;

    // Pass through the texture coordinate.
    gl_TexCoord[0] = gl_MultiTexCoord0;
}
//----------------------------------------------------------------------------
uniform vec4    StepSize;
uniform sampler3D VolumeNormals;
void p_CalcNormalsPerSlice_Pass2()
{
    vec3 index1 = gl_TexCoord[0].xyz; index1.x -= StepSize.x;
    vec3 index2 = gl_TexCoord[0].xyz; index2.x += StepSize.x;
    vec3 index3 = gl_TexCoord[0].xyz; index3.y -= StepSize.y;
    vec3 index4 = gl_TexCoord[0].xyz; index4.y += StepSize.y;
    vec3 index5 = gl_TexCoord[0].xyz; index5.z -= StepSize.z;
    vec3 index6 = gl_TexCoord[0].xyz; index6.z += StepSize.z;

    vec3 half = vec3(0.5, 0.5, 0.5);
    vec3 color = (texture3D( VolumeNormals, gl_TexCoord[0].xyz ).xyz - half.xyz);
    color += (texture3D( VolumeNormals, index1 ).xyz - half.xyz);
    color += (texture3D( VolumeNormals, index2 ).xyz - half.xyz);
    color += (texture3D( VolumeNormals, index3 ).xyz - half.xyz);
    color += (texture3D( VolumeNormals, index4 ).xyz - half.xyz);
    color += (texture3D( VolumeNormals, index5 ).xyz - half.xyz);
    color += (texture3D( VolumeNormals, index6 ).xyz - half.xyz);
    color = normalize(color).xyz;
    color += (1.0, 1.0, 1.0);
    color /= (2.0, 2.0, 2.0);

    gl_FragColor.rgb = color.xyz;
}
//----------------------------------------------------------------------------
