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

    vec3 shift_half = vec3(0.5, 0.5, 0.5);
    vec3 color = (texture3D( VolumeNormals, gl_TexCoord[0].xyz ).xyz - shift_half.xyz);
    color += (texture3D( VolumeNormals, index1 ).xyz - shift_half.xyz);
    color += (texture3D( VolumeNormals, index2 ).xyz - shift_half.xyz);
    color += (texture3D( VolumeNormals, index3 ).xyz - shift_half.xyz);
    color += (texture3D( VolumeNormals, index4 ).xyz - shift_half.xyz);
    color += (texture3D( VolumeNormals, index5 ).xyz - shift_half.xyz);
    color += (texture3D( VolumeNormals, index6 ).xyz - shift_half.xyz);
    color = normalize(color).xyz;
    color += (1.0, 1.0, 1.0);
    color /= (2.0, 2.0, 2.0);

    gl_FragColor.rgb = color.rgb;
}
//----------------------------------------------------------------------------
