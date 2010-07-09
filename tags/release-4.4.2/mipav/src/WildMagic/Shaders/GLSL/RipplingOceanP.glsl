//----------------------------------------------------------------------------
vec3 MapToUnit (vec3 kVector)
{
    // map [-1,1] to [0,1]
    vec3 kV = vec3(0.0,0.0,0.0);
    kV.x = 0.5*kVector.x + 0.5;
    kV.y = 0.5*kVector.y + 0.5;
    kV.z = 0.5*kVector.z + 0.5;
    //return 0.5*kVector + 0.5;
    return kV;
}
//----------------------------------------------------------------------------
vec3 MapFromUnit (vec3 kVector)
{
    // map [0,1] to [-1,1]
    vec3 kV = vec3(0.0,0.0,0.0);
    kV.x = 2.0*kVector.x - 1.0;
    kV.y = 2.0*kVector.y - 1.0;
    kV.z = 2.0*kVector.z - 1.0;
    //return 2.0*kVector - (1.0,1.0,1.0);
    return kV;
}
//----------------------------------------------------------------------------
uniform sampler2D aBumpSampler;
uniform sampler1D bWaterSampler;
uniform sampler2D cEnvSampler;
void p_RipplingOceanP()
{
    // A lot of this shader is making the water look just right.  It looks
    // pretty good in general, but there are some tweaks.  I will try to
    // explain what is necessary and what is specific to this case.

    // Sample the bumpmap twice.
    vec3 kNormPerturb0 = MapFromUnit(texture2D(aBumpSampler,gl_TexCoord[0].xy).xyz);
    vec3 kNormPerturb1 = MapFromUnit(texture2D(aBumpSampler,gl_TexCoord[1].xy).xyz);
    
    // The perturbed normal (in bumpmap space) is going to be the average.
    vec3 kNormPerturb = normalize((kNormPerturb0 + kNormPerturb1)*0.5);

    // Because the waves may have changed the surface, we will transform the
    // bump mapped normal into world space.  We cannot just use the
    // model->world transform here because that applies to the original
    // model.  Because that got changed (along with the normal) in the
    // vertex shader, we have to do it this way.
    vec3 kOldTangent = MapFromUnit(gl_TexCoord[3].xyz);
    vec3 kOldBinormal = MapFromUnit(gl_TexCoord[4].xyz);
    vec3 kOldNormal = MapFromUnit(gl_TexCoord[5].xyz);
    vec3 kNewNormal = kNormPerturb.x*kOldTangent +
        kNormPerturb.y*kOldBinormal + kNormPerturb.z*kOldNormal;

    // The water color is view dependent. We look this up in the gradient
    // texture.  Using the old normal (the water surface normal before bump
    // mapping) makes the Fresnel factor (and the water color) look more
    // right because it is much more low frequency than the bump mapped
    // ripples.
    vec3 kOldView = MapFromUnit(gl_TexCoord[2].xyz);
    float fFresnel = 1.0 - clamp(-dot(kOldNormal,kOldView), 0.0, 1.0);
    
    // This step could have been done in the texture itself.
    float fFresnelCubed = pow(fFresnel,3.0);
    
    // Get the water color from the gradient texture.  If we are looking
    // tangentially at the water, it will be bluer.  If we are looking
    // straight down (fFresnel close to zero) it will be greener.
    vec3 kWaterColor = texture1D(bWaterSampler,fFresnelCubed).xyz;

    // Get the reflection vector for specular reflections.
    vec3 kReflect = reflect(kNewNormal,kOldView);

    // We're going to sample the background texture, but we need to 
    // put the coordinates into [0,1].
    kReflect = MapToUnit(kReflect);

    // This step is just due to the texture that we are using.  It biases
    // towards the top, so that less of the water reflects the sun.  With an
    // an environment map, this is irrelevant.  A sphere map was tried, but
    // it was hard to get it just right.  Using the background texture is a
    // hack, but it looks good.
    kReflect.z = kReflect.z * 0.7 + 0.2;

    // This step is dependent upon knowing that up is in the z direction and
    // y is the forwards/backwards direction and x is left to right.  If we
    // had an environment map, it would not be an issue.
    vec3 kBackground = texture2D(cEnvSampler,kReflect.xz).xyz;

    // Use some factor of the background to find the specular reflection.
    // This factor is totally arbitrary and so the "glow" factor of the
    // background is the green component.  What I really want is the big sun
    // to be the have the strongest specular component so it will appear on
    // the water.  Green will do that.  Another way to do this would be to
    // store a glow map in the alpha channel and use that.  Also, we will use
    // Fresnel reflections as a factor so that the water reflects much more
    // when you look at it at an angle.  The specular is toned down a bit.
    float fSpecular = pow(kBackground.g,2.0)*fFresnelCubed;
    
    // Calculate a diffuse factor, but we do not want it too dark, so we will
    // add a small arbitrary ambient factor.
    vec3 kOldLightDir = MapFromUnit(gl_TexCoord[6].xyz);
    float fAmbient = gl_TexCoord[6].w;
    float fDiffuse = fAmbient - (1.0-fAmbient)*dot(kNewNormal,kOldLightDir);

    // Add the diffusely lit water color with some specular highlights.
    gl_FragColor.rgb = kWaterColor*fDiffuse + kBackground*fSpecular;
//     gl_FragColor.rgb -= kWaterColor*fDiffuse;
//     gl_FragColor.rgb -= kBackground*fSpecular;
//     gl_FragColor.rgb += kBackground.rgb;
    gl_FragColor.a = 1.0;
}
//----------------------------------------------------------------------------
