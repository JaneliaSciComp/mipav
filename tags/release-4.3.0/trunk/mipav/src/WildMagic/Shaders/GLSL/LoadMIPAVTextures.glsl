uniform sampler2D aSceneImage_TEXUNIT0; 

uniform sampler3D bVolumeImageA_TEXUNIT1; 
uniform sampler1D cColorMapA_TEXUNIT2; 
uniform sampler1D dOpacityMapA_TEXUNIT3; 
uniform sampler3D eNormalMapA_TEXUNIT4; 
uniform sampler3D fVolumeImageA_GM_TEXUNIT5; 
uniform sampler1D gOpacityMapA_GM_TEXUNIT6; 
uniform sampler3D hVolumeImageA_2nd_TEXUNIT7; 

uniform sampler3D iSurfaceImage_TEXUNIT8;

uniform sampler3D jVolumeImageB_TEXUNIT9; 
uniform sampler1D kColorMapB_TEXUNIT10; 
uniform sampler1D lOpacityMapB_TEXUNIT11; 
uniform sampler3D mNormalMapB_TEXUNIT12; 
uniform sampler3D nVolumeImageB_GM_TEXUNIT13; 
uniform sampler1D oOpacityMapB_GM_TEXUNIT14; 
uniform sampler3D pVolumeImageB_2nd_TEXUNIT15; 


void p_LoadMIPAVTextures()
{
    vec4 color  = vec4(0.0);

    color += texture2D( aSceneImage_TEXUNIT0, gl_TexCoord[0].xy ); 

    color += texture3D( bVolumeImageA_TEXUNIT1, gl_TexCoord[0].xyz ); 
    color += texture1D( cColorMapA_TEXUNIT2, gl_TexCoord[0].x ); 
    color += texture1D( dOpacityMapA_TEXUNIT3, gl_TexCoord[0].x ); 
    color += texture3D( eNormalMapA_TEXUNIT4, gl_TexCoord[0].xyz ); 
    color += texture3D( fVolumeImageA_GM_TEXUNIT5, gl_TexCoord[0].xyz ); 
    color += texture1D( gOpacityMapA_GM_TEXUNIT6, gl_TexCoord[0].x ); 
    color += texture3D( hVolumeImageA_2nd_TEXUNIT7, gl_TexCoord[0].xyz ); 

    color += texture3D( iSurfaceImage_TEXUNIT8, gl_TexCoord[0].xyz );

    color += texture3D( jVolumeImageB_TEXUNIT9, gl_TexCoord[0].xyz ); 
    color += texture1D( kColorMapB_TEXUNIT10, gl_TexCoord[0].x ); 
    color += texture1D( lOpacityMapB_TEXUNIT11, gl_TexCoord[0].x ); 
    color += texture3D( mNormalMapB_TEXUNIT12, gl_TexCoord[0].xyz ); 
    color += texture3D( nVolumeImageB_GM_TEXUNIT13, gl_TexCoord[0].xyz ); 
    color += texture1D( oOpacityMapB_GM_TEXUNIT14, gl_TexCoord[0].x ); 
    color += texture3D( pVolumeImageB_2nd_TEXUNIT15, gl_TexCoord[0].xyz ); 

    gl_FragColor = color;
}
