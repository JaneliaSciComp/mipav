//----------------------------------------------------------------------------
// uniform mat4 WVPMatrix;
uniform sampler3D imageA; 
uniform sampler3D imageB; 

uniform vec2 Min;
uniform vec2 Scale;

uniform vec3 ImageSize;
uniform vec3 ImageSizeInv;

uniform mat4 InverseTransformMatrix;

void v_VolumeHistogram2DV()
{
    gl_FrontColor = vec4(1.0,0.0,0.0,1.0);

    vec3 texCoord = gl_MultiTexCoord0.xyz;
    vec4 vert = gl_Vertex;

    vec4 kPos = vec4(0.0,0.0,0.0,1.0);
    kPos.xyz = texCoord * ImageSize;
    kPos = InverseTransformMatrix*kPos;
    texCoord = kPos.xyz * ImageSizeInv;

    vec4 vol1 = vec4(0.0);
    if ( (texCoord.x < 0.0) || (texCoord.x > 1.0) ||
         (texCoord.y < 0.0) || (texCoord.y > 1.0) ||
         (texCoord.z < 0.0) || (texCoord.z > 1.0)    )
    {
        gl_FrontColor.r = 0.0;
        gl_FrontColor.a = 0.0;
    }
    else
    {
        vol1 = texture3D(imageA, texCoord );
        float yPos = (vol1.r - Min.y) * Scale.y;
        yPos *= 2.0;
        yPos -= 1.0;
        vert.y = yPos;

        vol1.r = gl_Color.r;
        float xPos = (vol1.r - Min.x) * Scale.x;
        xPos *= 2.0;
        xPos -= 1.0;
        vert.x = xPos;
    }



//     vec4 vol1 = texture3D(imageA, texCoord );
//     float yPos = (vol1.r - Min.y) * Scale.y;
//     yPos *= 2.0;
//     yPos -= 1.0;
//     vert.y = yPos;
    
    

//     vec4 kPos = vec4(0.0,0.0,0.0,1.0);
//     kPos.xyz = texCoord * ImageSize;
//     kPos = InverseTransformMatrix*kPos;
//     texCoord = kPos.xyz * ImageSizeInv;

//      if ( (texCoord.x < 0.0) || (texCoord.x > 1.0) ||
//           (texCoord.y < 0.0) || (texCoord.y > 1.0) ||
//           (texCoord.z < 0.0) || (texCoord.z > 1.0)    )
//      {
//          gl_FrontColor.r = 0.0;
//          gl_FrontColor.a = 0.0;
//      }
//      else
//      {
//          vol1 = texture3D(imageB, texCoord );
//          //vol1 = texture3D(imageA, texCoord );
//          float yPos = (vol1.r - Min.y) * Scale.y;
//          yPos *= 2.0;
//          yPos -= 1.0;
//          vert.y = yPos;
//      }

    gl_Position = vert;

     //gl_FrontColor = texture3D(imageB, gl_MultiTexCoord0.xyz );
     //gl_FrontColor = texture3D(imageB, texCoord.xyz );
     //gl_Position = gl_Vertex * 2;
     //gl_FrontColor = gl_Color;
     //gl_FrontColor.r = vol1.r;
     //gl_Position = gl_Vertex;
}
//----------------------------------------------------------------------------
