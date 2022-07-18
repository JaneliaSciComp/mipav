//----------------------------------------------------------------------------
uniform sampler3D imageA; 
uniform sampler3D imageB; 
uniform sampler2D transformImage;

uniform vec2 Min;
uniform vec2 Scale;

uniform vec3 ImageSize;
uniform vec3 ImageSizeInv;

void v_VolumeHistogramTransformV()
{
    vec2 index = vec2(0.0,0.0);
    vec4 row0 = texture2D( transformImage, index );
    index.y = 0.33;
    vec4 row1 = texture2D( transformImage, index );
    index.y = 0.66;
    vec4 row2 = texture2D( transformImage, index );
    index.y = 1.0;
    vec4 row3 = texture2D( transformImage, index );

    mat4 InverseTransform = mat4(1.0);
    InverseTransform[0][0] = row0.x;
    InverseTransform[1][0] = row0.y;
    InverseTransform[2][0] = row0.z;
    InverseTransform[3][0] = row0.w;

    InverseTransform[0][1] = row1.x;
    InverseTransform[1][1] = row1.y;
    InverseTransform[2][1] = row1.z;
    InverseTransform[3][1] = row1.w;

    InverseTransform[0][2] = row2.x;
    InverseTransform[1][2] = row2.y;
    InverseTransform[2][2] = row2.z;
    InverseTransform[3][2] = row2.w;

    InverseTransform[0][3] = row3.x;
    InverseTransform[1][3] = row3.y;
    InverseTransform[2][3] = row3.z;
    InverseTransform[3][3] = row3.w;

//     InverseTransform[0] = row0;
//     InverseTransform[1] = row1;
//     InverseTransform[2] = row2;
//     InverseTransform[3] = row3;
    

    gl_FrontColor = vec4(1.0,0.0,0.0,1.0);

    vec3 texCoord = gl_Vertex.xyz;
    texCoord += 1.0;
    texCoord /= 2.0;

    vec4 target = texture3D(imageA, texCoord );

    vec4 vert = gl_Vertex;
    vert.z = 0;

    vec4 kPos = vec4(0.0,0.0,0.0,1.0);
    kPos.xyz = texCoord * ImageSize;
    kPos = InverseTransform*kPos;
    texCoord = kPos.xyz * ImageSizeInv;

    vec4 moving = texture3D(imageB, texCoord );
    gl_FrontColor.a = moving.a;
    float yPos = (moving.r - Min.y) * Scale.y;
    yPos *= 2.0;
    yPos -= 1.0;
    vert.y = yPos;

    float xPos = (target.r - Min.x) * Scale.x;
    xPos *= 2.0;
    xPos -= 1.0;
    vert.x = xPos;
    gl_Position = vert;

    if ( (InverseTransform[0][0] == 0.0) &&
         (InverseTransform[0][1] == 0.0) &&
         (InverseTransform[0][2] == 0.0) &&
         (InverseTransform[0][3] == 0.0) &&

         (InverseTransform[1][0] == 0.0) &&
         (InverseTransform[1][1] == 0.0) &&
         (InverseTransform[1][2] == 0.0) &&
         (InverseTransform[1][3] == 0.0) &&

         (InverseTransform[2][0] == 0.0) &&
         (InverseTransform[2][1] == 0.0) &&
         (InverseTransform[2][2] == 0.0) &&
         (InverseTransform[2][3] == 0.0) &&

         (InverseTransform[3][0] == 0.0) &&
         (InverseTransform[3][1] == 0.0) &&
         (InverseTransform[3][2] == 0.0) &&
         (InverseTransform[3][3] == 0.0)
         )
    {
        gl_FrontColor.a = 0.0;
    }
}
//----------------------------------------------------------------------------
