//----------------------------------------------------------------------------
uniform sampler3D imageA; 
uniform sampler3D imageB; 

uniform vec2 Min;
uniform vec2 Scale;

uniform vec3 ImageSize;
uniform vec3 ImageSizeInv;

uniform mat4 InverseTransform;
uniform float ZSlice;
uniform float UseZSlice;

void v_VolumeHistogram2DV()
{
    gl_FrontColor = vec4(1.0,0.0,0.0,1.0);

    vec3 texCoord = gl_Vertex.xyz;
    if ( UseZSlice == 1.0 )
    {
        texCoord.z = ZSlice;
    }
    texCoord += 1.0;
    texCoord /= 2.0;

    vec4 target = texture3D(imageA, texCoord );
    //gl_FrontColor = texture3D(imageA, texCoord ); 

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

    //gl_FrontColor += texture3D(imageB, texCoord ); 
    //gl_Position = gl_Vertex;
}
//----------------------------------------------------------------------------
