// transl output by Cg compiler
// cgc version 1.5.0023, build date Sep 26 2007 08:50:15
// command line args: -profile glslv
// source file: Default.cg
//vendor NVIDIA Corporation
//version 1.5.0.23
//profile glslv
//program v_Default
//semantic v_Default.WVPMatrix
//var float4x4 WVPMatrix :  : pend_s3_WVPMatrix[0], 4 : 2 : 1
//var float4 kModelPosition : $vin.POSITION : POSITION : 0 : 1
//var float4 kClipPosition : $vout.POSITION : POSITION : 1 : 1

uniform mat4 WVPMatrix;
//vec4 dash1_pend_s3_r_0001;

 // main procedure, the original name was v_Default
void  v_Default()
{


    //dash1_pend_s3_r_0001 = gl_Vertex.x*pend_s3_WVPMatrix[0];
    //dash1_pend_s3_r_0001 = dash1_pend_s3_r_0001 + gl_Vertex.y*pend_s3_WVPMatrix[1];
    //dash1_pend_s3_r_0001 = dash1_pend_s3_r_0001 + gl_Vertex.z*pend_s3_WVPMatrix[2];
    //dash1_pend_s3_r_0001 = dash1_pend_s3_r_0001 + gl_Vertex.w*pend_s3_WVPMatrix[3];
    gl_Position = WVPMatrix * gl_Vertex;
} // main end

// transl output by Cg compiler
// cgc version 1.5.0023, build date Sep 26 2007 08:50:15
// command line args: -profile glslf
// source file: Default.cg
//vendor NVIDIA Corporation
//version 1.5.0.23
//profile glslf
//program p_Default
//var float4 kPixelColor : $vout.COLOR : COLOR : 0 : 1


 // main procedure, the original name was p_Default
void p_Default()
{


    gl_FragColor = vec4( 1.0, 0.0, 1.0, 1.0);
} // main end
