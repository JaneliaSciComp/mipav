

kernel void CropKernel( global float* a, global float* b, 
    const int width, 
    const int height, 
    const int depth, 
    const int color, 
    const int numElements, 
    const float4 minValue, 
    const int DoClip,
    const float4 clip,
    const float4 clipInv,
    const float4 clipEye,
    const float4 clipEyeInv,
    const float4 clipArb,
    const float4 volumeMatrix1,
    const float4 volumeMatrix2,
    const float4 volumeMatrix3,
    const int slice )
    
     {
		int x = get_global_id(0); 
		int y = get_global_id(1); 
		int z = slice; 
		int pos = z * width * height + y * width + x; 
			// bound check, equivalent to the limit on a 'for' loop
		if (pos >= numElements)  {
		    return;
		}			
    
    float bClipped = 0.0;
    
    // axis-aligned clipping:
    if ( DoClip != 0.0 )
    {
        if ( x < clip.x )
        {
            bClipped = 1.0;
        }
        else if ( x > clipInv.x )
        {
            bClipped = 1.0;
        }
        else if ( y < clip.y )
        {
            bClipped = 1.0;
        }
        else if ( y > clipInv.y )
        {
            bClipped = 1.0;
        }
        else if ( z < clip.z )
        {
            bClipped = 1.0;
        }
        else if ( z > clipInv.z )
        {
            bClipped = 1.0;
        } 

        if ( bClipped != 1.0 )
        {
            // eye clipping and arbitrary clipping:
            float4 position = (0.0);
            position.x = (float)x/(float)(width-1);
            position.y = (float)y/(float)(height-1);
            position.z = (float)z/(float)(depth-1);
            
            float4 aPosition = (0.0);
            aPosition.x = position.x - 0.5;
            aPosition.y = position.y - 0.5;
            aPosition.z = position.z - 0.5;
            
            float x1 = dot( aPosition.xyz, volumeMatrix1.xyz );
            float y1 = dot( aPosition.xyz, volumeMatrix2.xyz );
            float z1 = dot( aPosition.xyz, volumeMatrix3.xyz );
            
            aPosition.x = x1 + 0.5;
            aPosition.y = y1 + 0.5;
            aPosition.z = z1 + 0.5;
            
            float fDot = dot( aPosition.xyz, clipEye.xyz );
            float fDotInv = dot( aPosition.xyz, clipEyeInv.xyz );
            
            
            float fDotArb = dot( position.xyz, clipArb.xyz );
            if ( (fDot < clipEye.w) || (fDotInv > clipEyeInv.w) || (fDotArb > clipArb.w) )
            {
                bClipped = 1.0;
            }
        }
    }
    if ( bClipped != 0.0 )
    {
        if ( color != 1.0 )
        {
		    b[pos*4 + 0] = minValue.w;
		    b[pos*4 + 1] = minValue.x;
		    b[pos*4 + 2] = minValue.y;
		    b[pos*4 + 3] = minValue.z;
		}
		else
		{
		    b[pos] = minValue.x;
        }
    }
    else
    {
        if ( color != 1.0 )
        {
		    b[pos*4 + 0] = a[pos*4 + 0];
		    b[pos*4 + 1] = a[pos*4 + 1];
		    b[pos*4 + 2] = a[pos*4 + 2];
		    b[pos*4 + 3] = a[pos*4 + 3];
		}
		else
		{
		    b[pos] = a[pos];
        }
    }
};
	