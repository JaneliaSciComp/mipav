

__kernel void Convolve25D(
    __global float *input,
    __global float *mask,
    __global float *output,
    const int2 imageSize,
    const int2 maskSize,
    const int2 maskOrigin,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    float sum = 0;
    float norm = 0;
    float maskVal = 0;
    float val = 0;
    for(int mx=0; mx<maskSize.x; mx++)
    {
        for(int my=0; my<maskSize.y; my++)
        {
            int mi = mul24(my, maskSize.x) + mx;
            int ix = gx - maskOrigin.x + mx;
            int iy = gy - maskOrigin.y + my;
            int i = gz * imageSize.x * imageSize.y + iy * imageSize.x + ix;
            if ( (ix >= 0) && (ix < imageSize.x) &&
                 (iy >= 0) && (iy < imageSize.y)    )
            {
               val = input[i];
               maskVal = mask[mi];
               sum += val * maskVal;
               
               if ( maskVal >= 0 )
               {
                  norm += maskVal;
               }
               else
               {
                  norm += -maskVal;
               }
            }
         }
    }
    output[globalIndex] = 0;
    if ( norm )
    {
       sum /= norm;
       output[globalIndex] = sum;
    }
}


__kernel void Convolve25D_Color(
    __global float *input,
    __global float *mask,
    __global float *output,
    const int2 imageSize,
    const int2 maskSize,
    const int2 maskOrigin,
    const int4 colorMask,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    float passThrough_red = input[ globalIndex * 4 + 1];
    float passThrough_green = input[ globalIndex * 4 + 2];
    float passThrough_blue = input[ globalIndex * 4 + 3];
    float sum_red = 0;
    float sum_green = 0;
    float sum_blue = 0;
    float norm = 0;
    float maskVal = 0;
    float val = 0;
    for(int mx=0; mx<maskSize.x; mx++)
    {
        for(int my=0; my<maskSize.y; my++)
        {
            int mi = mul24(my, maskSize.x) + mx;
            int ix = gx - maskOrigin.x + mx;
            int iy = gy - maskOrigin.y + my;
            int i = gz * imageSize.x * imageSize.y + iy * imageSize.x + ix;
            if ( (ix >= 0) && (ix < imageSize.x) &&
                 (iy >= 0) && (iy < imageSize.y)    )
            {
               maskVal = mask[mi];
               
               if ( colorMask.y != 0 )
               {
                  val = input[i*4 + 1];
                  sum_red += val * maskVal;
               }
               if ( colorMask.z != 0 )
               {
                  val = input[i*4 + 2];
                  sum_green += val * maskVal;
               }
               if ( colorMask.w != 0 )
               {
                  val = input[i*4 + 3];
                  sum_blue += val * maskVal;
               }
               
               
               if ( maskVal >= 0 )
               {
                  norm += maskVal;
               }
               else
               {
                  norm += -maskVal;
               }
            }
         }
    }
    output[globalIndex * 4 + 0] = 1;
    output[globalIndex * 4 + 1] = 0;
    output[globalIndex * 4 + 2] = 0;
    output[globalIndex * 4 + 3] = 0;
    if ( norm )
    {
       if ( colorMask.y != 0 )
       {
          sum_red /= norm;
          output[globalIndex * 4 + 1] = sum_red;
       }
       else
       {
          output[globalIndex * 4 + 1] = passThrough_red;
       } 
       
       
       if ( colorMask.z != 0 )
       {
          sum_green /= norm;
          output[globalIndex * 4 + 2] = sum_green;
       }
       else
       {
          output[globalIndex * 4 + 2] = passThrough_green;
       } 
       
       
       if ( colorMask.w != 0 )
       {
          sum_blue /= norm;
          output[globalIndex * 4 + 3] = sum_blue;
       }
       else
       {
          output[globalIndex * 4 + 3] = passThrough_blue;
       } 
    }
}





__kernel void Convolve3D(
    __global float *input,
    __global float *mask,
    __global float *output,
    const int4 imageSize,
    const int4 maskSize,
    const int4 maskOrigin,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    float sum = 0;
    float norm = 0;
    float maskVal = 0;
    float val = 0;
    for(int mx=0; mx<maskSize.x; mx++)
    {
        for(int my=0; my<maskSize.y; my++)
        {
            for(int mz=0; mz<maskSize.z; mz++)
            {
                int mi = mz * maskSize.x * maskSize.y + my * maskSize.x + mx;
                int ix = gx - maskOrigin.x + mx;
                int iy = gy - maskOrigin.y + my;
                int iz = gz - maskOrigin.z + mz;
                int i = iz * imageSize.x * imageSize.y + iy * imageSize.x + ix;
                if ( (ix >= 0) && (ix < imageSize.x) &&
                     (iy >= 0) && (iy < imageSize.y) &&
                     (iz >= 0) && (iz < imageSize.z)    )
                {
                   val = input[i];
                   maskVal = mask[mi];
                   sum += val * maskVal;
               
                   if ( maskVal >= 0 )
                   {
                      norm += maskVal;
                   }
                   else
                   {
                      norm += -maskVal;
                   }
                }
             }
         }
    }
    output[globalIndex] = 0;
    if ( norm > 0 )
    {
       sum /= norm;
       output[globalIndex] = sum;
    }
}




__kernel void Convolve3D_Color(
    __global float *input,
    __global float *mask,
    __global float *output,
    const int4 imageSize,
    const int4 maskSize,
    const int4 maskOrigin,
    const int4 colorMask,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    float passThrough_red = input[ globalIndex * 4 + 1];
    float passThrough_green = input[ globalIndex * 4 + 2];
    float passThrough_blue = input[ globalIndex * 4 + 3];
    float sum_red = 0;
    float sum_green = 0;
    float sum_blue = 0;
    
    float maskVal = 0;
    float norm = 0;
    float val = 0;
    for(int mx=0; mx<maskSize.x; mx++)
    {
        for(int my=0; my<maskSize.y; my++)
        {
            for(int mz=0; mz<maskSize.z; mz++)
            {
                int mi = mz * maskSize.x * maskSize.y + my * maskSize.x + mx;
                int ix = gx - maskOrigin.x + mx;
                int iy = gy - maskOrigin.y + my;
                int iz = gz - maskOrigin.z + mz;
                int i = iz * imageSize.x * imageSize.y + iy * imageSize.x + ix;
                if ( (ix >= 0) && (ix < imageSize.x) &&
                     (iy >= 0) && (iy < imageSize.y) &&
                     (iz >= 0) && (iz < imageSize.z)    )
                {
                   val = input[i];
                   maskVal = mask[mi];
                   
               
                   if ( colorMask.y != 0 )
                   {
                      val = input[i*4 + 1];
                      sum_red += val * maskVal;
                   }
                   if ( colorMask.z != 0 )
                   {
                      val = input[i*4 + 2];
                      sum_green += val * maskVal;
                   }
                   if ( colorMask.w != 0 )
                   {
                      val = input[i*4 + 3];
                      sum_blue += val * maskVal;
                   }
               
                   if ( maskVal >= 0 )
                   {
                      norm += maskVal;
                   }
                   else
                   {
                      norm += -maskVal;
                   }
                }
             }
         }
    }
    
    output[globalIndex * 4 + 0] = 1;
    output[globalIndex * 4 + 1] = 0;
    output[globalIndex * 4 + 2] = 0;
    output[globalIndex * 4 + 3] = 0;
    if ( norm > 0 )
    {
       if ( colorMask.y != 0 )
       {
          sum_red /= norm;
          output[globalIndex * 4 + 1] = sum_red;
       }
       else
       {
          output[globalIndex * 4 + 1] = passThrough_red;
       } 
       
       
       if ( colorMask.z != 0 )
       {
          sum_green /= norm;
          output[globalIndex * 4 + 2] = sum_green;
       }
       else
       {
          output[globalIndex * 4 + 2] = passThrough_green;
       } 
       
       
       if ( colorMask.w != 0 )
       {
          sum_blue /= norm;
          output[globalIndex * 4 + 3] = sum_blue;
       }
       else
       {
          output[globalIndex * 4 + 3] = passThrough_blue;
       } 
    }
}


__kernel void convolveX(
    __global float *input,
    __global float *dX,
    __global float *output,
    const int2 imageSize,
    const int maskSize,
    const int maskOrigin,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    float sumX = 0;
    float normX = 0;
    float dXVal = 0;
    float val = 0;
    for(int mx=0; mx<maskSize; mx++)
    {
        int mi = mx;
        int ix = gx - maskOrigin + mx;
        int i = gz * imageSize.x * imageSize.y + gy * imageSize.x + ix;
        if ( (ix >= 0) && (ix < imageSize.x) )
        {
           val = input[i];
           dXVal = dX[mi];
           sumX += val * dXVal;
           if ( dXVal >= 0 )
           {
              normX += dXVal;
           }
           else
           {
              normX += -dXVal;
           }
        }
    }
    output[globalIndex] = 0;
    if ( normX > 0 )
    {
       sumX /= normX;
       output[globalIndex] = sumX;
    }
}



__kernel void convolveX_color(
    __global float *input,
    __global float *dX,
    __global float *output,
    const int2 imageSize,
    const int maskSize,
    const int maskOrigin,
    const int4 colorMask,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    float passThrough_red = input[ globalIndex * 4 + 1];
    float passThrough_green = input[ globalIndex * 4 + 2];
    float passThrough_blue = input[ globalIndex * 4 + 3];
    
    float sumX_red = 0;            
    float sumX_green = 0;        
    float sumX_blue = 0;
    
    float normX = 0;    
    float dXVal = 0;    
    float val = 0;
    for(int mx=0; mx<maskSize; mx++)
    {
        int mi = mx;
        int ix = gx - maskOrigin + mx;
        int i = gz * imageSize.x * imageSize.y + gy * imageSize.x + ix;
        if ( (ix >= 0) && (ix < imageSize.x) )
        {
           dXVal = dX[mi];           
               
           if ( colorMask.y != 0 )
           {
              val = input[i*4 + 1];
              sumX_red += val * dXVal;
           }                
           if ( colorMask.z != 0 )
           {
              val = input[i*4 + 2];
              sumX_green += val * dXVal;
           }                
           if ( colorMask.w != 0 )
           {
              val = input[i*4 + 3];
              sumX_blue += val * dXVal;
           }
           
           if ( dXVal >= 0 )
           {
              normX += dXVal;
           }
           else
           {
              normX += -dXVal;
           }
        }
    }
    output[globalIndex * 4 + 0] = 1;
    output[globalIndex * 4 + 1] = 0;
    output[globalIndex * 4 + 2] = 0;
    output[globalIndex * 4 + 3] = 0;
    if ( normX > 0 )
    {
       if ( colorMask.y != 0 )
       {
          sumX_red /= normX;
          output[globalIndex * 4 + 1] = sumX_red;
       }
       else
       {
          output[globalIndex * 4 + 1] = passThrough_red;
       } 
       
       
       if ( colorMask.z != 0 )
       {
          sumX_green /= normX;
          output[globalIndex * 4 + 2] = sumX_green;
       }
       else
       {
          output[globalIndex * 4 + 2] = passThrough_green;
       } 
       
       
       if ( colorMask.w != 0 )
       {
          sumX_blue /= normX;
          output[globalIndex * 4 + 3] = sumX_blue;
       }
       else
       {
          output[globalIndex * 4 + 3] = passThrough_blue;
       } 
    }
}


__kernel void convolveY(
    __global float *input,
    __global float *dY,
    __global float *output,
    const int2 imageSize,
    const int maskSize,
    const int maskOrigin,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    float sumY = 0;
    float normY = 0;
    float dYVal = 0;
    float val = 0;
    for(int my=0; my<maskSize; my++)
    {
        int mi = my;
        int ix = gx;
        int iy = gy - maskOrigin + my;
        int i = gz * imageSize.x * imageSize.y + iy * imageSize.x + ix;
        if ( (iy >= 0) && (iy < imageSize.y) )
        {
           val = input[i];
           dYVal = dY[mi];
           sumY += val * dYVal;
           if ( dYVal >= 0 )
           {
              normY += dYVal;
           }
           else
           {
              normY += -dYVal;
           }
       }
    }
    output[globalIndex] = 0;
    if ( normY > 0 )
    {
       sumY /= normY;
       output[globalIndex] = sumY;
    }
}


__kernel void convolveY_color(
    __global float *input,
    __global float *dY,
    __global float *output,
    const int2 imageSize,
    const int maskSize,
    const int maskOrigin,
    const int4 colorMask,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    float passThrough_red = input[ globalIndex * 4 + 1];
    float passThrough_green = input[ globalIndex * 4 + 2];
    float passThrough_blue = input[ globalIndex * 4 + 3];
    
    float sumY_red = 0;            
    float sumY_green = 0;        
    float sumY_blue = 0;
    
    float normY = 0;    
    float dYVal = 0;
    float val = 0;
    for(int my=0; my<maskSize; my++)
    {
        int mi = my;
        int ix = gx;
        int iy = gy - maskOrigin + my;
        int i = gz * imageSize.x * imageSize.y + iy * imageSize.x + ix;
        if ( (iy >= 0) && (iy < imageSize.y) )
        {
           dYVal = dY[mi];           
               
           if ( colorMask.y != 0 )
           {
              val = input[i*4 + 1];
              sumY_red += val * dYVal;
           }                
           if ( colorMask.z != 0 )
           {
              val = input[i*4 + 2];
              sumY_green += val * dYVal;
           }                
           if ( colorMask.w != 0 )
           {
              val = input[i*4 + 3];
              sumY_blue += val * dYVal;
           }
           
           if ( dYVal >= 0 )
           {
              normY += dYVal;
           }
           else
           {
              normY += -dYVal;
           }
       }
    }
    output[globalIndex * 4 + 0] = 1;
    output[globalIndex * 4 + 1] = 0;
    output[globalIndex * 4 + 2] = 0;
    output[globalIndex * 4 + 3] = 0;
    if ( normY > 0 )
    {
       if ( colorMask.y != 0 )
       {
          sumY_red /= normY;
          output[globalIndex * 4 + 1] = sumY_red;
       }
       else
       {
          output[globalIndex * 4 + 1] = passThrough_red;
       } 
       
       
       if ( colorMask.z != 0 )
       {
          sumY_green /= normY;
          output[globalIndex * 4 + 2] = sumY_green;
       }
       else
       {
          output[globalIndex * 4 + 2] = passThrough_green;
       } 
       
       
       if ( colorMask.w != 0 )
       {
          sumY_blue /= normY;
          output[globalIndex * 4 + 3] = sumY_blue;
       }
       else
       {
          output[globalIndex * 4 + 3] = passThrough_blue;
       }
    }
}


__kernel void convolveZ(
    __global float *input,
    __global float *dZ,
    __global float *output,
    const int4 imageSize,
    const int maskSize,
    const int maskOrigin,
    const int clipZKernel,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
	if ( (clipZKernel != 0) && ((slice < (maskSize/2)) || (slice >= (imageSize.z - (maskSize/2)))) )
	{
		output[globalIndex] = input[globalIndex];
		return;
	} 
    float sumZ = 0;
    float normZ = 0;
    float dZVal = 0;
    float val = 0;
    for(int mz=0; mz<maskSize; mz++)
    {
        int mi = mz;
        int ix = gx;
        int iy = gy;
        int iz = gz - maskOrigin + mz;
        int i = iz * imageSize.x * imageSize.y + iy * imageSize.x + ix;
        if ( (iz >= 0) && (iz < imageSize.z) )
        {
           val = input[i];
           dZVal = dZ[mi];
           sumZ += val * dZVal;
           if ( dZVal >= 0 )
           {
              normZ += dZVal;
           }
           else
           {
              normZ += -dZVal;
           }
       }
    }
    output[globalIndex] = 0;
    if ( normZ > 0 )
    {
       sumZ /= normZ;
       output[globalIndex] = sumZ;
    }
}


__kernel void convolveZ_color(
    __global float *input,
    __global float *dZ,
    __global float *output,
    const int4 imageSize,
    const int maskSize,
    const int maskOrigin,
    const int4 colorMask,
    const int clipZKernel,
    const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
	if ( (clipZKernel != 0) && ((slice < (maskSize/2)) || (slice >= (imageSize.z - (maskSize/2)))) )
	{
			output[globalIndex * 4 + 0] = input[globalIndex * 4 + 0];
			output[globalIndex * 4 + 1] = input[globalIndex * 4 + 1];
			output[globalIndex * 4 + 2] = input[globalIndex * 4 + 2];
			output[globalIndex * 4 + 3] = input[globalIndex * 4 + 3];
			return;
	} 
	    float passThrough_red = input[ globalIndex * 4 + 1];
	    float passThrough_green = input[ globalIndex * 4 + 2];
	    float passThrough_blue = input[ globalIndex * 4 + 3];
	    
	    float sumZ_red = 0;            
	    float sumZ_green = 0;        
	    float sumZ_blue = 0;

	    float normZ = 0;
	    float dZVal = 0;
	    float val = 0;
	    for(int mz=0; mz<maskSize; mz++)
	    {
	        int mi = mz;
	        int ix = gx;
	        int iy = gy;
	        int iz = gz - maskOrigin + mz;
	        int i = iz * imageSize.x * imageSize.y + iy * imageSize.x + ix;
	        if ( (iz >= 0) && (iz < imageSize.z) )
	        {
	           dZVal = dZ[mi];
	           
	           if ( colorMask.y != 0 )
	           {
	           		val = input[i * 4 + 1];
	           		sumZ_red += val * dZVal;
	           }
	           if ( colorMask.z != 0 )
	           {
	           		val = input[i * 4 + 2];
	           		sumZ_green += val * dZVal;
	           }
	           if ( colorMask.w != 0 )
	           {
	           		val = input[i * 4 + 3];
	           		sumZ_blue += val * dZVal;
	           }
	           
	           if ( dZVal >= 0 )
	           {
	              normZ += dZVal;
	           }
	           else
	           {
	              normZ += -dZVal;
	           }
	       }
	    }
	    output[globalIndex * 4 + 0] = 1;
	    output[globalIndex * 4 + 1] = 0;
	    output[globalIndex * 4 + 2] = 0;
	    output[globalIndex * 4 + 3] = 0;
	    if ( normZ > 0 )
	    {
	       if ( colorMask.y != 0 )
	       {
	          sumZ_red /= normZ;
	          output[globalIndex * 4 + 1] = sumZ_red;
	       }
	       else
	       {
	          output[globalIndex * 4 + 1] = passThrough_red;
	       } 
	       
	       
	       if ( colorMask.z != 0 )
	       {
	          sumZ_green /= normZ;
	          output[globalIndex * 4 + 2] = sumZ_green;
	       }
	       else
	       {
	          output[globalIndex * 4 + 2] = passThrough_green;
	       } 
	       
	       
	       if ( colorMask.w != 0 )
	       {
	          sumZ_blue /= normZ;
	          output[globalIndex * 4 + 3] = sumZ_blue;
	       }
	       else
	       {
	          output[globalIndex * 4 + 3] = passThrough_blue;
	       }
	    }
    
}


__kernel void magnitude25D_color(

    __global float *inputX,
    __global float *inputY,
    __global float *output,
    const int4 imageSize,
    const int4 colorMask,
    const int slice )
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    
    output[globalIndex * 4 + 0] = 1;
    if ( colorMask.y != 0 )
    {
        output[globalIndex * 4 + 1] = sqrt( (inputX[globalIndex * 4 + 1] * inputX[globalIndex * 4 + 1]) +
                                    (inputY[globalIndex * 4 + 1] * inputY[globalIndex * 4 + 1]) );
    }
    else
    {
        output[globalIndex * 4 + 1] = inputX[globalIndex * 4 + 1];
    }
    
    
    if ( colorMask.z != 0 )
    {
        output[globalIndex * 4 + 2] = sqrt( (inputX[globalIndex * 4 + 2] * inputX[globalIndex * 4 + 2]) +
                                    (inputY[globalIndex * 4 + 2] * inputY[globalIndex * 4 + 2]) );
    }
    else
    {
        output[globalIndex * 4 + 2] = inputX[globalIndex * 4 + 2];
    }
    
    
    if ( colorMask.w != 0 )
    {
        output[globalIndex * 4 + 3] = sqrt( (inputX[globalIndex * 4 + 3] * inputX[globalIndex * 4 + 3]) +
                                    (inputY[globalIndex * 4 + 3] * inputY[globalIndex * 4 + 3]) );
    }
    else
    {
        output[globalIndex * 4 + 3] = inputX[globalIndex * 4 + 3];
    }
}

__kernel void magnitude25D(

    __global float *inputX,
    __global float *inputY,
    __global float *output,
    const int4 imageSize,
    const int slice )
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    output[globalIndex] = sqrt( (inputX[globalIndex] * inputX[globalIndex]) +
                                (inputY[globalIndex] * inputY[globalIndex]) );
}
    


__kernel void magnitude(

    __global float *inputX,
    __global float *inputY,
    __global float *inputZ,
    __global float *output,
    const int4 imageSize,
    const int maskSize,
    const int slice )
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
	if ( (slice < (maskSize/2)) || (slice >= (imageSize.z - (maskSize/2))) )
	{
        output[globalIndex] = sqrt( (inputX[globalIndex] * inputX[globalIndex]) +
                                    (inputY[globalIndex] * inputY[globalIndex]) );
	} 
	else
	{
    	output[globalIndex] = sqrt( (inputX[globalIndex] * inputX[globalIndex]) +
                                    (inputY[globalIndex] * inputY[globalIndex]) + 
                                    (inputZ[globalIndex] * inputZ[globalIndex]) );
	}
}
    


__kernel void magnitude_color(

    __global float *inputX,
    __global float *inputY,
    __global float *inputZ,
    __global float *output,
    const int4 imageSize,
    const int maskSize,
    const int4 colorMask,
    const int slice )
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
	if ( (slice < (maskSize/2)) || (slice >= (imageSize.z - (maskSize/2))) )
	{
	    output[globalIndex * 4 + 0] = 1;
        if ( colorMask.y != 0 )
        {
            output[globalIndex * 4 + 1] = sqrt( (inputX[globalIndex * 4 + 1] * inputX[globalIndex * 4 + 1]) +
                                                (inputY[globalIndex * 4 + 1] * inputY[globalIndex * 4 + 1]) );
        }
        else
        {
            output[globalIndex * 4 + 1] = inputX[globalIndex * 4 + 1];
        }
    
    
        if ( colorMask.z != 0 )
        {
            output[globalIndex * 4 + 2] = sqrt( (inputX[globalIndex * 4 + 2] * inputX[globalIndex * 4 + 2]) +
                                                (inputY[globalIndex * 4 + 2] * inputY[globalIndex * 4 + 2]) );
        }
        else
        {
            output[globalIndex * 4 + 2] = inputX[globalIndex * 4 + 2];
        }
    
    
        if ( colorMask.w != 0 )
        {
            output[globalIndex * 4 + 3] = sqrt( (inputX[globalIndex * 4 + 3] * inputX[globalIndex * 4 + 3]) +
                                                (inputY[globalIndex * 4 + 3] * inputY[globalIndex * 4 + 3]) );
        }
        else
        {
            output[globalIndex * 4 + 3] = inputX[globalIndex * 4 + 3];
        }
	} 
	else
	{
	    output[globalIndex * 4 + 0] = 1;
        if ( colorMask.y != 0 )
        {
            output[globalIndex * 4 + 1] = sqrt( (inputX[globalIndex * 4 + 1] * inputX[globalIndex * 4 + 1]) +
                                                (inputY[globalIndex * 4 + 1] * inputY[globalIndex * 4 + 1])  +
                                                (inputZ[globalIndex * 4 + 1] * inputZ[globalIndex * 4 + 1]) );
        }
        else
        {
            output[globalIndex * 4 + 1] = inputX[globalIndex * 4 + 1];
        }
    
    
        if ( colorMask.z != 0 )
        {
            output[globalIndex * 4 + 2] = sqrt( (inputX[globalIndex * 4 + 2] * inputX[globalIndex * 4 + 2]) +
                                                (inputY[globalIndex * 4 + 2] * inputY[globalIndex * 4 + 2])  +
                                                (inputZ[globalIndex * 4 + 2] * inputZ[globalIndex * 4 + 2]) );
        }
        else
        {
            output[globalIndex * 4 + 2] = inputX[globalIndex * 4 + 2];
        }
    
    
        if ( colorMask.w != 0 )
        {
            output[globalIndex * 4 + 3] = sqrt( (inputX[globalIndex * 4 + 3] * inputX[globalIndex * 4 + 3]) +
                                                (inputY[globalIndex * 4 + 3] * inputY[globalIndex * 4 + 3])  +
                                                (inputZ[globalIndex * 4 + 3] * inputZ[globalIndex * 4 + 3]) );
        }
        else
        {
            output[globalIndex * 4 + 3] = inputX[globalIndex * 4 + 3];
        }
	}
}
    


__kernel void negSum25D(

    __global float *inputX,
    __global float *inputY,
    __global float *output,
    const int4 imageSize,
    const int slice )
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    output[globalIndex] = - ( inputX[globalIndex] + inputY[globalIndex] );
}
    


__kernel void negSum(

    __global float *inputX,
    __global float *inputY,
    __global float *inputZ,
    __global float *output,
    const int4 imageSize,
    const int slice )
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
	output[globalIndex] = - ( inputX[globalIndex] + inputY[globalIndex] + inputZ[globalIndex] );
}
    


__kernel void gradientMagnitude25D(

    __global float *input,
    __global float *output,
    const int4 imageSize,
    const int totalSize,
    const int slice )
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int gz = slice;
    int globalIndex = gz * imageSize.x * imageSize.y + gy * imageSize.x + gx;
    float gradX = 0, gradY = 0;
    if ( (gx == 0) && (globalIndex + 1 < totalSize) )
    {
       gradX = 2 * input[globalIndex + 1] - input[globalIndex];
    }
    else if ( (gx == imageSize.x - 1) && (globalIndex - 1 >= 0) )
    {
       gradX = 2 * input[globalIndex] - input[globalIndex - 1];
    }
    else if ( globalIndex + 1 < totalSize && globalIndex - 1 >= 0 )
    {
       gradX = input[globalIndex + 1] - input[globalIndex - 1];
    }
    if ( gy == 0 )
    {
       int indexY = gz * imageSize.x * imageSize.y + (gy + 1) * imageSize.x + gx;
       if ( indexY < totalSize )
       {
          gradY = 2 * (input[indexY] - input[globalIndex]);
       }
    }
    else if ( gy == imageSize.y - 1 )
    {
       int indexY  = gz * imageSize.x * imageSize.y + (gy - 1) * imageSize.x + gx;
       if ( indexY >= 0 )
       {
          gradY = 2 * (input[globalIndex] - input[indexY]);
       }
    }
    else
    {
       int indexY1 = gz * imageSize.x * imageSize.y + (gy + 1) * imageSize.x + gx;    
       int indexY2  = gz * imageSize.x * imageSize.y + (gy - 1) * imageSize.x + gx;
       if ( indexY1 < totalSize &&  indexY2 >= 0 )
       {
          gradY = 2 * (input[indexY1] - input[indexY2]);
       }
    }
    
    output[globalIndex] = sqrt( gradX * gradX + gradY * gradY );
}
    

