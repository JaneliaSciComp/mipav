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
           sumX += (val * dXVal);
        }
    }
    output[globalIndex] = sumX;
    if ( maskSize == 0 )
    {
       output[globalIndex] = input[globalIndex];
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
        }
    }
       if ( colorMask.y != 0 )
       {
          output[globalIndex * 4 + 1] = sumX_red;
       }
       else
       {
          output[globalIndex * 4 + 1] = passThrough_red;
       } 
       
       
       if ( colorMask.z != 0 )
       {
          output[globalIndex * 4 + 2] = sumX_green;
       }
       else
       {
          output[globalIndex * 4 + 2] = passThrough_green;
       } 
       
       
       if ( colorMask.w != 0 )
       {
          output[globalIndex * 4 + 3] = sumX_blue;
       }
       else
       {
          output[globalIndex * 4 + 3] = passThrough_blue;
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
       }
    }
    output[globalIndex] =sumY;
    if ( maskSize == 0 )
    {
       output[globalIndex] = input[globalIndex];
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
       }
    }
       if ( colorMask.y != 0 )
       {
          output[globalIndex * 4 + 1] = sumY_red;
       }
       else
       {
          output[globalIndex * 4 + 1] = passThrough_red;
       } 
       
       
       if ( colorMask.z != 0 )
       {
          output[globalIndex * 4 + 2] = sumY_green;
       }
       else
       {
          output[globalIndex * 4 + 2] = passThrough_green;
       } 
       
       
       if ( colorMask.w != 0 )
       {
          output[globalIndex * 4 + 3] = sumY_blue;
       }
       else
       {
          output[globalIndex * 4 + 3] = passThrough_blue;
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
       }
    }
    output[globalIndex] = sumZ;
    if ( maskSize == 0 )
    {
       output[globalIndex] = input[globalIndex];
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
	           
	       }
	    }
	       if ( colorMask.y != 0 )
	       {
	          output[globalIndex * 4 + 1] = sumZ_red;
	       }
	       else
	       {
	          output[globalIndex * 4 + 1] = passThrough_red;
	       } 
	       
	       
	       if ( colorMask.z != 0 )
	       {
	          output[globalIndex * 4 + 2] = sumZ_green;
	       }
	       else
	       {
	          output[globalIndex * 4 + 2] = passThrough_green;
	       } 
	       
	       
	       if ( colorMask.w != 0 )
	       {
	          output[globalIndex * 4 + 3] = sumZ_blue;
	       }
	       else
	       {
	          output[globalIndex * 4 + 3] = passThrough_blue;
	       }
    
}




__kernel void convolveZMult(
    __global float *input,
    __global float *dZ,
    __global float *output,
    __global float *original,
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
		output[globalIndex] = original[globalIndex] * input[globalIndex];
		return;
	} 
    float sumZ = 0;
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
       }
    }
    output[globalIndex] = original[globalIndex] * sumZ;
    if ( maskSize == 0 )
    {
       output[globalIndex] = original[globalIndex] * input[globalIndex];
    }
}


__kernel void convolveZMult_color(
    __global float *input,
    __global float *dZ,
    __global float *output,
    __global float *original,
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
			output[globalIndex * 4 + 0] = original[globalIndex * 4 + 0] * input[globalIndex * 4 + 0];
			output[globalIndex * 4 + 1] = original[globalIndex * 4 + 1] * input[globalIndex * 4 + 1];
			output[globalIndex * 4 + 2] = original[globalIndex * 4 + 2] * input[globalIndex * 4 + 2];
			output[globalIndex * 4 + 3] = original[globalIndex * 4 + 3] * input[globalIndex * 4 + 3];
			return;
	} 
	    float passThrough_red = input[ globalIndex * 4 + 1];
	    float passThrough_green = input[ globalIndex * 4 + 2];
	    float passThrough_blue = input[ globalIndex * 4 + 3];
	    
	    float sumZ_red = 0;            
	    float sumZ_green = 0;        
	    float sumZ_blue = 0;

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
	       }
	    }
	       if ( colorMask.y != 0 )
	       {
	          output[globalIndex * 4 + 1] = original[globalIndex * 4 + 1] * sumZ_red;
	       }
	       else
	       {
	          output[globalIndex * 4 + 1] = original[globalIndex * 4 + 1] * passThrough_red;
	       } 
	       
	       
	       if ( colorMask.z != 0 )
	       {
	          output[globalIndex * 4 + 2] = original[globalIndex * 4 + 2] * sumZ_green;
	       }
	       else
	       {
	          output[globalIndex * 4 + 2] = original[globalIndex * 4 + 2] * passThrough_green;
	       } 
	       
	       
	       if ( colorMask.w != 0 )
	       {
	          output[globalIndex * 4 + 3] = original[globalIndex * 4 + 3] * sumZ_blue;
	       }
	       else
	       {
	          output[globalIndex * 4 + 3] = original[globalIndex * 4 + 3] * passThrough_blue;
	       }
}





__kernel void convolveZDiv(
    __global float *input,
    __global float *dZ,
    __global float *output,
    __global float *original,
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
		output[globalIndex] = original[globalIndex] / input[globalIndex];
		return;
	} 
    float sumZ = 0;
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
       }
    }
    if ( sumZ != 0 )
    {
       output[globalIndex] = original[globalIndex] / sumZ;
    }
    else
    {
       output[globalIndex] = 0;
    }
    if ( maskSize == 0 )
    {
       output[globalIndex] = original[globalIndex] / input[globalIndex];
    }
}




__kernel void convolveZDiv_color(
    __global float *input,
    __global float *dZ,
    __global float *output,
    __global float *original,
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
			output[globalIndex * 4 + 0] = original[globalIndex * 4 + 0] / input[globalIndex * 4 + 0];
			output[globalIndex * 4 + 1] = original[globalIndex * 4 + 1] / input[globalIndex * 4 + 1];
			output[globalIndex * 4 + 2] = original[globalIndex * 4 + 2] / input[globalIndex * 4 + 2];
			output[globalIndex * 4 + 3] = original[globalIndex * 4 + 3] / input[globalIndex * 4 + 3];
			return;
	} 
	    float passThrough_red = input[ globalIndex * 4 + 1];
	    float passThrough_green = input[ globalIndex * 4 + 2];
	    float passThrough_blue = input[ globalIndex * 4 + 3];
	    
	    float sumZ_red = 0;            
	    float sumZ_green = 0;        
	    float sumZ_blue = 0;

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
	       }
	    }
	       if ( colorMask.y != 0 )
	       {
	          if ( sumZ_red != 0 )
	          {
	             output[globalIndex * 4 + 1] = original[globalIndex * 4 + 1] / sumZ_red;
	          }
	          else
	          {
	             output[globalIndex * 4 + 1] = 0;
	          }
	       }
	       else
	       {
	          output[globalIndex * 4 + 1] = original[globalIndex * 4 + 1] / passThrough_red;
	       } 
	       
	       
	       if ( colorMask.z != 0 )
	       {
	          if ( sumZ_green != 0 )
	          {
	             output[globalIndex * 4 + 2] = original[globalIndex * 4 + 2] / sumZ_green;
	          }
	          else
	          {
	             output[globalIndex * 4 + 2] = 0;
	          }
	       }
	       else
	       {
	          output[globalIndex * 4 + 2] = original[globalIndex * 4 + 2] / passThrough_green;
	       } 
	       
	       
	       if ( colorMask.w != 0 )
	       {
	          if ( sumZ_blue != 0 )
	          {
	             output[globalIndex * 4 + 3] = original[globalIndex * 4 + 3] / sumZ_blue;
	          }
	          else
	          {
	             output[globalIndex * 4 + 3] = 0;
	          }
	       }
	       else
	       {
	          output[globalIndex * 4 + 3] = original[globalIndex * 4 + 3] / passThrough_blue;
	       }
}