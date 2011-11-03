

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
    float passThrough_green = input[ globalIndex * 4 + 1];
    float passThrough_blue = input[ globalIndex * 4 + 1];
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
          output[globalIndex * 4 + 3] = passThrough_green;
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
    float passThrough_green = input[ globalIndex * 4 + 1];
    float passThrough_blue = input[ globalIndex * 4 + 1];
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
          output[globalIndex * 4 + 3] = passThrough_green;
       } 
    }
}