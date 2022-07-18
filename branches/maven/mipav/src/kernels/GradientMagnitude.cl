

__kernel void gradientMagnitude25D(
    __global float *input,
    __global float *dX,
    __global float *dY,
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
    float sumX = 0;
    float sumY = 0;
    float normX = 0;
    float normY = 0;
    float dXVal = 0;
    float dYVal = 0;
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
               dXVal = dX[mi];
               dYVal = dY[mi];
               sumX += val * dXVal;
               sumY += val * dYVal;
               
               if ( dXVal >= 0 )
               {
                  normX += dXVal;
               }
               else
               {
                  normX += -dXVal;
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
    }
    output[globalIndex] = 0;
    if ( (normX > 0) && (normY > 0) )
    {
       sumX /= normX;
       sumY /= normY;
       output[globalIndex] = sqrt( sumX*sumX + sumY*sumY );
    }
}


__kernel void gradientMagnitude25D_Color(
    __global float *input,
    __global float *dX,
    __global float *dY,
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
    float sumX_red = 0;
    float sumY_red = 0;
    float sumX_green = 0;
    float sumY_green = 0;
    float sumX_blue = 0;
    float sumY_blue = 0;
    float normX = 0;
    float normY = 0;
    float dXVal = 0;
    float dYVal = 0;
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
               dXVal = dX[mi];
               dYVal = dY[mi];
               
               if ( colorMask.y != 0 )
               {
                  val = input[i*4 + 1];
                  sumX_red += val * dXVal;
                  sumY_red += val * dYVal;
               }
               if ( colorMask.z != 0 )
               {
                  val = input[i*4 + 2];
                  sumX_green += val * dXVal;
                  sumY_green += val * dYVal;
               }
               if ( colorMask.w != 0 )
               {
                  val = input[i*4 + 3];
                  sumX_blue += val * dXVal;
                  sumY_blue += val * dYVal;
               }
               
               
               if ( dXVal >= 0 )
               {
                  normX += dXVal;
               }
               else
               {
                  normX += -dXVal;
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
    }
    output[globalIndex * 4 + 0] = 1;
    output[globalIndex * 4 + 1] = 0;
    output[globalIndex * 4 + 2] = 0;
    output[globalIndex * 4 + 3] = 0;
    if ( (normX > 0) && (normY > 0) )
    {
       if ( colorMask.y != 0 )
       {
          sumX_red /= normX;
          sumY_red /= normY;
          output[globalIndex * 4 + 1] = sqrt( sumX_red*sumX_red + sumY_red*sumY_red );
       }
       else
       {
          output[globalIndex * 4 + 1] = passThrough_red;
       } 
       
       
       if ( colorMask.z != 0 )
       {
          sumX_green /= normX;
          sumY_green /= normY;
          output[globalIndex * 4 + 2] = sqrt( sumX_green*sumX_green + sumY_green*sumY_green );
       }
       else
       {
          output[globalIndex * 4 + 2] = passThrough_green;
       } 
       
       
       if ( colorMask.w != 0 )
       {
          sumX_blue /= normX;
          sumY_blue /= normY;
          output[globalIndex * 4 + 3] = sqrt( sumX_blue*sumX_blue + sumY_blue*sumY_blue );
       }
       else
       {
          output[globalIndex * 4 + 3] = passThrough_blue;
       } 
    }
}





__kernel void gradientMagnitude3D(
    __global float *input,
    __global float *dX,
    __global float *dY,
    __global float *dZ,
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
    float sumX = 0;
    float sumY = 0;
    float sumZ = 0;
    float normX = 0;
    float normY = 0;
    float normZ = 0;
    float dXVal = 0;
    float dYVal = 0;
    float dZVal = 0;
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
                   dXVal = dX[mi];
                   dYVal = dY[mi];
                   dZVal = dZ[mi];
                   sumX += val * dXVal;
                   sumY += val * dYVal;
                   sumZ += val * dZVal;
               
                   if ( dXVal >= 0 )
                   {
                      normX += dXVal;
                   }
                   else
                   {
                      normX += -dXVal;
                   }
               
                   if ( dYVal >= 0 )
                   {
                      normY += dYVal;
                   }
                   else
                   {
                      normY += -dYVal;
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
         }
    }
    output[globalIndex] = 0;
    if ( (normX > 0) && (normY > 0) && (normZ > 0) )
    {
       sumX /= normX;
       sumY /= normY;
       sumZ /= normZ;
       output[globalIndex] = sqrt( sumX*sumX + sumY*sumY + sumZ*sumZ );
    }
}




__kernel void gradientMagnitude3D_Color(
    __global float *input,
    __global float *dX,
    __global float *dY,
    __global float *dZ,
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
    float sumX_red = 0;
    float sumY_red = 0;
    float sumZ_red = 0;
    float sumX_green = 0;
    float sumY_green = 0;
    float sumZ_green = 0;
    float sumX_blue = 0;
    float sumY_blue = 0;
    float sumZ_blue = 0;
    
    float normX = 0;
    float normY = 0;
    float normZ = 0;
    float dXVal = 0;
    float dYVal = 0;
    float dZVal = 0;
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
                   dXVal = dX[mi];
                   dYVal = dY[mi];
                   dZVal = dZ[mi];
                   
               
                   if ( colorMask.y != 0 )
                   {
                      val = input[i*4 + 1];
                      sumX_red += val * dXVal;
                      sumY_red += val * dYVal;
                      sumZ_red += val * dZVal;
                   }
                   if ( colorMask.z != 0 )
                   {
                      val = input[i*4 + 2];
                      sumX_green += val * dXVal;
                      sumY_green += val * dYVal;
                      sumZ_green += val * dZVal;
                   }
                   if ( colorMask.w != 0 )
                   {
                      val = input[i*4 + 3];
                      sumX_blue += val * dXVal;
                      sumY_blue += val * dYVal;
                      sumZ_blue += val * dZVal;
                   }
               
                   if ( dXVal >= 0 )
                   {
                      normX += dXVal;
                   }
                   else
                   {
                      normX += -dXVal;
                   }
               
                   if ( dYVal >= 0 )
                   {
                      normY += dYVal;
                   }
                   else
                   {
                      normY += -dYVal;
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
         }
    }
    
    output[globalIndex * 4 + 0] = 1;
    output[globalIndex * 4 + 1] = 0;
    output[globalIndex * 4 + 2] = 0;
    output[globalIndex * 4 + 3] = 0;
    if ( (normX > 0) && (normY > 0)  && (normZ > 0) )
    {
       if ( colorMask.y != 0 )
       {
          sumX_red /= normX;
          sumY_red /= normY;
          sumZ_red /= normZ;
          output[globalIndex * 4 + 1] = sqrt( sumX_red*sumX_red + sumY_red*sumY_red + sumZ_red*sumZ_red );
       }
       else
       {
          output[globalIndex * 4 + 1] = passThrough_red;
       } 
       
       
       if ( colorMask.z != 0 )
       {
          sumX_green /= normX;
          sumY_green /= normY;
          sumZ_green /= normZ;
          output[globalIndex * 4 + 2] = sqrt( sumX_green*sumX_green + sumY_green*sumY_green + sumZ_green*sumZ_green );
       }
       else
       {
          output[globalIndex * 4 + 2] = passThrough_green;
       } 
       
       
       if ( colorMask.w != 0 )
       {
          sumX_blue /= normX;
          sumY_blue /= normY;
          sumZ_blue /= normZ;
          output[globalIndex * 4 + 3] = sqrt( sumX_blue*sumX_blue + sumY_blue*sumY_blue + sumZ_blue*sumZ_blue );
       }
       else
       {
          output[globalIndex * 4 + 3] = passThrough_blue;
       } 
    }
}