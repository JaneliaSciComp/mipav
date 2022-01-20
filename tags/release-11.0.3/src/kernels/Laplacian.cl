

__kernel void Laplacian25D(
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
       output[globalIndex] = -( sumX + sumY );
    }
}


__kernel void Laplacian3D(
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
       output[globalIndex] = -( sumX + sumY + sumZ );
    }
}