__kernel void NormalKernel25D(
	    __global float *input,
	    __global float *output,
	    const int numElements,
	    const int width,
	    const int height,
	    const int slice)
	{
	    int x = get_global_id(0);
	    int y = get_global_id(1);
	    int z = slice;
	    if ((x >= width) ||
	        (y >= height))
	    {
        return;
     }
     int sliceSize = width*height;
     int index[14];
     index[0] = z * sliceSize + (y-1)*width + (x-1);
     index[1] = z * sliceSize + (y-1)*width + (x+1);
     index[2] = z * sliceSize + y*width + (x-1);
     index[3] = z * sliceSize + y*width + (x+1);
     index[4] = z * sliceSize + (y+1)*width + (x-1);
     index[5] = z * sliceSize + (y+1)*width + (x+1);
     index[6] = z * sliceSize + (y-1)*width + x;
     index[7] = z * sliceSize + (y+1)*width + x;
     index[8] = (z-1) * sliceSize + y*width + (x-1);
     index[9] = (z+1) * sliceSize + y*width + (x-1);
     index[10] = (z-1) * sliceSize + y*width + x;
     index[11] = (z+1) * sliceSize + y*width + x;
     index[12] = (z-1) * sliceSize + y*width + (x+1);
     index[13] = (z+1) * sliceSize + y*width + (x+1);
	// The default:
	 int currentIndex = z * sliceSize + y * width + x;
     float val = input[currentIndex];
     float values[14];
     for ( int i = 0; i < 14; i++ ) {
        values[i] = val;
        if ( (index[i] < numElements) && (index[i] >= 0)  ) {
           values[i] = input[index[i]];
        }
     }
     float4 normal = (float4)(0.71 * (values[0] - values[1]) + (values[2] - values[3]) + .71 * (values[4] - values[5]),
                      0.71 * (values[0] - values[4]) + (values[6] - values[7]) + .71 * (values[1] - values[5]),
                      0.71 * (values[8] - values[9]) + (values[10] - values[11]) + .71 * (values[12] - values[13]), 0);
     normal = normalize(normal);
	// add the vector elements
     output[currentIndex*4 + 0] = 1;
     output[currentIndex*4 + 1] = normal.x;
     output[currentIndex*4 + 2] = normal.y;
     output[currentIndex*4 + 3] = normal.z;
};		

__kernel void NormalKernel25DColor(
	    __global float *input,
	    __global float *output,
	    const int numElements,
	    const int width,
	    const int height,
	    const int slice)
	{
	    int x = get_global_id(0);
	    int y = get_global_id(1);
	    int z = slice;
	    if ((x >= width) ||
	        (y >= height))
	    {
        return;
     }
     int sliceSize = width*height;
     int index[14];
     index[0] = z * sliceSize + (y-1)*width + (x-1);
     index[1] = z * sliceSize + (y-1)*width + (x+1);
     index[2] = z * sliceSize + y*width + (x-1);
     index[3] = z * sliceSize + y*width + (x+1);
     index[4] = z * sliceSize + (y+1)*width + (x-1);
     index[5] = z * sliceSize + (y+1)*width + (x+1);
     index[6] = z * sliceSize + (y-1)*width + x;
     index[7] = z * sliceSize + (y+1)*width + x;
     index[8] = (z-1) * sliceSize + y*width + (x-1);
     index[9] = (z+1) * sliceSize + y*width + (x-1);
     index[10] = (z-1) * sliceSize + y*width + x;
     index[11] = (z+1) * sliceSize + y*width + x;
     index[12] = (z-1) * sliceSize + y*width + (x+1);
     index[13] = (z+1) * sliceSize + y*width + (x+1);
	// The default:
	 int currentIndex = z * sliceSize + y * width + x;
     float val = (input[currentIndex * 4 + 1] + input[currentIndex * 4 + 2] + input[currentIndex * 4 + 3]) / 3.0;
     float values[14];
     for ( int i = 0; i < 14; i++ ) {
        values[i] = val;
        if ( (index[i] < numElements) && (index[i] >= 0)  ) {
           values[i] = (input[index[i] * 4 + 1] + input[index[i] * 4 + 2] + input[index[i] * 4 + 3]) / 3.0;
        }
     }
     float4 normal = (float4)(0.71 * (values[0] - values[1]) + (values[2] - values[3]) + .71 * (values[4] - values[5]),
                      0.71 * (values[0] - values[4]) + (values[6] - values[7]) + .71 * (values[1] - values[5]),
                      0.71 * (values[8] - values[9]) + (values[10] - values[11]) + .71 * (values[12] - values[13]), 0);
     normal = normalize(normal);
	// add the vector elements
     output[currentIndex*4 + 0] = 1;
     output[currentIndex*4 + 1] = normal.x;
     output[currentIndex*4 + 2] = normal.y;
     output[currentIndex*4 + 3] = normal.z;
};		

__kernel void NormalKernel25DSlices(
	    __global float *input,
	    __global float *output,
	    const int numElements,
	    const int width,
	    const int height)
{
	 int x = get_global_id(0);
	 int y = get_global_id(1);
	 int z = 1;
	 if ((x >= width) ||
	     (y >= height))
	 {
        return;
     }
     int sliceSize = width*height;
     int index[14];
     index[0] = z * sliceSize + (y-1)*width + (x-1);
     index[1] = z * sliceSize + (y-1)*width + (x+1);
     index[2] = z * sliceSize + y*width + (x-1);
     index[3] = z * sliceSize + y*width + (x+1);
     index[4] = z * sliceSize + (y+1)*width + (x-1);
     index[5] = z * sliceSize + (y+1)*width + (x+1);
     index[6] = z * sliceSize + (y-1)*width + x;
     index[7] = z * sliceSize + (y+1)*width + x;
     index[8] = (z-1) * sliceSize + y*width + (x-1);
     index[9] = (z+1) * sliceSize + y*width + (x-1);
     index[10] = (z-1) * sliceSize + y*width + x;
     index[11] = (z+1) * sliceSize + y*width + x;
     index[12] = (z-1) * sliceSize + y*width + (x+1);
     index[13] = (z+1) * sliceSize + y*width + (x+1);
	// The default:
	 int currentIndex = z * sliceSize + y * width + x;
     float val = input[currentIndex];
     float values[14];
     for ( int i = 0; i < 14; i++ ) {
        values[i] = val;
        if ( (index[i] < numElements) && (index[i] >= 0)  ) {
           values[i] = input[index[i]];
        }
     }
     float4 normal = (float4)(0.71 * (values[0] - values[1]) + (values[2] - values[3]) + .71 * (values[4] - values[5]),
                      0.71 * (values[0] - values[4]) + (values[6] - values[7]) + .71 * (values[1] - values[5]),
                      0.71 * (values[8] - values[9]) + (values[10] - values[11]) + .71 * (values[12] - values[13]), 0);
     normal = normalize(normal);
	// add the vector elements
	 currentIndex = y * width + x;
     output[currentIndex*4 + 0] = 1;
     output[currentIndex*4 + 1] = normal.x;
     output[currentIndex*4 + 2] = normal.y;
     output[currentIndex*4 + 3] = normal.z;
};		


__kernel void NormalKernel25DSlicesColor(
	    __global float *input,
	    __global float *output,
	    const int numElements,
	    const int width,
	    const int height)
{
	 int x = get_global_id(0);
	 int y = get_global_id(1);
	 int z = 1;
	 if ((x >= width) ||
	     (y >= height))
	 {
        return;
     }
     int sliceSize = width*height;
     int index[14];
     index[0] = z * sliceSize + (y-1)*width + (x-1);
     index[1] = z * sliceSize + (y-1)*width + (x+1);
     index[2] = z * sliceSize + y*width + (x-1);
     index[3] = z * sliceSize + y*width + (x+1);
     index[4] = z * sliceSize + (y+1)*width + (x-1);
     index[5] = z * sliceSize + (y+1)*width + (x+1);
     index[6] = z * sliceSize + (y-1)*width + x;
     index[7] = z * sliceSize + (y+1)*width + x;
     index[8] = (z-1) * sliceSize + y*width + (x-1);
     index[9] = (z+1) * sliceSize + y*width + (x-1);
     index[10] = (z-1) * sliceSize + y*width + x;
     index[11] = (z+1) * sliceSize + y*width + x;
     index[12] = (z-1) * sliceSize + y*width + (x+1);
     index[13] = (z+1) * sliceSize + y*width + (x+1);
	// The default:
	 int currentIndex = z * sliceSize + y * width + x;
     float val = (input[currentIndex * 4 + 1] + input[currentIndex * 4 + 2] + input[currentIndex * 4 + 3]) / 3.0;
     float values[14];
     float count = 0;
     float sum = 0;
     for ( int i = 0; i < 14; i++ ) {
        values[i] = val;
        for ( int j = 1; j < 4 ; j++ )
        {
           if ( (index[i] * 4 + j) < numElements && (index[i] * 4 + j) >= 0 )
           {
              sum += input[index[i] * 4 + j];
              count += 1;
           }
        }
        if ( count > 0 )
        {
           values[i]  = sum / count;
        }
     }
     float4 normal = (float4)(0.71 * (values[0] - values[1]) + (values[2] - values[3]) + .71 * (values[4] - values[5]),
                      0.71 * (values[0] - values[4]) + (values[6] - values[7]) + .71 * (values[1] - values[5]),
                      0.71 * (values[8] - values[9]) + (values[10] - values[11]) + .71 * (values[12] - values[13]), 0);
     normal = normalize(normal);
	// add the vector elements
	 currentIndex = y * width + x;
     output[currentIndex*4 + 0] = 1;
     output[currentIndex*4 + 1] = normal.x;
     output[currentIndex*4 + 2] = normal.y;
     output[currentIndex*4 + 3] = normal.z;
};		

