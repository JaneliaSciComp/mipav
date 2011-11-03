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

const sampler_t samplerA = CLK_NORMALIZED_COORDS_FALSE | CLK_ADDRESS_REPEAT | CLK_FILTER_LINEAR;
kernel void NormalKernelShared(__read_only image3d_t inImage, global float* b, int width, int height, int depth, int color, int numElements) {
		int x = get_global_id(0); 
		int y = get_global_id(1); 
		int z = get_global_id(2); 
		int pos = z * width * height + y * width + x; 
			// bound check, equivalent to the limit on a 'for' loop
		if (pos >= numElements)  {
		    return;
		}			

			// The default:
		int4 coord3D = { x, y, z, 0 };
		float4 val =  read_imagef( inImage, samplerA, coord3D );
			
		int4 index[14];
		index[0] = (int4){ (x-1), (y-1), z, 0 };
		index[1] = (int4){ (x+1), (y-1), z, 0 };
		index[2] = (int4){ (x-1), y, z, 0 };
		index[3] = (int4){ (x+1), y, z, 0 };
		index[4] = (int4){ (x-1), (y+1), z, 0 };
		index[5] = (int4){ (x+1), (y+1), z, 0 };
		index[6] = (int4){ x, (y-1), z, 0 };
		index[7] = (int4){ x, (y+1), z, 0 };
		index[8] = (int4){ (x-1), y, (z-1), 0 };
		index[9] = (int4){ (x-1), y, (z+1), 0 };
		index[10] = (int4){ x, y, (z-1), 0 };
		index[11] = (int4){ x, y, (z+1), 0 };
		index[12] = (int4){ (x+1), y, (z-1), 0 };
		index[13] = (int4){ (x+1), y, (z+1), 0 };

		float4 tempVal;
		float values[14];
		for ( int i = 0; i < 14; i++ ) {
		   values[i] = (val.x + val.y + val.z)/3.0;;
		   int newPos = index[i].z * width * height + index[i].y * width + index[i].x; 
		   if ( (newPos < numElements) && (newPos >= 0)  ) {
		      tempVal = read_imagef( inImage, samplerA, index[i] );
		      values[i] = tempVal.x;
		      if ( color == 4 ) {
		         values[i] = (tempVal.x + tempVal.y + tempVal.z)/3.0;
		      }
		   }
		}
		float4 normal = (float4)(0.71 * (values[0] - values[1]) + (values[2] - values[3]) + .71 * (values[4] - values[5]),
		                 0.71 * (values[0] - values[4]) + (values[6] - values[7]) + .71 * (values[1] - values[5]),
		                 0.71 * (values[8] - values[9]) + (values[10] - values[11]) + .71 * (values[12] - values[13]), 0);
			//+ "normal = normalize(normal);"
			// add the vector elements
		b[pos*4 + 0] = 1;
		b[pos*4 + 1] = 0.5 + normal.x / 8.0;
		b[pos*4 + 2] = 0.5 + normal.y / 8.0;
		b[pos*4 + 3] = 0.5 + normal.z / 8.0;
};
	