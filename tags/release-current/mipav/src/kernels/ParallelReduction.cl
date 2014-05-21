
__kernel void reduceSum(
__global int *input,
__global float *output, 
unsigned long n, 
unsigned long blockSize, 
__local volatile float* sdata
){
    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = get_local_id(0);
    unsigned int i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);

    sdata[tid] = (i < n) ? input[i] : 0;
    i += blockSize; 
    while (i < n) 
    {
        sdata[tid] += input[i];  
        i += blockSize; 
    }
    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    if (blockSize >= 512) { if (tid < 256) { sdata[tid] += sdata[tid + 256]; } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 256) { if (tid < 128) { sdata[tid] += sdata[tid + 128]; } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 128) { if (tid <  64) { sdata[tid] += sdata[tid +  64]; } barrier(CLK_LOCAL_MEM_FENCE); }
    
    if (tid < 32)
    {
        if (blockSize >=  64) { sdata[tid] += sdata[tid + 32]; }
        if (blockSize >=  32) { sdata[tid] += sdata[tid + 16]; }
        if (blockSize >=  16) { sdata[tid] += sdata[tid +  8]; }
        if (blockSize >=   8) { sdata[tid] += sdata[tid +  4]; }
        if (blockSize >=   4) { sdata[tid] += sdata[tid +  2]; }
        if (blockSize >=   2) { sdata[tid] += sdata[tid +  1]; }
    }
    
    // write result for this block to global mem 
    if (tid == 0) output[get_group_id(0)] = sdata[0];
}

__kernel void reduceMax(
__global float *input,
__global float *output, 
unsigned long n, 
unsigned long blockSize, 
__local volatile float* sdata
){
    // perform first level of reduction,
    // reading from global memory, writing to shared memory
    unsigned int tid = get_local_id(0);
    unsigned int i = get_group_id(0)*(get_local_size(0)*2) + get_local_id(0);

    sdata[tid] = (i < n) ? input[i] : 0;
    i += blockSize; 
    while (i < n) 
    {
        sdata[tid] = max( sdata[tid], input[i] );  
        i += blockSize; 
    }
    barrier(CLK_LOCAL_MEM_FENCE);

    // do reduction in shared mem
    if (blockSize >= 512) { if (tid < 256) { sdata[tid] = max( sdata[tid], sdata[tid + 256]); } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 256) { if (tid < 128) { sdata[tid] = max( sdata[tid], sdata[tid + 128]); } barrier(CLK_LOCAL_MEM_FENCE); }
    if (blockSize >= 128) { if (tid <  64) { sdata[tid] = max( sdata[tid], sdata[tid +  64]); } barrier(CLK_LOCAL_MEM_FENCE); }
    
    if (tid < 32)
    {
        if (blockSize >=  64) { sdata[tid] = max( sdata[tid], sdata[tid + 32]); }
        if (blockSize >=  32) { sdata[tid] = max( sdata[tid], sdata[tid + 16]); }
        if (blockSize >=  16) { sdata[tid] = max( sdata[tid], sdata[tid +  8]); }
        if (blockSize >=   8) { sdata[tid] = max( sdata[tid], sdata[tid +  4]); }
        if (blockSize >=   4) { sdata[tid] = max( sdata[tid], sdata[tid +  2]); }
        if (blockSize >=   2) { sdata[tid] = max( sdata[tid], sdata[tid +  1]); }
    }
    
    // write result for this block to global mem 
    if (tid == 0) output[get_group_id(0)] = sdata[0];
}
