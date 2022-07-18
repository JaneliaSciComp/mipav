
// The number of threads to use for triangle generation (limited by shared memory size)
#define NTHREADS 32


// classify voxel based on number of vertices it will generate
// one thread per voxel
__kernel
void
classifyVoxel(
__global int *voxelVerts, 
__global int *voxelOccupied,  
__global float *input,
__global int *table,
const int4 imageSize,
const int numVoxels,
const float isoValue,
const int slice)
{
    int gx = get_global_id(0);
    int gy = get_global_id(1);
    int sliceSize = imageSize.x*imageSize.y;
    int gz = slice;
    
    // read field values at neighbouring grid vertices
    float field[8];
    int globalIndex = gz * sliceSize + gy * imageSize.x + gx;  
    if ( globalIndex < numVoxels ) {
       field[0] = input[globalIndex];
    }
    globalIndex = gz * sliceSize + gy * imageSize.x + gx+1;  
    if ( globalIndex < numVoxels ) {
       field[1] = input[globalIndex];
    }
    globalIndex = gz * sliceSize + (gy+1) * imageSize.x + gx+1; 
    if ( globalIndex < numVoxels ) {
       field[2] = input[globalIndex];
    }
    globalIndex = gz * sliceSize + (gy+1) * imageSize.x + gx; 
    if ( globalIndex < numVoxels ) {
       field[3] = input[globalIndex];
    }
    globalIndex = (gz+1) * sliceSize + gy * imageSize.x + gx; 
    if ( globalIndex < numVoxels ) {
       field[4] = input[globalIndex];
    }
    globalIndex = (gz+1) * sliceSize + gy * imageSize.x + gx+1; 
    if ( globalIndex < numVoxels ) {
       field[5] = input[globalIndex];
    }
    globalIndex = (gz+1) * sliceSize + (gy+1) * imageSize.x + gx+1; 
    if ( globalIndex < numVoxels ) {
       field[6] = input[globalIndex];
    }
    globalIndex = (gz+1) * sliceSize + (gy+1) * imageSize.x + gx; 
    if ( globalIndex < numVoxels ) {
       field[7] = input[globalIndex];
    }

    // calculate flag indicating if each vertex is inside or outside isosurface
    int cubeindex;
	cubeindex =  (field[0] < isoValue); 
	cubeindex += (field[1] < isoValue)*2; 
	cubeindex += (field[2] < isoValue)*4; 
	cubeindex += (field[3] < isoValue)*8; 
	cubeindex += (field[4] < isoValue)*16; 
	cubeindex += (field[5] < isoValue)*32; 
	cubeindex += (field[6] < isoValue)*64; 
	cubeindex += (field[7] < isoValue)*128;

    // read number of vertices from texture
    int numVerts = table[cubeindex];

    globalIndex = gz * sliceSize + gy * imageSize.x + gx;  
    if (globalIndex < numVoxels) {
        voxelVerts[globalIndex] = numVerts;
        voxelOccupied[globalIndex] = (numVerts > 0);
    }
}

// compact voxel array
__kernel void compactVoxels(
__global int *compactedVoxelArray, 
__global int *voxelOccupied, 
__global int *voxelOccupiedScan, 
int numVoxels)
{
    int i = get_global_id(0);

    if (voxelOccupied[i] && (i < numVoxels)) {
        compactedVoxelArray[ voxelOccupiedScan[i] ] = i;
    }
}


// compute interpolated vertex along an edge
float4 vertexInterp(float isolevel, float4 p0, float4 p1, float f0, float f1)
{
    float t = (isolevel - f0) / (f1 - f0);
	return mix(p0, p1, t);
} 

// version that calculates flat surface normal for each triangle
__kernel
void
generateTriangles2(
__global float4 *pos,
__global int *compactedVoxelArray, 
__global int *numVertsScanned, 
__global float *input,
__global int *numVertsTable,
__global int *triTable,
const int4 imageSize,
const int numVoxels,
const float4 voxelSize, 
const float isoValue, 
const int activeVoxels, 
const int maxVerts)
{
    int i = get_global_id(0);
    int tid = get_local_id(0);

    if (i > activeVoxels - 1) {
        i = activeVoxels - 1;
    }

    int voxel = compactedVoxelArray[i];
    
    int sizeX = imageSize.x;
    int sizeY = imageSize.y;
    
    
    int temp = voxel;
    int gx = temp % sizeX;
    temp -= gx;
    temp /= sizeX;
    
    int gy = temp % sizeY;
    temp -= gy;
    temp /= sizeY;
    
    int gz = temp;
    int sliceSize = imageSize.x*imageSize.y;
    //int gx = get_global_id(0);
    //int gy = get_global_id(1);
    //int gz = slice;
    //int i = gz * sliceSize + gy * imageSize.x + gx;  
    
    // read field values at neighbouring grid vertices
    float field[8];
    int globalIndex = gz * sliceSize + gy * imageSize.x + gx;  
    if ( globalIndex < numVoxels ) {
       field[0] = input[globalIndex];
    }
    globalIndex = gz * sliceSize + gy * imageSize.x + gx+1;  
    if ( globalIndex < numVoxels ) {
       field[1] = input[globalIndex];
    }
    globalIndex = gz * sliceSize + (gy+1) * imageSize.x + gx+1; 
    if ( globalIndex < numVoxels ) {
       field[2] = input[globalIndex];
    }
    globalIndex = gz * sliceSize + (gy+1) * imageSize.x + gx; 
    if ( globalIndex < numVoxels ) {
       field[3] = input[globalIndex];
    }
    globalIndex = (gz+1) * sliceSize + gy * imageSize.x + gx; 
    if ( globalIndex < numVoxels ) {
       field[4] = input[globalIndex];
    }
    globalIndex = (gz+1) * sliceSize + gy * imageSize.x + gx+1; 
    if ( globalIndex < numVoxels ) {
       field[5] = input[globalIndex];
    }
    globalIndex = (gz+1) * sliceSize + (gy+1) * imageSize.x + gx+1; 
    if ( globalIndex < numVoxels ) {
       field[6] = input[globalIndex];
    }
    globalIndex = (gz+1) * sliceSize + (gy+1) * imageSize.x + gx; 
    if ( globalIndex < numVoxels ) {
       field[7] = input[globalIndex];
    }

    // calculate flag indicating if each vertex is inside or outside isosurface
    int cubeindex;
	cubeindex =  (field[0] < isoValue); 
	cubeindex += (field[1] < isoValue)*2; 
	cubeindex += (field[2] < isoValue)*4; 
	cubeindex += (field[3] < isoValue)*8; 
	cubeindex += (field[4] < isoValue)*16; 
	cubeindex += (field[5] < isoValue)*32; 
	cubeindex += (field[6] < isoValue)*64; 
	cubeindex += (field[7] < isoValue)*128;
    

    float4 p;
    p.x = -1.0f + (gx * voxelSize.x);
    p.y = -1.0f + (gy * voxelSize.y);
    p.z = -1.0f + (gz * voxelSize.z);
    p.w = 1.0f;

    // calculate cell vertex positions
    float4 v[8];
    v[0] = p;
    v[1] = p + (float4)(voxelSize.x, 0, 0,0);
    v[2] = p + (float4)(voxelSize.x, voxelSize.y, 0,0);
    v[3] = p + (float4)(0, voxelSize.y, 0,0);
    v[4] = p + (float4)(0, 0, voxelSize.z,0);
    v[5] = p + (float4)(voxelSize.x, 0, voxelSize.z,0);
    v[6] = p + (float4)(voxelSize.x, voxelSize.y, voxelSize.z,0);
    v[7] = p + (float4)(0, voxelSize.y, voxelSize.z,0);


	// find the vertices where the surface intersects the cube 
	__local float4 vertlist[16*NTHREADS];

	vertlist[tid] = vertexInterp(isoValue, v[0], v[1], field[0], field[1]);
    vertlist[NTHREADS+tid] = vertexInterp(isoValue, v[1], v[2], field[1], field[2]);
    vertlist[(NTHREADS*2)+tid] = vertexInterp(isoValue, v[2], v[3], field[2], field[3]);
    vertlist[(NTHREADS*3)+tid] = vertexInterp(isoValue, v[3], v[0], field[3], field[0]);
	vertlist[(NTHREADS*4)+tid] = vertexInterp(isoValue, v[4], v[5], field[4], field[5]);
    vertlist[(NTHREADS*5)+tid] = vertexInterp(isoValue, v[5], v[6], field[5], field[6]);
    vertlist[(NTHREADS*6)+tid] = vertexInterp(isoValue, v[6], v[7], field[6], field[7]);
    vertlist[(NTHREADS*7)+tid] = vertexInterp(isoValue, v[7], v[4], field[7], field[4]);
	vertlist[(NTHREADS*8)+tid] = vertexInterp(isoValue, v[0], v[4], field[0], field[4]);
    vertlist[(NTHREADS*9)+tid] = vertexInterp(isoValue, v[1], v[5], field[1], field[5]);
    vertlist[(NTHREADS*10)+tid] = vertexInterp(isoValue, v[2], v[6], field[2], field[6]);
    vertlist[(NTHREADS*11)+tid] = vertexInterp(isoValue, v[3], v[7], field[3], field[7]);
    barrier(CLK_LOCAL_MEM_FENCE);

    // output triangle vertices
    int numVerts = numVertsTable[cubeindex];

    float4 add;
    add.x = 1; add.y = 1; add.z = 1; add.w = 0;
    float4 scale;
    scale.x = .5 * imageSize.x; scale.y = .5 *imageSize.y; scale.z = .5 *imageSize.z; scale.w = 0;
    for(int i=0; i<numVerts; i+=3) {
        uint index = numVertsScanned[voxel] + i;

        float4 v[3];
        uint edge;
        edge = triTable[cubeindex*16+i];
        v[0] = vertlist[(edge*NTHREADS)+tid];

        edge = triTable[cubeindex*16+i+1];
        v[1] = vertlist[(edge*NTHREADS)+tid];

        edge = triTable[cubeindex*16+i+2];
        v[2] = vertlist[(edge*NTHREADS)+tid];

        // calculate triangle surface normal
        //float4 n = calcNormal(v[0], v[1], v[2]);

        if (index < (maxVerts - 3)) {
            //pos[index] = v[0];
            pos[index] = (v[0] + add)*scale;
            //pos[index].x = index;
            //norm[index] = n;

            //pos[index+1] = v[1];
            pos[index+1] = (v[1] + add)*scale;
            //pos[index+1].x = index+1;
            //norm[index+1] = n;

            //pos[index+2] = v[2];
            pos[index+2] = (v[2] + add)*scale;
            //pos[index+2].x = index+2;
            //norm[index+2] = n;
        }
    }
}

