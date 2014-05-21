package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


import static org.jocl.CL.CL_DEVICE_GLOBAL_MEM_SIZE;
import static org.jocl.CL.CL_DEVICE_MAX_MEM_ALLOC_SIZE;
import static org.jocl.CL.CL_DEVICE_MAX_WORK_GROUP_SIZE;
import static org.jocl.CL.CL_MEM_COPY_HOST_PTR;
import static org.jocl.CL.CL_TRUE;
import static org.jocl.CL.clBuildProgram;
import static org.jocl.CL.clCreateBuffer;
import static org.jocl.CL.clCreateCommandQueue;
import static org.jocl.CL.clCreateKernel;
import static org.jocl.CL.clCreateProgramWithSource;
import static org.jocl.CL.clEnqueueNDRangeKernel;
import static org.jocl.CL.clEnqueueReadBuffer;
import static org.jocl.CL.clFinish;
import static org.jocl.CL.clReleaseMemObject;
import static org.jocl.CL.clSetKernelArg;
import static org.jocl.CL.stringFor_errorCode;
import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;

import java.io.IOException;
import java.util.Vector;

import org.jocl.CL;
import org.jocl.Pointer;
import org.jocl.Sizeof;
import org.jocl.cl_command_queue;
import org.jocl.cl_kernel;
import org.jocl.cl_mem;
import org.jocl.cl_program;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Meshes.TriangleKey;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;
import WildMagic.LibImagics.Extraction.ExtractSurfaceCubes;


public class OpenCLAlgorithmMarchingCubes extends OpenCLAlgorithmBase
{

	private float isoValue;  
	
	/** If true then the input image is blurred slightly. */
    private final boolean blurFlag;

    /** The amount to blur to smooth surface. */
    private float blurSigma = 0.5f;

    /**
     * If true then the extracted surface is decimated into a continuous level of detail surface (clod).
     */
    private final boolean decimateFlag;

    /**
     * Path and name of extracted surface file. ".sur" will be appended if necessary.
     */
    private String surfaceFileName;

	
	// edge table maps 8-bit flag representing which cube vertices are inside
	// the isosurface to 12-bit number indicating which edges are intersected
    private int[] edgeTable = new int[]{
		0x0  , 0x109, 0x203, 0x30a, 0x406, 0x50f, 0x605, 0x70c,
		0x80c, 0x905, 0xa0f, 0xb06, 0xc0a, 0xd03, 0xe09, 0xf00,
		0x190, 0x99 , 0x393, 0x29a, 0x596, 0x49f, 0x795, 0x69c,
		0x99c, 0x895, 0xb9f, 0xa96, 0xd9a, 0xc93, 0xf99, 0xe90,
		0x230, 0x339, 0x33 , 0x13a, 0x636, 0x73f, 0x435, 0x53c,
		0xa3c, 0xb35, 0x83f, 0x936, 0xe3a, 0xf33, 0xc39, 0xd30,
		0x3a0, 0x2a9, 0x1a3, 0xaa , 0x7a6, 0x6af, 0x5a5, 0x4ac,
		0xbac, 0xaa5, 0x9af, 0x8a6, 0xfaa, 0xea3, 0xda9, 0xca0,
		0x460, 0x569, 0x663, 0x76a, 0x66 , 0x16f, 0x265, 0x36c,
		0xc6c, 0xd65, 0xe6f, 0xf66, 0x86a, 0x963, 0xa69, 0xb60,
		0x5f0, 0x4f9, 0x7f3, 0x6fa, 0x1f6, 0xff , 0x3f5, 0x2fc,
		0xdfc, 0xcf5, 0xfff, 0xef6, 0x9fa, 0x8f3, 0xbf9, 0xaf0,
		0x650, 0x759, 0x453, 0x55a, 0x256, 0x35f, 0x55 , 0x15c,
		0xe5c, 0xf55, 0xc5f, 0xd56, 0xa5a, 0xb53, 0x859, 0x950,
		0x7c0, 0x6c9, 0x5c3, 0x4ca, 0x3c6, 0x2cf, 0x1c5, 0xcc ,
		0xfcc, 0xec5, 0xdcf, 0xcc6, 0xbca, 0xac3, 0x9c9, 0x8c0,
		0x8c0, 0x9c9, 0xac3, 0xbca, 0xcc6, 0xdcf, 0xec5, 0xfcc,
		0xcc , 0x1c5, 0x2cf, 0x3c6, 0x4ca, 0x5c3, 0x6c9, 0x7c0,
		0x950, 0x859, 0xb53, 0xa5a, 0xd56, 0xc5f, 0xf55, 0xe5c,
		0x15c, 0x55 , 0x35f, 0x256, 0x55a, 0x453, 0x759, 0x650,
		0xaf0, 0xbf9, 0x8f3, 0x9fa, 0xef6, 0xfff, 0xcf5, 0xdfc,
		0x2fc, 0x3f5, 0xff , 0x1f6, 0x6fa, 0x7f3, 0x4f9, 0x5f0,
		0xb60, 0xa69, 0x963, 0x86a, 0xf66, 0xe6f, 0xd65, 0xc6c,
		0x36c, 0x265, 0x16f, 0x66 , 0x76a, 0x663, 0x569, 0x460,
		0xca0, 0xda9, 0xea3, 0xfaa, 0x8a6, 0x9af, 0xaa5, 0xbac,
		0x4ac, 0x5a5, 0x6af, 0x7a6, 0xaa , 0x1a3, 0x2a9, 0x3a0,
		0xd30, 0xc39, 0xf33, 0xe3a, 0x936, 0x83f, 0xb35, 0xa3c,
		0x53c, 0x435, 0x73f, 0x636, 0x13a, 0x33 , 0x339, 0x230,
		0xe90, 0xf99, 0xc93, 0xd9a, 0xa96, 0xb9f, 0x895, 0x99c,
		0x69c, 0x795, 0x49f, 0x596, 0x29a, 0x393, 0x99 , 0x190,
		0xf00, 0xe09, 0xd03, 0xc0a, 0xb06, 0xa0f, 0x905, 0x80c,
		0x70c, 0x605, 0x50f, 0x406, 0x30a, 0x203, 0x109, 0x0
	};

	// triangle table maps same cube vertex index to a list of up to 5 triangles
	// which are built from the interpolated edge vertices
	private static final int X = 255;
	private int[] triTable = new int[] {
	    X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X,
		0, 8, 3, X, X, X, X, X, X, X, X, X, X, X, X, X,
		0, 1, 9, X, X, X, X, X, X, X, X, X, X, X, X, X,
		1, 8, 3, 9, 8, 1, X, X, X, X, X, X, X, X, X, X,
		1, 2, 10, X, X, X, X, X, X, X, X, X, X, X, X, X,
		0, 8, 3, 1, 2, 10, X, X, X, X, X, X, X, X, X, X,
		9, 2, 10, 0, 2, 9, X, X, X, X, X, X, X, X, X, X,
		2, 8, 3, 2, 10, 8, 10, 9, 8, X, X, X, X, X, X, X,
		3, 11, 2, X, X, X, X, X, X, X, X, X, X, X, X, X,
		0, 11, 2, 8, 11, 0, X, X, X, X, X, X, X, X, X, X,
		1, 9, 0, 2, 3, 11, X, X, X, X, X, X, X, X, X, X,
		1, 11, 2, 1, 9, 11, 9, 8, 11, X, X, X, X, X, X, X,
		3, 10, 1, 11, 10, 3, X, X, X, X, X, X, X, X, X, X,
		0, 10, 1, 0, 8, 10, 8, 11, 10, X, X, X, X, X, X, X,
		3, 9, 0, 3, 11, 9, 11, 10, 9, X, X, X, X, X, X, X,
		9, 8, 10, 10, 8, 11, X, X, X, X, X, X, X, X, X, X,
		4, 7, 8, X, X, X, X, X, X, X, X, X, X, X, X, X,
		4, 3, 0, 7, 3, 4, X, X, X, X, X, X, X, X, X, X,
		0, 1, 9, 8, 4, 7, X, X, X, X, X, X, X, X, X, X,
		4, 1, 9, 4, 7, 1, 7, 3, 1, X, X, X, X, X, X, X,
		1, 2, 10, 8, 4, 7, X, X, X, X, X, X, X, X, X, X,
		3, 4, 7, 3, 0, 4, 1, 2, 10, X, X, X, X, X, X, X,
		9, 2, 10, 9, 0, 2, 8, 4, 7, X, X, X, X, X, X, X,
		2, 10, 9, 2, 9, 7, 2, 7, 3, 7, 9, 4, X, X, X, X,
		8, 4, 7, 3, 11, 2, X, X, X, X, X, X, X, X, X, X,
		11, 4, 7, 11, 2, 4, 2, 0, 4, X, X, X, X, X, X, X,
		9, 0, 1, 8, 4, 7, 2, 3, 11, X, X, X, X, X, X, X,
		4, 7, 11, 9, 4, 11, 9, 11, 2, 9, 2, 1, X, X, X, X,
		3, 10, 1, 3, 11, 10, 7, 8, 4, X, X, X, X, X, X, X,
		1, 11, 10, 1, 4, 11, 1, 0, 4, 7, 11, 4, X, X, X, X,
		4, 7, 8, 9, 0, 11, 9, 11, 10, 11, 0, 3, X, X, X, X,
		4, 7, 11, 4, 11, 9, 9, 11, 10, X, X, X, X, X, X, X,
		9, 5, 4, X, X, X, X, X, X, X, X, X, X, X, X, X,
		9, 5, 4, 0, 8, 3, X, X, X, X, X, X, X, X, X, X,
		0, 5, 4, 1, 5, 0, X, X, X, X, X, X, X, X, X, X,
		8, 5, 4, 8, 3, 5, 3, 1, 5, X, X, X, X, X, X, X,
		1, 2, 10, 9, 5, 4, X, X, X, X, X, X, X, X, X, X,
		3, 0, 8, 1, 2, 10, 4, 9, 5, X, X, X, X, X, X, X,
		5, 2, 10, 5, 4, 2, 4, 0, 2, X, X, X, X, X, X, X,
		2, 10, 5, 3, 2, 5, 3, 5, 4, 3, 4, 8, X, X, X, X,
		9, 5, 4, 2, 3, 11, X, X, X, X, X, X, X, X, X, X,
		0, 11, 2, 0, 8, 11, 4, 9, 5, X, X, X, X, X, X, X,
		0, 5, 4, 0, 1, 5, 2, 3, 11, X, X, X, X, X, X, X,
		2, 1, 5, 2, 5, 8, 2, 8, 11, 4, 8, 5, X, X, X, X,
		10, 3, 11, 10, 1, 3, 9, 5, 4, X, X, X, X, X, X, X,
		4, 9, 5, 0, 8, 1, 8, 10, 1, 8, 11, 10, X, X, X, X,
		5, 4, 0, 5, 0, 11, 5, 11, 10, 11, 0, 3, X, X, X, X,
		5, 4, 8, 5, 8, 10, 10, 8, 11, X, X, X, X, X, X, X,
		9, 7, 8, 5, 7, 9, X, X, X, X, X, X, X, X, X, X,
		9, 3, 0, 9, 5, 3, 5, 7, 3, X, X, X, X, X, X, X,
		0, 7, 8, 0, 1, 7, 1, 5, 7, X, X, X, X, X, X, X,
		1, 5, 3, 3, 5, 7, X, X, X, X, X, X, X, X, X, X,
		9, 7, 8, 9, 5, 7, 10, 1, 2, X, X, X, X, X, X, X,
		10, 1, 2, 9, 5, 0, 5, 3, 0, 5, 7, 3, X, X, X, X,
		8, 0, 2, 8, 2, 5, 8, 5, 7, 10, 5, 2, X, X, X, X,
		2, 10, 5, 2, 5, 3, 3, 5, 7, X, X, X, X, X, X, X,
		7, 9, 5, 7, 8, 9, 3, 11, 2, X, X, X, X, X, X, X,
		9, 5, 7, 9, 7, 2, 9, 2, 0, 2, 7, 11, X, X, X, X,
		2, 3, 11, 0, 1, 8, 1, 7, 8, 1, 5, 7, X, X, X, X,
		11, 2, 1, 11, 1, 7, 7, 1, 5, X, X, X, X, X, X, X,
		9, 5, 8, 8, 5, 7, 10, 1, 3, 10, 3, 11, X, X, X, X,
		5, 7, 0, 5, 0, 9, 7, 11, 0, 1, 0, 10, 11, 10, 0, X,
		11, 10, 0, 11, 0, 3, 10, 5, 0, 8, 0, 7, 5, 7, 0, X,
		11, 10, 5, 7, 11, 5, X, X, X, X, X, X, X, X, X, X,
		10, 6, 5, X, X, X, X, X, X, X, X, X, X, X, X, X,
		0, 8, 3, 5, 10, 6, X, X, X, X, X, X, X, X, X, X,
		9, 0, 1, 5, 10, 6, X, X, X, X, X, X, X, X, X, X,
		1, 8, 3, 1, 9, 8, 5, 10, 6, X, X, X, X, X, X, X,
		1, 6, 5, 2, 6, 1, X, X, X, X, X, X, X, X, X, X,
		1, 6, 5, 1, 2, 6, 3, 0, 8, X, X, X, X, X, X, X,
		9, 6, 5, 9, 0, 6, 0, 2, 6, X, X, X, X, X, X, X,
		5, 9, 8, 5, 8, 2, 5, 2, 6, 3, 2, 8, X, X, X, X,
		2, 3, 11, 10, 6, 5, X, X, X, X, X, X, X, X, X, X,
		11, 0, 8, 11, 2, 0, 10, 6, 5, X, X, X, X, X, X, X,
		0, 1, 9, 2, 3, 11, 5, 10, 6, X, X, X, X, X, X, X,
		5, 10, 6, 1, 9, 2, 9, 11, 2, 9, 8, 11, X, X, X, X,
		6, 3, 11, 6, 5, 3, 5, 1, 3, X, X, X, X, X, X, X,
		0, 8, 11, 0, 11, 5, 0, 5, 1, 5, 11, 6, X, X, X, X,
		3, 11, 6, 0, 3, 6, 0, 6, 5, 0, 5, 9, X, X, X, X,
		6, 5, 9, 6, 9, 11, 11, 9, 8, X, X, X, X, X, X, X,
		5, 10, 6, 4, 7, 8, X, X, X, X, X, X, X, X, X, X,
		4, 3, 0, 4, 7, 3, 6, 5, 10, X, X, X, X, X, X, X,
		1, 9, 0, 5, 10, 6, 8, 4, 7, X, X, X, X, X, X, X,
		10, 6, 5, 1, 9, 7, 1, 7, 3, 7, 9, 4, X, X, X, X,
		6, 1, 2, 6, 5, 1, 4, 7, 8, X, X, X, X, X, X, X,
		1, 2, 5, 5, 2, 6, 3, 0, 4, 3, 4, 7, X, X, X, X,
		8, 4, 7, 9, 0, 5, 0, 6, 5, 0, 2, 6, X, X, X, X,
		7, 3, 9, 7, 9, 4, 3, 2, 9, 5, 9, 6, 2, 6, 9, X,
		3, 11, 2, 7, 8, 4, 10, 6, 5, X, X, X, X, X, X, X,
		5, 10, 6, 4, 7, 2, 4, 2, 0, 2, 7, 11, X, X, X, X,
		0, 1, 9, 4, 7, 8, 2, 3, 11, 5, 10, 6, X, X, X, X,
		9, 2, 1, 9, 11, 2, 9, 4, 11, 7, 11, 4, 5, 10, 6, X,
		8, 4, 7, 3, 11, 5, 3, 5, 1, 5, 11, 6, X, X, X, X,
		5, 1, 11, 5, 11, 6, 1, 0, 11, 7, 11, 4, 0, 4, 11, X,
		0, 5, 9, 0, 6, 5, 0, 3, 6, 11, 6, 3, 8, 4, 7, X,
		6, 5, 9, 6, 9, 11, 4, 7, 9, 7, 11, 9, X, X, X, X,
		10, 4, 9, 6, 4, 10, X, X, X, X, X, X, X, X, X, X,
		4, 10, 6, 4, 9, 10, 0, 8, 3, X, X, X, X, X, X, X,
		10, 0, 1, 10, 6, 0, 6, 4, 0, X, X, X, X, X, X, X,
		8, 3, 1, 8, 1, 6, 8, 6, 4, 6, 1, 10, X, X, X, X,
		1, 4, 9, 1, 2, 4, 2, 6, 4, X, X, X, X, X, X, X,
		3, 0, 8, 1, 2, 9, 2, 4, 9, 2, 6, 4, X, X, X, X,
		0, 2, 4, 4, 2, 6, X, X, X, X, X, X, X, X, X, X,
		8, 3, 2, 8, 2, 4, 4, 2, 6, X, X, X, X, X, X, X,
		10, 4, 9, 10, 6, 4, 11, 2, 3, X, X, X, X, X, X, X,
		0, 8, 2, 2, 8, 11, 4, 9, 10, 4, 10, 6, X, X, X, X,
		3, 11, 2, 0, 1, 6, 0, 6, 4, 6, 1, 10, X, X, X, X,
		6, 4, 1, 6, 1, 10, 4, 8, 1, 2, 1, 11, 8, 11, 1, X,
		9, 6, 4, 9, 3, 6, 9, 1, 3, 11, 6, 3, X, X, X, X,
		8, 11, 1, 8, 1, 0, 11, 6, 1, 9, 1, 4, 6, 4, 1, X,
		3, 11, 6, 3, 6, 0, 0, 6, 4, X, X, X, X, X, X, X,
		6, 4, 8, 11, 6, 8, X, X, X, X, X, X, X, X, X, X,
		7, 10, 6, 7, 8, 10, 8, 9, 10, X, X, X, X, X, X, X,
		0, 7, 3, 0, 10, 7, 0, 9, 10, 6, 7, 10, X, X, X, X,
		10, 6, 7, 1, 10, 7, 1, 7, 8, 1, 8, 0, X, X, X, X,
		10, 6, 7, 10, 7, 1, 1, 7, 3, X, X, X, X, X, X, X,
		1, 2, 6, 1, 6, 8, 1, 8, 9, 8, 6, 7, X, X, X, X,
		2, 6, 9, 2, 9, 1, 6, 7, 9, 0, 9, 3, 7, 3, 9, X,
		7, 8, 0, 7, 0, 6, 6, 0, 2, X, X, X, X, X, X, X,
		7, 3, 2, 6, 7, 2, X, X, X, X, X, X, X, X, X, X,
		2, 3, 11, 10, 6, 8, 10, 8, 9, 8, 6, 7, X, X, X, X,
		2, 0, 7, 2, 7, 11, 0, 9, 7, 6, 7, 10, 9, 10, 7, X,
		1, 8, 0, 1, 7, 8, 1, 10, 7, 6, 7, 10, 2, 3, 11, X,
		11, 2, 1, 11, 1, 7, 10, 6, 1, 6, 7, 1, X, X, X, X,
		8, 9, 6, 8, 6, 7, 9, 1, 6, 11, 6, 3, 1, 3, 6, X,
		0, 9, 1, 11, 6, 7, X, X, X, X, X, X, X, X, X, X,
		7, 8, 0, 7, 0, 6, 3, 11, 0, 11, 6, 0, X, X, X, X,
		7, 11, 6, X, X, X, X, X, X, X, X, X, X, X, X, X,
		7, 6, 11, X, X, X, X, X, X, X, X, X, X, X, X, X,
		3, 0, 8, 11, 7, 6, X, X, X, X, X, X, X, X, X, X,
		0, 1, 9, 11, 7, 6, X, X, X, X, X, X, X, X, X, X,
		8, 1, 9, 8, 3, 1, 11, 7, 6, X, X, X, X, X, X, X,
		10, 1, 2, 6, 11, 7, X, X, X, X, X, X, X, X, X, X,
		1, 2, 10, 3, 0, 8, 6, 11, 7, X, X, X, X, X, X, X,
		2, 9, 0, 2, 10, 9, 6, 11, 7, X, X, X, X, X, X, X,
		6, 11, 7, 2, 10, 3, 10, 8, 3, 10, 9, 8, X, X, X, X,
		7, 2, 3, 6, 2, 7, X, X, X, X, X, X, X, X, X, X,
		7, 0, 8, 7, 6, 0, 6, 2, 0, X, X, X, X, X, X, X,
		2, 7, 6, 2, 3, 7, 0, 1, 9, X, X, X, X, X, X, X,
		1, 6, 2, 1, 8, 6, 1, 9, 8, 8, 7, 6, X, X, X, X,
		10, 7, 6, 10, 1, 7, 1, 3, 7, X, X, X, X, X, X, X,
		10, 7, 6, 1, 7, 10, 1, 8, 7, 1, 0, 8, X, X, X, X,
		0, 3, 7, 0, 7, 10, 0, 10, 9, 6, 10, 7, X, X, X, X,
		7, 6, 10, 7, 10, 8, 8, 10, 9, X, X, X, X, X, X, X,
		6, 8, 4, 11, 8, 6, X, X, X, X, X, X, X, X, X, X,
		3, 6, 11, 3, 0, 6, 0, 4, 6, X, X, X, X, X, X, X,
		8, 6, 11, 8, 4, 6, 9, 0, 1, X, X, X, X, X, X, X,
		9, 4, 6, 9, 6, 3, 9, 3, 1, 11, 3, 6, X, X, X, X,
		6, 8, 4, 6, 11, 8, 2, 10, 1, X, X, X, X, X, X, X,
		1, 2, 10, 3, 0, 11, 0, 6, 11, 0, 4, 6, X, X, X, X,
		4, 11, 8, 4, 6, 11, 0, 2, 9, 2, 10, 9, X, X, X, X,
		10, 9, 3, 10, 3, 2, 9, 4, 3, 11, 3, 6, 4, 6, 3, X,
		8, 2, 3, 8, 4, 2, 4, 6, 2, X, X, X, X, X, X, X,
		0, 4, 2, 4, 6, 2, X, X, X, X, X, X, X, X, X, X,
		1, 9, 0, 2, 3, 4, 2, 4, 6, 4, 3, 8, X, X, X, X,
		1, 9, 4, 1, 4, 2, 2, 4, 6, X, X, X, X, X, X, X,
		8, 1, 3, 8, 6, 1, 8, 4, 6, 6, 10, 1, X, X, X, X,
		10, 1, 0, 10, 0, 6, 6, 0, 4, X, X, X, X, X, X, X,
		4, 6, 3, 4, 3, 8, 6, 10, 3, 0, 3, 9, 10, 9, 3, X,
		10, 9, 4, 6, 10, 4, X, X, X, X, X, X, X, X, X, X,
		4, 9, 5, 7, 6, 11, X, X, X, X, X, X, X, X, X, X,
		0, 8, 3, 4, 9, 5, 11, 7, 6, X, X, X, X, X, X, X,
		5, 0, 1, 5, 4, 0, 7, 6, 11, X, X, X, X, X, X, X,
		11, 7, 6, 8, 3, 4, 3, 5, 4, 3, 1, 5, X, X, X, X,
		9, 5, 4, 10, 1, 2, 7, 6, 11, X, X, X, X, X, X, X,
		6, 11, 7, 1, 2, 10, 0, 8, 3, 4, 9, 5, X, X, X, X,
		7, 6, 11, 5, 4, 10, 4, 2, 10, 4, 0, 2, X, X, X, X,
		3, 4, 8, 3, 5, 4, 3, 2, 5, 10, 5, 2, 11, 7, 6, X,
		7, 2, 3, 7, 6, 2, 5, 4, 9, X, X, X, X, X, X, X,
		9, 5, 4, 0, 8, 6, 0, 6, 2, 6, 8, 7, X, X, X, X,
		3, 6, 2, 3, 7, 6, 1, 5, 0, 5, 4, 0, X, X, X, X,
		6, 2, 8, 6, 8, 7, 2, 1, 8, 4, 8, 5, 1, 5, 8, X,
		9, 5, 4, 10, 1, 6, 1, 7, 6, 1, 3, 7, X, X, X, X,
		1, 6, 10, 1, 7, 6, 1, 0, 7, 8, 7, 0, 9, 5, 4, X,
		4, 0, 10, 4, 10, 5, 0, 3, 10, 6, 10, 7, 3, 7, 10, X,
		7, 6, 10, 7, 10, 8, 5, 4, 10, 4, 8, 10, X, X, X, X,
		6, 9, 5, 6, 11, 9, 11, 8, 9, X, X, X, X, X, X, X,
		3, 6, 11, 0, 6, 3, 0, 5, 6, 0, 9, 5, X, X, X, X,
		0, 11, 8, 0, 5, 11, 0, 1, 5, 5, 6, 11, X, X, X, X,
		6, 11, 3, 6, 3, 5, 5, 3, 1, X, X, X, X, X, X, X,
		1, 2, 10, 9, 5, 11, 9, 11, 8, 11, 5, 6, X, X, X, X,
		0, 11, 3, 0, 6, 11, 0, 9, 6, 5, 6, 9, 1, 2, 10, X,
		11, 8, 5, 11, 5, 6, 8, 0, 5, 10, 5, 2, 0, 2, 5, X,
		6, 11, 3, 6, 3, 5, 2, 10, 3, 10, 5, 3, X, X, X, X,
		5, 8, 9, 5, 2, 8, 5, 6, 2, 3, 8, 2, X, X, X, X,
		9, 5, 6, 9, 6, 0, 0, 6, 2, X, X, X, X, X, X, X,
		1, 5, 8, 1, 8, 0, 5, 6, 8, 3, 8, 2, 6, 2, 8, X,
		1, 5, 6, 2, 1, 6, X, X, X, X, X, X, X, X, X, X,
		1, 3, 6, 1, 6, 10, 3, 8, 6, 5, 6, 9, 8, 9, 6, X,
		10, 1, 0, 10, 0, 6, 9, 5, 0, 5, 6, 0, X, X, X, X,
		0, 3, 8, 5, 6, 10, X, X, X, X, X, X, X, X, X, X,
		10, 5, 6, X, X, X, X, X, X, X, X, X, X, X, X, X,
		11, 5, 10, 7, 5, 11, X, X, X, X, X, X, X, X, X, X,
		11, 5, 10, 11, 7, 5, 8, 3, 0, X, X, X, X, X, X, X,
		5, 11, 7, 5, 10, 11, 1, 9, 0, X, X, X, X, X, X, X,
		10, 7, 5, 10, 11, 7, 9, 8, 1, 8, 3, 1, X, X, X, X,
		11, 1, 2, 11, 7, 1, 7, 5, 1, X, X, X, X, X, X, X,
		0, 8, 3, 1, 2, 7, 1, 7, 5, 7, 2, 11, X, X, X, X,
		9, 7, 5, 9, 2, 7, 9, 0, 2, 2, 11, 7, X, X, X, X,
		7, 5, 2, 7, 2, 11, 5, 9, 2, 3, 2, 8, 9, 8, 2, X,
		2, 5, 10, 2, 3, 5, 3, 7, 5, X, X, X, X, X, X, X,
		8, 2, 0, 8, 5, 2, 8, 7, 5, 10, 2, 5, X, X, X, X,
		9, 0, 1, 5, 10, 3, 5, 3, 7, 3, 10, 2, X, X, X, X,
		9, 8, 2, 9, 2, 1, 8, 7, 2, 10, 2, 5, 7, 5, 2, X,
		1, 3, 5, 3, 7, 5, X, X, X, X, X, X, X, X, X, X,
		0, 8, 7, 0, 7, 1, 1, 7, 5, X, X, X, X, X, X, X,
		9, 0, 3, 9, 3, 5, 5, 3, 7, X, X, X, X, X, X, X,
		9, 8, 7, 5, 9, 7, X, X, X, X, X, X, X, X, X, X,
		5, 8, 4, 5, 10, 8, 10, 11, 8, X, X, X, X, X, X, X,
		5, 0, 4, 5, 11, 0, 5, 10, 11, 11, 3, 0, X, X, X, X,
		0, 1, 9, 8, 4, 10, 8, 10, 11, 10, 4, 5, X, X, X, X,
		10, 11, 4, 10, 4, 5, 11, 3, 4, 9, 4, 1, 3, 1, 4, X,
		2, 5, 1, 2, 8, 5, 2, 11, 8, 4, 5, 8, X, X, X, X,
		0, 4, 11, 0, 11, 3, 4, 5, 11, 2, 11, 1, 5, 1, 11, X,
		0, 2, 5, 0, 5, 9, 2, 11, 5, 4, 5, 8, 11, 8, 5, X,
		9, 4, 5, 2, 11, 3, X, X, X, X, X, X, X, X, X, X,
		2, 5, 10, 3, 5, 2, 3, 4, 5, 3, 8, 4, X, X, X, X,
		5, 10, 2, 5, 2, 4, 4, 2, 0, X, X, X, X, X, X, X,
		3, 10, 2, 3, 5, 10, 3, 8, 5, 4, 5, 8, 0, 1, 9, X,
		5, 10, 2, 5, 2, 4, 1, 9, 2, 9, 4, 2, X, X, X, X,
		8, 4, 5, 8, 5, 3, 3, 5, 1, X, X, X, X, X, X, X,
		0, 4, 5, 1, 0, 5, X, X, X, X, X, X, X, X, X, X,
		8, 4, 5, 8, 5, 3, 9, 0, 5, 0, 3, 5, X, X, X, X,
		9, 4, 5, X, X, X, X, X, X, X, X, X, X, X, X, X,
		4, 11, 7, 4, 9, 11, 9, 10, 11, X, X, X, X, X, X, X,
		0, 8, 3, 4, 9, 7, 9, 11, 7, 9, 10, 11, X, X, X, X,
		1, 10, 11, 1, 11, 4, 1, 4, 0, 7, 4, 11, X, X, X, X,
		3, 1, 4, 3, 4, 8, 1, 10, 4, 7, 4, 11, 10, 11, 4, X,
		4, 11, 7, 9, 11, 4, 9, 2, 11, 9, 1, 2, X, X, X, X,
		9, 7, 4, 9, 11, 7, 9, 1, 11, 2, 11, 1, 0, 8, 3, X,
		11, 7, 4, 11, 4, 2, 2, 4, 0, X, X, X, X, X, X, X,
		11, 7, 4, 11, 4, 2, 8, 3, 4, 3, 2, 4, X, X, X, X,
		2, 9, 10, 2, 7, 9, 2, 3, 7, 7, 4, 9, X, X, X, X,
		9, 10, 7, 9, 7, 4, 10, 2, 7, 8, 7, 0, 2, 0, 7, X,
		3, 7, 10, 3, 10, 2, 7, 4, 10, 1, 10, 0, 4, 0, 10, X,
		1, 10, 2, 8, 7, 4, X, X, X, X, X, X, X, X, X, X,
		4, 9, 1, 4, 1, 7, 7, 1, 3, X, X, X, X, X, X, X,
		4, 9, 1, 4, 1, 7, 0, 8, 1, 8, 7, 1, X, X, X, X,
		4, 0, 3, 7, 4, 3, X, X, X, X, X, X, X, X, X, X,
		4, 8, 7, X, X, X, X, X, X, X, X, X, X, X, X, X,
		9, 10, 8, 10, 11, 8, X, X, X, X, X, X, X, X, X, X,
		3, 0, 9, 3, 9, 11, 11, 9, 10, X, X, X, X, X, X, X,
		0, 1, 10, 0, 10, 8, 8, 10, 11, X, X, X, X, X, X, X,
		3, 1, 10, 11, 3, 10, X, X, X, X, X, X, X, X, X, X,
		1, 2, 11, 1, 11, 9, 9, 11, 8, X, X, X, X, X, X, X,
		3, 0, 9, 3, 9, 11, 1, 2, 9, 2, 11, 9, X, X, X, X,
		0, 2, 11, 8, 0, 11, X, X, X, X, X, X, X, X, X, X,
		3, 2, 11, X, X, X, X, X, X, X, X, X, X, X, X, X,
		2, 3, 8, 2, 8, 10, 10, 8, 9, X, X, X, X, X, X, X,
		9, 10, 2, 0, 9, 2, X, X, X, X, X, X, X, X, X, X,
		2, 3, 8, 2, 8, 10, 0, 1, 8, 1, 10, 8, X, X, X, X,
		1, 10, 2, X, X, X, X, X, X, X, X, X, X, X, X, X,
		1, 3, 8, 9, 1, 8, X, X, X, X, X, X, X, X, X, X,
		0, 9, 1, X, X, X, X, X, X, X, X, X, X, X, X, X,
		0, 3, 8, X, X, X, X, X, X, X, X, X, X, X, X, X,
		X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X
	};

	// number of vertices for each case above
	int[] numVertsTable = new int[]{
	    0,
	    3,
	    3,
	    6,
	    3,
	    6,
	    6,
	    9,
	    3,
	    6,
	    6,
	    9,
	    6,
	    9,
	    9,
	    6,
	    3,
	    6,
	    6,
	    9,
	    6,
	    9,
	    9,
	    12,
	    6,
	    9,
	    9,
	    12,
	    9,
	    12,
	    12,
	    9,
	    3,
	    6,
	    6,
	    9,
	    6,
	    9,
	    9,
	    12,
	    6,
	    9,
	    9,
	    12,
	    9,
	    12,
	    12,
	    9,
	    6,
	    9,
	    9,
	    6,
	    9,
	    12,
	    12,
	    9,
	    9,
	    12,
	    12,
	    9,
	    12,
	    15,
	    15,
	    6,
	    3,
	    6,
	    6,
	    9,
	    6,
	    9,
	    9,
	    12,
	    6,
	    9,
	    9,
	    12,
	    9,
	    12,
	    12,
	    9,
	    6,
	    9,
	    9,
	    12,
	    9,
	    12,
	    12,
	    15,
	    9,
	    12,
	    12,
	    15,
	    12,
	    15,
	    15,
	    12,
	    6,
	    9,
	    9,
	    12,
	    9,
	    12,
	    6,
	    9,
	    9,
	    12,
	    12,
	    15,
	    12,
	    15,
	    9,
	    6,
	    9,
	    12,
	    12,
	    9,
	    12,
	    15,
	    9,
	    6,
	    12,
	    15,
	    15,
	    12,
	    15,
	    6,
	    12,
	    3,
	    3,
	    6,
	    6,
	    9,
	    6,
	    9,
	    9,
	    12,
	    6,
	    9,
	    9,
	    12,
	    9,
	    12,
	    12,
	    9,
	    6,
	    9,
	    9,
	    12,
	    9,
	    12,
	    12,
	    15,
	    9,
	    6,
	    12,
	    9,
	    12,
	    9,
	    15,
	    6,
	    6,
	    9,
	    9,
	    12,
	    9,
	    12,
	    12,
	    15,
	    9,
	    12,
	    12,
	    15,
	    12,
	    15,
	    15,
	    12,
	    9,
	    12,
	    12,
	    9,
	    12,
	    15,
	    15,
	    12,
	    12,
	    9,
	    15,
	    6,
	    15,
	    12,
	    6,
	    3,
	    6,
	    9,
	    9,
	    12,
	    9,
	    12,
	    12,
	    15,
	    9,
	    12,
	    12,
	    15,
	    6,
	    9,
	    9,
	    6,
	    9,
	    12,
	    12,
	    15,
	    12,
	    15,
	    15,
	    6,
	    12,
	    9,
	    15,
	    12,
	    9,
	    6,
	    12,
	    3,
	    9,
	    12,
	    12,
	    15,
	    12,
	    15,
	    9,
	    12,
	    12,
	    15,
	    15,
	    6,
	    9,
	    12,
	    6,
	    3,
	    6,
	    9,
	    9,
	    6,
	    9,
	    12,
	    6,
	    3,
	    9,
	    6,
	    12,
	    3,
	    6,
	    3,
	    3,
	    0,
	};
	
	private TriMesh resultMesh = null;

    public OpenCLAlgorithmMarchingCubes(final ModelImage image, final int level, final boolean entireImage, final boolean decFlag,
            final boolean blurFlag, final float sigma, final String fileName)
	{
		super(null, image, entireImage, CL.CL_DEVICE_TYPE_GPU);
        this.decimateFlag = decFlag;
        this.blurFlag = blurFlag;
        this.blurSigma = sigma;
        this.surfaceFileName = fileName;
		this.isoValue = level + 0.5f;
	}
	
	
	/**
	 * Prepares this class for destruction.
	 */
	public void finalize() {
		colorMask = null;
		destImage = null;
		srcImage = null;
		super.finalize();
	}


	/**
	 * Starts the program.
	 */
	public void runAlgorithm() {
		super.setStartTime();
		if (srcImage == null) {
			displayError("Source Image is null");

			return;
		}
		
		marchingCubes3D(0);
		setCompleted(true);
//		System.err.println( "OpenCL MarchingCubes Time : " + super.getElapsedTime() );
	}

	private void marchingCubes3D(int time)
	{		
		initCL(m_iDeviceType, null);
		int nBuffers = 5;
		int elementCount = width * height * depth * color;	
		long memoryUsed = nBuffers * elementCount;	
		long maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
		long totalMemSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_GLOBAL_MEM_SIZE);
		if ( (elementCount > (maxAllocSize / (Sizeof.cl_float))) || (memoryUsed >= (totalMemSize / Sizeof.cl_float)) )
		{
			// Try switching to the CPU device for more memory:
			MipavUtil.displayInfo( "Not enough GPU memory. Calling CPU version" );
			m_iDeviceType = CL.CL_DEVICE_TYPE_CPU;
			initCL(m_iDeviceType, null);
			maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
			if ( elementCount > (maxAllocSize / (Sizeof.cl_float)) )
			{
				// Both CPU and GPU devices do not have enough memory for the algorithm:
				MipavUtil.displayError( "Image size too big: select per-slice processing." );
				return;
			}
		}

		float[] input = new float[ elementCount ];
		try {
			this.entireImage = ( (srcImage.getMask() != null) && (srcImage.getMask().cardinality() > 0) ) ? this.entireImage : true;
			if ( this.entireImage )
			{
				srcImage.exportData( time * input.length, input.length, input );
			}
			else 
			{
				srcImage.exportDataUseMask( time * input.length, input.length, input );				
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		if ( blurFlag )
		{
			cl_mem blurImage = OpenCLAlgorithmGaussianBlur.gaussianBlurSep3D( cl, device, inputBuffer, time, elementCount, 
					new float[]{blurSigma, blurSigma, blurSigma}, width, height, depth, color, colorMask );

			clReleaseMemObject(inputBuffer);
			inputBuffer = blurImage;
		}


		int[] vertexCount = new int[ elementCount ];
		cl_mem vertexCountBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_int * vertexCount.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		int[] vertexOccupied = new int[ elementCount ];
		cl_mem vertexOccupiedBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_int * vertexOccupied.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem numVertsTableBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_int * numVertsTable.length, Pointer.to(numVertsTable), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem triTableBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_int * triTable.length, Pointer.to(triTable), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		

		int[] imageSize_3D = new int[]{width, height, depth, 0};

		// Read the program source code and create the program
		String source = readKernelFile("MarchingCubes.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 2D Kernel:
		String kernelName = (color == 1) ? "classifyVoxel" : "classifyVoxel_Color";
		cl_kernel classifyVoxel = clCreateKernel(program, kernelName, null);

		int arg = 0;
		clSetKernelArg(classifyVoxel, arg++, Sizeof.cl_mem, Pointer.to(vertexCountBuffer));
		clSetKernelArg(classifyVoxel, arg++, Sizeof.cl_mem, Pointer.to(vertexOccupiedBuffer));
		clSetKernelArg(classifyVoxel, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(classifyVoxel, arg++, Sizeof.cl_mem, Pointer.to(numVertsTableBuffer));
		clSetKernelArg(classifyVoxel, arg++, Sizeof.cl_int4, Pointer.to(imageSize_3D));
		clSetKernelArg(classifyVoxel, arg++, Sizeof.cl_int, Pointer.to(new int[]{elementCount}));
		clSetKernelArg(classifyVoxel, arg++, Sizeof.cl_float, Pointer.to(new float[]{isoValue}));
		if ( color != 1 )
		{
			clSetKernelArg(classifyVoxel, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(classifyVoxel, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg_3D = arg;



		long globalWorkSize[] = new long[]{width,height};        

		// create command queue:
		cl_command_queue commandQueue = clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(classifyVoxel, sliceArg_3D, Sizeof.cl_int, Pointer.to(new int[]{i}));
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
			}
			errcode[0] = clEnqueueNDRangeKernel(commandQueue, classifyVoxel, 2, null, globalWorkSize, null, 0, null, null);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
			}
		}
		errcode[0] = clFinish(commandQueue);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
		}
		errcode[0] = clEnqueueReadBuffer(commandQueue, vertexCountBuffer, CL_TRUE, 0, Sizeof.cl_int * vertexCount.length, Pointer.to(vertexCount), 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		errcode[0] = clEnqueueReadBuffer(commandQueue, vertexOccupiedBuffer, CL_TRUE, 0, Sizeof.cl_int * vertexOccupied.length, Pointer.to(vertexOccupied), 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		
		
		
		
		long numV = 0;
		long numO = 0;
		int[] vertexOccupiedScan = new int[vertexCount.length];
		int[] vertexScan = new int[vertexCount.length];
		for ( int i = 0; i < vertexCount.length; i++)
		{
			numV += vertexCount[i];
			vertexScan[i] = (int) numV - vertexCount[i];
			numO += vertexOccupied[i];
			vertexOccupiedScan[i] = (int)numO - vertexOccupied[i];
		}
		int activeVoxels = vertexOccupied[vertexScan.length-1] + vertexOccupiedScan[vertexOccupiedScan.length-1];
		int totalVerts = vertexCount[vertexScan.length-1] + vertexScan[vertexOccupiedScan.length-1];
//		System.err.println( "OpenCL Marching Cubes : " + numV + "   " + numO + "    " + elementCount
//				+ "   " + activeVoxels + "   " + totalVerts  );
		if ( totalVerts == 0 )
		{
			MipavUtil.displayError( "Marching Cubes failed" );
			clReleaseMemObject(inputBuffer);
			clReleaseMemObject(numVertsTableBuffer);
			clReleaseMemObject(triTableBuffer);
			clReleaseMemObject(vertexCountBuffer);
			clReleaseMemObject(vertexOccupiedBuffer);
			return;
		}

		int[] compactedVoxelArray = new int[ elementCount ];
		cl_mem compactedVoxelArraydBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_int * compactedVoxelArray.length, 
				null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem vertexOccupiedScanBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_int * vertexOccupiedScan.length, Pointer.to(vertexOccupiedScan), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		

		// Set up 2D Kernel:
		kernelName = (color == 1) ? "compactVoxels" : "compactVoxels_Color";
		cl_kernel compactVoxels = clCreateKernel(program, kernelName, null);
		arg = 0;
		clSetKernelArg(compactVoxels, arg++, Sizeof.cl_mem, Pointer.to(compactedVoxelArraydBuffer));
		clSetKernelArg(compactVoxels, arg++, Sizeof.cl_mem, Pointer.to(vertexOccupiedBuffer));
		clSetKernelArg(compactVoxels, arg++, Sizeof.cl_mem, Pointer.to(vertexOccupiedScanBuffer));
		clSetKernelArg(compactVoxels, arg++, Sizeof.cl_int, Pointer.to(new int[]{elementCount}));
		
		globalWorkSize[0] = elementCount;
		globalWorkSize[1] = 0;
		errcode[0] = clEnqueueNDRangeKernel(commandQueue, compactVoxels, 1, null, globalWorkSize, null, 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		errcode[0] = clFinish(commandQueue);
		errcode[0] = clEnqueueReadBuffer(commandQueue, compactedVoxelArraydBuffer, CL_TRUE, 0, 
				Sizeof.cl_float * compactedVoxelArray.length, Pointer.to(compactedVoxelArray), 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		
		
		
		


		cl_mem vertexScanBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_int * vertexScan.length, Pointer.to(vertexScan), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

//	    int maxVerts = width*height*100;
		float[] positions = new float[4*totalVerts];
		cl_mem positionsBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * positions.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		
		float[] voxelSize = new float[]{ 2.0f / width, 2.0f / height, 2.0f / depth, 0 };
	    
		

		// Set up 2D Kernel:
		kernelName = (color == 1) ? "generateTriangles2" : "generateTriangles2_Color";
		cl_kernel generateTriangles2 = clCreateKernel(program, kernelName, null);

		arg = 0;
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_mem, Pointer.to(positionsBuffer));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_mem, Pointer.to(compactedVoxelArraydBuffer));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_mem, Pointer.to(vertexScanBuffer));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_mem, Pointer.to(numVertsTableBuffer));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_mem, Pointer.to(triTableBuffer));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_int4, Pointer.to(imageSize_3D));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_int, Pointer.to(new int[]{elementCount}));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_float4, Pointer.to(voxelSize));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_float, Pointer.to(new float[]{isoValue}));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_float, Pointer.to(new float[]{activeVoxels}));
		clSetKernelArg(generateTriangles2, arg++, Sizeof.cl_float, Pointer.to(new float[]{totalVerts}));
//		clSetKernelArg(generateTriangles2, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
//		sliceArg_3D = arg;


		globalWorkSize[0] = (int) Math.ceil(activeVoxels / (float) 32);
		globalWorkSize[1] = 1;    
		while( globalWorkSize[0] > 65535 )
		{
			globalWorkSize[0]/=2;
			globalWorkSize[1]*=2;			
		}
		globalWorkSize[0]*=32;
		
		long[] localWorkSize = new long[]{32,1};
		
		errcode[0] = clEnqueueNDRangeKernel(commandQueue, generateTriangles2, 1, null, globalWorkSize, localWorkSize, 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		errcode[0] = clFinish(commandQueue);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
		}
		
		
		

		errcode[0] = clEnqueueReadBuffer(commandQueue, positionsBuffer, CL_TRUE, 0, 
				Sizeof.cl_float * positions.length, Pointer.to(positions), 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}



		int numVertices = positions.length/4;

		Vector<Vector3f> vertices = new Vector<Vector3f>();
		Vector<TriangleKey> triangles = new Vector<TriangleKey>();
		int triCount = 0;
        for ( int i = 0; i < numVertices; i++)
        {
        	Vector3f pos1 = new Vector3f( positions[i*4+0], positions[i*4+1], positions[i*4+2]);
        	i++;
        	Vector3f pos2 = new Vector3f( positions[i*4+0], positions[i*4+1], positions[i*4+2]);
        	i++;
        	Vector3f pos3 = new Vector3f( positions[i*4+0], positions[i*4+1], positions[i*4+2]);
        	if ( Float.isNaN( pos1.X) || Float.isNaN( pos1.Y ) || Float.isNaN( pos1.Z ) )
        	{
        		System.err.println( i-2 );
        		continue;
        	}
        	if ( Float.isNaN( pos2.X) || Float.isNaN( pos2.Y ) || Float.isNaN( pos2.Z ) )
        	{
        		System.err.println( i-1 );
        		continue;
        	}
        	if ( Float.isNaN( pos3.X) || Float.isNaN( pos3.Y ) || Float.isNaN( pos3.Z ) )
        	{
        		System.err.println( i );
        		continue;
        	}
        	vertices.add(pos1);
        	vertices.add(pos2);
        	vertices.add(pos3);
        	triangles.add( new TriangleKey(triCount, triCount+1, triCount+2) );
        	triCount += 3;
        }
		Vector<Vector3f> newVertices = new Vector<Vector3f>();
		Vector<TriangleKey> newTriangles = new Vector<TriangleKey>();
        ExtractSurfaceCubes.MakeUnique( vertices, triangles, newVertices, newTriangles );
        

        
        int iTQuantity = newTriangles.size();
		int[] aiConnect = new int[3 * iTQuantity];
		int iIndex = 0;

		for ( int i = 0; i < iTQuantity; i++ )
		{
			TriangleKey kT = newTriangles.elementAt(i);
			aiConnect[iIndex++] = kT.V[0];
			aiConnect[iIndex++] = kT.V[1];
			aiConnect[iIndex++] = kT.V[2];
		}
		resultMesh = new TriMesh( new VertexBuffer(newVertices), new IndexBuffer(aiConnect));
        
		
		Preferences.debug("\n\nVertex count reduced from " + vertices.size() + " to " + newVertices.size() + ".\n", Preferences.DEBUG_MINOR);
		Preferences.debug("Triangle count reduced from " + triangles.size() + " to " + newTriangles.size() + ".\n\n", Preferences.DEBUG_MINOR);

		if ( surfaceFileName != null )
		{
			//		System.err.println( "OpenCL Marching Cubes : " + numV + "   " + numO + "    " + elementCount + "     " + positions.length );
//			System.err.println( "OpenCL Marching Cubes : " + surfaceFileName );
			try {
				String fileName = ViewUserInterface.getReference().getDefaultDirectory() + surfaceFileName;
				saveMesh(newVertices, aiConnect, true, fileName );
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(numVertsTableBuffer);
		clReleaseMemObject(triTableBuffer);
		clReleaseMemObject(vertexCountBuffer);
		clReleaseMemObject(vertexOccupiedBuffer);
		clReleaseMemObject(compactedVoxelArraydBuffer);
		clReleaseMemObject(vertexOccupiedScanBuffer);
		clReleaseMemObject(vertexScanBuffer);
		clReleaseMemObject(positionsBuffer);
	}
	private float reductionKernel( cl_command_queue commandQueue, long elementCount, cl_mem inputBuffer )
	{
		int[] errcode = new int[1];
		int maxWorkSize = (int) Math.min( 512, OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_WORK_GROUP_SIZE) );

		// Read the program source code and create the program
		String source = OpenCLAlgorithmBase.readKernelFile("ParallelReduction.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, new String[]{ source }, null, errcode);
		checkError(errcode[0]);
		checkError( clBuildProgram(program, 0, null, "-cl-mad-enable", null, null) );
		cl_kernel reductionKernel = clCreateKernel(program, "reduceSum", errcode);
		checkError(errcode[0]);

		float[] sum = new float[ 1 ];
		cl_mem sumBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_float * sum.length, null, errcode);
		checkError(errcode[0]);

		int arg = 0;
		checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer)) );
		checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_mem, Pointer.to(sumBuffer)) );
		checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_long, Pointer.to(new long[]{elementCount})) );
		checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_long, Pointer.to(new long[]{maxWorkSize})) );
		checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_mem*maxWorkSize, null) );

		long local_work_size[] = new long[]{maxWorkSize};
		long global_work_size[] = new long[]{maxWorkSize};
		
		// initial reduction is into brightnessBuffer:
		checkError( clEnqueueNDRangeKernel(commandQueue, reductionKernel, 1, null, global_work_size, local_work_size, 0, null, null) );

		checkError( clFinish(commandQueue) );		
		checkError( clEnqueueReadBuffer(commandQueue, sumBuffer, CL_TRUE, 0, Sizeof.cl_float * sum.length, Pointer.to(sum), 0, null, null) );

		// continue reduction into the sumBuffer
//		checkError( clSetKernelArg(reductionKernel, 1, Sizeof.cl_mem, Pointer.to(sumBuffer)) );
		
	    return sum[0];
	}
	
	public TriMesh getMesh()
	{
		System.err.println( resultMesh.VBuffer.GetVertexQuantity() + " " + resultMesh.GetTriangleQuantity() );
		return resultMesh;
	}
	
	private void saveMesh( Vector<Vector3f> newVertices, int[] aiConnect,
			final boolean flip, final String kName) throws IOException {
        TransMatrix dicomMatrix = null;
        TransMatrix inverseDicomMatrix = null;
        // double[][] inverseDicomArray = null;
        float[] coord;
        float[] tCoord;

		int iVQuantity = newVertices.size();

    	float[] res = srcImage.getResolutions(0);
        float[] startLocation = srcImage.getFileInfo()[0].getOrigin();
        int[] direction = MipavCoordinateSystems.getModelDirections(srcImage);
        
        Vector3f[] transformedPositions = new Vector3f[iVQuantity];
        if ( srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL) )
        {

            // Get the DICOM transform that describes the transformation from
            // axial to this image orientation
            dicomMatrix = srcImage.getMatrix();
            inverseDicomMatrix = new TransMatrix(srcImage.getMatrix());
            inverseDicomMatrix.Inverse();
            // inverseDicomArray = inverseDicomMatrix.getMatrix();
            // inverseDicomMatrix = null;
            coord = new float[3];
            tCoord = new float[3];

            for ( int i = 0; i < iVQuantity; i++)
            {
            	Vector3f pos = newVertices.elementAt(i);
            	
                // Change the voxel coordinate into millimeter space
                coord[0] = pos.X * res[0];
                coord[1] = pos.Y * res[1];
                coord[2] = pos.Z * res[2];

                // Convert the point to axial millimeter DICOM space
                dicomMatrix.transform(coord, tCoord);

                // Add in the DICOM origin
                pos.X = startLocation[0] + tCoord[0];
                pos.Y = startLocation[1] + tCoord[1];
                pos.Z = startLocation[2] + tCoord[2];
                transformedPositions[i] = pos;
            }
        }
        else
        {
            for ( int i = 0; i < iVQuantity; i++ )
            {
            	Vector3f pos = newVertices.elementAt(i);
            	pos.X = (pos.X * res[0] * direction[0]) + startLocation[0];
            	pos.Y = (pos.Y * res[1] * direction[1]) + startLocation[1];
            	pos.Z = (pos.Z * res[2] * direction[2]) + startLocation[2];
            	transformedPositions[i] = pos;
            }
        }


        float[] box = new float[3];
        box[0] = (width - 1) * res[0];
        box[1] = (height - 1) * res[1];
        box[2] = (depth - 1) * res[2];
        
        TriMesh kMesh = new TriMesh( new VertexBuffer(transformedPositions), new IndexBuffer(aiConnect));
//        System.err.println( "OpenCLAlgorithmMarchingCubes " + kMesh.VBuffer.GetVertexQuantity() + " " + kMesh.GetTriangleQuantity() );
        FileSurface_WM.save(kName, kMesh, 0, kMesh.VBuffer, flip, direction, startLocation, box, inverseDicomMatrix);
    }
}
