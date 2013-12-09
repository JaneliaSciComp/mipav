package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


import static org.jocl.CL.CL_DEVICE_GLOBAL_MEM_SIZE;
import static org.jocl.CL.CL_DEVICE_MAX_MEM_ALLOC_SIZE;
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
import gov.nih.mipav.model.GaussianKernelFactory;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.*;

import org.jocl.CL;
import org.jocl.Pointer;
import org.jocl.Sizeof;
import org.jocl.cl_command_queue;
import org.jocl.cl_kernel;
import org.jocl.cl_mem;
import org.jocl.cl_program;


/**
 * OpenCL Algorithm computes the Gradient Magnitude of a 2D, 3D or 4D image.
 */
public class OpenCLAlgorithmGradientMagnitude extends OpenCLAlgorithmBase {

	/** Storage location of the first derivative of the Gaussian in the X direction for 2D images; */
	private float[] Gx2Data;

	/** Storage location of the first derivative of the Gaussian in the X direction. */
	private float[] GxData;

	/** Storage location of the first derivative of the Gaussian in the Y direction for 2D images; */
	private float[] Gy2Data;

	/** Storage location of the first derivative of the Gaussian in the Y direction. */
	private float[] GyData;

	/** Storage location of the first derivative of the Gaussian in the Z direction. */
	private float[] GzData;

	/** Extents of the 2D kernel. */
	private int[] kExtents_2D;

	/** Origins of the 2D kernel. */
	private int[] kOrigins_2D;

	/** Extents of the 3D kernel. */
	private int[] kExtents_3D;

	/** Origins of the 3D kernel. */
	private int[] kOrigins_3D;

	/** Standard deviations of the gaussian used to calculate the kernels. */
	private float[] sigmas;


	/**
	 * Creates a new AlgorithmGradientMagnitude object.
	 *
	 * @param  srcImg    source image model
	 * @param  sigmas    Gaussian standard deviations in each dimension
	 * @param  maskFlag  Flag, if true, indicates that the gradient magnitude will be calculated for the whole image
	 * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
	 *                   images disregard this flag.
	 */
	public OpenCLAlgorithmGradientMagnitude(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean separable, boolean img25D) {
		this(null, srcImg, sigmas, maskFlag, separable, img25D);
	}

	/**
	 * Creates a new AlgorithmGradientMagnitude object.
	 *
	 * @param  destImg   image model where result image is to stored
	 * @param  srcImg    source image model
	 * @param  sigmas    Gaussian standard deviations in each dimension
	 * @param  maskFlag  Flag, if true, indicates that the gradient magnitude will be calculated for the whole image
	 * @param  img25D    Flag, if true, indicates that each slice of the 3D volume should be processed independently. 2D
	 *                   images disregard this flag.
	 */
	public OpenCLAlgorithmGradientMagnitude(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean separable,
			boolean img25D) {
		super(destImg, srcImg, maskFlag, CL.CL_DEVICE_TYPE_GPU);
		this.sigmas = sigmas;
		this.separable = separable;
		this.image25D = img25D;
	}

	/**
	 * Prepares this class for destruction.
	 */
	public void finalize() {
		colorMask = null;
		kOrigins_2D = null;
		kExtents_2D = null;
		kOrigins_3D = null;
		kExtents_3D = null;
		sigmas = null;
		GxData = null;
		GyData = null;
		GzData = null;
		Gx2Data = null;
		Gy2Data = null;
		destImage = null;
		srcImage = null;
		super.finalize();
	}


	/**
	 * Starts the program.
	 */
	public void runAlgorithm() {
		if (srcImage == null) {
			displayError("Source Image is null");

			return;
		}
		if ( !separable )
		{
			if (srcImage.getNDims() == 2) {
				makeKernels2D();
			} else if ((srcImage.getNDims() == 3) && (image25D == false)) {
				makeKernels2D();
				makeKernels3D();
			} else if ((srcImage.getNDims() == 3) && (image25D == true)) {
				makeKernels2D();
			} else if (srcImage.getNDims() == 4) {
				makeKernels2D();
				makeKernels3D();
			}
		}
		if (threadStopped) {
			finalize();
			return;
		}

		if (srcImage.getNDims() == 2) {
			if ( !separable )
			{
				gradientMagnitude25D();
			}
			else
			{
				gradientMagnitudeSep25D();
			}
		} else if ((srcImage.getNDims() == 3) && (image25D == false)) {
			if ( !separable )
			{
				gradientMagnitude3D(0);
			}
			else
			{
				gradientMagnitudeSep3D(0);
			}
		} else if ((srcImage.getNDims() == 3) && (image25D == true)) {
			if ( !separable )
			{
				gradientMagnitude25D();
			}
			else
			{
				gradientMagnitudeSep25D();
			}
		} else if (srcImage.getNDims() == 4) {
			gradientMagnitude4D();
		}
		setCompleted(true);
	}

	/**
	 * Creates Gaussian derivative kernels.
	 */
	private void makeKernels2D() {
		int xkDim, ykDim;
		int[] derivOrder = new int[2];

		kExtents_2D = new int[2];
		derivOrder[0] = 1;
		derivOrder[1] = 0;

		xkDim = Math.round(8 * sigmas[0]);

		if ((xkDim % 2) == 0) {
			xkDim++;
		}

		if (xkDim < 3) {
			xkDim = 3;
		}

		kExtents_2D[0] = xkDim;

		ykDim = Math.round(8 * sigmas[1]);

		if ((ykDim % 2) == 0) {
			ykDim++;
		}

		if (ykDim < 3) {
			ykDim = 3;
		}

		kExtents_2D[1] = ykDim;

		Gx2Data = new float[xkDim * ykDim];

		GenerateGaussian Gx = new GenerateGaussian(Gx2Data, kExtents_2D, sigmas, derivOrder);
		Gx.calc(false);

		derivOrder[0] = 0;
		derivOrder[1] = 1;
		Gy2Data = new float[xkDim * ykDim];

		GenerateGaussian Gy = new GenerateGaussian(Gy2Data, kExtents_2D, sigmas, derivOrder);
		Gy.calc(true);


		kOrigins_2D = new int[2];
		kOrigins_2D[0] = (kExtents_2D[0]-1)>>1;
		kOrigins_2D[1] = (kExtents_2D[1]-1)>>1;
	}

	/**
	 * Creates Gaussian derivative kernels.
	 */
	private void makeKernels3D() {
		int xkDim, ykDim, zkDim;
		int[] derivOrder = new int[3];

		kExtents_3D = new int[3];
		derivOrder[0] = 1;
		derivOrder[1] = 0;
		derivOrder[2] = 0;

		xkDim = Math.round(8 * sigmas[0]);

		if ((xkDim % 2) == 0) {
			xkDim++;
		}

		if (xkDim < 3) {
			xkDim = 3;
		}

		kExtents_3D[0] = xkDim;

		ykDim = Math.round(8 * sigmas[1]);

		if ((ykDim % 2) == 0) {
			ykDim++;
		}

		if (ykDim < 3) {
			ykDim = 3;
		}

		kExtents_3D[1] = ykDim;

		zkDim = Math.round(8 * sigmas[2]);

		if ((zkDim % 2) == 0) {
			zkDim++;
		}

		if (zkDim < 3) {
			zkDim = 3;
		}

		kExtents_3D[2] = zkDim;


		GxData = new float[xkDim * ykDim * zkDim];

		GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents_3D, sigmas, derivOrder);
		Gx.calc(false);

		derivOrder[0] = 0;
		derivOrder[1] = 1;
		derivOrder[2] = 0;
		GyData = new float[xkDim * ykDim * zkDim];

		GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents_3D, sigmas, derivOrder);
		Gy.calc(true);

		derivOrder[0] = 0;
		derivOrder[1] = 0;
		derivOrder[2] = 1;
		GzData = new float[xkDim * ykDim * zkDim];

		GenerateGaussian Gz = new GenerateGaussian(GzData, kExtents_3D, sigmas, derivOrder);
		Gz.calc(true);

		kOrigins_3D = new int[3];
		kOrigins_3D[0] = (kExtents_3D[0]-1)>>1;
		kOrigins_3D[1] = (kExtents_3D[1]-1)>>1;
		kOrigins_3D[2] = (kExtents_3D[2]-1)>>1;   
	}

	/**
	 * 2D Implementation: Create the OpenCL Buffers, Kernel, and CommandQueue. 
	 * Run the Kernel and save the results into a new image.
	 */
	private void gradientMagnitude25D()
	{
		initCL(m_iDeviceType, null);

		int elementCount = width * height * depth * color;		
		float[] input = new float[ elementCount ];
		try {
			if ( this.entireImage )
			{
				srcImage.exportData( 0, input.length, input );
			}
			else
			{
				srcImage.exportDataUseMask( 0, input.length, input );
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		float[] output = new float[ elementCount ];

		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputBuffer = clCreateBuffer(cl, CL.CL_MEM_WRITE_ONLY,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveX = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * Gx2Data.length, Pointer.to(Gx2Data), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveY = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * Gy2Data.length, Pointer.to(Gy2Data), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		int[] imageSize = new int[]{width, height};
		int[] maskSize = kExtents_2D;
		int[] maskOrigins = kOrigins_2D;

		// Read the program source code and create the program
		String source = readKernelFile("GradientMagnitude.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);
		String kernelName = (color == 1) ? "gradientMagnitude25D" : "gradientMagnitude25D_Color";
		cl_kernel kernel = clCreateKernel(program, kernelName, null);

		int arg = 0;
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(convolveX));
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(convolveY));
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int2, Pointer.to(imageSize));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int2, Pointer.to(maskSize));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int2, Pointer.to(maskOrigins));
		if ( color != 1 )
		{
			clSetKernelArg(kernel, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernel, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg = arg;

		long globalWorkSize[] = new long[]{width,height};        
		System.err.println( kExtents_2D[0] + " " + kExtents_2D[1] + " " +
				(kExtents_2D[0] * kExtents_2D[1]) );

		// create command queue:
		cl_command_queue commandQueue = clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(kernel, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
			}
			errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernel, 2, null, globalWorkSize, null, 0, null, null);
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
		errcode[0] = clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( "ReadBuffer " + stringFor_errorCode(errcode[0]) );
		}
		saveImage(output, 0, true );

		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(outputBuffer);
	}


	/**
	 * 3D Implementation: Create the OpenCL Buffers, Kernel, and CommandQueue. 
	 * Run the Kernel and save the results into a new image.
	 */  
	private void gradientMagnitude3D( int time )
	{
		initCL(m_iDeviceType, null);

		int elementCount = width * height * depth * color;		
		float[] input = new float[ elementCount ];
		try {
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

		float[] output = new float[ elementCount ];

		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputBuffer = clCreateBuffer(cl, CL.CL_MEM_WRITE_ONLY,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveX_2D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * Gx2Data.length, Pointer.to(Gx2Data), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveY_2D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * Gy2Data.length, Pointer.to(Gy2Data), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveX_3D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GxData.length, Pointer.to(GxData), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveY_3D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GyData.length, Pointer.to(GyData), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveZ_3D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GzData.length, Pointer.to(GzData), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		int[] imageSize_2D = new int[]{width, height};
		int[] maskSize_2D = kExtents_2D;
		int[] maskOrigins_2D = kOrigins_2D;

		int[] imageSize_3D = new int[]{width, height, depth, 0};
		int[] maskSize_3D = new int[]{kExtents_3D[0], kExtents_3D[1], kExtents_3D[2], 0};
		int[] maskOrigins_3D = new int[]{kOrigins_3D[0], kOrigins_3D[1], kOrigins_3D[2], 0};

		// Read the program source code and create the program
		String source = readKernelFile("GradientMagnitude.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 2D Kernel:
		String kernelName = (color == 1) ? "gradientMagnitude25D" : "gradientMagnitude25D_Color";
		cl_kernel kernel_2D = clCreateKernel(program, kernelName, null);

		int arg = 0;
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(convolveX_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(convolveY_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int2, Pointer.to(imageSize_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int2, Pointer.to(maskSize_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int2, Pointer.to(maskOrigins_2D));
		if ( color != 1 )
		{
			clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernel_2D, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg_2D = arg;



		// Set up 3D Kernel:
		kernelName = (color == 1) ? "gradientMagnitude3D" : "gradientMagnitude3D_Color";
		cl_kernel kernel_3D = clCreateKernel(program, kernelName, null);

		arg = 0;
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_mem, Pointer.to(convolveX_3D));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_mem, Pointer.to(convolveY_3D));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_mem, Pointer.to(convolveZ_3D));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_int4, Pointer.to(imageSize_3D));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_int4, Pointer.to(maskSize_3D));
		clSetKernelArg(kernel_3D, arg++, Sizeof.cl_int4, Pointer.to(maskOrigins_3D));
		if ( color != 1 )
		{
			clSetKernelArg(kernel_3D, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernel_3D, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg_3D = arg;

		long globalWorkSize[] = new long[]{width,height};        
		System.err.println( kExtents_3D[0] + " " + kExtents_3D[1] + " " +  kExtents_3D[2] + " " +
				(kExtents_3D[0] * kExtents_3D[1] *  kExtents_3D[2]) );

		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		for ( int i = 0; i < depth; i++ )
		{
			if ( (i < maskSize_3D[2]) || (i >= depth - maskSize_3D[2]) )
			{
				errcode[0] = clSetKernelArg(kernel_2D, sliceArg_2D, Sizeof.cl_int, Pointer.to(new int[]{i}));
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
				errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernel_2D, 2, null, globalWorkSize, null, 0, null, null);
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
			}
			else
			{
				errcode[0] = clSetKernelArg(kernel_3D, sliceArg_3D, Sizeof.cl_int, Pointer.to(new int[]{i}));
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
				errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernel_3D, 2, null, globalWorkSize, null, 0, null, null);
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
			}
		}
		errcode[0] = clFinish(commandQueue);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
		}
		errcode[0] = clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		saveImage(output, time, true );

		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(outputBuffer);
	}

	/**
	 * 25D Implementation: Create the OpenCL Buffers, Kernel, and CommandQueue. 
	 * Run the Kernel and save the results into a new image.
	 */  
	private void gradientMagnitudeSep25D( )
	{
		initCL(m_iDeviceType, null);				

		int elementCount = width * height * depth * color;		
		long maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
		if ( (Sizeof.cl_float * elementCount) > maxAllocSize )
		{
			gradientMagnitudeSep25DSlices();
			return;
		}

		float[] input = new float[ elementCount ];
		try {
			if ( this.entireImage )
			{
				srcImage.exportData( 0, input.length, input );
			}
			else
			{
				srcImage.exportDataUseMask( 0, input.length, input );
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		float[] output = new float[ elementCount ];

		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputDerivX = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputDerivY = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		// Read the program source code and create the program
		String source = readKernelFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 3D Kernel:
		String kernelName = (color == 1) ? "magnitude25D" : "magnitude25D_color";
		cl_kernel magnitude = clCreateKernel(program, kernelName, null);


		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		long globalWorkSize[] = new long[]{width,height};        


		// Convolve Seperable X:
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.X_DERIVATIVE_KERNEL);
		Kernel gaussianKernel = gkf.createKernel();
		OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
				inputBuffer, outputDerivX, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask );

		// Convolve Seperable Y:
		gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.Y_DERIVATIVE_KERNEL);
		gaussianKernel = gkf.createKernel();
		OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
				inputBuffer, outputDerivY, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask );


		// Calculate magnitude:
		int arg = 0;
		clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(outputDerivX));
		clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(outputDerivY));
		clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(magnitude, arg++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		if ( color != 1 )
		{
			clSetKernelArg(magnitude, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(magnitude, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg = arg;

		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(magnitude, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
			}
			errcode[0] = clEnqueueNDRangeKernel(commandQueue, magnitude, 2, null, globalWorkSize, null, 0, null, null);
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
		clReleaseMemObject(outputDerivX);
		clReleaseMemObject(outputDerivY);
		errcode[0] = clEnqueueReadBuffer(commandQueue, inputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		saveImage(output, 0, true );

		clReleaseMemObject(inputBuffer);
	}


	private void gradientMagnitudeSep25DSlices( )
	{
		// Convolve Seperable X:
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.X_DERIVATIVE_KERNEL);
		Kernel gaussianKernelX = gkf.createKernel();

		gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.Y_DERIVATIVE_KERNEL);
		Kernel gaussianKernelY = gkf.createKernel();

		int[] errcode = new int[1];


		// Read the program source code and create the program
		String source = readKernelFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 3D Kernel:
		String kernelName = (color == 1) ? "magnitude25D" : "magnitude25D_color";
		cl_kernel magnitude = clCreateKernel(program, kernelName, null);


		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		long globalWorkSize[] = new long[]{width,height};  



		int elementCount = width * height * color;


		float[] output = new float[ elementCount ];
		cl_mem outputDerivX = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputDerivY = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}      
		

		float[] input = new float[ elementCount ];
		for ( int i = 0; i < depth; i++ )
		{
			try {
				if ( this.entireImage )
				{
					srcImage.exportData( i * input.length, input.length, input );
				}
				else
				{
					srcImage.exportDataUseMask( i * input.length, input.length, input );
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
			cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
					Sizeof.cl_float * input.length, Pointer.to(input), errcode);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( stringFor_errorCode(errcode[0]) );
			}

			OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
					inputBuffer, outputDerivX, width, height, 1, elementCount, gaussianKernelX, 
					color, colorMask );

			// Convolve Seperable Y:
			OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
					inputBuffer, outputDerivY, width, height, 1, elementCount, gaussianKernelY, 
					color, colorMask );


			// Calculate magnitude:
			int arg = 0;
			clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(outputDerivX));
			clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(outputDerivY));
			clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
			clSetKernelArg(magnitude, arg++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, 1, 0}));
			if ( color != 1 )
			{
				clSetKernelArg(magnitude, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
			}
			clSetKernelArg(magnitude, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
			
			errcode[0] = clEnqueueNDRangeKernel(commandQueue, magnitude, 2, null, globalWorkSize, null, 0, null, null);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
			}

			errcode[0] = clFinish(commandQueue);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
			}
			errcode[0] = clEnqueueReadBuffer(commandQueue, inputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( stringFor_errorCode(errcode[0]) );
			}
			saveImage(output, i, (i == depth-1) );

			clReleaseMemObject(inputBuffer);
		}
		clReleaseMemObject(outputDerivX);
		clReleaseMemObject(outputDerivY);
	}

	/**
	 * 3D Implementation: Create the OpenCL Buffers, Kernel, and CommandQueue. 
	 * Run the Kernel and save the results into a new image.
	 */  
	private void gradientMagnitudeSep3D( int time )
	{
		initCL(m_iDeviceType, null);				
		int nBuffers = 5;
		int elementCount = width * height * depth * color;	
		long memoryUsed = getMaxMemoryUsed( nBuffers, elementCount, sigmas );	
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

		float[] output = new float[ elementCount ];

		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputDerivX = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputDerivY = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputDerivZ = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		// Read the program source code and create the program
		String source = readKernelFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 3D Kernel:
		String kernelName = (color == 1) ? "magnitude" : "magnitude_color";
		cl_kernel magnitude = clCreateKernel(program, kernelName, null);


		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		long globalWorkSize[] = new long[]{width,height};        


		// Convolve Seperable X:
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.X_DERIVATIVE_KERNEL);
		Kernel gaussianKernel = gkf.createKernel();
		OpenCLAlgorithmConvolver.convolveSep3D( cl, device, 
				inputBuffer, outputDerivX, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask, true );

		// Convolve Seperable Y:
		gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.Y_DERIVATIVE_KERNEL);
		gaussianKernel = gkf.createKernel();
		OpenCLAlgorithmConvolver.convolveSep3D( cl, device, 
				inputBuffer, outputDerivY, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask, true );

		// Convolve Seperable Z:
		gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.Z_DERIVATIVE_KERNEL);
		gaussianKernel = gkf.createKernel();
		int[] kExtentsZ = gaussianKernel.getExtents();

		OpenCLAlgorithmConvolver.convolveSep3D( cl, device, 
				inputBuffer, outputDerivZ, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask, true );     		

		// Calculate magnitude:
		int arg = 0;
		clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(outputDerivX));
		clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(outputDerivY));
		clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(outputDerivZ));
		clSetKernelArg(magnitude, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(magnitude, arg++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(magnitude, arg++, Sizeof.cl_int, Pointer.to(new int[]{kExtentsZ[2]}));
		if ( color != 1 )
		{
			clSetKernelArg(magnitude, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(magnitude, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg = arg;

		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(magnitude, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
			}
			errcode[0] = clEnqueueNDRangeKernel(commandQueue, magnitude, 2, null, globalWorkSize, null, 0, null, null);
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
		clReleaseMemObject(outputDerivX);
		clReleaseMemObject(outputDerivY);
		clReleaseMemObject(outputDerivZ);
		errcode[0] = clEnqueueReadBuffer(commandQueue, inputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		saveImage(output, time, true );

		clReleaseMemObject(inputBuffer);
	}

	/**
	 * Calls gradientMagnitude3D for each volume in the time series.
	 */
	private void gradientMagnitude4D( )
	{
		for ( int i = 0; i < time; i++ )
		{
			if ( !separable )
			{
				gradientMagnitude3D(i);
			}
			else
			{
				gradientMagnitudeSep3D(i);
			}
		}
	}
}
