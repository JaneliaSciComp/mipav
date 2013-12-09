package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


import static org.jocl.CL.*;
import gov.nih.mipav.model.GaussianKernelFactory;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;

import java.io.IOException;

import org.jocl.CL;
import org.jocl.Pointer;
import org.jocl.Sizeof;
import org.jocl.cl_command_queue;
import org.jocl.cl_kernel;
import org.jocl.cl_mem;
import org.jocl.cl_program;


/**
 * OpenCL Algorithm computes Deconvolution for a 3D or 4D image.
 */
public class OpenCLAlgorithmDeconvolution extends OpenCLAlgorithmBase {

	/** Color flags. */
	private int[] colorMask = new int[]{0,0,0,0};

	/** Standard deviations of the Gaussian used to calculate the kernels. */
	private float[] sigmas;

	/** number of iterations in the deconvolution: */
	private int iterations;
	
	/** when true, the sigma values are multiplied by: 1.0 / (2*Math.sqrt(2*Math.log(2))) 
	 * before the gaussian is generated. */
	private boolean useConversion = false;

	/** derivative buffer in OpenCL for convolving along the x-axis */
	private cl_mem[] derivativeX = null;

	/** derivative buffer in OpenCL for convolving along the y-axis */
	private cl_mem[] derivativeY = null;

	/** derivative buffer in OpenCL for convolving along the z-axis */
	private cl_mem[] derivativeZ = null;

	/** extends for the gaussian kernels */
	private int[][] kExtents = null;

	/** center positions for the gaussian kernels */
	private int[][] kOrigins = null;

	/** OpenCL reduction kernel, calculates the max of and OpenCL buffer. */
	private cl_kernel reductionKernel = null;

	/** OpenCL scale kernel, multiplies the image by buffer1[0] / buffer2[0],
	 * where all buffers are OpenCL buffers on the GPU / CPU. */
	private cl_kernel scaleKernel = null;
	
	/** buffer for storing the current max of an OpenCL buffer */
	private cl_mem sumBuffer = null;
	/** buffer for storing the original maximum of the OpenCL buffer */
	private cl_mem brightnessBuffer = null;
	private float[] sum = null;
	
	/** second ModelImage for dual-deconvolution */
	private ModelImage srcImageB;

	/** second sigma values for dual-deconvolution */
	private float[] sigmasB;
	
	/** Maximum number of work items in a work group on OpenCL. */
	private long maxWorkSize;

	/**
	 * New Deconvolution algorithm, applies the deconvolution based on the
	 * sigma values and iterations to the input image. Overwrites the input image.
	 * @param srcImg
	 * @param sigmas
	 * @param maskFlag
	 * @param iterations
	 * @param conversion
	 */
	public OpenCLAlgorithmDeconvolution(ModelImage srcImg, float[] sigmas, boolean maskFlag, int iterations, boolean conversion)
	{
		this(null, srcImg, sigmas, maskFlag, iterations, conversion);
	}
	
	

	
	/**
	 * New Deconvolution algorithm, applies the deconvolution based on the
	 * sigma values and iterations to the input image. Stores the output in the destImage.
	 * @param destImg
	 * @param srcImg
	 * @param sigmas
	 * @param maskFlag
	 * @param iterations
	 * @param conversion
	 */
	public OpenCLAlgorithmDeconvolution(ModelImage destImg, ModelImage srcImg, 
			float[] sigmas, boolean maskFlag, int iterations, boolean conversion)
	{
		this(destImg, srcImg, sigmas, maskFlag, iterations, conversion, CL.CL_DEVICE_TYPE_GPU);
	}
	
	
	/**
	 * New Deconvolution algorithm, applies the deconvolution based on the
	 * sigma values and iterations to the input image. Stores the output in the destImage.
	 * Enables the user to specify the device type (CL.CL_DEVICE_TYPE_GPU or CL.CL_DEVICE_TYPE_CPU)
	 * @param destImg
	 * @param srcImg
	 * @param sigmas
	 * @param maskFlag
	 * @param iterations
	 * @param conversion
	 * @param deviceType
	 */
	public OpenCLAlgorithmDeconvolution(ModelImage destImg, ModelImage srcImg, float[] sigmas, 
			boolean maskFlag, int iterations, boolean conversion, long deviceType) {
		super(destImg, srcImg, maskFlag, deviceType);

		this.sigmas = sigmas;
		this.separable = false;
		this.image25D = false;
		this.iterations = iterations;
		this.useConversion = conversion;
	}
	
	/**
	 * New Dual image Deconvolution algorithm, applies the deconvolution based on the
	 * sigma values and iterations to the input image. Overwrites the input imageA as output.
	 * @param srcImgA
	 * @param srcImgB
	 * @param sigmasA
	 * @param sigmasB
	 * @param maskFlag
	 * @param iterations
	 * @param conversion
	 */
	public OpenCLAlgorithmDeconvolution(ModelImage srcImgA, ModelImage srcImgB, float[] sigmasA, float[] sigmasB,
			boolean maskFlag, int iterations, boolean conversion )
	{
		this(null, srcImgA, srcImgB, sigmasA, sigmasB, maskFlag, iterations, conversion);
	}

	/**
	 * New Dual image Deconvolution algorithm, applies the deconvolution based on the
	 * sigma values and iterations to the input image. 
	 * @param destImg
	 * @param srcImgA
	 * @param srcImgB
	 * @param sigmasA
	 * @param sigmasB
	 * @param maskFlag
	 * @param iterations
	 * @param conversion
	 */
	public OpenCLAlgorithmDeconvolution(ModelImage destImg, ModelImage srcImgA, ModelImage srcImgB, float[] sigmasA, float[] sigmasB,
			boolean maskFlag, int iterations, boolean conversion)
	{
		this(destImg, srcImgA, srcImgB, sigmasA, sigmasB, maskFlag, iterations, conversion, CL.CL_DEVICE_TYPE_GPU);
	}
	
	
	/**
	 * New Dual image Deconvolution algorithm, applies the deconvolution based on the
	 * sigma values and iterations to the input image. Stores the output in the destImage.
	 * Enables the user to specify the device type (CL.CL_DEVICE_TYPE_GPU or CL.CL_DEVICE_TYPE_CPU)
	 * @param destImg
	 * @param srcImgA
	 * @param srcImgB
	 * @param sigmasA
	 * @param sigmasB
	 * @param maskFlag
	 * @param iterations
	 * @param conversion
	 * @param deviceType
	 */
	public OpenCLAlgorithmDeconvolution(ModelImage destImg, ModelImage srcImgA, ModelImage srcImgB, 
			float[] sigmasA, float[] sigmasB, 
			boolean maskFlag, int iterations, boolean conversion, long deviceType)
	{
		this( destImg, srcImgA, sigmasA, maskFlag, iterations, conversion, deviceType);
		this.srcImageB = srcImgB;
		this.sigmasB = sigmasB;
	}
	/**
	 * Prepares this class for destruction.
	 */
	public void finalize() {
		destImage = null;
		srcImage = null;
		srcImageB = null;
		colorMask = null;
		sigmas = null;
		sigmasB = null;
		super.finalize();
	}
	
	/**
	 * Starts the program.
	 */
	public void runAlgorithm() {

		if (srcImage == null) {
			displayError("Source Image is null");
			finalize();

			return;
		}
		// 3D Image:
		if ( srcImage.getNDims() == 3 )
		{
			deconvolutionSep3D_2(0);
		} 
		// 3D Image:
		else if (srcImage.getNDims() == 4)
		{
			deconvolution4D();
		}		

		setCompleted(true);
	}
	

	/**
	 * Calls Deconvolution for each volume in the time series image.
	 */
	private void deconvolution4D( )
	{
		for ( int i = 0; i < time; i++ )
		{
			deconvolutionSep3D_2(i);
		}
	}

	/**
	 * Single - Image deconvolution
	 * @param time
	 */
	private void deconvolutionSep3D_2( int time )
	{
		// If srcImageB is not null, call the dual-image deconvolution.
		if ( srcImageB != null )
		{
			deconvolutionSep3D_Dual(time);
			return;
		}
		
		// Initialize OpenCL for the given device type:
		initCL(m_iDeviceType, null);

		// Test Memory for the GPU:
		int nBuffers = 4;
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
				MipavUtil.displayError( "Image size too big." );
				return;
			}
		}
		
		// Export the source image:
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

		int[] errcode = new int[1];
		//
		// Create the input, estimate, blurred, correlation buffers:
		//
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		checkError(errcode[0]);

		cl_mem estimateBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		checkError(errcode[0]);

		cl_mem blurredBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_float * input.length, null, errcode);
		checkError(errcode[0]);

		cl_mem tempBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_float * input.length, null, errcode);
		checkError(errcode[0]);

		//
		// Create and initialize the convolution array buffers:
		//
		derivativeX = new cl_mem[1];
		derivativeY = new cl_mem[1];
		derivativeZ = new cl_mem[1];
		kExtents = new int[1][];
		kOrigins = new int[1][];
		initConvolutionBuffers(0, sigmas);
		
		//
		// create command queue:
		//
		cl_command_queue commandQueue = clCreateCommandQueue(cl, device, 0, errcode);
		checkError(errcode[0]);
		long globalWorkSize[] = new long[]{width,height};
		

		int[] index = new int[1];
		Pointer indexP = Pointer.to(index);

		// Read the program source code and create the program
		String source = OpenCLAlgorithmBase.readKernelFile("Deconvolution.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		checkError( clBuildProgram(program, 0, null, "-cl-mad-enable", null, null) );
		
		// 
		// Set up the OpenCL kernels for the first blur and scale:
		// blurredBuffer = original / gaussian_blur( estimate, sigmas )
		//
		// Set up the X convolution kernel and arugments:
		String kernelName = (color == 1) ? "convolveX" : "convolveX_color";
		cl_kernel kernelX_1 = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		int argX = 0;
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_mem, Pointer.to(derivativeX[0]));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][0]}));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][0]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelX_1, argX++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelX_1, argX, Sizeof.cl_int, indexP);
		int sliceArgX_1 = argX;


		// Set up the Y convolution kernel and arguments:
		kernelName = (color == 1) ? "convolveY" : "convolveY_color";
		cl_kernel kernelY_1 = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		int argY = 0;
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_mem, Pointer.to(derivativeY[0]));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][1]}));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][1]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelY_1, argY++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelY_1, argY, Sizeof.cl_int, indexP);
		int sliceArgY_1 = argY;


		// Set up the Z convolution kernel and arguments:
		// During the first blur the convolution result is divided into the original input
		// the kernel also does the input / blurred step in the algorirhm.
		kernelName = (color == 1) ? "convolveZDiv" : "convolveZDiv_color";
		cl_kernel kernelZ_Div = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		int argZDiv = 0;
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_mem, Pointer.to(derivativeZ[0]));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][2]}));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][2]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		boolean clipZ = true;
		int clip = clipZ ? 1 : 0;
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int, Pointer.to(new int[]{clip}));
		clSetKernelArg(kernelZ_Div, argZDiv, Sizeof.cl_int, indexP);
		int sliceArgZDiv = argZDiv;


		
		
		
		
		

		// 
		// Set up the OpenCL kernels for the second blur and scale:
		// estimateBuffer *= gaussian_blur( blurBuffer, sigmas )
		//
		// Set up the X convolution kernel and arugments:
		kernelName = (color == 1) ? "convolveX" : "convolveX_color";
		cl_kernel kernelX_2 = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argX = 0;
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_mem, Pointer.to(derivativeX[0]));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][0]}));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][0]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelX_2, argX++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelX_2, argX, Sizeof.cl_int, indexP);
		int sliceArgX_2 = argX;


		// Set up the Y convolution kernel and arguments:
		kernelName = (color == 1) ? "convolveY" : "convolveY_color";
		cl_kernel kernelY_2 = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argY = 0;
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_mem, Pointer.to(derivativeY[0]));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][1]}));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][1]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelY_2, argY++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelY_2, argY, Sizeof.cl_int, indexP);
		int sliceArgY_2 = argY;
		
		// Set up the Z convolution kernel and results
		// This is the Z convolution for the second blur, which multiplies the results
		// back into the estimate image. This kernel does that multiply step.
		kernelName = (color == 1) ? "convolveZMult" : "convolveZMult_color";
		cl_kernel kernelZ_Mult = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		int argZMult = 0;
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_mem, Pointer.to(derivativeZ[0]));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][2]}));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][2]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int, Pointer.to(new int[]{clip}));
		clSetKernelArg(kernelZ_Mult, argZMult, Sizeof.cl_int, indexP);
		int sliceArgZMult = argZMult;
		
				
		// Iterate over the algorithm:
		for ( int iter = 0; iter < iterations; iter++ )
		{
			// 
			// Run the OpenCL kernels for the first blur and scale:
			// blurredBuffer = original / gaussian_blur( estimate, sigmas )
			//
			//convolve X:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelX_1, sliceArgX_1, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelX_1, 2, null, globalWorkSize, null, 0, null, null) );
			}
			
			//convolve Y:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelY_1, sliceArgY_1, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelY_1, 2, null, globalWorkSize, null, 0, null, null) );
			}
			
			//convolve Z and divide the original image by the result:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelZ_Div, sliceArgZDiv, Sizeof.cl_int, indexP) );
				checkError(  clEnqueueNDRangeKernel(commandQueue, kernelZ_Div, 2, null, globalWorkSize, null, 0, null, null) );
			}		

			

			// 
			// Run the OpenCL kernels for the second blur and scale:
			// estimateBuffer *= gaussian_blur( blurBuffer, sigmas )
			//
			//convolve X:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelX_2, sliceArgX_2, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelX_2, 2, null, globalWorkSize, null, 0, null, null) );
			}
			
			//convolve Y:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelY_2, sliceArgY_2, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelY_2, 2, null, globalWorkSize, null, 0, null, null) );
			}
			
			//convolve Z and multiply back into the estimate:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelZ_Mult, sliceArgZMult, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelZ_Mult, 2, null, globalWorkSize, null, 0, null, null) );
			}
		}

		checkError( clFinish(commandQueue) );
		
		// Create the CPU-output buffer:
		float[] output = new float[ elementCount ];
		// Read the OpenCL object into CPU memory:
		clEnqueueReadBuffer(commandQueue, estimateBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		// save the output to the ModelImage
		saveImage(output, time, true );

		// release the OpenCL buffers:
		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(estimateBuffer);
		clReleaseMemObject(blurredBuffer);
		clReleaseMemObject(tempBuffer);
		// release the derivative buffers:
		clReleaseMemObject(derivativeX[0]);
		clReleaseMemObject(derivativeY[0]);
		clReleaseMemObject(derivativeZ[0]);
		
		input = null;
		output = null;
	}

	/**
	 * Unused, for testing...
	 * @param time
	 */
	private void deconvolutionSep3D_2A( int time )
	{
		if ( srcImageB != null )
		{
			deconvolutionSep3D_Dual(time);
			return;
		}
		initCL(m_iDeviceType, null);

		// Test Memory for the GPU:
		int nBuffers = 4;
		int elementCount = width * height * depth * color;	
		long memoryUsed = getMaxMemoryUsed( nBuffers, elementCount, sigmas );	
		long maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
		long totalMemSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_GLOBAL_MEM_SIZE);
		if ( (elementCount > (maxAllocSize / (Sizeof.cl_float))) || (memoryUsed >= (totalMemSize / Sizeof.cl_float)) )
		{
			MipavUtil.displayInfo( "Not enough GPU memory. Calling CPU version" );
			m_iDeviceType = CL.CL_DEVICE_TYPE_CPU;
			initCL(m_iDeviceType, null);
			maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
			if ( elementCount > (maxAllocSize / (Sizeof.cl_float)) )
			{
				MipavUtil.displayError( "Image size too big." );
				return;
			}
		}
		long totalMemUsed = 0;
		
		// Export the source image:
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

		int[] errcode = new int[1];
		//
		// Create the input, estimate, blurred, correlation buffers:
		//
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		totalMemUsed += input.length;

		cl_mem estimateBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		totalMemUsed += input.length;

		cl_mem blurredBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * input.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		totalMemUsed += input.length;

		cl_mem correlationBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * input.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		totalMemUsed += input.length;
		//
		// Create the convolution buffers:    
		//
		double conversion = 1.0 / (2*Math.sqrt(2*Math.log(2)));
		for ( int i = 0; i < sigmas.length; i++ )
		{
			//sigmas[i] *= conversion;
		}
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.BLUR_KERNEL);
		Kernel gaussianKernel = gkf.createKernel();
		float[][] derivativeKernel = gaussianKernel.getData();
		int[] kExtents = gaussianKernel.getExtents();
		
//		float[][] derivativeKernel = new float[3][];
//		derivativeKernel[0] = getKernel(sigmas[0]);
//		derivativeKernel[1] = getKernel(sigmas[1]);
//		derivativeKernel[2] = getKernel(sigmas[2]);
//		int[] kExtents = new int[]{ derivativeKernel[0].length, derivativeKernel[1].length, derivativeKernel[2].length };
		
		int[] kOrigins = new int[kExtents.length];
		for ( int i = 0; i < kOrigins.length; i++ )
		{
			kOrigins[i] = (kExtents[i]-1)>>1;
		}
		System.err.println( kExtents[0] + " " + kExtents[1] + " " + kExtents[2] );
		System.err.println( kOrigins[0] + " " + kOrigins[1] + " " + kOrigins[2] );
		// x-convolution buffer:
		cl_mem derivativeX = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[0].length, Pointer.to(derivativeKernel[0]), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		totalMemUsed += derivativeKernel[0].length;
		// y-convolution buffer:
		cl_mem derivativeY = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[1].length, Pointer.to(derivativeKernel[1]), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		totalMemUsed += derivativeKernel[1].length;
		// z-convolution buffer:
		cl_mem derivativeZ = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[2].length, Pointer.to(derivativeKernel[2]), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		totalMemUsed += derivativeKernel[2].length;
		
		System.err.println( totalMemUsed + "   " + memoryUsed + "   " +  (totalMemSize / Sizeof.cl_float) );
		
		// At this point all the buffers have been created.
		
		
		
		
		
		
		
	

		// Create divide and multiply kernels:
		String divSource =
				"__kernel void "+
						"divide(__global float *a,"+
						"       __global float *b)"+
						"{"+
						"    int gid = get_global_id(0);"+
						"    b[gid] = a[gid] / b[gid];"+
						"}";
		String multSource =
				"__kernel void "+
						"multiply(__global float *a,"+
						"         __global float *b)"+
						"{"+
						"    int gid = get_global_id(0);"+
						"    b[gid] = a[gid] * b[gid];"+
						"}";

		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ divSource + multSource }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		cl_kernel divKernel = clCreateKernel(program, "divide", null);
		clSetKernelArg(divKernel, 0, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(divKernel, 1, Sizeof.cl_mem, Pointer.to(blurredBuffer));

		cl_kernel multKernel = clCreateKernel(program, "multiply", null);
		clSetKernelArg(multKernel, 0, Sizeof.cl_mem, Pointer.to(correlationBuffer));
		clSetKernelArg(multKernel, 1, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		
		// Set the work-item dimensions
		long global_work_size[] = new long[]{elementCount};

		
		
		
		

		// Read the program source code and create the program
		String source = OpenCLAlgorithmBase.readKernelFile("Deconvolution.cl");
		program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		errcode[0] = clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		// Set up 3D Kernel:
		String kernelName = (color == 1) ? "convolveX" : "convolveX_color";
		cl_kernel kernelX = clCreateKernel(program, kernelName, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		kernelName = (color == 1) ? "convolveY" : "convolveY_color";
		cl_kernel kernelY = clCreateKernel(program, kernelName, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		kernelName = (color == 1) ? "convolveZ" : "convolveZ_color";
		cl_kernel kernelZ = clCreateKernel(program, kernelName, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		// create command queue:
		cl_command_queue commandQueue = clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}


		long globalWorkSize[] = new long[]{width,height};

		// Set up 2D Kernel:		
		int argX = 0;
		clSetKernelArg(kernelX, argX++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelX, argX++, Sizeof.cl_mem, Pointer.to(derivativeX));
		clSetKernelArg(kernelX, argX++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelX, argX++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelX, argX++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0]}));
		clSetKernelArg(kernelX, argX++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelX, argX++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelX, argX, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArgX = argX;



		int argY = 0;
		clSetKernelArg(kernelY, argY++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelY, argY++, Sizeof.cl_mem, Pointer.to(derivativeY));
		clSetKernelArg(kernelY, argY++, Sizeof.cl_mem, Pointer.to(correlationBuffer));
		clSetKernelArg(kernelY, argY++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelY, argY++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[1]}));
		clSetKernelArg(kernelY, argY++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[1]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelY, argY++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelY, argY, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArgY = argY;


		// Set up 2D Kernel:		
		int argZ = 0;
		clSetKernelArg(kernelZ, argZ++, Sizeof.cl_mem, Pointer.to(correlationBuffer));
		clSetKernelArg(kernelZ, argZ++, Sizeof.cl_mem, Pointer.to(derivativeZ));
		clSetKernelArg(kernelZ, argZ++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelZ, argZ++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(kernelZ, argZ++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[2]}));
		clSetKernelArg(kernelZ, argZ++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[2]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelZ, argZ++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		boolean clipZ = false;
		int clip = clipZ ? 1 : 0;
		clSetKernelArg(kernelZ, argZ++, Sizeof.cl_int, Pointer.to(new int[]{clip}));
		clSetKernelArg(kernelZ, argZ, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArgZ = argZ;
		
		

        long start = System.currentTimeMillis();
		// Iterate over the algorithm:
		for ( int iter = 0; iter < iterations; iter++ )
		{
			start = System.currentTimeMillis();
			//convolve X:
			clSetKernelArg(kernelX, 0, Sizeof.cl_mem, Pointer.to(estimateBuffer));
			clSetKernelArg(kernelX, 2, Sizeof.cl_mem, Pointer.to(blurredBuffer));
			for ( int i = 0; i < depth; i++ )
			{
				errcode[0] = clSetKernelArg(kernelX, sliceArgX, Sizeof.cl_int, Pointer.to(new int[]{i}));
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
				errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernelX, 2, null, globalWorkSize, null, 0, null, null);
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
			}
			
			//convolve Y:
			clSetKernelArg(kernelY, 0, Sizeof.cl_mem, Pointer.to(blurredBuffer));
			clSetKernelArg(kernelY, 2, Sizeof.cl_mem, Pointer.to(correlationBuffer));
			for ( int i = 0; i < depth; i++ )
			{
				errcode[0] = clSetKernelArg(kernelY, sliceArgY, Sizeof.cl_int, Pointer.to(new int[]{i}));
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
				errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernelY, 2, null, globalWorkSize, null, 0, null, null);
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
			}
			
			//convolve Z:
			clSetKernelArg(kernelZ, 0, Sizeof.cl_mem, Pointer.to(correlationBuffer));
			clSetKernelArg(kernelZ, 2, Sizeof.cl_mem, Pointer.to(blurredBuffer));
			for ( int i = 0; i < depth; i++ )
			{
				errcode[0] = clSetKernelArg(kernelZ, sliceArgZ, Sizeof.cl_int, Pointer.to(new int[]{i}));
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
				errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernelZ, 2, null, globalWorkSize, null, 0, null, null);
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
			//System.err.println( "Convolve " + iter + " " + computeElapsedTime(start) );
			start = System.currentTimeMillis();

			// divide inputBuffer / blurredBuffer
			clEnqueueNDRangeKernel(commandQueue, divKernel, 1, null,
					global_work_size, null, 0, null, null);
			errcode[0] = clFinish(commandQueue);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
			}
			//System.err.println( "div " + iter + " " + computeElapsedTime(start) );
			start = System.currentTimeMillis();

			// blur result
			//convolve X:
			clSetKernelArg(kernelX, 0, Sizeof.cl_mem, Pointer.to(blurredBuffer));
			clSetKernelArg(kernelX, 2, Sizeof.cl_mem, Pointer.to(correlationBuffer));
			for ( int i = 0; i < depth; i++ )
			{
				errcode[0] = clSetKernelArg(kernelX, sliceArgX, Sizeof.cl_int, Pointer.to(new int[]{i}));
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
				errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernelX, 2, null, globalWorkSize, null, 0, null, null);
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
			}
			
			//convolve Y:
			clSetKernelArg(kernelY, 0, Sizeof.cl_mem, Pointer.to(correlationBuffer));
			clSetKernelArg(kernelY, 2, Sizeof.cl_mem, Pointer.to(blurredBuffer));
			for ( int i = 0; i < depth; i++ )
			{
				errcode[0] = clSetKernelArg(kernelY, sliceArgY, Sizeof.cl_int, Pointer.to(new int[]{i}));
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
				errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernelY, 2, null, globalWorkSize, null, 0, null, null);
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
			}
			
			//convolve Z:
			clSetKernelArg(kernelZ, 0, Sizeof.cl_mem, Pointer.to(blurredBuffer));
			clSetKernelArg(kernelZ, 2, Sizeof.cl_mem, Pointer.to(correlationBuffer));
			for ( int i = 0; i < depth; i++ )
			{
				errcode[0] = clSetKernelArg(kernelZ, sliceArgZ, Sizeof.cl_int, Pointer.to(new int[]{i}));
				if ( errcode[0] != CL.CL_SUCCESS )
				{
					System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
				}
				errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernelZ, 2, null, globalWorkSize, null, 0, null, null);
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
			//System.err.println( "Convolve2 " + iter + " " + computeElapsedTime(start) );
			start = System.currentTimeMillis();
			
			// multiply outputBuffer * correlationBuffer
			clEnqueueNDRangeKernel(commandQueue, multKernel, 1, null,
					global_work_size, null, 0, null, null);
			errcode[0] = clFinish(commandQueue);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
			}
			//System.err.println( "Mult " + iter + " " + computeElapsedTime(start) );
			start = System.currentTimeMillis();
		}

		errcode[0] = clFinish(commandQueue);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
		}
		
		// Create the CPU-output buffer:
		float[] output = new float[ elementCount ];
		// Read the OpenCL object into CPU memory:
		clEnqueueReadBuffer(commandQueue, estimateBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		// save the output to the ModelImage
		saveImage(output, time, true );

		// release the OpenCL buffers:
		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(estimateBuffer);
		clReleaseMemObject(blurredBuffer);
		clReleaseMemObject(correlationBuffer);
		// release the derivative buffers:
		clReleaseMemObject(derivativeX);
		clReleaseMemObject(derivativeY);
		clReleaseMemObject(derivativeZ);
		
		input = null;
		output = null;
	}
	
	
	
	
	
	
	
	
	

	/**
	 * Dual image deconvolution.
	 * @param time
	 */
	private synchronized void deconvolutionSep3D_Dual( int time )
	{
//		try {
			initCL(m_iDeviceType, null);
//		} catch (org.jocl.CLException e )
//		{
//			System.err.println( e.getStackTrace() );
//			return;
//		}

		// Test Memory for the GPU:
		int nBuffers = 5;
		int elementCount = width * height * depth * color;	
		long memoryUsed = getMaxMemoryUsed( nBuffers, elementCount, sigmas ) + getMaxMemoryUsed( sigmasB );	
		long maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
		long totalMemSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_GLOBAL_MEM_SIZE);
		if ( (elementCount > (maxAllocSize / (Sizeof.cl_float))) || (memoryUsed >= (totalMemSize / Sizeof.cl_float)) )
		{
			MipavUtil.displayInfo( "Not enough GPU memory. Calling CPU version" );
			m_iDeviceType = CL.CL_DEVICE_TYPE_CPU;
			initCL(m_iDeviceType, null);
			maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
			if ( elementCount > (maxAllocSize / (Sizeof.cl_float)) )
			{
				MipavUtil.displayError( "Image size too big." );
				return;
			}
		}
		
		// Export the source image:
		float[] inputA = new float[ elementCount ];
		try {
			if ( this.entireImage )
			{
				srcImage.exportData( time * inputA.length, inputA.length, inputA );
			}
			else
			{
				srcImage.exportDataUseMask( time * inputA.length, inputA.length, inputA );
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
		float[] inputB = new float[ elementCount ];
		try {
			if ( this.entireImage )
			{
				srcImageB.exportData( time * inputB.length, inputB.length, inputB );
			}
			else
			{
				srcImageB.exportDataUseMask( time * inputB.length, inputB.length, inputB );
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		int[] errcode = new int[1];
		//
		// Create the input, estimate, blurred, correlation buffers:
		//
		cl_mem inputBufferA = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * inputA.length, Pointer.to(inputA), errcode);
		checkError(errcode[0]);
		
		cl_mem inputBufferB = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * inputB.length, Pointer.to(inputB), errcode);
		checkError(errcode[0]);

		cl_mem estimateBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_float * elementCount, null, errcode);
		checkError(errcode[0]);

		cl_mem blurredBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_float * elementCount, null, errcode);
		checkError(errcode[0]);

		cl_mem tempBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_float * elementCount, null, errcode);
		checkError(errcode[0]);

		
		// create command queue:
		cl_command_queue commandQueue = clCreateCommandQueue(cl, device, 0, errcode);
		checkError(errcode[0]);
		long globalWorkSize[] = new long[]{width,height};
		
		// initialize the estimateBuffer to 0.5 * (inputBufferA + inputBufferB);
		initEstimate( commandQueue, elementCount, inputBufferA, inputBufferB, estimateBuffer );

		initReductionKernel( commandQueue, elementCount, estimateBuffer );
		initScaleKernel( commandQueue, elementCount, estimateBuffer );
		
		//
		// Create the convolution buffers:    
		//
		derivativeX = new cl_mem[2];
		derivativeY = new cl_mem[2];
		derivativeZ = new cl_mem[2];
		kExtents = new int[2][];
		kOrigins = new int[2][];
		initConvolutionBuffers(0, sigmas);
		initConvolutionBuffers(1, sigmasB);
		// At this point all the buffers have been created.			

		// Read the program source code and create the program
		String source = OpenCLAlgorithmBase.readKernelFile("Deconvolution.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		checkError( clBuildProgram(program, 0, null, "-cl-mad-enable", null, null) );


		int[] index = new int[1];
		Pointer indexP = Pointer.to(index);

		//
		// The following sets up:
		// 
        //  estimate *= blur(data_A / blur(estimate, view='a'), view='a')
		//
		//
		// Inner blur and divide:
		//
		// Set up the X convolution kernel and arugments:
		String kernelName = (color == 1) ? "convolveX" : "convolveX_color";
		cl_kernel kernelX_1 = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		int argX = 0;
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_mem, Pointer.to(derivativeX[0]));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][0]}));
		clSetKernelArg(kernelX_1, argX++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][0]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelX_1, argX++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelX_1, argX, Sizeof.cl_int, indexP);
		int sliceArgX_1A = argX;


		// Set up the Y convolution kernel and arguments:
		kernelName = (color == 1) ? "convolveY" : "convolveY_color";
		cl_kernel kernelY_1 = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		int argY = 0;
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_mem, Pointer.to(derivativeY[0]));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][1]}));
		clSetKernelArg(kernelY_1, argY++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][1]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelY_1, argY++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelY_1, argY, Sizeof.cl_int, indexP);
		int sliceArgY_1A = argY;


		// Set up the Z convolution kernel and arguments:
		// During the first blur the convolution result is divided into the original input
		// the kernel also does the input / blurred step in the algorirhm.
		kernelName = (color == 1) ? "convolveZDiv" : "convolveZDiv_color";
		cl_kernel kernelZ_Div = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		int argZDiv = 0;
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_mem, Pointer.to(derivativeZ[0]));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_mem, Pointer.to(inputBufferA));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][2]}));
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][2]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		boolean clipZ = false;
		int clip = clipZ ? 1 : 0;
		clSetKernelArg(kernelZ_Div, argZDiv++, Sizeof.cl_int, Pointer.to(new int[]{clip}));
		clSetKernelArg(kernelZ_Div, argZDiv, Sizeof.cl_int, indexP);
		int sliceArgZDivA = argZDiv;

		//
		// Outer blur and mult:
		//
		// Set up the X convolution kernel and arugments:
		kernelName = (color == 1) ? "convolveX" : "convolveX_color";
		cl_kernel kernelX_2 = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argX = 0;
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_mem, Pointer.to(derivativeX[0]));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][0]}));
		clSetKernelArg(kernelX_2, argX++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][0]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelX_2, argX++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelX_2, argX, Sizeof.cl_int, indexP);
		int sliceArgX_2A = argX;


		// Set up the Y convolution kernel and arguments:
		kernelName = (color == 1) ? "convolveY" : "convolveY_color";
		cl_kernel kernelY_2 = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argY = 0;
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_mem, Pointer.to(derivativeY[0]));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][1]}));
		clSetKernelArg(kernelY_2, argY++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][1]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelY_2, argY++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelY_2, argY, Sizeof.cl_int, indexP);
		int sliceArgY_2A = argY;
		
		// Set up the Z convolution kernel and results
		// This is the Z convolution for the second blur, which multiplies the results
		// back into the estimate image. This kernel does that multiply step.
		kernelName = (color == 1) ? "convolveZMult" : "convolveZMult_color";
		cl_kernel kernelZ_Mult = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		int argZMult = 0;
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_mem, Pointer.to(derivativeZ[0]));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0][2]}));
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0][2]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelZ_Mult, argZMult++, Sizeof.cl_int, Pointer.to(new int[]{clip}));
		clSetKernelArg(kernelZ_Mult, argZMult, Sizeof.cl_int, indexP);
		int sliceArgZMultA = argZMult;
		
		
		
		//
		// The following sets up:
		// 
        //  estimate *= blur(data_B / blur(estimate, view='b'), view='b')
		//
		//
		// Inner blur and divide:
		//
		// Set up the X convolution kernel and arugments:
		kernelName = (color == 1) ? "convolveX" : "convolveX_color";
		cl_kernel kernelX_1B = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argX = 0;
		clSetKernelArg(kernelX_1B, argX++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelX_1B, argX++, Sizeof.cl_mem, Pointer.to(derivativeX[1]));
		clSetKernelArg(kernelX_1B, argX++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelX_1B, argX++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelX_1B, argX++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[1][0]}));
		clSetKernelArg(kernelX_1B, argX++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[1][0]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelX_1B, argX++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelX_1B, argX, Sizeof.cl_int, indexP);
		int sliceArgX_1B = argX;


		// Set up the Y convolution kernel and arguments:
		kernelName = (color == 1) ? "convolveY" : "convolveY_color";
		cl_kernel kernelY_1B = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argY = 0;
		clSetKernelArg(kernelY_1B, argY++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelY_1B, argY++, Sizeof.cl_mem, Pointer.to(derivativeY[1]));
		clSetKernelArg(kernelY_1B, argY++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelY_1B, argY++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelY_1B, argY++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[1][1]}));
		clSetKernelArg(kernelY_1B, argY++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[1][1]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelY_1B, argY++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelY_1B, argY, Sizeof.cl_int, indexP);
		int sliceArgY_1B = argY;


		// Set up the Z convolution kernel and arguments:
		// During the first blur the convolution result is divided into the original input
		// the kernel also does the input / blurred step in the algorirhm.
		kernelName = (color == 1) ? "convolveZDiv" : "convolveZDiv_color";
		cl_kernel kernelZ_DivB = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argZDiv = 0;
		clSetKernelArg(kernelZ_DivB, argZDiv++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelZ_DivB, argZDiv++, Sizeof.cl_mem, Pointer.to(derivativeZ[1]));
		clSetKernelArg(kernelZ_DivB, argZDiv++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelZ_DivB, argZDiv++, Sizeof.cl_mem, Pointer.to(inputBufferB));
		clSetKernelArg(kernelZ_DivB, argZDiv++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(kernelZ_DivB, argZDiv++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[1][2]}));
		clSetKernelArg(kernelZ_DivB, argZDiv++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[1][2]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelZ_DivB, argZDiv++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelZ_DivB, argZDiv++, Sizeof.cl_int, Pointer.to(new int[]{clip}));
		clSetKernelArg(kernelZ_DivB, argZDiv, Sizeof.cl_int, indexP);
		int sliceArgZDivB = argZDiv;

		//
		// Outer blur and mult:
		//
		// Set up the X convolution kernel and arugments:
		kernelName = (color == 1) ? "convolveX" : "convolveX_color";
		cl_kernel kernelX_2B = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argX = 0;
		clSetKernelArg(kernelX_2B, argX++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelX_2B, argX++, Sizeof.cl_mem, Pointer.to(derivativeX[1]));
		clSetKernelArg(kernelX_2B, argX++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelX_2B, argX++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelX_2B, argX++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[1][0]}));
		clSetKernelArg(kernelX_2B, argX++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[1][0]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelX_2B, argX++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelX_2B, argX, Sizeof.cl_int, indexP);
		int sliceArgX_2B = argX;


		// Set up the Y convolution kernel and arguments:
		kernelName = (color == 1) ? "convolveY" : "convolveY_color";
		cl_kernel kernelY_2B = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argY = 0;
		clSetKernelArg(kernelY_2B, argY++, Sizeof.cl_mem, Pointer.to(tempBuffer));
		clSetKernelArg(kernelY_2B, argY++, Sizeof.cl_mem, Pointer.to(derivativeY[1]));
		clSetKernelArg(kernelY_2B, argY++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelY_2B, argY++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelY_2B, argY++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[1][1]}));
		clSetKernelArg(kernelY_2B, argY++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[1][1]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelY_2B, argY++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelY_2B, argY, Sizeof.cl_int, indexP);
		int sliceArgY_2B = argY;
		
		// Set up the Z convolution kernel and results
		// This is the Z convolution for the second blur, which multiplies the results
		// back into the estimate image. This kernel does that multiply step.
		kernelName = (color == 1) ? "convolveZMult" : "convolveZMult_color";
		cl_kernel kernelZ_MultB = clCreateKernel(program, kernelName, errcode);
		checkError(errcode[0]);
		argZMult = 0;
		clSetKernelArg(kernelZ_MultB, argZMult++, Sizeof.cl_mem, Pointer.to(blurredBuffer));
		clSetKernelArg(kernelZ_MultB, argZMult++, Sizeof.cl_mem, Pointer.to(derivativeZ[1]));
		clSetKernelArg(kernelZ_MultB, argZMult++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelZ_MultB, argZMult++, Sizeof.cl_mem, Pointer.to(estimateBuffer));
		clSetKernelArg(kernelZ_MultB, argZMult++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(kernelZ_MultB, argZMult++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[1][2]}));
		clSetKernelArg(kernelZ_MultB, argZMult++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[1][2]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelZ_MultB, argZMult++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelZ_MultB, argZMult++, Sizeof.cl_int, Pointer.to(new int[]{clip}));
		clSetKernelArg(kernelZ_MultB, argZMult, Sizeof.cl_int, indexP);
		int sliceArgZMultB = argZMult;
		
				
		// Iterate over the algorithm:
		for ( int iter = 0; iter < iterations; iter++ )
		{
			// 
			//  Compute:
	        //  estimate *= blur(data_A / blur(estimate, view='a'), view='a')
			//
			//convolve X:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelX_1, sliceArgX_1A, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelX_1, 2, null, globalWorkSize, null, 0, null, null) );
			}
			
			//convolve Y:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelY_1, sliceArgY_1A, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelY_1, 2, null, globalWorkSize, null, 0, null, null) );
			}
			
			//convolve Z and divide the original image by the result:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelZ_Div, sliceArgZDivA, Sizeof.cl_int, indexP) );
				checkError(  clEnqueueNDRangeKernel(commandQueue, kernelZ_Div, 2, null, globalWorkSize, null, 0, null, null) );
			}		
			// 2nd blur and multiply result into estimate:
			//convolve X:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelX_2, sliceArgX_2A, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelX_2, 2, null, globalWorkSize, null, 0, null, null) );
			}
			
			//convolve Y:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelY_2, sliceArgY_2A, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelY_2, 2, null, globalWorkSize, null, 0, null, null) );
			}
			
			//convolve Z and multiply back into the estimate:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelZ_Mult, sliceArgZMultA, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelZ_Mult, 2, null, globalWorkSize, null, 0, null, null) );
			}
			
			
			

			// 
			//  Compute:
	        //  estimate *= blur(data_B / blur(estimate, view='b'), view='b')
			//
			//convolve X:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelX_1B, sliceArgX_1B, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelX_1B, 2, null, globalWorkSize, null, 0, null, null) );
			}

			//convolve Y:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelY_1B, sliceArgY_1B, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelY_1B, 2, null, globalWorkSize, null, 0, null, null) );
			}

			//convolve Z and divide the original image by the result:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelZ_DivB, sliceArgZDivB, Sizeof.cl_int, indexP) );
				checkError(  clEnqueueNDRangeKernel(commandQueue, kernelZ_DivB, 2, null, globalWorkSize, null, 0, null, null) );
			}		
			// 2nd blur and multiply result into estimate:
			//convolve X:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelX_2B, sliceArgX_2B, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelX_2B, 2, null, globalWorkSize, null, 0, null, null) );
			}

			//convolve Y:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelY_2B, sliceArgY_2B, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelY_2B, 2, null, globalWorkSize, null, 0, null, null) );
			}

			//convolve Z and multiply back into the estimate:
			for ( int i = 0; i < depth; i++ )
			{
				index[0] = i;
				checkError( clSetKernelArg(kernelZ_MultB, sliceArgZMultB, Sizeof.cl_int, indexP) );
				checkError( clEnqueueNDRangeKernel(commandQueue, kernelZ_MultB, 2, null, globalWorkSize, null, 0, null, null) );
			}

			// 
			//  Compute:
	        //  estimate *= total_brightness / estimate.max(axis=0).max()  
			//
			reduce( commandQueue );
			scale( commandQueue, elementCount );
		}

		checkError( clFinish(commandQueue) );
		
		// Create the CPU-output buffer:
		float[] output = new float[ elementCount ];
		// Read the OpenCL object into CPU memory:
		clEnqueueReadBuffer(commandQueue, estimateBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		// save the output to the ModelImage
		saveImage(output, time, true );

		// release the OpenCL buffers:
		clReleaseMemObject(inputBufferA);
		clReleaseMemObject(inputBufferB);
		clReleaseMemObject(estimateBuffer);
		clReleaseMemObject(blurredBuffer);
		clReleaseMemObject(tempBuffer);
		// release the derivative buffers:
		clReleaseMemObject(derivativeX[0]);
		clReleaseMemObject(derivativeY[0]);
		clReleaseMemObject(derivativeZ[0]);
		clReleaseMemObject(derivativeX[1]);
		clReleaseMemObject(derivativeY[1]);
		clReleaseMemObject(derivativeZ[1]);

		
		
		
		
		
		
		

		clReleaseKernel(kernelX_1);
		clReleaseKernel(kernelY_1);
		clReleaseKernel(kernelZ_Div);
		clReleaseKernel(kernelX_2);
		clReleaseKernel(kernelY_2);
		clReleaseKernel(kernelZ_Mult);
		clReleaseKernel(kernelX_1B);
		clReleaseKernel(kernelY_1B);
		clReleaseKernel(kernelZ_DivB);
		clReleaseKernel(kernelX_2B);
		clReleaseKernel(kernelY_2B);
		clReleaseKernel(kernelZ_MultB);
		clReleaseKernel(reductionKernel);
		clReleaseKernel(scaleKernel);
		clReleaseProgram(program);
		clReleaseCommandQueue(commandQueue);
		
		
//		super.releaseContext();

		inputA = null;
		inputB = null;
		output = null;		
	}
	
	/**
	 * Computes a 1D Gaussian kernel for the given sigma value.
	 * @param sigma
	 * @return
	 */
	private float[] getKernel( float sigma )
	{
		float sd = sigma;
	    int lw = (int)(4.0 * sd + 0.5);
	    float[] weights = new float[(2 * lw + 1)];
	    weights[lw] = 1.0f;
	    double sum = 1.0;
	    sd = sd * sd;
	    //calculate the kernel:
	    //for ii in range(1, lw + 1):
	    for ( int ii = 0; ii < lw ; ii++ )
	    {
	    	float tmp = (float)Math.exp(-0.5 * (ii * ii) / sd);
	    	weights[lw + ii] = tmp;
	    	weights[lw - ii] = tmp;
	    	sum += 2.0 * tmp;
	    }
	    //for ii in range(2 * lw + 1):
	    for ( int ii = 0; ii < weights.length ; ii++ )
	    {
	    	weights[ii] /= sum;
	    }
	    return weights;
	}
	
	/**
	 * Determines the amount of memory used for the OpenCL buffers
	 * required for the gaussian kernels for the given sigmas.
	 * @param sigmas
	 * @return
	 */
	private long getMaxMemoryUsed( float[] sigmas )
	{
		int kernelSize = 0;
		for ( int i = 0; i < sigmas.length; i++ )
		{
			int halfWidth = (int)(4.0 * sigmas[i] + 0.5);
			int size = (2 * halfWidth + 1);
			kernelSize += size;
		}
		return kernelSize;
	}
	
	/**
	 * Creates the Convolution kernels (data arrays) in OpenCL.
	 * Creates 3 1D arrays in OpenCL for the x-convolution kernel
	 * the y-convolution kernel, and the z-convolution kernel.
	 * @param index (0 for single-image deconvolution, 0,1 for dual-image)
	 * @param sigmas
	 */
	private void initConvolutionBuffers(int index, float[] sigmas)
	{
		int[] errcode = new int[1];

		//
		// Create the convolution buffers:    
		//
		float[] localSigmas = new float[sigmas.length];
		if ( useConversion )
		{
			double conversion = 1.0 / (2*Math.sqrt(2*Math.log(2)));
			for ( int i = 0; i < sigmas.length; i++ )
			{
				localSigmas[i] = (float) (sigmas[i] * conversion);
			}
		}
		else
		{
			for ( int i = 0; i < sigmas.length; i++ )
			{
				localSigmas[i] = sigmas[i];
			}
		}
//		for ( int i = 0; i < sigmas.length; i++ )
//		{
//			System.err.print( localSigmas[i] + " " );
//		}
//		System.err.println("");
//		System.err.println("");
		
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(localSigmas);
		gkf.setKernelType(GaussianKernelFactory.BLUR_KERNEL);
		Kernel gaussianKernel = gkf.createKernel();
		float[][] derivativeKernel = gaussianKernel.getData();

//		for ( int i = 0; i < derivativeKernel[0].length; i++ )
//			System.err.print( derivativeKernel[0][i] + " " );
//		System.err.println("");
//		System.err.println("");
//		System.err.println("");

//		for ( int i = 0; i < derivativeKernel[1].length; i++ )
//			System.err.print( derivativeKernel[1][i] + " " );
//		System.err.println("");
//		System.err.println("");
//		System.err.println("");

//		for ( int i = 0; i < derivativeKernel[2].length; i++ )
//			System.err.print( derivativeKernel[2][i] + " " );
//		System.err.println("");
//		System.err.println("");
//		System.err.println("");

		int[] kExtentsTemp = gaussianKernel.getExtents();
		kExtents[index] = new int[kExtentsTemp.length];
		for ( int i = 0; i < kExtents[index].length; i++ )
		{
			kExtents[index][i] = kExtentsTemp[i];
		}

		//	float[][] derivativeKernel = new float[3][];
		//	derivativeKernel[0] = getKernel(localSigmas[0]);
		//	derivativeKernel[1] = getKernel(localSigmas[1]);
		//	derivativeKernel[2] = getKernel(localSigmas[2]);
		//	int[] kExtents = new int[]{ derivativeKernel[0].length, derivativeKernel[1].length, derivativeKernel[2].length };

		kOrigins[index] = new int[kExtents[index].length];
		for ( int i = 0; i < kOrigins[index].length; i++ )
		{
			kOrigins[index][i] = (kExtents[index][i]-1)>>1;
		}
//		System.err.println( kExtents[index][0] + " " + kExtents[index][1] + " " + kExtents[index][2] );
//		System.err.println( kOrigins[index][0] + " " + kOrigins[index][1] + " " + kOrigins[index][2] );
		
		// x-convolution buffer:
		derivativeX[index] = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[0].length, Pointer.to(derivativeKernel[0]), errcode);
		checkError(errcode[0]);
		
		// y-convolution buffer:
		derivativeY[index] = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[1].length, Pointer.to(derivativeKernel[1]), errcode);
		checkError(errcode[0]);
		
		// z-convolution buffer:
		derivativeZ[index] = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[2].length, Pointer.to(derivativeKernel[2]), errcode);
		checkError(errcode[0]);
	}
	
	
	/**
	 * Creates the first estimate image for dual-image deconvolution
	 * estimate = 0.5 * (imageBufferA + imageBufferB).
	 * This is calculated in OpenCL and the results stored in the OpenCL buffer.
	 * @param commandQueue 
	 * @param elementCount
	 * @param inputBufferA input A OpenCL buffer
	 * @param inputBufferB input B OpenCL buffer
	 * @param estimateBuffer output OpenCL buffer
	 */
	private void initEstimate( cl_command_queue commandQueue, long elementCount, 
			cl_mem inputBufferA, cl_mem inputBufferB, cl_mem estimateBuffer )
	{

		String avgSource =
				"__kernel void "+
						"avg(__global float *a,"+
						"         __global float *b,"+
						"         __global float *c)"+
						"{"+
						"    int gid = get_global_id(0);"+
						"    c[gid] = 0.5*(a[gid] + b[gid]);"+
						"}";
		long global_work_size[] = new long[]{elementCount};

		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ avgSource }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		cl_kernel multKernel = clCreateKernel(program, "avg", null);
		clSetKernelArg(multKernel, 0, Sizeof.cl_mem, Pointer.to(inputBufferA));
		clSetKernelArg(multKernel, 1, Sizeof.cl_mem, Pointer.to(inputBufferB));
		clSetKernelArg(multKernel, 2, Sizeof.cl_mem, Pointer.to(estimateBuffer));

		clEnqueueNDRangeKernel(commandQueue, multKernel, 1, null, global_work_size, null, 0, null, null);
	}

	/**
	 * Inializes the reduction kernel to calculate the maximum of the estimateBuffer
	 * during the dual-image deconvolution. Returns the max of the initial buffer.
	 * @param commandQueue
	 * @param elementCount
	 * @param inputBuffer, the estimateBuffer
	 * @return maximum of the inputBuffer
	 */
	private float initReductionKernel( cl_command_queue commandQueue, long elementCount, cl_mem inputBuffer )
	{
		int[] errcode = new int[1];
		maxWorkSize = Math.min( 512, OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_WORK_GROUP_SIZE) );
		if ( reductionKernel == null )
		{
			// Read the program source code and create the program
			String source = OpenCLAlgorithmBase.readKernelFile("ParallelReduction.cl");
			cl_program program = clCreateProgramWithSource(cl, 1, new String[]{ source }, null, errcode);
			checkError(errcode[0]);
			checkError( clBuildProgram(program, 0, null, "-cl-mad-enable", null, null) );
			reductionKernel = clCreateKernel(program, "reduceMax", errcode);
			checkError(errcode[0]);

		    sum = new float[ 1 ];
			sumBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_float * sum.length, null, errcode);
			checkError(errcode[0]);
			brightnessBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_float * sum.length, null, errcode);
			checkError(errcode[0]);
			
			int arg = 0;
			checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer)) );
			checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_mem, Pointer.to(brightnessBuffer)) );
			checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_long, Pointer.to(new long[]{elementCount})) );
			checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_long, Pointer.to(new long[]{maxWorkSize})) );
			checkError( clSetKernelArg(reductionKernel, arg++, Sizeof.cl_mem*maxWorkSize, null) );
		}
		long local_work_size[] = new long[]{maxWorkSize};
		long global_work_size[] = new long[]{maxWorkSize};
		
		// initial reduction is into brightnessBuffer:
		checkError( clEnqueueNDRangeKernel(commandQueue, reductionKernel, 1, null, global_work_size, local_work_size, 0, null, null) );

		checkError( clFinish(commandQueue) );		
		checkError( clEnqueueReadBuffer(commandQueue, brightnessBuffer, CL_TRUE, 0, Sizeof.cl_float * sum.length, Pointer.to(sum), 0, null, null) );

		// continue reduction into the sumBuffer
		checkError( clSetKernelArg(reductionKernel, 1, Sizeof.cl_mem, Pointer.to(sumBuffer)) );
		
	    return sum[0];
	}


	/**
	 * Initialize the scale OpenCL kernel which calculates
	 * estimateBuffer *= max(estimateBuffer)
	 * @param commandQueue
	 * @param elementCount
	 * @param inputBuffer
	 */
	private void initScaleKernel( cl_command_queue commandQueue, long elementCount, cl_mem inputBuffer )
	{
		int[] errcode = new int[1];
		if ( scaleKernel == null )
		{
			String scaleSource =
					"__kernel void "+
							"multiply(__global float *a,"+
							"         __global float *total_brightness,"+
							"         __global float *max)"+
							"{"+
							"    int gid = get_global_id(0);"+
							"    a[gid] = a[gid] * (total_brightness[0] / max[0]);"+
							"}";

			cl_program program = clCreateProgramWithSource(cl, 1, new String[]{ scaleSource }, null, errcode);
			checkError(errcode[0]);
			checkError( clBuildProgram(program, 0, null, "-cl-mad-enable", null, null) );
			scaleKernel = clCreateKernel(program, "multiply", errcode);
			checkError(errcode[0]);
			checkError( clSetKernelArg(scaleKernel, 0, Sizeof.cl_mem, Pointer.to(inputBuffer)) );
			checkError( clSetKernelArg(scaleKernel, 1, Sizeof.cl_mem, Pointer.to(brightnessBuffer)) );
			checkError( clSetKernelArg(scaleKernel, 2, Sizeof.cl_mem, Pointer.to(sumBuffer)) );
		}
	}

	
	
	/**
	 * Calls the reduction kernel to calculate the maximum of the estimateBuffer in OpenCL.
	 * @param commandQueue
	 */
	private void reduce( cl_command_queue commandQueue )
	{
		long local_work_size[] = new long[]{maxWorkSize};
		long global_work_size[] = new long[]{maxWorkSize};
		
		checkError( clEnqueueNDRangeKernel(commandQueue, reductionKernel, 1, null, global_work_size, local_work_size, 0, null, null) );
	}
	
	/**
	 * Calls the scale kernel which computes estimateBuffer *= max(estimateBuffer) in OpenCL
	 * should be called after a call to reduce( commandQueue )
	 * @param commandQueue
	 * @param elementCount
	 */
	private void scale( cl_command_queue commandQueue, long elementCount )
	{
		long global_work_size[] = new long[]{elementCount};
		checkError( clEnqueueNDRangeKernel(commandQueue, scaleKernel, 1, null, global_work_size, null, 0, null, null) );
	}
}

