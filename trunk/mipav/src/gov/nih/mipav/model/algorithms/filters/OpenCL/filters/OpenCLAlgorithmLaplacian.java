package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


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
import gov.nih.mipav.view.ViewJFrameImage;

import java.io.*;

import java.util.*;

import org.jocl.CL;
import org.jocl.Pointer;
import org.jocl.Sizeof;
import org.jocl.cl_command_queue;
import org.jocl.cl_kernel;
import org.jocl.cl_mem;
import org.jocl.cl_program;


/**
 * OpenCL Algorithm computes the Laplacian of a 2D, 3D or 4D image.
 */
public class OpenCLAlgorithmLaplacian extends OpenCLAlgorithmBase {


	/** An amplification factor greater than 1.0 causes this filter to act like a highpass filter. */
	private float amplificationFactor = 1.0f;

	/** Storage location of the second derivative of the Gaussian in the X direction for the 2D kernel. */
	private float[] GxxData2D;

	/** Storage location of the second derivative of the Gaussian in the Y direction for the 2D kernel. */
	private float[] GyyData2D;

	/** Storage location of the second derivative of the Gaussian in the X direction for the 3D kernel. */
	private float[] GxxData;

	/** Storage location of the second derivative of the Gaussian in the Y direction for the 3D kernel. */
	private float[] GyyData;

	/** Storage location of the second derivative of the Gaussian in the Z direction for the 3D kernel. */
	private float[] GzzData;

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
	 * Constructs a Laplacian algorithm object.
	 *
	 * @param  srcImg     source image model
	 * @param  sigmas     Gaussian's standard deviations in the each dimension
	 * @param  maskFlag   Flag that indicates that the Laplacian will be calculated for the whole image if equal to true
	 * @param  img25D     Flag, if true, indicates that each slice of the 3D volume should be processed independently.
	 *                    2D images disregard this flag.
	 * @param  ampFactor  An amplification factor greater than 1.0 causes this filter to act like a highpass filter.
	 */
	public OpenCLAlgorithmLaplacian(ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean separable, boolean img25D, float ampFactor) {
		this(null, srcImg, sigmas, maskFlag, separable, img25D, ampFactor);
	}


	/**
	 * Constructs a Laplacian algorithm object.
	 *
	 * @param  destImg    image model where result image is to stored
	 * @param  srcImg     source image model
	 * @param  sigmas     Gaussian's standard deviations in the each dimension
	 * @param  maskFlag   Flag that indicates that the Laplacian will be calculated for the whole image if equal to true
	 * @param  img25D     Flag, if true, indicates that each slice of the 3D volume should be processed independently.
	 *                    2D images disregard this flag.
	 * @param  ampFactor  An amplification factor greater than 1.0 causes this filter to act like a highpass filter.
	 */
	public OpenCLAlgorithmLaplacian(ModelImage destImg, ModelImage srcImg, float[] sigmas, boolean maskFlag, boolean separable, boolean img25D,
			float ampFactor) {
		super(destImg, srcImg, maskFlag, CL.CL_DEVICE_TYPE_GPU);
		this.sigmas = sigmas;
		this.entireImage = maskFlag;
		this.separable = separable;
		this.image25D = img25D;
		this.amplificationFactor = ampFactor;
	}


	/**
	 * Generates a zero crossing mask for a 2D function sets a Bitset object to 1 is a zero crossing is detected.
	 *
	 * @param  xDim       X dimension length
	 * @param  yDim       Y dimension length
	 * @param  buffer     array of data in which to find level crossing
	 * @param  edgeImage  edge map of level crossings
	 * @param  level      level of crossings to find (e.g. zero crossing of the Laplacian)
	 */
	public static void genLevelMask(int xDim, int yDim, float[] buffer, BitSet edgeImage, float level) {

		float x0, x1, x2, x3;
		int i, j, index;
		int indexY;

		int xxDim = xDim - 1;
		int yyDim = yDim - 1;

		edgeImage = new BitSet(xDim * yDim);

		x0 = buffer[0];
		x2 = buffer[xDim];

		for (j = 0; j < yyDim; j++) {
			indexY = j * xDim;

			for (i = 0; i < xxDim; i++) {
				index = indexY + i;
				x1 = buffer[index + 1];
				x3 = buffer[index + 1 + xDim];

				if ((x0 >= level) && (x1 >= level) && (x2 >= level) && (x3 >= level)) {
					edgeImage.clear(index);
				} else if ((x0 <= level) && (x1 <= level) && (x2 <= level) && (x3 <= level)) {
					edgeImage.clear(index);
				} else {
					edgeImage.set(index);
				}

				x0 = x1;
				x2 = x3;
			}
		}
	}

	/**
	 * Prepares this class for destruction.
	 */
	public void finalize() {
		sigmas = null;
		kExtents_2D = null;
		kExtents_3D = null;
		kOrigins_2D = null;
		kOrigins_3D = null;
		GxxData2D = null;
		GyyData2D = null;
		GxxData = null;
		GyyData = null;
		GzzData = null;
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
			}
		}
		if (threadStopped) {
			finalize();
			return;
		}

		if (srcImage.getNDims() == 2) {
			if ( !separable )
			{
				laplacian25D();
			}
			else
			{
				laplacianSep25D();
			}
		} else if ((srcImage.getNDims() == 3) && (image25D == false)) {
			if ( !separable )
			{
				laplacian3D(0);
			}
			else
			{
				laplacianSep3D(0);
			}
		} else if ((srcImage.getNDims() == 3) && (image25D == true)) {
			if ( !separable )
			{
				laplacian25D();
			}
			else
			{
				laplacianSep25D();
			}
		} else if (srcImage.getNDims() == 4) {
			laplacian4D();
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
		derivOrder[0] = 2;
		derivOrder[1] = 0;

		xkDim = Math.round(11 * sigmas[0]);

		if ((xkDim % 2) == 0) {
			xkDim++;
		}

		if (xkDim < 3) {
			xkDim = 3;
		}

		kExtents_2D[0] = xkDim;

		ykDim = Math.round(11 * sigmas[1]);

		if ((ykDim % 2) == 0) {
			ykDim++;
		}

		if (ykDim < 3) {
			ykDim = 3;
		}

		kExtents_2D[1] = ykDim;

		GxxData2D = new float[xkDim * ykDim];

		GenerateGaussian Gxx = new GenerateGaussian(GxxData2D, kExtents_2D, sigmas, derivOrder);
		Gxx.calc(false);

		derivOrder[0] = 0;
		derivOrder[1] = 2;
		GyyData2D = new float[xkDim * ykDim];

		GenerateGaussian Gyy = new GenerateGaussian(GyyData2D, kExtents_2D, sigmas, derivOrder);
		Gyy.calc(false);

		// Do not sum GxxData and GyyData here
		// That yields a different answer than summing after the convolutions and
		// summing after the convolutions is the correct procedure.
		// The LOG filter is decomposed into the sum of two separable filters
		// Log(x,y) = -G"(x)G(y) -G(x)G"(y) 
		// where G and G" are the 1D Gaussian and the second derivative of the 1D Gaussian.

		for (int i = 0; i < GxxData2D.length; i++) {
			GxxData2D[i] *= amplificationFactor;
			GyyData2D[i] *= amplificationFactor;
		}


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
		derivOrder[0] = 2;
		derivOrder[1] = 0;
		derivOrder[2] = 0;

		xkDim = Math.round(11 * sigmas[0]);

		if ((xkDim % 2) == 0) {
			xkDim++;
		}

		if (xkDim < 3) {
			xkDim = 3;
		}

		kExtents_3D[0] = xkDim;

		ykDim = Math.round(11 * sigmas[1]);

		if ((ykDim % 2) == 0) {
			ykDim++;
		}

		if (ykDim < 3) {
			ykDim = 3;
		}

		kExtents_3D[1] = ykDim;

		float scaleFactor = sigmas[2];
		sigmas[2] = sigmas[1];
		zkDim = Math.round(11 * sigmas[2]);

		if ((zkDim % 2) == 0) {
			zkDim++;
		}

		if (zkDim < 3) {
			zkDim = 3;
		}

		kExtents_3D[2] = zkDim;


		GxxData = new float[xkDim * ykDim * zkDim];

		GenerateGaussian Gxx = new GenerateGaussian(GxxData, kExtents_3D, sigmas, derivOrder);
		Gxx.calc(false);

		derivOrder[0] = 0;
		derivOrder[1] = 2;
		derivOrder[2] = 0;
		GyyData = new float[xkDim * ykDim * zkDim];

		GenerateGaussian Gyy = new GenerateGaussian(GyyData, kExtents_3D, sigmas, derivOrder);
		Gyy.calc(false);

		derivOrder[0] = 0;
		derivOrder[1] = 0;
		derivOrder[2] = 2;
		GzzData = new float[xkDim * ykDim * zkDim];

		GenerateGaussian Gzz = new GenerateGaussian(GzzData, kExtents_3D, sigmas, derivOrder);
		Gzz.calc(false);

		// Do not sum GxxData, GyyData. and GzzData here
		// That yields a different answer than summing after the convolutions and
		// summing after the convolutions is the correct procedure.
		// The LOG filter is decomposed into the sum of three separable filters
		// Log(x,y,z) = -G"(x)G(y)G(z) - G(x)G"(y)G(z) - G(x)G(y)G"(z) 
		// where G and G" are the 1D Gaussian and the second derivative of the 1D Gaussian.

		for (int i = 0; i < GxxData.length; i++) {
			GxxData[i] *= amplificationFactor;
			GyyData[i] *= amplificationFactor;
			GzzData[i] *= (amplificationFactor * scaleFactor);
		}

		kOrigins_3D = new int[3];
		kOrigins_3D[0] = (kExtents_3D[0]-1)>>1;
		kOrigins_3D[1] = (kExtents_3D[1]-1)>>1;
		kOrigins_3D[2] = (kExtents_3D[2]-1)>>1;
	}

	/**
	 * 2D Implementation: Create the OpenCL Buffers, Kernel, and CommandQueue. 
	 * Run the Kernel and save the results into a new image.
	 */
	private void laplacian25D()
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
				Sizeof.cl_float * GxxData2D.length, Pointer.to(GxxData2D), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveY = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GyyData2D.length, Pointer.to(GyyData2D), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		int[] imageSize = new int[]{width, height};
		int[] maskSize = kExtents_2D;
		int[] maskOrigins = kOrigins_2D;

		// Read the program source code and create the program
		String source = readFile("Laplacian.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);
		String kernelName = "Laplacian25D";
		cl_kernel kernel = clCreateKernel(program, kernelName, null);

		int arg = 0;
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(convolveX));
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(convolveY));
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int2, Pointer.to(imageSize));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int2, Pointer.to(maskSize));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int2, Pointer.to(maskOrigins));
		clSetKernelArg(kernel, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg = arg;

		long globalWorkSize[] = new long[]{width,height};        

		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, null);
		for ( int i = 0; i < depth; i++ )
		{
			clSetKernelArg(kernel, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
			clEnqueueNDRangeKernel(commandQueue, kernel, 2, null, globalWorkSize, null, 0, null, null);
		}
		clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		saveImage(output, 0, true );

		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(outputBuffer);
	}

	private void laplacianSep25D()
	{
		initCL(m_iDeviceType, null);

		int elementCount = width * height * depth * color;		

		long maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
		if ( (Sizeof.cl_float * elementCount) > maxAllocSize )
		{
			laplacianSep25DSlices();
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

		// Convolve Seperable X:
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.X_DERIVATIVE_KERNEL);
		gkf.setDerivativeOrder(2);
		gkf.setExtentScale(11);
		Kernel gaussianKernel = gkf.createXDerivativeKernel();
		if ( amplificationFactor != 1.0 )
		{
			float[][] data = gaussianKernel.getData();
			for ( int i = 0; i < data.length; i++ )
			{
				for ( int j = 0; j < data[i].length; j++ )
				{
					data[i][j] *= amplificationFactor;
				}
			}
		}
		OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
				inputBuffer, outputDerivX, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask );

		// Convolve Seperable Y:
		gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.Y_DERIVATIVE_KERNEL);
		gkf.setDerivativeOrder(2);
		gkf.setExtentScale(11);
		gaussianKernel = gkf.createYDerivativeKernel();
		if ( amplificationFactor != 1.0 )
		{
			float[][] data = gaussianKernel.getData();
			for ( int i = 0; i < data.length; i++ )
			{
				for ( int j = 0; j < data[i].length; j++ )
				{
					data[i][j] *= amplificationFactor;
				}
			}
		}
		OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
				inputBuffer, outputDerivY, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask );

		// Read the program source code and create the program
		String source = readFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 3D Kernel:
		String kernelName = "negSum25D";
		cl_kernel negSum = clCreateKernel(program, kernelName, null);
		long globalWorkSize[] = new long[]{width,height};        

		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);

		// Calculate negative sum:
		int arg = 0;
		clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(outputDerivX));
		clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(outputDerivY));
		clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(negSum, arg++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		if ( color != 1 )
		{
			clSetKernelArg(negSum, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(negSum, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg = arg;

		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(negSum, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
			}
			errcode[0] = clEnqueueNDRangeKernel(commandQueue, negSum, 2, null, globalWorkSize, null, 0, null, null);
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

	private void laplacianSep25DSlices()
	{	

		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.X_DERIVATIVE_KERNEL);
		gkf.setDerivativeOrder(2);
		gkf.setExtentScale(11);
		Kernel gaussianKernelX = gkf.createXDerivativeKernel();
		if ( amplificationFactor != 1.0 )
		{
			float[][] data = gaussianKernelX.getData();
			for ( int i = 0; i < data.length; i++ )
			{
				for ( int j = 0; j < data[i].length; j++ )
				{
					data[i][j] *= amplificationFactor;
				}
			}
		}


		gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.Y_DERIVATIVE_KERNEL);
		gkf.setDerivativeOrder(2);
		gkf.setExtentScale(11);
		Kernel gaussianKernelY = gkf.createYDerivativeKernel();
		if ( amplificationFactor != 1.0 )
		{
			float[][] data = gaussianKernelY.getData();
			for ( int i = 0; i < data.length; i++ )
			{
				for ( int j = 0; j < data[i].length; j++ )
				{
					data[i][j] *= amplificationFactor;
				}
			}
		}
		int[] errcode = new int[1];

		// Read the program source code and create the program
		String source = readFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 3D Kernel:
		String kernelName = "negSum25D";
		cl_kernel negSum = clCreateKernel(program, kernelName, null);
		long globalWorkSize[] = new long[]{width,height};        

		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}


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


			// Convolve Seperable X:
			OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
					inputBuffer, outputDerivX, width, height, 1, elementCount, gaussianKernelX, 
					color, colorMask );

			// Convolve Seperable Y:
			OpenCLAlgorithmConvolver.convolveSep2D( cl, device, 
					inputBuffer, outputDerivY, width, height, 1, elementCount, gaussianKernelY, 
					color, colorMask );

			// Calculate negative sum:
			int arg = 0;
			clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(outputDerivX));
			clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(outputDerivY));
			clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
			clSetKernelArg(negSum, arg++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, 1, 0}));
			if ( color != 1 )
			{
				clSetKernelArg(negSum, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
			}
			clSetKernelArg(negSum, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));

			errcode[0] = clEnqueueNDRangeKernel(commandQueue, negSum, 2, null, globalWorkSize, null, 0, null, null);
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
			saveImage(output, i, (i == depth-1));

			clReleaseMemObject(inputBuffer);
		}
		clReleaseMemObject(outputDerivX);
		clReleaseMemObject(outputDerivY);
	}



	/**
	 * 3D Implementation: Create the OpenCL Buffers, Kernel, and CommandQueue. 
	 * Run the Kernel and save the results into a new image.
	 */
	private void laplacian3D( int time )
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
				Sizeof.cl_float * GxxData2D.length, Pointer.to(GxxData2D), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveY_2D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GyyData2D.length, Pointer.to(GyyData2D), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveX_3D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GxxData.length, Pointer.to(GxxData), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveY_3D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GyyData.length, Pointer.to(GyyData), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem convolveZ_3D = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * GzzData.length, Pointer.to(GzzData), errcode);
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
		String source = readFile("Laplacian.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 2D Kernel:
		String kernelName = "Laplacian25D";
		cl_kernel kernel_2D = clCreateKernel(program, kernelName, null);

		int arg = 0;
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(convolveX_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(convolveY_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int2, Pointer.to(imageSize_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int2, Pointer.to(maskSize_2D));
		clSetKernelArg(kernel_2D, arg++, Sizeof.cl_int2, Pointer.to(maskOrigins_2D));
		clSetKernelArg(kernel_2D, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg_2D = arg;



		// Set up 3D Kernel:
			kernelName = "Laplacian3D";
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
			clSetKernelArg(kernel_3D, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
			int sliceArg_3D = arg;

			long globalWorkSize[] = new long[]{width,height};        


			// create command queue:
			cl_command_queue commandQueue = 
					clCreateCommandQueue(cl, device, 0, null);
			for ( int i = 0; i < depth; i++ )
			{
				if ( (i < maskSize_3D[2]) || (i >= depth - maskSize_3D[2]) )
				{
					clSetKernelArg(kernel_2D, sliceArg_2D, Sizeof.cl_int, Pointer.to(new int[]{i}));
					clEnqueueNDRangeKernel(commandQueue, kernel_2D, 2, null, globalWorkSize, null, 0, null, null);
				}
				else
				{
					clSetKernelArg(kernel_3D, sliceArg_3D, Sizeof.cl_int, Pointer.to(new int[]{i}));
					clEnqueueNDRangeKernel(commandQueue, kernel_3D, 2, null, globalWorkSize, null, 0, null, null);
				}
			}
			clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
			saveImage(output, time, true );

			clReleaseMemObject(inputBuffer);
			clReleaseMemObject(outputBuffer);
	}

	private void laplacianSep3D( int time )
	{
		initCL(m_iDeviceType, null);				

		int nBuffers = 4; // 4 buffers needed for the computation.
		int elementCount = width * height * depth * color;		
		long maxAllocSize = OpenCLAlgorithmBase.getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
		if ( (Sizeof.cl_float * elementCount * nBuffers) > maxAllocSize )
		{
			laplacianSep25DSlices();
			return;
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
		String source = readFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 3D Kernel:
		String kernelName = "negSum";
		cl_kernel negSum = clCreateKernel(program, kernelName, null);


		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		long globalWorkSize[] = new long[]{width,height};        


		float scaleFactor = sigmas[2];
		sigmas[2] = sigmas[1];

		// Convolve Seperable X:
		GaussianKernelFactory gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.X_DERIVATIVE_KERNEL);
		gkf.setDerivativeOrder(2);
		gkf.setExtentScale(11);
		Kernel gaussianKernel = gkf.createXDerivativeKernel();
		if ( amplificationFactor != 1.0 || scaleFactor != 1.0 )
		{
			float[][] data = gaussianKernel.getData();
			for ( int i = 0; i < data.length; i++ )
			{
				for ( int j = 0; j < data[i].length; j++ )
				{
					data[i][j] *= amplificationFactor;
					if ( i == 2 )
					{
						//data[i][j] *= scaleFactor;
					}
				}
			}
		}
		OpenCLAlgorithmConvolver.convolveSep3D( cl, device, 
				inputBuffer, outputDerivX, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask, false );

		// Convolve Seperable Y:
		gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.Y_DERIVATIVE_KERNEL);
		gkf.setDerivativeOrder(2);
		gkf.setExtentScale(11);
		gaussianKernel = gkf.createYDerivativeKernel();
		if ( amplificationFactor != 1.0 || scaleFactor != 1.0 )
		{
			float[][] data = gaussianKernel.getData();
			for ( int i = 0; i < data.length; i++ )
			{
				for ( int j = 0; j < data[i].length; j++ )
				{
					data[i][j] *= amplificationFactor;
					if ( i == 2 )
					{
						//data[i][j] *= scaleFactor;
					}
				}
			}
		}
		OpenCLAlgorithmConvolver.convolveSep3D( cl, device, 
				inputBuffer, outputDerivY, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask, false );

		// Convolve Seperable Z:
		gkf = GaussianKernelFactory.getInstance(sigmas);
		gkf.setKernelType(GaussianKernelFactory.Z_DERIVATIVE_KERNEL);
		gkf.setDerivativeOrder(2);
		gkf.setExtentScale(11);
		gaussianKernel = gkf.createZDerivativeKernel();
		if ( amplificationFactor != 1.0 || scaleFactor != 1.0 )
		{
			float[][] data = gaussianKernel.getData();
			for ( int i = 0; i < data.length; i++ )
			{
				for ( int j = 0; j < data[i].length; j++ )
				{
					data[i][j] *= amplificationFactor;
					if ( i == 2 )
					{
						data[i][j] *= scaleFactor;
					}
				}
			}
		}

		OpenCLAlgorithmConvolver.convolveSep3D( cl, device, 
				inputBuffer, outputDerivZ, width, height, depth, elementCount, gaussianKernel, 
				color, colorMask, false );     		

		// Calculate magnitude:
		int arg = 0;
		clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(outputDerivX));
		clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(outputDerivY));
		clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(outputDerivZ));
		clSetKernelArg(negSum, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(negSum, arg++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(negSum, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg = arg;

		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(negSum, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( i + " " + stringFor_errorCode(errcode[0]) );
			}
			errcode[0] = clEnqueueNDRangeKernel(commandQueue, negSum, 2, null, globalWorkSize, null, 0, null, null);
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
	 * Calls laplacian3D for each volume in the time series image.
	 */
	public void laplacian4D( )
	{
		for ( int i = 0; i < time; i++ )
		{
			if ( !separable )
			{
				laplacian3D(i);
			}
			else
			{
				laplacianSep3D(0);
			}
		}
	}
}
