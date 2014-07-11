package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


import static org.jocl.CL.CL_MEM_COPY_HOST_PTR;
import static org.jocl.CL.clBuildProgram;
import static org.jocl.CL.clCreateBuffer;
import static org.jocl.CL.clCreateCommandQueue;
import static org.jocl.CL.clCreateKernel;
import static org.jocl.CL.clCreateProgramWithSource;
import static org.jocl.CL.clEnqueueNDRangeKernel;
import static org.jocl.CL.clFinish;
import static org.jocl.CL.clReleaseCommandQueue;
import static org.jocl.CL.clReleaseKernel;
import static org.jocl.CL.clReleaseMemObject;
import static org.jocl.CL.clSetKernelArg;
import static org.jocl.CL.stringFor_errorCode;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.algorithms.*;

import org.jocl.CL;
import org.jocl.Pointer;
import org.jocl.Sizeof;
import org.jocl.cl_command_queue;
import org.jocl.cl_context;
import org.jocl.cl_device_id;
import org.jocl.cl_kernel;
import org.jocl.cl_mem;
import org.jocl.cl_program;


/**
 * OpenCL Algorithm convolves a kernel with a 2D, 3D or 4D image.
 */
public class OpenCLAlgorithmConvolver {


	public OpenCLAlgorithmConvolver() { }


	/**
	 * @param cl  the current cl context.
	 * @param device  the cl device to run the OpenCL code on.
	 * @param inputBuffer the cl_mem buffer containing the input image data.
	 * @param outputBuffer the cl_mem buffer for the output data.
	 * @param width input image width.
	 * @param height input image height.
	 * @param depth input image depth (0 for 2D, number of slices for 25D).
	 * @param size the size of the image.
	 * @param kKernel the Kernel containing the separable convolution kernels.
	 * @param color when 1, the input image is a MIPAV color image.
	 * @param colorMask the color mask, which determines how the RGB channels of the color image are treated in the convolution.
	 */
	public static synchronized void convolveSep2D( cl_context cl, cl_device_id device, 
			cl_mem inputBuffer, cl_mem outputBuffer, int width, int height, int depth, int size, Kernel kKernel, 
			int color, int[] colorMask )
	{
		int[] errcode = new int[1];

		// Read the program source code and create the program
		String source = OpenCLAlgorithmBase.readKernelFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);

		// Set up 3D Kernel:
		String kernelName = (color == 1) ? "convolveX" : "convolveX_color";
		cl_kernel kernelX = clCreateKernel(program, kernelName, null);

		kernelName = (color == 1) ? "convolveY" : "convolveY_color";
		cl_kernel kernelY = clCreateKernel(program, kernelName, null);

		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem outputX = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * size, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}


		long globalWorkSize[] = new long[]{width,height};    

		int[] kExtents = kKernel.getExtents();
		int[] kOrigins = new int[kExtents.length];
		for ( int i = 0; i < kOrigins.length; i++ )
		{
			kOrigins[i] = (kExtents[i]-1)>>1;
		}
		float[][] derivativeKernel = kKernel.getData();



		cl_mem derivativeX = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[0].length, Pointer.to(derivativeKernel[0]), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem derivativeY = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[1].length, Pointer.to(derivativeKernel[1]), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		// Set up 2D Kernel:		
		int arg = 0;
		clSetKernelArg(kernelX, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_mem, Pointer.to(derivativeX));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_mem, Pointer.to(outputX));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0]}));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelX, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelX, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg = arg;

		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(kernelX, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
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
		errcode[0] = clFinish(commandQueue);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
		}


		arg = 0;
		clSetKernelArg(kernelY, arg++, Sizeof.cl_mem, Pointer.to(outputX));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_mem, Pointer.to(derivativeY));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[1]}));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[1]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelY, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelY, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		sliceArg = arg;

		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(kernelY, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
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
		errcode[0] = clFinish(commandQueue);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
		}

		clReleaseMemObject(derivativeX);
		clReleaseMemObject(derivativeY);
		clReleaseMemObject(outputX);

		clReleaseCommandQueue(commandQueue);

		clReleaseKernel(kernelX);
		clReleaseKernel(kernelY);
	}
	
	/**
	 * @param cl  the current cl context.
	 * @param device  the cl device to run the OpenCL code on.
	 * @param inputBuffer the cl_mem buffer containing the input image data.
	 * @param outputBuffer the cl_mem buffer for the output data.
	 * @param width input image width.
	 * @param height input image height.
	 * @param depth input image depth.
	 * @param size the size of the image.
	 * @param kKernel the Kernel containing the separable convolution kernels.
	 * @param color when 1, the input image is a MIPAV color image.
	 * @param colorMask the color mask, which determines how the RGB channels of the color image are treated in the convolution.
	 * @param clipZ when true the z-component of the convolution kernel is applied only where it overlaps entirely with the input image,
	 * otherwise the z-component of the convolution kernel is truncated and applied to all image slices.
	 */
	public static synchronized void convolveSep3D( cl_context cl, cl_device_id device, 
			cl_mem inputBuffer, cl_mem outputBuffer, int width, int height, int depth, int size, Kernel kKernel, 
			int color, int[] colorMask, boolean clipZ )
	{
		int[] errcode = new int[1];

		// Read the program source code and create the program
		String source = OpenCLAlgorithmBase.readKernelFile("Convolve.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
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

		cl_mem outputXY = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * size, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}


		long globalWorkSize[] = new long[]{width,height};    

		int[] kExtents = kKernel.getExtents();
		int[] kOrigins = new int[kExtents.length];
		for ( int i = 0; i < kOrigins.length; i++ )
		{
			kOrigins[i] = (kExtents[i]-1)>>1;
		}
		float[][] derivativeKernel = kKernel.getData();



		cl_mem derivativeX = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[0].length, Pointer.to(derivativeKernel[0]), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem derivativeY = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[1].length, Pointer.to(derivativeKernel[1]), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		cl_mem derivativeZ = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * derivativeKernel[2].length, Pointer.to(derivativeKernel[2]), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		// Set up 2D Kernel:		
		int arg = 0;
		clSetKernelArg(kernelX, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_mem, Pointer.to(derivativeX));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[0]}));
		clSetKernelArg(kernelX, arg++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[0]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelX, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelX, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg = arg;

		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(kernelX, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
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
		errcode[0] = clFinish(commandQueue);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
		}


		arg = 0;
		clSetKernelArg(kernelY, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_mem, Pointer.to(derivativeY));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_mem, Pointer.to(outputXY));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_int2, Pointer.to(new int[]{width, height}));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[1]}));
		clSetKernelArg(kernelY, arg++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[1]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelY, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		clSetKernelArg(kernelY, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		sliceArg = arg;

		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(kernelY, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
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
		errcode[0] = clFinish(commandQueue);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
		}

		// Set up 2D Kernel:		
		arg = 0;
		clSetKernelArg(kernelZ, arg++, Sizeof.cl_mem, Pointer.to(outputXY));
		clSetKernelArg(kernelZ, arg++, Sizeof.cl_mem, Pointer.to(derivativeZ));
		clSetKernelArg(kernelZ, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernelZ, arg++, Sizeof.cl_int4, Pointer.to(new int[]{width, height, depth, 0}));
		clSetKernelArg(kernelZ, arg++, Sizeof.cl_int, Pointer.to(new int[]{kExtents[2]}));
		clSetKernelArg(kernelZ, arg++, Sizeof.cl_int, Pointer.to(new int[]{kOrigins[2]}));
		if ( color != 1 )
		{
			clSetKernelArg(kernelZ, arg++, Sizeof.cl_int4, Pointer.to(colorMask));
		}
		int clip = clipZ ? 1 : 0;
		clSetKernelArg(kernelZ, arg++, Sizeof.cl_int, Pointer.to(new int[]{clip}));
		clSetKernelArg(kernelZ, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		sliceArg = arg;

		for ( int i = 0; i < depth; i++ )
		{
			errcode[0] = clSetKernelArg(kernelZ, sliceArg, Sizeof.cl_int, Pointer.to(new int[]{i}));
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


		clReleaseMemObject(derivativeX);
		clReleaseMemObject(derivativeY);
		clReleaseMemObject(derivativeZ);
		clReleaseMemObject(outputXY);

		clReleaseCommandQueue(commandQueue);

		clReleaseKernel(kernelX);
		clReleaseKernel(kernelY);
		clReleaseKernel(kernelZ);
	}
}
