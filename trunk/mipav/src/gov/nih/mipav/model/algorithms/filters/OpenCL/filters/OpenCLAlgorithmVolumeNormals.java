package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


import static org.jocl.CL.*;
import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.IOException;

import javax.media.opengl.GL3;
import javax.media.opengl.GL3bc;

import WildMagic.LibRenderers.OpenGLRenderer.TextureID;

import org.jocl.*;


/**
 * OpenCL Algorithm implementation calculate the volume normals of a 3D image.
 * Two methods for the normal calculation are provided. One reads the ModelImage data and creates OpenCL Buffers for the data.
 * The other method reads the volume data from a shared OpenGL Texture
 */
public class OpenCLAlgorithmVolumeNormals extends OpenCLAlgorithmBase {

	/** OpenGL Texture ID containing the volume data. */
	private TextureID m_kTextureID = null;

	/**
	 * Create an OpenCL Algorithm for calculating the volume normals. Does not use OpenGL Shared texture.
	 * @param srcImg source image
	 * @param type OpenCL Platform type, may be CL.CL_DEVICE_TYPE_GPU to specify the GPU or CL.CL_DEVICE_TYPE_CPU to specify the CPU.
	 */
	public OpenCLAlgorithmVolumeNormals(final ModelImage srcImg, long type) {

		super( new ModelImage( ModelStorageBase.ARGB_FLOAT, srcImg.getExtents(), 
				JDialogBase.makeImageName(srcImg.getImageName(), "_Normals") ), srcImg, true, type);
	}

	/**
	 * Create an OpenCL Algorithm for calculating the volume normals. Does not use OpenGL Shared texture.
	 * @param srcImg source image
	 * @param type OpenCL Platform type, may be CL.CL_DEVICE_TYPE_GPU to specify the GPU or CL.CL_DEVICE_TYPE_CPU to specify the CPU.
	 */
	public OpenCLAlgorithmVolumeNormals(final ModelImage srcImg, ModelImage destImg, long type) {

		super( destImg, srcImg, true, type);
	}

	/**
	 * Create an OpenCL Algorithm for calculating the volume normals. Uses OpenGL Shared texture.
	 * @param srcImg source image
	 * @param gl OpenGL context, must be current
	 * @param textureID OpenCL texture ID
	 */
	public OpenCLAlgorithmVolumeNormals(final ModelImage srcImg, GL3bc gl, TextureID textureID) {

		super( new ModelImage( ModelStorageBase.ARGB_FLOAT, srcImg.getExtents(), 
				JDialogBase.makeImageName(srcImg.getImageName(), "_Normals") ), srcImg, true, CL.CL_DEVICE_TYPE_GPU);
		m_kGL = gl;
		m_kTextureID = textureID;
	}

	/**
	 * Prepare this class for destruction.
	 */
	public void finalize() {
		destImage = null;
		srcImage = null;
		super.finalize();
	}


	/**
	 * Starts the program.
	 */
	@Override
	public void runAlgorithm() {
		cl = null;
		device = null;
		if (srcImage == null) {
			displayError("Source Image is null");
			return;
		}
		if ( m_kGL != null )
		{
			calcNormalsShared();
		}
		else
		{
			calcNormals();
		}
	}


	/**
	 * Calculates the volume normals from the input source ModelImage.
	 * Creates the kernels, command queue, and OpenCL buffers. Execute the kernels on the command queue and
	 * saves the result into a new ModelImage.
	 */
	private void calcNormals()
	{
		int nBuffers = 2; // 2 buffers for the computation.
		int elementCount = width * height * depth * color;
		initCL(m_iDeviceType, null);
		long maxAllocSize = getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
		if ( elementCount > (maxAllocSize / (nBuffers * Sizeof.cl_float)) )
		{
			calcNormalsSlices();
			return;
		}

		float[] input = new float[ elementCount ];
		try {
			srcImage.exportData( 0, input.length, input );
		} catch (IOException e) {
			e.printStackTrace();
		}

		float[] output = new float[ width * height * depth * 4 ];

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

		String source = readKernelFile("VolumeNormals.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);
		String kernelName = ( color > 1 ) ? "NormalKernel25DColor" : "NormalKernel25D";
		cl_kernel kernel = clCreateKernel(program, kernelName, null);

		clSetKernelArg(kernel, 0, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel, 1, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel, 2, Sizeof.cl_int, Pointer.to(new int[]{elementCount}));
		clSetKernelArg(kernel, 3, Sizeof.cl_int, Pointer.to(new int[]{width}));
		clSetKernelArg(kernel, 4, Sizeof.cl_int, Pointer.to(new int[]{height}));
		clSetKernelArg(kernel, 5, Sizeof.cl_int, Pointer.to(new int[]{0}));

		long globalWorkSize[] = new long[2];
		globalWorkSize[0] = width;
		globalWorkSize[1] = height;


		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, null);
		for ( int i = 0; i < depth; i++ )
		{
			clSetKernelArg(kernel, 5, Sizeof.cl_int, Pointer.to(new int[]{i}));
			clEnqueueNDRangeKernel(commandQueue, kernel, 2, null, 
					globalWorkSize, null, 0, null, null);

		}
		clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		saveImage(output, 0, true );

		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(outputBuffer);

		//new ViewJFrameImage(destImage);		
	}


	/**
	 * Calculates the volume normals from the input source ModelImage.
	 * Creates the kernels, command queue, and OpenCL buffers. Execute the kernels on the command queue and
	 * saves the result into a new ModelImage.
	 */
	private void calcNormalsSlices()
	{
		int elementCount = width * height * 3 * color;

		float[] output = new float[ width * height * 4 ];

		int[] errcode = new int[1];

		cl_mem outputBuffer = clCreateBuffer(cl, CL.CL_MEM_WRITE_ONLY,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		String source = readKernelFile("VolumeNormals.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, null);
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);
		String kernelName = ( color > 1 ) ? "NormalKernel25DSlicesColor" : "NormalKernel25DSlices";
		cl_kernel kernel = clCreateKernel(program, kernelName, null);


		long globalWorkSize[] = new long[]{ width, height };

		// create command queue:
		cl_command_queue commandQueue = clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}


		float[] input = new float[ elementCount ];
		for ( int i = 0; i < depth; i++ )
		{
			try {
				if ( i < (depth - 2) )
				{
					srcImage.exportData( i * (width * height * color), input.length, input );
				}
				else if ( i < (depth - 1) )
				{
					srcImage.exportData( (i-1) * (width * height * color), input.length, input );
				}
				else
				{
					srcImage.exportData( (i-2) * (width * height * color), input.length, input );
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
			cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
					Sizeof.cl_float * input.length, Pointer.to(input), errcode);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( stringFor_errorCode(errcode[0]) );
			}

			clSetKernelArg(kernel, 0, Sizeof.cl_mem, Pointer.to(inputBuffer));
			clSetKernelArg(kernel, 1, Sizeof.cl_mem, Pointer.to(outputBuffer));
			clSetKernelArg(kernel, 2, Sizeof.cl_int, Pointer.to(new int[]{elementCount}));
			clSetKernelArg(kernel, 3, Sizeof.cl_int, Pointer.to(new int[]{width}));
			clSetKernelArg(kernel, 4, Sizeof.cl_int, Pointer.to(new int[]{height}));
			
			errcode[0] = clEnqueueNDRangeKernel(commandQueue, kernel, 2, null, 
					globalWorkSize, null, 0, null, null);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( "clEnqueueNDRangeKernel " + stringFor_errorCode(errcode[0]) );
			}

			errcode[0] = clFinish(commandQueue);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( "clFinish " + stringFor_errorCode(errcode[0]) );
			}

			errcode[0] = clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( "clEnqueueReadBuffer " + stringFor_errorCode(errcode[0]) );
			}
			saveImage(output, i, (i == depth-1) );

			clReleaseMemObject(inputBuffer);
		}
		clReleaseMemObject(outputBuffer);
	}


	/**
	 * Calculates the volume normals shared OpenGL texture.
	 * Creates the kernels, command queue, and OpenCL buffers. Execute the kernels on the command queue and
	 * saves the result into a new ModelImage.
	 */
	private void calcNormalsShared()
	{
		cl_mem inputBuffer;
		cl_mem normalsBuffer;

		int elementCount = width * height * depth;

		initCL(m_iDeviceType, m_kGL);
		long maxMemAllocSize = getLong(device, CL_DEVICE_MAX_MEM_ALLOC_SIZE);
		if ( (Sizeof.cl_float * elementCount) > maxMemAllocSize )
		{
			System.err.println( "Image too big..." );
			return;
		}
		int[] errcode = new int[1];

		m_kGL.glBindTexture(GL3.GL_TEXTURE_3D, m_kTextureID.ID);
		inputBuffer = clCreateFromGLTexture3D(cl, CL_MEM_READ_ONLY, GL3.GL_TEXTURE_3D, 0, m_kTextureID.ID, errcode );
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		float[] output = new float[ width * height * depth * 4 ];

		normalsBuffer = clCreateBuffer(cl, CL.CL_MEM_WRITE_ONLY,
				Sizeof.cl_float * output.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}


		long[] tex_globalWorkSize = new long[]{ width, height, depth };

		String source = readKernelFile("VolumeNormalsShared.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);
		cl_kernel kernel = clCreateKernel(program, "NormalKernelShared", errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}		

		clSetKernelArg(kernel, 0, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel, 1, Sizeof.cl_mem, Pointer.to(normalsBuffer));
		clSetKernelArg(kernel, 2, Sizeof.cl_int, Pointer.to(new int[]{width}));
		clSetKernelArg(kernel, 3, Sizeof.cl_int, Pointer.to(new int[]{height}));
		clSetKernelArg(kernel, 4, Sizeof.cl_int, Pointer.to(new int[]{depth}));
		clSetKernelArg(kernel, 5, Sizeof.cl_int, Pointer.to(new int[]{color}));
		clSetKernelArg(kernel, 6, Sizeof.cl_int, Pointer.to(new int[]{elementCount}));

		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, null);

		clEnqueueAcquireGLObjects(commandQueue, 1, new cl_mem[]{ inputBuffer }, 0, null, null );
		clEnqueueNDRangeKernel(commandQueue, kernel, 3, null, 
				tex_globalWorkSize, null, 0, null, null);
		clEnqueueReleaseGLObjects(commandQueue, 1, new cl_mem[]{ inputBuffer }, 0, null, null );
		clEnqueueReadBuffer(commandQueue, normalsBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);

		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(normalsBuffer);

		saveImage(output, 0, true );
		//new ViewJFrameImage(destImage);
	}
}
