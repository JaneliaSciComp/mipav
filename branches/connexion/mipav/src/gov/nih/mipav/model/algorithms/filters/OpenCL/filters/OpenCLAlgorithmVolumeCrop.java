package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


import static org.jocl.CL.*;

import java.io.IOException;

import gov.nih.mipav.model.GaussianKernelFactory;
import gov.nih.mipav.model.Kernel;
import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.JDialogBase;


import javax.media.opengl.GL3;

import WildMagic.LibFoundation.Mathematics.Matrix4f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibRenderers.OpenGLRenderer.TextureID;

import org.jocl.*;


/**
 */
public class OpenCLAlgorithmVolumeCrop extends OpenCLAlgorithmBase
{

	public OpenCLAlgorithmVolumeCrop(final ModelImage srcImg) {

		super( (ModelImage)srcImg.clone(), srcImg, true, CL.CL_DEVICE_TYPE_GPU);
	}

	/**
	 * Prepare this class for destruction.
	 */
	public void finalize() {
		destImage = null;
		srcImage = null;
		super.finalize();
	}


	public void runAlgorithm() {
		cl = null;
		device = null;
		if (srcImage == null) {
			displayError("Source Image is null");
			return;
		}
		calcCroppedShared();
	}

	private Vector3f clip;
	private Vector3f clipI;
	private Vector4f clipEye;
	private Vector4f clipEyeI;
	private Vector4f clipArb;
	private float[][] WVPMatrix;
	private int[] doClip;
	
	public void setClip( Vector3f clip, Vector3f clipI, boolean doClip )
	{
		int[] extents = srcImage.getExtents();
		this.clip = new Vector3f(clip.X * (extents[0]-1), clip.Y * (extents[1]-1), clip.Z * (extents[2]-1));
		this.clipI = new Vector3f(clipI.X * (extents[0]-1), clipI.Y * (extents[1]-1), clipI.Z * (extents[2]-1));
		this.doClip = doClip ? new int[]{1} : new int[]{0};
	}
	
	public void setClipEyeArb( Vector4f clipE, Vector4f clipEI, Vector4f clipA, float[] WVPMatrix )
	{
		int[] extents = srcImage.getExtents();
		this.clipEye = new Vector4f(clipE.X * (extents[0]-1), clipE.Y * (extents[1]-1), clipE.Z * (extents[2]-1), clipE.W * (extents[2]-1));
		this.clipEyeI = new Vector4f(clipEI.X * (extents[0]-1), clipEI.Y * (extents[1]-1), clipEI.Z * (extents[2]-1), clipEI.W * (extents[2]-1));
		this.clipArb = new Vector4f(clipA.X, clipA.Y, clipA.Z, clipA.W);
		this.WVPMatrix = new float[4][4];
		for ( int i = 0; i < WVPMatrix.length; i++ )
		{
			this.WVPMatrix[i/4][i%4] = WVPMatrix[i];
//			System.err.println( WVPMatrix[i] );
		}
//		for ( int i = 0; i < 4; i++ )
//		{
//			for ( int j = 0; j < 4; j++ )
//			{
//				System.err.println( this.WVPMatrix[i][j] );
//			}
//		}
	}
	
	

	private void calcCroppedShared()
	{
		initCL(m_iDeviceType, m_kGL);

		// Test Memory for the GPU:
		int nBuffers = 1;
		int elementCount = width * height * depth * color;	
		long memoryUsed =  nBuffers * elementCount;	
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

		int[] errcode = new int[1];
		cl_mem inputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
				Sizeof.cl_float * input.length, Pointer.to(input), errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		cl_mem outputBuffer = clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * input.length, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		


		String source = readFile("VolumeCrop.cl");
		cl_program program = clCreateProgramWithSource(cl, 1, 
				new String[]{ source }, null, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		clBuildProgram(program, 0, null, "-cl-mad-enable", null, null);
		cl_kernel kernel = clCreateKernel(program, "CropKernel", errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}		

		float min = (float) (srcImage.isColorImage() ? srcImage.getMinR() : srcImage.getMin());
		float minG = (float) (srcImage.isColorImage() ? srcImage.getMinG() : srcImage.getMin());
		float minB = (float) (srcImage.isColorImage() ? srcImage.getMinB() : srcImage.getMin());
		float minA = (float) (srcImage.isColorImage() ? srcImage.getMinA() : srcImage.getMin());
		
		int arg = 0;
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(inputBuffer));
		clSetKernelArg(kernel, arg++, Sizeof.cl_mem, Pointer.to(outputBuffer));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int, Pointer.to(new int[]{width}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int, Pointer.to(new int[]{height}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int, Pointer.to(new int[]{depth}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int, Pointer.to(new int[]{color}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int, Pointer.to(new int[]{elementCount}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_float4, Pointer.to(new float[]{ min, minG, minB, minA }));
		clSetKernelArg(kernel, arg++, Sizeof.cl_int, Pointer.to(doClip));
		clSetKernelArg(kernel, arg++, Sizeof.cl_float4, Pointer.to(new float[]{clip.X, clip.Y, clip.Z, 0}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_float4, Pointer.to(new float[]{clipI.X, clipI.Y, clipI.Z, 0}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_float4, Pointer.to(new float[]{clipEye.X, clipEye.Y, clipEye.Z, clipEye.W}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_float4, Pointer.to(new float[]{clipEyeI.X, clipEyeI.Y, clipEyeI.Z, clipEyeI.W}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_float4, Pointer.to(new float[]{clipArb.X, clipArb.Y, clipArb.Z, clipArb.W}));
		clSetKernelArg(kernel, arg++, Sizeof.cl_float4, Pointer.to(WVPMatrix[0]));
		clSetKernelArg(kernel, arg++, Sizeof.cl_float4, Pointer.to(WVPMatrix[1]));
		clSetKernelArg(kernel, arg++, Sizeof.cl_float4, Pointer.to(WVPMatrix[2]));
		clSetKernelArg(kernel, arg, Sizeof.cl_int, Pointer.to(new int[]{0}));
		int sliceArg = arg;		

		// create command queue:
		cl_command_queue commandQueue = 
				clCreateCommandQueue(cl, device, 0, errcode);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}

		long globalWorkSize[] = new long[]{width,height};        



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

		float[] output = new float[ elementCount ];
		errcode[0] = clEnqueueReadBuffer(commandQueue, outputBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
		if ( errcode[0] != CL.CL_SUCCESS )
		{
			System.err.println( stringFor_errorCode(errcode[0]) );
		}
		saveImage(output, 0, true );

		clReleaseMemObject(inputBuffer);
		clReleaseMemObject(outputBuffer);
	}
}
