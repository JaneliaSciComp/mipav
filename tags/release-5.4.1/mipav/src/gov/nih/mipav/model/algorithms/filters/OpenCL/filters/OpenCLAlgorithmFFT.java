package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


import static org.jocl.CL.CL_MEM_COPY_HOST_PTR;
import static org.jocl.CL.CL_TRUE;
import static org.jocl.CL.clCreateCommandQueue;
import static org.jocl.CL.clEnqueueReadBuffer;
import static org.jocl.CL.clReleaseMemObject;
import static org.jocl.CL.stringFor_errorCode;
import gov.nih.mipav.model.algorithms.OpenCLAlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.CLFFTPlan.InvalidContextException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewJFrameImage;

import org.jocl.*;


public class OpenCLAlgorithmFFT extends OpenCLAlgorithmBase {
	// ~ Static fields/initializers
	// -------------------------------------------------------------------------------------

	/** Inverse FFT */
	public static final int INVERSE = -1;

	/** Forward FFT */
	public static final int FORWARD = 1;

	/**
	 * Constants for xy plane, yz plane and zx plane.
	 */
	public static final int SLICE_XY = 0;

	public static final int SLICE_YZ = 1;

	public static final int SLICE_ZX = 2;

	/** Imaginary data. */
	private float[] imagData;

	/** If true process each slice one at a time in 3D image. */
	private boolean image25D;

	/** If true display log10 of 1 + magnitude. */
	private final boolean logMagDisplay;

	/** Real data. */
	private float[] realData;

	/** Transform direction. */
	private final int transformDir;

	/** If true allow unequal FFT dimensions. */
	private boolean unequalDim;
	private boolean complexInverse;

	long startTime;

	public OpenCLAlgorithmFFT(final ModelImage srcImg, final int transformDir, final boolean logMagDisplay,
			final boolean unequalDim, final boolean image25D, final boolean complexInverse) {
		this(null, srcImg, transformDir, logMagDisplay, unequalDim, image25D, complexInverse);
	}

	public OpenCLAlgorithmFFT(final ModelImage destImg, final ModelImage srcImg, final int transformDir,
			final boolean logMagDisplay, final boolean unequalDim, final boolean image25D, final boolean complexInverse) {

		super(destImg, srcImg, true, CL.CL_DEVICE_TYPE_GPU);
		this.transformDir = transformDir;
		this.logMagDisplay = logMagDisplay;

		if (transformDir == OpenCLAlgorithmFFT.FORWARD) {
			this.unequalDim = unequalDim;
			if (destImage == null) {
				srcImage.setUnequalDim(unequalDim);
				srcImage.setImage25D(image25D);
			} else {
				destImage.setUnequalDim(unequalDim);
				destImage.setImage25D(image25D);
			}
			this.image25D = image25D;
		} else if (transformDir == OpenCLAlgorithmFFT.INVERSE) {
			this.unequalDim = srcImage.getUnequalDim();
			this.image25D = srcImage.getImage25D();
			this.complexInverse = complexInverse;
		}


		width  = srcImg.getExtents().length > 0 ? srcImg.getExtents()[0] : 1;
		height = srcImg.getExtents().length > 1 ? srcImg.getExtents()[1] : 1;
		depth  = srcImg.getExtents().length > 2 ? srcImg.getExtents()[2] : 1;
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
	 * Returns reference to imaginary data array.
	 * 
	 * @return the reference the the imaginary data array
	 */
	public float[] getImaginaryData() {
		return imagData;
	}
	/**
	 * Returns reference to real data array.
	 * 
	 * @return the reference the the real data array
	 */
	public float[] getRealData() {
		return realData;
	}


	/**
	 * Starts the program.
	 */
	public void runAlgorithm() {

		if (srcImage == null) {
			displayError("Source Image is null");
			return;
		}
		destImage = null;
		int imgSize = width * height;
		if ( !image25D )
		{
			imgSize *= depth;
		}
		if ( transformDir == FORWARD )
		{
			if ( image25D )
			{
				float[][] data = new float[depth][imgSize*2];
				for ( int i = 0; i < depth; i++ )
				{
					for ( int j = 0; j < imgSize; j++ )
					{		
						data[i][j*2 + 0] = srcImage.getFloat(i*imgSize + j);
						data[i][j*2 + 1] = 0;
					}
				}
				calcFFT25D( data, srcImage.isComplexImage() );
			}
			else {
				float[] data = new float[imgSize*2];
				for ( int j = 0; j < imgSize; j++ )
				{		
					data[j*2 + 0] = srcImage.getFloat(j);
					data[j*2 + 1] = 0;
				}
				calcFFT( data, srcImage.isComplexImage() );
			}
		}
		else if ( srcImage.isComplexImage() )
		{
			if ( image25D )
			{
				float[][] data = new float[depth][imgSize * 2];
				for ( int i = 0; i < depth; i++ )
				{
					for ( int j = 0; j < imgSize; j++ )
					{
						data[i][j*2 + 0] = srcImage.getFloat(i*imgSize*2 + j*2 + 0);   
						data[i][j*2 + 1] = srcImage.getFloat(i*imgSize*2 + j*2 + 1); 
					}
				}
				calcFFT25D( data, srcImage.isComplexImage() );
			}
			else
			{
				float[] data = new float[imgSize * 2];
				for ( int j = 0; j < imgSize; j++ )
				{
					data[j*2 + 0] = srcImage.getFloat(j*2 + 0);   
					data[j*2 + 1] = srcImage.getFloat(j*2 + 1); 
				}
				calcFFT( data, srcImage.isComplexImage() );
			}
		}
	}


	public void saveImage(float[] data)
	{
		if ( destImage == null )
		{
			if ( complexInverse )
			{
				destImage = new ModelImage( ModelStorageBase.COMPLEX, srcImage.getExtents(), srcImage.getImageName() + "OpenCL_FFT_INV" );
			}
			else
			{
				destImage = new ModelImage( ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "OpenCL_FFT_INV" );
			}
		}
		int imgSize = width * height;
		float val, mag, re, im;
		int indexSrc, indexDest;
		for ( int slice = 0; slice < depth; slice++ )
		{
			for ( int y = 0; y < height; y++ )
			{
				for ( int x = 0; x < width; x++ )
				{
					indexDest = y*width + x;
					indexSrc = indexDest;

					re = data[slice * imgSize * 2 + indexSrc*2];
					im = data[slice * imgSize * 2 + indexSrc*2 + 1];

					//mag = (float)Math.sqrt( re*re + im*im);
					val = re / (width*height*depth);
					if ( complexInverse )
					{
						destImage.set(slice * imgSize * 2 + indexDest*2 + 0, re);
						destImage.set(slice * imgSize * 2 + indexDest*2 + 1, im);
					}
					else
					{
						destImage.set(slice * imgSize + indexDest, val);
					}
				}
			}
		}
		destImage.calcMinMax();
		new ViewJFrameImage(destImage);
	}

	public void saveImage(float[] data, int slice)
	{
		if ( destImage == null )
		{
			if ( complexInverse )
			{
				destImage = new ModelImage( ModelStorageBase.COMPLEX, srcImage.getExtents(), srcImage.getImageName() + "OpenCL_FFT_INV" );
			}
			else
			{
				destImage = new ModelImage( ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "OpenCL_FFT_INV" );
			}
		}
		int imgSize = width * height;
		float val, mag, re, im;
		int indexSrc, indexDest;
		for ( int y = 0; y < height; y++ )
		{
			for ( int x = 0; x < width; x++ )
			{
				indexDest = y*width + x;
				indexSrc = indexDest;

				re = data[indexSrc*2];
				im = data[indexSrc*2 + 1];

				mag = (float)Math.sqrt(re*re + im*im);
				val = mag / (width*height);
				if ( complexInverse )
				{
					destImage.set(slice * imgSize * 2 + indexDest*2 + 0, re);
					destImage.set(slice * imgSize * 2 + indexDest*2 + 1, im);
				}
				else
				{
					destImage.set(slice * imgSize + indexDest, val);
				}
			}
		}

		if ( slice == depth -1 )
		{
			destImage.calcMinMax();
			new ViewJFrameImage(destImage);
		}
	}



	public void saveImageFFT(float[] data)
	{
		if ( destImage == null )
		{
			destImage = new ModelImage( ModelStorageBase.COMPLEX, srcImage.getExtents(), srcImage.getImageName() + "OpenCL_FFT" );
		}
		int imgSize = width * height;
		float re, im;
		int indexSrc, indexDest;
		int ySrc, xSrc;
		for ( int slice = 0; slice < depth; slice++ )
		{
			for ( int y = 0; y < height; y++ )
			{
				for ( int x = 0; x < width; x++ )
				{
					indexDest = y*width + x;
					ySrc = (y + height/2)%height;
					xSrc = (x + width/2)%width;
					indexSrc = ySrc*width + xSrc;

					re = data[slice * imgSize * 2 + indexSrc*2];
					im = data[slice * imgSize * 2 + indexSrc*2 + 1];

					destImage.set(slice * imgSize * 2 + indexDest*2 + 0, re);
					destImage.set(slice * imgSize * 2 + indexDest*2 + 1, im);
				}
			}
		}
		destImage.setLogMagDisplay(logMagDisplay);
		destImage.calcMinMaxMag(logMagDisplay);
		destImage.setImage25D(false);
		new ViewJFrameImage(destImage);
	}

	public void saveImageFFT(float[] data, int slice)
	{
		if ( destImage == null )
		{
			destImage = new ModelImage( ModelStorageBase.COMPLEX, srcImage.getExtents(), srcImage.getImageName() + "OpenCL_FFT" );
		}
		int imgSize = width * height;
		float re, im;
		int indexSrc, indexDest;
		int ySrc, xSrc;
		for ( int y = 0; y < height; y++ )
		{
			for ( int x = 0; x < width; x++ )
			{
				indexDest = y*width + x;
				ySrc = (y + height/2)%height;
				xSrc = (x + width/2)%width;
				indexSrc = ySrc*width + xSrc;

				re = data[indexSrc*2];
				im = data[indexSrc*2 + 1];

				destImage.set(slice * imgSize * 2 + indexDest*2 + 0, re);
				destImage.set(slice * imgSize * 2 + indexDest*2 + 1, im);
			}
		}

		if ( slice == depth -1 )
		{
			destImage.setLogMagDisplay(logMagDisplay);
			destImage.calcMinMaxMag(logMagDisplay);
			destImage.setImage25D(true);
			new ViewJFrameImage(destImage);
		}
	}


	private void calcFFT(float[] input,  boolean isComplex)
	{

		cl_mem rBuffer;
		cl_mem rCBuffer;

		initCL(m_iDeviceType, null);

		cl_command_queue q = 
				clCreateCommandQueue(cl, device, 0, null);


		// pre-load and transform src, since that wont change
		float[] output = new float[width * height * depth * 2];
		rBuffer = CL.clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * input.length, Pointer.to(input), null);

		rCBuffer = CL.clCreateBuffer(cl, CL.CL_MEM_READ_WRITE,
				Sizeof.cl_float * output.length, Pointer.to(output), null);


		CLFFTPlan fftOp = null;
		if ( fftOp == null )
		{
			try {
				fftOp = new CLFFTPlan(cl, new int[]{width, height, depth}, CLFFTPlan.CLFFTDataFormat.InterleavedComplexFormat, device );
			} catch (InvalidContextException e) {
				e.printStackTrace();
			}
		}		

		//time = nanoTime() - time;
		//System.out.println("initCL : " + (time/1000000)+"ms");

		{
			if ( isComplex )
			{
				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Inverse, rCBuffer, rBuffer, null, null);  
				saveImage( output );  
			}
			else
			{
				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Forward, rBuffer, rCBuffer, null, null);
				saveImageFFT( output ); 
			}
		}

		clReleaseMemObject(rBuffer);
		clReleaseMemObject(rCBuffer);
		//fft.release();
	}


	private void calcFFT25D(float[][] input,  boolean isComplex)
	{
		initCL(m_iDeviceType, null);

		cl_command_queue q = 
				clCreateCommandQueue(cl, device, 0, null);


		CLFFTPlan fftOp = null;
		if ( fftOp == null )
		{
			try {
				fftOp = new CLFFTPlan(cl, new int[]{width, height}, CLFFTPlan.CLFFTDataFormat.InterleavedComplexFormat, device );
			} catch (InvalidContextException e) {
				e.printStackTrace();
			}
		}

		if ( isComplex )
		{
			// Enable exceptions and subsequently omit error checks in this sample
			//CL.setExceptionsEnabled(true);
		}
		//time = nanoTime() - time;
		//System.out.println("initCL : " + (time/1000000)+"ms");

		int[] errcode = new int[1];
		cl_mem rBuffer = null;
		// pre-load and transform src, since that wont change
		float[] output = new float[width * height * 2];
		cl_mem rCBuffer = CL.clCreateBuffer(cl, CL.CL_MEM_READ_WRITE, Sizeof.cl_float * output.length, null, null);
		for ( int i = 0; i < depth; i++ )
		{
			rBuffer = CL.clCreateBuffer(cl, CL.CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
					Sizeof.cl_float * input[i].length, Pointer.to(input[i]), errcode);
			if ( errcode[0] != CL.CL_SUCCESS )
			{
				System.err.println( stringFor_errorCode(errcode[0]) );
			}
			if ( isComplex )
			{
				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Inverse, rBuffer, rCBuffer, null, null);
				
				clEnqueueReadBuffer(q, rCBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
				saveImage( output, i );  
			}
			else
			{
				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Forward, rBuffer, rCBuffer, null, null);
				
				clEnqueueReadBuffer(q, rCBuffer, CL_TRUE, 0, Sizeof.cl_float * output.length, Pointer.to(output), 0, null, null);
				saveImageFFT( output, i ); 
			}			
			clReleaseMemObject(rBuffer);
		}
		clReleaseMemObject(rCBuffer);
		//fft.release();
	}
}
