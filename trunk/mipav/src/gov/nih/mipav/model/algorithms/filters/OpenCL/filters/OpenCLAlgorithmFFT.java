package gov.nih.mipav.model.algorithms.filters.OpenCL.filters;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.ViewJFrameImage;

import java.nio.IntBuffer;
import java.nio.FloatBuffer;
import java.util.Vector;
import java.util.BitSet;

import com.jogamp.opencl.CLBuffer;
import com.jogamp.opencl.CLCommandQueue;
import com.jogamp.opencl.CLContext;
import com.jogamp.opencl.CLDevice;
import com.jogamp.opencl.CLKernel;
import com.jogamp.opencl.CLProgram;
import com.jogamp.opencl.CLMemory.Mem;
import com.jogamp.opencl.CLPlatform;


public class OpenCLAlgorithmFFT extends AlgorithmBase {
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

	public static CLContext cl = null;
	public static CLDevice gpu = null;
	public static Vector<CLFFTPlan> fft = null;
	public static CLKernel splineTensor = null;

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

	private int width, height, depth;

	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	long startTime;

	public OpenCLAlgorithmFFT(final ModelImage srcImg, final int transformDir, final boolean logMagDisplay,
			final boolean unequalDim, final boolean image25D) {
		this(null, srcImg, transformDir, logMagDisplay, unequalDim, image25D);
	}

	public OpenCLAlgorithmFFT(final ModelImage destImg, final ModelImage srcImg, final int transformDir,
			final boolean logMagDisplay, final boolean unequalDim, final boolean image25D) {

		super(destImg, srcImg);
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
		}


		width  = srcImg.getExtents().length > 0 ? srcImg.getExtents()[0] : 1;
		height = srcImg.getExtents().length > 1 ? srcImg.getExtents()[1] : 1;
		depth  = srcImg.getExtents().length > 2 ? srcImg.getExtents()[2] : 1;
	}

	/**
	 * Prepare this class for destruction.
	 */
	@Override
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
	@Override
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
			destImage = new ModelImage( ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "OpenCL_FFT_INV" );
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

					mag = (float)Math.sqrt( re*re + im*im);
					val = mag / (width*height*depth);
					destImage.set(slice * imgSize + indexDest, val);
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
			destImage = new ModelImage( ModelStorageBase.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "OpenCL_FFT_INV" );
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

				mag = (float)Math.sqrt( re*re + im*im);
				val = mag / (width*height*depth);
				destImage.set(slice * imgSize + indexDest, val);
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

		CLCommandQueue q;
		CLBuffer<FloatBuffer> rBuffer;
		CLBuffer<FloatBuffer> rCBuffer;
		if ( fft == null )
		{
			fft = new Vector<CLFFTPlan> ();
		}
		CLFFTPlan fftOp = null;

		try {
			if ( cl == null || gpu == null )
			{			
				CLPlatform[] platforms = CLPlatform.listCLPlatforms();
				for (CLPlatform platform : platforms) {
					gpu = platform.getMaxFlopsDevice(CLDevice.Type.GPU);
					if(gpu != null) {
						break;
					}
				}
				cl = CLContext.create(gpu);
			}

			q = cl.getDevices()[0].createCommandQueue();

			rBuffer = cl.createFloatBuffer(width * height * depth * 2, Mem.READ_WRITE);
			rCBuffer = cl.createFloatBuffer(width * height * depth * 2, Mem.READ_WRITE);

			fftOp = checkFFT( width, height, depth );
			if ( fftOp == null )
			{
				fftOp = new CLFFTPlan(cl, new int[]{width, height, depth}, CLFFTPlan.CLFFTDataFormat.InterleavedComplexFormat, gpu );
				fft.add ( fftOp );
			}		
		} catch (Exception x) {
			System.out.println("failed to init cl");
			x.printStackTrace();
			return;
		}
		//time = nanoTime() - time;
		//System.out.println("initCL : " + (time/1000000)+"ms");

		// pre-load and transform src, since that wont change
		float[] output = new float[width * height * depth * 2];
		{
			if ( isComplex )
			{
				rCBuffer.getBuffer().position(0);
				rCBuffer.getBuffer().put(input);
				rCBuffer.getBuffer().position(0);
				q.putWriteBuffer(rCBuffer, false);

				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Inverse, rCBuffer, rBuffer, null, null);

				q.putReadBuffer(rBuffer, true);
				rBuffer.getBuffer().position(0);
				rBuffer.getBuffer().get(output);
				rBuffer.getBuffer().position(0);      
				saveImage( output );  
			}
			else
			{
				rBuffer.getBuffer().position(0);
				rBuffer.getBuffer().put(input);
				rBuffer.getBuffer().position(0);
				q.putWriteBuffer(rBuffer, false);

				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Forward, rBuffer, rCBuffer, null, null);

				q.putReadBuffer(rCBuffer, true);
				rCBuffer.getBuffer().position(0);
				rCBuffer.getBuffer().get(output);
				rCBuffer.getBuffer().position(0);       
				saveImageFFT( output ); 
			}
		}
		rBuffer.release();
		rCBuffer.release();
		//fft.release();
	}


	public static void FFT(float[] reData, float[] imData, int width, int height, int depth, boolean isComplex)
	{
		CLCommandQueue q;
		CLBuffer<FloatBuffer> rBuffer;
		CLBuffer<FloatBuffer> rCBuffer;
		if ( fft == null )
		{
			fft = new Vector<CLFFTPlan> ();
		}
		CLFFTPlan fftOp = null;

		try {
			if ( cl == null || gpu == null )
			{
				CLPlatform[] platforms = CLPlatform.listCLPlatforms();
				for (CLPlatform platform : platforms) {
					gpu = platform.getMaxFlopsDevice(CLDevice.Type.GPU);
					if(gpu != null) {
						break;
					}
				}

				cl = CLContext.create(gpu);
			}
			q = cl.getDevices()[0].createCommandQueue();

			rBuffer = cl.createFloatBuffer(width * height * depth * 2, Mem.READ_WRITE);
			rCBuffer = cl.createFloatBuffer(width * height * depth * 2, Mem.READ_WRITE);

			fftOp = checkFFT( width, height, depth );
			if ( fftOp == null )
			{
				fftOp = new CLFFTPlan(cl, new int[]{width, height, depth}, CLFFTPlan.CLFFTDataFormat.InterleavedComplexFormat, gpu );
				fft.add ( fftOp );
			}
		} catch (Exception x) {
			System.out.println("failed to init cl");
			x.printStackTrace();
			return;
		}
		//time = nanoTime() - time;
		//System.out.println("initCL : " + (time/1000000)+"ms");

		int length = width * height * depth;
		float[] input = new float[ length * 2 ];
		for ( int i = 0; i < length; i++ )
		{
			input[i * 2] = reData[i];
			input[i * 2 + 1] = imData[i];
		}

		// pre-load and transform src, since that wont change
		float[] output = new float[width * height * depth * 2];
		{
			if ( isComplex )
			{
				rCBuffer.getBuffer().position(0);
				rCBuffer.getBuffer().put(input);
				rCBuffer.getBuffer().position(0);
				q.putWriteBuffer(rCBuffer, false);

				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Inverse, rCBuffer, rBuffer, null, null);

				q.putReadBuffer(rBuffer, true);
				rBuffer.getBuffer().position(0);
				rBuffer.getBuffer().get(output);
				rBuffer.getBuffer().position(0);      

				for ( int i = 0; i < length; i++ )
				{
					reData[i] = output[i*2];
					imData[i] = output[i*2 + 1];
				}
			}
			else
			{
				rBuffer.getBuffer().position(0);
				rBuffer.getBuffer().put(input);
				rBuffer.getBuffer().position(0);
				q.putWriteBuffer(rBuffer, false);

				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Forward, rBuffer, rCBuffer, null, null);

				q.putReadBuffer(rCBuffer, true);
				rCBuffer.getBuffer().position(0);
				rCBuffer.getBuffer().get(output);
				rCBuffer.getBuffer().position(0);   
				for ( int i = 0; i < length; i++ )
				{
					reData[i] = output[i*2];
					imData[i] = output[i*2 + 1];
				}
			}
		}
		rBuffer.release();
		rCBuffer.release();
		//fftOp.release();
	}

	private static final String splineKernel = "kernel void fitSplines3DTensor( global bool *tMask,"
		+ " global float *workingBuffer,"
		+ " global float *d1Spline, global int *offsetSp," 
		+ " global float *AtF, global float *AtA,"
		+ " int dimX, int dimY,"
		+ " int newDimX, int newDimY,"
		+ " int lowerX, int lowerY, int lowerZ,"
		+ " int upperX, int upperY, int upperZ,"
		+ " int nProduct, int nDimensions )"
		+ " {"
		+ "    int index = get_global_id(0);"
		+ "    int indexTemp = index;"
		+ "    int x = indexTemp % dimX;"
		+ "    indexTemp -= x;"
		+ "    indexTemp /= dimX;"
		+ "    int y = indexTemp % dimY;"
		+ "    indexTemp -= y;"
		+ "    indexTemp /= dimY;"
		+ "    int z = indexTemp;"
		+ "    int fourX, fourY, fourZ;"
		+ "    int offset2, pValue, pLocation;"
		+ "    int i, j, k, l;"
		+ "    float value;"
		+ "    int tVal;"
		+ "    if ( (x >= lowerX) && (x <= upperX) &&"
		+ "         (y >= lowerY) && (y <= upperY) &&"
		+ "         (z >= lowerZ) && (z <= upperZ) )"
		+ "    {"
		+ "       fourX = 4 * x;"
		+ "       fourY = 4 * y;"
		+ "       fourZ = 4 * z;"
		+ "       offset2 = x + (y * newDimX);"
		+ "       index = offset2 + (z * newDimX * newDimY);"
		+ "       tVal = tMask[index];"
		+ "       if ( tVal != 0 )"
		+ "       {"
		+ "          value = workingBuffer[index];"
		+ "          pValue = 0;"
		+ "          pLocation = 0;"
		+ "          float values[64];"
		+ "          int locations[64];"
		+ "          int dloc_i[64];"
		+ "          int dloc_j[64];"
		+ "          for ( i = fourX; i < (fourX + 4); i++) {"
		+ "             float xv = d1Spline[i];"
		+ "             float xo = offsetSp[i];"
		+ "             for ( j = fourY; j < (fourY + 4); j++) {"
		+ "                float xyv = xv * d1Spline[newDimX * 4 + j];"
		+ "                float xyo = xo + offsetSp[newDimX * 4 + j];"
		+ "                for ( k = fourZ; k < (fourZ + 4); k++) {"
		+ "                   values[pValue++] = xyv * d1Spline[newDimX * 4 + newDimY * 4 + k];"
		+ "                   locations[pLocation++] = xyo + offsetSp[newDimX * 4 + newDimY * 4 + k];"
		+ "                }"
		+ "             }"
		+ "          }"            
		+ "          int pDloc_i = 0;"
		+ "          int pDloc_j = 0;"
		+ "          int pLocation = 0;"
		+ "          int pLocation2 = 1;"
		+ "          for (i = (64 - 2); i >= 0; i--) {"
		+ "             dloc_i[pDloc_i] = locations[pLocation2++] - locations[pLocation++];"
		+ "             dloc_j[pDloc_j++] = dloc_i[pDloc_i++] * nProduct;"
		+ "          }"
		+ "          int AtAP = 0;"
		+ "          int AtFP = 0;"
		+ "          int DP = AtAP + (locations[64 - 1] * nProduct) + locations[64 - 1];"
		+ "          int FP = AtFP + locations[64 - 1];"
		+ "          for (k = (64 - 1); k >= 0;) {"
		+ "             int UP = DP;"
		+ "             float value_k = values[k];"
		+ "             float AtfVal = AtF[FP];"
		+ "             barrier(CLK_GLOBAL_MEM_FENCE);"		
		+ "             AtF[FP] = AtfVal + value * value_k;"
		+ "             l = k - 1;"
		+ "             pValue = l;"
		+ "             pDloc_i = l;"   
		+ "             float AtAVal;"
		+ "             for ( l = k-1; l >= 0; l--) {"
		+ "                float incr = value_k * values[pValue--];"
		+ "                UP -= dloc_i[pDloc_i--];"
		+ "                int modUP = UP % nProduct;"
		+ "                int divUP = UP / nProduct;"
		+ "                AtAVal = AtA[divUP * nProduct + modUP];"
		+ "                barrier(CLK_GLOBAL_MEM_FENCE);"
		+ "                AtA[divUP * nProduct + modUP] = AtAVal + incr;"
		+ "                AtAVal = AtA[divUP * nProduct + modUP];"
		+ "                barrier(CLK_GLOBAL_MEM_FENCE);"		
		+ "                AtA[modUP * nProduct + divUP] = AtAVal + incr;"
		+ "             }"
		+ "             AtAVal = AtA[DP + (DP % nProduct)];"
		+ "             barrier(CLK_GLOBAL_MEM_FENCE);"
		+ "             AtA[DP + (DP % nProduct)] = AtAVal + value_k * value_k;"
		+ "             --k;"
		+ "             if (k >= 0) {"
		+ "                DP -= dloc_i[k] + dloc_j[k];"
		+ "                FP -= dloc_i[k];"
		+ "             }"
		+ "          }"
		+ "       }"
		+ "    }"
		+ " }";

		public static void fitSplines3DTensor(int[] mask, float[] workingBuffer, 
				float[][] d1Spline, int[][] offsetSp, int splineSize,
				double[][] AtF, float[][] AtA, 
				double[][] AtFTemp, float[][] AtATemp, 
				int[] dims, int[] newDims, int[] lower, int[] upper, 
				int nProduct, int nDimensions )
	{
		CLCommandQueue q;
		try {
			if ( cl == null || gpu == null )
			{
				CLPlatform[] platforms = CLPlatform.listCLPlatforms();
				for (CLPlatform platform : platforms) {
					gpu = platform.getMaxFlopsDevice(CLDevice.Type.GPU);
					if(gpu != null) {
						break;
					}
				}

				cl = CLContext.create(gpu);
			}
			q = cl.getDevices()[0].createCommandQueue();

			if ( splineTensor == null )
			{
				CLProgram prog = cl.createProgram(splineKernel);
				prog.build("-cl-mad-enable");

				
				splineTensor = prog.createCLKernel("fitSplines3DTensor");
			}

		} catch (Exception x) {
			System.out.println("failed to init cl");
			x.printStackTrace();
			return;
		}
		//time = nanoTime() - time;
		//System.out.println("initCL : " + (time/1000000)+"ms");
		int arg = 0;
		CLBuffer<IntBuffer> tBuffer = cl.createIntBuffer( mask.length );
		tBuffer.getBuffer().position(0);
		tBuffer.getBuffer().put(mask);
		tBuffer.getBuffer().position(0);
		q.putWriteBuffer(tBuffer, false);
		splineTensor.setArg(arg++, tBuffer);
		
		CLBuffer<FloatBuffer> wBuffer = cl.createFloatBuffer( workingBuffer.length );
		wBuffer.getBuffer().position(0);
		wBuffer.getBuffer().put(workingBuffer);
		wBuffer.getBuffer().position(0);
		q.putWriteBuffer(wBuffer, false);
		splineTensor.setArg(arg++, wBuffer);

		int index = 0;
		float[] d1Spline1D = new float[ splineSize ];
		int[] offsetSp1D = new int[ splineSize ];
		for ( int i = 0; i < d1Spline.length; i++ )
		{
			for ( int j = 0; j < d1Spline[i].length; j++ )
			{
				d1Spline1D[index] = d1Spline[i][j];
				offsetSp1D[index] = offsetSp[i][j];
				index++;
			}
		}
		CLBuffer<FloatBuffer> d1SplineBuffer = cl.createFloatBuffer( splineSize );
		d1SplineBuffer.getBuffer().position(0);
		d1SplineBuffer.getBuffer().put(d1Spline1D);
		d1SplineBuffer.getBuffer().position(0);
		q.putWriteBuffer(d1SplineBuffer, false);
		splineTensor.setArg(arg++, d1SplineBuffer);
		
		CLBuffer<IntBuffer> offsetSpBuffer = cl.createIntBuffer( splineSize );
		offsetSpBuffer.getBuffer().position(0);
		offsetSpBuffer.getBuffer().put(offsetSp1D);
		offsetSpBuffer.getBuffer().position(0);
		q.putWriteBuffer(offsetSpBuffer, false);
		splineTensor.setArg(arg++, offsetSpBuffer);

		index = 0;
		float[] AtA1D = new float[ nProduct * nProduct ];
		float[] AtF1D = new float[ nProduct ];
		for ( int i = 0; i < nProduct; i++ )
		{
			for ( int j = 0; j < nProduct; j++ )
			{
				AtA1D[index] = AtA[i][j];
			}
			AtF1D[index] = (float)AtF[i][0];
			index++;
		}
		CLBuffer<FloatBuffer> AtFBuffer = cl.createFloatBuffer( nProduct );
		AtFBuffer.getBuffer().position(0);
		AtFBuffer.getBuffer().put(AtF1D);
		AtFBuffer.getBuffer().position(0);
		q.putWriteBuffer(AtFBuffer, false);
		splineTensor.setArg(arg++, AtFBuffer);

		CLBuffer<FloatBuffer> AtABuffer = cl.createFloatBuffer( nProduct * nProduct );
		AtABuffer.getBuffer().position(0);
		AtABuffer.getBuffer().put(AtA1D);
		AtABuffer.getBuffer().position(0);
		q.putWriteBuffer(AtABuffer, false);
		splineTensor.setArg(arg++, AtABuffer);

		for ( int i = 0; i < 2; i++ )
		{
			splineTensor.setArg(arg++, dims[i]);
		}
		for ( int i = 0; i < 2; i++ )
		{
			splineTensor.setArg(arg++, newDims[i]);
		}
		for ( int i = 0; i < 3; i++ )
		{
			splineTensor.setArg(arg++, lower[i]);
		}
		for ( int i = 0; i < 3; i++ )
		{
			splineTensor.setArg(arg++, upper[i]);
		}
		splineTensor.setArg(arg++, nProduct);
		splineTensor.setArg(arg++, nDimensions);
		
		int localWorkSize = Math.min( mask.length/4, 256 );
		int numWorkGroups = (mask.length + localWorkSize - 1) / localWorkSize;
		int globalWorkSize = numWorkGroups * localWorkSize;
		q.putBarrier();
		q.put1DRangeKernel(splineTensor, 0, globalWorkSize, localWorkSize );
		q.putBarrier();
		
		q.putReadBuffer( AtFBuffer, true );
		AtFBuffer.getBuffer().position(0);
		AtFBuffer.getBuffer().get(AtF1D);
		AtFBuffer.getBuffer().position(0);

		q.putReadBuffer( AtABuffer, true );
		AtABuffer.getBuffer().position(0);
		AtABuffer.getBuffer().get(AtA1D);
		AtABuffer.getBuffer().position(0);


		index = 0;
		for ( int i = 0; i < d1Spline.length; i++ )
		{
			for ( int j = 0; j < d1Spline[i].length; j++ )
			{
				AtATemp[i][j] = AtA1D[index];
			}
			AtFTemp[i][0] = AtF1D[index];
			index++;
		}
		
		tBuffer.release();
		wBuffer.release();
		d1SplineBuffer.release();
		offsetSpBuffer.release();
		AtFBuffer.release();
		AtABuffer.release();
	}

	private static CLFFTPlan checkFFT( int width, int height, int depth )
	{
		for ( int i = 0; i < fft.size(); i++ )
		{
			CLFFTPlan fftOp = fft.elementAt(i);
			if ( (fftOp.size.x == width) && (fftOp.size.y == height) && (fftOp.size.z == depth) )
			{
				return fftOp;
			}
		}
		return null;
	}


	private void calcFFT25D(float[][] input,  boolean isComplex)
	{

		CLCommandQueue q;
		CLBuffer<FloatBuffer> rBuffer;
		CLBuffer<FloatBuffer> rCBuffer;
		if ( fft == null )
		{
			fft = new Vector<CLFFTPlan> ();
		}
		CLFFTPlan fftOp = null;

		try {
			if ( cl == null || gpu == null )
			{			
				CLPlatform[] platforms = CLPlatform.listCLPlatforms();
				for (CLPlatform platform : platforms) {
					gpu = platform.getMaxFlopsDevice(CLDevice.Type.GPU);
					if(gpu != null) {
						break;
					}
				}
				cl = CLContext.create(gpu);
			}

			q = cl.getDevices()[0].createCommandQueue();

			rBuffer = cl.createFloatBuffer(width * height * 2, Mem.READ_WRITE);
			rCBuffer = cl.createFloatBuffer(width * height * 2, Mem.READ_WRITE);
			fftOp = checkFFT( width, height, 1 );
			if ( fftOp == null )
			{
				fftOp = new CLFFTPlan(cl, new int[]{width, height}, CLFFTPlan.CLFFTDataFormat.InterleavedComplexFormat, gpu );
				fft.add ( fftOp );
			}
		} catch (Exception x) {
			System.out.println("failed to init cl");
			x.printStackTrace();
			return;
		}
		//time = nanoTime() - time;
		//System.out.println("initCL : " + (time/1000000)+"ms");

		// pre-load and transform src, since that wont change
		float[] output = new float[width * height * 2];
		for ( int i = 0; i < depth; i++ )
		{
			if ( isComplex )
			{
				rCBuffer.getBuffer().position(0);
				rCBuffer.getBuffer().put(input[i]);
				rCBuffer.getBuffer().position(0);
				q.putWriteBuffer(rCBuffer, false);

				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Inverse, rCBuffer, rBuffer, null, null);

				q.putReadBuffer(rBuffer, true);
				rBuffer.getBuffer().position(0);
				rBuffer.getBuffer().get(output);
				rBuffer.getBuffer().position(0);      
				saveImage( output, i );  
			}
			else
			{
				rBuffer.getBuffer().position(0);
				rBuffer.getBuffer().put(input[i]);
				rBuffer.getBuffer().position(0);
				q.putWriteBuffer(rBuffer, false);

				fftOp.executeInterleaved(q, 1, CLFFTPlan.CLFFTDirection.Forward, rBuffer, rCBuffer, null, null);

				q.putReadBuffer(rCBuffer, true);
				rCBuffer.getBuffer().position(0);
				rCBuffer.getBuffer().get(output);
				rCBuffer.getBuffer().position(0);       
				saveImageFFT( output, i ); 
			}			
		}
		rBuffer.release();
		rCBuffer.release();
		//fft.release();
	}
}
