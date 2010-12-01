package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;
import java.util.*;


/**
 * Computes the maximum intensity along each projection of a 3D image. 
 * @author joshim2
 *
 */
public class AlgorithmMaximumIntensityProjection extends AlgorithmBase {

	//	~ Instance fields ------------------------------------------------------------------------------------------------
	/** Array of Result Images */
	private Vector<ModelImage> resultImages;

	/** Source Image Resolutions */
	float [] imResolutions;

	/** Extents for X Projection Image */
	private int [] XExtents;

	/** Extents for Y Projection Image */
	private int [] YExtents;

	/** Extents for Z Projection Image */
	private int [] ZExtents;

	/** Minimum intensity value. */
	private int[] startSlice;

	/** Maximum intensity value. */
	private int[] stopSlice;

	/** Minimum intensity value. */
	private float[] min;

	/** Maximum intensity value. */
	private float[] max;

	private float minR;
	private float maxR;
	private float minG;
	private float maxG;
	private float minB;
	private float maxB;

	private boolean[] maximum;
	private boolean[] minimum;
	public static int Z_PROJECTION = 1;
	public static int Y_PROJECTION = 2;
	public static int X_PROJECTION = 4;
	public static int ALL = Z_PROJECTION | Y_PROJECTION | X_PROJECTION;
	private int projection = ALL;

	//	~ Constructors ---------------------------------------------------------------------------------------------------

	/**
	 * Estimates the maximum intensity projection in each direction of a 3D black and white image
	 * @param  srcImg    source image model
	 * @param  _min
	 * @param  _max
	 */
	public AlgorithmMaximumIntensityProjection(ModelImage srcImg, 
			int[] _startSlice, int[] _stopSlice,
			float[] _min, float[] _max, boolean[] _maximum, boolean[] _minimum, int _projection ) {
		super(null, srcImg);
		startSlice = _startSlice;
		stopSlice = _stopSlice;
		min = _min;
		max = _max;
		maximum = _maximum;
		minimum = _minimum;
		projection = _projection;
		imResolutions = srcImg.getResolutions(0);
		resultImages = new Vector<ModelImage>();

	}

	/**
	 * Estimates the maximum intensity projection in each direction of a 3D color image
	 * @param  srcImg    source image model
	 * @param  _minR
	 * @param  _maxR
	 * @param  _minG
	 * @param  _maxG
	 * @param  _minB
	 * @param  _maxB
	 */
	public AlgorithmMaximumIntensityProjection(ModelImage srcImg, float _minR, float _maxR,
			float _minG, float _maxG, float _minB, float _maxB) {
		super(null, srcImg);
		minR = _minR;
		maxR = _maxR;
		minG = _minG;
		maxG = _maxG;
		minB = _minB;
		maxB = _maxB;
		imResolutions = srcImg.getResolutions(0);
		resultImages = new Vector<ModelImage>();

	}

	//  ~ Methods --------------------------------------------------------------------------------------------------------

	/**
	 * Prepares this class for destruction.
	 */
	public void finalize() {
		super.finalize();
	}

	/**
	 * Runs the Maximum Intensity Projection algorithm.
	 */
	public void runAlgorithm() {

		if (srcImage == null) {
			displayError("Source Image is null");
			finalize();
			return;
		} else if (srcImage.isColorImage()) {
			calcColor();
		} else {
			calc();
		}
	}

	/**
	 * This method computes the maximum intenisty projection of a 3D color image.
	 */
	private void calcColor() {
		int length;
		int i, j, k;
		int index;
		float [] buffer;
		float [] resultBufferX;
		float [] resultBufferY;
		float [] resultBufferZ;
		float maxIntensityValueR = 0f;
		float maxIntensityValueG = 0f;
		float maxIntensityValueB = 0f;
		int [] dim = srcImage.getExtents();

		try {
			length = 4 * srcImage.getSliceSize() * srcImage.getExtents() [2];
			buffer = new float[length];
			srcImage.exportData(0, length, buffer);
		} catch (IOException error) {
			buffer = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
			return;
		} catch (OutOfMemoryError e) {
			buffer = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
			return;
		}

		int totalLength = (dim[0]*dim[1]) + (dim[2]*dim[0]) + (dim[1]*dim[2]);
		int mod = 0;
		fireProgressStateChanged(mod, srcImage.getImageName(), "Computing Maximum Intensity Projection ...");

		// For maximum intensity along Z-axis

		int lengthZ = dim[0] * dim[1]; // No. of pixels Z projection image
		resultBufferZ = new float[4*lengthZ];
		ZExtents = new int[2];
		System.arraycopy(dim, 0, ZExtents, 0, 2);

		for (i = 0; i < lengthZ; i++) {

			// Compute the max intensity value along Z-axis
			for (j = 0; j < dim[2]; j++) {
				index = 4*(i + (j * lengthZ));
				if ((buffer[index + 1] < minR) || (buffer[index + 1] > maxR)) {
					buffer[index + 1] = 0;
				}

				if (buffer[index + 1] > maxIntensityValueR) {
					maxIntensityValueR = buffer[index + 1];
				}

				if ((buffer[index + 2] < minG) || (buffer[index + 2] > maxG)) {
					buffer[index + 2] = 0;
				}

				if (buffer[index + 2] > maxIntensityValueG) {
					maxIntensityValueG = buffer[index + 2];
				}

				if ((buffer[index + 3] < minB) || (buffer[index + 3] > maxB)) {
					buffer[index + 3] = 0;
				}

				if (buffer[index + 3] > maxIntensityValueB) {
					maxIntensityValueB = buffer[index + 3];
				}
			}

			resultBufferZ[4*i+1] = maxIntensityValueR;
			maxIntensityValueR = 0f;
			resultBufferZ[4*i+2] = maxIntensityValueG;
			maxIntensityValueG = 0f;
			resultBufferZ[4*i+3] = maxIntensityValueB;
			maxIntensityValueB = 0f;
			mod = mod + 1; // For progressbar purposes
		}

		// Reconstruct Z Projection image from result buffer

		ModelImage ZProjectionImage = new ModelImage(srcImage.getType(), ZExtents, "ZProjectionImage");
		resultImages.add(ZProjectionImage);

		try {

			ZProjectionImage.importData(0, resultBufferZ, true);
		} catch (IOException error) {

			resultBufferZ = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
			return;
		} catch (OutOfMemoryError e) {

			resultBufferZ = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
			return;
		}

		// Set resolutions in line with the original source image resolutions
		float [] ZRes = new float[2];
		System.arraycopy(imResolutions, 0, ZRes, 0, 2);
		ZProjectionImage.setResolutions(0, ZRes);

		//Update progress bar
		fireProgressStateChanged(Math.round((mod / totalLength) * 100));

		// For maximum intensity along Y-axis

		int lengthY = dim[2] * dim[0]; // No. of pixels Y projection image
		resultBufferY = new float[4*lengthY];
		YExtents = new int[2];
		YExtents[0] = dim[0];
		YExtents[1] = dim[2];
		maxIntensityValueR = 0;
		maxIntensityValueG = 0;
		maxIntensityValueB = 0;

		for (i = 0; i < dim[2]; i++) {

			for (j = 0; j < dim[0]; j++) {

				for (k = 0; k < dim[1]; k++) {
					index = 4* ((dim[0]*dim[1]*i) + j + (dim[0]*k));
					if ((buffer[index + 1] < minR) || (buffer[index + 1] > maxR)) {
						buffer[index + 1] = 0;
					}

					if (buffer[index+1] > maxIntensityValueR) {
						maxIntensityValueR = buffer[index+1];
					}

					if ((buffer[index + 2] < minG) || (buffer[index + 2] > maxG)) {
						buffer[index + 2] = 0;
					}

					if (buffer[index+2] > maxIntensityValueG) {
						maxIntensityValueG = buffer[index+2];
					}

					if ((buffer[index + 3] < minB) || (buffer[index + 3] > maxB)) {
						buffer[index + 3] = 0;
					}

					if (buffer[index+3] > maxIntensityValueB) {
						maxIntensityValueB = buffer[index+3];
					}
				}
				resultBufferY[4*(j + i*dim[0]) + 1] = maxIntensityValueR;
				maxIntensityValueR = 0f;
				resultBufferY[4*(j + i*dim[0]) + 2] = maxIntensityValueG;
				maxIntensityValueG = 0f;
				resultBufferY[4*(j + i*dim[0]) + 3] = maxIntensityValueB;
				maxIntensityValueB = 0f;
				mod = mod + 1; // For progress bar purposes
			}           
		}

		// Reconstruct Y Projection image from result buffer

		ModelImage YProjectionImage = new ModelImage(srcImage.getType(), YExtents, "YProjectionImage");
		resultImages.add(YProjectionImage);

		try {

			YProjectionImage.importData(0, resultBufferY, true);
		} catch (IOException error) {

			resultBufferY = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
			return;
		} catch (OutOfMemoryError e) {

			resultBufferY = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
			return;
		}

		// Set resolutions in line with the original source image resolutions
		float [] YRes = new float[2];
		YRes[0] = imResolutions[0];
		YRes[1] = imResolutions[2];
		YProjectionImage.setResolutions(0, YRes);

		// Update progress bar
		fireProgressStateChanged(Math.round((mod / totalLength) * 100));

		// For maximum intensity along X-axis

		int lengthX = dim[1] * dim[2]; // No. of pixels X projection image
		resultBufferX = new float[4*lengthX];
		XExtents = new int[2];
		XExtents[0] = dim[2];
		XExtents[1] = dim[1];
		maxIntensityValueR = 0;
		maxIntensityValueG = 0;
		maxIntensityValueB = 0;

		for (i = 0; i < dim[2]; i++) {

			for (j = 0; j < dim[1]; j++) {

				for (k = 0; k < dim[0]; k++) {
					index = 4*((i*dim[1]*dim[0]) + (dim[0]*j) + k);
					if ((buffer[index+1] < minR) || (buffer[index+1] > maxR)) {
						buffer[index+1] = 0;
					}

					if (buffer[index+1] > maxIntensityValueR) {
						maxIntensityValueR = buffer[index+1];
					}

					if ((buffer[index+2] < minG) || (buffer[index+2] > maxG)) {
						buffer[index+2] = 0;
					}

					if (buffer[index+2] > maxIntensityValueG) {
						maxIntensityValueG = buffer[index+2];
					}

					if ((buffer[index+3] < minB) || (buffer[index+3] > maxB)) {
						buffer[index+3] = 0;
					}

					if (buffer[index+3] > maxIntensityValueB) {
						maxIntensityValueB = buffer[index+3];
					}
				}
				resultBufferX[4*(i + (j*dim[2])) + 1] = maxIntensityValueR;
				maxIntensityValueR = 0f;
				resultBufferX[4*(i + (j*dim[2])) + 2] = maxIntensityValueG;
				maxIntensityValueG = 0f;
				resultBufferX[4*(i + (j*dim[2])) + 3] = maxIntensityValueB;
				maxIntensityValueB = 0f;
				mod = mod + 1; // For progress bar purposes
			}
		}

		// Construct X Projection image from result buffer

		ModelImage XProjectionImage = new ModelImage(srcImage.getType(), XExtents, "XProjectionImage");
		resultImages.add(XProjectionImage);
		try {

			XProjectionImage.importData(0, resultBufferX, true);
		} catch (IOException error) {

			resultBufferX = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
			return;
		} catch (OutOfMemoryError e) {

			resultBufferX = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
			return;
		}

		// Set resolutions in line with the original source image resolutions
		float [] XRes = new float[2];
		XRes[0] = imResolutions[2];
		XRes[1] = imResolutions[1];
		XProjectionImage.setResolutions(0, XRes);

		// Update progress bar
		fireProgressStateChanged(Math.round((mod / totalLength) * 100));
	}

	/**
	 * This method computes the maximum intensity projection of a 3D black and white image.
	 */

	private void calc() {

		int length;
		int i, j, k;
		double [] buffer;
		double maxIntensityValue = Double.MIN_VALUE;
		double minIntensityValue = Double.MAX_VALUE;
		int dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
		int dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
		int dimZ = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;

		try {
			length = srcImage.getSize();
			buffer = new double[length];
			srcImage.exportData(0, length, buffer);
		} catch (IOException error) {
			buffer = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
			return;
		} catch (OutOfMemoryError e) {
			buffer = null;
			errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
			return;
		}

		int totalLength = (dimX*dimY) + (dimZ*dimX) + (dimY*dimZ);
		int mod = 0;
		fireProgressStateChanged(mod, srcImage.getImageName(), "Computing Maximum Intensity Projection ...");

		if ( (projection & Z_PROJECTION) == Z_PROJECTION )
		{
			// For maximum intensity along Z-axis

			int lengthZ = dimX * dimY; // No. of pixels Z projection image
			double[] resultBufferMaxZ = new double[lengthZ];
			double[] resultBufferMinZ = new double[lengthZ];
			ZExtents = new int[]{dimX,dimY};

			float minZ = min[0];
			float maxZ = max[0];
			int startZ = Math.max( 0, startSlice[0] );
			int stopZ = Math.min( lengthZ, stopSlice[0] );
			for (i = 0; i < lengthZ; i++)
			{
				// Compute the max intensity value along Z-axis
				for (j = startZ; j < stopZ; j++)
				{
					if ((buffer[i + (j * lengthZ)] >= minZ) && (buffer[i + (j * lengthZ)] <= maxZ))
					{
						if (buffer[i + (j * lengthZ)] > maxIntensityValue)
						{
							maxIntensityValue = buffer[i + (j * lengthZ)];
						}
						if ( buffer[i + (j * lengthZ)] < minIntensityValue )
						{
							minIntensityValue = buffer[i + (j * lengthZ)];
						}
					}
				}
				resultBufferMaxZ[i] = maxIntensityValue;
				maxIntensityValue = Double.MIN_VALUE;

				resultBufferMinZ[i] = minIntensityValue;
				minIntensityValue = Double.MAX_VALUE;
			}

			// Reconstruct Z Projection image from result buffer
			if ( maximum[0] )
			{
				ModelImage ZProjectionImage = new ModelImage(srcImage.getType(), ZExtents, "ZProjectionImageMax");
				resultImages.add(ZProjectionImage);
				try {
					ZProjectionImage.importData(0, resultBufferMaxZ, true);
				} catch (IOException error) {
					resultBufferMaxZ = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMaxZ = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}

				// Set resolutions in line with the original source image resolutions
				float [] ZRes = new float[2];
				System.arraycopy(imResolutions, 0, ZRes, 0, 2);
				ZProjectionImage.setResolutions(0, ZRes);
			}
			if ( minimum[0] )
			{
				ModelImage ZProjectionImage = new ModelImage(srcImage.getType(), ZExtents, "ZProjectionImageMin");
				resultImages.add(ZProjectionImage);
				try {
					ZProjectionImage.importData(0, resultBufferMinZ, true);
				} catch (IOException error) {
					resultBufferMinZ = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMinZ = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}

				// Set resolutions in line with the original source image resolutions
				float [] ZRes = new float[2];
				System.arraycopy(imResolutions, 0, ZRes, 0, 2);
				ZProjectionImage.setResolutions(0, ZRes);				
			}
		}

		//Update progress bar
		mod = dimX *dimY;
		fireProgressStateChanged(Math.round((mod / totalLength) * 100));

		if ( (projection & Y_PROJECTION) == Y_PROJECTION )
		{
			// For maximum intensity along Y-axis

			int lengthY = dimZ * dimX; // No. of pixels Y projection image
			double[] resultBufferMaxY = new double[lengthY];
			double[] resultBufferMinY = new double[lengthY];
			YExtents = new int[2];
			YExtents[0] = dimX;
			YExtents[1] = dimZ;
			maxIntensityValue = 0;
			float minY = min[1];
			float maxY = max[1];
			
			int startY = Math.max( 0, startSlice[1] );
			int stopY = Math.min( dimY, stopSlice[1] );

			for (i = 0; i < dimZ; i++)
			{
				for (j = 0; j < dimX; j++)
				{
					for (k = startY; k < stopY; k++)
					{
						if ((buffer[(dimX*dimY*i) + j + (dimX*k)] >= minY) &&
								(buffer[(dimX*dimY*i) + j + (dimX*k)] <= maxY))
						{
							if (buffer[(dimX*dimY*i) + j + (dimX*k)] > maxIntensityValue)
							{
								maxIntensityValue = buffer[(dimX*dimY*i) + j + (dimX*k)];
							}
							if (buffer[(dimX*dimY*i) + j + (dimX*k)] < minIntensityValue) 
							{
								minIntensityValue = buffer[(dimX*dimY*i) + j + (dimX*k)];
							}
						}
					}
					resultBufferMaxY[j + i*dimX] = maxIntensityValue;
					maxIntensityValue = Double.MIN_VALUE;
					resultBufferMinY[j + i*dimX] = minIntensityValue;
					minIntensityValue = Double.MAX_VALUE;
				} 			
			}

			// Reconstruct Y Projection image from result buffer
			if ( maximum[1] )
			{
				ModelImage YProjectionImage = new ModelImage(srcImage.getType(), YExtents, "YProjectionImageMax");
				resultImages.add(YProjectionImage);
				try {
					YProjectionImage.importData(0, resultBufferMaxY, true);
				} catch (IOException error) {
					resultBufferMaxY = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMaxY = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}
				// Set resolutions in line with the original source image resolutions
				float [] YRes = new float[2];
				YRes[0] = imResolutions[0];
				YRes[1] = imResolutions[2];
				YProjectionImage.setResolutions(0, YRes);
			}
			if ( minimum[1] )
			{
				ModelImage YProjectionImage = new ModelImage(srcImage.getType(), YExtents, "YProjectionImageMin");
				resultImages.add(YProjectionImage);
				try {
					YProjectionImage.importData(0, resultBufferMinY, true);
				} catch (IOException error) {
					resultBufferMinY = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMinY = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}
				// Set resolutions in line with the original source image resolutions
				float [] YRes = new float[2];
				YRes[0] = imResolutions[0];
				YRes[1] = imResolutions[2];
				YProjectionImage.setResolutions(0, YRes);
			}
		}

		// Update progress bar
		mod += dimX*dimZ;
		fireProgressStateChanged(Math.round((mod / totalLength) * 100));

		if ( (projection & X_PROJECTION) == X_PROJECTION )
		{
			// For maximum intensity along X-axis

			int lengthX = dimY * dimZ; // No. of pixels X projection image
			double[] resultBufferMaxX = new double[lengthX];
			double[] resultBufferMinX = new double[lengthX];
			XExtents = new int[2];
			XExtents[0] = dimZ;
			XExtents[1] = dimY;
			maxIntensityValue = 0;
			float minX = min[2];
			float maxX = max[2];
			
			int startX = Math.max( 0, startSlice[2] );
			int stopX = Math.min( dimX, stopSlice[2] );

			for (i = 0; i < dimZ; i++)
			{
				for (j = 0; j < dimY; j++)
				{
					for (k = startX; k < stopX; k++)
					{
						if ((buffer[(i*dimY*dimX) + (dimX*j) + k] >= minX) &&
								(buffer[(i*dimY*dimX) + (dimX*j) + k] <= maxX))
						{
							if (buffer[(i*dimY*dimX) + (dimX*j) + k] > maxIntensityValue)
							{
								maxIntensityValue = buffer[(i*dimY*dimX) + (dimX*j) + k];
							}

							if (buffer[(i*dimY*dimX) + (dimX*j) + k] < minIntensityValue)
							{
								minIntensityValue = buffer[(i*dimY*dimX) + (dimX*j) + k];
							}
						}
					}
					resultBufferMaxX[i + (j*dimZ)] = maxIntensityValue;
					maxIntensityValue = Double.MIN_VALUE;
					resultBufferMinX[i + (j*dimZ)] = minIntensityValue;
					minIntensityValue = Double.MAX_VALUE;
				}
			}

			// Construct X Projection image from result buffer
			if ( maximum[2] )
			{
				ModelImage XProjectionImage = new ModelImage(srcImage.getType(), XExtents, "XProjectionImageMax");
				resultImages.add(XProjectionImage);

				try {
					XProjectionImage.importData(0, resultBufferMaxX, true);
				} catch (IOException error) {
					resultBufferMaxX = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMaxX = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}

				// Set resolutions in line with the original source image resolutions
				float [] XRes = new float[2];
				XRes[0] = imResolutions[2];
				XRes[1] = imResolutions[1];
				XProjectionImage.setResolutions(0, XRes);
			}
			if ( minimum[2] )
			{
				ModelImage XProjectionImage = new ModelImage(srcImage.getType(), XExtents, "XProjectionImageMin");
				resultImages.add(XProjectionImage);

				try {
					XProjectionImage.importData(0, resultBufferMinX, true);
				} catch (IOException error) {
					resultBufferMinX = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMinX = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}

				// Set resolutions in line with the original source image resolutions
				float [] XRes = new float[2];
				XRes[0] = imResolutions[2];
				XRes[1] = imResolutions[1];
				XProjectionImage.setResolutions(0, XRes);
			}
		}
		// Update progress bar
		mod += dimY*dimZ;
		fireProgressStateChanged(Math.round((mod / totalLength) * 100));
	}


	/**
	 * This method returns the projection images in an arraylist
	 */
	public Vector<ModelImage> getResultImage() {
		return resultImages;

	}
}







