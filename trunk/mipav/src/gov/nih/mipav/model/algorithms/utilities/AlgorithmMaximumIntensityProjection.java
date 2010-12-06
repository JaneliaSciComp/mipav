package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

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

	/** Extents for Z Projection Image */
	private int [] ZExtents;

	private int startSlice;
	private int stopSlice;
	private int window;

	/** Minimum intensity value. */
	private double[] minIntensity;

	/** Maximum intensity value. */
	private double[] maxIntensity;

	private boolean computeMaximum;
	private boolean computeMinimum;
	public static final int X_PROJECTION = 0;
	public static final int Y_PROJECTION = 1;
	public static final int Z_PROJECTION = 2;
	private int projectionDirection = Z_PROJECTION;
	
	private int colorFactor = 1;

	//	~ Constructors ---------------------------------------------------------------------------------------------------

	/**
	 * Estimates the maximum intensity projection in each direction of a 3D black and white image
	 * @param  srcImg    source image model
	 * @param  _min
	 * @param  _max
	 */
	public AlgorithmMaximumIntensityProjection(ModelImage srcImg, 
			int _startSlice, int _stopSlice, int _window,
			float _minIntensity, float _maxIntensity, 
			boolean _computeMaximum, boolean _computeMinimum, int _projectionDirection ) {
		super(null, srcImg);
		startSlice = _startSlice;
		stopSlice = _stopSlice;
		window = _window;
		computeMaximum = _computeMaximum;
		computeMinimum = _computeMinimum;
		projectionDirection = _projectionDirection;
		imResolutions = srcImg.getResolutions(0);
		resultImages = new Vector<ModelImage>();
		
		colorFactor = 1;
		minIntensity = new double[colorFactor];
		minIntensity[0] = _minIntensity;
		maxIntensity = new double[colorFactor];
		maxIntensity[0] = _maxIntensity;
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
	public AlgorithmMaximumIntensityProjection(ModelImage srcImg, 
			int _startSlice, int _stopSlice, int _window,
			float _minR, float _maxR,
			float _minG, float _maxG, 
			float _minB, float _maxB, 
			boolean _computeMaximum, boolean _computeMinimum, int _projectionDirection) {
		super(null, srcImg);
		startSlice = _startSlice;
		stopSlice = _stopSlice;
		window = _window;
		computeMaximum = _computeMaximum;
		computeMinimum = _computeMinimum;
		projectionDirection = _projectionDirection;
		imResolutions = srcImg.getResolutions(0);
		resultImages = new Vector<ModelImage>();
		
		colorFactor = 4;
		minIntensity = new double[colorFactor];
		minIntensity[0] = 0;
		minIntensity[1] = _minR;
		minIntensity[2] = _minG;
		minIntensity[3] = _minB;
		maxIntensity = new double[colorFactor];
		maxIntensity[0] = 255;
		maxIntensity[1] = _maxR;
		maxIntensity[2] = _maxG;
		maxIntensity[3] = _maxB;
	}

	//  ~ Methods --------------------------------------------------------------------------------------------------------

	/**
	 * Prepares this class for destruction.
	 */
	public void finalize() {
		super.finalize();
	}

	/**
	 * Runs the Intensity Projection algorithm.
	 */
	public void runAlgorithm() {

		if (srcImage == null) {
			displayError("Source Image is null");
			finalize();
			return;
		} 
		
		switch ( projectionDirection ) 
		{
		case X_PROJECTION: calcXProjection(); break;
		case Y_PROJECTION: calcYProjection(); break;
		case Z_PROJECTION: calcZProjection(); break;
		}
	}

	private void calcZProjection() {

		int dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
		int dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
		int dimZ = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;

		double [] buffer;
		try {
			int length = srcImage.getSize();
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

		int lengthZ = dimX * dimY; // No. of pixels Z projection image
		int startZ = Math.max( 0, startSlice );
		int stopZ = Math.min( dimZ, stopSlice + 1 );

		// For maximum intensity along Z-axis
		int windowSize = window;
		int newSlices = (stopZ - startZ) - windowSize + 1;
		ZExtents = ( newSlices == 1 ) ? new int[]{dimX,dimY} : new int[]{dimX,dimY,newSlices};		
		
		double[][] resultBufferMaxZ = new double[newSlices][lengthZ * colorFactor]; 
		double[][] resultBufferMinZ = new double[newSlices][lengthZ * colorFactor];
		
		double[] maxIntensityValue = new double[colorFactor];
		double[] minIntensityValue = new double[colorFactor];
		for ( int c = 0; c < colorFactor; c++ )
		{
			maxIntensityValue[c] = Double.MIN_VALUE;
			minIntensityValue[c] = Double.MAX_VALUE;
		}
		
		ModelImage ZProjectionImageMax = null;
		ModelImage ZProjectionImageMin = null;
		if ( computeMaximum )
		{
			ZProjectionImageMax = new ModelImage( srcImage.getType(), ZExtents, 
					srcImage.getImageName() + "_ZProjectionMax" );
			resultImages.add(ZProjectionImageMax);
	        JDialogBase.updateFileInfo( srcImage, ZProjectionImageMax );
			// Set resolutions in line with the original source image resolutions
			float [] ZRes = ( newSlices == 1 ) ? new float[2] : new float[3];
			
			System.arraycopy(imResolutions, 0, ZRes, 0, 2);
			ZProjectionImageMax.setResolutions(0, ZRes);	
		}
		if ( computeMinimum )
		{
			ZProjectionImageMin = new ModelImage(srcImage.getType(), ZExtents,
					srcImage.getImageName() + "_ZProjectionMin" );
			resultImages.add(ZProjectionImageMin);
	        JDialogBase.updateFileInfo( srcImage, ZProjectionImageMin );
		}

		for ( int iSlice = 0; iSlice < newSlices; iSlice++ )
		{
			for (int i = 0; i < lengthZ; i++ )
			{
				// Compute the max intensity value along Z-axis
				for (int j = startZ + iSlice; j < startZ + iSlice + windowSize; j++)
				{
					for ( int c = 0; c < colorFactor; c++ )
					{
						if ((buffer[colorFactor * (i + (j * lengthZ)) + c] >= minIntensity[c]) &&
								(buffer[colorFactor * (i + (j * lengthZ)) + c] <= maxIntensity[c]))
						{
							if (buffer[colorFactor * (i + (j * lengthZ)) + c] > maxIntensityValue[c])
							{
								maxIntensityValue[c] = buffer[colorFactor * (i + (j * lengthZ)) + c];
							}
							if ( buffer[colorFactor * (i + (j * lengthZ)) + c] < minIntensityValue[c] )
							{
								minIntensityValue[c] = buffer[colorFactor * (i + (j * lengthZ)) + c];
							}
						}
					}
				}
				for ( int c = 0; c < colorFactor; c++ )
				{
					resultBufferMaxZ[iSlice][i*colorFactor + c] = maxIntensityValue[c];
					maxIntensityValue[c] = Double.MIN_VALUE;
					resultBufferMinZ[iSlice][i*colorFactor + c] = minIntensityValue[c];
					minIntensityValue[c] = Double.MAX_VALUE;
				}
			}

			// Reconstruct Z Projection image from result buffer
			if ( computeMaximum )
			{
				try {
					ZProjectionImageMax.importData(iSlice*lengthZ*colorFactor, resultBufferMaxZ[iSlice], false);
				} catch (IOException error) {
					resultBufferMaxZ = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMaxZ = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}
			}
			if ( computeMinimum )
			{
				try {
					ZProjectionImageMin.importData(iSlice*lengthZ*colorFactor, resultBufferMinZ[iSlice], false);
				} catch (IOException error) {
					resultBufferMinZ = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMinZ = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}			
			}
		}

		if ( ZProjectionImageMax != null )
		{
			ZProjectionImageMax.calcMinMax();
		}
		if ( ZProjectionImageMin != null )
		{
			ZProjectionImageMin.calcMinMax();
		}
		mod = totalLength;
		fireProgressStateChanged(Math.round((mod / totalLength) * 100));
	}


	private void calcYProjection() {

		int dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
		int dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
		int dimZ = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;

		double [] buffer;
		try {
			int length = srcImage.getSize();
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

		int startY = Math.max( 0, startSlice );
		int stopY = Math.min( dimY, stopSlice + 1 );

		int windowSize = window;
		int newSlices = (stopY - startY) - windowSize + 1;
		ZExtents = ( newSlices == 1 ) ? new int[]{dimX,dimZ} : new int[]{dimX,dimZ,newSlices};		

		float [] YRes = ( newSlices == 1 ) ? new float[] {imResolutions[0], imResolutions[2] } :
			new float[] {imResolutions[0], imResolutions[2], imResolutions[1] };

		int lengthY = dimZ * dimX; // No. of pixels Y projection image
		double[][] resultBufferMax = new double[newSlices][lengthY * colorFactor]; 
		double[][] resultBufferMin = new double[newSlices][lengthY * colorFactor];
		
		double[] maxIntensityValue = new double[colorFactor];
		double[] minIntensityValue = new double[colorFactor];
		for ( int c = 0; c < colorFactor; c++ )
		{
			maxIntensityValue[c] = Double.MIN_VALUE;
			minIntensityValue[c] = Double.MAX_VALUE;
		}
		
		ModelImage YProjectionImageMax = null;
		ModelImage YProjectionImageMin = null;
		if ( computeMaximum )
		{
			YProjectionImageMax = new ModelImage(srcImage.getType(), ZExtents,
					srcImage.getImageName() + "_YProjectionMax" );
			resultImages.add(YProjectionImageMax);
			YProjectionImageMax.setResolutions(YRes);
		}
		if ( computeMinimum )
		{
			YProjectionImageMin = new ModelImage(srcImage.getType(), ZExtents,
					srcImage.getImageName() + "_YProjectionMin" );
			resultImages.add(YProjectionImageMin);
			YProjectionImageMin.setResolutions(YRes);
		}			

		int index;
		for ( int iSlice = 0; iSlice < newSlices; iSlice++ )
		{
			for (int i = 0; i < dimZ; i++)
			{
				for (int j = 0; j < dimX; j++)
				{
					for (int k = startY + iSlice; k < startY + iSlice + windowSize; k++)
					{
						index = (dimX*dimY*i) + j + (dimX*k);
						index *= colorFactor;
						for ( int c = 0; c < colorFactor; c++ )
						{
							if ((buffer[index + c] >= minIntensity[c]) &&
									(buffer[index + c] <= maxIntensity[c]))
							{
								if (buffer[index + c] > maxIntensityValue[c])
								{
									maxIntensityValue[c] = buffer[index + c];
								}
								if (buffer[index + c] < minIntensityValue[c]) 
								{
									minIntensityValue[c] = buffer[index + c];
								}
							}
						}
					}
					for ( int c = 0; c < colorFactor; c++ )
					{						
						resultBufferMax[iSlice][colorFactor * (j + i*dimX) + c] = maxIntensityValue[c];
						maxIntensityValue[c] = Double.MIN_VALUE;
						resultBufferMin[iSlice][colorFactor * (j + i*dimX) + c] = minIntensityValue[c];
						minIntensityValue[c] = Double.MAX_VALUE;
					}
				} 			
			}
			// Reconstruct Z Projection image from result buffer
			if ( computeMaximum )
			{
				try {
					YProjectionImageMax.importData(iSlice*lengthY*colorFactor, resultBufferMax[iSlice], false);
				} catch (IOException error) {
					resultBufferMax = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMax = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}
			}
			if ( computeMinimum )
			{
				try {
					YProjectionImageMin.importData(iSlice*lengthY*colorFactor, resultBufferMin[iSlice], false);
				} catch (IOException error) {
					resultBufferMin = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMin = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}			
			}
		}

		if ( YProjectionImageMax != null )
		{
			YProjectionImageMax.calcMinMax();
		}
		if ( YProjectionImageMin != null )
		{
			YProjectionImageMin.calcMinMax();
		}
		mod = totalLength;
		fireProgressStateChanged(Math.round((mod / totalLength) * 100));
	}


	private void calcXProjection() {

		int dimX = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
		int dimY = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
		int dimZ = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;

		double [] buffer;
		try {
			int length = srcImage.getSize();
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

		int startX = Math.max( 0, startSlice );
		int stopX = Math.min( dimX, stopSlice + 1 );

		int windowSize = window;
		int newSlices = (stopX - startX) - windowSize + 1;
		ZExtents = ( newSlices == 1 ) ? new int[]{dimZ,dimY} : new int[]{dimZ,dimY,newSlices};		

		float [] XRes = ( newSlices == 1 ) ? new float[] {imResolutions[2], imResolutions[1] } :
			new float[] {imResolutions[2], imResolutions[1], imResolutions[0] };

		int lengthX = dimY * dimZ; // No. of pixels X projection image
		double[][] resultBufferMax = new double[newSlices][lengthX * colorFactor]; 
		double[][] resultBufferMin = new double[newSlices][lengthX * colorFactor];
		
		double[] maxIntensityValue = new double[colorFactor];
		double[] minIntensityValue = new double[colorFactor];
		for ( int c = 0; c < colorFactor; c++ )
		{
			maxIntensityValue[c] = Double.MIN_VALUE;
			minIntensityValue[c] = Double.MAX_VALUE;
		}

		ModelImage XProjectionImageMax = null;
		ModelImage XProjectionImageMin = null;
		if ( computeMaximum )
		{
			XProjectionImageMax = new ModelImage(srcImage.getType(), ZExtents,
					srcImage.getImageName() + "_XProjectionMax" );
			resultImages.add(XProjectionImageMax);
			XProjectionImageMax.setResolutions(XRes);
		}
		if ( computeMinimum )
		{
			XProjectionImageMin = new ModelImage(srcImage.getType(), ZExtents,
					srcImage.getImageName() + "_XProjectionMin" );
			resultImages.add(XProjectionImageMin);
			XProjectionImageMin.setResolutions(XRes);
		}			
		int index;
		for ( int iSlice = 0; iSlice < newSlices; iSlice++ )
		{
			for (int i = 0; i < dimZ; i++)
			{
				for (int j = 0; j < dimY; j++)
				{
					for (int k = startX + iSlice; k < startX + iSlice + windowSize; k++)
					{
						index = (i*dimY*dimX) + (dimX*j) + k;
						index *= colorFactor;
						for ( int c = 0; c < colorFactor; c++ )
						{
							if ((buffer[index+c] >= minIntensity[c]) &&
									(buffer[index+c] <= maxIntensity[c]))
							{
								if (buffer[index+c] > maxIntensityValue[c])
								{
									maxIntensityValue[c] = buffer[index+c];
								}

								if (buffer[index+c] < minIntensityValue[c])
								{
									minIntensityValue[c] = buffer[index+c];
								}
							}
						}
					}
					for ( int c = 0; c < colorFactor; c++ )
					{
						resultBufferMax[iSlice][colorFactor * (i + (j*dimZ)) + c] = maxIntensityValue[c];
						maxIntensityValue[c] = Double.MIN_VALUE;
						resultBufferMin[iSlice][colorFactor * (i + (j*dimZ)) + c] = minIntensityValue[c];
						minIntensityValue[c] = Double.MAX_VALUE;
					}
				}
			}
			// Reconstruct Z Projection image from result buffer
			if ( computeMaximum )
			{
				try {
					XProjectionImageMax.importData(iSlice*lengthX, resultBufferMax[iSlice], false);
				} catch (IOException error) {
					resultBufferMax = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMax = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}
			}
			if ( computeMinimum )
			{
				try {
					XProjectionImageMin.importData(iSlice*lengthX, resultBufferMin[iSlice], false);
				} catch (IOException error) {
					resultBufferMin = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Image(s) Locked", true);
					return;
				} catch (OutOfMemoryError e) {
					resultBufferMin = null;
					errorCleanUp("Algorithm Maximum Intensity Projection: Out of Memory", true);
					return;
				}			
			}
		}

		if ( XProjectionImageMax != null )
		{
			XProjectionImageMax.calcMinMax();
		}
		if ( XProjectionImageMin != null )
		{
			XProjectionImageMin.calcMinMax();
		}
		mod = totalLength;
		fireProgressStateChanged(Math.round((mod / totalLength) * 100));
	}



	/**
	 * This method returns the projection images in an arraylist
	 */
	public Vector<ModelImage> getResultImage() {
		return resultImages;

	}
}







