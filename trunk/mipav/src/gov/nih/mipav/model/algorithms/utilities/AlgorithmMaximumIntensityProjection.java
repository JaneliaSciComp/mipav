package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.*;
import java.util.*;


/**
 * Computes the maximum or the minimum intensity along each projection of a 3D image. 
 * The user can specify theshold values in image intensity for the calculation, as well as
 * which projection to compute along (X,Y, or Z), the start and end slice used in the calculation,
 * the size of a sliding 'window' in number of slices to use in the calculation, and whether to calculate the minimum,
 * maximum or both projections.
 * 
 * When the sliding window size is not equal to the number of slices used in the calculation the output is a 3D image,
 * where each slice of the output image is computed using the number of slices in the sliding window. When the
 * sliding window size is equal to the number of slices used in the calculation the output image is a 2D image.
 * @author joshim2
 *
 */
public class AlgorithmMaximumIntensityProjection extends AlgorithmBase {

	//	~ Instance fields ------------------------------------------------------------------------------------------------
	/** Array of Result Images */
	private Vector<ModelImage> resultImages;

	/** Source Image Resolutions */
	float [] imResolutions;

	/** The first slice in the intensity projection calculation: */
	private int startSlice;
	/** The last slice in the intensity projection calculation: */
	private int stopSlice;
	/** The number of slices used in the intensity projection, represents a sliding window. 
	 * When this number is not the difference between the stop and start slices, the output image is a 3D 
	 * that is a collection of projections. Otherwise the output image is a single 2D projection. */
	private int window;

	/** Minimum intensity threshold value. */
	private double[] minIntensity;

	/** Maximum intensity threshold value. */
	private double[] maxIntensity;

	/** When true, computes the maximum intensity projection. */
	private boolean computeMaximum;
	/** When true, computes the minimum intensity projection. */
	private boolean computeMinimum;
	/** Project along the X-Axis. */
	public static final int X_PROJECTION = 0;
	/** Project along the Y-Axis. */
	public static final int Y_PROJECTION = 1;
	/** Project along the Z-Axis. */
	public static final int Z_PROJECTION = 2;
	/** Default Projection is along the z-Axis. */
	private int projectionDirection = Z_PROJECTION;
	/** When the image is a color image, the colorFactor is set to 4, otherwise it defaults to 1. */
	private int colorFactor = 1;

	//	~ Constructors ---------------------------------------------------------------------------------------------------

	/**
	 * Estimates the maximum intensity projection in each direction of a 3D black and white image
	 * @param srcImg source image
	 * @param _startSlice the first slice used in the projection calculation.
	 * @param _stopSlice the last slice used in the projection calculation.
	 * @param _window the number of slices to use in the projection. 
	 * @param _minIntensity minimum intensity threshold.
	 * @param _maxIntensity maximum intensity threshold.
	 * @param _computeMaximum when true compute the maximum intensity projection.
	 * @param _computeMinimum when true compute the minimum intensity projection.
	 * @param _projectionDirection image axis to project along (X, Y, or Z).
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
	 * Estimates the maximum intensity projection of a 3D color image
	 * @param srcImg source image
	 * @param _startSlice the first slice used in the projection calculation.
	 * @param _stopSlice the last slice used in the projection calculation.
	 * @param _window the number of slices to use in the projection.
	 * @param  _minR minimum Red intensity threshold.
	 * @param  _maxR maximum Red intensity threshold.
	 * @param  _minG minimum Green intensity threshold.
	 * @param  _maxG maximum Green intensity threshold.
	 * @param  _minB minimum Blue intensity threshold.
	 * @param  _maxB maximum Blue intensity threshold.
	 * @param _computeMaximum when true compute the maximum intensity projection.
	 * @param _computeMinimum when true compute the minimum intensity projection.
	 * @param _projectionDirection image axis to project along (X, Y, or Z).
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
	@Override
	public void finalize() {
		minIntensity = null;
		maxIntensity = null;
		super.finalize();
	}

	/**
	 * Runs the Intensity Projection algorithm.
	 */
	@Override
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

	/**
	 * Calculates the Z Projection for color or black and white images.
	 */
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

		int lengthZ = dimX * dimY; // No. of pixels Z projection image
		int startZ = Math.max( 0, startSlice );
		int stopZ = Math.min( dimZ, stopSlice + 1 );

		// For maximum intensity along Z-axis
		int windowSize = window;
		int newSlices = (stopZ - startZ) - windowSize + 1;
		int[] extents = ( newSlices == 1 ) ? new int[]{dimX,dimY} : new int[]{dimX,dimY,newSlices};		
				
		double[] maxIntensityValue = new double[colorFactor];
		double[] minIntensityValue = new double[colorFactor];
		for ( int c = 0; c < colorFactor; c++ )
		{
			maxIntensityValue[c] = -Double.MAX_VALUE;
			minIntensityValue[c] = Double.MAX_VALUE;
		}
		
		ModelImage ZProjectionImageMax = null;
		ModelImage ZProjectionImageMin = null;
		if ( computeMaximum )
		{
			ZProjectionImageMax = new ModelImage( srcImage.getType(), extents, 
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
			ZProjectionImageMin = new ModelImage(srcImage.getType(), extents,
					srcImage.getImageName() + "_ZProjectionMin" );
			resultImages.add(ZProjectionImageMin);
	        JDialogBase.updateFileInfo( srcImage, ZProjectionImageMin );
		}
		
		float totalLength = (newSlices*lengthZ);
		int updates = (int)(totalLength/10f);
		int mod = 0;
		fireProgressStateChanged(mod, srcImage.getImageName(), "Computing Maximum Intensity Projection ...");
		int index;
		for ( int iSlice = 0; iSlice < newSlices; iSlice++ )
		{
			for (int i = 0; i < lengthZ; i++ )
			{
				int start = startZ + iSlice;
				int end = startZ + iSlice + windowSize;
				// Compute the max intensity value along Z-axis
				for (int j = start; j < end; j++)
				{
					for ( int c = 0; c < colorFactor; c++ )
					{
						index = colorFactor * (i + (j * lengthZ)) + c;
						if ((buffer[index] >= minIntensity[c]) &&
								(buffer[index] <= maxIntensity[c]))
						{
							if (buffer[index] > maxIntensityValue[c])
							{
								maxIntensityValue[c] = buffer[index];
							}
							if ( buffer[index] < minIntensityValue[c] )
							{
								minIntensityValue[c] = buffer[index];
							}
						}
					}
				}
				for ( int c = 0; c < colorFactor; c++ )
				{
					index = colorFactor * iSlice * lengthZ + i*colorFactor + c;
					if ( computeMaximum && ZProjectionImageMax != null )
					{
						ZProjectionImageMax.set( index, maxIntensityValue[c] );
					}
					maxIntensityValue[c] = -Double.MAX_VALUE;
					if ( computeMinimum && ZProjectionImageMin != null )
					{
						ZProjectionImageMin.set( index, minIntensityValue[c] );
					}
					minIntensityValue[c] = Double.MAX_VALUE;
				}
				mod++;
				if ( (mod%updates) == 0 )
				{
					fireProgressStateChanged(Math.round((mod / totalLength) * 100));
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
		fireProgressStateChanged(100);
	}

	/**
	 * Calculates the Y Projection for color or black and white images.
	 */
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

		int startY = Math.max( 0, startSlice );
		int stopY = Math.min( dimY, stopSlice + 1 );

		int windowSize = window;
		int newSlices = (stopY - startY) - windowSize + 1;
		int[] extents = ( newSlices == 1 ) ? new int[]{dimX,dimZ} : new int[]{dimX,dimZ,newSlices};		

		float [] YRes = ( newSlices == 1 ) ? new float[] {imResolutions[0], imResolutions[2] } :
			new float[] {imResolutions[0], imResolutions[2], imResolutions[1] };

		int lengthY = dimZ * dimX; // No. of pixels Y projection image
		
		double[] maxIntensityValue = new double[colorFactor];
		double[] minIntensityValue = new double[colorFactor];
		for ( int c = 0; c < colorFactor; c++ )
		{
			maxIntensityValue[c] = -Double.MAX_VALUE;
			minIntensityValue[c] = Double.MAX_VALUE;
		}
		
		ModelImage YProjectionImageMax = null;
		ModelImage YProjectionImageMin = null;
		if ( computeMaximum )
		{
			YProjectionImageMax = new ModelImage(srcImage.getType(), extents,
					srcImage.getImageName() + "_YProjectionMax" );
			resultImages.add(YProjectionImageMax);
			YProjectionImageMax.setResolutions(YRes);
		}
		if ( computeMinimum )
		{
			YProjectionImageMin = new ModelImage(srcImage.getType(), extents,
					srcImage.getImageName() + "_YProjectionMin" );
			resultImages.add(YProjectionImageMin);
			YProjectionImageMin.setResolutions(YRes);
		}			

		float totalLength = (newSlices*dimZ*dimX);
		int update = (int)(totalLength/10);
		int mod = 0;
		fireProgressStateChanged(mod, srcImage.getImageName(), "Computing Maximum Intensity Projection ...");
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
						if ( computeMaximum && YProjectionImageMax != null )
						{
							YProjectionImageMax.set( iSlice * lengthY * colorFactor + colorFactor * (j + i*dimX) + c, 
									maxIntensityValue[c] );
						}
						maxIntensityValue[c] = -Double.MAX_VALUE;			
						if ( computeMinimum && YProjectionImageMin != null )
						{
							YProjectionImageMin.set( iSlice * lengthY * colorFactor + colorFactor * (j + i*dimX) + c, 
									minIntensityValue[c] );
						}
						minIntensityValue[c] = Double.MAX_VALUE;
					}
					mod++;
					if ( (mod%update) == 0 )
					{
						fireProgressStateChanged(Math.round((mod / totalLength) * 100));
					}
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
		fireProgressStateChanged(100);
	}

	/**
	 * Calculates the X Projection for color or black and white images.
	 */
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

		int startX = Math.max( 0, startSlice );
		int stopX = Math.min( dimX, stopSlice + 1 );

		int windowSize = window;
		int newSlices = (stopX - startX) - windowSize + 1;
		int[] extents = ( newSlices == 1 ) ? new int[]{dimZ,dimY} : new int[]{dimZ,dimY,newSlices};		

		float [] XRes = ( newSlices == 1 ) ? new float[] {imResolutions[2], imResolutions[1] } :
			new float[] {imResolutions[2], imResolutions[1], imResolutions[0] };

		int lengthX = dimY * dimZ; // No. of pixels X projection image
		
		double[] maxIntensityValue = new double[colorFactor];
		double[] minIntensityValue = new double[colorFactor];
		for ( int c = 0; c < colorFactor; c++ )
		{
			maxIntensityValue[c] = -Double.MAX_VALUE;
			minIntensityValue[c] = Double.MAX_VALUE;
		}

		ModelImage XProjectionImageMax = null;
		ModelImage XProjectionImageMin = null;
		if ( computeMaximum )
		{
			XProjectionImageMax = new ModelImage(srcImage.getType(), extents,
					srcImage.getImageName() + "_XProjectionMax" );
			resultImages.add(XProjectionImageMax);
			XProjectionImageMax.setResolutions(XRes);
		}
		if ( computeMinimum )
		{
			XProjectionImageMin = new ModelImage(srcImage.getType(), extents,
					srcImage.getImageName() + "_XProjectionMin" );
			resultImages.add(XProjectionImageMin);
			XProjectionImageMin.setResolutions(XRes);
		}			

		float totalLength = (newSlices*dimZ*dimY);
		int update = (int)(totalLength/10);
		int mod = 0;
		fireProgressStateChanged(mod, srcImage.getImageName(), "Computing Maximum Intensity Projection ...");
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
						if ( computeMaximum && XProjectionImageMax != null )
						{
							XProjectionImageMax.set( iSlice*lengthX*colorFactor + colorFactor * (i + (j*dimZ)) + c, maxIntensityValue[c] );
						}
						maxIntensityValue[c] = -Double.MAX_VALUE;
						if ( computeMinimum && XProjectionImageMin != null )
						{
							XProjectionImageMin.set( iSlice*lengthX*colorFactor + colorFactor * (i + (j*dimZ)) + c, minIntensityValue[c] );
						}
						minIntensityValue[c] = Double.MAX_VALUE;
					}
					mod++;
					if ( (mod%update) == 0 )
					{
						fireProgressStateChanged(Math.round((mod / totalLength) * 100));
					}
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
		fireProgressStateChanged(100);
	}



	/**
	 * This method returns the projection images in a Vector.
	 */
	public Vector<ModelImage> getResultImage() {
		return resultImages;

	}
}







