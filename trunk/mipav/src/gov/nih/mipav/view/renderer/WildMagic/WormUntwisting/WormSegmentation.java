package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmEdgeLaplacianSep;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.PointStack;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeBuilder;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormData;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphCurve;
import gov.nih.mipav.view.renderer.flythroughview.FlyPathGraphSamples;
import gov.nih.mipav.view.renderer.flythroughview.ModelImage3DLayout;
import gov.nih.mipav.view.renderer.flythroughview.Skeleton3D;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Curves.Curve3f;
import WildMagic.LibFoundation.Intersection.IntrSphere3Sphere3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Sphere3f;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.BoxBV;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * Base class for implementing various strategies for automatic segmentation of the seam cells.
 */
public class WormSegmentation
{
	/**
	 * Returns a blurred image of the input image.
	 * 
	 * @param image
	 * @param sigma
	 * @return
	 */
	public static ModelImage blur(final ModelImage image, final int sigma) {
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_gblur";

		float[] sigmas = new float[] {sigma, sigma};
		if ( image.getNDims() == 3 )
		{
			sigmas = new float[] {sigma, sigma, sigma * getCorrectionFactor(image)};
		}
		OpenCLAlgorithmGaussianBlur blurAlgo;

		final ModelImage resultImage = new ModelImage(image.getType(), image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, resultImage);
		blurAlgo = new OpenCLAlgorithmGaussianBlur(resultImage, image, sigmas, true, true, false);

		blurAlgo.setRed(true);
		blurAlgo.setGreen(true);
		blurAlgo.setBlue(true);
		blurAlgo.run();

		return blurAlgo.getDestImage();
	}

//	public static HashMap<Float, Integer> estimateHistogram( final ModelImage image, float targetPercent, float[] targetValue )
//	{    	
//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;  	
//
//		// Calculate the histogram:
//		int maxCount = 0;
//		HashMap<Float, Integer> map = new HashMap<Float, Integer>();
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					float value = image.getFloat(x,y,z);
//					int count = 0;
//					if ( map.containsKey(value) )
//					{
//						count = map.get(value);
//					}
//					count++;
//					map.put( value, count );
//					if ( count > maxCount )
//					{
//						maxCount = count;
//					}
//				}
//			}
//		}
//		//    	System.err.println( map.get((float)image.getMin() ) + " " + map.get((float)image.getMax() ) );
//		// Sort the Histogram bins:
//		Set<Float> keySet = map.keySet();
//		Iterator<Float> keyIterator = keySet.iterator();
//		float[] keyArray = new float[keySet.size()];
//		int count = 0;
//		while ( keyIterator.hasNext() )
//		{
//			float value = keyIterator.next();
//			keyArray[count++] = value;
//		}
//
//		Arrays.sort(keyArray);
//		HashMap<Float, Integer> countValues = new HashMap<Float, Integer>();
//		int runningCount = 0;
//		float target = targetPercent * dimX*dimY*dimZ;
//		boolean found = false;
//		for ( int i = 0; i < keyArray.length; i++ )
//		{
//			count = map.get( keyArray[i] );
//			runningCount += count;
//			countValues.put( keyArray[i], runningCount );
//			if ( (runningCount >= target) && !found )
//			{
//				found = true;
//				targetValue[0] = keyArray[i];
//			}
//		}
//		map = null;
//		keyArray = null;
//		return countValues;
//	}


	private static float[] estimateHistogram( final ModelImage image, float[] targetValues, float percentage )
	{    	
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;  	

		// Calculate the histogram:
		int maxCount = 0;
		HashMap<Float, Integer> map = new HashMap<Float, Integer>();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					float value = -1;
					if ( image.isColorImage() )
					{
						value = image.getFloatC(x, y, z, 2); // seam cells are in green component
					}
					else
					{
						value = image.getFloat(x,y,z);
					}
					int count = 0;
					if ( map.containsKey(value) )
					{
						count = map.get(value);
					}
					count++;
					map.put( value, count );
					if ( count > maxCount )
					{
						maxCount = count;
					}
				}
			}
		}
		//    	System.err.println( map.get((float)image.getMin() ) + " " + map.get((float)image.getMax() ) );
		// Sort the Histogram bins:
		Set<Float> keySet = map.keySet();
		Iterator<Float> keyIterator = keySet.iterator();
		float[] keyArray = new float[keySet.size()];
		int count = 0;
		while ( keyIterator.hasNext() )
		{
			float value = keyIterator.next();
			keyArray[count++] = value;
		}

		Arrays.sort(keyArray);
		HashMap<Float, Integer> countValues = new HashMap<Float, Integer>();
		int runningCount = 0;
		float target = percentage * dimX*dimY*dimZ;
		float total = dimX*dimY*dimZ;
		boolean found = false;
		for ( int i = 0; i < keyArray.length; i++ )
		{
			count = map.get( keyArray[i] );
			runningCount += count;
			countValues.put( keyArray[i], runningCount );
			if ( (runningCount >= target) && !found )
			{
				found = true;
				if ( i > 0 )
				{
					targetValues[0] = keyArray[i-1];
					targetValues[1] = keyArray[i];
				}
				else
				{
					targetValues[0] = keyArray[i];
					targetValues[1] = keyArray[i];
				}
			}
			//    		System.err.println( i + " " + keyArray[i] + " " + runningCount/total );
		}
		//    	System.err.println( count_95P + " " + totalCount );
		//    	for ( int i = 0; i < countValues.length; i++ )
		//    	{
		//    		System.err.println( countValues[i].X + " " + countValues[i].Y );
		//    	}
		map = null;
		//		keyArray = null;
		countValues = null;
		return keyArray;
	}

	// need edges
	public static int fill(final ModelImage image, float cutOffMin, float cutOffMax, final Vector<Vector3f> seedList, ModelImage visited, final int id) {
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		int count = 0;
		while (seedList.size() > 0) {
			final Vector3f seed = seedList.remove(0);

			final int z = Math.round(seed.Z);
			final int y = Math.round(seed.Y);
			final int x = Math.round(seed.X);
			int index = z*dimY*dimX + y*dimX + x;
			if ( visited.getInt(index) != 0 )
			{
				continue;
			}
			visited.set(index, id);
			count++;

			for (int z1 = Math.max(0, z - 1); z1 <= Math.min(dimZ - 1, z + 1); z1++)
			{
				for (int y1 = Math.max(0, y - 1); y1 <= Math.min(dimY - 1, y + 1); y1++)
				{
					for (int x1 = Math.max(0, x - 1); x1 <= Math.min(dimX - 1, x + 1); x1++)
					{
						if ( ! ( (x == x1) && (y == y1) && (z == z1))) {
							index = z1*dimY*dimX + y1*dimX + x1;
							if ( visited.getInt(index) == 0 )
							{
								float value = image.getFloat(x1, y1, z1);
								if ( (value >= cutOffMin) && (value < cutOffMax) )
								{
									seedList.add( new Vector3f(x1,y1,z1) );
								}
							}
						}
					}
				}
			}							
		}
		return count;
	}


	public static int fill(final ModelImage image, float cutOffMin, float cutOffMax, final Vector<Vector3f> seedList, BitSet visited, ModelImage mask) {
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		int count = 0;
		while (seedList.size() > 0) {
			final Vector3f seed = seedList.remove(0);

			final int z = Math.round(seed.Z);
			final int y = Math.round(seed.Y);
			final int x = Math.round(seed.X);
			int index = z*dimY*dimX + y*dimX + x;			
			
			if ( visited.get(index) )
			{
				continue;
			}
			visited.set(index);
			
			float value = image.getFloat(x, y, z);
			if ( (value >= cutOffMin) && (value < cutOffMax) )
			{
				mask.setC(x, y, z, 1, value);
			}
			
			count++;

			for (int z1 = Math.max(0, z - 1); z1 <= Math.min(dimZ - 1, z + 1); z1++)
			{
				for (int y1 = Math.max(0, y - 1); y1 <= Math.min(dimY - 1, y + 1); y1++)
				{
					for (int x1 = Math.max(0, x - 1); x1 <= Math.min(dimX - 1, x + 1); x1++)
					{
						if ( ! ( (x == x1) && (y == y1) && (z == z1))) {
							index = z1*dimY*dimX + y1*dimX + x1;
							if ( !visited.get(index) )
							{
								value = image.getFloat(x1, y1, z1);
								if ( (value >= cutOffMin) && (value < cutOffMax) )
								{
									seedList.add( new Vector3f(x1,y1,z1) );
								}
							}
						}
					}
				}
			}							
		}
		return count;
	}



//	public static Vector<Vector3f> findMaxPeaks( ModelImage image, float minPeakVal )
//	{
//		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
//
//		Vector<Vector3f> peakPositions = new Vector<Vector3f>();
//
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				double maxRow = -Float.MAX_VALUE;
//				int maxRowIndex = -1;
//				for ( int x = 0; x < dimX; x++ )
//				{
//					double val = image.getFloat( x, y, z );
//					if ( val > maxRow )
//					{
//						maxRow = val;
//						maxRowIndex = x;
//					}
//				}
//				if ( maxRow >= minPeakVal )
//				{
//					Vector3f pos = new Vector3f( maxRowIndex, y, z );
//					if ( !peakPositions.contains( pos ) )
//					{
//						peakPositions.add( pos );
//					}
//				}
//			}
//		}
//
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int x = 0; x < dimX; x++ )
//			{
//				double maxColumn = -Float.MAX_VALUE;
//				int maxColumnIndex = -1;
//				for ( int y = 0; y < dimY; y++ )
//				{
//					double val = image.getFloat( x, y, z );
//					if ( val > maxColumn )
//					{
//						maxColumn = val;
//						maxColumnIndex = y;
//					}
//				}
//				if ( maxColumn >= minPeakVal )
//				{
//					Vector3f pos = new Vector3f( x, maxColumnIndex, z );
//					if ( !peakPositions.contains( pos ) )
//					{
//						peakPositions.add( pos );
//					}
//				}
//			}
//		}
//		for ( int x = 0; x < dimX; x++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				double maxDepth = -Float.MAX_VALUE;
//				int maxDepthIndex = -1;
//				for ( int z = 0; z < dimZ; z++ )
//				{
//					double val = image.getFloat( x, y, z );
//					if ( val > maxDepth )
//					{
//						maxDepth = val;
//						maxDepthIndex = z;
//					}
//				}
//				if ( maxDepth >= minPeakVal )
//				{
//					Vector3f pos = new Vector3f( x, y, maxDepthIndex );
//					if ( !peakPositions.contains( pos ) )
//					{
//						peakPositions.add( pos );
//					}
//				}
//			}
//		}
//		for ( int x = 0; x < dimX; x++ )
//		{
//			for ( int z = 0; z < dimZ; z++ )
//			{
//				double maxDepth = -Float.MAX_VALUE;
//				int maxDepthIndex = -1;
//				for ( int y = 0; y < dimY; y++ )
//				{
//					double val = image.getFloat( x, y, z );
//					if ( val > maxDepth )
//					{
//						maxDepth = val;
//						maxDepthIndex = y;
//					}
//				}
//				if ( maxDepth >= minPeakVal )
//				{
//					Vector3f pos = new Vector3f( x, maxDepthIndex, z );
//					if ( !peakPositions.contains( pos ) )
//					{
//						peakPositions.add( pos );
//					}
//				}
//			}
//		}
//		for ( int y = 0; y < dimY; y++ )
//		{
//			for ( int x = 0; x < dimX; x++ )
//			{
//				double maxDepth = -Float.MAX_VALUE;
//				int maxDepthIndex = -1;
//				for ( int z = 0; z < dimZ; z++ )
//				{
//					double val = image.getFloat( x, y, z );
//					if ( val > maxDepth )
//					{
//						maxDepth = val;
//						maxDepthIndex = z;
//					}
//				}
//				if ( maxDepth >= minPeakVal )
//				{
//					Vector3f pos = new Vector3f( x, y, maxDepthIndex );
//					if ( !peakPositions.contains( pos ) )
//					{
//						peakPositions.add( pos );
//					}
//				}
//			}
//		}
//		for ( int y = 0; y < dimY; y++ )
//		{
//			for ( int z = 0; z < dimZ; z++ )
//			{
//				double maxDepth = -Float.MAX_VALUE;
//				int maxDepthIndex = -1;
//				for ( int x = 0; x < dimX; x++ )
//				{
//					double val = image.getFloat( x, y, z );
//					if ( val > maxDepth )
//					{
//						maxDepth = val;
//						maxDepthIndex = x;
//					}
//				}
//				if ( maxDepth >= minPeakVal )
//				{
//					Vector3f pos = new Vector3f( maxDepthIndex, y, z );
//					if ( !peakPositions.contains( pos ) )
//					{
//						peakPositions.add( pos );
//					}
//				}
//			}
//		}
//
//		return peakPositions;
//	}

	//	public static void segmentImage( final ModelImage image, float cutOff )
	//	{
	//		String imageName = image.getImageName();
	//		if (imageName.contains("_clone")) {
	//			imageName = imageName.replaceAll("_clone", "");
	//		}
	//		if (imageName.contains("_laplace")) {
	//			imageName = imageName.replaceAll("_laplace", "");
	//		}
	//		if (imageName.contains("_gblur")) {
	//			imageName = imageName.replaceAll("_gblur", "");
	//		}
	//		imageName = imageName + "_segmentation";
	//		ModelImage visited = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), imageName);
	//		JDialogBase.updateFileInfo(image, visited);   
	//
	//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
	//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
	//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;  	
	//
	//		Vector<Vector3f> seedList = new Vector<Vector3f>();
	//		int count = 1;
	//		int maxFilled = -1;
	//		int maxID = -1;
	//		Vector3f seed = new Vector3f();
	//		for ( int z = 0; z < dimZ; z++ )
	//		{
	//			for ( int y = 0; y < dimY; y++ )
	//			{
	//				for ( int x = 0; x < dimX; x++ )
	//				{
	//					if ( image.getFloat(x, y, z) >= cutOff )
	//					{
	//						seed.set(x, y, z);
	//						seedList.add(seed);
	//						int numFilled = fill( image, cutOff, seedList, visited, count);
	//						if ( numFilled > 0 )
	//						{
	////							System.err.println(numFilled);
	//							if ( numFilled > maxFilled )
	//							{
	//								maxFilled = numFilled;
	//								maxID = count;
	//							}
	//							count++;
	//						}
	//					}
	//				}
	//			}
	//		}
	//
	//		imageName = imageName + "_segmentation";
	//		ModelImage segmentationImage = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), imageName);
	//		JDialogBase.updateFileInfo(image, segmentationImage);   
	//		for ( int z = 0; z < dimZ; z++ )
	//		{
	//			for ( int y = 0; y < dimY; y++ )
	//			{
	//				for ( int x = 0; x < dimX; x++ )
	//				{
	//					segmentationImage.set(x, y, z, (float)image.getMin() );
	//					int id = visited.getInt(x, y, z);
	//					if ( id == maxID )
	//					{
	//						segmentationImage.set(x, y, z, image.getFloat(x, y, z ) );
	//					}
	//				}
	//			}
	//		}
	//		
	//		visited.disposeLocal();
	//		visited = null;
	//		segmentationImage.calcMinMax();
	//		new ViewJFrameImage(segmentationImage);
	//	}

	public static VOIContour findLargestConnected( ModelImage image, float r, 
			int dimX, int dimY, int dimZ, int min, int max, int ID, Vector3f minEllipse, Vector3f maxEllipse, int width )
	{				
		Vector3f center2D = new Vector3f( dimX/2, dimY/2, 0);
		float[] averages = new float[dimX*dimY];
		int window = Math.max(2, width/2);
		float maxAverage = -1;
		for ( int y = 0; y < dimY; y++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				int index = y*dimX + x;
				averages[index] = 0;
				int count = 0;
				for ( int y1 = Math.max(0, y - window); y1 < Math.min(dimY,  y + window + 1); y1++ )
				{
					for ( int x1 = Math.max(0, x - window); x1 < Math.min(dimX,  x + window + 1); x1++ )
					{
						int indexW = y1*dimX + x1;
						int valueA = image.getInt(indexW * 4 + 0);
						int value = image.getInt(indexW * 4 + 1);
						if ( valueA != 0 )
						{
							averages[index] += value;
							count++;
						}
					}
				}
				if ( count != 0 )
				{
					averages[index] /= (float)count;
					if ( averages[index] > maxAverage )
					{
						maxAverage = averages[index];
					}
				}
				else
				{
					averages[index] = -1;
				}
			}
		}

		for ( int y = 0; y < dimY; y++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				int index = y*dimX + x;
				if ( averages[index] >= 0.9 * maxAverage )
				{
					averages[index] = 0;
					//					image.setC(x, y, 1, (byte) ( (0 & 0x000000ff)) );
					//					image.setC(x, y, 2, (byte) ( (0 & 0x000000ff)) );
					//					image.setC(x, y, 3, (byte) ( (0 & 0x000000ff)) );
				}
				else if ( averages[index] <=  0.85 * maxAverage )
				{
					averages[index] = 0;
					//					image.setC(x, y, 1, (byte) ( (0 & 0x000000ff)) );
					//					image.setC(x, y, 2, (byte) ( (0 & 0x000000ff)) );
					//					image.setC(x, y, 3, (byte) ( (0 & 0x000000ff)) );
				}
				else
				{
					averages[index] = 255;
					//					image.setC(x, y, 1, (byte) ( (255 & 0x000000ff)) );
					//					image.setC(x, y, 2, (byte) ( (255 & 0x000000ff)) );
					//					image.setC(x, y, 3, (byte) ( (255 & 0x000000ff)) );
				}
			}
		}

		float[] temp = new float[dimX*dimY];
		for ( int y = 0; y < dimY; y++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				int index = y*dimX + x;
				temp[index] = 0;
				if ( averages[index] != 0 )
				{
					for ( int y1 = Math.max(0, y-1); y1 < Math.min(dimY, y + 2); y1++ )
					{
						for ( int x1 = Math.max(0, x-1); x1 < Math.min(dimX, x + 2); x1++ )
						{
							index = y1*dimX + x1;
							temp[index] = 255;
						}
					}
				}
			}
		}

		float diameter = (float) (Math.PI * 2 * r);

		Vector<BitSet> components = new Vector<BitSet>();
		BitSet visited = new BitSet(dimY*dimX);
		for ( int y = 0; y < dimY; y++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				int index = y*dimX + x;
				float value = temp[index];
				if ( value == 255 )
				{
					if ( !visited.get(index) )
					{
						BitSet filled = new BitSet(dimZ*dimY*dimX);
						Vector<Vector3f> seeds = new Vector<Vector3f>();
						seeds.add( new Vector3f(x, y, 0) );

						Vector<Vector3f> insidePts = new Vector<Vector3f>();
						Vector3f minV = new Vector3f( Float.MAX_VALUE, Float.MAX_VALUE, 0 );
						Vector3f maxV = new Vector3f(-Float.MAX_VALUE,-Float.MAX_VALUE, 0 );

						fillMask( temp, visited, filled, seeds, dimX, dimY, dimZ, true, insidePts, minV, maxV );

						if ( (center2D.X > minV.X) && (center2D.Y > minV.Y) && (center2D.X < maxV.X) && (center2D.Y < maxV.Y) )
						{
							if ( filled.cardinality() > (diameter * 0.3) )
							{
								float dist = maxV.distance(minV);
								if ( dist > r )
								{
									Box3f orientedBox = ContBox3f.ContOrientedBox(insidePts.size(), insidePts);
									if ( ContBox3f.InBox( center2D, orientedBox) )
									{
										components.add(filled);
									}
									orientedBox.dispose();
									orientedBox = null;
								}
							}
						}
						insidePts.clear();
					}
				}
			}
		}


		Vector3f pt = new Vector3f();
		visited.clear();
		for ( int y = 0; y < dimY; y++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				//				pt.set(x, y, 0);
				int index = y*dimX + x;

				temp[index] = 0;

				for ( int i = 0; i < components.size(); i++ )
				{
					if ( components.elementAt(i).get(index) )
					{
						temp[index] = 255;
						visited.set(index);
						break;
					}
				}
			}
		}

		BitSet filled = new BitSet(dimZ*dimY*dimX);
		Vector<Vector3f> seeds = new Vector<Vector3f>();
		seeds.add( center2D );

		//		long time = System.currentTimeMillis();

		VOIContour ellipseAll = new VOIContour(false);
		fillMask( temp, visited, filled, seeds, ellipseAll, dimX, dimY, dimZ, width );

		//		System.err.println( "fillMask " + AlgorithmBase.computeElapsedTime(time) );
		//		time = System.currentTimeMillis();
		//		for ( int y = 0; y < dimY; y++ )
		//		{
		//			for ( int x = 0; x < dimX; x++ )
		//			{
		//				int index = y*dimX + x;
		//				if ( filled.get(index) )
		//				{
		//					ellipseAll.add(new Vector3f(x, y, 0) );				
		//				}
		//			}
		//		}
		minEllipse.set(  Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE);
		maxEllipse.set( -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE);
		if ( ellipseAll.size() < 3 )
		{
			VOIContour ellipseDefault = new VOIContour(true);
			final int numPts = 360;
			center2D = new Vector3f( dimX/2, dimY/2, 0);
			for (int i = 0; i < numPts; i++) {
				final double c = Math.cos(Math.PI * 2.0 * i / numPts);
				final double s = Math.sin(Math.PI * 2.0 * i / numPts);
				final Vector3f pos1 = Vector3f.scale((float) (r * c), Vector3f.UNIT_X);
				final Vector3f pos2 = Vector3f.scale((float) (r * s), Vector3f.UNIT_Y);
				final Vector3f pos = Vector3f.add(pos1, pos2);
				pos.add(center2D);
				ellipseDefault.addElement(pos);
				minEllipse.min(pos);
				maxEllipse.max(pos);
			}
			System.err.println("default circle 1 " + ID + " " + r + " " + width + " " + window );
			return ellipseDefault;
		}
		ellipseAll.convexHull();
		if ( ellipseAll.size() < 3 )
		{
			VOIContour ellipseDefault = new VOIContour(true);
			final int numPts = 360;
			center2D = new Vector3f( dimX/2, dimY/2, 0);
			for (int i = 0; i < numPts; i++) {
				final double c = Math.cos(Math.PI * 2.0 * i / numPts);
				final double s = Math.sin(Math.PI * 2.0 * i / numPts);
				final Vector3f pos1 = Vector3f.scale((float) (r * c), Vector3f.UNIT_X);
				final Vector3f pos2 = Vector3f.scale((float) (r * s), Vector3f.UNIT_Y);
				final Vector3f pos = Vector3f.add(pos1, pos2);
				pos.add(center2D);
				ellipseDefault.addElement(pos);
				minEllipse.min(pos);
				maxEllipse.max(pos);
			}
			System.err.println("default circle 2 " + ID + " " + r );
			return ellipseDefault;
		}
		//		System.err.println( "convexhull " + AlgorithmBase.computeElapsedTime(time) );

		//		time = System.currentTimeMillis();
		Vector3f dir = new Vector3f();
		for ( int i = 0; i < ellipseAll.size(); i++ )
		{
			dir.copy(ellipseAll.elementAt(i));
			dir.sub(center2D);
			float length = dir.normalize();
			length += 10;
			dir.scale(length);
			ellipseAll.elementAt(i).copy(center2D);
			ellipseAll.elementAt(i).add(dir);
			minEllipse.min(ellipseAll.elementAt(i));
			maxEllipse.max(ellipseAll.elementAt(i));
		}
		//		System.err.println( "add margin " + AlgorithmBase.computeElapsedTime(time) );



		if ( true )
			return ellipseAll;


		float rMin = r * 0.5f;
		float rMax = r * 1.5f;
		float rRange = (rMax - rMin);
		float rStep = rRange / 10;
		Vector<VOIContour> ellipseFits = new Vector<VOIContour>();
		Vector<Float> ellipseAreas = new Vector<Float>();
		for ( int cy = -50; cy < 51; cy += 5 )
		{
			for ( int cx = -50; cx < 51; cx += 5 )
			{
				center2D = new Vector3f( cx + dimX/2, cy + dimY/2, 0);
				for ( float rA = rMin; rA < rMax; rA += rStep )
				{
					//					for ( float rB = rMin; rB < rMax; rB += rStep )
					{
						float rB = (r * r) / rA;
						int zAngle = 0;
						//						for ( int zAngle = 0; zAngle < 90; zAngle += 15 )
						{
							VOIContour ellipse = new VOIContour(true);
							Vector3f axisA = new Vector3f( (float)Math.cos( Math.PI * zAngle / 360 ), (float)Math.sin( Math.PI * zAngle / 360 ), 0f );
							Vector3f axisB = new Vector3f( (float)Math.sin( Math.PI * zAngle / 360 ), (float)Math.cos( Math.PI * zAngle / 360 ), 0f );

							final int numPts = 360;
							for (int i = 0; i < numPts; i++) {
								final double c = Math.cos(Math.PI * 2.0 * i / numPts);
								final double s = Math.sin(Math.PI * 2.0 * i / numPts);
								final Vector3f pos1 = Vector3f.scale((float) (rA * c), axisA);
								final Vector3f pos2 = Vector3f.scale((float) (rB * s), axisB);
								final Vector3f pos = Vector3f.add(pos1, pos2);
								pos.add(center2D);
								ellipse.addElement(pos);
							}
							ellipseFits.add(ellipse);
							ellipseAreas.add( new Float(Math.PI * rA * rB) );
						}
					}
				}
			}
		}

		float areaDefault = (float) (Math.PI * r * r);
		Vector2d[] costFits = new Vector2d[ellipseFits.size()];
		for ( int i = 0; i < costFits.length; i++ )
		{
			int fit = (ellipseFits.elementAt(i).size() - ellipseMatch( temp, ellipseFits.elementAt(i), dimX, dimY));// ellipseFits.elementAt(i).size();
			if ( (ellipseAreas.elementAt(i) / areaDefault) < .30 )
			{
				costFits[i] = new Vector2d( Double.MAX_VALUE, i );
			}
			else
			{
				costFits[i] = new Vector2d( fit, i );
			}
		}

		Arrays.sort(costFits);
		VOIContour ellipse = ellipseFits.elementAt((int)costFits[0].Y);

		VOIContour ellipseDefault = new VOIContour(true);
		final int numPts = 360;
		center2D = new Vector3f( dimX/2, dimY/2, 0);
		for (int i = 0; i < numPts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numPts);
			final double s = Math.sin(Math.PI * 2.0 * i / numPts);
			final Vector3f pos1 = Vector3f.scale((float) (r * c), Vector3f.UNIT_X);
			final Vector3f pos2 = Vector3f.scale((float) (r * s), Vector3f.UNIT_Y);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center2D);
			ellipseDefault.addElement(pos);
		}


		//		for ( int i = 0; i < ellipse.size(); i++ )
		//		{
		//			Vector3f dir = Vector3f.sub( ellipse.elementAt(i), center2D );
		//			dir.normalize();
		//			Vector3f pt = ellipse.elementAt(i);
		//			pt.add(dir);
		//			boolean inside = false;
		//			boolean border = false;
		//			boolean outside = false;
		//			while ( (pt.X >= 0) && (pt.X < dimX) && (pt.Y >= 0) && (pt.Y < dimY) )
		//			{
		//				int x = (int) pt.X;
		//				int y = (int) pt.Y;
		//				
		//				int index = y*dimX + x;
		//				int value = image.getInt(index * 4 + 0);
		//				if ( value == 0 )
		//				{
		//					break;
		//				}
		//
		//				if ( averages[index] >= 0.9 * maxAverage )
		//				{
		//					outside = true;
		//					if ( inside && border )
		//					{
		//						break;
		//					}
		//					inside = false;
		//					border = false;
		//					outside = false;
		//				}
		//				else if ( averages[index] <=  0.85 * maxAverage )
		//				{
		//					inside = true;
		//					border = false;
		//					outside = false;
		//				}
		//				else
		//				{
		//					border = true;
		//					outside = false;
		//				}
		//				pt.add(dir);
		//			}
		//		}
		//		
		//		int maxIndex = -1;
		//		int maxSize = -1;
		//		VOIContour[] levelSets = new VOIContour[ellipse.size()];
		//		for ( int i = 0; i < ellipse.size(); i++ )
		//		{
		//			Vector3f pt = ellipse.elementAt(i);
		//			int x = (int) pt.X;
		//			int y = (int) pt.Y;
		//			levelSets[i] = singleLevelSet2( averages, x, y, dimX, dimY );
		//			if ( levelSets[i] != null )
		//			{
		//				if ( levelSets[i].contains( center2D.X, center2D.Y ) )
		//				{
		//					if ( levelSets[i].size() > maxSize )
		//					{
		//						maxSize = levelSets[i].size();
		//						maxIndex = i;
		//					}
		//				}
		//			}
		//		}




		//		ellipse.convexHull();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( ellipse.contains( x, y ) )
					{
						//						int index = z*dimY*dimX + y*dimX + x;
						//						int value = image.getInt(index * 4 + 3);
						image.setC(x, y, 0, (byte) ( (255 & 0x000000ff)) );
						image.setC(x, y, 1, (byte) ( (255 & 0x000000ff)) );
						//						image.setC(x, y, 2, (byte) ( (value & 0x000000ff)) );
						//						image.setC(x, y, 3, (byte) ( (value & 0x000000ff)) );
					}
					if ( ellipseDefault.contains( x, y ) )
					{
						image.setC(x, y, 0, (byte) ( (255 & 0x000000ff)) );
						image.setC(x, y, 3, (byte) ( (255 & 0x000000ff)) );						
					}
					//					else
					//					{
					//						image.setC(x, y, 0, (byte) ( (0 & 0x000000ff)) );
					//						image.setC(x, y, 1, (byte) ( (0 & 0x000000ff)) );
					//						image.setC(x, y, 2, (byte) ( (0 & 0x000000ff)) );
					//						image.setC(x, y, 3, (byte) ( (0 & 0x000000ff)) );						
					//					}
				}
			}
		}
		return ellipse;
	}

	private static int ellipseMatch( float[] data, VOIContour ellipse, int dimX, int dimY )
	{
		int count = 0;
		boolean[] matched = new boolean[data.length];
		for ( int i = 0; i < ellipse.size(); i++ )
		{
			Vector3f pt = ellipse.elementAt(i);
			int x = (int) pt.X;
			int y = (int) pt.Y;

			if ( (pt.X >= 0) && (pt.X < dimX) && (pt.Y >= 0) && (pt.Y < dimY) )
			{
				if ( !matched[y*dimX + x] && (data[y * dimX + x ] != 0) )
				{
					count++;
					matched[y*dimX + x] = true;
				}
			}
		}
		return count;
	}


//	public static void outline( String directory, ModelImage image, ModelImage mp, float threshold, int blurVal, int slice )
//	{
//		int cubeSize = 3;
//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
//
//
//		ModelImage blur = blur(image, blurVal);
//
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					double average = 0;
//					double meanVariance = 0;
//					int count = 0;
//					for ( int z1 = Math.max(0, z-cubeSize); z1 <= Math.min(dimZ-1, z+cubeSize); z1++ )
//					{
//						for ( int y1 = Math.max(0, y-cubeSize); y1 <= Math.min(dimY-1, y+cubeSize); y1++ )
//						{
//							for ( int x1 = Math.max(0, x-cubeSize); x1 <= Math.min(dimX-1, x+cubeSize); x1++ )
//							{
//								float val = blur.getFloat(x1,y1,z1);
//								average += val;
//								count++;
//							}
//						}
//					}
//					average /= (float)count;
//
//
//					for ( int z1 = Math.max(0, z-cubeSize); z1 <= Math.min(dimZ-1, z+cubeSize); z1++ )
//					{
//						for ( int y1 = Math.max(0, y-cubeSize); y1 <= Math.min(dimY-1, y+cubeSize); y1++ )
//						{
//							for ( int x1 = Math.max(0, x-cubeSize); x1 <= Math.min(dimX-1, x+cubeSize); x1++ )
//							{
//								float val = blur.getFloat(x1,y1,z1);
//								meanVariance += ((val - average) * (val - average));
//							}
//						}
//					}
//					meanVariance /= (float)count;
//
//
//					float value = image.getFloat(x,y,z);
//					mp.setC(x, y, z, 3, average);
//					mp.setC(x, y, z, 2, meanVariance);
//					mp.setC(x, y, z, 1, value);
//				}
//			}
//		}
//
//		//		System.err.println( "Saving mp image to : " + directory + mp.getImageName() + "_segmentation_" + ".tif" );
//		//		ModelImage.saveImage( mp, mp.getImageName() + ".tif", directory, false ); 
//		mp.calcMinMax();
//		new ViewJFrameImage((ModelImage) mp);
//	}

//	public static ModelImage outlineA( ModelImage blur, float threshold )
//	{
//		final int dimX = blur.getExtents().length > 0 ? blur.getExtents()[0] : 1;
//		final int dimY = blur.getExtents().length > 1 ? blur.getExtents()[1] : 1;
//		final int dimZ = blur.getExtents().length > 2 ? blur.getExtents()[2] : 1; 
//
//
//
//		BitSet skinMask = new BitSet(dimX*dimY*dimZ);
//
//		int count = 0;
//		Vector<Float> maxVals = new Vector<Float>();
//		Vector<Float> minVals = new Vector<Float>();
//		Vector<Vector3f> maxPts = new Vector<Vector3f>();
//		Vector<Vector3f> maxHighPts = new Vector<Vector3f>();
//		Vector<Vector3f> minPts = new Vector<Vector3f>();
//		for ( int y = 0; y < dimY; y++ )
//		{
//			for ( int x = 0; x < dimX; x++ )
//			{
//				float max = -1;
//				int maxZ = -1;
//				float min = Float.MAX_VALUE;
//				int minZ = -1;
//				boolean up = true;
//				boolean down = false;
//				for ( int z = 0; z < dimZ; z++ )
//				{
//					float value = blur.getFloat(x,y,z);
//					//					if ( value >= threshold )
//					//						continue;
//					//					System.err.println( value );
//					if ( up && (value < max) )
//					{
//						if ( maxZ != -1 )
//						{
//							//							System.err.println( "max = " + max + " " + maxX );
//							maxPts.add( new Vector3f(x, y, maxZ) );
//							maxVals.add(new Float(max));
//							max = -1;
//							maxZ = -1;
//							up = false;
//							down = true;
//						}
//					}
//					if ( down && (value > min) )
//					{
//						if ( minZ != -1 )
//						{
//							//							System.err.println( "min = " + min + " " + minX );
//							minPts.add( new Vector3f(x, y, minZ) );
//							minVals.add(new Float(min));
//							min = Float.MAX_VALUE;
//							minZ = -1;
//							up = true;
//							down = false;
//						}
//					}
//					if ( up && (value > max) )
//					{
//						max = value;
//						maxZ = z;
//					}
//					if ( down && (value < min) )
//					{
//						min = value;
//						minZ = z;
//					}
//				}
//
//				if ( maxPts.size() > 0 )
//				{
//					int zStart1 = (int) maxPts.firstElement().Z;
//					int zStart2 = (int) maxPts.lastElement().Z;
//
//					float maxValue = maxVals.firstElement();
//					for ( int z = zStart1; z <= zStart2; z++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value < maxValue )
//						{
//							break;
//						}
//						skinMask.set(z*dimY*dimX + y*dimX + x);
//					}
//
//					maxValue = maxVals.lastElement();
//					for ( int z = zStart2; z >= zStart1; z-- )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value < maxValue )
//						{
//							break;
//						}
//						skinMask.set(z*dimY*dimX + y*dimX + x);
//					}					
//				}
//
//				maxVals.clear();
//				minVals.clear();
//				maxPts.clear();
//				minPts.clear();
//				maxHighPts.clear();
//			}
//		}
//
//
//
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				float max = -1;
//				int maxX = -1;
//				float min = Float.MAX_VALUE;
//				int minX = -1;
//				boolean up = true;
//				boolean down = false;
//				for ( int x = 0; x < dimX; x++ )
//				{
//					float value = blur.getFloat(x,y,z);
//					//					if ( value >= threshold )
//					//						continue;
//					//					System.err.println( value );
//					if ( up && (value < max) )
//					{
//						if ( maxX != -1 )
//						{
//							//							System.err.println( "max = " + max + " " + maxX );
//							maxPts.add( new Vector3f(maxX, y, z) );
//							maxVals.add(new Float(max));
//							max = -1;
//							maxX = -1;
//							up = false;
//							down = true;
//						}
//					}
//					if ( down && (value > min) )
//					{
//						if ( minX != -1 )
//						{
//							//							System.err.println( "min = " + min + " " + minX );
//							minPts.add( new Vector3f(minX, y, z) );
//							minVals.add(new Float(min));
//							min = Float.MAX_VALUE;
//							minX = -1;
//							up = true;
//							down = false;
//						}
//					}
//					if ( up && (value > max) )
//					{
//						max = value;
//						maxX = x;
//					}
//					if ( down && (value < min) )
//					{
//						min = value;
//						minX = x;
//					}
//				}
//
//				if ( maxPts.size() > 0 )
//				{
//					int xStart1 = (int) maxPts.firstElement().X;
//					int xStart2 = (int) maxPts.lastElement().X;
//
//					float maxValue = maxVals.firstElement();
//					for ( int x = xStart1; x <= xStart2; x++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value < maxValue )
//						{
//							break;
//						}
//						skinMask.set(z*dimY*dimX + y*dimX + x);
//					}
//
//					maxValue = maxVals.lastElement();
//					for ( int x = xStart2; x >= xStart1; x-- )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value < maxValue )
//						{
//							break;
//						}
//						skinMask.set(z*dimY*dimX + y*dimX + x);
//					}					
//				}
//				maxVals.clear();
//				minVals.clear();
//				maxPts.clear();
//				minPts.clear();
//				maxHighPts.clear();
//			}
//		}
//
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int x = 0; x < dimX; x++ )
//			{
//				float max = -1;
//				int maxY = -1;
//				float min = Float.MAX_VALUE;
//				int minY = -1;
//				boolean up = true;
//				boolean down = false;
//				for ( int y = 0; y < dimY; y++ )
//				{
//					float value = blur.getFloat(x,y,z);
//					//					if ( value >= threshold )
//					//						continue;
//					//					System.err.println( value );
//					if ( up && (value < max) )
//					{
//						if ( maxY != -1 )
//						{
//							//							System.err.println( "max = " + max + " " + maxX );
//							maxPts.add( new Vector3f(x, maxY, z) );
//							maxVals.add(new Float(max));
//							max = -1;
//							maxY = -1;
//							up = false;
//							down = true;
//						}
//					}
//					if ( down && (value > min) )
//					{
//						if ( minY != -1 )
//						{
//							//							System.err.println( "min = " + min + " " + minX );
//							minPts.add( new Vector3f(x, minY, z) );
//							minVals.add(new Float(min));
//							min = Float.MAX_VALUE;
//							minY = -1;
//							up = true;
//							down = false;
//						}
//					}
//					if ( up && (value > max) )
//					{
//						max = value;
//						maxY = y;
//					}
//					if ( down && (value < min) )
//					{
//						min = value;
//						minY = y;
//					}
//				}
//
//				if ( maxPts.size() > 0 )
//				{
//					int yStart1 = (int) maxPts.firstElement().Y;
//					int yStart2 = (int) maxPts.lastElement().Y;
//
//					float maxValue = maxVals.firstElement();
//					for ( int y = yStart1; y <= yStart2; y++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value < maxValue )
//						{
//							break;
//						}
//						skinMask.set(z*dimY*dimX + y*dimX + x);
//					}
//
//					maxValue = maxVals.lastElement();
//					for ( int y = yStart2; y >= yStart1; y-- )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value < maxValue )
//						{
//							break;
//						}
//						skinMask.set(z*dimY*dimX + y*dimX + x);
//					}					
//				}
//
//				maxVals.clear();
//				minVals.clear();
//				maxPts.clear();
//				minPts.clear();
//				maxHighPts.clear();
//			}
//		}
//
//
//		Vector<BitSet> components = new Vector<BitSet>();
//		BitSet visited = new BitSet(dimZ*dimY*dimX);
//		for ( int y = 0; y < dimY; y++ )
//		{
//			for ( int x = 0; x < dimX; x++ )
//			{
//				for ( int z = 0; z < dimZ; z++ )
//				{
//					int index = z*dimY*dimX + y*dimX + x;
//					if ( skinMask.get(index) && !visited.get(index) )
//					{
//						BitSet filled = new BitSet(dimZ*dimY*dimX);
//						Vector<Vector3f> seeds = new Vector<Vector3f>();
//						seeds.add( new Vector3f(x, y, z) );
//						fillMask( skinMask, visited, filled, seeds, dimX, dimY, dimZ );
//						if ( filled.cardinality() > 1 )
//						{
//							components.add(filled);
//							//							System.err.println( x + " " + y + " " + z + "    " + filled.cardinality() );
//						}
//					}
//				}
//			}
//		}
//
//		int maxIndex = -1;
//		int max = -1;
//		for ( int i = 0; i < components.size(); i++ )
//		{
//			if ( components.elementAt(i).cardinality() > max )
//			{
//				max = components.elementAt(i).cardinality();
//				maxIndex = i;
//			}
//		}
//
//		BitSet largest = skinMask;
//		if ( maxIndex != -1 )
//		{
//			largest = components.elementAt(maxIndex);	
//			System.err.println( "Skin Surface " + maxIndex + " " + components.elementAt(maxIndex).cardinality() );
//		}
//
//		BitSet visited2 = new BitSet(dimX*dimY*dimZ);
//		ModelImage surface = new ModelImage( ModelStorageBase.INTEGER, blur.getExtents(), "Surface Test" );
//		JDialogBase.updateFileInfo(blur, surface);
//		Vector<Vector3f> seedList = new Vector<Vector3f>();
//		float minV = Float.MAX_VALUE;
//		float maxV = -Float.MAX_VALUE;
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					int index = z*dimX*dimY + y*dimX + x;
//					if ( largest.get(index) )
//					{
//						float val = blur.getFloat(x,y,z);
//						if ( val > maxV )
//						{
//							maxV = val;
//						}
//						if ( val < minV )
//						{
//							minV = val;
//						}
//						//						mp.setC(x, y, z, 1, 255);
//						seedList.add(new Vector3f(x,y,z));
//						surface.set(x, y, z, 10);
//						visited2.set(index);
//					}	
//				}
//			}
//		}
//		System.err.println( "min max " + minV + " " + maxV );
//		System.err.println( seedList.size() );
//
//		//		System.err.println( "Saving mp image to : " + directory + mp.getImageName() + ".tif" );
//		//		ModelImage.saveImage( mp, mp.getImageName() + ".tif", directory, false ); 
//		//		
//		//		blur.calcMinMax();
//		//		fill2(blur, (float) Math.min(minV, blur.getMax() ), (float) blur.getMax(), seedList, surface, visited2, 10);
//		//		surface.calcMinMax();
//		//		new ViewJFrameImage((ModelImage) surface);
//
//		return surface;
//
//	}

	//	public static ModelImage outlineB( String directory, ModelImage image, ModelImage mp, float threshold, int blurVal, boolean edgesOnly )
	//	{
	//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
	//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
	//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
	//
	//		String imageName = image.getImageName();
	//		if (imageName.contains("_clone")) {
	//			imageName = imageName.replaceAll("_clone", "");
	//		}
	//		imageName = imageName + "_Outline_MP_Z";
	//
	//		ModelImage blur = blur(image, blurVal);
	//
	//		System.err.println( "Threshold value = " + threshold );		
	//
	//		BitSet skinMask = new BitSet(dimX*dimY*dimZ);
	//
	//		int count = 0;
	//		Vector<Float> maxVals = new Vector<Float>();
	//		Vector<Float> minVals = new Vector<Float>();
	//		Vector<Vector3f> maxPts = new Vector<Vector3f>();
	//		Vector<Vector3f> maxHighPts = new Vector<Vector3f>();
	//		Vector<Vector3f> minPts = new Vector<Vector3f>();
	//		for ( int y = 0; y < dimY; y++ )
	//		{
	//			for ( int x = 0; x < dimX; x++ )
	//			{
	//				float max = -1;
	//				int maxZ = -1;
	//				float min = Float.MAX_VALUE;
	//				int minZ = -1;
	//				boolean up = true;
	//				boolean down = false;
	//				for ( int z = 0; z < dimZ; z++ )
	//				{
	//					float value = blur.getFloat(x,y,z);
	//					//					System.err.println( value );
	//					if ( up && (value < max) )
	//					{
	//						if ( maxZ != -1 )
	//						{
	//							//							System.err.println( "max = " + max + " " + maxX );
	//							maxPts.add( new Vector3f(x, y, maxZ) );
	//							maxVals.add(new Float(max));
	//							max = -1;
	//							maxZ = -1;
	//							up = false;
	//							down = true;
	//						}
	//					}
	//					if ( down && (value > min) )
	//					{
	//						if ( minZ != -1 )
	//						{
	//							//							System.err.println( "min = " + min + " " + minX );
	//							minPts.add( new Vector3f(x, y, minZ) );
	//							minVals.add(new Float(min));
	//							min = Float.MAX_VALUE;
	//							minZ = -1;
	//							up = true;
	//							down = false;
	//						}
	//					}
	//					if ( up && (value > max) )
	//					{
	//						max = value;
	//						maxZ = z;
	//					}
	//					if ( down && (value < min) )
	//					{
	//						min = value;
	//						minZ = z;
	//					}
	//				}
	//
	//				for ( int i = 0; i < maxPts.size(); i++ )
	//				{
	//					int zStart = (int) maxPts.elementAt(i).Z;
	//					int zEnd = zStart;
	//					float maxValue = maxVals.elementAt(i);
	//					for ( int z = zStart; z < dimZ; z++ )
	//					{
	//						float value = blur.getFloat(x, y, z);
	//						if ( value < maxValue )
	//						{
	//							break;
	//						}
	//						zEnd = z;
	//						if ( maxValue > threshold )
	//						{
	////							mp.setC(x, y, z, 3, 10);
	////							skinMask.set(z*dimY*dimX + y*dimX + x);
	//						}
	//					}
	//					int avgZ = (int)((zStart + zEnd)/2f);
	//					maxPts.elementAt(i).Z = avgZ;
	//					if ( maxValue > threshold )
	//					{
	//						maxHighPts.add( new Vector3f(x, y, avgZ) );
	//					}
	//
	//					count++;
	//					//					System.err.println( avgX + " " + maxValue );
	//				}
	//
	//				for ( int i = 0; i < minPts.size(); i++ )
	//				{
	//					int zStart = (int) minPts.elementAt(i).Z;
	//					int zEnd = zStart;
	//					float minValue = minVals.elementAt(i);
	//					for ( int z = zStart; z < dimZ; z++ )
	//					{
	//						float value = blur.getFloat(x, y, z);
	//						if ( value > minValue )
	//						{
	//							break;
	//						}
	//						zEnd = z;
	//					}
	//					int avgZ = (int)((zStart + zEnd)/2f);
	//					minPts.elementAt(i).Z = avgZ;
	//					//					mp.setC(avgX,  y, z, 2, 255 );
	//					count++;
	//					//					System.err.println( avgX + " " + minValue );
	//				}
	//
	//				if ( maxHighPts.size() > 1 )
	//				{
	//				for ( int i = 0; i < maxHighPts.size(); i++ )
	//				{
	//					Vector3f pt = maxHighPts.elementAt(i);
	//					float val = blur.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
	//					
	//					Vector3f first = maxHighPts.firstElement();
	//					float firstVal = blur.getFloat((int)first.X, (int)first.Y, (int)first.Z);
	//					
	//					Vector3f last = maxHighPts.firstElement();
	//					float lastVal = blur.getFloat((int)last.X, (int)last.Y, (int)last.Z);
	//					if ( (i == 0) || (i == maxHighPts.size() -1) )
	//					{
	//						skinMask.set((int)pt.Z*dimY*dimX + y*dimX + x);
	////						mp.setC(x, y, (int)pt.Z, 1, 255);
	//					}
	////					else if ( (val > firstVal) && (val > lastVal) )
	////					{
	////						skinMask.set((int)pt.Z*dimY*dimX + y*dimX + x);
	//////						mp.setC(x, y, (int)pt.Z, 1, 255);
	////					}
	//				}
	//				}
	//
	//				maxVals.clear();
	//				minVals.clear();
	//				maxPts.clear();
	//				minPts.clear();
	//				maxHighPts.clear();
	//			}
	//		}
	//
	//
	//
	//		for ( int z = 0; z < dimZ; z++ )
	//		{
	//			for ( int y = 0; y < dimY; y++ )
	//			{
	//				float max = -1;
	//				int maxX = -1;
	//				float min = Float.MAX_VALUE;
	//				int minX = -1;
	//				boolean up = true;
	//				boolean down = false;
	//				for ( int x = 0; x < dimX; x++ )
	//				{
	//					float value = blur.getFloat(x,y,z);
	//					//					System.err.println( value );
	//					if ( up && (value < max) )
	//					{
	//						if ( maxX != -1 )
	//						{
	//							//							System.err.println( "max = " + max + " " + maxX );
	//							maxPts.add( new Vector3f(maxX, y, z) );
	//							maxVals.add(new Float(max));
	//							max = -1;
	//							maxX = -1;
	//							up = false;
	//							down = true;
	//						}
	//					}
	//					if ( down && (value > min) )
	//					{
	//						if ( minX != -1 )
	//						{
	//							//							System.err.println( "min = " + min + " " + minX );
	//							minPts.add( new Vector3f(minX, y, z) );
	//							minVals.add(new Float(min));
	//							min = Float.MAX_VALUE;
	//							minX = -1;
	//							up = true;
	//							down = false;
	//						}
	//					}
	//					if ( up && (value > max) )
	//					{
	//						max = value;
	//						maxX = x;
	//					}
	//					if ( down && (value < min) )
	//					{
	//						min = value;
	//						minX = x;
	//					}
	//				}
	//
	//				for ( int i = 0; i < maxPts.size(); i++ )
	//				{
	//					int xStart = (int) maxPts.elementAt(i).X;
	//					int xEnd = xStart;
	//					float maxValue = maxVals.elementAt(i);
	//					for ( int x = xStart; x < dimX; x++ )
	//					{
	//						float value = blur.getFloat(x, y, z);
	//						if ( value < maxValue )
	//						{
	//							break;
	//						}
	//						xEnd = x;
	//						if ( maxValue > threshold )
	//						{
	////							mp.setC(x, y, z, 1, 10);
	////							skinMask.set(z*dimY*dimX + y*dimX + x);
	//						}
	//					}
	//					int avgX = (int)((xStart + xEnd)/2f);
	//					maxPts.elementAt(i).X = avgX;
	//					if ( maxValue > threshold )
	//					{
	//						maxHighPts.add( new Vector3f(avgX,y,z));
	//					}
	//
	//					count++;
	//					//					System.err.println( avgX + " " + maxValue );
	//				}
	//
	//				for ( int i = 0; i < minPts.size(); i++ )
	//				{
	//					int xStart = (int) minPts.elementAt(i).X;
	//					int xEnd = xStart;
	//					float minValue = minVals.elementAt(i);
	//					for ( int x = xStart; x < dimX; x++ )
	//					{
	//						float value = blur.getFloat(x, y, z);
	//						if ( value > minValue )
	//						{
	//							break;
	//						}
	//						xEnd = x;
	//					}
	//					int avgX = (int)((xStart + xEnd)/2f);
	//					minPts.elementAt(i).X = avgX;
	//					//					mp.setC(avgX,  y, z, 2, 255 );
	//					count++;
	//					//					System.err.println( avgX + " " + minValue );
	//				}
	//				
	//				if ( maxHighPts.size() > 1 )
	//				{
	//				for ( int i = 0; i < maxHighPts.size(); i++ )
	//				{
	//					Vector3f pt = maxHighPts.elementAt(i);
	//					float val = blur.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
	//					
	//					Vector3f first = maxHighPts.firstElement();
	//					float firstVal = blur.getFloat((int)first.X, (int)first.Y, (int)first.Z);
	//					
	//					Vector3f last = maxHighPts.firstElement();
	//					float lastVal = blur.getFloat((int)last.X, (int)last.Y, (int)last.Z);
	//					if ( (i == 0) || (i == maxHighPts.size() -1) )
	//					{
	//						skinMask.set(z*dimY*dimX + y*dimX + (int)pt.X);
	////						mp.setC((int)pt.X, y, z, 2, 255);
	//					}
	////					else if ( (val > firstVal) && (val > lastVal) )
	////					{
	////						skinMask.set(z*dimY*dimX + y*dimX + (int)pt.X);
	//////						mp.setC((int)pt.X, y, z, 2, 255);
	////					}
	//				}
	//				}
	//
	//				maxVals.clear();
	//				minVals.clear();
	//				maxPts.clear();
	//				minPts.clear();
	//				maxHighPts.clear();
	//			}
	//		}
	//		
	//		for ( int z = 0; z < dimZ; z++ )
	//		{
	//			for ( int x = 0; x < dimX; x++ )
	//			{
	//				float max = -1;
	//				int maxY = -1;
	//				float min = Float.MAX_VALUE;
	//				int minY = -1;
	//				boolean up = true;
	//				boolean down = false;
	//				for ( int y = 0; y < dimY; y++ )
	//				{
	//					float value = blur.getFloat(x,y,z);
	//					//					System.err.println( value );
	//					if ( up && (value < max) )
	//					{
	//						if ( maxY != -1 )
	//						{
	//							//							System.err.println( "max = " + max + " " + maxX );
	//							maxPts.add( new Vector3f(x, maxY, z) );
	//							maxVals.add(new Float(max));
	//							max = -1;
	//							maxY = -1;
	//							up = false;
	//							down = true;
	//						}
	//					}
	//					if ( down && (value > min) )
	//					{
	//						if ( minY != -1 )
	//						{
	//							//							System.err.println( "min = " + min + " " + minX );
	//							minPts.add( new Vector3f(x, minY, z) );
	//							minVals.add(new Float(min));
	//							min = Float.MAX_VALUE;
	//							minY = -1;
	//							up = true;
	//							down = false;
	//						}
	//					}
	//					if ( up && (value > max) )
	//					{
	//						max = value;
	//						maxY = y;
	//					}
	//					if ( down && (value < min) )
	//					{
	//						min = value;
	//						minY = y;
	//					}
	//				}
	//
	//				for ( int i = 0; i < maxPts.size(); i++ )
	//				{
	//					int yStart = (int) maxPts.elementAt(i).Y;
	//					int yEnd = yStart;
	//					float maxValue = maxVals.elementAt(i);
	//					for ( int y = yStart; y < dimY; y++ )
	//					{
	//						float value = blur.getFloat(x, y, z);
	//						if ( value < maxValue )
	//						{
	//							break;
	//						}
	//						yEnd = y;
	//						if ( maxValue > threshold )
	//						{
	////							mp.setC(x, y, z, 2, 10);
	////							skinMask.set(z*dimY*dimX + y*dimX + x);
	//						}
	//					}
	//					int avgY = (int)((yStart + yEnd)/2f);
	//					maxPts.elementAt(i).Y = avgY;
	//					if ( maxValue > threshold )
	//					{
	//						maxHighPts.add( new Vector3f(x, avgY, z) );
	//					}
	//
	//					count++;
	//					//					System.err.println( avgX + " " + maxValue );
	//				}
	//
	//				for ( int i = 0; i < minPts.size(); i++ )
	//				{
	//					int yStart = (int) minPts.elementAt(i).Y;
	//					int yEnd = yStart;
	//					float minValue = minVals.elementAt(i);
	//					for ( int y = yStart; y < dimY; y++ )
	//					{
	//						float value = blur.getFloat(x, y, z);
	//						if ( value > minValue )
	//						{
	//							break;
	//						}
	//						yEnd = y;
	//					}
	//					int avgY = (int)((yStart + yEnd)/2f);
	//					minPts.elementAt(i).Y = avgY;
	//					//					mp.setC(avgX,  y, z, 2, 255 );
	//					count++;
	//					//					System.err.println( avgX + " " + minValue );
	//				}
	//
	//				if ( maxHighPts.size() > 1 )
	//				{
	//				for ( int i = 0; i < maxHighPts.size(); i++ )
	//				{
	//					Vector3f pt = maxHighPts.elementAt(i);
	//					float val = blur.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
	//					
	//					Vector3f first = maxHighPts.firstElement();
	//					float firstVal = blur.getFloat((int)first.X, (int)first.Y, (int)first.Z);
	//					
	//					Vector3f last = maxHighPts.firstElement();
	//					float lastVal = blur.getFloat((int)last.X, (int)last.Y, (int)last.Z);
	//					if ( (i == 0) || (i == maxHighPts.size() -1) )
	//					{
	//						skinMask.set(z*dimY*dimX + (int)pt.Y*dimX + x);
	////						mp.setC(x, (int)pt.Y, z, 3, 255);
	//					}
	////					else if ( (val > firstVal) && (val > lastVal) )
	////					{
	////						skinMask.set(z*dimY*dimX + (int)pt.Y*dimX + x);
	//////						mp.setC(x, (int)pt.Y, z, 3, 255);
	////					}
	//				}
	//				}
	//				maxVals.clear();
	//				minVals.clear();
	//				maxPts.clear();
	//				minPts.clear();
	//				maxHighPts.clear();
	//			}
	//		}
	//
	////		mp.calcMinMax();
	////		new ViewJFrameImage((ModelImage)mp.clone());
	//
	//		Vector<BitSet> components = new Vector<BitSet>();
	//		BitSet visited = new BitSet(dimZ*dimY*dimX);
	//		for ( int y = 0; y < dimY; y++ )
	//		{
	//			for ( int x = 0; x < dimX; x++ )
	//			{
	//				for ( int z = 0; z < dimZ; z++ )
	//				{
	//					int index = z*dimY*dimX + y*dimX + x;
	//					if ( skinMask.get(index) && !visited.get(index) )
	//					{
	//						BitSet filled = new BitSet(dimZ*dimY*dimX);
	//						Vector<Vector3f> seeds = new Vector<Vector3f>();
	//						seeds.add( new Vector3f(x, y, z) );
	//						fillMask( skinMask, visited, filled, seeds, dimX, dimY, dimZ );
	//						if ( filled.cardinality() > 1 )
	//						{
	//							components.add(filled);
	//							//							System.err.println( x + " " + y + " " + z + "    " + filled.cardinality() );
	//						}
	//					}
	//				}
	//			}
	//		}
	//
	//		int maxIndex = -1;
	//		int max = -1;
	//		for ( int i = 0; i < components.size(); i++ )
	//		{
	//			if ( components.elementAt(i).cardinality() > max )
	//			{
	//				max = components.elementAt(i).cardinality();
	//				maxIndex = i;
	//			}
	//		}
	//
	//		BitSet largest = skinMask;
	//		if ( maxIndex != -1 )
	//		{
	//			 largest = components.elementAt(maxIndex);	
	//			 System.err.println( "Skin Surface " + maxIndex + " " + components.elementAt(maxIndex).cardinality() );
	//		}
	////		BitSet xAdd = new BitSet(dimX*dimY*dimZ);
	////		BitSet yAdd = new BitSet(dimX*dimY*dimZ);
	////		BitSet zAdd = new BitSet(dimX*dimY*dimZ);
	////		int sliceCount = 10;
	////		while ( sliceCount > 0 )
	////		{
	////			sliceCount = 0;
	////			for ( int z = 1; z < dimZ-1; z++ )
	////			{
	////				for ( int y = 0; y < dimY; y++ )
	////				{
	////					for ( int x = 0; x < dimX; x++ )
	////					{
	////						int index = z*dimX*dimY + y*dimX + x;
	////						int indexM1 = (z-1)*dimX*dimY + y*dimX + x;
	////						int indexP1 = (z+1)*dimX*dimY + y*dimX + x;
	////						if ( !largest.get(index) && largest.get(indexM1) && largest.get(indexP1) )
	////						{
	////							xAdd.set(index);
	////							sliceCount++;
	////						}	
	////					}
	////				}
	////			}
	////			for ( int x = 1; x < dimX-1; x++ )
	////			{
	////				for ( int y = 0; y < dimY; y++ )
	////				{
	////					for ( int z = 0; z < dimZ; z++ )
	////					{
	////						int index = z*dimX*dimY + y*dimX + x;
	////						int indexM1 = z*dimX*dimY + y*dimX + (x-1);
	////						int indexP1 = z*dimX*dimY + y*dimX + (x+1);
	////						if ( !largest.get(index) && largest.get(indexM1) && largest.get(indexP1) )
	////						{
	////							yAdd.set(index);
	////							sliceCount++;
	////						}	
	////					}
	////				}
	////			}
	////			for ( int y = 1; y < dimY-1; y++ )
	////			{
	////				for ( int x = 0; x < dimX; x++ )
	////				{
	////					for ( int z = 0; z < dimZ; z++ )
	////					{
	////						int index = z*dimX*dimY + y*dimX + x;
	////						int indexM1 = z*dimX*dimY + (y-1)*dimX + x;
	////						int indexP1 = z*dimX*dimY + (y+1)*dimX + x;
	////						if ( !largest.get(index) && largest.get(indexM1) && largest.get(indexP1) )
	////						{
	////							zAdd.set(index);
	////							sliceCount++;
	////						}		
	////					}
	////				}
	////			}
	////			largest.or(xAdd);  xAdd.clear();
	////			largest.or(yAdd);  yAdd.clear();
	////			largest.or(zAdd);  zAdd.clear();
	////
	////			System.err.println( sliceCount );
	////		}
	//
	//		BitSet visited2 = new BitSet(dimX*dimY*dimZ);
	//		ModelImage surface = new ModelImage( ModelStorageBase.INTEGER, image.getExtents(), "Surface Test" );
	//		JDialogBase.updateFileInfo(image, surface);
	//		Vector<Vector3f> seedList = new Vector<Vector3f>();
	//		float minV = Float.MAX_VALUE;
	//		float maxV = -Float.MAX_VALUE;
	//		for ( int z = 0; z < dimZ; z++ )
	//		{
	//			for ( int y = 0; y < dimY; y++ )
	//			{
	//				for ( int x = 0; x < dimX; x++ )
	//				{
	//					int index = z*dimX*dimY + y*dimX + x;
	//					if ( largest.get(index) )
	//					{
	//						float val = blur.getFloat(x,y,z);
	//						if ( val > maxV )
	//						{
	//							maxV = val;
	//						}
	//						if ( val < minV )
	//						{
	//							minV = val;
	//						}
	//						mp.setC(x, y, z, 1, 255);
	//						seedList.add(new Vector3f(x,y,z));
	//						surface.set(x, y, z, 10);
	//						visited2.set(index);
	//					}	
	//				}
	//			}
	//		}
	//		System.err.println( "min max " + minV + " " + maxV );
	//		System.err.println( seedList.size() );
	//		
	////		ModelImage inside = new ModelImage( ModelStorageBase.SHORT, mp.getExtents(), "inside mask" );
	////		inside(mp, mp);
	////		center(mp, mp);
	//		
	//
	////		ModelImage3DLayout volumeLayout = new ModelImage3DLayout(dimX, dimY, dimZ, 1, 1, 1, 0, 0, 0);
	////
	////        // Perform the skeletonization of the input image.
	////        // Extract the centerline curve.
	////		Skeleton3D skeleton = new Skeleton3D(mp,volumeLayout);
	////
	////        FlyPathGraphSamples kFlyPathGraphSamples = skeleton.getPathGraph(1, minV.distance(maxV) );
	////
	////        FlyPathGraphCurve flyPathGraphCurve = new FlyPathGraphCurve(kFlyPathGraphSamples, 0.07f, 2);
	////
	////        if ( flyPathGraphCurve.getNumBranches() > 0 )
	////        {
	////        	System.err.println( "Path found" );
	////        	int numV = 100;
	////        	Curve3f kCurve = flyPathGraphCurve.getCurvePosition(0);
	////        	float fStep = kCurve.GetTotalLength() / (numV - 1);
	////
	////        	short id = (short) mp.getVOIs().getUniqueID();
	////        	VOI centerPath = new VOI(id, "centerPath", VOI.POLYLINE, (float) Math.random());
	////        	mp.registerVOI(centerPath);
	////        	VOIContour path = new VOIContour(false);
	////        	for (int i = 0; i < numV; i++ )
	////        	{
	////        		float fDist = i * fStep;
	////        		float fTime = kCurve.GetTime(fDist, 100, 1e-02f);      
	////        		Vector3f kPoint = kCurve.GetPosition(fTime);
	////        		path.add(kPoint);
	////        		System.err.println(kPoint);
	////        	}
	////        }
	////		System.err.println( count );
	//
	//
	//		System.err.println( "Saving mp image to : " + directory + mp.getImageName() + ".tif" );
	//		ModelImage.saveImage( mp, mp.getImageName() + ".tif", directory, false ); 
	//		
	//		blur.calcMinMax();
	//		fill2(blur, (float) Math.min(minV, blur.getMax() ), (float) blur.getMax(), seedList, surface, visited2, new Vector3f(), 10);
	//		surface.calcMinMax();
	//		new ViewJFrameImage((ModelImage) surface);
	//		
	//		return surface;
	//
	////		ModelImage surface2 = new ModelImage( ModelStorageBase.INTEGER, image.getExtents(), "Surface Test 2" );
	////		JDialogBase.updateFileInfo(image, surface2);
	////		outside(surface, surface2, 10, 10);
	////		surface2.calcMinMax();
	////		new ViewJFrameImage((ModelImage) surface2);
	////		mp.calcMinMax();
	////		new ViewJFrameImage((ModelImage) mp);
	//
	//	}




	public static void outside( String directory, ModelImage image, ModelImage mp, float threshold, int blurVal )
	{
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_Outline_MP_Z";

		ModelImage blur = blur(image, blurVal);

		System.err.println( "Threshold value = " + threshold );		

		BitSet skinMask = new BitSet(dimX*dimY*dimZ);

		Vector<Float> maxVals = new Vector<Float>();
		Vector<Float> minVals = new Vector<Float>();
		Vector<Vector3f> maxPts = new Vector<Vector3f>();
		Vector<Vector3f> maxHighPts = new Vector<Vector3f>();
		Vector<Vector3f> minPts = new Vector<Vector3f>();

		Vector<Vector3f> leftSide = new Vector<Vector3f>();
		Vector<Vector3f> rightSide = new Vector<Vector3f>();

		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				float max = -1;
				int maxX = -1;
				float min = Float.MAX_VALUE;
				int minX = -1;
				boolean up = true;
				boolean down = false;
				for ( int x = 0; x < dimX; x++ )
				{
					float value = blur.getFloat(x,y,z);
					//					System.err.println( value );
					if ( up && (value < max) )
					{
						if ( maxX != -1 )
						{
							//							System.err.println( "max = " + max + " " + maxX );
							maxPts.add( new Vector3f(maxX, y, z) );
							maxVals.add(new Float(max));
							max = -1;
							maxX = -1;
							up = false;
							down = true;
						}
					}
					if ( down && (value > min) )
					{
						if ( minX != -1 )
						{
							//							System.err.println( "min = " + min + " " + minX );
							minPts.add( new Vector3f(minX, y, z) );
							minVals.add(new Float(min));
							min = Float.MAX_VALUE;
							minX = -1;
							up = true;
							down = false;
						}
					}
					if ( up && (value > max) )
					{
						max = value;
						maxX = x;
					}
					if ( down && (value < min) )
					{
						min = value;
						minX = x;
					}
				}

				for ( int i = 0; i < maxPts.size(); i++ )
				{
					int xStart = (int) maxPts.elementAt(i).X;
					int xEnd = xStart;
					float maxValue = maxVals.elementAt(i);
					for ( int x = xStart; x < dimX; x++ )
					{
						float value = blur.getFloat(x, y, z);
						if ( value < maxValue )
						{
							break;
						}
						xEnd = x;
						if ( maxValue > threshold )
						{
							//							mp.setC(x, y, z, 1, 10);
							//							skinMask.set(z*dimY*dimX + y*dimX + x);
						}
					}
					int avgX = (int)((xStart + xEnd)/2f);
					if ( avgX > (xStart + 10) )
					{
						avgX = xStart + 10;
					}
					maxPts.elementAt(i).X = avgX;
					if ( maxValue > threshold )
					{
						//						maxHighPts.add( new Vector3f(avgX,y,z));
						leftSide.add( new Vector3f( avgX, y, z ) );
						break;
					}

					//					System.err.println( avgX + " " + maxValue );
				}


				//				if ( maxHighPts.size() > 1 )
				//				{
				//					for ( int i = 0; i < maxHighPts.size(); i++ )
				//					{
				//						Vector3f pt = maxHighPts.elementAt(i);
				//						float val = blur.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
				//
				//						Vector3f first = maxHighPts.firstElement();
				//						float firstVal = blur.getFloat((int)first.X, (int)first.Y, (int)first.Z);
				//
				//						Vector3f last = maxHighPts.firstElement();
				//						float lastVal = blur.getFloat((int)last.X, (int)last.Y, (int)last.Z);
				//						if ( (i == 0) || (i == maxHighPts.size() -1) )
				//						{
				//							skinMask.set(z*dimY*dimX + y*dimX + (int)pt.X);
				//							if ( i == 0 )
				//							{
				//								leftSide.add( new Vector3f((int)pt.X, y, z) );
				//							}
				//							else
				//							{
				//								rightSide.add(0, new Vector3f((int)pt.X, y, z));
				//							}
				//							
				//							//						mp.setC((int)pt.X, y, z, 2, 255);
				//						}
				//						//					else if ( (val > firstVal) && (val > lastVal) )
				//						//					{
				//						//						skinMask.set(z*dimY*dimX + y*dimX + (int)pt.X);
				//						//					}
				//					}
				//				}

				maxVals.clear();
				minVals.clear();
				maxPts.clear();
				minPts.clear();
				maxHighPts.clear();




				max = -1;
				maxX = -1;
				min = Float.MAX_VALUE;
				minX = -1;
				up = true;
				down = false;
				for ( int x = dimX-1; x >= 0; x-- )
				{
					float value = blur.getFloat(x,y,z);
					//					System.err.println( value );
					if ( up && (value < max) )
					{
						if ( maxX != -1 )
						{
							//							System.err.println( "max = " + max + " " + maxX );
							maxPts.add( new Vector3f(maxX, y, z) );
							maxVals.add(new Float(max));
							max = -1;
							maxX = -1;
							up = false;
							down = true;
						}
					}
					if ( down && (value > min) )
					{
						if ( minX != -1 )
						{
							//							System.err.println( "min = " + min + " " + minX );
							minPts.add( new Vector3f(minX, y, z) );
							minVals.add(new Float(min));
							min = Float.MAX_VALUE;
							minX = -1;
							up = true;
							down = false;
						}
					}
					if ( up && (value > max) )
					{
						max = value;
						maxX = x;
					}
					if ( down && (value < min) )
					{
						min = value;
						minX = x;
					}
				}

				for ( int i = 0; i < maxPts.size(); i++ )
				{
					int xStart = (int) maxPts.elementAt(i).X;
					int xEnd = xStart;
					float maxValue = maxVals.elementAt(i);
					for ( int x = xStart; x < dimX; x++ )
					{
						float value = blur.getFloat(x, y, z);
						if ( value < maxValue )
						{
							break;
						}
						xEnd = x;
						if ( maxValue > threshold )
						{
							//							mp.setC(x, y, z, 1, 10);
							//							skinMask.set(z*dimY*dimX + y*dimX + x);
						}
					}
					int avgX = (int)((xStart + xEnd)/2f);
					if ( avgX > (xStart + 10) )
					{
						avgX = xStart + 10;
					}
					maxPts.elementAt(i).X = avgX;
					if ( maxValue > threshold )
					{
						//						maxHighPts.add( new Vector3f(avgX,y,z));
						rightSide.add( 0, new Vector3f( avgX, y, z ) );
						break;
					}

					//					System.err.println( avgX + " " + maxValue );
				}
				maxVals.clear();
				minVals.clear();
				maxPts.clear();
				minPts.clear();
				maxHighPts.clear();
			}


			//			System.err.println( "Left edge " + leftSide.size() );
			if ( leftSide.size() > 1 )
			{
				VOIContour contour = new VOIContour(true);
				for ( int i = 0; i < leftSide.size(); i++ )
				{
					contour.add( leftSide.elementAt(i) );
				}
				for ( int i = 0; i < rightSide.size(); i++ )
				{
					contour.add( rightSide.elementAt(i) );
				}
				short id = (short) image.getVOIs().getUniqueID();
				VOI wormContours = new VOI(id, "contours" + z, VOI.POLYLINE, (float) Math.random());
				blur.registerVOI(wormContours);
				wormContours.getCurves().add(contour);

				leftSide.clear();
				rightSide.clear();
			}
		}
		blur.calcMinMax();
		new ViewJFrameImage(blur);


		BitSet largest = skinMask;

		for ( int z = 0; z < dimZ; z++ )		
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					int index = z*dimX*dimY + y*dimX + x;
					if ( largest.get(index) )
					{
						mp.setC(x, y, z, 1, 255);
					}	
				}
			}
		}


		System.err.println( "Saving mp image to : " + directory + mp.getImageName() + ".tif" );
		ModelImage.saveImage( mp, mp.getImageName() + ".tif", directory, false ); 
		mp.calcMinMax();
		new ViewJFrameImage((ModelImage) mp);

	}



//	public static void outline2( String directory, ModelImage image, ModelImage mp, float threshold )
//	{
//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
//
//		String imageName = image.getImageName();
//		if (imageName.contains("_clone")) {
//			imageName = imageName.replaceAll("_clone", "");
//		}
//		imageName = imageName + "_Outline_MP_Z";
//
//		ModelImage blur = blur(image, 3);
//
//		System.err.println( "Threshold value = " + threshold );		
//		//		outside(blur, mp, threshold);
//
//
//		//		float[] targetP = new float[2];
//		//		estimateHistogram( image, targetP, .1f );
//		//		System.err.println( targetP[0] + " " + targetP[1] );
//		//		estimateHistogram( image, targetP, .98f );
//		//		System.err.println( targetP[0] + " " + targetP[1] );
//
//		//		int z = dimZ/2;
//		//		{
//		//			int y = dimY/2;
//		//			{
//		//				for ( int x = 0; x < dimX; x++ )
//		//				{
//		//					float val = blur.getFloat(x, y, z);
//		//					System.err.println( x + " " + val );
//		//				}
//		//			}
//		//		}
//
//		BitSet skinMask = new BitSet(dimX*dimY*dimZ);
//
//		int count = 0;
//		Vector<Float> maxVals = new Vector<Float>();
//		Vector<Float> minVals = new Vector<Float>();
//		Vector<Vector3f> maxPts = new Vector<Vector3f>();
//		Vector<Vector3f> minPts = new Vector<Vector3f>();
//		//		for ( int y = 0; y < dimY; y++ )
//		//		{
//		//			for ( int x = 0; x < dimX; x++ )
//		//			{
//		//				float max = -1;
//		//				int maxZ = -1;
//		//				float min = Float.MAX_VALUE;
//		//				int minZ = -1;
//		//				boolean up = true;
//		//				boolean down = false;
//		//				for ( int z = 0; z < dimZ; z++ )
//		//				{
//		//					float value = blur.getFloat(x,y,z);
//		//					//					System.err.println( value );
//		//					if ( up && (value < max) )
//		//					{
//		//						if ( maxZ != -1 )
//		//						{
//		//							//							System.err.println( "max = " + max + " " + maxX );
//		//							maxPts.add( new Vector3f(x, y, maxZ) );
//		//							maxVals.add(new Float(max));
//		//							max = -1;
//		//							maxZ = -1;
//		//							up = false;
//		//							down = true;
//		//						}
//		//					}
//		//					if ( down && (value > min) )
//		//					{
//		//						if ( minZ != -1 )
//		//						{
//		//							//							System.err.println( "min = " + min + " " + minX );
//		//							minPts.add( new Vector3f(x, y, minZ) );
//		//							minVals.add(new Float(min));
//		//							min = Float.MAX_VALUE;
//		//							minZ = -1;
//		//							up = true;
//		//							down = false;
//		//						}
//		//					}
//		//					if ( up && (value > max) )
//		//					{
//		//						max = value;
//		//						maxZ = z;
//		//					}
//		//					if ( down && (value < min) )
//		//					{
//		//						min = value;
//		//						minZ = z;
//		//					}
//		//				}
//		//
//		//				for ( int i = 0; i < maxPts.size(); i++ )
//		//				{
//		//					int zStart = (int) maxPts.elementAt(i).Z;
//		//					int zEnd = zStart;
//		//					float maxValue = maxVals.elementAt(i);
//		//					for ( int z = zStart; z < dimZ; z++ )
//		//					{
//		//						float value = blur.getFloat(x, y, z);
//		//						if ( value < maxValue )
//		//						{
//		//							break;
//		//						}
//		//						zEnd = z;
//		//						if ( maxValue > threshold )
//		//						{
//		////							mp.setC(x, y, z, 3, 10);
//		//							skinMask.set(z*dimY*dimX + y*dimX + x);
//		//						}
//		//					}
//		//					int avgZ = (int)((zStart + zEnd)/2f);
//		//					maxPts.elementAt(i).Z = avgZ;
//		//
//		//					count++;
//		//					//					System.err.println( avgX + " " + maxValue );
//		//				}
//		//
//		//				for ( int i = 0; i < minPts.size(); i++ )
//		//				{
//		//					int zStart = (int) minPts.elementAt(i).Z;
//		//					int zEnd = zStart;
//		//					float minValue = minVals.elementAt(i);
//		//					for ( int z = zStart; z < dimZ; z++ )
//		//					{
//		//						float value = blur.getFloat(x, y, z);
//		//						if ( value > minValue )
//		//						{
//		//							break;
//		//						}
//		//						zEnd = z;
//		//					}
//		//					int avgZ = (int)((zStart + zEnd)/2f);
//		//					minPts.elementAt(i).Z = avgZ;
//		//					//					mp.setC(avgX,  y, z, 2, 255 );
//		//					count++;
//		//					//					System.err.println( avgX + " " + minValue );
//		//				}
//		//
//		//				maxVals.clear();
//		//				minVals.clear();
//		//				maxPts.clear();
//		//				minPts.clear();
//		//			}
//		//		}
//
//
//
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				float max = -1;
//				int maxX = -1;
//				float min = Float.MAX_VALUE;
//				int minX = -1;
//				boolean up = true;
//				boolean down = false;
//				for ( int x = 0; x < dimX; x++ )
//				{
//					float value = blur.getFloat(x,y,z);
//					//					System.err.println( value );
//					if ( up && (value < max) )
//					{
//						if ( maxX != -1 )
//						{
//							//							System.err.println( "max = " + max + " " + maxX );
//							maxPts.add( new Vector3f(maxX, y, z) );
//							maxVals.add(new Float(max));
//							max = -1;
//							maxX = -1;
//							up = false;
//							down = true;
//						}
//					}
//					if ( down && (value > min) )
//					{
//						if ( minX != -1 )
//						{
//							//							System.err.println( "min = " + min + " " + minX );
//							minPts.add( new Vector3f(minX, y, z) );
//							minVals.add(new Float(min));
//							min = Float.MAX_VALUE;
//							minX = -1;
//							up = true;
//							down = false;
//						}
//					}
//					if ( up && (value > max) )
//					{
//						max = value;
//						maxX = x;
//					}
//					if ( down && (value < min) )
//					{
//						min = value;
//						minX = x;
//					}
//				}
//
//				for ( int i = 0; i < maxPts.size(); i++ )
//				{
//					int xStart = (int) maxPts.elementAt(i).X;
//					int xEnd = xStart;
//					float maxValue = maxVals.elementAt(i);
//					for ( int x = xStart; x < dimX; x++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value < maxValue )
//						{
//							break;
//						}
//						xEnd = x;
//						if ( maxValue > threshold )
//						{
//							//							mp.setC(x, y, z, 1, 10);
//							skinMask.set(z*dimY*dimX + y*dimX + x);
//						}
//					}
//					int avgX = (int)((xStart + xEnd)/2f);
//					maxPts.elementAt(i).X = avgX;
//
//					count++;
//					//					System.err.println( avgX + " " + maxValue );
//				}
//
//				for ( int i = 0; i < minPts.size(); i++ )
//				{
//					int xStart = (int) minPts.elementAt(i).X;
//					int xEnd = xStart;
//					float minValue = minVals.elementAt(i);
//					for ( int x = xStart; x < dimX; x++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value > minValue )
//						{
//							break;
//						}
//						xEnd = x;
//					}
//					int avgX = (int)((xStart + xEnd)/2f);
//					minPts.elementAt(i).X = avgX;
//					//					mp.setC(avgX,  y, z, 2, 255 );
//					count++;
//					//					System.err.println( avgX + " " + minValue );
//				}
//
//				maxVals.clear();
//				minVals.clear();
//				maxPts.clear();
//				minPts.clear();
//			}
//
//
//
//
//			/*
//			for ( int y = 0; y < dimY; y++ )
//			{
//				int startEdgeX = dimX;
//				int endEdgeX = 1;
//				for ( int x = 1; x < dimX; x++ )
//				{
//					if ( mp.getFloatC(x,y,z,1) > 0 )
//					{
//						if ( startEdgeX > x )
//						{
//							startEdgeX = x;
//						}
//						else
//						{
//							endEdgeX = x;
//							break;
//						}
//					}
//				}
//				if ( startEdgeX >= endEdgeX )
//				{
//					continue;
//				}
//
//
//				int startX = endEdgeX;
//				float prevVal = blur.getFloat(startEdgeX, y, z);
//				for ( int x = startEdgeX + 1; x <= endEdgeX; x++ )
//				{
//					float val = blur.getFloat(x, y, z);
//					if ( val < prevVal )
//					{
//						startX = x;
//						break;
//					}
//					prevVal = val;
//				}
//				int endX = startEdgeX;
//				prevVal = blur.getFloat(endEdgeX, y, z);
//				for ( int x = endEdgeX - 1; x >= startEdgeX; x-- )
//				{
//					float val = blur.getFloat(x, y, z);
//					if ( val < prevVal )
//					{
//						endX = x;
//						break;
//					}
//					prevVal = val;
//				}
//				if ( startX >= endX )
//				{
//					continue;
//				}
////				System.err.println( y + "   " + startEdgeX + " " + startX + " " + endEdgeX + " " + endX );
//
//				prevVal = blur.getFloat(startX, y, z);
//				boolean up = false;
//				boolean down = false;
//				Vector<Vector3f> upPts = new Vector<Vector3f>();
//				Vector<Vector3f> downPts = new Vector<Vector3f>();
//				downPts.add(new Vector3f(startX, y, z) );
//				for ( int x = startX+1; x <= endX; x++ )
//				{
//					float val = blur.getFloat(x, y, z);
////					System.err.print( val + " " + prevVal + " " );
//					if ( val > prevVal )
//					{
//						up = true;
//						if ( down )
//						{
//							// changed dir:
//							downPts.add(new Vector3f(x-1,y,z));
////							System.err.println( prevVal + "    bottom " + downPts.lastElement() );
//							down = false;
//						}
//					}
//					else if ( val < prevVal )
//					{
//						down = true;
//						if ( up )
//						{
//							// changed dir:
//							upPts.add(new Vector3f(x-1,y,z));
////							System.err.println( y + "   " + upPts.lastElement() );
//							up = false;
//						}
//					}
//					prevVal = val;
////					System.err.println( up + " " + down );
//				}
//				downPts.add(new Vector3f(endX, y, z) );
//
//				Vector<Vector3f> peaks = new Vector<Vector3f>();
////				Vector<Vector3f> startPts = new Vector<Vector3f>();
////				Vector<Vector3f> endPts = new Vector<Vector3f>();
//				if ( upPts.size() > 0 )
//				{
//					for ( int i = 0; i < upPts.size(); i++ )
//					{
//						Vector3f ptLeft = downPts.elementAt(i);
//						Vector3f ptRight = downPts.elementAt(i+1);
//						float downValL = blur.getFloat((int)ptLeft.X, (int)ptLeft.Y, (int)ptLeft.Z);
//						float downValR = blur.getFloat((int)ptRight.X, (int)ptRight.Y, (int)ptRight.Z);
//
//
//						Vector3f pt = upPts.elementAt(i);
//						float upVal = blur.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
//
////						System.err.println(y + " " + downValL + " " + upVal + " " + downValR + "    " + pt );
//						if ( ((upVal > threshold) && (upVal > (1.5 * downValL)) && (upVal > (1.5 * downValR))) )
////							if ( ((upVal > 5) && (i==0)) || ((upVal > 5) && (i==(upPts.size()-1))) || ((upVal > 5) && (upVal > (1.5 * downValL)) && (upVal > (1.5 * downValR))) )
//						{
////							System.err.println("        added" + pt );
//							peaks.add(new Vector3f(pt));
//						}
//					}
//				}
//				for ( int i = 0; i < peaks.size(); i++ )
//				{
//					Vector3f pt = peaks.elementAt(i);
//					for ( int x = (int) Math.max(0, pt.X-1); x <= Math.min(dimX-1, pt.X+1); x++ )
//					{
//						int index = (z * dimY *dimX + y *dimX + x);
//						skinMask.set(index);
//						count++;
//					}
//				}
//				if ( downPts.size() > 0 )
//				{
//					for ( int i = 0; i < downPts.size() - 1; i++ )
//					{
////						System.err.print( downPts.elementAt(i) + "    "  );
//						float max = -1;
//						int maxX = -1;
//						for ( int x = (int) Math.floor(downPts.elementAt(i).X); x <= downPts.elementAt(i+1).X; x++ )
//						{
//							if ( blur.getFloat(x, y, z) > max )
//							{
//								max = blur.getFloat(x,y,z);
//								maxX = x;
//							}
//						}
//						if ( maxX != -1 )
//						{
//							mp.setC(maxX, y, z, 3, 10f);
//							System.err.print( maxX + " " + y + " " + max + "      " );
//						}
//					}
//					System.err.println("");
//				}
//				downPts.clear();
//				upPts.clear();
//			}
////			System.err.println( z + " " + y );
//			 */
//		}
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int x = 0; x < dimX; x++ )
//			{
//				float max = -1;
//				int maxY = -1;
//				float min = Float.MAX_VALUE;
//				int minY = -1;
//				boolean up = true;
//				boolean down = false;
//				for ( int y = 0; y < dimY; y++ )
//				{
//					float value = blur.getFloat(x,y,z);
//					//					System.err.println( value );
//					if ( up && (value < max) )
//					{
//						if ( maxY != -1 )
//						{
//							//							System.err.println( "max = " + max + " " + maxX );
//							maxPts.add( new Vector3f(x, maxY, z) );
//							maxVals.add(new Float(max));
//							max = -1;
//							maxY = -1;
//							up = false;
//							down = true;
//						}
//					}
//					if ( down && (value > min) )
//					{
//						if ( minY != -1 )
//						{
//							//							System.err.println( "min = " + min + " " + minX );
//							minPts.add( new Vector3f(x, minY, z) );
//							minVals.add(new Float(min));
//							min = Float.MAX_VALUE;
//							minY = -1;
//							up = true;
//							down = false;
//						}
//					}
//					if ( up && (value > max) )
//					{
//						max = value;
//						maxY = y;
//					}
//					if ( down && (value < min) )
//					{
//						min = value;
//						minY = y;
//					}
//				}
//
//				for ( int i = 0; i < maxPts.size(); i++ )
//				{
//					int yStart = (int) maxPts.elementAt(i).Y;
//					int yEnd = yStart;
//					float maxValue = maxVals.elementAt(i);
//					for ( int y = yStart; y < dimY; y++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value < maxValue )
//						{
//							break;
//						}
//						yEnd = y;
//						if ( maxValue > threshold )
//						{
//							//							mp.setC(x, y, z, 2, 10);
//							skinMask.set(z*dimY*dimX + y*dimX + x);
//						}
//					}
//					int avgY = (int)((yStart + yEnd)/2f);
//					maxPts.elementAt(i).Y = avgY;
//
//					count++;
//					//					System.err.println( avgX + " " + maxValue );
//				}
//
//				for ( int i = 0; i < minPts.size(); i++ )
//				{
//					int yStart = (int) minPts.elementAt(i).Y;
//					int yEnd = yStart;
//					float minValue = minVals.elementAt(i);
//					for ( int y = yStart; y < dimY; y++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						if ( value > minValue )
//						{
//							break;
//						}
//						yEnd = y;
//					}
//					int avgY = (int)((yStart + yEnd)/2f);
//					minPts.elementAt(i).Y = avgY;
//					//					mp.setC(avgX,  y, z, 2, 255 );
//					count++;
//					//					System.err.println( avgX + " " + minValue );
//				}
//
//				maxVals.clear();
//				minVals.clear();
//				maxPts.clear();
//				minPts.clear();
//			}
//		}
//
//
//
//
//		/*
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int x = 0; x < dimX; x++ )
//			{
//				int startEdgeY = dimY;
//				int endEdgeY = 1;
//				for ( int y = 1; y < dimY; y++ )
//				{
//					if ( mp.getFloatC(x,y,z,1) > 0 )
//					{
//						if ( startEdgeY > y )
//						{
//							startEdgeY = y;
//						}
//						else
//						{
//							endEdgeY = y;
//							break;
//						}
//					}
//				}
//				if ( startEdgeY >= endEdgeY )
//				{
//					continue;
//				}
//
//
//				int startY = endEdgeY;
//				float prevVal = blur.getFloat(x, startEdgeY, z);
//				for ( int y = startEdgeY + 1; y <= endEdgeY; y++ )
//				{
//					float val = blur.getFloat(x, y, z);
//					if ( val < prevVal )
//					{
//						startY = y;
//						break;
//					}
//					prevVal = val;
//				}
//				int endY = startEdgeY;
//				prevVal = blur.getFloat(x, endEdgeY, z);
//				for ( int y = endEdgeY - 1; y >= startEdgeY; y-- )
//				{
//					float val = blur.getFloat(x, y, z);
//					if ( val < prevVal )
//					{
//						endY = y;
//						break;
//					}
//					prevVal = val;
//				}
//				if ( startY >= endY )
//				{
//					continue;
//				}
////				System.err.println( y + "   " + startEdgeX + " " + startX + " " + endEdgeX + " " + endX );
//
//				prevVal = blur.getFloat(x, startY, z);
//				boolean up = false;
//				boolean down = false;
//				Vector<Vector3f> upPts = new Vector<Vector3f>();
//				Vector<Vector3f> downPts = new Vector<Vector3f>();
//				downPts.add(new Vector3f(x, startY, z) );
//				for ( int y = startY+1; y <= endY; y++ )
//				{
//					float val = blur.getFloat(x, y, z);
////					System.err.print( val + " " + prevVal + " " );
//					if ( val > prevVal )
//					{
//						up = true;
//						if ( down )
//						{
//							// changed dir:
//							downPts.add(new Vector3f(x,y-1,z));
////							System.err.println( prevVal + "    bottom " + downPts.lastElement() );
//							down = false;
//						}
//					}
//					else if ( val < prevVal )
//					{
//						down = true;
//						if ( up )
//						{
//							// changed dir:
//							upPts.add(new Vector3f(x,y-1,z));
////							System.err.println( y + "   " + upPts.lastElement() );
//							up = false;
//						}
//					}
//					prevVal = val;
////					System.err.println( up + " " + down );
//				}
//				downPts.add(new Vector3f(x, endY, z) );
//
//				Vector<Vector3f> peaks = new Vector<Vector3f>();
//				if ( upPts.size() > 0 )
//				{
//					for ( int i = 0; i < upPts.size(); i++ )
//					{
//						Vector3f ptLeft = downPts.elementAt(i);
//						Vector3f ptRight = downPts.elementAt(i+1);
//						float downValL = blur.getFloat((int)ptLeft.X, (int)ptLeft.Y, (int)ptLeft.Z);
//						float downValR = blur.getFloat((int)ptRight.X, (int)ptRight.Y, (int)ptRight.Z);
//
//
//						Vector3f pt = upPts.elementAt(i);
//						float upVal = blur.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
//
////						System.err.println(y + " " + downValL + " " + upVal + " " + downValR + "    " + pt );
//						if ( ((upVal > threshold) && (upVal > (1.5 * downValL)) && (upVal > (1.5 * downValR))) )
////							if ( ((upVal > 5) && (i==0)) || ((upVal > 5) && (i==(upPts.size()-1))) || ((upVal > 5) && (upVal > (1.5 * downValL)) && (upVal > (1.5 * downValR))) )
//						{
////							System.err.println("        added" + pt );
//							peaks.add(new Vector3f(pt));
//						}
//					}
//				}
//				for ( int i = 0; i < peaks.size(); i++ )
//				{
//					Vector3f pt = peaks.elementAt(i);
//					for ( int y = (int) Math.max(0, pt.Y-1); y <= Math.min(dimY-1, pt.Y+1); y++ )
//					{
//						int index = (z * dimY *dimX + y *dimX + x);
//						skinMask.set(index);
//						count++;
//					}
//				}
//				downPts.clear();
//				upPts.clear();
//			}
//		}
//
//		for ( int x = 0; x < dimX; x++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				int startEdgeZ = dimZ;
//				int endEdgeZ = 1;
//				for ( int z = 1; z < dimZ; z++ )
//				{
//					if ( mp.getFloatC(x,y,z,1) > 0 )
//					{
//						if ( startEdgeZ > z )
//						{
//							startEdgeZ = z;
//						}
//						else
//						{
//							endEdgeZ = z;
//							break;
//						}
//					}
//				}
//				if ( startEdgeZ >= endEdgeZ )
//				{
//					continue;
//				}
//
//
//				int startZ = endEdgeZ;
//				float prevVal = blur.getFloat(x, y, startEdgeZ);
//				for ( int z = startEdgeZ + 1; z <= endEdgeZ; z++ )
//				{
//					float val = blur.getFloat(x, y, z);
//					if ( val < prevVal )
//					{
//						startZ = z;
//						break;
//					}
//					prevVal = val;
//				}
//				int endZ = startEdgeZ;
//				prevVal = blur.getFloat(x, y, endEdgeZ);
//				for ( int z = endEdgeZ - 1; z >= startEdgeZ; z-- )
//				{
//					float val = blur.getFloat(x, y, z);
//					if ( val < prevVal )
//					{
//						endZ = z;
//						break;
//					}
//					prevVal = val;
//				}
//				if ( startZ >= endZ )
//				{
//					continue;
//				}
//
//
//				prevVal = blur.getFloat(x, y, startZ);
//				boolean up = false;
//				boolean down = false;
//				Vector<Vector3f> upPts = new Vector<Vector3f>();
//				Vector<Vector3f> downPts = new Vector<Vector3f>();
//				downPts.add(new Vector3f(x, y, startZ) );
//				for ( int z = startZ+1; z <= endZ; z++ )
//				{
//					float val = blur.getFloat(x, y, z);
////					System.err.print( val + " " + prevVal + " " );
//					if ( val > prevVal )
//					{
//						up = true;
//						if ( down )
//						{
//							// changed dir:
//							downPts.add(new Vector3f(x,y,z-1));
////							System.err.println( prevVal + "    bottom " + downPts.lastElement() );
//							down = false;
//						}
//					}
//					else if ( val < prevVal )
//					{
//						down = true;
//						if ( up )
//						{
//							// changed dir:
//							upPts.add(new Vector3f(x,y,z-1));
////							System.err.println( y + "   " + upPts.lastElement() );
//							up = false;
//						}
//					}
//					prevVal = val;
////					System.err.println( up + " " + down );
//				}
//
//				downPts.add(new Vector3f(x, y, endZ) );
//
//				Vector<Vector3f> peaks = new Vector<Vector3f>();
//				if ( upPts.size() > 0 )
//				{
//					for ( int i = 0; i < upPts.size(); i++ )
//					{
//						Vector3f ptLeft = downPts.elementAt(i);
//						Vector3f ptRight = downPts.elementAt(i+1);
//						float downValL = blur.getFloat((int)ptLeft.X, (int)ptLeft.Y, (int)ptLeft.Z);
//						float downValR = blur.getFloat((int)ptRight.X, (int)ptRight.Y, (int)ptRight.Z);
//
//
//						Vector3f pt = upPts.elementAt(i);
//						float upVal = blur.getFloat((int)pt.X, (int)pt.Y, (int)pt.Z);
//
////						System.err.println(y + " " + downValL + " " + upVal + " " + downValR + "    " + pt );
//						if ( ((upVal > threshold) && (upVal > (1.5 * downValL)) && (upVal > (1.5 * downValR))) )
////							if ( ((upVal > 5) && (i==0)) || ((upVal > 5) && (i==(upPts.size()-1))) || ((upVal > 5) && (upVal > (1.5 * downValL)) && (upVal > (1.5 * downValR))) )
//						{
////							System.err.println("        added" + pt );
//							peaks.add(new Vector3f(pt));
//						}
//					}
//				}
//				for ( int i = 0; i < peaks.size(); i++ )
//				{
//					Vector3f pt = peaks.elementAt(i);
//					for ( int z = (int) Math.max(0, pt.Z-1); z <= Math.min(dimZ-1, pt.Z+1); z++ )
//					{
//						int index = (z * dimY *dimX + y *dimX + x);
//						skinMask.set(index);
//						count++;
//					}
//				}
//				downPts.clear();
//				upPts.clear();
//			}
//		}
//		 */
//
//		Vector<BitSet> components = new Vector<BitSet>();
//		BitSet visited = new BitSet(dimZ*dimY*dimX);
//		for ( int y = 0; y < dimY; y++ )
//		{
//			for ( int x = 0; x < dimX; x++ )
//			{
//				for ( int z = 0; z < dimZ; z++ )
//				{
//					int index = z*dimY*dimX + y*dimX + x;
//					if ( skinMask.get(index) && !visited.get(index) )
//					{
//						BitSet filled = new BitSet(dimZ*dimY*dimX);
//						Vector<Vector3f> seeds = new Vector<Vector3f>();
//						seeds.add( new Vector3f(x, y, z) );
//						fillMask( skinMask, visited, filled, seeds, dimX, dimY, dimZ );
//						if ( filled.cardinality() > 1 )
//						{
//							components.add(filled);
//							//							System.err.println( x + " " + y + " " + z + "    " + filled.cardinality() );
//						}
//					}
//				}
//			}
//		}
//
//		int maxIndex = -1;
//		int max = -1;
//		for ( int i = 0; i < components.size(); i++ )
//		{
//			System.err.println( i + " " + components.elementAt(i).cardinality() );
//			if ( components.elementAt(i).cardinality() > max )
//			{
//				max = components.elementAt(i).cardinality();
//				maxIndex = i;
//			}
//		}
//
//
//		if ( maxIndex != -1 )
//		{
//			BitSet largest = components.elementAt(maxIndex);	
//
//			for ( int z = 0; z < dimZ; z++ )
//			{
//				for ( int y = 0; y < dimY; y++ )
//				{
//					for ( int x = 0; x < dimX; x++ )
//					{
//						int index = z*dimX*dimY + y*dimX + x;
//						if ( largest.get(index) )
//						{
//							//							for ( int z1 = Math.max(0, z-1); z1 <= Math.min(z+1, dimZ); z1++ )
//							//								for ( int y1 = Math.max(0, y-1); y1 <= Math.min(y+1, dimY); y1++ )
//							//									for ( int x1 = Math.max(0, x-1); x1 <= Math.min(x+1, dimX); x1++ )
//							//										mp.set(x1, y1, z1, 1);
//							mp.setC(x, y, z, 2, 255);
//						}
//						//						mp.setC(x, y, z, 3, image.getFloat(x,y,z));
//					}
//				}
//			}
//		}
//
//
//		System.err.println( count );
//
//
//		System.err.println( "Saving mp image to : " + directory + mp.getImageName() + ".tif" );
//		ModelImage.saveImage( mp, mp.getImageName() + ".tif", directory, false ); 
//		mp.calcMinMax();
//		new ViewJFrameImage((ModelImage) mp);
//
//	}




//	public static void findContours( String directory, ModelImage image, ModelImage mp, ModelImage contourImage, float threshold )
//	{
//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
//
//		String imageName = image.getImageName();
//		if (imageName.contains("_clone")) {
//			imageName = imageName.replaceAll("_clone", "");
//		}
//		imageName = imageName + "_contours";
//		ModelImage blur = blur(image, 3);
//
//		Vector3f temp = new Vector3f();
//		//		Vector<Vector3f> inside = new Vector<Vector3f>();
//		//		Vector<Vector3f> pts = new Vector<Vector3f>();
//		for ( int y = 0; y < dimY; y++ )
//			//		int y = dimY/2;
//		{
//			//			inside.clear();
//			Vector3f min = new Vector3f( Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE );
//			Vector3f max = new Vector3f(-Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE );
//			for ( int z = 0; z < dimZ; z++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					if ( blur.getFloat(x,y,z) > threshold)
//						//						if ( mp.getFloat(x,y,z) != 0)
//					{
//						temp.set(x,y,z);
//						min.min(temp);
//						max.max(temp);
//						//						pts.add(new Vector3f(temp));
//					}
//				}
//			}
//			//			System.err.println( y + " " + min.X + " " + max.X );
//			Vector<Vector3f> perimeter = new Vector<Vector3f>();
//			for ( int x = (int)min.X; x <= max.X; x++ )
//			{
//				perimeter.add(new Vector3f(x,y,(int)min.Z));
//			}
//			for ( int z = (int)min.Z; z <= max.Z; z++ )
//			{
//				perimeter.add(new Vector3f(max.X,y,z));
//			}
//			for ( int x = (int)max.X; x >= min.X; x-- )
//			{
//				perimeter.add(new Vector3f(x,y,(int)max.Z));
//			}
//			for ( int z = (int)max.Z; z >= min.Z; z-- )
//			{
//				perimeter.add(new Vector3f(min.X,y,z));
//			}
//
//			//			for ( int z = (int)min.Z; z <= max.Z; z++ )
//			//			{
//			//				for ( int x = (int)min.X; x <= max.X; x++ )
//			//				{
//			//					if ( isInside( x, y, z, min, max, mp) )
//			//					{
//			//						inside.add( new Vector3f(x,y,z));
//			//					}
//			//				}
//			//			}
//			growContour(null, perimeter, blur, contourImage, threshold);
//			//			pts.clear();
//		}
//
//		//		System.err.println( "Saving mp image to : " + directory + contourImage.getImageName() + ".tif" );
//		//		ModelImage.saveImage( contourImage, contourImage.getImageName() + ".tif", directory, false ); 
//		blur.calcMinMax();
//		new ViewJFrameImage((ModelImage) blur);
//	}


//	private static void growContour1(Vector<Vector3f> pts, Vector<Vector3f> perimeter, ModelImage image, ModelImage contourImage )
//	{
//		if ( perimeter.size() < 3 )
//			return;
//
//		short id = (short) image.getVOIs().getUniqueID();
//		VOI wormContours = new VOI(id, "contours", VOI.POLYLINE, (float) Math.random());
//		image.registerVOI(wormContours);
//		VOIContour contour = new VOIContour(true);
//		wormContours.getCurves().add(contour);
//
//		Vector3f min = new Vector3f(Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE );
//		Vector3f max = new Vector3f(-1,-1,-1);
//		for ( int i = 0; i < perimeter.size(); i++ )
//		{
//			min.min(perimeter.elementAt(i));
//			max.max(perimeter.elementAt(i));
//		}
//		float length = min.distance(max)/2f;
//
//		Vector3f center = new Vector3f();
//		for ( int i = 0; i < pts.size(); i++ )
//		{
//			center.add(pts.elementAt(i));
//		}
//		center.scale(1f/(float)pts.size());
//
//		float avgDist = 0;
//		for ( int i = 0; i < pts.size(); i++ )
//		{
//			avgDist += center.distance(pts.elementAt(i));
//		}
//		avgDist /= (float)pts.size();
//
//		final int numPts = 360;
//		for (int i = 0; i < numPts; i++) {
//			final double c = Math.cos(Math.PI * 2.0 * i / numPts);
//			final double s = Math.sin(Math.PI * 2.0 * i / numPts);
//			final Vector3f pos1 = Vector3f.scale((float) (avgDist * c), Vector3f.UNIT_X);
//			final Vector3f pos2 = Vector3f.scale((float) (avgDist * s), Vector3f.UNIT_Z);
//			final Vector3f pos = Vector3f.add(pos1, pos2);
//			pos.add(center);
//			contour.addElement(pos);
//		}
//
//	}
//
//	private static void growContour(Vector<Vector3f> inside, Vector<Vector3f> perimeter, ModelImage image, ModelImage contourImage, float threshold )
//	{
//		if ( perimeter.size() < 3 )
//			return;
//
//		short id = (short) image.getVOIs().getUniqueID();
//		VOI wormContours = new VOI(id, "contours", VOI.POLYLINE, (float) Math.random());
//
//		VOIContour contour = new VOIContour(true);
//		wormContours.getCurves().add(contour);
//		Vector3f center = new Vector3f();
//		Vector3f min = new Vector3f(Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE );
//		Vector3f max = new Vector3f(-1,-1,-1);
//		boolean[] grow = new boolean[perimeter.size()];
//		for ( int i = 0; i < perimeter.size(); i++ )
//		{
//			center.add(perimeter.elementAt(i));
//			contour.add(new Vector3f(perimeter.elementAt(i)));
//			min.min(perimeter.elementAt(i));
//			max.max(perimeter.elementAt(i));
//			grow[i] = true;
//		}
//		center.scale(1f/(float)perimeter.size());
//		float length = min.distance(max)/2f;
//		//		System.err.println( min );
//		//		System.err.println( min );
//
//
//		for ( int step = 1; step < length; step++ )
//		{
//			for ( int i = 0; i < contour.size(); i++ )
//			{
//				if ( !grow[i] )
//				{
//					continue;
//				}
//				Vector3f pt = contour.elementAt(i);
//				if ( image.getFloat( Math.round(pt.X), Math.round(pt.Y), Math.round(pt.Z)) > threshold )
//				{
//					grow[i] = false;
//					continue;
//				}
//				int indexM1 = (i+contour.size()-1)%contour.size();
//				int indexP1 = (i+1)%contour.size();
//				Vector3f ptM1 = contour.elementAt(indexM1);
//				Vector3f ptP1 = contour.elementAt(indexP1);
//				if ( image.getFloat( Math.round(ptM1.X), Math.round(ptM1.Y), Math.round(ptM1.Z)) > threshold )
//				{
//					grow[i] = false;
//					continue;
//				}
//				if ( image.getFloat( Math.round(ptP1.X), Math.round(ptP1.Y), Math.round(ptP1.Z)) > threshold )
//				{
//					grow[i] = false;
//					continue;
//				}
//				if ( pt.distance(ptM1) > 5 || pt.distance(ptP1) > 5 )
//				{
//					grow[i] = false;
//					grow[indexM1] = false;
//					grow[indexP1] = false;
//					continue;
//				}
//				Vector3f dir = Vector3f.sub(center, perimeter.elementAt(i));
//				dir.normalize();
//				pt.add(dir);
//			}	
//			boolean allStop = true;
//			for ( int i = 0; i < contour.size(); i++ )
//			{
//				if ( grow[i] )
//				{
//					allStop = false;
//				}
//			}
//			if ( allStop )
//			{
//				break;
//			}
//		}
//		for ( int i = contour.size() - 1; i >= 0; i-- )
//		{
//			Vector3f pt = contour.elementAt(i);
//			if ( image.getFloat( Math.round(pt.X), Math.round(pt.Y), Math.round(pt.Z)) <= threshold )
//			{
//				contour.remove(i);
//			}
//		}
//		if ( contour.size() > 3 )
//		{
//			image.registerVOI(wormContours);
//		}
//	}
//
//	private static void growContour2(Vector<Vector3f> inside, Vector<Vector3f> perimeter, ModelImage image, ModelImage contourImage )
//	{
//		short id = (short) image.getVOIs().getUniqueID();
//		VOI wormContours = new VOI(id, "contours", VOI.POLYLINE, (float) Math.random());
//
//		int maxIndex = -1;
//		int maxSize = -1;
//		for ( int i = 0; i < inside.size(); i++ )
//		{
//			//		while ( inside.size() > 0 )
//			//		{
//
//			Vector3f startPos = inside.elementAt(i);
//			//			Vector3f startPos = inside.lastElement();
//			//			inside.remove(inside.lastElement());
//			if ( image.getFloat( Math.round(startPos.X), Math.round(startPos.Y), Math.round(startPos.Z)) > 0 )
//			{
//				continue;
//			}
//			VOIContour contour = new VOIContour(true);
//			for ( int j = 0; j < perimeter.size(); j++ )
//			{
//				//				contour.add(perimeter.elementAt(j));
//				Vector3f dir = Vector3f.sub(perimeter.elementAt(j), startPos);
//				int length = (int)dir.normalize();
//				Vector3f newPt = new Vector3f(startPos);
//				boolean addPt = false;
//				for ( int k = 0; k < length; k++ )
//				{
//					newPt.add(dir);
//					if ( image.getFloat( Math.round(newPt.X), Math.round(newPt.Y), Math.round(newPt.Z)) > 0 )
//					{
//						addPt = true;
//						break;
//					}
//					if ( newPt.distance(startPos) >= length)
//					{
//						break;
//					}
//				}
//				if ( addPt && !newPt.isEqual(startPos) && !newPt.isEqual(perimeter.elementAt(j)) && !(newPt.distance(startPos) >= length) )
//				{
//					contour.add(newPt);
//				}
//			}
//			System.err.println( i + " " + (i*100/(float)(inside.size())) + "% " + contour.size() );
//			if ( contour.size() > 3 )
//			{
//				Vector3f center = new Vector3f();
//				//				boolean[] remove = new boolean[contour.size()];
//				//				float[] rescale = new float[contour.size()];
//				for ( int j = 0; j < contour.size(); j++ )
//				{
//					center.add(contour.elementAt(j));
//					//					remove[j] = false;
//				}
//				center.scale(1f/(float)contour.size());
//
//				int checkCount = 1;
//				boolean recheck = true;
//				while ( recheck && (checkCount < contour.size()) )
//				{
//					//					System.err.println(checkCount++ + " " + contour.size() );
//					recheck = false;
//					//					for ( int j = 0; j < contour.size(); j++ )
//					//					{
//					//						remove[j] = false;
//					//					}
//
//					for ( int j = 0; j < contour.size(); j++ )
//					{
//						int index1 = j;
//						int index2 = (j+1)%contour.size();
//						if ( (contour.elementAt(index1).distance(contour.elementAt(index2)) > 10) )
//						{
//							if ( startPos.distance(contour.elementAt(index1)) < startPos.distance(contour.elementAt(index2)) )
//							{
//								//								System.err.println( j + " " + contour.elementAt(j).distance(contour.elementAt((j+1)%contour.size())) + " " + 
//								//										startPos.distance(contour.elementAt(j)) + " " +
//								//										startPos.distance(contour.elementAt((j+1)%contour.size()))
//								//										);
//
//								//								remove[(j+1)%contour.size()] = true;
//								//								rescale[(j+1)%contour.size()] = startPos.distance(contour.elementAt(j));
//								recheck = true;
//
//
//								Vector3f dir = Vector3f.sub( contour.elementAt(index2), startPos);
//								dir.normalize();
//								dir.scale(startPos.distance(contour.elementAt(index1)));
//								dir.add(startPos);
//								contour.elementAt(index2).copy(dir);
//
//								break;
//							}
//						}
//						//					System.err.println( j + " " + contour.elementAt(j).distance(contour.elementAt((j+1)%contour.size())));
//					}
//					//					if ( recheck && (contour.size() > 3) )
//					//					{
//					//						for ( int j = 0; j < contour.size(); j++ )
//					//						{
//					//							if ( remove[j] )
//					//							{
//					//								Vector3f pos1 = contour.elementAt((j+contour.size()-1)%contour.size());
//					//								Vector3f pos2 = contour.elementAt((j+1)%contour.size());
//					//								Vector3f avg = Vector3f.add(pos1, pos2);
//					//								avg.scale(0.5f);
//					//								contour.elementAt(j).copy(avg);
//					//								
//					////								Vector3f dir = Vector3f.sub( contour.elementAt(j), startPos);
//					////								dir.normalize();
//					////								dir.scale(rescale[j]);
//					////								dir.add(startPos);
//					////								contour.elementAt(j).copy(dir);
//					//							}
//					//						}
//					//					}
//				}
//				//				
//				//				
//				//				
//				////				contour.trimPoints(1, true);
//				//////				contour.convexHull()
//				////				for ( int j = inside.size() -1; j >= 0; j-- )
//				////				{
//				////					Vector3f pos = inside.elementAt(j);
//				////					if ( contour.contains( pos.X, pos.Y, pos.Z ) )
//				////					{
//				////						inside.remove(j);
//				////					}
//				////				}
//				int insideCount = 0;
//				for ( int j = 0; j < inside.size(); j++ )
//				{
//					Vector3f pos = inside.elementAt(j);
//					if ( contour.contains( pos.X, pos.Y, pos.Z ) )
//					{
//						insideCount++;
//					}
//				}
//				if ( insideCount > maxSize )
//				{
//					maxIndex = i;
//					maxSize = insideCount;
//				}
//				wormContours.getCurves().add(contour);
//				//				System.err.println( i + " " + (i*100/(float)(inside.size())) + "% " + contour.size() + " " + insideCount );
//			}
//		}
//		System.err.println( maxIndex + " " + maxSize );
//		if ( wormContours.getCurves().size() > 0 )
//		{
//			VOIContour contour = (VOIContour) wormContours.getCurves().remove(maxIndex);
//			wormContours.getCurves().removeAllElements();
//			wormContours.getCurves().add(contour);
//			//			System.err.println( "WormContours " + wormContours.getCurves().size() );
//			image.registerVOI(wormContours);
//		}
//	}
//
//	private static boolean isInside( int xStart, int yStart, int zStart, Vector3f min, Vector3f max, ModelImage image )
//	{		
//		int count = 0;
//		for ( int x = xStart; x >= min.X; x-- )
//		{
//			if ( image.getFloat(x,yStart,zStart) > 0 )
//			{
//				count++;
//				break;
//			}
//		}
//		for ( int x = xStart; x <= max.X; x++ )
//		{
//			if ( image.getFloat(x,yStart,zStart) > 0 )
//			{
//				count++;
//				break;
//			}
//		}
//		for ( int y = yStart; y >= min.Y; y-- )
//		{
//			if ( image.getFloat(xStart,y,zStart) > 0 )
//			{
//				count++;
//				break;
//			}
//		}
//		for ( int y = yStart; y <= max.Y; y++ )
//		{
//			if ( image.getFloat(xStart,y,zStart) > 0 )
//			{
//				count++;
//				break;
//			}
//		}
//		for ( int z = zStart; z >= min.Z; z-- )
//		{
//			if ( image.getFloat(xStart,yStart,z) > 0 )
//			{
//				count++;
//				break;
//			}
//		}
//		for ( int z = zStart; z <= max.Z; z++ )
//		{
//			if ( image.getFloat(xStart,yStart,z) > 0 )
//			{
//				count++;
//				break;
//			}
//		}
//
//		return (count >= 4);
//	}
//
//	private static void center( ModelImage skinSurface, ModelImage output )
//	{
//		final int dimX = skinSurface.getExtents().length > 0 ? skinSurface.getExtents()[0] : 1;
//		final int dimY = skinSurface.getExtents().length > 1 ? skinSurface.getExtents()[1] : 1;
//		final int dimZ = skinSurface.getExtents().length > 2 ? skinSurface.getExtents()[2] : 1; 
//
//		BitSet insideMask = new BitSet(dimX*dimY*dimZ);
//		Vector3f startPt = null;
//		for ( int z = dimZ/2; (startPt == null) && (z < dimZ); z++ )
//		{
//			for ( int y = dimY/2; (startPt == null) && (y < dimY); y++ )
//			{
//				for ( int x = dimX/2; (startPt == null) && (x < dimX); x++ )
//				{
//					if ( skinSurface.getFloatC(dimX/2, dimY/2, dimZ/2, 2) > 0 )
//					{
//						startPt = new Vector3f(x,y,z);
//					}
//				}
//			}
//		}
//		if ( startPt == null )
//		{
//			for ( int z = dimZ/2; (startPt == null) && (z >= 0); z-- )
//			{
//				for ( int y = dimY/2; (startPt == null) && (y >= 0); y-- )
//				{
//					for ( int x = dimX/2; (startPt == null) && (x >= 0); x-- )
//					{
//						if ( skinSurface.getFloatC(dimX/2, dimY/2, dimZ/2, 2) > 0 )
//						{
//							startPt = new Vector3f(x,y,z);
//						}
//					}
//				}
//			}			
//		}
//
//		if ( startPt == null )
//		{
//			return;
//		}
//
//		Vector<Vector3f> surfacePts = new Vector<Vector3f>();
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					if ( skinSurface.getFloatC(x, y, z, 2) > 0 )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						insideMask.set(index);
//					}
//					if ( skinSurface.getFloatC(x, y, z, 1) > 0 )
//					{
//						surfacePts.add( new Vector3f(x,y,z) );
//					}
//				}
//			}
//		}
//		while ( insideMask.cardinality() > 0 )
//		{
//			for ( int i = 0; i < surfacePts.size(); i++ )
//			{
//
//			}
//		}
//	}
//
//	private static void inside( ModelImage skinSurface, ModelImage output )
//	{
//		final int dimX = skinSurface.getExtents().length > 0 ? skinSurface.getExtents()[0] : 1;
//		final int dimY = skinSurface.getExtents().length > 1 ? skinSurface.getExtents()[1] : 1;
//		final int dimZ = skinSurface.getExtents().length > 2 ? skinSurface.getExtents()[2] : 1; 
//
//		BitSet xMask = new BitSet(dimX*dimY*dimZ);
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				boolean surfaceFound = false;
//				boolean insideFound = false;
//				int minX = dimX;
//				for ( int x = 0; x < dimX; x++ )
//				{
//					float val = skinSurface.getFloatC(x, y, z, 1);
//					if ( (val > 0) && !insideFound )
//					{
//						surfaceFound = true;
//					}
//					else if ( (val > 0) && insideFound )
//					{
//						surfaceFound = true;
//						insideFound = false;
//					}
//					if ( (val == 0) && surfaceFound )
//					{
//						insideFound = true;
//						if ( minX == dimX )
//						{
//							minX = x;
//						}
//					}
//					if ( (val == 0) && insideFound )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						xMask.set(index);
//					}
//				}
//				surfaceFound = false;
//				insideFound = false;
//				int maxX = -1;
//				for ( int x = dimX - 1; x >= 0; x-- )
//				{
//					float val = skinSurface.getFloatC(x, y, z, 1);
//					if ( (val > 0) && !insideFound )
//					{
//						surfaceFound = true;
//					}
//					else if ( (val > 0) && insideFound )
//					{
//						surfaceFound = true;
//						insideFound = false;
//					}
//					if ( (val == 0) && surfaceFound )
//					{
//						insideFound = true;
//						if ( maxX == -1 )
//						{
//							maxX = x;
//						}
//					}
//					if ( (val == 0) && insideFound )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						xMask.set(index);
//					}
//				}
//				for ( int x = 0; x < dimX; x++ )
//				{
//					if ( x < minX )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						xMask.set(index, false);
//					}
//					else if ( x > maxX )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						xMask.set(index, false);
//					}
//				}
//			}
//		}
//
//
//		BitSet yMask = new BitSet(dimX*dimY*dimZ);
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int x = 0; x < dimX; x++ )
//			{
//				boolean surfaceFound = false;
//				boolean insideFound = false;
//				int minY = dimY;
//				for ( int y = 0; y < dimY; y++ )
//				{
//					float val = skinSurface.getFloatC(x, y, z, 1);
//					if ( (val > 0) && !insideFound )
//					{
//						surfaceFound = true;
//					}
//					else if ( (val > 0) && insideFound )
//					{
//						surfaceFound = true;
//						insideFound = false;
//					}
//					if ( (val == 0) && surfaceFound )
//					{
//						insideFound = true;
//						if ( minY == dimY )
//						{
//							minY = y;
//						}
//					}
//					if ( (val == 0) && insideFound )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						yMask.set(index);
//					}
//				}
//				surfaceFound = false;
//				insideFound = false;
//				int maxY = -1;
//				for ( int y = dimY - 1; y >= 0; y-- )
//				{
//					float val = skinSurface.getFloatC(x, y, z, 1);
//					if ( (val > 0) && !insideFound )
//					{
//						surfaceFound = true;
//					}
//					else if ( (val > 0) && insideFound )
//					{
//						surfaceFound = true;
//						insideFound = false;
//					}
//					if ( (val == 0) && surfaceFound )
//					{
//						insideFound = true;
//						if ( maxY == -1 )
//						{
//							maxY = y;
//						}
//					}
//					if ( (val == 0) && insideFound )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						yMask.set(index);
//					}
//				}
//				for ( int y = 0; y < dimY; y++ )
//				{
//					if ( y < minY )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						yMask.set(index, false);
//					}
//					else if ( y > maxY )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						yMask.set(index, false);
//					}
//				}
//			}
//		}
//
//
//		BitSet zMask = new BitSet(dimX*dimY*dimZ);
//		for ( int x = 0; x < dimX; x++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				boolean surfaceFound = false;
//				boolean insideFound = false;
//				int minZ = dimZ;
//				for ( int z = 0; z < dimZ; z++ )
//				{
//					float val = skinSurface.getFloatC(x, y, z, 1);
//					if ( (val > 0) && !insideFound )
//					{
//						surfaceFound = true;
//					}
//					else if ( (val > 0) && insideFound )
//					{
//						surfaceFound = true;
//						insideFound = false;
//					}
//					if ( (val == 0) && surfaceFound )
//					{
//						insideFound = true;
//						if ( minZ == dimZ )
//						{
//							minZ = z;
//						}
//					}
//					if ( (val == 0) && insideFound )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						zMask.set(index);
//					}
//				}
//				surfaceFound = false;
//				insideFound = false;
//				int maxZ = -1;
//				for ( int z = dimZ - 1; z >= 0; z-- )
//				{
//					float val = skinSurface.getFloatC(x, y, z, 1);
//					if ( (val > 0) && !insideFound )
//					{
//						surfaceFound = true;
//					}
//					else if ( (val > 0) && insideFound )
//					{
//						surfaceFound = true;
//						insideFound = false;
//					}
//					if ( (val == 0) && surfaceFound )
//					{
//						insideFound = true;
//						if ( maxZ == -1 )
//						{
//							maxZ = z;
//						}
//					}
//					if ( (val == 0) && insideFound )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						zMask.set(index);
//					}
//				}
//				for ( int z = 0; z < dimZ; z++ )
//				{
//					if ( z < minZ )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						zMask.set(index, false);
//					}
//					else if ( z > maxZ )
//					{
//						int index = z*dimY*dimX + y*dimX + x;
//						zMask.set(index, false);
//					}
//				}
//			}
//		}
//		xMask.and(yMask);
//		xMask.and(zMask);
//
//
//		//		BitSet xy = new BitSet(dimX*dimY*dimZ);
//		//		xy.or(xMask);
//		//		xy.and(yMask);
//		//		BitSet xz = new BitSet(dimX*dimY*dimZ);
//		//		xz.or(xMask);
//		//		xz.and(zMask);
//		//		BitSet zy = new BitSet(dimX*dimY*dimZ);
//		//		zy.or(zMask);
//		//		zy.and(yMask);
//		//		
//		//		xMask.clear();
//		//		xMask.or(xy);
//		//		xMask.or(xz);
//		//		xMask.or(zy);
//
//
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					int index = z*dimY*dimX + y*dimX + x;
//					if ( xMask.get(index) )
//					{
//						if ( output.isColorImage() )
//						{
//							output.setC(x, y, z, 2, 255);
//						}
//						else
//						{
//							output.set(x, y, z, 100);
//						}
//					}
//				}
//			}
//		}
//	}


	public static ModelImage[] outside( ModelImage blur, float threshold )
	{
		final int dimX = blur.getExtents().length > 0 ? blur.getExtents()[0] : 1;
		final int dimY = blur.getExtents().length > 1 ? blur.getExtents()[1] : 1;
		final int dimZ = blur.getExtents().length > 2 ? blur.getExtents()[2] : 1; 

		String imageName = blur.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_" + threshold;
		final ModelImage outside = new ModelImage(ModelStorageBase.INTEGER, blur.getExtents(), imageName);
		JDialogBase.updateFileInfo(blur, outside);

		final ModelImage inside = new ModelImage(ModelStorageBase.INTEGER, blur.getExtents(), imageName);
		JDialogBase.updateFileInfo(blur, inside);


		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				int xStart = dimX;
				int xEnd = -1;
				for ( int x = 0; x < dimX; x++ )
				{
					float val = blur.getFloat(x, y, z);
					if ( val >= threshold )
					{
						outside.set(x, y, z, 1);
						xStart = x;
						break;
					}
				}
				for ( int x = dimX - 1; x >= 0; x-- )
				{
					float val = blur.getFloat(x, y, z);
					if ( val >= threshold )
					{
						outside.set(x, y, z, 1);
						xEnd = x;
						break;
					}
				}
				if ( xEnd > xStart )
				{
					for ( int x = xStart+1; x < xEnd; x++ )
					{
						inside.set(x, y, z, 1);
					}
				}
			}
		}


		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				int yStart = dimY;
				int yEnd = -1;
				for ( int y = 0; y < dimY; y++ )
				{
					float val = blur.getFloat(x, y, z);
					if ( val >= threshold )
					{
						outside.set(x, y, z, 1);
						yStart = y;
						break;
					}
				}
				for ( int y = dimY - 1; y >= 0; y-- )
				{
					float val = blur.getFloat(x, y, z);
					if ( val >= threshold )
					{
						outside.set(x, y, z, 1);
						yEnd = y;
						break;
					}
				}
				if ( yEnd > yStart )
				{
					for ( int y = yStart +1; y < yEnd; y++ )
					{
						int value = inside.getInt(x,y,z);
						inside.set(x, y, z, value+1);
					}
				}
			}
		}

		for ( int x = 0; x < dimX; x++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				int zStart = dimZ;
				int zEnd = -1;
				for ( int z = 0; z < dimZ; z++ )
				{
					float val = blur.getFloat(x, y, z);
					if ( val >= threshold )
					{
						outside.set(x, y, z, 1);
						zStart = z;
						break;
					}
				}
				for ( int z = dimZ - 1; z >= 0; z-- )
				{
					float val = blur.getFloat(x, y, z);
					if ( val >= threshold )
					{
						outside.set(x, y, z, 1);
						zEnd = z;
						break;
					}
				}
				if ( zEnd > zStart )
				{
					for ( int z = zStart +1; z < zEnd; z++ )
					{
						int value = inside.getInt(x,y,z);
						inside.set(x, y, z, value+1);
					}
				}
			}
		}

		for ( int i = 0; i < inside.getDataSize(); i++ )
		{
			int value = inside.getInt(i);
			if ( value >= 3 )
			{
				inside.set(i, 1);
			}
			else
			{
				inside.set(i, 0);
			}
		}

		return new ModelImage[]{outside,inside};
	}



//	private static void fillMask( BitSet inside, BitSet visited, BitSet connected, Vector<Vector3f> seeds, int dimX, int dimY, int dimZ )
//	{
//		while ( seeds.size() > 0 )
//		{
//			Vector3f start = seeds.remove(0);
//			int x = (int) start.X;
//			int y = (int) start.Y;
//			int z = (int) start.Z;
//			int startIndex = z*dimY*dimX + y*dimX + x;
//			connected.set(startIndex);
//			visited.set(startIndex);
//
//			for ( int z1 = Math.max(0, z - 1); z1 <= Math.min(z+1, dimZ-1); z1++ )
//			{
//				for ( int y1 = Math.max(0, y - 1); y1 <= Math.min(y+1, dimY-1); y1++ )
//				{
//					for ( int x1 = Math.max(0, x - 1); x1 <= Math.min(x+1, dimX-1); x1++ )
//					{
//						int index = z1*dimY*dimX + y1*dimX + x1;
//						if ( !visited.get(index) && !connected.get(index) && inside.get(index) )
//						{
//							visited.set(index);
//							seeds.add( new Vector3f(x1,y1,z1) );
//						}
//					}
//				}
//			}
//		}
//	}

	private static void fillMask( float[] image, BitSet visited, BitSet connected, Vector<Vector3f> seeds, 
			int dimX, int dimY, int dimZ, boolean diagonals, Vector<Vector3f> insidePts, Vector3f minV, Vector3f maxV )
	{
		while ( seeds.size() > 0 )
		{
			Vector3f start = seeds.remove(0);
			insidePts.add(start);
			minV.min(start);
			maxV.max(start);

			int x = (int) start.X;
			int y = (int) start.Y;
			int z = (int) start.Z;
			int startIndex = z*dimY*dimX + y*dimX + x;
			connected.set(startIndex);
			visited.set(startIndex);

			if ( diagonals )
			{
				for ( int y1 = Math.max(0, y - 1); y1 <= Math.min(y+1, dimY-1); y1++ )
				{
					for ( int x1 = Math.max(0, x - 1); x1 <= Math.min(x+1, dimX-1); x1++ )
					{
						int index = y1*dimX + x1;
						float value = image[index];
						if ( value == 0 )
						{
							visited.set(index);
						}
						else if ( !visited.get(index) && !connected.get(index) && (value == 255) )
						{
							visited.set(index);
							seeds.add( new Vector3f(x1,y1,0) );
						}
					}
				}
			}
			else
			{

				for ( int x1 = Math.max(0, x - 1); x1 <= Math.min(x+1, dimX-1); x1++ )
				{
					int index = y*dimX + x1;
					float value = image[index];
					if ( value == 0 )
					{
						visited.set(index);
					}
					else
					{
						value = image[index];
						if ( !visited.get(index) && !connected.get(index) && (value == 255) )
						{
							visited.set(index);
							seeds.add( new Vector3f(x1,y,0) );
						}
					}
				}

				for ( int y1 = Math.max(0, y - 1); y1 <= Math.min(y+1, dimY-1); y1++ )
				{
					int index = y1*dimX + x;
					float value = image[index];
					if ( value == 0 )
					{
						visited.set(index);
					}
					else
					{
						value = image[index];
						if ( !visited.get(index) && !connected.get(index) && (value == 255) )
						{
							visited.set(index);
							seeds.add( new Vector3f(x,y1,0) );
						}
					}
				}

			}
		}
	}


	private static void fillMask( float[] image, BitSet visited, BitSet connected, Vector<Vector3f> seeds, Vector<Vector3f> boundary, 
			int dimX, int dimY, int dimZ, int width )
	{
		while ( seeds.size() > 0 )
		{
			Vector3f start = seeds.remove(0);
			int x = (int) start.X;
			int y = (int) start.Y;
			int z = (int) start.Z;
			int startIndex = z*dimY*dimX + y*dimX + x;
			connected.set(startIndex);
			visited.set(startIndex);

			boolean allZero = true;
			for ( int y1 = Math.max(0, y - width); y1 <= Math.min(y+width, dimY-1) && allZero; y1++ )
			{
				for ( int x1 = Math.max(0, x - width); x1 <= Math.min(x+width, dimX-1) && allZero; x1++ )
				{
					int index = y1*dimX + x1;
					float value = image[index];
					if ( value != 0 )
					{
						allZero = false;
						boundary.add(start);
					}
				}
			}
			if ( allZero )
			{
				for ( int y1 = Math.max(0, y - width); y1 <= Math.min(y+width, dimY-1) && allZero; y1++ )
				{
					for ( int x1 = Math.max(0, x - width); x1 <= Math.min(x+width, dimX-1) && allZero; x1++ )
					{
						int index = y1*dimX + x1;
						if ( !visited.get(index) && !connected.get(index) )
						{
							visited.set(index);
							seeds.add( new Vector3f(x1,y1,0) );
						}
					}
				}
			}
		}
	}


	public static int reduceDuplicates( ModelImage image, Vector<Vector3f> tempSeamCells )
	{
		Vector3f negCenter = new Vector3f(-1,-1,-1);
		int count = 0;
		for ( int i = 0; i < tempSeamCells.size(); i++ )
		{
			if ( !tempSeamCells.elementAt(i).equals(negCenter) )
			{
				count++;
			}
		}
		while ( count > 22 )
		{
			int closestI = -1;
			int closestJ = -1;
			float minDistance = Float.MAX_VALUE;
			for ( int i = 0; i < tempSeamCells.size(); i++ )
			{
				if ( tempSeamCells.elementAt(i).equals(negCenter) )
					continue;
				for ( int j = i+1; j < tempSeamCells.size(); j++ )
				{
					if ( tempSeamCells.elementAt(j).equals(negCenter) )
						continue;

					float distance = tempSeamCells.elementAt(i).distance(tempSeamCells.elementAt(j));
					if ( distance < minDistance )
					{
						minDistance = distance;
						closestI = i;
						closestJ = j;
					}
				}
			}
			if ( (closestI != -1) && (closestJ != -1) )
			{
				tempSeamCells.elementAt(closestI).add(tempSeamCells.elementAt(closestJ));
				tempSeamCells.elementAt(closestI).scale(0.5f);
				tempSeamCells.elementAt(closestJ).copy(negCenter);
				count--;
			}
		}

		return count;
	}


	public static int reduceDuplicates( ModelImage image, Vector<Vector3f> tempSeamCells, int shortDistance, int longDistance, boolean deleteSingletons )
	{

		Vector3f negCenter = new Vector3f(-1,-1,-1);
		for ( int j = 0; j < tempSeamCells.size(); j++ )
		{
			if ( !tempSeamCells.elementAt(j).equals(negCenter) )
			{
				Vector3f newCenter = new Vector3f(tempSeamCells.elementAt(j));
				int count = 1;
				for ( int k = j+1; k < tempSeamCells.size(); k++ )
				{
					if ( !tempSeamCells.elementAt(k).equals(negCenter) )
					{
						float distance = tempSeamCells.elementAt(j).distance(tempSeamCells.elementAt(k));
						if ( distance < shortDistance )
						{
							newCenter.add(tempSeamCells.elementAt(k));
							tempSeamCells.elementAt(k).copy(negCenter);
							count++;
						}
						else if ( distance < longDistance )
						{
							boolean merge = true;
							Vector3f dir = Vector3f.sub( tempSeamCells.elementAt(k), tempSeamCells.elementAt(j) );
							distance = dir.normalize();
							Vector3f start = new Vector3f( tempSeamCells.elementAt(j) );
							Vector3f end = new Vector3f( tempSeamCells.elementAt(k) );
							float valueStart = image.getFloatTriLinearBounds( start.X, start.Y, start.Z );
							float valueEnd = image.getFloatTriLinearBounds( end.X, end.Y, end.Z );
							for ( int p = 1; p < distance; p++ )
							{
								start.add(dir);
								float value = image.getFloatTriLinearBounds( start.X, start.Y, start.Z );
								if ( (value < valueStart) && (value < valueEnd) )
								{
									merge = false;
								}
							}
							if ( merge )
							{
								newCenter.add(tempSeamCells.elementAt(k));
								tempSeamCells.elementAt(k).copy(negCenter);
								count++;								
							}							
						}
						//						System.err.println(j + "   " + k + "   " + distance );
					}
				}
				if ( count > 1 )
				{
					newCenter.scale(1f/count);
					tempSeamCells.elementAt(j).copy(newCenter);
				}
				else if ( deleteSingletons )
				{
					tempSeamCells.elementAt(j).copy(negCenter);
				}
			}
		}
		int count = 0;
		for ( int j = 0; j < tempSeamCells.size(); j++ )
		{
			if ( !tempSeamCells.elementAt(j).equals(negCenter) )
			{
				count++;
			}
		}
		return count;
	}



	public static void saveAllVOIsTo(String voiDir, final ModelImage image)
	{		
		final ViewVOIVector VOIs = image.getVOIs();
		if ( VOIs == null )
		{
			return;
		}

		final int nVOI = VOIs.size();
		if ( nVOI <= 0 )
		{
			return;
		}
		try {
			final File voiFileDir = new File(voiDir);

			if (voiFileDir.exists() && voiFileDir.isDirectory()) {
				final String[] list = voiFileDir.list();
				for (int i = 0; i < list.length; i++) {
					final File lrFile = new File(voiDir + list[i]);
					lrFile.delete();
				}
			} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { 
			} else { // voiFileDir does not exist
				voiFileDir.mkdir();
			}

			for (int i = 0; i < nVOI; i++) {
				if ( VOIs.VOIAt(i).getCurves().size() <= 0 )
				{
					continue;
				}
				if (VOIs.VOIAt(i).getCurveType() != VOI.ANNOTATION) {
					final FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".xml", voiDir, image);
					fileVOI.writeXML(VOIs.VOIAt(i), true, true);
				} else {
					final FileVOI fileVOI = new FileVOI(VOIs.VOIAt(i).getName() + ".lbl", voiDir, image);
					fileVOI.writeAnnotationInVoiAsXML(VOIs.VOIAt(i).getName(), true);
				}
			}

		} catch (final IOException error) {
			MipavUtil.displayError("Error writing all VOIs to " + voiDir + ": " + error);
		}

	} // end saveAllVOIsTo()

	public static void saveAnnotations( ModelImage image, Vector<Vector3f> positions, Color color )
	{
		if ( positions.size() <= 0 )
			return;
		VOI annotations = new VOI( (short)1, "SeamCells", VOI.ANNOTATION, 0 );
		for ( int i = 0; i < positions.size(); i++ )
		{
			VOIText text = new VOIText();
			text.setText( "A" + (i+1) );
			text.setColor( color );
			text.add( positions.elementAt(i) );
			text.add( positions.elementAt(i) );
			text.setUseMarker(false);
			annotations.getCurves().add(text);
		}
		annotations.setColor(color);
		image.registerVOI(annotations);
	}

//	public static Vector<Vector3f> segmentImage( final ModelImage image, float cutOff )
//	{
//		String imageName = image.getImageName();
//		if (imageName.contains("_clone")) {
//			imageName = imageName.replaceAll("_clone", "");
//		}
//		if (imageName.contains("_laplace")) {
//			imageName = imageName.replaceAll("_laplace", "");
//		}
//		if (imageName.contains("_gblur")) {
//			imageName = imageName.replaceAll("_gblur", "");
//		}
//		imageName = imageName + "_segmentation";
//		ModelImage segmentationImage = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), imageName);
//		JDialogBase.updateFileInfo(image, segmentationImage);   
//
//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;  	
//
//		Vector<Vector3f> seedList = new Vector<Vector3f>();
//		int count = 1;
//		Vector3f seed = new Vector3f();
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					if ( image.getFloat(x, y, z) >= cutOff )
//					{
//						seed.set(x, y, z);
//						seedList.add(seed);
//						int numFilled = fill( image, cutOff, cutOff+1, seedList, segmentationImage, count);
//						if ( numFilled > 0 )
//						{
//							count++;
//						}
//					}
//				}
//			}
//		}
//		segmentationImage.calcMinMax();
//
//		count = (int) segmentationImage.getMax();
//		Vector3f[] seamCells = new Vector3f[count];
//		int[] counts = new int[count];
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					int id = segmentationImage.getInt(x, y, z);
//					if ( id > 0 )
//					{
//						id--;
//						if ( seamCells[id] == null )
//						{
//							seamCells[id] = new Vector3f();
//							counts[id] = 0;
//						}
//						seamCells[id].add(x,y,z);
//						counts[id]++;
//					}
//				}
//			}
//		}
//
//		Vector<Vector3f> annotations = new Vector<Vector3f>();
//
//		for ( int i = 0; i < seamCells.length; i++ )
//		{
//			if ( counts[i] > 1 )
//			{
//				System.err.println( i + "   " + counts[i] );
//				seamCells[i].scale( 1f/counts[i]);
//				annotations.add(seamCells[i]);
//			}
//
//		}
//		return annotations;
//	}




//	public static ModelImage segmentNose(ModelImage image, Vector3f pt, boolean display)
//	{
//
//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
//
//		int cubeHalf = 7/2;
//		int zStart = (int)Math.max(0, Math.min( dimZ -1, pt.Z - cubeHalf) );
//		int zEnd = (int)Math.max(0, Math.min( dimZ -1, pt.Z + cubeHalf) );
//		int yStart = (int)Math.max(0, Math.min( dimY -1, pt.Y - cubeHalf) );
//		int yEnd = (int)Math.max(0, Math.min( dimY -1, pt.Y + cubeHalf) );
//		int xStart = (int)Math.max(0, Math.min( dimX -1, pt.X - cubeHalf) );
//		int xEnd = (int)Math.max(0, Math.min( dimX -1, pt.X + cubeHalf) );
//
//		float avg = 0;
//		int count = 0;
//		for ( int z = zStart; z <= zEnd; z++ )
//		{
//			for ( int y = yStart; y <= yEnd; y++ )
//			{
//				for ( int x = xStart; x <= xEnd; x++ )
//				{
//					float value = image.getFloat(x,y,z);
//					avg += value;
//					count++;
//				}
//			}    					
//		}
//
//		avg /= count;
//
//
//
//
//		String imageName = image.getImageName();
//		if (imageName.contains("_clone")) {
//			imageName = imageName.replaceAll("_clone", "");
//		}
//		imageName = imageName + "_nose";
//		final ModelImage resultImage = new ModelImage(image.getType(), image.getExtents(), imageName);
//		JDialogBase.updateFileInfo(image, resultImage);
//
//		float value = image.getFloatTriLinearBounds( pt.X, pt.Y, pt.Z );
//		Vector<Vector3f> seedList = new Vector<Vector3f>();
//		seedList.add(pt);
//		//		min = 1.5f*min;
//		////		min = (float) Math.min( value - 1, 0.25 * (value + min/2f) );
//		//		max = (float) Math.max( max + 1, 0.95 * (max + image.getMax()/2f) );
//
//		float max = 2 * (avg + value)/2f;
//		float min = 0.5f*(avg + value)/2f;
//		float minLimit = 1;
//		float maxLimit = max;
//		System.err.println( "SegmentNose " + image.getMin() + " " + image.getMax() + " " + avg + " " + value + " " + min + " " + max );
//		int numFilled = fill( image, min, max, seedList, resultImage, 1);
//		count = 0;
//		float prevMin = -1;
//		int prevFilled = -1;
//		while ( ((numFilled < 60000) || (numFilled > 100000)) && (count < 100))
//		{
//			count++;
//			System.err.println( "SegmentNose " + numFilled + " " + min + " " + max + " " + count );
//			if ( numFilled < 60000 )
//			{
//				maxLimit = min;
//			}
//			else if ( numFilled > 100000 )
//			{
//				minLimit = min;
//			}
//			if ( (prevFilled == numFilled) || (prevMin == min) )
//			{
//				max = 0.9f * max;
//				minLimit = 1;
//				maxLimit = max;
//			}
//			prevMin = min;
//			min = (minLimit + maxLimit)/2f;
//			resultImage.setAll(0.0f);
//			seedList.clear();
//			seedList.add(pt);
//			prevFilled = numFilled;
//			numFilled = fill( image, min, max, seedList, resultImage, 1);
//		}
//		resultImage.calcMinMax();
//		System.err.println( "SegmentNose " + numFilled + " " + min + " " + max + " " + value );
//		if ( display )
//		{
//			new ViewJFrameImage(resultImage);
//		}
//		return resultImage;
//	}

	//	public static Vector<Vector3f> segmentSeam(ModelImage image, int minRadius, int maxRadius, String outputDir)
	//	{
	//		ModelImage seamCellImage = new ModelImage( ModelStorageBase.FLOAT, image.getExtents(), "seamCellImage" );	
	//		seamCellImage.setImageName("seamCellImage");
	//		JDialogBase.updateFileInfo(image, seamCellImage);
	//		
	//		
	//		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
	//		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
	//		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
	//		
	//		float[] targetP = new float[2];
	//		float targetPercentage = .99f;
	//		WormSegmentation.estimateHistogram( image, targetP, targetPercentage );
	////		System.err.println( targetPercentage + " % " + targetP[0] + " " + targetP[1] );						
	//
	//		BitSet mask = new BitSet(dimX*dimY*dimZ);
	//
	//		// segment the seam cells:
	//		Vector<Vector3f> foundNuclei = WormSegmentation.findMarkers(image, seamCellImage, mask, targetP[0], (float) image.getMax(), minRadius, maxRadius );
	//
	//		ModelImage.saveImage(seamCellImage, seamCellImage.getImageName() + ".xml", outputDir, false);
	//		seamCellImage.disposeLocal(false);
	//		return foundNuclei;
	//	}

	private float intensityMin;
	public float getMinSegmentationIntensity()
	{
		return intensityMin;
	}

	public boolean segmentSeamNew(ModelImage image, VOIContour centerList, VOI clusterList, int minRadius, int maxRadius, String outputDir, int numCells, boolean needTenth)
	{
		ModelImage wormBlur = WormSegmentation.blur(image, minRadius);
		ModelImage temp = WormSegmentation.blur(image, (int) Math.max(minRadius/2f, minRadius - 2));

		for ( int i = 0; i < wormBlur.getDataSize(); i++ )
		{
			wormBlur.set(i, Math.max(0, temp.getFloat(i) - wormBlur.getFloat(i) ));
		}
		wormBlur.calcMinMax();

		float[] finalThreshold = new float[1];
		boolean pass = WormSegmentation.segmentSeamThreshold(wormBlur, temp, centerList, clusterList, finalThreshold,
				1f, (float)(wormBlur.getMax()-1), 5f, (float)wormBlur.getMax(), numCells, minRadius, needTenth );


		System.err.println("clusters : " + centerList.size() + " " + pass );

		wormBlur.disposeLocal(false);
		wormBlur = null;
		temp.disposeLocal(false);
		temp = null;


		return pass;
	}


	public static float testVariance( ModelImage image, Vector3f pt, int cubeSize)
	{
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 

		int cubeHalf = cubeSize/2;
		int count = 0;
		double meanIntensity = 0;
		int zStart = (int)Math.max(0, Math.min( dimZ -1, pt.Z - cubeHalf) );
		int zEnd = (int)Math.max(0, Math.min( dimZ -1, pt.Z + cubeHalf) );
		int yStart = (int)Math.max(0, Math.min( dimY -1, pt.Y - cubeHalf) );
		int yEnd = (int)Math.max(0, Math.min( dimY -1, pt.Y + cubeHalf) );
		int xStart = (int)Math.max(0, Math.min( dimX -1, pt.X - cubeHalf) );
		int xEnd = (int)Math.max(0, Math.min( dimX -1, pt.X + cubeHalf) );

		Vector3f test = new Vector3f();
		for ( int z = zStart; z <= zEnd; z++ )
		{
			for ( int y = yStart; y <= yEnd; y++ )
			{
				for ( int x = xStart; x <= xEnd; x++ )
				{
					test.set(x,y,z);
					if ( test.distance(pt) <= cubeHalf )
					{
						float value = image.getFloat(x,y,z);
						meanIntensity += value;
						count++;
					}
				}
			}    					
		}
		meanIntensity /= count;

		count = 0;
		float meanVariance = 0;
		for ( int z = zStart; z <= zEnd; z++ )
		{
			for ( int y = yStart; y <= yEnd; y++ )
			{
				for ( int x = xStart; x <= xEnd; x++ )
				{
					test.set(x,y,z);
					if ( test.distance(pt) <= cubeHalf )
					{
						float value = image.getFloat(x,y,z);
						meanVariance += ((value - meanIntensity) * (value - meanIntensity));
						count++;
					}
				}
			}
		}    			
		meanVariance /= count;
		return meanVariance;
	}

	/**
	 * Returns the amount of correction which should be applied to the z-direction sigma (assuming that correction is
	 * requested).
	 * 
	 * @return the amount to multiply the z-sigma by to correct for resolution differences
	 */
	protected static float getCorrectionFactor(final ModelImage image) {
		final int index = image.getExtents()[2] / 2;
		final float xRes = image.getFileInfo(index).getResolutions()[0];
		final float zRes = image.getFileInfo(index).getResolutions()[2];

		return xRes / zRes;
	}



//	private static boolean isSphereShell( Vector3f pt, ModelImage image, int diameterMin, int diameterMax, TriMesh sphere )
//	{
//		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
//
//		int iX = (int)Math.max(0, Math.min( dimX -1, pt.X));
//		int iY = (int)Math.max(0, Math.min( dimY -1, pt.Y));
//		int iZ = (int)Math.max(0, Math.min( dimZ -1, pt.Z));
//		float value = image.getFloat( iX, iY, iZ );
//		if ( value != 0 )
//		{
//			return false;
//		}
//		Vector3f dir = new Vector3f();
//		Vector3f pos = new Vector3f();
//		VertexBuffer vBuffer = sphere.VBuffer;
//		int count = 0;
//		int totalCount = 0;
//		for ( int i = 0; i < vBuffer.GetVertexQuantity(); i++ )
//		{
//			vBuffer.GetPosition3(i, dir);
//			dir.normalize();
//			boolean centerFound = false;
//			boolean edgeFound = false;
//			boolean outsideFound = false;
//			for ( int z = 0; z <= diameterMax; z++ )
//			{
//				pos.copy(dir);
//				pos.scale(z);
//				pos.add(pt);
//				iX = (int)Math.max(0, Math.min( dimX -1, pos.X));
//				iY = (int)Math.max(0, Math.min( dimY -1, pos.Y));
//				iZ = (int)Math.max(0, Math.min( dimZ -1, pos.Z));
//				value = image.getFloat( iX, iY, iZ );
//				if ( (value == image.getMin()) && !centerFound )
//				{
//					centerFound = true;
//				}
//				if ( (value > image.getMin()) && !centerFound )
//				{
//					break;
//				}
//				if ( (value > image.getMin()) && centerFound && !edgeFound )
//				{
//					//					if ( z < diameterMin )
//					//					{
//					//						break;
//					//					}
//					edgeFound = true;
//				}
//				if ( (value == image.getMin()) && centerFound && edgeFound && !outsideFound )
//				{
//					outsideFound = true;
//					break;
//				}
//			}
//			if ( centerFound && edgeFound && outsideFound )
//			{
//				count++;
//			}
//			totalCount++;
//		}
//		System.err.print( count + "   " + totalCount + "   " + (((float)count/(float)totalCount)) + "   " + (((float)count/(float)totalCount) >= 0.60));
//		return (((float)count/(float)totalCount) >= 0.60);
//	}





	public WormSegmentation() {}

	/**
	 * Creates a single level set. Takes a starting point and finds a closed path along the levelset back to the
	 * starting point.
	 */
	private static VOIContour singleLevelSet2(float[] image, float startPtX, float startPtY, int xDim, int yDim ) {

		Stack<int[]> stack = new Stack<int[]>();
		BitSet map = new BitSet(xDim * yDim);


		if ((startPtX < 0) || (startPtX >= (xDim - 1))) {
			return null;
		}

		if ((startPtY < 0) || (startPtY >= (yDim - 1))) {
			return null;
		}


		int x = (int) (startPtX + 0.5);
		int y = (int) (startPtY + 0.5);

		//		float level = image.getFloat( y * xDim + x);
		float level = image[ y * xDim + x];

		int index = (y * xDim) + x;

		/** Used to calculate the levelset contour. */
		PointStack levelSetStack = new PointStack(500);
		levelSetStack.addPoint(x, y);
		map.set((y * xDim) + x);

		int dir = -1;
		float diff = 100000;
		double distance;
		do {
			index = (y * xDim) + x;

			if ((x >= 2) && (x < (xDim - 2)) && (y >= 2) && (y < (yDim - 2))) {

				if ((avgPix(image, xDim, index - xDim) >= level) &&
						((avgPix(image, xDim, index - xDim + 1) < level) || (avgPix(image, xDim, index) < level) ||
								(avgPix(image, xDim, index - xDim - 1) < level) || (avgPix(image, xDim, index - (2 * xDim)) < level)) &&
								(map.get(index - xDim) == false)) {
					dir = 1;
					diff = Math.abs(avgPix(image, xDim, index - xDim) - avgPix(image, xDim, index));
				}

				if ((avgPix(image, xDim, index - xDim + 1) >= level) &&
						((avgPix(image, xDim, index - xDim + 2) < level) || (avgPix(image, xDim, index + 1) < level) ||
								(avgPix(image, xDim, index - xDim) < level) || (avgPix(image, xDim, index - (2 * xDim) + 1) < level)) &&
								(map.get(index - xDim + 1) == false)) {

					if (Math.abs(avgPix(image, xDim, index - xDim + 1) - avgPix(image, xDim, index)) < diff) {
						dir = 2;
						diff = Math.abs(avgPix(image, xDim, index - xDim + 1) - avgPix(image, xDim, index));
					}
				}

				if ((avgPix(image, xDim, index + 1) >= level) &&
						((avgPix(image, xDim, index + 2) < level) || (avgPix(image, xDim, index + xDim + 1) < level) || (avgPix(image, xDim, index) < level) ||
								(avgPix(image, xDim, index - xDim + 1) < level)) && (map.get(index + 1) == false)) {

					if (Math.abs(avgPix(image, xDim, index + 1) - avgPix(image, xDim, index)) < diff) {
						dir = 3;
						diff = Math.abs(avgPix(image, xDim, index + 1) - avgPix(image, xDim, index));
					}
				}

				if ((avgPix(image, xDim, index + xDim + 1) >= level) &&
						((avgPix(image, xDim, index + xDim + 2) < level) || (avgPix(image, xDim, index + (2 * xDim) + 1) < level) ||
								(avgPix(image, xDim, index + 1) < level) || (avgPix(image, xDim, index + xDim) < level)) &&
								(map.get(index + xDim + 1) == false)) {

					if (Math.abs(avgPix(image, xDim, index + xDim + 1) - avgPix(image, xDim, index)) < diff) {
						dir = 4;
						diff = Math.abs(avgPix(image, xDim, index + xDim + 1) - avgPix(image, xDim, index));
					}
				}

				if ((avgPix(image, xDim, index + xDim) >= level) &&
						((avgPix(image, xDim, index + xDim + 1) < level) || (avgPix(image, xDim, index + (2 * xDim)) < level) ||
								(avgPix(image, xDim, index + xDim - 1) < level) || (avgPix(image, xDim, index) < level)) &&
								(map.get(index + xDim) == false)) {

					if (Math.abs(avgPix(image, xDim, index + xDim) - avgPix(image, xDim, index)) < diff) {
						dir = 5;
						diff = Math.abs(avgPix(image, xDim, index + xDim) - avgPix(image, xDim, index));
					}
				}

				if ((avgPix(image, xDim, index + xDim - 1) >= level) &&
						((avgPix(image, xDim, index + xDim) < level) || (avgPix(image, xDim, index + (2 * xDim) - 1) < level) ||
								(avgPix(image, xDim, index + xDim - 2) < level) || (avgPix(image, xDim, index - 1) < level)) &&
								(map.get(index + xDim - 1) == false)) {

					if (Math.abs(avgPix(image, xDim, index + xDim - 1) - avgPix(image, xDim, index)) < diff) {
						dir = 6;
						diff = Math.abs(avgPix(image, xDim, index + xDim - 1) - avgPix(image, xDim, index));
					}
				}

				if ((avgPix(image, xDim, index - 1) >= level) &&
						((avgPix(image, xDim, index) < level) || (avgPix(image, xDim, index + xDim - 1) < level) || (avgPix(image, xDim, index - 2) < level) ||
								(avgPix(image, xDim, index - xDim - 1) < level)) && (map.get(index - 1) == false)) {

					if (Math.abs(avgPix(image, xDim, index - 1) - avgPix(image, xDim, index)) < diff) {
						dir = 7;
						diff = Math.abs(avgPix(image, xDim, index - 1) - avgPix(image, xDim, index));
					}
				}

				if ((avgPix(image, xDim, index - xDim - 1) >= level) &&
						((avgPix(image, xDim, index - xDim) < level) || (avgPix(image, xDim, index - 1) < level) ||
								(avgPix(image, xDim, index - xDim - 2) < level) || (avgPix(image, xDim, index - (2 * xDim) - 1) < level)) &&
								(map.get(index - xDim - 1) == false)) {

					if (Math.abs(avgPix(image, xDim, index - xDim - 1) - avgPix(image, xDim, index)) < diff) {
						dir = 0;
						// diff = Math.abs(imageBufferActive[index-xDim-1] - imageBufferActive[index]);
					}
				}

				diff = 1000000;
				if (dir == 1) {
					// x = x;
					y = y - 1;
					map.set(index - xDim);
					paths(image, xDim, levelSetStack, map, stack, index, 1, level);
				} else if (dir == 2) {
					x = x + 1;
					y = y - 1;
					map.set(index - xDim + 1);
					paths(image, xDim, levelSetStack, map, stack, index, 2, level);
				} else if (dir == 3) {
					x = x + 1;
					// y = y;
					map.set(index + 1);
					paths(image, xDim, levelSetStack, map, stack, index, 3, level);
				} else if (dir == 4) {
					x = x + 1;
					y = y + 1;
					map.set(index + xDim + 1);
					paths(image, xDim, levelSetStack, map, stack, index, 4, level);
				} else if (dir == 5) {
					// x = x;
					y = y + 1;
					map.set(index + xDim);
					paths(image, xDim, levelSetStack, map, stack, index, 5, level);
				} else if (dir == 6) {
					x = x - 1;
					y = y + 1;
					map.set(index + xDim - 1);
					paths(image, xDim, levelSetStack, map, stack, index, 6, level);
				} else if (dir == 7) {
					x = x - 1;
					// y = y;
					map.set(index - 1);
					paths(image, xDim, levelSetStack, map, stack, index, 7, level);
				} else if (dir == 0) {
					x = x - 1;
					y = y - 1;
					map.set(index - xDim - 1);
					paths(image, xDim, levelSetStack, map, stack, index, 0, level);
				} else {

					if (!stack.empty()) {
						int ptr = (stack.pop())[0];
						x = levelSetStack.getPointX(ptr);
						y = levelSetStack.getPointY(ptr);
						levelSetStack.setIndex(ptr);
					} else {
						x = y = -1;
					}
				}

				dir = -1;
			} else { // near edge of image
				levelSetStack.reset();

				break;
			}

			if ((x == -1) || (y == -1)) {
				levelSetStack.reset();

				break;
			}

			levelSetStack.addPoint(x, y);

			distance = ((x - startPtX) * (x - startPtX)) + ((y - startPtY) * (y - startPtY));

			if ((distance < 2.1) && (levelSetStack.size() < 10)) {
				distance = 10;
			}
		} while (distance > 2.1);



		if (levelSetStack.size() != 0) {

			VOIContour kVOI = new VOIContour( false, true );
			for ( int i = 0; i < levelSetStack.size(); i++ )
			{
				kVOI.add( new Vector3f( levelSetStack.getPointX(i), levelSetStack.getPointY(i), 0 ) );
			}
			return kVOI;
		} 
		return null;
	}

	/**
	 * This method calculates the average pixel value based on the four neighbors (N, S, E, W).
	 *
	 * @param   index  the center pixel where the average pixel value is to be calculated.
	 *
	 * @return  the average pixel value as a float.
	 */
	private static float avgPix(ModelImage image, int xDim, int index) {

		if ((index > xDim) && (index < (image.getDataSize() - xDim))) {

			float sum = image.getFloat(index);

			sum += image.getFloat(index - xDim);
			sum += image.getFloat(index - 1);
			sum += image.getFloat(index + 1);
			sum += image.getFloat(index + xDim);

			return sum / 5.0f;
		}
		return (image.getFloat(index));
	}

	private static float avgPix(float[] image, int xDim, int index) {

		if ((index > xDim) && (index < (image.length - xDim))) {

			float sum = image[index];

			sum += image[index - xDim];
			sum += image[index - 1];
			sum += image[index + 1];
			sum += image[index + xDim];

			return sum / 5.0f;
		}
		return (image[index]);
	}

	/**
	 * Generates the possible paths of the level set and pushes them onto a stack. Looks in the 8 neighborhood
	 * directions for the possible paths.
	 *
	 * @param  index  image location
	 * @param  i      DOCUMENT ME!
	 */
	private static void paths( ModelImage image, int xDim, PointStack levelSetStack, BitSet map, Stack<int[]> stack, int index, int i, float level) {

		int[] intPtr = null;

		try {
			intPtr = new int[1];
		} catch (OutOfMemoryError error) {
			System.gc();
			MipavUtil.displayError("Out of memory: ComponentEditImage.mouseDragged");

			return;
		}

		intPtr[0] = levelSetStack.size() - 1;

		if ((i != 0) && (image.getFloat(index - xDim - 1) <= level) && (map.get(index - xDim - 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 1) && (image.getFloat(index - xDim) <= level) && (map.get(index - xDim) == false)) {
			stack.push(intPtr);
		} else if ((i != 2) && (image.getFloat(index - xDim + 1) <= level) && (map.get(index - xDim + 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 3) && (image.getFloat(index + 1) <= level) && (map.get(index + 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 4) && (image.getFloat(index + xDim + 1) <= level) && (map.get(index + xDim + 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 5) && (image.getFloat(index + xDim) <= level) && (map.get(index + xDim) == false)) {
			stack.push(intPtr);
		} else if ((i != 6) && (image.getFloat(index + xDim - 1) <= level) && (map.get(index + xDim - 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 7) && (image.getFloat(index - 1) <= level) && (map.get(index - 1) == false)) {
			stack.push(intPtr);
		}
	}

	private static void paths( float[] image, int xDim, PointStack levelSetStack, BitSet map, Stack<int[]> stack, int index, int i, float level) {

		int[] intPtr = null;

		try {
			intPtr = new int[1];
		} catch (OutOfMemoryError error) {
			System.gc();
			MipavUtil.displayError("Out of memory: ComponentEditImage.mouseDragged");

			return;
		}

		intPtr[0] = levelSetStack.size() - 1;

		if ((i != 0) && (image[index - xDim - 1] <= level) && (map.get(index - xDim - 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 1) && (image[index - xDim] <= level) && (map.get(index - xDim) == false)) {
			stack.push(intPtr);
		} else if ((i != 2) && (image[index - xDim + 1] <= level) && (map.get(index - xDim + 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 3) && (image[index + 1] <= level) && (map.get(index + 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 4) && (image[index + xDim + 1] <= level) && (map.get(index + xDim + 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 5) && (image[index + xDim] <= level) && (map.get(index + xDim) == false)) {
			stack.push(intPtr);
		} else if ((i != 6) && (image[index + xDim - 1] <= level) && (map.get(index + xDim - 1) == false)) {
			stack.push(intPtr);
		} else if ((i != 7) && (image[index - 1] <= level) && (map.get(index - 1) == false)) {
			stack.push(intPtr);
		}
	}


	public static void segmentNuclei( ModelImage image )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		int window = 5;

		double max = image.getMax();
		double min = image.getMin();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					float average = 0;
					int count = 0;
					for ( int z1 = Math.max(0, z - window); z1 < Math.min(dimZ,  z + window + 1); z1++ )
					{
						for ( int y1 = Math.max(0, y - window); y1 < Math.min(dimY,  y + window + 1); y1++ )
						{
							for ( int x1 = Math.max(0, x - window); x1 < Math.min(dimX,  x + window + 1); x1++ )
							{
								average += image.getFloat(x1,y1,z1);
								count++;
							}
						}
					}
					average /= (float)count;

					// 'inside' = high average, low standardDeviation
					//					if ( average > 100 )
					//					{
					//						float standardDeviation = 0;
					//						count = 0;
					//						for ( int z1 = Math.max(0, z - window); z1 < Math.min(dimZ,  z + window + 1); z1++ )
					//						{
					//							for ( int y1 = Math.max(0, y - window); y1 < Math.min(dimY,  y + window + 1); y1++ )
					//							{
					//								for ( int x1 = Math.max(0, x - window); x1 < Math.min(dimX,  x + window + 1); x1++ )
					//								{
					//									standardDeviation += ((image.getFloat(x1,y1,z1) - average)*(image.getFloat(x1,y1,z1) - average));
					//									count++;
					//								}
					//							}
					//						}
					//						standardDeviation /= (float)count;
					//					
					//					}
				}
			}
		}

	}




	public static float[] robustHistogram(ModelImage image, Vector<Vector3f> dataPoints )
	{
		double min = image.getMin();
		double max = image.getMax();    	    	
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    	

		// Calculate the histogram:
		HashMap<Float, Integer> map = new HashMap<Float, Integer>();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					float value = image.getFloat(x,y,z);
					int count = 0;
					if ( map.containsKey(value) )
					{
						count = map.get(value);
					}
					count++;
					map.put( value, count );
				}
			}
		}

		int totalCount = dimX * dimY * dimZ;
		int count_2P = (int) (0.02 * totalCount);
		int count_985P = (int) (0.985 * totalCount);
		int count_995P = (int) (0.995 * totalCount);
		int count_999P = (int) (0.999 * totalCount);

		// Sort the Histogram bins:
		Set<Float> keySet = map.keySet();
		Iterator<Float> keyIterator = keySet.iterator();
		float[] keyArray = new float[keySet.size()];
		int count = 0;
		while ( keyIterator.hasNext() )
		{
			float value = keyIterator.next();
			keyArray[count++] = value;
		}

		Arrays.sort(keyArray);
		int[] counts = new int[keyArray.length];
		int runningCount = 0;
		boolean minFound = false;
		boolean maxLRFound = false;
		boolean found985 = false;
		boolean found995 = false;
		float min_2P = (float) image.getMin();
		float max_LR = (float) image.getMax();
		float lr985 = min_2P;
		float lr995 = max_LR;
		for ( int i = 0; i < counts.length; i++ )
		{
			counts[i] = map.get( keyArray[i] );
			runningCount += counts[i];

			if ( !minFound && (runningCount >= count_2P) )
			{
				min_2P = keyArray[i];
				minFound = true;
			}
			if ( !found985 && (runningCount >= count_985P) )
			{
				lr985 = keyArray[i];
				found985 = true;
			}
			if ( !found995 && (runningCount >= count_995P) )
			{
				lr995 = keyArray[i];
				found995 = true;
			}
			if ( !maxLRFound && (runningCount >= count_999P) )
			{
				max_LR = keyArray[i];
				maxLRFound = true;
			}
		}

		if ( dataPoints != null )
		{
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						float value = image.getFloat(x,y,z);
						if ( value < min_2P )
						{
							value = min_2P;
						}
						else if ( value > max_LR )
						{
							value = max_LR;    					
						}
						if ( value >= (.90f*max_LR) )
						{
							dataPoints.add( new Vector3f( x, y, z ) );
						}
						//    				image.set(x,y,z,value);
					}
				}
			}
		}
		//		image.calcMinMax();
		return new float[]{lr985,lr995};
	}

	public static Vector<Vector3f> seamCellSegmentation( ModelImage image, float minValue, float maxValue )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
		Vector3f min = new Vector3f(dimX, dimY, dimZ);		
		Vector3f max = new Vector3f(0,0,0);

		// find the left right markers based on the threshold values and clustering...
		Vector<Vector3f> segmentation = new Vector<Vector3f>();
		Vector<Vector3d> centers = findMarkers(image, null, maxValue, (float)image.getMax(), 15, 30, min, max);
		for ( int i = 0; i < centers.size(); i++ )
		{
			Vector3d center = centers.elementAt(i);
			segmentation.add( new Vector3f( (float)center.X, (float)center.Y, (float)center.Z ) );
		}
		return segmentation;
	}


	public static Vector<Vector3d> findMarkers( ModelImage image, BitSet leftRightMask, float intensityMin, float intensityMax, int diameter, int maxDiameter, Vector3f min, Vector3f max )
	{
		int cubeSize = diameter;
		int cubeHalf = cubeSize/2;    	

		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;    

		Vector3f temp = new Vector3f();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					float value = image.getFloat(x,y,z);
					if ( (value > intensityMin) && (value <= intensityMax) )
					{
						temp.set(x,y,z);
						min.min( temp );
						max.max( temp );
					}
				}
			}
		}
		//    	System.err.println( "Min bounding box = " + min );
		//    	System.err.println( "Max bounding box = " + max );

		if ( min.X < cubeHalf ) min.X = cubeHalf;
		if ( min.Y < cubeHalf ) min.Y = cubeHalf;
		if ( min.Z < cubeHalf ) min.Z = cubeHalf;

		if ( max.X > (dimX - cubeHalf) ) max.X = (dimX - cubeHalf);
		if ( max.Y > (dimY - cubeHalf) ) max.Y = (dimY - cubeHalf);
		if ( max.Z > (dimZ - cubeHalf) ) max.Z = (dimZ - cubeHalf);

		//    	System.err.println( "Min bounding box = " + min );
		//    	System.err.println( "Max bounding box = " + max );


		//    	Vector<Vector2d> cubeIntensities = new Vector<Vector2d>();
		Vector<Vector3f> cubeCenters = new Vector<Vector3f>();

		//    	double minVar = Double.MAX_VALUE;
		//    	double maxVar = -Double.MAX_VALUE;

		Vector3f centerP = new Vector3f();
		Vector3f testP = new Vector3f();
		// Shifting 5x5x5 voxel cube through the volume, calculating the mean intesity and variance:
		for ( int z = (int) min.Z; z < max.Z; z++ )
		{
			for ( int y = (int) min.Y; y < max.Y; y++ )
			{
				for ( int x = (int) min.X; x < max.X; x++ )
				{
					float value = image.getFloat(x,y,z);

					if ( (value > intensityMin) && (value <= intensityMax) )
					{
						boolean solid = true;
						centerP.set(x,y,z);

						if ( leftRightMask != null )
						{
							int index = (z * dimX * dimY + y * dimX + x);	
							if (leftRightMask.get(index) )
							{
								continue;
							}
						}

						int count = 0;
						double meanIntensity = 0;
						for ( int z2 = z - cubeHalf; z2 < (z+cubeHalf); z2++ )
						{
							for ( int y2 = y - cubeHalf; y2 < (y+cubeHalf); y2++ )
							{
								for ( int x2 = x - cubeHalf; x2 < (x+cubeHalf); x2++ )
								{
									if ( (x2 < dimX) && (y2 < dimY) && (z2 < dimZ) )
									{
										testP.set(x2,y2,z2);
										if ( centerP.distance(testP) <= cubeHalf )
										{
											value = image.getFloat(x2,y2,z2);
											meanIntensity += value;
											count++;

											if ( value < (intensityMin / 2) )
											{
												solid = false;
											}
										}
									}
								}
							}    					
						}
						meanIntensity /= count;

						if ( solid && meanIntensity >= intensityMin )
						{
							Vector3f center = new Vector3f( x, y, z );
							cubeCenters.add(center);
						}
					}
				}
			}
		}    	

		Vector<Vector3d> clusterCenters = makeClusters( cubeCenters, cubeSize );
		//    	Vector<VOIContour> clusterList = makeClusters( cubeCenters, cubeSize, maxDiameter );
		//    	
		//    	VOIContour centerPoints = new VOIContour(false);
		//    	while ( clusterList.size() > 0 )
		//    	{
		//    		int maxSize = -1;
		//    		int maxIndex = -1;
		//    		for ( int i = 0; i < clusterList.size(); i++ )
		//    		{
		//    			VOIContour cluster = clusterList.elementAt(i);
		//    			if ( cluster.size() > maxSize )
		//    			{
		//    				maxSize = cluster.size();
		//    				maxIndex = i;
		//    			}
		//    		}
		//    		if ( maxIndex >= 0 && maxIndex < clusterList.size() )
		//    		{
		//    			VOIContour cluster = clusterList.elementAt(maxIndex);
		////    			System.err.println( "   cluster = " + cluster.size() );
		//    			Vector3f center = new Vector3f();
		//    			for ( int j = 0; j < cluster.size(); j++ )
		//    			{
		//    				center.add( cluster.elementAt(j) );
		//    			}
		//    			center.scale( 1f/cluster.size() );
		//    			centerPoints.add(center);
		//    			clusterList.remove(maxIndex);
		//    		}
		//    	}

		if ( diameter != maxDiameter )
		{
			for ( int i = clusterCenters.size() - 1; i >= 0; i-- )
			{
				for ( int j = i-1; j >= 0; j-- )
				{
					if ( clusterCenters.elementAt(i).distance( clusterCenters.elementAt(j) ) < maxDiameter )
					{
						if ( mergePoints( image, intensityMin, clusterCenters.elementAt(i), clusterCenters.elementAt(j) ) )
						{
							//    					System.err.println( "Merging points" );
							Vector3d pt = clusterCenters.remove(i);
							clusterCenters.elementAt(j).add(pt);
							clusterCenters.elementAt(j).scale(0.5f);    			
							break;
						}
					}
				}
			}
		}
		for ( int i = 0; i < clusterCenters.size(); i++ )
		{
			clusterCenters.elementAt(i).X = Math.round(clusterCenters.elementAt(i).X);
			clusterCenters.elementAt(i).Y = Math.round(clusterCenters.elementAt(i).Y);
			clusterCenters.elementAt(i).Z = Math.round(clusterCenters.elementAt(i).Z);
		}
		for ( int i = clusterCenters.size()-1; i >= 0; i-- )
		{
			for ( int j = i-1; j >= 0; j-- )
			{
				if ( clusterCenters.elementAt(i).isEqual( clusterCenters.elementAt(j) ) )
				{
					clusterCenters.remove(i);
					break;
				}
			}
		}
		cubeCenters.clear();
		cubeCenters = null;
		return clusterCenters;
	}

	private static Vector<Vector3d> makeClusters( Vector<Vector3f> cubeCenters, int diameter )
	{

		Vector<VOIContour> clusterList = new Vector<VOIContour>();
		Vector<Vector3d> clusterSum = new Vector<Vector3d>();
		for ( int i = 0; i < cubeCenters.size(); i++ )
		{
			Vector3f p1 = cubeCenters.elementAt(i);
			if ( clusterList.size() == 0 )
			{
				VOIContour cluster = new VOIContour( false );
				cluster.add(p1);
				clusterList.add( cluster );
				clusterSum.add( new Vector3d(p1.X, p1.Y, p1.Z) );
			}
			else
			{
				float minDist = Float.MAX_VALUE;
				int minClusterIndex = -1;
				for ( int j = 0; j < clusterList.size(); j++ )
				{
					VOIContour cluster = clusterList.elementAt(j);
					Vector3d sum = clusterSum.elementAt(j);
					double scale = 1.0/cluster.size();
					Vector3f avg = new Vector3f( (float)(sum.X * scale), (float)(sum.Y * scale), (float)(sum.Z * scale) );
					float distance = p1.distance( avg );
					if ( distance < minDist )
					{
						minDist = distance;
						minClusterIndex = j;
					}
				}
				if ( (minClusterIndex == -1) || (minDist > diameter) )
				{
					VOIContour cluster = new VOIContour( false );
					cluster.add(p1);
					clusterList.add( cluster );    		
					clusterSum.add( new Vector3d(p1.X, p1.Y, p1.Z) );		
				}
				else
				{
					clusterList.elementAt(minClusterIndex).add(p1);
					clusterSum.elementAt(minClusterIndex).add( new Vector3d(p1.X, p1.Y, p1.Z) );
				}
			}
		}	
		for ( int i = 0; i < clusterList.size(); i++ )
		{
			VOIContour cluster = clusterList.elementAt(i);
			Vector3d sum = clusterSum.elementAt(i);
			double scale = 1.0/cluster.size();
			Vector3d avg = new Vector3d( (sum.X * scale), (sum.Y * scale), (sum.Z * scale) );
			sum.copy( avg );
		}
		clusterList.clear();
		clusterList = null;
		return clusterSum;    	
	}

	private static boolean mergePoints( ModelImage image, float intensityMin, Vector3d p1, Vector3d p2 )
	{    	
		Vector3d dir = Vector3d.sub( p2, p1 );
		dir.normalize();
		double steps = p1.distance(p2);
		Vector3d start = new Vector3d(p1);
		for ( int i = 0; i < steps; i++ )
		{
			start.add(dir);
			if ( image.getFloatTriLinearBounds( (float)start.X, (float)start.Y, (float)start.Z ) < (.95*intensityMin) )
			{
				return false;
			}
		}
		return true;
	}


	private static boolean expandList(ModelImage image, VOI clusterList, VOIContour centerList, float minRadius )
	{
		boolean modified = false;
		for ( int i = clusterList.getCurves().size() -1; i >= 0; i-- )
		{
			Box3f box = ContBox3f.ContOrientedBox(clusterList.getCurves().elementAt(i).size(), clusterList.getCurves().elementAt(i));
			float minExtent = box.Extent[0];
			if ( box.Extent[1] < minExtent )
			{
				minExtent = box.Extent[1];
			}
			if ( box.Extent[2] < minExtent )
			{
				minExtent = box.Extent[2];
			}
			float maxExtent = box.Extent[0];
			if ( box.Extent[1] > maxExtent )
			{
				maxExtent = box.Extent[1];
			}
			if ( box.Extent[2] > maxExtent )
			{
				maxExtent = box.Extent[2];
			}
			if ( (minExtent*1.5 < maxExtent) && (maxExtent > 2*minRadius) )
			{
				VOIContour cluster = (VOIContour)clusterList.getCurves().elementAt(i);
				modified |= splitCluster( image, clusterList, cluster, centerList, box, i, minRadius );
			}

		}
		return modified;		
	}

	private static boolean splitCluster( ModelImage image, VOI clusterList, VOIContour cluster, Vector<Vector3f> centerList, Box3f box, int index, float minRadius )
	{
		boolean modified = false;

		int maxIndex = 0;
		float maxE = box.Extent[0];
		if ( box.Extent[1] > maxE)
		{
			maxE = box.Extent[1];
			maxIndex = 1;
		}
		if ( box.Extent[2] > maxE)
		{
			maxE = box.Extent[2];
			maxIndex = 2;
		}

		Vector3f dir0 = box.Axis[maxIndex];
		dir0.normalize();
		dir0.scale(box.Extent[maxIndex]/2f);


		Vector3f maxPtFromStart = new Vector3f(box.Center);
		maxPtFromStart.add(dir0);
		Vector3f maxPtFromStop = new Vector3f(box.Center);
		maxPtFromStop.sub(dir0);

		float maxPointDistance = maxPtFromStart.distance(maxPtFromStop);
		if ( maxPointDistance < minRadius )
		{
			// no split
			return false;
		}
		else
		{
//			Vector3f minPt = getMid(maxPtFromStart, maxPtFromStop, cluster);
			
			Vector3f minPt = Vector3f.add(maxPtFromStart,maxPtFromStop);
			minPt.scale(0.5f);

			int x = Math.round(minPt.X);
			int y = Math.round(minPt.Y);
			int z = Math.round(minPt.Z);
			float minPtValue = image.isColorImage() ? image.getFloatC(x, y, z, 2) : image.getFloat(x, y, z);			

			Plane3f halfPlane = new Plane3f( dir0, minPt );
			VOIContour subCluster1 = new VOIContour(false);
			VOIContour subCluster2 = new VOIContour(false);

			Vector3f center1 = new Vector3f();
			Vector3f center2 = new Vector3f();
			for ( int i = 0; i < cluster.size(); i++ )
			{
				int side = halfPlane.WhichSide(cluster.elementAt(i));
				if ( side == -1 )
				{
					subCluster1.add(cluster.elementAt(i));
					center1.add(cluster.elementAt(i));
				}
				else if ( side == 1 )
				{
					subCluster2.add(cluster.elementAt(i));
					center2.add(cluster.elementAt(i));
				}
			}

			if ( (subCluster1.size() == 0) || (subCluster2.size() == 0) )
				return false;

			center1.scale(1f/(float)subCluster1.size());
			float minDist = Float.MAX_VALUE;
			int minIndex = -1;
			for ( int i = 0; i < subCluster1.size(); i++ )
			{
				float distance = center1.distance(subCluster1.elementAt(i));
				if ( distance < minDist )
				{
					minDist = distance;
					minIndex = i;
				}
			}
			if ( minIndex != -1 )
			{
				center1.copy(subCluster1.elementAt(minIndex));
			}
			else
			{
				System.err.println( "error" );
			}
			
			center2.scale(1f/(float)subCluster2.size());
			minDist = Float.MAX_VALUE;
			minIndex = -1;
			for ( int i = 0; i < subCluster2.size(); i++ )
			{
				float distance = center2.distance(subCluster2.elementAt(i));
				if ( distance < minDist )
				{
					minDist = distance;
					minIndex = i;
				}
			}
			if ( minIndex != -1 )
			{
				center2.copy(subCluster2.elementAt(minIndex));
			}
			else
			{
				System.err.println( "error" );
			}

			x = Math.round(center1.X);
			y = Math.round(center1.Y);
			z = Math.round(center1.Z);
			float max1Value = image.isColorImage() ? image.getFloatC(x, y, z, 2) : image.getFloat(x, y, z);

			x = Math.round(center2.X);
			y = Math.round(center2.Y);
			z = Math.round(center2.Z);
			float max2Value = image.isColorImage() ? image.getFloatC(x, y, z, 2) : image.getFloat(x, y, z);

			if ( (minPtValue < max1Value) && (minPtValue <  max2Value) )
			{						
				Box3f boxSub1 = ContBox3f.ContOrientedBox(subCluster1.size(), subCluster1);
				if ( (boxSub1.Extent[0] <= 2) || (boxSub1.Extent[1] <= 2) || (boxSub1.Extent[2] <= 2) )
				{
					//subCluster1 invalid
					for ( int j = 0; j < subCluster1.size(); j++ )
					{
						cluster.remove(subCluster1.elementAt(j));
					}
					centerList.elementAt(index).copy(center2);
//					System.err.println( "   pruning cluster " + (index+1) );
					return true;
				}

				Box3f boxSub2 = ContBox3f.ContOrientedBox(subCluster2.size(), subCluster2);
				if ( (boxSub2.Extent[0] <= 2) || (boxSub2.Extent[1] <= 2) || (boxSub2.Extent[2] <= 2) )
				{
					//subCluster1 invalid
					for ( int j = 0; j < subCluster2.size(); j++ )
					{
						cluster.remove(subCluster2.elementAt(j));
					}
					centerList.elementAt(index).copy(center1);
//					System.err.println( "   pruning cluster " + (index+1) );
					return true;
				}

				if ( center1.distance(center2) > minRadius )
				{
//					System.err.println( "   Splitting cluster " + (index+1) );
					centerList.elementAt(index).copy(center1);
					centerList.add(center2);

					cluster.removeAllElements();
					cluster.addAll(subCluster1);
					clusterList.getCurves().add(subCluster2);
					modified = true;
				}
			}
			else
			{				
				Vector3f dir = Vector3f.sub(maxPtFromStop, maxPtFromStart);
				float length = dir.normalize();
				Vector3f pt = new Vector3f(maxPtFromStart);
				minPt = new Vector3f(maxPtFromStart);
				minPtValue = Float.MAX_VALUE;
				for ( int i = 0; i < length; i++ )
				{
					x = Math.round(pt.X);
					y = Math.round(pt.Y);
					z = Math.round(pt.Z);
					float value = image.isColorImage() ? image.getFloatC(x, y, z, 2) : image.getFloat(x, y, z);
					if ( value < minPtValue )
					{
						minPtValue = value;
						minPt.copy(pt);
					}
					pt.add(dir);
				}
				


				halfPlane = new Plane3f( dir0, minPt );
				subCluster1 = new VOIContour(false);
				subCluster2 = new VOIContour(false);

				center1 = new Vector3f();
				center2 = new Vector3f();
				for ( int i = 0; i < cluster.size(); i++ )
				{
					int side = halfPlane.WhichSide(cluster.elementAt(i));
					if ( side == -1 )
					{
						subCluster1.add(cluster.elementAt(i));
						center1.add(cluster.elementAt(i));
					}
					else if ( side == 1 )
					{
						subCluster2.add(cluster.elementAt(i));
						center2.add(cluster.elementAt(i));
					}
				}

				if ( (subCluster1.size() == 0) || (subCluster2.size() == 0) )
					return false;

				center1.scale(1f/(float)subCluster1.size());
				minDist = Float.MAX_VALUE;
				minIndex = -1;
				for ( int i = 0; i < subCluster1.size(); i++ )
				{
					float distance = center1.distance(subCluster1.elementAt(i));
					if ( distance < minDist )
					{
						minDist = distance;
						minIndex = i;
					}
				}
				if ( minIndex != -1 )
				{
					center1.copy(subCluster1.elementAt(minIndex));
				}
				else
				{
					System.err.println( "error" );
				}
				
				center2.scale(1f/(float)subCluster2.size());
				minDist = Float.MAX_VALUE;
				minIndex = -1;
				for ( int i = 0; i < subCluster2.size(); i++ )
				{
					float distance = center2.distance(subCluster2.elementAt(i));
					if ( distance < minDist )
					{
						minDist = distance;
						minIndex = i;
					}
				}
				if ( minIndex != -1 )
				{
					center2.copy(subCluster2.elementAt(minIndex));
				}
				else
				{
					System.err.println( "error" );
				}

				x = Math.round(center1.X);
				y = Math.round(center1.Y);
				z = Math.round(center1.Z);
				max1Value = image.isColorImage() ? image.getFloatC(x, y, z, 2) : image.getFloat(x, y, z);

				x = Math.round(center2.X);
				y = Math.round(center2.Y);
				z = Math.round(center2.Z);
				max2Value = image.isColorImage() ? image.getFloatC(x, y, z, 2) : image.getFloat(x, y, z);


//							System.err.println( minPtValue + "   " + max1Value + "   " + max2Value );
//							System.err.println( minPt.distance(maxPtFromStart) + "    " + minPt.distance(maxPtFromStop) );
//							System.err.println("");

				if ( (minPtValue < max1Value) && (minPtValue <  max2Value) )
				{						
					Box3f boxSub1 = ContBox3f.ContOrientedBox(subCluster1.size(), subCluster1);
					if ( (boxSub1.Extent[0] <= 2) || (boxSub1.Extent[1] <= 2) || (boxSub1.Extent[2] <= 2) )
					{
						//subCluster1 invalid
						for ( int j = 0; j < subCluster1.size(); j++ )
						{
							cluster.remove(subCluster1.elementAt(j));
						}
						centerList.elementAt(index).copy(center2);
//						System.err.println( "   pruning cluster2 " + (index+1) );
						return true;
					}


					Box3f boxSub2 = ContBox3f.ContOrientedBox(subCluster2.size(), subCluster2);
					if ( (boxSub2.Extent[0] <= 2) || (boxSub2.Extent[1] <= 2) || (boxSub2.Extent[2] <= 2) )
					{
						//subCluster1 invalid
						for ( int j = 0; j < subCluster2.size(); j++ )
						{
							cluster.remove(subCluster2.elementAt(j));
						}
						centerList.elementAt(index).copy(center1);
//						System.err.println( "   pruning cluster2 " + (index+1) );
						return true;
					}

					if ( center1.distance(center2) > minRadius )
					{
//						System.err.println( "   Splitting cluster2 " + (index+1) );
						centerList.elementAt(index).copy(center1);
						centerList.add(center2);

						cluster.removeAllElements();
						cluster.addAll(subCluster1);
						clusterList.getCurves().add(subCluster2);
						modified = true;
					}
				}
			}
		}
		return modified;
	}


//	public static void findLargestConnected( ModelImage image, BitSet mask, float minValue )
//	{
//		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
//		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
//		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
//
//		//		float minValue = Float.MAX_VALUE;
//
//		Vector<Vector3f> seeds = new Vector<Vector3f>();
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					int index = z * dimX * dimY + y * dimX + x;
//					if ( mask.get(index) )
//					{
//						seeds.add( new Vector3f(x,y,z));
//						//						float value = 0;
//						//						if ( image.isColorImage() )
//						//						{
//						//							value = image.getFloatC(x, y, z, 1 ); 
//						//						}
//						//						else
//						//						{
//						//							value = image.getFloat(x,y,z);
//						//						}
//						//						if ( value < minValue )
//						//						{
//						//							minValue = value;
//						//						}
//					}
//				}
//			}
//		}
//		System.err.println("findLargest " + minValue );
//		if ( minValue == image.getMin() )
//		{
//			System.err.println("findLargest " + minValue + " == " + image.getMin() );
//			return;
//		}
//
//		BitSet visited = new BitSet(dimX*dimY*dimZ);
//		//		visited.or(mask);
//
//		fillMask(  image, visited, mask, seeds, 
//				dimX, dimY, dimZ, minValue, (float)image.getMax() );
//
//
//		ModelImage results = new ModelImage( ModelStorageBase.ARGB, image.getExtents(), "skin" );
//		for ( int z = 0; z < dimZ; z++ )
//		{
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					int index = z * dimX * dimY + y * dimX + x;
//					if ( mask.get(index) )
//					{
//						results.setC(x, y, z, 3, 1);
//					}					
//				}
//			}
//		}
//		results.calcMinMax();
//		new ViewJFrameImage(results);
//	}

//	private static void fillMask( ModelImage image, BitSet visited, BitSet connected, Vector<Vector3f> seeds, 
//			int dimX, int dimY, int dimZ, float min, float max )
//	{
//		while ( seeds.size() > 0 )
//		{
//			Vector3f start = seeds.remove(0);
//			int x = (int) start.X;
//			int y = (int) start.Y;
//			int z = (int) start.Z;
//			int startIndex = z*dimY*dimX + y*dimX + x;
//			connected.set(startIndex);
//			visited.set(startIndex);
//
//			for ( int y1 = Math.max(0, y - 1); y1 <= Math.min(y+1, dimY-1); y1++ )
//			{
//				for ( int x1 = Math.max(0, x - 1); x1 <= Math.min(x+1, dimX-1); x1++ )
//				{
//					int index = y1*dimX + x1;
//					float value = image.getFloat(index);
//					if ( value == 0 )
//					{
//						visited.set(index);
//					}
//					else
//					{
//						value = image.getFloat(index);
//						if ( !visited.get(index) && !connected.get(index) && (value >= min) && (value <= max) )
//						{
//							visited.set(index);
//							seeds.add( new Vector3f(x1,y1,0) );
//						}
//					}
//				}
//			}
//		}
//	}



	private static Vector3f getMid( Vector3f pt1, Vector3f pt2, Vector<Vector3f> points)
	{
		Vector3f mid = new Vector3f();
		float minDist = Float.MAX_VALUE;
		for ( int i = 0; i < points.size(); i++ )
		{
			float distance1 = points.elementAt(i).distance(pt1);
			float distance2 = points.elementAt(i).distance(pt2);
			float diff = Math.abs(distance1 - distance2);
			if ( diff < minDist )
			{
				minDist = diff;
				mid.copy(points.elementAt(i));
			}
		}
		return mid;
	}



	public static boolean segmentSeamThreshold(ModelImage image, ModelImage temp, VOIContour positions, VOI clusterList, float[] finalThreshold,
			float stepSize, float threshold, float thresholdMin, float thresholdMax, int minCount, int minRadius, boolean needTenth )
	{
		float[] targetP = new float[2];
		float targetPercentage = .95f;
		float[] sortedValues = WormSegmentation.estimateHistogram( image, targetP, targetPercentage );
		int index = sortedValues.length - 1;
		threshold = sortedValues[index];
		boolean found = false;

		Vector<Integer> indexValues = new Vector<Integer>();
		Vector<Integer> cellCounts = new Vector<Integer>();
		threshold = sortedValues[index];
		while ( (threshold > thresholdMin) && (index >= 0) )
		{
			positions.clear();
			clusterList.getCurves().clear();	
			WormSegmentation.segmentSeam(image, positions, clusterList, threshold);
			int numCells = positions.size();
			indexValues.add(index);
			cellCounts.add(numCells);
			//			System.err.println( indexValues.size() + "   " + index + "   " + numCells );
			index -= 10;
			if ( index >= 0 )
			{
				threshold = sortedValues[index];
			}
		}
		for ( int i = 0; i < indexValues.size()-1; i++ )
		{
			if ( (cellCounts.elementAt(i) >= minCount) && (cellCounts.elementAt(i+1) > cellCounts.elementAt(i)) )
			{
				index = indexValues.elementAt(i);
				index += 10;
				for ( int j = 0; j < 10; j++ )
				{
					positions.clear();
					clusterList.getCurves().clear();	
					threshold = sortedValues[index--];
					WormSegmentation.segmentSeam(image, positions, clusterList, threshold);
					//					int numCells = positions.size();
					boolean pass = testSeamCells(image, temp, positions, clusterList, minCount, minRadius, needTenth);
					//					System.err.println( "    " + i + "  " + numCells + " " + threshold + "  " + index + "   " + pass );
					if ( pass )
					{
						finalThreshold[0] = threshold;
						found = true;
						break;
					}
				}
			}
			else if ( ((i + 1) == (indexValues.size()-1)) &&  
					(cellCounts.elementAt(i) < minCount) && (cellCounts.elementAt(i+1) >= minCount) )
			{
				index = indexValues.elementAt(i+1);
				index += 10;
				for ( int j = 0; j < 10; j++ )
				{
					positions.clear();
					clusterList.getCurves().clear();	
					threshold = sortedValues[index--];
					WormSegmentation.segmentSeam(image, positions, clusterList, threshold);
					//					int numCells = positions.size();
					boolean pass = testSeamCells(image, temp, positions, clusterList, minCount, minRadius, needTenth);
					//					System.err.println( "    " + i + "  " + numCells + " " + threshold + "  " + index + "   " + pass );
					if ( pass )
					{
						finalThreshold[0] = threshold;
						found = true;
						break;
					}
				}
			}
		}

		return found;
	}


	public static VOIContour fill2(final ModelImage image, float cutOffMin, float cutOffMax, final Vector<Vector3f> seedList, 
			BitSet visited, Vector3f center ) {
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		//		System.err.println( cutOffMin + " " + cutOffMax );
		int count = 0;
		VOIContour pointsList = new VOIContour(false);
		while (seedList.size() > 0) {
			final Vector3f seed = seedList.remove(0);

			final int z = Math.round(seed.Z);
			final int y = Math.round(seed.Y);
			final int x = Math.round(seed.X);
			int index = z*dimY*dimX + y*dimX + x;
			visited.set(index);

			center.add(seed);
			pointsList.add(seed);
			count++;

			for (int z1 = Math.max(0, z - 1); z1 <= Math.min(dimZ - 1, z + 1); z1++)
			{
				for (int y1 = Math.max(0, y - 1); y1 <= Math.min(dimY - 1, y + 1); y1++)
				{
					for (int x1 = Math.max(0, x - 1); x1 <= Math.min(dimX - 1, x + 1); x1++)
					{
						if ( ! ( (x == x1) && (y == y1) && (z == z1))) {
							index = z1*dimY*dimX + y1*dimX + x1;
							if ( !visited.get(index) )
							{
								float value = image.getFloat(x1, y1, z1);
								if ( (value >= cutOffMin) && (value <= cutOffMax) )
								{
									seedList.add( new Vector3f(x1,y1,z1) );
								}
								visited.set(index);
							}
						}
					}
				}
			}							
			if ( count >= image.getDataSize() )
			{
				break;
			}
		}
		//		System.err.println("fill2 " + count);
		center.scale(1f/(float)count);
		float minDist = Float.MAX_VALUE;
		int minIndex = -1;
		for ( int i = 0; i < pointsList.size(); i++ )
		{
			float distance = center.distance(pointsList.elementAt(i));
			if ( distance < minDist )
			{
				minDist = distance;
				minIndex = i;
			}
		}
		if ( minIndex != -1 )
		{
			center.copy(pointsList.elementAt(minIndex));
		}
		else
		{
			System.err.println( "error" );
		}
		return pointsList;
	}



	public static void segmentSeam(ModelImage image, Vector<Vector3f> positions, VOI clusterList, float threshold)
	{
		separateID(image, threshold, positions, clusterList);
	}


	private static void separateID( ModelImage image, float threshold, Vector<Vector3f> positions, VOI clusterList )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
		BitSet visited = new BitSet(dimX*dimY*dimZ);
		Vector<Vector3f> seedList = new Vector<Vector3f>();
		boolean done = false;
		while ( !done )
		{
			boolean found = false;
			for ( int z = 0; z < dimZ && !found; z++ )
			{
				for ( int y = 0; y < dimY && !found; y++ )
				{
					for ( int x = 0; x < dimX && !found; x++ )
					{
						float value = image.getFloat(x, y, z);
						int index = z*dimX*dimY + y*dimX + x;
						if ( !visited.get(index) )
						{
							if ( value >= threshold )
							{
								seedList.add(new Vector3f(x,y,z));
								//								seamCells.set(x, y, z, id);
								visited.set(index);
								found = true;
							}
						}
					}
				}
			}
			if ( !found )
			{
				done = true;
				break;
			}

			Vector3f center = new Vector3f();
			VOIContour cluster = WormSegmentation.fill2(image, threshold, (float)image.getMax(), seedList, visited, center);
			clusterList.getCurves().add(cluster);
			positions.add(center);
		}
	}

	private static boolean testSeamCells( ModelImage image, ModelImage temp, VOIContour positions, VOI clusterList, int numCells, int minRadius, boolean needTenth )
	{
		//		System.err.println( positions.size() );
		for ( int i = positions.size() - 1; i >= 0; i-- )
		{
			VOIContour cluster = (VOIContour) clusterList.getCurves().elementAt(i);
			if ( cluster.size() > 0 )
			{
				Box3f box = ContBox3f.ContOrientedBox(cluster.size(), cluster);
				//				System.err.println( (i+1) + "    " + cluster.size() + "    " + box.Extent[0] + "   " + box.Extent[1] + "   " + box.Extent[2]);
				if ( (box.Extent[0] <= 2) || (box.Extent[1] <= 2) || (box.Extent[2] <= 2) )
				{
					// Remove Cluster:
					clusterList.getCurves().remove( i );
					positions.remove( i );
				}
			}
		}
		//		System.err.println( positions.size() );

		expandList( image, clusterList, positions, minRadius );
		if ( positions.size() < numCells)
		{
			return false;
		}
		if ( !needTenth && (positions.size() == numCells) )
		{
			return true;
		}
		for ( int i = 0; i < temp.getDataSize(); i++ )
		{
			temp.set(i, 0);
		}
		int clusterCount = 1;
		for ( int i = 0; i < positions.size(); i++ )
		{
			VOIContour cluster = (VOIContour) clusterList.getCurves().elementAt(i);
			for ( int j = 0; j < cluster.size(); j++ )
			{
				int x = Math.round(cluster.elementAt(j).X);
				int y = Math.round(cluster.elementAt(j).Y);
				int z = Math.round(cluster.elementAt(j).Z);

				temp.set(x, y, z, clusterCount);
			}
			clusterCount++;
		}

		LatticeBuilder buildTest = new LatticeBuilder();
		buildTest.setSeamImage(temp);
		buildTest.setSeamIDs(positions);
		boolean pass = buildTest.findTenthPair(image, positions) & (clusterList.getCurves().size() >= numCells);
		return pass;
	}

	/*
	private static void separateID( ModelImage image, float threshold, Vector<Vector3f> positions, VOI clusterList )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 

		BitSet visited = new BitSet(dimX*dimY*dimZ);
		Vector<Vector3f> seedList = new Vector<Vector3f>();

		int id = 1;
		boolean done = false;
		while ( !done )
		{
			boolean found = false;
			for ( int z = 0; z < dimZ && !found; z++ )
			{
				for ( int y = 0; y < dimY && !found; y++ )
				{
					for ( int x = 0; x < dimX && !found; x++ )
					{
						float value = image.getFloat(x, y, z);
						int index = z*dimX*dimY + y*dimX + x;
						if ( !visited.get(index) )
						{
							if ( value >= threshold )
							{
								seedList.add(new Vector3f(x,y,z));
//								seamCells.set(x, y, z, id);
								visited.set(index);
								found = true;
							}
						}
					}
				}
			}
			if ( !found )
			{
				done = true;
				break;
			}

			Vector3f center = new Vector3f();
			VOIContour cluster = WormSegmentation.fill2(image, threshold, threshold, seedList, visited, center);
			clusterList.getCurves().add(cluster);
			positions.add(center);
			id++;
		}
	}
	 */
}
