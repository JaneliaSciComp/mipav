package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.PointStack;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.FileSurface_WM;

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
import WildMagic.LibFoundation.Intersection.IntrSphere3Sphere3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.Sphere3f;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.IndexBuffer;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.Transformation;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

/**
 * Base class for implementing various strategies for automatic segmentation of the seam cells.
 */
public abstract class WormSegmentation
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

	public static HashMap<Float, Integer> estimateHistogram( final ModelImage image, float targetPercent, float[] targetValue )
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
    				float value = image.getFloat(x,y,z);
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
    	float target = targetPercent * dimX*dimY*dimZ;
    	boolean found = false;
    	for ( int i = 0; i < keyArray.length; i++ )
    	{
    		count = map.get( keyArray[i] );
    		runningCount += count;
    		countValues.put( keyArray[i], runningCount );
    		if ( (runningCount >= target) && !found )
    		{
    			found = true;
    			targetValue[0] = keyArray[i];
    		}
    	}
    	map = null;
    	keyArray = null;
    	return countValues;
    }

	
	public static HashMap<Float, Integer> estimateHistogram( final ModelImage image, float[] targetValues, float percentage )
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
    				float value = image.getFloat(x,y,z);
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
    	keyArray = null;
    	return countValues;
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

	public static Vector<Vector3f> findCenters( final ModelImage image, Vector<Vector3f> pts )
	{

		Attributes attr = new Attributes();
		attr.SetPChannels(3);
		StandardMesh std = new StandardMesh(attr);
		TriMesh sphere = std.Sphere(3);
//		System.err.println( sphere.VBuffer.GetVertexQuantity() );
		
		int minDiameter = 5;
		int maxDiameter = 35;

		Vector<Vector3f> centers = new Vector<Vector3f>(); 
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
		
		Vector3f pt = new Vector3f();
		for ( int i = 0; i < pts.size(); i++ )
		{
			pt.copy(pts.elementAt(i));
			int iX = (int)Math.max(0, Math.min( dimX -1, pt.X));
			int iY = (int)Math.max(0, Math.min( dimY -1, pt.Y));
			int iZ = (int)Math.max(0, Math.min( dimZ -1, pt.Z));
			float value = image.getFloat( iX, iY, iZ );
			System.err.print( i + "   " + value + "  " );
			if ( value == 0 )
			{
				if ( isSphereShell( pt, image, minDiameter, maxDiameter, sphere ) )
				{
					centers.add( new Vector3f(pt) );
				}
			}
			System.err.println("");
		}

//		System.err.println( centers.size() );
		return centers;
	}



	public static Vector<Vector3f> findCenters( final ModelImage image, final Vector3f min, final Vector3f max )
	{

		Attributes attr = new Attributes();
		attr.SetPChannels(3);
		StandardMesh std = new StandardMesh(attr);
		TriMesh sphere = std.Sphere(3);
		System.err.println( sphere.VBuffer.GetVertexQuantity() );
		
		int minDiameter = 5;
		int maxDiameter = 30;

		Vector<Vector3f> centers = new Vector<Vector3f>(); 
		
		Vector3f pt = new Vector3f();
		for ( int z = (int)min.Z; z <= max.Z; z++ )
		{
			for ( int y = (int)min.Y; y <= max.Y; y++ )
			{
				for ( int x = (int)min.X; x <= max.X; x++ )
				{
					pt.set(x,y,z);
					float value = image.getFloat( x, y, z );
					if ( value == 0 )
					{
						if ( isSphereShell( pt, image, minDiameter, maxDiameter, sphere ) )
						{
							centers.add( new Vector3f(pt) );
						}
					}
				}
			}
//			if ( centers.size() > 0 )
//			{
			System.err.println( z + " " + centers.size() + " " + Math.round(max.Z) );
//			}
		}

//		System.err.println( centers.size() );
		return centers;
	}


	
	
	public static Vector<Vector3f> findMaxPeaks( ModelImage image, float minPeakVal )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		Vector<Vector3f> peakPositions = new Vector<Vector3f>();

		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				double maxRow = -Float.MAX_VALUE;
				int maxRowIndex = -1;
				for ( int x = 0; x < dimX; x++ )
				{
					double val = image.getFloat( x, y, z );
					if ( val > maxRow )
					{
						maxRow = val;
						maxRowIndex = x;
					}
				}
				if ( maxRow >= minPeakVal )
				{
					Vector3f pos = new Vector3f( maxRowIndex, y, z );
					if ( !peakPositions.contains( pos ) )
					{
						peakPositions.add( pos );
					}
				}
			}
		}

		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				double maxColumn = -Float.MAX_VALUE;
				int maxColumnIndex = -1;
				for ( int y = 0; y < dimY; y++ )
				{
					double val = image.getFloat( x, y, z );
					if ( val > maxColumn )
					{
						maxColumn = val;
						maxColumnIndex = y;
					}
				}
				if ( maxColumn >= minPeakVal )
				{
					Vector3f pos = new Vector3f( x, maxColumnIndex, z );
					if ( !peakPositions.contains( pos ) )
					{
						peakPositions.add( pos );
					}
				}
			}
		}
		for ( int x = 0; x < dimX; x++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				double maxDepth = -Float.MAX_VALUE;
				int maxDepthIndex = -1;
				for ( int z = 0; z < dimZ; z++ )
				{
					double val = image.getFloat( x, y, z );
					if ( val > maxDepth )
					{
						maxDepth = val;
						maxDepthIndex = z;
					}
				}
				if ( maxDepth >= minPeakVal )
				{
					Vector3f pos = new Vector3f( x, y, maxDepthIndex );
					if ( !peakPositions.contains( pos ) )
					{
						peakPositions.add( pos );
					}
				}
			}
		}
		for ( int x = 0; x < dimX; x++ )
		{
			for ( int z = 0; z < dimZ; z++ )
			{
				double maxDepth = -Float.MAX_VALUE;
				int maxDepthIndex = -1;
				for ( int y = 0; y < dimY; y++ )
				{
					double val = image.getFloat( x, y, z );
					if ( val > maxDepth )
					{
						maxDepth = val;
						maxDepthIndex = y;
					}
				}
				if ( maxDepth >= minPeakVal )
				{
					Vector3f pos = new Vector3f( x, maxDepthIndex, z );
					if ( !peakPositions.contains( pos ) )
					{
						peakPositions.add( pos );
					}
				}
			}
		}
		for ( int y = 0; y < dimY; y++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				double maxDepth = -Float.MAX_VALUE;
				int maxDepthIndex = -1;
				for ( int z = 0; z < dimZ; z++ )
				{
					double val = image.getFloat( x, y, z );
					if ( val > maxDepth )
					{
						maxDepth = val;
						maxDepthIndex = z;
					}
				}
				if ( maxDepth >= minPeakVal )
				{
					Vector3f pos = new Vector3f( x, y, maxDepthIndex );
					if ( !peakPositions.contains( pos ) )
					{
						peakPositions.add( pos );
					}
				}
			}
		}
		for ( int y = 0; y < dimY; y++ )
		{
			for ( int z = 0; z < dimZ; z++ )
			{
				double maxDepth = -Float.MAX_VALUE;
				int maxDepthIndex = -1;
				for ( int x = 0; x < dimX; x++ )
				{
					double val = image.getFloat( x, y, z );
					if ( val > maxDepth )
					{
						maxDepth = val;
						maxDepthIndex = x;
					}
				}
				if ( maxDepth >= minPeakVal )
				{
					Vector3f pos = new Vector3f( maxDepthIndex, y, z );
					if ( !peakPositions.contains( pos ) )
					{
						peakPositions.add( pos );
					}
				}
			}
		}
		
		return peakPositions;
	}

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
	
	private static int ellipseMatch( Vector<Vector3f> data, VOIContour ellipse )
	{
		int count = 0;
		for ( int i = 0; i < ellipse.size(); i++ )
		{
			if ( data.contains(ellipse.elementAt(i)) )
			{
				count++;
			}
//			Vector3f pt = ellipse.elementAt(i);
//			int x = (int) pt.X;
//			int y = (int) pt.Y;
//			if ( data[y * dimX + x ] != 0 )
//			{
//				count++;
//			}
		}
		return count;
	}
	
	
	public static VOIContour findLargestConnected2( ModelImage image, Vector3f center, Vector3f left, Vector3f right, float radius, 
			int dimX, int dimY, int dimZ, int min, int max, int ID )
	{		
		Vector3f center2D = new Vector3f( dimX/2, dimY/2, 0);
		Vector3f pt = new Vector3f(0,0,0);
		
		Vector<BitSet> components = new Vector<BitSet>();
		BitSet visited = new BitSet(dimZ*dimY*dimX);
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					int index = z*dimY*dimX + y*dimX + x;
					int value = image.getInt(index * 4 + 0);
					if ( value == 0 )
					{
						visited.set(index);
						continue;
					}
					
					value = image.getInt(index * 4 + 1);
					if ( (value >= min) && (value <= max) )
					{
						if ( !visited.get(index) )
						{
							BitSet filled = new BitSet(dimZ*dimY*dimX);
							Vector<Vector3f> seeds = new Vector<Vector3f>();
							seeds.add( new Vector3f(x, y, z) );
							fillMask( image, visited, filled, seeds, dimX, dimY, dimZ, min, max, true );
							if ( filled.cardinality() > 1 )
							{
								components.add(filled);
								//							System.err.println( x + " " + y + " " + z + "    " + filled.cardinality() );
							}
						}
					}
				}
			}
		}

		visited.clear();
		for ( int i = 0; i < components.size(); i++ )
		{
			visited.or(components.elementAt(i));
		}

		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					int index = z*dimY*dimX + y*dimX + x;
					if ( visited.get(index) )
					{
						image.setC(x, y, 1, (byte) ( (255 & 0x000000ff)) );						
					}
				}
			}
		}
		visited.clear();
		
		components.removeAllElements();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					int index = z*dimY*dimX + y*dimX + x;
					int value = image.getInt(index * 4 + 0);
					if ( value == 0 )
					{
						visited.set(index);
						continue;
					}
					
					value = image.getInt(index * 4 + 1);
					if ( (value >= min) && (value <= max) )
					{
						if ( !visited.get(index) )
						{
							BitSet filled = new BitSet(dimZ*dimY*dimX);
							Vector<Vector3f> seeds = new Vector<Vector3f>();
							seeds.add( new Vector3f(x, y, z) );
							fillMask( image, visited, filled, seeds, dimX, dimY, dimZ, min, max, true );
							if ( filled.cardinality() > 1 )
							{
								components.add(filled);
								//							System.err.println( x + " " + y + " " + z + "    " + filled.cardinality() );
							}
						}
					}
				}
			}
		}

		int maxIndex = -1;
		int maxSize = -1;
		for ( int i = 0; i < components.size(); i++ )
		{
			int c = components.elementAt(i).cardinality();
			if ( c > maxSize )
			{
				maxSize = c;
				maxIndex = i;
			}
		}
		
		if ( maxIndex != -1 )
		{
			visited.clear();
			visited.or( components.remove(maxIndex) );
			
			
//			maxIndex = -1;
//			maxSize = -1;
//			for ( int i = 0; i < components.size(); i++ )
//			{
//				int c = components.elementAt(i).cardinality();
//				if ( c > maxSize )
//				{
//					maxSize = c;
//					maxIndex = i;
//				}
//			}
//			if ( maxIndex != -1 )
//			{
//				int c = components.elementAt(maxIndex).cardinality();
//				if ( c > (.05*dimX*dimY) )
//				{
//					visited.or( components.remove(maxIndex) );
//					maxIndex = -1;
//					maxSize = -1;
//					for ( int i = 0; i < components.size(); i++ )
//					{
//						c = components.elementAt(i).cardinality();
//						if ( c > maxSize )
//						{
//							maxSize = c;
//							maxIndex = i;
//						}
//					}
//					if ( maxIndex != -1 )
//					{
//						c = components.elementAt(maxIndex).cardinality();
//						if ( c > (.05*dimX*dimY) )
//						{
//							visited.or( components.remove(maxIndex) );
//						}
//					}
//				}
//			}
			
			
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						int index = z*dimY*dimX + y*dimX + x;
						if ( visited.get(index) )
						{
							image.setC(x, y, 2, (byte) ( (255 & 0x000000ff)) );						
						}
					}
				}
			}
		}
		
//		if ( true )
//			return null;
		
	
		VOIContour ellipse = new VOIContour(true);
		final int numPts = 360;
		for (int i = 0; i < numPts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numPts);
			final double s = Math.sin(Math.PI * 2.0 * i / numPts);
			final Vector3f pos1 = Vector3f.scale((float) (3 * c), Vector3f.UNIT_X);
			final Vector3f pos2 = Vector3f.scale((float) (3 * s), Vector3f.UNIT_Y);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center2D);
			ellipse.addElement(pos);
		}
		
		for ( int i = 0; i < ellipse.size(); i++ )
		{
			Vector3f dir = Vector3f.sub( ellipse.elementAt(i), center2D );
			dir.normalize();
			pt = ellipse.elementAt(i);
			pt.add(dir);
			while ( (pt.X >= 0) && (pt.X < dimX) && (pt.Y >= 0) && (pt.Y < dimY) )
//				while ( (center2D.distance(pt) < radius) && (pt.X >= 0) && (pt.X < dimX) && (pt.Y >= 0) && (pt.Y < dimY) )
			{
				int x = (int) pt.X;
				int y = (int) pt.Y;
				float valueA = image.getFloatC( x, y, 0, 0);
				float valueR = image.getFloatC( x, y, 0, 1);
				float valueG = image.getFloatC( x, y, 0, 2);
				float valueB = image.getFloatC( x, y, 0, 3);
				if ( ((valueR != valueB) && (valueR == 255) && (valueR == valueG)) || (valueA == 0) )
				{
					break;
				}
				pt.add(dir);
			}
		}

		Vector<Float> distances = new Vector<Float>();
		float average = 0;
		for ( int i = 0; i < ellipse.size(); i++ )
		{
			distances.add(center2D.distance(ellipse.elementAt(i)));
			average += center2D.distance(ellipse.elementAt(i));			
		}
		average /= (float)ellipse.size();		
		
		float std = 0;
		for ( int i = 0; i < ellipse.size(); i++ )
		{
			std += ((distances.elementAt(i) - average)*(distances.elementAt(i) - average));
		}
		std /= (float)ellipse.size();
		std = (float) Math.sqrt(std);

		int size = ellipse.size();
		boolean changing = true;
		while ( changing )
		{
			size = ellipse.size();
			int stepSize = 25;
			for ( int i = ellipse.size() - 1; i >= 0; i-- )
			{
				if ( distances.elementAt(i) > (average + 2 * std) )
				{
					boolean found = false;
					for ( int j = Math.max(0, i - stepSize); j < i; j++ )
					{
						if ( distances.elementAt(j) < 2*average )
						{
							found = true;
							break;
						}
					}
					if ( !found )
						continue;
					for ( int j = Math.min(size - 1, i + 1); j < Math.min( size - 1, i + stepSize + 1); j++ )
					{
						if ( distances.elementAt(i) > (average + 2 * std) )
						{
							ellipse.remove(i);
							distances.remove(i);
//							System.err.println( ID + " : removing " + i );
							break;
						}
					}
				}
			}
			changing = (size != ellipse.size());
		}

		for ( int i = 0; i < ellipse.size(); i++ )
		{
			if ( distances.elementAt(i) > (average + 2 * std) )
			{
				Vector3f dir = Vector3f.sub( center2D, ellipse.elementAt(i) );
				dir.normalize();
				float maxVal = -1;
				Vector3f maxVec = new Vector3f();
				Vector3f temp = new Vector3f(ellipse.elementAt(i));
				temp.add(dir);
				float dist = center2D.distance(temp);
				while ( dist > average )
				{
					int index = (int) (temp.Y*dimX + temp.X);
					int value = image.getInt(index * 4 + 3);
					if ( value > maxVal )
					{
						maxVal = value;
						maxVec.copy(temp);
					}
					temp.add(dir);
					dist = center2D.distance(temp);
				}
				if ( !maxVec.isEqual(Vector3f.ZERO ) )
				{
					ellipse.elementAt(i).copy(temp);
				}
			}
		}
		
		ellipse.convexHull();
		for ( int i = 0; i < ellipse.size(); i++ )
		{
			Vector3f dir = Vector3f.sub( ellipse.elementAt(i), center2D );
			float length = dir.normalize();
			length += 5;
			dir.scale(length);
			ellipse.elementAt(i).copy(center2D);
			ellipse.elementAt(i).add(dir);			
		}
		
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( ellipse.contains( x, y ) )
					{
						int index = z*dimY*dimX + y*dimX + x;
						int value = image.getInt(index * 4 + 3);
						image.setC(x, y, 0, (byte) ( (255 & 0x000000ff)) );
						image.setC(x, y, 1, (byte) ( (value & 0x000000ff)) );
						image.setC(x, y, 2, (byte) ( (value & 0x000000ff)) );
						image.setC(x, y, 3, (byte) ( (value & 0x000000ff)) );
					}
					else
					{
						image.setC(x, y, 0, (byte) ( (0 & 0x000000ff)) );
						image.setC(x, y, 1, (byte) ( (0 & 0x000000ff)) );
						image.setC(x, y, 2, (byte) ( (0 & 0x000000ff)) );
						image.setC(x, y, 3, (byte) ( (0 & 0x000000ff)) );								
					}
				}
			}
		}
		
		return ellipse;
	}

	public static void outline( String directory, ModelImage image, ModelImage mp, int slice )
	{
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
		
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_Outline_MP_Z";
		
		ModelImage blur = blur(image, 5);

		BitSet insideX = new BitSet(dimZ*dimY*dimX);
		BitSet insideY = new BitSet(dimZ*dimY*dimX);
		BitSet insideZ = new BitSet(dimZ*dimY*dimX);
		int offSet = 20;
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				float startVal = blur.getFloat(offSet, y, z);
				boolean fill = false;
				for ( int x = offSet; x < dimX -offSet; x++ )
				{
					float val = blur.getFloat(x, y, z);
					if ( val > 1.5 * startVal )
					{
						fill = true;
					}
					else if ( fill )
					{
						break;
					}
					if ( fill )
					{
						insideX.set(z*dimY*dimX + y*dimX + x);
					}
				}
				startVal = blur.getFloat(dimX - offSet, y, z);
				fill = false;
				for ( int x = dimX - offSet; x >= offSet; x-- )
				{
					float val = blur.getFloat(x, y, z);
					if ( val > 1.5 * startVal )
					{
						fill = true;
					}
					else if ( fill )
					{
						break;
					}
					if ( insideX.get(z*dimY*dimX + y*dimX + x) )
					{
						break;
					}
					if ( fill )
					{
						insideX.set(z*dimY*dimX + y*dimX + x);
					}
				}
			}
		}
		

		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				float startVal = blur.getFloat(x, offSet, z);
				boolean fill = false;
				for ( int y = offSet; y < dimY - offSet; y++ )
				{
					float val = blur.getFloat(x, y, z);
					if ( val > 1.5 * startVal )
					{
						fill = true;
					}
					else if ( fill )
					{
						break;
					}
					if ( fill )
					{
						insideY.set(z*dimY*dimX + y*dimX + x);
					}
				}
				startVal = blur.getFloat(x, dimY - offSet, z);
				fill = false;
				for ( int y = dimY - offSet; y >= offSet; y-- )
				{
					float val = blur.getFloat(x, y, z);
					if ( val > 1.5 * startVal )
					{
						fill = true;
					}
					else if ( fill )
					{
						break;
					}
					if ( insideY.get(z*dimY*dimX + y*dimX + x) )
					{
						break;
					}
					if ( fill )
					{
						insideY.set(z*dimY*dimX + y*dimX + x);
					}
				}
			}
		}
		


		for ( int y = 0; y < dimY; y++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				float startVal = blur.getFloat(x, y, offSet);
				boolean fill = false;
				for ( int z = offSet; z < dimZ - offSet; z++ )
				{
					float val = blur.getFloat(x, y, z);
					if ( val > 1.5 * startVal )
					{
						fill = true;
					}
					else if ( fill )
					{
						break;
					}
					if ( fill )
					{
						insideZ.set(z*dimY*dimX + y*dimX + x);
					}
				}
				startVal = blur.getFloat(x, y, dimZ - offSet);
				fill = false;
				for ( int z = dimZ - offSet; z >= offSet; z-- )
				{
					float val = blur.getFloat(x, y, z);
					if ( val > 1.5 * startVal )
					{
						fill = true;
					}
					else if ( fill )
					{
						break;
					}
					if ( insideZ.get(z*dimY*dimX + y*dimX + x) )
					{
						break;
					}
					if ( fill )
					{
						insideZ.set(z*dimY*dimX + y*dimX + x);
					}
				}
			}
		}
		insideX.and(insideY);
		insideX.and(insideZ);
		
		
		Vector<BitSet> components = new Vector<BitSet>();
		BitSet visited = new BitSet(dimZ*dimY*dimX);
		for ( int y = 0; y < dimY; y++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				for ( int z = 0; z < dimZ; z++ )
				{
					int index = z*dimY*dimX + y*dimX + x;
					if ( insideX.get(index) && !visited.get(index) )
					{
						BitSet filled = new BitSet(dimZ*dimY*dimX);
						Vector<Vector3f> seeds = new Vector<Vector3f>();
						seeds.add( new Vector3f(x, y, z) );
						fillMask( insideX, visited, filled, seeds, dimX, dimY, dimZ );
						if ( filled.cardinality() > 1 )
						{
							components.add(filled);
//							System.err.println( x + " " + y + " " + z + "    " + filled.cardinality() );
						}
					}
				}
			}
		}
		
		int maxIndex = -1;
		int max = -1;
		for ( int i = 0; i < components.size(); i++ )
		{
			if ( components.elementAt(i).cardinality() > max )
			{
				max = components.elementAt(i).cardinality();
				maxIndex = i;
			}
		}
//		
		
		if ( maxIndex != -1 )
		{
			BitSet largest = components.elementAt(maxIndex);			
			blur = blur(image, 3);

//			Vector<Vector3f> surfacePoints = new Vector<Vector3f>();

			float[] targetP = new float[2];
			estimateHistogram( image, targetP, .85f );
			System.err.println( targetP[0] + " " + targetP[1] );
			float targetValue = targetP[0];
			int newCount = 0;
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX - 5; x++ )
					{
						float value = blur.getFloat(x, y, z);
						int index = z*dimX*dimY + y*dimX + x;
						if ( (value > targetValue) && largest.get(index) )
						{
							mp.set(x,y,z,1);
							newCount++;
							break;
						}
					}
					for ( int x = dimX - 5; x >= 0; x-- )
					{
						float value = blur.getFloat(x, y, z);
						int index = z*dimX*dimY + y*dimX + x;
						if ( (value > targetValue) && largest.get(index) )
						{
							mp.set(x,y,z,1);
							newCount++;
							break;
						}
					}
				}
				for ( int x = 0; x < dimX; x++ )
				{
					for ( int y = 0; y < dimY - 5; y++ )
					{
						float value = blur.getFloat(x, y, z);
						int index = z*dimX*dimY + y*dimX + x;
						if ( (value > targetValue) && largest.get(index) )
						{
							mp.set(x,y,z,1);
							newCount++;
							break;
						}
					}
					for ( int y = dimY - 5; y >= 0; y-- )
					{
						float value = blur.getFloat(x, y, z);
						int index = z*dimX*dimY + y*dimX + x;
						if ( (value > targetValue) && largest.get(index) )
						{
							mp.set(x,y,z,1);
							newCount++;
							break;
						}
					}
				}
			}
			for ( int x = 0; x < dimX; x++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int z = 0; z < dimZ - 5; z++ )
					{
						float value = blur.getFloat(x, y, z);
						int index = z*dimX*dimY + y*dimX + x;
						if ( (value > targetValue) && largest.get(index) )
						{
							mp.set(x,y,z,1);
							newCount++;
							break;
						}
					}
					for ( int z = dimZ - 5; z >= 0; z-- )
					{
						float value = blur.getFloat(x, y, z);
						int index = z*dimX*dimY + y*dimX + x;
						if ( (value > targetValue) && largest.get(index) )
						{
							mp.set(x,y,z,1);
							newCount++;
							break;
						}
					}
				}
			}
				
			System.err.println(newCount);
//			mp.calcMinMax();
//			new ViewJFrameImage((ModelImage)mp.clone());
//			
//			newCount = 0;
//
//			for ( int z = 0; z < dimZ; z++ )
//			{
//				for ( int y = 0; y < dimY; y++ )
//				{
//					int upCount = 0;
//					int downCount = 0;
//					int sameCount = 0;
//					float previousValue = -1;
//					for ( int x = 0; x < dimX - 5; x++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						int index = z * dimX*dimY + y * dimX + x;						
//						if ( !largest.get(index) )
//						{
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;
//							previousValue = -1;
//							continue;
//						}
//						if ( previousValue == -1 )
//						{
//							previousValue = value;
//							continue;
//						}
//						if ( value > previousValue )
//						{
//							if ( (downCount != 0) || (sameCount != 0) )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;								
//							}
//							upCount++;
//						}
//						if ( value == previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								sameCount++;
//							}
//						}
//						if ( value < previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								downCount++;
//							}
//						}
//						if ( (upCount >= 2) && (downCount >= 2) && (sameCount < 4) )
//						{
//							// found a 'peak'
//							if ( mp.getFloat((int)(x - (downCount + Math.floor(sameCount/2f))), y, z) == 0 )
//							{
//								newCount++;
//							}
//							mp.set((int)(x - (downCount + Math.floor(sameCount/2f))), y, z, 1f);
//							surfacePoints.add( new Vector3f(x,y,z) );
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;		
//						}
//						previousValue = value;
//					}
//					upCount = 0;
//					downCount = 0;
//					sameCount = 0;
//					previousValue = -1;
//					for ( int x = dimX - 5; x >= 0; x-- )
//					{
//						float value = blur.getFloat(x, y, z);
//						int index = z * dimX*dimY + y * dimX + x;
//						if ( !largest.get(index) )
//						{
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;
//							previousValue = -1;
//							continue;
//						}
//						if ( previousValue == -1 )
//						{
//							previousValue = value;
//							continue;
//						}
//						if ( value > previousValue )
//						{
//							if ( (downCount != 0) || (sameCount != 0) )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;								
//							}
//							upCount++;
//						}
//						if ( value == previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								sameCount++;
//							}
//						}
//						if ( value < previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								downCount++;
//							}
//						}
//						if ( (upCount >= 2) && (downCount >= 2) && (sameCount < 4) )
//						{
//							// found a 'peak'
//							if ( mp.getFloat((int)(x + (downCount + Math.ceil(sameCount/2f))), y, z) == 0 )
//							{
//								newCount++;
//							}
//							mp.set((int)(x + (downCount + Math.ceil(sameCount/2f))), y, z, 1f);
//							surfacePoints.add( new Vector3f(x,y,z) );
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;		
//						}
//						previousValue = value;
//					}
//				}
//				
//
//				for ( int x = 0; x < dimX; x++ )
//				{
//					int upCount = 0;
//					int downCount = 0;
//					int sameCount = 0;
//					float previousValue = -1;
//					for ( int y = 0; y < dimY - 5; y++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						int index = z * dimX*dimY + y * dimX + x;
//						if ( !largest.get(index) )
//						{
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;
//							previousValue = -1;
//							continue;
//						}
//						if ( previousValue == -1 )
//						{
//							previousValue = value;
//							continue;
//						}
//						if ( value > previousValue )
//						{
//							if ( (downCount != 0) || (sameCount != 0) )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;								
//							}
//							upCount++;
//						}
//						if ( value == previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								sameCount++;
//							}
//						}
//						if ( value < previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								downCount++;
//							}
//						}
//						if ( (upCount >= 2) && (downCount >= 2) && (sameCount < 4) )
//						{
//							// found a 'peak'
//							if ( mp.getFloat(x, (int)(y - (downCount + Math.floor(sameCount/2f))), z) == 0 )
//							{
//								newCount++;
//							}
//							mp.set(x, (int)(y - (downCount + Math.floor(sameCount/2f))), z, 1f);
//							surfacePoints.add( new Vector3f(x,y,z) );
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;		
//						}
//						previousValue = value;
//					}
//					upCount = 0;
//					downCount = 0;
//					sameCount = 0;
//					previousValue = -1;
//					for ( int y = dimY - 5; y >= 0; y-- )
//					{
//						float value = blur.getFloat(x, y, z);
//						int index = z * dimX*dimY + y * dimX + x;
//						if ( !largest.get(index) )
//						{
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;
//							previousValue = -1;
//							continue;
//						}
//						if ( previousValue == -1 )
//						{
//							previousValue = value;
//							continue;
//						}
//						if ( value > previousValue )
//						{
//							if ( (downCount != 0) || (sameCount != 0) )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;								
//							}
//							upCount++;
//						}
//						if ( value == previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								sameCount++;
//							}
//						}
//						if ( value < previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								downCount++;
//							}
//						}
//						if ( (upCount >= 2) && (downCount >= 2) && (sameCount < 4) )
//						{
//							// found a 'peak'
//							if ( mp.getFloat(x, (int)(y + (downCount + Math.ceil(sameCount/2f))), z) == 0 )
//							{
//								newCount++;
//							}
//							mp.set(x, (int)(y + (downCount + Math.ceil(sameCount/2f))), z, 1f);
//							surfacePoints.add( new Vector3f(x,y,z) );
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;		
//						}
//						previousValue = value;
//					}
//				}
//			}
//			for ( int x = 0; x < dimX; x++ )
//			{
//				for ( int y = 0; y < dimY; y++ )
//				{
//					int upCount = 0;
//					int downCount = 0;
//					int sameCount = 0;
//					float previousValue = -1;
//					for ( int z = 0; z < dimZ - 5; z++ )
//					{
//						float value = blur.getFloat(x, y, z);
//						int index = z * dimX*dimY + y * dimX + x;
//						if ( !largest.get(index) )
//						{
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;
//							previousValue = -1;
//							continue;
//						}
//						if ( previousValue == -1 )
//						{
//							previousValue = value;
//							continue;
//						}
//						if ( value > previousValue )
//						{
//							if ( (downCount != 0) || (sameCount != 0) )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;								
//							}
//							upCount++;
//						}
//						if ( value == previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								sameCount++;
//							}
//						}
//						if ( value < previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								downCount++;
//							}
//						}
//						if ( (upCount >= 2) && (downCount >= 2) && (sameCount < 4) )
//						{
//							// found a 'peak'
//							if ( mp.getFloat(x, y, (int)(z - (downCount + Math.floor(sameCount/2f)))) == 0 )
//							{
//								newCount++;
//							}
//							mp.set(x, y, (int)(z - (downCount + Math.floor(sameCount/2f))), 1f);
//							surfacePoints.add( new Vector3f(x,y,z) );
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;		
//						}
//						previousValue = value;
//					}
//					upCount = 0;
//					downCount = 0;
//					sameCount = 0;
//					previousValue = -1;
//					for ( int z = dimZ - 5; z >= 0; z-- )
//					{
//						float value = blur.getFloat(x, y, z);
//						int index = z * dimX*dimY + y * dimX + x;
//						if ( !largest.get(index) )
//						{
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;
//							previousValue = -1;
//							continue;
//						}
//						if ( previousValue == -1 )
//						{
//							previousValue = value;
//							continue;
//						}
//						if ( value > previousValue )
//						{
//							if ( (downCount != 0) || (sameCount != 0) )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;								
//							}
//							upCount++;
//						}
//						if ( value == previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								sameCount++;
//							}
//						}
//						if ( value < previousValue )
//						{
//							if ( upCount < 2 )
//							{
//								upCount = 0;
//								downCount = 0;
//								sameCount = 0;									
//							}
//							else
//							{
//								downCount++;
//							}
//						}
//						if ( (upCount >= 2) && (downCount >= 2) && (sameCount < 4) )
//						{
//							// found a 'peak'
//							if ( mp.getFloat(x, y, (int)(z + (downCount + Math.ceil(sameCount/2f)))) == 0 )
//							{
//								newCount++;
//							}
//							mp.set(x, y, (int)(z + (downCount + Math.ceil(sameCount/2f))), 1f);
//							surfacePoints.add( new Vector3f(x,y,z) );
//							upCount = 0;
//							downCount = 0;
//							sameCount = 0;		
//						}
//						previousValue = value;
//					}
//				}
//			}
//				
//			System.err.println(newCount);
			
			
			
			
			
//			Vector<Vector3f> neighbors = new Vector<Vector3f>();
//			
//			newCount = 0;
//			for ( int z = 5; z < dimZ-5; z++ )
//			{
//				for ( int y = 5; y < dimY-5; y++ )
//				{
//					for ( int x = 5; x < dimX-5; x++ )
//					{
//						if ( mp.getFloat(x, y, z) == 1 )
//						{
//							neighbors.clear();
//							for ( int z1 = Math.max(0, z - 5); z1 <= Math.min(z + 5, dimZ - 1); z1++ )
//							{
//								for ( int y1 = Math.max(0, y - 5); y1 <= Math.min(y + 5, dimY - 1); y1++ )
//								{
//									for ( int x1 = Math.max(0, x - 5); x1 <= Math.min(x + 5, dimX - 1); x1++ )
//									{
//										if ( (x1 != x) && (y1 != y) && (z1 != z) && mp.getFloat(x1, y1, z1) == 1 )
//										{
//											neighbors.add( new Vector3f(x1,y1,z1) );
//										}
//									}
//								}
//							}
//							Vector3f pt = new Vector3f(x,y,z);
//							float minDist = Float.MAX_VALUE;
//							int minIndex = -1;
//							for ( int i = 0; i < neighbors.size(); i++ )
//							{
//								float distance = neighbors.elementAt(i).distance(pt);
//								if ( (distance > 1) && (distance < minDist) )
//								{
//									minDist = distance;
//									minIndex = i;
//								}
//							}
//							if ( minIndex != -1 )
//							{
//								Vector3f closest1 = new Vector3f();
//								closest1.copy( neighbors.remove(minIndex) );
//								
//								if ( closest1.distance(pt) > 1 )
//								{
//									Vector3f start = new Vector3f(pt);
//									Vector3f dir = Vector3f.sub( closest1, pt );
//									float length = dir.normalize();
//									for ( int i = 0; i <= length; i++ )
//									{
//										start.add(dir);
//										if ( mp.getFloat( Math.round(start.X), Math.round(start.Y), Math.round(start.Z) ) != 1 )
//										{
//											mp.set( Math.round(start.X), Math.round(start.Y), Math.round(start.Z), 1);
//											surfacePoints.add( new Vector3f( Math.round(start.X), Math.round(start.Y), Math.round(start.Z)) );
//											newCount++;
//										}
//									}
//								}
//							}
//							minDist = Float.MAX_VALUE;
//							minIndex = -1;
//							for ( int i = 0; i < neighbors.size(); i++ )
//							{
//								float distance = neighbors.elementAt(i).distance(pt);
//								if ( (distance > 1) && (distance < minDist) )
//								{
//									minDist = distance;
//									minIndex = i;
//								}
//							}
//							if ( minIndex != -1 )
//							{
//								Vector3f closest2 = new Vector3f();
//								closest2.copy( neighbors.remove(minIndex) );
//								if ( closest2.distance(pt) > 1 )
//								{
//									Vector3f start = new Vector3f(pt);
//									Vector3f dir = Vector3f.sub( closest2, pt );
//									float length = dir.normalize();
//									for ( int i = 0; i <= length; i++ )
//									{
//										start.add(dir);
//										if ( mp.getFloat( Math.round(start.X), Math.round(start.Y), Math.round(start.Z) ) != 1 )
//										{
//											mp.set( Math.round(start.X), Math.round(start.Y), Math.round(start.Z), 1);
//											surfacePoints.add( new Vector3f( Math.round(start.X), Math.round(start.Y), Math.round(start.Z)) );
//											newCount++;
//										}
//									}
//								}
//							}
//						}
//					}
//				}
//			}
//			System.err.println( newCount );
			
//			
			
			
			
			
			
			
//			newCount = 0;
//			insideX.and(insideY);
//			insideX.and(insideZ);
//			Vector<Vector3f> seeds = new Vector<Vector3f>();
//			for ( int z = 0; z < dimZ; z++ )
//			{
//				for ( int y = 0; y < dimY; y++ )
//				{
//					for ( int x = 0; x < dimX; x++ )
//					{
//						if ( insideX.get(z*dimY*dimX + y*dimX + x) )
//						{
//							newCount++;
//							seeds.add( new Vector3f(x, y, z) );
//						}
//					}
//				}
//			}
//			System.err.println( newCount );
//			if ( seeds.size() > 0 )
//			{
//				Vector<Sphere3f> surfaces = new Vector<Sphere3f>();
//				Sphere3f maxS = findMaxSphere( seeds, surfacePoints, surfaces );
//				while ( (maxS != null) && (seeds.size() > 0) )
//				{
//					surfaces.add(maxS);
//					
//
//					for ( int i = seeds.size() - 1; i >= 0; i-- )
//					{
//						for ( int j = 0; j < surfaces.size(); j++ )
//						{
//							if ( Sphere3f.InSphere( seeds.elementAt(i), surfaces.elementAt(j) ) )
//							{
//								seeds.remove(i);
//								break;
//							}
//						}
//					}
//					
//					if ( seeds.size() == 0 )
//					{
//						break;
//					}
//					maxS = findMaxSphere( seeds, surfacePoints, surfaces );
//				}
//				
//				if ( surfaces.size() > 0 )
//				{
//					try {
//						File surfaceFile = new File(directory + JDialogBase.makeImageName( image.getImageName(), "_surface_mesh.xml"));
//						if ( surfaceFile.exists() )
//						{
//							if ( !surfaceFile.delete() )
//							{
//								System.err.println( "Unable to delete " + directory + JDialogBase.makeImageName( image.getImageName(), "_surface_mesh.xml") );
//							}
//						}
//						surfaceFile = new File(directory + JDialogBase.makeImageName( image.getImageName(), "_surface_mesh.sur"));
//						if ( surfaceFile.exists() )
//						{
//							if ( !surfaceFile.delete() )
//							{
//								System.err.println( "Unable to delete " + directory + JDialogBase.makeImageName( image.getImageName(), "_surface_mesh.sur") );
//							}
//						}
//						TriMesh sphereSurface = mergeSpheres( surfaces );
//						
//						FileSurface_WM.save( directory + JDialogBase.makeImageName( image.getImageName(), "_surface_mesh.xml"), sphereSurface, image, true );
//					} catch (IOException e) {
//						e.printStackTrace();
//					}
//				}
//			}
			
			
//			Vector<Vector3f> points = new Vector<Vector3f>();
//			Vector3f center = new Vector3f();
//			int count = 0;
//			visited.clear();
//			for ( int z = 0; z < dimZ; z++ )
//			{
//				for ( int y = 0; y < dimY; y++ )
//				{
//					for ( int x = 0; x < dimX; x++ )
//					{
//						if ( insideX.get(z*dimY*dimX + y*dimX + x) && !visited.get(z*dimY*dimX + y*dimX + x) && (mp.getFloat(x, y, z) == 1))
//						{
//							visited.set( z*dimY*dimX + y*dimX + x );
//							int endX = x;
//							center.set(x,y,z);
//							count = 0;
//							// find edges around point:
//							for ( int x1 = x+1; x1 < dimX; x1++ )
//							{
//								if ( mp.getFloat(x1, y, z) == 1 )
//								{
//									center.add(x1,y,z);
//									endX = x1;
//									count++;
//									break;
//								}
//								visited.set( z*dimY*dimX + y*dimX + x1 );
//							}
//							for ( int y1 = y+1; y1 < dimY; y1++ )
//							{
//								if ( mp.getFloat(x, y1, z) == 1 )
//								{
//									center.add(x,y1,z);
//									count++;
//									break;
//								}
//								visited.set( z*dimY*dimX + y1*dimX + x );
//							}
//							for ( int z1 = z+1; z1 < dimZ; z1++ )
//							{
//								if ( mp.getFloat(x, y, z1) == 1 )
//								{
//									center.add(x,y,z1);
//									count++;
//									break;
//								}
//								visited.set( z1*dimY*dimX + y*dimX + x );
//							}
//							if ( count > 0 )
//							{
//								center.scale(1f/(float)count);
//								if ( !points.contains(center) )
//								{
//									points.add( new Vector3f(center) );
//								}
//								
//								x = endX;
//							}
//						}
//					}
//				}
//			}
//			
//
//			VOI annotations = new VOI( (short)1, "medial_axis", VOI.ANNOTATION, 1);
//			for ( int i = 0; i < points.size(); i++ )
//			{
//				center = points.elementAt(i);
//				VOIText text = new VOIText();
//				text.setText( "x" );
//				text.setColor( new Color(255, 0, 0) );
//				text.add( new Vector3f( center ) );
//				text.add( new Vector3f( center ) );
//				text.lastElement().X++;
//				text.setUseMarker(true);
//				annotations.getCurves().add(text);
//			}
//			System.err.println( largest.cardinality() + " " + insideX.cardinality() + " " + annotations.getCurves().size() );
//			if ( annotations.getCurves().size() > 0 )
//			{
//				mp.registerVOI(annotations);
//			}
//			for ( int y = 0; y < dimY; y++ )
//			{
//				for ( int x = 0; x < dimX; x++ )
//				{
//					for ( int z = 0; z < dimZ; z++ )
//					{
//						if ( largest.get(z*dimY*dimX + y*dimX + x) )
//						{
//							mp.set(x, y, z, 1f);
//						}
//					}
//				}
//			}

//			for ( int z = 0; z < dimZ; z++ )
//			{
//				for ( int y = 0; y < dimY; y++ )
//				{
//					for ( int x = 0; x < dimX - 5; x++ )
//					{
//						int index = z * dimX*dimY + y * dimX + x;
//						if ( largest.get(index) )
//						{
//							mp.set(x, y, z,1);
//						}
//						else
//						{
//							mp.set(x,y,z,0);
//						}
//					}
//				}
//			}

			System.err.println( "Saving mp image to : " + directory + mp.getImageName() + ".tif" );
			ModelImage.saveImage( mp, mp.getImageName() + ".tif", directory, false ); 
			mp.calcMinMax();
			new ViewJFrameImage((ModelImage) mp.clone());
		}
//		else
//		{
//			System.err.println( "FAILED Segmentation" );
//		}
	}
	
	private static Sphere3f findMaxSphere(Vector<Vector3f> seeds, Vector<Vector3f> surfacePoints, Vector<Sphere3f> surfaces)
	{
		int minDimeter = 10;
		int maxIndex = -1;
		float maxDiameter = minDimeter;
		Vector<Sphere3f> spheres = new Vector<Sphere3f>();
		
		for ( int i = 0; i < seeds.size(); i++ )
		{
			boolean found = false;
			for ( int j = 0; j < surfaces.size(); j++ )
			{
				if ( Sphere3f.InSphere( seeds.elementAt(i), surfaces.elementAt(j) ) )
				{
					found = true;
					break;
				}
			}
			if ( !found )
			{
				Sphere3f sphere = new Sphere3f( seeds.elementAt(i), .5f );
				while ( testSphere(sphere, surfacePoints, surfaces) )
				{
					float diameter = growSphere(sphere);
					if ( diameter > maxDiameter )
					{
						maxDiameter = diameter;
						maxIndex = i;
					}
				}
				spheres.add(sphere);
			}
		}
		if ( maxIndex != -1 )
		{
			return spheres.elementAt(maxIndex);
		}
		return null;
	}
	
	private static boolean testSphere( Sphere3f sphere, Vector<Vector3f> surfacePoints, Vector<Sphere3f> spheres )
	{
		Vector<Vector3f> contactPoints = new Vector<Vector3f>();
		for ( int i = 0; i < surfacePoints.size(); i++ )
		{
			if ( Sphere3f.InSphere(surfacePoints.elementAt(i), sphere) )
			{
				contactPoints.add(surfacePoints.elementAt(i) );
			}
		}
		for ( int i = 0; i < spheres.size(); i++ )
		{
			IntrSphere3Sphere3f intersect = new IntrSphere3Sphere3f(sphere, spheres.elementAt(i) );
			if ( intersect.Find() )
			{
				contactPoints.add( intersect.GetCenter() );
			}
		}
		if ( contactPoints.size() == 0 )
		{
			return true;
		}
		Vector3f averageDir = new Vector3f();
		for ( int i = 0; i < contactPoints.size(); i++ )
		{
			Vector3f dir = Vector3f.sub( sphere.Center, contactPoints.elementAt(i) );
			dir.normalize();
			averageDir.add(dir);
		}
		averageDir.scale(1f/(float)contactPoints.size() );
		if ( averageDir.isEqual( Vector3f.ZERO ) )
		{
			return false;
		}
		sphere.Center.add(averageDir);
		for ( int i = 0; i < surfacePoints.size(); i++ )
		{
			if ( Sphere3f.InSphere(surfacePoints.elementAt(i), sphere) )
			{
				return false;
			}
		}
		for ( int i = 0; i < spheres.size(); i++ )
		{
			IntrSphere3Sphere3f intersect = new IntrSphere3Sphere3f(sphere, spheres.elementAt(i) );
			if ( intersect.Find() )
			{
				return false;
			}
		}
		return true;
	}
	
	private static float growSphere( Sphere3f sphere )
	{
		sphere.Radius += .5;
		return sphere.Radius;
	}

	private static TriMesh mergeSpheres( Vector<Sphere3f> spheres )
	{
		Attributes attributes = new Attributes();
		attributes.SetPChannels(3);
		attributes.SetNChannels(3);
		attributes.SetCChannels(0,4);
		StandardMesh std = new StandardMesh(attributes);
		
		int numVertices = 0;
		int numTris = 0;
		Vector<TriMesh> sphereSurfaces = new Vector<TriMesh>();
		for ( int i = 0; i < spheres.size(); i++ )
		{
			Sphere3f sphere = spheres.elementAt(i);
			System.err.println( sphere.Center + " " + sphere.Radius );
			Transformation xfrm = new Transformation();
			xfrm.SetUniformScale( sphere.Radius );
			xfrm.SetTranslate( sphere.Center );
			std.SetTransformation( xfrm );
			TriMesh sphereSurface = std.Sphere(2);
			
			sphereSurfaces.add(sphereSurface);
			numVertices += sphereSurface.VBuffer.GetVertexQuantity();
			numTris += sphereSurface.IBuffer.GetIndexQuantity();
		}
		VertexBuffer vertexBuffer = new VertexBuffer(attributes, numVertices);
		int vertexCount = 0;
		IndexBuffer indexBuffer = new IndexBuffer(numTris);
		int indexCount = 0;
		int offSet = 0;
		for ( int i = 0; i < sphereSurfaces.size(); i++ )
		{
			TriMesh sphereSurface = sphereSurfaces.elementAt(i);
			for ( int j = 0; j < sphereSurface.VBuffer.GetVertexQuantity(); j++ )
			{
				vertexBuffer.SetPosition3( vertexCount, sphereSurface.VBuffer.GetPosition3(j) );
				vertexBuffer.SetNormal3( vertexCount, sphereSurface.VBuffer.GetNormal3(j) );
				vertexBuffer.SetColor4( 0, vertexCount, 0, 0, 1, 1 );
				vertexCount++;
			}
			for ( int j = 0; j < sphereSurface.IBuffer.GetIndexQuantity(); j++ )
			{
				indexBuffer.GetData()[indexCount++] = sphereSurface.IBuffer.GetData()[j] + offSet;
			}
			offSet += sphereSurface.VBuffer.GetVertexQuantity();
		}
		return new TriMesh(vertexBuffer, indexBuffer);
	}
	

	private static void fillMask( BitSet inside, BitSet visited, BitSet connected, Vector<Vector3f> seeds, int dimX, int dimY, int dimZ )
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

			for ( int z1 = Math.max(0, z - 1); z1 <= Math.min(z+1, dimZ-1); z1++ )
			{
				for ( int y1 = Math.max(0, y - 1); y1 <= Math.min(y+1, dimY-1); y1++ )
				{
					for ( int x1 = Math.max(0, x - 1); x1 <= Math.min(x+1, dimX-1); x1++ )
					{
						int index = z1*dimY*dimX + y1*dimX + x1;
						if ( !visited.get(index) && !connected.get(index) && inside.get(index) )
						{
							visited.set(index);
							seeds.add( new Vector3f(x1,y1,z1) );
						}
					}
				}
			}
		}
	}
	

	private static void fillMask( ModelImage image, BitSet visited, BitSet connected, Vector<Vector3f> seeds, 
			int dimX, int dimY, int dimZ, int min, int max, boolean diagonals )
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
			
			if ( diagonals )
			{
				for ( int y1 = Math.max(0, y - 1); y1 <= Math.min(y+1, dimY-1); y1++ )
				{
					for ( int x1 = Math.max(0, x - 1); x1 <= Math.min(x+1, dimX-1); x1++ )
					{
						int index = y1*dimX + x1;
						int value = image.getInt(index * 4 + 0);
						if ( value == 0 )
						{
							visited.set(index);
						}
						else
						{
							value = image.getInt(index * 4 + 1);
							if ( !visited.get(index) && !connected.get(index) && (value >= min) && (value <= max) )
							{
								visited.set(index);
								seeds.add( new Vector3f(x1,y1,0) );
							}
						}
					}
				}
			}
			else
			{

				for ( int x1 = Math.max(0, x - 1); x1 <= Math.min(x+1, dimX-1); x1++ )
				{
					int index = y*dimX + x1;
					int value = image.getInt(index * 4 + 0);
					if ( value == 0 )
					{
						visited.set(index);
					}
					else
					{
						value = image.getInt(index * 4 + 1);
						if ( !visited.get(index) && !connected.get(index) && (value >= min) && (value <= max) )
						{
							visited.set(index);
							seeds.add( new Vector3f(x1,y,0) );
						}
					}
				}

				for ( int y1 = Math.max(0, y - 1); y1 <= Math.min(y+1, dimY-1); y1++ )
				{
					int index = y1*dimX + x;
					int value = image.getInt(index * 4 + 0);
					if ( value == 0 )
					{
						visited.set(index);
					}
					else
					{
						value = image.getInt(index * 4 + 1);
						if ( !visited.get(index) && !connected.get(index) && (value >= min) && (value <= max) )
						{
							visited.set(index);
							seeds.add( new Vector3f(x,y1,0) );
						}
					}
				}
				
			}
		}
	}

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

	public static Vector<Vector3f> segmentImage( final ModelImage image, float cutOff )
	{
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		if (imageName.contains("_laplace")) {
			imageName = imageName.replaceAll("_laplace", "");
		}
		if (imageName.contains("_gblur")) {
			imageName = imageName.replaceAll("_gblur", "");
		}
		imageName = imageName + "_segmentation";
		ModelImage segmentationImage = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, segmentationImage);   

		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;  	

		Vector<Vector3f> seedList = new Vector<Vector3f>();
		int count = 1;
		Vector3f seed = new Vector3f();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( image.getFloat(x, y, z) >= cutOff )
					{
						seed.set(x, y, z);
						seedList.add(seed);
						int numFilled = fill( image, cutOff, cutOff+1, seedList, segmentationImage, count);
						if ( numFilled > 0 )
						{
							count++;
						}
					}
				}
			}
		}
		segmentationImage.calcMinMax();

		count = (int) segmentationImage.getMax();
		Vector3f[] seamCells = new Vector3f[count];
		int[] counts = new int[count];
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					int id = segmentationImage.getInt(x, y, z);
					if ( id > 0 )
					{
						id--;
						if ( seamCells[id] == null )
						{
							seamCells[id] = new Vector3f();
							counts[id] = 0;
						}
						seamCells[id].add(x,y,z);
						counts[id]++;
					}
				}
			}
		}

		Vector<Vector3f> annotations = new Vector<Vector3f>();
		
		for ( int i = 0; i < seamCells.length; i++ )
		{
			if ( counts[i] > 1 )
			{
				System.err.println( i + "   " + counts[i] );
				seamCells[i].scale( 1f/counts[i]);
				annotations.add(seamCells[i]);
			}
			
		}
		return annotations;
	}
	
	
	
	
	public static ModelImage segmentNose(ModelImage image, Vector3f pt, boolean display)
	{

		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 

    	int cubeHalf = 7/2;
		int zStart = (int)Math.max(0, Math.min( dimZ -1, pt.Z - cubeHalf) );
		int zEnd = (int)Math.max(0, Math.min( dimZ -1, pt.Z + cubeHalf) );
		int yStart = (int)Math.max(0, Math.min( dimY -1, pt.Y - cubeHalf) );
		int yEnd = (int)Math.max(0, Math.min( dimY -1, pt.Y + cubeHalf) );
		int xStart = (int)Math.max(0, Math.min( dimX -1, pt.X - cubeHalf) );
		int xEnd = (int)Math.max(0, Math.min( dimX -1, pt.X + cubeHalf) );
		
		float avg = 0;
		int count = 0;
		for ( int z = zStart; z <= zEnd; z++ )
		{
			for ( int y = yStart; y <= yEnd; y++ )
			{
				for ( int x = xStart; x <= xEnd; x++ )
				{
					float value = image.getFloat(x,y,z);
					avg += value;
					count++;
				}
			}    					
		}
		
		avg /= count;
		
		
		
		
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_nose";
		final ModelImage resultImage = new ModelImage(image.getType(), image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, resultImage);

		float value = image.getFloatTriLinearBounds( pt.X, pt.Y, pt.Z );
		Vector<Vector3f> seedList = new Vector<Vector3f>();
		seedList.add(pt);
//		min = 1.5f*min;
////		min = (float) Math.min( value - 1, 0.25 * (value + min/2f) );
//		max = (float) Math.max( max + 1, 0.95 * (max + image.getMax()/2f) );

		float max = 2 * (avg + value)/2f;
		float min = 0.5f*(avg + value)/2f;
		float minLimit = 1;
		float maxLimit = max;
		System.err.println( "SegmentNose " + image.getMin() + " " + image.getMax() + " " + avg + " " + value + " " + min + " " + max );
		int numFilled = fill( image, min, max, seedList, resultImage, 1);
		count = 0;
		float prevMin = -1;
		int prevFilled = -1;
		while ( ((numFilled < 60000) || (numFilled > 100000)) && (count < 100))
		{
			count++;
			System.err.println( "SegmentNose " + numFilled + " " + min + " " + max + " " + count );
			if ( numFilled < 60000 )
			{
				maxLimit = min;
			}
			else if ( numFilled > 100000 )
			{
				minLimit = min;
			}
			if ( (prevFilled == numFilled) || (prevMin == min) )
			{
				max = 0.9f * max;
				minLimit = 1;
				maxLimit = max;
			}
			prevMin = min;
			min = (minLimit + maxLimit)/2f;
			resultImage.setAll(0.0f);
			seedList.clear();
			seedList.add(pt);
			prevFilled = numFilled;
			numFilled = fill( image, min, max, seedList, resultImage, 1);
		}
		resultImage.calcMinMax();
		System.err.println( "SegmentNose " + numFilled + " " + min + " " + max + " " + value );
		if ( display )
		{
			new ViewJFrameImage(resultImage);
		}
		return resultImage;
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

	
	
	private static boolean isSphereShell( Vector3f pt, ModelImage image, int diameterMin, int diameterMax, TriMesh sphere )
	{
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1; 
		
		int iX = (int)Math.max(0, Math.min( dimX -1, pt.X));
		int iY = (int)Math.max(0, Math.min( dimY -1, pt.Y));
		int iZ = (int)Math.max(0, Math.min( dimZ -1, pt.Z));
		float value = image.getFloat( iX, iY, iZ );
		if ( value != 0 )
		{
			return false;
		}
		Vector3f dir = new Vector3f();
		Vector3f pos = new Vector3f();
		VertexBuffer vBuffer = sphere.VBuffer;
		int count = 0;
		int totalCount = 0;
		for ( int i = 0; i < vBuffer.GetVertexQuantity(); i++ )
		{
			vBuffer.GetPosition3(i, dir);
			dir.normalize();
			boolean centerFound = false;
			boolean edgeFound = false;
			boolean outsideFound = false;
			for ( int z = 0; z <= diameterMax; z++ )
			{
				pos.copy(dir);
				pos.scale(z);
				pos.add(pt);
				iX = (int)Math.max(0, Math.min( dimX -1, pos.X));
				iY = (int)Math.max(0, Math.min( dimY -1, pos.Y));
				iZ = (int)Math.max(0, Math.min( dimZ -1, pos.Z));
				value = image.getFloat( iX, iY, iZ );
				if ( (value == image.getMin()) && !centerFound )
				{
					centerFound = true;
				}
				if ( (value > image.getMin()) && !centerFound )
				{
					break;
				}
				if ( (value > image.getMin()) && centerFound && !edgeFound )
				{
//					if ( z < diameterMin )
//					{
//						break;
//					}
					edgeFound = true;
				}
				if ( (value == image.getMin()) && centerFound && edgeFound && !outsideFound )
				{
					outsideFound = true;
					break;
				}
			}
			if ( centerFound && edgeFound && outsideFound )
			{
				count++;
			}
			totalCount++;
		}
		System.err.print( count + "   " + totalCount + "   " + (((float)count/(float)totalCount)) + "   " + (((float)count/(float)totalCount) >= 0.60));
		return (((float)count/(float)totalCount) >= 0.60);
	}
	
	

	protected Vector<Vector3f> results;


	protected String outputDir;
	public WormSegmentation() {}
	
	public String getOutputDir()
	{
		return outputDir;
	}
	public Vector<Vector3f> getResults()
	{
		return results;
	}
	public void setOutputDir( String dir )
	{
		outputDir = new String(dir);
	}

	
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
	
	
}
