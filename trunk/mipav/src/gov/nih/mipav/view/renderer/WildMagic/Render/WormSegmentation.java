package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
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
import java.util.Vector;

import WildMagic.LibFoundation.Intersection.IntrSphere3Sphere3f;
import WildMagic.LibFoundation.Mathematics.Sphere3f;
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

		final float[] sigmas = new float[] {sigma, sigma, sigma * getCorrectionFactor(image)};
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

}
