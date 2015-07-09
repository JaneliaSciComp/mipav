package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.CountDownLatch;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.SceneGraph.Attributes;
import WildMagic.LibGraphics.SceneGraph.StandardMesh;
import WildMagic.LibGraphics.SceneGraph.TriMesh;
import WildMagic.LibGraphics.SceneGraph.VertexBuffer;

public abstract class WormSegmentation
{
	public WormSegmentation() {}

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

	
	public static void saveAllVOIsTo(String voiDir, final ModelImage image)
	{		
		try {
			final ViewVOIVector VOIs = image.getVOIs();

			final File voiFileDir = new File(voiDir);

			if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
				final String[] list = voiFileDir.list();
				for (int i = 0; i < list.length; i++) {
					final File lrFile = new File(voiDir + list[i]);
					lrFile.delete();
				}
			} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
			} else { // voiFileDir does not exist
				voiFileDir.mkdir();
			}

			final int nVOI = VOIs.size();

			for (int i = 0; i < nVOI; i++) {
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

		image.registerVOI(annotations);
	}
	
	public static int reduceDuplicates( ModelImage image, Vector<Vector3f> tempSeamCells, boolean deleteSingletons )
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
						if ( distance < 3 )
						{
							newCenter.add(tempSeamCells.elementAt(k));
							tempSeamCells.elementAt(k).copy(negCenter);
							count++;
						}
						else if ( distance < 10 )
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
					newCenter.scale(1f/(float)count);
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

	// need edges
	public static int fill(final ModelImage image, float cutOff, final Vector<Vector3f> seedList, ModelImage visited, final int id) {
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
								if ( value >= cutOff )
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


	/**
	 * Returns the amount of correction which should be applied to the z-direction sigma (assuming that correction is
	 * requested).
	 * 
	 * @return the amount to multiply the z-sigma by to correct for resolution differences
	 */
	private static float getCorrectionFactor(final ModelImage image) {
		final int index = image.getExtents()[2] / 2;
		final float xRes = image.getFileInfo(index).getResolutions()[0];
		final float zRes = image.getFileInfo(index).getResolutions()[2];

		return xRes / zRes;
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
						int numFilled = fill( image, cutOff, seedList, segmentationImage, count);
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
				seamCells[i].scale( 1f/(float)counts[i]);
				annotations.add(seamCells[i]);
			}
			
		}
		return annotations;
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
		meanVariance /= (float)count;
		return meanVariance;
	}


}
