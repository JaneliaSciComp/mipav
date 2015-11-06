package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmLaplacian;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.Color;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;

public class WormSegmentationLoG extends WormSegmentation
{
	public static Vector2d[] estimateHistogram( final ModelImage image, float stepSize )
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
		int numSteps = (int) (Math.ceil(0.02f/stepSize)) + 1;
		Vector2d[] countValues = new Vector2d[numSteps];
		int step = 0;

		int totalCount = dimX * dimY * dimZ;
		int count_98P = (int) (0.98 * totalCount);
		int count_stepSize = (int)(stepSize*totalCount);
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
		for ( int i = 0; i < counts.length; i++ )
		{
			counts[i] = map.get( keyArray[i] );
			runningCount += counts[i];

			while ( runningCount >= count_98P )
			{
				countValues[step++] = new Vector2d( (float)count_98P/(float)totalCount, keyArray[i] );
				count_98P += count_stepSize;
			}
		}
		//    	System.err.println( count_95P + " " + totalCount );
		//    	for ( int i = 0; i < countValues.length; i++ )
		//    	{
		//    		System.err.println( countValues[i].X + " " + countValues[i].Y );
		//    	}
		keyArray = null;
		return countValues;
	}

	// need edges
	public static int fill(final ModelImage image, float cutOff, final ModelImage segmentation, final ModelImage colorSegmentation, 
			final Vector<Vector3f> seedList, ModelImage visited, final int id, final Vector<Vector4f> edgeList) {
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
			segmentation.set(x,  y, z, id);//image.getFloat(x,y,z));

			final Color color = new Color(Color.HSBtoRGB(id/25f, 1, 1));
			if ( colorSegmentation != null )
			{
				colorSegmentation.setC(x, y, z, 0, 1);
				colorSegmentation.setC(x, y, z, 1, color.getRed() / 255f);
				colorSegmentation.setC(x, y, z, 2, color.getGreen() / 255f);
				colorSegmentation.setC(x, y, z, 3, color.getBlue() / 255f);
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
							if ( visited.getInt(index) == 0 )
							{
								float value = image.getFloat(x1, y1, z1);
								if ( value > cutOff )
								{
									seedList.add( new Vector3f(x1,y1,z1) );
								}
								else if ( edgeList != null )
								{
									edgeList.add( new Vector4f(x1,y,z1, id) );
								}
							}
						}
					}
				}
			}							
		}
		return count;
	}


	public static ModelImage laplace(final ModelImage image, final int sigma )
	{
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_laplace";

		final float[] sigmas = new float[] {sigma, sigma, sigma * getCorrectionFactor(image)};


		OpenCLAlgorithmLaplacian laplacianAlgo;
		final ModelImage resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, resultImage);
		laplacianAlgo = new OpenCLAlgorithmLaplacian(resultImage, image, sigmas, true, true, false, 1.0f);

		laplacianAlgo.setRunningInSeparateThread(false);
		laplacianAlgo.run();
		return laplacianAlgo.getDestImage();
	}


	public static ModelImage LoG(final ModelImage image) {
		ModelImage blur = blur(image, 3);
		blur.calcMinMax();

		ModelImage laplace = laplace(blur, 3);
		
		blur.disposeLocal();
		blur = null;

		return laplace;
	}

	public static Vector<Vector3f> seamCellSegmentation( ModelImage image )
	{

		ModelImage loG = LoG(image);
		loG.calcMinMax();
		Vector2d[] histogram = estimateHistogram(loG, 0.0005f );
		float cutoff = -1;
		int cutoffIndex = -1;

		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

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
		ModelImage visited = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, visited); 
		ModelImage visited2 = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, visited2); 


		int index = Math.max(0,  histogram.length - 15);
		float min = (float)histogram[index].Y;

		int[] cutoffindex = new int[]{histogram.length - 1};

		Vector<Vector3f> annotations = segmentFirst10SeamCells( loG, histogram, cutoffindex, 0, 14, visited, null, null );
		//		System.err.println( imageName + " " + annotations.getCurves().size() );
		visited.calcMinMax();
		int[] midLineCutoffindex = new int[]{cutoffindex[0]};
		segmentMidLine( loG, histogram, midLineCutoffindex, null, visited );
		Vector<Vector3f> newAnnotations = segmentFirst10SeamCells( loG, histogram, cutoffindex, annotations.size(), 25 - annotations.size(), visited2, null, visited );
		if ( newAnnotations != null )
		{
			annotations.addAll( newAnnotations );
		}

		//		System.err.println( imageName + " " + annotations.size() );
		//		image.unregisterAllVOIs();
		//		image.registerVOI(annotations);
		//		imageName = image.getImageName();
		//		if (imageName.contains("_clone")) {
		//			imageName = imageName.replaceAll("_clone", "");
		//		}
		//		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "seam_cells" + File.separator;
		//		saveAllVOIsTo(voiDir, image);
		//		image.unregisterAllVOIs();

		//		return annotations;
		//		ModelImage seg = segmentImage(loG, visited); 
		//		seg.registerVOI(annotations);
		if ( loG != null )
		{
			loG.disposeLocal();
			loG = null;
		}
		if ( visited != null )
		{
			visited.disposeLocal();
			visited = null;
		}
		if ( visited2 != null )
		{
			visited2.disposeLocal();
			visited2 = null;
		}
		//		visited.calcMinMax();
		//		visited.registerVOI(annotations);
		//		return visited;

		return annotations;
	}

	public static Vector<Vector3f> segmentFirst10SeamCells( final ModelImage loG, Vector2d[] histogram, int[] cutoffIndex,
			int currentAnnotations, int annotationLimit, ModelImage visited, 
			ModelImage colorSegmentation, ModelImage segmentation )
			{
		float cutoff = (float)histogram[cutoffIndex[0]].Y;
		float edgeCutOff = cutoff;
		if ( cutoffIndex[0] > 0 )
		{
			edgeCutOff = (float)histogram[cutoffIndex[0]-1].Y;
		}
		//    	VOI annotations = new VOI( (short)1, "SeamCells", VOI.ANNOTATION, 0 );
		Vector<Vector3f> annotations = new Vector<Vector3f> ();
		segmentImage(loG, cutoff, visited, edgeCutOff, annotations, colorSegmentation, segmentation);
		//		System.err.println( annotations.size() + " " + cutoffIndex[0] + " " + cutoff );
		int totalAnnotations = annotations.size() + currentAnnotations;
		while ( (cutoffIndex[0] > 1) && (totalAnnotations < annotationLimit) )
		{
			cutoffIndex[0]--;
			cutoff = edgeCutOff;
			if ( cutoffIndex[0] > 0 )
			{
				edgeCutOff = (float)histogram[cutoffIndex[0]-1].Y;
			}
			annotations.clear();
			visited.setAll(0);
			if ( colorSegmentation != null )
			{
				colorSegmentation.setAll(0);
			}
			segmentImage(loG, cutoff, visited, edgeCutOff, annotations, colorSegmentation, segmentation);
			totalAnnotations = annotations.size() + currentAnnotations;
			//			System.err.println( annotations.size() + " " + cutoffIndex[0] + " " + cutoff );
		}

		return annotations;
			}








	public static VOI segmentImage( final ModelImage image, float cutOff, ModelImage visited, float edgeCutOff, Vector<Vector3f> annotations, ModelImage colorSegmentation, ModelImage seamCellSegmentation )
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

		Vector<Vector4f> edgeList = new Vector<Vector4f>();
		Vector<Vector3f> seedList = new Vector<Vector3f>();
		int count = 1;
		Vector3f seed = new Vector3f();
		boolean done = false;
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
						//    					System.err.println( image.getFloat(x,y,z) + " " + cutOff );
						int numFilled = fill( image, edgeCutOff, segmentationImage, colorSegmentation, seedList, visited, count, edgeList);
						if ( numFilled > 0 )
						{
							//    						System.err.println( numFilled );
							count++;
							//    						done = true;
							//    						break;
						}
					}
				}
				if ( done )
				{
					break;
				}
			}
			if ( done )
			{
				break;
			}
		}
		if ( seamCellSegmentation != null )
		{
			Vector<Integer> removeIDs = new Vector<Integer>();
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						if ( seamCellSegmentation.getInt(x, y, z) != 0 )
						{
							int id = segmentationImage.getInt(x, y, z);
							if ( !removeIDs.contains(id) )
							{
								removeIDs.add(id);
							}
						}
					}
				}
			}
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						int id = segmentationImage.getInt(x, y, z);
						if ( removeIDs.contains(id) )
						{
							segmentationImage.set(x, y, z, 0);
							if ( colorSegmentation != null )
							{
								colorSegmentation.setC(x, y, z, 0, 0);
								colorSegmentation.setC(x, y, z, 1, 0);
								colorSegmentation.setC(x, y, z, 2, 0);
								colorSegmentation.setC(x, y, z, 3, 0);
							}
							int index = z*dimX*dimY + y*dimX + x;
							visited.set(index, 0);
						}
					}
				}
			}
		}


		segmentationImage.calcMinMax();

		count = (int) segmentationImage.getMax();
		Vector3f[] seamCells = new Vector3f[count];
		Vector3f[] min = new Vector3f[count];
		Vector3f[] max = new Vector3f[count];
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
							min[id] = new Vector3f( Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE);
							max[id] = new Vector3f(-Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE);
							counts[id] = 0;
						}
						seamCells[id].add(x,y,z);
						counts[id]++;
						min[id].X = Math.min(min[id].X, x);
						min[id].Y = Math.min(min[id].Y, y);
						min[id].Z = Math.min(min[id].Z, z);

						max[id].X = Math.max(max[id].X, x);
						max[id].Y = Math.max(max[id].Y, y);
						max[id].Z = Math.max(max[id].Z, z);
					}
				}
			}
		}

		Vector<Vector3f> newEdgeList = new Vector<Vector3f>();
		//    	for ( int j = 0; j < edgeList.size(); j++ )
		//    	{
		//    		Vector4f edgePt = edgeList.elementAt(j);
		//    		newEdgeList.add( new Vector3f(edgePt.X, edgePt.Y, edgePt.Z) );
		//    	}
		for ( int i = 0; i < seamCells.length; i++ )
		{
			if ( seamCells[i] != null )
			{
				seamCells[i].scale( 1f/counts[i]);    			
			}
		}
		//    	for ( int i = 0; i < seamCells.length; i++ )
		//    	{
		//    		if ( seamCells[i] != null )
		//    		{
		//    			for ( int j = i+1; j < seamCells.length; j++ )
		//    			{
		//    				float distance = seamCells[i].distance(seamCells[j]);
		//
		//    				if ( distance < 10 )
		//    				{
		//    					seamCells[i].add( seamCells[j] );
		//    					seamCells[i].scale(0.5f);
		//    					seamCells[j] = null;
		//    				}
		//    			}
		//    		}
		//    	}
		//    	for ( int i = 0; i < annotations.getCurves().size(); i++ )
		//    	{
		//    		Vector3f pos0 = annotations.getCurves().elementAt(i).elementAt(0);
		//    		Vector3f pos1 = annotations.getCurves().elementAt(i).elementAt(1);
		//    		for ( int j = 0; j < seamCells.length; j++ )
		//    		{
		//    			if ( seamCells[j] != null )
		//    			{
		//    				float distance = pos0.distance(seamCells[j]);
		//    				if ( distance < 10 )
		//    				{
		//    					pos0.add(seamCells[j]);
		//    					pos0.scale(0.5f);
		//    					pos1.copy(pos0);
		//    					seamCells[j] = null;
		//    				}
		//    			}
		//    		}
		//    	}

		for ( int i = 0; i < seamCells.length; i++ )
		{
			if ( seamCells[i] != null )
			{
				//    			BoxBV bv = annotationBounds.elementAt(i);
				//    			Vector3f minBB = new Vector3f();
				//    			Vector3f maxBB = new Vector3f();
				//    			bv.GetBox().ComputeBounds(minBB, maxBB);
				//    			System.err.println( minBB + "    " + maxBB);
				//    			System.err.println( min[i] + "    " + max[i]);
				float maxSpan = Math.max( (max[i].X - min[i].X), Math.max( (max[i].Y - min[i].Y), (max[i].Z - min[i].Z) ) );
				float minSpan = Math.min( (max[i].X - min[i].X), Math.min( (max[i].Y - min[i].Y), (max[i].Z - min[i].Z) ) );
				float ratio = maxSpan/minSpan;
				if ( ratio >= 3 )
				{
					//    			System.err.println( counts[i] + " " + ratio + " " + (max[i].X - min[i].X) + " " + (max[i].Y - min[i].Y) + " " + (max[i].Z - min[i].Z));

					// remove annotation data:
					seamCells[i] = null;
					counts[i] = 0;
					for ( int z = 0; z < dimZ; z++ )
					{
						for ( int y = 0; y < dimY; y++ )
						{
							for ( int x = 0; x < dimX; x++ )
							{
								int id = segmentationImage.getInt(x, y, z);
								if ( id == (i+1) )
								{
									segmentationImage.set(x, y, z, 0);
									if ( colorSegmentation != null )
									{
										colorSegmentation.setC(x, y, z, 0, 0);
										colorSegmentation.setC(x, y, z, 1, 0);
										colorSegmentation.setC(x, y, z, 2, 0);
										colorSegmentation.setC(x, y, z, 3, 0);
									}
									int index = z*dimX*dimY + y*dimX + x;
									//    								visited.clear(index);
									visited.set(index, 0);
								}
							}
						}
					}
					for ( int j = 0; j < edgeList.size(); j++ )
					{
						Vector4f edgePt = edgeList.elementAt(j);
						if ( edgePt.W != (i+1) )
						{
							newEdgeList.add( new Vector3f(edgePt.X, edgePt.Y, edgePt.Z) );
						}
					}
				}
			}
		}

		int annotationCount = annotations.size();
		for ( int i = 0; i < seamCells.length; i++ )
		{
			if ( seamCells[i] != null )
			{
				//    			VOIText text = new VOIText();
				//    			text.setText( "A" + annotationCount++ );
				//    			text.setColor( new Color(255, 255, 255) );
				//    			text.add( seamCells[i] );
				//    			text.add( seamCells[i] );
				//    			text.setUseMarker(false);
				//    			annotations.getCurves().add(text);

				annotations.add(seamCells[i]);

				float maxSpan = Math.max( (max[i].X - min[i].X), Math.max( (max[i].Y - min[i].Y), (max[i].Z - min[i].Z) ) );
				float minSpan = Math.min( (max[i].X - min[i].X), Math.min( (max[i].Y - min[i].Y), (max[i].Z - min[i].Z) ) );
				float ratio = maxSpan/minSpan;
				//    			System.err.println( text.getText() + " " + counts[i] + " " + maxSpan + " " + minSpan + " " + ratio);
				//    			System.err.println( text.getText() + " " + counts[i] + " " + (max[i].X - min[i].X) + " " + (max[i].Y - min[i].Y) + " " + (max[i].Z - min[i].Z) + " " + ratio);
			}
		}

		//		fill( image, edgeCutOff/2f, segmentationImage, null, newEdgeList, visited, -1, null);
		//		fill( image, edgeCutOff/2f, segmentationImage, colorSegmentation, newEdgeList, visited, -1, null);


		//    	colorSegmentation.registerVOI(annotation);
		////		System.err.println( count + " " + segmentationImage.getMin() + " " + segmentationImage.getMax() );
		//		new ViewJFrameImage(colorSegmentation);

		//    	new ViewJFrameImage( (ModelImage)segmentationImage.clone());
		segmentationImage.disposeLocal();
		segmentationImage = null;

		return null;

		//    	segmentationImage.registerVOI(annotation);
		//		return segmentationImage;
	}

	public static void segmentMidLine( final ModelImage image, Vector2d[] histogram, 
			int[] cutOffIndex,
			ModelImage colorSegmentation, ModelImage seamCellSegmentation )
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

		imageName = imageName + "_visited";
		ModelImage visited = new ModelImage(ModelStorageBase.INTEGER, image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, visited);   

		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;  	

		Vector<Vector4f> edgeList = new Vector<Vector4f>();
		Vector<Vector3f> seedList = new Vector<Vector3f>();
		Vector3f seed = new Vector3f();

		float range = (float) (image.getMax() - image.getMin());
		float stepSize = Math.min(10, range/1000);
		int totalCount = 0;
		float cutOff = (float) histogram[cutOffIndex[0]].Y;
		int segTarget = 25000;
		while ( totalCount < segTarget )
		{
			int count = 1;
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
							if ( fill( image, cutOff, segmentationImage, colorSegmentation, seedList, visited, count, null) > 0 )
							{
								count++;
							}
						}
					}
				}
			}

			Vector<Integer> removeIDs = new Vector<Integer>();
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						if ( seamCellSegmentation.getInt(x, y, z) != 0 )
						{
							int id = segmentationImage.getInt(x, y, z);
							if ( !removeIDs.contains(id) )
							{
								removeIDs.add(id);
							}
						}
					}
				}
			}
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						int id = segmentationImage.getInt(x, y, z);
						if ( removeIDs.contains(id) )
						{
							segmentationImage.set(x, y, z, 0);
							if ( colorSegmentation != null )
							{
								colorSegmentation.setC(x, y, z, 0, 0);
								colorSegmentation.setC(x, y, z, 1, 0);
								colorSegmentation.setC(x, y, z, 2, 0);
								colorSegmentation.setC(x, y, z, 3, 0);
							}
							int index = z*dimX*dimY + y*dimX + x;
							visited.set(index, 0);										
						}
					}
				}
			}


			segmentationImage.calcMinMax();

			count = (int) segmentationImage.getMax();
			Vector3f[] seamCells = new Vector3f[count];
			Vector3f[] min = new Vector3f[count];
			Vector3f[] max = new Vector3f[count];
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
								min[id] = new Vector3f( Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE);
								max[id] = new Vector3f(-Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE);
								counts[id] = 0;
							}
							seamCells[id].add(x,y,z);
							counts[id]++;
							min[id].X = Math.min(min[id].X, x);
							min[id].Y = Math.min(min[id].Y, y);
							min[id].Z = Math.min(min[id].Z, z);

							max[id].X = Math.max(max[id].X, x);
							max[id].Y = Math.max(max[id].Y, y);
							max[id].Z = Math.max(max[id].Z, z);
						}
					}
				}
			}

			for ( int i = 0; i < seamCells.length; i++ )
			{
				if ( seamCells[i] != null )
				{
					seamCells[i].scale( 1f/counts[i]);    			
				}
			}

			for ( int i = 0; i < seamCells.length; i++ )
			{
				if ( seamCells[i] != null )
				{
					float maxSpan = Math.max( (max[i].X - min[i].X), Math.max( (max[i].Y - min[i].Y), (max[i].Z - min[i].Z) ) );
					float minSpan = Math.min( (max[i].X - min[i].X), Math.min( (max[i].Y - min[i].Y), (max[i].Z - min[i].Z) ) );
					float ratio = maxSpan/minSpan;
					if ( maxSpan < 60 )
					{
						// remove annotation data:
						seamCells[i] = null;
						counts[i] = 0;
						for ( int z = 0; z < dimZ; z++ )
						{
							for ( int y = 0; y < dimY; y++ )
							{
								for ( int x = 0; x < dimX; x++ )
								{
									int id = segmentationImage.getInt(x, y, z);
									if ( id == (i+1) )
									{
										segmentationImage.set(x, y, z, 0);
										if ( colorSegmentation != null )
										{
											colorSegmentation.setC(x, y, z, 0, 0);
											colorSegmentation.setC(x, y, z, 1, 0);
											colorSegmentation.setC(x, y, z, 2, 0);
											colorSegmentation.setC(x, y, z, 3, 0);
										}
										int index = z*dimX*dimY + y*dimX + x;
										visited.set(index, 0);
									}
								}
							}
						}
					}
				}
			}

			totalCount = 0;
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						if ( visited.getInt(x, y, z) > 0 )
						{
							totalCount++;
						}
					}
				}
			}
			//				System.err.println( count + " " + totalCount + " " + cutOff);
			if ( totalCount < segTarget )
			{
				cutOffIndex[0]--;
				if ( cutOffIndex[0] < 0 )
				{
					break;
				}
				cutOff = (float) histogram[cutOffIndex[0]].Y;	
				segmentationImage.setAll(0);
				visited.setAll(0);
				if ( colorSegmentation != null )
				{
					colorSegmentation.setAll(0);
				}
			}
		}

		int nextID = (int) (seamCellSegmentation.getMax() + 1);
		if ( totalCount > 0 )
		{
			for ( int z = 0; z < dimZ; z++ )
			{
				for ( int y = 0; y < dimY; y++ )
				{
					for ( int x = 0; x < dimX; x++ )
					{
						int id = segmentationImage.getInt(x, y, z);
						if ( id != 0 )
						{
							seamCellSegmentation.set(x, y, z, nextID);
						}
					}
				}
			}			
		}
		segmentationImage.disposeLocal();
		segmentationImage = null;

		visited.disposeLocal();
		visited = null;

	}


	public WormSegmentationLoG() {}


}
