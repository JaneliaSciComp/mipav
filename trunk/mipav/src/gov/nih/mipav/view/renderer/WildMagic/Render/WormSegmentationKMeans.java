package gov.nih.mipav.view.renderer.WildMagic.Render;

import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOIContour;

import java.util.Random;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public class WormSegmentationKMeans extends WormSegmentation
{
	public WormSegmentationKMeans() {}
	
	public static Vector<Vector3f> seamCellSegmentation( ModelImage image )
	{
		ModelImage temp = (ModelImage)image.clone();
		temp.calcMinMax();
		float maxValue = (float) (1.0 * image.getMax());
		float minValue = (float) (0.5 * image.getMax());
		
		Vector<Vector3f> seam1 = segmentAll1(image, temp, minValue, maxValue, 20, 1);
		maxValue = minValue;
		minValue = (float) (0.4 * image.getMax());
		Vector<Vector3f> seam2 = segmentAll1(image, temp, minValue, maxValue, 20, .5f);
		maxValue = minValue;
		minValue = (float) (0.3 * image.getMax());
		Vector<Vector3f> seam3 = segmentAll1(image, temp, minValue, maxValue, 20, .25f);
		maxValue = minValue;
		minValue = (float) (0.2 * image.getMax());
		Vector<Vector3f> seam4 = segmentAll1(image, temp, minValue, maxValue, 20, .75f);
		maxValue = minValue;
		minValue = (float) (0.15 * image.getMax());
		Vector<Vector3f> seam5 = segmentAll1(image, temp, minValue, maxValue, 20, .9f);

		Vector<Vector3f> seamCells = new Vector<Vector3f>();
		Vector<Vector3f> tempSeamCells = new Vector<Vector3f>();
		if ( seam1 != null )		tempSeamCells.addAll(seam1);
		if ( seam2 != null )		tempSeamCells.addAll(seam2);
		if ( seam3 != null )		tempSeamCells.addAll(seam3);
		if ( seam4 != null )		tempSeamCells.addAll(seam4);
		if ( seam5 != null )		tempSeamCells.addAll(seam5);

		Vector3f negCenter = new Vector3f(-1,-1,-1);
//		for ( int i = 0; i < tempSeamCells.size(); i++ )
//		{
//			if ( !tempSeamCells.elementAt(i).equals(negCenter) )
//			{
//				Vector3f newCenter = new Vector3f(tempSeamCells.elementAt(i));
//				int count = 1;
//				for ( int j = i+1; j < tempSeamCells.size(); j++ )
//				{
//					if ( !tempSeamCells.elementAt(j).equals(negCenter) )
//					{
//						if ( tempSeamCells.elementAt(i).distance(tempSeamCells.elementAt(j)) < 4 )
//						{
//							newCenter.add(tempSeamCells.elementAt(j));
//							tempSeamCells.elementAt(j).copy(negCenter);
//							count++;
//						}
//					}
//				}
//				if ( count > 1 )
//				{
//					newCenter.scale(1f/(float)count);
//					tempSeamCells.elementAt(i).copy(newCenter);
//				}
//				else
//				{
//					tempSeamCells.elementAt(i).copy(negCenter);
//				}
//			}
//		}
		WormSegmentation.reduceDuplicates(image, tempSeamCells, 5, 15, true);
		
		for ( int i = 0; i < tempSeamCells.size(); i++ )
		{
			// System.err.println( i + "     " + potentialClusters[i] );
			if ( !tempSeamCells.elementAt(i).equals(negCenter) )
			{
				seamCells.add(tempSeamCells.elementAt(i));
			}
		}
		temp.disposeLocal();
		temp = null;
		return seamCells;
	}

	/**
	 * IN PROGRESS new segmentation algorithm based on K-means clustering.
	 * 
	 * @param image
	 * @param minValue
	 * @param maxValue
	 * @param numClusters
	 * @param color
	 * @return
	 */
	public static Vector<Vector3f> segmentAll1(final ModelImage original, final ModelImage image, final float minValue, final float maxValue, final int numClusters, final float color) {
//		final ModelImage result = new ModelImage( image.getType(), image.getExtents(), "temp" );//(ModelImage) image.clone();
//		JDialogBase.updateFileInfo(image, result); 

//		System.err.println("segmentAll " + minValue + " " + maxValue);

		final Vector<Vector3f> positions = new Vector<Vector3f>();
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					// result.set(x,y,z,0);
					if ( (image.getFloat(x, y, z) > minValue) && (image.getFloat(x, y, z) <= maxValue)) {
						positions.add(new Vector3f(x, y, z));
						// result.set(x,y,z, image.getFloat(x,y,z));
					}
				}
			}
		}
//		System.err.println("segmentAll " + positions.size());
		if ( positions.size() == 0 )
		{
			return null;
		}

		final int numAttempts = 1000;
		final Random randomGen = new Random();
		double minCost = Float.MAX_VALUE;
		Vector3f[] potentialClusters = null;
		for (int i = 0; i < numAttempts; i++) {
			// generate random potential cluster centers:
			final Vector3f[] centers = new Vector3f[numClusters];
			for (int j = 0; j < numClusters; j++) {
				final int index = (int) (randomGen.nextFloat() * (positions.size()));
				centers[j] = new Vector3f(positions.elementAt(index));
			}
			boolean allSeparate = false;
			int count = 0;
			int limit = 30;
			while ( !allSeparate )
			{
				float minDist = Float.MAX_VALUE;
				int minIndex = -1;
				for (int j = 0; j < numClusters; j++) 
				{
					for ( int k = j+1; k < numClusters; k++ )
					{
						float dist = centers[j].distance(centers[k]);
						if ( dist < minDist )
						{
							minDist = dist;
							minIndex = j;
						}
					}
				}
				if ( (minDist < limit) && (minIndex != -1) )
				{
					final int index = (int) (randomGen.nextFloat() * (positions.size()));
					centers[minIndex] = new Vector3f(positions.elementAt(index));
//					System.err.println( minDist + " " + minIndex );
				}		
				else
				{
					allSeparate = true;
				}
				count++;
				if ( count >= 1000 )
				{
					limit -= 2;
					count = 0;
				}
			}
//			System.err.println("Done centers");
//			final Vector<Integer> indexList = new Vector<Integer>();
//			while (indexList.size() < numClusters) {
//				final int index = (int) (randomGen.nextFloat() * (positions.size()));
//				if ( !indexList.contains(index)) {
//					indexList.add(index);
//				}
//			}
//			final Vector3f[] centers = new Vector3f[numClusters];
//			for (int j = 0; j < numClusters; j++) {
//				centers[j] = new Vector3f(positions.elementAt(indexList.elementAt(j)));
//			}

			boolean done = false;
			while ( !done) {
				VOIContour[] groups = new VOIContour[numClusters];
				// for each position find closest center and put in that group:
				for (int j = 0; j < positions.size(); j++) {
					int minGroupIndex = -1;
					float minGroupDist = Float.MAX_VALUE;
					for (int k = 0; k < numClusters; k++) {
						final float distance = positions.elementAt(j).distance(centers[k]);
						if (distance < minGroupDist) {
							minGroupDist = distance;
							minGroupIndex = k;
						}
					}
					if (groups[minGroupIndex] == null) {
						groups[minGroupIndex] = new VOIContour(false);
					}
					groups[minGroupIndex].add(positions.elementAt(j));
				}

				// calculate new center positions based on the average of group:
				Vector3f[] listCenters = new Vector3f[numClusters];
				for (int j = 0; j < groups.length; j++) {
					listCenters[j] = new Vector3f();
					if (groups[j] != null) {
						for (int k = 0; k < groups[j].size(); k++) {
							listCenters[j].add(groups[j].elementAt(k));
						}
						listCenters[j].scale(1f / groups[j].size());
					}
				}
				float maxMoved = -Float.MAX_VALUE;
				// check distance moved, if less than threshold, done:
				for (int j = 0; j < numClusters; j++) {
					final float dist = centers[j].distance(listCenters[j]);
					if (dist > maxMoved) {
						maxMoved = dist;
					}
					centers[j].copy(listCenters[j]);
				}
				if (maxMoved < 2) {
					// done:
					done = true;
					// calculate cost:
//					double cost = 0;
//					for (int j = 0; j < groups.length; j++) {
//						if (groups[j] != null) {
//							for (int k = 0; k < groups[j].size(); k++) {
//								cost += centers[j].distance(groups[j].elementAt(k));
//							}
//						}
//					}
//					cost /= positions.size();
					double cost = 0;
					for (int j = 0; j < centers.length; j++) {
						cost += 10*(float) (image.getMax() - image.getFloat((int)centers[j].X, (int)centers[j].Y, (int)centers[j].Z ));
					}
					
					// check for min cost and save the clusters w/smallest cost so far:
					if (cost < minCost) {
						minCost = cost;
						if (potentialClusters != null) {
							potentialClusters = null;
						}
						potentialClusters = new Vector3f[numClusters];
						for (int j = 0; j < centers.length; j++) {
							potentialClusters[j] = new Vector3f(centers[j]);
						}
					}
				} else {
					for (int j = 0; j < numClusters; j++) {
						if (groups[j] != null) {
							groups[j].clear();
						}
					}
					groups = null;
					listCenters = null;
				}
			}
//			System.err.println( i );
		}

		Vector3f negCenter = new Vector3f(-1,-1,-1);
		for ( int i = 0; i < potentialClusters.length; i++ )
		{
			if ( !potentialClusters[i].equals(negCenter) )
			{
				Vector3f newCenter = new Vector3f(potentialClusters[i]);
				int count = 1;
				for ( int j = i+1; j < potentialClusters.length; j++ )
				{
					if ( !potentialClusters[j].equals(negCenter) )
					{
						if ( potentialClusters[i].distance(potentialClusters[j]) < 10 )
						{
							newCenter.add(potentialClusters[j]);
							potentialClusters[j].copy(negCenter);
							count++;
						}
					}
				}
				if ( count > 1 )
				{
					newCenter.scale(1f/count);
					potentialClusters[i].copy(newCenter);
				}
			}
		}
		
		Vector<Vector3f> seamcells = new Vector<Vector3f>();
//		final short sID = (short) (image.getVOIs().getUniqueID());
//		final VOI clusters = new VOI(sID, "clusters", VOI.POINT, color);
		for (int i = 0; i < potentialClusters.length; i++) {
			// System.err.println( i + "     " + potentialClusters[i] );
			if ( !potentialClusters[i].equals(negCenter) )
			{
//				clusters.importPoint(potentialClusters[i]);
				seamcells.add(potentialClusters[i]);
			}
		}

//		for (int z = 0; z < dimZ; z++) {
//			for (int y = 0; y < dimY; y++) {
//				for (int x = 0; x < dimX; x++) {
//					result.set(x, y, z, original.getMin());
//				}
//			}
//		}
//
//		for (int i = 0; i < clusters.getCurves().size(); i++)
//		{
//			Vector3f pos = clusters.getCurves().elementAt(i).elementAt(0);
//			for (int z = (int) Math.max(0, pos.Z - 25); z < (int) Math.min(dimZ, pos.Z + 25); z++)
//			{
//				for (int y = (int) Math.max(0, pos.Y - 25); y < (int) Math.min(dimY, pos.Y + 25); y++)
//				{
//					for (int x = (int) Math.max(0, pos.X - 25); x < (int) Math.min(dimX, pos.X + 25); x++)
//					{
//						result.set(x, y, z, original.get(x, y, z));
//					}
//				}
//			}
//		}
//		result.registerVOI(clusters);
		
		
//		original.registerVOI(clusters);
//		System.err.println(minCost + " " + seamcells.size());

//		// result.restoreVOIs( image.getVOIsCopy() );
//		result.calcMinMax();
//		// new ViewJFrameImage(result);
//		return result;
		return seamcells;
	}
}
