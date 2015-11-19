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
		float maxValue = (float) (1.0 * image.getMax());
		float minValue = (float) (0.5 * image.getMax());
		
		Vector<Vector3f> seam1 = segmentAll1(image, minValue, maxValue, 20);
		maxValue = minValue;
		minValue = (float) (0.4 * image.getMax());
		Vector<Vector3f> seam2 = segmentAll1(image, minValue, maxValue, 20);
		maxValue = minValue;
		minValue = (float) (0.3 * image.getMax());
		Vector<Vector3f> seam3 = segmentAll1(image, minValue, maxValue, 20);
		maxValue = minValue;
		minValue = (float) (0.2 * image.getMax());
		Vector<Vector3f> seam4 = segmentAll1(image, minValue, maxValue, 20);
		maxValue = minValue;
		minValue = (float) (0.15 * image.getMax());
		Vector<Vector3f> seam5 = segmentAll1(image, minValue, maxValue, 20);

		Vector<Vector3f> seamCells = new Vector<Vector3f>();
		Vector<Vector3f> tempSeamCells = new Vector<Vector3f>();
		if ( seam1 != null )		tempSeamCells.addAll(seam1);
		if ( seam2 != null )		tempSeamCells.addAll(seam2);
		if ( seam3 != null )		tempSeamCells.addAll(seam3);
		if ( seam4 != null )		tempSeamCells.addAll(seam4);
		if ( seam5 != null )		tempSeamCells.addAll(seam5);

		Vector3f negCenter = new Vector3f(-1,-1,-1);
		WormSegmentation.reduceDuplicates(image, tempSeamCells, 5, 15, true);
		
		for ( int i = 0; i < tempSeamCells.size(); i++ )
		{
			if ( !tempSeamCells.elementAt(i).equals(negCenter) )
			{
				seamCells.add(tempSeamCells.elementAt(i));
			}
		}
		return seamCells;
	}

	/**
	 * Segmentation algorithm based on K-means clustering.
	 * 
	 * @param image input image to segment
	 * @param minValue minimum value to include in the data.
	 * @param maxValue maximum value to include in the data.
	 * @param numClusters number of clusters.
	 * @return the segmented clusters.
	 */
	public static Vector<Vector3f> segmentAll1( final ModelImage image, 
			final float minValue, final float maxValue, final int numClusters) {

		// Get position data from the image for all values within range:
		final Vector<Vector3f> positions = new Vector<Vector3f>();
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					if ( (image.getFloat(x, y, z) > minValue) && (image.getFloat(x, y, z) <= maxValue)) {
						positions.add(new Vector3f(x, y, z));
					}
				}
			}
		}
		if ( positions.size() == 0 )
		{
			// nothing within range:
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
			// ensure all the potential clusters are separated by a minimum distance:
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
					// calculate cost -- each cluster center must be at a high-intensity voxel:
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
		}

		// Combine clusters that are too close together:
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
		for (int i = 0; i < potentialClusters.length; i++) {
			if ( !potentialClusters[i].equals(negCenter) )
			{
				seamcells.add(potentialClusters[i]);
			}
		}
		return seamcells;
	}
}
