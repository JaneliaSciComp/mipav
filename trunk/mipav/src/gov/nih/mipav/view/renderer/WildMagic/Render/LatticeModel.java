package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import java.awt.Color;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.BitSet;
import java.util.Random;
import java.util.Vector;

import javax.swing.JFileChooser;

import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Distance.DistanceSegment3Segment3;
import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.*;


/**
 * Supports the worm-straightening algorithms that use a 3D lattice as the basis of the straightening process.
 */
public class LatticeModel {

	/**
	 * Saves all VOIs to the specified file.
	 * 
	 * @param voiDir
	 * @param image
	 */
	private static void saveAllVOIsTo(final String voiDir, final ModelImage image) {
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
	public static ModelImage segmentAll1(final ModelImage image, final float minValue, final float maxValue, final int numClusters, final float color) {
		final ModelImage result = (ModelImage) image.clone();

		System.err.println("segmentAll " + minValue + " " + maxValue);

		final Vector<Vector3f> positions = new Vector<Vector3f>();
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					// result.set(x,y,z,0);
					if ( (image.getFloat(x, y, z) > minValue) && (image.getFloat(x, y, z) < maxValue)) {
						positions.add(new Vector3f(x, y, z));
						// result.set(x,y,z, image.getFloat(x,y,z));
					}
				}
			}
		}
		System.err.println("segmentAll " + positions.size());

		final int numAttempts = 1000;
		final Random randomGen = new Random();
		double minCost = Float.MAX_VALUE;
		Vector3f[] potentialClusters = null;
		for (int i = 0; i < numAttempts; i++) {
			// generate random potential cluster centers:
			final Vector<Integer> indexList = new Vector<Integer>();
			while (indexList.size() < numClusters) {
				final int index = (int) (randomGen.nextFloat() * (positions.size()));
				if ( !indexList.contains(index)) {
					indexList.add(index);
				}
			}
			final Vector3f[] centers = new Vector3f[numClusters];
			for (int j = 0; j < numClusters; j++) {
				centers[j] = new Vector3f(positions.elementAt(indexList.elementAt(j)));
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
					// calculate cost:
					double cost = 0;
					for (int j = 0; j < groups.length; j++) {
						if (groups[j] != null) {
							for (int k = 0; k < groups[j].size(); k++) {
								cost += centers[j].distance(groups[j].elementAt(k));
							}
						}
					}
					cost /= positions.size();
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

		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					for (int pos = 0; pos < potentialClusters.length; pos++) {
						if (potentialClusters[pos].distance(new Vector3f(x, y, z)) < 25) {
							result.set(x, y, z, image.getMin());
						}
					}
				}
			}
		}

		final short sID = (short) (image.getVOIs().getUniqueID());
		final VOI clusters = new VOI(sID, "clusters", VOI.POINT, color);
		for (int i = 0; i < potentialClusters.length; i++) {
			// System.err.println( i + "     " + potentialClusters[i] );
			clusters.importPoint(potentialClusters[i]);
		}
		image.registerVOI(clusters);
		System.err.println(minCost + " " + clusters.getCurves().size());

		// result.restoreVOIs( image.getVOIsCopy() );
		result.calcMinMax();
		// new ViewJFrameImage(result);
		return result;
	}

	/**
	 * 
	 * IN PROGRESS new segmentation algorithm based on K-means clustering. Segments the digestive tract.
	 * 
	 * @param image
	 * @param imageNoSeam
	 * @param minValue
	 * @param maxValue
	 * @param numClusters
	 * @param color
	 */
	public static void segmentAll2(final ModelImage image, final ModelImage imageNoSeam, final float minValue, final float maxValue, final int numClusters,
			final float color) {
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		System.err.println("segmentAll " + minValue + " " + maxValue);

		final Vector<Vector3f> positions = new Vector<Vector3f>();
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					if ( (imageNoSeam.getFloat(x, y, z) > minValue) && (imageNoSeam.getFloat(x, y, z) < maxValue)) {
						positions.add(new Vector3f(x, y, z));
					}
				}
			}
		}
		System.err.println("segmentAll " + positions.size());

		final int numAttempts = 1000;
		final Random randomGen = new Random();
		double minCost = Float.MAX_VALUE;
		Vector3f[] potentialClusters = null;
		for (int i = 0; i < numAttempts; i++) {
			// generate random potential cluster centers:
			final Vector<Integer> indexList = new Vector<Integer>();
			while (indexList.size() < numClusters) {
				final int index = (int) (randomGen.nextFloat() * (positions.size()));
				if ( !indexList.contains(index)) {
					indexList.add(index);
				}
			}
			final Vector3f[] centers = new Vector3f[numClusters];
			for (int j = 0; j < numClusters; j++) {
				centers[j] = new Vector3f(positions.elementAt(indexList.elementAt(j)));
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
					// calculate cost:
					double cost = 0;
					for (int j = 0; j < groups.length; j++) {
						if (groups[j] != null) {
							for (int k = 0; k < groups[j].size(); k++) {
								cost += centers[j].distance(groups[j].elementAt(k));
							}
						}
					}
					cost /= positions.size();
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

		ModelImage result = segmentAll2(imageNoSeam);
		final Vector<Vector3f> seeds = new Vector<Vector3f>();
		final Vector<Vector3f> newClusters = new Vector<Vector3f>();
		final Vector<Vector3f> midLineClusters = new Vector<Vector3f>();
		float maxSize = -Float.MAX_VALUE;
		int maxIndex = -1;
		for (int i = 0; i < potentialClusters.length; i++) {
			seeds.clear();
			seeds.add(potentialClusters[i]);
			final float size = fill(result, (float) (0.1 * result.getMax()), seeds);
			if (size > maxSize) {
				maxSize = size;
				maxIndex = i;
			}
			System.err.println("potential clusters " + i + " " + size);
			final BitSet mask = result.getMask();
			int count = 0;
			for (int j = 0; j < potentialClusters.length; j++) {
				final int index = (int) (potentialClusters[j].Z * dimX * dimY + potentialClusters[j].Y * dimX + potentialClusters[j].X);
				if (mask.get(index)) {
					count++;
				}
			}
			if ( (size > 0) && (size < 10000)) {
				newClusters.add(potentialClusters[i]);
			} else if (size > 0) {
				midLineClusters.add(potentialClusters[i]);
			}
			mask.clear();
		}

		System.err.println("newClusters " + newClusters.size());
		System.err.println("midLineClusters " + midLineClusters.size());

		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "segmentation" + File.separator;
		final File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}

		if (midLineClusters.size() > 0) {
			for (int i = 0; i < midLineClusters.size(); i++) {
				seeds.clear();
				seeds.add(potentialClusters[maxIndex]);
				final float size = fill(result, (float) (0.1 * result.getMax()), seeds);
				System.err.println("Midline clusters " + i + " " + size);
			}

			image.setMask((BitSet) result.getMask().clone());

			final ModelImage tmpImage = segmentAll2(image);

			final VOI seamCells = image.getVOIs().elementAt(0);
			final Vector<Vector3f> newMidLineClusters = new Vector<Vector3f>();
			final boolean[] removeSeam = new boolean[seamCells.getCurves().size()];
			for (int i = 0; i < seamCells.getCurves().size(); i++) {
				tmpImage.getMask().clear();
				seeds.clear();
				seeds.add(seamCells.getCurves().elementAt(i).elementAt(0));
				final float size = fill(tmpImage, (float) (0.1 * result.getMax()), seeds);
				System.err.println("Original clusters " + i + " " + size);
				if ( (size >= 10000)) {
					newMidLineClusters.add(seamCells.getCurves().elementAt(i).elementAt(0));
				}
				boolean midLine = false;
				for (int j = 0; j < midLineClusters.size(); j++) {
					final int index = (int) (midLineClusters.elementAt(j).Z * dimX * dimY + midLineClusters.elementAt(j).Y * dimX + midLineClusters
							.elementAt(j).X);
					if (tmpImage.getMask().get(index)) {
						System.err.println("found midLine");
						midLine = true;
						break;
					}
				}
				if (midLine) {
					midLineClusters.add(seamCells.getCurves().elementAt(i).elementAt(0));
					image.getMask().or(tmpImage.getMask());
					removeSeam[i] = true;
				}
			}
			for (int i = removeSeam.length - 1; i >= 0; i--) {
				if (removeSeam[i]) {
					seamCells.getCurves().remove(i);
				}
			}
			System.err.println("newMidLineClusters " + newMidLineClusters.size());

			for (int z = 0; z < dimZ; z++) {
				for (int y = 0; y < dimY; y++) {
					for (int x = 0; x < dimX; x++) {
						final int index = z * dimY * dimX + y * dimX + x;
						if (image.getMask().get(index)) {
							result.set(x, y, z, image.get(x, y, z));
						} else {
							result.set(x, y, z, image.getMin());
						}
					}
				}
			}

			voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "segmentation" + File.separator;
			result.setImageName(imageName + "_midLine.xml");
			ModelImage.saveImage(result, imageName + "_midLine.xml", voiDir, false);
			result.disposeLocal();
			result = null;
		}

		if (newClusters.size() > 0) {
			final short sID = (short) (image.getVOIs().getUniqueID());
			final VOI clusters = new VOI(sID, "clusters2", VOI.POINT, color);
			for (int i = 0; i < newClusters.size(); i++) {
				System.err.println("new cluster " + newClusters.elementAt(i));
				clusters.importPoint(newClusters.elementAt(i));
			}
			image.registerVOI(clusters);
		}
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "segmentation" + File.separator + "seam_cells"
				+ File.separator;
		saveAllVOIsTo(voiDir, image);
		// new ViewJFrameImage((ModelImage)image.clone());
	}

	/**
	 * IN PROGRESS segmentation based on difference of gaussians.
	 * 
	 * @param image
	 * @param annotations
	 * @param time
	 * @param statistics
	 * @return
	 */
	public static ModelImage segmentAnnotations(final ModelImage image, final VOI annotations, final int time, final BufferedWriter statistics) {
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		final ModelImage result = segmentAll2(image);

		final float cutoff = (float) (0.125 * result.getMax());
		final float[] minValue = new float[annotations.getCurves().size()];
		final Vector<Vector3f> positions = new Vector<Vector3f>();
		for (int i = 0; i < annotations.getCurves().size(); i++) {
			final VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
				continue;
			}
			final Vector3f pos = text.elementAt(0);
			final Vector3f seed = new Vector3f(pos);
			minValue[i] = result.getFloat((int) pos.X, (int) pos.Y, (int) pos.Z);

			if (minValue[i] < cutoff) {
				for (int z = (int) (pos.Z - 4); z <= pos.Z + 4; z++) {
					for (int y = (int) (pos.Y - 4); y <= pos.Y + 4; y++) {
						for (int x = (int) (pos.X - 4); x <= pos.X + 4; x++) {

							final float value = result.getFloat(x, y, z);
							if ( (value >= cutoff) && (value > minValue[i])) {
								minValue[i] = value;
								seed.set(x, y, z);
							} else if ( (minValue[i] < cutoff) && (value > minValue[i])) {
								minValue[i] = value;
								seed.set(x, y, z);
							}
						}
					}
				}
			}
			positions.add(seed);
		}

		// System.err.println( minValue + "     " + (float)(0.1 * result.getMax()) + "     " + 0.75f * minValue );
		// if ( minValue < (float)(0.1 * result.getMax()) )
		// {
		// minValue = 0.75f * minValue;
		// }
		final BitSet allNodes = new BitSet(dimX * dimY * dimZ);
		final Vector<Vector3f> seeds = new Vector<Vector3f>();
		int posCount = 0;
		for (int i = 0; i < annotations.getCurves().size(); i++) {
			final VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
				continue;
			}
			final Vector3f pos = positions.elementAt(posCount++);
			seeds.clear();
			seeds.add(pos);
			float size = fill(result, minValue[i], seeds);
			// System.err.println( text.getText() + " " + size );
			// System.err.println( minValue[i] + "     " + (float)(0.1 * result.getMax()) + "     " + overallMin + " " +
			// 100* (overallMin/result.getMax()) + " " + size );

			if (size == 0) {
				System.err.println(i + "    " + minValue[i] + "     " + 100 * (minValue[i] / result.getMax()) + "  " + size);
			}

			if (size > 20000) {
				result.getMask().clear();
				System.err.println(i + "    " + minValue[i] + "     " + 100 * (minValue[i] / result.getMax()) + "  " + size);

				// pos = text.elementAt(0);
				size = 0;
				for (int z = (int) (pos.Z - 4); z <= pos.Z + 4; z++) {
					for (int y = (int) (pos.Y - 4); y <= pos.Y + 4; y++) {
						for (int x = (int) (pos.X - 4); x <= pos.X + 4; x++) {
							if (pos.distance(new Vector3f(x, y, z)) <= 4) {
								final float value = result.getFloat(x, y, z);
								if (value >= minValue[i]) {
									final int index = z * dimY * dimX + y * dimX + x;
									result.getMask().set(index);
									size++;
								}
							}
						}
					}
				}
			}

			System.err.println(i + "    " + minValue[i] + "     " + 100 * (minValue[i] / result.getMax()) + "  " + size);

			allNodes.or(result.getMask());
			result.getMask().clear();

			final String sameSide = text.getText().contains("L") ? "L" : "R";
			final String opposite = text.getText().contains("L") ? "R" : "L";
			final String[] ids = text.getText().split(sameSide);
			int index = -1;
			for (int j = 0; j < ids.length; j++) {
				if (ids[j].length() > 0) {
					index = Integer.valueOf(ids[j]);
				}
			}
			if (index != -1) {
				float distancePair = 0;
				float distancePrevSame = 0;
				float distancePrevOpposite = 0;
				float distanceNextSame = 0;
				float distanceNextOpposite = 0;
				final String pair = new String(index + opposite);
				final String prevSame = new String( (index - 1) + sameSide);
				final String prevOpposite = new String( (index - 1) + opposite);
				final String nextSame = new String( (index + 1) + sameSide);
				final String nextOpposite = new String( (index + 1) + opposite);
				for (int j = 0; j < annotations.getCurves().size(); j++) {
					final VOIText text2 = (VOIText) annotations.getCurves().elementAt(j);
					if (text2.getText().equals(pair)) {
						distancePair = text.elementAt(0).distance(text2.elementAt(0));
					}
					if (text2.getText().equals(prevSame)) {
						distancePrevSame = text.elementAt(0).distance(text2.elementAt(0));
					}
					if (text2.getText().equals(prevOpposite)) {
						distancePrevOpposite = text.elementAt(0).distance(text2.elementAt(0));
					}
					if (text2.getText().equals(nextSame)) {
						distanceNextSame = text.elementAt(0).distance(text2.elementAt(0));
					}
					if (text2.getText().equals(nextOpposite)) {
						distanceNextOpposite = text.elementAt(0).distance(text2.elementAt(0));
					}
				}
				final float imageValue = image.getFloat((int) pos.X, (int) pos.Y, (int) pos.Z);
				final float dogValue = result.getFloat((int) pos.X, (int) pos.Y, (int) pos.Z);
				try {
					statistics.write(time + "," + text.getText() + "," + distancePair + "," + distancePrevSame + "," + distancePrevOpposite + ","
							+ distanceNextSame + "," + distanceNextOpposite + "," + size + "," + imageValue + "," + dogValue + "," + minValue[i] + "\n");
				} catch (final IOException e) {}
			}
		}

		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					final int index = z * dimY * dimX + y * dimX + x;
					if (allNodes.get(index)) {
						result.set(x, y, z, image.get(x, y, z));
					} else {
						result.set(x, y, z, image.getMin());
					}
				}
			}
		}
		// new ViewJFrameImage(result);
		result.calcMinMax();
		return result;
	}

	/**
	 * Returns a blurred image of the input image.
	 * 
	 * @param image
	 * @param sigma
	 * @return
	 */
	private static ModelImage blur(final ModelImage image, final int sigma) {
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
	 * IN PROGRESS to support k-means segmentation.
	 * 
	 * @param image
	 * @param intensityMin
	 * @param seedList
	 * @return
	 */
	private static int fill(final ModelImage image, final float intensityMin, final Vector<Vector3f> seedList) {
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		int count = 0;

		final BitSet mask = new BitSet(dimX * dimY * dimZ);
		while (seedList.size() > 0) {
			final Vector3f seed = seedList.remove(0);

			final int z = Math.round(seed.Z);
			final int y = Math.round(seed.Y);
			final int x = Math.round(seed.X);
			int index = z * dimX * dimY + y * dimX + x;
			if (mask.get(index)) {
				continue;
			}
			mask.set(index);
			float value;
			if (image.isColorImage()) {
				value = image.getFloatC(x, y, z, 2);
			} else {
				value = image.getFloat(x, y, z);
			}
			if ( (value >= intensityMin)) {
				for (int z1 = Math.max(0, z - 1); z1 <= Math.min(dimZ - 1, z + 1); z1++) {
					for (int y1 = Math.max(0, y - 1); y1 <= Math.min(dimY - 1, y + 1); y1++) {
						for (int x1 = Math.max(0, x - 1); x1 <= Math.min(dimX - 1, x + 1); x1++) {
							if ( ! ( (x == x1) && (y == y1) && (z == z1))) {
								index = z1 * dimX * dimY + y1 * dimX + x1;
								if ( !mask.get(index)) {
									if (image.isColorImage()) {
										value = image.getFloatC(x1, y1, z1, 2);
									} else {
										value = image.getFloat(x1, y1, z1);
									}
									if (value >= intensityMin) {
										seedList.add(new Vector3f(x1, y1, z1));
									}
								}
							}
						}
					}
				}
				count++;
			}
		}

		image.getMask().or(mask);
		return count;
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

	/**
	 * IN PROGRESS to support difference-of-gaussian segmentation
	 * 
	 * @param image
	 * @return
	 */
	private static ModelImage segmentAll2(final ModelImage image) {
		ModelImage blurs = blur(image, 3);
		blurs.calcMinMax();
		// new ViewJFrameImage(blurs);

		ModelImage blurb = blur(image, 5);
		blurb.calcMinMax();
		// new ViewJFrameImage(blurb);

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_gblur";
		final ModelImage resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, resultImage);

		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					resultImage.set(x, y, z, Math.max(0, blurs.getFloat(x, y, z) - blurb.getFloat(x, y, z)));
				}
			}
		}
		blurs.disposeLocal();
		blurs = null;
		blurb.disposeLocal();
		blurb = null;
		resultImage.calcMinMax();
		resultImage.restoreVOIs(image.getVOIsCopy());
		return resultImage;
	}

	private ModelImage imageA;

	private VOIVector latticeGrid;

	private VOI lattice = null;

	private VOIContour left;

	private VOIContour right;

	private VOIContour center;

	private VOIContour leftBackup;

	private VOIContour rightBackup;

	private float[] afTimeC;

	private float[] allTimes;

	private NaturalSpline3 centerSpline;

	private NaturalSpline3 leftSpline;

	private NaturalSpline3 rightSpline;

	private VOIContour centerPositions;

	private VOIContour leftPositions;

	private VOIContour rightPositions;

	private float length;

	private VOI leftLine;

	private VOI rightLine;

	private VOI centerLine;

	private Vector<Float> wormDiameters;

	private Vector<Vector3f> rightVectors;

	private Vector<Vector3f> upVectors;

	private int extent = -1;

	private Vector<Box3f> boxBounds;

	private Vector<Ellipsoid3f> ellipseBounds;

	private VOI samplingPlanes;

	private VOI displayContours;

	private VOI displayInterpolatedContours;

	private Vector3f pickedPoint = null;

	private int pickedAnnotation = -1;

	private VOI showSelectedVOI = null;

	private VOIContour[] showSelected = null;

	private final int DiameterBuffer = 0;

	private final int SampleLimit = 5;

	private final float minRange = .025f;

	private VOI leftMarker;

	private VOI rightMarker;

	private VOI growContours;

	private VOI annotationVOIs;

	private Vector3f wormOrigin = null;
	private Vector3f transformedOrigin = new Vector3f();

	private ModelImage markerSegmentation;

	private int[][] markerVolumes;

	private int[] markerIDs;

	private boolean[] completedIDs;

	private int[] currentID;

	/**
	 * Creates a new LatticeModel
	 * 
	 * @param imageA
	 */
	public LatticeModel(final ModelImage imageA) {
		this.imageA = imageA;
	}

	/**
	 * Creats a new LatticeModel with the given input lattice.
	 * 
	 * @param imageA
	 * @param lattice
	 */
	public LatticeModel(final ModelImage imageA, final VOI lattice) {
		this.imageA = imageA;
		this.lattice = lattice;

		// Assume image is isotropic (square voxels).
		if (lattice.getCurves().size() != 2) {
			return;
		}
		left = (VOIContour) lattice.getCurves().elementAt(0);
		right = (VOIContour) lattice.getCurves().elementAt(1);
		if (left.size() != right.size()) {
			return;
		}

		this.imageA.registerVOI(lattice);
		updateLattice(true);
	}

	/**
	 * Creates a new LatticeModel with the given set of annotations.
	 * 
	 * @param imageA
	 * @param annotation
	 * @param doAnnotation
	 */
	public LatticeModel(final ModelImage imageA, final VOI annotation, final boolean doAnnotation) {
		this.imageA = imageA;
		this.lattice = null;
		this.setAnnotations(annotation);
	}

	/**
	 * Add an annotation to the worm image.
	 * 
	 * @param textVOI
	 */
	public void addAnnotation(final VOI textVOI) {
		if (annotationVOIs == null) {
			final int colorID = 0;
			annotationVOIs = new VOI((short) colorID, "annotationVOIs", VOI.ANNOTATION, -1.0f);
			imageA.registerVOI(annotationVOIs);
		}
		final VOIText text = (VOIText) textVOI.getCurves().firstElement().clone();
		final Color c = text.getColor();
		text.update(new ColorRGBA(c.getRed() / 255.0f, c.getGreen() / 255.0f, c.getBlue() / 255.0f, 1f));
		annotationVOIs.getCurves().add(text);
		annotationVOIs.setColor(c);

		if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
			if (wormOrigin == null) {
				wormOrigin = new Vector3f(text.elementAt(0));
				// updateLattice(true);
			} else {
				wormOrigin.copy(text.elementAt(0));
				// updateLattice(false);
			}
		}
	}

	/**
	 * Adds a new left/right marker to the worm image.
	 * 
	 * @param pt
	 */
	public void addLeftRightMarker(final Vector3f pt) {
		if (lattice == null) {
			final short id = (short) imageA.getVOIs().getUniqueID();
			lattice = new VOI(id, "lattice", VOI.POLYLINE, (float) Math.random());

			left = new VOIContour(false);
			right = new VOIContour(false);
			lattice.getCurves().add(left);
			lattice.getCurves().add(right);

			this.imageA.registerVOI(lattice);
		}
		if (left.size() == right.size()) {
			left.add(new Vector3f(pt));
			pickedPoint = left.lastElement();
			// System.err.println( pt );

			if (leftMarker == null) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				leftMarker = new VOI(id, "leftMarker", VOI.POINT, (float) Math.random());
				this.imageA.registerVOI(leftMarker);
				leftMarker.importPoint(pt);
			} else {
				leftMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
				leftMarker.update();
			}
			return;
		} else {
			right.add(new Vector3f(pt));
			pickedPoint = right.lastElement();
			// System.err.println( pt );

			if (rightMarker == null) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				rightMarker = new VOI(id, "rightMarker", VOI.POINT, (float) Math.random());
				this.imageA.registerVOI(rightMarker);
				rightMarker.importPoint(pt);
			} else {
				rightMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
				rightMarker.update();
			}
		}
		// if ( left.size() == right.size() && left.size() > 1 )
		{
			updateLattice(true);
		}
	}

	/**
	 * Clears the selected VOI or Annotation point.
	 */
	public void clear3DSelection() {
		pickedPoint = null;
		pickedAnnotation = -1;
		if (showSelected != null) {
			imageA.unregisterVOI(showSelectedVOI);
		}
		final VOIVector vois = imageA.getVOIs();
		for (int i = vois.size() - 1; i >= 0; i--) {
			final VOI voi = vois.elementAt(i);
			final String name = voi.getName();
			if (name.equals("showSelected")) {
				// System.err.println( "clear3DSelection " + vois.elementAt(i).getName() );
				imageA.unregisterVOI(voi);
			}
		}
	}

	/**
	 * Enables user to start editing the lattice.
	 */
	public void clearAddLeftRightMarkers() {
		imageA.unregisterVOI(leftMarker);
		imageA.unregisterVOI(rightMarker);
		if (leftMarker != null) {
			leftMarker.dispose();
			leftMarker = null;
		}
		if (rightMarker != null) {
			rightMarker.dispose();
			rightMarker = null;
		}
	}

	/**
	 * Deletes the selected annotation or lattice point.
	 * 
	 * @param doAnnotation
	 */
	public void deleteSelectedPoint(final boolean doAnnotation) {
		if (doAnnotation) {
			if (pickedAnnotation != -1) {
				final VOIText text = (VOIText) annotationVOIs.getCurves().remove(pickedAnnotation);
				clear3DSelection();

				if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
					wormOrigin = null;
					// updateLattice(true);
				}
			}
		} else if ( !doAnnotation) {
			boolean deletedLeft = false;
			boolean deletedRight = false;
			if ( (rightMarker != null) && pickedPoint.equals(rightMarker.getCurves().elementAt(0).elementAt(0))) {
				imageA.unregisterVOI(rightMarker);
				rightMarker.dispose();
				rightMarker = null;

				deletedRight = true;
			}

			if ( (leftMarker != null) && pickedPoint.equals(leftMarker.getCurves().elementAt(0).elementAt(0))) {
				imageA.unregisterVOI(leftMarker);
				leftMarker.dispose();
				leftMarker = null;
				deletedLeft = true;

				if (rightMarker != null) {
					imageA.unregisterVOI(rightMarker);
					rightMarker.dispose();
					rightMarker = null;
					deletedRight = true;
				}
			}
			if (deletedLeft || deletedRight) {
				if (deletedLeft) {
					left.remove(left.lastElement());
				}
				if (deletedRight) {
					right.remove(right.lastElement());
				}
			} else {
				final int leftIndex = left.indexOf(pickedPoint);
				final int rightIndex = right.indexOf(pickedPoint);
				if (leftIndex != -1) {
					left.remove(leftIndex);
					right.remove(leftIndex);
					deletedLeft = true;
					deletedRight = true;
				} else if (rightIndex != -1) {
					left.remove(rightIndex);
					right.remove(rightIndex);
					deletedLeft = true;
					deletedRight = true;
				}
			}
			clear3DSelection();
			updateLattice(deletedLeft | deletedRight);
		}
		pickedPoint = null;
		pickedAnnotation = -1;
	}

	/**
	 * Deletes this LatticeModel
	 */
	public void dispose() {
		if (latticeGrid != null) {
			for (int i = latticeGrid.size() - 1; i >= 0; i--) {
				final VOI marker = latticeGrid.remove(i);
				imageA.unregisterVOI(marker);
			}
		}
		imageA.unregisterVOI(lattice);
		imageA.unregisterVOI(displayContours);
		imageA.unregisterVOI(leftLine);
		imageA.unregisterVOI(rightLine);
		imageA.unregisterVOI(centerLine);
		clear3DSelection();

		imageA = null;
		latticeGrid = null;
		lattice = null;
		left = null;
		right = null;
		center = null;
		afTimeC = null;
		allTimes = null;
		centerSpline = null;
		leftSpline = null;
		rightSpline = null;
		centerPositions = null;
		leftPositions = null;
		rightPositions = null;
		leftLine = null;
		rightLine = null;
		centerLine = null;

		// if ( centerTangents != null )
		// centerTangents.clear();
		// centerTangents = null;

		if (wormDiameters != null) {
			wormDiameters.clear();
		}
		wormDiameters = null;

		if (rightVectors != null) {
			rightVectors.clear();
		}
		rightVectors = null;

		if (upVectors != null) {
			upVectors.clear();
		}
		upVectors = null;

		if (boxBounds != null) {
			boxBounds.clear();
		}
		boxBounds = null;

		if (ellipseBounds != null) {
			ellipseBounds.clear();
		}
		ellipseBounds = null;

		samplingPlanes = null;
		displayContours = null;
		pickedPoint = null;
		showSelectedVOI = null;
		showSelected = null;
	}

	/**
	 * Returns the currently selected lattice point.
	 * 
	 * @return
	 */
	public Vector3f getPicked() {
		return pickedPoint;
	}

	/**
	 * Finds the closest point to the input point and sets it as the currently selected lattice or annotation point.
	 * 
	 * @param pt
	 * @param doAnnotation
	 * @return
	 */
	public Vector3f getPicked(final Vector3f pt, final boolean doAnnotation) {
		pickedPoint = null;

		if (doAnnotation) {
			if (annotationVOIs == null) {
				return null;
			}
			pickedAnnotation = -1;
			float minDist = Float.MAX_VALUE;
			for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
				final Vector3f annotationPt = annotationVOIs.getCurves().elementAt(i).elementAt(0);
				final float distance = pt.distance(annotationPt);
				if (distance < minDist) {
					minDist = distance;
					if (minDist <= 12) {
						pickedAnnotation = i;
					}
				}
			}
			if (pickedAnnotation != -1) {
				pickedPoint = annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(0);
			}
		} else {
			if (left == null) {
				return pickedPoint;
			}
			int closestL = -1;
			float minDistL = Float.MAX_VALUE;
			for (int i = 0; i < left.size(); i++) {
				final float distance = pt.distance(left.elementAt(i));
				if (distance < minDistL) {
					minDistL = distance;
					if (minDistL <= 12) {
						closestL = i;
					}
				}
			}
			int closestR = -1;
			float minDistR = Float.MAX_VALUE;
			if (right != null) {
				for (int i = 0; i < right.size(); i++) {
					final float distance = pt.distance(right.elementAt(i));
					if (distance < minDistR) {
						minDistR = distance;
						if (minDistR <= 12) {
							closestR = i;
						}
					}
				}
			}

			// System.err.println( minDistL + " " + minDistR );
			if ( (closestL != -1) && (closestR != -1)) {
				if (minDistL < minDistR) {
					// System.err.println( "Picked Lattice Left " + closestL );
					pickedPoint = left.elementAt(closestL);
				} else {
					// System.err.println( "Picked Lattice Right " + closestR );
					pickedPoint = right.elementAt(closestR);
				}
			} else if (closestL != -1) {
				// System.err.println( "Picked Lattice Left " + closestL );
				pickedPoint = left.elementAt(closestL);
			} else if (closestR != -1) {
				// System.err.println( "Picked Lattice Right " + closestR );
				pickedPoint = right.elementAt(closestR);
			}

			if (pickedPoint != null) {
				updateLattice(false);
			}
		}
		return pickedPoint;
	}

	/**
	 * Entry point in the lattice-based straightening algorithm. At this point a lattice must be defined, outlining how
	 * the worm curves in 3D. A lattice is defined ad a VOI with two curves of equal length marking the left-hand and
	 * right-hand sides or the worm.
	 * 
	 * @param displayResult, when true intermediate volumes and results are displayed as well as the final straighened
	 *            image.
	 */
	public void interpolateLattice(final boolean displayResult) {
		// save the original lattice into a backup in case the lattice
		// is modified to better fit the fluorescent marker segmentation:
		leftBackup = new VOIContour(false);
		rightBackup = new VOIContour(false);
		for (int i = 0; i < left.size(); i++) {
			leftBackup.add(new Vector3f(left.elementAt(i)));
			rightBackup.add(new Vector3f(right.elementAt(i)));
		}

		// Determine the distances between points on the lattice
		// distances are along the curve, not straight-line distances:
		final float[] closestTimes = new float[afTimeC.length];
		final float[] leftDistances = new float[afTimeC.length];
		final float[] rightDistances = new float[afTimeC.length];
		for (int i = 0; i < afTimeC.length; i++) {
			float minDif = Float.MAX_VALUE;
			for (int j = 0; j < allTimes.length; j++) {
				final float dif = Math.abs(allTimes[j] - afTimeC[i]);
				if (dif < minDif) {
					minDif = dif;
					closestTimes[i] = allTimes[j];
				}
			}
			leftDistances[i] = 0;
			rightDistances[i] = 0;
			if (i > 0) {
				rightDistances[i] = rightSpline.GetLength(closestTimes[i - 1], closestTimes[i]);
				leftDistances[i] = leftSpline.GetLength(closestTimes[i - 1], closestTimes[i]);
			}
		}

		// save the lattice statistics -- distance between pairs and distances between
		// lattice points along the curves
		saveLatticeStatistics(imageA, length, left, right, leftDistances, rightDistances, "_before");
		// save the original annotation positions
		saveAnnotationStatistics(imageA, null, null, null, "_before");

		// modify markers based on volume segmentation:
		markerVolumes = new int[left.size()][2];
		markerIDs = new int[left.size()];
		completedIDs = new boolean[left.size()];
		currentID = new int[] {0};
		// segment the left-right markers based on the lattice points:
		markerSegmentation = segmentMarkers(imageA, left, right, markerIDs, markerVolumes, false);
		// save the updated lattice positions, including any volume estimation for the marker segmentat at that lattice
		// point:
		saveLatticePositions(imageA, null, null, left, right, markerVolumes, "_before");
		for (int i = 0; i < completedIDs.length; i++) {
			if (markerIDs[i] == 0) {
				completedIDs[i] = true;
			}
		}

		// The algorithm interpolates between the lattice points, creating two smooth curves from head to tail along
		// the left and right-hand sides of the worm body. A third curve down the center-line of the worm body is
		// also generated. Eventually, the center-line curve will be used to determine the number of sample points
		// along the length of the straightened worm, and therefore the final length of the straightened worm volume.
		generateCurves();

		boxBounds = new Vector<Box3f>();
		ellipseBounds = new Vector<Ellipsoid3f>();
		final short sID = (short) (imageA.getVOIs().getUniqueID());
		samplingPlanes = new VOI(sID, "samplingPlanes");
		for (int i = 0; i < centerPositions.size(); i++) {
			final Vector3f rkEye = centerPositions.elementAt(i);
			final Vector3f rkRVector = rightVectors.elementAt(i);
			final Vector3f rkUVector = upVectors.elementAt(i);

			final Vector3f[] output = new Vector3f[4];
			final Vector3f rightV = Vector3f.scale(extent, rkRVector);
			final Vector3f upV = Vector3f.scale(extent, rkUVector);
			output[0] = Vector3f.add(Vector3f.neg(rightV), Vector3f.neg(upV));
			output[1] = Vector3f.add(rightV, Vector3f.neg(upV));
			output[2] = Vector3f.add(rightV, upV);
			output[3] = Vector3f.add(Vector3f.neg(rightV), upV);
			for (int j = 0; j < 4; j++) {
				output[j].add(rkEye);
			}
			final VOIContour kBox = new VOIContour(true);
			for (int j = 0; j < 4; j++) {
				kBox.addElement(output[j].X, output[j].Y, output[j].Z);
			}
			kBox.update(new ColorRGBA(0, 0, 1, 1));
			{
				samplingPlanes.importCurve(kBox);
			}

			final float curve = centerSpline.GetCurvature(allTimes[i]);
			final float scale = curve;
			final VOIContour ellipse = new VOIContour(true);
			final Ellipsoid3f ellipsoid = makeEllipse(rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse);
			ellipseBounds.add(ellipsoid);

			final Box3f box = new Box3f(ellipsoid.Center, ellipsoid.Axis, new float[] {extent, extent, 1});
			boxBounds.add(box);
		}

		createWormModel(imageA, samplingPlanes, ellipseBounds, wormDiameters, 2 * extent, displayResult);
	}

	/**
	 * Enables the user to move an annotation point with the mouse.
	 * 
	 * @param startPt 3D start point of a ray intersecting the volume.
	 * @param endPt 3D end point of a ray intersecting the volume.
	 * @param pt point along the ray with the maximum intensity value.
	 */
	public void modifyAnnotation(final Vector3f startPt, final Vector3f endPt, final Vector3f pt) {
		if (annotationVOIs == null) {
			return;
		}
		if (annotationVOIs.getCurves().size() == 0) {
			return;
		}
		if (pickedPoint != null) {
			final Vector3f diff = Vector3f.sub(pt, pickedPoint);
			pickedPoint.copy(pt);
			annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(1).add(diff);
			annotationVOIs.getCurves().elementAt(pickedAnnotation).update();
		} else {
			pickedPoint = null;
			pickedAnnotation = -1;
			float minDist = Float.MAX_VALUE;
			for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
				final Vector3f annotationPt = annotationVOIs.getCurves().elementAt(i).elementAt(0);
				final float distance = pt.distance(annotationPt);
				if (distance < minDist) {
					minDist = distance;
					if (minDist <= 12) {
						pickedAnnotation = i;
					}
				}
			}
		}
		if (pickedAnnotation != -1) {
			pickedPoint = annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(0);
			updateSelected();

			final VOIText text = (VOIText) annotationVOIs.getCurves().elementAt(pickedAnnotation);
			if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
				wormOrigin.copy(pickedPoint);
				// updateLattice(false);
			}
		}

	}

	/**
	 * Enables the user to modify the lattice point with the mouse.
	 * 
	 * @param startPt 3D start point of a ray intersecting the volume.
	 * @param endPt 3D end point of a ray intersecting the volume.
	 * @param pt point along the ray with the maximum intensity value.
	 */
	public void modifyLattice(final Vector3f startPt, final Vector3f endPt, final Vector3f pt) {
		if (pickedPoint != null) {
			pickedPoint.copy(pt);
			updateLattice(false);
			return;
		}
		pickedPoint = null;
		int closestL = -1;
		float minDistL = Float.MAX_VALUE;
		for (int i = 0; i < left.size(); i++) {
			final float distance = pt.distance(left.elementAt(i));
			if (distance < minDistL) {
				minDistL = distance;
				if (minDistL <= 12) {
					closestL = i;
				}
			}
		}
		int closestR = -1;
		float minDistR = Float.MAX_VALUE;
		for (int i = 0; i < right.size(); i++) {
			final float distance = pt.distance(right.elementAt(i));
			if (distance < minDistR) {
				minDistR = distance;
				if (minDistR <= 12) {
					closestR = i;
				}
			}
		}
		// System.err.println( minDistL + " " + minDistR );
		if ( (closestL != -1) && (closestR != -1)) {
			if (minDistL < minDistR) {
				// System.err.println( "Picked Lattice Left " + closestL );
				pickedPoint = left.elementAt(closestL);
			} else {
				// System.err.println( "Picked Lattice Right " + closestR );
				pickedPoint = right.elementAt(closestR);
			}
		} else if (closestL != -1) {
			// System.err.println( "Picked Lattice Left " + closestL );
			pickedPoint = left.elementAt(closestL);
		} else if (closestR != -1) {
			// System.err.println( "Picked Lattice Right " + closestR );
			pickedPoint = right.elementAt(closestR);
		}
		if (pickedPoint != null) {
			updateLattice(false);
			return;
		}
		// look at the vector under the mouse and see which lattice point is closest...
		final Segment3f mouseVector = new Segment3f(startPt, endPt);
		float minDist = Float.MAX_VALUE;
		for (int i = 0; i < left.size(); i++) {
			DistanceVector3Segment3 dist = new DistanceVector3Segment3(left.elementAt(i), mouseVector);
			float distance = dist.Get();
			if (distance < minDist) {
				minDist = distance;
				pickedPoint = left.elementAt(i);
			}
			dist = new DistanceVector3Segment3(right.elementAt(i), mouseVector);
			distance = dist.Get();
			if (distance < minDist) {
				minDist = distance;
				pickedPoint = right.elementAt(i);
			}
		}
		if ( (pickedPoint != null) && (minDist <= 12)) {
			updateLattice(false);
			return;
		}

		addInsertionPoint(startPt, endPt, pt);
	}

	/**
	 * Enables the user to move the selected point (lattice or annotation) with the arrow keys.
	 * 
	 * @param direction
	 * @param doAnnotation
	 */
	public void moveSelectedPoint(final Vector3f direction, final boolean doAnnotation) {
		if (pickedPoint != null) {
			pickedPoint.add(direction);
			if (doAnnotation && (pickedAnnotation != -1)) {
				annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(1).add(direction);
				annotationVOIs.getCurves().elementAt(pickedAnnotation).update();
				updateSelected();
			} else {
				updateLattice(false);
			}
		}
	}

	/**
	 * VOI operation redo
	 */
	public void redo() {
		updateLinks();
	}

	/**
	 * Enables the user to save annotations to a user-selected file.
	 */
	public void saveAnnotations() {
		final JFileChooser chooser = new JFileChooser();

		if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
			chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
		} else {
			chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
		}

		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		final int returnVal = chooser.showSaveDialog(null);

		String fileName = null, directory = null, voiDir;
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			fileName = chooser.getSelectedFile().getName();
			directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
			Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, "true");
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}

		if (fileName != null) {
			voiDir = new String(directory + fileName + File.separator);

			clear3DSelection();

			imageA.unregisterAllVOIs();
			imageA.registerVOI(annotationVOIs);
			saveAllVOIsTo(voiDir, imageA);

			imageA.unregisterAllVOIs();
			imageA.registerVOI(annotationVOIs);
			if (leftMarker != null) {
				imageA.registerVOI(leftMarker);
			}
			if (rightMarker != null) {
				imageA.registerVOI(rightMarker);
			}
			if (lattice != null) {
				imageA.registerVOI(lattice);
			}
			updateLattice(true);
		}

	}

	/**
	 * Enables the user to save the lattice to a user-selected file.
	 */
	public void saveLattice() {
		final JFileChooser chooser = new JFileChooser();

		if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
			chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
		} else {
			chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
		}

		chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		final int returnVal = chooser.showSaveDialog(null);

		String fileName = null, directory = null, voiDir;
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			fileName = chooser.getSelectedFile().getName();
			directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
			Preferences.setProperty(Preferences.PREF_VOI_LPS_SAVE, "true");
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}

		if (fileName != null) {
			voiDir = new String(directory + fileName + File.separator);

			clear3DSelection();

			imageA.unregisterAllVOIs();
			imageA.registerVOI(lattice);
			lattice.setColor(new Color(0, 0, 255));
			lattice.getCurves().elementAt(0).update(new ColorRGBA(0, 0, 1, 1));
			lattice.getCurves().elementAt(1).update(new ColorRGBA(0, 0, 1, 1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for (int j = 0; j < lattice.getCurves().elementAt(0).size(); j++) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				final VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float) Math.random());
				final VOIContour mainAxis = new VOIContour(false);
				mainAxis.add(lattice.getCurves().elementAt(0).elementAt(j));
				mainAxis.add(lattice.getCurves().elementAt(1).elementAt(j));
				marker.getCurves().add(mainAxis);
				marker.setColor(new Color(255, 255, 0));
				mainAxis.update(new ColorRGBA(1, 1, 0, 1));
				if (j == 0) {
					marker.setColor(new Color(0, 255, 0));
					mainAxis.update(new ColorRGBA(0, 1, 0, 1));
				}
				imageA.registerVOI(marker);
			}

			saveAllVOIsTo(voiDir, imageA);

			imageA.unregisterAllVOIs();
			imageA.registerVOI(lattice);
			if (leftMarker != null) {
				imageA.registerVOI(leftMarker);
			}
			if (rightMarker != null) {
				imageA.registerVOI(rightMarker);
			}
			if (annotationVOIs != null) {
				imageA.registerVOI(annotationVOIs);
			}
			updateLattice(true);
		}

	}

	/**
	 * Saves the lattice to the specified file and directory.
	 * 
	 * @param directory
	 * @param fileName
	 */
	public void saveLattice(final String directory, final String fileName) {
		if (fileName != null) {
			final String voiDir = new String(directory + fileName + File.separator);

			clear3DSelection();

			imageA.unregisterAllVOIs();
			imageA.registerVOI(lattice);
			lattice.setColor(new Color(0, 0, 255));
			lattice.getCurves().elementAt(0).update(new ColorRGBA(0, 0, 1, 1));
			lattice.getCurves().elementAt(1).update(new ColorRGBA(0, 0, 1, 1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);
			for (int j = 0; j < lattice.getCurves().elementAt(0).size(); j++) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				final VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float) Math.random());
				final VOIContour mainAxis = new VOIContour(false);
				mainAxis.add(lattice.getCurves().elementAt(0).elementAt(j));
				mainAxis.add(lattice.getCurves().elementAt(1).elementAt(j));
				marker.getCurves().add(mainAxis);
				marker.setColor(new Color(255, 255, 0));
				mainAxis.update(new ColorRGBA(1, 1, 0, 1));
				if (j == 0) {
					marker.setColor(new Color(0, 255, 0));
					mainAxis.update(new ColorRGBA(0, 1, 0, 1));
				}
				imageA.registerVOI(marker);
			}

			saveAllVOIsTo(voiDir, imageA);

			imageA.unregisterAllVOIs();
			imageA.registerVOI(lattice);
			if (leftMarker != null) {
				imageA.registerVOI(leftMarker);
			}
			if (rightMarker != null) {
				imageA.registerVOI(rightMarker);
			}
			if (annotationVOIs != null) {
				imageA.registerVOI(annotationVOIs);
			}
			updateLattice(true);
		}
	}

	/**
	 * Tests the current worm-segmentation algorithm.
	 */
	public void segmentWorm() {
		imageA.calcMinMax();
		float minValue = (float) (0.1 * imageA.getMax());
		float maxValue = (float) (1.0 * imageA.getMax());
		final ModelImage imageNoSeam = segmentAll1(imageA, minValue, maxValue, 20, 1);
		minValue = (float) (0.25 * imageNoSeam.getMax());
		maxValue = (float) (1.0 * imageNoSeam.getMax());
		segmentAll2(imageA, imageNoSeam, minValue, maxValue, 10, .5f);
	}

	/**
	 * Called when new annotations are loaded from file, replaces current annotations.
	 * 
	 * @param newAnnotations
	 */
	public void setAnnotations(final VOI newAnnotations) {
		if (annotationVOIs != null) {
			imageA.unregisterVOI(annotationVOIs);
		}
		annotationVOIs = newAnnotations;
		if (pickedAnnotation != -1) {
			clear3DSelection();
		}
		clearAddLeftRightMarkers();

		for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
			final VOIText text = (VOIText) annotationVOIs.getCurves().elementAt(i);
			final Color c = text.getColor();
			text.update(new ColorRGBA(c.getRed() / 255.0f, c.getGreen() / 255.0f, c.getBlue() / 255.0f, 1f));
			text.elementAt(1).copy(text.elementAt(0));
			text.elementAt(1).add(6, 0, 0);

			if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
				if (wormOrigin == null) {
					wormOrigin = new Vector3f(text.elementAt(0));
					// updateLattice(true);
				} else {
					wormOrigin.copy(text.elementAt(0));
					// updateLattice(false);
				}
			}
		}
	}

	/**
	 * Called when a new lattice is loaded from file, replaces the current lattice.
	 * 
	 * @param newLattice
	 */
	public void setLattice(final VOI newLattice) {
		if (lattice != null) {
			imageA.unregisterVOI(lattice);
		}
		this.lattice = newLattice;

		// Assume image is isotropic (square voxels).
		if (lattice.getCurves().size() != 2) {
			return;
		}
		left = (VOIContour) lattice.getCurves().elementAt(0);
		right = (VOIContour) lattice.getCurves().elementAt(1);
		if (left.size() != right.size()) {
			return;
		}

		this.imageA.registerVOI(lattice);
		clear3DSelection();
		clearAddLeftRightMarkers();
		updateLattice(true);
	}

	/**
	 * Sets the currently selected point (lattice or annotation).
	 * 
	 * @param pt
	 * @param doAnnotation
	 */
	public void setPicked(final Vector3f pt, final boolean doAnnotation) {
		if (pickedPoint == null) {
			return;
		}

		if (doAnnotation && (pickedAnnotation != -1)) {
			final Vector3f diff = Vector3f.sub(pt, pickedPoint);
			pickedPoint.copy(pt);
			annotationVOIs.getCurves().elementAt(pickedAnnotation).elementAt(1).add(diff);
			annotationVOIs.getCurves().elementAt(pickedAnnotation).update();
			updateSelected();
		} else if ( !doAnnotation) {
			if ( (leftMarker != null) && pickedPoint.equals(leftMarker.getCurves().elementAt(0).elementAt(0))) {
				leftMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
				leftMarker.update();
			}
			if ( (rightMarker != null) && pickedPoint.equals(rightMarker.getCurves().elementAt(0).elementAt(0))) {
				rightMarker.getCurves().elementAt(0).elementAt(0).copy(pt);
				rightMarker.update();
			}
			pickedPoint.copy(pt);
			updateLattice(false);
		}
	}

	/**
	 * Enables the user to visualize the final expanded contours.
	 */
	public void showExpandedModel() {
		if (displayInterpolatedContours != null) {
			if ( (imageA.isRegistered(displayInterpolatedContours) == -1)) {
				imageA.registerVOI(displayInterpolatedContours);
				imageA.notifyImageDisplayListeners();
			} else if ( (imageA.isRegistered(displayInterpolatedContours) != -1)) {
				imageA.unregisterVOI(displayInterpolatedContours);
				imageA.notifyImageDisplayListeners();
			}
		}
	}

	/**
	 * Enables the user to visualize the simple ellipse-based model of the worm during lattice construction.
	 */
	public void showModel() {
		if ( (imageA.isRegistered(displayContours) == -1)) {
			imageA.registerVOI(displayContours);
			imageA.notifyImageDisplayListeners();
		} else if ( (imageA.isRegistered(displayContours) != -1)) {
			imageA.unregisterVOI(displayContours);
			imageA.notifyImageDisplayListeners();
		}
	}

	/**
	 * VOI operation undo.
	 */
	public void undo() {
		updateLinks();
	}

	// private int fill( ModelImage image, ModelImage model, float intensityMin,
	// Vector<Vector3f> seedList )
	// {
	// int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
	// int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
	// int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
	//
	// double averageValue = 0;
	// int count = 0;
	//
	//
	// while ( seedList.size() > 0 )
	// {
	// Vector3f seed = seedList.remove(0);
	//
	// int z = Math.round(seed.Z);
	// int y = Math.round(seed.Y);
	// int x = Math.round(seed.X);
	// float value = model.getFloat(x,y,z);
	// if ( value != 0 )
	// {
	// continue;
	// }
	// if ( image.isColorImage() )
	// {
	// value = image.getFloatC(x, y, z, 2);
	// }
	// else
	// {
	// value = image.getFloat(x, y, z);
	// }
	// if ( (value >= intensityMin) )
	// {
	// for ( int z1 = Math.max(0, z-1); z1 <= Math.min(dimZ-1, z+1); z1++ )
	// {
	// for ( int y1 = Math.max(0, y-1); y1 <= Math.min(dimY-1, y+1); y1++ )
	// {
	// for ( int x1 = Math.max(0, x-1); x1 <= Math.min(dimX-1, x+1); x1++ )
	// {
	// if ( !((x == x1) && (y == y1) && (z == z1)) )
	// {
	// if ( image.isColorImage() )
	// {
	// value = image.getFloatC(x1, y1, z1, 2);
	// }
	// else
	// {
	// value = image.getFloat(x1, y1, z1);
	// }
	// if ( value >= intensityMin )
	// {
	// seedList.add( new Vector3f(x1,y1,z1) );
	// }
	// }
	// }
	// }
	// }
	// count++;
	// model.set(x, y, z, 1);
	// if ( image.isColorImage() )
	// {
	// value = image.getFloatC(x, y, z, 2);
	// }
	// else
	// {
	// value = image.getFloat(x, y, z);
	// }
	// averageValue += value;
	// }
	// }
	// // if ( count != 0 )
	// // {
	// // averageValue /= (float)count;
	// // System.err.println( "fill markers " + count + " " + (float)averageValue + " " +
	// (float)(averageValue/image.getMax()) );
	// // }
	// return count;
	// }

	/**
	 * Adds a point to the lattice.
	 * 
	 * @param startPt
	 * @param endPt
	 * @param maxPt
	 */
	private void addInsertionPoint(final Vector3f startPt, final Vector3f endPt, final Vector3f maxPt) {
		final Segment3f mouseVector = new Segment3f(startPt, endPt);
		float minDistL = Float.MAX_VALUE;
		int minIndexL = -1;
		Vector3f newLeft = null;
		for (int i = 0; i < left.size() - 1; i++) {
			final Segment3f leftS = new Segment3f(left.elementAt(i), left.elementAt(i + 1));
			final DistanceSegment3Segment3 dist = new DistanceSegment3Segment3(mouseVector, leftS);
			final float distance = dist.Get();
			if (distance < minDistL) {
				minDistL = distance;
				if (minDistL <= 12) {
					// System.err.println( dist.GetSegment0Parameter() + " " + dist.GetSegment1Parameter() );
					minIndexL = i;
					newLeft = Vector3f.add(leftS.Center, Vector3f.scale(dist.GetSegment1Parameter(), leftS.Direction));
					newLeft.copy(maxPt);
				}
			}
		}
		float minDistR = Float.MAX_VALUE;
		int minIndexR = -1;
		Vector3f newRight = null;
		for (int i = 0; i < left.size() - 1; i++) {
			final Segment3f rightS = new Segment3f(right.elementAt(i), right.elementAt(i + 1));
			final DistanceSegment3Segment3 dist = new DistanceSegment3Segment3(mouseVector, rightS);
			final float distance = dist.Get();
			if (distance < minDistR) {
				minDistR = distance;
				if (minDistR <= 12) {
					// System.err.println( dist.GetSegment0Parameter() + " " + dist.GetSegment1Parameter() );
					minIndexR = i;
					newRight = Vector3f.add(rightS.Center, Vector3f.scale(dist.GetSegment1Parameter(), rightS.Direction));
					newRight.copy(maxPt);
				}
			}
		}
		if ( (minIndexL != -1) && (minIndexR != -1)) {
			if (minDistL < minDistR) {
				// System.err.println( "Add to left " + (minIndexL+1) );
				left.add(minIndexL + 1, newLeft);
				pickedPoint = left.elementAt(minIndexL + 1);
				newRight = Vector3f.add(right.elementAt(minIndexL), right.elementAt(minIndexL + 1));
				newRight.scale(0.5f);
				right.add(minIndexL + 1, newRight);

				updateLattice(true);
			} else {
				// System.err.println( "Add to right " + (minIndexR+1) );
				right.add(minIndexR + 1, newRight);
				pickedPoint = right.elementAt(minIndexR + 1);
				newLeft = Vector3f.add(left.elementAt(minIndexR), left.elementAt(minIndexR + 1));
				newLeft.scale(0.5f);
				left.add(minIndexR + 1, newLeft);

				updateLattice(true);
			}
		} else if (minIndexL != -1) {
			// System.err.println( "Add to left " + (minIndexL+1) );
			left.add(minIndexL + 1, newLeft);
			pickedPoint = left.elementAt(minIndexL + 1);
			newRight = Vector3f.add(right.elementAt(minIndexL), right.elementAt(minIndexL + 1));
			newRight.scale(0.5f);
			right.add(minIndexL + 1, newRight);

			updateLattice(true);
		} else if (minIndexR != -1) {
			// System.err.println( "Add to right " + (minIndexR+1) );
			right.add(minIndexR + 1, newRight);
			pickedPoint = right.elementAt(minIndexR + 1);
			newLeft = Vector3f.add(left.elementAt(minIndexR), left.elementAt(minIndexR + 1));
			newLeft.scale(0.5f);
			left.add(minIndexR + 1, newLeft);

			updateLattice(true);
		}
	}

	// private void fill( ModelImage image, ModelImage model, ModelImage markers, float intensityMin, Box3f box )
	// {
	// Vector3f min = new Vector3f();
	// Vector3f max = new Vector3f();
	// box.ComputeBounds(min, max);
	// Vector3f pt = new Vector3f();
	// int count = 0;
	// for ( int z = (int)min.Z; z <= max.Z; z++ )
	// {
	// for ( int y = (int)min.Y; y <= max.Y; y++ )
	// {
	// for ( int x = (int)min.X; x <= max.X; x++ )
	// {
	// pt.set(x, y, z);
	// if ( ContBox3f.InBox( pt, box ) )
	// {
	// float value = image.getFloat( x, y, z );
	// if ( value >= intensityMin )
	// {
	// value = markers.getFloat( x, y, z );
	// if ( value == 0 )
	// {
	// count++;
	// model.set(x, y, z, 1);
	// }
	// }
	// }
	// }
	// }
	// }
	// System.err.println( "fill " + count );
	// }

	/**
	 * As part of growing the contours to fit the worm model, this function checks that all annotations are included in
	 * the current version of the model.
	 * 
	 * @param model
	 * @return
	 */
	private boolean checkAnnotations(final ModelImage model) {
		boolean outsideFound = false;
		for (int i = 0; i < left.size(); i++) {
			Vector3f position = left.elementAt(i);
			int x = Math.round(position.X);
			int y = Math.round(position.Y);
			int z = Math.round(position.Z);
			float value = model.getFloat(x, y, z);
			// float value = model.getFloatTriLinearBounds( position.X, position.Y, position.Z );
			if (value == 0) {
				outsideFound = true;
			}
			position = right.elementAt(i);
			x = Math.round(position.X);
			y = Math.round(position.Y);
			z = Math.round(position.Z);
			value = model.getFloat(x, y, z);
			// value = model.getFloatTriLinearBounds( position.X, position.Y, position.Z );
			if (value == 0) {
				outsideFound = true;
			}
		}

		if (annotationVOIs == null) {
			return !outsideFound;
			// return true;
		}
		if (annotationVOIs.getCurves().size() == 0) {
			return !outsideFound;
			// return true;
		}

		// outsideFound = false;
		for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
			final VOIText text = (VOIText) annotationVOIs.getCurves().elementAt(i);
			final Vector3f position = text.elementAt(0);
			final int x = Math.round(position.X);
			final int y = Math.round(position.Y);
			final int z = Math.round(position.Z);
			final float value = model.getFloat(x, y, z);
			// float value = model.getFloatTriLinearBounds( position.X, position.Y, position.Z );
			// System.err.println( text.getText() + " " + position + "  " + value );
			if (value == 0) {
				outsideFound = true;
			}
		}
		return !outsideFound;
	}

	/**
	 * Resets the natural spline curves when the lattice changes.
	 */
	private void clearCurves() {

		if (center != null) {
			center.dispose();
			center = null;
		}
		afTimeC = null;
		centerSpline = null;
		leftSpline = null;
		rightSpline = null;

		centerPositions = null;
		leftPositions = null;
		rightPositions = null;

		if (wormDiameters != null) {
			wormDiameters.removeAllElements();
			wormDiameters = null;
		}
		if (rightVectors != null) {
			rightVectors.removeAllElements();
			rightVectors = null;
		}
		if (upVectors != null) {
			upVectors.removeAllElements();
			upVectors = null;
		}

		allTimes = null;

		if (centerLine != null) {
			imageA.unregisterVOI(centerLine);
			centerLine.dispose();
			centerLine = null;
		}
		if (rightLine != null) {
			imageA.unregisterVOI(rightLine);
			rightLine.dispose();
			rightLine = null;
		}
		if (leftLine != null) {
			imageA.unregisterVOI(leftLine);
			leftLine.dispose();
			leftLine = null;
		}

		if (displayContours != null) {
			imageA.unregisterVOI(displayContours);
			displayContours.dispose();
			displayContours = null;
		}
	}

	/**
	 * Creates the worm model based on the segmented left-right marker images and the current lattice and natural
	 * splines fitting the lattice.
	 * 
	 * @param imageA
	 * @param samplingPlanes
	 * @param ellipseBounds
	 * @param diameters
	 * @param diameter
	 * @param straighten
	 * @param displayResult
	 */
	private void createWormModel(final ModelImage imageA, final VOI samplingPlanes, final Vector<Ellipsoid3f> ellipseBounds, final Vector<Float> diameters,
			final int diameter, final boolean displayResult) {
		final int[] resultExtents = new int[] {diameter, diameter, samplingPlanes.getCurves().size()};

		String imageName = imageA.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		ModelImage model = new ModelImage(ModelStorageBase.FLOAT, imageA.getExtents(), imageName + "_model.xml");
		JDialogBase.updateFileInfo(imageA, model);

		ModelImage insideConflict = new ModelImage(ModelStorageBase.BOOLEAN, imageA.getExtents(), imageName + "_insideConflict.xml");
		JDialogBase.updateFileInfo(imageA, insideConflict);

		ModelImage inside = new ModelImage(ModelStorageBase.INTEGER, imageA.getExtents(), imageName + "_insideMask.xml");
		JDialogBase.updateFileInfo(imageA, inside);

		// 7. The set of ellipses from the head of the worm to the tail defines an approximate outer boundary of the
		// worm in 3D.
		// The centers of each ellipse are spaced one voxel apart along the center line curve of the worm, and each
		// ellipse
		// corresponds to a single output slice in the final straightened image. This step generates a model of the worm
		// where each voxel that falls within one of the ellipses is labeled with the corresponding output slice value.
		// Voxels where multiple ellipses intersect are labeled as conflict voxels. Once all ellipses have been
		// evaluated,
		// the conflict voxels are removed from the model.
		final int dimX = imageA.getExtents().length > 0 ? imageA.getExtents()[0] : 1;
		final int dimY = imageA.getExtents().length > 1 ? imageA.getExtents()[1] : 1;
		final int dimZ = imageA.getExtents().length > 2 ? imageA.getExtents()[2] : 1;
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					model.set(x, y, z, 0);
					insideConflict.set(x, y, z, false);
				}
			}
		}

		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			final Vector3f[] corners = new Vector3f[4];
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}

			float planeDist = -Float.MAX_VALUE;
			if (i < (samplingPlanes.getCurves().size() - 1)) {
				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
				for (int j = 0; j < 4; j++) {
					final float distance = corners[j].distance(kBox.elementAt(j));
					if (distance > planeDist) {
						planeDist = distance;
					}
				}
			}

			if (i < (samplingPlanes.getCurves().size() - 1)) {
				planeDist *= 3;
				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
				final Vector3f[] steps = new Vector3f[4];
				final Vector3f[] cornersSub = new Vector3f[4];
				for (int j = 0; j < 4; j++) {
					steps[j] = Vector3f.sub(kBox.elementAt(j), corners[j]);
					steps[j].scale(1f / planeDist);
					cornersSub[j] = new Vector3f(corners[j]);
				}
				for (int j = 0; j < planeDist; j++) {
					initializeModelandConflicts(imageA, model, insideConflict, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i),
							1.5f * diameters.elementAt(i), boxBounds.elementAt(i), i + 1);
					for (int k = 0; k < 4; k++) {
						cornersSub[k].add(steps[k]);
					}
				}
			} else {
				planeDist = 15;
				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i - 1);
				final Vector3f[] steps = new Vector3f[4];
				final Vector3f[] cornersSub = new Vector3f[4];
				for (int j = 0; j < 4; j++) {
					steps[j] = Vector3f.sub(corners[j], kBox.elementAt(j));
					steps[j].scale(1f / planeDist);
					// cornersSub[j] = Vector3f.add( corners[j], kBox.elementAt(j) ); cornersSub[j].scale(0.5f);
					cornersSub[j] = new Vector3f(corners[j]);
				}
				for (int j = 0; j < planeDist; j++) {
					initializeModelandConflicts(imageA, model, insideConflict, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i),
							1.5f * diameters.elementAt(i), boxBounds.elementAt(i), i + 1);
					for (int k = 0; k < 4; k++) {
						cornersSub[k].add(steps[k]);
					}
				}
			}
		}

		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					if (insideConflict.getBoolean(x, y, z)) {
						model.set(x, y, z, 0);
					}
				}
			}
		}
		insideConflict.disposeLocal();
		insideConflict = null;

		// Save the marker segmentation image:
		saveImage(imageName, markerSegmentation, true);
		markerImageToColor(markerSegmentation, displayResult);

		// 8. The marker segmentation image is used to resolve conflicts where multiple ellipses overlap.
		// Each slice in the output image should extend only to the edges of the left-right markers for the
		// corresponding region of the worm volume. This prevents a slice from extending beyond the worm boundary and
		// capturing the
		// adjacent fold of worm. Because the marker segmentation image only segments the left-right markers it is not
		// possible
		// to resolve all potential conflicts.

		// Calculate which slice IDs correspond to which segmented markers:
		final float[] sliceIDs = new float[samplingPlanes.getCurves().size()];
		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
			sliceIDs[i] = 0;
			final VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			final Vector3f[] corners = new Vector3f[4];
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}
			mapSliceIDstoMarkerIDs(model, markerSegmentation, sliceIDs, markerIDs, completedIDs, currentID, 0, i, resultExtents, corners,
					ellipseBounds.elementAt(i));
		}

		// Fill in the marker segmentation image with the corresponding slice IDs:
		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			final Vector3f[] corners = new Vector3f[4];
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}

			float planeDist = -Float.MAX_VALUE;
			if (i < (samplingPlanes.getCurves().size() - 1)) {
				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
				for (int j = 0; j < 4; j++) {
					final float distance = corners[j].distance(kBox.elementAt(j));
					if (distance > planeDist) {
						planeDist = distance;
					}
				}
			}

			if (sliceIDs[i] != 0) {
				if (i < (samplingPlanes.getCurves().size() - 1)) {
					planeDist *= 3;
					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
					final Vector3f[] steps = new Vector3f[4];
					final Vector3f[] cornersSub = new Vector3f[4];
					for (int j = 0; j < 4; j++) {
						steps[j] = Vector3f.sub(kBox.elementAt(j), corners[j]);
						steps[j].scale(1f / planeDist);
						cornersSub[j] = new Vector3f(corners[j]);
					}
					for (int j = 0; j < planeDist; j++) {
						fillMarkerSegmentationImage(model, markerSegmentation, sliceIDs, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i));
						for (int k = 0; k < 4; k++) {
							cornersSub[k].add(steps[k]);
						}
					}
				} else {
					planeDist = 15;
					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i - 1);
					final Vector3f[] steps = new Vector3f[4];
					final Vector3f[] cornersSub = new Vector3f[4];
					for (int j = 0; j < 4; j++) {
						steps[j] = Vector3f.sub(corners[j], kBox.elementAt(j));
						steps[j].scale(1f / planeDist);
						cornersSub[j] = new Vector3f(corners[j]);
					}
					for (int j = 0; j < planeDist; j++) {
						fillMarkerSegmentationImage(model, markerSegmentation, sliceIDs, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i));
						for (int k = 0; k < 4; k++) {
							cornersSub[k].add(steps[k]);
						}
					}
				}
			}
		}

		// if ( displayResult )
		// {
		// markerSegmentation.calcMinMax();
		// new ViewJFrameImage((ModelImage)markerSegmentation.clone());
		// }

		// resolve conflicts in the model with the marker segmentation image:
		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			final Vector3f[] corners = new Vector3f[4];
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}

			float planeDist = -Float.MAX_VALUE;
			if (i < (samplingPlanes.getCurves().size() - 1)) {
				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
				for (int j = 0; j < 4; j++) {
					final float distance = corners[j].distance(kBox.elementAt(j));
					if (distance > planeDist) {
						planeDist = distance;
					}
				}
			}

			if (sliceIDs[i] != 0) {
				if (i < (samplingPlanes.getCurves().size() - 1)) {
					planeDist *= 3;
					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
					final Vector3f[] steps = new Vector3f[4];
					final Vector3f[] cornersSub = new Vector3f[4];
					for (int j = 0; j < 4; j++) {
						steps[j] = Vector3f.sub(kBox.elementAt(j), corners[j]);
						steps[j].scale(1f / planeDist);
						cornersSub[j] = new Vector3f(corners[j]);
					}
					for (int j = 0; j < planeDist; j++) {
						resolveModelConflicts(model, markerSegmentation, sliceIDs, 0, i, resultExtents, cornersSub, i + 1);
						for (int k = 0; k < 4; k++) {
							cornersSub[k].add(steps[k]);
						}
					}
				} else {
					planeDist = 15;
					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i - 1);
					final Vector3f[] steps = new Vector3f[4];
					final Vector3f[] cornersSub = new Vector3f[4];
					for (int j = 0; j < 4; j++) {
						steps[j] = Vector3f.sub(corners[j], kBox.elementAt(j));
						steps[j].scale(1f / planeDist);
						cornersSub[j] = new Vector3f(corners[j]);
					}
					for (int j = 0; j < planeDist; j++) {
						resolveModelConflicts(model, markerSegmentation, sliceIDs, 0, i, resultExtents, cornersSub, i + 1);
						for (int k = 0; k < 4; k++) {
							cornersSub[k].add(steps[k]);
						}
					}
				}
			}
		}

		// 9. The last step is an attempt to ensure that as much of the worm data is captured by the algorithm as
		// possible.
		// Using the marker segmentation image where possible as a guide to the worm boundary, each slice of worm model
		// is
		// grown outward. The points on the boundary are expanded in an iterative process until the point comes in
		// contact
		// with another edge of the worm. For areas of the worm where it folds back on itself this results in a
		// flattened
		// cross-section where the folds press against each other.
		// For areas of the worm where the cross-section does not contact other sections of the worm the 2D contour
		// extends
		// outward until it reaches the edge of the sample plane, capturing as much data as possible.
		int growStep = 0;
		while ( (growStep < 25) && ( !checkAnnotations(model) || (growStep < 20))) {
			growEdges(model, markerSegmentation, sliceIDs, growStep++);
		}
		if ( !checkAnnotations(model)) {
			System.err.println("    generateMasks " + growStep + " " + false);
		}

		final short sID = (short) (imageA.getVOIs().getUniqueID());
		if (displayInterpolatedContours != null) {
			imageA.unregisterVOI(displayInterpolatedContours);
			displayInterpolatedContours.dispose();
			displayInterpolatedContours = null;
		}
		displayInterpolatedContours = new VOI(sID, "interpolatedContours");
		displayInterpolatedContours.setColor(Color.blue);

		for (int i = 0; i < growContours.getCurves().size(); i += 30) {
			final VOIContour contour = (VOIContour) growContours.getCurves().elementAt(i).clone();
			contour.trimPoints(0.5, true);
			displayInterpolatedContours.getCurves().add(contour);
			contour.update(new ColorRGBA(0, 1, 0, 1));
			contour.setVolumeDisplayRange(minRange);
		}

		// Call the straightening step:
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					if (model.getFloat(x, y, z) != 0) {
						inside.set(x, y, z, 1);
					}
				}
			}
		}
		saveImage(imageName, inside, false);
		straighten(imageA, resultExtents, imageName, model, true, displayResult, true);

		markerSegmentation.disposeLocal();
		markerSegmentation = null;

		inside.disposeLocal();
		inside = null;

		model.disposeLocal();
		model = null;
	}

	/**
	 * Fills in the fluorescent marker for the marker segmentation.
	 * 
	 * @param image
	 * @param gmImage
	 * @param model
	 * @param gmMin
	 * @param intensityMin
	 * @param centerPt
	 * @param seedList
	 * @param saveSeedList
	 * @param maxDiameter
	 * @param id
	 * @return
	 */
	private int fill(final ModelImage image, final ModelImage gmImage, final ModelImage model, final float gmMin, final float intensityMin,
			final Vector3f centerPt, final Vector<Vector3f> seedList, final Vector<Vector3f> saveSeedList,

			final int maxDiameter, final int id) {
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		double averageValue = 0;
		int count = 0;

		while (seedList.size() > 0) {
			final Vector3f seed = seedList.remove(0);
			if (centerPt.distance(seed) > maxDiameter) {
				saveSeedList.add(seed);
				continue;
			}

			final int z = Math.round(seed.Z);
			final int y = Math.round(seed.Y);
			final int x = Math.round(seed.X);
			float value = model.getFloat(x, y, z);
			if (value != 0) {
				continue;
			}
			float valueGM;
			if (image.isColorImage()) {
				value = image.getFloatC(x, y, z, 2);
				valueGM = gmImage.getFloatC(x, y, z, 2);
			} else {
				value = image.getFloat(x, y, z);
				valueGM = gmImage.getFloat(x, y, z);
			}
			if ( (value >= intensityMin) && (valueGM >= gmMin)) {
				for (int z1 = Math.max(0, z - 1); z1 <= Math.min(dimZ - 1, z + 1); z1++) {
					for (int y1 = Math.max(0, y - 1); y1 <= Math.min(dimY - 1, y + 1); y1++) {
						for (int x1 = Math.max(0, x - 1); x1 <= Math.min(dimX - 1, x + 1); x1++) {
							if ( ! ( (x == x1) && (y == y1) && (z == z1))) {
								if (image.isColorImage()) {
									value = image.getFloatC(x1, y1, z1, 2);
									valueGM = gmImage.getFloatC(x1, y1, z1, 2);
								} else {
									value = image.getFloat(x1, y1, z1);
									valueGM = gmImage.getFloat(x1, y1, z1);
								}
								if (value >= intensityMin) {
									seedList.add(new Vector3f(x1, y1, z1));
								}
							}
						}
					}
				}
				count++;
				model.set(x, y, z, id);
				if (image.isColorImage()) {
					value = image.getFloatC(x, y, z, 2);
				} else {
					value = image.getFloat(x, y, z);
				}
				averageValue += value;
			}
		}
		// if ( count != 0 )
			// {
			// averageValue /= (float)count;
		// System.err.println( "fill markers " + count + " " + (float)averageValue + " " +
		// (float)(averageValue/image.getMax()) );
		// }
		return count;
	}

	/**
	 * Fills each model slice of the marker segmentation image with the current slice value.
	 * 
	 * @param model
	 * @param markerSegmentation
	 * @param sliceIDs
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param ellipseBound
	 */
	private void fillMarkerSegmentationImage(final ModelImage model, final ModelImage markerSegmentation, final float[] sliceIDs, final int tSlice,
			final int slice, final int[] extents, final Vector3f[] verts, final Ellipsoid3f ellipseBound) {
		final int iBound = extents[0];
		final int jBound = extents[1];

		final int[] dimExtents = markerSegmentation.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];
		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

		final int buffFactor = 1;

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		final Vector3f currentPoint = new Vector3f();
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > markerSegmentation.getSize()))) {

					// do nothing
				} else {
					currentPoint.set(x, y, z);
					final boolean isInside = ellipseBound.Contains(currentPoint);
					if (isInside) {
						markerSegmentation.set(iIndex, jIndex, kIndex, sliceIDs[slice]);
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}
	}

	/**
	 * Generates the set of natural spline curves to fit the current lattice.
	 */
	private void generateCurves() {
		clearCurves();
		short sID;

		// 1. The center line of the worm is calculated from the midpoint between the left and right points of the
		// lattice.
		center = new VOIContour(false);
		for (int i = 0; i < left.size(); i++) {
			final Vector3f centerPt = Vector3f.add(left.elementAt(i), right.elementAt(i));
			centerPt.scale(0.5f);
			center.add(centerPt);
		}

		// 2. Three curves are generated from the three sets of points (left, center, right) using natural splines
		// to fit the points. Natural splines generate curves that pass through the control points, have continuous
		// first and second derivatives and minimize the bending between points.
		afTimeC = new float[center.size()];
		centerSpline = smoothCurve(center, afTimeC);
		leftSpline = smoothCurve2(left, afTimeC);
		rightSpline = smoothCurve2(right, afTimeC);

		centerPositions = new VOIContour(false);
		leftPositions = new VOIContour(false);
		rightPositions = new VOIContour(false);

		wormDiameters = new Vector<Float>();
		rightVectors = new Vector<Vector3f>();
		upVectors = new Vector<Vector3f>();

		// 3. The center curve is uniformly sampled along the length of the curve.
		// The step size is set to be one voxel. This determines the length of the final straightened
		// image and ensures that each slice in the straightened image is equally spaced. The points
		// along the curve are the center-points of the output slices.
		// 4. Each spline can be parametrized with a parameter t, where the start of the curve has t = 0
		// and the end of the curve has t = 1. This parameter t is calculated on the center curve, and
		// used to determine the corresponding locations on the left and right hand curves, which may be
		// longer or shorter than the center curve, depending on how the worm bends. Using the parametrization
		// ensures that the left and right hand curves are sampled the same number of times as the center curve
		// and that the points from start to end on all curves are included.
		// 5. Given the current point on the center curve and the corresponding positions on the left and right hand
		// curves, the 2D sampling plane can be defined. The center point of the plane is the current point on the
		// center curve.
		// The plane normal is the first derivative of the center line spline. The plane horizontal axis is the vector
		// from the position on the left hand curve to the position on the right hand curve.
		// The plane vertical axis is the cross-product of the plane normal with the plane horizontal axis.
		// This method fully defines the sample plane location and orientation as it sweeps through the 3D volume of the
		// worm.
		length = centerSpline.GetLength(0, 1);
		allTimes = new float[(int) (Math.ceil(length)) + 1];
		extent = -1;
		for (int i = 0; i <= length; i++) {
			final float t = centerSpline.GetTime(i);
			centerPositions.add(centerSpline.GetPosition(t));
			leftPositions.add(leftSpline.GetPosition(t));
			rightPositions.add(rightSpline.GetPosition(t));

			allTimes[i] = t;
			final Vector3f normal = centerSpline.GetTangent(t);
			final Vector3f leftPt = leftSpline.GetPosition(t);
			final Vector3f rightPt = rightSpline.GetPosition(t);

			final Vector3f rightDir = Vector3f.sub(rightPt, leftPt);
			float diameter = rightDir.normalize();
			diameter /= 2f;
			diameter += DiameterBuffer;
			if (diameter > extent) {
				extent = (int) Math.ceil(diameter);
			}
			wormDiameters.add(diameter);
			rightVectors.add(rightDir);

			final Vector3f upDir = Vector3f.cross(normal, rightDir);
			upDir.normalize();
			upVectors.add(upDir);
		}
		extent += 10;

		// 6. Once the sample planes are defined, the worm cross-section within each plane needs to be determined.
		// Without a model of the worm cross-section the sample planes will overlap in areas where the worm folds
		// back on top of itself. The first step in modeling the worm cross-section is to define an ellipse
		// within each sample plane, centered in the plane. The long axis of the ellipse is parallel to the
		// horizontal axis of the sample plane. The length is the distance between the left and right hand points.
		// The ellipse short axis is in the direction of the plane vertical axis; the length is set to 1/2 the length
		// of the ellipse long axis. This ellipse-based model approximates the overall shape of the worm, however
		// it cannot model how the worm shape changes where sections of the worm press against each other.
		// The next step of the algorithm attempts to solve this problem.
		sID = (short) (imageA.getVOIs().getUniqueID());
		displayContours = new VOI(sID, "wormContours");
		for (int i = 0; i < centerPositions.size(); i += 30) {
			final Vector3f rkEye = centerPositions.elementAt(i);
			final Vector3f rkRVector = rightVectors.elementAt(i);
			final Vector3f rkUVector = upVectors.elementAt(i);

			final float curve = centerSpline.GetCurvature(allTimes[i]);
			final float scale = curve;// (curve - minCurve)/(maxCurve - minCurve);
			// System.err.println( scale );
			final VOIContour ellipse = new VOIContour(true);
			ellipse.setVolumeDisplayRange(minRange);
			makeEllipse2D(rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse);
			displayContours.getCurves().add(ellipse);
		}

		sID = (short) (imageA.getVOIs().getUniqueID());
		centerLine = new VOI(sID, "center line");
		centerLine.getCurves().add(centerPositions);
		centerLine.setColor(Color.red);
		centerPositions.update(new ColorRGBA(1, 0, 0, 1));
		// centerPositions.setVolumeDisplayRange(minRange);

		sID++;
		leftLine = new VOI(sID, "left line");
		leftLine.getCurves().add(leftPositions);
		leftLine.setColor(Color.magenta);
		leftPositions.update(new ColorRGBA(1, 0, 1, 1));
		// leftPositions.setVolumeDisplayRange(minRange);

		sID++;
		rightLine = new VOI(sID, "right line");
		rightLine.getCurves().add(rightPositions);
		rightLine.setColor(Color.green);
		rightPositions.update(new ColorRGBA(0, 1, 0, 1));
		// rightPositions.setVolumeDisplayRange(minRange);

		imageA.registerVOI(leftLine);
		imageA.registerVOI(rightLine);
		imageA.registerVOI(centerLine);
	}

	/**
	 * Grows the outer edge of the worm model outward to capture more of the worm, using the segmented marker image as a
	 * guide.
	 * 
	 * @param model
	 * @param markers
	 * @param sliceIDs
	 * @param step
	 */
	private void growEdges(final ModelImage model, final ModelImage markers, final float[] sliceIDs, final int step) {
		final int dimX = model.getExtents().length > 0 ? model.getExtents()[0] : 1;
		final int dimY = model.getExtents().length > 1 ? model.getExtents()[1] : 1;
		final int dimZ = model.getExtents().length > 2 ? model.getExtents()[2] : 1;

		if (step == 0) {
			if (growContours != null) {
				growContours.dispose();
				growContours = null;
			}
			final short sID = (short) (imageA.getVOIs().getUniqueID());
			growContours = new VOI(sID, "growContours");

			for (int i = 0; i < centerPositions.size(); i++) {
				final int value = i + 1;

				final Vector3f rkEye = centerPositions.elementAt(i);
				final Vector3f rkRVector = rightVectors.elementAt(i);
				final Vector3f rkUVector = upVectors.elementAt(i);

				final float curve = centerSpline.GetCurvature(allTimes[i]);
				final float scale = curve;
				final VOIContour ellipse = new VOIContour(true);
				ellipse.setVolumeDisplayRange(minRange);
				makeEllipse2D(rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), scale, ellipse);

				interpolateContour(ellipse);
				for (int j = 0; j < ellipse.size(); j++) {
					final Vector3f start = new Vector3f(ellipse.elementAt(j));
					final Vector3f pt = ellipse.elementAt(j);
					Vector3f diff = Vector3f.sub(pt, centerPositions.elementAt(i));
					diff.normalize();

					pt.copy(centerPositions.elementAt(i));
					int x = Math.round(pt.X);
					int y = Math.round(pt.Y);
					int z = Math.round(pt.Z);
					float currentValue = model.getFloat(x, y, z);
					while ( ( (currentValue != 0) && Math.abs(currentValue - value) <= SampleLimit)) {
						pt.add(diff);
						x = Math.round(pt.X);
						y = Math.round(pt.Y);
						z = Math.round(pt.Z);
						if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) {
							break;
						}
						currentValue = model.getFloat(x, y, z);
					}
					if ( !pt.isEqual(centerPositions.elementAt(i))) {
						pt.sub(diff);
					}
					final float distStart = start.distance(centerPositions.elementAt(i));
					float distPt = pt.distance(centerPositions.elementAt(i));
					if (distStart > distPt) {
						x = Math.round(start.X);
						y = Math.round(start.Y);
						z = Math.round(start.Z);
						if ( ! ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ))) {
							currentValue = model.getFloat(x, y, z);
							if ( ( (currentValue != 0) && Math.abs(currentValue - value) <= SampleLimit)) {
								diff = Vector3f.sub(start, pt);
								diff.normalize();
								while ( !pt.isEqual(start) && (distPt < distStart)) {
									pt.add(diff);
									x = Math.round(pt.X);
									y = Math.round(pt.Y);
									z = Math.round(pt.Z);
									if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) {
										break;
									}
									model.set(x, y, z, currentValue);
									distPt = pt.distance(centerPositions.elementAt(i));
								}
							}
						}
					}
				}
				growContours.getCurves().add(ellipse);
			}
			// return;
		}

		for (int i = 0; i < centerPositions.size(); i++) {
			final int value = i + 1;
			final VOIContour ellipse = (VOIContour) growContours.getCurves().elementAt(i);
			interpolateContour(ellipse);
			for (int j = 0; j < ellipse.size(); j++) {
				final Vector3f pt = ellipse.elementAt(j);
				final Vector3f diff = Vector3f.sub(pt, centerPositions.elementAt(i));
				final float distance = diff.normalize();
				// diff.scale(0.5f);

				final float x = pt.X + diff.X;
				final float y = pt.Y + diff.Y;
				final float z = pt.Z + diff.Z;
				boolean extend = true;
				for (int z1 = Math.max(0, (int) Math.floor(z)); (z1 <= Math.min(dimZ - 1, Math.ceil(z))) && extend; z1++) {
					for (int y1 = Math.max(0, (int) Math.floor(y)); (y1 <= Math.min(dimY - 1, Math.ceil(y))) && extend; y1++) {
						for (int x1 = Math.max(0, (int) Math.floor(x)); (x1 <= Math.min(dimX - 1, Math.ceil(x))) && extend; x1++) {
							final float currentValue = model.getFloat(x1, y1, z1);
							if (currentValue != 0) {
								if (Math.abs(currentValue - value) > SampleLimit) {
									extend = false;
									break;
								}
							}
							if (markers != null) {
								final float markerValue = markers.getFloat(x1, y1, z1);
								if ( (markerValue != 0) && (markerValue != sliceIDs[i])) {
									extend = false;
									break;
								}
							}
						}
					}
				}
				if (extend) {
					for (int z1 = Math.max(0, (int) Math.floor(z)); (z1 <= Math.min(dimZ - 1, Math.ceil(z))) && extend; z1++) {
						for (int y1 = Math.max(0, (int) Math.floor(y)); (y1 <= Math.min(dimY - 1, Math.ceil(y))) && extend; y1++) {
							for (int x1 = Math.max(0, (int) Math.floor(x)); (x1 <= Math.min(dimX - 1, Math.ceil(x))) && extend; x1++) {
								final float currentValue = model.getFloat(x1, y1, z1);
								if (currentValue == 0) {
									model.set(x1, y1, z1, value);
								}
							}
						}
					}
					pt.add(diff);
				}
			}
		}
	}

	/**
	 * First pass generating the worm model from the simple ellipse-based model. Any voxels that fall inside overlapping
	 * ellipses are set as conflict voxels.
	 * 
	 * @param image
	 * @param model
	 * @param insideConflict
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param ellipseBound
	 * @param diameter
	 * @param boxBound
	 * @param value
	 */
	private void initializeModelandConflicts(final ModelImage image, final ModelImage model, final ModelImage insideConflict, final int tSlice,
			final int slice, final int[] extents, final Vector3f[] verts, final Ellipsoid3f ellipseBound, final float diameter, final Box3f boxBound,
			final float value) {
		final int iBound = extents[0];
		final int jBound = extents[1];

		final int[] dimExtents = image.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];
		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

		int buffFactor = 1;

		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
			buffFactor = 4;
		}

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		final Vector3f currentPoint = new Vector3f();

		final boolean[][] values = new boolean[iBound][jBound];
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				values[i][j] = false;
				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

					// do nothing
				} else {
					currentPoint.set(x, y, z);
					final boolean isInside = ellipseBound.Contains(currentPoint);
					if ( !isInside) {
						// do nothing
					} else {
						values[i][j] = true;
						for (int z1 = Math.max(0, (int) Math.floor(z)); z1 <= Math.min(dimExtents[2] - 1, Math.ceil(z)); z1++) {
							for (int y1 = Math.max(0, (int) Math.floor(y)); y1 <= Math.min(dimExtents[1] - 1, Math.ceil(y)); y1++) {
								for (int x1 = Math.max(0, (int) Math.floor(x)); x1 <= Math.min(dimExtents[0] - 1, Math.ceil(x)); x1++) {
									final float currentValue = model.getFloat(x1, y1, z1);
									if (currentValue != 0) {
										if (Math.abs(currentValue - value) < SampleLimit) {
											// model.set(x1, y1, z1, (currentValue + value)/2f);
										} else {
											insideConflict.set(x1, y1, z1, true);
										}
									} else {
										model.set(x1, y1, z1, value);
									}
								}
							}
						}
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}
	}

	/**
	 * Interpolates the input contour so that the spacing between contour points is <= 1 voxel.
	 * 
	 * @param contour
	 */
	private void interpolateContour(final VOIContour contour) {
		int index = 0;
		while (index < contour.size()) {
			final Vector3f p1 = contour.elementAt(index);
			final Vector3f p2 = contour.elementAt( (index + 1) % contour.size());
			// System.err.println( index + " " + (index+1)%contour.size() );
			final float distance = p1.distance(p2);
			if (distance > 1) {
				final Vector3f dir = Vector3f.sub(p2, p1);
				dir.normalize();
				final int count = (int) distance;
				final float stepSize = distance / (count + 1);
				float currentStep = stepSize;
				index++;
				for (int i = 0; i < count; i++) {
					final Vector3f newPt = new Vector3f();
					newPt.scaleAdd(currentStep, dir, p1);
					contour.add(index++, newPt);
					// System.err.println( "    adding pt at " + (index-1) + " " + newPt.distance(p1) + " " +
					// newPt.distance(p2) );
					currentStep += stepSize;
				}
			} else {
				index++;
			}
		}
		// System.err.println(contour.size());
		// for ( int i = 0; i < contour.size(); i++ )
		// {
		// System.err.println( contour.elementAt(i) + " " + contour.elementAt(i).distance(
		// contour.elementAt((i+1)%contour.size() ) ) );
		// }
	}

	/**
	 * Generates the 3D 1-voxel thick ellipsoids used in the intial worm model.
	 * 
	 * @param right
	 * @param up
	 * @param center
	 * @param diameterA
	 * @param scale
	 * @param ellipse
	 * @return
	 */
	private Ellipsoid3f makeEllipse(final Vector3f right, final Vector3f up, final Vector3f center, final float diameterA, final float scale,
			final VOIContour ellipse) {
		final int numPts = 32;
		final double[] adCos = new double[32];
		final double[] adSin = new double[32];
		for (int i = 0; i < numPts; i++) {
			adCos[i] = Math.cos(Math.PI * 2.0 * i / numPts);
			adSin[i] = Math.sin(Math.PI * 2.0 * i / numPts);
		}
		final float diameterB = diameterA / 2f;// + (1-scale) * diameterA/4f;
		for (int i = 0; i < numPts; i++) {
			final Vector3f pos1 = Vector3f.scale((float) (diameterA * adCos[i]), right);
			final Vector3f pos2 = Vector3f.scale((float) (diameterB * adSin[i]), up);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
		}
		final float[] extents = new float[] {diameterA, diameterB, 1};
		final Vector3f[] axes = new Vector3f[] {right, up, Vector3f.cross(right, up)};
		return new Ellipsoid3f(center, axes, extents);
	}

	/**
	 * Generates the simple 2D VOI contour ellipse for the worm model.
	 * 
	 * @param right
	 * @param up
	 * @param center
	 * @param diameterA
	 * @param scale
	 * @param ellipse
	 */
	private void makeEllipse2D(final Vector3f right, final Vector3f up, final Vector3f center, final float diameterA, final float scale,
			final VOIContour ellipse) {
		final int numPts = 32;
		final float diameterB = diameterA / 2f;// + (1-scale) * diameterA/4f;
		for (int i = 0; i < numPts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numPts);
			final double s = Math.sin(Math.PI * 2.0 * i / numPts);
			final Vector3f pos1 = Vector3f.scale((float) (diameterA * c), right);
			final Vector3f pos2 = Vector3f.scale((float) (diameterB * s), up);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
		}
	}

	/**
	 * Generates the VOI that highlights which point (lattice or annotation) is currently selected by the user.
	 * 
	 * @param right
	 * @param up
	 * @param center
	 * @param diameter
	 * @param ellipse
	 */
	private void makeSelectionFrame(final Vector3f right, final Vector3f up, final Vector3f center, final float diameter, final VOIContour ellipse) {
		final int numPts = 12;
		for (int i = 0; i < numPts; i++) {
			final double c = Math.cos(Math.PI * 2.0 * i / numPts);
			final double s = Math.sin(Math.PI * 2.0 * i / numPts);
			final Vector3f pos1 = Vector3f.scale((float) (diameter * c), right);
			final Vector3f pos2 = Vector3f.scale((float) (diameter * s), up);
			final Vector3f pos = Vector3f.add(pos1, pos2);
			pos.add(center);
			ellipse.addElement(pos);
		}
	}

	/**
	 * Writes out slice IDs that match the segmented marker IDs, set the value for every slice in the straightened image
	 * to the corresponding marker ID for the marker that intersects that slice. Only markers that are inside the
	 * current worm model are included -- even when multiple markers intersect the same slice. the markers that are
	 * inside the worm model are included.
	 * 
	 * @param model
	 * @param markerSegmentation
	 * @param sliceIDs
	 * @param markerIDs
	 * @param completedIDs
	 * @param currentID
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param ellipseBound
	 */
	private void mapSliceIDstoMarkerIDs(final ModelImage model, final ModelImage markerSegmentation, final float[] sliceIDs, final int[] markerIDs,
			final boolean[] completedIDs, final int[] currentID, final int tSlice, final int slice, final int[] extents, final Vector3f[] verts,
			final Ellipsoid3f ellipseBound) {
		final int iBound = extents[0];
		final int jBound = extents[1];

		final int[] dimExtents = markerSegmentation.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];
		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

		final int buffFactor = 1;

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		final Vector3f currentPoint = new Vector3f();

		final float[] values = new float[iBound * jBound];
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				values[j * iBound + i] = 0;
				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > markerSegmentation.getSize()))) {

					// do nothing
				} else {
					currentPoint.set(x, y, z);
					final boolean isInside = ellipseBound.Contains(currentPoint);
					final float currentValue = model.getFloat(iIndex, jIndex, kIndex);
					// float currentValue = model.getFloatTriLinearBounds(x, y, z);
					if (isInside && (currentValue != 0)) {
						values[j * iBound + i] = markerSegmentation.getFloat(iIndex, jIndex, kIndex);
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}

		float markerPt = 0;

		boolean inconsistent = false;
		for (int j = 0; j < values.length && !inconsistent; j++) {
			if (values[j] > 0) {
				for (int k = j + 1; k < values.length && !inconsistent; k++) {
					if ( (values[k] != 0) && (values[j] != values[k])) {
						inconsistent = true;
						break;
					}
				}
				markerPt = values[j];
			}
		}
		if (inconsistent || (markerPt == 0)) {
			return;
		}

		if ( (markerIDs[currentID[0]] < markerPt) && completedIDs[currentID[0]]) {
			for (int i = currentID[0] + 1; i < markerIDs.length; i++) {
				if (markerIDs[i] == 0) {
					continue;
				} else {
					currentID[0] = i;
					break;
				}
			}
		}

		if (markerIDs[currentID[0]] != markerPt) {
			return;
		}
		completedIDs[currentID[0]] = true;

		sliceIDs[slice] = markerPt;
		// System.err.println( slice + " " + markerPt + " " + markerIDs[ currentID[0] ] );
	}

	/**
	 * Converts the marker segmentation image into a color image where each marker is colored based on the corresponding
	 * lattice ID
	 * 
	 * @param image
	 * @param displayResult
	 */
	private void markerImageToColor(final ModelImage image, final boolean displayResult) {

		String imageName = imageA.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		ModelImage colorImage = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), imageName + "_color_markers.xml");
		JDialogBase.updateFileInfo(imageA, colorImage);
		final float numColors = (float) image.getMax();
		// for ( int i = 0; i < numColors; i++ )
		// {
		// Color color = new Color( Color.HSBtoRGB( i / numColors, 1, 1) );
		// System.err.println( i + " " + (i / numColors) + " " + color );
		// }

		final int dimX = colorImage.getExtents().length > 0 ? colorImage.getExtents()[0] : 1;
		final int dimY = colorImage.getExtents().length > 1 ? colorImage.getExtents()[1] : 1;
		final int dimZ = colorImage.getExtents().length > 2 ? colorImage.getExtents()[2] : 1;
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					final float value = image.getFloat(x, y, z);
					if (value > 0) {
						final Color color = new Color(Color.HSBtoRGB(value / numColors, 1, 1));
						colorImage.setC(x, y, z, 0, 1);
						colorImage.setC(x, y, z, 1, color.getRed() / 255f);
						colorImage.setC(x, y, z, 2, color.getGreen() / 255f);
						colorImage.setC(x, y, z, 3, color.getBlue() / 255f);
					}
				}
			}
		}

		saveImage(imageName, colorImage, false);
		if (displayResult) {
			image.calcMinMax();
			new ViewJFrameImage((ModelImage) image.clone());
			colorImage.calcMinMax();
			new ViewJFrameImage((ModelImage) colorImage.clone());
		}
		colorImage.disposeLocal();
		colorImage = null;
	}

	/**
	 * Moves the lattice point to better match the marker segementation results.
	 * 
	 * @param markerImage
	 * @param pt
	 * @param dir
	 * @param id
	 */
	private void moveMarker(final ModelImage markerImage, final Vector3f pt, final Vector3f dir, final int id) {
		final int dimX = markerImage.getExtents().length > 0 ? markerImage.getExtents()[0] : 1;
		final int dimY = markerImage.getExtents().length > 1 ? markerImage.getExtents()[1] : 1;
		final int dimZ = markerImage.getExtents().length > 2 ? markerImage.getExtents()[2] : 1;

		dir.normalize();
		final Vector3f temp = new Vector3f(pt);
		temp.add(dir);
		int x = Math.round(temp.X);
		int y = Math.round(temp.Y);
		int z = Math.round(temp.Z);
		if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) {
			return;
		}
		float value = markerImage.getFloat(x, y, z);
		while (value == id) {
			pt.copy(temp);
			temp.add(dir);
			x = Math.round(temp.X);
			y = Math.round(temp.Y);
			z = Math.round(temp.Z);
			if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) {
				return;
			}
			value = markerImage.getFloat(x, y, z);
		}
	}

	/**
	 * Given a point in the twisted volume, calculates and returns the corresponding point in the straightened image.
	 * 
	 * @param model
	 * @param originToStraight
	 * @param pt
	 * @param text
	 * @return
	 */
	private Vector3f originToStraight(final ModelImage model, final ModelImage originToStraight, final Vector3f pt, final String text) {
		final int x = Math.round(pt.X);
		final int y = Math.round(pt.Y);
		final int z = Math.round(pt.Z);

		final float outputA = originToStraight.getFloatC(x, y, z, 0);
		final float outputX = originToStraight.getFloatC(x, y, z, 1);
		final float outputY = originToStraight.getFloatC(x, y, z, 2);
		final float outputZ = originToStraight.getFloatC(x, y, z, 3);

		if (outputA == 0) {
			final float m = model.getFloat(x, y, z);
			if (m != 0) {
				final int dimX = model.getExtents().length > 0 ? model.getExtents()[0] : 1;
				final int dimY = model.getExtents().length > 1 ? model.getExtents()[1] : 1;
				final int dimZ = model.getExtents().length > 2 ? model.getExtents()[2] : 1;

				int count = 0;
				final Vector3f pts = new Vector3f();
				for (int z1 = Math.max(0, z - 2); z1 < Math.min(dimZ, z + 2); z1++) {
					for (int y1 = Math.max(0, y - 2); y1 < Math.min(dimY, y + 2); y1++) {
						for (int x1 = Math.max(0, x - 2); x1 < Math.min(dimX, x + 2); x1++) {
							final float a1 = originToStraight.getFloatC(x1, y1, z1, 0);
							final float m1 = model.getFloat(x1, y1, z1);
							if ( (a1 != 0) && (m1 == m)) {
								final float x2 = originToStraight.getFloatC(x1, y1, z1, 1);
								final float y2 = originToStraight.getFloatC(x1, y1, z1, 2);
								final float z2 = originToStraight.getFloatC(x1, y1, z1, 3);
								pts.add(x2, y2, z2);
								count++;
							}
						}
					}
				}
				if (count != 0) {
					// System.err.println( imageA.getImageName() + " originToStraight " + text + " " + pt + " OK ");
					pts.scale(1f / count);
					return pts;
				}
			} else {
				System.err.println(imageA.getImageName() + " originToStraight " + text + " " + pt);
			}
		}

		return new Vector3f(outputX, outputY, outputZ);
	}

	/**
	 * Resolves conflict voxels (where possible) in the worm model using the updated marker segmentation image.
	 * 
	 * @param model
	 * @param markerSegmentation
	 * @param sliceIDs
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param value
	 */
	private void resolveModelConflicts(final ModelImage model, final ModelImage markerSegmentation, final float[] sliceIDs, final int tSlice, final int slice,
			final int[] extents, final Vector3f[] verts, final float value) {
		final int iBound = extents[0];
		final int jBound = extents[1];

		final int[] dimExtents = markerSegmentation.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];
		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

		final int buffFactor = 1;

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		final Vector3f currentPoint = new Vector3f();
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > markerSegmentation.getSize()))) {

					// do nothing
				} else {
					currentPoint.set(x, y, z);
					final float currentValue = model.getFloat(iIndex, jIndex, kIndex);
					final float markerValue = markerSegmentation.getFloat(iIndex, jIndex, kIndex);
					if ( (currentValue == 0) && (markerValue == sliceIDs[slice])) {
						model.set(iIndex, jIndex, kIndex, value);
					}
				}

				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}
	}

	/**
	 * Saves the annotation statistics to a file.
	 * 
	 * @param image
	 * @param model
	 * @param originToStraight
	 * @param outputDim
	 * @param postFix
	 * @return
	 */
	private VOI saveAnnotationStatistics(final ModelImage image, final ModelImage model, final ModelImage originToStraight, final int[] outputDim,
			final String postFix) {
		if (annotationVOIs == null) {
			return null;
		}
		if (annotationVOIs.getCurves().size() == 0) {
			return null;
		}

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "statistics" + File.separator;
		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir
			// does
			// not
			// exist
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + "AnnotationInfo" + postFix + ".csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + "AnnotationInfo" + postFix + ".csv");
		}

		VOI transformedAnnotations = null;
		try {
			if (originToStraight != null) {
				transformedAnnotations = new VOI(annotationVOIs);
			}

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("name" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "x_um" + "," + "y_um" + "," + "z_um" + "\n");
			for (int i = 0; i < annotationVOIs.getCurves().size(); i++) {
				final VOIText text = (VOIText) annotationVOIs.getCurves().elementAt(i);
				Vector3f position = text.elementAt(0);
				if ( (model != null) && (originToStraight != null)) {
					position = originToStraight(model, originToStraight, position, text.getText());

					transformedAnnotations.getCurves().elementAt(i).elementAt(0).copy(position);
					transformedAnnotations.getCurves().elementAt(i).elementAt(1).set(position.X + 5, position.Y, position.Z);
				}
				bw.write(text.getText() + "," + (position.X - transformedOrigin.X) + "," + (position.Y - transformedOrigin.Y) + ","
						+ (position.Z - transformedOrigin.Z) + "," +

                        VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + VOILatticeManagerInterface.VoxelSize
                        * (position.Y - transformedOrigin.Y) + "," + VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + "\n");
			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
			e.printStackTrace();
		}

		return transformedAnnotations;
	}

	/**
	 * Saves the image to the output_images directory.
	 * 
	 * @param imageName
	 * @param image
	 * @param saveAsTif
	 */
	private void saveImage(final String imageName, final ModelImage image, final boolean saveAsTif) {
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "output_images" + File.separator;
		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir
			// does
			// not
			// exist
			voiFileDir.mkdir();
		}

		final File file = new File(voiDir + imageName);
		if (file.exists()) {
			file.delete();
		}
		// System.err.println( voiDir );
		// System.err.println( image.getImageName() + ".xml" );
		ModelImage.saveImage(image, image.getImageName() + ".xml", voiDir, false);
		if (saveAsTif) {
			ModelImage.saveImage(image, image.getImageName() + ".tif", voiDir, false);
		}
	}

	/**
	 * Saves the lattice positions to a file.
	 * 
	 * @param image
	 * @param model
	 * @param originToStraight
	 * @param left
	 * @param right
	 * @param volumes
	 * @param postFix
	 */
	private void saveLatticePositions(final ModelImage image, final ModelImage model, final ModelImage originToStraight, final VOIContour left,
			final VOIContour right, final int[][] volumes, final String postFix) {
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "statistics" + File.separator;
		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir
			// does
			// not
			// exist
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + "LatticePositions" + postFix + ".csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + "LatticePositions" + postFix + ".csv");
		}

		try {
			final float cubicVolume = VOILatticeManagerInterface.VoxelSize * VOILatticeManagerInterface.VoxelSize * VOILatticeManagerInterface.VoxelSize;


			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("name" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "x_um" + "," + "y_um" + "," + "z_um" + "," + "volume_voxel" + ","
					+ "volume_um" + "\n");
			for (int i = 0; i < left.size(); i++) {
				Vector3f position = left.elementAt(i);
				// if ( (model != null) && (originToStraight != null) )
				// {
				// position = originToStraight( model, originToStraight, position, "left"+i);
				// }
				bw.write("L" + i + "," + (position.X - transformedOrigin.X) + "," + (position.Y - transformedOrigin.Y) + ","
						+ (position.Z - transformedOrigin.Z) + "," +

                        VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + VOILatticeManagerInterface.VoxelSize
                        * (position.Y - transformedOrigin.Y) + "," + VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + ","
                        + volumes[i][0] + "," + cubicVolume * volumes[i][0] + "\n");

				position = right.elementAt(i);
				// if ( originToStraight != null )
				// {
				// position = originToStraight( model, originToStraight, position, "right"+i);
				// }
				bw.write("R" + i + "," + (position.X - transformedOrigin.X) + "," + (position.Y - transformedOrigin.Y) + ","
						+ (position.Z - transformedOrigin.Z) + "," +

                        VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + VOILatticeManagerInterface.VoxelSize
                        * (position.Y - transformedOrigin.Y) + "," + VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + ","
                        + volumes[i][1] + "," + cubicVolume * volumes[i][1] + "\n");
			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
			e.printStackTrace();
		}
	}

	/**
	 * Saves the lattice statistics to a file.
	 * 
	 * @param image
	 * @param length
	 * @param left
	 * @param right
	 * @param leftPairs
	 * @param rightPairs
	 * @param postFix
	 */
	private void saveLatticeStatistics(final ModelImage image, final float length, final VOIContour left, final VOIContour right, final float[] leftPairs,
			final float[] rightPairs, final String postFix) {
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "statistics" + File.separator;
		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
			// String[] list = voiFileDir.list();
			// for ( int i = 0; i < list.length; i++ )
			// {
			// File lrFile = new File( voiDir + list[i] );
			// lrFile.delete();
			// }
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + "LatticeInfo" + postFix + ".csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + "LatticeInfo" + postFix + ".csv");
		}

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("Total Length:," + VOILatticeManagerInterface.VoxelSize * length + "\n");
			bw.newLine();
			bw.write("pair" + "," + "diameter" + "," + "left distance" + "," + "right distance" + "\n");
			for (int i = 0; i < left.size(); i++) {
				bw.write(i + "," + VOILatticeManagerInterface.VoxelSize * left.elementAt(i).distance(right.elementAt(i)) + ","
						+ VOILatticeManagerInterface.VoxelSize * leftPairs[i] + "," + VOILatticeManagerInterface.VoxelSize * rightPairs[i] + "\n");
			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
			e.printStackTrace();
		}
	}

	/**
	 * Produces an image segmentation of the left-right fluorescent markers using the lattice points as a guide. The
	 * fluorescent markers in the worm volume are automatically segmented, using the positions of the lattice as a
	 * guide. During segmentation the voxels that fall within the segmented regions are labeled with a corresponding
	 * marker ID. In the event that not all 10 markers are segmented, which may occur if the input lattice points are
	 * placed just outside the fluorescent marker boundaries, the lattice points will be modified to be inside the
	 * segmented boundaries.
	 * 
	 * @param image
	 * @param left
	 * @param right
	 * @param markerIDs
	 * @param markerVolumes
	 * @param segAll
	 * @return
	 */
	private ModelImage segmentMarkers(final ModelImage image, final VOIContour left, final VOIContour right, final int[] markerIDs,
			final int[][] markerVolumes, final boolean segAll) {
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		final ModelImage markerSegmentation = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName + "_markers.xml");
		JDialogBase.updateFileInfo(image, markerSegmentation);

		// Generate the gradient magnitude image:
		ModelImage gmImage = VolumeImage.getGradientMagnitude(image, 0);

		// determine the maxiumum image value of the fluorescent markers at each lattice point:
		float maxValue = -Float.MAX_VALUE;
		for (int i = 0; i < left.size(); i++) {
			Vector3f temp = right.elementAt(i);
			int x = Math.round(temp.X);
			int y = Math.round(temp.Y);
			int z = Math.round(temp.Z);
			float value;
			if (image.isColorImage()) {
				value = image.getFloatC(x, y, z, 2); // Green Channel contains markers
			} else {
				value = image.getFloat(x, y, z);
			}
			if (value > maxValue) {
				maxValue = value;
			}
			temp = left.elementAt(i);
			x = Math.round(temp.X);
			y = Math.round(temp.Y);
			z = Math.round(temp.Z);
			if (image.isColorImage()) {
				value = image.getFloatC(x, y, z, 2); // Green Channel contains markers
			} else {
				value = image.getFloat(x, y, z);
			}
			if (value > maxValue) {
				maxValue = value;
			}
		}

		// Find the minimum distance between all lattice points (left, right):
		float minOverall = Float.MAX_VALUE;
		for (int i = 0; i < left.size(); i++) {
			final Vector3f tempL = left.elementAt(i);
			for (int j = i + 1; j < left.size(); j++) {
				final float dist = tempL.distance(left.elementAt(j));
				if (dist < minOverall) {
					minOverall = dist;
				}
			}
			for (int j = 0; j < right.size(); j++) {
				final float dist = tempL.distance(right.elementAt(j));
				if (dist < minOverall) {
					minOverall = dist;
				}
			}
		}
		for (int i = 0; i < right.size(); i++) {
			final Vector3f tempR = right.elementAt(i);
			for (int j = i + 1; j < right.size(); j++) {
				final float dist = tempR.distance(right.elementAt(j));
				if (dist < minOverall) {
					minOverall = dist;
				}
			}
			for (int j = 0; j < left.size(); j++) {
				final float dist = tempR.distance(left.elementAt(j));
				if (dist < minOverall) {
					minOverall = dist;
				}
			}
		}

		minOverall *= 0.75;
		final int step = (int) Math.max(1, (minOverall / 3f));

		final Vector<Vector3f> seedList = new Vector<Vector3f>();
		final VOIContour[][] savedSeedList = new VOIContour[left.size()][2];
		final int[][] counts = new int[left.size()][2];
		for (int diameter = step; diameter <= minOverall; diameter += step) {
			for (int i = 0; i < left.size(); i++) {
				// int diameter = (int) (minDistance[i][0]/2f);
				if (savedSeedList[i][0] == null) {
					savedSeedList[i][0] = new VOIContour(false);
					seedList.clear();
					seedList.add(new Vector3f(left.elementAt(i)));
					counts[i][0] = fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, new Vector3f(left.elementAt(i)), seedList,
							savedSeedList[i][0], diameter, i + 1);
					// System.err.println( "left " + i + " " + counts[i][0] );
				} else {
					seedList.clear();
					counts[i][0] += fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, new Vector3f(left.elementAt(i)), savedSeedList[i][0],
							seedList, diameter, i + 1);
					savedSeedList[i][0].clear();
					savedSeedList[i][0].addAll(seedList);
					// System.err.println( "left " + i + " " + counts[i][0] );
				}
			}
			for (int i = 0; i < right.size(); i++) {
				// int diameter = (int) (minDistance[i][1]/2f);
				if (savedSeedList[i][1] == null) {
					savedSeedList[i][1] = new VOIContour(false);
					seedList.clear();
					seedList.add(new Vector3f(right.elementAt(i)));
					counts[i][1] = fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, new Vector3f(right.elementAt(i)), seedList,
							savedSeedList[i][0], diameter, i + 1);
					// System.err.println( "right " + i + " " + counts[i][1] );
				} else {
					seedList.clear();
					counts[i][1] += fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, new Vector3f(right.elementAt(i)), savedSeedList[i][0],
							seedList, diameter, i + 1);
					savedSeedList[i][1].clear();
					savedSeedList[i][1].addAll(seedList);
					// System.err.println( "right " + i + " " + counts[i][1] );
				}
			}
		}
		for (int i = 0; i < left.size(); i++) {
			markerVolumes[i][0] = counts[i][0];
			markerVolumes[i][1] = counts[i][1];
		}

		for (int i = 0; i < left.size(); i++) {
			if (counts[i][0] == 0) {
				// markerIDs[i] = 0;
				final Vector3f dir = Vector3f.sub(right.elementAt(i), left.elementAt(i));
				dir.normalize();
				dir.scale(step);
				seedList.clear();
				final Vector3f newPt = Vector3f.add(left.elementAt(i), dir);
				seedList.add(newPt);
				savedSeedList[i][0].clear();
				counts[i][0] = fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, newPt, seedList, savedSeedList[i][0], (int) minOverall, i + 1);
				if (counts[i][0] == 0) {
					seedList.clear();
					seedList.add(new Vector3f(left.elementAt(i)));
					savedSeedList[i][0].clear();
					counts[i][0] = fill(image, gmImage, markerSegmentation, 0, 0, new Vector3f(left.elementAt(i)), seedList, savedSeedList[i][0], step, i + 1);
				}
				markerIDs[i] = i + 1;
				moveMarker(markerSegmentation, left.elementAt(i), Vector3f.sub(left.elementAt(i), right.elementAt(i)), i + 1);
			} else {
				markerIDs[i] = i + 1;
				moveMarker(markerSegmentation, left.elementAt(i), Vector3f.sub(left.elementAt(i), right.elementAt(i)), i + 1);
			}
		}
		for (int i = 0; i < right.size(); i++) {
			if (counts[i][1] == 0) {
				// markerIDs[i] = 0;
				final Vector3f dir = Vector3f.sub(left.elementAt(i), right.elementAt(i));
				dir.normalize();
				dir.scale(step);
				seedList.clear();
				final Vector3f newPt = Vector3f.add(right.elementAt(i), dir);
				seedList.add(newPt);
				savedSeedList[i][1].clear();
				counts[i][1] = fill(image, gmImage, markerSegmentation, 10, 0.1f * maxValue, newPt, seedList, savedSeedList[i][0], (int) minOverall, i + 1);
				if (counts[i][1] == 0) {
					seedList.clear();
					seedList.add(new Vector3f(right.elementAt(i)));
					savedSeedList[i][1].clear();
					counts[i][1] = fill(image, gmImage, markerSegmentation, 0, 0, new Vector3f(right.elementAt(i)), seedList, savedSeedList[i][1], step, i + 1);
					markerIDs[i] = i + 1;
					moveMarker(markerSegmentation, right.elementAt(i), Vector3f.sub(right.elementAt(i), left.elementAt(i)), i + 1);
				}
			} else {
				markerIDs[i] = i + 1;
				moveMarker(markerSegmentation, right.elementAt(i), Vector3f.sub(right.elementAt(i), left.elementAt(i)), i + 1);
			}
		}
		markerSegmentation.calcMinMax();
		// new ViewJFrameImage((ModelImage)markerSegmentation.clone());

		if (segAll) {
			final ModelImage markerSegmentation2 = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName + "_markers2.xml");
			JDialogBase.updateFileInfo(image, markerSegmentation2);

			seedList.clear();
			for (int i = 0; i < left.size(); i++) {
				seedList.add(new Vector3f(left.elementAt(i)));
				seedList.add(new Vector3f(right.elementAt(i)));

				if (markerVolumes[i][0] == 0) {
					final Vector3f dir = Vector3f.sub(right.elementAt(i), left.elementAt(i));
					dir.normalize();
					dir.scale(step);
					final Vector3f newPt = Vector3f.add(left.elementAt(i), dir);
					seedList.add(newPt);
				}
				if (markerVolumes[i][1] == 0) {
					final Vector3f dir = Vector3f.sub(left.elementAt(i), right.elementAt(i));
					dir.normalize();
					dir.scale(step);
					final Vector3f newPt = Vector3f.add(right.elementAt(i), dir);
					seedList.add(newPt);
				}
			}

			savedSeedList[0][0].clear();
			final int count = fill(image, gmImage, markerSegmentation2, 10, 0.1f * maxValue, Vector3f.ZERO, seedList, savedSeedList[0][0], Integer.MAX_VALUE,
					(int) (.75 * (image.getMax() - image.getMin())));

			System.err.println("segment all " + count);
			markerSegmentation2.calcMinMax();
			new ViewJFrameImage((ModelImage) markerSegmentation2.clone());
		}

		gmImage.disposeLocal();
		gmImage = null;

		return markerSegmentation;
	}

	/**
	 * Generates the Natural Spline for the lattice center-line curve. Sets the time values for each point on the curve.
	 * 
	 * @param curve
	 * @param time
	 * @return
	 */
	private NaturalSpline3 smoothCurve(final VOIContour curve, final float[] time) {
		float totalDistance = 0;
		for (int i = 0; i < curve.size() - 1; i++) {
			totalDistance += curve.elementAt(i).distance(curve.elementAt(i + 1));
		}

		final Vector3f[] akPoints = new Vector3f[curve.size()];
		float distance = 0;
		for (int i = 0; i < curve.size(); i++) {
			if (i > 0) {
				distance += curve.elementAt(i).distance(curve.elementAt(i - 1));
				time[i] = distance / totalDistance;
				akPoints[i] = new Vector3f(curve.elementAt(i));
			} else {
				time[i] = 0;
				akPoints[i] = new Vector3f(curve.elementAt(i));
			}
		}

		return new NaturalSpline3(NaturalSpline3.BoundaryType.BT_FREE, curve.size() - 1, time, akPoints);
	}

	/**
	 * Generates the Natural Spline curves for the left and right curves for the lattice. The time points are passed to
	 * the spline and correspond to the time points along the center-line curve so that all three curves match in
	 * time-space.
	 * 
	 * @param curve
	 * @param time
	 * @return
	 */
	private NaturalSpline3 smoothCurve2(final VOIContour curve, final float[] time) {
		final Vector3f[] akPoints = new Vector3f[curve.size()];
		for (int i = 0; i < curve.size(); i++) {
			akPoints[i] = new Vector3f(curve.elementAt(i));
		}

		return new NaturalSpline3(NaturalSpline3.BoundaryType.BT_FREE, curve.size() - 1, time, akPoints);
	}

	/**
	 * Once the 3D model of the worm is finalized, a 2D slice plane is swept through the model. At each sample-point
	 * along the 3D center spline of the worm, the 2D plane is intersected with the original 3D volume. Voxels that fall
	 * outside the updated 2D worm contour are set to the image minimum value (typically = 0). Voxels that fall inside
	 * the 2D worm contour are copied into the output slice. The set of 2D slices from the worm head to tail are
	 * concatenated to form the final 3D straightened volume.
	 * 
	 * During the straightening step as well as during the model-building process or anytime the 2D sample plane is
	 * intersected with the 3D volume steps are taken by the algorithm to minimize sampling artifacts. Due to the
	 * twisted configuration of the worm, sampling the volume along the outer-edge of a curve will result under-sampling
	 * the data while the inside edge of the curve will be over-sampled.
	 * 
	 * To reduce sampling artifacts the sample planes are interpolated between sample points, using the maximum distance
	 * between points along consecutive contours to determine the amount of super-sampling. The multiple sample planes
	 * are averaged to produce the final slice in the straightened image. In addition, each contour is modeled as having
	 * the thickness of one voxel and sample points that fall between voxels in the volume are trilinearly interpolated.
	 * 
	 * @param image
	 * @param resultExtents
	 * @param baseName
	 * @param model
	 * @param saveStats
	 * @param displayResult
	 * @param saveAsTif
	 */
	private void straighten(final ModelImage image, final int[] resultExtents, final String baseName, final ModelImage model, final boolean saveStats,
			final boolean displayResult, final boolean saveAsTif) {
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		final int colorFactor = image.isColorImage() ? 4 : 1;
		final float[] values = new float[resultExtents[0] * resultExtents[1] * colorFactor];

		float[] dataOrigin = null;
		float[] sampleDistance = null;
		float[] sampleDistanceP = null;

		ModelImage resultImage = new ModelImage(image.getType(), resultExtents, imageName + "_straight.xml");
		JDialogBase.updateFileInfo(image, resultImage);
		resultImage.setResolutions(new float[] {1, 1, 1});

		ModelImage straightToOrigin = null;
		ModelImage originToStraight = null;
		ModelImage overlap2 = null;

		if (saveStats) {
			dataOrigin = new float[resultExtents[0] * resultExtents[1] * 4];
			straightToOrigin = new ModelImage(ModelStorageBase.ARGB_FLOAT, resultExtents, imageName + "_toOriginal.xml");
			JDialogBase.updateFileInfo(image, straightToOrigin);
			straightToOrigin.setResolutions(new float[] {1, 1, 1});
			for (int i = 0; i < straightToOrigin.getDataSize(); i++) {
				straightToOrigin.set(i, 0);
			}

			originToStraight = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), imageName + "_toStraight.xml");
			JDialogBase.updateFileInfo(image, originToStraight);
			for (int i = 0; i < originToStraight.getDataSize(); i++) {
				originToStraight.set(i, 0);
			}

			overlap2 = new ModelImage(ModelStorageBase.FLOAT, resultExtents, imageName + "_sampleDensity.xml");
			JDialogBase.updateFileInfo(image, overlap2);
			overlap2.setResolutions(new float[] {1, 1, 1});

			sampleDistance = new float[resultExtents[0] * resultExtents[1]];
			sampleDistanceP = new float[resultExtents[0] * resultExtents[1] * 4];
			final int length = resultExtents[0] * resultExtents[1];
			for (int j = 0; j < length; j++) {
				sampleDistance[j] = 0;
				for (int c = 0; c < 4; c++) {
					// sampleDistance[j * 4 + c] = 0;
					sampleDistanceP[j * 4 + c] = 0;
				}
			}
		}

		for (int i = 0; i < samplingPlanes.getCurves().size(); i++) {
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			final Vector3f[] corners = new Vector3f[4];
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}
			float planeDist = -Float.MAX_VALUE;
			if (i < (samplingPlanes.getCurves().size() - 1)) {
				kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
				for (int j = 0; j < 4; j++) {
					final float distance = corners[j].distance(kBox.elementAt(j));
					if (distance > planeDist) {
						planeDist = distance;
					}
					// System.err.println( distance + "  " + centerPositions.elementAt(i).distance(
					// centerPositions.elementAt(i+1) ) );
				}
				// System.err.println("");
			}
			try {
				for (int j = 0; j < values.length / colorFactor; j++) {
					if (colorFactor == 4) {
						values[ (j * 4) + 0] = (float) image.getMinA();
						values[ (j * 4) + 1] = (float) image.getMinR();
						values[ (j * 4) + 2] = (float) image.getMinG();
						values[ (j * 4) + 3] = (float) image.getMinB();
					}
					/* not color: */
					else {
						values[j] = (float) image.getMin();
					}
					if (dataOrigin != null) {
						for (int c = 0; c < 4; c++) {
							dataOrigin[j * 4 + c] = 0;
						}
					}
				}

				int planeCount = 0;
				if (i < (samplingPlanes.getCurves().size() - 1)) {
					planeDist *= 3;
					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i + 1);
					final Vector3f[] steps = new Vector3f[4];
					final Vector3f[] cornersSub = new Vector3f[4];
					for (int j = 0; j < 4; j++) {
						steps[j] = Vector3f.sub(kBox.elementAt(j), corners[j]);
						steps[j].scale(1f / planeDist);
						cornersSub[j] = new Vector3f(corners[j]);
					}
					for (int j = 0; j < planeDist; j++) {
						writeDiagonal(image, model, originToStraight, 0, i, resultExtents, cornersSub, values, dataOrigin, sampleDistance, sampleDistanceP);
						planeCount++;
						for (int k = 0; k < 4; k++) {
							cornersSub[k].add(steps[k]);
						}
					}
				} else {
					planeDist = 15;
					kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i - 1);
					final Vector3f[] steps = new Vector3f[4];
					final Vector3f[] cornersSub = new Vector3f[4];
					for (int j = 0; j < 4; j++) {
						steps[j] = Vector3f.sub(corners[j], kBox.elementAt(j));
						steps[j].scale(1f / planeDist);
						// cornersSub[j] = Vector3f.add( corners[j], kBox.elementAt(j) ); cornersSub[j].scale(0.5f);
						cornersSub[j] = new Vector3f(corners[j]);
					}
					for (int j = 0; j < planeDist; j++) {
						writeDiagonal(image, model, originToStraight, 0, i, resultExtents, cornersSub, values, dataOrigin, sampleDistance, sampleDistanceP);
						planeCount++;
						for (int k = 0; k < 4; k++) {
							cornersSub[k].add(steps[k]);
						}
					}
					// writeDiagonal( image, model, originToStraight, 0, i, resultExtents, corners, values, dataOrigin);
				}
				for (int j = 0; j < values.length / colorFactor; j++) {
					if (colorFactor == 4) {
						values[ (j * 4) + 1] /= planeCount;
						values[ (j * 4) + 2] /= planeCount;
						values[ (j * 4) + 3] /= planeCount;
					}
					/* not color: */
					else {
						values[j] /= planeCount;
					}
					if (dataOrigin != null) {
						for (int c = 1; c < 4; c++) {
							dataOrigin[j * 4 + c] /= planeCount;
						}
					}
				}

				resultImage.importData(i * values.length, values, false);
				if (straightToOrigin != null) {
					straightToOrigin.importData(i * dataOrigin.length, dataOrigin, false);
				}
				if (overlap2 != null) {
					overlap2.importData(i * sampleDistance.length, sampleDistance, false);
				}

			} catch (final IOException e) {
				e.printStackTrace();
			}
		}

		VOI transformedAnnotations = null;
		if (saveStats && (straightToOrigin != null)) {
			testOriginToStraight(model, originToStraight);
			saveImage(baseName, overlap2, true);

			saveImage(baseName, straightToOrigin, false);
			saveImage(baseName, originToStraight, false);
			
			// calculate the transformed origin point:
			if ( (model != null) && (originToStraight != null) && (wormOrigin != null)) {
				transformedOrigin = originToStraight(model, originToStraight, wormOrigin, "wormOrigin");
			}
			else if ( transformedOrigin == null )
			{
				transformedOrigin = new Vector3f();
			}
			
			transformedAnnotations = saveAnnotationStatistics(imageA, model, originToStraight, resultExtents, "_after");
			straightToOrigin.disposeLocal();
			straightToOrigin = null;

			overlap2.disposeLocal();
			overlap2 = null;

			short id = (short) image.getVOIs().getUniqueID();
			final VOI lattice = new VOI(id, "lattice", VOI.POLYLINE, (float) Math.random());
			final VOIContour leftSide = new VOIContour(false);
			final VOIContour rightSide = new VOIContour(false);
			lattice.getCurves().add(leftSide);
			lattice.getCurves().add(rightSide);
			for (int i = 0; i < left.size(); i++) {

				// USE backup makerers
				final Vector3f leftPt = originToStraight(model, originToStraight, leftBackup.elementAt(i), "left" + i);

				// USE backup makerers
				final Vector3f rightPt = originToStraight(model, originToStraight, rightBackup.elementAt(i), "right" + i);

				if (leftPt.isEqual(Vector3f.ZERO) || rightPt.isEqual(Vector3f.ZERO)) {
					System.err.println("    " + imageA.getImageName() + " " + i + " " + leftPt + "     " + rightPt);
				} else if ( (leftSide.size() > 0) && ( (leftPt.Z <= leftSide.lastElement().Z) || (rightPt.Z <= rightSide.lastElement().Z))) {
					System.err.println("    " + imageA.getImageName() + " " + i + " " + leftPt + "     " + rightPt);
				} else {
					leftSide.add(leftPt);
					rightSide.add(rightPt);
				}
			}
			final float[] leftDistances = new float[leftSide.size()];
			final float[] rightDistances = new float[leftSide.size()];
			for (int i = 0; i < leftSide.size(); i++) {
				leftDistances[i] = 0;
				rightDistances[i] = 0;
				if (i > 1) {
					leftDistances[i] = leftSide.elementAt(i).distance(leftSide.elementAt(i - 1));
					rightDistances[i] = rightSide.elementAt(i).distance(rightSide.elementAt(i - 1));
				}
			}

			resultImage.registerVOI(lattice);
			lattice.setColor(new Color(0, 0, 255));
			lattice.getCurves().elementAt(0).update(new ColorRGBA(0, 0, 1, 1));
			lattice.getCurves().elementAt(1).update(new ColorRGBA(0, 0, 1, 1));
			lattice.getCurves().elementAt(0).setClosed(false);
			lattice.getCurves().elementAt(1).setClosed(false);

			id = (short) image.getVOIs().getUniqueID();
			for (int j = 0; j < leftSide.size(); j++) {
				id = (short) image.getVOIs().getUniqueID();
				final VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float) Math.random());
				final VOIContour mainAxis = new VOIContour(false);
				mainAxis.add(leftSide.elementAt(j));
				mainAxis.add(rightSide.elementAt(j));
				marker.getCurves().add(mainAxis);
				marker.setColor(new Color(255, 255, 0));
				mainAxis.update(new ColorRGBA(1, 1, 0, 1));
				if (j == 0) {
					marker.setColor(new Color(0, 255, 0));
					mainAxis.update(new ColorRGBA(0, 1, 0, 1));
				}
				resultImage.registerVOI(marker);
			}

			String voiDir = resultImage.getImageDirectory() + JDialogBase.makeImageName(baseName, "") + File.separator + "straightened_lattice"
					+ File.separator;
			saveAllVOIsTo(voiDir, resultImage);

			final VOIVector temp = resultImage.getVOIsCopy();
			resultImage.resetVOIs();
			if (transformedAnnotations != null) {
				resultImage.registerVOI(transformedAnnotations);
				voiDir = resultImage.getImageDirectory() + JDialogBase.makeImageName(baseName, "") + File.separator + "straightened_annotations"
						+ File.separator;
				saveAllVOIsTo(voiDir, resultImage);
			}
			saveLatticeStatistics(image, resultExtents[2], leftSide, rightSide, leftDistances, rightDistances, "_after");

			resultImage.restoreVOIs(temp);
			if (transformedAnnotations != null) {
				resultImage.registerVOI(transformedAnnotations);
			}

			saveNeuriteData( imageA, resultImage, model, originToStraight, baseName );


			final int[] markerIDs = new int[leftSide.size()];
			final int[][] markerVolumes = new int[leftSide.size()][2];
			ModelImage straightMarkers = segmentMarkers(resultImage, leftSide, rightSide, markerIDs, markerVolumes, false);
			saveLatticePositions(imageA, model, originToStraight, leftSide, rightSide, markerVolumes, "_after");

			saveImage(baseName, straightMarkers, true);
			if (displayResult) {
				straightMarkers.calcMinMax();
				new ViewJFrameImage(straightMarkers);
			} else {
				straightMarkers.disposeLocal();
				straightMarkers = null;
			}
		}

		saveImage(baseName, resultImage, saveAsTif);
		if (displayResult) {
			resultImage.calcMinMax();
			new ViewJFrameImage(resultImage);
		} else {
			resultImage.disposeLocal();
			resultImage = null;
		}

		if (originToStraight != null) {
			originToStraight.disposeLocal();
			originToStraight = null;
		}
	}

	/**
	 * Tests and updates the origin-to-straight image so that each voxel in the original twisted image has a
	 * corresponding output voxel in the straightened image.
	 * 
	 * @param model
	 * @param originToStraight
	 */
	private void testOriginToStraight(final ModelImage model, final ModelImage originToStraight) {
		final int dimX = model.getExtents().length > 0 ? model.getExtents()[0] : 1;
		final int dimY = model.getExtents().length > 1 ? model.getExtents()[1] : 1;
		final int dimZ = model.getExtents().length > 2 ? model.getExtents()[2] : 1;

		int missing1 = 0;
		int missing2 = 0;
		int modelCount = 0;

		final Vector3f pts = new Vector3f();
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					final float a = originToStraight.getFloatC(x, y, z, 0);
					final float m = model.getFloat(x, y, z);
					if (m != 0) {
						modelCount++;
					}
					if ( (a == 0) && (m != 0)) {
						missing1++;

						int count = 0;
						pts.set(0, 0, 0);
						for (int z1 = Math.max(0, z - 1); z1 < Math.min(dimZ, z + 1); z1++) {
							for (int y1 = Math.max(0, y - 1); y1 < Math.min(dimY, y + 1); y1++) {
								for (int x1 = Math.max(0, x - 1); x1 < Math.min(dimX, x + 1); x1++) {
									final float a1 = originToStraight.getFloatC(x1, y1, z1, 0);
									final float m1 = model.getFloat(x1, y1, z1);
									if ( (a1 != 0) && (m1 == m)) {
										final float x2 = originToStraight.getFloatC(x1, y1, z1, 1);
										final float y2 = originToStraight.getFloatC(x1, y1, z1, 2);
										final float z2 = originToStraight.getFloatC(x1, y1, z1, 3);
										pts.add(x2, y2, z2);
										count++;
									}
								}
							}
						}
						if (count != 0) {
							pts.scale(1f / count);
							originToStraight.setC(x, y, z, 0, 1);
							originToStraight.setC(x, y, z, 1, pts.X);
							originToStraight.setC(x, y, z, 2, pts.Y);
							originToStraight.setC(x, y, z, 3, pts.Z);
							// test.set(x,y,z,0);
						} else {
							// test.set(x,y,z,1);
							missing2++;
						}
					}
					if ( (a != 0) && (m != 0)) {
						// test.set(x,y,z,0);
					}
				}
			}
		}
		// System.err.println( modelCount + " " + missing1 + " " + missing2 );
		// System.err.println( missing1/(float)modelCount + " " + missing2/(float)modelCount );
		//
		// test.calcMinMax();
		// new ViewJFrameImage(test);
	}

	/**
	 * Updates the lattice data structures for rendering whenever the user changes the lattice.
	 * 
	 * @param rebuild
	 */
	private void updateLattice(final boolean rebuild) {
		if (left == null || right == null) {
			return;
		}
		if (right.size() == 0) {
			return;
		}
		if (rebuild) {
			// System.err.println( "new pt added" );
			if (latticeGrid != null) {
				for (int i = latticeGrid.size() - 1; i >= 0; i--) {
					final VOI marker = latticeGrid.remove(i);
					imageA.unregisterVOI(marker);
				}
			} else {
				latticeGrid = new VOIVector();
			}
			for (int j = 0; j < Math.min(left.size(), right.size()); j++) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				final VOI marker = new VOI(id, "pair_" + j, VOI.POLYLINE, (float) Math.random());
				final VOIContour mainAxis = new VOIContour(false);
				mainAxis.add(left.elementAt(j));
				mainAxis.add(right.elementAt(j));
				marker.getCurves().add(mainAxis);
				marker.setColor(new Color(255, 255, 0));
				mainAxis.update(new ColorRGBA(1, 1, 0, 1));
				if (j == 0) {
					marker.setColor(new Color(0, 255, 0));
					mainAxis.update(new ColorRGBA(0, 1, 0, 1));
				}
				imageA.registerVOI(marker);
				latticeGrid.add(marker);
			}
		} else {
			for (int i = 0; i < latticeGrid.size(); i++) {
				final VOI marker = latticeGrid.elementAt(i);
				marker.getCurves().elementAt(0).elementAt(0).copy(left.elementAt(i));
				marker.getCurves().elementAt(0).elementAt(1).copy(right.elementAt(i));
				marker.update();
			}
		}
		left.update();
		right.update();

		if (centerLine != null) {
			imageA.unregisterVOI(centerLine);
		}
		if (rightLine != null) {
			imageA.unregisterVOI(rightLine);
		}
		if (leftLine != null) {
			imageA.unregisterVOI(leftLine);
		}
		boolean showContours = false;
		if (displayContours != null) {
			showContours = (imageA.isRegistered(displayContours) != -1);
			if (showContours) {
				imageA.unregisterVOI(displayContours);
			}
		}

		if ( (left.size() == right.size()) && (left.size() >= 2)) {
			generateCurves();
			if (showContours) {
				imageA.registerVOI(displayContours);
			}
		}

		updateSelected();

		// when everything's done, notify the image listeners
		imageA.notifyImageDisplayListeners();
	}

	/**
	 * Updates the lattice data structures on undo/redo.
	 */
	private void updateLinks() {
		if (latticeGrid != null) {
			latticeGrid.clear();
		} else {
			latticeGrid = new VOIVector();
		}

		annotationVOIs = null;
		leftMarker = null;
		rightMarker = null;
		final VOIVector vois = imageA.getVOIs();
		for (int i = 0; i < vois.size(); i++) {
			final VOI voi = vois.elementAt(i);
			final String name = voi.getName();
			// System.err.println( vois.elementAt(i).getName() );
			if (name.equals("lattice")) {
				lattice = voi;
				left = (VOIContour) lattice.getCurves().elementAt(0);
				right = (VOIContour) lattice.getCurves().elementAt(1);
			} else if (name.equals("left line")) {
				leftLine = voi;
			} else if (name.equals("right line")) {
				rightLine = voi;
			} else if (name.equals("center line")) {
				centerLine = voi;
			} else if (name.contains("pair_")) {
				latticeGrid.add(voi);
			} else if (name.contains("wormContours")) {
				displayContours = voi;
			} else if (name.contains("interpolatedContours")) {
				displayInterpolatedContours = voi;
			} else if (name.equals("showSelected")) {
				showSelectedVOI = voi;
				// System.err.println("updateLinks showSelected ");
			} else if (name.equals("leftMarker")) {
				leftMarker = voi;
				// System.err.println("updateLinks showSelected ");
			} else if (name.equals("rightMarker")) {
				rightMarker = voi;
				// System.err.println("updateLinks showSelected ");
			} else if (name.equals("annotationVOIs")) {
				annotationVOIs = voi;
			}
		}
		clear3DSelection();
		if (showSelected != null) {
			for (int i = 0; i < showSelected.length; i++) {
				showSelected[i].dispose();
			}
			showSelected = null;
		}
		showSelectedVOI = null;
		updateLattice(true);
	}

	/**
	 * Updates the VOI displaying which point (lattice or annotation) is currently selected when the selection changes.
	 */
	private void updateSelected() {
		if (pickedPoint != null) {
			if (showSelectedVOI == null) {
				final short id = (short) imageA.getVOIs().getUniqueID();
				showSelectedVOI = new VOI(id, "showSelected", VOI.POLYLINE, (float) Math.random());
				imageA.registerVOI(showSelectedVOI);
			}
			if (showSelected == null) {
				showSelected = new VOIContour[3];
				showSelected[0] = new VOIContour(true);
				makeSelectionFrame(Vector3f.UNIT_X, Vector3f.UNIT_Y, pickedPoint, 4, showSelected[0]);
				showSelectedVOI.getCurves().add(showSelected[0]);
				showSelected[0].update(new ColorRGBA(0, 1, 1, 1));

				showSelected[1] = new VOIContour(true);
				makeSelectionFrame(Vector3f.UNIT_Z, Vector3f.UNIT_Y, pickedPoint, 4, showSelected[1]);
				showSelectedVOI.getCurves().add(showSelected[1]);
				showSelected[1].update(new ColorRGBA(0, 1, 1, 1));

				showSelected[2] = new VOIContour(true);
				makeSelectionFrame(Vector3f.UNIT_Z, Vector3f.UNIT_X, pickedPoint, 4, showSelected[2]);
				showSelectedVOI.getCurves().add(showSelected[2]);
				showSelected[2].update(new ColorRGBA(0, 1, 1, 1));

				showSelectedVOI.setColor(new Color(0, 255, 255));
			} else {
				for (int i = 0; i < showSelected.length; i++) {
					final Vector3f center = new Vector3f();
					for (int j = 0; j < showSelected[i].size(); j++) {
						center.add(showSelected[i].elementAt(j));
					}
					center.scale(1f / showSelected[i].size());
					final Vector3f diff = Vector3f.sub(pickedPoint, center);
					for (int j = 0; j < showSelected[i].size(); j++) {
						showSelected[i].elementAt(j).add(diff);
					}
				}
				showSelectedVOI.update();
			}
			if (imageA.isRegistered(showSelectedVOI) == -1) {
				imageA.registerVOI(showSelectedVOI);
			}
		}
	}

	/**
	 * Called from the straightening function. Exports the finalized worm model slice-by-slice into the straightened
	 * image.
	 * 
	 * @param image
	 * @param model
	 * @param originToStraight
	 * @param tSlice
	 * @param slice
	 * @param extents
	 * @param verts
	 * @param values
	 * @param dataOrigin
	 * @param sampleDistance
	 * @param sampleDistanceP
	 */
	private void writeDiagonal(final ModelImage image, final ModelImage model, final ModelImage originToStraight, final int tSlice, final int slice,
			final int[] extents, final Vector3f[] verts, final float[] values, final float[] dataOrigin, final float[] sampleDistance,
			final float[] sampleDistanceP) {
		final int iBound = extents[0];
		final int jBound = extents[1];
		final int[] dimExtents = image.getExtents();

		/*
		 * Get the loop multiplication factors for indexing into the 1D array with 3 index variables: based on the
		 * coordinate-systems: transformation:
		 */
		final int iFactor = 1;
		final int jFactor = dimExtents[0];
		final int kFactor = dimExtents[0] * dimExtents[1];
		final int tFactor = dimExtents[0] * dimExtents[1] * dimExtents[2];

		int buffFactor = 1;

		if ( (image.getType() == ModelStorageBase.ARGB) || (image.getType() == ModelStorageBase.ARGB_USHORT)
				|| (image.getType() == ModelStorageBase.ARGB_FLOAT)) {
			buffFactor = 4;
		}

		final Vector3f center = new Vector3f();
		for (int i = 0; i < verts.length; i++) {
			center.add(verts[i]);
		}
		center.scale(1f / verts.length);

		/* Calculate the slopes for traversing the data in x,y,z: */
		float xSlopeX = verts[1].X - verts[0].X;
		float ySlopeX = verts[1].Y - verts[0].Y;
		float zSlopeX = verts[1].Z - verts[0].Z;

		float xSlopeY = verts[3].X - verts[0].X;
		float ySlopeY = verts[3].Y - verts[0].Y;
		float zSlopeY = verts[3].Z - verts[0].Z;

		float x0 = verts[0].X;
		float y0 = verts[0].Y;
		float z0 = verts[0].Z;

		xSlopeX /= (iBound);
		ySlopeX /= (iBound);
		zSlopeX /= (iBound);

		xSlopeY /= (jBound);
		ySlopeY /= (jBound);
		zSlopeY /= (jBound);

		/* loop over the 2D image (values) we're writing into */
		float x = x0;
		float y = y0;
		float z = z0;

		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				/* calculate the ModelImage space index: */
				final int index = ( (iIndex * iFactor) + (jIndex * jFactor) + (kIndex * kFactor) + (tSlice * tFactor));

				if (sampleDistance != null) {
					sampleDistance[ (j * iBound) + i] = 0;
				}

				// Bounds checking:
				if ( ( (iIndex < 0) || (iIndex >= dimExtents[0])) || ( (jIndex < 0) || (jIndex >= dimExtents[1]))
						|| ( (kIndex < 0) || (kIndex >= dimExtents[2])) || ( (index < 0) || ( (index * buffFactor) > image.getSize()))) {

					// do nothing
				} else {
					float currentValue = (slice + 1);
					if (model != null) {
						// currentValue = model.getFloat((int)x, (int)y, (int)z);
						// currentValue = model.getFloatTriLinearBounds(x, y, z);
						currentValue = model.getFloat(iIndex, jIndex, kIndex);
					}
					if (currentValue == 0) {
						if (buffFactor == 4) {
							values[ ( ( (j * iBound) + i) * 4) + 0] = Math.max((float) image.getMinA(), values[ ( ( (j * iBound) + i) * 4) + 0]);
							values[ ( ( (j * iBound) + i) * 4) + 1] = Math.max((float) image.getMinR(), values[ ( ( (j * iBound) + i) * 4) + 1]);
							values[ ( ( (j * iBound) + i) * 4) + 2] = Math.max((float) image.getMinG(), values[ ( ( (j * iBound) + i) * 4) + 2]);
							values[ ( ( (j * iBound) + i) * 4) + 3] = Math.max((float) image.getMinB(), values[ ( ( (j * iBound) + i) * 4) + 3]);
						}
						/* not color: */
						else {
							values[ (j * iBound) + i] = Math.max((float) image.getMin(), values[ (j * iBound) + i]);
						}
					} else if (Math.abs(currentValue - (slice + 1)) < SampleLimit) {
						/* if color: */
						if (buffFactor == 4) {
							final float tempV = Math.max(image.getFloatC(iIndex, jIndex, kIndex, 1), image.getFloatC(iIndex, jIndex, kIndex, 2));
							if ( (tempV > values[ ( ( (j * iBound) + i) * 4) + 1]) || (tempV > values[ ( ( (j * iBound) + i) * 4) + 2])) {
								values[ ( ( (j * iBound) + i) * 4) + 0] = image.getFloatC(iIndex, jIndex, kIndex, 0);
								values[ ( ( (j * iBound) + i) * 4) + 1] = image.getFloatC(iIndex, jIndex, kIndex, 1);
								values[ ( ( (j * iBound) + i) * 4) + 2] = image.getFloatC(iIndex, jIndex, kIndex, 2);
								values[ ( ( (j * iBound) + i) * 4) + 3] = image.getFloatC(iIndex, jIndex, kIndex, 3);
								if (dataOrigin != null) {
									dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
									dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] = x;
									dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] = y;
									dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] = z;
								}
							}
						}
						/* not color: */
						else {
							final float tempV = image.getFloat(iIndex, jIndex, kIndex);
							// if ( tempV > values[ (j * iBound) + i] )
							// {
							// values[ (j * iBound) + i] = tempV;
							// if ( dataOrigin != null )
							// {
							// dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
							// dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] = x;
							// dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] = y;
							// dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] = z;
							// }
							// }
							values[ (j * iBound) + i] += tempV;
							if (dataOrigin != null) {
								dataOrigin[ ( ( (j * iBound) + i) * 4) + 0] = 1;
								dataOrigin[ ( ( (j * iBound) + i) * 4) + 1] += x;
								dataOrigin[ ( ( (j * iBound) + i) * 4) + 2] += y;
								dataOrigin[ ( ( (j * iBound) + i) * 4) + 3] += z;
							}
						}

						if ( (sampleDistanceP != null) && (sampleDistance != null)) {
							if ( (sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1] != 0) && (sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2] != 0)
									&& (sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3] != 0)) {
								float distance = (x - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1])
										* (x - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1]) + (y - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2])
										* (y - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2]) + (z - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3])
										* (z - sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3]);
								distance = (float) Math.sqrt(distance);
								sampleDistance[ (j * iBound) + i] = distance;
							}
						}

						if (originToStraight != null) {
							originToStraight.setC(iIndex, jIndex, kIndex, 0, 1);
							originToStraight.setC(iIndex, jIndex, kIndex, 1, i);
							originToStraight.setC(iIndex, jIndex, kIndex, 2, j);
							originToStraight.setC(iIndex, jIndex, kIndex, 3, slice);
						}
					}
				}
				if (sampleDistanceP != null) {
					sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 0] = 1;
					sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 1] = x;
					sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 2] = y;
					sampleDistanceP[ ( ( (j * iBound) + i) * 4) + 3] = z;
				}
				/*
				 * Inner loop: Move to the next diagonal point along the x-direction of the plane, using the xSlopeX,
				 * ySlopeX and zSlopeX values:
				 */
				x = x + xSlopeX;
				y = y + ySlopeX;
				z = z + zSlopeX;
			}

			/*
			 * Outer loop: Move to the next diagonal point along the y-direction of the plane, using the xSlopeY,
			 * ySlopeY and zSlopeY values:
			 */
			x0 = x0 + xSlopeY;
			y0 = y0 + ySlopeY;
			z0 = z0 + zSlopeY;
		}

		if ( (xSlopeX > 1) || (ySlopeX > 1) || (zSlopeX > 1) || (xSlopeY > 1) || (ySlopeY > 1) || (zSlopeY > 1)) {
			System.err.println("writeDiagonal " + xSlopeX + " " + ySlopeX + " " + zSlopeX);
			System.err.println("writeDiagonal " + xSlopeY + " " + ySlopeY + " " + zSlopeY);
		}
	}


	private Vector<VOI> neuriteData;
	/**
	 * Generates a natural spline curve to fit the input set of annotation points to model a neurite.
	 */
	public void addNeurite( VOI annotionVOI, String name ) {
		short sID;

		// 1. The center line of the worm is calculated from the midpoint between the left and right points of the
		// lattice.
		VOIContour neurite = new VOIContour(false);
		for (int i = 0; i < annotionVOI.getCurves().size(); i++) {
			VOIText text = (VOIText) annotionVOI.getCurves().elementAt(i);
			neurite.add( new Vector3f( text.elementAt(0) ) );
		}
		float[] time = new float[neurite.size()];
		NaturalSpline3 neuriteSpline = smoothCurve(neurite, time);

		VOIContour neuriterPositions = new VOIContour(false);

		float length = neuriteSpline.GetLength(0, 1);
		for (int i = 0; i <= length; i++) {
			final float t = neuriteSpline.GetTime(i);
			neuriterPositions.add(neuriteSpline.GetPosition(t));
		}

		sID = (short) (imageA.getVOIs().getUniqueID());
		VOI neuriteVOI = new VOI(sID, name, VOI.POLYLINE, (float) Math.random() );
		neuriteVOI.getCurves().add(neuriterPositions);
		neuriteVOI.setColor(Color.white);
		neuriterPositions.update(new ColorRGBA(1, 1, 1, 1));

		if ( neuriteData == null )
		{
			neuriteData = new Vector<VOI>();
		}
		for ( int i = 0; i < neuriteData.size(); i++ )
		{
			if ( neuriteData.elementAt(i).getName().equals(name) )
			{
				imageA.unregisterVOI( neuriteData.remove(i) );
				break;
			}
		}
		neuriteData.add(neuriteVOI);
		imageA.registerVOI(neuriteVOI);
	}

	private void saveNeuriteData( ModelImage wormImage, ModelImage resultImage, ModelImage model, ModelImage originToStraight, String baseName )
	{
		if ( neuriteData == null )
		{
			return;
		}
		if ( neuriteData.size() <= 0 )
		{
			return;
		}
		if ( (model == null) || (originToStraight == null)) {
			return;
		}

		VOIVector temp = wormImage.getVOIsCopy();
		for ( int i = 0; i < neuriteData.size(); i++ )
		{
			wormImage.resetVOIs();
			wormImage.registerVOI( neuriteData.elementAt(i) );
			String voiDir = resultImage.getImageDirectory() + JDialogBase.makeImageName(baseName, "") + File.separator + neuriteData.elementAt(i).getName() + "_spline" + File.separator;
			saveAllVOIsTo(voiDir, wormImage);
			
			saveSpline( wormImage, neuriteData.elementAt(i), Vector3f.ZERO, "_before" );
		}
		wormImage.restoreVOIs(temp);


		temp = resultImage.getVOIsCopy();
		for ( int i = 0; i < neuriteData.size(); i++ )
		{
			resultImage.resetVOIs();
			VOI transformedData = convertToStraight( model, originToStraight, neuriteData.elementAt(i));
			resultImage.registerVOI( transformedData );
			String voiDir = resultImage.getImageDirectory() + JDialogBase.makeImageName(baseName, "") + File.separator + neuriteData.elementAt(i).getName() + "_straightened_spline" + File.separator;
			saveAllVOIsTo(voiDir, resultImage);
			
			saveSpline( wormImage, transformedData, transformedOrigin, "_after" );
		}
		resultImage.restoreVOIs(temp);
	}

	private VOI convertToStraight( ModelImage model, ModelImage originToStraight, VOI data )
	{
		VOIContour straightSpline = new VOIContour(false);
		VOIContour spline = (VOIContour) data.getCurves().elementAt(0);
		for (int i = 0; i < spline.size(); i++)
		{
			Vector3f position = originToStraight(model, originToStraight, spline.elementAt(i), null);
			straightSpline.add(position);
		}

		VOI transformedAnnotations = new VOI(data.getID(), data.getName(), VOI.POLYLINE, 0.5f );
		transformedAnnotations.getCurves().add(straightSpline);
		return transformedAnnotations;
	}


	private void saveSpline(final ModelImage image, VOI data, Vector3f transformedOrigin, final String postFix) {

		VOIContour spline = (VOIContour) data.getCurves().elementAt(0);
		
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "statistics" + File.separator;
		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir
			// does
			// not
			// exist
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + data.getName() + postFix + ".csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + data.getName() + postFix + ".csv");
		}

		try {			
			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);
			bw.write("index" + "," + "x_voxels" + "," + "y_voxels" + "," + "z_voxels" + "," + "x_um" + "," + "y_um" + "," + "z_um" + "\n");
			for (int i = 0; i < spline.size(); i++) {
				Vector3f position = spline.elementAt(i);
				bw.write(i + "," + (position.X - transformedOrigin.X) + "," + (position.Y - transformedOrigin.Y) + ","
						+ (position.Z - transformedOrigin.Z) + "," +

                        VOILatticeManagerInterface.VoxelSize * (position.X - transformedOrigin.X) + "," + VOILatticeManagerInterface.VoxelSize
                        * (position.Y - transformedOrigin.Y) + "," + VOILatticeManagerInterface.VoxelSize * (position.Z - transformedOrigin.Z) + "\n");
			}
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
			e.printStackTrace();
		}
	}

}
