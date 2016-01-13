package gov.nih.mipav.view.renderer.WildMagic.Render;


import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogAnnotation;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import java.awt.Color;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;

import javax.swing.JFileChooser;

import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Distance.DistanceSegment3Segment3;
import WildMagic.LibFoundation.Distance.DistanceVector3Segment3;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;


/**
 * Supports the worm-straightening algorithms that use a 3D lattice as the basis of the straightening process.
 */
public class LatticeModelEM extends LatticeModel
{

	private Vector<Vector4f> nucleiCenters;
	private Vector<String> nucleiNames;

	/**
	 * Creates a new LatticeModel
	 * 
	 * @param imageA
	 */
	public LatticeModelEM(final ModelImage imageA) {
		super(imageA);
	}

	/**
	 * Creats a new LatticeModel with the given input lattice.
	 * 
	 * @param imageA
	 * @param lattice
	 */
	public LatticeModelEM(final ModelImage imageA, final VOI lattice) {
		super(imageA, lattice);
	}

	/**
	 * Creates a new LatticeModel with the given set of annotations.
	 * 
	 * @param imageA
	 * @param annotation
	 * @param doAnnotation
	 */
	public LatticeModelEM(final ModelImage imageA, final VOI annotation, final boolean doAnnotation) {
		super(imageA, annotation, doAnnotation);
	}
	


	/**
	 * Deletes this LatticeModel
	 */
	public void dispose() {
		super.dispose();
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

			final VOIContour ellipse = new VOIContour(true);
			final Ellipsoid3f ellipsoid = makeEllipse(rkRVector, rkUVector, rkEye, wormDiameters.elementAt(i), wormDiameters.elementAt(i), ellipse);
			ellipseBounds.add(ellipsoid);

			final Box3f box = new Box3f(ellipsoid.Center, ellipsoid.Axis, new float[] {extent, extent, 1});
			boxBounds.add(box);
		}
		createWormModel(imageA, samplingPlanes, ellipseBounds, wormDiameters, 2 * extent, displayResult);
	}

	public void setNucleiMarkers(VOI nucleiVOIs)
	{
		nucleiCenters = new Vector<Vector4f>();
		nucleiNames = new Vector<String>();
		for ( int i = 0; i < nucleiVOIs.getCurves().size(); i++ )
		{
			VOIText text = (VOIText)nucleiVOIs.getCurves().elementAt(i);
			float radius = text.size() > 1 ? text.elementAt(0).distance(text.elementAt(1)) : 1;
			Vector3f pos = text.elementAt(0);
			Vector4f centerRadius = new Vector4f( pos.X, pos.Y, pos.Z, radius );
			nucleiCenters.add(centerRadius);
			nucleiNames.add(text.getText());
//			System.err.println( text.getText() + " " + pos + " " + radius );
		}
	}
	

	private void createWormModel(final ModelImage imageA, final VOI samplingPlanes, final Vector<Ellipsoid3f> ellipseBounds, final Vector<Float> diameters,
			final int diameter, final boolean displayResult) {
		final int[] resultExtents = new int[] {diameter, diameter, samplingPlanes.getCurves().size()};

		String imageName = imageA.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		ModelImage model = new ModelImage(ModelStorageBase.FLOAT, imageA.getExtents(), imageName + "_model.xml");
		JDialogBase.updateFileInfo(imageA, model);
		
		ModelImage nucleiMarkers = null;

		ModelImage insideConflict = new ModelImage(ModelStorageBase.BOOLEAN, imageA.getExtents(), imageName + "_insideConflict.xml");
		JDialogBase.updateFileInfo(imageA, insideConflict);

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
		
		
		int[] nucleiSlices = null;
		if ( nucleiCenters != null )
		{
			nucleiMarkers = new ModelImage(ModelStorageBase.FLOAT, imageA.getExtents(), imageName + "_model.xml");
			JDialogBase.updateFileInfo(imageA, model);
			
			nucleiSlices = new int[nucleiCenters.size()];
			int missingCount = 0;
			Vector3f center = new Vector3f();
			Vector3f currentPoint = new Vector3f();
			for ( int i = 0; i < nucleiCenters.size(); i++ )
			{
				Vector4f pos = nucleiCenters.elementAt(i);
				center.set(pos.X, pos.Y, pos.Z);
				currentPoint.set(pos.X, pos.Y, pos.Z);
				int minIndex = -1;
				float minDistance = Float.MAX_VALUE;
				for ( int j = 0; j < ellipseBounds.size(); j++ )
				{
					if ( ellipseBounds.elementAt(j).Contains(currentPoint) )
					{
						float distance = currentPoint.distance(ellipseBounds.elementAt(j).Center );
						if ( distance < minDistance )
						{
							minDistance = distance;
							minIndex = j;
						}
					}
				}
				nucleiSlices[i] = minIndex + 1;
				if ( minIndex == -1 )
				{
					missingCount++;
				}
				else
				{
					for ( int z = (int) Math.max(0, Math.floor(pos.Z - pos.W)); z <= Math.min(dimZ - 1, Math.ceil(pos.Z + pos.W)); z++ )
					{
						for ( int y = (int) Math.max(0, Math.floor(pos.Y - pos.W)); y <= Math.min(dimY - 1, Math.ceil(pos.Y + pos.W)); y++ )
						{
							for ( int x = (int) Math.max(0, Math.floor(pos.X - pos.W)); x <= Math.min(dimX - 1, Math.ceil(pos.X + pos.W)); x++ )
							{
								currentPoint.set( x, y, z );
								if ( center.distance(currentPoint) <= pos.W )
								{
									minIndex = -1;
									minDistance = Float.MAX_VALUE;
									int startIndex = (int)Math.max(0, nucleiSlices[i] - pos.W);
									int endIndex = (int)Math.min(nucleiSlices[i] + pos.W, ellipseBounds.size() - 1);
									for ( int j = startIndex; j <= endIndex; j++ )
									{
										if ( ellipseBounds.elementAt(j).Contains(currentPoint) )
										{
											float distance = currentPoint.distance(ellipseBounds.elementAt(j).Center );
											if ( distance < minDistance )
											{
												minDistance = distance;
												minIndex = j;
											}
										}
									}
									if ( minIndex != -1 )
									{
										for (int z1 = Math.max(0, (int) Math.floor(z)); (z1 <= Math.min(dimZ - 1, Math.ceil(z))); z1++) {
											for (int y1 = Math.max(0, (int) Math.floor(y)); (y1 <= Math.min(dimY - 1, Math.ceil(y))); y1++) {
												for (int x1 = Math.max(0, (int) Math.floor(x)); (x1 <= Math.min(dimX - 1, Math.ceil(x))); x1++) {
													nucleiMarkers.set(x1, y1, z1, minIndex );
												}
											}
										}
									}
								}
							}
						}					
					}
				}
			}
//			nucleiMarkers.calcMinMax();
//			new ViewJFrameImage((ModelImage)nucleiMarkers.clone());
			System.err.println( "Pre Grow " + missingCount + " " + nucleiCenters.size() );
//			return;
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
					initializeModelandConflicts(imageA, model, nucleiMarkers, insideConflict, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i),
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
					initializeModelandConflicts(imageA, model, nucleiMarkers, insideConflict, 0, i, resultExtents, cornersSub, ellipseBounds.elementAt(i),
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
		
//		model.calcMinMax();
//		new ViewJFrameImage((ModelImage)model.clone());
		insideConflict.disposeLocal(true);
		insideConflict = null;
		
		
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
		int max = (int) (resultExtents[0] / 3);
		System.err.println( max );
		while ( (growStep < max) && ( !checkAnnotations(model, false) || (growStep < 20)))
		{
//			for (int z = 0; z < dimZ; z++) {
//				for (int y = 0; y < dimY; y++) {
//					for (int x = 0; x < dimX; x++) {
//						if (model.getFloat(x, y, z) != 0) {
//							inside.set(x, y, z, 1);
//						}
//						else
//						{
//							inside.set(x, y, z, 0);
//						}
//					}
//				}
//			}
//			inside.setImageName( imageName + "_" + growStep + "_insideMask.xml" );
//			saveImage(imageName, inside, false, "masks");
			
			growEdges(maskImage, model, null, null, growStep++);

//			model.calcMinMax();
//			new ViewJFrameImage((ModelImage)model.clone());
			
		}
//		model.calcMinMax();
//		new ViewJFrameImage((ModelImage)model.clone());
		

		System.err.println("    generateMasks " + growStep );
		if ( !checkAnnotations(model, true)) {
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

		if ( growContours != null )
		{
			for (int i = 0; i < growContours.getCurves().size(); i += 30) {
				final VOIContour contour = (VOIContour) growContours.getCurves().elementAt(i).clone();
				contour.trimPoints(0.5, true);
				displayInterpolatedContours.getCurves().add(contour);
				contour.update(new ColorRGBA(0, 1, 0, 1));
				contour.setVolumeDisplayRange(minRange);
			}
		}


		ModelImage inside = new ModelImage(ModelStorageBase.INTEGER, imageA.getExtents(), imageName + "_insideMask.xml");
		JDialogBase.updateFileInfo(imageA, inside);
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
		inside.setImageName( imageName + "_" + growStep + "_insideMask.xml" );
		saveImage(imageName, inside, false);
		inside.disposeLocal();
		inside = null;

		saveImage(imageName, model, false);
		straighten(imageA, resultExtents, imageName, model, true, displayResult, true);
//		straighten(imageA, resultExtents, imageName, model, false, displayResult, true);
		
		model.disposeLocal();
		model = null;
	}

	
	private void growEdges(final ModelImage mask, final ModelImage model, final ModelImage markers, final float[] sliceIDs, final int step) {
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
					float length = diff.normalize();

					pt.copy(centerPositions.elementAt(i));
					int x = Math.round(pt.X);
					int y = Math.round(pt.Y);
					int z = Math.round(pt.Z);
					float currentValue = model.getFloat(x, y, z);
					while ( ( (length != 0) && (currentValue != 0) && Math.abs(currentValue - value) <= SampleLimit)) {
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
								length = diff.normalize();
								while ( (length != 0) && !pt.isEqual(start) && (distPt < distStart)) {
									pt.add(diff);
									x = Math.round(pt.X);
									y = Math.round(pt.Y);
									z = Math.round(pt.Z);
									if ( (x < 0) || (x >= dimX) || (y < 0) || (y >= dimY) || (z < 0) || (z >= dimZ)) {
										break;
									}
//									System.err.println( pt + "    " + start + "    " + centerPositions.elementAt(i) );
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
			float targetValue = (sliceIDs == null) ? value : sliceIDs[i];
			final VOIContour ellipse = (VOIContour) growContours.getCurves().elementAt(i);
			interpolateContour(ellipse);
						
			for (int j = 0; j < ellipse.size(); j++) {
				final Vector3f pt = ellipse.elementAt(j);
				final Vector3f diff = Vector3f.sub(pt, centerPositions.elementAt(i));
				final float distance = diff.normalize();
				if ( distance > (wormDiameters.elementAt(i)) )
					continue;
				// diff.scale(0.5f);
//				if ( mask != null )
//				{
//					final float maskValue = mask.getFloat(Math.round(pt.X), Math.round(pt.Y), Math.round(pt.Z));
//					if ( maskValue != 0 )
//					{
//						continue;
//					}
//				}

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
								if ( (markerValue != 0) && (markerValue != targetValue)) {
									extend = false;
									break;
								}
							}
							if ( mask != null )
							{
								final float maskValue = mask.getFloat(x1, y1, z1);
								if ( maskValue != 0 )
								{
									extend = false;
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
	private void initializeModelandConflicts(final ModelImage image, final ModelImage model, final ModelImage nucleiMarkers, final ModelImage insideConflict, final int tSlice,
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

//		final boolean[][] values = new boolean[iBound][jBound];
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
//				values[i][j] = false;
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
//						values[i][j] = true;
						for (int z1 = Math.max(0, (int) Math.floor(z)); z1 <= Math.min(dimExtents[2] - 1, Math.ceil(z)); z1++) {
							for (int y1 = Math.max(0, (int) Math.floor(y)); y1 <= Math.min(dimExtents[1] - 1, Math.ceil(y)); y1++) {
								for (int x1 = Math.max(0, (int) Math.floor(x)); x1 <= Math.min(dimExtents[0] - 1, Math.ceil(x)); x1++) {
									boolean foundNuclei = false;
									float nucleiValue = 0;
									if ( nucleiMarkers != null )
									{
										nucleiValue = nucleiMarkers.getFloat(x1,y1,z1);
										if ( nucleiValue != 0 )
										{
											foundNuclei = true;
										}
									}
									if ( foundNuclei )
									{
										model.set(x1,y1,z1,nucleiValue);
									}
									else
									{
										final float currentValue = model.getFloat(x1, y1, z1);
										if (currentValue != 0) {
											if (Math.abs(currentValue - value) < SampleLimit) {
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
				final Vector3f leftPt = originToStraight(model, originToStraight, left.elementAt(i), "left" + i);
				final Vector3f rightPt = originToStraight(model, originToStraight, right.elementAt(i), "right" + i);

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
			
			if ( nucleiCenters != null )
			{
				saveNucleiVolumes( imageA, resultImage, model, originToStraight, baseName );
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


	private void saveNucleiVolumes( ModelImage image, ModelImage result, ModelImage model, ModelImage originToStraight, String baseName )
	{
		ModelImage output = new ModelImage( result.getDataType(), result.getExtents(), JDialogBase.makeImageName( result.getImageName(), "_nucleiMarkers.xml") );
		JDialogBase.updateFileInfo(result, output);

		// original dimensions:
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		for ( int i = 0; i < nucleiCenters.size(); i++ )
		{
			Vector3f p1 = new Vector3f();
			Vector3f center = new Vector3f();
			Vector4f pos = nucleiCenters.elementAt(i);
			center.set( pos.X, pos.Y, pos.Z );
			Vector3f straightCenter = originToStraight(model, originToStraight, center, "center" );
			int volumeCount = 0;
			int foundCount = 0;
			int outsideCount = 0;
//			float centerValue = model.getFloat( Math.round(center.X), Math.round(center.Y), Math.round(center.Z) );
			for ( int z = (int) Math.max(0, Math.floor(pos.Z - pos.W)); z <= Math.min(dimZ - 1, Math.ceil(pos.Z + pos.W)); z++ )
			{
				for ( int y = (int) Math.max(0, Math.floor(pos.Y - pos.W)); y <= Math.min(dimY - 1, Math.ceil(pos.Y + pos.W)); y++ )
				{
					for ( int x = (int) Math.max(0, Math.floor(pos.X - pos.W)); x <= Math.min(dimX - 1, Math.ceil(pos.X + pos.W)); x++ )
					{
						p1.set( x, y, z );
						if ( center.distance(p1) <= pos.W )
						{
							volumeCount++;
//							float currentValue = model.getFloat(x, y, z);
//							if ( (currentValue != 0) && (Math.abs(currentValue - centerValue) <= SampleLimit) )
							{
								Vector3f newPt = originToStraight(model, originToStraight, p1, nucleiNames.elementAt(i) + "_" + x + "_" + y + "_" + z );
								if ( !newPt.isEqual(Vector3f.ZERO) )
								{
									output.set( Math.round(newPt.X), Math.round(newPt.Y), Math.round(newPt.Z), 1 );
									foundCount++;
									if ( straightCenter.distance(newPt) > pos.W )
									{
										outsideCount++;
									}
								}
							}
						}
					}
				}					
			}
//			System.err.println( 100*(float)(foundCount)/(float)volumeCount + " " + 100*(float)(outsideCount)/(float)foundCount );
		}
		saveImage( baseName, output, false );
	}
	
}
