package gov.nih.mipav.view.renderer.WildMagic.WormUntwisting;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageMath;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import WildMagic.LibFoundation.Curves.NaturalSpline3;
import WildMagic.LibFoundation.Distance.DistanceVector3Ellipsoid3;
import WildMagic.LibFoundation.Distance.DistanceVector3Plane3;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Ellipsoid3f;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4d;
import WildMagic.LibFoundation.Mathematics.Vector4f;


/**
 * Supports the worm-straightening algorithms that use a 3D lattice as the basis of the straightening process.
 */
public class LatticeModelEM extends LatticeModel
{
	private static final LatticeModel PlugInAlgorithmWormUntwisting = null;
	private ModelImageLargeFormat imageA_LF;
	private Vector<Vector3f> nucleiCenters;
	private Vector<Float> nucleiDiameter;
	private Vector<String> nucleiNames;
	private Vector<Vector3f> outputNucleiCenters;
	private float outputXResolution = 1;
	private float outputYResolution = 1;
	private float outputZResolution = 1;
	private boolean unTwist = true;
	private boolean mask = true;
	private boolean untwistSpreadSheet = true;
	private float length;
	private NaturalSpline3 leftSpline, rightSpline;
	
	/**
	 * Creates a new LatticeModel
	 * 
	 * @param imageA
	 */
	public LatticeModelEM(final ModelImageLargeFormat imageA) {
		super(null);
		imageA_LF = imageA;

		outputDirectory = imageA.getOutputDirectory();
		System.err.println(outputDirectory);
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
	public void interpolateLattice( float outputResolutionX, float outputResolutionY, float outputResolutionZ ) {
		long time = System.currentTimeMillis();
		generateCurves(outputResolutionX, outputResolutionY, outputResolutionZ);
		outputXResolution = outputResolutionX;
		outputYResolution = outputResolutionY;
		System.err.println( "output image size " + (2 * extent) / outputResolutionX + " " + (2 * extent) / outputResolutionY );

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
		saveLatticeStatistics(outputDirectory, length, left, right, leftDistances, rightDistances, "_before");
		// save the original annotation positions
		saveAnnotationStatistics(outputDirectory, null, null, null, "_before");

		// The algorithm interpolates between the lattice points, creating two smooth curves from head to tail along
		// the left and right-hand sides of the worm body. A third curve down the center-line of the worm body is
		// also generated. Eventually, the center-line curve will be used to determine the number of sample points
		// along the length of the straightened worm, and therefore the final length of the straightened worm volume.
		//		generateCurves();

		boxBounds = new Vector<Box3f>();
		ellipseBounds = new Vector<Ellipsoid3f>();
		final short sID = voiID++;
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
		
		saveSamplePlanes( samplingPlanes, outputDirectory );
		saveDiameters( wormDiameters, outputDirectory );
		savePositions( leftPositions, outputDirectory, "left" );
		savePositions( rightPositions, outputDirectory, "right" );
		savePositions( centerPositions, outputDirectory, "center" );

		if ( imageA_LF != null )
		{
			final int[] resultExtents = new int[] {(int) ((2 * extent) / outputResolutionX), (int) ((2 * extent) / outputResolutionY), samplingPlanes.getCurves().size()};
			System.err.println( "...straightening" );
			straighten(imageA_LF, resultExtents);
			System.err.println( "...done" );	
		}
		System.err.println( "Total Striaghtening " + AlgorithmBase.computeElapsedTime(time) );
	}

	public void setLattice( VOI newLattice, float latticeScale )
	{
		if ( (latticeScale != 1) || ((imageA_LF != null) && (imageA_LF.getXRes() != 1) && (imageA_LF.getYRes() != 1) && (imageA_LF.getZRes() != 1)) )
		{
			scaleLattice(newLattice, latticeScale);
		}
		this.lattice = newLattice;
		if ( this.lattice == null )
		{
			left = null;
			right = null;
			return;
		}

		// Assume image is isotropic (square voxels).
		if (lattice.getCurves().size() != 2) {
			return;
		}
		left = (VOIContour) lattice.getCurves().elementAt(0);
		right = (VOIContour) lattice.getCurves().elementAt(1);
		if (left.size() != right.size()) {
			return;
		}
	}


	public void setNucleiMarkers(VOI nucleiVOIs, float scale)
	{
		nucleiCenters = new Vector<Vector3f>();
		nucleiNames = new Vector<String>();
//		for ( int i = 0; i < left.size(); i++ )
//		{
//			nucleiCenters.add(new Vector3f(left.elementAt(i)));
//			nucleiNames.add( "L" + i );
//			nucleiCenters.add(new Vector3f(right.elementAt(i)));
//			nucleiNames.add( "R" + i );
//		}
		
		nucleiDiameter = new Vector<Float>();
		for ( int i = 0; i < nucleiVOIs.getCurves().size(); i++ )
		{
			float diameter = 1;
			VOIText text = (VOIText)nucleiVOIs.getCurves().elementAt(i);
			Vector3f pos = new Vector3f(text.elementAt(0));
			fileToWorm( pos, scale );	

			if ( text.size() > 1 )
			{
				Vector3f pos2 = new Vector3f(text.elementAt(1));
				fileToWorm(pos2, scale);
				diameter = pos.distance(pos2);		
			}
			Vector3f center = new Vector3f( pos.X, pos.Y, pos.Z );
			nucleiCenters.add(center);
			nucleiDiameter.add(new Float(diameter));
			nucleiNames.add(text.getText());
		}

		outputNucleiCenters = new Vector<Vector3f>();
		
//		writeMarkers(imageA_LF);
	}


	/**
	 * Generates the set of natural spline curves to fit the current lattice.
	 */
	protected void generateCurves(float outputResolutionX, float outputResolutionY, float outputResolutionZ)
	{

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
		normalVectors = new Vector<Vector3f>();
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
		int maxLength = (int) Math.ceil(length/outputResolutionZ);
		float step = 1;
		if ( maxLength != length )
		{
			step = length / maxLength;
		}
		allTimes = new float[maxLength + 1];
		extent = -1;
		float minDiameter = Float.MAX_VALUE;
		outputZResolution = step;

		for (int i = 0; i <= maxLength; i++) {
			final float t = centerSpline.GetTime(i*step);
			centerPositions.add(centerSpline.GetPosition(t));
			leftPositions.add(leftSpline.GetPosition(t));
			rightPositions.add(rightSpline.GetPosition(t));

			allTimes[i] = t;
			Vector3f normal = new Vector3f(centerSpline.GetTangent(t));
//			normalVectors.add(normal);
			//			System.err.println( normal );
			final Vector3f leftPt = leftSpline.GetPosition(t);
			final Vector3f rightPt = rightSpline.GetPosition(t);

			final Vector3f rightDir = Vector3f.sub(rightPt, leftPt);
			float diameter = rightDir.normalize();
			diameter /= 2f;
			diameter += (DiameterBuffer);
			if (diameter > extent) {
				extent = (int) Math.ceil(diameter);
			}
			if ( (diameter > 0) && (diameter < minDiameter) )
			{
				minDiameter = diameter;
			}
			wormDiameters.add(diameter);
			rightVectors.add(rightDir);

			final Vector3f upDir = Vector3f.cross(normal, rightDir);
			upDir.normalize();
			upVectors.add(upDir);
			

			Vector3f normalTest = Vector3f.cross(rightDir, upDir);
			normalVectors.add(normalTest);
		}
		extent += 10;
		for ( int i = 0; i < wormDiameters.size(); i++ )
		{
			if ( wormDiameters.elementAt(i) < minDiameter )
			{
				wormDiameters.set(i, minDiameter);
			}
		}
		System.err.println( "generateCurves " + length + " " + maxLength + " " + step + " " + extent );
	}

	
	private void untwist(final ModelImageLargeFormat image, final int[] resultExtents)
	{
		long time = System.currentTimeMillis();
		if ( samplingPlanes == null )
		{
			samplingPlanes = loadSamplePlanes( outputDirectory );
		}
		if ( wormDiameters == null )
		{
			wormDiameters = loadDiameters( outputDirectory );
		}
		int size = samplingPlanes.getCurves().size();

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		int[] resultExtents2D = new int[]{resultExtents[0], resultExtents[1]};
		ModelImage resultImage = new ModelImage( ModelStorageBase.ARGB, resultExtents2D, imageName + "_straight_unmasked.xml");		
		resultImage.setResolutions(new float[] {1, 1});
		
		ModelImage straightToTwisted = new ModelImage( ModelStorageBase.ARGB_FLOAT, resultExtents2D, imageName + "_straightToTwisted_unmasked.xml");		
		straightToTwisted.setResolutions(new float[] {1, 1});

		final Vector3f[] corners = new Vector3f[4];
		
		for (int i = 0; i < size; i++)
		{			
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}

			writeDiagonal(image, size, i, resultExtents, corners, wormDiameters.elementAt(i), resultImage, straightToTwisted, true, null, null);

			resultImage.setImageName(imageName + "_straight_unmasked");
			saveImage(resultImage, i, true);
			
			straightToTwisted.setImageName(imageName + "_straightToTwisted_unmasked");
			saveImage(straightToTwisted, i, true);
			
			
			System.err.println( (i+1) + " " + size );
		}
		
		resultImage.disposeLocal(false);
		resultImage = null;
		straightToTwisted.disposeLocal(false);
		straightToTwisted = null;

		System.err.println( "writeDiagonal " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
	}
	
	private void untwistMarkers(final ModelImageLargeFormat image, final int[] resultExtents)
	{
		long time = System.currentTimeMillis();
		if ( samplingPlanes == null )
		{
			samplingPlanes = loadSamplePlanes( outputDirectory );
		}
		if ( wormDiameters == null )
		{
			wormDiameters = loadDiameters( outputDirectory );
		}
		int size = samplingPlanes.getCurves().size();
		float resX = image.getZRes();
		float resY = image.getZRes();
		float resZ = image.getZRes();
		
		
//		Vector<Ellipsoid3f> ellipseBounds = new Vector<Ellipsoid3f>();
//		for ( int i = 0; i < size; i++ )
//		{
//			ellipseBounds.add( new Ellipsoid3f( centerPositions.elementAt(i), rightVectors.elementAt(i),
//                    upVectors.elementAt(i), normalVectors.elementAt(i),
//                    leftPositions.elementAt(i).distance(rightPositions.elementAt(i)), 
//                    leftPositions.elementAt(i).distance(rightPositions.elementAt(i)), 2) );
//		}
		
		int count = 0;
		int[] targetSlice = new int[nucleiCenters.size()];
		for ( int i = 0; i < nucleiCenters.size(); i++ )
		{
			double minDist = Double.MAX_VALUE;
			int minIndex = -1;
			int closeCount = 0;
			for ( int j = 0; j < size; j++ )
			{
				float distance = centerPositions.elementAt(j).distance( nucleiCenters.elementAt(i) );
				float radius = leftPositions.elementAt(j).distance(rightPositions.elementAt(j))/2f;
				
				if ( (distance <= (1.05*radius)) )
				{					
					Plane3f plane = new Plane3f(normalVectors.elementAt(j), centerPositions.elementAt(j) );
					DistanceVector3Plane3 dist = new DistanceVector3Plane3(nucleiCenters.elementAt(i), plane);
					
					double eDistance = dist.Get(); //GetSquared ( markerCenters.elementAt(i), ellipseBounds.elementAtGetSquared ( nucleiCenters.elementAt(i), ellipseBounds.elementAt(j) );
					if ( eDistance < minDist )
					{
						minDist = eDistance;
						minIndex = j;
					}
					if ( minIndex == -1 )
					{
						System.err.println( nucleiNames.elementAt(i) + " " + GetSquared ( nucleiCenters.elementAt(i), ellipseBounds.elementAt(j) ) );
					}
					closeCount++;
				}
			}
			targetSlice[i] = minIndex;
			if ( minIndex != -1 )
			{
				count++;
//				System.err.println( nucleiNames.elementAt(i) + " " + minIndex + " " + closeCount );
			}
		}
		
		System.err.println( "Found " + count + " out of " + nucleiCenters.size() );

		Vector3f[] averageCenters = new Vector3f[nucleiCenters.size()];
		
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		int[] resultExtents2D = new int[]{resultExtents[0], resultExtents[1]};
		ModelImage resultImage = new ModelImage( ModelStorageBase.INTEGER, resultExtents2D, imageName + "_straight_unmasked.xml");		
		resultImage.setResolutions(new float[] {1, 1});

		final Vector3f[] corners = new Vector3f[4];		
		float centerX = resultExtents2D[0] / 2f;
		float centerY = resultExtents2D[1] / 2f;
		Vector3f radiusV = new Vector3f();
		
		System.err.println( outputXResolution + " " + outputZResolution + " " + resZ );
		
		count = 0;
		for (int i = 0; i < nucleiCenters.size(); i++)
		{			
			int target = targetSlice[i];
			if ( target != -1 )
			{
				VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(target);
				for (int j = 0; j < 4; j++) {
					corners[j] = kBox.elementAt(j);
				}
				averageCenters[i] = writeDiagonal(image, target, resultExtents, corners, nucleiCenters.elementAt(i) );
				if ( averageCenters[i].X != Float.MAX_VALUE )
				{
					count++;
				}
			}
			System.err.println( target + " " + nucleiCenters.size() );
		}
		
		System.err.println( "Found " + count + " out of " + nucleiCenters.size() );

		String voiDir = outputDirectory;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}
		voiDir = outputDirectory + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + imageName + "NucleiStraight.csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + imageName + "NucleiStraight.csv");
		}

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);

			bw.write("Name" + "," + "X" + "," + "Y" + "," + "Z" + "\n");

			for ( int i = 0; i < nucleiCenters.size(); i++ )
			{
				if ( averageCenters[i] != null )
				{
					Vector3f center = averageCenters[i];

					// calculate the output in worm coordinates:
					center.X -= resultExtents2D[0] / 2f;
					center.Y -= resultExtents2D[1] / 2f;
					center.scale( resX, resY, resZ );
					//				center.X += resultExtents2D[0] / 2f;
					//				center.Y += resultExtents2D[1] / 2f;

					System.err.println( nucleiNames.elementAt(i) + " " + center.X + " " + center.Y + " " + center.Z );
					bw.write(nucleiNames.elementAt(i) + "," + center.X + "," + center.Y + "," + center.Z + "\n");
				}
			}
						
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveNucleiInfo");
			e.printStackTrace();
		}
		
		
		resultImage.disposeLocal(false);
		resultImage = null;

		System.err.println( "untwist markers " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
	}

	
	private void untwistMarkers2(final ModelImageLargeFormat image, final int[] resultExtents)
	{
		long time = System.currentTimeMillis();
		if ( samplingPlanes == null )
		{
			samplingPlanes = loadSamplePlanes( outputDirectory );
		}
		if ( wormDiameters == null )
		{
			wormDiameters = loadDiameters( outputDirectory );
		}
		int size = samplingPlanes.getCurves().size();
		float resX = image.getZRes();
		float resY = image.getZRes();
		float resZ = image.getZRes();
		

		HashMap<Integer, Vector<Vector3d>> nucleiGroups = new HashMap<Integer, Vector<Vector3d>>();
		HashMap<Integer, Vector<Integer>> nucleiGroupCounts = new HashMap<Integer, Vector<Integer>>();
		Vector3f[] currentStartRange = new Vector3f[nucleiCenters.size()];
		
//		Vector<HashMap<Integer,Integer>> nucleiCounts = new Vector<HashMap<Integer,Integer>>();
//		for ( int i = 0; i < nucleiCenters.size(); i++ )
//		{
//			nucleiCounts.add( new HashMap<Integer,Integer>() );
//		}
		
		
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		int[] resultExtents2D = new int[]{resultExtents[0], resultExtents[1]};
		ModelImage resultImage = new ModelImage( ModelStorageBase.INTEGER, resultExtents2D, imageName + "_straight_unmasked.xml");		
		resultImage.setResolutions(new float[] {1, 1});

		Vector3f minV = new Vector3f();
		Vector3f maxV = new Vector3f();
		final Vector3f[] corners = new Vector3f[4];		
		float centerX = resultExtents2D[0] / 2f;
		float centerY = resultExtents2D[1] / 2f;
		Vector3f radiusV = new Vector3f();
		
		System.err.println( outputXResolution + " " + outputZResolution + " " + resZ );
		Vector3f pos = new Vector3f();
		Vector3f start = new Vector3f();
		for (int i = 0; i < size; i++)
		{			
			VOIContour kBox = (VOIContour) samplingPlanes.getCurves().elementAt(i);
			for (int j = 0; j < 4; j++) {
				corners[j] = kBox.elementAt(j);
			}

			minV.set( Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE );
			maxV.set(-Float.MAX_VALUE,-Float.MAX_VALUE,-Float.MAX_VALUE );
			writeDiagonal(image, size, i, resultExtents, corners, wormDiameters.elementAt(i), resultImage, null, true, minV, maxV);

			float radius = leftPositions.elementAt(i).distance(rightPositions.elementAt(i));
			radius /= outputXResolution;			
			radius = Math.max(3, radius/2f);
			
			radiusV.set( centerX - radius, centerY - radius, 0 );
			minV.max( radiusV );
			radiusV.set( centerX + radius, centerY + radius, 0 );
			maxV.min( radiusV );
			
			for ( int y = (int) Math.max(0, minV.Y); y < Math.min( resultExtents2D[1], maxV.Y); y++ )
			{
				for ( int x = (int) Math.max(0, minV.X); x < Math.min( resultExtents2D[0], maxV.X); x++ )
				{
					int value = resultImage.getInt(x, y);
					if ( value != 0 )
					{
						value--;
						// value = nuclei ID:
//						if ( value == 164 )
//						{
//							System.err.println( nucleiGroups.containsKey(value) );
//						}
						if ( !nucleiGroups.containsKey(value) )
						{
							Vector<Vector3d> groups = new Vector<Vector3d>();
							groups.add( new Vector3d(x,y,i) );							
							nucleiGroups.put(value, groups);
							
							Vector<Integer> counts = new Vector<Integer>();
							counts.add( new Integer(1) );
							nucleiGroupCounts.put(value, counts);							

							currentStartRange[value] = new Vector3f(x,y,i);
						}
						else
						{
							pos.set(x,y,i);
							pos.X -= resultExtents2D[0] / 2f;
							pos.Y -= resultExtents2D[1] / 2f;
							pos.scale( resX, resY, resZ );
							
							start.copy( currentStartRange[value] );
							start.X -= resultExtents2D[0] / 2f;
							start.Y -= resultExtents2D[1] / 2f;
							start.scale( resX, resY, resZ );
							
							Vector<Vector3d> groups = nucleiGroups.get(value);
							Vector<Integer> counts = nucleiGroupCounts.get(value);
							boolean inRange = (pos.distance(start) < nucleiDiameter.elementAt(value) );
							if ( inRange )
							{
								groups.lastElement().add(x, y, i);
								Integer c = counts.remove(counts.size()-1);
								c++;
								counts.add(c);
							}
							else
							{
								currentStartRange[value] = new Vector3f(x,y,i);
								groups.add(new Vector3d(x,y,i));
								counts.add(new Integer(1));
							}
						}

						
//						int count = 0;
//						if ( nucleiCounts.elementAt(value).containsKey(i) )
//						{
//							count = nucleiCounts.elementAt(value).get(i);
//						}
//						count++;
//						nucleiCounts.elementAt(value).put(i, count);
					}
				}
			}
			
			System.err.println( (i+1) + " " + size );
		}

		String voiDir = outputDirectory;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}
		voiDir = outputDirectory + "statistics" + File.separator;

		voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}

		File file = new File(voiDir + imageName + "NucleiStraight.csv");
		if (file.exists()) {
			file.delete();
			file = new File(voiDir + imageName + "NucleiStraight.csv");
		}

		try {

			final FileWriter fw = new FileWriter(file);
			final BufferedWriter bw = new BufferedWriter(fw);

			bw.write("Name" + "," + "X" + "," + "Y" + "," + "Z" + "\n");

			for ( int i = 0; i < nucleiCenters.size(); i++ )
			{
				if ( !nucleiGroups.containsKey(i) )
					continue;

				Vector<Vector3d> groups = nucleiGroups.get(i);
				Vector<Integer> counts = nucleiGroupCounts.get(i);
				int maxSize = 0;
				int maxIndex = -1;
				for ( int j = 0; j < counts.size(); j++ )
				{
					Vector3d center = new Vector3d( groups.elementAt(j) );
					center.scale( 1f / (float)counts.elementAt(j) );
					System.err.println( nucleiNames.elementAt(i) + " " + counts.elementAt(j) + "      " + center.X + " " + center.Y + " " + center.Z );
					
					if ( counts.elementAt(j) > maxSize )
					{
						maxSize = counts.elementAt(j);
						maxIndex = j;
					}
				}
				if ( maxIndex == -1 )
					continue;

				
				Vector3d center = new Vector3d( groups.elementAt(maxIndex) );
				System.err.println( nucleiNames.elementAt(i) + " MAX " + counts.elementAt(maxIndex) + "      " + center.X + " " + center.Y + " " + center.Z );
								
				// calculate average position:
				center.scale( 1f / (float)maxSize );
				
				// calculate the output in worm coordinates:
				center.X -= resultExtents2D[0] / 2f;
				center.Y -= resultExtents2D[1] / 2f;
				center.scale( resX, resY, resZ );
//				center.X += resultExtents2D[0] / 2f;
//				center.Y += resultExtents2D[1] / 2f;
				
				System.err.println( nucleiNames.elementAt(i) + " " + center.X + " " + center.Y + " " + center.Z + " " +
						maxSize + " " + counts.size() );
				bw.write(nucleiNames.elementAt(i) + "," + center.X + "," + center.Y + "," + center.Z + "\n");	
				System.err.println("");
			}
			
//			for ( int i = 0; i < nucleiCenters.size(); i++ )
//			{
//				HashMap<Integer,Integer> sliceLocations = nucleiCounts.elementAt(i);
//				Set<Integer> keysSet = sliceLocations.keySet();
//				int numKeys = sliceLocations.size();
//				Integer[] keys = new Integer[numKeys];
//				int k = 0;
//				Iterator<Integer> iterator = keysSet.iterator();
//				while ( iterator.hasNext() )
//				{
//					keys[k++] = iterator.next();
//				}
//				Arrays.sort(keys);
//				float radius = nucleiRadius.elementAt(i);
//				int range = (int) Math.abs(keys[0]*resZ - keys[numKeys-1]*resZ );
//				System.err.println( nucleiNames.elementAt(i) + " " + radius + " " + range + " " + (range < radius) );
//				for ( k = 0; k < keys.length; k++ )
//				{
//					int count = sliceLocations.get(keys[k]);
//					System.err.println( nucleiNames.elementAt(i) + " slice " + keys[k] + " count " + count );
//					bw.write(nucleiNames.elementAt(i) + "," + keys[k] + "," + count + "\n");	
//				}
//			}
			
			bw.newLine();
			bw.close();
		} catch (final Exception e) {
			System.err.println("CAUGHT EXCEPTION WITHIN saveNucleiInfo");
			e.printStackTrace();
		}
		
		
		resultImage.disposeLocal(false);
		resultImage = null;

		System.err.println( "untwist markers " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
	}
	
	private void mask(final ModelImageLargeFormat image, final int[] resultExtents)
	{		
		long time = System.currentTimeMillis();
		
		if ( centerPositions == null )
		{
			centerPositions = loadPositions( outputDirectory, "center" );
		}
		if ( leftPositions == null )
		{
			leftPositions = loadPositions( outputDirectory, "left" );
		}
		if ( rightPositions == null )
		{
			rightPositions = loadPositions( outputDirectory, "right" );
		}
		if ( wormDiameters == null )
		{
			wormDiameters = loadDiameters( outputDirectory );
		}
		
		int size = centerPositions.size();
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		final int dimX = resultExtents[0];
		final int dimY = resultExtents[1];
		final int dimZ = 1;

		Vector3f center2D = new Vector3f( dimX/2, dimY/2, 0);

		int min = 240;
		int max = 255;

		VOIContour[] contoursForward = new VOIContour[size];
		float[] areas = new float[size];
		Vector3f[] minEllipse = new Vector3f[size];
		Vector3f[] maxEllipse = new Vector3f[size];
		
		ModelImage resultImage;
		ModelImage straightToTwisted;

		String voiDir = outputDirectory + "contours" + File.separator;
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

		int width = (int) (Math.max(1, 80 / outputXResolution));
		boolean[] contoursOK = new boolean[size];
		for (int i = 0; i < size; i++)
		{			
			minEllipse[i] = new Vector3f();
			maxEllipse[i] = new Vector3f();
			
			resultImage = readImage(imageName + "_straight_unmasked", i);
			straightToTwisted = readImage(imageName + "_straightToTwisted_unmasked", i);
			
			
			float radius = leftPositions.elementAt(i).distance(rightPositions.elementAt(i));
			radius /= outputXResolution;			
			float r = Math.max(3, radius/2f);
//			System.err.println( i + " " + leftPositions.elementAt(i).distance(rightPositions.elementAt(i)) + " " + radius + " " + r + " " + dimX + " " + dimY );
			contoursForward[i] = WormSegmentation.findLargestConnected( resultImage, r, 
					dimX, dimY, dimZ, min, max, i, minEllipse[i], maxEllipse[i], width );

			areas[i] = 0;
			if ( contoursForward[i] != null )
			{
				areas[i] = (float) contoursForward[i].area();
				float bb = minEllipse[i].distance(maxEllipse[i])/2f;
				float targetArea = (float) (Math.PI * r * r);
				float ratioArea = targetArea / areas[i];
				float ratioRadius = r / bb;
//				System.err.println( i + " " + areas[i] + " " + targetArea + " " + ratioArea + "    " + bb + " " + r +  " " + ratioRadius );
				if ( (ratioArea > .75) && (ratioArea < 1.5) && (ratioRadius > .65) && (ratioRadius < 1.5) )
				{
					contoursOK[i] = true;
				}
				else
				{
					contoursOK[i] = false;
				}
			}
			else
			{
				contoursOK[i] = false;
			}
			
			resultImage.disposeLocal(false);
			resultImage = null;
			straightToTwisted.disposeLocal(false);
			straightToTwisted = null;
			
//			System.err.println((i+1) + " " + size );
		}
		System.err.println( "   findLargestConnected " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();

		boolean badFound = false;
		int start = -1;
		for (int i = 0; i < size; i++)
		{
			if ( !contoursOK[i] && !badFound )
			{
				start = i-1;
				badFound = true;
			}
			else if ( contoursOK[i] && badFound )
			{
				if ( start == -1 )
				{
					for ( int j = 0; j < i; j++ )
					{
						contoursForward[j] = new VOIContour(contoursForward[i]);
						contoursOK[j] = true;
						System.err.println( j );
					}
				}
				else
				{
					VOI newContours = interpolateContours(contoursForward[start], contoursForward[i], (i - (start+1)), dimX, dimY);
					for ( int j = 0; j < (i - (start+1)); j++ )
					{
						int indexInterp = start + 1 + j;
						if ( newContours.getCurves().elementAt(j).size() > 0 )
						{
							contoursForward[indexInterp] = new VOIContour((VOIContour) newContours.getCurves().elementAt(j));
						}
						else
						{
							if ( Math.abs(indexInterp - start) < Math.abs(indexInterp - i) && (contoursForward[start].size() > 0))
							{
								contoursForward[indexInterp] = new VOIContour(contoursForward[start]);
							}
							else if ( contoursForward[i].size() > 0 )
							{
								contoursForward[indexInterp] = new VOIContour(contoursForward[i]);
							}
							else
							{
								if ( contoursForward[start].size() > 0 )
								{
									contoursForward[indexInterp] = new VOIContour(contoursForward[start]);
								}
								else if ( contoursForward[i].size() > 0 )
								{
									contoursForward[indexInterp] = new VOIContour(contoursForward[i]);
								}
							}
						}
						contoursOK[indexInterp] = true;
						System.err.println( indexInterp );
						
						Vector3f dir = new Vector3f();
						VOIContour ellipse = contoursForward[indexInterp];
						for ( int k = 0; k < ellipse.size(); k++ )
						{
							dir.copy(ellipse.elementAt(k));
							dir.sub(center2D);
							float length = dir.normalize();
							length += 10;
							dir.scale(length);
							ellipse.elementAt(k).copy(center2D);
							ellipse.elementAt(k).add(dir);
						}
					}
				}

				badFound = false;
				start = -1;
			}
		}

		if ( badFound && (start != -1) )
		{
			for ( int j = start + 1; j < size; j++ )
			{
				contoursForward[j] = new VOIContour(contoursForward[start]);
				contoursOK[j] = true;
				System.err.println( j );
			}
		}
		
		System.err.println( "   interpolateContours " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
		
		

		
		for (int i = 0; i < size; i++)
		{			
			resultImage = readImage(imageName + "_straight_unmasked", i);
			straightToTwisted = readImage(imageName + "_straightToTwisted_unmasked", i);
			

			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					if ( (contoursForward[i] != null) && !contoursForward[i].contains( x, y ) )
					{
						resultImage.setC(x, y, 0, (byte) ( (0 & 0x000000ff)) );
						resultImage.setC(x, y, 1, (byte) ( (0 & 0x000000ff)) );	
						resultImage.setC(x, y, 2, (byte) ( (0 & 0x000000ff)) );	
						resultImage.setC(x, y, 3, (byte) ( (0 & 0x000000ff)) );

						straightToTwisted.setC(x, y, 0, 0 );
						straightToTwisted.setC(x, y, 1, 0 );	
						straightToTwisted.setC(x, y, 2, 0 );	
						straightToTwisted.setC(x, y, 3, 0 );
					}
				}
			}

			VOI outputContour = new VOI( (short)i, "contour_" + i, VOI.POLYLINE, (float) Math.random());
			outputContour.getCurves().add( contoursForward[i] );
			resultImage.registerVOI( outputContour );

			voiDir = outputDirectory + "contours" + File.separator + imageName + "_contour_" + i + File.separator;
			saveAllVOIsTo(voiDir, resultImage);
			resultImage.unregisterAllVOIs();

			resultImage.setImageName(imageName + "_straight");
			saveImage(resultImage, i, true);
			
			straightToTwisted.setImageName(imageName + "_straightToTwisted");
			saveImage(straightToTwisted, i, true);
			
			resultImage.disposeLocal(false);
			resultImage = null;
			straightToTwisted.disposeLocal(false);
			straightToTwisted = null;
			
			System.err.println((i+1) + " " + size );
		}
		System.err.println( "  write masks and contours " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
	}
	
	
	private void straighten(final ModelImageLargeFormat image, final int[] resultExtents) {
		if ( unTwist )
		{
			if ( nucleiCenters != null )
			{
				String imageName = image.getImageName();
				if (imageName.contains("_clone")) {
					imageName = imageName.replaceAll("_clone", "");
				}				
				String nucleiImageName = outputDirectory + JDialogBase.makeImageName(imageName, "_markers");
				ModelImageLargeFormat nucleiImage = new ModelImageLargeFormat( ModelImageLargeFormat.INT, image.getExtents(), nucleiImageName, true );
				nucleiImage.setResolutions(image.getXRes(), image.getYRes(), image.getZRes());
				image.clearTable();
				System.err.println( "untwisting markers" );
				untwistMarkers(nucleiImage, resultExtents);
				System.err.println( "DONE untwisting markers" );
			}
//			untwist(image, resultExtents);
		}
//		if ( mask )
//		{
//			mask(image, resultExtents);
//		}
//		if ( untwistSpreadSheet )
//		{
//			untwistData(image);
//		}
	}

	protected void writeDiagonal(final ModelImageLargeFormat image, 
			final int curveLength, final int slice,
			final int[] extents, final Vector3f[] verts, float diameter, ModelImage result, final ModelImage straightToTwisted, 
			boolean checkNuclei, Vector3f minV, Vector3f maxV ) 
	{
		final int iBound = extents[0];
		final int jBound = extents[1];

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

		//		Vector3f position = new Vector3f();
		//		float[] indexValues = new float[4];

		int[] imageValues = new int[4];
		Vector3f wormPt = new Vector3f();
		Vector3f indexPt = new Vector3f();
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {
				if ( result.getType() == ModelStorageBase.ARGB )
				{
					result.setC(i, j, 0, (byte) ( (0 & 0x000000ff)) );
					result.setC(i, j, 1, (byte) ( (0 & 0x000000ff)) );
					result.setC(i, j, 2, (byte) ( (0 & 0x000000ff)) );
					result.setC(i, j, 3, (byte) ( (0 & 0x000000ff)) );
				}
				else
				{
					result.set(i,j,0,0);
				}
				if ( straightToTwisted != null )
				{
					straightToTwisted.setC(i, j, 0, 0 );
					straightToTwisted.setC(i, j, 1, 0 );
					straightToTwisted.setC(i, j, 2, 0 );
					straightToTwisted.setC(i, j, 3, 0 );
				}

				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				// Bounds checking:
				if ( !image.checkBounds(iIndex, jIndex, kIndex) ) {
					// do nothing
				} else {
					if ( (minV != null) && (maxV != null) )
					{
						indexPt.set(i,j,0);
						minV.min(indexPt);
						maxV.max(indexPt);
					}
					if ( image.getType() == ModelImageLargeFormat.INT )
					{
						final Integer tempV = image.get(iIndex, jIndex, kIndex);
						imageValues[0] = tempV;
						imageValues[1] = tempV;
						imageValues[2] = tempV;
						imageValues[3] = tempV;
					}
					else
					{
						float[] imageV = image.getC(iIndex, jIndex, kIndex);
						imageValues[0] = (int) imageV[0];
						imageValues[1] = (int) imageV[1];
						imageValues[2] = (int) imageV[2];
						imageValues[3] = (int) imageV[3];
					}

					if ( result.getType() == ModelStorageBase.ARGB )
					{
						result.setC(i, j, 0, (byte) ( (imageValues[0] & 0x000000ff)) );
						result.setC(i, j, 1, (byte) ( (imageValues[1] & 0x000000ff)) );
						result.setC(i, j, 2, (byte) ( (imageValues[2] & 0x000000ff)) );
						result.setC(i, j, 3, (byte) ( (imageValues[3] & 0x000000ff)) );
					}
					else
					{
						result.set(i, j, 0, imageValues[1]);
					}
					if ( straightToTwisted != null )
					{
						wormPt.set(x, y, z);
//						image.fileCoordinates(wormPt);
						straightToTwisted.setC(i, j, 0, 1 );
						straightToTwisted.setC(i, j, 1, wormPt.X );
						straightToTwisted.setC(i, j, 2, wormPt.Y );
						straightToTwisted.setC(i, j, 3, wormPt.Z );
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

//		if ( (xSlopeX > 1) || (ySlopeX > 1) || (zSlopeX > 1) || (xSlopeY > 1) || (ySlopeY > 1) || (zSlopeY > 1)) {
//			System.err.println("writeDiagonal " + xSlopeX + " " + ySlopeX + " " + zSlopeX);
//			System.err.println("writeDiagonal " + xSlopeY + " " + ySlopeY + " " + zSlopeY);
//		}
	}

	protected Vector3f writeDiagonal(final ModelImageLargeFormat image, int slice,
			final int[] extents, final Vector3f[] verts, Vector3f target ) 
	{
		final int iBound = extents[0];
		final int jBound = extents[1];

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
		
		float minDistance = Float.MAX_VALUE;
		Vector3f closest = new Vector3f(Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE);
		Vector3f pt = new Vector3f();
		for (int j = 0; j < jBound; j++) {

			/* Initialize the first diagonal point(x,y,z): */
			x = x0;
			y = y0;
			z = z0;

			for (int i = 0; i < iBound; i++) {

				final int iIndex = Math.round(x);
				final int jIndex = Math.round(y);
				final int kIndex = Math.round(z);

				// Bounds checking:
				if ( !image.checkBounds(iIndex, jIndex, kIndex) ) {
					// do nothing
				} else {
					pt.set(x, y, z);
					float dist = pt.distance(target);
					if ( dist < minDistance )
					{
						minDistance = dist;
						closest.set(i, j, slice);
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
		return closest;
	}


	private void saveImage(final ModelImage image, int sliceID, boolean isTif)
	{
		String imageName = image.getImageName();
		String voiDir = outputDirectory + JDialogBase.makeImageName(imageName, "") + File.separator;
		File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) { // do nothing
		} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}

		imageName = imageName + "_" + sliceID;
		final File file = new File(voiDir + imageName);
		if (file.exists()) {
			file.delete();
		}
		if ( isTif )
		{
			ModelImage.saveImage(image, imageName + ".tif", voiDir, false);
		}
		else
		{
			ModelImage.saveImage(image, imageName + ".xml", voiDir, false);
		}
		//		System.err.println( "saveImage " + voiDir + " " + imageName + ".tif" );
	}


	private void fileToWorm( Vector3f pt, float scale )
	{
		Vector3f scaleDown = new Vector3f(0,0,0);
		Vector3f scaleUp = new Vector3f(0,0,0);
		Vector3f scaleV = new Vector3f(scale, scale, 1);
		Vector3f resolutionsScale = new Vector3f(1, 1, 1);
		if ( imageA_LF != null )
		{
			scaleDown.X = (int)(imageA_LF.getExtents()[0]/(scale*2));
			scaleDown.Y = (int)(imageA_LF.getExtents()[1]/(scale*2));

			scaleUp.X = imageA_LF.getExtents()[0]/2;
			scaleUp.Y = imageA_LF.getExtents()[1]/2;

			resolutionsScale.X = imageA_LF.getXRes();
			resolutionsScale.Y = imageA_LF.getYRes();
			resolutionsScale.Z = imageA_LF.getZRes();

			pt.sub(scaleDown);
			pt.mult(scaleV);
			pt.mult(resolutionsScale);
			pt.add(scaleUp);			
		}
	}

	private void wormToFile( Vector3f pt, float scale )
	{
		Vector3f scaleDown = new Vector3f(0,0,0);
		Vector3f scaleUp = new Vector3f(0,0,0);
		Vector3f scaleV = new Vector3f(1f/scale, 1f/scale, 1);
		Vector3f resolutionsScale = new Vector3f(1, 1, 1);
		if ( imageA_LF != null )
		{
			scaleDown.X = (int)(imageA_LF.getExtents()[0]/(scale*2));
			scaleDown.Y = (int)(imageA_LF.getExtents()[1]/(scale*2));

			scaleUp.X = imageA_LF.getExtents()[0]/2;
			scaleUp.Y = imageA_LF.getExtents()[1]/2;

			resolutionsScale.X = 1f/imageA_LF.getXRes();
			resolutionsScale.Y = 1f/imageA_LF.getYRes();
			resolutionsScale.Z = 1f/imageA_LF.getZRes();

			pt.sub(scaleUp);			
			pt.mult(resolutionsScale);
			pt.mult(scaleV);
			pt.add(scaleDown);
		}
	}



	private void scaleLattice( VOI lattice, float scale )
	{
		VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
		VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
		for ( int i = 0; i < Math.min( left.size(), right.size() ); i++ )
		{
			fileToWorm(left.elementAt(i), scale);
			fileToWorm(right.elementAt(i), scale);
		}

	}


	private VOI interpolateContours(VOIContour contour1, VOIContour contour2, int numSlices, int dimX, int dimY )
	{
		int centerX = dimX/2;
		int centerY = dimY/2;

		Vector3f geomCenter1 = contour1.getGeometricCenter();
		Vector3f geomCenter2 = contour2.getGeometricCenter();

		//reposition VOIs to center
		float transX1 = centerX - geomCenter1.X;
		float transY1 = centerY - geomCenter1.Y;
		float transX2 = centerX - geomCenter2.X;
		float transY2 = centerY- geomCenter2.Y;
		contour1.translate(transX1, transY1, 0); contour1.update();
		contour2.translate(transX2, transY2, 0); contour2.update();


		//generate binary mask images
		//generate distance map image for shape interpolation
		ModelImage distanceMap1 = new ModelImage(ModelStorageBase.BOOLEAN, new int[]{dimX,dimY}, "dMap1" );
		ModelImage distanceMap2 = new ModelImage(ModelStorageBase.BOOLEAN, new int[]{dimX,dimY}, "dMap2" );


		for ( int y = 0; y < dimY; y++ )
		{
			for ( int x = 0; x < dimX; x++ )
			{
				int index = y*dimX + x;
				if ( contour1.contains( x, y ) )
				{
					distanceMap1.set(index, true);
				}
				if ( contour2.contains( x, y ) )
				{
					distanceMap2.set(index, true);
				}
			}
		}


		AlgorithmMorphology2D distanceMapAlgo1 = new AlgorithmMorphology2D(distanceMap1, 0, 0, AlgorithmMorphology2D.DISTANCE_MAP_FOR_SHAPE_INTERPOLATION, 0, 0, 0, 0, true);
		distanceMapAlgo1.run();
		AlgorithmMorphology2D distanceMapAlgo2 = new AlgorithmMorphology2D(distanceMap2, 0, 0, AlgorithmMorphology2D.DISTANCE_MAP_FOR_SHAPE_INTERPOLATION, 0, 0, 0, 0, true);
		distanceMapAlgo2.run();

		VOI curves = new VOI((short)0, "interp");
		if ( numSlices == 1 )
		{
			ModelImage averageDistanceMaps = new ModelImage(distanceMap1.getType(), distanceMap1.getExtents(), "average_0");
			AlgorithmImageCalculator mathAlgo = new AlgorithmImageCalculator(averageDistanceMaps, distanceMap1, distanceMap2, AlgorithmImageCalculator.AVERAGE, AlgorithmImageMath.CLIP, true, null);
			mathAlgo.run();

			ModelImage inBetweenBooleanShapes = new ModelImage(ModelStorageBase.BOOLEAN, distanceMap1.getExtents(), "booleanShape_0");
			int length = distanceMap1.getExtents()[0] * distanceMap1.getExtents()[1];
			float[] avgDistMapBuff = new float[length];
			boolean[] boolBuff = new boolean[length];
			try {
				averageDistanceMaps.exportData(0,length,avgDistMapBuff);
			}catch(Exception e) {

			}
			for(int i=0;i<avgDistMapBuff.length;i++) {
				if(avgDistMapBuff[i] >= 0) {
					boolBuff[i] = true;
				}else {
					boolBuff[i] = false;
				}
			}
			try {
				inBetweenBooleanShapes.importData(0, boolBuff, true);
			}catch(Exception e) {

			}

			AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(inBetweenBooleanShapes);
			VOIExtractionAlgo.run();

			//we are only working with one contour on a 2d slice here
			ViewVOIVector VOIs = (ViewVOIVector) inBetweenBooleanShapes.getVOIs();
			VOIContour newContour = null;
			if ( VOIs != null )
			{
				if ( VOIs.size() > 0 )
				{
					VOI tempVOI = (VOI)(VOIs.VOIAt(0).clone());
					tempVOI.setUID(tempVOI.hashCode());
					if ( tempVOI != null )
					{
						Vector<VOIBase> contours = tempVOI.getCurves();
						if ( contours.size() > 0 )
						{
							newContour = (VOIContour)contours.elementAt(0);
						}
					}
				}
			}

			boolean isLineVertical = false;
			float diffX = Math.abs(geomCenter1.X - geomCenter2.X);
			float stepX = (diffX/2);

			float newX = 0;
			float newY = 0;
			if(geomCenter1.X < geomCenter2.X) {
				newX = geomCenter1.X + stepX;
			}else if(geomCenter1.X > geomCenter2.X) {
				newX = geomCenter2.X + stepX;
			}else {
				isLineVertical = true;
			}

			if(!isLineVertical) {
				//get newY coordinate
				newY = linearInterpGetY(geomCenter1, geomCenter2, newX);
			}
			if(isLineVertical) {
				newX = geomCenter1.X;
				float diffY = Math.abs(geomCenter1.Y - geomCenter2.Y);
				float stepY = (diffY/2);
				if(geomCenter1.Y < geomCenter2.Y) {
					newY = geomCenter1.Y + stepY;
				}else if(geomCenter1.Y > geomCenter2.Y) {
					newY = geomCenter2.Y + stepY;
				}else {
					newY = geomCenter1.Y;
				}
			}

			if ( newContour != null )
			{
				float transX = newX - centerX;
				float transY = newY - centerY;
				newContour.translate(transX, transY, 0);
				curves.getCurves().add(newContour);
			}
			else
			{
				curves.getCurves().add(new VOIContour(false));
			}

			averageDistanceMaps.disposeLocal(false);
			inBetweenBooleanShapes.disposeLocal(false);
		}
		else
		{ 
			for( int i = 1, k = numSlices; i <= numSlices; i++,k-- )
			{
				int denominator = numSlices + 1;
				double numerator_A = k;
				double numerator_B = i;

				double factor_A = (numerator_A/denominator);
				double factor_B = (numerator_B/denominator);

				int index = i - 1;
				ModelImage tempA = new ModelImage(distanceMap1.getType(), distanceMap1.getExtents(), "tempA_" + index);
				ModelImage tempB = new ModelImage(distanceMap2.getType(), distanceMap2.getExtents(), "tempB_" + index);

				ModelImage averageDistanceMaps = new ModelImage(distanceMap1.getType(), distanceMap1.getExtents(), "average_" + index);

				AlgorithmImageMath mathAlgoA = new AlgorithmImageMath(tempA, distanceMap1, AlgorithmImageMath.MULTIPLY, factor_A, 0, 0, AlgorithmImageMath.CLIP, true);
				mathAlgoA.run();

				AlgorithmImageMath mathAlgoB = new AlgorithmImageMath(tempB, distanceMap2, AlgorithmImageMath.MULTIPLY, factor_B, 0, 0, AlgorithmImageMath.CLIP, true);
				mathAlgoB.run();

				AlgorithmImageCalculator mathAlgoAdd = new AlgorithmImageCalculator(averageDistanceMaps, tempA, tempB, AlgorithmImageCalculator.ADD, AlgorithmImageMath.CLIP, true, null);
				mathAlgoAdd.run();

				ModelImage inBetweenBooleanShapes = new ModelImage(ModelStorageBase.BOOLEAN, distanceMap1.getExtents(), "booleanShape_" + index);
				int length = distanceMap1.getExtents()[0] * distanceMap1.getExtents()[1];
				float[] avgDistMapBuff = new float[length];
				boolean[] boolBuff = new boolean[length];
				try {
					averageDistanceMaps.exportData(0,length,avgDistMapBuff);
				}catch(Exception e) {
				}
				for(int m=0;m<avgDistMapBuff.length;m++) {
					if(avgDistMapBuff[m] >= 0) {
						boolBuff[m] = true;
					}else {
						boolBuff[m] = false;
					}
				}
				try {
					inBetweenBooleanShapes.importData(0, boolBuff, true);
				}catch(Exception e) {
				}

				//new ViewJFrameImage( (ModelImage)averageDistanceMaps[index].clone() );
				//new ViewJFrameImage( (ModelImage)inBetweenBooleanShapes[index].clone() );

				AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(inBetweenBooleanShapes);
				VOIExtractionAlgo.run();
				
				VOIContour newContour = null;
				//we are only working with one contour on a 2d slice here
				ViewVOIVector VOIs = inBetweenBooleanShapes.getVOIs();
				if ( VOIs != null )
				{
					if ( VOIs.size() > 0 )
					{
						VOI tempVOI = new VOI(VOIs.VOIAt(0));
						if ( tempVOI != null )
						{
							tempVOI.setUID(tempVOI.hashCode());
							Vector<VOIBase> contours = tempVOI.getCurves();
							newContour = (VOIContour)contours.elementAt(0);
						}
					}
				}

				boolean isLineVertical = false;
				float diffX = Math.abs(geomCenter1.X - geomCenter2.X);
				float stepX = diffX/(denominator);
				float newX = 0;
				float newY = 0;
				if(geomCenter1.X < geomCenter2.X) {
					newX = (int)geomCenter1.X + (stepX * i);
				}else if(geomCenter1.X > geomCenter2.X) {
					//newX = (int)geomCenter2.X + (stepX * i);
					newX = (int)geomCenter1.X - (stepX * i);
				}else {
					//line is vertical...to do later
					isLineVertical = true;
				}

				if(!isLineVertical) {
					//get newY coordinate
					newY = linearInterpGetY(geomCenter1, geomCenter2, newX);
				}
				if(isLineVertical) {
					newX = geomCenter1.X;
					float diffY = Math.abs(geomCenter1.Y - geomCenter2.Y);
					float stepY = (diffY/denominator);
					if(geomCenter1.Y < geomCenter2.Y) {
						newY = geomCenter1.Y + (stepY * i);
					}else if(geomCenter1.Y > geomCenter2.Y) {
						//newY = geomCenter2.Y + (stepY * i);
						newY = geomCenter1.Y - (stepY * i);
					}else {
						newY = geomCenter1.Y;
					}
				}


				if ( newContour != null )
				{
					float transX = newX - centerX;
					float transY = newY - centerY;
					newContour.translate(transX, transY, 0);
					newContour.update();
					curves.getCurves().add(new VOIContour(newContour));
				}
				else
				{
					curves.getCurves().add(new VOIContour(false));
				}

				if(tempA != null) {
					tempA.disposeLocal(false);
					tempA = null;
				}
				if(tempB != null) {
					tempB.disposeLocal(false);
					tempB = null;
				}
				averageDistanceMaps.disposeLocal(false);
				inBetweenBooleanShapes.disposeLocal(false);
			}

		}
		distanceMap1.disposeLocal(false);
		distanceMap2.disposeLocal(false);

		return curves;
	}

	/** get y based on equation of linear interpolation **/
	private float linearInterpGetY(Vector3f geomCenter1, Vector3f geomCenter2, float x) {
		float y = 0;
		y = ((geomCenter1.Y) + (x - geomCenter1.X)*(geomCenter2.Y - geomCenter1.Y)/(geomCenter2.X - geomCenter1.X));
		return y;
	}



	private void writeMarkers(final ModelImageLargeFormat image)
	{
		long time = System.currentTimeMillis();
		

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		float resX = image.getXRes();
		float resY = image.getYRes();
		float resZ = image.getZRes();
		
		int[] extents2D = new int[]{image.getExtents()[0], image.getExtents()[1]};
		System.err.println( extents2D[0] + " " + extents2D[1] );
		
		ModelImage resultImage = new ModelImage( ModelStorageBase.INTEGER, extents2D, imageName + "_markers.xml");		
		resultImage.setResolutions(new float[] {1, 1});

		int size = image.getExtents()[2];

		Vector3f pt = new Vector3f();
		Vector3f centerPt = new Vector3f();
		int centerCount = 0;
		for (int z = 0; z < size; z++)
		{			
			for ( int y = 0; y < extents2D[1]; y++ )
			{
				for ( int x = 0; x < extents2D[0]; x++ )
				{
					resultImage.set(x, y, 0);
				}
			}
			for ( int i = 0; i < nucleiCenters.size(); i++ )
			{
				if ( nucleiCenters.elementAt(i).Z == (z*resZ) )
				{
					centerCount++;
					centerPt.set( nucleiCenters.elementAt(i).X, nucleiCenters.elementAt(i).Y, z );
					
					float radius = nucleiDiameter.elementAt(i)/2f;
					for ( int y = (int) -radius; y < radius; y++ )
					{
						for ( int x = (int) -radius; x < radius; x++ )
						{
							pt.set( nucleiCenters.elementAt(i).X + x, nucleiCenters.elementAt(i).Y + y, z);
							if ( centerPt.distance(pt) < radius )
							{
								wormToFile(pt,1);
								resultImage.set( Math.min( extents2D[0] - 1, Math.max( 0, (int)(pt.X) ) ), 
										Math.min( extents2D[1] - 1, Math.max( 0, (int)(pt.Y) ) ), (i+1) );
							}
						}
					}
				}
			}

			resultImage.setImageName(imageName + "_markers");
			saveImage(resultImage, z, false);			
			
			System.err.println( (z+1) + " " + size );
		}
		System.err.println( centerCount + " out of " + nucleiCenters.size() );
		resultImage.disposeLocal(false);
		resultImage = null;

		System.err.println( "writeMarkers " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
		
	}
	

	private void writeMarkersDebug(final ModelImageLargeFormat image)
	{
		long time = System.currentTimeMillis();
		

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		float resX = image.getXRes();
		float resY = image.getYRes();
		float resZ = image.getZRes();
		
		int[] extents2D = new int[]{image.getExtents()[0], image.getExtents()[1]};
		System.err.println( extents2D[0] + " " + extents2D[1] );
		
		ModelImage resultImage = new ModelImage( ModelStorageBase.ARGB, extents2D, imageName + "_markers.xml");		
		resultImage.setResolutions(new float[] {1, 1});

		int size = image.getExtents()[2];

		Vector3f pt = new Vector3f();
		Vector3f centerPt = new Vector3f();
		int centerCount = 0;
		for (int z = 0; z < size; z++)
		{			
			for ( int y = 0; y < extents2D[1]; y++ )
			{
				for ( int x = 0; x < extents2D[0]; x++ )
				{
					pt.set(x, y, z);
					fileToWorm(pt, 1);
					int value = image.get((int)(pt.X), (int)(pt.Y), (int)(pt.Z));
					resultImage.setC(x, y, 0, (byte) ( (255 & 0x000000ff)) );
					resultImage.setC(x, y, 1, (byte) ( (value & 0x000000ff)) );
					resultImage.setC(x, y, 2, (byte) ( (value & 0x000000ff)) );
					resultImage.setC(x, y, 3, (byte) ( (value & 0x000000ff)) );
				}
			}
			for ( int i = 0; i < nucleiCenters.size(); i++ )
			{
				if ( nucleiCenters.elementAt(i).Z == (z*resZ) )
				{
					centerCount++;
					centerPt.set( nucleiCenters.elementAt(i).X, nucleiCenters.elementAt(i).Y, z );
					
					float radius = nucleiDiameter.elementAt(i)/2f;
					for ( int y = (int) -radius; y < radius; y++ )
					{
						for ( int x = (int) -radius; x < radius; x++ )
						{
							pt.set( nucleiCenters.elementAt(i).X + x, nucleiCenters.elementAt(i).Y + y, z);
							if ( centerPt.distance(pt) < radius )
							{
								wormToFile(pt,1);
//								resultImage.setC((int)pt.X, (int)pt.Y, 1, i + 100 );
								resultImage.setC( Math.min( extents2D[0] - 1, Math.max( 0, (int)(pt.X) ) ), 
										Math.min( extents2D[1] - 1,Math.max( 0, (int)(pt.Y) ) ), 1, (byte) ( (255 & 0x000000ff)) );
							}
						}
					}
				}
			}

			resultImage.setImageName(imageName + "_markers_debug");
			saveImage(resultImage, z, true);			
			
			System.err.println( (z+1) + " " + size );
		}
		System.err.println( centerCount + " out of " + nucleiCenters.size() );
		resultImage.disposeLocal(false);
		resultImage = null;

		System.err.println( "writeMarkers " + AlgorithmBase.computeElapsedTime(time) );
		time = System.currentTimeMillis();
		
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
	protected NaturalSpline3 smoothCurve2(final VOIContour curve, final float[] time) {
		final Vector3f[] akPoints = new Vector3f[curve.size()];
		for (int i = 0; i < curve.size(); i++) {
			akPoints[i] = new Vector3f(curve.elementAt(i));
			//			System.err.println( akPoints[i] + " " + time[i] );
		}

		return new NaturalSpline3(NaturalSpline3.BoundaryType.BT_FREE, curve.size() - 1, time, akPoints);
	}
	
}
