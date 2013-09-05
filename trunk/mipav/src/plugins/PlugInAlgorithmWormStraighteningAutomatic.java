import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBSpline;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.event.ActionListener;
import java.awt.event.WindowListener;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.BitSet;
import java.util.Vector;

import WildMagic.LibFoundation.Approximation.ApprEllipsoidFit3f;
import WildMagic.LibFoundation.Intersection.Intersector.IntersectionInfo;
import WildMagic.LibFoundation.Intersection.IntrBox3Box3f;
import WildMagic.LibFoundation.Intersection.IntrPlane3Plane3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Line3f;
import WildMagic.LibFoundation.Mathematics.Mathf;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmWormStraighteningAutomatic extends PlugInAlgorithmWormStraightening
{

	private class Box {
		Vector3f[] corners = new Vector3f[4];
		public Box() {}
		public Box(Box b)
		{
			for ( int i = 0; i < corners.length; i++ )
			{
				corners[i] = new Vector3f(b.corners[i]);
			}
		}
	}

	private ModelImage wormImage, resultImage;
	private int diameter;
	private int[] resultExtents = new int[3];
	private int[] resultUnits = new int[3];
	private float[] resultResolutions = new float[3];

	private float[] outputResolutions = new float[3];

	private VOI originalPoints;
	private VOI originalCurve;
	private VOI samplingPlanes;


	private Vector3f centerShift = new Vector3f(1,1,1);
	private float[] sourceResolutions;


	private float stepSize = 1.0f;
	private Vector<Vector3f> curvePositions = new Vector<Vector3f>();


	private Vector<Vector3f> curveTangents = new Vector<Vector3f>();
	private Vector<Box> unitBox = new Vector<Box>();
	private Vector<Box> boundingBoxes = new Vector<Box>();

	private Box origBox;

	private Vector3f origNormal;

	private float wormLength = -1;
	private float headDiameter = -1;
	private float tailDiameter = -1;
	private float maxDiameter = -1;

	private boolean fillData = false;
	private boolean displayOriginal = false;
	private boolean displayMask = false;
	private boolean saveTransform = false;
	private String transformFileName = null;


	private double minAngle10 = (Math.PI/18f);

	private double minAngle30 = (Math.PI/6f);


	private double minAngle45 = (Math.PI/4f);

	private double minAngle60 = (Math.PI/3f);

	private double minAngle90 = (Math.PI/2f);

	public PlugInAlgorithmWormStraighteningAutomatic(ModelImage wormImage)
	{
		this(wormImage, 1.0f);
	}

	public PlugInAlgorithmWormStraighteningAutomatic(ModelImage wormImage, float stepSize)
	{
		this.wormImage = wormImage;
		this.stepSize = stepSize;
		this.sourceResolutions = wormImage.getResolutions(0);
	}

	private VOI findHeadTail(ModelImage wormImage)
	{		
		ModelImage wormBlur = blur();
		float minTailRadius = 25;
		float minHeadRadius = ( headDiameter/2f) - 5;
		float minRadius = ( headDiameter/2f) + 5;
		float maxRadius = ( maxDiameter/2f) + 5;
		float minVal = (float)wormBlur.getMin();
		float minPeakVal = (float)(wormBlur.getMax() * .05);
//		System.err.println( headDiameter + " " + minHeadRadius + " " + maxRadius + " " + minTailRadius );
//		System.err.println( minVal + " " + (float)wormBlur.getMax() + "    " + minPeakVal );


		Vector<Vector3f> uniqueNormals = halfSphere(8, 16, 1);

		Vector3f pos = new Vector3f();		

		Vector<Vector3f> vectorFieldPositions = new Vector<Vector3f>();
		Vector<Vector3f> vectorFieldDirections = new Vector<Vector3f>();

		wormImage.resetVOIs();

		Vector<Vector3f> tailPositions = new Vector<Vector3f>();
		Vector<Vector3f> tailDirections = new Vector<Vector3f>();
		//		Vector<Vector3f> headPositions = new Vector<Vector3f>();
		//		Vector<Vector3f> headDirections = new Vector<Vector3f>();

		ViewJProgressBar progressBar = new ViewJProgressBar("Automatic Path", "searching for worm center...", 0, 100, true,
                this, this, true);
		
		long start = System.currentTimeMillis();
		Vector<Vector3f> peakPositions = findMaxPeaks( wormBlur, minPeakVal );
//		System.err.println( "Time peakPositions = " + computeElapsedTime(start) );
		progressBar.updateValue(1);
		if ( threadStopped )
		{
			progressBar.setVisible(false);
			progressBar.dispose();
			return null;
		}

		int size = peakPositions.size();
		start = System.currentTimeMillis();
		for ( int i = 0; i < peakPositions.size(); i++ )
		{
			pos.copy(peakPositions.elementAt(i));
			testSphere( wormImage, wormBlur, vectorFieldPositions, vectorFieldDirections, 
					tailPositions, tailDirections,
					new Vector3f(pos), uniqueNormals, minRadius, maxRadius, minHeadRadius, minTailRadius, minVal, minPeakVal );

			progressBar.updateValueImmed( (int)(100 * (i / (float)size)) );
			if ( threadStopped )
			{
				progressBar.setVisible(false);
				progressBar.dispose();
				return null;
			}

			//			Vector3f newPos = testSphereInit( wormBlur, vectorFieldPositions, vectorFieldDirections,
			//					new Vector3f(pos), uniqueNormals, minRadius, maxRadius, minVal, minPeakVal);			
			//			Vector3f headTangent = testSphereHead( wormImage, new Vector3f(pos), uniqueNormals, minHeadRadius, minPeakVal, minVal );
			//			if ( headTangent != null )
			//			{
			//				
			//				if ( newPos != null )
			//				{
			//					System.err.println( "Found head and sphere  " + headTangent );
			//					System.err.println( "                       " + vectorFieldDirections.lastElement() );
			//				}
			//				
			////				System.err.println( "Found Head " + pos );
			//				headPositions.add(new Vector3f(pos) );
			//				headDirections.add( headTangent );
			//				vectorFieldPositions.add( new Vector3f(pos));
			//				vectorFieldDirections.add(headTangent);
			//			}
			//			if ( newPos == null )
			//			{
			//				Vector3f tailTangent = testSphereTail( wormBlur, new Vector3f(pos), uniqueNormals, minTailRadius, minPeakVal );
			//				if ( tailTangent != null )
			//				{
			//					tailCenter = new Vector3f(pos);
			////					System.err.println( "Found tail " + tailCenter );
			//					vectorFieldPositions.add(0, tailCenter);
			//					vectorFieldDirections.add(0, tailTangent);
			//					
			//					tailPositions.add( new Vector3f(tailCenter) );
			//					tailDirections.add( tailTangent );
			//				}
			//			}

		}
//		System.err.println( "Time testSphere = " + computeElapsedTime(start) );

//		System.err.println( "Total points " + vectorFieldPositions.size() );

		//		if ( true )
		//		{
		//			return showPoints( wormImage, vectorFieldPositions );
		//		}


		if ( threadStopped )
		{
			progressBar.setVisible(false);
			progressBar.dispose();
			return null;
		}

		start = System.currentTimeMillis();

		VOI tracks = traceTracks( wormBlur, minPeakVal, vectorFieldPositions, vectorFieldDirections );
		if ( tracks != null )
		{
			tracks = averageTracks( wormBlur, tracks );
		}

		VOIContour longest = findLongest(tracks);
		//		wormBlur.resetVOIs();
		//		short sID = (short)(wormBlur.getVOIs().getUniqueID());
		//		VOI longestTrack = new VOI( sID, "longestTrack" );
		//		longestTrack.getCurves().add(longest);
		//		wormBlur.registerVOI(longestTrack);
		//		
		//		wormBlur.calcMinMax();
		//		new ViewJFrameImage(wormBlur);

//		System.err.println( "Longest path found : " + longest.size() );
		VOI path = extendPath(wormImage, wormBlur, minVal, tailPositions, longest);

//		System.err.println( "Time path = " + computeElapsedTime(start) );
		
		progressBar.setVisible(false);
		progressBar.dispose();
		return path;
	}

	public void runAlgorithm()
	{
		if ( transformFileName != null )
		{
			extractPlaneSlices(transformFileName);
		}
		else if ( wormImage.getVOIs().size() == 0 )
		{
			VOI path = findHeadTail(wormImage);
			if ( path != null )
			{
				wormImage.registerVOI(path);
				saveVOI( wormImage.getImageName() + "_path", wormImage.getImageDirectory(), wormImage, path );
				MipavUtil.displayInfo( "Path file saved as " + wormImage.getImageName() + "_path" );
				new ViewJFrameImage(wormImage);
			}
		}
		else if ( wormImage.getVOIs().size() > 0 )
		{
			Vector<VOIBase> kVectors = wormImage.getVOIs().VOIAt(0).getCurves();

			float[] xPoints = new float[kVectors.size()];
			float[] yPoints = new float[kVectors.size()];
			float[] zPoints = new float[kVectors.size()];
			for ( int i = 0; i < kVectors.size(); i++ )
			{
				Vector3f temp = kVectors.elementAt(i).elementAt(0);
				xPoints[i] = temp.X;
				yPoints[i] = temp.Y;
				zPoints[i] = temp.Z;
			}

			interpolatePointsTangents( xPoints, yPoints, zPoints );
			extractPlaneSlices();
		}

		setCompleted(true);
	}
	
	public void setDiameter( float headSize, float tailSize, float maxSize )
	{
		this.headDiameter = headSize;
		this.tailDiameter = tailSize;
		this.maxDiameter = maxSize;
	}


	public void setFill( boolean fillData )
	{
		this.fillData = fillData;
	}


	public void setOutput( boolean displayOriginal, boolean displayMask )
	{
		this.displayOriginal = displayOriginal;
		this.displayMask = displayMask;
	}


	public void setSaveTransform( boolean saveTransform )
	{
		this.saveTransform = saveTransform;
	}

	public void setTransformFile( String file )
	{
		transformFileName = new String(file);
	}



	public void setWormLength( float length )
	{
		this.wormLength = length;
	}

	private boolean allOverlap( VOIContour test, Vector<Vector3f> overlapList )
	{
		for ( int i = 0; i < test.size(); i++ )
		{
			if ( !overlapList.contains( test.elementAt(i) ) )
			{
				return false;
			}
		}
		return true;
	}


	private VOI averageTracks( ModelImage image, VOI tracks ) 
	{
		VOI newTracks = averageTracks(tracks);
		removeDuplicates(newTracks);		
		VOI newTracks2 = averageTracks(newTracks);
		removeDuplicates(newTracks2);	
		mergeTracks(image, newTracks2);
		jointracks(newTracks2);	
		return newTracks2;
	}


	private VOI averageTracks( VOI tracks ) 
	{
		float minRadius = ( headDiameter/2f) - 5;
		VOI newTracks = new VOI( (short)(tracks.getID() + 1), "newTracks" );
		for ( int i = 0; i < tracks.getCurves().size(); i++ )
		{
			VOIContour track1 = (VOIContour) tracks.getCurves().elementAt(i);
			VOIContour avg = new VOIContour(false);
			for ( int p1 = 0; p1 < track1.size(); p1++ )
			{
				Vector3f pt1 = track1.elementAt(p1);
				Vector3f averagePt = new Vector3f(pt1);
				int averageCount = 1;
				for ( int j = 0; j < tracks.getCurves().size(); j++ )
				{
					VOIContour track2 = (VOIContour) tracks.getCurves().elementAt(j);
					if ( track1 != track2 )
					{
						for ( int p2 = 0; p2 < track2.size(); p2++ )
						{
							Vector3f pt2 = track2.elementAt(p2);
							if ( pt1.distance(pt2) < minRadius)
							{
								averagePt.add(pt2);
								averageCount++;
							}
						}
					}
				}
				averagePt.scale(1f/averageCount);
				averagePt.X = Math.round(averagePt.X);
				averagePt.Y = Math.round(averagePt.Y);
				averagePt.Z = Math.round(averagePt.Z);
				if ( !avg.contains(averagePt) )
				{
					avg.add(averagePt);
				}
				//				System.err.println( "replacing " + pt1 + "    with    " + averagePt );
			}
			avg.update( new ColorRGBA( 1, 1, 0, 1 ) );
			newTracks.getCurves().add(avg);
		}
		return newTracks;
	}



	private ModelImage blur()
	{
		final String name = JDialogBase.makeImageName(wormImage.getImageName(), "_gblur");

		float[] sigmas = new float[] { 3, 3,3 * getCorrectionFactor() };
		OpenCLAlgorithmGaussianBlur blurAlgo;

		resultImage = new ModelImage( wormImage.getType(), wormImage.getExtents(), name );
		JDialogBase.updateFileInfo( wormImage, resultImage );
		blurAlgo = new OpenCLAlgorithmGaussianBlur(resultImage, wormImage, 
				sigmas, true, true, false);   

		blurAlgo.setRed(true);
		blurAlgo.setGreen(true);
		blurAlgo.setBlue(true);
		blurAlgo.run();

		return blurAlgo.getDestImage();
	}


	private Vector<Vector3f> checkOverlap( VOIContour longest, VOIContour next )
	{
		Vector<Vector3f> overlapList = null;
		for ( int i = 0; i < longest.size(); i++ )
		{

			for ( int j = 0; j < next.size(); j++ )
			{
				if ( nearEquals( longest.elementAt(i), next.elementAt(j) ) )
				{
					if ( overlapList == null )
					{
						//						System.err.print( "checkOverlap " );
						overlapList = new Vector<Vector3f>();
					}
					//					System.err.print( i + "," + j + "  " );
					overlapList.add( next.elementAt(j) );
				}
			}
		}
		if ( overlapList != null )
		{
			//			System.err.println( "" );
		}
		return overlapList;
	}

	private boolean checkTrackOverlap( VOI tracks )
	{
		for ( int i = 0; i < tracks.getCurves().size(); i++ )
		{
			for ( int j = i+1; j < tracks.getCurves().size(); j++ )
			{
				Vector<Vector3f> overlapList = checkOverlap( (VOIContour)(tracks.getCurves().elementAt(i)),
						(VOIContour)(tracks.getCurves().elementAt(j)) );

				if ( overlapList != null )
				{
					return true;
				}
			}
		}
		return false;
	}


	private void createTestImage()
	{
		ModelImage testImage = new ModelImage( ModelStorageBase.ARGB_FLOAT, new int[]{256,256,256}, "rainBowTest" );
		ModelLUT kLUT = new ModelLUT(ModelLUT.SPECTRUM, 256, new int[]{4,256});
		final float[][] lutData = kLUT.exportRGB_LUT(false);

		int startX = 256 / 8;
		int endX = startX * 3;
		int half = 256/2;

		for ( int y = 96; y < 160; y++ )
		{
			for ( int x = startX; x < endX; x++ )
			{
				float val = (endX - x)*4;
				int index = (int) Math.max(0, val);
				index = Math.min(255, index);
				testImage.setC(x, y, half, 0, 1);
				testImage.setC(x, y, half, 1, lutData[0][index]);
				testImage.setC(x, y, half, 2, lutData[1][index]);
				testImage.setC(x, y, half, 3, lutData[2][index]);
			}
		}
		VOI kPVOI = new VOI( (short) 0,  "resultVOI", VOI.POINT, 0 );
		for ( int z = 10; z <= half; z++ )
		{
			kPVOI.importPoint( new Vector3f( 64, 128, z ) );
		}

		ModelImage mirror = null;
		ModelImage sumImage = new ModelImage( ModelStorageBase.ARGB_FLOAT, new int[]{256,256,256}, "rainBow" );
		TransMatrix xfrm;
		Vector3f center = new Vector3f( 256f/2f, 256f/2f, 256f/2f );
		Vector3f pointA = new Vector3f( -64, 0, 0 );
		Vector3f pointB = new Vector3f();
		for ( int i = 0; i < 180; i++ )
		{			
//			System.err.println( "Rotating " + i );

			xfrm = new TransMatrix(4);
			xfrm.setRotate( 0, i+1, 0, TransMatrix.DEGREES );

			xfrm.transformAsPoint3Df(pointA, pointB);
			pointB.add( 128, 128, 128 );
//			System.err.println( pointB );
			kPVOI.importPoint( new Vector3f( pointB.X, pointB.Y, pointB.Z ) );


			//			AlgorithmTransform rotAlg = new AlgorithmTransform( testImage, xfrm, AlgorithmTransform.NEAREST_NEIGHBOR, 1.0f,
			//					1.0f, 1.0f, 256, 256, 256, testImage.getUnitsOfMeasure(), false, true, false, true, center );
			//			rotAlg.setFillValue(0);
			//			rotAlg.setUpdateOriginFlag(true);
			//			rotAlg.setUseScannerAnatomical(false);
			//			rotAlg.run();
			//			ModelImage result = rotAlg.getTransformedImage();
			//
			//			
			//			for ( int z = 0; z < 256; z++ )
			//			{
			//				for ( int y = 0; y < 256; y++ )
			//				{
			//					for ( int x = 0; x < 256; x++ )
			//					{
			//						for ( int c = 0; c < 4; c++ )
			//						{
			//							sumImage.setC( x, y, z, c, Math.max( sumImage.getFloatC( x, y, z, c ), 
			//									Math.max(testImage.getFloatC( x, y, z, c), result.getFloatC( x, y, z, c) ) ) );
			//						}
			//					}
			//				}
			//			}
			//			if ( i == 179 )
			//			{
			//				mirror = result;
			//			}
			//			else
			//			{
			//				result.disposeLocal();
			//				result = null;
			//			}
			//			rotAlg.disposeLocal();
			//			rotAlg = null;
			//			System.gc();
			xfrm = null;
		}
		for ( int z = half; z >= 10; z-- )
		{
			kPVOI.importPoint( new Vector3f( 192, 128, z ) );
		}

		saveVOI("originalPoints", wormImage.getImageDirectory(), sumImage, kPVOI);

		//		for ( int z = 10; z <= half; z++ )
		//		{
		//			for ( int y = 0; y < 256; y++ )
		//			{
		//				for ( int x = 0; x < 256; x++ )
		//				{
		//					for ( int c = 0; c < 4; c++ )
		//					{
		//						sumImage.setC( x, y, z, c, Math.max(testImage.getFloatC( x, y, half, c), mirror.getFloatC( x, y, half, c) ) );
		//					}
		//				}
		//			}
		//		}
		//		
		//		mirror.calcMinMax();
		//		new ViewJFrameImage(mirror);
		//		
		//		testImage.calcMinMax();
		//		new ViewJFrameImage(testImage);
		//		
		//		sumImage.calcMinMax();
		//		new ViewJFrameImage(sumImage);
	}


	private VOI extendPath( ModelImage image, ModelImage imageBlur, float minVal, Vector<Vector3f> tailPositions, VOIContour path )
	{
		short sID = (short)(image.getVOIs().getUniqueID());
		VOI pathPoints = new VOI( sID, "path" );
		pathPoints.setCurveType( VOI.POINT );

		path.trimPoints( 0.8f, true );

		Vector3f first = path.firstElement();
		Vector3f last = path.lastElement();

		boolean firstTail = false;
		float minDistFirst = Float.MAX_VALUE;
		float minDistLast = Float.MAX_VALUE;
		int minIndexFirst = -1;
		int minIndexLast = -1;
		for ( int i = 0; i < tailPositions.size(); i++ )
		{
			float dist = first.distance( tailPositions.elementAt(i) );
			if ( dist < minDistFirst )
			{
				minDistFirst = dist;
				minIndexFirst = i;
			}
			dist = last.distance( tailPositions.elementAt(i) );
			if ( dist < minDistLast )
			{
				minDistLast = dist;
				minIndexLast = i;
			}				
		}

		if ( minDistFirst < minDistLast )
		{
			firstTail = true;
		}
		else if ( (minDistFirst == minDistLast) && (minDistFirst == Float.MAX_VALUE) )
		{

			float firstV = imageBlur.getFloatTriLinearBounds( first.X, first.Y, first.Z );
			float lastV = imageBlur.getFloatTriLinearBounds( last.X, last.Y, last.Z );
			if ( firstV > lastV )
			{
				firstTail = true;
			}			
		}		



		float totalLength = 0;
		Vector3f tailStart = tailPositions.elementAt( minIndexLast );
		if ( firstTail )
		{
			if ( minIndexFirst != -1 )
			{
				path.add(0, tailPositions.elementAt( minIndexFirst ) );
			}
			for ( int i = 0; i < path.size(); i++ )
			{
				if ( i == 0 )
				{
					totalLength += tailStart.distance( path.elementAt(i) );
				}
				else
				{
					totalLength += path.elementAt(i-1).distance( path.elementAt(i) );
				}
				pathPoints.importPoint( new Vector3f( path.elementAt(i) ) );
				if ( totalLength >= wormLength )
				{
					//					break;
				}
			}
		}
		else
		{
			if ( minIndexLast != -1 )
			{
				path.add(tailPositions.elementAt( minIndexLast ) );
			}
			for ( int i = path.size() - 1; i >= 0; i-- )
			{
				if ( i == (path.size() - 1) )
				{
					totalLength += tailStart.distance( path.elementAt(i) );
				}
				else
				{
					totalLength += path.elementAt(i+1).distance( path.elementAt(i) );
				}
				pathPoints.importPoint( new Vector3f( path.elementAt(i) ) );
				if ( totalLength >= wormLength )
				{
					break;
				}
			}			
		}
//		System.err.println( "extendPath  " + totalLength + " " + wormLength );

		// Add onto the tail:
		first = pathPoints.getCurves().firstElement().elementAt(0);
		Vector3f dir = Vector3f.sub( first, pathPoints.getCurves().elementAt(2).elementAt(0) );
		dir.normalize();
		dir.scale(10);
		Vector3f next = Vector3f.add( first, dir );
		while ( imageBlur.getFloatTriLinearBounds( next.X, next.Y, next.Z ) > minVal )
		{
			VOIPoint voiPt = new VOIPoint( VOI.POINT, next);
			voiPt.update(new ColorRGBA(1,0,1,1));
			pathPoints.getCurves().add( 0, voiPt );
			next = Vector3f.add( next, dir );
		}

		// Add onto the head:
		int size = pathPoints.getCurves().size();
		last = pathPoints.getCurves().lastElement().elementAt(0);
		dir = Vector3f.sub( last, pathPoints.getCurves().elementAt(size-3).elementAt(0) );
		dir.normalize();
		dir.scale(10);
		next = Vector3f.add( last, dir );
		while ( (imageBlur.getFloatTriLinearBounds( next.X, next.Y, next.Z ) > minVal) ) //&& (totalLength < wormLength) )
		{
			totalLength += pathPoints.getCurves().lastElement().elementAt(0).distance(next);
			VOIPoint voiPt = new VOIPoint( VOI.POINT, next);
			voiPt.update(new ColorRGBA(0,0,1,1));
			pathPoints.getCurves().add( voiPt );
			next = Vector3f.add( next, dir );
		}
//		System.err.println( "extendPath  " + totalLength + " " + wormLength );

		//		sID = (short) image.getVOIs().getUniqueID();
		//		VOI pathVOI = new VOI( sID, "longestPath", VOI.CONTOUR, 1.0f );
		//		pathVOI.getCurves().add(path);
		//		image.registerVOI( pathVOI );


		return pathPoints;
	}

	private void extendTracks( ModelImage image, float minPeakVal, Vector<Vector3f> positions, Vector<Vector3f> directions,
			Vector3f previousPt, Vector3f start, Vector3f dir, VOIContour track, boolean forward )
	{
		int step = 10;
		float minRadius = (headDiameter/2f) - 5;
		Box verts = new Box();
		verts.corners[0] = new Vector3f(-1,-1, 0);
		verts.corners[1] = new Vector3f( 1,-1, 0);
		verts.corners[2] = new Vector3f( 1, 1, 0);
		verts.corners[3] = new Vector3f(-1, 1, 0);

		Vector3f direction = new Vector3f(dir);
		if ( previousPt != null )
		{
			direction = Vector3f.sub(start,  previousPt);
			//			Vector3f directionN = Vector3f.sub(start,  previousPt);
			//			if ( direction != null )
			//			{
			//				float angle = direction.angle(directionN);    
			//				if ( angle > minAngle90 )
			//				{
			//					direction.neg();
			//				}
			//				direction.add(directionN);
			//				direction.scale(0.5f);
			//			}
		}
		direction.normalize();
		Vector3f next = new Vector3f();
		next.scaleAdd( step, direction, start );

		float angle = dir.angle(Vector3f.UNIT_Z);    
		Vector3f rotationAxis = Vector3f.cross( Vector3f.UNIT_Z, dir );
		rotationAxis.normalize();

		// Create a rotation angle-degrees about the rotation axis:
		Matrix3f transformTotal = new Matrix3f();
		transformTotal.fromAxisAngle( rotationAxis, angle );

		Box[] results = getBoundingBox(transformTotal, verts, next, 30 );


		try {
			next = image.findMax( 0, 0, results[0].corners, minRadius/2f, false);
			if ( (next != null) && !track.contains(next) && (image.getFloatTriLinearBounds( next.X, next.Y, next.Z) > (minPeakVal/2f)) )
			{
				int nextIndex = positions.indexOf(next);
				Vector3f nextDir = null;
				if ( nextIndex >= 0 )
				{
					nextDir = directions.elementAt(nextIndex);
//					System.err.println( "extendTracks " + nextDir );
				}

				Vector3f dir2 = Vector3f.sub(next, start);
				dir2.normalize();
				if ( direction.angle(dir2) < minAngle45)
				{
					if ( forward )
					{
						track.add( next );
					}
					else
					{
						track.add( 0, next );
					}
					extendTracks( image, minPeakVal, positions, directions, start, next, dir2, track, forward );
				}
				else if ( previousPt != null )
				{
					dir2 = Vector3f.sub(next, previousPt);
					dir2.normalize();
					if ( direction.angle(dir2) < minAngle30)
					{
						track.remove(start);
						if ( forward )
						{
							track.add( next );
						}
						else
						{
							track.add( 0, next );
						}
						extendTracks( image, minPeakVal, positions, directions, previousPt, next, dir2, track, forward );
					}
				}
			}
		} catch (IOException e) {
		}
	}

	private void extractPlaneSlices()
	{

		Vector3f kNormal = new Vector3f( Vector3f.UNIT_X );
		Vector3f kRight = new Vector3f( );
		Vector3f kUp = new Vector3f( );
		origBox = findClosestNormal(curvePositions, kNormal, kUp, kRight);
		origNormal = new Vector3f(kNormal);

		findClosestExtentsResolutions( origBox, kNormal, wormImage.getExtents(), sourceResolutions, wormImage.getUnitsOfMeasure() );


		short sID = (short)(wormImage.getVOIs().getUniqueID());
		samplingPlanes = new VOI(sID, "samplingPlanes");

		smoothTangents( origBox, kNormal, kUp, kRight );

		//		wormImage.resetVOIs();		
		//		testBoxIntersections();


		resultImage = new ModelImage(wormImage.getType(), resultExtents, wormImage.getImageName() + "_straigntened");
		JDialogBase.updateFileInfo( wormImage, resultImage );
		resultImage.setResolutions(resultResolutions);



		ObjectOutputStream objstream = null;
		String transformFileName = wormImage.getImageDirectory() + wormImage.getImageName() + "_transform";
		if ( saveTransform )
		{
			try {
				objstream = new ObjectOutputStream(new FileOutputStream(transformFileName));

				// write origNormal:
				objstream.writeFloat(origNormal.X); objstream.writeFloat(origNormal.Y); objstream.writeFloat(origNormal.Z);

				// write diameter
				objstream.writeInt(diameter);

				// write outputResolutions:
				objstream.writeFloat(outputResolutions[0]);
				objstream.writeFloat(outputResolutions[1]);
				objstream.writeFloat(outputResolutions[2]);
				// write units:
				objstream.writeInt(resultUnits[0]);
				objstream.writeInt(resultUnits[1]);
				objstream.writeInt(resultUnits[2]);

				// write diameters:
				objstream.writeFloat(tailDiameter);
				objstream.writeFloat(headDiameter);
				objstream.writeFloat(maxDiameter);

				// write result resolutions (z)
				objstream.writeFloat(resultResolutions[2]);

				// write result extents (z)
				objstream.writeInt(resultExtents[2]);
				
				// write number of bounding boxes:
				objstream.writeInt(boundingBoxes.size());
			} catch (final FileNotFoundException e) {
				e.printStackTrace();
			} catch (final IOException e) {
				e.printStackTrace();
			}
		}





		int colorFactor = wormImage.isColorImage() ? 4 : 1;

		float[][] values = new float[resultExtents[2]][resultExtents[0] * resultExtents[1] * colorFactor]; 

		BitSet duplicateMask = new BitSet( wormImage.getExtents()[0] * wormImage.getExtents()[1] * wormImage.getExtents()[2] );

		Vector3f lpsOrigin = new Vector3f();
		float size = boundingBoxes.size()-1;
		float size1 = size / 3;
		float size2 = size1 + size1;
		float size3 = size - size2;
		float diameterInterp = tailDiameter;
		for( int i = 0; i < boundingBoxes.size(); i++ )
		{
			Box box = boundingBoxes.elementAt(i);
			if ( i < size1 )
			{
				diameterInterp = ((size1 - i)/size1) * tailDiameter + (i/size1)*maxDiameter;
			}
			else if ( i > size2 )
			{
				diameterInterp = ((size3 - (i-size2))/size3) * maxDiameter + ((i-size2)/size3)*headDiameter;
			}
			else
			{
				diameterInterp = maxDiameter;
			}
			//			System.err.println( i + "    " + diameterInterp );
			if ( fillData && (i > 0) )
			{
				System.arraycopy(values[i-1], 0, values[i], 0, values[i].length);
			}
			try {
				wormImage.exportDiagonal( duplicateMask, 0, i, resultExtents, box.corners, diameterInterp/2f, !fillData, values[i], true);

				if ( i == 0 )
				{
					MipavCoordinateSystems.fileToScanner( box.corners[0], lpsOrigin, wormImage );
				}

				resultImage.importData(i*values[i].length, values[i], false);
			}catch(IOException e) {
				e.printStackTrace();
			}

			if ( objstream != null )
			{
				try {
					// write diameter:
					objstream.writeFloat(diameterInterp);

					// write box:
					for ( int j = 0; j < 4; j++ )
					{
						objstream.writeFloat(box.corners[j].X); objstream.writeFloat(box.corners[j].Y); objstream.writeFloat(box.corners[j].Z);
					}
				} catch (final FileNotFoundException e) {
					e.printStackTrace();
				} catch (final IOException e) {
					e.printStackTrace();
				}
			}
		}

		if ( displayMask )
		{
			ModelImage duplicateMaskImage = new ModelImage( ModelStorageBase.BOOLEAN, wormImage.getExtents(), wormImage.getImageName() + "_mask" );
			try {
				duplicateMaskImage.importData( 0, duplicateMask, true );
				new ViewJFrameImage(duplicateMaskImage);
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		resultImage.calcMinMax();
		resultImage.setImageName( wormImage.getImageName() + "_straightened_isotropic" );
		//		ModelImage.saveImage( resultImage, wormImage.getImageName() + "_straightened_isotropic.tif", wormImage.getImageDirectory() );
		//		new ViewJFrameImage(resultImage);




		if ( (outputResolutions[0] != resultResolutions[0]) || (outputResolutions[1] != resultResolutions[1]) ||
				(outputResolutions[2] != resultResolutions[2]) )
		{
			int oXdim = Math.round( 1 + (((resultExtents[0] - 1) * resultResolutions[0]) / outputResolutions[0]));
			int oYdim = Math.round( 1 + (((resultExtents[1] - 1) * resultResolutions[1]) / outputResolutions[1]));
			int oZdim = Math.round( 1 + (((resultExtents[2] - 1) * resultResolutions[2]) / outputResolutions[2]));
			float oXres = outputResolutions[0];
			float oYres = outputResolutions[1];
			float oZres = outputResolutions[2];

			int iXdim = resultExtents[0];
			int iYdim = resultExtents[1];
			int iZdim = resultExtents[2];
			float iXres = resultResolutions[0];
			float iYres = resultResolutions[1];
			float iZres = resultResolutions[2];

			TransMatrix xfrm = new TransMatrix(4);
			float Sx = ( (oXdim - 1) * oXres) / ( (iXdim - 1) * iXres);
			float Sy = ( (oYdim - 1) * oYres) / ( (iYdim - 1) * iYres);
			float Sz = ( (oZdim - 1) * oZres) / ( (iZdim - 1) * iZres);
			xfrm.setZoom(Sx, Sy, Sz);


			AlgorithmTransform  algoTrans = new AlgorithmTransform(resultImage, xfrm, AlgorithmTransform.TRILINEAR, 
					oXres, oYres, oZres, oXdim, oYdim, oZdim, resultUnits,
					false, true, false, false, null);
			algoTrans.setFillValue((float) resultImage.getMin());
			algoTrans.setUpdateOriginFlag(true);
			algoTrans.setUseScannerAnatomical(false);

			algoTrans.run();

			resultImage = algoTrans.getTransformedImage();

			resultImage.calcMinMax();
			resultImage.setImageName( wormImage.getImageName() + "_straightened_non_isotropic" );
			//    		ModelImage.saveImage( resultImage, wormImage.getImageName() + "_straightened_non_isotropic.tif", wormImage.getImageDirectory() );
			//    		new ViewJFrameImage(resultImage);                       
		}


		if ( !origNormal.equals( Vector3f.UNIT_Z ) )
		{
			// rotate the image back:
			AlgorithmRotate kRotate = null;
			if ( origNormal.equals( Vector3f.UNIT_X ) )
			{
				kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.Y_AXIS_PLUS);
			}
			else if ( origNormal.equals( Vector3f.UNIT_X_NEG ) )
			{
				kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.Y_AXIS_MINUS);
			}
			else if ( origNormal.equals( Vector3f.UNIT_Y ) )
			{
				kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.X_AXIS_MINUS);
			}
			else if ( origNormal.equals( Vector3f.UNIT_Y_NEG ) )
			{
				kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.X_AXIS_PLUS);
			}
			else if ( origNormal.equals( Vector3f.UNIT_Z_NEG ) )
			{
				kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.Y_AXIS_180);
			}
			if ( kRotate != null )
			{
				kRotate.run();
				resultImage = kRotate.returnImage();

				resultImage.calcMinMax();
				resultImage.setImageName( wormImage.getImageName() + "_straightened_rotated" );
				//        		ModelImage.saveImage( resultImage, wormImage.getImageName() + "_straightened_rotated.tif", wormImage.getImageDirectory() );
				//        		new ViewJFrameImage(resultImage);   
			}

		}

		int[][] padR = new int[][]{{0,0},{0,0},{0,0}};
		boolean bPadR = false;
		for ( int i = 0; i < 3; i++ )
		{
			if ( wormImage.getExtents()[i] > resultImage.getExtents()[i] )
			{
				padR[i][0] = (int) (Math.floor((wormImage.getExtents()[i] - resultImage.getExtents()[i])/2f));
				padR[i][1] = (int) (Math.ceil((wormImage.getExtents()[i] - resultImage.getExtents()[i])/2f));
				bPadR = true;
			}
		}
		if ( displayOriginal )
		{
			//			wormImage.resetVOIs();		
			//			wormImage.registerVOI( samplingPlanes );
			wormImage.calcMinMax();
			new ViewJFrameImage(wormImage);
		}
		if ( bPadR )
		{
			ModelImage resultPad = (ModelImage)resultImage.clone();
			resultPad.setImageName( resultImage.getImageName() + "_pad" );
			AlgorithmAddMargins kPad = new AlgorithmAddMargins(resultPad, padR[0], padR[1], padR[2] );
			kPad.run();
			resultPad = kPad.getDestImage();
			resultPad.calcMinMax();
			new ViewJFrameImage(resultPad);
			//            System.err.println( resultPad.getExtents()[0] + " " + resultPad.getExtents()[1] + " " + resultPad.getExtents()[2] );
		}
		else
		{
			new ViewJFrameImage(resultImage);
		}        


		if ( saveTransform )
		{
			try {
				objstream.close();
				MipavUtil.displayInfo( "Transforms saved to: " + wormImage.getImageName() + "_transform");
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		if ( !displayOriginal )
		{
			wormImage.disposeLocal();
			wormImage = null;
		}
	}


	private void extractPlaneSlices( String file )
	{
		ObjectInputStream objstream = null;
		try {
			objstream = new ObjectInputStream(new FileInputStream(file));
		} catch (final FileNotFoundException e) {
			e.printStackTrace();
		} catch (final IOException e) {
			e.printStackTrace();
		}

		if ( objstream == null )
		{
			MipavUtil.displayError( "Cannot read transform file " + file );
			return;
		}


		try {
			origNormal = new Vector3f( objstream.readFloat(), objstream.readFloat(), objstream.readFloat() );

			diameter = objstream.readInt();
			outputResolutions[0] = objstream.readFloat();
			outputResolutions[1] = objstream.readFloat();
			outputResolutions[2] = objstream.readFloat();
			resultUnits[0] = objstream.readInt();
			resultUnits[1] = objstream.readInt();
			resultUnits[2] = objstream.readInt();


			tailDiameter = objstream.readFloat();
			headDiameter = objstream.readFloat();
			maxDiameter = objstream.readFloat();

			resultResolutions[0] = 1;
			resultResolutions[1] = 1;
			resultResolutions[2] = objstream.readFloat();
			resultExtents[0] = diameter;
			resultExtents[1] = diameter;
			resultExtents[2] = objstream.readInt();


			resultImage = new ModelImage(wormImage.getType(), resultExtents, wormImage.getImageName() + "_straigntened");
			JDialogBase.updateFileInfo( wormImage, resultImage );
			resultImage.setResolutions(resultResolutions);


			int colorFactor = wormImage.isColorImage() ? 4 : 1;

			float[][] values = new float[resultExtents[2]][resultExtents[0] * resultExtents[1] * colorFactor]; 

			BitSet duplicateMask = new BitSet( wormImage.getExtents()[0] * wormImage.getExtents()[1] * wormImage.getExtents()[2] );

			boundingBoxes.clear();
			int numBoundingBoxes = objstream.readInt();
			Vector<Float> diameterInterpList = new Vector<Float>();
			for( int i = 0; i < numBoundingBoxes; i++ )
			{
				float diameterInterp = objstream.readFloat();
				diameterInterpList.add( diameterInterp );
				
				Box box = new Box();
				for ( int j = 0; j < 4; j++ )
				{
					box.corners[j] = new Vector3f( objstream.readFloat(), objstream.readFloat(), objstream.readFloat() );
				}
				boundingBoxes.add(box);
			}

			Vector3f lpsOrigin = new Vector3f();
			for( int i = 0; i < boundingBoxes.size(); i++ )
			{
				float diameterInterp = diameterInterpList.elementAt(i);
				Box box = boundingBoxes.elementAt(i);
				if ( fillData && (i > 0) )
				{
					System.arraycopy(values[i-1], 0, values[i], 0, values[i].length);
				}
				try {
					wormImage.exportDiagonal( duplicateMask, 0, i, resultExtents, box.corners, diameterInterp/2f, !fillData, values[i], true);

					if ( i == 0 )
					{
						MipavCoordinateSystems.fileToScanner( box.corners[0], lpsOrigin, wormImage );
					}

					resultImage.importData(i*values[i].length, values[i], false);
				} catch(IOException e) {
					e.printStackTrace();
				}
			}

			resultImage.calcMinMax();
			resultImage.setImageName( wormImage.getImageName() + "_straightened_isotropic" );


			if ( (outputResolutions[0] != resultResolutions[0]) || (outputResolutions[1] != resultResolutions[1]) ||
					(outputResolutions[2] != resultResolutions[2]) )
			{
				int oXdim = Math.round( 1 + (((resultExtents[0] - 1) * resultResolutions[0]) / outputResolutions[0]));
				int oYdim = Math.round( 1 + (((resultExtents[1] - 1) * resultResolutions[1]) / outputResolutions[1]));
				int oZdim = Math.round( 1 + (((resultExtents[2] - 1) * resultResolutions[2]) / outputResolutions[2]));
				float oXres = outputResolutions[0];
				float oYres = outputResolutions[1];
				float oZres = outputResolutions[2];

				int iXdim = resultExtents[0];
				int iYdim = resultExtents[1];
				int iZdim = resultExtents[2];
				float iXres = resultResolutions[0];
				float iYres = resultResolutions[1];
				float iZres = resultResolutions[2];

				TransMatrix xfrm = new TransMatrix(4);
				float Sx = ( (oXdim - 1) * oXres) / ( (iXdim - 1) * iXres);
				float Sy = ( (oYdim - 1) * oYres) / ( (iYdim - 1) * iYres);
				float Sz = ( (oZdim - 1) * oZres) / ( (iZdim - 1) * iZres);
				xfrm.setZoom(Sx, Sy, Sz);


				AlgorithmTransform  algoTrans = new AlgorithmTransform(resultImage, xfrm, AlgorithmTransform.TRILINEAR, 
						oXres, oYres, oZres, oXdim, oYdim, oZdim, resultUnits,
						false, true, false, false, null);
				algoTrans.setFillValue((float) resultImage.getMin());
				algoTrans.setUpdateOriginFlag(true);
				algoTrans.setUseScannerAnatomical(false);

				algoTrans.run();

				resultImage = algoTrans.getTransformedImage();

				resultImage.calcMinMax();
				resultImage.setImageName( wormImage.getImageName() + "_straightened_non_isotropic" );
			}


			if ( !origNormal.equals( Vector3f.UNIT_Z ) )
			{
				// rotate the image back:
				AlgorithmRotate kRotate = null;
				if ( origNormal.equals( Vector3f.UNIT_X ) )
				{
					kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.Y_AXIS_PLUS);
				}
				else if ( origNormal.equals( Vector3f.UNIT_X_NEG ) )
				{
					kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.Y_AXIS_MINUS);
				}
				else if ( origNormal.equals( Vector3f.UNIT_Y ) )
				{
					kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.X_AXIS_MINUS);
				}
				else if ( origNormal.equals( Vector3f.UNIT_Y_NEG ) )
				{
					kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.X_AXIS_PLUS);
				}
				else if ( origNormal.equals( Vector3f.UNIT_Z_NEG ) )
				{
					kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.Y_AXIS_180);
				}
				if ( kRotate != null )
				{
					kRotate.run();
					resultImage = kRotate.returnImage();

					resultImage.calcMinMax();
					resultImage.setImageName( wormImage.getImageName() + "_straightened_rotated" );
				}

			}

			int[][] padR = new int[][]{{0,0},{0,0},{0,0}};
			boolean bPadR = false;
			for ( int i = 0; i < 3; i++ )
			{
				if ( wormImage.getExtents()[i] > resultImage.getExtents()[i] )
				{
					padR[i][0] = (int) (Math.floor((wormImage.getExtents()[i] - resultImage.getExtents()[i])/2f));
					padR[i][1] = (int) (Math.ceil((wormImage.getExtents()[i] - resultImage.getExtents()[i])/2f));
					bPadR = true;
				}
			}


			if ( bPadR )
			{
				ModelImage resultPad = (ModelImage)resultImage.clone();
				resultPad.setImageName( resultImage.getImageName() + "_pad" );
				AlgorithmAddMargins kPad = new AlgorithmAddMargins(resultPad, padR[0], padR[1], padR[2] );
				kPad.run();
				resultPad = kPad.getDestImage();
				resultPad.calcMinMax();
				new ViewJFrameImage(resultPad);
			}
			else
			{
				new ViewJFrameImage(resultImage);
			}        

		} catch (IOException e1) {
			e1.printStackTrace();
		}

		try {
			objstream.close();
		} catch (IOException e) {
			e.printStackTrace();
		}

		wormImage.disposeLocal();
		wormImage = null;
	}

	private Vector3f findCenter( Vector3f center, Vector3f planeVector, Vector3f tanVector, ModelImage image,	
			float innerDiameter, float minPeakVal, float minVal )
	{

		Matrix3f rotationM = new Matrix3f();

		Vector3f rotVector = new Vector3f();
		Vector3f temp = new Vector3f();


		int iRadialSamples = 30;
		float fInvRS = 1.0f/iRadialSamples;
		boolean minFoundAll = true;
		Vector3f radialCenter = new Vector3f();
		int radialCount = 0;

		for (int iR = 0; iR < iRadialSamples; iR++)
		{
			float fAngle = Mathf.TWO_PI*fInvRS*iR;
			rotationM.fromAxisAngle( tanVector,  fAngle);
			temp.copy( planeVector );
			rotationM.mult(temp, rotVector);

			boolean minFound = false;
			int minStartIndex = -1, minEndIndex = -1;
			double minValFound = Float.MAX_VALUE;
			for ( int j = 1; j < innerDiameter; j++ )
			{
				temp.copy(rotVector);
				temp.scale(j);
				temp.add( center );

				double val = image.getFloatTriLinearBounds( temp.X, temp.Y, temp.Z );
				//				double valC = imageC.getFloatTriLinearBounds( temp.X, temp.Y, temp.Z, 1);
				if ( val < minValFound )
				{
					minValFound = val;
				}
				//				if ( valC < minValFoundC )
				//				{
				//					minValFoundC = valC;
				//				}
				if ( !minFound && (val <= (minPeakVal/2.5f)) )
				{
					minFound = true;
					minStartIndex = j;
					radialCenter.add( temp );
					radialCount++;
				}
				if ( minFound && (val > (minPeakVal/2f)) )
				{
					//					minFound = false;
					minEndIndex = j;
					break;
				}
				if ( val <= minVal )
				{
					minFound = false;
					minStartIndex = -1;
					minEndIndex = -1;
					break;
				}
				minEndIndex = j;
			}
			if ( (minStartIndex != -1) &&  (minEndIndex != -1) && ((minEndIndex - minStartIndex) > (3*innerDiameter/4f)) )
			{
				//				System.err.println( (innerDiameter/4f) + "    " + (minEndIndex - minStartIndex) );
				minFound = true;
			}

			minFoundAll &= minFound;
		}
		if ( minFoundAll )
		{
			radialCenter.scale( 1f/radialCount );
			return radialCenter;
		}
		return null;
	}

	private void findClosestExtentsResolutions( Box initBox, Vector3f kNormal, int[] extents, float[] res, int[] units )
	{
		if ( kNormal.equals(Vector3f.UNIT_X ) || kNormal.equals(Vector3f.UNIT_X_NEG ) )
		{
			diameter = Math.min( extents[1], extents[2] );
			outputResolutions[0] = res[2];
			outputResolutions[1] = res[1];
			outputResolutions[2] = res[0];
			resultUnits[0] = units[2];
			resultUnits[1] = units[1];
			resultUnits[2] = units[0];
		}
		else if ( kNormal.equals(Vector3f.UNIT_Y ) || kNormal.equals(Vector3f.UNIT_Y_NEG ) )
		{
			diameter = Math.min( extents[0], extents[2] );
			outputResolutions[0] = res[0];
			outputResolutions[1] = res[2];
			outputResolutions[2] = res[1];
			resultUnits[0] = units[0];
			resultUnits[1] = units[2];
			resultUnits[2] = units[1];
		}
		else if ( kNormal.equals(Vector3f.UNIT_Z ) || kNormal.equals(Vector3f.UNIT_Z_NEG ) )
		{
			diameter = Math.min( extents[0], extents[1] );
			outputResolutions[0] = res[0];
			outputResolutions[1] = res[1];
			outputResolutions[2] = res[2];
			resultUnits[0] = units[0];
			resultUnits[1] = units[1];
			resultUnits[2] = units[2];

		}
		//diameter /= 2f;
		//		System.err.println( "Diameter " + diameter );
		if ( tailDiameter != -1 )
		{
			//	diameter = (int) Math.ceil( tailDiameter ); 
			//			System.err.println( "Diameter " + diameter );
		}
		if ( tailDiameter == -1 )
		{
			tailDiameter = diameter;
			headDiameter = diameter;
			maxDiameter = diameter;
		}

		resultResolutions[0] = 1;
		resultResolutions[1] = 1;
		resultResolutions[2] = stepSize;
		resultExtents[0] = diameter;
		resultExtents[1] = diameter;
		resultExtents[2] = curvePositions.size();


		Vector3f point = new Vector3f( extents[0]/2f, extents[1]/2f, extents[2]/2f );

		Box newBox = transformBoxSlicesInit(initBox, point, kNormal);
	}

	/**
	 * Sets up the initial bounding box for the diagonal slice sample based on the first tangent in the bspline.
	 * @param kNormal
	 * @param kTangent
	 * @return
	 */
	private Box findClosestNormal( Vector<Vector3f> contour, Vector3f kNormal, Vector3f kUp, Vector3f kRight )
	{
		// The contour is 3-dimensional (not entirely on one of the three orthogonal planes):
		Matrix3f kMat = new Matrix3f();
		float[] afScale = new float[3];

		new ApprEllipsoidFit3f( contour.size(), contour, kUp, kMat, afScale );

		Vector3f kX = new Vector3f( kMat.M00, kMat.M01, kMat.M02 );  kX.normalize();
		Vector3f kY = new Vector3f( kMat.M10, kMat.M11, kMat.M12 );  kY.normalize();
		Vector3f kZ = new Vector3f( kMat.M20, kMat.M21, kMat.M22 );  kZ.normalize();

		//Vector3f[] kBasis = new Vector3f[]{ kX, kY, kZ };
		kMat = new Matrix3f( kX, kY, kZ, false );
		Vector3f kRot = kMat.mult( Vector3f.UNIT_Z_NEG );             

		//		System.err.println( kRot );
		return findClosestNormal( kNormal, kRot, kUp, kRight );        
	}


	/**
	 * Sets up the initial bounding box for the diagonal slice sample based on the first tangent in the bspline.
	 * @param kNormal
	 * @param kTangent
	 * @return
	 */
	private Box findClosestNormal( Vector3f kNormal, Vector3f kTangent, Vector3f kUp, Vector3f kRight )
	{
		Box verts = new Box();
		verts.corners = new Vector3f[] {
				new Vector3f(0,-1,-1),
				new Vector3f(0,-1, 1),
				new Vector3f(0, 1, 1),
				new Vector3f(0, 1,-1)
		};

		float angle = kTangent.angle(kNormal);      

		float minAngle = angle;
		Vector3f minVec = new Vector3f(kNormal);

		float temp = kTangent.angle( Vector3f.UNIT_X );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_X);
		}
		temp = kTangent.angle( Vector3f.UNIT_X_NEG );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_X_NEG);
		}
		temp = kTangent.angle( Vector3f.UNIT_Y );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_Y);
		}
		temp = kTangent.angle( Vector3f.UNIT_Y_NEG );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_Y_NEG);
		}
		temp = kTangent.angle( Vector3f.UNIT_Z );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_Z);
		}
		temp = kTangent.angle( Vector3f.UNIT_Z_NEG );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_Z_NEG);
		}

		if (minVec.equals(kNormal) )
		{
			return verts;
		}
		if ( minVec.equals(Vector3f.UNIT_X ) || minVec.equals(Vector3f.UNIT_X_NEG ) )
		{
			verts.corners[0] = new Vector3f(0,-1,-1);
			verts.corners[1] = new Vector3f(0,-1, 1);
			verts.corners[2] = new Vector3f(0, 1, 1);
			verts.corners[3] = new Vector3f(0, 1,-1);
			kNormal.copy(minVec);
			kUp.copy(Vector3f.UNIT_Y);
			kRight.copy(Vector3f.UNIT_Z);
			return verts;
		}
		if ( minVec.equals(Vector3f.UNIT_Y ) || minVec.equals(Vector3f.UNIT_Y_NEG ) )
		{
			verts.corners[0] = new Vector3f(-1, 0,-1);
			verts.corners[1] = new Vector3f( 1, 0,-1);
			verts.corners[2] = new Vector3f( 1, 0, 1);
			verts.corners[3] = new Vector3f(-1, 0, 1);
			kNormal.copy(minVec);
			kUp.copy(Vector3f.UNIT_Z);
			kRight.copy(Vector3f.UNIT_X);
			return verts;
		}
		if ( minVec.equals(Vector3f.UNIT_Z ) || minVec.equals(Vector3f.UNIT_Z_NEG ) )
		{
			verts.corners[0] = new Vector3f(-1,-1, 0);
			verts.corners[1] = new Vector3f( 1,-1, 0);
			verts.corners[2] = new Vector3f( 1, 1, 0);
			verts.corners[3] = new Vector3f(-1, 1, 0);
			kNormal.copy(minVec);
			kUp.copy(Vector3f.UNIT_Y);
			kRight.copy(Vector3f.UNIT_X);
			return verts;
		}
		return verts;
	}

	private Vector3f findHead( Vector3f center, Vector3f planeVector, Vector3f tanVector, ModelImage image,	
			float ringDiameter, float minPeakVal, float minVal )
	{

		Matrix3f rotationM = new Matrix3f();

		Vector3f rotVector = new Vector3f();
		Vector3f temp = new Vector3f();


		int iRadialSamples = 30;
		float fInvRS = 1.0f/iRadialSamples;
		boolean minFoundAll = true;

		int[] peakDistance = new int[iRadialSamples];
		for (int iR = 0; iR < iRadialSamples; iR++)
		{
			float fAngle = Mathf.TWO_PI*fInvRS*iR;
			rotationM.fromAxisAngle( tanVector,  fAngle);
			temp.copy( planeVector );
			rotationM.mult(temp, rotVector);

			boolean minFound = false;
			boolean peakFound = false;
			int minIndex = -1, peakIndex = -1;
			for ( int j = (int)(ringDiameter/2f); j < ringDiameter; j++ )
			{
				temp.copy(rotVector);
				temp.scale(j);
				temp.add( center );

				double val = image.getFloatTriLinearBounds( temp.X, temp.Y, temp.Z );
				if ( !minFound && (val <= (minPeakVal/2.5f)) )
				{
					minFound = true;
					minIndex = j;
				}
				if ( !peakFound && (val > (minPeakVal/2f)) )
				{
					peakFound = true;
					peakIndex = j;
				}
				if ( val <= minVal )
				{
					minFound = false;
					minIndex = -1;
					peakIndex = -1;
					break;
				}
			}
			if ( (minIndex != -1) && (peakIndex != -1) && (peakIndex < minIndex) )
			{
				minFound = true;
			}
			peakDistance[iR] = peakIndex;

			minFoundAll &= (minFound & peakFound);
		}
		if ( minFoundAll )
		{
			float average = 0;
			for ( int i = 0; i < peakDistance.length; i++ )
			{
				average += peakDistance[i];
			}
			average /= peakDistance.length;
			float maxDiff = ringDiameter / 10f;
			for ( int i = 0; i < peakDistance.length; i++ )
			{
				if ( Math.abs( average - peakDistance[i] ) > maxDiff )
				{
					return null;
				}
			}
			return center;
		}
		return null;
	}


	private VOIContour findLongest( VOI tracks )
	{
		int maxLength = -1;
		int index = -1;
		for ( int i = 0; i < tracks.getCurves().size(); i++ )
		{
			if ( tracks.getCurves().elementAt(i).size() > maxLength )
			{
				maxLength = tracks.getCurves().elementAt(i).size();
				index = i;
			}
		}
		if ( index != -1 )
		{
			VOIContour longest = new VOIContour((VOIContour)tracks.getCurves().elementAt(index));
			tracks.getCurves().removeAllElements();
			return longest;
		}
		return null;
	}


	private Vector<Vector3f> findMaxPeaks( ModelImage image, float minPeakVal )
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
				if ( maxRow > minPeakVal )
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
				if ( maxColumn > minPeakVal )
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
				if ( maxDepth > minPeakVal )
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
				if ( maxDepth > minPeakVal )
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
				if ( maxDepth > minPeakVal )
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
				if ( maxDepth > minPeakVal )
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



	private Vector3f findTail( Vector3f center, Vector3f planeVector, Vector3f tanVector, ModelImage image,	
			float tailDiameter, float minPeakVal )
	{

		Matrix3f rotationM = new Matrix3f();
		Vector3f rotVector = new Vector3f();
		Vector3f temp = new Vector3f();

		int iRadialSamples = 30;
		float fInvRS = 1.0f/iRadialSamples;

		for (int iR = 0; iR < iRadialSamples; iR++)
		{
			float fAngle = Mathf.TWO_PI*fInvRS*iR;
			rotationM.fromAxisAngle( tanVector,  fAngle);
			temp.copy( planeVector );
			rotationM.mult(temp, rotVector);

			int minEndIndex = -1;
			for ( int j = 1; j <= tailDiameter; j++ )
			{
				temp.copy(rotVector);
				temp.scale(j);
				temp.add( center );

				double val = image.getFloatTriLinearBounds( temp.X, temp.Y, temp.Z );
				if ( val < minPeakVal )
				{
					break;
				}
				minEndIndex = j;
			}
			if ( minEndIndex != tailDiameter )
			{
				return null;
			}
		}
		return center;
	}

	private Box[] getBoundingBox( Matrix3f transformTotal, Box box, Vector3f point, float diameter )
	{

		Vector3f[] outVertices = new Vector3f[4];

		Vector3f centerModel = new Vector3f();

		Box nextBox = new Box();
		for (int i = 0; i < 4; i++) {
			outVertices[i] = new Vector3f();

			// Rotate the points in the bounding box:
			// verts is a 'unit' cube centered at (0,0,0) with
			// corners from (-1,-1,-1) -> (1,1,1)
			transformTotal.mult(box.corners[i], outVertices[i]);

			nextBox.corners[i] = new Vector3f( outVertices[i] );
		} 

		Box boundingBox = new Box();
		for (int i = 0; i < 4; i++) {			
			// Shift so the position is back to the cube with corners from (0,0,0) -> (1,1,1)
			outVertices[i].X += centerShift.X;
			outVertices[i].Y += centerShift.Y;
			outVertices[i].Z += centerShift.Z;

			outVertices[i].X /= 2f;
			outVertices[i].Y /= 2f;
			outVertices[i].Z /= 2f;


			// Scale back to the image dimensions:
			boundingBox.corners[i] = new Vector3f();
			boundingBox.corners[i].X = (diameter * outVertices[i].X) / sourceResolutions[0];
			boundingBox.corners[i].Y = (diameter * outVertices[i].Y) / sourceResolutions[1];
			boundingBox.corners[i].Z = (diameter * outVertices[i].Z) / sourceResolutions[2];

			centerModel.X += boundingBox.corners[i].X;
			centerModel.Y += boundingBox.corners[i].Y;
			centerModel.Z += boundingBox.corners[i].Z;
		}

		centerModel.X /= 4f;
		centerModel.Y /= 4f;
		centerModel.Z /= 4f;

		for (int i = 0; i < 4; i++) {
			// move from the center of the volume to the position:
			// (centerModel and kPosition are in image-index coordinates)
			boundingBox.corners[i].X += (point.X - centerModel.X);
			boundingBox.corners[i].Y += (point.Y - centerModel.Y);
			boundingBox.corners[i].Z += (point.Z - centerModel.Z);
		}		

		return new Box[]{boundingBox, nextBox};
	}

	/**
	 * Returns the amount of correction which should be applied to the z-direction sigma (assuming that correction is
	 * requested).
	 *
	 * @return  the amount to multiply the z-sigma by to correct for resolution differences
	 */
	private float getCorrectionFactor() {
		int index = wormImage.getExtents()[2] / 2;
		float xRes = wormImage.getFileInfo(index).getResolutions()[0];
		float zRes = wormImage.getFileInfo(index).getResolutions()[2];

		return xRes / zRes;
	}



	private void getUpRightVectors( Box initBox, Vector3f kNormal, Vector<Box3f> planes )
	{
		samplingPlanes.removeCurves();
		planes.clear();
		Box[] results;

		//results = getBoundingBox(new Matrix3f(), initBox, point );
		boundingBoxes.clear();
		unitBox.clear();
		//boundingBoxes.add(new Box(results[0]));
		//unitBox.add(new Box(results[1]));



		Vector3f tanVector = new Vector3f( curveTangents.elementAt(0) );
		//get coordinate of control point of b-spline
		Vector3f point = new Vector3f( curvePositions.elementAt(0) );
		// Angle between the tangent and the 'default' z-direction:
		float angle = tanVector.angle(kNormal);    
		Vector3f rotationAxis = Vector3f.cross( kNormal, tanVector );
		rotationAxis.normalize();

		// Create a rotation angle-degrees about the rotation axis:
		Matrix3f transformTotal = new Matrix3f();
		transformTotal.fromAxisAngle( rotationAxis, angle );

		results = getBoundingBox(transformTotal, initBox, point, diameter );
		boundingBoxes.add(new Box(results[0]));
		unitBox.add(new Box(results[1]));

		Box boundingBox = boundingBoxes.elementAt(0);
		planes.add( makeBox3f( boundingBox, tanVector) );

		VOIContour kBox = new VOIContour(true);
		for ( int pt = 0; pt < 4; pt++ )
		{
			kBox.addElement( boundingBox.corners[pt].X, boundingBox.corners[pt].Y, boundingBox.corners[pt].Z );
		}
		kBox.update( new ColorRGBA(0,0,1,1) );			
		samplingPlanes.importCurve(kBox);

		for( int i = 1; i < curveTangents.size(); i++ )
		{

			transformBoxSlices( i );
			boundingBox = boundingBoxes.elementAt(i);
			planes.add( makeBox3f( boundingBox, curveTangents.elementAt(i)) );

			kBox = new VOIContour(true);
			for ( int pt = 0; pt < 4; pt++ )
			{
				kBox.addElement( boundingBox.corners[pt].X, boundingBox.corners[pt].Y, boundingBox.corners[pt].Z );
			}
			kBox.update( new ColorRGBA(0,0,1,1) );			
			samplingPlanes.importCurve(kBox);
		}

	}



	/** Standard meshes.  Each mesh is centered at (0,0,0) and has an up-axis
	 * of (0,0,1).  The other axes forming the coordinate system are (1,0,0)
	 * and (0,1,0).  An application may transform the meshes as necessary.
	 * @param iZSamples number of z-samples.
	 * @param iRadialSamples number of radial samples.
	 * @param fRadius sphere radius.
	 * @return Sphere TriMesh.
	 */
	private Vector<Vector3f> halfSphere (int iZSamples, int iRadialSamples, float fRadius)
	{
		int iZSm1 = iZSamples-1;
		int iRSp1 = iRadialSamples+1;

		Vector<Vector3f> positions = new Vector<Vector3f>();

		// generate geometry
		float fInvRS = 1.0f/iRadialSamples;
		float fZFactor = 1.0f/iZSm1;

		// Generate points on the unit circle to be used in computing the mesh
		// points on a cylinder slice.
		float[] afSin = new float[iRSp1];
		float[] afCos = new float[iRSp1];
		for (int iR = 0; iR < iRadialSamples; iR++)
		{
			float fAngle = Mathf.TWO_PI*fInvRS*iR;
			afCos[iR] = (float)Math.cos(fAngle);
			afSin[iR] = (float)Math.sin(fAngle);
		}
		afSin[iRadialSamples] = afSin[0];
		afCos[iRadialSamples] = afCos[0];


		Vector3f kSliceCenter = new Vector3f();
		Vector3f kRadial= new Vector3f();

		for (int iZ = 0; iZ < iZSm1; iZ++)
		{
			float fZFraction = fZFactor*iZ;  // in (0,1)
			float fZ = fRadius*fZFraction;

			// compute radius of slice
			float fSliceRadius = (float)Math.sqrt(Math.abs(fRadius*fRadius-fZ*fZ));

			// compute slice vertices:
			for (int iR = 0; iR < iRadialSamples; iR++)
			{
				kRadial.set(afCos[iR],afSin[iR],0.0f).scale(fSliceRadius);

				// compute center of slice
				kSliceCenter.set(0.0f,0.0f,fZ).add(kRadial);
				positions.add( new Vector3f(kSliceCenter) );
			}
		}


		// north pole
		positions.add( new Vector3f(0, 0, fRadius) );
		return positions;
	}

	private boolean inBounds( ModelImage image, Vector3f pt )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		if ( (pt.X >= 0) && (pt.X < dimX) &&
				(pt.Y >= 0) && (pt.Y < dimY) &&
				(pt.Z >= 0) && (pt.Z < dimZ) )
			return true;
		return false;
	}


	private void interpolatePointsTangents(float[] xPoints, float[] yPoints, float[] zPoints)
	{
		float totalArcLength = 0;
		// 1. Translate points into resolution space:
		short sID = (short)(wormImage.getVOIs().getUniqueID());
		originalPoints = new VOI(sID, "originalPoints", VOI.POINT, .5f );
		originalPoints.setCurveType( VOI.POINT );
		for ( int i = 0; i < xPoints.length; i++ )
		{
			xPoints[i] *= sourceResolutions[0];
			yPoints[i] *= sourceResolutions[1];
			zPoints[i] *= sourceResolutions[2];
			if ( i > 0 )
			{
				totalArcLength += Math.sqrt( ((xPoints[i] - xPoints[i-1]) *(xPoints[i] - xPoints[i-1])) + 
						((yPoints[i] - yPoints[i-1]) *(yPoints[i] - yPoints[i-1])) + 
						((zPoints[i] - zPoints[i-1]) *(zPoints[i] - zPoints[i-1])) );
			}
			originalPoints.importPoint( new Vector3f( xPoints[i], yPoints[i], zPoints[i] ) );
		}
		//		System.err.println("Total Arc Length " + totalArcLength);
		//		int numPts = (int) (totalArcLength / stepSize);
		//		System.err.println( "number of points " + numPts );

		// 2 Smooth points:
		AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints, zPoints);
		float totalL = arcLength.getTotalArcLength();
		int interpolationPts = (int)((totalL + 5) / stepSize);

		AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(wormImage, originalPoints, interpolationPts, true);
		smoothAlgo.run();
		//this is the result b-spline curve
		VOI resultVOI = smoothAlgo.getResultVOI();
		Vector<VOIBase> contours = resultVOI.getCurves();
		int nPoints = contours.size();
		float[] xSmoothedPoints = new float[nPoints+5];
		float[] ySmoothedPoints = new float[nPoints+5];
		float[] zSmoothedPoints = new float[nPoints+5];

		//		System.err.println( nPoints );

		Vector3f point = ((VOIPoint)contours.get(0)).exportPoint();
		xSmoothedPoints[0] = point.X;
		ySmoothedPoints[0] = point.Y;
		zSmoothedPoints[0] = point.Z;

		xSmoothedPoints[1] = point.X;
		ySmoothedPoints[1] = point.Y;
		zSmoothedPoints[1] = point.Z;

		Vector<Vector3f> contour = new Vector<Vector3f>();
		for (int i = 0; i < nPoints; i++) {
			point = ((VOIPoint)contours.get(i)).exportPoint();
			contour.add(new Vector3f(point));
			xSmoothedPoints[i + 2] = point.X;
			ySmoothedPoints[i + 2] = point.Y;
			zSmoothedPoints[i + 2] = point.Z;  
		}

		point = ((VOIPoint)contours.get(nPoints-1)).exportPoint();
		xSmoothedPoints[nPoints + 2] = point.X;
		ySmoothedPoints[nPoints + 2] = point.Y;
		zSmoothedPoints[nPoints + 2] = point.Z;

		xSmoothedPoints[nPoints + 3] = point.X;
		ySmoothedPoints[nPoints + 3] = point.Y;
		zSmoothedPoints[nPoints + 3] = point.Z;

		xSmoothedPoints[nPoints + 4] = point.X;
		ySmoothedPoints[nPoints + 4] = point.Y;
		zSmoothedPoints[nPoints + 4] = point.Z;



		//alg to get tangent vector
		AlgorithmBSpline bSplineAlgo = smoothAlgo.getbSplineAlgo();

		Vector<Vector3f> positions = new Vector<Vector3f>();
		Vector<Vector3f> tangents = new Vector<Vector3f>();
		for ( int i = 0; i < nPoints+1; i++ )
		{
			float floatIndex = i+2;
			positions.add( bSplineAlgo.bSplineJetXYZ(0, floatIndex, xSmoothedPoints, ySmoothedPoints, zSmoothedPoints) );
			tangents.add(bSplineAlgo.bSplineJetXYZ(1, floatIndex, xSmoothedPoints, ySmoothedPoints, zSmoothedPoints) );
		}

		bSplineAlgo = null;
		xSmoothedPoints = null;
		ySmoothedPoints = null;
		zSmoothedPoints = null;

		// 3 Interpolate points, tangents so evenly spaced in resolutions-space:

		Vector<Vector3f> positionsInterp = new Vector<Vector3f>();
		Vector<Vector3f> tangentsInterp = new Vector<Vector3f>();

		positionsInterp.add( new Vector3f( positions.elementAt(0) ));
		tangentsInterp.add( new Vector3f( tangents.elementAt(0) ));

		Vector3f currentPoint = new Vector3f( positions.elementAt(0) );
		Vector3f currentTangent = new Vector3f( tangents.elementAt(0) );

		//		System.err.println( positions.size() );
		//		System.err.println( 0 + " " + positions.elementAt(0) );
		for ( int i = 1; i < positions.size(); i++ )
		{
			Vector3f nextPoint  = new Vector3f( positions.elementAt(i) );
			Vector3f nextTangent  = new Vector3f( tangents.elementAt(i) );
			Vector3f direction = Vector3f.sub(nextPoint, currentPoint);
			direction.normalize();
			direction.scale(stepSize);

			float distance = nextPoint.distance(currentPoint);
			while ( distance >= stepSize )
			{
				Vector3f newPoint = Vector3f.add(currentPoint, direction );
				float interpFactor = newPoint.distance(currentPoint) / nextPoint.distance(currentPoint);

				Vector3f tangent1 = Vector3f.scale( 1 - interpFactor, currentTangent );
				Vector3f tangent2 = Vector3f.scale(     interpFactor, nextTangent );
				Vector3f newTangent = Vector3f.add( tangent1, tangent2 );

				positionsInterp.add( new Vector3f(newPoint));
				tangentsInterp.add( new Vector3f(newTangent));
				//				System.err.println( i + " " + newPoint + " " + newPoint.distance(positionsInterp.elementAt(positionsInterp.size()-2)) );

				currentPoint.copy(newPoint);
				currentTangent.copy(newTangent);
				distance = nextPoint.distance(currentPoint);
			}

		}


		//		Vector<Vector3f> positionsInterp = positions;
		//		Vector<Vector3f> tangentsInterp = tangents;

		// 4 Translate Points / Tangents back into image-index space:
		sID = (short)(wormImage.getVOIs().getUniqueID());
		originalCurve = new VOI(sID, "originalCurve", VOI.POINT, .5f );
		originalCurve.setCurveType( VOI.POINT );
		for ( int i = 0; i < positionsInterp.size(); i++ )
		{
			Vector3f temp = new Vector3f(positionsInterp.elementAt(i));
			if ( i > 0 )
			{
				//				System.err.println( i + " " + temp.distance( positionsInterp.elementAt(i-1) ) );
			}
			temp.X /= sourceResolutions[0];
			temp.Y /= sourceResolutions[1];
			temp.Z /= sourceResolutions[2];
			curvePositions.add( new Vector3f(temp) );
			originalCurve.importPoint(  temp );

			temp = new Vector3f(tangentsInterp.elementAt(i));
			temp.X /= sourceResolutions[0];
			temp.Y /= sourceResolutions[1];
			temp.Z /= sourceResolutions[2];
			curveTangents.add( new Vector3f(temp) );
		}


		Vector3f kNormal = new Vector3f( Vector3f.UNIT_X );
		Vector3f kRight = new Vector3f( );
		Vector3f kUp = new Vector3f( );
		findClosestNormal(curvePositions, kNormal, kUp, kRight);
		Vector3f kOriginalNormal = new Vector3f(kNormal);
		for ( int i = 0; i < curveTangents.size(); i++ )
		{
			Vector3f tanVector = curveTangents.elementAt(i);
			//normalizing tangent
			tanVector.normalize();

			// Angle between the tangent and the 'default' z-direction:
			float angle = tanVector.angle(kNormal); 
			if ( angle > minAngle90 )
			{
				tanVector.neg();
			}
			kNormal = tanVector;
		}

		kNormal = new Vector3f(kOriginalNormal);
		for ( int i = 0; i < curveTangents.size(); i++ )
		{
			Vector3f tanVector = curveTangents.elementAt(i);

			// Angle between the tangent and the 'default' z-direction:
			float angle = tanVector.angle(kNormal); 
			if ( angle > minAngle90 )
			{
				tanVector.neg();
				System.err.println( "Tangent Test Faild" );
			}
			kNormal = tanVector;
		}

		saveVOI("originalCurve", wormImage.getImageDirectory(), wormImage, originalCurve);

		//register new b-spline voi
		wormImage.resetVOIs();
		wormImage.registerVOI(originalCurve);
	}

	private void jointracks( VOI tracks ) 
	{
		float minDistance = -1;
		int prevSize = -1;
		while ( tracks.getCurves().size() != prevSize )
		{
			prevSize = tracks.getCurves().size();

			Vector3f track1Start, track1End, track2Start, track2End;
			float closestDistance = Float.MAX_VALUE, dist;
			float furthestDistance = -Float.MAX_VALUE;
			int indexA = -1, indexB = -1, which = -1;
			for ( int i = tracks.getCurves().size() - 1; i >= 0; i-- )
			{
				VOIContour track1 = (VOIContour) tracks.getCurves().elementAt(i);
				track1Start = track1.elementAt(0);
				track1End = track1.elementAt(track1.size()-1);
				for ( int j = tracks.getCurves().size() - 1; j >= 0; j-- )
				{
					VOIContour track2 = (VOIContour) tracks.getCurves().elementAt(j);
					if ( track1 == track2 )
					{
						continue;
					}
					track2Start = track2.elementAt(0);
					track2End = track2.elementAt(track2.size()-1);

					dist = track1Start.distance(track2Start);
					if ( dist > furthestDistance )
					{
						furthestDistance = dist;
					}
					if ( (dist < closestDistance) && (dist > minDistance) )
					{
						closestDistance = dist;
						indexA = i;
						indexB = j;
						which = 0;
					}
					dist = track1Start.distance(track2End);
					if ( dist > furthestDistance )
					{
						furthestDistance = dist;
					}
					if ( (dist < closestDistance) && (dist > minDistance) )
					{
						closestDistance = dist;
						indexA = i;
						indexB = j;
						which = 1;
					}
					dist = track1End.distance(track2Start);
					if ( dist > furthestDistance )
					{
						furthestDistance = dist;
					}
					if ( (dist < closestDistance) && (dist > minDistance) )
					{
						closestDistance = dist;
						indexA = i;
						indexB = j;
						which = 2;
					}
					dist = track1End.distance(track2End);
					if ( dist > furthestDistance )
					{
						furthestDistance = dist;
					}
					if ( (dist < closestDistance) && (dist > minDistance) )
					{
						closestDistance = dist;
						indexA = i;
						indexB = j;
						which = 3;
					}  			    			
				}
			}
			if ( indexA != -1 && indexB != -1 && which != -1 )
			{
				VOIContour track1 = (VOIContour) tracks.getCurves().elementAt(indexA);
				track1Start = track1.elementAt(0);
				track1End = track1.elementAt(track1.size()-1);

				VOIContour track2 = (VOIContour) tracks.getCurves().elementAt(indexB);
				track2Start = track2.elementAt(0);
				track2End = track2.elementAt(track2.size()-1);

				if ( which == 0 )
				{
					Vector3f dirA = Vector3f.sub( track1.elementAt(1), track1Start );
					Vector3f dirB = Vector3f.sub( track2Start, track2.elementAt(1) );
					dirA.normalize();
					dirB.normalize();
					float angle = dirA.angle(dirB);
					if ( angle < minAngle90 )
					{
						//add track2 onto track1:
						for ( int i = 0; i < track2.size(); i++ )
						{
							track1.add(0, track2.elementAt(i) );
						}
						track1.trimPoints( 0.5f, true );
						track1.update( new ColorRGBA(1,0,0,1) );
						tracks.getCurves().remove(track2);
					}
				}
				if ( which == 1 )
				{
					Vector3f dirA = Vector3f.sub( track1.elementAt(1), track1Start );
					Vector3f dirB = Vector3f.sub( track2End, track2.elementAt(track2.size()-2) );
					dirA.normalize();
					dirB.normalize();
					float angle = dirA.angle(dirB);
					if ( angle < minAngle90 )
					{
						//add track2 onto track1:
						for ( int i = track2.size() -1; i >=0; i-- )
						{
							track1.add(0, track2.elementAt(i) );
						}
						track1.trimPoints( 0.5f, true );
						track1.update( new ColorRGBA(1,0,0,1) );
						tracks.getCurves().remove(track2);
					}
				}
				if ( which == 2 )
				{
					Vector3f dirA = Vector3f.sub( track1End, track1.elementAt(track1.size()-1)  );
					Vector3f dirB = Vector3f.sub( track2.elementAt(1), track2Start );
					dirA.normalize();
					dirB.normalize();
					float angle = dirA.angle(dirB);
					if ( angle < minAngle90 )
					{
						//add track2 onto track1:
						for ( int i = 0; i < track2.size(); i++ )
						{
							track1.add(track2.elementAt(i) );
						}
						track1.trimPoints( 0.5f, true );
						track1.update( new ColorRGBA(1,0,0,1) );
						tracks.getCurves().remove(track2);
					}
				}
				if ( which == 3 )
				{
					Vector3f dirA = Vector3f.sub( track1End, track1.elementAt(track1.size()-1)  );
					Vector3f dirB = Vector3f.sub( track2End, track2.elementAt(track2.size()-1) );
					dirA.normalize();
					dirB.normalize();
					float angle = dirA.angle(dirB);
					if ( angle < minAngle90 )
					{
						//add track2 onto track1:
						for ( int i = track2.size() -1; i >=0; i-- )
						{
							track1.add(track2.elementAt(i) );
						}
						track1.trimPoints( 0.5f, true );
						track1.update( new ColorRGBA(1,0,0,1) );
						tracks.getCurves().remove(track2);
					}
				}
			}
		}
	}


	private Box3f makeBox3f( Box b, Vector3f t )
	{

		Vector3f bottom = Vector3f.add( b.corners[0], b.corners[1]);
		bottom.scale(0.5f);
		Vector3f top = Vector3f.add( b.corners[2], b.corners[3]);
		top.scale(0.5f);

		Vector3f left = Vector3f.add( b.corners[0], b.corners[3]);
		left.scale(0.5f);
		Vector3f right = Vector3f.add( b.corners[1], b.corners[2]);
		right.scale(0.5f);

		Vector3f planeCenter = new Vector3f();
		for ( int j = 0; j < 4; j++ )
		{
			planeCenter.X += b.corners[j].X;
			planeCenter.Y += b.corners[j].Y;
			planeCenter.Z += b.corners[j].Z;
		}
		planeCenter.X /= 4f;
		planeCenter.Y /= 4f;
		planeCenter.Z /= 4f;
		Vector3f up = Vector3f.sub(top,bottom);
		up.normalize();
		right = Vector3f.sub(right,left);
		right.normalize();
		Vector3f tanVec = new Vector3f(t);
		tanVec.normalize();
		return new Box3f( planeCenter, up, 
				right, tanVec, 
				b.corners[3].distance(b.corners[0] )/2f,
				b.corners[1].distance(b.corners[0] )/2f,
				0f );
	}

	private Plane3f makePlane3f( Box b, Vector3f t )
	{
		Vector3f planeCenter = new Vector3f();
		for ( int j = 0; j < 4; j++ )
		{
			planeCenter.X += b.corners[j].X;
			planeCenter.Y += b.corners[j].Y;
			planeCenter.Z += b.corners[j].Z;
		}
		planeCenter.X /= 4f;
		planeCenter.Y /= 4f;
		planeCenter.Z /= 4f;
		Vector3f tanVec = new Vector3f(t);
		tanVec.normalize();
		return new Plane3f( tanVec, planeCenter );
	}
	private void mergeMult( VOI tracks, VOIContour contour1, VOIContour contour2 )
	{
		if ( contour1.size() == 1 )
		{
			tracks.getCurves().remove(contour1);
			return;
		}
		else if ( contour2.size() == 1 )
		{
			tracks.getCurves().remove(contour2);
			return;
		}
		boolean[] matches = new boolean[contour2.size()];
		for ( int i = 0; i < contour2.size(); i++ )
		{
			matches[i] = false;
		}
		for ( int i = 0; i < contour1.size(); i++ )
		{
			Vector3f pt = contour1.elementAt(i);
			for ( int j = contour2.size() - 1; j >=0; j-- )
			{
				if ( nearEquals( pt, contour2.elementAt(j) ) )
				{
					matches[j] = true;
				}
			}
		}
		VOIContour subContour = new VOIContour(false);
		for ( int i = 0; i < contour2.size(); i++ )
		{
			if ( !matches[i] )
			{
				subContour.add( new Vector3f(contour2.elementAt(i) ) );
			}
			else if ( subContour.size() > 2 )
			{
				tracks.getCurves().add(subContour);
				subContour = new VOIContour(false);
			}
		}
		tracks.getCurves().remove( contour2 );
	}
	private void mergeTracks( ModelImage image, VOI tracks )
	{

		//		System.err.println( "mergeTracks " + tracks.getCurves().size() );

		while( checkTrackOverlap( tracks ) )
		{
			removeDuplicates(tracks);

			boolean overlapFound = false;
			for ( int i = 0; i < tracks.getCurves().size(); i++ )
			{				
				for ( int j = i+1; j < tracks.getCurves().size(); j++ )
				{
					Vector<Vector3f> overlapList = checkOverlap( (VOIContour)(tracks.getCurves().elementAt(i)),
							(VOIContour)(tracks.getCurves().elementAt(j)) );

					if ( overlapList != null )
					{
						VOIContour contour1 = (VOIContour)(tracks.getCurves().elementAt(i));
						VOIContour contour2 = (VOIContour)(tracks.getCurves().elementAt(j));
						if ( contour1.size() >= contour2.size() )
						{
							mergeMult( tracks, contour1, contour2 );
						}
						else
						{
							mergeMult( tracks, contour2, contour1 );
						}
						overlapFound = true;
						break;
					}
				}
				if ( overlapFound )
				{
					break;
				}
			}
		}

		//		System.err.println( "mergeTracks " + tracks.getCurves().size() );
	}
	private boolean nearEquals( Vector3f v1, Vector3f v2 )
	{
		return (v1.distance(v2) < 5);
	}
	private void removeDuplicates( VOI tracks )
	{
		//		System.err.println( "removeDuplicates " + tracks.getCurves().size() );

		for ( int i = 0; i < tracks.getCurves().size(); i++ )
		{
			Vector<VOIContour> deleteList = new Vector<VOIContour>();
			for ( int j = i+1; j < tracks.getCurves().size(); j++ )
			{
				Vector<Vector3f> overlapList = checkOverlap( (VOIContour)(tracks.getCurves().elementAt(i)),
						(VOIContour)(tracks.getCurves().elementAt(j)) );

				if ( overlapList != null )
				{
					if ( allOverlap( (VOIContour)(tracks.getCurves().elementAt(j)), overlapList ) )
					{
						//						System.err.println( "...delete" );
						deleteList.add( (VOIContour)(tracks.getCurves().elementAt(j)) );
					}
				}
			}
			if ( deleteList.size() > 0 )
			{
				for ( int j = 0; j < deleteList.size(); j++ )
				{
					tracks.getCurves().remove( deleteList.elementAt(j) );
				}
				deleteList.clear();
			}
		}
		//		System.err.println( "removeDuplicates " + tracks.getCurves().size() );
	}

	private void saveVOI( String fileName, String directory, ModelImage kImage, VOI voi)
	{
		FileVOI fileVOI;
		try {
			fileVOI = new FileVOI(fileName + ".xml", directory, kImage);
			fileVOI.writeXML(voi, true, true);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private float scaledDistance(Vector3f kP0, Vector3f kP1, float[] scale)
	{
		float dX = kP0.X - kP1.X;
		float dY = kP0.Y - kP1.Y;
		float dZ = kP0.Z - kP1.Z;
		dX *= scale[0];
		dY *= scale[1];
		dZ *= scale[2];
		return (float) Math.sqrt(dX * dX + dY * dY + dZ * dZ);
	}

	private VOI showPoints( ModelImage image, Vector<Vector3f> positions )
	{
		short sID = (short)(image.getVOIs().getUniqueID());
		VOI pathPoints = new VOI( sID, "path" );
		pathPoints.setCurveType( VOI.POINT );
		for ( int i = 0; i < positions.size(); i++ )
		{
			VOIPoint voiPt = new VOIPoint( VOI.POINT, positions.elementAt(i));
			voiPt.update(new ColorRGBA(1,0,1,1));
			pathPoints.getCurves().add( 0, voiPt );
		}
		return pathPoints;
	}


	private boolean smoothTangents( Box initBox, Vector3f kNormal, Vector3f kUp, Vector3f kRight )
	{
		Vector<Box3f> planes = new Vector<Box3f>();
		getUpRightVectors( initBox, kNormal, planes );
		//samplingPlanes.removeCurves();

		//		short sID = (short)(wormImage.getVOIs().getUniqueID());
		//		VOI intersectionLines = new VOI(sID, "intersectionLines");
		//		System.err.println( "DONE INTERSECTION CORRECTION" );
		for ( int i = 0; i < planes.size(); i++ )
		{
			if ( i > 0 )
			{
				IntrBox3Box3f intersect = new IntrBox3Box3f( planes.elementAt(i-1), planes.elementAt(i) );
				if (intersect.Test())
				{
					//					System.err.println( "box " + i + " intersects box " + (i-1) );
					samplingPlanes.getCurves().elementAt(i-1).update( new ColorRGBA(1,0,1,1) );
					samplingPlanes.getCurves().elementAt(i).update( new ColorRGBA(1,0,1,1) );
					//        			Box boundingBox = boundingBoxes.elementAt(i-1);        			
					//        			VOIContour kBox = new VOIContour(true);
					//        			for ( int pt = 0; pt < 4; pt++ )
					//        			{
					//        				kBox.addElement( boundingBox.corners[pt].X, boundingBox.corners[pt].Y, boundingBox.corners[pt].Z );
					//        			}
					//        			kBox.update( new ColorRGBA(1,0,1,1) );			
					//        			samplingPlanes.importCurve(kBox);
					//        			
					//        			boundingBox = boundingBoxes.elementAt(i);        			
					//        			kBox = new VOIContour(true);
					//        			for ( int pt = 0; pt < 4; pt++ )
					//        			{
					//        				kBox.addElement( boundingBox.corners[pt].X, boundingBox.corners[pt].Y, boundingBox.corners[pt].Z );
					//        			}
					//        			kBox.update( new ColorRGBA(1,0,1,1) );			
					//        			samplingPlanes.importCurve(kBox);
					//
					//            		Box boundingBox0 = boundingBoxes.elementAt(i-1);
					//            		Box boundingBox1 = boundingBoxes.elementAt(i);
					//            		boolean intersectB = false;
					//            		VOIContour intersectionLine = new VOIContour(false);
					//            		intersectionLine.update( new ColorRGBA(1,1,1,1) );		
					//        			for ( int j = 0; j < 4; j++ )
					//        			{
					//        				Segment3f seg = new Segment3f( boundingBox0.corners[j], boundingBox0.corners[(j+1)%4] );
					//        				IntrSegment3Box3f intersectSegment = new IntrSegment3Box3f(seg, planes.elementAt(i), true );
					//        				if ( intersectSegment.Test() )
					//        				{
					//        					intersectB = true;
					//        					intersectSegment.Find();
					//        					for ( int k = 0; k < intersectSegment.GetQuantity(); k++ )
					//        					{
					//        						intersectionLine.add( intersectSegment.GetPoint(k) );
					//        					}
					//        				}
					//        			}
					//        			for ( int j = 0; j < 4; j++ )
					//        			{
					//        				Segment3f seg = new Segment3f( boundingBox1.corners[j], boundingBox1.corners[(j+1)%4] );
					//        				IntrSegment3Box3f intersectSegment = new IntrSegment3Box3f(seg, planes.elementAt(i-1), true );
					//        				if ( intersectSegment.Test() )
					//        				{
					//        					intersectB = true;
					//        					intersectSegment.Find();
					//        					for ( int k = 0; k < intersectSegment.GetQuantity(); k++ )
					//        					{
					//        						intersectionLine.add( intersectSegment.GetPoint(k) );
					//        					}
					//        				}
					//        			}
					//        			if ( !intersectB )
					//        			{
					//            			System.err.println( "box " + i + " DOES NOT intersects box " + (i-1) );
					//        			}	
					////        			if ( intersectionLine.size() > 1 )
					//        			{
					//        				intersectionLines.importCurve(intersectionLine);
					//        			}
					////        			else
					////        			{        				
					////            			samplingPlanes.getCurves().elementAt( samplingPlanes.getCurves().size()-2).update(new ColorRGBA(0,0,1,1));
					////        			}
				}
			}
		}

		//		wormImage.resetVOIs();		
		//		wormImage.registerVOI( samplingPlanes );
		//		wormImage.registerVOI( intersectionLines );
		wormImage.calcMinMax();
		//		new ViewJFrameImage(wormImage);
		return true;
	}

	private void testBoxIntersections()
	{
		short sID = (short)(wormImage.getVOIs().getUniqueID());
		VOI intersectionLines = new VOI(sID, "intersectionLines");
		boolean added = false;
		for( int i = 0; i < boundingBoxes.size(); i++ )
		{
			for ( int j = i+1; j < boundingBoxes.size(); j++ )
			{
				Vector<VOIContour> lines = testIntersection( boundingBoxes.elementAt(i), curveTangents.elementAt(i), 
						boundingBoxes.elementAt(j), curveTangents.elementAt(j), true );
				if ( lines != null )
				{
					added = true;
					for ( int k = 0; k < lines.size(); k++ )
					{
						intersectionLines.importCurve( lines.elementAt(k) );
					}
				}
			}
		}
		if ( added )
		{
			wormImage.registerVOI(intersectionLines);
		}
	}

	private boolean testIntersection( Box b1, Vector3f t1, Box b2, Vector3f t2 )
	{
		Box3f box1 = makeBox3f( b1, t1 );
		Box3f box2 = makeBox3f( b2, t2 );

		IntrBox3Box3f intersect = new IntrBox3Box3f( box1, box2 );
		return intersect.Test();
	}

	private Vector<VOIContour> testIntersection( Box b1, Vector3f t1, Box b2, Vector3f t2, boolean bPlanes )
	{
		Box3f box1 = makeBox3f( b1, t1 );
		Box3f box2 = makeBox3f( b2, t2 );
		IntrBox3Box3f intersectBoxes = new IntrBox3Box3f( box1, box2 );
		if ( !intersectBoxes.Test() )
		{
			return null;
		}
		System.err.print( "Boxes intersect...." );


		Vector<VOIContour> intersectionLines = null;
		Plane3f p0 = makePlane3f( b1, t1 );
		Plane3f p1 = makePlane3f( b2, t2 );
		IntrPlane3Plane3f intersection = new IntrPlane3Plane3f( p0, p1 );
		if ( intersection.Test() )
		{
//			System.err.print( "Planes intersect..." );
			intersection.Find();
			if ( intersection.GetIntersectionType() == IntersectionInfo.IT_LINE )
			{
//				System.err.println( "  intersection is line" );
				Line3f intrLine = intersection.GetIntersectionLine();

				intersectionLines = new Vector<VOIContour>();
				VOIContour line = new VOIContour(false);
				Segment3f segment = new Segment3f(intrLine.Origin, intrLine.Direction, maxDiameter/2);
				Vector3f point0 = new Vector3f();
				segment.GetNegEnd(point0);
				line.add(point0);
				Vector3f point1 = new Vector3f();
				segment.GetPosEnd(point1);
				line.add(point1);
				intersectionLines.add(line);




				//				IntrLine3Box3f intrLB = new IntrLine3Box3f( intrLine, box1 );
				//				if ( intrLB.Test() )
				//				{
				//					intrLB.Find();
				//					System.err.println( "   Line intersects Box 1 " + intrLB.GetQuantity() );
				//					if ( intrLB.GetQuantity() > 1 )
				//					{
				//						System.err.println( "     adding line" );
				//						if ( intersectionLines == null )
				//						{
				//							intersectionLines = new Vector<VOIContour>();
				//						}
				//						VOIContour line = new VOIContour(false);
				//						for ( int i = 0; i < intrLB.GetQuantity(); i++ )
				//						{
				//							line.add( intrLB.GetPoint(i) );
				//						}
				//						intersectionLines.add(line);
				//					}
				//				}
				//				
				//
				//				intrLB = new IntrLine3Box3f( intrLine, box2 );
				//				if ( intrLB.Test() )
				//				{
				//					intrLB.Find();
				//					System.err.println( "   Line intersects Box 2 " + intrLB.GetQuantity() );
				//					if ( intrLB.GetQuantity() > 1 )
				//					{
				//						if ( intersectionLines == null )
				//						{
				//							intersectionLines = new Vector<VOIContour>();
				//						}
				//						System.err.println( "     adding line" );
				//						VOIContour line = new VOIContour(false);
				//						for ( int i = 0; i < intrLB.GetQuantity(); i++ )
				//						{
				//							line.add( intrLB.GetPoint(i) );
				//						}
				//						intersectionLines.add(line);
				//					}
				//				}
			}
			else if ( intersection.GetIntersectionType() == IntersectionInfo.IT_PLANE )
			{
//				System.err.println( "  intersection is plane" );
			}
		}
		else
		{
//			System.err.println("");
		}
		return intersectionLines;
	}

	private Vector3f testSphere( ModelImage image, ModelImage blurImage, Vector<Vector3f> positions, Vector<Vector3f> directions, 
			Vector<Vector3f> tailPositions, Vector<Vector3f> tailDirections,
			Vector3f center, Vector<Vector3f> inputNormals,
			float innerDiameter, float outerDiameter, float ringDiameter, float tailDiameter, float minVal, float minPeakVal )
	{
		Vector3f temp = new Vector3f();

		Box verts = new Box();
		verts.corners[0] = new Vector3f(-1,-1, 0);
		verts.corners[1] = new Vector3f( 1,-1, 0);
		verts.corners[2] = new Vector3f( 1, 1, 0);
		verts.corners[3] = new Vector3f(-1, 1, 0);
		Vector3f prevVector = new Vector3f(Vector3f.UNIT_Z);

		float tempDiameter = Math.min( blurImage.getExtents()[0], Math.min( blurImage.getExtents()[1], blurImage.getExtents()[2] ) );

		Vector3f[] tanVectorResults = new Vector3f[inputNormals.size()];
		Vector3f[] centerResults = new Vector3f[inputNormals.size()];
		int[] radiusCountResults = new int[inputNormals.size()];
		int[] radiusResults = new int[inputNormals.size()];

		boolean minFoundAll = true;
		Vector3f headPos = null;
		Vector3f tailPos = null;
		for ( int i = 0; i < inputNormals.size(); i++ )
		{
			Vector3f tanVector = inputNormals.elementAt(i);
			Vector3f tanVectorN = Vector3f.neg(tanVector);

			// Angle between the normal and the previous normal:
			float angle = tanVector.angle(prevVector);    
			float angleN = tanVectorN.angle(prevVector);  
			if ( angle > angleN )
			{
				angle = angleN;
				tanVector = tanVectorN;
			}
			Vector3f rotationAxis = Vector3f.cross( prevVector, tanVector );
			rotationAxis.normalize();

			// Create a rotation angle-degrees about the rotation axis:
			Matrix3f transformTotal = new Matrix3f();
			transformTotal.fromAxisAngle( rotationAxis, angle );

			tanVectorResults[i] = new Vector3f(tanVector);

			Box[] results = getBoundingBox(transformTotal, verts, center, tempDiameter );


			Vector3f planeVector = Vector3f.add( results[0].corners[2], results[0].corners[3] );
			planeVector.scale(.5f);
			planeVector.sub(center);
			planeVector.normalize();

			if ( headPos == null )
			{
				headPos = findHead( center, planeVector, tanVector, image, ringDiameter, minPeakVal, minVal );
				if ( headPos != null )
				{
					positions.add( new Vector3f( center ) );
					directions.add( new Vector3f( tanVector ) );
				}
			}
			if ( tailPos == null )
			{
				tailPos = findTail( center, planeVector, tanVector, blurImage, tailDiameter, minPeakVal);
				if ( tailPos != null )
				{
					positions.add( new Vector3f( center ) );
					directions.add( new Vector3f( tanVector ) );
					tailPositions.add( new Vector3f( center ) );
					tailDirections.add( new Vector3f( tanVector ) );
				}
			}

			Vector3f radialCenter = findCenter( center, planeVector, tanVector, blurImage, innerDiameter, minPeakVal, minVal);
			minFoundAll &= (radialCenter != null);
			centerResults[i] = center;
			if ( radialCenter != null )
			{

				Matrix3f rotationM = new Matrix3f();
				Vector3f rotVector = new Vector3f();

				int iRadialSamples = 30;
				float fInvRS = 1.0f/iRadialSamples;

				int[] radiusCount = new int[(int) (outerDiameter - innerDiameter) + 1];
				boolean[] minFoundRadius = new boolean[(int) (outerDiameter - innerDiameter) + 1];
				for ( int j = 0; j < radiusCount.length; j++ )
				{
					radiusCount[j] = 0;
					minFoundRadius[j] = false;
				}
				for (int iR = 0; iR < iRadialSamples; iR++)
				{
					float fAngle = Mathf.TWO_PI*fInvRS*iR;
					rotationM.fromAxisAngle( tanVector,  fAngle);
					temp.copy( planeVector );
					rotationM.mult(temp, rotVector);

					for ( int j = (int) innerDiameter; j < outerDiameter; j++ )
					{
						temp.copy(rotVector);
						temp.scale(j);
						temp.add(center);
						//						temp.add(radialCenter);
						double val = blurImage.getFloatTriLinearBounds( temp.X, temp.Y, temp.Z );
						if ( val >= (minPeakVal/2f) )
						{
							radiusCount[(int) (j-innerDiameter)]++;
						}
						if ( val <= minVal )
						{
							minFoundRadius[(int) (j-innerDiameter)] = true;
						}
					}
				}
				int maxCount = 0;
				int maxRadius = 0;
				for ( int j = 1; j < radiusCount.length-1; j++ )
				{
					int sum = radiusCount[j-1] + radiusCount[j] + radiusCount[j+1];
					if ( !minFoundRadius[j] && (sum > maxCount) )
					{
						maxCount = sum;
						maxRadius = (int) (j + innerDiameter);
					}
				}
				if ( maxCount > 0 )
				{
					radiusCountResults[i] = maxCount;
					radiusResults[i] = maxRadius;
				}
			}
			else
			{
				radiusCountResults[i] = -1;
				radiusResults[i] = -1;				
			}
		}
		if ( minFoundAll )
		{
			return null;
		}

		int index = -1;
		int maxCount = -1;
		for ( int i = 0; i < radiusCountResults.length; i++ )
		{
			if ( radiusCountResults[i] > maxCount )
			{
				maxCount = radiusCountResults[i];
				index = i;
			}
		}
		if ( index != -1 )
		{				
			if ( radiusCountResults[index] > 50 )
			{
				Vector3f tanVector = tanVectorResults[index];
				if ( (positions != null) && (directions != null) )
				{
					Vector3f unitCenter = new Vector3f( Math.round(centerResults[index].X), Math.round(centerResults[index].Y), Math.round(centerResults[index].Z) );

					if ( !positions.contains(unitCenter) )
					{
						positions.add( unitCenter );
						tanVector.normalize();
						directions.add( new Vector3f(tanVector) );
					}
				}
				return centerResults[index];
			}
		}
		return null;
	}

	private Vector3f testSphereHead( ModelImage blurImage, Vector3f center, Vector<Vector3f> inputNormals,
			float ringDiameter, float minPeakVal, float minVal )
	{
		Box verts = new Box();
		verts.corners[0] = new Vector3f(-1,-1, 0);
		verts.corners[1] = new Vector3f( 1,-1, 0);
		verts.corners[2] = new Vector3f( 1, 1, 0);
		verts.corners[3] = new Vector3f(-1, 1, 0);
		Vector3f prevVector = new Vector3f(Vector3f.UNIT_Z);

		float tempDiameter = Math.min( blurImage.getExtents()[0], Math.min( blurImage.getExtents()[1], blurImage.getExtents()[2] ) );

		for ( int i = 0; i < inputNormals.size(); i++ )
		{
			Vector3f tanVector = inputNormals.elementAt(i);
			Vector3f tanVectorN = Vector3f.neg(tanVector);

			// Angle between the normal and the previous normal:
			float angle = tanVector.angle(prevVector);    
			float angleN = tanVectorN.angle(prevVector);  
			if ( angle > angleN )
			{
				angle = angleN;
				tanVector = tanVectorN;
			}
			Vector3f rotationAxis = Vector3f.cross( prevVector, tanVector );
			rotationAxis.normalize();

			// Create a rotation angle-degrees about the rotation axis:
			Matrix3f transformTotal = new Matrix3f();
			transformTotal.fromAxisAngle( rotationAxis, angle );

			Box[] results = getBoundingBox(transformTotal, verts, center, tempDiameter );

			Vector3f planeVector = Vector3f.add( results[0].corners[2], results[0].corners[3] );
			planeVector.scale(.5f);
			planeVector.sub(center);
			planeVector.normalize();



			Vector3f radialCenter = findHead( center, planeVector, tanVector, blurImage, ringDiameter, minPeakVal, minVal );
			if ( radialCenter != null )
			{
				return tanVector;
			}
		}
		return null;
	}

	private Vector3f testSphereInit( ModelImage blurImage, Vector<Vector3f> positions, Vector<Vector3f> directions,
			Vector3f center, Vector<Vector3f> inputNormals,
			float innerDiameter, float outerDiameter, float minVal, float minPeakVal )
	{
		Vector3f temp = new Vector3f();

		Box verts = new Box();
		verts.corners[0] = new Vector3f(-1,-1, 0);
		verts.corners[1] = new Vector3f( 1,-1, 0);
		verts.corners[2] = new Vector3f( 1, 1, 0);
		verts.corners[3] = new Vector3f(-1, 1, 0);
		Vector3f prevVector = new Vector3f(Vector3f.UNIT_Z);

		float tempDiameter = Math.min( blurImage.getExtents()[0], Math.min( blurImage.getExtents()[1], blurImage.getExtents()[2] ) );

		Vector3f[] tanVectorResults = new Vector3f[inputNormals.size()];
		Vector3f[] centerResults = new Vector3f[inputNormals.size()];
		int[] radiusCountResults = new int[inputNormals.size()];
		int[] radiusResults = new int[inputNormals.size()];

		boolean minFoundAll = true;
		for ( int i = 0; i < inputNormals.size(); i++ )
		{
			Vector3f tanVector = inputNormals.elementAt(i);
			Vector3f tanVectorN = Vector3f.neg(tanVector);

			// Angle between the normal and the previous normal:
			float angle = tanVector.angle(prevVector);    
			float angleN = tanVectorN.angle(prevVector);  
			if ( angle > angleN )
			{
				angle = angleN;
				tanVector = tanVectorN;
			}
			Vector3f rotationAxis = Vector3f.cross( prevVector, tanVector );
			rotationAxis.normalize();

			// Create a rotation angle-degrees about the rotation axis:
			Matrix3f transformTotal = new Matrix3f();
			transformTotal.fromAxisAngle( rotationAxis, angle );

			tanVectorResults[i] = new Vector3f(tanVector);

			Box[] results = getBoundingBox(transformTotal, verts, center, tempDiameter );


			Vector3f planeVector = Vector3f.add( results[0].corners[2], results[0].corners[3] );
			planeVector.scale(.5f);
			planeVector.sub(center);
			planeVector.normalize();



			Vector3f radialCenter = findCenter( center, planeVector, tanVector, blurImage, innerDiameter, minPeakVal, minVal);
			minFoundAll &= (radialCenter != null);
			centerResults[i] = center;//radialCenter;
			if ( radialCenter != null )
			{
				//				planeVector = Vector3f.add( results[0].corners[2], results[0].corners[3] );
				//				planeVector.scale(.5f);
				//				planeVector.sub(center);
				//				planeVector.normalize();

				Matrix3f rotationM = new Matrix3f();
				Vector3f rotVector = new Vector3f();

				int iRadialSamples = 30;
				float fInvRS = 1.0f/iRadialSamples;

				int[] radiusCount = new int[(int) (outerDiameter - innerDiameter) + 1];
				boolean[] minFoundRadius = new boolean[(int) (outerDiameter - innerDiameter) + 1];
				for ( int j = 0; j < radiusCount.length; j++ )
				{
					radiusCount[j] = 0;
					minFoundRadius[j] = false;
				}
				for (int iR = 0; iR < iRadialSamples; iR++)
				{
					float fAngle = Mathf.TWO_PI*fInvRS*iR;
					rotationM.fromAxisAngle( tanVector,  fAngle);
					temp.copy( planeVector );
					rotationM.mult(temp, rotVector);

					for ( int j = (int) innerDiameter; j < outerDiameter; j++ )
					{
						temp.copy(rotVector);
						temp.scale(j);
						temp.add(center);
						//						temp.add(radialCenter);
						double val = blurImage.getFloatTriLinearBounds( temp.X, temp.Y, temp.Z );
						if ( val >= (minPeakVal/2f) )
						{
							radiusCount[(int) (j-innerDiameter)]++;
						}
						if ( val <= minVal )
						{
							minFoundRadius[(int) (j-innerDiameter)] = true;
						}
					}
				}
				int maxCount = 0;
				int maxRadius = 0;
				for ( int j = 1; j < radiusCount.length-1; j++ )
				{
					int sum = radiusCount[j-1] + radiusCount[j] + radiusCount[j+1];
					if ( !minFoundRadius[j] && (sum > maxCount) )
					{
						maxCount = sum;
						maxRadius = (int) (j + innerDiameter);
					}
					//					System.err.println( i + "    " + minFoundRadius[j] + " " + radiusCount[j] + " " + (int) (j + innerDiameter) + "    " +  maxRadius );
				}
				if ( maxCount > 0 )
				{
					radiusCountResults[i] = maxCount;
					radiusResults[i] = maxRadius;

					//					try {
					//						blurImage.exportDiagonal( null, 0, i, new int[]{diameter,diameter}, results[0].corners, outerDiameter/2f, !fillData, values, true);
					//						ModelImage slice = new ModelImage( ModelStorageBase.FLOAT, new int[]{diameter,diameter}, blurImage.getImageName() + tanVector.toString() + "_" + i );
					//						slice.importData( values );
					//						slice.calcMinMax();
					//						new ViewJFrameImage(slice);
					//					} catch (IOException e) {
					//						// TODO Auto-generated catch block
					//						e.printStackTrace();
					//					}
				}






			}
			else
			{
				radiusCountResults[i] = -1;
				radiusResults[i] = -1;				
			}
			//			System.err.println( i + "    " + tanVector + " " + minFoundAll + " " + radiusCountResults[i] + " " + radiusResults[i] );
		}
		if ( minFoundAll )
		{
			return null;
		}


		int index = -1;
		int maxCount = -1;
		for ( int i = 0; i < radiusCountResults.length; i++ )
		{
			if ( radiusCountResults[i] > maxCount )
			{
				maxCount = radiusCountResults[i];
				index = i;
			}
		}
		if ( index != -1 )
		{				
			if ( radiusCountResults[index] > 50 )
			{
				//				System.err.println( "testSphererInit " + radiusCountResults[index] );
				Vector3f tanVector = tanVectorResults[index];
				//				Vector3f tanVectorN = Vector3f.neg(tanVector);
				//
				//				// Angle between the normal and the previous normal:
				//				float angle = tanVector.angle(prevVector);    
				//				float angleN = tanVectorN.angle(prevVector);  
				//				if ( angle > angleN )
				//				{
				//					angle = angleN;
				//					tanVector = tanVectorN;
				//				}
				//				Vector3f rotationAxis = Vector3f.cross( prevVector, tanVector );
				//				rotationAxis.normalize();
				//
				//				// Create a rotation angle-degrees about the rotation axis:
				//				Matrix3f transformTotal = new Matrix3f();
				//				transformTotal.fromAxisAngle( rotationAxis, angle );
				//
				//				Box[] results = getBoundingBox(transformTotal, verts, centerResults[index] );
				//				
				//				
				//				Vector3f planeVector = Vector3f.add( results[0].corners[2], results[0].corners[3] );
				//				planeVector.scale(.5f);
				//				planeVector.sub(centerResults[index]);
				//				planeVector.normalize();



				//				markCenter(duplicateMask, centerResults[index], planeVector, tanVector, radiusResults[index],
				//						blurImage.getExtents()[0], blurImage.getExtents()[1], blurImage.getExtents()[2] );

				if ( (positions != null) && (directions != null) )
				{
					Vector3f unitCenter = new Vector3f( Math.round(centerResults[index].X), Math.round(centerResults[index].Y), Math.round(centerResults[index].Z) );

					if ( !positions.contains(unitCenter) )
					{
						positions.add( unitCenter );
						tanVector.normalize();
						directions.add( new Vector3f(tanVector) );
						//						planeVector.scale( radiusResults[index]);
						//						directions.add( new Vector3f(planeVector) );
					}
				}
				return centerResults[index];
			}
		}
		return null;
	}

	private Vector3f testSphereTail( ModelImage blurImage, Vector3f center, Vector<Vector3f> inputNormals,
			float tailDiameter, float minPeakVal )
	{
		Box verts = new Box();
		verts.corners[0] = new Vector3f(-1,-1, 0);
		verts.corners[1] = new Vector3f( 1,-1, 0);
		verts.corners[2] = new Vector3f( 1, 1, 0);
		verts.corners[3] = new Vector3f(-1, 1, 0);
		Vector3f prevVector = new Vector3f(Vector3f.UNIT_Z);

		float tempDiameter = Math.min( blurImage.getExtents()[0], Math.min( blurImage.getExtents()[1], blurImage.getExtents()[2] ) );

		for ( int i = 0; i < inputNormals.size(); i++ )
		{
			Vector3f tanVector = inputNormals.elementAt(i);
			Vector3f tanVectorN = Vector3f.neg(tanVector);

			// Angle between the normal and the previous normal:
			float angle = tanVector.angle(prevVector);    
			float angleN = tanVectorN.angle(prevVector);  
			if ( angle > angleN )
			{
				angle = angleN;
				tanVector = tanVectorN;
			}
			Vector3f rotationAxis = Vector3f.cross( prevVector, tanVector );
			rotationAxis.normalize();

			// Create a rotation angle-degrees about the rotation axis:
			Matrix3f transformTotal = new Matrix3f();
			transformTotal.fromAxisAngle( rotationAxis, angle );

			Box[] results = getBoundingBox(transformTotal, verts, center, tempDiameter );

			Vector3f planeVector = Vector3f.add( results[0].corners[2], results[0].corners[3] );
			planeVector.scale(.5f);
			planeVector.sub(center);
			planeVector.normalize();



			Vector3f radialCenter = findTail( center, planeVector, tanVector, blurImage, tailDiameter, minPeakVal);
			if ( radialCenter != null )
			{
				return tanVector;
			}
		}
		return null;
	}

	private boolean testUpRightVectors( Vector<Box3f> planes )
	{
//		System.err.println( "testUpRightVectors" );
		boolean bResult = true;
		int count = 0;
		Vector<Vector3f> newTangents = new Vector<Vector3f>();
		for ( int i = 0; i < planes.size(); i++ )
		{
			if ( i > 0 )
			{
				IntrBox3Box3f intersect = new IntrBox3Box3f( planes.elementAt(i-1), planes.elementAt(i) );
				if (intersect.Test())
				{
//					System.err.println( "box " + i + " intersects box " + (i-1) );
					if ( i > 1 )
					{
						//System.err.println( tangents.elementAt(i-1) );
						Vector3f tanM1 = newTangents.elementAt(i-1);
						tanM1.copy( newTangents.elementAt(i-2));
						tanM1.add( curveTangents.elementAt(i));
						tanM1.scale(0.5f);
						//System.err.println( " -->  " + tangents.elementAt(i-1) );
						count++;
						bResult = false;
					}
					if ( i < planes.size() - 1 )
					{
						//System.err.println( tangents.elementAt(i-1) );
						Vector3f tan = curveTangents.elementAt(i);
						tan.copy( newTangents.elementAt(i-1));
						tan.add( curveTangents.elementAt(i+1));
						tan.scale(0.5f);
					}
					newTangents.add( new Vector3f( curveTangents.elementAt(i) ) );
				}
				else
				{
					newTangents.add( new Vector3f( curveTangents.elementAt(i) ) );
				}
			}
			else
			{
				newTangents.add( new Vector3f( curveTangents.elementAt(i) ) );
			}
		}
//		System.err.println( count + "    " + newTangents.size() + " " + curveTangents.size() );
		curveTangents.clear();
		curveTangents.addAll(newTangents);
		return bResult;
	}


	private VOI traceTracks( ModelImage image, float minPeakVal, Vector<Vector3f> positions, Vector<Vector3f> directions )
	{
		if ( (positions == null) || (directions == null) )
		{
			return null;
		}
		if ( (positions.size() == 0) || (directions.size() == 0) )
		{
			return null;
		}

		image.resetVOIs();
		short sID = (short)(image.getVOIs().getUniqueID());
		VOI trackB = new VOI(sID, "optimized", VOI.CONTOUR, 0f );
		trackB.setCurveType( VOI.CONTOUR );
		for ( int i = 0; i < positions.size(); i++ )
		{
			Vector3f start = positions.elementAt(i);
			Vector3f dir = directions.elementAt(i);
			dir.normalize();
			VOIContour track = new VOIContour(false);
			extendTracks( image, minPeakVal, positions, directions, null, start, dir, track, true );		
			Vector3f previousPt = null;
			if ( track.size() > 1 )
			{
				previousPt = track.elementAt(1);
			}
			extendTracks( image, minPeakVal, positions, directions, previousPt, start, Vector3f.neg(dir), track, false );	

			track.trimPoints(0.8f, true);
			if ( track.size() > 2 )
			{
				track.update( new ColorRGBA( (float)Math.random(), (float)Math.random(), (float)Math.random(), 1) );
				trackB.getCurves().add(track);
				//				System.err.println( "adding track " + track.size()  + "    " + i + " / " + positions.size() );
			}
		}
		if ( trackB.getCurves().size() > 0 )
		{
			//			System.err.println( "Number of tracks " + trackB.getCurves().size() );
			//			removeDuplicates(trackB);
			//			System.err.println( "remove duplicates done "  + trackB.getCurves().size() );
			//			jointracks( trackB );
			//			System.err.println( "join tracks done" );
			//			System.err.println( "Number of tracks " + trackB.getCurves().size() );
			//			image.registerVOI(trackB);
			//			System.err.println( "register VOI done" );
			return trackB;
		}
		return null;
	}

	private void transformBoxSlices( int index )
	{
		Vector3f kNormal = new Vector3f( curveTangents.elementAt(index-1) );
		Box box = unitBox.elementAt(index-1);

		//get coordinate of control point of b-spline
		Vector3f point = new Vector3f( curvePositions.elementAt(index) );

		Vector3f tanVector = new Vector3f( curveTangents.elementAt(index) );
		// Angle between the tangent and the 'default' z-direction:
		float angle = tanVector.angle(kNormal);    
		Vector3f rotationAxis = Vector3f.cross( kNormal, tanVector );
		rotationAxis.normalize();

		// Create a rotation angle-degrees about the rotation axis:
		Matrix3f transformTotal = new Matrix3f();
		transformTotal.fromAxisAngle( rotationAxis, angle );

		Box[] results = getBoundingBox( transformTotal, box, point, diameter );

		if ( testIntersection( boundingBoxes.elementAt(index-1), kNormal, results[0], tanVector ) )
		{
			//			System.err.println( "Intersection AAA" + index + " " + (index-1) );
			if ( (index - 2) >= 0)
			{
				// correct previous plane:
				kNormal.copy( curveTangents.elementAt(index-2) );
				box = unitBox.elementAt(index-2);
				//get coordinate of control point of b-spline
				point = new Vector3f( curvePositions.elementAt(index-1) );
			}
			else
			{
				// correct previous plane:
				kNormal.copy( origNormal );
				box = origBox;
				//get coordinate of control point of b-spline
				point = new Vector3f( curvePositions.elementAt(index-1) );
			}
			tanVector.add(kNormal);
			tanVector.scale(0.5f);
			tanVector.normalize();
			// Angle between the tangent and the 'default' z-direction:
			angle = tanVector.angle(kNormal);    
			rotationAxis = Vector3f.cross( kNormal, tanVector );
			rotationAxis.normalize();

			// Create a rotation angle-degrees about the rotation axis:
			transformTotal = new Matrix3f();
			transformTotal.fromAxisAngle( rotationAxis, angle );

			Box[] prevResults = getBoundingBox( transformTotal, box, point, diameter );
			curveTangents.set(index-1, new Vector3f(tanVector));
			boundingBoxes.set(index-1, new Box(prevResults[0]));
			unitBox.set(index-1, new Box(prevResults[1]));

			//recalculate current plane:
			kNormal = new Vector3f( curveTangents.elementAt(index-1) );
			box = unitBox.elementAt(index-1);

			//get coordinate of control point of b-spline
			point = new Vector3f( curvePositions.elementAt(index) );

			tanVector = new Vector3f( curveTangents.elementAt(index) );
			// Angle between the tangent and the 'default' z-direction:
			angle = tanVector.angle(kNormal);    
			rotationAxis = Vector3f.cross( kNormal, tanVector );
			rotationAxis.normalize();

			// Create a rotation angle-degrees about the rotation axis:
			transformTotal = new Matrix3f();
			transformTotal.fromAxisAngle( rotationAxis, angle );

			results = getBoundingBox( transformTotal, box, point, diameter );
		}				
		boundingBoxes.add(new Box(results[0]));
		unitBox.add(new Box(results[1]));

		//		if ( testIntersection( boundingBoxes.elementAt(index-1), curveTangents.elementAt(index-1),
		//				boundingBoxes.elementAt(index), curveTangents.elementAt(index) ) )
		//		{
		//			System.err.println( "Intersection BBB " + index + " " + (index-1) );
		//		}
	}


	private Box transformBoxSlicesInit( Box initialBox, Vector3f kPosition, Vector3f tangentVect )
	{
		Vector3f[] outVertices = new Vector3f[4];
		Box newBox = new Box();
		Vector3f centerModel = new Vector3f();

		for (int i = 0; i < 4; i++) {
			outVertices[i] = new Vector3f(initialBox.corners[i]);
			newBox.corners[i] = new Vector3f();
		}

		for (int i = 0; i < 4; i++) {			
			// Shift so the position is back to the cube with corners from (0,0,0) -> (2,2,2)
			outVertices[i].X += centerShift.X;
			outVertices[i].Y += centerShift.Y;
			outVertices[i].Z += centerShift.Z;

			outVertices[i].X /= 2f;
			outVertices[i].Y /= 2f;
			outVertices[i].Z /= 2f;


			// Scale back to the image dimensions:
			newBox.corners[i].X = (diameter * outVertices[i].X);
			newBox.corners[i].Y = (diameter * outVertices[i].Y);
			newBox.corners[i].Z = (diameter * outVertices[i].Z);

			centerModel.X += newBox.corners[i].X;
			centerModel.Y += newBox.corners[i].Y;
			centerModel.Z += newBox.corners[i].Z;
		}

		centerModel.X /= 4f;
		centerModel.Y /= 4f;
		centerModel.Z /= 4f;

		for (int i = 0; i < 4; i++) {
			// move from the center of the volume to the position:
			// (centerModel and kPosition are in image-index coordinates)
			newBox.corners[i].X += (kPosition.X - centerModel.X);
			newBox.corners[i].Y += (kPosition.Y - centerModel.Y);
			newBox.corners[i].Z += (kPosition.Z - centerModel.Z);
		}
		return newBox;
	}
}




