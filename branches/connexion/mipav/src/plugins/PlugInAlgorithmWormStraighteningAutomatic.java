import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBSpline;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
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
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;

import WildMagic.LibFoundation.Approximation.ApprEllipsoidFit3f;
import WildMagic.LibFoundation.Intersection.IntrBox3Box3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
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
	private float targetDistanceX;

	private float targetDistanceY;
	private float targetCross_P2_P0;

	private float targetCross_P3_P1;
	private float[] sourceResolutions;


	private float stepSize = 1.0f;
	private Vector<Vector3f> curvePositions = new Vector<Vector3f>();


	private Vector<Vector3f> curveTangents = new Vector<Vector3f>();
	private Vector<Box> unitBox = new Vector<Box>();
	private Vector<Box> boundingBoxes = new Vector<Box>();

	private Box origBox;

	private Vector3f origNormal;

	private float headDiameter = -1;
	private float tailDiameter = -1;
	private float maxDiameter = -1;

	private boolean fillData = false;
	private boolean displayOriginal = false;
	private boolean displayMask = false;


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


	public void runAlgorithm()
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

		//call the method that does the work
		extractPlaneSlices();


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
			System.err.println( "Rotating " + i );

			xfrm = new TransMatrix(4);
			xfrm.setRotate( 0, i+1, 0, TransMatrix.DEGREES );

			xfrm.transformAsPoint3Df(pointA, pointB);
			pointB.add( 128, 128, 128 );
			System.err.println( pointB );
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

		resultImage = new ModelImage(wormImage.getType(), resultExtents, wormImage.getImageName() + "_straigntened");
		JDialogBase.updateFileInfo( wormImage, resultImage );
		resultImage.setResolutions(resultResolutions);



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
//				System.err.println( "Y_AXIS_PLUS" );
				kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.Y_AXIS_PLUS);
			}
			else if ( origNormal.equals( Vector3f.UNIT_X_NEG ) )
			{
//				System.err.println( "Y_AXIS_MINUS" );
				kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.Y_AXIS_MINUS);
			}
			else if ( origNormal.equals( Vector3f.UNIT_Y ) )
			{
//				System.err.println( "X_AXIS_MINUS" );
				kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.X_AXIS_MINUS);
			}
			else if ( origNormal.equals( Vector3f.UNIT_Y_NEG ) )
			{
//				System.err.println( "X_AXIS_PLUS" );
				kRotate = new AlgorithmRotate(resultImage, AlgorithmRotate.X_AXIS_PLUS);
			}
			else if ( origNormal.equals( Vector3f.UNIT_Z_NEG ) )
			{
//				System.err.println( "Y_AXIS_180" );
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

		int[][] padW = new int[][]{{0,0},{0,0},{0,0}};
		int[][] padR = new int[][]{{0,0},{0,0},{0,0}};
		boolean bPadW = false;
		boolean bPadR = false;
		for ( int i = 0; i < 3; i++ )
		{
			if ( wormImage.getExtents()[i] < resultImage.getExtents()[i] )
			{
				padW[i][0] = (int) (Math.floor((resultImage.getExtents()[i] - wormImage.getExtents()[i])/2f));
				padW[i][1] = (int) (Math.ceil((resultImage.getExtents()[i] - wormImage.getExtents()[i])/2f));

				//				System.err.println( "PadW " + i + " " + padW[i][0] + " " + padW[i][1] );
				bPadW = true;
			}
			if ( wormImage.getExtents()[i] > resultImage.getExtents()[i] )
			{
				padR[i][0] = (int) (Math.floor((wormImage.getExtents()[i] - resultImage.getExtents()[i])/2f));
				padR[i][1] = (int) (Math.ceil((wormImage.getExtents()[i] - resultImage.getExtents()[i])/2f));

				//				System.err.println( "PadR " + i + " " + padR[i][0] + " " + padR[i][1] );
				bPadR = true;
			}
		}
//		ModelImage wormPad = null;
		ModelImage resultPad = null;
//		if ( bPadW )
//		{
//			wormPad = (ModelImage)wormImage.clone();
//			wormPad.setImageName( wormImage.getImageName() + "_pad" );
//
//			AlgorithmAddMargins kPad = new AlgorithmAddMargins(wormPad, padW[0], padW[1], padW[2] );
//			kPad.run();
//			wormPad = kPad.getDestImage();
//			wormPad.calcMinMax();
//			new ViewJFrameImage(wormPad);		
//			//            System.err.println( wormPad.getExtents()[0] + " " + wormPad.getExtents()[1] + " " + wormPad.getExtents()[2] );
//		}
		//        else
		if ( displayOriginal )
		{
			wormImage.resetVOIs();		
//			wormImage.registerVOI( samplingPlanes );
			wormImage.calcMinMax();
			new ViewJFrameImage(wormImage);
		}
		else
		{
			wormImage.disposeLocal();
			wormImage = null;
		}
		if ( bPadR )
		{
			resultPad = (ModelImage)resultImage.clone();
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

		targetDistanceX = scaledDistance( newBox.corners[1], newBox.corners[0], res );
		targetDistanceY = scaledDistance( newBox.corners[3], newBox.corners[0], res );
//		System.err.println( targetDistanceX + " " + targetDistanceY );


		targetCross_P2_P0 = scaledDistance( newBox.corners[2], newBox.corners[0], res );
		targetCross_P3_P1 = scaledDistance( newBox.corners[3], newBox.corners[1], res );
//		System.err.println( targetCross_P2_P0 + " " + targetCross_P3_P1 );
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

	private Box[] getBoundingBox( Matrix3f transformTotal, Box box, Vector3f point )
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
			// Shift so the position is back to the cube with corners from (0,0,0) -> (2,2,2)
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

		results = getBoundingBox(transformTotal, initBox, point );
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
			if ( (180*angle/Math.PI) > 90 )
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
			if ( (180*angle/Math.PI) > 90 )
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

		wormImage.resetVOIs();		
		wormImage.registerVOI( samplingPlanes );
		//		wormImage.registerVOI( intersectionLines );
		wormImage.calcMinMax();
		//		new ViewJFrameImage(wormImage);
		return true;
	}




	private boolean testIntersection( Box b1, Vector3f t1, Box b2, Vector3f t2 )
	{
		Box3f box1 = makeBox3f( b1, t1 );
		Box3f box2 = makeBox3f( b2, t2 );

		IntrBox3Box3f intersect = new IntrBox3Box3f( box1, box2 );
		return intersect.Test();
	}

	private boolean testUpRightVectors( Vector<Box3f> planes )
	{
		System.err.println( "testUpRightVectors" );
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
					System.err.println( "box " + i + " intersects box " + (i-1) );
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
		System.err.println( count + "    " + newTangents.size() + " " + curveTangents.size() );
		curveTangents.clear();
		curveTangents.addAll(newTangents);
		return bResult;
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

		Box[] results = getBoundingBox( transformTotal, box, point );

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

			Box[] prevResults = getBoundingBox( transformTotal, box, point );
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

			results = getBoundingBox( transformTotal, box, point );
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
