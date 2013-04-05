import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBSpline;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR2D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRotate;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.io.IOException;
import java.util.Vector;

import WildMagic.LibFoundation.Approximation.ApprEllipsoidFit3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmWormStraighteningAutomatic extends PlugInAlgorithmWormStraightening
{

	private ModelImage wormImage, resultImage;

	private int interpolationPts;

	private AlgorithmBSmooth smoothAlgo;

	private Vector3f[] boxSliceVertices;

	private int diameter;
	private int[] resultExtents = new int[3];
	private float[] resultResolutions = new float[3];
		
	private VOI originalPoints;

	private VOI originalCurve;
	private VOI samplingPlanes;
	private VOI transformLines;
	Vector3f centerModel = new Vector3f(0,0,0);
	Vector3f centerScreen = new Vector3f(0,0,0);
	Vector3f centerShift = new Vector3f(1,1,1);

	final Vector3f[] verts = new Vector3f[]
			{
			new Vector3f(0,-1,-1),
			new Vector3f(0,-1, 1),
			new Vector3f(0, 1, 1),
			new Vector3f(0, 1,-1)
			};
	
	
	ModelImage refImage = null;
	ModelImage matchImage = null;
	
	public PlugInAlgorithmWormStraighteningAutomatic(ModelImage wormImage)
	{
		this.wormImage = wormImage;
	}


	public void runAlgorithm() {

        short sID = (short)(wormImage.getVOIs().getUniqueID());

		Vector<VOIBase> kVectors = wormImage.getVOIs().VOIAt(0).getCurves();
		originalPoints = new VOI(sID, "originalPoints");
		originalPoints.setCurveType( VOI.POINT );

		float[] xPoints = new float[kVectors.size()];
		float[] yPoints = new float[kVectors.size()];
		float[] zPoints = new float[kVectors.size()];
		Vector3f temp;
		for ( int i = 0; i < kVectors.size(); i++ )
		{
			temp = kVectors.elementAt(i).elementAt(0);
			originalPoints.importPoint( temp );
			xPoints[i] = temp.X;
			yPoints[i] = temp.Y;
			zPoints[i] = temp.Z;
		}
		saveVOI("originalPoints", wormImage.getImageDirectory(), wormImage, originalPoints);

			
        AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints, zPoints);
        float totalL = arcLength.getTotalArcLength();
        System.err.println( "interpolationPts " + interpolationPts + " " + totalL );
        interpolationPts = (int)(totalL + 5);

		smoothAlgo = new AlgorithmBSmooth(wormImage, originalPoints, interpolationPts, false);
		smoothAlgo.run();
		//this is the result b-spline curve
		VOI resultVOI = smoothAlgo.getResultVOI();

		xPoints = new float[resultVOI.getCurves().size()];
		yPoints = new float[resultVOI.getCurves().size()];
		zPoints = new float[resultVOI.getCurves().size()];
		originalPoints.removeCurves();
		for ( int i = 0; i < resultVOI.getCurves().size(); i++ )
		{
			temp = resultVOI.getCurves().elementAt(i).elementAt(0);
			originalPoints.importPoint( temp );
			xPoints[i] = temp.X;
			yPoints[i] = temp.Y;
			zPoints[i] = temp.Z;
		}			
        arcLength = new AlgorithmArcLength(xPoints, yPoints, zPoints);
        totalL = arcLength.getTotalArcLength();
        System.err.println( "interpolationPts " + interpolationPts + " " + totalL );
        interpolationPts = (int)(totalL + 5);
        
		smoothAlgo = new AlgorithmBSmooth(wormImage, originalPoints, interpolationPts, false);
		smoothAlgo.run();
		resultVOI = smoothAlgo.getResultVOI();
		

        sID = (short)(wormImage.getVOIs().getUniqueID());
		originalCurve = new VOI(sID, "originalCurve");
		originalCurve.setCurveType( VOI.POINT );
		for ( int i = 0; i < resultVOI.getCurves().size(); i++ )
		{
			temp = resultVOI.getCurves().elementAt(i).elementAt(0);
			originalCurve.importPoint(  new Vector3f(temp) );
		}

		saveVOI("originalCurve", wormImage.getImageDirectory(), wormImage, originalCurve);

		//register new b-spline voi
		wormImage.resetVOIs();
		wormImage.registerVOI(resultVOI);

				
		//update image
		wormImage.notifyImageDisplayListeners();

		//call the method that does the work
		doPlanesWithRegistration();


		setCompleted(true);
	}
	

	private void doPlanesWithRegistration() {
		System.err.println("doPlanesWithRegistration");


		//In order to determine the perpendicular plane along each point, we also need the tangent vector (normal vector to plane) at that
		//point along the b-spline
		//set up points needed for bSplineJetXYZ() method which returns the tangent vector
		VOIVector VOIs = wormImage.getVOIs();
		Vector<VOIBase> contours = VOIs.VOIAt(0).getCurves();
		int nPoints = contours.size();
		float[] xPoints = new float[nPoints+5];
		float[] yPoints = new float[nPoints+5];
		float[] zPoints = new float[nPoints+5];
		
		System.err.println( nPoints );

		Vector3f point = ((VOIPoint)contours.get(0)).exportPoint();
		xPoints[0] = point.X;
		yPoints[0] = point.Y;
		zPoints[0] = point.Z;

		point = ((VOIPoint)contours.get(1)).exportPoint();
		xPoints[1] = point.X;
		yPoints[1] = point.Y;
		zPoints[1] = point.Z;

		Vector<Vector3f> contour = new Vector<Vector3f>();
		for (int i = 2; i < nPoints; i++) {
			point = ((VOIPoint)contours.get(i)).exportPoint();
			contour.add(new Vector3f(point));
			xPoints[i] = point.X;
			yPoints[i] = point.Y;
			zPoints[i] = point.Z;  
		}

		point = ((VOIPoint)contours.get(nPoints-1)).exportPoint();
		xPoints[nPoints + 2] = point.X;
		yPoints[nPoints + 2] = point.Y;
		zPoints[nPoints + 2] = point.Z;

		xPoints[nPoints + 3] = point.X;
		yPoints[nPoints + 3] = point.Y;
		zPoints[nPoints + 3] = point.Z;

		xPoints[nPoints + 4] = point.X;
		yPoints[nPoints + 4] = point.Y;
		zPoints[nPoints + 4] = point.Z;



		//alg to get tangent vector
		AlgorithmBSpline bSplineAlgo = smoothAlgo.getbSplineAlgo();
		

		float floatIndex = 2;
		Vector3f tanVector = bSplineAlgo.bSplineJetXYZ(1, floatIndex, xPoints, yPoints, zPoints);
		//normalizing tangent
		tanVector.normalize();
		Vector3f kNormal = new Vector3f( Vector3f.UNIT_X );
		//kNormal = findClosestNormal(kNormal, tanVector);
		kNormal = findClosestNormal(contour, kNormal);
		Vector3f origNormal = new Vector3f(kNormal);
		Vector3f origPoint = null;
		
		findClosestExtentsResolutions(kNormal, wormImage.getExtents(), wormImage.getResolutions(0) );
		
		resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents, wormImage.getImageName() + "_straigntened");
		JDialogBase.updateFileInfo( wormImage, resultImage );
		resultImage.setResolutions(resultResolutions);
		
		
		
		
		
		
		
		
		
		

		short sID = (short)(resultImage.getVOIs().getUniqueID());
		VOI kPVOI = new VOI( sID,  "resultVOI" );

        sID = (short)(wormImage.getVOIs().getUniqueID());
		samplingPlanes = new VOI(sID, "samplingPlanes");
		
		

		float angle;

		float[][] values = new float[resultExtents[2]][resultExtents[0] * resultExtents[1]]; 
		int start = 0;
		int resultSliceSize = resultExtents[0] * resultExtents[1];

		Matrix3f origTransform = null;

		Vector3f lpsOrigin = new Vector3f();
		Vector3f lpsPosition = new Vector3f();
		Vector3f lpsPositionPrev = new Vector3f();
		float lineLength = 0;
		int count = 0;
		for(int i=0;i< Math.min( interpolationPts-2, resultExtents[2]-1);i++) {

			//determining tangent vector at point
			int index = i;
			floatIndex = (float)index + 2;
			tanVector = bSplineAlgo.bSplineJetXYZ(1, floatIndex, xPoints, yPoints, zPoints);
			//normalizing tangent
			tanVector.normalize();

			//get coordinate of control point of b-spline
			point = ((VOIPoint)contours.get(index)).exportPoint();

			if ( i == 0 )
			{
				MipavCoordinateSystems.fileToScanner( point, lpsPosition, wormImage );
			}
			if ( i > 0 )
			{
				lpsPositionPrev = new Vector3f( lpsPosition );
				MipavCoordinateSystems.fileToScanner( point, lpsPosition, wormImage );

				lineLength += lpsPositionPrev.distance( lpsPosition );
				//System.err.println( lpsPositionPrev.distance( lpsPosition ) + "       " + lineLength  + "          " + lineLength / (float)i);
				count++;
			}

			if ( i == 0 )
			{
//				kNormal = findClosestNormal(kNormal, tanVector);
//				origNormal = new Vector3f(kNormal);
				origPoint = new Vector3f(point);
			}

			// Angle between the tangent and the 'default' z-direction:
			angle = tanVector.angle(kNormal);    
			Vector3f rotationAxis = Vector3f.cross( kNormal, tanVector );
			rotationAxis.normalize();
			if ( (180*angle/Math.PI) > 45 )
			{
//				System.err.println( "" );
//				System.err.println( "" );
//				System.err.println( "" );
//				System.err.println( "" );
//				System.err.println( "ANGLE " + angle );
//				System.err.println( tanVector );
//				System.err.println( kNormal );
				tanVector.neg();
				angle = tanVector.angle(kNormal);    
				rotationAxis = Vector3f.cross( kNormal, tanVector );
				rotationAxis.normalize();
//				System.err.println( "ANGLE " + angle );
			}

			// Create a rotation angle-degrees about the rotation axis:
			Matrix3f transformTotal = new Matrix3f();
			transformTotal.fromAxisAngle( rotationAxis, angle );
			if ( i == 0 )
			{
				origTransform = new Matrix3f(transformTotal);
			}
			
			transformBoxSlices(transformTotal, point, tanVector);

			kNormal = new Vector3f( tanVector );
			//now we call export diagonal and pass in those vertices

			try {
//				System.out.println("-------");
//				System.out.println(boxSliceVertices[0].X + " , " + boxSliceVertices[0].Y + " , " + boxSliceVertices[0].Z);
//				System.out.println(boxSliceVertices[1].X + " , " + boxSliceVertices[1].Y + " , " + boxSliceVertices[1].Z);
//				System.out.println(boxSliceVertices[2].X + " , " + boxSliceVertices[2].Y + " , " + boxSliceVertices[2].Z);
//				System.out.println(boxSliceVertices[3].X + " , " + boxSliceVertices[3].Y + " , " + boxSliceVertices[3].Z);

				wormImage.exportDiagonal(0, i, resultExtents, boxSliceVertices, values[i], true);

				if ( i == 0 )
				{
					MipavCoordinateSystems.fileToScanner( boxSliceVertices[0], lpsOrigin, wormImage );
				}
				else if ( i > 0 )
				{
					float fAngle = (float)testRegistration( values[i-1], values[i], resultExtents );
//					if ( (180*angle/Math.PI) > 45 )
//					{
//						System.out.println( i + "   ANGLES  " + (180*angle/Math.PI) + "   " + (180*fAngle/Math.PI) );
//					}
					if ( Math.abs(fAngle) > 0 )
					{

						if ( refImage != null )
						{
							refImage.disposeLocal();
							refImage = null;
						}
						if ( matchImage != null )
						{
							matchImage.disposeLocal();
							matchImage = null;
						}
						
						registerPlanes( point, tanVector, fAngle );
						wormImage.exportDiagonal(0, i, resultExtents, boxSliceVertices, values[i], true);
					}
				}

				VOIContour kBox = new VOIContour(true);
				for ( int pt = 0; pt < 4; pt++ )
				{
					kBox.addElement( boxSliceVertices[pt].X, boxSliceVertices[pt].Y, boxSliceVertices[pt].Z );
				}
				kBox.update( new ColorRGBA(0,0,1,1) );			
				samplingPlanes.importCurve(kBox);


				VOIPoint kPointVOI = new VOIPoint();
				kPointVOI.add(new Vector3f( resultExtents[0] / 2, resultExtents[1] / 2, i ) );
				resultImage.importData(i*values[i].length, values[i], false);
				kPVOI.getCurves().add(kPointVOI);
				start = start + resultSliceSize;
			}catch(IOException e) {
				e.printStackTrace();
			}
		}

		if ( refImage != null )
		{
			refImage.disposeLocal();
			refImage = null;
		}
		if ( matchImage != null )
		{
			matchImage.disposeLocal();
			matchImage = null;
		}

		float[] alpsOrigin = new float[]{ lpsOrigin.X, lpsOrigin.Y, lpsOrigin.Z };
		FileInfoBase[] fileInfoBases = resultImage.getFileInfo();
		for (int i = 0; i < fileInfoBases.length; i++) {
			fileInfoBases[i].setOrigin(alpsOrigin);
		}

		if ( origTransform != null )
		{
			System.err.println( origTransform );
		}
		
        TransMatrix matrix = new TransMatrix(4);

        matrix.set(0,0,(double)origTransform.get(0,0));
        matrix.set(0,1,(double)origTransform.get(0,1));
        matrix.set(0,2,(double)origTransform.get(0,2));
        
        matrix.set(1,0,(double)origTransform.get(1,0));
        matrix.set(1,1,(double)origTransform.get(1,1));
        matrix.set(1,2,(double)origTransform.get(1,2));
        
        matrix.set(2,0,(double)origTransform.get(2,0));
        matrix.set(2,1,(double)origTransform.get(2,1));
        matrix.set(2,2,(double)origTransform.get(2,2));
                

        TransMatrix toOrigin = new TransMatrix(4);
        toOrigin.setTranslate(-resultExtents[0]/2f, -resultExtents[1]/2f, 0);
        
        TransMatrix fromOrigin = new TransMatrix(4);
		fromOrigin.setTranslate(resultExtents[0]/2f, resultExtents[1]/2f, 0);
        
        matrix.multLeft(toOrigin).mult(fromOrigin);
        
        resultImage.setMatrix( matrix );

        System.err.println( "" );
        System.err.println( "" );
        System.err.println( "" );
        System.err.println( "Origninal Normal" );
        System.err.println( origNormal );
        System.err.println( "Origninal Point" );
        System.err.println( origPoint );
        
		resultImage.registerVOI( kPVOI );
		
		
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
        	}
             
        }

        System.err.println( "" );
        System.err.println( "" );
        System.err.println( "" );
        System.err.println( "RESOLUTIONS" );
        System.err.println( wormImage.getResolutions(0)[0] + " " + wormImage.getResolutions(0)[1] + " " + wormImage.getResolutions(0)[2] );
        System.err.println( resultImage.getResolutions(0)[0] + " " + resultImage.getResolutions(0)[1] + " " + resultImage.getResolutions(0)[2] );
        
        System.err.println( "EXTENTS" );
        System.err.println( wormImage.getExtents()[0] + " " + wormImage.getExtents()[1] + " " + wormImage.getExtents()[2] );
        System.err.println( resultImage.getExtents()[0] + " " + resultImage.getExtents()[1] + " " + resultImage.getExtents()[2] );

        int[][] padW = new int[][]{{0,0},{0,0},{0,0}};
        int[][] padR = new int[][]{{0,0},{0,0},{0,0}};
        boolean bPadW = false;
        boolean bPadR = false;
        for ( int i = 0; i < 3; i++ )
        {
        	if ( wormImage.getExtents()[i] < resultImage.getExtents()[i] )
        	{
        		padW[i][0] = (resultImage.getExtents()[i] - wormImage.getExtents()[i])/2;
        		padW[i][1] = (resultImage.getExtents()[i] - wormImage.getExtents()[i])/2;
        		bPadW = true;
        	}
        	if ( wormImage.getExtents()[i] > resultImage.getExtents()[i] )
        	{
        		padR[i][0] = (wormImage.getExtents()[i] - resultImage.getExtents()[i])/2;
        		padR[i][1] = (wormImage.getExtents()[i] - resultImage.getExtents()[i])/2;
        		bPadR = true;
        	}
        }
        ModelImage wormPad = null;
        ModelImage resultPad = null;
        if ( bPadW )
        {
        	wormPad = (ModelImage)wormImage.clone();
        	AlgorithmAddMargins kPad = new AlgorithmAddMargins(wormPad, padW[0], padW[1], padW[2] );
        	kPad.run();
        	wormPad = kPad.getDestImage();
            System.err.println( wormPad.getExtents()[0] + " " + wormPad.getExtents()[1] + " " + wormPad.getExtents()[2] );
        }
        if ( bPadR )
        {
        	resultPad = (ModelImage)resultImage.clone();
        	AlgorithmAddMargins kPad = new AlgorithmAddMargins(resultPad, padR[0], padR[1], padR[2] );
        	kPad.run();
        	resultPad = kPad.getDestImage();
            System.err.println( resultPad.getExtents()[0] + " " + resultPad.getExtents()[1] + " " + resultPad.getExtents()[2] );
        }
        

		VOIVector wormVOIs = (wormPad != null) ? wormPad.getVOIs() : wormImage.getVOIs();
		Vector<VOIBase> wormContours = wormVOIs.VOIAt(0).getCurves();
		
		VOIVector resultVOIs = (resultPad != null) ? resultPad.getVOIs() : resultImage.getVOIs();
		Vector<VOIBase> resultContours = resultVOIs.VOIAt(0).getCurves();

		sID = (short)(resultImage.getVOIs().getUniqueID());
		transformLines = new VOI( sID, "transformLines" );
		for ( int i = 0; i < Math.min(wormContours.size(),resultContours.size()); i++ )
		{
			Vector3f point1 = ((VOIPoint)wormContours.get(i)).exportPoint();
			Vector3f point2 = ((VOIPoint)resultContours.get(i)).exportPoint();
			VOIContour kLine = new VOIContour(true);
			kLine.addElement( new Vector3f(point1) );
			kLine.addElement( new Vector3f(point2) );
			transformLines.importCurve(kLine);
		}
		saveVOI( "transformLines", resultImage.getImageDirectory(), resultImage, transformLines );
		
		wormImage.resetVOIs();		
		wormImage.registerVOI( samplingPlanes );
		if ( wormPad != null )
		{
			wormPad.calcMinMax();
			new ViewJFrameImage(wormPad);			

			ModelImage.saveImage( wormPad, wormImage.getImageName() + "_Resampled.tif", wormImage.getImageDirectory() );
		}
		
		resultImage.resetVOIs();
		if ( resultPad != null )
		{
			resultPad.registerVOI( transformLines );
			resultPad.calcMinMax();
			new ViewJFrameImage(resultPad);
			ModelImage.saveImage( resultPad, wormImage.getImageName() + "_Straigntened.tif", wormImage.getImageDirectory() );
		}
		else
		{
			resultImage.registerVOI( transformLines );		
			ModelImage.saveImage( resultImage, wormImage.getImageName() + "_Straigntened.tif", wormImage.getImageDirectory() );	
		}
        
		wormImage.calcMinMax();
		new ViewJFrameImage(wormImage);
		resultImage.calcMinMax();
		new ViewJFrameImage(resultImage);
	}
		
	
	private void findClosestExtentsResolutions( Vector3f kNormal, int[] extents, float[] res )
	{
		if ( kNormal.equals(Vector3f.UNIT_X ) || kNormal.equals(Vector3f.UNIT_X_NEG ) )
		{
			resultExtents[0] = extents[2];
			resultExtents[1] = extents[1];
			
			resultResolutions[0] = res[2];
			resultResolutions[1] = res[1];
		}
		else if ( kNormal.equals(Vector3f.UNIT_Y ) || kNormal.equals(Vector3f.UNIT_Y_NEG ) )
		{
			resultExtents[0] = extents[0];
			resultExtents[1] = extents[2];
			
			resultResolutions[0] = res[0];
			resultResolutions[1] = res[2];
		}
		else if ( kNormal.equals(Vector3f.UNIT_Z ) || kNormal.equals(Vector3f.UNIT_Z_NEG ) )
		{
			resultExtents[0] = extents[0];
			resultExtents[1] = extents[1];
			
			resultResolutions[0] = res[0];
			resultResolutions[1] = res[1];
		}
		resultExtents[2] = interpolationPts-2;
		resultResolutions[2] = res[2];
		
		diameter = resultExtents[0];

//        int minExtent = Math.min( wormImage.getExtents()[0], wormImage.getExtents()[1]);
//        if ( wormImage.getExtents().length > 2 )
//        {
//        	minExtent = Math.min( minExtent, wormImage.getExtents()[2]);
//        }
//		diameter = minExtent;
	}


	/**
	 * Sets up the initial bounding box for the diagonal slice sample based on the first tangent in the bspline.
	 * @param kNormal
	 * @param kTangent
	 * @return
	 */
	private Vector3f findClosestNormal( Vector<Vector3f> contour, Vector3f kNormal )
	{
		// The contour is 3-dimensional (not entirely on one of the three orthogonal planes):
        Vector3f kUp = new Vector3f();
        Matrix3f kMat = new Matrix3f();
        float[] afScale = new float[3];

        // Try different Powell for fitting the points.
        new ApprEllipsoidFit3f( contour.size(), contour, kUp, kMat, afScale );
        
        Vector3f kX = new Vector3f( kMat.M00, kMat.M01, kMat.M02 );  kX.normalize();
        Vector3f kY = new Vector3f( kMat.M10, kMat.M11, kMat.M12 );  kY.normalize();
        Vector3f kZ = new Vector3f( kMat.M20, kMat.M21, kMat.M22 );  kZ.normalize();
        
        //Vector3f[] kBasis = new Vector3f[]{ kX, kY, kZ };
        kMat = new Matrix3f( kX, kY, kZ, false );
        Vector3f kRot = kMat.mult( Vector3f.UNIT_Z_NEG );             
        
        System.err.println( kRot );
        return findClosestNormal( kNormal, kRot );        
	}

	
	
	/**
	 * Sets up the initial bounding box for the diagonal slice sample based on the first tangent in the bspline.
	 * @param kNormal
	 * @param kTangent
	 * @return
	 */
	private Vector3f findClosestNormal( Vector3f kNormal, Vector3f kTangent )
	{
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
			return kNormal;
		}
		if ( minVec.equals(Vector3f.UNIT_X ) || minVec.equals(Vector3f.UNIT_X_NEG ) )
		{
			verts[0] = new Vector3f(0,-1,-1);
			verts[1] = new Vector3f(0,-1, 1);
			verts[2] = new Vector3f(0, 1, 1);
			verts[3] = new Vector3f(0, 1,-1);
			return minVec;
		}
//		if ( minVec.equals(Vector3f.UNIT_X_NEG ) )
//		{
//			verts[0] = new Vector3f(0, 1, 1);
//			verts[1] = new Vector3f(0, 1,-1);
//			verts[2] = new Vector3f(0,-1,-1);
//			verts[3] = new Vector3f(0,-1, 1);
//			return minVec;
//		}
		if ( minVec.equals(Vector3f.UNIT_Y ) || minVec.equals(Vector3f.UNIT_Y_NEG ) )
		{
			verts[0] = new Vector3f(-1, 0,-1);
			verts[1] = new Vector3f( 1, 0,-1);
			verts[2] = new Vector3f( 1, 0, 1);
			verts[3] = new Vector3f(-1, 0, 1);
			return minVec;
		}
//		if ( minVec.equals(Vector3f.UNIT_Y_NEG ) )
//		{
//			verts[0] = new Vector3f( 1, 0, 1);
//			verts[1] = new Vector3f(-1, 0, 1);
//			verts[2] = new Vector3f(-1, 0,-1);
//			verts[3] = new Vector3f( 1, 0,-1);
//			return minVec;
//		}
		if ( minVec.equals(Vector3f.UNIT_Z ) || minVec.equals(Vector3f.UNIT_Z_NEG ) )
		{
			verts[0] = new Vector3f(-1,-1, 0);
			verts[1] = new Vector3f( 1,-1, 0);
			verts[2] = new Vector3f( 1, 1, 0);
			verts[3] = new Vector3f(-1, 1, 0);
			return minVec;
		}
//		if ( minVec.equals(Vector3f.UNIT_Z_NEG ) )
//		{
//			verts[0] = new Vector3f( 1,-1, 0);
//			verts[1] = new Vector3f(-1,-1, 0);
//			verts[2] = new Vector3f(-1, 1, 0);
//			verts[3] = new Vector3f( 1, 1, 0);
//			return minVec;
//		}
		return kNormal;
	}
	
	private void registerPlanes( Vector3f kPosition, Vector3f tangentVect, float fAngle )
	{
		fAngle = (float)((Math.PI * fAngle) / 180f);
		// Create a rotation angle-degrees about the rotation axis:
		Matrix3f kSpinTransform = new Matrix3f(false);
		kSpinTransform.fromAxisAngle( tangentVect, fAngle );

		centerModel.X = 0;
		centerModel.Y = 0;
		centerModel.Z = 0;
		
		Vector3f[] outVertices = new Vector3f[4];
		for (int i = 0; i < 4; i++) {
			outVertices[i] = new Vector3f();
			boxSliceVertices[i] = new Vector3f();

			// Rotate the points in the bounding box:
			// verts is a 'unit' cube centered at (0,0,0) with
			// corners from (-1,-1,-1) -> (1,1,1)
			kSpinTransform.mult(verts[i], outVertices[i]);
			
			verts[i].X = outVertices[i].X;
			verts[i].Y = outVertices[i].Y;
			verts[i].Z = outVertices[i].Z;
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
			boxSliceVertices[i].X = (diameter - 1) * outVertices[i].X;
			boxSliceVertices[i].Y = (diameter - 1) * outVertices[i].Y;
			boxSliceVertices[i].Z = (diameter - 1) * outVertices[i].Z;

			centerModel.X += boxSliceVertices[i].X;
			centerModel.Y += boxSliceVertices[i].Y;
			centerModel.Z += boxSliceVertices[i].Z;
		}

		centerModel.X /= 4f;
		centerModel.Y /= 4f;
		centerModel.Z /= 4f;

		for (int i = 0; i < 4; i++) {
			// move from the center of the volume to the position:
			// (centerModel and kPosition are in image-index coordinates)
			boxSliceVertices[i].X += (kPosition.X - centerModel.X);
			boxSliceVertices[i].Y += (kPosition.Y - centerModel.Y);
			boxSliceVertices[i].Z += (kPosition.Z - centerModel.Z);
		}
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
	
	private double testRegistration( float[] targetSlice, float[] newSlice, int[] extents )
	{
		if ( matchImage != null )
		{
			refImage = matchImage;
		}
		else
		{
			refImage = new ModelImage( ModelStorageBase.FLOAT, new int[]{extents[0], extents[1]}, "ref" );
		}
		matchImage = new ModelImage( ModelStorageBase.FLOAT, new int[]{extents[0], extents[1]}, "match" );
		try {
			refImage.importData( 0, targetSlice, true );
			matchImage.importData( 0, newSlice, true );
		} catch (IOException e) { }

		AlgorithmRegOAR2D reg = new AlgorithmRegOAR2D(refImage, matchImage, 
        		AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED, 1, AlgorithmTransform.BILINEAR, -45f, 45f, 1f,
                                    .5f, false, true);
		reg.run();
		double angle = -reg.getRotation();
		
		refImage.disposeLocal();
		refImage = null;
		
		return angle;//( (180f*angle)/(Math.PI) );
	}




	/**
	 * transform the points used to render the triSliceImages textures.
	 * 
	 * @param kTransform the Transform3D used to rotate the boxes.
	 */
	private void transformBoxSlices(Matrix3f kTransform, Vector3f kPosition, Vector3f tangentVect ) {
		if (kTransform == null) {
			kTransform = new Matrix3f(false);
		}
		Vector3f[] outVertices = new Vector3f[4];

		boxSliceVertices = new Vector3f[4];

		centerModel.X = 0;
		centerModel.Y = 0;
		centerModel.Z = 0;
		for (int i = 0; i < 4; i++) {
			outVertices[i] = new Vector3f();
			boxSliceVertices[i] = new Vector3f();

			// Rotate the points in the bounding box:
			// verts is a 'unit' cube centered at (0,0,0) with
			// corners from (-1,-1,-1) -> (1,1,1)
			kTransform.mult(verts[i], outVertices[i]);

			verts[i].X = outVertices[i].X;
			verts[i].Y = outVertices[i].Y;
			verts[i].Z = outVertices[i].Z;
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
			boxSliceVertices[i].X = (diameter - 1) * outVertices[i].X;
			boxSliceVertices[i].Y = (diameter - 1) * outVertices[i].Y;
			boxSliceVertices[i].Z = (diameter - 1) * outVertices[i].Z;

			centerModel.X += boxSliceVertices[i].X;
			centerModel.Y += boxSliceVertices[i].Y;
			centerModel.Z += boxSliceVertices[i].Z;
		}

		centerModel.X /= 4f;
		centerModel.Y /= 4f;
		centerModel.Z /= 4f;

		for (int i = 0; i < 4; i++) {
			// move from the center of the volume to the position:
			// (centerModel and kPosition are in image-index coordinates)
			boxSliceVertices[i].X += (kPosition.X - centerModel.X);
			boxSliceVertices[i].Y += (kPosition.Y - centerModel.Y);
			boxSliceVertices[i].Z += (kPosition.Z - centerModel.Z);
		}
	}



}
