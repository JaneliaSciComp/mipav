import java.awt.Dimension;
import java.io.IOException;
import java.util.Vector;

import javax.media.j3d.GeometryArray;
import javax.media.j3d.QuadArray;
import javax.media.j3d.Transform3D;

import WildMagic.LibFoundation.Intersection.IntrLine3Plane3f;
import WildMagic.LibFoundation.Intersection.IntrSegment3Plane3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Line3f;
import WildMagic.LibFoundation.Mathematics.Matrix3f;
import WildMagic.LibFoundation.Mathematics.Plane3f;
import WildMagic.LibFoundation.Mathematics.Segment3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBSpline;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmExtractSurfaceCubes;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmConcatMult2Dto3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmExtractSlices;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmExtractSlicesVolumes;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmFlip;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.J3D.ViewJFrameVolumeView;


public class PlugInAlgorithmWormStraighteningAutomatic extends PlugInAlgorithmWormStraightening {

	private ModelImage wormImage, resultImage, matchImage;

	private int interpolationPts;

	private AlgorithmBSmooth smoothAlgo;

	private ViewJFrameImage vjf;

	private Vector3f[] boxSliceVertices;


	private float xBox, yBox, zBox;

	private float[] resols = new float[3];


	private int xDim, yDim, zDim;
	private int maxDim;
	private int minDim;

	private int diameter;
	private int[] resultExtents = new int[3];

	private int[] wormImageExtents;




	public PlugInAlgorithmWormStraighteningAutomatic(ModelImage wormImage, int interpolationPts, int diameter) {
		this.wormImage = wormImage;

		wormImageExtents = wormImage.getExtents();

		xDim = wormImage.getExtents()[0];
		yDim = wormImage.getExtents()[1];
		zDim = wormImage.getExtents()[2];

		maxDim = Math.max( Math.max( xDim, yDim ), zDim );
		minDim = Math.min( Math.min( xDim, yDim ), zDim );

		resols[0] = wormImage.getResolutions(0)[0];
		resols[1] = wormImage.getResolutions(0)[1];
		resols[2] = wormImage.getResolutions(0)[2];


		xBox = (xDim - 1) * resols[0];
		yBox = (yDim - 1) * resols[1];
		zBox = (zDim - 1) * resols[2];
		float maxBox = xBox;

		if (yBox > maxBox) {
			maxBox = yBox;
		}

		if (zBox > maxBox) {
			maxBox = zBox;
		}


		// Normalize the size
		// xBox range between 0 - 1.
		xBox = xBox / maxBox;
		yBox = yBox / maxBox;
		zBox = zBox / maxBox;

		this.diameter = diameter;
		System.err.println( xDim + " " + yDim + " " + zDim + " " + diameter );

		this.interpolationPts = interpolationPts;
	}


	public void runAlgorithm() {

		//first run b-spline algorithm on input points VOI
		VOIVector VOIs = wormImage.getVOIs();
		// reverses order of voi points:
		//Vector<VOIBase> points = VOIs.VOIAt(0).getCurves();
		//int size = points.size();
		//for ( int i = size-1; i >= 0; i-- )
		//{
		//	points.add( points.remove( i ) );
		//}

		smoothAlgo = new AlgorithmBSmooth(wormImage, VOIs.VOIAt(0), interpolationPts, false);
		smoothAlgo.run();

		//this is the result b-spline curve
		VOI resultVOI = smoothAlgo.getResultVOI();

		//remove old points voi
		VOIs.remove(0);

		//register new b-spline voi
		wormImage.registerVOI(resultVOI);

		//update image
		wormImage.notifyImageDisplayListeners();

		//call the method that does the work
		doPlanes();


		setCompleted(true);
	}





	/**
	 * doPlanes() works the following way:  It essentially determines the perpendicular planes through the worms
	 * at the points along the b-spline. It builds of a result image by stacking these planes.
	 */
	private void doPlanes() {
		//first create empty result image and set up voi to transfer points to result image\
		//!!!!!!!!!!!!!!!!!!NEED TO REMOVE THAT -1 AT SOME POINT
		resultExtents[0] = diameter;
		resultExtents[1] = diameter;
		resultExtents[2] = interpolationPts-2;

		float[] res = {wormImage.getResolutions(0)[0], wormImage.getResolutions(0)[1],wormImage.getResolutions(0)[2]};
		resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents, "RESULT IMAGE");
		FileInfoBase[] fileInfoBases = resultImage.getFileInfo();
		for (int i = 0; i < fileInfoBases.length; i++) {
			fileInfoBases[i].setEndianess(wormImage.getFileInfo()[0].getEndianess());
			fileInfoBases[i].setUnitsOfMeasure(wormImage.getFileInfo()[0].getUnitsOfMeasure());
			fileInfoBases[i].setResolutions(res);
		}
		resultImage.setFileInfo(fileInfoBases);
		VOI newPtVOI = new VOI((short) 0, "point3D.voi", VOI.POINT, -1.0f);
		newPtVOI.setUID(newPtVOI.hashCode());
		resultImage.registerVOI(newPtVOI);


		//In order to determine the perpendicular plane along each point, we also need the tangent vector (normal vector to plane) at that
		//point along the b-spline
		//set up points needed for bSplineJetXYZ() method which returns the tangent vector
		VOIVector VOIs = wormImage.getVOIs();
		Vector<VOIBase> contours = VOIs.VOIAt(0).getCurves();
		int nPoints = contours.size();
		float[] xPoints = new float[nPoints+5];
		float[] yPoints = new float[nPoints+5];
		float[] zPoints = new float[nPoints+5];

		Vector3f point = ((VOIPoint)contours.get(0)).exportPoint();
		xPoints[0] = point.X;
		yPoints[0] = point.Y;
		zPoints[0] = point.Z;

		point = ((VOIPoint)contours.get(0)).exportPoint();
		xPoints[1] = point.X;
		yPoints[1] = point.Y;
		zPoints[1] = point.Z;

		for (int i = 2; i < nPoints; i++) {
			point = ((VOIPoint)contours.get(i)).exportPoint();
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

		float angle;

		float[] values;
		int start = 0;
		int resultSliceSize = resultExtents[0] * resultExtents[1];

		Vector3f lpsOrigin = new Vector3f();
		Vector3f lpsPosition = new Vector3f();
		Vector3f lpsPositionPrev = new Vector3f();
		Vector3f kNormal = new Vector3f( Vector3f.UNIT_X );
		float lineLength = 0;
		int count = 0;
		for(int i=0;i< Math.min( interpolationPts-2, resultExtents[2]-1);i++) {

			//determining tangent vector at point
			int index = i;
			float floatIndex = (float)index + 2;
			Vector3f tanVector = bSplineAlgo.bSplineJetXYZ(1, floatIndex, xPoints, yPoints, zPoints);
			//normalizing tangent
			tanVector.Normalize();

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

				lineLength += lpsPositionPrev.Distance( lpsPosition );
				//System.err.println( lpsPositionPrev.Distance( lpsPosition ) + "       " + lineLength  + "          " + lineLength / (float)i);
				count++;
			}

			if ( i == 0 )
			{
				kNormal = findClosestNormal(kNormal, tanVector);
			}

			// Angle between the tangent and the 'default' z-direction:
			angle = tanVector.Angle(kNormal);       

			Vector3f rotationAxis = new Vector3f();
			rotationAxis.Cross( kNormal, tanVector );
			rotationAxis.Normalize();

			// Create a rotation angle-degrees about the rotation axis:
			Matrix3f transformTotal = new Matrix3f();
			transformTotal.FromAxisAngle( rotationAxis, angle );
			transformBoxSlices(transformTotal, point, tanVector);

			kNormal = new Vector3f( tanVector );
			//now we call export diagonal and pass in those vertices

			values = new float[resultExtents[0] * resultExtents[1]]; 
			try {
				//System.out.println("-------");
				//System.out.println(boxSliceVertices[0].X + " , " + boxSliceVertices[0].Y + " " + boxSliceVertices[0].Z);
				//System.out.println(boxSliceVertices[1].X + " , " + boxSliceVertices[1].Y + " " + boxSliceVertices[1].Z);
				//System.out.println(boxSliceVertices[2].X + " , " + boxSliceVertices[2].Y + " " + boxSliceVertices[2].Z);
				//System.out.println(boxSliceVertices[3].X + " , " + boxSliceVertices[3].Y + " " + boxSliceVertices[3].Z);

				wormImage.exportDiagonal(0, i, resultExtents, boxSliceVertices, values, true);

				if ( i == 0 )
				{
					MipavCoordinateSystems.fileToScanner( boxSliceVertices[0], lpsOrigin, wormImage );
				}

				VOIContour kBox = new VOIContour(true);
				for ( int pt = 0; pt < 4; pt++ )
				{
					kBox.addElement( boxSliceVertices[pt].X, boxSliceVertices[pt].Y, boxSliceVertices[pt].Z );
				}

				short sID = (short)(wormImage.getVOIs().getUniqueID());
				String kName = kBox.getClass().getName();
				int nameIndex = kName.lastIndexOf('.') + 1;
				kName = kName.substring(nameIndex);
				float fAngle = (float)((360 * angle) / (2.0 * Math.PI));
				VOI kBoxVOI = new VOI( sID,  kName + "_" + sID + "_" + fAngle, kBox.getType(), .7f );
				kBoxVOI.setOpacity(1f);
				kBoxVOI.getCurves().add(kBox);
				//kBox.update( new ColorRGBA(0,0,1,1) );			

				wormImage.registerVOI( kBoxVOI );

				resultImage.importData(i*values.length, values, false);
				start = start + resultSliceSize;
			}catch(IOException e) {
				e.printStackTrace();
			}
		}


		res[2] = lineLength / (float)count;
		resultImage.setResolutions(res);
		float[] alpsOrigin = new float[]{ lpsOrigin.X, lpsOrigin.Y, lpsOrigin.Z };
		fileInfoBases = resultImage.getFileInfo();
		for (int i = 0; i < fileInfoBases.length; i++) {
			fileInfoBases[i].setOrigin(alpsOrigin);
		}


		wormImage.calcMinMax();
		resultImage.calcMinMax();
		new ViewJFrameImage(resultImage);
	}




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
			kTransform.Mult(verts[i], outVertices[i]);

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
	
	
	/**
	 * Sets up the initial bounding box for the diagonal slice sample based on the first tangent in the bspline.
	 * @param kNormal
	 * @param kTangent
	 * @return
	 */
	private Vector3f findClosestNormal( Vector3f kNormal, Vector3f kTangent )
	{
		float angle = kTangent.Angle(kNormal);      

		float minAngle = angle;
		Vector3f minVec = new Vector3f(kNormal);

		float temp = kTangent.Angle( Vector3f.UNIT_X );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_X);
		}
		temp = kTangent.Angle( Vector3f.UNIT_X_NEG );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_X_NEG);
		}
		temp = kTangent.Angle( Vector3f.UNIT_Y );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_Y);
		}
		temp = kTangent.Angle( Vector3f.UNIT_Y_NEG );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_Y_NEG);
		}
		temp = kTangent.Angle( Vector3f.UNIT_Z );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_Z);
		}
		temp = kTangent.Angle( Vector3f.UNIT_Z_NEG );
		if ( temp < minAngle )
		{
			minAngle = temp;
			minVec = new Vector3f(Vector3f.UNIT_Z_NEG);
		}

		if (minVec.equals(kNormal) )
		{
			return kNormal;
		}
		if ( minVec.equals(Vector3f.UNIT_X ) )
		{
			verts[0] = new Vector3f(0,-1,-1);
			verts[1] = new Vector3f(0,-1, 1);
			verts[2] = new Vector3f(0, 1, 1);
			verts[3] = new Vector3f(0, 1,-1);
			return minVec;
		}
		if ( minVec.equals(Vector3f.UNIT_X_NEG ) )
		{
			verts[0] = new Vector3f(0, 1, 1);
			verts[1] = new Vector3f(0, 1,-1);
			verts[2] = new Vector3f(0,-1,-1);
			verts[3] = new Vector3f(0,-1, 1);
			return minVec;
		}
		if ( minVec.equals(Vector3f.UNIT_Y ) )
		{
			verts[0] = new Vector3f(-1, 0,-1);
			verts[1] = new Vector3f( 1, 0,-1);
			verts[2] = new Vector3f( 1, 0, 1);
			verts[3] = new Vector3f(-1, 0, 1);
			return minVec;
		}
		if ( minVec.equals(Vector3f.UNIT_Y_NEG ) )
		{
			verts[0] = new Vector3f( 1, 0, 1);
			verts[1] = new Vector3f(-1, 0, 1);
			verts[2] = new Vector3f(-1, 0,-1);
			verts[3] = new Vector3f( 1, 0,-1);
			return minVec;
		}
		if ( minVec.equals(Vector3f.UNIT_Z ) )
		{
			verts[0] = new Vector3f(-1,-1, 0);
			verts[1] = new Vector3f( 1,-1, 0);
			verts[2] = new Vector3f( 1, 1, 0);
			verts[3] = new Vector3f(-1, 1, 0);
			return minVec;
		}
		if ( minVec.equals(Vector3f.UNIT_Z_NEG ) )
		{
			verts[0] = new Vector3f( 1,-1, 0);
			verts[1] = new Vector3f(-1,-1, 0);
			verts[2] = new Vector3f(-1, 1, 0);
			verts[3] = new Vector3f( 1, 1, 0);
			return minVec;
		}
		return kNormal;
	}











}
