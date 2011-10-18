import java.awt.Dimension;
import java.io.IOException;
import java.util.Vector;

import javax.media.j3d.GeometryArray;
import javax.media.j3d.QuadArray;
import javax.media.j3d.Transform3D;
import javax.vecmath.AxisAngle4f;
import javax.vecmath.Matrix3f;
import javax.vecmath.Point3f;
import javax.vecmath.Vector3d;

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
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.renderer.J3D.ViewJFrameVolumeView;


public class PlugInAlgorithmWormStraightening extends AlgorithmBase {

	private ModelImage wormImage, resultImage, matchImage;

	private int interpolationPts;

	private AlgorithmBSmooth smoothAlgo;

	private ViewJFrameImage vjf;

	private Vector3f[] boxSliceVertices;


	private float xBox, yBox, zBox;

	private float[] resols = new float[3];


	private int xDim, yDim, zDim;

	private int[] resultExtents = new int[3];

	private int[] wormImageExtents;




	public PlugInAlgorithmWormStraightening(ModelImage wormImage, int interpolationPts) {
		this.wormImage = wormImage;

		wormImageExtents = wormImage.getExtents();

		xDim = wormImage.getExtents()[0];
		yDim = wormImage.getExtents()[1];
		zDim = wormImage.getExtents()[2];


		centerModel = new Point3f((xDim - 1)/2f, (yDim - 1)/2f, (zDim - 1)/2f);

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

		this.interpolationPts = interpolationPts;
	}


	public void runAlgorithm() {

		//first run b-spline algorithm on input points VOI
		VOIVector VOIs = wormImage.getVOIs();
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


		//testOutputZ();
		//testOutputRotateY();
		
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
		resultExtents[0] = 400;
		resultExtents[1] = 400;
		resultExtents[2] = 400;

		//resultExtents = wormImage.getExtents();

		float[] res = {wormImage.getResolutions(0)[0], wormImage.getResolutions(0)[1],wormImage.getResolutions(0)[2]};
		resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents, "RESULT IMAGE");
		resultImage.setResolutions(res);
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




		//In oreder to determine the perpendicalur plane along each point, we also need the tangent vector (normal vector to plane) at that
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

		//when wanting to visualize the planes on the original image points...first blank out the image
		//!!!!!!make sure you comment this out when wanting to populate result image
		//wormImage.setAll(0);

		javax.vecmath.Vector3f imageOrientationVectZ = new javax.vecmath.Vector3f(0,0,1);
		float angle;

		//Vector3f[] tanVectors = new Vector3f[interpolationPts-1];

		float[] values;
		int start = 0;
		int resultSliceSize = resultExtents[0] * resultExtents[1];
		for(int i=0;i< Math.min( interpolationPts-1, resultExtents[2]-1);i++) {

			//determining tangent vector at point
			int index = i;
			float floatIndex = (float)index + 2;
			Vector3f tanVector = bSplineAlgo.bSplineJetXYZ(1, floatIndex, xPoints, yPoints, zPoints);
			//normalizing tangent
			tanVector.Normalize();

			//get coordinate of control point of b-spline
			point = ((VOIPoint)contours.get(index)).exportPoint();
			Point3f position = new Point3f(point.X, point.Y, point.Z );

			// Angle between the tangent and the 'default' z-direction:
			angle = tanVector.Angle(Vector3f.UNIT_Z);             
			javax.vecmath.Vector3f rotationAxis = new javax.vecmath.Vector3f();

			// rotation axis is the cross product of the 'default' z-direction and the tangent:
			javax.vecmath.Vector3f tangentVect = new javax.vecmath.Vector3f(tanVector.X, tanVector.Y, tanVector.Z );
			rotationAxis.cross( tangentVect, imageOrientationVectZ );

			// Create a rotation angle-degrees about the rotation axis:
			Transform3D transformTotal = new Transform3D();
			transformTotal.setRotation( new AxisAngle4f( rotationAxis, angle ) );
			// Transform boxSliceVertices (rotation, and then transate to position)
			setProbeTG(transformTotal, position );
			
			//now we call export diagonal and pass in those vertices
			values = new float[resultExtents[0] * resultExtents[1]]; 
			try {
				//System.out.println("-------");
				//System.out.println(boxSliceVertices[0].X + " , " + boxSliceVertices[0].Y + " " + boxSliceVertices[0].Z);
				//System.out.println(boxSliceVertices[1].X + " , " + boxSliceVertices[1].Y + " " + boxSliceVertices[1].Z);
				//System.out.println(boxSliceVertices[2].X + " , " + boxSliceVertices[2].Y + " " + boxSliceVertices[2].Z);
				//System.out.println(boxSliceVertices[3].X + " , " + boxSliceVertices[3].Y + " " + boxSliceVertices[3].Z);

				wormImage.exportDiagonal(0, i, wormImageExtents, boxSliceVertices, values, true);

				resultImage.importData(i*values.length, values, false);
				start = start + resultSliceSize;
			}catch(IOException e) {
				e.printStackTrace();
			}
		}

		wormImage.calcMinMax();
		resultImage.calcMinMax();
		new ViewJFrameImage(resultImage);
	}



	private void testOutputZ() {
		//first create empty result image and set up voi to transfer points to result image\
		//!!!!!!!!!!!!!!!!!!NEED TO REMOVE THAT -1 AT SOME POINT
		resultExtents[0] = 400;
		resultExtents[1] = 400;
		resultExtents[2] = 400;

		//resultExtents = wormImage.getExtents();

		float[] res = {wormImage.getResolutions(0)[0], wormImage.getResolutions(0)[1],wormImage.getResolutions(0)[2]};
		resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents, "RESULT IMAGE");
		resultImage.setResolutions(res);
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
		
		float[] values;
		for(int i=0;i< resultExtents[2]-1;i++) {
			
			// Test case: should output the same image as wormImage (no rotation, translation in z)
			{
				Transform3D transformTotal = new Transform3D();
				setProbeTG(transformTotal, new Point3f( (xDim - 1)/2f, (yDim - 1)/2f, (zDim - 1) * (i/400f) )  );
			}
			//now we call export diagonal and pass in those vertices
			values = new float[resultExtents[0] * resultExtents[1]]; 
			try {
				//System.out.println("-------");
				//System.out.println(boxSliceVertices[0].X + " , " + boxSliceVertices[0].Y + " " + boxSliceVertices[0].Z);
				//System.out.println(boxSliceVertices[1].X + " , " + boxSliceVertices[1].Y + " " + boxSliceVertices[1].Z);
				//System.out.println(boxSliceVertices[2].X + " , " + boxSliceVertices[2].Y + " " + boxSliceVertices[2].Z);
				//System.out.println(boxSliceVertices[3].X + " , " + boxSliceVertices[3].Y + " " + boxSliceVertices[3].Z);

				wormImage.exportDiagonal(0, i, wormImageExtents, boxSliceVertices, values, true);

				resultImage.importData(i*values.length, values, false);
			}catch(IOException e) {
				e.printStackTrace();
			}
		}

		wormImage.calcMinMax();
		resultImage.calcMinMax();
		new ViewJFrameImage(resultImage);
	}

	private void testOutputRotateY() {
		//first create empty result image and set up voi to transfer points to result image\
		//!!!!!!!!!!!!!!!!!!NEED TO REMOVE THAT -1 AT SOME POINT
		resultExtents[0] = 400;
		resultExtents[1] = 400;
		resultExtents[2] = 400;

		//resultExtents = wormImage.getExtents();

		float[] res = {wormImage.getResolutions(0)[0], wormImage.getResolutions(0)[1],wormImage.getResolutions(0)[2]};
		resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents, "RESULT IMAGE");
		resultImage.setResolutions(res);
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
		
		float[] values;
		for(int i=0;i< resultExtents[2]-1;i++) {
			// Test case: should output image rotated about 'y' -90 degrees (compare with utilities->rotate y -90)

			Transform3D transformTotal = new Transform3D();
			transformTotal.setRotation( new AxisAngle4f( new javax.vecmath.Vector3f(0, 1, 0), (float)(Math.PI / 2f) ) );
			setProbeTG(transformTotal, new Point3f( (xDim - 1) * (i/400f), (yDim - 1)/2f, (zDim - 1)/2f )  );

			//now we call export diagonal and pass in those vertices
			values = new float[resultExtents[0] * resultExtents[1]]; 
			try {
				//System.out.println("-------");
				//System.out.println(boxSliceVertices[0].X + " , " + boxSliceVertices[0].Y + " " + boxSliceVertices[0].Z);
				//System.out.println(boxSliceVertices[1].X + " , " + boxSliceVertices[1].Y + " " + boxSliceVertices[1].Z);
				//System.out.println(boxSliceVertices[2].X + " , " + boxSliceVertices[2].Y + " " + boxSliceVertices[2].Z);
				//System.out.println(boxSliceVertices[3].X + " , " + boxSliceVertices[3].Y + " " + boxSliceVertices[3].Z);

				wormImage.exportDiagonal(0, i, wormImageExtents, boxSliceVertices, values, true);

				resultImage.importData(i*values.length, values, false);
			}catch(IOException e) {
				e.printStackTrace();
			}
		}

		wormImage.calcMinMax();
		resultImage.calcMinMax();
		new ViewJFrameImage(resultImage);
	}


	Point3f centerModel = new Point3f(0,0,0);
	Point3f centerScreen = new Point3f(0,0,0);
	Point3f centerShift = new Point3f(1,1,1);
	final Point3f[] verts = new Point3f[]
			{
			new Point3f(-1,-1,0),
			new Point3f( 1,-1,0),
			new Point3f( 1, 1,0),
			new Point3f(-1, 1,0)
			};


	public void setProbeTG(final Transform3D kTransform, Point3f kPosition) {

		/* Get the translation and inverse translation transforms: */
		final javax.vecmath.Vector3f kTranslateVector = new javax.vecmath.Vector3f();
		kTransform.get(kTranslateVector);

		final Transform3D kTranslate = new Transform3D();
		kTranslate.setTranslation(kTranslateVector);

		final Transform3D kTranslateInv = new Transform3D();
		kTranslateInv.setTranslation(new javax.vecmath.Vector3f( -kTranslateVector.x, -kTranslateVector.y, -kTranslateVector.z));

		/* Get the rotation transform: */
		final Matrix3f kRotationMatrix = new Matrix3f();
		kTransform.getRotationScale(kRotationMatrix);

		final Transform3D kRotate = new Transform3D();
		kRotate.setRotationScale(kRotationMatrix);

		/*
		 * Concatenate the three transformations to rotate about the probe position:
		 */
		final Transform3D kTransformTotal = new Transform3D();
		kTransformTotal.mul(kTranslate);
		kTransformTotal.mul(kRotate);
		kTransformTotal.mul(kTranslateInv);

		transformBoxSlices(kTransformTotal, kPosition);
	}

	/**
	 * transform the points used to render the triSliceImages textures.
	 * 
	 * @param kTransform the Transform3D used to rotate the boxes.
	 */
	private void transformBoxSlices(Transform3D kTransform, Point3f kPosition) {
		if (kTransform == null) {
			kTransform = new Transform3D();
			kTransform.setIdentity();
		}
		Point3f[] outVertices = new Point3f[4];

		if (boxSliceVertices == null) {
			boxSliceVertices = new Vector3f[4];
		}

		for (int i = 0; i < 4; i++) {
			outVertices[i] = new Point3f();
			if (boxSliceVertices[i] == null) {
				boxSliceVertices[i] = new Vector3f();
			}

			// Rotate the points in the bounding box:
				// verts is a 'unit' cube centered at (0,0,0) with
			// corners from (-1,-1,-1) -> (1,1,1)
			kTransform.transform(verts[i], outVertices[i]);
			//System.err.println( verts[i] + "                  " + outVertices[i] );

			// Shift so the position is back to the cube with corners from (0,0,0) -> (2,2,2)
			outVertices[i].x += centerShift.x;
			outVertices[i].y += centerShift.y;
			outVertices[i].z += centerShift.z;
			//System.err.println( outVertices[i] );

			// Scale back to the image dimensions:
			boxSliceVertices[i].X = (xDim - 1) * outVertices[i].x/2f;
			boxSliceVertices[i].Y = (yDim - 1) * outVertices[i].y/2f;
			boxSliceVertices[i].Z = (zDim - 1) * outVertices[i].z/2f;

			// move from the center of the volume to the position:
			// (centerModel and kPosition are in image-index coordinates)
			boxSliceVertices[i].X += (kPosition.x - centerModel.x);
			boxSliceVertices[i].Y += (kPosition.y - centerModel.y);
			boxSliceVertices[i].Z += (kPosition.z - centerModel.z);
		}
	}



	/**
	 * Translate from normalized plane coordinates to Model coordinates:
	 * 
	 * @param screen the input point to be transformed from normalized plane coordinates
	 * @param model the output point in Model coordinates
	 */
	/* private void ScreenToModel(javax.vecmath.Vector3f screen, Vector3f model) {
        model.X = Math.round( ( (screen.x + xBox) * ((float) xDim - 1)) / (2.0f * xBox));
        model.Y = Math.round( ( (screen.y - yBox) * ((float) yDim - 1)) / ( -2.0f * yBox));
        model.Z = Math.round( ( (screen.z - zBox) * ((float) zDim - 1)) / ( -2.0f * zBox));
    }*/























}
