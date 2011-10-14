import java.awt.Dimension;
import java.io.IOException;
import java.util.Vector;

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
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;


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
		
		resols[0] = wormImage.getResolutions(0)[0];
		resols[1] = wormImage.getResolutions(0)[1];
		resols[2] = wormImage.getResolutions(0)[2];
        
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
		resultExtents[2] = interpolationPts-1;
		
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
        
        //variables needed for loop
        int xExtent = wormImage.getExtents()[0];
        int yExtent = wormImage.getExtents()[1];
        int zExtent = wormImage.getExtents()[2];

        double p;
        int newY = 0;
        float[] newPtx = new float[1];
        float[] newPty = new float[1];
        float[] newPtz = new float[1];

        //alg to get tangent vector
        AlgorithmBSpline bSplineAlgo = smoothAlgo.getbSplineAlgo();
        
        //when wanting to visualize the planes on the original image points...first blank out the image
        //!!!!!!make sure you comment this out when wanting to populate result image
        //wormImage.setAll(0);
        
        javax.vecmath.Vector3f compareVect = new javax.vecmath.Vector3f(1,0,0);
        javax.vecmath.Vector3f imageOrientationVectX = new javax.vecmath.Vector3f(1,0,0);
        javax.vecmath.Vector3f imageOrientationVectY = new javax.vecmath.Vector3f(0,1,0);
        javax.vecmath.Vector3f imageOrientationVectZ = new javax.vecmath.Vector3f(0,0,1);
        javax.vecmath.Vector3f tangentVect ;
        float angle, rotationAngle;
		double halfPi = Math.PI/2.0;
		double quarterPi = Math.PI/4.0;
		double startRange = halfPi - quarterPi;
		double endRange = halfPi + quarterPi;
          
        String[] planeOrientations = new String[interpolationPts-1];
        boolean[] doFlip = new boolean[interpolationPts-1];
        int sliceSize = xExtent*yExtent;
        
        //Vector3f[] tanVectors = new Vector3f[interpolationPts-1];

        float[] values;
        int start = 0;
        int resultSliceSize = resultExtents[0] * resultExtents[1];
        //for(int i=0;i<interpolationPts-1;i++) {
         for(int i=17;i < 18;i++) {

        		System.out.println("^^ " + i);
        		//determining tangent vector at point
            	int index = i;
            	float floatIndex = (float)index + 2;
            	Vector3f tanVector = bSplineAlgo.bSplineJetXYZ(1, floatIndex, xPoints, yPoints, zPoints);
            	//normalizing tangent
                tanVector.Normalize();
                
                //used later for when to determine whether to flip the image or not
                tangentVect = new javax.vecmath.Vector3f(tanVector.X,tanVector.Y,tanVector.Z);
                angle = tangentVect.angle(compareVect);
                if(angle <= halfPi) {
            		doFlip[i] = false;
            	}else {
            		doFlip[i] = true;
            	}

                //get coordinate of control point of b-spline
                point = ((VOIPoint)contours.get(index)).exportPoint();
                float x1 = point.X * res[0];
                float y1 = point.Y * res[1];
                float z1 = point.Z * res[2]; 
                
                float xC = wormImage.getImageCentermm(false).X;
                float yC = wormImage.getImageCentermm(false).Y;
                float zC = wormImage.getImageCentermm(false).Z; 
 
                //I think rotation should be about the center of the image as 
                // opposed to the point of interection of the plane
                
                //need to first get the 4 corners for where plane cuts at image
                
                Transform3D translateC = new Transform3D();
                translateC.setTranslation(new javax.vecmath.Vector3f(xC,yC,zC));
                
                Transform3D translatePlane = new Transform3D();
                translatePlane.setTranslation(new javax.vecmath.Vector3f(x1,y1,z1));
                
                Transform3D translateInvC = new Transform3D();
                translateInvC.setTranslation(new javax.vecmath.Vector3f(-xC,-yC,-zC));
                
               Transform3D rotateX = new Transform3D();
               Transform3D rotateY = new Transform3D();
               Transform3D rotateZ = new Transform3D();
     
               //rotate.setRotation(new AxixAngle4f() );
               angle = tangentVect.angle(imageOrientationVectX);
               rotateX.setRotation(new AxisAngle4f(imageOrientationVectX, angle));
               System.out.println("X axis angle: " + angle);
               
               angle = tangentVect.angle(imageOrientationVectY);
               rotateY.setRotation(new AxisAngle4f(imageOrientationVectY, angle));
               System.out.println("Y axis angle: " + angle);
               
               angle = tangentVect.angle(imageOrientationVectZ);
               rotateZ.setRotation(new AxisAngle4f(imageOrientationVectZ, angle));
               System.out.println("Z axis angle: " + angle);
               
               Transform3D transformTotal = new Transform3D();
               transformTotal.mul(translatePlane);
               //transformTotal.mul(translateC);
               transformTotal.mul(rotateX);
               transformTotal.mul(rotateY);
               transformTotal.mul(rotateZ);
               transformTotal.mul(translateInvC);

               transformBoxSlices(transformTotal); 
                
                //now we call export diagonal and pass in those vertices
                values = new float[resultExtents[0] * resultExtents[1]]; 
                try {
                	System.out.println("-------");
                	System.out.println(boxSliceVertices[0].X + " , " + boxSliceVertices[0].Y + " " + boxSliceVertices[0].Z);
                	System.out.println(boxSliceVertices[1].X + " , " + boxSliceVertices[1].Y + " " + boxSliceVertices[1].Z);
                	System.out.println(boxSliceVertices[2].X + " , " + boxSliceVertices[2].Y + " " + boxSliceVertices[2].Z);
                	System.out.println(boxSliceVertices[3].X + " , " + boxSliceVertices[3].Y + " " + boxSliceVertices[3].Z);
                	
                	wormImage.exportDiagonal(0, i, wormImageExtents, boxSliceVertices, values, true);
                	
                	resultImage.importData(start, values, false);
                	start = start + resultSliceSize;
                }catch(IOException e) {
                	e.printStackTrace();
                }
        }
        
        wormImage.calcMinMax();
        resultImage.calcMinMax();
       
        new ViewJFrameImage(resultImage);
        
  /*      
        System.out.println("aaa");
        
        //now we need to translate and line up all the slices based on the 0th point coordinate
        //pad image so that we can translate slices appropriately
        float[] padValue = new float[3];
        padValue[0] = 0.0f;
        int[] marginX = new int[2];
        int[] marginY = new int[2];
        int[] marginZ = new int[2];
        marginX[0] = 50;
        marginX[1] = 50;
        marginY[0] = 50;
        marginY[1] = 50;
        AlgorithmAddMargins imageMarginsAlgo = new AlgorithmAddMargins(resultImage, marginX, marginY, marginZ );
        imageMarginsAlgo.setPadValue(padValue);
        imageMarginsAlgo.run();
        resultImage = imageMarginsAlgo.getSrcImage();
        
        
        System.out.println("bbb");
        
        //now extract each slice and translate each slice so that voi point moves to center
        ModelImage[] slices = new ModelImage[resultImage.getExtents()[2]];
        ModelImage[] extractedImages;
        int[] destExtents = new int[2];
        destExtents[0] = resultImage.getExtents()[0];
        destExtents[1] = resultImage.getExtents()[1];
        AlgorithmExtractSlicesVolumes extractSlicesAlgo;
        AlgorithmTransform algoTrans;
        int newCenterX = Math.round(resultImage.getExtents()[0]/2); 
        int newCenterY = Math.round(resultImage.getExtents()[1]/2);
        VOIs = resultImage.getVOIs();
        contours = VOIs.VOIAt(0).getCurves();
        boolean[] checkListExtract = new boolean[resultImage.getExtents()[2]];
        double tx = 0;
        double ty = 0;
        double tZ = 0;
        float diffX = 0;
        float diffY = 0;
        TransMatrix xfrm;
        float oXres = resultImage.getResolutions(0)[0];
        float oYres = resultImage.getResolutions(0)[1];
        int oXdim = resultImage.getExtents()[0];
        int oYdim = resultImage.getExtents()[1];
        int[] units = new int[2];
        units[0] = resultImage.getUnitsOfMeasure(0);
        units[1] = resultImage.getUnitsOfMeasure(1);
        
        System.out.println("ccc");
        int[] destExtents2 = new int[3];
        destExtents2[0] = resultImage.getExtents()[0];
        destExtents2[1] = resultImage.getExtents()[1];
        destExtents2[2] = resultImage.getExtents()[2];

		
        ModelImage finalImage = new ModelImage(resultImage.getType(), destExtents2, "final image");
        finalImage.setResolutions(resultImage.getResolutions(0));
		fileInfoBases = finalImage.getFileInfo();
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i].setEndianess(resultImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(resultImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(resultImage.getResolutions(0));
        } 
        finalImage.setFileInfo(fileInfoBases);
        

        
        System.out.println(fileInfoBases[0].getUnitsOfMeasure().length);
        AlgorithmFlip flipAlgo;
 
        for(int i=0;i<resultImage.getExtents()[2];i++) {
        	System.out.println("i is " + i);
        	for(int k=0;k<resultImage.getExtents()[2];k++) {
        		if(k==i) {
        			checkListExtract[k] = true;
        		}else {
        			checkListExtract[k] = false;
        		}
        	}
        	extractSlicesAlgo = new AlgorithmExtractSlicesVolumes(resultImage, checkListExtract);
        	extractSlicesAlgo.run();
        	extractedImages = extractSlicesAlgo.getExtractedImages();
        	
        	newPtVOI = new VOI((short) 0, "point2D.voi", VOI.POINT, -1.0f);
            newPtVOI.setUID(newPtVOI.hashCode());
            extractedImages[0].registerVOI(newPtVOI);
            
            newPtx[0] = ((VOIPoint)contours.get(i)).exportPoint().X;
	        newPty[0] = ((VOIPoint)contours.get(i)).exportPoint().Y;
	        newPtz[0] = 0;
	        newPtVOI.importCurve(newPtx, newPty, newPtz);
        	
        	
        	//we might have to flip the image...so do it here
	        if(doFlip[i]) {
	        	flipAlgo = new AlgorithmFlip(extractedImages[0], AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE_AND_VOI, true);
	        	
	        	flipAlgo.run();
	        }

        	//now get the point coordinate
	        VOIVector VOIs2 = extractedImages[0].getVOIs();
	        Vector<VOIBase> contours2 = VOIs2.VOIAt(0).getCurves();
        	point = ((VOIPoint)contours2.get(0)).exportPoint();
        	diffX = point.X - newCenterX;
        	diffX = -(diffX);
        	diffY = point.Y - newCenterY;
        	diffY = -(diffY);
        	
        	tx = diffX * resultImage.getResolutions(0)[0];
        	ty = diffY * resultImage.getResolutions(0)[1];
        	
        	xfrm = new TransMatrix(3);
        	
        	xfrm.setTranslate(tx, ty);
            xfrm.setRotate(0);
            xfrm.setSkew(0, 0);
            xfrm.setZoom(1, 1);
  
            algoTrans = new AlgorithmTransform(extractedImages[0], xfrm, 1, oXres, oYres, oXdim, oYdim, units, false, false, false, false, null);
    
            algoTrans.setUpdateOriginFlag(true);
            
            algoTrans.run();
            
            slices[i] = algoTrans.getTransformedImage();
  
        }

        AlgorithmConcatMult2Dto3D alg = new AlgorithmConcatMult2Dto3D(slices, finalImage);
        
        alg.run();
 
        System.out.println(finalImage.getFileInfo()[0].getUnitsOfMeasure().length);
        finalImage.calcMinMax();
        System.out.println(finalImage.toString());
        System.out.println("ccc");
        vjf = new ViewJFrameImage(finalImage);
    
       */
	
	}
	

	
	/**
     * transform the points used to render the triSliceImages textures.
     * 
     * @param kTransform the Transform3D used to rotate the boxes.
     */
    private void transformBoxSlices(Transform3D kTransform) {
        if (kTransform == null) {
            kTransform = new Transform3D();
            kTransform.setIdentity();
        }
        Point3f[] inVertices = new Point3f[4];
        for(int i=0;i<inVertices.length;i++) {
        	inVertices[i] = new Point3f();
        }

        
        // 
        //
        //		a-----b
        //		|	  |
        //		|	  |
        //		|     |
        //		c-----d
        //
        //
       
        //let slice = 0
        //a = (0, 0, slice)
        //b = (xDim - 1, 0, slice)
        //c = (0, yDim - 1, slice)
        //d = (xDim - 1, yDim - 1, slice)
        
        javax.vecmath.Vector3f a = new javax.vecmath.Vector3f(0,0,0);
        javax.vecmath.Vector3f b = new javax.vecmath.Vector3f(xDim-1,0,0);
        javax.vecmath.Vector3f c = new javax.vecmath.Vector3f(0,yDim-1,0);
        javax.vecmath.Vector3f d = new javax.vecmath.Vector3f(xDim-1,yDim-1,0);

        float x = 0;
        float y = 0;
        float z = 0;

        x = a.x * resols[0];
        y = a.y * resols[1];
        z = a.z * resols[2];
        inVertices[0] = new Point3f(x,y,z);
        
        x = b.x * resols[0];
        y = b.y * resols[1];
        z = b.z * resols[2];
        inVertices[1] = new Point3f(x,y,z);
        
        x = c.x * resols[0];
        y = c.y * resols[1];
        z = c.z * resols[2];
        inVertices[2] = new Point3f(x,y,z);
        
        x = d.x * resols[0];
        y = d.y * resols[1];
        z = d.z * resols[2];
        inVertices[3] = new Point3f(x,y,z);
        
        /*x = xBox * (a.x / (xDim -1));
        y = yBox * (a.y / (yDim -1));
        z = zBox * (a.z / (zDim -1));
        inVertices[0] = new Point3f(x,y,z);
        
        x = xBox * (b.x / (xDim -1));
        y = yBox * (b.y / (yDim -1));
        z = zBox * (b.z / (zDim -1));
        inVertices[1] = new Point3f(x,y,z);
        
        x = xBox * (c.x / (xDim -1));
        y = yBox * (c.y / (yDim -1));
        z = zBox * (c.z / (zDim -1));
        inVertices[2] = new Point3f(x,y,z);
        
        x = xBox * (d.x / (xDim -1));
        y = yBox * (d.y / (yDim -1));
        z = zBox * (d.z / (zDim -1));
        inVertices[3] = new Point3f(x,y,z);
*/
        /*x = (2*xBox) * (a.x / (xDim -1)) - xBox;
        y = (2*yBox) * (a.y / (yDim -1)) - yBox;
        z = (2*zBox) * (a.z / (zDim -1)) - zBox;
        inVertices[0] = new Point3f(x,y,z);
        
        x = (2*xBox) * (b.x / (xDim -1)) - xBox;
        y = (2*yBox) * (b.y / (yDim -1)) - yBox;
        z = (2*zBox) * (b.z / (zDim -1)) - zBox;
        inVertices[1] = new Point3f(x,y,z);
        
        x = (2*xBox) * (c.x / (xDim -1)) - xBox;
        y = (2*yBox) * (c.y / (yDim -1)) - yBox;
        z = (2*zBox) * (c.z / (zDim -1)) - zBox;
        inVertices[2] = new Point3f(x,y,z);
        
        x = (2*xBox) * (d.x / (xDim -1)) - xBox;
        y = (2*yBox) * (d.y / (yDim -1)) - yBox;
        z = (2*zBox) * (d.z / (zDim -1)) - zBox;
        inVertices[3] = new Point3f(x,y,z);*/
        

        Point3f[] outVertices = new Point3f[4];

        boxSliceVertices = new Vector3f[4];
            
        for (int i = 0; i < 4; i++) {
            outVertices[i] = new Point3f();
            
            boxSliceVertices[i] = new Vector3f();
            
            /* Rotate the points in the bounding box: */
            kTransform.transform(inVertices[i], outVertices[i]);
            /* Convert the points to ModelImage space: */
            //ScreenToModel(new javax.vecmath.Vector3f(outVertices[i].x, outVertices[i].y, outVertices[i].z),
            //        boxSliceVertices[i]);
            boxSliceVertices[i].X = outVertices[i].x;
            boxSliceVertices[i].Y = outVertices[i].y;
            boxSliceVertices[i].Z = outVertices[i].z;
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
