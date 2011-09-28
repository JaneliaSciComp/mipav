import java.awt.Dimension;
import java.util.Vector;

import javax.vecmath.Vector3d;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBSpline;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;


public class PlugInAlgorithmWormStraightening extends AlgorithmBase {
	
	private ModelImage wormImage, resultImage, matchImage;
	
	private int interpolationPts;
	
	private AlgorithmBSmooth smoothAlgo;
	
	private ViewJFrameImage vjf;
	
	

	
	public PlugInAlgorithmWormStraightening(ModelImage wormImage, int interpolationPts) {
		this.wormImage = wormImage;
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
		//first create empty result image and set up voi to transfer points to result image
		int[] extents = {50, 400, interpolationPts};
		float[] res = {wormImage.getResolutions(0)[2], wormImage.getResolutions(0)[1],1.0f};
		resultImage = new ModelImage(ModelStorageBase.FLOAT, extents, "RESULT IMAGE");
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
		
	    
	    
	    
		//In oreder to determine the perpendicalur plane along each point, we also need the tangent vector at that
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
        final float[] newPtx = new float[1];
        final float[] newPty = new float[1];
        final float[] newPtz = new float[1];
        
        
        //alg to get tangent vector
        AlgorithmBSpline bSplineAlgo = smoothAlgo.getbSplineAlgo();
        
        //when wanting to visualize the planes on the original image points...first blank out the image
        //!!!!!!make sure you comment this out when wanting to populate result image
        wormImage.setAll(0);
        

        
        //*****this new way of basing it on tolerance rahter than going from positive to negative or vice versa
        //results in an arrayoutof bouds error when on the last point.
        //.....so....need to get last one to work everntually!!!
        for(int i=0;i<interpolationPts-1;i++) {

        		System.out.println("^^ " + i);
        		
        		//determining tangent vector at point
            	int index = i;
            	float floatIndex = (float)index + 2;
            	Vector3f tanVector = bSplineAlgo.bSplineJetXYZ(1, floatIndex, xPoints, yPoints, zPoints);
            	//normalizing tangent
                tanVector.Normalize();
                
                //Equation of plane through P1(x1,y1,z1) with normal vector a = <a1,a2,a3) is
                //a1(x-x1) + a2(y-y1) + a3(z-z1) = 0
                double a1 = tanVector.X;
                double a2 = tanVector.Y;
                double a3 = tanVector.Z;
                point = ((VOIPoint)contours.get(index)).exportPoint();
                double x1 = point.X;
                double y1 = point.Y;
                double z1 = point.Z;  

                double tolerance = 1;
                float pix;
                double oldP;
                
                
                
                for(int z=0;z<zExtent;z++) {
                	newY = 0;
        	        for(int y=0;y<yExtent;y++) {
        	        	//boolean isNegFirst = true;
        	        				oldP = -500;
        			    xLoop:    	for(int x=0;x<xExtent;x++) {
        				        		p = (a1*(x-x1)) + (a2*(y-y1)) + (a3*(z-z1));
				        				if(p < tolerance && p > (-tolerance)) {
				        					if(oldP == -500) {
	        				        			/*pix = wormImage.getFloat(x, y, z);
	        				        			
	        				        			resultImage.set(z, newY, i, pix);
	        				        			//transfer point over to result image if this is the point coordinate
	        				        			if(x == Math.round(x1) && y == Math.round(y1) && z == Math.round(z1)) {
		    				        				newPtx[0] = z;
		    								        newPty[0] = newY;
		    								        newPtz[0] = i;
		    				        				newPtVOI.importCurve(newPtx, newPty, newPtz);
		    				        			}*/

					        					wormImage.set(x, y, z, 100);
					        					oldP = p;
					        					newY++;
				        					}else {
				        						if(oldP < 0 && p > 0) {
				        							
				        						}else if(oldP > 0 && p < 0) {
				        							
				        						}else {

		        				        			/*pix = wormImage.getFloat(x, y, z);
		        				        			
		        				        			resultImage.set(z, newY, i, pix);
		        				        			//transfer point over to result image if this is the point coordinate
		        				        			if(x == Math.round(x1) && y == Math.round(y1) && z == Math.round(z1)) {
			    				        				newPtx[0] = z;
			    								        newPty[0] = newY;
			    								        newPtz[0] = i;
			    				        				newPtVOI.importCurve(newPtx, newPty, newPtz);
			    				        			}
*/
						        					wormImage.set(x, y, z, 200);
						        					oldP = p;
						        					newY++;
				        						}
				        					}

    				        			}	
            				        	
        				        		
        				        			
        				        			
        				        		
        				        		//this is the old way of doing it...going from neg to pos or vice versa
        				        		/*if(p<0) {
        				        			if(x==0) {
        				        				isNegFirst = true;
        				        			}else {
        				        				if(!isNegFirst) {
        				        					//means we have crossed over to positive...so get
        	    				        			//the pixel value of the one before it
        	    				        			pix = wormImage.getFloat(x-1, y, z);
        	    				        			
        	    				        			resultImage.set(z, newY, i, pix);
        	    				        			if(x-1 == Math.round(x1) && y == Math.round(y1) && z == Math.round(z1)) {
        	    				        				System.out.println("found");
        	    				        				newPtx[0] = z;
        	    								        newPty[0] = newY;
        	    								        newPtz[0] = i;
        	    								        System.out.println("*** i is " + i);
        	    				        				newPtVOI.importCurve(newPtx, newPty, newPtz);
        	    				        			}
        	    	 			        			
        	    				        			//if(i==19) {
        	    				        				//wormImage.set(x-1, y, z, 29000);
        	    				        			//}
        	    				        			//resultImage.set(z, newY, pix);
        	    				        			
        	    				        			newY++;

        	    				        			break xLoop;
        				        				}
        				        			}
        				        			
        				        		}else if(p>0) {
        				        			
        				        			if(x==0) {
        				        				isNegFirst = false;
        				        			}else {
        				        				if(isNegFirst) {
        				        					//means we have crossed over to positive...so get
        	    				        			//the pixel value of the one before it
        	    				        			pix = wormImage.getFloat(x-1, y, z);
        	    				        			
        	    				        			resultImage.set(z, newY, i, pix);
        	    				        			
        	    				        			if(x-1 == Math.round(x1) && y == Math.round(y1) && z == Math.round(z1)) {
        	    				        				System.out.println("found");
        	    				        				newPtx[0] = z;
        	    								        newPty[0] = newY;
        	    								        newPtz[0] = i;
        	    								        System.out.println("### i is " + i);
        	    				        				newPtVOI.importCurve(newPtx, newPty, newPtz);
        	    				        			}
        	    				        			//if(i==19) {
        	    				        				//wormImage.set(x-1, y, z, 29000);
        	    				        			//}
        	    				        			//resultImage.set(z, newY, pix);
        	    				        			
        	    				        			newY++;

        	    				        			break xLoop;
        				        				}
        				        			}
        				        		}else {
        				        			if(x == 0) {
        				        				//System.out.println("ZERO " + i);
        				        			}
        				        		}*/
        			        		
        			        		}
        	        }
                }

            
            
        }
        
        
        
        
        wormImage.calcMinMax();
        resultImage.calcMinMax();
        
        
        
        //pad image so that we can translate slices appropriately
        /*float[] padValue = new float[3];
        padValue[0] = 0.0f;
        AlgorithmAddMargins imageMarginsAlgo = new AlgorithmAddMargins(resultImage, marginX, marginY, marginZ );
        imageMarginsAlgo.setPadValue(padValue);*/
        
        
        
        //vjf = new ViewJFrameImage(resultImage);
        
       
        
        
        
		
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	/**
	 * not being used anymore
	 * 
	 */
	private void createMatchImage() {
		//first get length of b-spline
		VOIVector VOIs = wormImage.getVOIs();
		Vector<VOIBase> contours = VOIs.VOIAt(0).getCurves();
        int nPoints = contours.size();
        System.out.println("------ " + nPoints);
        float[] xPoints = new float[nPoints];
        float[] yPoints = new float[nPoints];
        float[] zPoints = new float[nPoints];
        for (int i = 0; i < nPoints; i++) {
     	   Vector3f point = ((VOIPoint)contours.get(i)).exportPoint();
            xPoints[i] = point.X;
            yPoints[i] = point.Y;
            zPoints[i] = point.Z;
        }
        
        AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints, zPoints);
        float length = arcLength.getTotalArcLength();
        System.out.println(length);
		int[] extents = {700, 700, 40};
        matchImage = new ModelImage(ModelStorageBase.UBYTE, extents, "matchTESTImage");
        for (int i = 0; i < matchImage.getExtents()[2]; i++) {
        	matchImage.setResolutions(i, wormImage.getResolutions(0));
        }
        FileInfoBase[] fileInfoBases = matchImage.getFileInfo();
        for (int i = 0; i < fileInfoBases.length; i++) {
            //fileInfoBases[i] = new FileInfoImageXML(matchImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(wormImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(wormImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(wormImage.getFileInfo()[0].getResolutions());
            //fileInfoBases[i].setExtents(extents);
            fileInfoBases[i].setOrigin(wormImage.getFileInfo()[0].getOrigin());
            //fileInfoBases[i].setDataType(ModelStorageBase.UBYTE);

        }
        matchImage.setFileInfo(fileInfoBases);

        VOI newPtVOI = null;

        newPtVOI = new VOI((short) 0, "point3D.voi", VOI.POINT, -1.0f);
        newPtVOI.setUID(newPtVOI.hashCode());
        matchImage.registerVOI(newPtVOI);
        
        float[] x = new float[1];
        float[] y = new float[1];
        float[] z = new float[1];
        float totalLength = 0;
        
        for(int k=1;k<nPoints;k++) {
        	Vector3f point1 = ((VOIPoint)contours.get(k-1)).exportPoint();
        	Vector3f point2 = ((VOIPoint)contours.get(k)).exportPoint();
        	float distance = point1.Distance(point2);
        	totalLength = totalLength + distance;
        }
        
        System.out.println(totalLength);

        x[0] = 449;
        z[0] = 24;
        
        int yStart = Math.round((700 - totalLength)/2);

        y[0] = yStart;
        newPtVOI.importCurve(x, y, z);
        
        totalLength = 0;
        for(int k=1;k<nPoints;k++) {
        	Vector3f point1 = ((VOIPoint)contours.get(k-1)).exportPoint();
        	Vector3f point2 = ((VOIPoint)contours.get(k)).exportPoint();
        	
        	float distance = point1.Distance(point2);
        	totalLength = totalLength + distance;
        	
        	int yCoord = Math.round(yStart + totalLength);
        	
        	y[0] = yCoord;
            newPtVOI.importCurve(x, y, z);
        }

        matchImage.notifyImageDisplayListeners();
        
        vjf = new ViewJFrameImage(matchImage);
        
        
	}
	
	
	

}
