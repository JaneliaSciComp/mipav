import java.awt.Dimension;
import java.util.Vector;

import javax.vecmath.Vector3d;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBSpline;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
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


public class PlugInAlgorithmWormStraightening extends AlgorithmBase {
	
	private ModelImage wormImage, resultImage, matchImage;
	
	private int interpolationPts;
	
	private AlgorithmBSmooth smoothAlgo;
	
	private ViewJFrameImage vjf;
	

	
	public PlugInAlgorithmWormStraightening() {}

	
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
		//first create empty result image and set up voi to transfer points to result image\
		//!!!!!!!!!!!!!!!!!!NEED TO REMOVE THAT -1 AT SOME POINT
		int[] extents = {50, 400, interpolationPts-1};
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
        float[] newPtx = new float[1];
        float[] newPty = new float[1];
        float[] newPtz = new float[1];
        
        
        //alg to get tangent vector
        AlgorithmBSpline bSplineAlgo = smoothAlgo.getbSplineAlgo();
        
        //when wanting to visualize the planes on the original image points...first blank out the image
        //!!!!!!make sure you comment this out when wanting to populate result image
        //wormImage.setAll(0);
        
        javax.vecmath.Vector3f compareAngle = new javax.vecmath.Vector3f(1,0,0);
        javax.vecmath.Vector3f v ;
        float angle;
		double halfPi = Math.PI/2.0;
		double quarterPi = Math.PI/4.0;
		double startRange = halfPi - quarterPi;
		double endRange = halfPi + quarterPi;
        
        //*****this new way of basing it on tolerance rahter than going from positive to negative or vice versa
        //results in an arrayoutof bouds error when on the last point.
        //.....so....need to get last one to work everntually!!!
        String[] planeOrientations = new String[interpolationPts-1];
        boolean[] doFlip = new boolean[interpolationPts-1];
        //Vector3f[] tanVectors = new Vector3f[interpolationPts-1];
        boolean isCloseToHorizontal;
        
        
        for(int i=0;i<interpolationPts-1;i++) {

        		System.out.println("^^ " + i);
        		
        		//determining tangent vector at point
            	int index = i;
            	float floatIndex = (float)index + 2;
            	Vector3f tanVector = bSplineAlgo.bSplineJetXYZ(1, floatIndex, xPoints, yPoints, zPoints);
            	//normalizing tangent
                tanVector.Normalize();
                
                //tanVectors[i] = tanVector;
                
                
                v = new javax.vecmath.Vector3f(tanVector.X,tanVector.Y,tanVector.Z);
                angle = v.angle(compareAngle);
                if(angle <= halfPi) {
            		doFlip[i] = false;
            	}else {
            		doFlip[i] = true;
            	}
                
                //if angle is within PI/2 +- Pi/4, then this means it is more of a horizontal line
                if(angle >= startRange && angle <= endRange) {
                	isCloseToHorizontal = true;
                }else {
                	isCloseToHorizontal = false;
                }
                
                
                
                
                //Equation of plane through P1(x1,y1,z1) with normal vector a = <a1,a2,a3) is
                //a1(x-x1) + a2(y-y1) + a3(z-z1) = 0
                double a1 = tanVector.X;
                double a2 = tanVector.Y;
                double a3 = tanVector.Z;
                point = ((VOIPoint)contours.get(index)).exportPoint();
                
                
                
                //double x1 = point.X;
                //double y1 = point.Y;
                //double z1 = point.Z;  
                
                
                int x1 = Math.round(point.X);
                int y1 = Math.round(point.Y);
                int z1 = Math.round(point.Z);

                double tolerance = 1;
                float pix;
                double oldP;
                
                
                
                
                //////////////////////////////////////////////////////////
                //BILLS STUFF
                
                /*
                System.out.println("doing new way");
                double root = Math.sqrt(a1*a1 + a2*a2 + a3*a3);
                double dx = a1/root;
                double dy = a2/root;
                double dz = a3/root;
                double p1;
                double p2;
                double x1a = x1 - dx;
                double y1a = y1 - dy;
                double z1a = z1 - dz;
                double x1b = x1 + dx;
                double y1b = y1 + dy;
                double z1b = z1 + dz;
                for (int z = 0; z < zExtent; z++) {
                  for (int y = 0; y < yExtent; y++) {
                        for (int x = 0; x < xExtent; x++) {
                              p = (a1*(x-x1)) + (a2*(y-y1)) + (a3*(z-z1));
                                    if(p < tolerance && p > (-tolerance)) {
                                          if (((Math.round(x+dx))>= 0) && ((Math.round(x+dx)) < xExtent) &&
                                          ((Math.round(y+dy))>= 0) && ((Math.round(y+dy)) < yExtent) &&
                                          ((Math.round(z+dz))>= 0) && ((Math.round(z+dz)) < zExtent)) {
                                                p1 = (a1*(x - x1a)) + (a2*(y - y1a)) + (a3*(z - z1a));
                                                if (Math.abs(p1) < Math.abs(p)) {
                                                      wormImage.set((int)Math.round(x + dx),(int)Math.round(y + dy),(int)Math.round(z + dz), 100);
                                                      continue;
                                                }
                                          }
                                          
                                          if (((Math.round(x-dx))>= 0) && ((Math.round(x-dx)) < xExtent) &&
                                                ((Math.round(y-dy))>= 0) && ((Math.round(y-dy)) < yExtent) &&
                                                ((Math.round(z-dz))>= 0) && ((Math.round(z-dz)) < zExtent)) {
                                                p2 = (a1*(x - x1b)) + (a2*(y - y1b)) + (a3*(z - z1b));
                                                if (Math.abs(p2) < Math.abs(p)) {
                                                      wormImage.set((int)Math.round(x - dx),(int)Math.round(y - dy),(int)Math.round(z - dz), 100);
                                                      continue;   
                                                }
                                          }
                                          
                                          wormImage.set(x, y, z, 100);
                                    }
                        }
                  }
                }

                
                
                
                */
                
                
                
                
               ////////////////////////////////////////////////////////////// 
                
                
                
                
                
                for(int z=0;z<zExtent;z++) {
                	newY = 0;
        	        for(int y=0;y<yExtent;y++) {
        	        	//boolean isNegFirst = true;
        	        				oldP = -500;
        			    xLoop:    	for(int x=0;x<xExtent;x++) {
        				        		p = (a1*(x-x1)) + (a2*(y-y1)) + (a3*(z-z1));
        				        		/*if(z==0 && y == 0 && x == 0) {
        				        			if(p < 0) {
        				        				System.out.println("i = " + i + " :negative");
        				        				planeOrientations[i] = "negative";
        				        			}else if( p > 0){
        				        				System.out.println("i = " + i + " :positive");
        				        				planeOrientations[i] = "positive";
        				        			}
        				        		}*/
        				        		
        				        		
        				        		
				        				if(p < tolerance && p > (-tolerance)) {
				        					if(oldP == -500) {
	        				        			pix = wormImage.getFloat(x, y, z);
	        				        			
	        				        			resultImage.set(z, newY, i, pix);
	        				        			//transfer point over to result image if this is the point coordinate
	        				        			if(x == Math.round(x1) && y == Math.round(y1) && z == Math.round(z1)) {
		    				        				newPtx[0] = z;
		    								        newPty[0] = newY;
		    								        newPtz[0] = i;
		    				        				newPtVOI.importCurve(newPtx, newPty, newPtz);
		    				        			}

					        					//wormImage.set(x, y, z, 1);
					        					oldP = p;
					        					newY++;
				        					}else {
				        						if(oldP < 0 && p > 0) {
				        							
				        						}else if(oldP > 0 && p < 0) {
				        							
				        						}else {

		        				        			pix = wormImage.getFloat(x, y, z);
		        				        			
		        				        			resultImage.set(z, newY, i, pix);
		        				        			//transfer point over to result image if this is the point coordinate
		        				        			if(x == Math.round(x1) && y == Math.round(y1) && z == Math.round(z1)) {
			    				        				newPtx[0] = z;
			    								        newPty[0] = newY;
			    								        newPtz[0] = i;
			    				        				newPtVOI.importCurve(newPtx, newPty, newPtz);
			    				        			}

						        					//wormImage.set(x, y, z, 2);
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
