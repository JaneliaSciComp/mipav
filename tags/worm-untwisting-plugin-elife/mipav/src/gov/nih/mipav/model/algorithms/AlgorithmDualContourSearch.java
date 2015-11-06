package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;

public class AlgorithmDualContourSearch extends AlgorithmBase {
	
	/*
	 * References: 1.) Global and Local Active Contours for Head Boundary Extraction by Steve R. Gunn
	 *                 and Mark S. Nixon, International Journal of Computer Vision, 30(1), pp. 43-54, 1998.
	 *             2.) Active Contours for Head Boundary Extraction by Global and Local Energy Minimisation
	 *                 by Steve R. Gunn and Mark S. Nixon, November, 1997.
	 *             3.) Unsupervised cell nucleus segmentation with active contours by Pascal Bamford and
	 *                 Brian Lovell, Signal Processing, 71, 1998, pp. 203-213.
	 *             4.) Principles of Digital and Analog Communications Second Edition by Jerry D. Gibson,
	 *                 Macmillan Publishing Company, 1993, pp. 363 -366 for Viterbi algorithm.
	 */
	
	private int innerIndex;
	private int outerIndex;
	private int contourPoints;
	private int linePoints;
	private double regularization;
	private VOI resultVOI = null;
	/** Storage location of the first derivative of the Gaussian in the X direction. */
    private double[] GxData;

    /** Storage location of the first derivative of the Gaussian in the Y direction. */
    private double[] GyData;
    
    /** Dimensionality of the kernel. */
    private int[] kExtents;
    
    /** Standard deviations of the gaussian used to calculate the kernels. */
    private double[] sigmas;
    
    // Contract from solution in going to next slice
    private int pixelsContract;
    
    // Expand from solution in going to next slice
    private int pixelsExpand;
	
	
	public  AlgorithmDualContourSearch(ModelImage srcImg, int innerIndex, int outerIndex, int contourPoints, int linePoints,
			                           double regularization, double[] sigmas, int pixelsContract, int pixelsExpand) {
		
		super(null, srcImg);
		this.innerIndex = innerIndex;
		this.outerIndex = outerIndex;
		this.contourPoints = contourPoints;
		this.linePoints = linePoints;
		this.regularization = regularization;
		this.sigmas = sigmas;
		this.pixelsContract = pixelsContract;
		this.pixelsExpand = pixelsExpand;
	}
	
	public void runAlgorithm() {
	    if (srcImage.getNDims() == 2) {
	    	run2D();
	    }
	    else {
	    	run3D();
	    }
	}
	
	/**
     * Accessor that returns the resultant VOI.
     *
     * @return  resultant VOI that has localized to the boundaries of the object
     */
    public VOI getResultVOI() {
        return resultVOI;
    }
	
	public void run2D() {
		 VOIVector VOIs;
		 VOI innerVOI;
		 VOI outerVOI;
		 VOIContour innerContour;
		 VOIContour outerContour;
		 Vector3f innerCenter;
		 double xCenter;
		 double yCenter;
		 int i;
		 int xDim;
		 int yDim;
		 double largestDistance;
		 double distance;
		 double theta;
		 double costheta;
		 double sintheta;
		 double minDistance;
		 double maxDistance;
		 boolean snear[] = new boolean[1];
		 int i1[] = new int[1];
		 int i2[] = new int[1];
		 double delX;
		 double delY;
		 double innerX = -1.0;
		 double innerY = -1.0;
		 double outerX = -1.0;
		 double outerY = -1.0;
		 double boundaryDistance;
		 double xArray[][] = new double[contourPoints][linePoints];
		 double yArray[][] = new double[contourPoints][linePoints];
		 double grad[][] = new double[contourPoints][linePoints];
		 double Eimage[][] = new double[contourPoints][linePoints];
		 double maxGrad;
		 double minGrad;
		 int j;
		 int length;
		 double[] imgBuffer;
		 double gx;
		 double gy;
		 Vector2d interpPt;
		 int bestLinePoint[] = new int[contourPoints];
		 double stateEnergy[][] = new double[contourPoints][linePoints];
		 int statePreceding[][] = new int[contourPoints][linePoints];
		 int presentLinePoint;
		 int nextLinePoint;
		 double Eint;
		 double num;
		 double numX;
		 double numY;
		 double denom;
		 double denomX;
		 double denomY;
		 double minState;
		 int ip1;
		 int im1;
		 Vector3f resultPt[] = new Vector3f[contourPoints];
		 double stateEnergyTemp;
		 int startingLinePoint;
		 int terminatingLinePoint;
		 int xBounds[] = new int[2];
		 int yBounds[] = new int[2];
		 int zBounds[] = new int[2];
		 double largestBoundaryDistance;
		 int x;
		 int y;
		 boolean halvingMethod;
		 int innerPolarity = 1;
		 int outerPolarity = 1;
		 boolean selfTest = false;
		 if (selfTest) {
			 generateTestImage();
			 setCompleted(false);
			 return;
		 }
		 
		 xDim = srcImage.getExtents()[0];
		 yDim = srcImage.getExtents()[1];
		 
		 try {
	            length = xDim * yDim;
	            imgBuffer = new double[length];
	            srcImage.exportData(0, length, imgBuffer); // locks and releases lock

	            fireProgressStateChanged(srcImage.getImageName(), "Searching for boundary ...");

	        } catch (IOException error) {
	            displayError("Algorithm DualContourSearch: Image(s) locked");
	            setCompleted(false);

	            return;
	        } catch (OutOfMemoryError e) {
	            displayError("Algorithm DualContourSearch:  Out of Memory");
	            setCompleted(false);

	            return;
	        }
		 
		 VOIs = srcImage.getVOIs();
		 makeKernels2D();
         resultVOI = new VOI((short) VOIs.size(), "resultVOI", VOI.CONTOUR, 1.0f/3.0f);
         
		 innerVOI = VOIs.VOIAt(innerIndex);
		 outerVOI = VOIs.VOIAt(outerIndex);
		 innerContour = (VOIContour)innerVOI.getCurves().elementAt(0);
		 // Lines protruding out from contours can flip the polarity of pinpol function
		 boundaryDistance = innerContour.pinpol(2*xDim, 2*yDim, snear, i1, i2);
		 if (boundaryDistance > 0.0) {
			 innerPolarity = -1;
		 }
		 outerContour = (VOIContour)outerVOI.getCurves().elementAt(0);
		 boundaryDistance = outerContour.pinpol(2*xDim, 2*yDim, snear, i1, i2);
		 if (boundaryDistance > 0.0) {
			 outerPolarity = -1;
		 }
		 innerCenter = innerContour.getGeometricCenter();
		 xCenter = innerCenter.X;
		 yCenter = innerCenter.Y;
		 boundaryDistance = innerContour.pinpol(xCenter, yCenter, snear, i1, i2) * innerPolarity;
         if (boundaryDistance < 0.0) {
        	 // point outside polygon
        	 Preferences.debug("Inner contour geometric center is outside innter contour\n", Preferences.DEBUG_ALGORITHM);
        	 innerContour.getBounds(xBounds, yBounds, zBounds);
        	 largestBoundaryDistance = 0.0;
        	 for (y = yBounds[0]; y <= yBounds[1]; y++) {
        	     for (x = xBounds[0]; x <= xBounds[1]; x++) {
        	    	 boundaryDistance = innerContour.pinpol(x, y, snear, i1, i2) * innerPolarity;
        	    	 if (boundaryDistance > largestBoundaryDistance) {
        	    		 largestBoundaryDistance = boundaryDistance;
        	    		 xCenter = x;
        	    		 yCenter = y;
        	    	 }
        	     }
        	 }
        	 Preferences.debug("largestBoundaryDistance = " + largestBoundaryDistance + "\n", Preferences.DEBUG_ALGORITHM);
         } // if (boundaryDistance < 0.0)
		 
		 largestDistance = Math.sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1));
		 
		 for (i = 0; i < contourPoints; i++) {
		     theta = 2.0 * i * Math.PI/contourPoints;
		     costheta = Math.cos(theta);
		     sintheta = Math.sin(theta);
		     minDistance = 0.0;
		     maxDistance = largestDistance;
		     distance = largestDistance/2.0;
		     boundaryDistance = Double.MAX_VALUE;
		     halvingMethod = true;
		     while (Math.abs(boundaryDistance) > 1.0E-1) {
		    	 if (halvingMethod) {
		             distance = (minDistance + maxDistance)/2.0;
		    	 }
		    	 else {
		    		 distance = distance - 0.1;
		    		 if (distance <= 0.0) {
		    			 setCompleted(false);
		    			 return;
		    		 }
		    	 }
		         if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) {
		             halvingMethod = false;
		             distance = largestDistance;
		             if (costheta > 0) {
		                 distance = Math.min(distance, (xDim - 1 - xCenter)/costheta);	 
		             }
		             if (costheta < 0) {
		            	 distance = Math.min(distance, -xCenter/costheta);
		             }
		             if (sintheta > 0) {
		                 distance = Math.min(distance, (yDim - 1 - yCenter)/sintheta);	 
		             }
		             if (sintheta < 0) {
		            	 distance = Math.min(distance, -yCenter/sintheta);
		             }
		             Preferences.debug("For i = " + i + " for inner contour leaving halving method setting distance = " + distance + "\n",
		            		 Preferences.DEBUG_ALGORITHM);
		         } //  if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) 
		         delX = costheta*distance;
		         delY = sintheta*distance;
		         innerX = xCenter + delX;
		         innerY = yCenter + delY;
		         boundaryDistance = innerContour.pinpol(innerX, innerY, snear, i1, i2) * innerPolarity;
		         if (halvingMethod) {
			         if (boundaryDistance < 0.0) {
			        	 // point outside polygon
			        	 maxDistance = distance;
			         }
			         else if (boundaryDistance > 0.0) {
			        	 // point inside polygon
			        	 minDistance = distance;
			         }
		         } // if (halvingMethod)
		     } // while (Math.abs(boundaryDistance) > 1.0E-1)
		     xArray[i][0] = innerX;
		     yArray[i][0] = innerY;
		     minDistance = distance;
		     maxDistance = largestDistance;
		     boundaryDistance = Double.MAX_VALUE;
		     halvingMethod = true;
		     while (Math.abs(boundaryDistance) > 1.0E-1) {
		    	 if (halvingMethod) {
		             distance = (minDistance + maxDistance)/2.0;
		    	 }
		    	 else {
		    		 distance = distance - 0.1;
		    		 if (distance <= 0.0) {
		    			 setCompleted(false);
		    			 return;
		    		 }
		    	 }
		         if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) {
		             halvingMethod = false;
		             distance = largestDistance;
		             if (costheta > 0) {
		                 distance = Math.min(distance, (xDim - 1 - xCenter)/costheta);	 
		             }
		             if (costheta < 0) {
		            	 distance = Math.min(distance, -xCenter/costheta);
		             }
		             if (sintheta > 0) {
		                 distance = Math.min(distance, (yDim - 1 - yCenter)/sintheta);	 
		             }
		             if (sintheta < 0) {
		            	 distance = Math.min(distance, -yCenter/sintheta);
		             }
		             Preferences.debug("For i = " + i + " for outer contour leaving halving method setting distance = " + distance + "\n",
		            		 Preferences.DEBUG_ALGORITHM);
		         } //  if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) 
		         delX = costheta*distance;
		         delY = sintheta*distance;
		         outerX = xCenter + delX;
		         outerY = yCenter + delY;
		         boundaryDistance = outerContour.pinpol(outerX, outerY, snear, i1, i2) * outerPolarity;
		         if (halvingMethod) {
			         if (boundaryDistance < 0.0) {
			        	 // point outside polygon
			        	 maxDistance = distance;
			         }
			         else if (boundaryDistance > 0.0) {
			        	 // point inside polygon
			        	 minDistance = distance;
			         }
		         } // if (halvingMethod)
		     } // while (Math.abs(boundaryDistance) > 1.0E-1)
		     xArray[i][linePoints-1] = outerX;
		     yArray[i][linePoints-1] = outerY;
		     delX = (outerX - innerX)/(linePoints-1);
		     delY = (outerY - innerY)/(linePoints-1);
		     for (j = 1; j <= linePoints-2; j++) {
		    	 xArray[i][j] = innerX + j * delX;
		         yArray[i][j] = innerY + j * delY;
		     }
		 } // for (i = 0; i < contourPoints; i++)
		 
		 maxGrad = -Double.MAX_VALUE;
		 minGrad = Double.MAX_VALUE;
		 for (i = 0; i < contourPoints; i++) {
			 for (j = 0; j < linePoints; j++) {
				 interpPt = new Vector2d(xArray[i][j], yArray[i][j]);
				 gx = AlgorithmConvolver.convolve2DPt(interpPt, srcImage.getExtents(), imgBuffer, kExtents, GxData);
                 gy = AlgorithmConvolver.convolve2DPt(interpPt, srcImage.getExtents(), imgBuffer, kExtents, GyData);

                 grad[i][j] = Math.sqrt(gx*gx + gy*gy);
                 if (grad[i][j] < minGrad) {
                	 minGrad = grad[i][j];
                 }
                 if (grad[i][j] > maxGrad) {
                	 maxGrad = grad[i][j];
                 }
			 }
		 }
		 
		 for (i = 0; i < contourPoints; i++) {
			 for (j = 0; j < linePoints; j++) {
			     Eimage[i][j] = -(grad[i][j] - minGrad)/(maxGrad - minGrad);
			 }
		 }
		 
		 
		 for (i = 0; i < linePoints; i++) {
			 statePreceding[0][i] = linePoints/2;
		 }
		 
		 for (i = 1; i <= contourPoints-2; i++) {
			 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
			     minState = Double.MAX_VALUE;
			     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
			    	 numX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
			    			 xArray[i+1][nextLinePoint];
			    	 numY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
			    			 yArray[i+1][nextLinePoint];
		    		 num = (numX*numX) + (numY*numY);
		    		 denomX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - xArray[i+1][nextLinePoint];
		    		 denomY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - yArray[i+1][nextLinePoint];
		    		 denom = (denomX*denomX) + (denomY*denomY);
		    		 Eint = num/denom;
		    		 stateEnergyTemp = stateEnergy[i-1][presentLinePoint]
		    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
		    		 if (stateEnergyTemp < minState) {
		    	    	 minState = stateEnergyTemp;
		    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
		    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
		    	     }
			     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
			 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
		 } // for (i = 1; i <= contourPoints-2; i++)
		 
		 minState = Double.MAX_VALUE;
		 terminatingLinePoint = -1;
		 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
			 if (stateEnergy[contourPoints-2][nextLinePoint] < minState) {
			     minState = stateEnergy[contourPoints-2][nextLinePoint];
			     terminatingLinePoint = nextLinePoint;
			 }
		 }
		 
		 
		 nextLinePoint = terminatingLinePoint;
		 startingLinePoint = -1;
		 for (i = contourPoints-2; i >= contourPoints/2 -1; i--) {
		     presentLinePoint = statePreceding[i][nextLinePoint];
		     if (i == contourPoints/2) {
		         startingLinePoint = presentLinePoint;	 
		     }
		     else if (i == contourPoints/2 - 1) {
		    	 terminatingLinePoint = presentLinePoint;
		     }
		     nextLinePoint = presentLinePoint;
		 } // for (i = contourPoints-2; i >= contourPoints/2 -1; i--)
		 
		 
		 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
			 statePreceding[contourPoints/2][nextLinePoint] = startingLinePoint;
		 }
		 
		 for (i = 0; i < contourPoints; i++) {
			 for (j = 0; j < linePoints; j++) {
				 stateEnergy[i][j] = 0.0;
			 }
		 }
		 
		 for (i = contourPoints/2+1; i <= contourPoints-1; i++) {
			 if (i == contourPoints - 1) {
				 ip1 = 0;
			 }
			 else {
				 ip1 = i + 1;
			 }
			 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
			     minState = Double.MAX_VALUE;
			     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
			    	 numX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
			    			 xArray[ip1][nextLinePoint];
			    	 numY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
			    			 yArray[ip1][nextLinePoint];
		    		 num = (numX*numX) + (numY*numY);
		    		 denomX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - xArray[ip1][nextLinePoint];
		    		 denomY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - yArray[ip1][nextLinePoint];
		    		 denom = (denomX*denomX) + (denomY*denomY);
		    		 Eint = num/denom;
		    		 stateEnergyTemp = stateEnergy[i-1][presentLinePoint]
		    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
		    		 if (stateEnergyTemp < minState) {
		    	    	 minState = stateEnergyTemp;
		    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
		    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
		    	     }
			     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
			 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
		 } // for (i = contourPoints/2+1; i <= contourPoints-1; i++)
		 
		 for (i = 0; i < contourPoints/2-1; i++) {
			 if (i == 0) {
				 im1 = contourPoints-1;
			 }
			 else {
				 im1 = i - 1;
			 }
			 if (i < contourPoints/2 - 2) {
				 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				     minState = Double.MAX_VALUE;
				     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
				    	 numX = xArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
				    			 xArray[i+1][nextLinePoint];
				    	 numY = yArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
				    			 yArray[i+1][nextLinePoint];
			    		 num = (numX*numX) + (numY*numY);
			    		 denomX = xArray[im1][statePreceding[im1][presentLinePoint]] - xArray[i+1][nextLinePoint];
			    		 denomY = yArray[im1][statePreceding[im1][presentLinePoint]] - yArray[i+1][nextLinePoint];
			    		 denom = (denomX*denomX) + (denomY*denomY);
			    		 Eint = num/denom;
			    		 stateEnergyTemp = stateEnergy[im1][presentLinePoint]
			    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
			    		 if (stateEnergyTemp < minState) {
			    	    	 minState = stateEnergyTemp;
			    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
			    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
			    	     }
				     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
				 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
			 } // if (i < contourPoints/2 - 2)
			 else {
				 nextLinePoint = terminatingLinePoint;
				 minState = Double.MAX_VALUE;
			     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
			    	 numX = xArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
			    			 xArray[i+1][nextLinePoint];
			    	 numY = yArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
			    			 yArray[i+1][nextLinePoint];
		    		 num = (numX*numX) + (numY*numY);
		    		 denomX = xArray[im1][statePreceding[im1][presentLinePoint]] - xArray[i+1][nextLinePoint];
		    		 denomY = yArray[im1][statePreceding[im1][presentLinePoint]] - yArray[i+1][nextLinePoint];
		    		 denom = (denomX*denomX) + (denomY*denomY);
		    		 Eint = num/denom;
		    		 stateEnergyTemp = stateEnergy[im1][presentLinePoint]
		    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
		    		 if (stateEnergyTemp < minState) {
		    	    	 minState = stateEnergyTemp;
		    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
		    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
		    	     }
			     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
			 }
		 } // for (i = 0; i < contourPoints/2-1; i++)
		 
		 bestLinePoint[contourPoints/2] = startingLinePoint;
		 bestLinePoint[contourPoints/2 - 1] = terminatingLinePoint;
		 nextLinePoint = terminatingLinePoint;
		 for (i = contourPoints/2 - 2; i >= 0; i--) {
		     presentLinePoint = statePreceding[i][nextLinePoint];
		     bestLinePoint[i] = presentLinePoint;
		     nextLinePoint = presentLinePoint;
		 }
		 
		 for (i = contourPoints-1; i >= contourPoints/2+1; i--) {
			 presentLinePoint = statePreceding[i][nextLinePoint];
		     bestLinePoint[i] = presentLinePoint;
		     nextLinePoint = presentLinePoint;	 
		 }
		 
		 
		 
		 for (i = 0; i < contourPoints; i++) {
			 resultPt[i] = new Vector3f((float)xArray[i][bestLinePoint[i]], (float)yArray[i][bestLinePoint[i]], 0.0f);
		 }
		 resultVOI.importCurve(resultPt);
		 VOIs.add(resultVOI);
		 srcImage.setVOIs(VOIs);
         srcImage.notifyImageDisplayListeners();
		
		setCompleted(true);
		return;
	}
	
	public void run3D() {
		 VOIVector VOIs;
		 VOI innerVOI;
		 VOI outerVOI;
		 VOIContour innerContour;
		 VOIContour outerContour;
		 Vector3f innerCenter;
		 double xCenter;
		 double yCenter;
		 int i;
		 int xDim;
		 int yDim;
		 int zDim;
		 double largestDistance;
		 double distance;
		 double theta;
		 double costheta[] = new double[contourPoints];
		 double sintheta[] = new double[contourPoints];
		 double minDistance;
		 double maxDistance;
		 boolean snear[] = new boolean[1];
		 int i1[] = new int[1];
		 int i2[] = new int[1];
		 double delX;
		 double delY;
		 double innerX = -1.0;
		 double innerY = -1.0;
		 double outerX = -1.0;
		 double outerY = -1.0;
		 double boundaryDistance;
		 double xArray[][] = new double[contourPoints][linePoints];
		 double yArray[][] = new double[contourPoints][linePoints];
		 double grad[][] = new double[contourPoints][linePoints];
		 double Eimage[][] = new double[contourPoints][linePoints];
		 double maxGrad;
		 double minGrad;
		 int j;
		 int length;
		 double[] imgBuffer;
		 double gx;
		 double gy;
		 Vector2d interpPt;
		 int bestLinePoint[] = new int[contourPoints];
		 double stateEnergy[][] = new double[contourPoints][linePoints];
		 int statePreceding[][] = new int[contourPoints][linePoints];
		 int presentLinePoint;
		 int nextLinePoint;
		 double Eint;
		 double num;
		 double numX;
		 double numY;
		 double denom;
		 double denomX;
		 double denomY;
		 double minState;
		 int ip1;
		 int im1;
		 Vector3f resultPt[] = new Vector3f[contourPoints];
		 double stateEnergyTemp;
		 int startingLinePoint;
		 int terminatingLinePoint;
		 int xBounds[] = new int[2];
		 int yBounds[] = new int[2];
		 int zBounds[] = new int[2];
		 double largestBoundaryDistance;
		 int x;
		 int y;
		 boolean halvingMethod;
		 int innerPolarity = 1;
		 int outerPolarity = 1;
		 int startingZ;
		 
		 xDim = srcImage.getExtents()[0];
		 yDim = srcImage.getExtents()[1];
		 zDim = srcImage.getExtents()[2];
		 makeKernels2D();
		 VOIs = srcImage.getVOIs();
		 innerVOI = VOIs.VOIAt(innerIndex);
		 outerVOI = VOIs.VOIAt(outerIndex);
		 innerContour = (VOIContour)innerVOI.getCurves().elementAt(0);
		 outerContour = (VOIContour)outerVOI.getCurves().elementAt(0);
		 startingZ = Math.round(innerContour.elementAt(0).Z);
		 length = xDim * yDim;
         imgBuffer = new double[length];
         double startingContractX[] = new double[contourPoints];
         double startingContractY[] = new double[contourPoints];
         double startingExpandX[] = new double[contourPoints];
         double startingExpandY[] = new double[contourPoints];
         int z;
		 short voiNum = (short)VOIs.size();
		 
		 for (i = 0; i < contourPoints; i++) {
		     theta = 2.0 * i * Math.PI/contourPoints;
		     costheta[i] = Math.cos(theta);
		     sintheta[i] = Math.sin(theta);
		 }
		 
		 try {
	            
	            srcImage.exportData(startingZ*length, length, imgBuffer); // locks and releases lock

	            fireProgressStateChanged(srcImage.getImageName(), "Searching for boundary ...");

	        } catch (IOException error) {
	            displayError("Algorithm DualContourSearch: Image(s) locked");
	            setCompleted(false);

	            return;
	        } catch (OutOfMemoryError e) {
	            displayError("Algorithm DualContourSearch:  Out of Memory");
	            setCompleted(false);

	            return;
	        }
		 
		 
		 
         resultVOI = new VOI(voiNum++, "resultVOI" + startingZ, VOI.CONTOUR, 1.0f/3.0f);
        
		 
		 // Lines protruding out from contours can flip the polarity of pinpol function
		 boundaryDistance = innerContour.pinpol(2*xDim, 2*yDim, snear, i1, i2);
		 if (boundaryDistance > 0.0) {
			 innerPolarity = -1;
		 }
		
		 boundaryDistance = outerContour.pinpol(2*xDim, 2*yDim, snear, i1, i2);
		 if (boundaryDistance > 0.0) {
			 outerPolarity = -1;
		 }
		 innerCenter = innerContour.getGeometricCenter();
		 xCenter = innerCenter.X;
		 yCenter = innerCenter.Y;
		 boundaryDistance = innerContour.pinpol(xCenter, yCenter, snear, i1, i2) * innerPolarity;
        if (boundaryDistance < 0.0) {
       	 // point outside polygon
       	 Preferences.debug("Inner contour geometric center is outside innter contour\n", Preferences.DEBUG_ALGORITHM);
       	 innerContour.getBounds(xBounds, yBounds, zBounds);
       	 largestBoundaryDistance = 0.0;
       	 for (y = yBounds[0]; y <= yBounds[1]; y++) {
       	     for (x = xBounds[0]; x <= xBounds[1]; x++) {
       	    	 boundaryDistance = innerContour.pinpol(x, y, snear, i1, i2) * innerPolarity;
       	    	 if (boundaryDistance > largestBoundaryDistance) {
       	    		 largestBoundaryDistance = boundaryDistance;
       	    		 xCenter = x;
       	    		 yCenter = y;
       	    	 }
       	     }
       	 }
       	 Preferences.debug("largestBoundaryDistance = " + largestBoundaryDistance + "\n", Preferences.DEBUG_ALGORITHM);
        } // if (boundaryDistance < 0.0)
		 
		 largestDistance = Math.sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1));
		 
		 for (i = 0; i < contourPoints; i++) {
		     theta = 2.0 * i * Math.PI/contourPoints;
		     costheta[i] = Math.cos(theta);
		     sintheta[i] = Math.sin(theta);
		     minDistance = 0.0;
		     maxDistance = largestDistance;
		     distance = largestDistance/2.0;
		     boundaryDistance = Double.MAX_VALUE;
		     halvingMethod = true;
		     while (Math.abs(boundaryDistance) > 1.0E-1) {
		    	 if (halvingMethod) {
		             distance = (minDistance + maxDistance)/2.0;
		    	 }
		    	 else {
		    		 distance = distance - 0.1;
		    		 if (distance <= 0.0) {
		    			 setCompleted(false);
		    			 return;
		    		 }
		    	 }
		         if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) {
		             halvingMethod = false;
		             distance = largestDistance;
		             if (costheta[i] > 0) {
		                 distance = Math.min(distance, (xDim - 1 - xCenter)/costheta[i]);	 
		             }
		             if (costheta[i] < 0) {
		            	 distance = Math.min(distance, -xCenter/costheta[i]);
		             }
		             if (sintheta[i] > 0) {
		                 distance = Math.min(distance, (yDim - 1 - yCenter)/sintheta[i]);	 
		             }
		             if (sintheta[i] < 0) {
		            	 distance = Math.min(distance, -yCenter/sintheta[i]);
		             }
		             Preferences.debug("For i = " + i + " for inner contour leaving halving method setting distance = " + distance + "\n",
		            		 Preferences.DEBUG_ALGORITHM);
		         } //  if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) 
		         delX = costheta[i]*distance;
		         delY = sintheta[i]*distance;
		         innerX = xCenter + delX;
		         innerY = yCenter + delY;
		         boundaryDistance = innerContour.pinpol(innerX, innerY, snear, i1, i2) * innerPolarity;
		         if (halvingMethod) {
			         if (boundaryDistance < 0.0) {
			        	 // point outside polygon
			        	 maxDistance = distance;
			         }
			         else if (boundaryDistance > 0.0) {
			        	 // point inside polygon
			        	 minDistance = distance;
			         }
		         } // if (halvingMethod)
		     } // while (Math.abs(boundaryDistance) > 1.0E-1)
		     xArray[i][0] = innerX;
		     yArray[i][0] = innerY;
		     minDistance = distance;
		     maxDistance = largestDistance;
		     boundaryDistance = Double.MAX_VALUE;
		     halvingMethod = true;
		     while (Math.abs(boundaryDistance) > 1.0E-1) {
		    	 if (halvingMethod) {
		             distance = (minDistance + maxDistance)/2.0;
		    	 }
		    	 else {
		    		 distance = distance - 0.1;
		    		 if (distance <= 0.0) {
		    			 setCompleted(false);
		    			 return;
		    		 }
		    	 }
		         if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) {
		             halvingMethod = false;
		             distance = largestDistance;
		             if (costheta[i] > 0) {
		                 distance = Math.min(distance, (xDim - 1 - xCenter)/costheta[i]);	 
		             }
		             if (costheta[i] < 0) {
		            	 distance = Math.min(distance, -xCenter/costheta[i]);
		             }
		             if (sintheta[i] > 0) {
		                 distance = Math.min(distance, (yDim - 1 - yCenter)/sintheta[i]);	 
		             }
		             if (sintheta[i] < 0) {
		            	 distance = Math.min(distance, -yCenter/sintheta[i]);
		             }
		             Preferences.debug("For i = " + i + " for outer contour leaving halving method setting distance = " + distance + "\n",
		            		 Preferences.DEBUG_ALGORITHM);
		         } //  if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) 
		         delX = costheta[i]*distance;
		         delY = sintheta[i]*distance;
		         outerX = xCenter + delX;
		         outerY = yCenter + delY;
		         boundaryDistance = outerContour.pinpol(outerX, outerY, snear, i1, i2) * outerPolarity;
		         if (halvingMethod) {
			         if (boundaryDistance < 0.0) {
			        	 // point outside polygon
			        	 maxDistance = distance;
			         }
			         else if (boundaryDistance > 0.0) {
			        	 // point inside polygon
			        	 minDistance = distance;
			         }
		         } // if (halvingMethod)
		     } // while (Math.abs(boundaryDistance) > 1.0E-1)
		     xArray[i][linePoints-1] = outerX;
		     yArray[i][linePoints-1] = outerY;
		     delX = (outerX - innerX)/(linePoints-1);
		     delY = (outerY - innerY)/(linePoints-1);
		     for (j = 1; j <= linePoints-2; j++) {
		    	 xArray[i][j] = innerX + j * delX;
		         yArray[i][j] = innerY + j * delY;
		     }
		 } // for (i = 0; i < contourPoints; i++)
		 
		 maxGrad = -Double.MAX_VALUE;
		 minGrad = Double.MAX_VALUE;
		 for (i = 0; i < contourPoints; i++) {
			 for (j = 0; j < linePoints; j++) {
				 interpPt = new Vector2d(xArray[i][j], yArray[i][j]);
				 gx = AlgorithmConvolver.convolve2DPt(interpPt, srcImage.getExtents(), imgBuffer, kExtents, GxData);
                gy = AlgorithmConvolver.convolve2DPt(interpPt, srcImage.getExtents(), imgBuffer, kExtents, GyData);

                grad[i][j] = Math.sqrt(gx*gx + gy*gy);
                if (grad[i][j] < minGrad) {
               	 minGrad = grad[i][j];
                }
                if (grad[i][j] > maxGrad) {
               	 maxGrad = grad[i][j];
                }
			 }
		 }
		 
		 for (i = 0; i < contourPoints; i++) {
			 for (j = 0; j < linePoints; j++) {
			     Eimage[i][j] = -(grad[i][j] - minGrad)/(maxGrad - minGrad);
			 }
		 }
		 
		 
		 for (i = 0; i < linePoints; i++) {
			 statePreceding[0][i] = linePoints/2;
		 }
		 
		 for (i = 1; i <= contourPoints-2; i++) {
			 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
			     minState = Double.MAX_VALUE;
			     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
			    	 numX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
			    			 xArray[i+1][nextLinePoint];
			    	 numY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
			    			 yArray[i+1][nextLinePoint];
		    		 num = (numX*numX) + (numY*numY);
		    		 denomX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - xArray[i+1][nextLinePoint];
		    		 denomY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - yArray[i+1][nextLinePoint];
		    		 denom = (denomX*denomX) + (denomY*denomY);
		    		 Eint = num/denom;
		    		 stateEnergyTemp = stateEnergy[i-1][presentLinePoint]
		    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
		    		 if (stateEnergyTemp < minState) {
		    	    	 minState = stateEnergyTemp;
		    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
		    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
		    	     }
			     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
			 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
		 } // for (i = 1; i <= contourPoints-2; i++)
		 
		 minState = Double.MAX_VALUE;
		 terminatingLinePoint = -1;
		 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
			 if (stateEnergy[contourPoints-2][nextLinePoint] < minState) {
			     minState = stateEnergy[contourPoints-2][nextLinePoint];
			     terminatingLinePoint = nextLinePoint;
			 }
		 }
		 
		 
		 nextLinePoint = terminatingLinePoint;
		 startingLinePoint = -1;
		 for (i = contourPoints-2; i >= contourPoints/2 -1; i--) {
		     presentLinePoint = statePreceding[i][nextLinePoint];
		     if (i == contourPoints/2) {
		         startingLinePoint = presentLinePoint;	 
		     }
		     else if (i == contourPoints/2 - 1) {
		    	 terminatingLinePoint = presentLinePoint;
		     }
		     nextLinePoint = presentLinePoint;
		 } // for (i = contourPoints-2; i >= contourPoints/2 -1; i--)
		 
		 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
			 statePreceding[contourPoints/2][nextLinePoint] = startingLinePoint;
		 }
		 
		 for (i = 0; i < contourPoints; i++) {
			 for (j = 0; j < linePoints; j++) {
				 stateEnergy[i][j] = 0.0;
			 }
		 }
		 
		 for (i = contourPoints/2+1; i <= contourPoints-1; i++) {
			 if (i == contourPoints - 1) {
				 ip1 = 0;
			 }
			 else {
				 ip1 = i + 1;
			 }
			 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
			     minState = Double.MAX_VALUE;
			     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
			    	 numX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
			    			 xArray[ip1][nextLinePoint];
			    	 numY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
			    			 yArray[ip1][nextLinePoint];
		    		 num = (numX*numX) + (numY*numY);
		    		 denomX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - xArray[ip1][nextLinePoint];
		    		 denomY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - yArray[ip1][nextLinePoint];
		    		 denom = (denomX*denomX) + (denomY*denomY);
		    		 Eint = num/denom;
		    		 stateEnergyTemp = stateEnergy[i-1][presentLinePoint]
		    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
		    		 if (stateEnergyTemp < minState) {
		    	    	 minState = stateEnergyTemp;
		    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
		    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
		    	     }
			     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
			 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
		 } // for (i = contourPoints/2+1; i <= contourPoints-1; i++)
		 
		 for (i = 0; i < contourPoints/2-1; i++) {
			 if (i == 0) {
				 im1 = contourPoints-1;
			 }
			 else {
				 im1 = i - 1;
			 }
			 if (i < contourPoints/2 - 2) {
				 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				     minState = Double.MAX_VALUE;
				     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
				    	 numX = xArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
				    			 xArray[i+1][nextLinePoint];
				    	 numY = yArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
				    			 yArray[i+1][nextLinePoint];
			    		 num = (numX*numX) + (numY*numY);
			    		 denomX = xArray[im1][statePreceding[im1][presentLinePoint]] - xArray[i+1][nextLinePoint];
			    		 denomY = yArray[im1][statePreceding[im1][presentLinePoint]] - yArray[i+1][nextLinePoint];
			    		 denom = (denomX*denomX) + (denomY*denomY);
			    		 Eint = num/denom;
			    		 stateEnergyTemp = stateEnergy[im1][presentLinePoint]
			    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
			    		 if (stateEnergyTemp < minState) {
			    	    	 minState = stateEnergyTemp;
			    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
			    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
			    	     }
				     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
				 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
			 } // if (i < contourPoints/2 - 2)
			 else {
				 nextLinePoint = terminatingLinePoint;
				 minState = Double.MAX_VALUE;
			     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
			    	 numX = xArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
			    			 xArray[i+1][nextLinePoint];
			    	 numY = yArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
			    			 yArray[i+1][nextLinePoint];
		    		 num = (numX*numX) + (numY*numY);
		    		 denomX = xArray[im1][statePreceding[im1][presentLinePoint]] - xArray[i+1][nextLinePoint];
		    		 denomY = yArray[im1][statePreceding[im1][presentLinePoint]] - yArray[i+1][nextLinePoint];
		    		 denom = (denomX*denomX) + (denomY*denomY);
		    		 Eint = num/denom;
		    		 stateEnergyTemp = stateEnergy[im1][presentLinePoint]
		    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
		    		 if (stateEnergyTemp < minState) {
		    	    	 minState = stateEnergyTemp;
		    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
		    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
		    	     }
			     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
			 }
		 } // for (i = 0; i < contourPoints/2-1; i++)
		 
		 bestLinePoint[contourPoints/2] = startingLinePoint;
		 bestLinePoint[contourPoints/2 - 1] = terminatingLinePoint;
		 nextLinePoint = terminatingLinePoint;
		 for (i = contourPoints/2 - 2; i >= 0; i--) {
		     presentLinePoint = statePreceding[i][nextLinePoint];
		     bestLinePoint[i] = presentLinePoint;
		     nextLinePoint = presentLinePoint;
		 }
		 
		 for (i = contourPoints-1; i >= contourPoints/2+1; i--) {
			 presentLinePoint = statePreceding[i][nextLinePoint];
		     bestLinePoint[i] = presentLinePoint;
		     nextLinePoint = presentLinePoint;	 
		 }
		  
		 for (i = 0; i < contourPoints; i++) {
			 resultPt[i] = new Vector3f((float)xArray[i][bestLinePoint[i]], (float)yArray[i][bestLinePoint[i]], (float)startingZ);
			 startingContractX[i] = xArray[i][bestLinePoint[i]] - pixelsContract * costheta[i];
			 startingExpandX[i] = xArray[i][bestLinePoint[i]] + pixelsExpand * costheta[i];
			 startingContractY[i] = yArray[i][bestLinePoint[i]] - pixelsContract * sintheta[i];
			 startingExpandY[i] = yArray[i][bestLinePoint[i]] + pixelsExpand * sintheta[i];
		 }
		 resultVOI.importCurve(resultPt);
		 VOIs.add(resultVOI);
		 
		 innerContour.removeAllElements();
		 outerContour.removeAllElements();
		 for (i = 0; i < contourPoints; i++) {
			 innerContour.add(new Vector3f((float)startingContractX[i], (float)startingContractY[i], (float)(startingZ - 1)));
			 outerContour.add(new Vector3f((float)startingExpandX[i], (float)startingExpandY[i], (float)(startingZ - 1)));
		 }
		 for (z = startingZ - 1; z >= 0; z--) {
			 fireProgressStateChanged(100 * (startingZ - z)/zDim);
			 try {
		            
		            srcImage.exportData(z*length, length, imgBuffer); // locks and releases lock


		        } catch (IOException error) {
		            displayError("Algorithm DualContourSearch: Image(s) locked");
		            setCompleted(false);

		            return;
		        } catch (OutOfMemoryError e) {
		            displayError("Algorithm DualContourSearch:  Out of Memory");
		            setCompleted(false);

		            return;
		        }
			 
			 
			 
	         resultVOI = new VOI(voiNum++, "resultVOI" + z, VOI.CONTOUR, 1.0f/3.0f);
	        
			 
			 // Lines protruding out from contours can flip the polarity of pinpol function
			 boundaryDistance = innerContour.pinpol(2*xDim, 2*yDim, snear, i1, i2);
			 if (boundaryDistance > 0.0) {
				 innerPolarity = -1;
			 }
			
			 boundaryDistance = outerContour.pinpol(2*xDim, 2*yDim, snear, i1, i2);
			 if (boundaryDistance > 0.0) {
				 outerPolarity = -1;
			 }
			 innerCenter = innerContour.getGeometricCenter();
			 xCenter = innerCenter.X;
			 yCenter = innerCenter.Y;
			 boundaryDistance = innerContour.pinpol(xCenter, yCenter, snear, i1, i2) * innerPolarity;
	        if (boundaryDistance < 0.0) {
	       	 // point outside polygon
	       	 Preferences.debug("Inner contour geometric center is outside innter contour\n", Preferences.DEBUG_ALGORITHM);
	       	 innerContour.getBounds(xBounds, yBounds, zBounds);
	       	 largestBoundaryDistance = 0.0;
	       	 for (y = yBounds[0]; y <= yBounds[1]; y++) {
	       	     for (x = xBounds[0]; x <= xBounds[1]; x++) {
	       	    	 boundaryDistance = innerContour.pinpol(x, y, snear, i1, i2) * innerPolarity;
	       	    	 if (boundaryDistance > largestBoundaryDistance) {
	       	    		 largestBoundaryDistance = boundaryDistance;
	       	    		 xCenter = x;
	       	    		 yCenter = y;
	       	    	 }
	       	     }
	       	 }
	       	 Preferences.debug("largestBoundaryDistance = " + largestBoundaryDistance + "\n", Preferences.DEBUG_ALGORITHM);
	        } // if (boundaryDistance < 0.0)
			 
			 largestDistance = Math.sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1));
			 
			 for (i = 0; i < contourPoints; i++) {
			     theta = 2.0 * i * Math.PI/contourPoints;
			     costheta[i] = Math.cos(theta);
			     sintheta[i] = Math.sin(theta);
			     minDistance = 0.0;
			     maxDistance = largestDistance;
			     distance = largestDistance/2.0;
			     boundaryDistance = Double.MAX_VALUE;
			     halvingMethod = true;
			     while (Math.abs(boundaryDistance) > 1.0E-1) {
			    	 if (halvingMethod) {
			             distance = (minDistance + maxDistance)/2.0;
			    	 }
			    	 else {
			    		 distance = distance - 0.1;
			    		 if (distance <= 0.0) {
			    			 setCompleted(false);
			    			 return;
			    		 }
			    	 }
			         if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) {
			             halvingMethod = false;
			             distance = largestDistance;
			             if (costheta[i] > 0) {
			                 distance = Math.min(distance, (xDim - 1 - xCenter)/costheta[i]);	 
			             }
			             if (costheta[i] < 0) {
			            	 distance = Math.min(distance, -xCenter/costheta[i]);
			             }
			             if (sintheta[i] > 0) {
			                 distance = Math.min(distance, (yDim - 1 - yCenter)/sintheta[i]);	 
			             }
			             if (sintheta[i] < 0) {
			            	 distance = Math.min(distance, -yCenter/sintheta[i]);
			             }
			             Preferences.debug("For i = " + i + " for inner contour leaving halving method setting distance = " + distance + "\n",
			            		 Preferences.DEBUG_ALGORITHM);
			         } //  if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) 
			         delX = costheta[i]*distance;
			         delY = sintheta[i]*distance;
			         innerX = xCenter + delX;
			         innerY = yCenter + delY;
			         boundaryDistance = innerContour.pinpol(innerX, innerY, snear, i1, i2) * innerPolarity;
			         if (halvingMethod) {
				         if (boundaryDistance < 0.0) {
				        	 // point outside polygon
				        	 maxDistance = distance;
				         }
				         else if (boundaryDistance > 0.0) {
				        	 // point inside polygon
				        	 minDistance = distance;
				         }
			         } // if (halvingMethod)
			     } // while (Math.abs(boundaryDistance) > 1.0E-1)
			     xArray[i][0] = innerX;
			     yArray[i][0] = innerY;
			     minDistance = distance;
			     maxDistance = largestDistance;
			     boundaryDistance = Double.MAX_VALUE;
			     halvingMethod = true;
			     while (Math.abs(boundaryDistance) > 1.0E-1) {
			    	 if (halvingMethod) {
			             distance = (minDistance + maxDistance)/2.0;
			    	 }
			    	 else {
			    		 distance = distance - 0.1;
			    		 if (distance <= 0.0) {
			    			 setCompleted(false);
			    			 return;
			    		 }
			    	 }
			         if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) {
			             halvingMethod = false;
			             distance = largestDistance;
			             if (costheta[i] > 0) {
			                 distance = Math.min(distance, (xDim - 1 - xCenter)/costheta[i]);	 
			             }
			             if (costheta[i] < 0) {
			            	 distance = Math.min(distance, -xCenter/costheta[i]);
			             }
			             if (sintheta[i] > 0) {
			                 distance = Math.min(distance, (yDim - 1 - yCenter)/sintheta[i]);	 
			             }
			             if (sintheta[i] < 0) {
			            	 distance = Math.min(distance, -yCenter/sintheta[i]);
			             }
			             Preferences.debug("For i = " + i + " for outer contour leaving halving method setting distance = " + distance + "\n",
			            		 Preferences.DEBUG_ALGORITHM);
			         } //  if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) 
			         delX = costheta[i]*distance;
			         delY = sintheta[i]*distance;
			         outerX = xCenter + delX;
			         outerY = yCenter + delY;
			         boundaryDistance = outerContour.pinpol(outerX, outerY, snear, i1, i2) * outerPolarity;
			         if (halvingMethod) {
				         if (boundaryDistance < 0.0) {
				        	 // point outside polygon
				        	 maxDistance = distance;
				         }
				         else if (boundaryDistance > 0.0) {
				        	 // point inside polygon
				        	 minDistance = distance;
				         }
			         } // if (halvingMethod)
			     } // while (Math.abs(boundaryDistance) > 1.0E-1)
			     xArray[i][linePoints-1] = outerX;
			     yArray[i][linePoints-1] = outerY;
			     delX = (outerX - innerX)/(linePoints-1);
			     delY = (outerY - innerY)/(linePoints-1);
			     for (j = 1; j <= linePoints-2; j++) {
			    	 xArray[i][j] = innerX + j * delX;
			         yArray[i][j] = innerY + j * delY;
			     }
			 } // for (i = 0; i < contourPoints; i++)
			 
			 maxGrad = -Double.MAX_VALUE;
			 minGrad = Double.MAX_VALUE;
			 for (i = 0; i < contourPoints; i++) {
				 for (j = 0; j < linePoints; j++) {
					 interpPt = new Vector2d(xArray[i][j], yArray[i][j]);
					 gx = AlgorithmConvolver.convolve2DPt(interpPt, srcImage.getExtents(), imgBuffer, kExtents, GxData);
	                gy = AlgorithmConvolver.convolve2DPt(interpPt, srcImage.getExtents(), imgBuffer, kExtents, GyData);

	                grad[i][j] = Math.sqrt(gx*gx + gy*gy);
	                if (grad[i][j] < minGrad) {
	               	 minGrad = grad[i][j];
	                }
	                if (grad[i][j] > maxGrad) {
	               	 maxGrad = grad[i][j];
	                }
				 }
			 }
			 
			 for (i = 0; i < contourPoints; i++) {
				 for (j = 0; j < linePoints; j++) {
				     Eimage[i][j] = -(grad[i][j] - minGrad)/(maxGrad - minGrad);
				 }
			 }
			 
			 
			 for (i = 0; i < linePoints; i++) {
				 statePreceding[0][i] = linePoints/2;
			 }
			 
			 for (i = 1; i <= contourPoints-2; i++) {
				 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				     minState = Double.MAX_VALUE;
				     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
				    	 numX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
				    			 xArray[i+1][nextLinePoint];
				    	 numY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
				    			 yArray[i+1][nextLinePoint];
			    		 num = (numX*numX) + (numY*numY);
			    		 denomX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - xArray[i+1][nextLinePoint];
			    		 denomY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - yArray[i+1][nextLinePoint];
			    		 denom = (denomX*denomX) + (denomY*denomY);
			    		 Eint = num/denom;
			    		 stateEnergyTemp = stateEnergy[i-1][presentLinePoint]
			    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
			    		 if (stateEnergyTemp < minState) {
			    	    	 minState = stateEnergyTemp;
			    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
			    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
			    	     }
				     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
				 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
			 } // for (i = 1; i <= contourPoints-2; i++)
			 
			 minState = Double.MAX_VALUE;
			 terminatingLinePoint = -1;
			 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				 if (stateEnergy[contourPoints-2][nextLinePoint] < minState) {
				     minState = stateEnergy[contourPoints-2][nextLinePoint];
				     terminatingLinePoint = nextLinePoint;
				 }
			 }
			 
			 
			 nextLinePoint = terminatingLinePoint;
			 startingLinePoint = -1;
			 for (i = contourPoints-2; i >= contourPoints/2 -1; i--) {
			     presentLinePoint = statePreceding[i][nextLinePoint];
			     if (i == contourPoints/2) {
			         startingLinePoint = presentLinePoint;	 
			     }
			     else if (i == contourPoints/2 - 1) {
			    	 terminatingLinePoint = presentLinePoint;
			     }
			     nextLinePoint = presentLinePoint;
			 } // for (i = contourPoints-2; i >= contourPoints/2 -1; i--)
			 
			 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				 statePreceding[contourPoints/2][nextLinePoint] = startingLinePoint;
			 }
			 
			 for (i = 0; i < contourPoints; i++) {
				 for (j = 0; j < linePoints; j++) {
					 stateEnergy[i][j] = 0.0;
				 }
			 }
			 
			 for (i = contourPoints/2+1; i <= contourPoints-1; i++) {
				 if (i == contourPoints - 1) {
					 ip1 = 0;
				 }
				 else {
					 ip1 = i + 1;
				 }
				 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				     minState = Double.MAX_VALUE;
				     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
				    	 numX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
				    			 xArray[ip1][nextLinePoint];
				    	 numY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
				    			 yArray[ip1][nextLinePoint];
			    		 num = (numX*numX) + (numY*numY);
			    		 denomX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - xArray[ip1][nextLinePoint];
			    		 denomY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - yArray[ip1][nextLinePoint];
			    		 denom = (denomX*denomX) + (denomY*denomY);
			    		 Eint = num/denom;
			    		 stateEnergyTemp = stateEnergy[i-1][presentLinePoint]
			    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
			    		 if (stateEnergyTemp < minState) {
			    	    	 minState = stateEnergyTemp;
			    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
			    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
			    	     }
				     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
				 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
			 } // for (i = contourPoints/2+1; i <= contourPoints-1; i++)
			 
			 for (i = 0; i < contourPoints/2-1; i++) {
				 if (i == 0) {
					 im1 = contourPoints-1;
				 }
				 else {
					 im1 = i - 1;
				 }
				 if (i < contourPoints/2 - 2) {
					 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
					     minState = Double.MAX_VALUE;
					     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
					    	 numX = xArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
					    			 xArray[i+1][nextLinePoint];
					    	 numY = yArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
					    			 yArray[i+1][nextLinePoint];
				    		 num = (numX*numX) + (numY*numY);
				    		 denomX = xArray[im1][statePreceding[im1][presentLinePoint]] - xArray[i+1][nextLinePoint];
				    		 denomY = yArray[im1][statePreceding[im1][presentLinePoint]] - yArray[i+1][nextLinePoint];
				    		 denom = (denomX*denomX) + (denomY*denomY);
				    		 Eint = num/denom;
				    		 stateEnergyTemp = stateEnergy[im1][presentLinePoint]
				    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
				    		 if (stateEnergyTemp < minState) {
				    	    	 minState = stateEnergyTemp;
				    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
				    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
				    	     }
					     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
					 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
				 } // if (i < contourPoints/2 - 2)
				 else {
					 nextLinePoint = terminatingLinePoint;
					 minState = Double.MAX_VALUE;
				     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
				    	 numX = xArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
				    			 xArray[i+1][nextLinePoint];
				    	 numY = yArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
				    			 yArray[i+1][nextLinePoint];
			    		 num = (numX*numX) + (numY*numY);
			    		 denomX = xArray[im1][statePreceding[im1][presentLinePoint]] - xArray[i+1][nextLinePoint];
			    		 denomY = yArray[im1][statePreceding[im1][presentLinePoint]] - yArray[i+1][nextLinePoint];
			    		 denom = (denomX*denomX) + (denomY*denomY);
			    		 Eint = num/denom;
			    		 stateEnergyTemp = stateEnergy[im1][presentLinePoint]
			    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
			    		 if (stateEnergyTemp < minState) {
			    	    	 minState = stateEnergyTemp;
			    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
			    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
			    	     }
				     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
				 }
			 } // for (i = 0; i < contourPoints/2-1; i++)
			 
			 bestLinePoint[contourPoints/2] = startingLinePoint;
			 bestLinePoint[contourPoints/2 - 1] = terminatingLinePoint;
			 nextLinePoint = terminatingLinePoint;
			 for (i = contourPoints/2 - 2; i >= 0; i--) {
			     presentLinePoint = statePreceding[i][nextLinePoint];
			     bestLinePoint[i] = presentLinePoint;
			     nextLinePoint = presentLinePoint;
			 }
			 
			 for (i = contourPoints-1; i >= contourPoints/2+1; i--) {
				 presentLinePoint = statePreceding[i][nextLinePoint];
			     bestLinePoint[i] = presentLinePoint;
			     nextLinePoint = presentLinePoint;	 
			 }
			  
			 innerContour.removeAllElements();
			 outerContour.removeAllElements();
			 for (i = 0; i < contourPoints; i++) {
				 resultPt[i] = new Vector3f((float)xArray[i][bestLinePoint[i]], (float)yArray[i][bestLinePoint[i]], (float)z);
				 if (z > 0) {
					 innerContour.add(new Vector3f((float)(xArray[i][bestLinePoint[i]] - pixelsContract * costheta[i]),
							 (float)(yArray[i][bestLinePoint[i]] - pixelsContract * sintheta[i]), (float)(z-1)));
					 outerContour.add(new Vector3f((float)(xArray[i][bestLinePoint[i]] + pixelsExpand * costheta[i]),
							 (float)(yArray[i][bestLinePoint[i]] + pixelsExpand * sintheta[i]), (float)(z-1)));
				 }
			 }
			 resultVOI.importCurve(resultPt);
			 VOIs.add(resultVOI);	 
		 } // for (z = startingZ - 1; z >= 0; z--)
		 innerContour.removeAllElements();
		 outerContour.removeAllElements();
		 for (i = 0; i < contourPoints; i++) {
			 innerContour.add(new Vector3f((float)startingContractX[i], (float)startingContractY[i], (float)(startingZ + 1)));
			 outerContour.add(new Vector3f((float)startingExpandX[i], (float)startingExpandY[i], (float)(startingZ + 1)));
		 }
		 for (z = startingZ + 1; z < zDim; z++) {
			 fireProgressStateChanged(z * 100/zDim);
			 try {
		            
		            srcImage.exportData(z*length, length, imgBuffer); // locks and releases lock

		        } catch (IOException error) {
		            displayError("Algorithm DualContourSearch: Image(s) locked");
		            setCompleted(false);

		            return;
		        } catch (OutOfMemoryError e) {
		            displayError("Algorithm DualContourSearch:  Out of Memory");
		            setCompleted(false);

		            return;
		        }
			 
			 
			 
	         resultVOI = new VOI(voiNum++, "resultVOI" + z, VOI.CONTOUR, 1.0f/3.0f);
	        
			 
			 // Lines protruding out from contours can flip the polarity of pinpol function
			 boundaryDistance = innerContour.pinpol(2*xDim, 2*yDim, snear, i1, i2);
			 if (boundaryDistance > 0.0) {
				 innerPolarity = -1;
			 }
			
			 boundaryDistance = outerContour.pinpol(2*xDim, 2*yDim, snear, i1, i2);
			 if (boundaryDistance > 0.0) {
				 outerPolarity = -1;
			 }
			 innerCenter = innerContour.getGeometricCenter();
			 xCenter = innerCenter.X;
			 yCenter = innerCenter.Y;
			 boundaryDistance = innerContour.pinpol(xCenter, yCenter, snear, i1, i2) * innerPolarity;
	        if (boundaryDistance < 0.0) {
	       	 // point outside polygon
	       	 Preferences.debug("Inner contour geometric center is outside innter contour\n", Preferences.DEBUG_ALGORITHM);
	       	 innerContour.getBounds(xBounds, yBounds, zBounds);
	       	 largestBoundaryDistance = 0.0;
	       	 for (y = yBounds[0]; y <= yBounds[1]; y++) {
	       	     for (x = xBounds[0]; x <= xBounds[1]; x++) {
	       	    	 boundaryDistance = innerContour.pinpol(x, y, snear, i1, i2) * innerPolarity;
	       	    	 if (boundaryDistance > largestBoundaryDistance) {
	       	    		 largestBoundaryDistance = boundaryDistance;
	       	    		 xCenter = x;
	       	    		 yCenter = y;
	       	    	 }
	       	     }
	       	 }
	       	 Preferences.debug("largestBoundaryDistance = " + largestBoundaryDistance + "\n", Preferences.DEBUG_ALGORITHM);
	        } // if (boundaryDistance < 0.0)
			 
			 largestDistance = Math.sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1));
			 
			 for (i = 0; i < contourPoints; i++) {
			     theta = 2.0 * i * Math.PI/contourPoints;
			     costheta[i] = Math.cos(theta);
			     sintheta[i] = Math.sin(theta);
			     minDistance = 0.0;
			     maxDistance = largestDistance;
			     distance = largestDistance/2.0;
			     boundaryDistance = Double.MAX_VALUE;
			     halvingMethod = true;
			     while (Math.abs(boundaryDistance) > 1.0E-1) {
			    	 if (halvingMethod) {
			             distance = (minDistance + maxDistance)/2.0;
			    	 }
			    	 else {
			    		 distance = distance - 0.1;
			    		 if (distance <= 0.0) {
			    			 setCompleted(false);
			    			 return;
			    		 }
			    	 }
			         if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) {
			             halvingMethod = false;
			             distance = largestDistance;
			             if (costheta[i] > 0) {
			                 distance = Math.min(distance, (xDim - 1 - xCenter)/costheta[i]);	 
			             }
			             if (costheta[i] < 0) {
			            	 distance = Math.min(distance, -xCenter/costheta[i]);
			             }
			             if (sintheta[i] > 0) {
			                 distance = Math.min(distance, (yDim - 1 - yCenter)/sintheta[i]);	 
			             }
			             if (sintheta[i] < 0) {
			            	 distance = Math.min(distance, -yCenter/sintheta[i]);
			             }
			             Preferences.debug("For i = " + i + " for inner contour leaving halving method setting distance = " + distance + "\n",
			            		 Preferences.DEBUG_ALGORITHM);
			         } //  if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) 
			         delX = costheta[i]*distance;
			         delY = sintheta[i]*distance;
			         innerX = xCenter + delX;
			         innerY = yCenter + delY;
			         boundaryDistance = innerContour.pinpol(innerX, innerY, snear, i1, i2) * innerPolarity;
			         if (halvingMethod) {
				         if (boundaryDistance < 0.0) {
				        	 // point outside polygon
				        	 maxDistance = distance;
				         }
				         else if (boundaryDistance > 0.0) {
				        	 // point inside polygon
				        	 minDistance = distance;
				         }
			         } // if (halvingMethod)
			     } // while (Math.abs(boundaryDistance) > 1.0E-1)
			     xArray[i][0] = innerX;
			     yArray[i][0] = innerY;
			     minDistance = distance;
			     maxDistance = largestDistance;
			     boundaryDistance = Double.MAX_VALUE;
			     halvingMethod = true;
			     while (Math.abs(boundaryDistance) > 1.0E-1) {
			    	 if (halvingMethod) {
			             distance = (minDistance + maxDistance)/2.0;
			    	 }
			    	 else {
			    		 distance = distance - 0.1;
			    		 if (distance <= 0.0) {
			    			 setCompleted(false);
			    			 return;
			    		 }
			    	 }
			         if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) {
			             halvingMethod = false;
			             distance = largestDistance;
			             if (costheta[i] > 0) {
			                 distance = Math.min(distance, (xDim - 1 - xCenter)/costheta[i]);	 
			             }
			             if (costheta[i] < 0) {
			            	 distance = Math.min(distance, -xCenter/costheta[i]);
			             }
			             if (sintheta[i] > 0) {
			                 distance = Math.min(distance, (yDim - 1 - yCenter)/sintheta[i]);	 
			             }
			             if (sintheta[i] < 0) {
			            	 distance = Math.min(distance, -yCenter/sintheta[i]);
			             }
			             Preferences.debug("For i = " + i + " for outer contour leaving halving method setting distance = " + distance + "\n",
			            		 Preferences.DEBUG_ALGORITHM);
			         } //  if (halvingMethod && ((maxDistance - minDistance) < 1.0E-6)) 
			         delX = costheta[i]*distance;
			         delY = sintheta[i]*distance;
			         outerX = xCenter + delX;
			         outerY = yCenter + delY;
			         boundaryDistance = outerContour.pinpol(outerX, outerY, snear, i1, i2) * outerPolarity;
			         if (halvingMethod) {
				         if (boundaryDistance < 0.0) {
				        	 // point outside polygon
				        	 maxDistance = distance;
				         }
				         else if (boundaryDistance > 0.0) {
				        	 // point inside polygon
				        	 minDistance = distance;
				         }
			         } // if (halvingMethod)
			     } // while (Math.abs(boundaryDistance) > 1.0E-1)
			     xArray[i][linePoints-1] = outerX;
			     yArray[i][linePoints-1] = outerY;
			     delX = (outerX - innerX)/(linePoints-1);
			     delY = (outerY - innerY)/(linePoints-1);
			     for (j = 1; j <= linePoints-2; j++) {
			    	 xArray[i][j] = innerX + j * delX;
			         yArray[i][j] = innerY + j * delY;
			     }
			 } // for (i = 0; i < contourPoints; i++)
			 
			 maxGrad = -Double.MAX_VALUE;
			 minGrad = Double.MAX_VALUE;
			 for (i = 0; i < contourPoints; i++) {
				 for (j = 0; j < linePoints; j++) {
					 interpPt = new Vector2d(xArray[i][j], yArray[i][j]);
					 gx = AlgorithmConvolver.convolve2DPt(interpPt, srcImage.getExtents(), imgBuffer, kExtents, GxData);
	                gy = AlgorithmConvolver.convolve2DPt(interpPt, srcImage.getExtents(), imgBuffer, kExtents, GyData);

	                grad[i][j] = Math.sqrt(gx*gx + gy*gy);
	                if (grad[i][j] < minGrad) {
	               	 minGrad = grad[i][j];
	                }
	                if (grad[i][j] > maxGrad) {
	               	 maxGrad = grad[i][j];
	                }
				 }
			 }
			 
			 for (i = 0; i < contourPoints; i++) {
				 for (j = 0; j < linePoints; j++) {
				     Eimage[i][j] = -(grad[i][j] - minGrad)/(maxGrad - minGrad);
				 }
			 }
			 
			 
			 for (i = 0; i < linePoints; i++) {
				 statePreceding[0][i] = linePoints/2;
			 }
			 
			 for (i = 1; i <= contourPoints-2; i++) {
				 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				     minState = Double.MAX_VALUE;
				     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
				    	 numX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
				    			 xArray[i+1][nextLinePoint];
				    	 numY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
				    			 yArray[i+1][nextLinePoint];
			    		 num = (numX*numX) + (numY*numY);
			    		 denomX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - xArray[i+1][nextLinePoint];
			    		 denomY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - yArray[i+1][nextLinePoint];
			    		 denom = (denomX*denomX) + (denomY*denomY);
			    		 Eint = num/denom;
			    		 stateEnergyTemp = stateEnergy[i-1][presentLinePoint]
			    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
			    		 if (stateEnergyTemp < minState) {
			    	    	 minState = stateEnergyTemp;
			    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
			    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
			    	     }
				     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
				 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
			 } // for (i = 1; i <= contourPoints-2; i++)
			 
			 minState = Double.MAX_VALUE;
			 terminatingLinePoint = -1;
			 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				 if (stateEnergy[contourPoints-2][nextLinePoint] < minState) {
				     minState = stateEnergy[contourPoints-2][nextLinePoint];
				     terminatingLinePoint = nextLinePoint;
				 }
			 }
			 
			 
			 nextLinePoint = terminatingLinePoint;
			 startingLinePoint = -1;
			 for (i = contourPoints-2; i >= contourPoints/2 -1; i--) {
			     presentLinePoint = statePreceding[i][nextLinePoint];
			     if (i == contourPoints/2) {
			         startingLinePoint = presentLinePoint;	 
			     }
			     else if (i == contourPoints/2 - 1) {
			    	 terminatingLinePoint = presentLinePoint;
			     }
			     nextLinePoint = presentLinePoint;
			 } // for (i = contourPoints-2; i >= contourPoints/2 -1; i--)
			 
			 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				 statePreceding[contourPoints/2][nextLinePoint] = startingLinePoint;
			 }
			 
			 for (i = 0; i < contourPoints; i++) {
				 for (j = 0; j < linePoints; j++) {
					 stateEnergy[i][j] = 0.0;
				 }
			 }
			 
			 for (i = contourPoints/2+1; i <= contourPoints-1; i++) {
				 if (i == contourPoints - 1) {
					 ip1 = 0;
				 }
				 else {
					 ip1 = i + 1;
				 }
				 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
				     minState = Double.MAX_VALUE;
				     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
				    	 numX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
				    			 xArray[ip1][nextLinePoint];
				    	 numY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
				    			 yArray[ip1][nextLinePoint];
			    		 num = (numX*numX) + (numY*numY);
			    		 denomX = xArray[i-1][statePreceding[i-1][presentLinePoint]] - xArray[ip1][nextLinePoint];
			    		 denomY = yArray[i-1][statePreceding[i-1][presentLinePoint]] - yArray[ip1][nextLinePoint];
			    		 denom = (denomX*denomX) + (denomY*denomY);
			    		 Eint = num/denom;
			    		 stateEnergyTemp = stateEnergy[i-1][presentLinePoint]
			    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
			    		 if (stateEnergyTemp < minState) {
			    	    	 minState = stateEnergyTemp;
			    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
			    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
			    	     }
				     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
				 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
			 } // for (i = contourPoints/2+1; i <= contourPoints-1; i++)
			 
			 for (i = 0; i < contourPoints/2-1; i++) {
				 if (i == 0) {
					 im1 = contourPoints-1;
				 }
				 else {
					 im1 = i - 1;
				 }
				 if (i < contourPoints/2 - 2) {
					 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
					     minState = Double.MAX_VALUE;
					     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
					    	 numX = xArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
					    			 xArray[i+1][nextLinePoint];
					    	 numY = yArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
					    			 yArray[i+1][nextLinePoint];
				    		 num = (numX*numX) + (numY*numY);
				    		 denomX = xArray[im1][statePreceding[im1][presentLinePoint]] - xArray[i+1][nextLinePoint];
				    		 denomY = yArray[im1][statePreceding[im1][presentLinePoint]] - yArray[i+1][nextLinePoint];
				    		 denom = (denomX*denomX) + (denomY*denomY);
				    		 Eint = num/denom;
				    		 stateEnergyTemp = stateEnergy[im1][presentLinePoint]
				    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
				    		 if (stateEnergyTemp < minState) {
				    	    	 minState = stateEnergyTemp;
				    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
				    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
				    	     }
					     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
					 } // for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++)
				 } // if (i < contourPoints/2 - 2)
				 else {
					 nextLinePoint = terminatingLinePoint;
					 minState = Double.MAX_VALUE;
				     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
				    	 numX = xArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*xArray[i][presentLinePoint] +
				    			 xArray[i+1][nextLinePoint];
				    	 numY = yArray[im1][statePreceding[im1][presentLinePoint]] - 2.0*yArray[i][presentLinePoint] + 
				    			 yArray[i+1][nextLinePoint];
			    		 num = (numX*numX) + (numY*numY);
			    		 denomX = xArray[im1][statePreceding[im1][presentLinePoint]] - xArray[i+1][nextLinePoint];
			    		 denomY = yArray[im1][statePreceding[im1][presentLinePoint]] - yArray[i+1][nextLinePoint];
			    		 denom = (denomX*denomX) + (denomY*denomY);
			    		 Eint = num/denom;
			    		 stateEnergyTemp = stateEnergy[im1][presentLinePoint]
			    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
			    		 if (stateEnergyTemp < minState) {
			    	    	 minState = stateEnergyTemp;
			    	    	 statePreceding[i][nextLinePoint] = presentLinePoint;
			    	    	 stateEnergy[i][nextLinePoint] = stateEnergyTemp;
			    	     }
				     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
				 }
			 } // for (i = 0; i < contourPoints/2-1; i++)
			 
			 bestLinePoint[contourPoints/2] = startingLinePoint;
			 bestLinePoint[contourPoints/2 - 1] = terminatingLinePoint;
			 nextLinePoint = terminatingLinePoint;
			 for (i = contourPoints/2 - 2; i >= 0; i--) {
			     presentLinePoint = statePreceding[i][nextLinePoint];
			     bestLinePoint[i] = presentLinePoint;
			     nextLinePoint = presentLinePoint;
			 }
			 
			 for (i = contourPoints-1; i >= contourPoints/2+1; i--) {
				 presentLinePoint = statePreceding[i][nextLinePoint];
			     bestLinePoint[i] = presentLinePoint;
			     nextLinePoint = presentLinePoint;	 
			 }
			  
			 innerContour.removeAllElements();
			 outerContour.removeAllElements();
			 for (i = 0; i < contourPoints; i++) {
				 resultPt[i] = new Vector3f((float)xArray[i][bestLinePoint[i]], (float)yArray[i][bestLinePoint[i]], (float)z);
				 if (z < zDim-1) {
					 innerContour.add(new Vector3f((float)(xArray[i][bestLinePoint[i]] - pixelsContract * costheta[i]),
							 (float)(yArray[i][bestLinePoint[i]] - pixelsContract * sintheta[i]), (float)(z+1)));
					 outerContour.add(new Vector3f((float)(xArray[i][bestLinePoint[i]] + pixelsExpand * costheta[i]),
							 (float)(yArray[i][bestLinePoint[i]] + pixelsExpand * sintheta[i]), (float)(z+1)));
				 }
			 }
			 resultVOI.importCurve(resultPt);
			 VOIs.add(resultVOI);	 
		 } // for (z = startingZ + 1; z < zDim; z++)
		 srcImage.setVOIs(VOIs);
        srcImage.notifyImageDisplayListeners();
		
		setCompleted(true);
		return;
	}
	
	/**
     * Makes derivative kernels to be used in the calculation of the gradient magnitude.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 1;
        derivOrder[1] = 0;

        xkDim = (int)Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = (int)Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        GxData = new double[xkDim * ykDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);

        Gx.dcalc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        GyData = new double[xkDim * ykDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);

        Gy.dcalc(true);
    }
    
    private void generateTestImage() {
    	int extents[] = new int[2];
    	extents[0] = 1200;
    	extents[1] = 1200;
    	double xcen = 599.5;
    	double ycen = 599.5;
    	int x;
    	int y;
    	double theta;
    	int i;
    	double radius = 400;
    	double a = 0.2 * radius;
    	int n = 30;
    	byte buffer[] = new byte[extents[0]*extents[1]];
    	boolean found;
    	double pixelDistX;
    	double pixelDistY;
    	double pixelDistSquared;
    	int xboundary;
    	int yboundary;
    	double boundaryDistX;
    	double boundaryDistY;
    	double boundaryDistSquared;
    	
    	for (i = 0; i < 360000; i++) {
    		theta = i * 2.0 * Math.PI/360000.0;
    	    x = (int)Math.round((radius + a * Math.sin(n*theta))*Math.cos(theta) + xcen);
    	    y = (int)Math.round((radius + a * Math.sin(n*theta))*Math.sin(theta) + ycen);
    	    buffer[x + y * extents[0]] = 1;
    	}
    	
    	for (y = 0; y < extents[1]; y++) {
    		for (x = 0; x < extents[0]; x++) {
    		    pixelDistX = x - xcen;
    		    pixelDistY = y - ycen;
    		    pixelDistSquared = pixelDistX * pixelDistX + pixelDistY * pixelDistY;
    		    theta = Math.atan2(pixelDistY, pixelDistX);
    		    xboundary = (int)Math.round((radius + a * Math.sin(n*theta))*Math.cos(theta) + xcen);
        	    yboundary = (int)Math.round((radius + a * Math.sin(n*theta))*Math.sin(theta) + ycen);
        	    boundaryDistX = xboundary - xcen;
        	    boundaryDistY = yboundary - ycen;
        	    boundaryDistSquared = boundaryDistX * boundaryDistX + boundaryDistY * boundaryDistY;
        	    if (pixelDistSquared <= boundaryDistSquared) {
        	    	buffer[x + y * extents[0]] = 1;
        	    }
    		}
    	}
    	
    	ModelImage regularizationTestImage = new ModelImage(ModelStorageBase.BYTE, extents, "regularizationTestImage");
    	try {
    	    regularizationTestImage.importData(0, buffer, true);	
    	}
    	catch(IOException e) {
    		
    	}
    	new ViewJFrameImage(regularizationTestImage);
    }
}