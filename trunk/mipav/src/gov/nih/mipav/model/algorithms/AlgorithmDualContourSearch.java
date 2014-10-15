package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2f;
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
    private float[] GxData;

    /** Storage location of the first derivative of the Gaussian in the Y direction. */
    private float[] GyData;
    
    /** Dimensionality of the kernel. */
    private int[] kExtents;
    
    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;
	
	
	public  AlgorithmDualContourSearch(ModelImage srcImg, int innerIndex, int outerIndex, int contourPoints, int linePoints,
			                           double regularization, float[] sigmas) {
		
		super(null, srcImg);
		this.innerIndex = innerIndex;
		this.outerIndex = outerIndex;
		this.contourPoints = contourPoints;
		this.linePoints = linePoints;
		this.regularization = regularization;
		this.sigmas = sigmas;
	}
	
	/**
     * Accessor that returns the resultant VOI.
     *
     * @return  resultant VOI that has localized to the boundaries of the object
     */
    public VOI getResultVOI() {
        return resultVOI;
    }
	
	public void runAlgorithm() {
		 VOIVector VOIs;
		 VOI innerVOI;
		 VOI outerVOI;
		 VOIContour innerContour;
		 VOIContour outerContour;
		 Vector3f innerCenter;
		 float xCenter;
		 float yCenter;
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
		 float[] imgBuffer;
		 float gx;
		 float gy;
		 Vector2f interpPt;
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
		 boolean problem;
		 double stateEnergyTemp;
		 int startingLinePoint;
		 int terminatingLinePoint;
		 
		 xDim = srcImage.getExtents()[0];
		 yDim = srcImage.getExtents()[1];
		 
		 try {
	            length = xDim * yDim;
	            imgBuffer = new float[length];
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
		 outerContour = (VOIContour)outerVOI.getCurves().elementAt(0);
		 innerCenter = innerContour.getGeometricCenter();
		 xCenter = innerCenter.X;
		 yCenter = innerCenter.Y;
		 boundaryDistance = innerContour.pinpol(xCenter, yCenter, snear, i1, i2);
         if (boundaryDistance < 0.0) {
        	 // point outside polygon
        	 Preferences.debug("Inner contour geometric center is outside innter contour\n", Preferences.DEBUG_ALGORITHM);
        	 if (!snear[0]) {
        		 // Nearer to vertex i1[0] than to a line segment
        		 xCenter = innerContour.elementAt(i1[0]).X;
        		 yCenter = innerContour.elementAt(i1[0]).Y;
        	 }
        	 else {
        		 xCenter = (innerContour.elementAt(i1[0]).X + innerContour.elementAt(i2[0]).X)/2.0f;
        		 yCenter = (innerContour.elementAt(i1[0]).Y + innerContour.elementAt(i2[0]).Y)/2.0f;	 
        	 }
         }
		 
		 largestDistance = Math.sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1));
		 problem = true;
		 loop: while (problem) {
			 problem = false;
			 for (i = 0; i < contourPoints; i++) {
			     theta = 2.0 * i * Math.PI/contourPoints;
			     costheta = Math.cos(theta);
			     sintheta = Math.sin(theta);
			     minDistance = 0.0;
			     maxDistance = largestDistance;
			     boundaryDistance = Double.MAX_VALUE;
			     while (Math.abs(boundaryDistance) > 1.0E-1) {
			         distance = (minDistance + maxDistance)/2.0;
			         if (((largestDistance - minDistance) < 1.0E-6) && (snear[0] == false)) {
			             // Have a branch point on outside of contour that must be deleted
			        	 innerContour.removeElementAt(i1[0]);
			        	 Preferences.debug("Deleting inner contour outside branch with index " + i1[0] + "\n", 
			        			 Preferences.DEBUG_ALGORITHM );
			        	 innerCenter = innerContour.getGeometricCenter();
			        	 xCenter = innerCenter.X;
			    		 yCenter = innerCenter.Y;
			    		 problem = true;
			    		 continue loop;
			         } // if (((largestDistance - minDistance) < 1.0E-6) && (snear[0] == false)) 
			         delX = costheta*distance;
			         delY = sintheta*distance;
			         innerX = xCenter + delX;
			         innerY = yCenter + delY;
			         boundaryDistance = innerContour.pinpol(innerX, innerY, snear, i1, i2);
			         if (boundaryDistance < 0.0) {
			        	 // point outside polygon
			        	 maxDistance = distance;
			         }
			         else if (boundaryDistance > 0.0) {
			        	 // point inside polygon
			        	 minDistance = distance;
			         }
			     } // while (Math.abs(boundaryDistance) > 1.0E-1)
			     xArray[i][0] = innerX;
			     yArray[i][0] = innerY;
			     maxDistance = largestDistance;
			     boundaryDistance = Double.MAX_VALUE;
			     while (Math.abs(boundaryDistance) > 1.0E-1) {
			         distance = (minDistance + maxDistance)/2.0;
			         if (((largestDistance - minDistance) < 1.0E-6) && (snear[0] == false)) {
			             // Have a branch point on outside of contour that must be deleted
			        	 outerContour.removeElementAt(i1[0]);
			        	 Preferences.debug("Deleting outer contour outside branch with index " + i1[0] + "\n", 
			        			 Preferences.DEBUG_ALGORITHM );
			    		 problem = true;
			    		 continue loop;
			         } // if (((largestDistance - minDistance) < 1.0E-6) && (snear[0] == false)) 
			         delX = costheta*distance;
			         delY = sintheta*distance;
			         outerX = xCenter + delX;
			         outerY = yCenter + delY;
			         boundaryDistance = outerContour.pinpol(outerX, outerY, snear, i1, i2);
			         if (boundaryDistance < 0.0) {
			        	 // point outside polygon
			        	 maxDistance = distance;
			         }
			         else if (boundaryDistance > 0.0) {
			        	 // point inside polygon
			        	 minDistance = distance;
			         }
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
		 } // loop: while (problem)
		 
		 maxGrad = -Double.MAX_VALUE;
		 minGrad = Double.MAX_VALUE;
		 for (i = 0; i < contourPoints; i++) {
			 for (j = 0; j < linePoints; j++) {
				 interpPt = new Vector2f((float)xArray[i][j], (float)yArray[i][j]);
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
		    	 terminatingLinePoint = nextLinePoint;
		     }
		     nextLinePoint = presentLinePoint;
		 } // for (i = contourPoints-2; i >= contourPoints/2 -1; i--)
		 
		 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
			 statePreceding[contourPoints/2][nextLinePoint] = startingLinePoint;
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
	
	/**
     * Makes derivative kernels to be used in the calculation of the gradient magnitude.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 1;
        derivOrder[1] = 0;

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        kExtents[1] = ykDim;

        GxData = new float[xkDim * ykDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);

        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        GyData = new float[xkDim * ykDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);

        Gy.calc(true);
    }
}