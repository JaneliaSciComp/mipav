package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Vector;

public class AlgorithmDualContourSearch extends AlgorithmBase {
	
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
		 int k;
		 int length;
		 float[] imgBuffer;
		 float gx;
		 float gy;
		 Vector2f interpPt;
		 int bestLinePoint[] = new int[contourPoints];
		 double lastState[][] = new double[linePoints][linePoints];
		 double state[][] = new double[linePoints][linePoints];
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
		 Vector<Vector3f>resultCurve = new Vector();
		 
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
		 
		 largestDistance = Math.sqrt((xDim-1)*(xDim-1) + (yDim-1)*(yDim-1));
		 for (i = 0; i < contourPoints; i++) {
		     theta = 2.0 * i * Math.PI/contourPoints;
		     costheta = Math.cos(theta);
		     sintheta = Math.sin(theta);
		     minDistance = 0.0;
		     maxDistance = largestDistance;
		     boundaryDistance = Double.MAX_VALUE;
		     while (Math.abs(boundaryDistance) > 1.0E-1) {
		         distance = (minDistance + maxDistance)/2.0;
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
		 
		 bestLinePoint[0] = linePoints/2;
		 for (i = 1; i <= contourPoints-2; i++) {
			 minState = Double.MAX_VALUE;
		     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
		    	 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
		    		 numX = xArray[i-1][bestLinePoint[i-1]] - 2.0*xArray[i][presentLinePoint] + xArray[i+1][nextLinePoint];
		    		 numY = yArray[i-1][bestLinePoint[i-1]] - 2.0*yArray[i][presentLinePoint] + yArray[i+1][nextLinePoint];
		    		 num = (numX*numX) + (numY*numY);
		    		 denomX = xArray[i-1][bestLinePoint[i-1]] - xArray[i+1][nextLinePoint];
		    		 denomY = yArray[i-1][bestLinePoint[i-1]] - yArray[i+1][nextLinePoint];
		    		 denom = (denomX*denomX) + (denomY*denomY);
		    		 Eint = num/denom;
		    	     state[nextLinePoint][presentLinePoint] = lastState[presentLinePoint][bestLinePoint[i-1]]
		    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
		    	     if (state[nextLinePoint][presentLinePoint] < minState) {
		    	    	 minState = state[nextLinePoint][presentLinePoint];
		    	    	 bestLinePoint[i] = presentLinePoint;
		    	     }
		    	 }
		     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
		     for (j = 0; j < linePoints; j++) {
		    	 for (k = 0; k < linePoints; k++) {
		    		 lastState[j][k] = state[j][k];
		    	 }
		     }
		 } // for (i = 1; i <= contourPoints-2; i++)
		 
		 
		 for (j = 0; j < linePoints; j++) {
	    	 for (k = 0; k < linePoints; k++) {
	    		 lastState[j][k] = 0;
	    	 }
	     }
		 for (i = contourPoints/2+1; i <= contourPoints-1; i++) {
			 if (i == contourPoints - 1) {
				 ip1 = 0;
			 }
			 else {
				 ip1 = i + 1;
			 }
			 minState = Double.MAX_VALUE;
		     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
		    	 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
		    		 numX = xArray[i-1][bestLinePoint[i-1]] - 2.0*xArray[i][presentLinePoint] + xArray[ip1][nextLinePoint];
		    		 numY = yArray[i-1][bestLinePoint[i-1]] - 2.0*yArray[i][presentLinePoint] + yArray[ip1][nextLinePoint];
		    		 num = (numX*numX) + (numY*numY);
		    		 denomX = xArray[i-1][bestLinePoint[i-1]] - xArray[ip1][nextLinePoint];
		    		 denomY = yArray[i-1][bestLinePoint[i-1]] - yArray[ip1][nextLinePoint];
		    		 denom = (denomX*denomX) + (denomY*denomY);
		    		 Eint = num/denom;
		    	     state[nextLinePoint][presentLinePoint] = lastState[presentLinePoint][bestLinePoint[i-1]]
		    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
		    	     if (state[nextLinePoint][presentLinePoint] < minState) {
		    	    	 minState = state[nextLinePoint][presentLinePoint];
		    	    	 bestLinePoint[i] = presentLinePoint;
		    	     }
		    	 }
		     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
		     for (j = 0; j < linePoints; j++) {
		    	 for (k = 0; k < linePoints; k++) {
		    		 lastState[j][k] = state[j][k];
		    	 }
		     }	 
		 } // for (i = contourPoints/2+1; i <= contourPoints-1; i++)
		 
		 for (i = 0; i <= contourPoints/2-1; i++) {
			 if (i == 0) {
				 im1 = contourPoints-1;
			 }
			 else {
				 im1 = i - 1;
			 }
			 minState = Double.MAX_VALUE;
		     for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++) {
		    	 for (nextLinePoint = 0; nextLinePoint < linePoints; nextLinePoint++) {
		    		 numX = xArray[im1][bestLinePoint[im1]] - 2.0*xArray[i][presentLinePoint] + xArray[i+1][nextLinePoint];
		    		 numY = yArray[im1][bestLinePoint[im1]] - 2.0*yArray[i][presentLinePoint] + yArray[i+1][nextLinePoint];
		    		 num = (numX*numX) + (numY*numY);
		    		 denomX = xArray[im1][bestLinePoint[im1]] - xArray[i+1][nextLinePoint];
		    		 denomY = yArray[im1][bestLinePoint[im1]] - yArray[i+1][nextLinePoint];
		    		 denom = (denomX*denomX) + (denomY*denomY);
		    		 Eint = num/denom;
		    	     state[nextLinePoint][presentLinePoint] = lastState[presentLinePoint][bestLinePoint[im1]]
		    	    		 + regularization*Eint + (1.0 - regularization)*Eimage[i][presentLinePoint];
		    	     if (state[nextLinePoint][presentLinePoint] < minState) {
		    	    	 minState = state[nextLinePoint][presentLinePoint];
		    	    	 bestLinePoint[i] = presentLinePoint;
		    	     }
		    	 }
		     } // for (presentLinePoint = 0; presentLinePoint < linePoints; presentLinePoint++)
		     for (j = 0; j < linePoints; j++) {
		    	 for (k = 0; k < linePoints; k++) {
		    		 lastState[j][k] = state[j][k];
		    	 }
		     }	 	 
		 } // for (i = 0; i <= contourPoints/2-1; i++)
		 
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