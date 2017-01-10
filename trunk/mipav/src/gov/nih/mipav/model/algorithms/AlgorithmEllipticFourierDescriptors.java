package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.*;

import java.util.*;

/** Smoothing VOI with Elliptic Fourier Descriptors.  The number of coefficients can range from 1 to the number of 
 * points in the curve divided by 2.
 * References:
 * 1.) Feature Extraction & Image Processing for Computer Vision, Third Edition, Mark S. Nixon and
 * Alberto S. Aguado, 2012, pp. 369-378.
 * 2.) Boundary Finding with Parametrically Deformable Models by Lawrence H. Staib and James S. Duncan.
 * @author ilb
 *
 */

public class AlgorithmEllipticFourierDescriptors extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------

    /** The voi selected by the user. */
    private VOI activeVOI;

    /** Number of coefficients. */
    private int coefficients;

    /** The resultant polygon and the evolution has completed. */
    private VOI resultVOI;

    /** Source image. */
    private ModelImage srcImage;
    
//~ Constructors ---------------------------------------------------------------------------------------------------
    
    public AlgorithmEllipticFourierDescriptors() {
       
    }

    /**
     * Creates a new AlgorithmEllipticFourierDescriptors object.
     *
     * @param  srcImg     2D or 3D source image
     * @param  activeVOI  the selected voi
     * @param  coefficients       number of coefficients
     */
    public AlgorithmEllipticFourierDescriptors(ModelImage srcImg, VOI activeVOI, int coefficients) {

        srcImage = srcImg;
        this.activeVOI = activeVOI;
        this.coefficients = coefficients;
    }
    
    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Accessor that returns a voi that is a smoothed version of the original.
     *
     * @return  resultVOI
     */
    public VOI getResultVOI() {
        return resultVOI;
    }
    
    /**
     * Starts the smooth algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        } else {
        	calc();
        }
    }
    
    /**
     * Prepares the data and runs the algorithm for a 2D or 3D image.
     */
    private void calc() {
    	boolean isPoint = false;
    	boolean isPolyLine = false;                    
        fireProgressStateChanged(srcImage.getImageName(), "Elliptic Fourier Descriptors smooth: Evolving boundary ...");

        fireProgressStateChanged(25);
        if(activeVOI.getCurveType() == VOI.POINT) {
        	resultVOI = new VOI((short) srcImage.getVOIs().size(), "Esmooth-VOI", VOI.POINT, -1.0f);
        	isPoint = true;
        } else if (activeVOI.getCurveType() == VOI.POLYLINE) {
        	resultVOI = new VOI((short) srcImage.getVOIs().size(), "Esmooth-VOI", VOI.POLYLINE, -1.0f);
        	isPolyLine = true;
        }else {
        	resultVOI = new VOI((short) srcImage.getVOIs().size(), "Esmooth-VOI", VOI.CONTOUR, -1.0f);
        }
        
        Vector<VOIBase> contours = activeVOI.getCurves();
        int nContours = contours.size();
        
        if(isPoint) {
        	
        	 int nPoints = contours.size();
             
             System.out.println("** " + nPoints);

             float[] xPoints = new float[nPoints];
             float [] yPoints = new float[nPoints];
             float[] xSmooth = new float[nPoints];
             float[] ySmooth = new float[nPoints];
             float zPoint = ((VOIPoint)contours.get(0)).exportPoint().Z;
         	 Vector3f point;
         	for (int i = 0; i < nPoints; i++) {
            	point = ((VOIPoint)contours.get(i)).exportPoint();
                xPoints[i] = point.X;
                yPoints[i] = point.Y;
            }
            runEllipticFourierDescriptors(xPoints, yPoints, xSmooth, ySmooth);
            for (int i = 0; i < nPoints; i++) {
            	
            	point = new Vector3f(xSmooth[i], ySmooth[i], zPoint);
            	VOIPoint resultPoint = new VOIPoint(VOI.POINT,point);
            	resultVOI.importCurve(resultPoint);
            } 
             
             
        } else if (isPolyLine) {
        	for (int elementNum = 0; elementNum < nContours; elementNum++) {
	            fireProgressStateChanged((int) (25 + (75 * (((float) elementNum) / nContours))));
	
	            if (((VOIContour) (contours.elementAt(elementNum))).isActive()) {
	                int nPoints = contours.elementAt(elementNum).size();
	                //System.err.println("Element number is: " + elementNum);
	                    float[] xPoints = new float[nPoints];
	                    float[] yPoints = new float[nPoints];
	                    float[] xSmooth = new float[nPoints];
	                    float[] ySmooth = new float[nPoints];
	              
	                    float zPoint = contours.elementAt(elementNum).elementAt(0).Z;
	                    for (int i = 0; i < nPoints; i++) {
	                        xPoints[i] = contours.elementAt(elementNum).elementAt(i).X;
	                        yPoints[i] = contours.elementAt(elementNum).elementAt(i).Y;
	                    }
	            
	                    VOIContour resultContour = new VOIContour( false, contours.elementAt(elementNum).isClosed());
	                    runEllipticFourierDescriptorsOpenCurve(xPoints, yPoints, xSmooth, ySmooth);
	                    for (int i = 0; i < nPoints; i++) {
	                        resultContour.add(new Vector3f(xSmooth[i], ySmooth[i], zPoint));
	                    }
	                    fireProgressStateChanged(25 + (((75 * elementNum) + 50) / nContours));
	
	                    
	                    resultVOI.importCurve(resultContour);
	
	                    if (threadStopped) {
	                        finalize();
	
	                        return;
	                    }
	                
	            } // if ( ((VOIContour)(contours[0].elementAt(elementNum))).isActive() )
	        } // for(elementNum = 0; elementNum < nContours; elementNum++)	
        }else {
	        for (int elementNum = 0; elementNum < nContours; elementNum++) {
	            fireProgressStateChanged((int) (25 + (75 * (((float) elementNum) / nContours))));
	
	            if (((VOIContour) (contours.elementAt(elementNum))).isActive()) {
	                int nPoints = contours.elementAt(elementNum).size();
	                //System.err.println("Element number is: " + elementNum);
	                    float[] xPoints = new float[nPoints];
	                    float[] yPoints = new float[nPoints];
	                    float[] xSmooth = new float[nPoints];
	                    float[] ySmooth = new float[nPoints];
	              
	                    float zPoint = contours.elementAt(elementNum).elementAt(0).Z;
	                    for (int i = 0; i < nPoints; i++) {
	                        xPoints[i] = contours.elementAt(elementNum).elementAt(i).X;
	                        yPoints[i] = contours.elementAt(elementNum).elementAt(i).Y;
	                    }
	            
	                    VOIContour resultContour = new VOIContour( false, contours.elementAt(elementNum).isClosed());
	                    runEllipticFourierDescriptors(xPoints, yPoints, xSmooth, ySmooth);
	                    for (int i = 0; i < nPoints; i++) {
	                        resultContour.add(new Vector3f(xSmooth[i], ySmooth[i], zPoint));
	                    }
	                    fireProgressStateChanged(25 + (((75 * elementNum) + 50) / nContours));
	
	                    
	                    resultVOI.importCurve(resultContour);
	
	                    if (threadStopped) {
	                        finalize();
	
	                        return;
	                    }
	                
	            } // if ( ((VOIContour)(contours[0].elementAt(elementNum))).isActive() )
	        } // for(elementNum = 0; elementNum < nContours; elementNum++)
        }

        fireProgressStateChanged(100);
        
        setCompleted(true);
    }
    
    private void runEllipticFourierDescriptors(float xPoints[], float yPoints[], float xSmooth[], float ySmooth[]) {
    	int i, k;
    	double prod;
    	double cosProd;
    	double sinProd;
    	int m = xPoints.length;
    	double scale = 2.0/m;
    	double xCurv;
    	double yCurv;
    	int localCoefficients = Math.min(coefficients, m/2);
    	// Fourier coefficients
    	double ax[] = new double[localCoefficients+1];
    	double bx[] = new double[localCoefficients+1];
    	double ay[] = new double[localCoefficients+1];
    	double by[] = new double[localCoefficients+1];
    	
    	double xSum = 0;
    	double ySum = 0;
    	for (i = 0; i < m; i++) {
    		xSum = xSum + xPoints[i];
    		ySum = ySum + yPoints[i];
    	}
    	double xAverage = xSum/m;
    	double yAverage = ySum/m;
    	ax[0] = 2.0 * xAverage;
    	ay[0] = 2.0 * yAverage;
    	
    	
    	double t = 2.0 * Math.PI/m;
    	
    	for (k = 1; k <= localCoefficients; k++) {
    		for (i = 1; i <= m; i++) {
    			prod = k * t * (i-1);
    			cosProd = Math.cos(prod);
    			sinProd = Math.sin(prod);
    			ax[k] = ax[k] + xPoints[i-1] * cosProd;
    			bx[k] = bx[k] + xPoints[i-1] * sinProd;
    			ay[k] = ay[k] + yPoints[i-1] * cosProd;
    			by[k] = by[k] + yPoints[i-1] * sinProd;
    		}
    		ax[k] = ax[k] * scale;
    		bx[k] = bx[k] * scale;
    		ay[k] = ay[k] * scale;
    		by[k] = by[k] * scale;
    	}
    	
        double omega = 2.0 * Math.PI/m;
    	for (i = 0; i < m; i++) {
        	xCurv = ax[0]/2.0;
        	yCurv = ay[0]/2.0;
        	for (k = 1; k <= localCoefficients; k++) {
        		prod = k * i * omega;
        		cosProd = Math.cos(prod);
        		sinProd = Math.sin(prod);
        		xCurv = xCurv + ax[k]*cosProd + bx[k]*sinProd;
        		yCurv = yCurv + ay[k]*cosProd + by[k]*sinProd;
        	}
        	xSmooth[i] = (float)xCurv;
        	ySmooth[i] = (float)yCurv;
        }
    }
    
    private void runEllipticFourierDescriptorsOpenCurve(float xPoints[], float yPoints[], float xSmooth[], float ySmooth[]) {
    	int i, k;
    	double prod;
    	double cosProd;
    	int m = xPoints.length;
    	double scale = 1.0/m;
    	double xCurv;
    	double yCurv;
    	int localCoefficients = Math.min(coefficients, m/2);
    	// Fourier coefficients
    	double ax[] = new double[localCoefficients+1];
    	double ay[] = new double[localCoefficients+1];
    	
    	double xSum = 0;
    	double ySum = 0;
    	for (i = 0; i < m; i++) {
    		xSum = xSum + xPoints[i];
    		ySum = ySum + yPoints[i];
    	}
    	double xAverage = xSum/m;
    	double yAverage = ySum/m;
    	ax[0] = 2.0 * xAverage;
    	ay[0] = 2.0 * yAverage;
    	
    	
    	double t = Math.PI/m;
    	
    	for (k = 1; k <= localCoefficients; k++) {
    		for (i = 1; i <= m; i++) {
    			prod = k * t * (i-1);
    			cosProd = Math.cos(prod);
    			ax[k] = ax[k] + xPoints[i-1] * cosProd;
    			ay[k] = ay[k] + yPoints[i-1] * cosProd;
    		}
    		for (i = m+1; i <= 2*m; i++) {
    			prod = k * t * (i-1);
    			cosProd = Math.cos(prod);
    			ax[k] = ax[k] + xPoints[2*m-i] * cosProd;
    			ay[k] = ay[k] + yPoints[2*m-i] * cosProd;
    		}
    		ax[k] = ax[k] * scale;
    		ay[k] = ay[k] * scale;
    	}
    	
        double omega = 2.0 * Math.PI/m;
    	for (i = 0; i < m; i++) {
        	xCurv = ax[0]/2.0;
        	yCurv = ay[0]/2.0;
        	for (k = 1; k <= localCoefficients; k++) {
        		prod = k * i * omega;
        		cosProd = Math.cos(prod);
        		xCurv = xCurv + ax[k]*cosProd;
        		yCurv = yCurv + ay[k]*cosProd;
        	}
        	xSmooth[i] = (float)xCurv;
        	ySmooth[i] = (float)yCurv;
        }
    }
    
    

}