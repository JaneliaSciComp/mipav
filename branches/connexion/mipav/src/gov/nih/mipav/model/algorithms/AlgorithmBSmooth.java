package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.util.*;


/**
 * Smoothing of VOI using 1 iteration of bSplines. All selected curves in all slices of the voi will be smoothed. The
 * number of interpolated points is user selectable. The user chooses whether or not to trim out nearly collinear
 * points. The user chooses whether or not to remove the original selected curve. If the original curve is not removed,
 * the new curve will have a different color. If the original curve is removed, the new curve will have the same color.
 *
 * @see  AlgorithmBSpline
 */

public class AlgorithmBSmooth extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The voi selected by the user. */
    private VOI activeVOI;

    /** Number of interpolation points. */
    private int nPts;

    /** The resultant polygon and the evolution has completed. */
    private VOI resultVOI;

    /** Source image. */
    private ModelImage srcImage;

    /** Trim out nearly collinear points. */
    private boolean trim;
    
    private AlgorithmBSpline bSpline;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    public AlgorithmBSmooth() {
       
    }

    /**
     * Creates a new AlgorithmBSmooth object.
     *
     * @param  srcImg     2D or 3D source image
     * @param  activeVOI  the selected voi
     * @param  nPts       number of interpolation points
     * @param  trim       trim out nearly collinear points
     */
    public AlgorithmBSmooth(ModelImage srcImg, VOI activeVOI, int nPts, boolean trim) {

        srcImage = srcImg;
        this.activeVOI = activeVOI;
        this.nPts = nPts;
        this.trim = trim;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
    
    public void setNPts(int nPts) {
        this.nPts = nPts;
    }


    /**
     * Starts the smooth algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        } else {

            if (srcImage.getNDims() == 2) {
                calc2D();
            } else if (srcImage.getNDims() > 2) {
                calc3D();
            }
        }

    }

    /**
     * Prepares the data and runs the algorithm for a 2D image.
     */
    private void calc2D() {
    	boolean isPoint = false;
        fireProgressStateChanged(srcImage.getImageName(), "Bspline smooth: Evolving boundary ...");

        fireProgressStateChanged(25);
        if(activeVOI.getCurveType() == VOI.POINT) {
        	resultVOI = new VOI((short) srcImage.getVOIs().size(), "Bsmooth-VOI", VOI.POINT, -1.0f);
        	isPoint = true;
        }else {
        	resultVOI = new VOI((short) srcImage.getVOIs().size(), "Bsmooth-VOI", VOI.CONTOUR, -1.0f);
        }
        
        Vector<VOIBase> contours = activeVOI.getCurves();
        int nContours = contours.size();
        
        if(isPoint) {
        	
        	 int nPoints = contours.size();
             
             System.out.println("** " + nPoints);

             float[] xPoints = new float[nPoints + 5];
             float[] yPoints = new float[nPoints + 5];
             float[] zPoints = new float[nPoints + 5];
             setPoints2(xPoints, yPoints, zPoints);
            
             runSmooth2(xPoints, yPoints, zPoints);
             
             //resultVOI.importCurve(resultContour);
             
             
   
        }else {
	        for (int elementNum = 0; elementNum < nContours; elementNum++) {
	            fireProgressStateChanged((int) (25 + (75 * (((float) elementNum) / nContours))));
	
	            if (((VOIContour) (contours.elementAt(elementNum))).isActive()) {
	                int nPoints = contours.elementAt(elementNum).size();
	                //System.err.println("Element number is: " + elementNum);
	                if (nPoints > 5) {
	                    float[] xPoints = new float[nPoints + 5];
	                    float[] yPoints = new float[nPoints + 5];
	                    float[] zPoints = new float[nPoints + 5];
	                    fireProgressStateChanged(25 + (((75 * elementNum) + 5) / nContours));
	                    setPoints(xPoints, yPoints, zPoints, contours.elementAt(elementNum));
	                    fireProgressStateChanged(25 + (((75 * elementNum) + 25) / nContours));
	                    VOIContour resultContour = new VOIContour( false, contours.elementAt(elementNum).isClosed());
	                    runSmooth(xPoints, yPoints, zPoints, resultContour);
	                    fireProgressStateChanged(25 + (((75 * elementNum) + 50) / nContours));
	
	                    if (trim) {
	                        resultContour.trimPoints(Preferences.getTrimVoi(), Preferences.getTrimAdjacient());
	                    }
	                    resultVOI.importCurve(resultContour);
	
	                    if (threadStopped) {
	                        finalize();
	
	                        return;
	                    }
	                } else {
	                	// nPoints is less than 5.  doesn't mean we want to scrap the contour though!
	                	resultVOI.importCurve(contours.elementAt(elementNum));
	                	
	                }
	            } // if ( ((VOIContour)(contours[0].elementAt(elementNum))).isActive() )
	        } // for(elementNum = 0; elementNum < nContours; elementNum++)
        }

        fireProgressStateChanged(100);
        
        setCompleted(true);
    }

    /**
     * Prepares the data and runs the algorithm for a 3D image.
     */
    private void calc3D() {
    	boolean isPoint = false;
    	if(activeVOI.getCurveType() == VOI.POINT) {
    		resultVOI = new VOI((short) srcImage.getVOIs().size(), "BsmoothVOI.voi", VOI.POINT, -1.0f);
    		isPoint = true;
    	}else {
    		resultVOI = new VOI((short) srcImage.getVOIs().size(), "BsmoothVOI.voi", VOI.CONTOUR, -1.0f);
    	}

        fireProgressStateChanged(srcImage.getImageName(), "Bspline smooth: Evolving boundary ...");

        fireProgressStateChanged(0);
        Vector<VOIBase> contours = activeVOI.getCurves();

        int nContours = contours.size();
        
        System.out.println("num countours is " + nContours);
        
        
        if(isPoint) {
        	
        	 int nPoints = contours.size();
             
             System.out.println("** " + nPoints);

             float[] xPoints = new float[nPoints + 5];
             float[] yPoints = new float[nPoints + 5];
             float[] zPoints = new float[nPoints + 5];
             setPoints2(xPoints, yPoints, zPoints);
            
             runSmooth2(xPoints, yPoints, zPoints);
             
             //resultVOI.importCurve(resultContour);
             
             
   
        }else {
        	for (int elementNum = 0; elementNum < nContours; elementNum++) {
            
                if (((VOIContour) (contours.elementAt(elementNum))).isActive()) {
                    if (contours.elementAt(elementNum).size() > 5) {
                    	System.out.println(contours.elementAt(elementNum).size());
                        float[] xPoints = new float[contours.elementAt(elementNum).size() + 5];
                        float[] yPoints = new float[contours.elementAt(elementNum).size() + 5];
                        float[] zPoints = new float[contours.elementAt(elementNum).size() + 5];
                        setPoints(xPoints, yPoints, zPoints, contours.elementAt(elementNum));
                        VOIContour resultContour = new VOIContour(false, contours.elementAt(elementNum).isClosed());
                        runSmooth(xPoints, yPoints, zPoints, resultContour);

                        if (threadStopped) {
                            finalize();
                            return;
                        }

                        if (trim) {
                            resultContour.trimPoints(Preferences.getTrimVoi(), Preferences.getTrimAdjacient());
                        }
                        resultVOI.importCurve(resultContour);
                    } else {
                        // nPoints is less than 5.  doesn't mean we want to scrap the contour though!
                        resultVOI.importCurve(contours.elementAt(elementNum));

                    }
                }
            }
        }
        
        
        
        
        

        

        fireProgressStateChanged(100);
        setCompleted(true);


        return;


    }
    
    
    
    
    
    
    
    
    /**
     * Actual function that smooths the voi with bsplines.
     *
     * @param  xPoints    x coordinates that describe the contour
     * @param  yPoints    y coordinates that describe the contour
     * @param  resultGon  resultant polygon
     */
    private void runSmooth2(float[] xPoints, float[] yPoints, float[] zPoints) {
        float pct;
        float index;

        AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints, zPoints);
        bSpline = new AlgorithmBSpline();

        float[] newXPts = new float[nPts];
        float[] newYPts = new float[nPts];
        float[] newZPts = new float[nPts];

        for (int i = 0; i < nPts; i++) {
            pct = i / (float) (nPts);

            /** Note that pct = 0 returns an index = 2 while pct = 1.0
             * returns an index 2 below the maximum array index */
            index = arcLength.invlen(pct);
            Vector3f interpPt = bSpline.bSplineJetXYZ(0, index, xPoints, yPoints, zPoints);
            newXPts[i] = interpPt.X;
            newYPts[i] = interpPt.Y;
            newZPts[i] = interpPt.Z;
        }

        for (int i = 0; i < nPts; i++) {
        	
        	Vector3f point = new Vector3f(newXPts[i],newYPts[i],newZPts[i]);
        	VOIPoint resultPoint = new VOIPoint(VOI.POINT,point);
        	resultVOI.importCurve(resultPoint);
        }
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    


    /**
     * Actual function that smooths the voi with bsplines.
     *
     * @param  xPoints    x coordinates that describe the contour
     * @param  yPoints    y coordinates that describe the contour
     * @param  resultGon  resultant polygon
     */
    public void runSmooth(float[] xPoints, float[] yPoints, float[] zPoints, VOIBase resultContour) {
        float pct;
        float index;

        AlgorithmArcLength arcLength = new AlgorithmArcLength(xPoints, yPoints, zPoints);
        AlgorithmBSpline bSpline = new AlgorithmBSpline();

        float[] newXPts = new float[nPts];
        float[] newYPts = new float[nPts];
        float[] newZPts = new float[nPts];

        for (int i = 0; i < nPts; i++) {
            pct = i / (float) (nPts);

            /** Note that pct = 0 returns an index = 2 while pct = 1.0
             * returns an index 2 below the maximum array index */
            index = arcLength.invlen(pct);
            Vector3f interpPt = bSpline.bSplineJetXYZ(0, index, xPoints, yPoints, zPoints);
            newXPts[i] = interpPt.X;
            newYPts[i] = interpPt.Y;
            newZPts[i] = interpPt.Z;
        }

        for (int i = 0; i < nPts; i++) {
            resultContour.add(new Vector3f( Math.round(newXPts[i]), Math.round(newYPts[i]), Math.round(newZPts[i])));
        }
    }

    /**
     * Takes the polygon and forms two special arrays for use in the Bspline.
     *
     * @param  xPoints  storage location of array of x coord. points
     * @param  yPoints  storage location array of y coord. points
     * @param  gon      initial polygon
     */
    public void setPoints(float[] xPoints, float[] yPoints, float[] zPoints, VOIBase contour) {

        /** Note that 0 is used twice - once in the 0 to 1 segment and once
         * in the n-1 to zero segment. */

        int nPoints = contour.size();
        System.out.println("nPoints is " + nPoints);
        xPoints[0] = contour.elementAt(nPoints - 2).X;
        yPoints[0] = contour.elementAt(nPoints - 2).Y;
        zPoints[0] = contour.elementAt(nPoints - 2).Z;
        
        xPoints[1] = contour.elementAt(nPoints - 1).X;
        yPoints[1] = contour.elementAt(nPoints - 1).Y;
        zPoints[1] = contour.elementAt(nPoints - 1).Z;


        for (int i = 0; i < nPoints; i++) {
            xPoints[i + 2] = contour.elementAt(i).X;
            yPoints[i + 2] = contour.elementAt(i).Y;
            zPoints[i + 2] = contour.elementAt(i).Z;
        }
        
        xPoints[nPoints + 2] = contour.elementAt(0).X;
        yPoints[nPoints + 2] = contour.elementAt(0).Y;
        zPoints[nPoints + 2] = contour.elementAt(0).Z;
        
        xPoints[nPoints + 3] = contour.elementAt(1).X;
        yPoints[nPoints + 3] = contour.elementAt(1).Y;
        zPoints[nPoints + 3] = contour.elementAt(1).Z;
        
        xPoints[nPoints + 4] = contour.elementAt(2).X;
        yPoints[nPoints + 4] = contour.elementAt(2).Y;
        zPoints[nPoints + 4] = contour.elementAt(2).Z;
    }
    
    
    
    
    
    
    
    
    
    
    
    /**
     * Takes the polygon and forms two special arrays for use in the Bspline.
     *
     * @param  xPoints  storage location of array of x coord. points
     * @param  yPoints  storage location array of y coord. points
     * @param  gon      initial polygon
     */
    private void setPoints2(float[] xPoints, float[] yPoints, float[] zPoints) {
    	
    	Vector<VOIBase> contours = activeVOI.getCurves();
    	Vector3f point;
    	

        /** Note that 0 is used twice - once in the 0 to 1 segment and once
         * in the n-1 to zero segment. */

        int nPoints = contours.size();
        point = ((VOIPoint)contours.get(0)).exportPoint();
        System.out.println("nPoints is " + nPoints);
        xPoints[0] = point.X;
        yPoints[0] = point.Y;
        zPoints[0] = point.Z;
        
        xPoints[1] = point.X;
        yPoints[1] = point.Y;
        zPoints[1] = point.Z;


        for (int i = 0; i < nPoints; i++) {
        	point = ((VOIPoint)contours.get(i)).exportPoint();
            xPoints[i + 2] = point.X;
            yPoints[i + 2] = point.Y;
            zPoints[i + 2] = point.Z;
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
    }

	public synchronized AlgorithmBSpline getbSplineAlgo() {
		return bSpline;
	}
    
    
    
    
    
    
    
    
    
    

}
