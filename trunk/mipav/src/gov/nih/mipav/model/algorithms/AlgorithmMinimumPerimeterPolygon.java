package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.MipavUtil;

import java.util.*;

/** Smoothing VOI with minimum perimeter polygon.
 * References:
 * 1.) Digital ImageProcessing Fourth Edition, Rafael C. Gonzalez and Richard E. Woods,
 * Boundary Approximations Using Minimum-Perimeter Polygons, pp. 963-968.
 * @author ilb
 *
 */

public class AlgorithmMinimumPerimeterPolygon extends AlgorithmBase {
	
	//~ Instance fields ------------------------------------------------------------------------------------------------

    /** The voi selected by the user. */
    private VOI activeVOI;

    /** Square cell length. */
    private int squareCellLength;

    /** The resultant polygon and the evolution has completed. */
    private VOI resultVOI;

    /** Source image. */
    private ModelImage srcImage;
    
//~ Constructors ---------------------------------------------------------------------------------------------------
    
    public AlgorithmMinimumPerimeterPolygon() {
       
    }

    /**
     * Creates a new AlgorithmMinimumPerimeterPolygon object.
     *
     * @param  srcImg     2D or 3D source image
     * @param  activeVOI  the selected voi
     * @param  squareCellLength square cell length
     */
    public AlgorithmMinimumPerimeterPolygon(ModelImage srcImg, VOI activeVOI, int squareCellLength) {

        srcImage = srcImg;
        this.activeVOI = activeVOI;
        this.squareCellLength = squareCellLength;
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
    	int i,j;                 
        fireProgressStateChanged(srcImage.getImageName(), "Minimum Perimeter Polygon: Evolving boundary ...");

        fireProgressStateChanged(25);
        
        resultVOI = new VOI((short) srcImage.getVOIs().size(), "MPPsmooth-VOI", VOI.CONTOUR, -1.0f);
        
        Vector<VOIBase> contours = activeVOI.getCurves();
        int nContours = contours.size();
        
        
	        for (int elementNum = 0; elementNum < nContours; elementNum++) {
	            fireProgressStateChanged((int) (25 + (75 * (((float) elementNum) / nContours))));
	
	            if (((VOIContour) (contours.elementAt(elementNum))).isActive()) {
	                int nPoints = contours.elementAt(elementNum).size();
	                System.out.println("Original contour has " + nPoints + " points");
	                    float[] xPoints = new float[nPoints];
	                    float[] yPoints = new float[nPoints];
	                    Vector<Integer> ulx = new Vector<Integer>();
	                    Vector<Integer> uly = new Vector<Integer>();
	              
	                    float zPoint = contours.elementAt(elementNum).elementAt(0).Z;
	                    // Algorithm proceeds counterclockwise starting with the upper leftmost point
	                    ((VOIContour) (contours.elementAt(elementNum))).makeCounterClockwise();
	                    
	                    Vector<Integer>newX = new Vector<Integer>();
	                    Vector<Integer>newY = new Vector<Integer>();
	                    Vector<Boolean>isWVertex = new Vector<Boolean>();
	                    int xpos, ypos;
	                    for (i = 0; i < xPoints.length; i++) {
	                    	xPoints[i] = contours.elementAt(elementNum).elementAt(i).X;
	                        yPoints[i] = contours.elementAt(elementNum).elementAt(i).Y;	
	                        xpos = squareCellLength*(int)(Math.floor(xPoints[i]/squareCellLength));
	                        ypos = squareCellLength*(int)(Math.floor(yPoints[i]/squareCellLength));
	                        if ((i == 0) || (xpos != ulx.get(ulx.size()-1)) || (ypos != uly.get(uly.size()-1))) {
	                        	ulx.add(xpos);
	                        	uly.add(ypos);
	                        }
	                    }
	                    //Find upperMost, leftMost vertex;
	                    int ulVertex = -1;
	                    int leftMost = Integer.MAX_VALUE;
	                    int upperMost = Integer.MAX_VALUE;
	                    for (i = 0; i < ulx.size(); i++) {
	                        if (uly.get(i) < upperMost) {
	                        	ulVertex = i;
	                        	upperMost = uly.get(i);
	                        	leftMost = ulx.get(i);
	                        }
	                        else if ((uly.get(i) == upperMost) && (ulx.get(i) < leftMost)) {
	                        	ulVertex = i;
	                        	leftMost = ulx.get(i);
	                        }
	                    }
	                    System.out.println("ulx has " + ulx.size() + " points");
	                    // The first vertex is always a convex w vertex of the mpp
                        newX.add(ulx.get(ulVertex)+squareCellLength);
                    	newY.add(uly.get(ulVertex)+squareCellLength);
                    	isWVertex.add(true);
                    	boolean isNorth1, isEast1, isSouth1, isWest1, isNorth2, isEast2, isSouth2, isWest2;
	                    for (i = ulVertex+1; i < ulx.size(); i++) {
	                        isNorth1 = (uly.get(i) < uly.get(i-1)) && (ulx.get(i) == ulx.get(i-1));
	                        isEast1 = (ulx.get(i) > ulx.get(i-1)) && (uly.get(i) == uly.get(i-1));
	                        isSouth1 = (uly.get(i) > uly.get(i-1)) && (ulx.get(i) == ulx.get(i-1));
	                        isWest1 = (ulx.get(i) < ulx.get(i-1)) && (uly.get(i) == uly.get(i-1));
	                        if (i < ulx.size()-1) {
	                            isNorth2 = (uly.get(i+1) < uly.get(i)) && (ulx.get(i+1) == ulx.get(i));
	                            isEast2 = (ulx.get(i+1) > ulx.get(i)) && (uly.get(i+1) == uly.get(i));
	                            isSouth2 = (uly.get(i+1) > uly.get(i)) && (ulx.get(i+1) == ulx.get(i));
	                            isWest2 = (ulx.get(i+1) < ulx.get(i)) && (uly.get(i+1) == uly.get(i));
	                        }
	                        else {
	                        	isNorth2 = (uly.get(0) < uly.get(i)) && (ulx.get(0) == ulx.get(i));
	                            isEast2 = (ulx.get(0) > ulx.get(i)) && (uly.get(0) == uly.get(i));
	                            isSouth2 = (uly.get(0) > uly.get(i)) && (ulx.get(0) == ulx.get(i));
	                            isWest2 = (ulx.get(0) < ulx.get(i)) && (uly.get(0) == uly.get(i));	
	                        }
	                        if (isWest1 && isSouth2) {
	                        	newX.add(ulx.get(i)+squareCellLength);
	                        	newY.add(uly.get(i)+squareCellLength);
	                        	isWVertex.add(true);	
	                        }
	                        else if (isSouth1 && isEast2) {
	                        	newX.add(ulx.get(i)+squareCellLength);
	                        	newY.add(uly.get(i));
	                        	isWVertex.add(true);		
	                        }
	                        else if (isEast1 && isNorth2) {
	                        	newX.add(ulx.get(i));
	                        	newY.add(uly.get(i));
	                        	isWVertex.add(true);	
	                        }
	                        else if (isNorth1 && isWest2) {
	                        	newX.add(ulx.get(i));
	                        	newY.add(uly.get(i)+squareCellLength);
	                        	isWVertex.add(true);	
	                        }
	                        else if (isSouth1 && isWest2) {
	                        	newX.add(ulx.get(i));
	                        	newY.add(uly.get(i));
	                        	isWVertex.add(false);
	                        }
	                        else if (isEast1 && isSouth2) {
	                        	newX.add(ulx.get(i));
	                        	newY.add(uly.get(i)+squareCellLength);
	                        	isWVertex.add(false);	
	                        }
	                        else if (isNorth1 && isEast2) {
	                        	newX.add(ulx.get(i)+squareCellLength);
	                        	newY.add(uly.get(i)+squareCellLength);
	                        	isWVertex.add(false);	
	                        }
	                        else if (isWest1 && isNorth2) {
	                        	newX.add(ulx.get(i)+squareCellLength);
	                        	newY.add(uly.get(i));
	                        	isWVertex.add(false);	
	                        }
	                    }
	                    for (i = 0; i < ulVertex; i++) {
	                    	if (i == 0) {
	                    		isNorth1 = (uly.get(i) < uly.get(uly.size()-1)) && (ulx.get(i) == ulx.get(ulx.size()-1));
		                        isEast1 = (ulx.get(i) > ulx.get(ulx.size()-1)) && (uly.get(i) == uly.get(uly.size()-1));
		                        isSouth1 = (uly.get(i) > uly.get(uly.size()-1)) && (ulx.get(i) == ulx.get(ulx.size()-1));
		                        isWest1 = (ulx.get(i) < ulx.get(ulx.size()-1)) && (uly.get(i) == uly.get(uly.size()-1));	
	                    	}
	                    	else {
		                        isNorth1 = (uly.get(i) < uly.get(i-1)) && (ulx.get(i) == ulx.get(i-1));
		                        isEast1 = (ulx.get(i) > ulx.get(i-1)) && (uly.get(i) == uly.get(i-1));
		                        isSouth1 = (uly.get(i) > uly.get(i-1)) && (ulx.get(i) == ulx.get(i-1));
		                        isWest1 = (ulx.get(i) < ulx.get(i-1)) && (uly.get(i) == uly.get(i-1));
	                    	}
	                        if (i < ulx.size()-1) {
	                            isNorth2 = (uly.get(i+1) < uly.get(i)) && (ulx.get(i+1) == ulx.get(i));
	                            isEast2 = (ulx.get(i+1) > ulx.get(i)) && (uly.get(i+1) == uly.get(i));
	                            isSouth2 = (uly.get(i+1) > uly.get(i)) && (ulx.get(i+1) == ulx.get(i));
	                            isWest2 = (ulx.get(i+1) < ulx.get(i)) && (uly.get(i+1) == uly.get(i));
	                        }
	                        else {
	                        	isNorth2 = (uly.get(0) < uly.get(i)) && (ulx.get(0) == ulx.get(i));
	                            isEast2 = (ulx.get(0) > ulx.get(i)) && (uly.get(0) == uly.get(i));
	                            isSouth2 = (uly.get(0) > uly.get(i)) && (ulx.get(0) == ulx.get(i));
	                            isWest2 = (ulx.get(0) < ulx.get(i)) && (uly.get(0) == uly.get(i));	
	                        }
	                        if (isWest1 && isSouth2) {
	                        	newX.add(ulx.get(i)+squareCellLength);
	                        	newY.add(uly.get(i)+squareCellLength);
	                        	isWVertex.add(true);	
	                        }
	                        else if (isSouth1 && isEast2) {
	                        	newX.add(ulx.get(i)+squareCellLength);
	                        	newY.add(uly.get(i));
	                        	isWVertex.add(true);		
	                        }
	                        else if (isEast1 && isNorth2) {
	                        	newX.add(ulx.get(i));
	                        	newY.add(uly.get(i));
	                        	isWVertex.add(true);	
	                        }
	                        else if (isNorth1 && isWest2) {
	                        	newX.add(ulx.get(i));
	                        	newY.add(uly.get(i)+squareCellLength);
	                        	isWVertex.add(true);	
	                        }
	                        else if (isSouth1 && isWest2) {
	                        	newX.add(ulx.get(i));
	                        	newY.add(uly.get(i));
	                        	isWVertex.add(false);
	                        }
	                        else if (isEast1 && isSouth2) {
	                        	newX.add(ulx.get(i));
	                        	newY.add(uly.get(i)+squareCellLength);
	                        	isWVertex.add(false);	
	                        }
	                        else if (isNorth1 && isEast2) {
	                        	newX.add(ulx.get(i)+squareCellLength);
	                        	newY.add(uly.get(i)+squareCellLength);
	                        	isWVertex.add(false);	
	                        }
	                        else if (isWest1 && isNorth2) {
	                        	newX.add(ulx.get(i)+squareCellLength);
	                        	newY.add(uly.get(i));
	                        	isWVertex.add(false);	
	                        }
	                    }
	                    System.out.println("newX has " + newX.size() + " points");
	                    for (i = 0; i < newX.size(); i++) {
	                    	for (j = i+1; j < newX.size(); j++) {
	                    		if ((newX.get(i)).equals(newX.get(j))&&(newY.get(i)).equals(newY.get(j))) {
	                    		    MipavUtil.displayError("Self intersection found");
	                    		    setCompleted(false);
	                    		    return;
	                    		}
	                    	}
	                    }
	                    boolean test = false;
	                    if (test) {
	                    	// Figure 12.8 in Digital Image Processing
	                        // Test newX and newY to VX and VY
	                    	newX.clear();
	                    	newY.clear();
	                    	isWVertex.clear();
	                    	newX.add(4);
	                    	newY.add(1);
	                    	isWVertex.add(true);
	                    	newX.add(3);
	                    	newY.add(2);
	                    	isWVertex.add(false);
	                    	
	                    } // if (test)
	                    Vector<Integer>VX = new Vector<Integer>();
	                    Vector<Integer>VY = new Vector<Integer>();
	                    VX.add(newX.get(0));
	                    VY.add(newY.get(0));
	                    int VLX = newX.get(0);
	                    int VLY = newY.get(0);
	                    int BCX = VLX;
	                    int BCY = VLY;
	                    int WCX = VLX;
	                    int WCY = VLY;
	                    int k = 1;
	                    while (k < newX.size()) {
		                    int VkX = newX.get(k);
		                    int VkY = newY.get(k);
		                    double detVWV = det(VLX,WCX,VkX,VLY,WCY,VkY);
		                    double detVBV = det(VLX,BCX,VkX,VLY,BCY,VkY);
		                    if (detVWV > 1.0E-8) {
		                      VX.add(WCX);
		                      VY.add(WCY);
		                      VLX = WCX;
		                      VLY = WCY;
		                      BCX = VLX;
		                      BCY = VLY;
		                    }
		                    else if ((detVWV <= 0) && (detVBV >= 0)) {
		                        if (isWVertex.get(k)) {
		                        	WCX = VkX;
		                        	WCY = VkY;
		                        }
		                        else {
		                        	BCX = VkX;
		                        	BCY = VkY;
		                        }
		                    }
		                    else if (detVBV < -1.0E-8) {
		                    	VX.add(BCX);
			                    VY.add(BCY);
			                    VLX = BCX;
			                    VLY = BCY;
			                    WCX = VLX;
			                    WCY = VLY;
		                    }
		                    k++;
	                    } // while (k < newX.size())
	                    System.out.println("VX has " + VX.size() + " points");
	            
	                    VOIContour resultContour = new VOIContour( false, contours.elementAt(elementNum).isClosed());
	                   for (i = 0; i < VX.size(); i++) {
	                          resultContour.add(new Vector3f(VX.get(i), VY.get(i), zPoint));
	                    }
	                    fireProgressStateChanged(25 + (((75 * elementNum) + 50) / nContours));
	
	                    
	                    resultVOI.importCurve(resultContour);
	
	                    if (threadStopped) {
	                        finalize();
	
	                        return;
	                    }
	                
	            } // if ( ((VOIContour)(contours[0].elementAt(elementNum))).isActive() )
	        } // for(elementNum = 0; elementNum < nContours; elementNum++)

        fireProgressStateChanged(100);
        
        setCompleted(true);
    }
    
    private double det(double x1, double x2, double x3, double y1, double y2, double y3) {
	    double determinant;
	    // Compute twice the area of the triangle defined by three points
	    // with coords (x1, y1), (x2, y2), and (x3, y3) using determinant
	    // formula
	    
	    // Input parameters: 
	    // x1, y1 coords of point 1
	    // x2, y2 coords of point 2
	    // x3, y3 coords of point 3
	    
	    // Output parameters:
	    // determinant twice the area of the triangle defined by the three points
	    
	    // Notes:
	    
	    // determinant is positive is points 1, 2, and 3 define the triangle in
	    // counterclockwise order
	    // determinant is negative if points 1, 2, and 3 define the triangle in
	    // clockwise order
	    // determinant is zero if at least 2 of the points are coincident or if
	    // all three points are collinear
	    
	    determinant = (x1-x3)*(y2-y3) - (x2-x3)*(y1-y3);
	    return determinant;    
	} // det

}