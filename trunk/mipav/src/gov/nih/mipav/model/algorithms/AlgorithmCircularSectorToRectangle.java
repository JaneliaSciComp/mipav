package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 z1 is the upper right point on rmax
 z2 is the upper left point on rmax
 z3 is the lower left point on rmin
 z4 is the lower right point on rmin
 theta is the angle of the sector
 theta = alpha * PI, with 0 < alpha <= 1
 */

public class AlgorithmCircularSectorToRectangle extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private double x[];
    
    private double y[];
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmCircularSectorToRectangle - default constructor.
     */
    public AlgorithmCircularSectorToRectangle() { }

    /**
     * AlgorithmTPSpline - constructor for 2D case.
     *
     * @param  x     array with x coordinates of source boundaries
     * @param  y     array with y coordinates of source boundaries
     */
    public AlgorithmCircularSectorToRectangle(ModelImage destImg, ModelImage srcImg, 
                                              double x[], double y[]) {
        super(destImg, srcImg);
        this.x = x;
        this.y = y;
    }

    

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        
        super.finalize();
    }
    
    /**
     * Constructs a string of the contruction parameters and out puts the string to the messsage frame if the history
     * logging procedure is turned on.
     */
    private void constructLog() {

        
            historyString = new String("GaussianBlur(" + String.valueOf(destImage.getExtents()[0]) + ", " +
                                       String.valueOf(destImage.getExtents()[1]) + ")\n");
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        double x1, x2, x3, x4, y1, y2, y3, y4;
        
        double xc, yc;
        
        double rmin, rmax;
        
        double theta1, theta2;
        
        // Angle of sector
        double theta;
        
        double alpha;
        
        BitSet mask = null;
        
        int xDimSource;
        
        int yDimSource;
        
        int sourceSlice;
        
        int xmin;
        
        int xmax;
        
        int ymin;
        
        double minsq, maxsq;
        int i, j;
        int index, index1;
        double ry, r;
        double ang;
        
        int xDimDest;
        int yDimDest;
        int destSlice;
        float srcBuffer[];
        float destBuffer[];
        double var;
        double yp;
        double xp;
        double rscale;
        double yDest;
        double xDest;
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        constructLog();
        
        x1 = x[0];
        x2 = x[1];
        x3 = x[2];
        x4 = x[3];
        y1 = y[0];
        y2 = y[1];
        y3 = y[2];
        y4 = y[3];
        // Calculate center point of circle
        // (yc - y4)/(xc - x4) = (y4 - y1)/(x4 - x1)
        // yc - y4 = (xc - x4) * (y4 - y1)/(x4 - x1)
        // yc = (y4 - y1)/(x4 - x1) * xc + (y4 - x4*(y4 - y1)/(x4 - x1))
        // yc = (y4 - y1)/(x4 - x1) * xc + (x4*y1 - x1*y4)/(x4 - x1)
        
        // (yc - y3)/(xc - x3) = (y3 - y2)/(x3 - x2)
        // yc - y3 = (xc - x3) * (y3 - y2)/(x3 - x2)
        // yc = (y3 - y2)/(x3 - x2) * xc + (y3 - x3*(y3 - y2)/(x3 - x2))
        // yc = (y3 - y2)/(x3 - x2) * xc + (x3*y2 - x2*y3)/(x3 - x2)
        
        // xc*((y4 - y1)/(x4 - x1) - (y3 - y2)/(x3 - x2)) +
        // ((x4*y1 - x1*y4)/(x4 - x1) - (x3*y2 - x2*y3)/(x3 - x2)) = 0
        
        xc = (x3*y2 - x2*y3)/(x3 - x2) - (x4*y1 - x1*y4)/(x4 - x1);
        xc = xc/((y4 - y1)/(x4 - x1) - (y3 - y2)/(x3 - x2));
        
        yc = (y4 - y1)/(x4 - x1) * xc + (x4*y1 - x1*y4)/(x4 - x1);
        Preferences.debug("x center = " + xc + " y center = " + yc + "\n");
        
        rmin = Math.sqrt((x4 - xc)*(x4 - xc) + (y4 - yc)*(y4 - yc));
        minsq = rmin * rmin;
        
        rmax = Math.sqrt((x1 - xc)*(x1 - xc) + (y1 - yc)*(y1 - yc));
        maxsq = rmax * rmax;
        Preferences.debug("rmin = " + rmin + " rmax = " + rmax + "\n");
        
        // Calculate angle along line from center to z4 to z1 in -PI to PI radians
        theta1 = Math.atan2((y1 - y4), (x1 - x4));
        
        // Calculate angle along line from center to z3 to z2 in -PI to PI radians
        theta2 = Math.atan2((y3 - y2), (x3 - x2));
        
        // Angle of sector in radians
        // theta = alpha * PI
        theta = theta2 - theta1;
        
        alpha = theta/Math.PI;
        Preferences.debug("alpha = " + alpha + "\n");
        
        // Find the area of the image that is part of the sector
        xDimSource = srcImage.getExtents()[0];
        yDimSource = srcImage.getExtents()[1];
        sourceSlice = xDimSource * yDimSource;
        // All bits are initially false
        mask = new BitSet(sourceSlice);
        for (j = 0; j < yDimSource; j++) {
            ry = (j - yc)*(j - yc);
            index1 = j * xDimSource;
            for (i = 0; i < xDimSource; i++) {
                r = ry + (i - xc)*(i - xc);
                if ((r >= minsq) && (r <= maxsq)) {
                    ang = Math.atan2((j - yc), (i - xc));
                    if ((ang >= theta1) && (ang <= theta2)) {
                        index = index1 + i;
                        mask.set(index);
                    }
                }
            }
        } // for (j = 0; j < yDimSource; j++)
        
        xDimDest = destImage.getExtents()[0];
        yDimDest = destImage.getExtents()[1];
        destSlice = xDimDest * yDimDest;
        srcBuffer = new float[sourceSlice];
        try {
            srcImage.exportData(0, sourceSlice, srcBuffer);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.exportData");
            setCompleted(false);
            return;
        }
        destBuffer = new float[destSlice];
        // 2 mappings
        // Mapping 1 is simply a mapping from 1 rectangle to another in which the
        // x and y axes are scaled and inverted
        // Map from z1" = (xDimDest-1, yDimDest-1) to z1' = (0,-log(sqrt(rmax/rmin))/theta)
        // Map from z2" = (0, yDimDest-1) to z2' = (1,-log(sqrt(rmax/rmin))/theta)
        // Map from z3" = (0, 0) to z3' = (1, log(sqrt(rmax/rmin))/theta)
        // Map from z4" = (xDimDest-1, 0) to z4' = (0, log(sqrt(rmax/rmin))/theta)
        
        // x" = (1 - x')*(xDim - 1)
        // 1 - x' = x"/(xDim - 1)
        // x' - 1 = -x"/(xDim - 1)
        // x' = 1 - x"/(xDim - 1)
        // var = (log(sqrt(rmax/rmin))/theta
        // y" = (var - y') * (yDim - 1)/(2 * var)
        // var - y' = (2 * var * y")/(yDim - 1)
        // y' - var = -(2 * var * y")/(yDim - 1)
        // y' = var - (2 *var * y")/(yDim - 1)
        var = 0.5*Math.log(rmax/rmin)/theta;
        rscale = Math.sqrt(rmax*rmin);
        for (j = 0; j < yDimDest; j++) {
            index1 = j * xDimDest;
            yp = var - (2.0 * var * j)/(yDimDest - 1);
            r = rscale * Math.exp(-theta*yp);
            for (i = 0; i < xDimDest; i++) {
                xp = 1.0 - (double)i/(xDimDest - 1);
                ang = theta * xp;
                xDest = r * Math.cos(ang);
                yDest = r * Math.sin(ang);
            }
        }
    }


    

}
