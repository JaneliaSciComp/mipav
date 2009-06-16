package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;

/**
 
 */
public class AlgorithmRandomCircleGeneration extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    // Circle radius
    private int radius;
    
    // number of circles to be drawn
    private int numCircles;
    
    private ModelImage testImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * AlgorithmRandomCircleGeneration - default constructor.
     */
    public AlgorithmRandomCircleGeneration() { }

    /**
     * AlgorithmRandomCircleGeneration.
     *
     * @param  srcImg   Blank source image in which circles will be drawn
     * @param  radius   Circle radius
     * @param  numCircles Number of circles to be drawn
     */
    public AlgorithmRandomCircleGeneration(ModelImage srcImage, int radius, int numCircles) {
        super(null, srcImage);
        this.radius = radius;
        this.numCircles = numCircles;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * finalize -
     */
    public void finalize() {
        super.finalize();
    }
    
    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        int xDim;
        int yDim;
        byte mask[];
        int x;
        int y;
        int yDistSquared;
        int xDistSquared;
        int radiusSquared;
        int xMaskDim;
        int yMaskDim;
        int distSquared;
        int lowestDistSquared;
        int i;
        int j;
        int attempts;
        boolean found;
        byte buffer[];
        int length;
        int xCenter = radius;
        int yCenter = radius;
        /** Reference to the random number generator. */
        RandomNumberGen randomGen;
        int circlesDrawn;
        int circleXCenter[] = new int[numCircles];
        int circleYCenter[] = new int[numCircles];
        double nearestNeighborDistance[];
        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        

        fireProgressStateChanged(srcImage.getImageName(), "Random circle generation ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;
        buffer = new byte[length];
        // Create a mask for setting circles
        radiusSquared = radius * radius;
        xMaskDim = 2 * radius + 1;
        yMaskDim = xMaskDim;
        mask = new byte[xMaskDim * yMaskDim];
        for (y = 0; y <= 2*radius; y++) {
            yDistSquared = (y - radius);
            yDistSquared = yDistSquared * yDistSquared;
            for (x = 0; x <= 2*radius; x++) {
                xDistSquared = (x - radius);
                xDistSquared = xDistSquared * xDistSquared;
                distSquared = xDistSquared + yDistSquared;
                if (distSquared <= radiusSquared) {
                    mask[x + y * xMaskDim] = 1;
                }
            }
        } // for (y = 0; y <= 2*radius; y++)
        
        randomGen = new RandomNumberGen();
        for (i = 1; i <= numCircles; i++) {
        found = false;
        attempts = 0;
            while ((!found) && (attempts <= 100)) {
                found = true;
                xCenter = randomGen.genUniformRandomNum(radius, xDim - radius - 1);
                yCenter = randomGen.genUniformRandomNum(radius, yDim - radius - 1);
                yloop:
                for (y = 0; y <= 2*radius; y++) {
                    for (x = 0; x <= 2*radius; x++) {
                        if (mask[x + y * xMaskDim] == 1) {
                            if (buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] == 1) {
                                found = false;
                                attempts++;
                                break yloop;
                            }
                        }
                    }
                } // for (y = 0; y <= 2*radius; y++)
            } // while ((!found) && (attempts <= 100)
            if (!found) {
                break;
            }
            circleXCenter[i-1] = xCenter;
            circleYCenter[i-1] = yCenter;
            for (y = 0; y <= 2*radius; y++) {
                for (x = 0; x <= 2*radius; x++) {
                    if (mask[x + y * xMaskDim] == 1) {
                        buffer[(xCenter + x - radius) + xDim*(yCenter + y - radius)] = 1;
                    }
                }
            }
        } // for (i = 1; i <= numCircles; i++)
        circlesDrawn = i-1;
        Preferences.debug(circlesDrawn + " circles drawn.  " + numCircles + " circles requested.\n");
        System.out.println(circlesDrawn + " circles drawn.  " + numCircles + " circles requested.");
        
        nearestNeighborDistance = new double[circlesDrawn];
        for (i = 0; i < circlesDrawn; i++) {
            lowestDistSquared = Integer.MAX_VALUE;
            for (j = 0; j < circlesDrawn; j++) {
                if (i != j) {
                    xDistSquared = circleXCenter[i] - circleXCenter[j];
                    xDistSquared = xDistSquared * xDistSquared;
                    yDistSquared = circleYCenter[i] - circleYCenter[j];
                    yDistSquared = yDistSquared * yDistSquared;
                    distSquared = xDistSquared + yDistSquared;
                    if (distSquared < lowestDistSquared) {
                        lowestDistSquared = distSquared;
                        nearestNeighborDistance[i] = Math.sqrt(distSquared);
                    }
                }
            }
        } // for (i = 0; i < circlesDrawn; i++)
        
        try {
            srcImage.importData(0, buffer, true);
        }
        catch(IOException e) {
            MipavUtil.displayError("IOException on srcImage.importData(0, buffer, true");
            setCompleted(false);
            return;  
        }
        
        setCompleted(true);
        return;
    }
}
