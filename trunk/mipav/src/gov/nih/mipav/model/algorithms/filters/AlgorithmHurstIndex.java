package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;


/**
 * DOCUMENT ME!
 *
 * @version  0.1 June 13, 2012
 * @author   William Gandler 
 * Reference: "Sonographic Texture Characterization of Salivary  Gland Tumors by Fractal Analysis",
 * by Toru Chikui, Kenji Tokumori, Kazunori Yoshiura, Kazunari Oobi, Seiji Nakamura, and Katsumasa Nakamura,
 * Ultrasound in Medicine and Biology, Vol. 31, No. 10, pp. 1297-1304, 2005.
 * In 2D for every point in the image find the absolute value of the intensity differences to all points 
 * between a minimum distance of minDist and a maximum distance of maxDist.  Have the option to define the
 * distance as the integer part of the Euclidean distance.  For every distance between minDist and maxDist
 * find the average value of the absolute value of the intensity difference.  Fit the best fit to the line
 * log(average absolute value of the intensity difference) = log(c) + H*log(distance) where H is the Hurst index.
 * 
 * The Hurst index varies from 0 to 1.  If H is 0.5, each step can be up or down completely at random.  If H is
 * less than 0.5, each step upward is likely to be followed by a downward step and each downward step is likely
 * to be followed by an upward step.  Therefore, a lower Hurst index indicates a higher complexity of the
 * distribution of pixel values.
 */
public class AlgorithmHurstIndex extends AlgorithmBase {
    
    /** Red channel. */
    private static final int RED_OFFSET = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private double minDist;
    
    private double maxDist;
    
    private boolean integerDistancePart;
    
    private int RGBOffset = RED_OFFSET;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmHurstIndex object for black and white image.
     *
     * @param              destImg            image model where result image is to stored
     * @param              srcImg             source image model
     * @param              minDist            minimum distance between pixels
     * @param              maxDist            maximum distance between pixels
     * @param              integerDistancePart   If true, take the integer part of the Euclidean distance as the distance.
     *                                           if false, take the Euclidean distance as the distance.
     */
    public AlgorithmHurstIndex(ModelImage destImg, ModelImage srcImg, double minDist, double maxDist, boolean integerDistancePart) {
        super(destImg, srcImg);
        this.minDist = minDist;
        this.maxDist = maxDist;
        this.integerDistancePart = integerDistancePart;
    }
    
    /**
     * Creates a new AlgorithmHurstIndex object for color image.
     *
     * @param              destImg            image model where result image is to stored
     * @param              srcImg             source image model
     * @param              RGBOffset          selects red, green, or blue channel
     * @param              minDist            minimum distance between pixels
     * @param              maxDist            maximum distance between pixels
     * @param              integerDistancePart   If true, take the integer part of the Euclidean distance as the distance.
     *                                           if false, take the Euclidean distance as the distance.
     */
    public AlgorithmHurstIndex(ModelImage destImg, ModelImage srcImg, int RGBOffset, 
                                    double minDist, double maxDist, boolean integerDistancePart) {
        super(destImg, srcImg);
        this.RGBOffset = RGBOffset;
        this.minDist = minDist;
        this.maxDist = maxDist;
        this.integerDistancePart = integerDistancePart;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");
            finalize();


            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        

        fireProgressStateChanged(0, null, "Running Hurst index ...");
        
        calculateHurstIndex();
    }

    /**
     * DOCUMENT ME!
     */
    private void calculateHurstIndex() {
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int zDim;
        double xDiff;
        double yDiff;
        double xDiffSquared;
        double yDiffSquared;
        double distance;
        double absIntensityDiff;
        double[] sourceBuffer = new double[sliceSize];
        float[] floatBuffer;
        int x, y;
        int i, j;
        int index;
        int pos;
        int newPos;
        int z;
        int iStart;
        int iFinish;
        int jStart;
        int jFinish;
        int numDistances;
        double distanceArray[];
        double intensityArray[];
        int numAtIndex;
        double sumDistance;
        double sumDistanceSquared;
        double sumIntensity;
        double sumDistanceIntensity;
        double hurstIndex;
        double hurstBuffer[];
        
        if (srcImage.getNDims() == 2) {
            zDim = 1;
        }
        else {
            zDim = srcImage.getExtents()[2];
        }


        for (z = 0; z < zDim; z++) {
        try {
            if (srcImage.isColorImage()) {
                floatBuffer = new float[sliceSize];
                srcImage.exportRGBData(RGBOffset, 4*z*sliceSize, sliceSize, floatBuffer);  
                for (i = 0; i < sliceSize; i++) {
                    sourceBuffer[i] = (double)floatBuffer[i];
                }
                floatBuffer = null;
            }
            else {
                srcImage.exportData(z*sliceSize, sliceSize, sourceBuffer);
            }
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmHurstIndex: IOException on srcImage.exportData(0,sliceSize,sourceBuffer)");
            setCompleted(false);

            return;
        }
        hurstBuffer = new double[sliceSize];
        
        
        List<DistanceIntensity> list = new ArrayList<DistanceIntensity>();
        for (y = 0; (y <= yDim-1) && !threadStopped; y++) {
            
            fireProgressStateChanged(((int)((z * 100.0f/zDim) + (y * (100.0f / (zDim*(yDim-1)))))), null, null);
            jStart = Math.max(0, (int)(y - maxDist));
            jFinish = Math.min(yDim-1, (int)Math.ceil(y + maxDist));
            
            for (x = 0; x <= xDim-1; x++) {
                pos = x + (y * xDim);
                iStart = Math.max(0, (int)(x - maxDist));
                iFinish = Math.min(xDim-1, (int)Math.ceil(x + maxDist));
                list.clear();
                for (j = jStart; j <= jFinish; j++) {
                    yDiff = j - y;
                    yDiffSquared = yDiff*yDiff;
                    for (i = iStart; i <= iFinish; i++) {
                        xDiff = i - x;
                        xDiffSquared = xDiff * xDiff;
                        distance = Math.sqrt(xDiffSquared + yDiffSquared);
                        if (integerDistancePart) {
                            distance = Math.floor(distance);
                        }
                        if ((distance >= minDist) && (distance <= maxDist)) {
                            newPos = i + (j * xDim);
                            absIntensityDiff = Math.abs(sourceBuffer[newPos] - sourceBuffer[pos]);
                            list.add(new DistanceIntensity(distance, absIntensityDiff));
                        } // if ((distance >= minDist) && (distance <= maxDist))
                    } // for (i = iStart; i <= iFinish; i++)
                } // for (j = jStart; j <= jFinish; j++)
                Collections.sort(list, new DistanceIntensityComparator());
                numDistances = 1;
                for (i = 1; i < list.size(); i++) {
                    if (list.get(i).getDistance() > list.get(i-1).getDistance()) {
                        numDistances++;
                    }
                }
                distanceArray = new double[numDistances];
                intensityArray = new double[numDistances];
                distanceArray[0] = list.get(0).getDistance();
                intensityArray[0] = list.get(0).getIntensity();
                index = 0;
                numAtIndex = 1;
                for (i = 1; i < list.size(); i++) {
                    if (list.get(i).getDistance() > list.get(i-1).getDistance()) {
                        intensityArray[index] = intensityArray[index]/numAtIndex;
                        index++;
                        numAtIndex = 1;
                        distanceArray[index] = list.get(i).getDistance();
                        intensityArray[index] = list.get(i).getIntensity();
                    }
                    else {
                        intensityArray[index] += list.get(i).getIntensity();
                        numAtIndex++;
                    }
                } // for (i = 1; i < list.size(); i++)
                intensityArray[index] = intensityArray[index]/numAtIndex;
                
                sumDistance = 0.0;
                sumDistanceSquared = 0.0;
                sumIntensity = 0.0;
                sumDistanceIntensity = 0.0;
                for (i = 0; i < numDistances; i++) {
                    distanceArray[i] = Math.log(distanceArray[i]);
                    intensityArray[i] = Math.log(intensityArray[i]);
                    sumDistance += distanceArray[i];
                    sumDistanceSquared += distanceArray[i]*distanceArray[i];
                    sumIntensity += intensityArray[i];
                    sumDistanceIntensity += distanceArray[i]*intensityArray[i];
                }
                hurstIndex = (sumDistanceIntensity - sumDistance*sumIntensity/numDistances)/
                        (sumDistanceSquared - sumDistance*sumDistance/numDistances);
                hurstBuffer[pos] = hurstIndex;
            } // for (x = 0; x <= xDim-1; x++)
        } // for (y = 0; (y <= yDim-1) && !threadStopped; y++)
        
        

        if (threadStopped) {
            finalize();

            return;
        }
        
        try {
            destImage.importData(z*sliceSize, hurstBuffer, false);
        } catch (IOException error) {
            MipavUtil.displayError("AlgorithmHurstIndex: IOException on destImage.importData(0,hurstBuffer,false)");
            setCompleted(false);

            return;
        }

       
        } // for (z = 0; z < zDim; z++)
        destImage.calcMinMax();
        
       
        setCompleted(true);

        return;
    }
    
    class DistanceIntensity {
        private double distance;
        private double intensity;
        
        public DistanceIntensity(double distance, double intensity) {
            this.distance = distance;
            this.intensity = intensity;
        }
        
        double getDistance() {
            return distance;
        }
        
        double getIntensity() {
            return intensity;
        }
    }
    
    class DistanceIntensityComparator implements Comparator<DistanceIntensity> {
        public int compare(DistanceIntensity di1, DistanceIntensity di2) {
            if (di1.getDistance() > di2.getDistance()) {
                return 1;
            }
            else if (di1.getDistance() < di2.getDistance()) {
                return -1;
            }
            else if (di1.getIntensity() > di2.getIntensity()) {
                return 1;
            }
            else if (di1.getIntensity() < di2.getIntensity()) {
                return -1;
            }
            else {
                return 0;
            }
        }
    }

}
