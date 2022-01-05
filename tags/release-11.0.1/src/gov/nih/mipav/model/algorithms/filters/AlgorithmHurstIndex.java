package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;


/**
 * DOCUMENT ME!
 *
 * @version  0.1 June 25, 2012
 * @author   William Gandler 

 * In 2D for image points find the absolute value of the intensity differences to all points 
 * between a minimum distance of minDist and a maximum distance of maxDist. Do this either for all points
 * in the image or only between points in a given contour or polyline curve.  If done for all points in an
 * image, the Hurst image will be produced as a double value in a result image having the same dimensions
 * as the source image.  If done only for VOIs, there will be no result image.  If done only for VOIs, there
 * will just be a Hurst value outputted on the data panel for each contour or polyline curve.  Have the option
 * to define the distance as the rounded Euclidean distance.  For every distance between minDist and maxDist
 * find the average value of the absolute value of the intensity difference.  Find the best fit to the line
 * log(average absolute value of the intensity difference) = log(c) + H*log(distance) where H is the Hurst index.
 * 
 * For VOIs and slices the Hurst index varies from 0 to 1.  If H is 0.5, each step can be up or down completely at random. 
 * If H is less than 0.5, each step upward is likely to be followed by a downward step and each downward step is likely
 * to be followed by an upward step.  Therefore, a lower Hurst index indicates a higher complexity of the
 * distribution of pixel values.
 * 
 * Dimensionality = 3 - Hurst coefficient.
 * 
 * The fractal dimension has lower values on the boundary of different texture regions, and tends to be
 * constant over uniform texture regions.
 * 
 * References: 1.) "Sonographic Texture Characterization of Salivary  Gland Tumors by Fractal Analysis",
 * by Toru Chikui, Kenji Tokumori, Kazunori Yoshiura, Kazunari Oobu, Seiji Nakamura, and Katsumasa Nakamura,
 * Ultrasound in Medicine and Biology, Vol. 31, No. 10, pp. 1297-1304, 2005.
 * 
 * 2.) "Fractal Analysis of Medical Images in the Irregular Regions of Interest", by Edward Oczeretko, 
 * Marta Borowska, Agnieszka Kitlas, Andrzej Borusiewicz, and Malgorzata Sobolewska-Siemieniuk.
 * 
 * 3.) "Fractal Feature Analysis and Classification in Medical Imaging", by Chi-Chang Chen, John S. Daponte,
 * and Martin D. Fox, IEEE Transactions on Medical Imaging, Vol. 8, No. 2, June, 1989, pp. 133-142.
 * 
 */
public class AlgorithmHurstIndex extends AlgorithmBase {
    
    /** Red channel. */
    private static final int RED_OFFSET = 1;
    
    private static final int PIXEL_GROUPING = 1;
    
    private static final int VOI_GROUPING = 2;
    
    //private static final int SLICE_GROUPING = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private double minDist;
    
    private double maxDist;
    
    // If true, take the rounding of the Euclidean distnace as the distance
    // If false, take the Euclidean distance as the distance
    private boolean integerDistanceRound;
    
    
    private int RGBOffset = RED_OFFSET; 
    
    private int grouping = PIXEL_GROUPING;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    
    /**
     * Creates a new AlgorithmHurstIndex object for black and white image.
     *
     * @param              destImg            destination image model
     * @param              srcImg             source image model
     * @param              minDist            minimum distance between pixels
     * @param              maxDist            maximum distance between pixels
     * @param              integerDistanceRound   If true, take the rounding of the Euclidean distance as the distance.
     *                                           if false, take the Euclidean distance as the distance.
     */
    public AlgorithmHurstIndex(ModelImage destImg, ModelImage srcImg, double minDist, double maxDist, boolean integerDistanceRound) {
        super(destImg, srcImg);
        this.minDist = minDist;
        this.maxDist = maxDist;
        this.integerDistanceRound = integerDistanceRound;
    }
    
    /**
     * Creates a new AlgorithmHurstIndex object for color image.
     *
     * @param              destImg            destination image model
     * @param              srcImg             source image model
     * @param              RGBOffset          selects red, green, or blue channel
     * @param              minDist            minimum distance between pixels
     * @param              maxDist            maximum distance between pixels
     * @param              integerDistancePart   If true, take the integer part of the Euclidean distance as the distance.
     *                                           if false, take the Euclidean distance as the distance.
     */
    public AlgorithmHurstIndex(ModelImage destImg, ModelImage srcImg, int RGBOffset, 
                                    double minDist, double maxDist, boolean integerDistanceRound) {
        super(destImg, srcImg);
        this.RGBOffset = RGBOffset;
        this.minDist = minDist;
        this.maxDist = maxDist;
        this.integerDistanceRound = integerDistanceRound;
    }

    /**
     * Creates a new AlgorithmHurstIndex object for black and white image.
     *
     * @param              srcImg             source image model
     * @param              minDist            minimum distance between pixels
     * @param              maxDist            maximum distance between pixels
     * @param              integerDistanceRound   If true, take the rounding of the Euclidean distance as the distance.
     *                                           if false, take the Euclidean distance as the distance.
     * @param              grouping           VOI_GROUPING or SLICE_GROUPING
     */
    public AlgorithmHurstIndex(ModelImage srcImg, double minDist, double maxDist, boolean integerDistanceRound,
                               int grouping) {
        super(null, srcImg);
        this.minDist = minDist;
        this.maxDist = maxDist;
        this.integerDistanceRound = integerDistanceRound;
        this.grouping = grouping;
    }
    
    /**
     * Creates a new AlgorithmHurstIndex object for color image.
     *
     * @param              srcImg             source image model
     * @param              RGBOffset          selects red, green, or blue channel
     * @param              minDist            minimum distance between pixels
     * @param              maxDist            maximum distance between pixels
     * @param              integerDistancePart   If true, take the integer part of the Euclidean distance as the distance.
     *                                           if false, take the Euclidean distance as the distance.
     * @param              grouping           VOI_GROUPING or SLICE_GROUPING
     */
    public AlgorithmHurstIndex(ModelImage srcImg, int RGBOffset, 
                                    double minDist, double maxDist, boolean integerDistanceRound, int grouping) {
        super(null, srcImg);
        this.RGBOffset = RGBOffset;
        this.minDist = minDist;
        this.maxDist = maxDist;
        this.integerDistanceRound = integerDistanceRound;
        this.grouping = grouping;
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
        
       if (destImage != null) {
            calculatePixelHurstIndex();
       }
       else if (grouping == VOI_GROUPING){
           calculateVOIHurstIndex();
       }
       else {
           calculateSliceHurstIndex();
       }
    }

    /**
     * DOCUMENT ME!
     */
    private void calculatePixelHurstIndex() {
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
        
        hurstBuffer = new double[sliceSize];  
        List<DistanceIntensity> list = new ArrayList<DistanceIntensity>();  

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
                if (srcImage.isColorImage()) {
                    MipavUtil.displayError(
                    "AlgorithmHurstIndex: IOException on srcImage.exportRGBData(RGBOffset,4*z*sliceSize,sliceSize,floatBuffer)");
                }
                else {
                    MipavUtil.displayError(
                    "AlgorithmHurstIndex: IOException on srcImage.exportData(z*sliceSize,sliceSize,sourceBuffer)");    
                }
                setCompleted(false);
    
                return;
            }
            
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
                            if (integerDistanceRound) {
                                distance = Math.round(distance);
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
                            if (intensityArray[index] > 0.0) {
                                index++;
                            }
                            else {
                                numDistances--;
                            }
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
                    if (intensityArray[index] == 0.0) {
                        numDistances--;
                    }
                    
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
                MipavUtil.displayError("" +
                        "AlgorithmHurstIndex: IOException on destImage.importData(z*sliceSize, hurstBuffer, false)");
               setCompleted(false);

               return;
           }
           
        } // for (z = 0; z < zDim; z++)
               
        destImage.calcMinMax();
       
        setCompleted(true);

        return;
    }
    
    private void calculateVOIHurstIndex() {
        
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
        VOIVector voiVector;
        ModelImage maskImage;
        int vIndex;
        VOIBaseVector curves = null;
        int vIndex2Size;
        int vIndex2;
        int xBounds[] = new int[2];
        int yBounds[] = new int[2];
        int zBounds[] = new int[2];
        int zPos;
        int pos2;
        ViewUserInterface UI = ViewUserInterface.getReference();
        DecimalFormat kDecimalFormat = new DecimalFormat();
        kDecimalFormat.setMinimumFractionDigits(3);
        kDecimalFormat.setMaximumFractionDigits(3);
        
        if (srcImage.getNDims() == 2) {
            zDim = 1;
        }
        else {
            zDim = srcImage.getExtents()[2];
        }
        List<DistanceIntensity> list = new ArrayList<DistanceIntensity>();
        
        voiVector = srcImage.getVOIs();
        
        if (voiVector.size() == 0) {
            MipavUtil.displayError("No VOIs are present");
            setCompleted(false);
            return;
        }
        
        try {
            maskImage = new ModelImage(ModelStorageBase.SHORT, srcImage.getExtents(), "Short Image");
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory error reating maskImage");
            setCompleted(false);
            return;    
        }
        
        for (vIndex = 0; vIndex < voiVector.size(); vIndex++) {
            VOI presentVOI = voiVector.elementAt(vIndex);
            if ((presentVOI.getCurveType() == VOI.CONTOUR) || (presentVOI.getCurveType() == VOI.POLYLINE)) {
                if (presentVOI.getName() != null) {
                    UI.setDataText("VOI name = " + presentVOI.getName() + "\n");
                }
                UI.setDataText("VOI ID = " + presentVOI.getID() + "\n");
                curves = presentVOI.getCurves();    
                vIndex2Size = curves.size();
                UI.setDataText("Number of elements in VOI = " + vIndex2Size + "\n");
                for (vIndex2 = 0; vIndex2 < vIndex2Size; vIndex2++) {
                    curves.get(vIndex2).getBounds(xBounds, yBounds, zBounds);
                    UI.setDataText("Element # " + vIndex2 + "\n");
                    if (curves.get(vIndex2).getLabel() != null) {
                        UI.setDataText("Element label = " + curves.get(vIndex2).getLabel() + "\n");
                    }
                    if (curves.get(vIndex2).getName() != null) {
                        UI.setDataText("Element name = " + curves.get(vIndex2).getName() + "\n");
                    }
                    maskImage.clearMask();
                    
                    (voiVector.elementAt(vIndex)).createOneElementBinaryMask3D(maskImage.getMask(), xDim, yDim, false, false, vIndex2);

                    BitSet mask = maskImage.getMask();

                    for (z = zBounds[0]; z <= zBounds[1]; z++) {
                    zPos = z * sliceSize;
                    list.clear();
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
                        if (srcImage.isColorImage()) {
                            MipavUtil.displayError(
                            "AlgorithmHurstIndex: IOException on srcImage.exportRGBData(RGBOffset,4*z*sliceSize,sliceSize,floatBuffer)");
                        }
                        else {
                            MipavUtil.displayError(
                            "AlgorithmHurstIndex: IOException on srcImage.exportData(z*sliceSize,sliceSize,sourceBuffer)");    
                        }
                        setCompleted(false);
            
                        return;
                    } 
                    
                    for (y = yBounds[0]; (y <= yBounds[1]) && !threadStopped; y++) {
                        
                        fireProgressStateChanged(((int)((z * 100.0f/zDim) + (y * (100.0f / (zDim*(yDim-1)))))), null, null);
                        jStart = Math.max(yBounds[0], (int)(y - maxDist));
                        jFinish = Math.min(yBounds[1], (int)Math.ceil(y + maxDist));
                        
                        for (x = xBounds[0]; x <= xBounds[1]; x++) {
                            pos = x + (y * xDim);
                            if (mask.get(zPos + pos)) {
                                iStart = Math.max(xBounds[0], (int)(x - maxDist));
                                iFinish = Math.min(xBounds[1], (int)Math.ceil(x + maxDist));
                                
                                for (j = jStart; j <= jFinish; j++) {
                                    yDiff = j - y;
                                    yDiffSquared = yDiff*yDiff;
                                    for (i = iStart; i <= iFinish; i++) {
                                        pos2 = i + (j * xDim);
                                        if (mask.get(zPos + pos2)) {
                                            xDiff = i - x;
                                            xDiffSquared = xDiff * xDiff;
                                            distance = Math.sqrt(xDiffSquared + yDiffSquared);
                                            if (integerDistanceRound) {
                                                distance = Math.round(distance);
                                            }
                                            if ((distance >= minDist) && (distance <= maxDist)) {
                                                absIntensityDiff = Math.abs(sourceBuffer[pos2] - sourceBuffer[pos]);
                                                list.add(new DistanceIntensity(distance, absIntensityDiff));
                                            } // if ((distance >= minDist) && (distance <= maxDist))
                                        } // if (mask.get(zPos + pos2))
                                    } // for (i = iStart; i <= iFinish; i++)
                                } // for (j = jStart; j <= jFinish; j++)
                            } // if (mask.get(zPos + pos)
                        } // for (x = 0; x <= xBounds[1]; x++)
                    } // for (y = 0; (y <= yBounds[1]) && !threadStopped; y++)
                    
                    
            
                    if (threadStopped) {
                        finalize();
            
                        return;
                    }
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
                            if (intensityArray[index] > 0.0) {
                                index++;
                            }
                            else {
                                numDistances--;
                            }
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
                    if (intensityArray[index] == 0.0) {
                        numDistances--;
                    }
                    
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
                    UI.setDataText("Slice number = " + z + " Hurst Index = " + 
                            kDecimalFormat.format(hurstIndex) + " Dimensionality = " + 
                            kDecimalFormat.format(3.0 - hurstIndex) + "\n");
                    } // for (z = zBounds[0]; z <= zBounds[1]; z++)
                } // for (vIndex2 = 0; vIndex2 < vIndex2Size; vIndex2++)
            } // if ((presentVOI.getCurveType() == VOI.CONTOUR) || (presentVOI.getCurveType() == VOI.POLYLINE))
        } // for (vIndex = 0; vIndex < voiVector.size(); vIndex++)
        
       
        setCompleted(true);

        return;
        
    }
    
    private void calculateSliceHurstIndex() {
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
        int pos2;
        ViewUserInterface UI = ViewUserInterface.getReference();
        DecimalFormat kDecimalFormat = new DecimalFormat();
        kDecimalFormat.setMinimumFractionDigits(3);
        kDecimalFormat.setMaximumFractionDigits(3);
        
        if (srcImage.getNDims() == 2) {
            zDim = 1;
        }
        else {
            zDim = srcImage.getExtents()[2];
        }
        List<DistanceIntensity> list = new ArrayList<DistanceIntensity>();  

        for (z = 0; z < zDim; z++) {
            list.clear();
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
                if (srcImage.isColorImage()) {
                    MipavUtil.displayError(
                    "AlgorithmHurstIndex: IOException on srcImage.exportRGBData(RGBOffset,4*z*sliceSize,sliceSize,floatBuffer)");
                }
                else {
                    MipavUtil.displayError(
                    "AlgorithmHurstIndex: IOException on srcImage.exportData(z*sliceSize,sliceSize,sourceBuffer)");    
                }
                setCompleted(false);
    
                return;
            } 
            
            for (y = 0; (y < yDim) && !threadStopped; y++) {
                
                fireProgressStateChanged(((int)((z * 100.0f/zDim) + (y * (100.0f / (zDim*(yDim-1)))))), null, null);
                jStart = Math.max(0, (int)(y - maxDist));
                jFinish = Math.min(yDim-1, (int)Math.ceil(y + maxDist));
                
                for (x = 0; x < xDim; x++) {
                    pos = x + (y * xDim);
                    iStart = Math.max(0, (int)(x - maxDist));
                    iFinish = Math.min(xDim-1, (int)Math.ceil(x + maxDist));
                    
                    for (j = jStart; j <= jFinish; j++) {
                        yDiff = j - y;
                        yDiffSquared = yDiff*yDiff;
                        for (i = iStart; i <= iFinish; i++) {
                            pos2 = i + (j * xDim);
                            xDiff = i - x;
                            xDiffSquared = xDiff * xDiff;
                            distance = Math.sqrt(xDiffSquared + yDiffSquared);
                            if (integerDistanceRound) {
                                distance = Math.round(distance);
                            }
                            if ((distance >= minDist) && (distance <= maxDist)) {
                                absIntensityDiff = Math.abs(sourceBuffer[pos2] - sourceBuffer[pos]);
                                list.add(new DistanceIntensity(distance, absIntensityDiff));
                            } // if ((distance >= minDist) && (distance <= maxDist))
                        } // for (i = iStart; i <= iFinish; i++)
                    } // for (j = jStart; j <= jFinish; j++)
                } // for (x = 0; x < xDim; x++)
            } // for (y = 0; (y < yDim) && !threadStopped; y++)
            
            
    
            if (threadStopped) {
                finalize();
    
                return;
            }
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
                    if (intensityArray[index] > 0.0) {
                        index++;
                    }
                    else {
                        numDistances--;
                    }
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
            if (intensityArray[index] == 0.0) {
                numDistances--;
            }
            
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
            UI.setDataText("Slice number = " + z + " Hurst Index = " + 
                    kDecimalFormat.format(hurstIndex) + " Dimensionality = " + 
                    kDecimalFormat.format(3.0 - hurstIndex) + "\n");
        } // for (z = 0; z < zDim; z++)
               
       
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
