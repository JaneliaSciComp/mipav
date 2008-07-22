import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 *
 * @version  July 22, 2008
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmFociAndStrands.java $ $Revision: 72 $ $Date: 2/06/06 5:50p $
 *           PlugInAlgorithmFociAndStrands is used to:
 *           1. Count the total number of foci(red dots) in the cell.
 *           2. Count the total number of foci that co-localize with the green strands.
 *           3. In cases where the strands are well separated and do not tangle, count the number of 
 *              foci per strand.
 *           4. Calculate the average number of foci per strand.
 *           5. For cases where there are two foci per strand, measure the distance between the foci
 *              along the strand.</p>
 *           Use a contour VOI if you wish to process only a portion of the image.
 *           
 *           <p>1.) Obtain the red portion of the image.
 *            
 *           <p>2.) red threshold = red minimum + red fraction * (red maximum - red minimum)
 *           if (redBuffer[i] >= red threshold), set byteBuffer equal to 1.  Otherwise byteBuffer is 0.  
 *           
 *           <p>3.) ID objects in red thresholded image which have at least redMin pixels.
 *           
 *           <p>4.) Export the green portion of the image to greenBuffer.
 *
 *           <p>5.) green threshold = green minimum + green fraction * (green maximum - green minimum)
 *           if (greenBuffer[i] >= green threshold), set byteBuffer equal to 1.  Otherwise byteBuffer is 0.  
 *           
 *           <p>6.) ID objects in green thresholded image which have at least greenMin pixels
 *           
 *           <p>7.) For each red object find a green object, if any, with which it colocalizes.
 *           Calculate the center of mass of the red object.
 *           
 *           <p>8.) For each green strand store the number of red foci on it.  Store the positions of
 *           focus 1 and focus 2, if present.
 *           
 *           <p>9.) Display the positions of the colocalized red foci on the source image with circles.
 *           
 *           <p>10.) Remove all green strands except those with 2 foci.
 *           
 *           <p>11.) Skeletonize the green strands with 2 foci.
 *           
 *           <p>12.) Find the points on the skeletonized green strands nearest their 2 colocalized red foci.
 *           Use these points as the foci on the skeletonized green strands in the distance calculations.
 *           
 *           <p>13.) If a point is not a focus and has only 1 branch, then prune it.  Keep repeating the
 *           process until no more pruning can occur.
 */
public class PlugInAlgorithmFociAndStrands extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int BOTH_FUZZY_HARD = 0;

    /** DOCUMENT ME! */
    public static final int FUZZY_ONLY = 1;

    /** DOCUMENT ME! */
    public static final int HARD_ONLY = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Minimum number of pixels in a red focus */
    private int redMin = 1;

    /** Focus requires a minimum red value >=
     *  image red min + redFraction * (image red max - image red min)
     */
    private float redFraction = 0.30f;
    
    
    /** Strand requires a minimum green value >=
     *  image green min + greenFraction * (image green max - image green min)
     */
    private float greenFraction = 0.10f;

    /** Minimum number of pixels in green strand */
    private int greenMin = 50;
    
    private float interpolationDivisor = 24.0f;
    
    private ModelImage redImage;
    private ViewJFrameImage redFrame;
    private ModelImage greenImage;
    private ViewJFrameImage greenFrame;
    private ModelImage skeletonizedImage;
    private ViewJFrameImage skeletonizedFrame;
    private ModelImage prunedImage;
    private ViewJFrameImage prunedFrame;
    //** Radius of circles drawn around colocalized foci */
    private float radius;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg         Source image model.
     * @param  redMin
     * @param  redFraction
     * @param  greenMin
     * @param  greenFraction
     * @param  radius
     */
    public PlugInAlgorithmFociAndStrands(ModelImage srcImg, int redMin, float redFraction, int greenMin,
                                         float greenFraction, float radius) {
        super(null, srcImg);
        this.redMin = redMin;
        this.redFraction = redFraction;
        this.greenMin = greenMin;
        this.greenFraction = greenFraction;
        this.radius = radius;
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
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() > 2) {
            calc3D();
        }
    } // end runAlgorithm()

    /**
     * DOCUMENT ME!
     */
    private void calc2D() {

        BitSet imageMask = null;
        int redIDMinus1;
        short redOnGreen[];
        float redX[];
        float redY[];
        float redColocX1[];
        float redColocY1[];
        float redColocX2[];
        float redColocY2[];
        float redTotal[];
        int numRedColocalize;
        int length; // total number of data-elements (pixels) in image
        float[] redBuffer;
        ModelImage grayImage;
        float threshold;
        boolean wholeImage;
        int kernel;
        float circleDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        int i, j, k;
        int x, y;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        AlgorithmMorphology2D idObjectsAlgo2D;
        AlgorithmMorphology2D skeletonizeAlgo2D;
        byte[] byteBuffer;
        float[] greenBuffer = null;
        ViewVOIVector VOIs = null;
        int nVOIs;
        int index;
        ViewUserInterface UI = ViewUserInterface.getReference();
        VOI newPtVOI;
        VOI newCircleVOI;
        int maxCirclePoints = (int)Math.ceil(2.0 * Math.PI * radius);
        float[] xArr = new float[maxCirclePoints];
        float[] yArr = new float[maxCirclePoints];
        float[] zArr = new float[maxCirclePoints];
        double theta;
        FileInfoBase fileInfo;
        int numGreenObjects = 0;
        short[] greenIDArray = null;
        short[] skeletonizedArray = null;
        int numGreenStrandFoci[];
        int greenIDMinus1;
        float avgFociOnStrands;
        int greenStrandFocus1[];
        int greenStrandFocus2[];
        int greenLeft[];
        int greenRight[];
        int greenTop[];
        int greenBottom[];
        double distX;
        double distY;
        double distanceSq;
        double minDistanceSq1;
        double minDistanceSq2;
        int redSkelX[];
        int redSkelY[];
        int xInt[] = new int[1];
        int yInt[] = new int[1];
        int zInt[] = new int[1];
        int numRedColocalize2;
        float resX = srcImage.getFileInfo()[0].getResolutions()[0];
        float resY = srcImage.getFileInfo()[0].getResolutions()[1];
        double resDiag = Math.sqrt(resX*resX + resY*resY);
        boolean branchPruned;
        int redSkelIndex1;
        int redSkelIndex2;
        int numBranches;
        boolean xplus;
        boolean xminus;
        boolean yplus;
        boolean yminus;
        boolean found[];
        double fociDistance[];
        boolean loop[];
        
        long time;
        int fileNameLength;
        Font courier = MipavUtil.courier12;
        DecimalFormat df = new DecimalFormat("0.000E0");
        DecimalFormat dfFract = new DecimalFormat("0.000");

        time = System.currentTimeMillis();
        
        // image length is length in 2 dims
        length = xDim * yDim;
        redBuffer = new float[length];
        byteBuffer = new byte[length];

        fireProgressStateChanged("Processing image ...");

        fireProgressStateChanged("Creating red image");
        fireProgressStateChanged(30);
        try {
            srcImage.exportRGBData(1, 0, length, redBuffer); // export red data
        } catch (IOException error) {
            redBuffer = null;
            greenBuffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        }

        grayImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), srcImage.getImageName() + "_gray");
        fileInfo = grayImage.getFileInfo()[0];
        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        grayImage.setFileInfo(fileInfo, 0);
        
        threshold = (float)(srcImage.getMinR() + redFraction * (srcImage.getMaxR() - srcImage.getMinR()));
        Preferences.debug("red threshold = " + threshold + "\n");
        
        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        wholeImage = true;
        for (i = 0; i < nVOIs; i++) {
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                imageMask = srcImage.generateVOIMask();
                wholeImage = false;
                break;
            }
        }
        
        for (i = 0; i < length; i++) {
            if ((wholeImage || imageMask.get(i)) && (redBuffer[i] >= threshold)) {
                byteBuffer[i] = 1;
            }
            else {
                byteBuffer[i] = 0;
            }
        }
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }
        
        // Put the red objects in redIDArray
        // Run on entire image since the region outside the voi has
        // had byteBuffer set equal to zero.
        fireProgressStateChanged("IDing objects in red segmented image");
        fireProgressStateChanged(44);
        kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
        circleDiameter = 0.0f;
        method = AlgorithmMorphology2D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        numPruningPixels = 0;
        edgingType = 0;  
        idObjectsAlgo2D = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, true);
        idObjectsAlgo2D.setMinMax(redMin, 200000);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;

        grayImage.calcMinMax();
        int numRedObjects = (int) grayImage.getMax();
        Preferences.debug("numRedObjects = " + numRedObjects + "\n");
        
        /*redImage = (ModelImage)grayImage.clone();
        redImage.setImageName(srcImage.getImageName() + "_red");
        redFrame = new ViewJFrameImage(redImage);
        redFrame.setTitle(srcImage.getImageName() + "_red");*/
        
        short [] redIDArray = new short[length];

        try {
            grayImage.exportData(0, length, redIDArray);
        } catch (IOException error) {
            byteBuffer = null;
            redIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        fireProgressStateChanged("Creating green image");
        fireProgressStateChanged(46);
        
        greenBuffer = new float[length];

        try {

            srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        }   
        
        threshold = (float)(srcImage.getMinG() + greenFraction * (srcImage.getMaxG() - srcImage.getMinG()));
        Preferences.debug("Green threshold = " + threshold + "\n");
        
        for (i = 0; i < length; i++) {
            if ((wholeImage || imageMask.get(i)) && (greenBuffer[i] >= threshold)) {
                byteBuffer[i] = 1;
            }
            else {
                byteBuffer[i] = 0;
            }
        }
        srcImage.clearMask();
        
        try {
            grayImage.importData(0, byteBuffer, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }
        
        // Put the green objects IDs in greenIDArray
        fireProgressStateChanged("IDing objects in green segmented image");
        fireProgressStateChanged(58);
        kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
        circleDiameter = 0.0f;
        method = AlgorithmMorphology2D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo2D = new AlgorithmMorphology2D(grayImage, kernel, circleDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, true);
        idObjectsAlgo2D.setMinMax(greenMin, 10000);
        idObjectsAlgo2D.run();
        idObjectsAlgo2D.finalize();
        idObjectsAlgo2D = null;

        grayImage.calcMinMax();
        numGreenObjects = (int) grayImage.getMax();
        Preferences.debug("numGreenObjects = " + numGreenObjects + "\n");
        greenIDArray = new short[length];
        
        greenImage = (ModelImage)grayImage.clone();
        greenImage.setImageName(srcImage.getImageName() + "_green");
        greenFrame = new ViewJFrameImage(greenImage);
        greenFrame.setTitle(srcImage.getImageName() + "_green");

        try {
            grayImage.exportData(0, length, greenIDArray);
        } catch (IOException error) {
            byteBuffer = null;
            greenIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        redOnGreen = new short[numRedObjects];
        redX = new float[numRedObjects];
        redY = new float[numRedObjects];
        redTotal = new float[numRedObjects];
        numGreenStrandFoci = new int[numGreenObjects];
        greenStrandFocus1 = new int[numGreenObjects];
        greenStrandFocus2 = new int[numGreenObjects];
        for (y = 0; y < yDim; y++) {
            index = y * xDim;
            for (x = 0; x < xDim; x++) {
                i = index + x;
                if (redIDArray[i] != 0) {
                    redIDMinus1 = redIDArray[i] - 1;
                    redX[redIDMinus1] += x * redBuffer[i];
                    redY[redIDMinus1] += y * redBuffer[i];
                    redTotal[redIDMinus1] += redBuffer[i];
                    if (redOnGreen[redIDMinus1] == 0) {
                        if (greenIDArray[i] != 0) {
                            redOnGreen[redIDMinus1] = greenIDArray[i];
                        }
                        else if ((x >= 1) && (greenIDArray[i-1] != 0)) {
                            redOnGreen[redIDMinus1] = greenIDArray[i-1];
                        }
                        else if ((x <= xDim - 2) && (greenIDArray[i+1] != 0)) {
                            redOnGreen[redIDMinus1] = greenIDArray[i+1];
                        }
                        else if ((y >= 1) && (greenIDArray[i - xDim] != 0)) {
                            redOnGreen[redIDMinus1] = greenIDArray[i-xDim];
                        }
                        else if ((y <= yDim - 2) && (greenIDArray[i + xDim] != 0)) {
                            redOnGreen[redIDMinus1] = greenIDArray[i+xDim];
                        }
                    }
                }
            }
        }
        numRedColocalize = 0;
        redColocX1 = new float[numGreenObjects];
        redColocY1 = new float[numGreenObjects];
        redColocX2 = new float[numGreenObjects];
        redColocY2 = new float[numGreenObjects];
        for (i = 0; i < numRedObjects; i++) {
            redX[i] = redX[i]/redTotal[i];
            redY[i] = redY[i]/redTotal[i];
            if (redOnGreen[i] != 0) {
                greenIDMinus1 = redOnGreen[i] - 1;
                numGreenStrandFoci[greenIDMinus1]++;
                if (numGreenStrandFoci[greenIDMinus1] == 1) {
                    greenStrandFocus1[greenIDMinus1] = numRedColocalize + 1;
                    redColocX1[greenIDMinus1] = redX[i];
                    redColocY1[greenIDMinus1] = redY[i];
                }
                else if (numGreenStrandFoci[greenIDMinus1] == 2) {
                    greenStrandFocus2[greenIDMinus1] = numRedColocalize + 1;
                    redColocX2[greenIDMinus1] = redX[i];
                    redColocY2[greenIDMinus1] = redY[i];
                }
                //newPtVOI = new VOI((short) (numRedColocalize + nVOIs), Integer.toString(numRedColocalize+1), 1, VOI.POINT, -1.0f);
                //newPtVOI.setColor(Color.white);
                //xArr[0] = redX[i];
                //yArr[0] = redY[i];
                //zArr[0] = 0.0f;
                //newPtVOI.importCurve(xArr, yArr, zArr, 0);
                //((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
                //((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setLabel(Integer.toString(numRedColocalize + 1));
                //srcImage.registerVOI(newPtVOI);
                newCircleVOI = new VOI((short) (numRedColocalize + nVOIs), Integer.toString(numRedColocalize+1),
                                        1, VOI.CONTOUR, -1.0f);
                newCircleVOI.setColor(Color.white);
                for (j = 0; j < maxCirclePoints; j++) {
                    theta = j * 2.0 * Math.PI/maxCirclePoints;  
                    xArr[j] = (float)(redX[i] + radius * Math.cos(theta));
                    yArr[j] = (float)(redY[i] + radius * Math.sin(theta));
                    zArr[j] = 0.0f;
                }
                newCircleVOI.importCurve(xArr, yArr, zArr, 0);
                ((VOIContour)(newCircleVOI.getCurves()[0].elementAt(0))).setFixed(true);
                newCircleVOI.setActive(false);
                ((VOIContour)(newCircleVOI.getCurves()[0].elementAt(0))).setActive(false);
                ((VOIContour)(newCircleVOI.getCurves()[0].elementAt(0))).setDoGeometricCenterLabel(true);
                ((VOIContour)(newCircleVOI.getCurves()[0].elementAt(0))).setClosed(true);
                ((VOIContour) (newCircleVOI).getCurves()[0].elementAt(0)).setLabel(Integer.toString(numRedColocalize + 1));
                ((VOIContour) (newCircleVOI.getCurves()[0].elementAt(0))).setName(Integer.toString(numRedColocalize + 1));
                srcImage.registerVOI(newCircleVOI);
                numRedColocalize++;
            }
        }
        Preferences.debug(numRedColocalize + " of " + numRedObjects + " red foci co-localize with green strands\n");
        
        numRedColocalize2 = 0;
        for (i = 0; i < numGreenObjects; i++) {
          if (numGreenStrandFoci[i] == 2) {
              numRedColocalize2 += 2;
          }
        }
        Preferences.debug(numRedColocalize2 + " of red foci are on green strands with 2 red foci\n");
        
        avgFociOnStrands = (float)numRedColocalize/(float)numGreenObjects;
        
        // Remove all green strands but those with 2 foci
        for (y = 0; y < yDim; y++) {
            index = y * xDim;
            for (x = 0; x < xDim; x++) {
                i = index + x;
                if (greenIDArray[i] != 0) {
                    greenIDMinus1 = greenIDArray[i] - 1;
                    if (numGreenStrandFoci[greenIDMinus1] != 2) {
                        greenIDArray[i] = 0;
                    }
                }
            }
        }
        
        try {
            grayImage.importData(0, greenIDArray, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }
        
        // Skeletonize the green strands with 2 foci
        method = AlgorithmMorphology2D.SKELETONIZE;
        numPruningPixels = 0;
        skeletonizeAlgo2D = new AlgorithmMorphology2D(grayImage, 0, 0.0f, method, 0,
                0, numPruningPixels, 0, true);
        skeletonizeAlgo2D.run();
        skeletonizeAlgo2D.finalize();
        skeletonizeAlgo2D = null;
        
        // Find the points on the skeletonized green strands nearest their 2 colocalized red foci
        skeletonizedArray = new short[length];
        try {
            grayImage.exportData(0, length, skeletonizedArray);
        } catch (IOException error) {
            byteBuffer = null;
            greenIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        greenLeft = new int[numGreenObjects];
        greenRight = new int[numGreenObjects];
        greenTop = new int[numGreenObjects];
        greenBottom = new int[numGreenObjects];
        for (i = 0; i < numGreenObjects; i++) {
            greenLeft[i] = xDim - 1;
            greenTop[i] = yDim - 1;
        }
        for (y = 0; y < yDim; y++) {
            index = y * xDim;
            for (x = 0; x < xDim; x++) {
                i = index + x;
                if (skeletonizedArray[i] != 0) {
                    greenIDMinus1 = skeletonizedArray[i] - 1;
                    if (numGreenStrandFoci[greenIDMinus1] == 2) {
                        if (x < greenLeft[greenIDMinus1]) {
                            greenLeft[greenIDMinus1] = x;
                        }
                        if (x > greenRight[greenIDMinus1]) {
                            greenRight[greenIDMinus1] = x;
                        }
                        if (y < greenTop[greenIDMinus1]) {
                            greenTop[greenIDMinus1] = y;
                        }
                        if (y > greenBottom[greenIDMinus1]) {
                            greenBottom[greenIDMinus1] = y;
                        } 
                    }
                }
            }
        }
        
        skeletonizedImage = (ModelImage)grayImage.clone();
        skeletonizedImage.setImageName(srcImage.getImageName() + "_skeletonized");
        skeletonizedFrame = new ViewJFrameImage(skeletonizedImage);
        skeletonizedFrame.setTitle(srcImage.getImageName() + "_skeletonized");
        
        redSkelX = new int[numRedColocalize2];
        redSkelY = new int[numRedColocalize2];
        for (i = 0, k = 0; i < numGreenObjects; i++) {
            minDistanceSq1 = Double.MAX_VALUE;
            minDistanceSq2 = Double.MAX_VALUE;
            if (numGreenStrandFoci[i] == 2) {
                for (y = greenTop[i]; y <= greenBottom[i]; y++) {
                    index = y * xDim;
                    for (x = greenLeft[i]; x <= greenRight[i]; x++) {
                        j = index + x;
                        if (skeletonizedArray[j] == (i+1)) {
                            distX = redColocX1[i] - x;
                            distY = redColocY1[i] - y;
                            distanceSq = (distX * distX + distY * distY);
                            if (distanceSq < minDistanceSq1) {
                                minDistanceSq1 = distanceSq;
                                redSkelX[k] = x;
                                redSkelY[k] = y;
                            }
                            distX = redColocX2[i] - x;
                            distY = redColocY2[i] - y;
                            distanceSq = (distX * distX + distY * distY);
                            if (distanceSq < minDistanceSq2) {
                                minDistanceSq2 = distanceSq;
                                redSkelX[k+1] = x;
                                redSkelY[k+1] = y;
                            }
                        }
                    }
                }
                newPtVOI = new VOI((short) (greenStrandFocus1[i]), Integer.toString(greenStrandFocus1[i]), 1, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k];
                yInt[0] = redSkelY[k];
                zInt[0] = 0;
                newPtVOI.importCurve(xInt, yInt, zInt, 0);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setLabel(Integer.toString(greenStrandFocus1[i]));
                skeletonizedImage.registerVOI(newPtVOI);
                newPtVOI = new VOI((short) (greenStrandFocus2[i]), Integer.toString(greenStrandFocus2[i]), 1, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k+1];
                yInt[0] = redSkelY[k+1];
                zInt[0] = 0;
                newPtVOI.importCurve(xInt, yInt, zInt, 0);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setLabel(Integer.toString(greenStrandFocus2[i]));
                skeletonizedImage.registerVOI(newPtVOI);
                k += 2;
            } // if (numGreenStrandFoci[i] == 2)
        } // for (i = 0, k = 0; i < numGreenObjects; i++)
        
        // If a point is not a focus and has only 1 branch, then prune it.
        // Keep repeating the process until no more pruning can occur
        for (i = 0, k = 0; i < numGreenObjects; i++) {
            if (numGreenStrandFoci[i] == 2) {
                branchPruned = true;
                redSkelIndex1 = redSkelX[k] + xDim * redSkelY[k];
                redSkelIndex2 = redSkelX[k+1] + xDim * redSkelY[k+1];
                while(branchPruned) {
                    branchPruned = false; 
                    for (y = greenTop[i]; y <= greenBottom[i]; y++) {
                        index = y * xDim;
                        for (x = greenLeft[i]; x <= greenRight[i]; x++) {
                            j = index + x;
                            if ((skeletonizedArray[j] == (i+1)) && (j != redSkelIndex1) && (j != redSkelIndex2)) {
                                numBranches = 0;
                                xplus = false;
                                xminus = false;
                                yplus = false;
                                yminus = false;
                                if (x > 0) {
                                    if (skeletonizedArray[j-1] == (i+1)) {
                                        numBranches++;
                                        xminus = true;
                                    }
                                } // if (x > 0)
                                if (x < xDim - 1) {
                                    if (skeletonizedArray[j+1] == (i+1)) {
                                        numBranches++;
                                        xplus = true;
                                    }
                                } // if (x < xDim - 1)
                                if (y > 0) {
                                    if (skeletonizedArray[j - xDim] == (i+1)) {
                                        numBranches++;
                                        yminus = true;
                                    }
                                } // if ( y > 0)
                                if (y < yDim - 1) {
                                    if (skeletonizedArray[j + xDim] == (i+1)) {
                                        numBranches++;
                                        yplus = true;
                                    }    
                                } // if (y < yDim - 1)
                                if ((!xplus) && (!yplus)) {
                                    if (skeletonizedArray[j + xDim + 1] == (i+1)) {
                                        numBranches++;
                                    }
                                }
                                if ((!xplus) && (!yminus)) {
                                    if (skeletonizedArray[j - xDim + 1] == (i+1)) {
                                        numBranches++;
                                    }
                                }
                                if ((!xminus) && (!yplus)) {
                                    if (skeletonizedArray[j + xDim - 1] == (i+1)) {
                                        numBranches++;
                                    }
                                }
                                if ((!xminus) && (!yminus)) {
                                    if (skeletonizedArray[j - xDim - 1] == (i+1)) {
                                        numBranches++;
                                    }
                                }
                                if (numBranches == 1) {
                                    branchPruned = true;
                                    skeletonizedArray[j] = 0;
                                }
                            } // if ((skeletonizedArray[j] == (i+1)) && (j != redSkelIndex1) && (j != redSkelIndex2))
                        } // for (x = greenLeft[i]; x <= greenRight[i]; x++)
                    } // for (y = greenTop[i]; y <= greenBottom[i]; y++)
                } //  while(branchPruned)
                k += 2;
            } // if (numGreenStrandFoci[i] == 2)
        } // for (i = 0, k = 0; i < numGreenObjects; i++)
        
        try {
            grayImage.importData(0, skeletonizedArray, true);
        } catch (IOException error) {
            byteBuffer = null;
            greenIDArray = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }
        
        prunedImage = (ModelImage)grayImage.clone();
        prunedImage.setImageName(srcImage.getImageName() + "_pruned");
        prunedFrame = new ViewJFrameImage(prunedImage);
        prunedFrame.setTitle(srcImage.getImageName() + "_pruned");
        
        for (i = 0, k = 0; i < numGreenObjects; i++) {
            if (numGreenStrandFoci[i]  == 2) {
                newPtVOI = new VOI((short) (greenStrandFocus1[i]), Integer.toString(greenStrandFocus1[i]), 1, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k];
                yInt[0] = redSkelY[k];
                zInt[0] = 0;
                newPtVOI.importCurve(xInt, yInt, zInt, 0);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setLabel(Integer.toString(greenStrandFocus1[i]));
                prunedImage.registerVOI(newPtVOI);
                newPtVOI = new VOI((short) (greenStrandFocus2[i]), Integer.toString(greenStrandFocus2[i]), 1, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k+1];
                yInt[0] = redSkelY[k+1];
                zInt[0] = 0;
                newPtVOI.importCurve(xInt, yInt, zInt, 0);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setLabel(Integer.toString(greenStrandFocus2[i]));
                prunedImage.registerVOI(newPtVOI);
                k += 2;
            } // if (numGreenStrandFoci[i] == 2)
        } // for (i = 0, k = 0; i < numGreenObjects; i++)
        
        found = new boolean[length];
        fociDistance = new double[numRedColocalize2/2];
        loop = new boolean[numRedColocalize2/2];
        for (i = 0, k = 0; i < numGreenObjects; i++) {
            if (numGreenStrandFoci[i] == 2) {
                for (j = 0; j < length; j++) {
                    found[j] = false;
                }
                x = redSkelX[k];
                y = redSkelY[k];
                index = redSkelX[k] + xDim * redSkelY[k];
                found[index] = true;
                while ((x != redSkelX[k+1]) && (y != redSkelY[k+1])) {
                    if ((x > 0) && (skeletonizedArray[index - 1] == (i+1)) && (!found[index-1])) {
                        index = index - 1;
                        x = x - 1;
                        found[index] = true;
                        fociDistance[k/2] += resX;
                    }
                    else if ((x < xDim - 1) && (skeletonizedArray[index + 1] == (i+1)) && (!found[index+1])) {
                        index = index + 1;
                        x = x + 1;
                        found[index] = true;
                        fociDistance[k/2] += resX;
                    }
                    else if ((y > 0) && (skeletonizedArray[index - xDim] == (i+1)) && (!found[index-xDim])) {
                        index = index - xDim;
                        y = y - 1;
                        found[index] = true;
                        fociDistance[k/2] += resY;
                    }
                    else if ((y < yDim - 1) && (skeletonizedArray[index + xDim] == (i+1)) && (!found[index+xDim])) {
                        index = index + xDim;
                        y = y + 1;
                        found[index] = true;
                        fociDistance[k/2] += resY;
                    }
                    else if ((x > 0) && (y > 0) && (skeletonizedArray[index - xDim - 1] == (i+1)) && (!found[index-xDim-1])) {
                        index = index - xDim - 1;
                        x = x - 1;
                        y = y - 1;
                        found[index] = true;
                        fociDistance[k/2] += resDiag;
                    }
                    else if ((x < xDim - 1) && (y > 0) && (skeletonizedArray[index - xDim + 1] == (i+1)) &&
                             (!found[index - xDim + 1])) {
                        index = index - xDim + 1;
                        x = x + 1;
                        y = y - 1;
                        found[index] = true;
                        fociDistance[k/2] += resDiag;
                    }
                    else if ((x > 0) && (y < yDim - 1) && (skeletonizedArray[index + xDim - 1] == (i+1)) &&
                             (!found[index + xDim - 1])) {
                        index = index + xDim - 1;
                        x = x - 1;
                        y = y + 1;
                        found[index] = true;
                        fociDistance[k/2] += resDiag;
                    }
                    else if ((x < xDim - 1) && (y < yDim - 1) && (skeletonizedArray[index + xDim + 1] == (i+1)) &&
                             (!found[index + xDim + 1])) {
                        index = index + xDim + 1;
                        x = x +1;
                        y = y + 1;
                        found[index] = true;
                        fociDistance[k/2] += resDiag;
                    }
                    else {
                        loop[k/2] = true;
                        break;
                    }
                }
                
                k += 2;
            } // if (numGreenStrandFoci[i] == 2)
        } // for (i = 0, k = 0; i < numGreenObjects; i++)
        
        grayImage.disposeLocal();
        grayImage = null;

        srcImage.notifyImageDisplayListeners();
        skeletonizedImage.notifyImageDisplayListeners();
        prunedImage.notifyImageDisplayListeners();
        
        UI.getMessageFrame().addTab("PlugInAlgorithmFociAndStrands");
        UI.getMessageFrame().setFont("PlugInAlgorithmFociAndStrands", courier);
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", "\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", "File Name");
        fileNameLength = srcImage.getFileInfo(0).getFileName().length();
        if (fileNameLength > 9) {
            for (i = 0; i < fileNameLength - 9; i++) {
                UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", " ");
            }
        }
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", " \tNFoci\tNFociOnStrands\tNStrands\tAvgFociOnStrands\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands",
                srcImage.getFileInfo(0).getFileName() + " \t" + numRedObjects + "\t" + numRedColocalize + "\t\t" +
                numGreenObjects + "\t\t" + dfFract.format(avgFociOnStrands) + "\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", "Focus1\tFocus2\tDistance\n");
        for (i = 0, k = 0; i < numGreenObjects; i++) {
            if (numGreenStrandFoci[i] == 2) {
                if (loop[k]) {
                    UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", greenStrandFocus1[i] + "\t" +
                                                 greenStrandFocus2[i] + "\n"); 
                }
                else {
                    UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", greenStrandFocus1[i] + "\t" +
                            greenStrandFocus2[i] +  "\t" + df.format(fociDistance[k]) + "\n");     
                }
                k++;
            }
        }
        

        if (threadStopped) {
            finalize();

            return;
        }

        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmCenterDistance2 elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
    }
    
    /**
     * DOCUMENT ME!
     */
    private void calc3D() {

        BitSet imageMask = null;
        short redOnGreen[];
        int numRedColocalize;
        int numRedColocalize2;
        int totLength, sliceLength;
        float[] redBuffer;
        ModelImage grayImage;
        float threshold;
        boolean wholeImage;
        int kernel;
        float sphereDiameter;
        int method;
        int itersDilation;
        int itersErosion;
        int numPruningPixels;
        int edgingType;
        int i, j, k;
        int x, y, z;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        AlgorithmMorphology3D idObjectsAlgo3D;
        AlgorithmMorphology3D skeletonizeAlgo3D;
        byte[] byteBuffer;
        float[] greenBuffer = null;
        ViewVOIVector VOIs = null;
        int nVOIs;
        VOI newCircleVOI;
        int maxCirclePoints = (int)Math.ceil(2.0 * Math.PI * radius);
        float[] xArr = new float[maxCirclePoints];
        float[] yArr = new float[maxCirclePoints];
        float[] zArr = new float[maxCirclePoints];
        double theta;
        int index;
        ViewUserInterface UI = ViewUserInterface.getReference();

        FileInfoBase fileInfo;
        int numRedObjects = 0;
        int numGreenObjects = 0;
        short[] redIDArray = null;
        short[] greenIDArray = null;
        short[] skeletonizedArray = null;
        int index2;
        
        long time;
        int fileNameLength;
        Font courier = MipavUtil.courier12;
        DecimalFormat df = new DecimalFormat("0.000E0");
        DecimalFormat dfFract = new DecimalFormat("0.000");
        float redX[];
        float redY[];
        float redZ[];
        float redTotal[];
        int numGreenStrandFoci[];
        int greenStrandFocus1[];
        int greenStrandFocus2[];
        int redIDMinus1;
        int greenIDMinus1;
        VOI newPtVOI;
        float avgFociOnStrands;
        float redColocX1[];
        float redColocX2[];
        float redColocY1[];
        float redColocY2[];
        float redColocZ1[];
        float redColocZ2[];
        int greenLeft[];
        int greenRight[];
        int greenTop[];
        int greenBottom[];
        int greenFront[];
        int greenBack[];
        int redSkelX[];
        int redSkelY[];
        int redSkelZ[];
        double minDistanceSq1;
        double minDistanceSq2;
        double distX;
        double distY;
        double distZ;
        double distanceSq;
        int xInt[] = new int[1];
        int yInt[] = new int[1];
        int zInt[] = new int[1];
        
        time = System.currentTimeMillis();

        fireProgressStateChanged("Processing image ...");
        
        sliceLength = xDim * yDim;
        totLength = sliceLength * zDim;
        redBuffer = new float[totLength];
        byteBuffer = new byte[totLength];

        
            
        fireProgressStateChanged("Creating red image");
        fireProgressStateChanged(30);
        try {
            srcImage.exportRGBData(1, 0, totLength, redBuffer); // export red data       
        } catch (IOException error) {
            redBuffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        }
        grayImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), srcImage.getImageName() + "_gray");

        for (i = 0; i < srcImage.getExtents()[2]; i++) {
            fileInfo = grayImage.getFileInfo()[i];
            fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
            fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            grayImage.setFileInfo(fileInfo, i);
        } // for (i = 0; i < srcImage.getExtents()[2]; i++)
        
        threshold = (float)(srcImage.getMinR() + redFraction * (srcImage.getMaxR() - srcImage.getMinR()));
        Preferences.debug("red threshold = " + threshold + "\n");
        
        VOIs = srcImage.getVOIs();
        nVOIs = VOIs.size();
        wholeImage = true;
        for (i = 0; i < nVOIs; i++) {
            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                imageMask = srcImage.generateVOIMask();
                wholeImage = false;
                break;
            }
        }
        
        for (i = 0; i < totLength; i++) {
            if ((wholeImage || imageMask.get(i)) && (redBuffer[i] >= threshold)) {
                byteBuffer[i] = 1;
            }
            else {
                byteBuffer[i] = 0;
            }
        }

        try {
            grayImage.importData(0, byteBuffer, true);
        } catch (IOException error) {
            redBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }

        // Put the red objects in redIDArray
        // Run on entire image since the region outside the voi has
        // had byteBuffer set equal to zero.
        fireProgressStateChanged("IDing objects in red segmented image");
        fireProgressStateChanged(44);
        kernel = AlgorithmMorphology3D.SIZED_SPHERE;
        sphereDiameter = 0.0f;
        method = AlgorithmMorphology3D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        numPruningPixels = 0;
        edgingType = 0;
        idObjectsAlgo3D = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, true);
        idObjectsAlgo3D.setMinMax(redMin, 2000000);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;
        System.gc();

        grayImage.calcMinMax();
        numRedObjects = (int) grayImage.getMax();
        Preferences.debug("numRedObjects = " + numRedObjects + "\n");
        redIDArray = new short[totLength];
        
        /*redImage = (ModelImage)grayImage.clone();
        redImage.setImageName(srcImage.getImageName() + "_red");
        redFrame = new ViewJFrameImage(redImage);
        redFrame.setTitle(srcImage.getImageName() + "_red");*/

        try {
            grayImage.exportData(0, totLength, redIDArray);
        } catch (IOException error) {
            byteBuffer = null;
            redIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }

        fireProgressStateChanged("Creating green image");
        fireProgressStateChanged(46);
        
        greenBuffer = new float[totLength];
        
        try {

            srcImage.exportRGBData(2, 0, totLength, greenBuffer); // export green data
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Algorithm CenterDistance reports: source image locked", true);

            return;
        }  
        
        threshold = (float)(srcImage.getMinG() + greenFraction * (srcImage.getMaxG() - srcImage.getMinG()));
        Preferences.debug("Green threshold = " + threshold + "\n");
        
        for (i = 0; i < totLength; i++) {
            if ((wholeImage || imageMask.get(i)) && (greenBuffer[i] >= threshold)) {
                byteBuffer[i] = 1;
            }
            else {
                byteBuffer[i] = 0;
            }
        }

        try {
            grayImage.importData(0, byteBuffer, true);
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);

            return;
        }

        

        // Put the green objects IDs in greenIDArray
        fireProgressStateChanged("IDing objects in green segmented image");
        fireProgressStateChanged(58);
        kernel = AlgorithmMorphology3D.SIZED_SPHERE;
        sphereDiameter = 0.0f;
        method = AlgorithmMorphology3D.ID_OBJECTS;
        itersDilation = 0;
        itersErosion = 0;
        idObjectsAlgo3D = new AlgorithmMorphology3D(grayImage, kernel, sphereDiameter, method, itersDilation,
                                                    itersErosion, numPruningPixels, edgingType, true);
        idObjectsAlgo3D.setMinMax(greenMin, 100000);
        idObjectsAlgo3D.run();
        idObjectsAlgo3D.finalize();
        idObjectsAlgo3D = null;
        System.gc();

        grayImage.calcMinMax();
        numGreenObjects = (int) grayImage.getMax();
        Preferences.debug("numGreenObjects = " + numGreenObjects + "\n");
        greenIDArray = new short[totLength];
        
        greenImage = (ModelImage)grayImage.clone();
        greenImage.setImageName(srcImage.getImageName() + "_green");
        greenFrame = new ViewJFrameImage(greenImage);
        greenFrame.setTitle(srcImage.getImageName() + "_green");

        try {
            grayImage.exportData(0, totLength, greenIDArray);
        } catch (IOException error) {
            byteBuffer = null;
            greenIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        
        redOnGreen = new short[numRedObjects];
        redX = new float[numRedObjects];
        redY = new float[numRedObjects];
        redZ = new float[numRedObjects];
        redTotal = new float[numRedObjects];
        numGreenStrandFoci = new int[numGreenObjects];
        greenStrandFocus1 = new int[numGreenObjects];
        greenStrandFocus2 = new int[numGreenObjects];
        for (z = 0; z < zDim; z++) {
            index2 = z *sliceLength;
            for (y = 0; y < yDim; y++) {
                index = index2 + y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = index + x;
                    if (redIDArray[i] != 0) {
                        redIDMinus1 = redIDArray[i] - 1;
                        redX[redIDMinus1] += x * redBuffer[i];
                        redY[redIDMinus1] += y * redBuffer[i];
                        redZ[redIDMinus1] += z * redBuffer[i];
                        redTotal[redIDMinus1] += redBuffer[i];
                        if (redOnGreen[redIDMinus1] == 0) {
                            if (greenIDArray[i] != 0) {
                                redOnGreen[redIDArray[i] - 1] = greenIDArray[i];
                            }
                            else if ((x >= 1) && (greenIDArray[i-1] != 0)) {
                                redOnGreen[redIDArray[i] - 1] = greenIDArray[i-1];
                            }
                            else if ((x <= xDim - 2) && (greenIDArray[i+1] != 0)) {
                                redOnGreen[redIDArray[i] - 1] = greenIDArray[i+1];
                            }
                            else if ((y >= 1) && (greenIDArray[i - xDim] != 0)) {
                                redOnGreen[redIDArray[i] - 1] = greenIDArray[i-xDim];
                            }
                            else if ((y <= yDim - 2) && (greenIDArray[i + xDim] != 0)) {
                                redOnGreen[redIDArray[i] - 1] = greenIDArray[i+xDim];
                            }
                            else if ((z >= 1) && (greenIDArray[i - sliceLength] != 0)) {
                                redOnGreen[redIDArray[i] - 1] = greenIDArray[i-sliceLength];
                            }
                            else if ((z <= zDim - 2) && (greenIDArray[i + sliceLength] != 0)) {
                                redOnGreen[redIDArray[i] - 1] = greenIDArray[i + sliceLength];
                            }
                        }
                    }
                }
            }
        }
        
        numRedColocalize = 0;
        redColocX1 = new float[numGreenObjects];
        redColocY1 = new float[numGreenObjects];
        redColocX2 = new float[numGreenObjects];
        redColocY2 = new float[numGreenObjects];
        redColocZ1 = new float[numGreenObjects];
        redColocZ2 = new float[numGreenObjects];
        for (i = 0; i < numRedObjects; i++) {
            redX[i] = redX[i]/redTotal[i];
            redY[i] = redY[i]/redTotal[i];
            redZ[i] = redZ[i]/redTotal[i];
            if (redOnGreen[i] != 0) {
                greenIDMinus1 = redOnGreen[i] - 1;
                numGreenStrandFoci[greenIDMinus1]++;
                if (numGreenStrandFoci[greenIDMinus1] == 1) {
                    greenStrandFocus1[greenIDMinus1] = numRedColocalize + 1;
                    redColocX1[greenIDMinus1] = redX[i];
                    redColocY1[greenIDMinus1] = redY[i];
                    redColocZ1[greenIDMinus1] = redZ[i];
                }
                else if (numGreenStrandFoci[greenIDMinus1] == 2) {
                    greenStrandFocus2[greenIDMinus1] = numRedColocalize + 1;
                    redColocX2[greenIDMinus1] = redX[i];
                    redColocY2[greenIDMinus1] = redY[i];
                    redColocZ2[greenIDMinus1] = redZ[i];
                }
                //newPtVOI = new VOI((short) (numRedColocalize + nVOIs), Integer.toString(numRedColocalize+1), zDim, VOI.POINT, -1.0f);
                //newPtVOI.setColor(Color.white);
                //xArr[0] = redX[i];
                //yArr[0] = redY[i];
                //zArr[0] = Math.round(redZ[i]);
                //newPtVOI.importCurve(xArr, yArr, zArr, (int)zArr[0]);
                //((VOIPoint) (newPtVOI.getCurves()[(int)zArr[0]].elementAt(0))).setFixed(true);
                //((VOIPoint) (newPtVOI.getCurves()[(int)zArr[0]].elementAt(0))).setLabel(Integer.toString(numRedColocalize + 1));
                //srcImage.registerVOI(newPtVOI);
                newCircleVOI = new VOI((short) (numRedColocalize + nVOIs), Integer.toString(numRedColocalize+1),
                        zDim, VOI.CONTOUR, -1.0f);
                newCircleVOI.setColor(Color.white);
                for (j = 0; j < maxCirclePoints; j++) {
                    theta = j * 2.0 * Math.PI/maxCirclePoints;  
                    xArr[j] = (float)(redX[i] + radius * Math.cos(theta));
                    yArr[j] = (float)(redY[i] + radius * Math.sin(theta));
                    zArr[j] = Math.round(redZ[i]);
                }
                newCircleVOI.importCurve(xArr, yArr, zArr, (int)zArr[0]);
                ((VOIContour)(newCircleVOI.getCurves()[(int)zArr[0]].elementAt(0))).setFixed(true);
                newCircleVOI.setActive(false);
                ((VOIContour)(newCircleVOI.getCurves()[(int)zArr[0]].elementAt(0))).setActive(false);
                ((VOIContour)(newCircleVOI.getCurves()[(int)zArr[0]].elementAt(0))).setDoGeometricCenterLabel(true);
                ((VOIContour)(newCircleVOI.getCurves()[(int)zArr[0]].elementAt(0))).setClosed(true);
                ((VOIContour) (newCircleVOI).getCurves()[(int)zArr[0]].elementAt(0)).setLabel(Integer.toString(numRedColocalize + 1));
                ((VOIContour) (newCircleVOI.getCurves()[(int)zArr[0]].elementAt(0))).setName(Integer.toString(numRedColocalize + 1));
                srcImage.registerVOI(newCircleVOI);
                
                numRedColocalize++;
            }
        }
        Preferences.debug(numRedColocalize + " of " + numRedObjects + " red foci co-localize with green strands\n");
        
        avgFociOnStrands = (float)numRedColocalize/(float)numGreenObjects;
        
        numRedColocalize2 = 0;
        for (i = 0; i < numGreenObjects; i++) {
          if (numGreenStrandFoci[i] == 2) {
              numRedColocalize2 += 2;
          }
        }
        Preferences.debug(numRedColocalize2 + " of red foci are on green strands with 2 red foci\n");
        
        avgFociOnStrands = (float)numRedColocalize/(float)numGreenObjects;
        
        // Remove all green strands but those with 2 foci
        for (z = 0; z < zDim; z++) {
            index2 = z * sliceLength;
            for (y = 0; y < yDim; y++) {
                index = index2 + y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = index + x;
                    if (greenIDArray[i] != 0) {
                        greenIDMinus1 = greenIDArray[i] - 1;
                        if (numGreenStrandFoci[greenIDMinus1] != 2) {
                            greenIDArray[i] = 0;
                        }
                    }
                }
            }
        }
        
        try {
            grayImage.importData(0, greenIDArray, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on grayImage.importData", true);
            return;
        }
        
        // Skeletonize the green strands with 2 foci
        method = AlgorithmMorphology3D.SKELETONIZE;
        numPruningPixels = 0;
        skeletonizeAlgo3D = new AlgorithmMorphology3D(grayImage, 0, 0.0f, method, 0,
                0, numPruningPixels, 0, true);
        skeletonizeAlgo3D.run();
        skeletonizeAlgo3D.finalize();
        skeletonizeAlgo3D = null;
        
        // Find the points on the skeletonized green strands nearest their 2 colocalized red foci
        skeletonizedArray = new short[totLength];
        try {
            grayImage.exportData(0, totLength, skeletonizedArray);
        } catch (IOException error) {
            byteBuffer = null;
            greenIDArray = null;
            errorCleanUp("Error on grayImage.exportData", true);

            return;
        }
        greenLeft = new int[numGreenObjects];
        greenRight = new int[numGreenObjects];
        greenTop = new int[numGreenObjects];
        greenBottom = new int[numGreenObjects];
        greenFront = new int[numGreenObjects];
        greenBack = new int[numGreenObjects];
        for (i = 0; i < numGreenObjects; i++) {
            greenLeft[i] = xDim - 1;
            greenTop[i] = yDim - 1;
            greenFront[i] = zDim - 1;
        }
        for (z = 0; z < zDim; z++) {
            for (y = 0; y < yDim; y++) {
                index = y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = index + x;
                    if (skeletonizedArray[i] != 0) {
                        greenIDMinus1 = skeletonizedArray[i] - 1;
                        if (numGreenStrandFoci[greenIDMinus1] == 2) {
                            if (x < greenLeft[greenIDMinus1]) {
                                greenLeft[greenIDMinus1] = x;
                            }
                            if (x > greenRight[greenIDMinus1]) {
                                greenRight[greenIDMinus1] = x;
                            }
                            if (y < greenTop[greenIDMinus1]) {
                                greenTop[greenIDMinus1] = y;
                            }
                            if (y > greenBottom[greenIDMinus1]) {
                                greenBottom[greenIDMinus1] = y;
                            } 
                            if (z < greenFront[greenIDMinus1]) {
                                greenFront[greenIDMinus1] = z;
                            }
                            if (z > greenBack[greenIDMinus1]) {
                                greenBack[greenIDMinus1] = z;
                            }
                        }
                    }
                }
            }
        }
        
        skeletonizedImage = (ModelImage)grayImage.clone();
        skeletonizedImage.setImageName(srcImage.getImageName() + "_skeletonized");
        skeletonizedFrame = new ViewJFrameImage(skeletonizedImage);
        skeletonizedFrame.setTitle(srcImage.getImageName() + "_skeletonized");
        
        redSkelX = new int[numRedColocalize2];
        redSkelY = new int[numRedColocalize2];
        redSkelZ = new int[numRedColocalize2];
        for (i = 0, k = 0; i < numGreenObjects; i++) {
            minDistanceSq1 = Double.MAX_VALUE;
            minDistanceSq2 = Double.MAX_VALUE;
            if (numGreenStrandFoci[i] == 2) {
                for (z = greenFront[i]; z <= greenBack[i]; z++) {
                    index2 = z * sliceLength;
                    for (y = greenTop[i]; y <= greenBottom[i]; y++) {
                        index = index2 + y * xDim;
                        for (x = greenLeft[i]; x <= greenRight[i]; x++) {
                            j = index + x;
                            if (skeletonizedArray[j] == (i+1)) {
                                distX = redColocX1[i] - x;
                                distY = redColocY1[i] - y;
                                distZ = redColocZ1[i] - z;
                                distanceSq = (distX * distX + distY * distY + distZ * distZ);
                                if (distanceSq < minDistanceSq1) {
                                    minDistanceSq1 = distanceSq;
                                    redSkelX[k] = x;
                                    redSkelY[k] = y;
                                    redSkelZ[k] = z;
                                }
                                distX = redColocX2[i] - x;
                                distY = redColocY2[i] - y;
                                distZ = redColocZ2[i] - z;
                                distanceSq = (distX * distX + distY * distY + distZ * distZ);
                                if (distanceSq < minDistanceSq2) {
                                    minDistanceSq2 = distanceSq;
                                    redSkelX[k+1] = x;
                                    redSkelY[k+1] = y;
                                    redSkelZ[k+1] = z;
                                }
                            }
                        }
                    }
                }
                newPtVOI = new VOI((short) (greenStrandFocus1[i]), Integer.toString(greenStrandFocus1[i]), zDim, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k];
                yInt[0] = redSkelY[k];
                zInt[0] = redSkelZ[k];
                newPtVOI.importCurve(xInt, yInt, zInt, zInt[0]);
                ((VOIPoint) (newPtVOI.getCurves()[zInt[0]].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[zInt[0]].elementAt(0))).setLabel(Integer.toString(greenStrandFocus1[i]));
                skeletonizedImage.registerVOI(newPtVOI);
                newPtVOI = new VOI((short) (greenStrandFocus2[i]), Integer.toString(greenStrandFocus2[i]), zDim, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k+1];
                yInt[0] = redSkelY[k+1];
                zInt[0] = redSkelZ[k+1];
                newPtVOI.importCurve(xInt, yInt, zInt, zInt[0]);
                ((VOIPoint) (newPtVOI.getCurves()[zInt[0]].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[zInt[0]].elementAt(0))).setLabel(Integer.toString(greenStrandFocus2[i]));
                skeletonizedImage.registerVOI(newPtVOI);
                k += 2;
            } // if (numGreenStrandFoci[i] == 2)
        } // for (i = 0, k = 0; i < numGreenObjects; i++)
        
        grayImage.disposeLocal();
        grayImage = null;

        srcImage.notifyImageDisplayListeners();
        skeletonizedImage.notifyImageDisplayListeners();
        
        UI.getMessageFrame().addTab("PlugInAlgorithmFociAndStrands");
        UI.getMessageFrame().setFont("PlugInAlgorithmFociAndStrands", courier);
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", "\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", "File Name");
        fileNameLength = srcImage.getFileInfo(0).getFileName().length();
        if (fileNameLength > 9) {
            for (i = 0; i < fileNameLength - 9; i++) {
                UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", " ");
            }
        }
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", " \tNFoci\tNFociOnStrands\tNStrands\tAvgFociOnStrands\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands",
                srcImage.getFileInfo(0).getFileName() + " \t" + numRedObjects + "\t" + numRedColocalize + "\t\t" +
                numGreenObjects + "\t\t" + dfFract.format(avgFociOnStrands) + "\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", "Focus1\tFocus2\tDistance\n");
        for (i = 0; i < numGreenObjects; i++) {
            if (numGreenStrandFoci[i] == 2) {
                UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", greenStrandFocus1[i] + "\t" +
                                             greenStrandFocus2[i] + "\n");     
            }
        }
        
        if (threadStopped) {
            finalize();

            return;
        }
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmCenterDistance2 elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
    }

    
}
