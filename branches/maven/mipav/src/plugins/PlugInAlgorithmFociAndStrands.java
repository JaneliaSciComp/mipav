import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.flythroughview.*;

import java.awt.*;

import java.io.*;

import java.text.*;

import java.util.*;


/**
 *
 * @version  July 25, 2008
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
 *           <p>11.) Skeletonize the green strands with 2 foci.  For 2D AlgorithmMorphology2D is used.
 *           For 3D AlgorithmMorphology3D is not used because skeletonization is 2.5D rather than 3D.
 *           For 3D Skeleton3D is used. 
 *           
 *           <p>12.) Find the points on the skeletonized green strands nearest their 2 colocalized red foci.
 *           Use these points as the foci on the skeletonized green strands in the distance calculations. 
 *           
 *           <p>13.) If a point is not a focus and has only 1 branch, then prune it.  Keep repeating the
 *           process until no more pruning can occur.
 *           
 *           <p>14.) Find the distance between the 2 foci.  Record the number of branches on the path.  If there are
 *           2 branches, then push the index onto branchIndex and the distance so far onto distanceVector.  If there
 *           are 3 branches, then push the index twice onto branchIndex and push the distance twice onto distanceVector.
 *           If there are no branches, then we have looped back to a point we already went over, so pop the first 
 *           element from branchIndex to return to the first untried branch and pop the first element from distanceVector
 *           to restore the distance traveled at the point.  For 2D there can be up to 3 branches.  For 3D there can
 *           be up to 5 branches.
 */
public class PlugInAlgorithmFociAndStrands extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

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
    
    //private ModelImage redImage;
    //private ViewJFrameImage redFrame;
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
     * @param  redMin         Minimum number of pixels in a red focus
     * @param  redFraction    Focus requires a minimum red value >= 
     *                        image red min + redFraction * (image red max - image red min)
     * @param  greenMin       Minimum number of pixels in a green strand
     * @param  greenFraction  Strand requires a minimum green value >=
     *                        image green min + greeFraction * (image green max - image green min)
     * @param  radius         Raidus of circles drawn around colocalized foci
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
        } else if (srcImage.getNDims() == 3) {
            calc3D();
        } else {
            displayError("Cannot handle more than 3D");
            return;
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
        int i, j, k, m;
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
        double resXY = Math.sqrt(resX*resX + resY*resY);
        boolean branchPruned;
        int redSkelIndex1;
        int redSkelIndex2;
        int numBranches;
        boolean found[];
        double fociDistance[];
        boolean loop[];
        boolean incomplete[];
        IntVector branchIndex = new IntVector(100,100);
        boolean bxp;
        boolean bxm;
        boolean byp;
        boolean bym;
        boolean bxpyp;
        boolean bxpym;
        boolean bxmyp;
        boolean bxmym;
        Vector <Double> distanceVector = new Vector<Double>(100,100);
        boolean noPushIndex[];
        
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

        fireProgressStateChanged("Creating red image");
        fireProgressStateChanged(5);
        try {
            srcImage.exportRGBData(1, 0, length, redBuffer); // export red data
        } catch (IOException error) {
            redBuffer = null;
            greenBuffer = null;
            errorCleanUp("Algorithm FociAndStrands reports: source image locked", true);

            return;
        }

        grayImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), srcImage.getImageName() + "_gray");
        fileInfo = grayImage.getFileInfo()[0];
        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        grayImage.setFileInfo(fileInfo, 0);
        
        fireProgressStateChanged("Thresholding red image");
        fireProgressStateChanged(10);
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
        fireProgressStateChanged("IDing objects in red thresholded image");
        fireProgressStateChanged(15);
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
        fireProgressStateChanged(20);
        
        greenBuffer = new float[length];

        try {

            srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Algorithm FociAndStrands reports: source image locked", true);

            return;
        }   
        
        fireProgressStateChanged("Thresholding green image");
        fireProgressStateChanged(25);
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
        fireProgressStateChanged("IDing objects in green thresholded image");
        fireProgressStateChanged(30);
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
        
        fireProgressStateChanged("Finding green strands colocalized with red foci");
        fireProgressStateChanged(40);
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
                newCircleVOI = new VOI((short) (numRedColocalize + nVOIs), Integer.toString(numRedColocalize+1), VOI.CONTOUR, -1.0f);
                newCircleVOI.setColor(Color.white);
                for (j = 0; j < maxCirclePoints; j++) {
                    theta = j * 2.0 * Math.PI/maxCirclePoints;  
                    xArr[j] = (float)(redX[i] + radius * Math.cos(theta));
                    yArr[j] = (float)(redY[i] + radius * Math.sin(theta));
                    zArr[j] = 0.0f;
                }
                newCircleVOI.importCurve(xArr, yArr, zArr);
                ((VOIContour)(newCircleVOI.getCurves().elementAt(0))).setFixed(true);
                newCircleVOI.setActive(false);
                ((VOIContour)(newCircleVOI.getCurves().elementAt(0))).setActive(false);
                ((VOIContour)(newCircleVOI.getCurves().elementAt(0))).setDoGeometricCenterLabel(true);
                ((VOIContour)(newCircleVOI.getCurves().elementAt(0))).setClosed(true);
                ((VOIContour) (newCircleVOI.getCurves().elementAt(0))).setLabel(Integer.toString(numRedColocalize + 1));
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
        fireProgressStateChanged("Skeletonizing green strands");
        fireProgressStateChanged(50);
        method = AlgorithmMorphology2D.SKELETONIZE;
        numPruningPixels = 0;
        skeletonizeAlgo2D = new AlgorithmMorphology2D(grayImage, 0, 0.0f, method, 0,
                0, numPruningPixels, 0, true);
        skeletonizeAlgo2D.run();
        skeletonizeAlgo2D.finalize();
        skeletonizeAlgo2D = null;
        
        // Find the points on the skeletonized green strands nearest their 2 colocalized red foci
        fireProgressStateChanged("Finding foci on skeletonized green strands");
        fireProgressStateChanged(60);
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
                newPtVOI = new VOI((short) (greenStrandFocus1[i]), Integer.toString(greenStrandFocus1[i]), VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k];
                yInt[0] = redSkelY[k];
                zInt[0] = 0;
                newPtVOI.importCurve(xInt, yInt, zInt);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(greenStrandFocus1[i]));
                skeletonizedImage.registerVOI(newPtVOI);
                newPtVOI = new VOI((short) (greenStrandFocus2[i]), Integer.toString(greenStrandFocus2[i]), VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k+1];
                yInt[0] = redSkelY[k+1];
                zInt[0] = 0;
                newPtVOI.importCurve(xInt, yInt, zInt);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(greenStrandFocus2[i]));
                skeletonizedImage.registerVOI(newPtVOI);
                k += 2;
            } // if (numGreenStrandFoci[i] == 2)
        } // for (i = 0, k = 0; i < numGreenObjects; i++)
        
        // If a point is not a focus and has only 1 branch, then prune it.
        // Keep repeating the process until no more pruning can occur
        fireProgressStateChanged("Pruning green strands");
        fireProgressStateChanged(70);
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
                                bxp = false;
                                bxm = false;
                                byp = false;
                                bym = false;
                                if (x > 0) {
                                    if (skeletonizedArray[j-1] == (i+1)) {
                                        numBranches++;
                                        bxm = true;
                                    }
                                } // if (x > 0)
                                if (x < xDim - 1) {
                                    if (skeletonizedArray[j+1] == (i+1)) {
                                        numBranches++;
                                        bxp = true;
                                    }
                                } // if (x < xDim - 1)
                                if (y > 0) {
                                    if (skeletonizedArray[j - xDim] == (i+1)) {
                                        numBranches++;
                                        bym = true;
                                    }
                                } // if ( y > 0)
                                if (y < yDim - 1) {
                                    if (skeletonizedArray[j + xDim] == (i+1)) {
                                        numBranches++;
                                        byp = true;
                                    }    
                                } // if (y < yDim - 1)
                                if ((x > 0) && (y > 0) && (!bxm) && (!bym)) {
                                    if (skeletonizedArray[j - xDim - 1] == (i+1)) {
                                        numBranches++;
                                    }
                                }
                                if ((x > 0) && (y < yDim - 1) && (!bxm) && (!byp)) {
                                    if (skeletonizedArray[j + xDim - 1] == (i+1)) {
                                        numBranches++;
                                    }
                                }
                                if ((x < xDim - 1) && (y > 0) && (!bxp) && (!bym)) {
                                    if (skeletonizedArray[j - xDim + 1] == (i+1)) {
                                        numBranches++;
                                    }
                                }
                                if ((x < xDim -1) && (y < yDim - 1) && (!bxp) && (!byp)) {
                                    if (skeletonizedArray[j + xDim + 1] == (i+1)) {
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
                newPtVOI = new VOI((short) (greenStrandFocus1[i]), Integer.toString(greenStrandFocus1[i]), VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k];
                yInt[0] = redSkelY[k];
                zInt[0] = 0;
                newPtVOI.importCurve(xInt, yInt, zInt);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(greenStrandFocus1[i]));
                prunedImage.registerVOI(newPtVOI);
                newPtVOI = new VOI((short) (greenStrandFocus2[i]), Integer.toString(greenStrandFocus2[i]), VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k+1];
                yInt[0] = redSkelY[k+1];
                zInt[0] = 0;
                newPtVOI.importCurve(xInt, yInt, zInt);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(greenStrandFocus2[i]));
                prunedImage.registerVOI(newPtVOI);
                k += 2;
            } // if (numGreenStrandFoci[i] == 2)
        } // for (i = 0, k = 0; i < numGreenObjects; i++)
        
        fireProgressStateChanged("Finding distance between foci");
        fireProgressStateChanged(80);
        found = new boolean[length];
        noPushIndex = new boolean[length];
        fociDistance = new double[numRedColocalize2/2];
        loop = new boolean[numRedColocalize2/2];
        incomplete = new boolean[numRedColocalize/2];
        for (i = 0, k = 0; i < numGreenObjects; i++) {
            if (numGreenStrandFoci[i] == 2) {
                for (j = 0; j < length; j++) {
                    found[j] = false;
                    noPushIndex[j] = false;
                }
                branchIndex.removeAllElements();
                distanceVector.removeAllElements();
                x = redSkelX[k];
                y = redSkelY[k];
                index = redSkelX[k] + xDim * redSkelY[k];
                found[index] = true;
                while ((x != redSkelX[k+1]) || (y != redSkelY[k+1])) {
                    numBranches = 0;
                    bxm = false;
                    bxp = false;
                    bym = false;
                    byp = false;
                    bxmym = false;
                    bxmyp = false;
                    bxpym = false;
                    bxpyp = false;
                    if ((x > 0) && (skeletonizedArray[index - 1] == (i+1)) && (!found[index-1])) {
                        bxm = true;
                        numBranches++;
                    }
                    if ((x < xDim - 1) && (skeletonizedArray[index + 1] == (i+1)) && (!found[index+1])) {
                        bxp = true;
                        numBranches++;
                    }
                    if ((y > 0) && (skeletonizedArray[index - xDim] == (i+1)) && (!found[index-xDim])) {
                        bym = true;
                        numBranches++;
                    }
                    if ((y < yDim - 1) && (skeletonizedArray[index + xDim] == (i+1)) && (!found[index+xDim])) {
                        byp = true;
                        numBranches++;
                    }
                    if ((x > 0) && (y > 0) && (!bxm) && (!bym) && (skeletonizedArray[index - xDim - 1] == (i+1)) &&
                             (!found[index-xDim-1])) {
                        bxmym = true;
                        numBranches++;
                    }
                    if ((x > 0) && (y < yDim - 1) && (!bxm) && (!byp) && (skeletonizedArray[index + xDim - 1] == (i+1)) &&
                            (!found[index + xDim - 1])) {
                       bxmyp = true;
                       numBranches++;
                    }
                    if ((x < xDim - 1) && (y > 0) && (!bxp) && (!bym) && (skeletonizedArray[index - xDim + 1] == (i+1)) &&
                             (!found[index - xDim + 1])) {
                        bxpym = true;
                        numBranches++;
                    }
                    if ((x < xDim - 1) && (y < yDim - 1) && (!bxp) && (!byp) &&
                             (skeletonizedArray[index + xDim + 1] == (i+1)) && (!found[index + xDim + 1])) {
                        bxpyp = true;
                        numBranches++;
                    }
                    if (numBranches == 0) {
                        loop[k/2] = true;
                        if (!branchIndex.isEmpty()) {
                            index = branchIndex.popFirstIn();
                            x = index % xDim;
                            y = index / xDim;
                            fociDistance[k/2] = distanceVector.remove(0).doubleValue();
                            continue;
                        }
                        else {
                            incomplete[k/2] = true;
                            break;
                        }
                    }
                    if ((numBranches >= 2) && (!noPushIndex[index])) {
                        for (m = 0; m < numBranches - 1; m++) {
                            branchIndex.push(index);
                            distanceVector.add(Double.valueOf(fociDistance[k/2]));
                        }
                        noPushIndex[index] = true;
                    }
                    if (bxm) {
                        index = index - 1;
                        x = x - 1;
                        found[index] = true;
                        fociDistance[k/2] += resX;    
                    }
                    else if (bxp) {
                        index = index + 1;
                        x = x + 1;
                        found[index] = true;
                        fociDistance[k/2] += resX;
                    }
                    else if (bym) {
                        index = index - xDim;
                        y = y - 1;
                        found[index] = true;
                        fociDistance[k/2] += resY;    
                    }
                    else if (byp) {
                        index = index + xDim;
                        y = y + 1;
                        found[index] = true;
                        fociDistance[k/2] += resY;
                    }
                    else if (bxmym) {
                        index = index - xDim - 1;
                        x = x - 1;
                        y = y - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXY;
                    }
                    else if (bxmyp) {
                        index = index + xDim - 1;
                        x = x - 1;
                        y = y + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXY;
                    }
                    else if (bxpym) {
                        index = index - xDim + 1;
                        x = x + 1;
                        y = y - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXY;
                    }
                    else if (bxpyp) {
                        index = index + xDim + 1;
                        x = x + 1;
                        y = y + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXY;
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
                if (incomplete[k]) {
                    UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", greenStrandFocus1[i] + "\t" +
                                                 greenStrandFocus2[i] + "\n"); 
                }
                else if (!loop[k] ){
                    UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", greenStrandFocus1[i] + "\t" +
                            greenStrandFocus2[i] +  "\t" + df.format(fociDistance[k]) + "\n");     
                }
                else {
                    UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", greenStrandFocus1[i] + "\t" +
                            greenStrandFocus2[i] +  "\t" + df.format(fociDistance[k]) + "\t" + "loop\n");         
                }
                k++;
            }
        }
        

        if (threadStopped) {
            finalize();

            return;
        }

        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmFociAndStrands elapsed time in seconds = " + (time/1000.0));
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
        int i, j, k, m;
        int x, y, z;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        AlgorithmMorphology3D idObjectsAlgo3D;
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
        boolean branchPruned;
        int redSkelIndex1;
        int redSkelIndex2;
        int numBranches;
        boolean bxm;
        boolean bxp;
        boolean bym;
        boolean byp;
        boolean bzm;
        boolean bzp;
        boolean bxmym;
        boolean bxmyp;
        boolean bxpym;
        boolean bxpyp;
        boolean bxmzm;
        boolean bxmzp;
        boolean bxpzm;
        boolean bxpzp;
        boolean bymzm;
        boolean bymzp;
        boolean bypzm;
        boolean bypzp;
        boolean bxmymzm;
        boolean bxmymzp;
        boolean bxmypzm;
        boolean bxmypzp;
        boolean bxpymzm;
        boolean bxpymzp;
        boolean bxpypzm;
        boolean bxpypzp;
        boolean found[];
        boolean noPushIndex[];
        double fociDistance[];
        boolean loop[];
        boolean incomplete[];
        IntVector branchIndex = new IntVector(100,100);
        Vector <Double> distanceVector = new Vector<Double>(100,100);
        float resX = srcImage.getFileInfo()[0].getResolutions()[0];
        float resY = srcImage.getFileInfo()[0].getResolutions()[1];
        float resZ = srcImage.getFileInfo()[0].getResolutions()[2];
        double resXY = Math.sqrt(resX*resX + resY*resY);
        double resXZ = Math.sqrt(resX*resX + resZ*resZ);
        double resYZ = Math.sqrt(resY*resY + resZ*resZ);
        double resXYZ = Math.sqrt(resX*resX + resY*resY + resZ*resZ);
        ModelImage3DLayout volumeLayout;
        ModelImage objectImage;
        int objectExtents[] = new int[3];
        short objectArray[];
        int objectSlice;
        int index2ob;
        int indexob;
        int job;
        Skeleton3D objectSkeleton;
        int maxBranches = 50;
        float minBranchLength = 1.0f;
        
        time = System.currentTimeMillis();
        
        sliceLength = xDim * yDim;
        totLength = sliceLength * zDim;
        redBuffer = new float[totLength];
        byteBuffer = new byte[totLength];

        
            
        fireProgressStateChanged("Creating red image");
        fireProgressStateChanged(5);
        try {
            srcImage.exportRGBData(1, 0, totLength, redBuffer); // export red data       
        } catch (IOException error) {
            redBuffer = null;
            errorCleanUp("Algorithm FociAndStrands reports: source image locked", true);

            return;
        }
        grayImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), srcImage.getImageName() + "_gray");

        for (i = 0; i < srcImage.getExtents()[2]; i++) {
            fileInfo = grayImage.getFileInfo()[i];
            fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
            fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            grayImage.setFileInfo(fileInfo, i);
        } // for (i = 0; i < srcImage.getExtents()[2]; i++)
        
        fireProgressStateChanged("Thresholding red image");
        fireProgressStateChanged(10);
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
        fireProgressStateChanged("IDing objects in red thresholded image");
        fireProgressStateChanged(15);
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
        fireProgressStateChanged(20);
        
        greenBuffer = new float[totLength];
        
        try {

            srcImage.exportRGBData(2, 0, totLength, greenBuffer); // export green data
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Algorithm FociAndStrands reports: source image locked", true);

            return;
        }  
        
        fireProgressStateChanged("Thresholding green image");
        fireProgressStateChanged(25);
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
        fireProgressStateChanged("IDing objects in green thresholded image");
        fireProgressStateChanged(30);
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
        
        fireProgressStateChanged("Finding green strands colocalized with red foci");
        fireProgressStateChanged(40);
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
                            else if ((z >= 1) && (greenIDArray[i - sliceLength] != 0)) {
                                redOnGreen[redIDMinus1] = greenIDArray[i-sliceLength];
                            }
                            else if ((z <= zDim - 2) && (greenIDArray[i + sliceLength] != 0)) {
                                redOnGreen[redIDMinus1] = greenIDArray[i + sliceLength];
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
                newCircleVOI = new VOI((short) (numRedColocalize + nVOIs), Integer.toString(numRedColocalize+1), VOI.CONTOUR, -1.0f);
                newCircleVOI.setColor(Color.white);
                for (j = 0; j < maxCirclePoints; j++) {
                    theta = j * 2.0 * Math.PI/maxCirclePoints;  
                    xArr[j] = (float)(redX[i] + radius * Math.cos(theta));
                    yArr[j] = (float)(redY[i] + radius * Math.sin(theta));
                    zArr[j] = Math.round(redZ[i]);
                }
                newCircleVOI.importCurve(xArr, yArr, zArr);
                ((VOIContour)(newCircleVOI.getCurves().elementAt(0))).setFixed(true);
                newCircleVOI.setActive(false);
                ((VOIContour)(newCircleVOI.getCurves().elementAt(0))).setActive(false);
                ((VOIContour)(newCircleVOI.getCurves().elementAt(0))).setDoGeometricCenterLabel(true);
                ((VOIContour)(newCircleVOI.getCurves().elementAt(0))).setClosed(true);
                ((VOIContour) (newCircleVOI.getCurves().elementAt(0))).setLabel(Integer.toString(numRedColocalize + 1));
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
            index2 = z * sliceLength;
            for (y = 0; y < yDim; y++) {
                index = index2 + y * xDim;
                for (x = 0; x < xDim; x++) {
                    i = index + x;
                    if (greenIDArray[i] != 0) {
                        greenIDMinus1 = greenIDArray[i] - 1;
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
        
        // Skeletonize the green strands with 2 foci
        fireProgressStateChanged("Skeletonizing green strands");
        fireProgressStateChanged(50);
        skeletonizedArray = new short[totLength];
        for (i = 0; i < numGreenObjects; i++) {
            if (numGreenStrandFoci[i] == 2) {
                objectExtents[0] = greenRight[i] - greenLeft[i] + 1;
                objectExtents[1] = greenBottom[i] - greenTop[i] + 1;
                objectSlice = objectExtents[0] * objectExtents[1];
                objectExtents[2] = greenBack[i] - greenFront[i] + 1;
                volumeLayout = new ModelImage3DLayout(objectExtents[0], objectExtents[1], objectExtents[2], resX, resY, resZ, 0, 0, 0);
                objectImage = new ModelImage(ModelStorageBase.SHORT, objectExtents, srcImage.getImageName() + "_object");
                objectArray = new short[objectSlice*objectExtents[2]];
                for (z = greenFront[i]; z <= greenBack[i]; z++) {
                    index2 = z * sliceLength;
                    index2ob = (z - greenFront[i]) * objectSlice;
                    for (y = greenTop[i]; y <= greenBottom[i]; y++) {
                        index = index2 + y * xDim;
                        indexob = index2ob + (y - greenTop[i]) * objectExtents[0];
                        for (x = greenLeft[i]; x <= greenRight[i]; x++) {
                            j = index + x;
                            job = indexob + (x - greenLeft[i]);
                            if (greenIDArray[j] == (i+1)) {
                                objectArray[job] = 1;
                            }
                        }
                    }
                }
                try {
                    objectImage.importData(0, objectArray, true);
                }
                catch (IOException error) {
                    byteBuffer = null;
                    errorCleanUp("Error on objectImage.importData", true);
                    return;
                }
                objectSkeleton = new Skeleton3D(objectImage, volumeLayout);
                objectArray = objectSkeleton.getSkeletonizedArray(maxBranches, minBranchLength);
                objectSkeleton.dispose();
                objectSkeleton = null;
                objectImage.disposeLocal();
                objectImage = null;
                for (z = greenFront[i]; z <= greenBack[i]; z++) {
                    index2 = z * sliceLength;
                    index2ob = (z - greenFront[i]) * objectSlice;
                    for (y = greenTop[i]; y <= greenBottom[i]; y++) {
                        index = index2 + y * xDim;
                        indexob = index2ob + (y - greenTop[i]) * objectExtents[0];
                        for (x = greenLeft[i]; x <= greenRight[i]; x++) {
                            j = index + x;
                            job = indexob + (x - greenLeft[i]);
                            if (objectArray[job] != 0) {
                                skeletonizedArray[j] = (short)(i+1);
                            }
                            else {
                                skeletonizedArray[j] = 0;
                            }
                        }
                    }
                }
            }
        }
        
        skeletonizedImage = new ModelImage(ModelStorageBase.SHORT, srcImage.getExtents(),
                                           srcImage.getImageName() + "_skeletonized");
        try {
            skeletonizedImage.importData(0, skeletonizedArray, true);
        }
        catch (IOException error) {
            byteBuffer = null;
            errorCleanUp("Error on skeletonizedImage.importData", true);
            return;
        }
        skeletonizedFrame = new ViewJFrameImage(skeletonizedImage);
        skeletonizedFrame.setTitle(srcImage.getImageName() + "_skeletonized");
       
        
        // Find the points on the skeletonized green strands nearest their 2 colocalized red foci 
        fireProgressStateChanged("Finding foci on skeletonized green strands");
        fireProgressStateChanged(60);
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
                newPtVOI = new VOI((short) (greenStrandFocus1[i]), Integer.toString(greenStrandFocus1[i]), VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k];
                yInt[0] = redSkelY[k];
                zInt[0] = redSkelZ[k];
                newPtVOI.importCurve(xInt, yInt, zInt);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(greenStrandFocus1[i]));
                skeletonizedImage.registerVOI(newPtVOI);
                newPtVOI = new VOI((short) (greenStrandFocus2[i]), Integer.toString(greenStrandFocus2[i]), VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k+1];
                yInt[0] = redSkelY[k+1];
                zInt[0] = redSkelZ[k+1];
                newPtVOI.importCurve(xInt, yInt, zInt);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(greenStrandFocus2[i]));
                skeletonizedImage.registerVOI(newPtVOI);
                k += 2;
            } // if (numGreenStrandFoci[i] == 2)
        } // for (i = 0, k = 0; i < numGreenObjects; i++)
        
        // If a point is not a focus and has only 1 branch, then prune it.
        // Keep repeating the process until no more pruning can occur
        fireProgressStateChanged("Pruning green strands");
        fireProgressStateChanged(70);
        for (i = 0, k = 0; i < numGreenObjects; i++) {
            if (numGreenStrandFoci[i] == 2) {
                branchPruned = true;
                redSkelIndex1 = redSkelX[k] + xDim * redSkelY[k] + sliceLength * redSkelZ[k];
                redSkelIndex2 = redSkelX[k+1] + xDim * redSkelY[k+1] + sliceLength * redSkelZ[k+1];
                while(branchPruned) {
                    branchPruned = false; 
                    for (z = greenFront[i]; z <= greenBack[i]; z++) {
                        index2 = z * sliceLength;
                        for (y = greenTop[i]; y <= greenBottom[i]; y++) {
                            index = index2 + y * xDim;
                            for (x = greenLeft[i]; x <= greenRight[i]; x++) {
                                j = index + x;
                                if ((skeletonizedArray[j] == (i+1)) && (j != redSkelIndex1) && (j != redSkelIndex2)) {
                                    numBranches = 0;
                                    bxm = false;
                                    bxp = false;
                                    bym = false;
                                    byp = false;
                                    bzm = false;
                                    bzp = false;
                                    bxmym = false;
                                    bxmyp = false;
                                    bxpym = false;
                                    bxpyp = false;
                                    bxmzm = false;
                                    bxmzp = false;
                                    bxpzm = false;
                                    bxpzp = false;
                                    bymzm = false;
                                    bymzp = false;
                                    bypzm = false;
                                    bypzp = false;
                                    if (x > 0) {
                                        if (skeletonizedArray[j-1] == (i+1)) {
                                            numBranches++;
                                            bxm = true;
                                        }
                                    } // if (x > 0)
                                    if (x < xDim - 1) {
                                        if (skeletonizedArray[j+1] == (i+1)) {
                                            numBranches++;
                                            bxp = true;
                                        }
                                    } // if (x < xDim - 1)
                                    if (y > 0) {
                                        if (skeletonizedArray[j - xDim] == (i+1)) {
                                            numBranches++;
                                            bym = true;
                                        }
                                    } // if ( y > 0)
                                    if (y < yDim - 1) {
                                        if (skeletonizedArray[j + xDim] == (i+1)) {
                                            numBranches++;
                                            byp = true;
                                        }    
                                    } // if (y < yDim - 1)
                                    if (z > 0) {
                                        if (skeletonizedArray[j - sliceLength] == (i+1)) {
                                            numBranches++;
                                            bzm = true;
                                        }
                                    }
                                    if (z < zDim - 1) {
                                        if (skeletonizedArray[j + sliceLength] == (i+1)) {
                                            numBranches++;
                                            bzp = true;
                                        }
                                    }
                                    if ((x > 0) && (y > 0) && (!bxm) && (!bym)) {
                                        if (skeletonizedArray[j - xDim - 1] == (i+1)) {
                                            numBranches++;
                                            bxmym = true;
                                        }
                                    }
                                    if ((x > 0) && (y < yDim - 1) && (!bxm) && (!byp)) {
                                        if (skeletonizedArray[j + xDim - 1] == (i+1)) {
                                            numBranches++;
                                            bxmyp = true;
                                        }
                                    }
                                    if ((x < xDim - 1) && (y > 0) && (!bxp) && (!bym)) {
                                        if (skeletonizedArray[j - xDim + 1] == (i+1)) {
                                            numBranches++;
                                            bxpym = true;
                                        }
                                    }
                                    if ((x < xDim -1) && (y < yDim - 1) && (!bxp) && (!byp)) {
                                        if (skeletonizedArray[j + xDim + 1] == (i+1)) {
                                            numBranches++;
                                            bxpyp = true;
                                        }
                                    }
                                    if ((x > 0) && (z > 0) && (!bxm) && (!bzm)) {
                                        if (skeletonizedArray[j - sliceLength - 1] == (i+1)) {
                                            numBranches++;
                                            bxmzm = true;
                                        }
                                    }
                                    if ((x > 0) && (z < zDim - 1) && (!bxm) && (!bzp)) {
                                        if (skeletonizedArray[j + sliceLength - 1] == (i+1)) {
                                            numBranches++;
                                            bxmzp = true;
                                        }
                                    }
                                    if ((x < xDim - 1) && (z > 0) && (!bxp) && (!bzm)) {
                                        if (skeletonizedArray[j - sliceLength + 1] == (i+1)) {
                                            numBranches++;
                                            bxpzm = true;
                                        }
                                    }
                                    if ((x < xDim - 1) && (z < zDim - 1) && (!bxp) && (!bzp)) {
                                        if (skeletonizedArray[j + sliceLength + 1] == (i+1)) {
                                            numBranches++;
                                            bxpzp = true;
                                        }
                                    }
                                    if ((y > 0) && (z > 0) && (!bym) && (!bzm)) {
                                        if (skeletonizedArray[j - sliceLength - xDim] == (i+1)) {
                                            numBranches++;
                                            bymzm = true;
                                        }
                                    }
                                    if ((y > 0) && (z < zDim - 1) && (!bym) && (!bzp)) {
                                        if (skeletonizedArray[j + sliceLength - xDim] == (i+1)) {
                                            numBranches++;
                                            bymzp = true;
                                        }
                                    }
                                    if ((y < yDim - 1) && (z > 0) && (!byp) && (!bzm)) {
                                        if (skeletonizedArray[j - sliceLength + xDim] == (i+1)) {
                                            numBranches++;
                                            bypzm = true;
                                        }
                                    }
                                    if ((y < yDim - 1) && (z < zDim - 1) && (!byp) && (!bzp)) {
                                        if (skeletonizedArray[j + sliceLength + xDim] == (i+1)) {
                                            numBranches++;
                                            bypzp = true;
                                        }
                                    }
                                    if ((x > 0) && (y > 0) && (z > 0) && (!bxm) && (!bym) && (!bzm) && (!bxmym) && (!bxmzm) && (!bymzm)) {
                                        if (skeletonizedArray[j - sliceLength - xDim - 1] == (i+1)) {
                                            numBranches++;
                                        }
                                    }
                                    if ((x > 0) && (y > 0) && (z < zDim - 1) && (!bxm) && (!bym) && (!bzp) && (!bxmym) &&
                                        (!bxmzp) && (!bymzp)) {
                                        if (skeletonizedArray[j + sliceLength - xDim - 1] == (i+1)) {
                                            numBranches++;
                                        }
                                    }
                                    if ((x > 0) && (y < yDim - 1) && (z > 0) && (!bxm) && (!byp) && (!bzm) && (!bxmyp) && 
                                        (!bxmzm) && (!bypzm)) {
                                        if (skeletonizedArray[j - sliceLength + xDim - 1] == (i+1)) {
                                            numBranches++;
                                        }
                                    }
                                    if ((x > 0) && (y < yDim - 1) && (z < zDim - 1) && (!bxm) && (!byp) && (!bzp) &&
                                        (!bxmyp) && (!bxmzp) && (!bypzp)) {
                                        if (skeletonizedArray[j + sliceLength + xDim - 1] == (i+1)) {
                                            numBranches++;
                                        }
                                    }
                                    if ((x < xDim - 1) && (y > 0) && (z > 0) && (!bxp) && (!bym) && (!bzm) && (!bxpym) &&
                                        (!bxpzm) && (!bymzm)) {
                                        if (skeletonizedArray[j - sliceLength - xDim + 1] == (i+1)) {
                                            numBranches++;
                                        }
                                    }
                                    if ((x < xDim - 1) && (y > 0) && (z < zDim - 1) && (!bxp) && (!bym) && (!bzp) &&
                                        (!bxpym) && (!bxpzp) && (!bymzp)) {
                                        if (skeletonizedArray[j + sliceLength - xDim + 1] == (i+1)) {
                                            numBranches++;
                                        }
                                    }
                                    if ((x < xDim - 1) && (y < yDim - 1) && (z > 0) && (!bxp) && (!byp) && (!bzm) && 
                                        (!bxpyp) && (!bxpzm) && (!bypzm)) {
                                        if (skeletonizedArray[j - sliceLength + xDim + 1] == (i+1)) {
                                            numBranches++;
                                        }
                                    }
                                    if ((x < xDim - 1) && (y < yDim - 1) && (z < zDim - 1) && (!bxp) && (!byp) &&
                                        (!bzp) && (!bxpyp) && (!bxpzp) && (!bypzp)) {
                                        if (skeletonizedArray[j + sliceLength + xDim + 1] == (i+1)) {
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
                    } // for (z = greenFront[i]; z <= greenBack[i]; z++)
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
                newPtVOI = new VOI((short) (greenStrandFocus1[i]), Integer.toString(greenStrandFocus1[i]), VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k];
                yInt[0] = redSkelY[k];
                zInt[0] = redSkelZ[k];
                newPtVOI.importCurve(xInt, yInt, zInt);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(greenStrandFocus1[i]));
                prunedImage.registerVOI(newPtVOI);
                newPtVOI = new VOI((short) (greenStrandFocus2[i]), Integer.toString(greenStrandFocus2[i]), VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xInt[0] = redSkelX[k+1];
                yInt[0] = redSkelY[k+1];
                zInt[0] = redSkelZ[k+1];
                newPtVOI.importCurve(xInt, yInt, zInt);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves().elementAt(0))).setLabel(Integer.toString(greenStrandFocus2[i]));
                prunedImage.registerVOI(newPtVOI);
                k += 2;
            } // if (numGreenStrandFoci[i] == 2)
        } // for (i = 0, k = 0; i < numGreenObjects; i++)
        
        fireProgressStateChanged("Finding distance between foci");
        fireProgressStateChanged(80);
        found = new boolean[totLength];
        noPushIndex = new boolean[totLength];
        fociDistance = new double[numRedColocalize2/2];
        loop = new boolean[numRedColocalize2/2];
        incomplete = new boolean[numRedColocalize/2];
        for (i = 0, k = 0; i < numGreenObjects; i++) {
            if (numGreenStrandFoci[i] == 2) {
                for (j = 0; j < totLength; j++) {
                    found[j] = false;
                    noPushIndex[j] = false;
                }
                branchIndex.removeAllElements();
                distanceVector.removeAllElements();
                x = redSkelX[k];
                y = redSkelY[k];
                z = redSkelZ[k];
                index = redSkelX[k] + xDim * redSkelY[k] + sliceLength * redSkelZ[k];
                found[index] = true;
                while ((x != redSkelX[k+1]) || (y != redSkelY[k+1]) || (z != redSkelZ[k+1])) {
                    numBranches = 0;
                    bxm = false;
                    bxp = false;
                    bym = false;
                    byp = false;
                    bzm = false;
                    bzp = false;
                    bxmym = false;
                    bxmyp = false;
                    bxpym = false;
                    bxpyp = false;
                    bxmzm = false;
                    bxmzp = false;
                    bxpzm = false;
                    bxpzp = false;
                    bymzm = false;
                    bymzp = false;
                    bypzm = false;
                    bypzp = false;
                    bxmymzm = false;
                    bxmymzp = false;
                    bxmypzm = false;
                    bxmypzp = false;
                    bxpymzm = false;
                    bxpymzp = false;
                    bxpypzm = false;
                    bxpypzp = false;
                    if ((x > 0) && (skeletonizedArray[index - 1] == (i+1)) && (!found[index-1])) {
                        bxm = true;
                        numBranches++;
                    }
                    if ((x < xDim - 1) && (skeletonizedArray[index + 1] == (i+1)) && (!found[index+1])) {
                        bxp = true;
                        numBranches++;
                    }
                    if ((y > 0) && (skeletonizedArray[index - xDim] == (i+1)) && (!found[index-xDim])) {
                        bym = true;
                        numBranches++;
                    }
                    if ((y < yDim - 1) && (skeletonizedArray[index + xDim] == (i+1)) && (!found[index+xDim])) {
                        byp = true;
                        numBranches++;
                    }
                    if ((z > 0) && (skeletonizedArray[index - sliceLength] == (i+1)) && (!found[index - sliceLength])) {
                        bzm = true;
                        numBranches++;
                    }
                    if ((z < zDim - 1) && (skeletonizedArray[index + sliceLength] == (i+1)) && (!found[index + sliceLength])) {
                        bzp = true;
                        numBranches++;
                    }
                    if ((x > 0) && (y > 0) && (!bxm) && (!bym) && (skeletonizedArray[index - xDim - 1] == (i+1)) &&
                             (!found[index-xDim-1])) {
                        bxmym = true;
                        numBranches++;
                    }
                    if ((x > 0) && (y < yDim - 1) && (!bxm) && (!byp) && (skeletonizedArray[index + xDim - 1] == (i+1)) &&
                            (!found[index + xDim - 1])) {
                       bxmyp = true;
                       numBranches++;
                    }
                    if ((x < xDim - 1) && (y > 0) && (!bxp) && (!bym) && (skeletonizedArray[index - xDim + 1] == (i+1)) &&
                             (!found[index - xDim + 1])) {
                        bxpym = true;
                        numBranches++;
                    }
                    if ((x < xDim - 1) && (y < yDim - 1) && (!bxp) && (!byp) &&
                             (skeletonizedArray[index + xDim + 1] == (i+1)) && (!found[index + xDim + 1])) {
                        bxpyp = true;
                        numBranches++;
                    }
                    if ((x > 0) && (z > 0) && (!bxm) && (!bzm) && (skeletonizedArray[index - sliceLength - 1] == (i+1)) &&
                             (!found[index-sliceLength-1])) {
                        bxmzm = true;
                        numBranches++;
                    }
                    if ((x > 0) && (z < zDim - 1) && (!bxm) && (!bzp) && (skeletonizedArray[index + sliceLength - 1] == (i+1)) &&
                            (!found[index + sliceLength - 1])) {
                       bxmzp = true;
                       numBranches++;
                    }
                    if ((x < xDim - 1) && (z > 0) && (!bxp) && (!bzm) && (skeletonizedArray[index - sliceLength + 1] == (i+1)) &&
                             (!found[index - sliceLength + 1])) {
                        bxpzm = true;
                        numBranches++;
                    }
                    if ((x < xDim - 1) && (z < zDim - 1) && (!bxp) && (!bzp) &&
                             (skeletonizedArray[index + sliceLength + 1] == (i+1)) && (!found[index + sliceLength + 1])) {
                        bxpzp = true;
                        numBranches++;
                    }
                    if ((y > 0) && (z > 0) && (!bym) && (!bzm) && (skeletonizedArray[index - sliceLength - xDim] == (i+1)) &&
                             (!found[index-sliceLength-xDim])) {
                        bymzm = true;
                        numBranches++;
                    }
                    if ((y > 0) && (z < zDim - 1) && (!bym) && (!bzp) && (skeletonizedArray[index + sliceLength - xDim] == (i+1)) &&
                            (!found[index + sliceLength - xDim])) {
                       bymzp = true;
                       numBranches++;
                    }
                    if ((y < yDim - 1) && (z > 0) && (!byp) && (!bzm) && (skeletonizedArray[index - sliceLength + xDim] == (i+1)) &&
                             (!found[index - sliceLength + xDim])) {
                        bypzm = true;
                        numBranches++;
                    }
                    if ((y < yDim - 1) && (z < zDim - 1) && (!byp) && (!bzp) &&
                             (skeletonizedArray[index + sliceLength + xDim] == (i+1)) && (!found[index + sliceLength + xDim])) {
                        bypzp = true;
                        numBranches++;
                    }
                    if ((x > 0) && (y > 0) && (z > 0) && (!bxm) && (!bym) && (!bzm) && (!bxmym) && (!bxmzm) && (!bymzm) &&
                        (skeletonizedArray[index - sliceLength - xDim - 1] == (i+1)) && (!found[index - sliceLength - xDim - 1])) {
                        bxmymzm = true;
                        numBranches++;
                    }
                    if ((x > 0) && (y > 0) && (z < zDim - 1) && (!bxm) && (!bym) && (!bzp) && (!bxmym) && (!bxmzp) && (!bymzp) &&
                        (skeletonizedArray[index + sliceLength - xDim - 1] == (i+1)) && (!found[index + sliceLength - xDim - 1])) {
                        bxmymzp = true;
                        numBranches++;
                    }
                    if ((x > 0) && (y < yDim - 1) && (z > 0) && (!bxm) && (!byp) && (!bzm) && (!bxmyp) && (!bxmzm) && (!bypzm) &&
                        (skeletonizedArray[index - sliceLength + xDim - 1] == (i+1)) && (!found[index - sliceLength + xDim - 1])) {
                        bxmypzm = true;
                        numBranches++;
                    }
                    if ((x > 0) && (y < yDim - 1) && (z < zDim - 1) && (!bxm) && (!byp) && (!bzp) && (!bxmyp) && (!bxmzp) && (!bypzp) &&
                        (skeletonizedArray[index + sliceLength + xDim - 1] == (i+1)) && (!found[index + sliceLength + xDim - 1])) {
                        bxmypzp = true;
                        numBranches++;
                    }
                    if ((x < xDim - 1) && (y > 0) && (z > 0) && (!bxp) && (!bym) && (!bzm) && (!bxpym) && (!bxpzm) && (!bymzm) &&
                        (skeletonizedArray[index - sliceLength - xDim + 1] == (i+1)) && (!found[index - sliceLength - xDim + 1])) {
                        bxpymzm = true;
                        numBranches++;
                    }
                    if ((x < xDim - 1) && (y > 0) && (z < zDim - 1) && (!bxp) && (!bym) && (!bzp) && (!bxpym) && (!bxpzp) && (!bymzp) &&
                        (skeletonizedArray[index + sliceLength - xDim + 1] == (i+1)) && (!found[index + sliceLength - xDim + 1])) {
                        bxpymzp = true;
                        numBranches++;
                    }
                    if ((x < xDim - 1) && (y < yDim - 1) && (z > 0) && (!bxp) && (!byp) && (!bzm) && (!bxpyp) && (!bxpzm) && (!bypzm) &&
                        (skeletonizedArray[index - sliceLength + xDim + 1] == (i+1)) && (!found[index - sliceLength + xDim + 1])) {
                        bxpypzm = true;
                        numBranches++;
                    }
                    if ((x < xDim - 1) && (y < yDim - 1) && (z < zDim - 1) && (!bxp) && (!byp) && (!bzp) && (!bxpyp) && (!bxpzp) &&
                        (!bypzp) &&
                        (skeletonizedArray[index + sliceLength + xDim + 1] == (i+1)) && (!found[index + sliceLength + xDim + 1])) {
                        bxpypzp = true;
                        numBranches++;
                    }
                    if (numBranches == 0) {
                        loop[k/2] = true;
                        if (!branchIndex.isEmpty()) {
                            index = branchIndex.popFirstIn();
                            x = (index % sliceLength) % xDim;
                            y = (index % sliceLength) / xDim;
                            z = index / sliceLength;
                            fociDistance[k/2] = distanceVector.remove(0).doubleValue();
                            continue;
                        }
                        else {
                            incomplete[k/2] = true;
                            break;
                        }
                    }
                    if ((numBranches >= 2) && (!noPushIndex[index])) {
                        for (m = 0; m < numBranches - 1; m++) {
                            branchIndex.push(index);
                            distanceVector.add(Double.valueOf(fociDistance[k/2]));
                        }
                        noPushIndex[index] = true;
                    }
                    if (bxm) {
                        index = index - 1;
                        x = x - 1;
                        found[index] = true;
                        fociDistance[k/2] += resX;    
                    }
                    else if (bxp) {
                        index = index + 1;
                        x = x + 1;
                        found[index] = true;
                        fociDistance[k/2] += resX;
                    }
                    else if (bym) {
                        index = index - xDim;
                        y = y - 1;
                        found[index] = true;
                        fociDistance[k/2] += resY;    
                    }
                    else if (byp) {
                        index = index + xDim;
                        y = y + 1;
                        found[index] = true;
                        fociDistance[k/2] += resY;
                    }
                    else if (bzm) {
                        index = index - sliceLength;
                        z = z - 1;
                        found[index] = true;
                        fociDistance[k/2] += resZ;
                    }
                    else if (bzp) {
                        index = index + sliceLength;
                        z = z + 1;
                        found[index] = true;
                        fociDistance[k/2] += resZ;
                    }
                    else if (bxmym) {
                        index = index - xDim - 1;
                        x = x - 1;
                        y = y - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXY;
                    }
                    else if (bxmyp) {
                        index = index + xDim - 1;
                        x = x - 1;
                        y = y + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXY;
                    }
                    else if (bxpym) {
                        index = index - xDim + 1;
                        x = x + 1;
                        y = y - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXY;
                    }
                    else if (bxpyp) {
                        index = index + xDim + 1;
                        x = x + 1;
                        y = y + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXY;
                    }
                    else if (bxmzm) {
                        index = index - sliceLength - 1;
                        x = x - 1;
                        z = z - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXZ;
                    }
                    else if (bxmzp) {
                        index = index + sliceLength - 1;
                        x = x - 1;
                        z = z + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXZ;
                    }
                    else if (bxpzm) {
                        index = index - sliceLength + 1;
                        x = x + 1;
                        z = z - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXZ;
                    }
                    else if (bxpzp) {
                        index = index + sliceLength + 1;
                        x = x + 1;
                        z = z + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXZ;
                    }
                    else if (bymzm) {
                        index = index - sliceLength - xDim;
                        y = y - 1;
                        z = z - 1;
                        found[index] = true;
                        fociDistance[k/2] += resYZ;
                    }
                    else if (bymzp) {
                        index = index + sliceLength - xDim;
                        y = y - 1;
                        z = z + 1;
                        found[index] = true;
                        fociDistance[k/2] += resYZ;
                    }
                    else if (bypzm) {
                        index = index - sliceLength + xDim;
                        y = y + 1;
                        z = z - 1;
                        found[index] = true;
                        fociDistance[k/2] += resYZ;
                    }
                    else if (bypzp) {
                        index = index + sliceLength + xDim;
                        y = y + 1;
                        z = z + 1;
                        found[index] = true;
                        fociDistance[k/2] += resYZ;
                    }
                    else if (bxmymzm) {
                        index = index - sliceLength - xDim - 1;
                        x = x - 1;
                        y = y - 1;
                        z = z - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXYZ;
                    }
                    else if (bxmymzp) {
                        index = index + sliceLength - xDim - 1;
                        x = x - 1;
                        y = y - 1;
                        z = z + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXYZ;
                    }
                    else if (bxmypzm) {
                        index = index - sliceLength + xDim - 1;
                        x = x - 1;
                        y = y + 1;
                        z = z - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXYZ;
                    }
                    else if (bxmypzp) {
                        index = index + sliceLength + xDim - 1;
                        x = x - 1;
                        y = y + 1;
                        z = z + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXYZ;
                    }
                    else if (bxpymzm) {
                        index = index - sliceLength - xDim + 1;
                        x = x + 1;
                        y = y - 1;
                        z = z - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXYZ;
                    }
                    else if (bxpymzp) {
                        index = index + sliceLength - xDim + 1;
                        x = x + 1;
                        y = y - 1;
                        z = z + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXYZ;
                    }
                    else if (bxpypzm) {
                        index = index - sliceLength + xDim + 1;
                        x = x + 1;
                        y = y + 1;
                        z = z - 1;
                        found[index] = true;
                        fociDistance[k/2] += resXYZ;
                    }
                    else if (bxpypzp) {
                        index = index + sliceLength + xDim + 1;
                        x = x + 1;
                        y = y + 1;
                        z = z + 1;
                        found[index] = true;
                        fociDistance[k/2] += resXYZ;
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
                if (incomplete[k]) {
                    UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", greenStrandFocus1[i] + "\t" +
                                                 greenStrandFocus2[i] + "\n"); 
                }
                else if (!loop[k] ){
                    UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", greenStrandFocus1[i] + "\t" +
                            greenStrandFocus2[i] +  "\t" + df.format(fociDistance[k]) + "\n");     
                }
                else {
                    UI.getMessageFrame().append("PlugInAlgorithmFociAndStrands", greenStrandFocus1[i] + "\t" +
                            greenStrandFocus2[i] +  "\t" + df.format(fociDistance[k]) + "\t" + "loop\n");         
                }
                k++;
            }
        }
        
        if (threadStopped) {
            finalize();

            return;
        }
        
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmFociAndStrands elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
    }

    
}
