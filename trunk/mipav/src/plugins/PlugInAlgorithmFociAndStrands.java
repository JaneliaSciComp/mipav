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
 * @version  July 18, 2008
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
 *           
 *           <p>1.) Obtain the red portion of the image.
 *           
 *           <p>2.) Obtain a red histogram.  Set the threshold so that the redFraction
 *           portion of the cumulative histogram is at or above threshold for the fuzzy c means.
 *           
 *           <p>3.) Perform a 3 level fuzzy c means segmentation on the red image.  Values below threshold
 *           are set equal to 0 and values >= threshold are assigned values of 1, 2, and 3
 *            
 *           <p>4.) Convert the red fuzzy segmented value of 3 to 1 and the other values to 0. 
 *           
 *           <p>5.) ID objects in red segmented image which have at least redMin pixels.
 *           
 *           <p>6.) Export the green portion of the image to greenBuffer.
 *
 *           <p>7.) Obtain histogram information on the green image. Set the threshold so that the greenFraction
 *           portion of the cumulative histogram is at or above threshold for the fuzzy c means. 
 *           
 *           <p>8.) Perform a 3 level fuzzy c means segmentation on the green image.  Green values below threshold
 *           are set to 0, and green values >= threshold are set to values of 1, 2, and 3.
 *           
 *           <p>9.) Create an image in which all the green values originally equal to 3 in the fuzzy
 *           c means segmentation are set to 1 and all the other green values are set to 0.
 *           
 *           <p>10.) ID objects in this green segmented image which have at least greenMin pixels.
 *           
 *           
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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  srcImg         Source image model.
     * @param  redMin
     * @param  greenMin
     */
    public PlugInAlgorithmFociAndStrands(ModelImage srcImg, int redMin, float redFraction, int greenMin,
                                         float greenFraction) {
        super(null, srcImg);
        this.redMin = redMin;
        this.redFraction = redFraction;
        this.greenMin = greenMin;
        this.greenFraction = greenFraction;
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
        int i;
        int x, y;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        AlgorithmMorphology2D idObjectsAlgo2D;
        byte[] byteBuffer;
        float[] greenBuffer = null;
        ViewVOIVector VOIs = null;
        int nVOIs;
        int index;
        ViewUserInterface UI = ViewUserInterface.getReference();
        VOI newPtVOI;
        float[] xArr = new float[1];
        float[] yArr = new float[1];
        float[] zArr = new float[1];

        FileInfoBase fileInfo;
        int numGreenObjects = 0;
        short[] greenIDArray = null;
        int numGreenStrandFoci[];
        int greenIDMinus1;
        float avgFociOnStrands;
        int greenStrandFocus1[];
        int greenStrandFocus2[];
        
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
        
        grayImage.disposeLocal();
        grayImage = null;
        
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
        for (i = 0; i < numRedObjects; i++) {
            redX[i] = redX[i]/redTotal[i];
            redY[i] = redY[i]/redTotal[i];
            if (redOnGreen[i] != 0) {
                greenIDMinus1 = redOnGreen[i] - 1;
                numGreenStrandFoci[greenIDMinus1]++;
                if (numGreenStrandFoci[greenIDMinus1] == 1) {
                    greenStrandFocus1[greenIDMinus1] = numRedColocalize + 1;
                }
                else if (numGreenStrandFoci[greenIDMinus1] == 2) {
                    greenStrandFocus2[greenIDMinus1] = numRedColocalize + 1;
                }
                newPtVOI = new VOI((short) (numRedColocalize + nVOIs), Integer.toString(numRedColocalize+1), 1, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xArr[0] = redX[i];
                yArr[0] = redY[i];
                zArr[0] = 0.0f;
                newPtVOI.importCurve(xArr, yArr, zArr, 0);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[0].elementAt(0))).setLabel(Integer.toString(numRedColocalize + 1));
                srcImage.registerVOI(newPtVOI);
                numRedColocalize++;
            }
        }
        Preferences.debug(numRedColocalize + " of " + numRedObjects + " red foci co-localize with green strands\n");
        
        avgFociOnStrands = (float)numRedColocalize/(float)numGreenObjects;

        srcImage.notifyImageDisplayListeners();
        
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
    
    /**
     * DOCUMENT ME!
     */
    private void calc3D() {

        BitSet imageMask = null;
        short redOnGreen[];
        int numRedColocalize;
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
        int i, j;
        int x, y, z;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        AlgorithmMorphology3D idObjectsAlgo3D;
        byte[] byteBuffer;
        float[] greenBuffer = null;
        ViewVOIVector VOIs = null;
        int nVOIs;
        float[] xArr = new float[1];
        float[] yArr = new float[1];
        float[] zArr = new float[1];
        int index;
        ViewUserInterface UI = ViewUserInterface.getReference();

        FileInfoBase fileInfo;
        int numRedObjects = 0;
        int numGreenObjects = 0;
        short[] redIDArray = null;
        short[] greenIDArray = null;
        
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
        
        
        grayImage.disposeLocal();
        grayImage = null;
        
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
        for (i = 0; i < numRedObjects; i++) {
            redX[i] = redX[i]/redTotal[i];
            redY[i] = redY[i]/redTotal[i];
            redZ[i] = redZ[i]/redTotal[i];
            if (redOnGreen[i] != 0) {
                greenIDMinus1 = redOnGreen[i] - 1;
                numGreenStrandFoci[greenIDMinus1]++;
                if (numGreenStrandFoci[greenIDMinus1] == 1) {
                    greenStrandFocus1[greenIDMinus1] = numRedColocalize + 1;
                }
                else if (numGreenStrandFoci[greenIDMinus1] == 2) {
                    greenStrandFocus2[greenIDMinus1] = numRedColocalize + 1;
                }
                newPtVOI = new VOI((short) (numRedColocalize + nVOIs), Integer.toString(numRedColocalize+1), zDim, VOI.POINT, -1.0f);
                newPtVOI.setColor(Color.white);
                xArr[0] = redX[i];
                yArr[0] = redY[i];
                zArr[0] = Math.round(redZ[i]);
                newPtVOI.importCurve(xArr, yArr, zArr, (int)zArr[0]);
                ((VOIPoint) (newPtVOI.getCurves()[(int)zArr[0]].elementAt(0))).setFixed(true);
                ((VOIPoint) (newPtVOI.getCurves()[(int)zArr[0]].elementAt(0))).setLabel(Integer.toString(numRedColocalize + 1));
                srcImage.registerVOI(newPtVOI);
                numRedColocalize++;
            }
        }
        Preferences.debug(numRedColocalize + " of " + numRedObjects + " red foci co-localize with green strands\n");
        
        avgFociOnStrands = (float)numRedColocalize/(float)numGreenObjects;

        srcImage.notifyImageDisplayListeners();
        
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
