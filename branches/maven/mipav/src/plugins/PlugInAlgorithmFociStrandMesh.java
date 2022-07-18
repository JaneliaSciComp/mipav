import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;

import java.util.*;


/**
 *
 * @version  October 17, 2008
 * @author   DOCUMENT ME!
 * @see      AlgorithmBase
 *
 *           <p>$Logfile: /mipav/src/plugins/PlugInAlgorithmFociAndStrands.java $ $Revision: 72 $ $Date: 2/06/06 5:50p $
 *           PlugInAlgorithmFociStrandMesh is used to:
 *           1. Count the total number of foci(red dots) that colocalize with the green strands.
 *           2. Indicate with a white circle those foci counted as colocalized.
 *           3. Count of all the red foci that do not colocalize with green strands.</p>
 *          
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
 *           if (greenBuffer[i] >= green threshold), set greenArray equal to 1.  Otherwise greenArray is 0.  
 *           
 *           <p>6.) For each red object find if there is any green object with which it colocalizes.
 *           Calculate the center of mass of the red object.
 *           
 *           <p>7.) Display the positions of the colocalized red foci on the source image with circles.
 *           
 */
public class PlugInAlgorithmFociStrandMesh extends AlgorithmBase {

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
    
    //private ModelImage redImage;
    //private ViewJFrameImage redFrame;
    private ModelImage greenImage;
    private ViewJFrameImage greenFrame;
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
     * @param  greenFraction  Strand requires a minimum green value >=
     *                        image green min + greeFraction * (image green max - image green min)
     * @param  radius         Raidus of circles drawn around colocalized foci
     */
    public PlugInAlgorithmFociStrandMesh(ModelImage srcImg, int redMin, float redFraction,
                                         float greenFraction, float radius) {
        super(null, srcImg);
        this.redMin = redMin;
        this.redFraction = redFraction;
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
        byte redOnGreen[];
        float redX[];
        float redY[];
        float redTotal[];
        int numRedColocalize;
        int numRedNotColocalize;
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
        int i, j;
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
        VOI newCircleVOI;
        int maxCirclePoints = (int)Math.ceil(2.0 * Math.PI * radius);
        float[] xArr = new float[maxCirclePoints];
        float[] yArr = new float[maxCirclePoints];
        float[] zArr = new float[maxCirclePoints];
        double theta;
        FileInfoBase fileInfo;
        byte[] greenArray = null;
        
        long time;
        int fileNameLength;
        Font courier = MipavUtil.courier12;

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
            errorCleanUp("Algorithm FociStrandMesh reports: source image locked", true);

            return;
        }

        grayImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), srcImage.getImageName() + "_gray");
        fileInfo = grayImage.getFileInfo()[0];
        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        grayImage.setFileInfo(fileInfo, 0);
        
        fireProgressStateChanged("Thresholding red image");
        fireProgressStateChanged(20);
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
        fireProgressStateChanged(30);
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
        fireProgressStateChanged(40);
        
        greenBuffer = new float[length];

        try {

            srcImage.exportRGBData(2, 0, length, greenBuffer); // export green data
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Algorithm FociStrandMesh reports: source image locked", true);

            return;
        }   
        
        fireProgressStateChanged("Thresholding green image");
        fireProgressStateChanged(60);
        threshold = (float)(srcImage.getMinG() + greenFraction * (srcImage.getMaxG() - srcImage.getMinG()));
        Preferences.debug("Green threshold = " + threshold + "\n");
        
        greenArray = new byte[length];
        for (i = 0; i < length; i++) {
            if ((wholeImage || imageMask.get(i)) && (greenBuffer[i] >= threshold)) {
                greenArray[i] = 1;
            }
            else {
                greenArray[i] = 0;
            }
        }
        srcImage.clearMask();
        greenImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), srcImage.getImageName() + "_green");
        try {
            greenImage.importData(0, greenArray, true);
        } catch (IOException error) {
            byteBuffer = null;
            greenArray = null;
            errorCleanUp("Error on greenImage.importData", true);

            return;
        }
        greenImage.setImageName(srcImage.getImageName() + "_green");
        greenFrame = new ViewJFrameImage(greenImage);
        greenFrame.setTitle(srcImage.getImageName() + "_green");
        
        
        fireProgressStateChanged("Finding red foci colocalized with green strands");
        fireProgressStateChanged(80);
        redOnGreen = new byte[numRedObjects];
        redX = new float[numRedObjects];
        redY = new float[numRedObjects];
        redTotal = new float[numRedObjects];
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
                        if (greenArray[i] == 1) {
                            redOnGreen[redIDMinus1] = 1;
                        }
                        else if ((x >= 1) && (greenArray[i-1] == 1)) {
                            redOnGreen[redIDMinus1] = 1;
                        }
                        else if ((x <= xDim - 2) && (greenArray[i+1] == 1)) {
                            redOnGreen[redIDMinus1] = 1;
                        }
                        else if ((y >= 1) && (greenArray[i - xDim] == 1)) {
                            redOnGreen[redIDMinus1] = 1;
                        }
                        else if ((y <= yDim - 2) && (greenArray[i + xDim] == 1)) {
                            redOnGreen[redIDMinus1] = 1;
                        }
                    }
                }
            }
        }
        numRedColocalize = 0;
        for (i = 0; i < numRedObjects; i++) {
            redX[i] = redX[i]/redTotal[i];
            redY[i] = redY[i]/redTotal[i];
            if (redOnGreen[i] == 1) {
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
        numRedNotColocalize = numRedObjects - numRedColocalize;
        
        
        grayImage.disposeLocal();
        grayImage = null;

        srcImage.notifyImageDisplayListeners();
        
        UI.getMessageFrame().addTab("PlugInAlgorithmFociStrandMesh");
        UI.getMessageFrame().setFont("PlugInAlgorithmFociStrandMesh", courier);
        UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh", "\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh", "File Name");
        fileNameLength = srcImage.getFileInfo(0).getFileName().length();
        if (fileNameLength > 9) {
            for (i = 0; i < fileNameLength - 9; i++) {
                UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh", " ");
            }
        }
        UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh", " \tNFoci\tNFociOnStrands\tNFociNotOnStrands\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh",
                srcImage.getFileInfo(0).getFileName() + " \t" + numRedObjects + "\t" + numRedColocalize + "\t\t" +
                numRedNotColocalize + "\n");
        
        

        if (threadStopped) {
            finalize();

            return;
        }

        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmFociStrandMesh elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
    }
    
    /**
     * DOCUMENT ME!
     */
    private void calc3D() {

        BitSet imageMask = null;
        byte redOnGreen[];
        int numRedColocalize;
        int numRedNotColocalize;
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
        short[] redIDArray = null;
        byte[] greenArray = null;
        int index2;
        
        long time;
        int fileNameLength;
        Font courier = MipavUtil.courier12;
        float redX[];
        float redY[];
        float redZ[];
        float redTotal[];
        int redIDMinus1;
        
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
        fireProgressStateChanged(20);
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
        fireProgressStateChanged(30);
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
        fireProgressStateChanged(40);
        
        greenBuffer = new float[totLength];
        
        try {

            srcImage.exportRGBData(2, 0, totLength, greenBuffer); // export green data
        } catch (IOException error) {
            greenBuffer = null;
            errorCleanUp("Algorithm FociAndStrands reports: source image locked", true);

            return;
        }  
        
        fireProgressStateChanged("Thresholding green image");
        fireProgressStateChanged(60);
        threshold = (float)(srcImage.getMinG() + greenFraction * (srcImage.getMaxG() - srcImage.getMinG()));
        Preferences.debug("Green threshold = " + threshold + "\n");
        
        greenArray = new byte[totLength];
        for (i = 0; i < totLength; i++) {
            if ((wholeImage || imageMask.get(i)) && (greenBuffer[i] >= threshold)) {
                greenArray[i] = 1;
            }
            else {
                greenArray[i] = 0;
            }
        }
        srcImage.clearMask();
        greenImage = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), srcImage.getImageName() + "_green");
        try {
            greenImage.importData(0, greenArray, true);
        } catch (IOException error) {
            byteBuffer = null;
            greenArray = null;
            errorCleanUp("Error on greenImage.importData", true);

            return;
        }
        greenImage.setImageName(srcImage.getImageName() + "_green");
        greenFrame = new ViewJFrameImage(greenImage);
        greenFrame.setTitle(srcImage.getImageName() + "_green");

        
        
        fireProgressStateChanged("Finding red foci colocalized with green strands");
        fireProgressStateChanged(80);
        redOnGreen = new byte[numRedObjects];
        redX = new float[numRedObjects];
        redY = new float[numRedObjects];
        redZ = new float[numRedObjects];
        redTotal = new float[numRedObjects];
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
                            if (greenArray[i] == 1) {
                                redOnGreen[redIDMinus1] = 1;
                            }
                            else if ((x >= 1) && (greenArray[i-1] == 1)) {
                                redOnGreen[redIDMinus1] = 1;
                            }
                            else if ((x <= xDim - 2) && (greenArray[i+1] == 1)) {
                                redOnGreen[redIDMinus1] = 1;
                            }
                            else if ((y >= 1) && (greenArray[i - xDim] == 1)) {
                                redOnGreen[redIDMinus1] = 1;
                            }
                            else if ((y <= yDim - 2) && (greenArray[i + xDim] == 1)) {
                                redOnGreen[redIDMinus1] = 1;
                            }
                            else if ((z >= 1) && (greenArray[i - sliceLength] == 1)) {
                                redOnGreen[redIDMinus1] = 1;
                            }
                            else if ((z <= zDim - 2) && (greenArray[i + sliceLength] == 1)) {
                                redOnGreen[redIDMinus1] = 1;
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
            if (redOnGreen[i] == 1) {
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
        numRedNotColocalize = numRedObjects - numRedColocalize;
        
        
        grayImage.disposeLocal();
        grayImage = null;

        srcImage.notifyImageDisplayListeners();
        
        UI.getMessageFrame().addTab("PlugInAlgorithmFociStrandMesh");
        UI.getMessageFrame().setFont("PlugInAlgorithmFociStrandMesh", courier);
        UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh", "\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh", "File Name");
        fileNameLength = srcImage.getFileInfo(0).getFileName().length();
        if (fileNameLength > 9) {
            for (i = 0; i < fileNameLength - 9; i++) {
                UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh", " ");
            }
        }
        UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh", " \tNFoci\tNFociOnStrands\tNFociNotOnStrands\n");
        UI.getMessageFrame().append("PlugInAlgorithmFociStrandMesh",
                srcImage.getFileInfo(0).getFileName() + " \t" + numRedObjects + "\t" + numRedColocalize + "\t\t" +
                numRedNotColocalize + "\n");
        
        if (threadStopped) {
            finalize();

            return;
        }
        
        fireProgressStateChanged(100);
        time = System.currentTimeMillis() - time;
        Preferences.debug("PlugInAlgorithmFociStrandMesh elapsed time in seconds = " + (time/1000.0));
        setCompleted(true);
    }

    
}
