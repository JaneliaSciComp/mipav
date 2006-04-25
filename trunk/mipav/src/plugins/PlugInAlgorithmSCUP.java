import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * The OAI -- Osteoarthritis Initiative – is a nationwide research study sponsored by the National Institutes of Health,
 * that will help us better understand how to prevent and treat knee osteoarthritis, one of the most common causes of
 * disability in adults. It is a four-year study and will recruit men and women aged 45 and above at high risk for
 * developing symptomatic knee osteoarthritis. Osteoarthritis causes more health problems and medical expenses than any
 * other form of arthritis. Symptoms of osteoarthritis can range from stiffness and mild pain to severe joint pain and
 * even disability. The OAI cohort will be 5000 participants with clinically significant knee OA or at high risk for
 * developing incident OA and obtain the appropriate images and bio-specimens needed for investigation and validation of
 * OA biomarkers. The large number of images that results from the OAI is a major obstacle to overcome. Manual image
 * segmentation is laborious and subject to inter and intra-observer variability when performing volumetric analysis.
 * Therefore, BIRSS has started a multistage segmentation and quantification technique to automatically or
 * semi-automatically process the entire cohort.
 */
public class PlugInAlgorithmSCUP extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage destImage = null;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private float[] imgBuffer2 = null;

    /** DOCUMENT ME! */
    private BitSet obMask = null;

    /** DOCUMENT ME! */
    private ModelImage srcImage = null;

    /** DOCUMENT ME! */
    private ModelImage tempImage1 = null; // thresholded black 80,100

    /** DOCUMENT ME! */
    private ModelImage tempImage1a = null; // thresholded black IDobjects 0,50

    /** DOCUMENT ME! */
    private ModelImage tempImage1b = null; // thresholded black IDobjects 50,5000

    /** DOCUMENT ME! */
    private ModelImage tempImage1c = null; // thresholded black IDobjects BONE 5000,15000

    /** DOCUMENT ME! */
    private ModelImage tempImage2 = null; // thresholded muscle 100,200

    /** DOCUMENT ME! */
    private ModelImage tempImage2a = null; // thresholded muscle IDobjects 0,5000

    /** DOCUMENT ME! */
    private ModelImage tempImage3 = null; // thresholded 95,100 bone marrow

    /** DOCUMENT ME! */
    private ModelImage tempImage3a = null; // thresholded bone marrow IDobjects 5000,15000

    /** DOCUMENT ME! */
    private ModelImage tempImage3b = null; // thresholded bone marrow IDobjects 5000,15000

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     *
     * @param  destImage  the result image
     * @param  srcImage   the source image
     */
    public PlugInAlgorithmSCUP(ModelImage destImage, ModelImage srcImage) {
        super(destImage, srcImage);
        this.srcImage = srcImage;
        this.destImage = destImage;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up the algorithm's memory.
     */
    public void disposeLocal() {
        imgBuffer = null;
    }

    /**
     * Clean up the algorithm's memory.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * Return the algorithm's result image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return this.destImage;
    }

    /**
     * Run method for algorithm.
     */
    public void runAlgorithm() {
        int i, j;
        int x, y;
        int xx, yy;
        int bkgrdFound;
        int z = 1;
        int xDim, yDim, sliceSize;
        int bkgrd = 85;
        int muscle = 170;
        int fat = 255;
        int bone = 100;
        int bone_marrow = 200;
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        imgBuffer = new float[sliceSize];
        imgBuffer2 = new float[sliceSize];

        if (srcImage.getNDims() == 3) {
            z = srcImage.getExtents()[2];
        }

        /*--------------------------------------------------------------------------*/
        /*----------------- Boundary Correction algorithm here--------------------- */
        /*--------------------------------------------------------------------------*/
        for (j = 0; j < z; j++) {

            try {
                srcImage.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer);
                obMask = new BitSet(sliceSize);

                // -----------setting the outer bkgrd imgBuffers on mask----------
                // first column
                x = 0;

                for (y = 0; y < yDim; y++) {
                    i = x + (y * xDim);

                    if (imgBuffer[i] == bkgrd) {
                        obMask.set(i);
                    }
                }

                // last column
                x = xDim - 1;

                for (y = 0; y < yDim; y++) {
                    i = x + (y * xDim);

                    if (imgBuffer[i] == bkgrd) {
                        obMask.set(i);
                    }
                }

                // first row
                y = 0;

                for (x = 0; x < xDim; x++) {
                    i = x + (y * xDim);

                    if (imgBuffer[i] == bkgrd) {
                        obMask.set(i);
                    }
                }

                // last row
                y = yDim - 1;

                for (x = 0; x < xDim; x++) {
                    i = x + (y * xDim);

                    if (imgBuffer[i] == bkgrd) {
                        obMask.set(i);
                    }
                }

                // setting bkgrd imgBuffers 4-connected to 1 of original boundary, as background.
                do {
                    bkgrdFound = 0;

                    for (y = 0; y < yDim; y++) {

                        for (x = 0; x < xDim; x++) {
                            i = x + (y * xDim);

                            if (obMask.get(i)) {

                                // checks left nearest neighbor
                                if ((x != 0) && (imgBuffer[i - 1] == bkgrd) && (!obMask.get(i - 1))) {
                                    obMask.set(i - 1);
                                    bkgrdFound++;
                                }

                                // right nearest neighbor
                                if ((x != (xDim - 1)) && (imgBuffer[i + 1] == bkgrd) && (!obMask.get(i + 1))) {
                                    obMask.set(i + 1);
                                    bkgrdFound++;
                                }

                                // top
                                if ((y != 0) && (imgBuffer[i - xDim] == bkgrd) && (!obMask.get(i - xDim))) {
                                    obMask.set(i - xDim);
                                    bkgrdFound++;
                                }

                                // bottom
                                if ((y != (yDim - 1)) && (imgBuffer[i + xDim] == bkgrd) && (!obMask.get(i + xDim))) {
                                    obMask.set(i + xDim);
                                    bkgrdFound++;
                                }
                            }
                        }
                    }
                } while (bkgrdFound > 0);

                // convert gray imgBuffer with outer bkgrd imgBuffer in its 5x5 neighborhood, into bkgrd
                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        i = x + (y * xDim);

                        if (imgBuffer[i] == muscle) {

                            // check 5x5 neighborhood
                            if ((x != 0) && (y != 0) && (x != (xDim - 1)) && (y != (yDim - 1))) {

                                for (yy = -2; yy <= 2; yy++) {

                                    for (xx = -2; xx <= 2; xx++) {

                                        if (obMask.get(i + xx + (yy * xDim))) {
                                            imgBuffer[i] = bkgrd;
                                            obMask.set(i + xx + (yy * xDim));
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // convert next 2 pixels (muscle) radially inward of boundary, into fat
                for (y = 2; y < (yDim - 2); y++) {

                    for (x = 2; x < (xDim - 2); x++) {
                        i = x + (y * xDim);

                        // check 5x5 neighborhood
                        if (imgBuffer[i] == muscle) {

                            for (yy = -6; yy <= 6; yy++) {

                                for (xx = -6; xx <= 6; xx++) {

                                    if (obMask.get(i + xx + (yy * xDim))) {
                                        imgBuffer[i] = fat;
                                    }
                                }
                            }
                        }
                    }
                }

                destImage.importData((j * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmSCUP");
            }
        }

        System.out.println("done with boundary correction loop");
        destImage.calcMinMax();
        setCompleted(true);

        /*--------------------------------------------------------------------------*/
        /*----------------- Boundary Correction algorithm END here----------------- */
        /*--------------------------------------------------------------------------*/
        // ****************************what i really plan to use****************************
        // AlgorithmMask VOIExtractAlgo = null;
        // VOIExtractAlgo = new AlgorithmMask(destImage, srcImage, 0,false,true);
        // VOIExtractAlgo.run();
        /*--------------------------------------------------------------------------*/
        /*------------------- NOISE Correction algorithm here---------------------- */
        /*--------------------------------------------------------------------------*/
        // 1.CONVERTING INSIDE BLACK NOISE into MUSCLE
        // THRESHOLD BLACK (85-100)
        float[] threshold1 = { 85, 100 }; // threshold all black
        AlgorithmThresholdDual threshAlgo1 = null;
        tempImage1 = new ModelImage(destImage.getType(), destImage.getExtents(), "TEMP IMAGE",
                                    destImage.getUserInterface());
        threshAlgo1 = new AlgorithmThresholdDual(tempImage1, destImage, threshold1, 1, true, true, true);
        threshAlgo1.run();
        tempImage1a = (ModelImage) tempImage1.clone(); // thresholded background image
        tempImage1b = (ModelImage) tempImage1.clone(); // thresholded background image
        tempImage1c = (ModelImage) tempImage1.clone(); // thresholded background image

        // ID_OBJECTS SMALL NOISE (0-50)
        AlgorithmMorphology3D idObjAlgo1 = null;
        idObjAlgo1 = new AlgorithmMorphology3D(tempImage1a, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        idObjAlgo1.setMinMax(0, 50);
        idObjAlgo1.run();

        // CONVERTING BLACK NOISE TO MUSCLE
        for (j = 0; j < z; j++) {

            try {
                destImage.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer); // thresholded background
                tempImage1a.exportData((j * imgBuffer2.length), imgBuffer2.length, imgBuffer2); // thresholded

                // background noise
                for (i = 0; i < imgBuffer.length; i++) {

                    if (imgBuffer2[i] != 0) { // whenever there's noise..
                        imgBuffer[i] = muscle; // make image background pixel = muscle
                    }
                }

                destImage.importData((j * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmSCUP2");
            }
        }

        System.out.println("1 done");

        // 2. CONVERTING REMAINING BLACK INSIDE CONTOURS TO FAT
        // RE-USE black THRESHOLD - tempImage1b
        // ID_OBJECTS REMAINING LARGER BLACK INSIDE CONTOURS (50-5000)
        AlgorithmMorphology3D idObjAlgo1a = null;
        idObjAlgo1a = new AlgorithmMorphology3D(tempImage1b, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        idObjAlgo1a.setMinMax(50, 5000);
        idObjAlgo1a.run();

        // CONVERTING INSIDE BLACK CONTOURS TO FAT
        for (j = 0; j < z; j++) {

            try {
                destImage.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer);
                tempImage1b.exportData((j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);

                for (i = 0; i < imgBuffer.length; i++) {

                    if (imgBuffer2[i] != 0) {
                        imgBuffer[i] = fat;
                    }
                }

                destImage.importData((j * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmSCUP2");
            }
        }

        System.out.println("2 done");

        // 3. Converting White Inner Noise into muscle
        // Re-Use fat threshold tempImage2a
        float[] threshold2 = { 100, 200 };
        AlgorithmThresholdDual threshAlgo2 = null;
        tempImage2 = new ModelImage(destImage.getType(), destImage.getExtents(), "TEMP IMAGE2",
                                    destImage.getUserInterface());
        threshAlgo2 = new AlgorithmThresholdDual(tempImage2, destImage, threshold2, 1, true, true, true);
        threshAlgo2.run();
        tempImage2a = (ModelImage) tempImage2.clone();

        // ID_OBJECTS
        AlgorithmMorphology3D idObjAlgo2 = null;
        idObjAlgo2 = new AlgorithmMorphology3D(tempImage2, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        idObjAlgo2.setMinMax(0, 50);
        idObjAlgo2.run();

        // CONVERTING
        for (j = 0; j < z; j++) {

            try {
                destImage.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer);
                tempImage2.exportData((j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);

                for (i = 0; i < imgBuffer.length; i++) {

                    if (imgBuffer2[i] != 0) {
                        imgBuffer[i] = muscle;
                    }
                }

                destImage.importData((j * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmSCUP2");
            }
        }

        System.out.println("3 done");

        // 4. CONVERTING OUTER MUSCLE ARTIFACT TO FAT ***only needs to be done on subcutaneous fat region**
        // THRESHOLD
        // ID_OBJECTS
        AlgorithmMorphology3D idObjAlgo3 = null;
        idObjAlgo3 = new AlgorithmMorphology3D(tempImage2a, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        idObjAlgo3.setMinMax(0, 5000);
        idObjAlgo3.run();

        // CONVERTING
        for (j = 0; j < z; j++) {

            try {
                destImage.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer);
                tempImage2.exportData((j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);

                for (i = 0; i < imgBuffer.length; i++) {

                    if (imgBuffer2[i] != 0) {
                        imgBuffer[i] = fat;
                    }
                }

                destImage.importData((j * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmSCUP2");
            }
        }

        System.out.println("4 done");

        // -------------------------BONE CLEANUP----------------------------------//
        // RE-USE THRESHOLD - tempImage1b
        // ID_OBJECTS
        AlgorithmMorphology3D idObjAlgo4 = null;
        idObjAlgo4 = new AlgorithmMorphology3D(tempImage1c, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        idObjAlgo4.setMinMax(5000, 15000);
        idObjAlgo4.run();
        new ViewJFrameImage(destImage);

        // CONVERTING
        for (j = 0; j < z; j++) {

            try {
                destImage.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer);
                tempImage1c.exportData((j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);

                for (i = 0; i < imgBuffer.length; i++) {

                    if (imgBuffer2[i] != 0) {
                        imgBuffer[i] = bone;
                    }
                }

                destImage.importData((j * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmSCUP2");
            }
        }

        System.out.println("bone cleanup done");

        // -------------------------BONE MARROW CLEANUP----------------------------------//
        // THRESHOLD BLACK
        float[] threshold3 = { 95, 105 };
        AlgorithmThresholdDual threshAlgo5 = null;
        tempImage3 = new ModelImage(destImage.getType(), destImage.getExtents(), "TEMP IMAGE",
                                    destImage.getUserInterface());
        threshAlgo5 = new AlgorithmThresholdDual(tempImage3, destImage, threshold3, 1, true, true, true);
        threshAlgo5.run();
        tempImage3a = (ModelImage) tempImage3.clone();

        // tempImage3.setImageName("Thresholded bone");
        // new ViewJFrameImage(tempImage3);
        // MORPHOLOGICAL CLOSE
        AlgorithmMorphology3D idObjAlgo5 = null;
        idObjAlgo5 = new AlgorithmMorphology3D(tempImage3a, AlgorithmMorphology3D.CONNECTED24, 0.0f,
                                               AlgorithmMorphology3D.CLOSE, 1, 1, 0, 1, true);
        idObjAlgo5.run();

        // System.out.println("done with bone double-CLOSE");
        // tempImage3a.setImageName("Thresholded bone, CLOSED");
        // new ViewJFrameImage(tempImage3a);
        tempImage3b = (ModelImage) tempImage3a.clone();

        // MORPHOLOGICAL fill hole
        AlgorithmMorphology3D idObjAlgo6 = null;
        idObjAlgo6 = new AlgorithmMorphology3D(tempImage3b, 4, 2, AlgorithmMorphology3D.FILL_HOLES, 0, 0, 0, 1, true);
        idObjAlgo6.run();

        // System.out.println("done with FILL HOLE");
        // tempImage3b.setImageName("Thresholded bone, closed, FILL_HOLE'd");
        // new ViewJFrameImage(tempImage3b);
        ModelImage tempImage3c = (ModelImage) tempImage3b.clone();

        // morphological ERODE
        AlgorithmMorphology3D idObjAlgo7 = null;
        idObjAlgo7 = new AlgorithmMorphology3D(tempImage3c, AlgorithmMorphology3D.CONNECTED6, 0.0f,
                                               AlgorithmMorphology3D.ERODE, 0, 1, 0, 1, true);
        idObjAlgo7.run();

        // System.out.println("done with bone CLOSE");
        // tempImage3a.setImageName("Thresholded bone, CLOSED, ERODED");
        // new ViewJFrameImage(tempImage3a);
        // using mask to change to bone_marrow
        for (j = 0; j < z; j++) {

            try {
                destImage.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer);
                tempImage3c.exportData((j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);

                for (i = 1; i < imgBuffer.length; i++) {

                    if ((imgBuffer2[i] != 0) && (imgBuffer[i] != bone)) {
                        imgBuffer[i] = bone_marrow;
                    }
                }

                destImage.importData((j * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmSCUP2");
            }
        }

        System.out.println("bone marrow intensity change done");

        // new ViewJFrameImage((ModelImage)destImage.clone());
        // --------------converting rest of what's inside muscle bundle --> to fat----------------
        mask = srcImage.generateVOIMask();
        System.err.println("mask " + mask.length());

        int counter = 0;

        for (j = 0; j < z; j++) {

            try {
                destImage.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer);
                counter = 0;

                for (i = 0; i < imgBuffer.length; i++) {

                    if (mask.get(i) && ((int) imgBuffer[i] == bkgrd)) {

                        // System.out.println("found background in bundle:" + i);
                        counter++;
                        imgBuffer[i] = fat;
                    }
                }

                System.err.println("Counter @z: " + j + ", is " + counter);
                destImage.importData((j * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmSCUP2");
            }
        }

        System.out.println("black to fat inside muscle bundle conversion done");
        destImage.calcMinMax();
        setCompleted(true);
    }
}
