import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import java.io.*;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.model.algorithms.AlgorithmFuzzyCMeans;
import gov.nih.mipav.model.algorithms.AlgorithmIHN3Correction;
import gov.nih.mipav.model.file.FileInfoBase;

/**
 * <p>Title: </p>
 * <p>Description: </p>
 * <p>Copyright: Copyright (c) 2004</p>
 * <p>Company: </p>
 * @author not attributable
 * @version 1.0
 *
 * The OAI -- Osteoarthritis Initiative – is a nationwide research study sponsored
 * by the National Institutes of Health, that will help us better understand how
 * to prevent and treat knee osteoarthritis, one of the most common causes of
 * disability in adults.  It is a four-year study and will recruit men and women
 * aged 45 and above at high risk for developing symptomatic knee osteoarthritis.
 * Osteoarthritis causes more health problems and medical expenses than any other
 * form of arthritis. Symptoms of osteoarthritis can range from stiffness and mild
 * pain to severe joint pain and even disability.
 The OAI cohort will be 5000 participants with clinically significant knee OA or at
 high risk for developing incident OA and obtain the appropriate images and
 bio-specimens needed for investigation and validation of OA biomarkers.  The large
 number of images that results from the OAI is a major obstacle to overcome.  Manual
 image segmentation is laborious and subject to inter and intra-observer variability
 when performing volumetric analysis.  Therefore, BIRSS has started a multistage segmentation
 and quantification technique to automatically or semi-automatically process the entire cohort.
 */


public class PlugInAlgorithmPipelineB
    extends AlgorithmBase {

    private ModelImage destImageA = null;
    private ModelImage destImageB = null;
    private ModelImage destImage1 = null;
    private ModelImage destImage2 = null;
    private ModelImage destImage2crop = null;
    private ModelImage destImage3a = null;
    private ModelImage destImage3b = null;
    private ModelImage[] HardSeg1 = null;
    private ModelImage[] HardSeg2 = null;
    private ModelImage tempImage1 = null;
    private ModelImage tempImage2 = null;
    private ModelImage tempImage3 = null;
    private ModelImage tempImage4 = null;
    private ModelImage tempImage5 = null;
    private ModelImage fieldImage = null;
    private ModelImage voiMask = null;
    private ModelImage obMask = null;
    private ModelImage obMaskA = null;
    private ModelImage obMaskB = null;

    private int[] imgBuffer = null;
    private int[] imgBuffer1 = null;
    private int[] imgBuffer2 = null;
    private int[] imgBuffer3 = null;

    //hardSeg1 intensities (3 class segmentation)
    public static int BACKGROUND = 85;
    public static int MUSCLE = 170;
    public static int FAT = 255;

    //hardSeg2 intensities (4 class segmentation)
    public static int BACKGROUND_2 = 63;
    public static int FAT_2_A = 189;
    public static int FAT_2_B = 252;
    public static int MUSCLE_2 = 126;

    //intensity transformations (relabeled intensities)
    public static int BACKGROUND_NEW = 0;
    public static int BONE = 100;
    public static int BONE_MARROW = 200;
    public static int SUB_CUT_FAT = 225;
// public static int FAT = 255;

    public static int subcutfatCount = 0;
    public static int fatCount = 0;
    public static int muscleCount = 0;
    public static int boneCount = 0;
    public static int bone_marrowCount = 0;
    public static int total_thighCount = 0;

    private AlgorithmFuzzyCMeans firstFuzz = null;
    private ViewUserInterface UI = ViewUserInterface.getReference();

    /**
     * Default constructor
     * @param srcImage the source image
     */
    public PlugInAlgorithmPipelineB(ModelImage destImageA, ModelImage destImageB,
                                    ModelImage obMaskA, ModelImage obMaskB) {
        this.obMaskA = obMaskA;
        this.obMaskB = obMaskB;
        this.destImageA = destImageA;
        this.destImageB = destImageB;
    }

    public void runAlgorithm() {

        buildProgressBar("Pipeline_Segmentation", "Operating source images...", 0, 100);
        initProgressBar();

        int ii, i, j, z, nClasses;
        double min, max;
        int xDim, yDim, sliceSize;
        float realpixelSize = 1;

        xDim = 1;
        yDim = 1;
        z = 1;

        for (ii = 1; ii <= 2; ii++) {
            if (ii == 1) {
                xDim = destImageA.getExtents()[0];
                yDim = destImageA.getExtents()[1];
                if (destImageA.getNDims() == 3) {
                    z = destImageA.getExtents()[2];
                }

                destImage2 = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage2",
                                            destImageA.getUserInterface());
                destImage2crop = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage2crop",
                                                destImageA.getUserInterface());
                destImage3a = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3a",
                                             destImageA.getUserInterface());
                destImage3b = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3b",
                                             destImageA.getUserInterface());
                tempImage1 = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImage1",
                                            destImageA.getUserInterface());
                tempImage2 = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImage2",
                                            destImageA.getUserInterface());
                tempImage3 = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImage3",
                                            destImageA.getUserInterface());
                tempImage5 = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImage5",
                                            destImageA.getUserInterface());
                fieldImage = new ModelImage(destImageA.getType(), destImageA.getExtents(), "fieldImage",
                                            destImageA.getUserInterface());
 //               voiMask = new ModelImage(destImageA.getType(), destImageA.getExtents(), "voiMask",
   //                                      destImageA.getUserInterface());

                destImage1 = (ModelImage) destImageA.clone();
                destImage1.setVOIs(destImageA.getVOIs());
                obMask = (ModelImage) obMaskA.clone();
            }
            else if (ii == 2) {
                xDim = destImageB.getExtents()[0];
                yDim = destImageB.getExtents()[1];
                if (destImageB.getNDims() == 3) {
                    z = destImageB.getExtents()[2];
                }

                destImage2 = new ModelImage(destImageB.getType(), destImageB.getExtents(), "destImage2",
                                            destImageB.getUserInterface());
                destImage2crop = new ModelImage(destImageB.getType(), destImageB.getExtents(), "destImage2crop",
                                                destImageB.getUserInterface());
                destImage3a = new ModelImage(destImageB.getType(), destImageB.getExtents(), "destImage3a",
                                             destImageB.getUserInterface());
                destImage3b = new ModelImage(destImageB.getType(), destImageB.getExtents(), "destImage3b",
                                             destImageB.getUserInterface());
                tempImage1 = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImage1",
                                            destImageB.getUserInterface());
                tempImage2 = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImage2",
                                            destImageB.getUserInterface());
                tempImage3 = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImage3",
                                            destImageB.getUserInterface());
                tempImage5 = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImage5",
                                            destImageB.getUserInterface());
                fieldImage = new ModelImage(destImageB.getType(), destImageB.getExtents(), "fieldImage",
                                            destImageB.getUserInterface());
//                voiMask = new ModelImage(destImageB.getType(), destImageB.getExtents(), "voiMask",
  //                                       destImageB.getUserInterface());

                destImage1 = (ModelImage) destImageB.clone();
                destImage1.setVOIs(destImageB.getVOIs());
                obMask = (ModelImage) obMaskB.clone();
            }

            sliceSize = xDim * yDim;
            imgBuffer = new int[sliceSize];
            imgBuffer1 = new int[sliceSize];
            imgBuffer2 = new int[sliceSize];
            imgBuffer3 = new int[sliceSize];

            progressBar.updateValue(50 * (ii - 1) + 2, activeImage);
            progressBar.setMessage("Converting VOI to Mask");

            //--------------------------STEP 1: VOI to Mask --------------------
            voiMask = destImage1.generateBinaryImage(false, false);
//            new ViewJFrameImage(voiMask);
            progressBar.updateValue(50 * (ii - 1) + 4, activeImage);
            progressBar.setMessage("Taking N3 inside VOI");

            //----------------------- STEP 2: N3 inside VOI --------------------
            AlgorithmIHN3Correction ihn3Algo1 = null;
            ihn3Algo1 = new AlgorithmIHN3Correction(destImage2, fieldImage, destImage1,
                100f, 150, 0.0001f, 33.3f, 4f, 0.2f, 0.01f, false, false, false);
            ihn3Algo1.setProgressBarVisible(false);
            //third to last is maskFlag = true, so that VOI not used.

//            destImage2.calcMinMax();
//            destImage2.setImageName("N3'd data");
//            new ViewJFrameImage(destImage2);

            ihn3Algo1.run();
            ihn3Algo1.finalize();
            ihn3Algo1 = null;

          destImage1.disposeLocal();
          destImage1 = null;

            fieldImage.disposeLocal();
            fieldImage = null;

            progressBar.updateValue(50 * (ii - 1) + 39, activeImage);
            progressBar.setMessage("Taking Fuzzy C Means over entire image");

            //----------------------- STEP 3: FUZZY CMEANS----------------------
            //A)-WHOLE IMAGE ------------------------- OUTPUT: HardSegs1[0], [1]

            HardSeg1 = new ModelImage[1];
            FileInfoBase fileInfo1;

            if (ii == 1) {
                HardSeg1[0] = new ModelImage(ModelStorageBase.UBYTE, destImageA.getExtents(),
                                             "Hard-Fuzzy" + "_seg2", destImageA.getUserInterface());
                fileInfo1 = HardSeg1[0].getFileInfo()[0];
                fileInfo1.setResolutions(destImageA.getFileInfo()[0].getResolutions());
                fileInfo1.setUnitsOfMeasure(destImageA.getFileInfo()[0].getUnitsOfMeasure());
                HardSeg1[0].setFileInfo(fileInfo1, 0);
            }
            else if (ii == 2) {
                HardSeg1[0] = new ModelImage(ModelStorageBase.UBYTE, destImageB.getExtents(),
                                             "Hard-Fuzzy" + "_seg2", destImageB.getUserInterface());
                fileInfo1 = HardSeg1[0].getFileInfo()[0];
                fileInfo1.setResolutions(destImageB.getFileInfo()[0].getResolutions());
                fileInfo1.setUnitsOfMeasure(destImageB.getFileInfo()[0].getUnitsOfMeasure());
                HardSeg1[0].setFileInfo(fileInfo1, 0);
            }

            destImage2.calcMinMax();
            min = destImage2.getMin();
            max = destImage2.getMax();
            nClasses = 3;
            float centroid_array[] = new float[nClasses];

            for (i = 0; i < nClasses; i++) {
                centroid_array[i] = (float) (min + (max - min) * (i + 1) / (nClasses + 1));
            }
            firstFuzz = null;
            firstFuzz = new AlgorithmFuzzyCMeans(HardSeg1, destImage2, nClasses, 4,
                                                 1, 2, 2.0f, 20000, 200000, false, AlgorithmFuzzyCMeans.HARD_ONLY,
                                                 false, 0.0f, 200, 0.01f, true);
            firstFuzz.setCentroids(centroid_array);
            firstFuzz.setProgressBarVisible(false);
            firstFuzz.run();
            firstFuzz.finalize();
            firstFuzz = null;

            progressBar.updateValue(50 * (ii - 1) + 40, activeImage);
            progressBar.setMessage("Taking Fuzzy-c Means inside muscle bundle");
//            HardSeg1[0].calcMinMax();
//            new ViewJFrameImage(HardSeg1[0]);

            //----------------------------------------------------------------------
            //B) SEGMENT INSIDE THE MUSCLE BUNDLE----------------OUTPUT: HardSegs2[0], [1]
            HardSeg2 = new ModelImage[1];
            FileInfoBase fileInfo2;

            if (ii == 1) {
                HardSeg2[0] = new ModelImage(ModelStorageBase.UBYTE, destImageA.getExtents(),
                                             "Hard-Fuzzy" + "_seg2", destImageA.getUserInterface());
                fileInfo2 = HardSeg2[0].getFileInfo()[0];
                fileInfo2.setResolutions(destImageA.getFileInfo()[0].getResolutions());
                fileInfo2.setUnitsOfMeasure(destImageA.getFileInfo()[0].getUnitsOfMeasure());
                HardSeg2[0].setFileInfo(fileInfo2, 0);
            }
            else if (ii == 2) {
                HardSeg2[0] = new ModelImage(ModelStorageBase.UBYTE, destImageB.getExtents(),
                                             "Hard-Fuzzy" + "_seg2", destImageB.getUserInterface());
                fileInfo2 = HardSeg2[0].getFileInfo()[0];
                fileInfo2.setResolutions(destImageB.getFileInfo()[0].getResolutions());
                fileInfo2.setUnitsOfMeasure(destImageB.getFileInfo()[0].getUnitsOfMeasure());
                HardSeg2[0].setFileInfo(fileInfo2, 0);
            }

            //crop MUSCLE bundle region to get new min/max for secondFuzzy
            for (j = 0; j < z; j++) {
                try {
                    destImage2.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    voiMask.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer1);
                    for (i = 0; i < imgBuffer.length; i++) {
                        if (imgBuffer1[i] == 0) {
                            imgBuffer[i] = 0;
                        }
                    }
                    destImage2crop.importData( (j * imgBuffer.length), imgBuffer, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }

            destImage2.disposeLocal();
            destImage2 = null;
            destImage2crop.calcMinMax();
            min = destImage2crop.getMin();
            max = destImage2crop.getMax();

            int nClasses2 = 4;
            float centroid_array2[] = new float[nClasses2];
            for (i = 0; i < nClasses2; i++) {
                centroid_array2[i] = (float) (min + (max - min) * (i + 1) / (nClasses2 + 1));
                System.out.println(centroid_array2[i]);
            }

            AlgorithmFuzzyCMeans secondFuzz = null;
            secondFuzz = new AlgorithmFuzzyCMeans(HardSeg2, destImage2crop, nClasses2, 4,
                                                  1, 2, 2.0f, 20000, 200000, false, AlgorithmFuzzyCMeans.HARD_ONLY,
                                                  true, 0.0f, 200, 0.01f, true);
            secondFuzz.setCentroids(centroid_array2);
            secondFuzz.setProgressBarVisible(false);
            secondFuzz.run();
            secondFuzz.finalize();
            secondFuzz = null;

            destImage2crop.disposeLocal();
            destImage2crop = null;

            progressBar.updateValue(50 * (ii - 1) + 41, activeImage);
            progressBar.setMessage("Labeling Subcutaneous Fat and Background");
//            HardSeg2[0].calcMinMax();
//            new ViewJFrameImage(HardSeg2[0]);
//            System.out.println("Fuzzy'd segmented whole and interior bundle Images");



            //--------- STEP 4: clean subcutaneous FAT & clean background ----------
            for (j = 0; j < z; j++) {
                try {
                    HardSeg1[0].exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    HardSeg2[0].exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer1);
                    obMask.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer2);
                    voiMask.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer3);
                    for (i = 0; i < imgBuffer.length; i++) {
                        //bonemarrow (in whole fuzzy'd image) image, sets everything outside bundle to 0
                        if (imgBuffer3[i] == 0) {
                            imgBuffer[i] = 0;
                        }
                        //clean subcutaneous FAT (on bundle fuzzy'd image)
                        if (imgBuffer2[i] == 1 && imgBuffer3[i] == 0) {
                            imgBuffer1[i] = SUB_CUT_FAT;
                        }
                        //clean background (on bundle fuzzy'd image)
                        else if (imgBuffer2[i] == 0 && imgBuffer3[i] == 0) {
                            imgBuffer1[i] = BACKGROUND_NEW;
                        }
                    }
                    destImage3a.importData( (j * imgBuffer.length), imgBuffer, false);
                    destImage3b.importData( (j * imgBuffer.length), imgBuffer1, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }

            HardSeg1[0].disposeLocal();
            HardSeg1[0] = null;
            HardSeg2[0].disposeLocal();
            HardSeg2[0] = null;

            progressBar.updateValue(50 * (ii - 1) + 42, activeImage);
            progressBar.setMessage("Labeling Bone");
//            System.out.println("BONE marrow image, isolated to bundle");

            //-----------CLEANUP WHOLE FUZZY IMAGE FOR BONE SEGMENTATION -------
            //-------------------------BONE CLEANUP-----------------------------//
            //threshold background (some of which is really bone)
            float threshold1[] = {
                BACKGROUND - 10, BACKGROUND + 10}; //threshold all black
            AlgorithmThresholdDual threshAlgo1 = null;
            threshAlgo1 = new AlgorithmThresholdDual(tempImage1, destImage3a, threshold1, 1, true, true, true);
            threshAlgo1.setProgressBarVisible(false);
            threshAlgo1.run();
            threshAlgo1.finalize();
            threshAlgo1 = null;

            //ID_OBJECTS, size ~bone
            AlgorithmMorphology3D idObjAlgo1 = null;
            idObjAlgo1 = new AlgorithmMorphology3D(tempImage1, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
            idObjAlgo1.setMinMax(5000, 10000);
            idObjAlgo1.setProgressBarVisible(false);
            idObjAlgo1.run();
            idObjAlgo1.finalize();
            idObjAlgo1 = null;

            //CONVERTING filtered 'background', to bone.
            for (j = 0; j < z; j++) {
                try {
                    destImage3a.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    tempImage1.exportData( (j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                    for (i = 0; i < imgBuffer.length; i++) {
                        if (imgBuffer2[i] != 0) {
                            imgBuffer[i] = BONE;
                        }
                    }
                    destImage3a.importData( (j * imgBuffer.length), imgBuffer, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImage3A in AlgorithmPipeline2");
                }
            }

            progressBar.updateValue(50 * (ii - 1) + 43, activeImage);
            progressBar.setMessage("Labeling Bone Marrow");
            //           System.out.println("BONE cleanup done");

            //-------------------------BONE MARROW CLEANUP----------------------------------//
            //THRESHOLD BLACK
            float threshold2[] = {
                BONE - 10, BONE + 10}; //where 100 is BONE
            AlgorithmThresholdDual threshAlgo2 = null;
            threshAlgo2 = new AlgorithmThresholdDual(tempImage2, destImage3a, threshold2, 1, true, true, true);
            threshAlgo2.setProgressBarVisible(false);
            threshAlgo2.run();
            threshAlgo2.finalize();
            threshAlgo2 = null;

            //MORPHOLOGICAL CLOSE
            AlgorithmMorphology3D idObjAlgo2a = null;
            idObjAlgo2a = new AlgorithmMorphology3D(tempImage2, AlgorithmMorphology3D.CONNECTED24, 0.0f,
                AlgorithmMorphology3D.CLOSE, 1, 1, 0, 1, true);
            idObjAlgo2a.setProgressBarVisible(false);
            idObjAlgo2a.run();
            idObjAlgo2a.finalize();
            idObjAlgo2a = null;

            //MORPHOLOGICAL fill hole
            AlgorithmMorphology3D idObjAlgo2b = null;
            idObjAlgo2b = new AlgorithmMorphology3D(tempImage2, 4, 2, AlgorithmMorphology3D.FILL_HOLES, 0, 0, 0, 1, true);
            idObjAlgo2b.setProgressBarVisible(false);
            idObjAlgo2b.run();
            idObjAlgo2b.finalize();
            idObjAlgo2b = null;

            //morphological ERODE
            AlgorithmMorphology3D idObjAlgo2c = null;
            idObjAlgo2c = new AlgorithmMorphology3D(tempImage2, AlgorithmMorphology3D.CONNECTED6, 0.0f,
                AlgorithmMorphology3D.ERODE, 0, 1, 0, 1, true);
            idObjAlgo2c.setProgressBarVisible(false);
            idObjAlgo2c.run();
            idObjAlgo2c.finalize();
            idObjAlgo2c = null;

            //using mask to change to BONE_MARROW
            for (j = 0; j < z; j++) {
                try {
                    destImage3a.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    tempImage2.exportData( (j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                    for (i = 1; i < imgBuffer.length; i++) {
                        if (imgBuffer2[i] != 0 && imgBuffer[i] != BONE) {
                            imgBuffer[i] = BONE_MARROW;
                        }
                    }
                    destImage3a.importData( (j * imgBuffer.length), imgBuffer, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }
            System.out.println("BONE marrow intensity change done");
            progressBar.updateValue(50 * (ii - 1) + 44, activeImage);
            progressBar.setMessage("Eliminating noise inside muscle bundle");
//            destImage3a.calcMinMax();
//            new ViewJFrameImage(destImage3a);

            tempImage2.disposeLocal();
            tempImage2 = null;


            //-----------------------------cleaning up blotchy bone marrow----------------------
//threshold background (some of which is really bone)
            float threshold4[] = {
                BONE_MARROW - 10, BONE_MARROW + 10}; //threshold all black

            int[] dimExtents = tempImage1.getExtents();

            AlgorithmThresholdDual threshAlgo4 = null;
            threshAlgo4 = new AlgorithmThresholdDual(tempImage1, destImage3a, threshold4, 1, true, true, true);

            threshAlgo4.setProgressBarVisible(false);
            threshAlgo4.run();
            threshAlgo4.finalize();
            threshAlgo4 = null;

//ID_OBJECTS, size ~bone
            AlgorithmMorphology3D idObjAlgo6 = null;
            idObjAlgo6 = new AlgorithmMorphology3D(tempImage1, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
            idObjAlgo6.setMinMax(1, 1000);
            idObjAlgo6.setProgressBarVisible(false);
            idObjAlgo6.run();
            idObjAlgo6.finalize();
            idObjAlgo6 = null;

//CONVERTING filtered 'background', to bone.
            for (j = 0; j < z; j++) {
                try {
                    destImage3a.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    tempImage1.exportData( (j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                    for (i = 0; i < imgBuffer.length; i++) {
                        if (imgBuffer2[i] != 0) {
                            imgBuffer[i] = BONE;
                        }
                    }
                    destImage3a.importData( (j * imgBuffer.length), imgBuffer, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImage3A in AlgorithmPipeline2");
                }
            }
            tempImage1.disposeLocal();
            tempImage1 = null;
            progressBar.updateValue(50 * (ii - 1) + 43, activeImage);
            progressBar.setMessage("Labeling Bone Marrow");
//           System.out.println("BONE cleanup done");


            //--------------- STEP 5: inside bundle CLEANUP ----------------------- OUTPUT: destImage3b

            //1.CONVERTING INSIDE BLACK NOISE into MUSCLE
            //THRESHOLD BLACK (85-100) ---output: tempImage1
            float threshold3[] = {
                BACKGROUND_2 - 10, BACKGROUND_2 + 10}; //threshold all black
            AlgorithmThresholdDual threshAlgo3 = null;
            threshAlgo3 = new AlgorithmThresholdDual(tempImage3, destImage3b, threshold3, 1, true, true, true);
            threshAlgo3.setProgressBarVisible(false);
            threshAlgo3.run();

            tempImage4 = (ModelImage) tempImage3.clone(); //thresholded background image

            //ID_OBJECTS SMALL NOISE (0-50) ----output: tempImage1a
            AlgorithmMorphology3D idObjAlgo3 = null;
            idObjAlgo3 = new AlgorithmMorphology3D(tempImage3, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
            idObjAlgo3.setMinMax(0, 50);
            idObjAlgo3.setProgressBarVisible(false);
            idObjAlgo3.run();
            idObjAlgo3.finalize();
            idObjAlgo3 = null;

            //CONVERTING BLACK NOISE TO MUSCLE ----output: destImage
            //CONVERTING MUSCLE2 TO MUSCLE
            for (j = 0; j < z; j++) {
                try {
                    destImage3b.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer); //thresholded background
                    tempImage3.exportData( (j * imgBuffer2.length), imgBuffer2.length, imgBuffer2); //thresholded background noise
                    for (i = 0; i < imgBuffer.length; i++) {
                        if (imgBuffer2[i] != 0 || imgBuffer[i] == MUSCLE_2) { //whenever there's noise..
                            imgBuffer[i] = MUSCLE; //make image background pixel = MUSCLE
                        }
                    }
                    destImage3b.importData( (j * imgBuffer.length), imgBuffer, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }

            tempImage3.disposeLocal();
            tempImage3 = null;

//            System.out.println("eliminated small BACKGROUND noise inside MUSCLE bundle");

            progressBar.updateValue(50 * (ii - 1) + 45, activeImage);

            //2. CONVERTING REMAINING BLACK INSIDE CONTOURS TO FAT -------------
            //ID_OBJECTS REMAINING LARGER BLACK INSIDE CONTOURS (50-5000)
            AlgorithmMorphology3D idObjAlgo4 = null;
            idObjAlgo4 = new AlgorithmMorphology3D(tempImage4, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
            idObjAlgo4.setMinMax(50, 5000);
            idObjAlgo4.setProgressBarVisible(false);
            idObjAlgo4.run();
            idObjAlgo4.finalize();
            idObjAlgo4 = null;

            //CONVERTING INSIDE BLACK CONTOURS TO FAT --------------------------
            for (j = 0; j < z; j++) {
                try {
                    destImage3b.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    tempImage4.exportData( (j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                    for (i = 0; i < imgBuffer.length; i++) {
                        if (imgBuffer2[i] != 0) {
                            imgBuffer[i] = FAT;
                        }
                    }
                    destImage3b.importData( (j * imgBuffer.length), imgBuffer, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }

            tempImage4.disposeLocal();
            tempImage4 = null;
//            System.out.println("converted black contours inside MUSCLE bundle to FAT");


            //3. Converting White Inside Noise into MUSCLE
            float threshold5[] = {
                FAT_2_A - 10, FAT_2_B + 10};
            AlgorithmThresholdDual threshAlgo5 = null;
            threshAlgo5 = new AlgorithmThresholdDual(tempImage5, destImage3b, threshold5, 1, true, true, true);
            threshAlgo5.setProgressBarVisible(false);

            threshAlgo5.run();
            threshAlgo5.finalize();
            threshAlgo5 = null;

            progressBar.updateValue(50 * (ii - 1) + 46, activeImage);

            //ID_OBJECTS
            AlgorithmMorphology3D idObjAlgo5 = null;
            idObjAlgo5 = new AlgorithmMorphology3D(tempImage5, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
            idObjAlgo5.setMinMax(0, 50);
            idObjAlgo5.setProgressBarVisible(false);
            idObjAlgo5.run();
            idObjAlgo5.finalize();

            //CONVERTING
            for (j = 0; j < z; j++) {
                try {
                    destImage3b.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    tempImage5.exportData( (j * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                    for (i = 0; i < imgBuffer.length; i++) {
                        if (imgBuffer2[i] != 0) {
                            imgBuffer[i] = MUSCLE;
                        }
                    }
                    destImage3b.importData( (j * imgBuffer.length), imgBuffer, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }
//            System.out.println("cleaned white noise inside bundle to MUSCLE");

            tempImage5.disposeLocal();
            tempImage5 = null;

            //--------- STEP 6: CONVERTING REST OF WHAT'S INSIDE MUSCLE BUNDLE TO FAT ----------
            for (j = 0; j < z; j++) {
                try {
                    destImage3b.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    voiMask.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer1);
                    obMask.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer2);
                    for (i = 0; i < imgBuffer.length; i++) {
                        if (imgBuffer1[i] == 1) {
                            if (imgBuffer[i] == FAT_2_A || imgBuffer[i] == FAT_2_B) {
                                imgBuffer[i] = FAT; //interstitial fat
                            }
                        }
                        else {
                            if (imgBuffer2[i] == 1) {
                                imgBuffer[i] = SUB_CUT_FAT; //subcutaneous fat
                            }
                        }
                    }
                    destImage3b.importData( (j * imgBuffer.length), imgBuffer, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }

            voiMask.disposeLocal();
            voiMask = null;

            obMask.disposeLocal();
            obMask = null;

            progressBar.updateValue(50 * (ii - 1) + 48, activeImage);
            progressBar.setMessage("Recreating Result Image");

            //--------------- STEP7: bringing two pieces together --------------
            for (j = 0; j < z; j++) {
                try {
                    destImage3a.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    destImage3b.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer1);
                    for (i = 0; i < imgBuffer.length; i++) {
                        if (imgBuffer[i] == BONE || imgBuffer[i] == BONE_MARROW) {
                            imgBuffer1[i] = imgBuffer[i];
                        }
                    }
                    destImage3b.importData( (j * imgBuffer.length), imgBuffer1, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }

            destImage3a.disposeLocal();
            destImage3a = null;

            progressBar.updateValue(50 * (ii - 1) + 49, activeImage);
            progressBar.setMessage("Tissue Type Counts/Volumes");

            //------------------STEP8: counting tissue types -------------------
            for (j = 0; j < z; j++) {
                try {
                    destImage3b.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                    for (i = 0; i < imgBuffer.length; i++) {
                        if (imgBuffer[i] == SUB_CUT_FAT) {
                            subcutfatCount++;
                            total_thighCount++;
                        }
                        else if (imgBuffer[i] == FAT) {
                            fatCount++;
                            total_thighCount++;
                        }
                        else if (imgBuffer[i] == MUSCLE) {
                            muscleCount++;
                            total_thighCount++;
                        }
                        else if (imgBuffer[i] == BONE) {
                            boneCount++;
                            total_thighCount++;
                        }
                        else if (imgBuffer[i] == BONE_MARROW) {
                            bone_marrowCount++;
                            total_thighCount++;
                        }
                    }
                    destImage3b.importData( (j * imgBuffer.length), imgBuffer, false);
                }
                catch (IOException ex) {
                    System.err.println("error exporting data from srcImage in AlgorithmPipeline2");
                }
            }
            float[] res = destImageA.getFileInfo()[0].getResolutions();
            float pixelSize = res[0] * res[1] * res[2];
            if (ii == 1) {
                destImageA = (ModelImage) destImage3b.clone();
                destImageA.calcMinMax();
                realpixelSize = pixelSize;
                UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID,
                                            "Right thigh \n");
            }
            else {
                destImageB = (ModelImage) destImage3b.clone();
                destImageB.calcMinMax();
                UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID,
                                            "Left thigh \n");
            }


            UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID,
                                        "COUNTS: \t subcutaneousFAT= " + subcutfatCount + ", \n \t interstitialFAT= " +
                                        fatCount +
                                        ", \n \t MUSCLE= " + muscleCount + ", \n \t BONE= " + boneCount +
                                        ", \n \t BONE_MARROW= " + bone_marrowCount + ", \n \t TOTAL THIGH= " +
                                        total_thighCount +
                                        "\n \n VOLUME: (cubic Millimeters) \n \t subcutaneousFAT= " +
                                        subcutfatCount * realpixelSize +
                                        ", \n \t interstitialFAT= " + fatCount * realpixelSize +
                                        ", \n \t MUSCLE= " + muscleCount * realpixelSize +
                                        ", \n \t BONE= " + boneCount * realpixelSize +
                                        ",\n \t BONE_MARROW= " + bone_marrowCount * realpixelSize +
                                        ",\n \t TOTAL THIGH= " + total_thighCount * realpixelSize +
                                        "\n \n");

            UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID,
                                        "INTENSITIES: \n \t subcutaneousFAT= " + SUB_CUT_FAT +
                                        ", \n \t interstitialFAT= " + FAT + ", \n \t MUSCLE= " + MUSCLE +
                                        ", \n \t BONE= "
                                        + BONE + ", \n \t BONE_MARROW= " + BONE_MARROW + "\n \n");

            AlgorithmHistogram algoGetVol = null;
            algoGetVol = new AlgorithmHistogram(destImage3b, 256);
            algoGetVol.setProgressBarVisible(false);
            algoGetVol.run();


            destImage3b.disposeLocal();
            destImage3b = null;


            progressBar.updateValue(50 * (ii - 1) + 50, activeImage);

        }
        System.out.println("thigh cleanup done --destImage");
        setCompleted(true);
        disposeProgressBar();

    }

    public ModelImage getResultImageA() {
        return this.destImageA;
    }

    public ModelImage getResultImageB() {
        return this.destImageB;
    }

    public void finalize() {
        disposeLocal();
        super.finalize();
        progressBar.dispose();
    }

    public void disposeLocal() {
        imgBuffer = null;

        obMaskA.disposeLocal();
        obMaskA = null;
        obMaskB.disposeLocal();
        obMaskB = null;
    }
}
