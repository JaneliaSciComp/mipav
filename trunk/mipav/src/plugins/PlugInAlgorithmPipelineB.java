import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import java.io.*;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.model.algorithms.AlgorithmFuzzyCMeans;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;
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


public class PlugInAlgorithmPipelineB extends AlgorithmBase {
    public PlugInAlgorithmPipelineB() {
        try {
            jbInit();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    private ModelImage destImageA = null;
    private ModelImage destImageB = null;
    private ModelImage destImage1 = null;
    private ModelImage destImage3a = null;
    private ModelImage destImage3b = null;
    private ModelImage obMask = null;
    private ModelImage obMaskA = null;
    private ModelImage obMaskB = null;
    private ModelImage voiMask = null;
    private ModelImage fieldImage = null;
    private ModelImage[] HardSeg1 = null;
    private ModelImage[] HardSeg2 = null;
    
    private ModelImage tempImage1 = null;
    private ModelImage tempImage2 = null;
    private ModelImage tempImage3 = null;
    private ModelImage tempImage4 = null;
    private ModelImage tempImage5 = null;
    private ModelImage tempImage6 = null;
    private ModelImage tempImageOne = null;
    private ModelImage tempImageTwo = null;
    private ModelImage tempImageThree = null;
    private ModelImage tempImageFour = null;


    private ModelImage BoneID = null;

    private int[] imgBuffer = null;
    private int[] imgBuffer1 = null;
    private int[] imgBuffer2 = null;
    private int[] imgBuffer3 = null;
    private int[] numObjects = null;
    private int[] boneObject = null;

    private int[] centroidX = null;
    private int[] centroidY = null;
    private float[] distFromCent = null;

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
    //  public static int FAT = 255;
    // public static int MUSCLE = 170;

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

        int xDim, yDim, zDim, sliceSize, aa, bb, cc, dd, ee, n, x, y, xmin, xmax;
        double min, max;
        int i, nClasses, id;
        float realpixelSize = 1;

        xDim = 1;
        yDim = 1;
        zDim = 1;
        

        HardSeg1 = new ModelImage[1];
        FileInfoBase fileInfo1;
        HardSeg2 = new ModelImage[1];
        FileInfoBase fileInfo2;

        for (aa = 1; aa <= 2; aa++) {
            if (aa == 1) {
                xDim = destImageA.getExtents()[0];
                yDim = destImageA.getExtents()[1];
                if (destImageA.getNDims() == 3) {
                    zDim = destImageA.getExtents()[2];
                }
                destImage3a = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3a", destImageA.getUserInterface());
                destImage3b = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3b", destImageA.getUserInterface());
 /*               tempImage1 = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImage1", destImageA.getUserInterface());
  */
                tempImage2 = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImage2", destImageA.getUserInterface());
                tempImage3 = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImage3", destImageA.getUserInterface());
                tempImage5 = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImage5", destImageA.getUserInterface());
                tempImageTwo = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImageTwo",destImageA.getUserInterface());
                tempImageThree = new ModelImage(destImageA.getType(), destImageA.getExtents(), "tempImageTwo",destImageA.getUserInterface());
                fieldImage = new ModelImage(destImageA.getType(), destImageA.getExtents(), "fieldImage", destImageA.getUserInterface());
                BoneID = new ModelImage(destImageA.getType(), destImageA.getExtents(), "BoneID", destImageA.getUserInterface());
				HardSeg1[0] = new ModelImage(ModelStorageBase.UBYTE, destImageA.getExtents(), "Hard-Fuzzy_seg1", destImageA.getUserInterface());
				fileInfo1 = HardSeg1[0].getFileInfo()[0];
				fileInfo1.setResolutions(destImageA.getFileInfo()[0].getResolutions());
				fileInfo1.setUnitsOfMeasure(destImageA.getFileInfo()[0].getUnitsOfMeasure());
				HardSeg1[0].setFileInfo(fileInfo1, 0);
				HardSeg2[0] = new ModelImage(ModelStorageBase.UBYTE, destImageA.getExtents(), "Hard-Fuzzy_seg2", destImageA.getUserInterface());
				fileInfo2 = HardSeg2[0].getFileInfo()[0];
				fileInfo2.setResolutions(destImageA.getFileInfo()[0].getResolutions());
				fileInfo2.setUnitsOfMeasure(destImageA.getFileInfo()[0].getUnitsOfMeasure());
				HardSeg2[0].setFileInfo(fileInfo2, 0);
                destImage1 = (ModelImage) destImageA.clone();
                destImage1.setVOIs(destImageA.getVOIs());
                obMask = (ModelImage) obMaskA.clone();
                voiMask = new ModelImage(destImageA.getType(), destImageA.getExtents(), "voiMask", destImageA.getUserInterface());
            } else if (aa == 2) {
                xDim = destImageB.getExtents()[0];
                yDim = destImageB.getExtents()[1];
                if (destImageB.getNDims() == 3) {
                    zDim = destImageB.getExtents()[2];
                }
                destImage3a = new ModelImage(destImageB.getType(), destImageB.getExtents(), "destImage3a", destImageB.getUserInterface());
                destImage3b = new ModelImage(destImageB.getType(), destImageB.getExtents(), "destImage3b", destImageB.getUserInterface());
 /*               tempImage1 = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImage1", destImageB.getUserInterface());
   */
                tempImage2 = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImage2", destImageB.getUserInterface());
                tempImage3 = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImage3", destImageB.getUserInterface());
                tempImage5 = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImage5", destImageB.getUserInterface());
                tempImageTwo = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImageTwo",destImageB.getUserInterface());
                tempImageThree = new ModelImage(destImageB.getType(), destImageB.getExtents(), "tempImageTwo",destImageB.getUserInterface());
                fieldImage = new ModelImage(destImageB.getType(), destImageB.getExtents(), "fieldImage", destImageB.getUserInterface());
                BoneID = new ModelImage(destImageB.getType(), destImageB.getExtents(), "BoneID", destImageB.getUserInterface());
				HardSeg1[0] = new ModelImage(ModelStorageBase.UBYTE, destImageB.getExtents(), "Hard-Fuzzy" + "_seg2", destImageB.getUserInterface());
				fileInfo1 = HardSeg1[0].getFileInfo()[0];
				fileInfo1.setResolutions(destImageB.getFileInfo()[0].getResolutions());
				fileInfo1.setUnitsOfMeasure(destImageB.getFileInfo()[0].getUnitsOfMeasure());
				HardSeg1[0].setFileInfo(fileInfo1, 0);
				HardSeg2[0] = new ModelImage(ModelStorageBase.UBYTE,destImageB.getExtents(), "Hard-Fuzzy_seg2",destImageB.getUserInterface());
				fileInfo2 = HardSeg2[0].getFileInfo()[0];
				fileInfo2.setResolutions(destImageB.getFileInfo()[0].getResolutions());
				fileInfo2.setUnitsOfMeasure(destImageB.getFileInfo()[0].getUnitsOfMeasure());
				HardSeg2[0].setFileInfo(fileInfo2, 0);
                destImage1 = (ModelImage) destImageB.clone();
                destImage1.setVOIs(destImageB.getVOIs());
                obMask = (ModelImage) obMaskB.clone();
                voiMask = new ModelImage(destImageB.getType(), destImageB.getExtents(), "voiMask", destImageA.getUserInterface());
            }
            
            sliceSize = xDim * yDim;
            imgBuffer = new int[sliceSize];
            imgBuffer1 = new int[sliceSize];
            imgBuffer2 = new int[sliceSize];
            imgBuffer3 = new int[sliceSize];
            numObjects = new int[zDim];
            boneObject= new int[zDim];
            

            progressBar.updateValue(50 * (aa - 1) + 2, activeImage);
            progressBar.setMessage("Converting VOI to Mask");

            
            
            //STEP 1: VOI to Mask 
            
            voiMask = destImage1.generateBinaryImage(false, false);
            progressBar.updateValue(50 * (aa - 1) + 4, activeImage);
            progressBar.setMessage("Taking N3 inside VOI");
            
            
            
            //STEP 2: N3 inside VOI 
            
            AlgorithmIHN3Correction ihn3Algo1 = null;
            ihn3Algo1 = new AlgorithmIHN3Correction(destImage1, fieldImage,destImage1,100f, 150, 0.0001f, 33.3f, 4f, 0.2f, 0.01f, false, false, false);
            ihn3Algo1.setProgressBarVisible(false);
            ihn3Algo1.run();            
            ihn3Algo1.finalize();
            ihn3Algo1 = null;
            fieldImage.disposeLocal();
            fieldImage = null;
            progressBar.updateValue(50 * (aa - 1) + 39, activeImage);
            progressBar.setMessage("Taking Fuzzy C Means over entire image");

            
            
            //STEP 3: FUZZY CMEANS-WHOLE IMAGE

            HardFuzzy(HardSeg1, destImage1, 3);
            progressBar.updateValue(50 * (aa - 1) + 40, activeImage);
            progressBar.setMessage("Taking Fuzzy-c Means inside muscle bundle");
            
            
            
            //STEP 4: FUZZY CMEANS- INSIDE MUSCLE BUNDLE
            
            for (bb = 0; bb < zDim; bb++) {
                try {
                    destImage1.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
                    voiMask.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer1);
                    for (cc = 0; cc < imgBuffer.length; cc++) {
                        if (imgBuffer1[cc] == 0) {
                            imgBuffer[cc] = 0;
                        }
                    }
                    destImage1.importData((bb * imgBuffer.length), imgBuffer, false);
                } catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }
            HardFuzzy(HardSeg2, destImage1, 4);
            progressBar.updateValue(50 * (aa - 1) + 41, activeImage);
            progressBar.setMessage("Labeling Subcutaneous Fat and Background");

            
            
            //STEP 5: clean subcutaneous FAT & clean background 
            
            for (bb = 0; bb < zDim; bb++) {
                try {
                    HardSeg1[0].exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
                    HardSeg2[0].exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer1);
                    obMask.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer2);
                    voiMask.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer3);
                    for (cc = 0; cc < imgBuffer.length; cc++) {
                        //sets everything outside bundle to 0. (in whole fuzzy'd image used to find bone marrow)
                        if (imgBuffer3[cc] == 0) {
                            imgBuffer[cc] = BACKGROUND_NEW;
                        }
                        //clean subcutaneous FAT (on bundle fuzzy'd image used to segment muscle)
                        if (imgBuffer2[cc] == 1 && imgBuffer3[cc] == 0) {
                            imgBuffer1[cc] = SUB_CUT_FAT;
                        }
                        //clean background (on bundle fuzzy'd image used to segment muscle)
                        else if (imgBuffer2[cc] == 0 && imgBuffer3[cc] == 0) {
                            imgBuffer1[cc] = BACKGROUND_NEW;
                        }
                    }
                    destImage3a.importData((bb * imgBuffer.length), imgBuffer, false); //processed whole fuzzied image
                    destImage3b.importData((bb * imgBuffer.length), imgBuffer1, false); //processed bundle fuzzied image
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from destImageA in AlgorithmPipeline2");
                }
            }

            HardSeg1[0].disposeLocal();            HardSeg1[0] = null;
            HardSeg2[0].disposeLocal();            HardSeg2[0] = null;

            progressBar.updateValue(50 * (aa - 1) + 42, activeImage);
            progressBar.setMessage("Labeling Bone");
            ShowImage(destImage3a,destImage3a,"image before ");

            
            
            //STEP 6: threshold background to find bone

            int MinMax[] = {1000, 30000};
            float threshCuts[] = {BACKGROUND - 10, BACKGROUND + 10};

     //       threshold(BoneID, destImage3a, threshCuts);
            
            AlgorithmThresholdDual threshAlgo = null;
            threshAlgo = new AlgorithmThresholdDual(BoneID, destImage3a, threshCuts, 1, true, true, true);
            threshAlgo.setProgressBarVisible(false);
            threshAlgo.run();
            
            ShowImage(tempImage1, BoneID, "thresh'd boneID");
            IDObjects(BoneID, MinMax);
            ShowImage(tempImage2, BoneID, "IDObjected thresh'd boneID");
       //     Open6(BoneID);
         //   IDObjects(BoneID, MinMax);
            Close24(BoneID);
            ShowImage(tempImage3, BoneID, "closed idobjected thresh'd boneID");

                   
            
            
            //NEW STEP STEP6B - operates on a slice per slice basis

                  //    numObjects = (int) BoneID.getMax();

            n = 0;
            for (bb = 0; bb < zDim; bb++) {
            	
            	numObjects[bb] = 0;
            	boneObject[bb] = 1;
                try {
                    BoneID.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
                    //get numObjects per Slice
                    for (cc = 0; cc < imgBuffer.length; cc++) {
                        if (imgBuffer[cc]>numObjects[bb]) {
                            numObjects[bb] = imgBuffer[cc];
                        }
                    }
                    
                    //initialize centroid variables (for particular slice)
                    centroidX = new int[numObjects[bb]];
                    centroidY = new int[numObjects[bb]];
                    distFromCent = new float[numObjects[bb]];                    
                    for (id = 0; id <= numObjects[bb] - 1; id++) {
                        centroidX[id] = 0;
                        centroidY[id] = 0;
                    }
                    
                    //obtain centroid per slice   & obtain distance between center and centroid of each object (per slice)                
                    for (id = 0; id <= numObjects[bb] - 1; id++) {
                        for (x = 0; x < xDim; x++) {
                            for (y = 0; y < yDim; y++) {
                                i = x + y * xDim;
                                if (imgBuffer[i] == id) {
                                    // if (imgBuffer[bb] == id+1) {
								   centroidX[id] = centroidX[id] + x;
								   centroidY[id] = centroidY[id] + y;
								   n++;
                                }
                            }
                        }
                        centroidX[id] = centroidX[id] / n;
                        centroidY[id] = centroidY[id] / n;
                        
                        distFromCent[id] = Math.abs((centroidX[id] - xDim / 2) ^2 + (centroidY[id] - yDim / 2) ^ 2);
                //        System.out.println("distFromCent[" +id+"]: "+ distFromCent[id]);
                //        System.out.println("numObjects:  " + numObjects[bb]);
                    }
//                  using centroids to find correct bone object
                    for (id = 0; id <= numObjects[bb] - 1; id++) {
                        if (id > 0 && id < numObjects[bb] - 1) {
                            if (distFromCent[id] > distFromCent[id + 1]) {
                                   boneObject[bb] = id+1;
                            }
                        }
                    }
//                  eliminating incorrect bone objects
                    for (x = 0; x < xDim; x++) {
                        for (y = 0; y < yDim; y++) {
                            i = x + y * xDim;
                            if (imgBuffer[i] != boneObject[bb]) {
                                imgBuffer[i] = 0;
                            }
                        }
                    }
                    BoneID.importData((cc * imgBuffer.length), imgBuffer, false);
                } catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
                }
            }
            tempImageTwo = (ModelImage) BoneID.clone();
            tempImageTwo.calcMinMax();
            new ViewJFrameImage(tempImageTwo);
            
            
//END STEP 6B NEW STEP



                           //STEP 7: CONVERTING TO BONE
                           
                           for (bb = 0; bb < zDim; bb++) {
                               try {
                                   destImage3a.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
                                   BoneID.exportData((bb * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                                   for (cc = 0; cc < imgBuffer.length; cc++) {
                                       if (imgBuffer2[cc] != 0) {
                                           imgBuffer[cc] = BONE;
                                       }
                                   }
                                   destImage3a.importData((bb * imgBuffer.length), imgBuffer, false);
                               } catch (IOException ex) {
                                   System.err.println( "error exporting bone data from destImage3A in AlgorithmPipeline2");
                               }
                           }
                           progressBar.updateValue(50 * (aa - 1) + 43, activeImage);
                           progressBar.setMessage("Labeling Bone");
                           ShowImage(tempImage4, destImage3a, "image with converted bone, before marrow filled");

                           
                           
                           //STEP 8: BONE MARROW FILLING
                           
                           //since fill hole would not work here as there is no complete contour inside which a hole exists..
                           //create own 'fill hole' algorithm
                           
                           tempImageOne = (ModelImage) BoneID.clone();
                           System.out.println("cloning BoneID");
                                       
                           FillHole(BoneID);
                           ShowImage(tempImage5, BoneID, "image marrow filled");
                           
                           xmax = 0;
                           xmin = 0;
                           ee = 0;
                           dd = 0;
                           
                           if (BoneID == tempImageOne) {
                               for (bb = 0; bb < zDim; bb++) {
                                   try {
                                       destImage3a.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
                                       BoneID.exportData((bb * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                                       for (y = 0; y < yDim; y++) {
                                           for (x = 0; x < xDim; x++) {
                                               i = x + y * xDim;
                                               if (imgBuffer2[i - 1] == 0 &&imgBuffer2[i] == 1) {
                                                   for(ee=0;ee<xDim-x-1;ee++){
                                                       if(imgBuffer2[i+ee-1]==0 && imgBuffer2[i+ee]==1){
                                                       	xmax = ee;
                                                       	for(dd=0;dd<xmax;dd++){
                                                       		if(imgBuffer2[i+dd-1]==1 && imgBuffer2[i+dd]==0){
                                                       			xmin = dd;
                                                       		}
                                                       		else{
                                                       			xmin = 0;
                                                       			xmax = 0;
                                                       			
                                                       		}                                        }
                                                   }
                                                   for(ee=xmin;ee<xmax;ee++){
                                                   	imgBuffer[i+ee]=BONE_MARROW;
                                                   }
                                               }
                                           }
                                       }
                                       destImage3a.importData((bb * imgBuffer.length),
                                                              imgBuffer, false);
                                   }
                                   }
                                       catch (IOException ex) {
                                       System.err.println(
                                               "error exporting bone marrow data from destImageA in AlgorithmPipeline2");
                                   }
                               }
                           }
                           else{
                           	for (bb = 0; bb < zDim; bb++) {
                                   try {
                                       destImage3a.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
                                       tempImageOne.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer1);
                                       BoneID.exportData((bb * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                                       for (y = 0; y < yDim; y++) {
                                           for (x = 0; x < xDim; x++) {
                                               i = x + y * xDim;
                                               if (imgBuffer1[i] == 0 &&imgBuffer2[i] == 1) {
                                               	imgBuffer[i]=BONE_MARROW;
                                                   }
                                               }
                                           }
                                       destImage3a.importData((bb * imgBuffer.length), imgBuffer, false);
                                   } catch (IOException ex) {
                                       System.err.println(
                                               "error exporting bone marrow data from destImageA in AlgorithmPipeline2");
                                   }
                               }
                           }

                           System.out.println("BONE marrow intensity change done");
                           progressBar.updateValue(50 * (aa - 1) + 44, activeImage);
                           progressBar.setMessage("Eliminating noise inside muscle bundle");
                           
                           
                           
            
            //STEP 9: CLEANING BLOTCHY BONE MARROW
            
            threshCuts[0]=BONE_MARROW - 10;            threshCuts[1]=BONE_MARROW + 10; 
   //         threshold(tempImageTwo, destImage3a, threshCuts);
            
            threshAlgo = new AlgorithmThresholdDual(tempImageTwo, destImage3a, threshCuts, 1, true, true, true);
            threshAlgo.setProgressBarVisible(false);
            threshAlgo.run();

            MinMax[0]=1;            MinMax[1]=1000;
            IDObjects(tempImageTwo, MinMax);

            //destImage3a contains labeled bone/bone marrow. CONVERTING noisy bone marrow, to bone.
            for (ee = 0; ee < zDim; ee++) {
                try {
                    destImage3a.exportData((ee * imgBuffer.length), imgBuffer.length, imgBuffer);
                    tempImageTwo.exportData((ee * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                    for (bb = 0; bb < imgBuffer.length; bb++) {
                        if (imgBuffer2[bb] != 0) {
                            imgBuffer[bb] = BONE;
                        }
                    }
                    destImage3a.importData((ee * imgBuffer.length), imgBuffer, false);
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from destImage3A in AlgorithmPipeline2");
                }
            }
            progressBar.updateValue(50 * (aa - 1) + 43, activeImage);
            progressBar.setMessage("Labeling Bone Marrow");
            
 //          ShowImage(tempImage5, destImage3a, "destImage3a before blotchy marrow cleanup");
            
            
            
            //STEP 10: inside bundle CLEANUP 
            //1.CONVERTING INSIDE BLACK NOISE (65 used in 4class destImage3b)into MUSCLE
            //conversion: black noise --> muscle, muscle2 --> muscle.
            threshCuts[0] = BACKGROUND_2 - 10;            threshCuts[1]=BACKGROUND_2 + 10;
            MinMax[0]=0;            MinMax[1]=50;
            
            
   //         threshold(tempImageThree, destImage3b, threshCuts);
            threshAlgo = new AlgorithmThresholdDual(tempImageThree, destImage3b, threshCuts, 1, true, true, true);
            threshAlgo.setProgressBarVisible(false);
            threshAlgo.run();
            
            tempImageFour = (ModelImage) tempImageThree.clone();
            IDObjects(tempImageThree, MinMax);
            
            for (ee = 0; ee < zDim; ee++) {
                try {
                    destImage3b.exportData((ee * imgBuffer.length), imgBuffer.length, imgBuffer); //4 class segmented muscle bundle
                    tempImageThree.exportData((ee * imgBuffer2.length), imgBuffer2.length, imgBuffer2); //thresholded background noise
                    for (bb = 0; bb < imgBuffer.length; bb++) {
                        if (imgBuffer2[bb] != 0 || imgBuffer[bb] == MUSCLE_2) { //wherever there's muscle2 or bkgrd noise
                            imgBuffer[bb] = MUSCLE; //make image background pixel = MUSCLE
                        }
                    }
                    destImage3b.importData((ee * imgBuffer.length), imgBuffer, false);
                } catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP5 black to muscle");
                }
            }
            progressBar.updateValue(50 * (aa - 1) + 45, activeImage);

            tempImageThree.disposeLocal();
            tempImageThree = null;
            
            
 //           ShowImage(tempImage6, destImage3b, "Bone Marrow intensity change DONE");

            //2. CONVERTING REMAINING BLACK INSIDE CONTOURS TO FAT -------------
            //ID_OBJECTS REMAINING LARGER BLACK INSIDE CONTOURS (50-5000)
            //conversion: inner black contours --> fat
            MinMax[0]=50;
            MinMax[1]=5000;
            IDObjects(tempImageFour, MinMax);

            for (ee = 0; ee < zDim; ee++) {
                try {
                    destImage3b.exportData((ee * imgBuffer.length),
                                           imgBuffer.length, imgBuffer);
                    tempImageFour.exportData((ee * imgBuffer2.length),
                                          imgBuffer2.length, imgBuffer2);
                    for (bb = 0; bb < imgBuffer.length; bb++) {
                        if (imgBuffer2[bb] != 0) {
                            imgBuffer[bb] = FAT;
                        }
                    }
                    destImage3b.importData((ee * imgBuffer.length), imgBuffer, false);
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from destImageA in AlgorithmPipeline2-STEP5-blacktofat");
                }
            }
            //3. Converting White Inside Noise into MUSCLE
            //Threshold/IDObjects/Convert
            threshCuts[0] = FAT_2_B - 10;
            threshCuts[1]= FAT_2_B + 10;
    //        threshold(tempImageFour, destImage3b, threshCuts);
            threshAlgo = new AlgorithmThresholdDual(tempImageFour, destImage3b, threshCuts, 1, true, true, true);
            threshAlgo.setProgressBarVisible(false);
            threshAlgo.run();
            
            
            progressBar.updateValue(50 * (aa - 1) + 46, activeImage);
            
            MinMax[0]=0;
            MinMax[1]=50;
            IDObjects(tempImageFour, MinMax);

            for (ee = 0; ee < zDim; ee++) {
                try {
                    destImage3b.exportData((ee * imgBuffer.length), imgBuffer.length, imgBuffer);
                    tempImageFour.exportData((ee * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                    for (bb = 0; bb < imgBuffer.length; bb++) {
                        if (imgBuffer2[bb] != 0) {
                            imgBuffer[bb] = MUSCLE;
                        }
                    }
                    destImage3b.importData((ee * imgBuffer.length), imgBuffer, false);
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from destImageA in AlgorithmPipeline2-STEP5-converting");
                }
            }
            tempImageFour.disposeLocal();
            tempImageFour = null;

            //- STEP 6: CONVERTING REST OF WHAT'S INSIDE MUSCLE BUNDLE TO FAT --
            //INPUT: destImage3b, voiMask, obMask -----------OUTPUT: destImage3b
            for (ee = 0; ee < zDim; ee++) {
                try {
                    destImage3b.exportData((ee * imgBuffer.length), imgBuffer.length, imgBuffer);
                    voiMask.exportData((ee * imgBuffer.length), imgBuffer.length, imgBuffer1);
                    obMask.exportData((ee * imgBuffer.length), imgBuffer.length, imgBuffer2);
                    for (bb = 0; bb < imgBuffer.length; bb++) {
                        if (imgBuffer1[bb] == 1) {
                            if (imgBuffer[bb] == FAT_2_A ||
                                imgBuffer[bb] == FAT_2_B) {
                                imgBuffer[bb] = FAT; //interstitial fat
                            }
                        } else {
                            if (imgBuffer2[bb] == 1) {
                                imgBuffer[bb] = SUB_CUT_FAT; //subcutaneous fat
                            }
                        }
                    }
                    destImage3b.importData((ee * imgBuffer.length), imgBuffer, false);
                } catch (IOException ex) {
                    System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP6");
                }
            }

            voiMask.disposeLocal();
            voiMask = null;

            obMask.disposeLocal();
            obMask = null;

            progressBar.updateValue(50 * (aa - 1) + 48, activeImage);
            progressBar.setMessage("Recreating Result Image");

            //--------------- STEP7: bringing two pieces together --------------
            //INPUT: destImage3a,3b -------------------------OUTPUT: destImage3b
            for (ee = 0; ee < zDim; ee++) {
                try {
                    destImage3a.exportData((ee * imgBuffer.length), imgBuffer.length, imgBuffer);
                    destImage3b.exportData((ee * imgBuffer.length), imgBuffer.length, imgBuffer1);
                    for (bb = 0; bb < imgBuffer.length; bb++) {
                        if (imgBuffer[bb] == BONE ||
                            imgBuffer[bb] == BONE_MARROW) {
                            imgBuffer1[bb] = imgBuffer[bb];
                        }
                    }
                    destImage3b.importData((ee * imgBuffer.length), imgBuffer1, false);
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from destImageA in AlgorithmPipeline2-STEP7");
                }
            }

            destImage3a.disposeLocal();
            destImage3a = null;

            progressBar.updateValue(50 * (aa - 1) + 49, activeImage);
            progressBar.setMessage("Tissue Type Counts/Volumes");

            //--------------------STEP8: final cleanup -------------------------
            //INPUT: destImage3b ----------------------------OUTPUT: destImage3b
            //what's not bone/bone marrow/interstitial fat/subcutaneous fat/background_2, make interstitial fat
            for (ee = 0; ee < zDim; ee++) {
                try {
                    destImage3b.exportData((ee * imgBuffer.length),
                                           imgBuffer.length, imgBuffer1);
                    for (bb = 0; bb < imgBuffer.length; bb++) {
                        if (imgBuffer1[bb] != BONE &&
                            imgBuffer1[bb] != BONE_MARROW &&
                            imgBuffer1[bb] != SUB_CUT_FAT &&
                            imgBuffer1[bb] != BACKGROUND_NEW &&
                            imgBuffer1[bb] != MUSCLE) {
                            imgBuffer1[bb] = FAT;
                        }
                    }
                    destImage3b.importData((ee * imgBuffer.length), imgBuffer1, false);
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from destImageA in AlgorithmPipeline2-STEP8");
                }
            }

            //------------------STEP9: counting tissue types -------------------
            for (ee = 0; ee < zDim; ee++) {
                try {
                    destImage3b.exportData((ee * imgBuffer.length),
                                           imgBuffer.length, imgBuffer);
                    for (bb = 0; bb < imgBuffer.length; bb++) {
                        if (imgBuffer[bb] == SUB_CUT_FAT) {
                            subcutfatCount++;
                            total_thighCount++;
                        } else if (imgBuffer[bb] == FAT) {
                            fatCount++;
                            total_thighCount++;
                        } else if (imgBuffer[bb] == MUSCLE) {
                            muscleCount++;
                            total_thighCount++;
                        } else if (imgBuffer[bb] == BONE) {
                            boneCount++;
                            total_thighCount++;
                        } else if (imgBuffer[bb] == BONE_MARROW) {
                            bone_marrowCount++;
                            total_thighCount++;
                        }
                    }
                    destImage3b.importData((ee * imgBuffer.length),
                                           imgBuffer, false);
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from srcImage in AlgorithmPipeline2");
                }
            }

            float[] res = destImageA.getFileInfo()[0].getResolutions();
            float pixelSize = res[0] * res[1] * res[2];
            if (aa == 1) {
                destImageA = (ModelImage) destImage3b.clone();
                destImageA.calcMinMax();
                realpixelSize = pixelSize;
                UI.getMessageFrame().append("Segmented Images - Results:  " +
                                            PlugInAlgorithmPipeline.patientID,
                                            "Right thigh \n");
            } else {
                destImageB = (ModelImage) destImage3b.clone();
                destImageB.calcMinMax();
                UI.getMessageFrame().append("Segmented Images - Results:  " +
                                            PlugInAlgorithmPipeline.patientID,
                                            "Left thigh \n");
            }

            UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID, "COUNTS: \t subcutaneousFAT= " + subcutfatCount + ", \n \t interstitialFAT= " + fatCount +
                                        ", \n \t MUSCLE= " + muscleCount + ", \n \t BONE= " + boneCount + ", \n \t BONE_MARROW= " + bone_marrowCount +
                                        ", \n \t TOTAL THIGH= " + total_thighCount + "\n \n VOLUME: (cubic Millimeters) \n \t subcutaneousFAT= " +
                                        subcutfatCount * realpixelSize + ", \n \t interstitialFAT= " + fatCount * realpixelSize + ", \n \t MUSCLE= " +
                                        muscleCount * realpixelSize + ", \n \t BONE= " + boneCount * realpixelSize + ",\n \t BONE_MARROW= " +
                                        bone_marrowCount * realpixelSize + ",\n \t TOTAL THIGH= " + total_thighCount * realpixelSize + "\n \n");

            UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID, "INTENSITIES: \n \t subcutaneousFAT= " +
                                        SUB_CUT_FAT + ", \n \t interstitialFAT= " + FAT + ", \n \t MUSCLE= " + MUSCLE + ", \n \t BONE= " + BONE +
                                        ", \n \t BONE_MARROW= " + BONE_MARROW + "\n \n");

            destImage3b.disposeLocal();
            destImage3b = null;

            progressBar.updateValue(50 * (aa - 1) + 50, activeImage);

        }
        System.out.println("thigh cleanup done --destImage");
        setCompleted(true);
        disposeProgressBar();

    }
    
    
    
    
    
    
    
    
    
    
    
    public ModelImage HardFuzzy(ModelImage[] HardSeg, ModelImage srcImage, int nClasses){
        double min = srcImage.getMin();
        double max = srcImage.getMax();

        float centroid_array[] = new float[nClasses];

        for (int bb = 0; bb < nClasses; bb++) {
            centroid_array[bb] = (float) (min +(max - min) * (bb + 1) /(nClasses + 1));
        }
        firstFuzz = null;
        firstFuzz = new AlgorithmFuzzyCMeans(HardSeg, srcImage, nClasses,
                                             4, 1, 2, 2.0f, 20000, 200000, false,
                                             AlgorithmFuzzyCMeans.HARD_ONLY, false,
                                             0.0f, 200, 0.01f, true);
        firstFuzz.setCentroids(centroid_array);
        firstFuzz.setProgressBarVisible(false);
        firstFuzz.run();
        firstFuzz.finalize();
        firstFuzz = null;
        
        return HardSeg[0];
        }
	    
	public ModelImage Open6(ModelImage sourceImg){
		AlgorithmMorphology3D MorphOpen = null;
	    MorphOpen = new AlgorithmMorphology3D(BoneID, AlgorithmMorphology3D.CONNECTED6, 1, AlgorithmMorphology3D.OPEN, 1, 1, 0, 0, true);
	    MorphOpen.setProgressBarVisible(false);
	    MorphOpen.run();
	    return sourceImg;
	}
	
	public ModelImage Close24(ModelImage sourceImg){
		AlgorithmMorphology3D MorphClose = null;
		MorphClose = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED24, 0.0f, AlgorithmMorphology3D.CLOSE, 1, 1, 0, 1, true);
		MorphClose.setProgressBarVisible(false);
		MorphClose.run();
		return sourceImg;
	}
	
	public ModelImage FillHole(ModelImage sourceImg){
		AlgorithmMorphology3D MorphFILL = null;
		MorphFILL = new AlgorithmMorphology3D(sourceImg, 4, 2, AlgorithmMorphology3D.FILL_HOLES, 0, 0, 0, 1, true);
	    MorphFILL.setProgressBarVisible(false);
	    MorphFILL.run();
	    return sourceImg;
	}
		
	public void ShowImage(ModelImage cloneImg, ModelImage sourceImg, String Name){
        cloneImg = (ModelImage) sourceImg.clone();
        cloneImg.calcMinMax();
        cloneImg.setImageName(Name);
        new ViewJFrameImage(cloneImg);
	}
	
    public ModelImage IDObjects(ModelImage sourceImg, int [] MinMax) {
    	AlgorithmMorphology3D MorphIDObj = null;
    	MorphIDObj = new AlgorithmMorphology3D(sourceImg, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
    	MorphIDObj.setMinMax(MinMax[0], MinMax[1]);
    	MorphIDObj.setProgressBarVisible(false);
    	MorphIDObj.run();
    	return sourceImg;
    }

 /*   public ModelImage threshold(ModelImage resultImage, ModelImage threshSourceImg, float [] thresh) {
        AlgorithmThresholdDual threshAlgo = null;
        threshAlgo = new AlgorithmThresholdDual(resultImage, threshSourceImg, thresh, 1, true, true, true);
        threshAlgo.setProgressBarVisible(false);
        threshAlgo.run();
        return resultImage;
    }
*/
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

    private void jbInit() throws Exception {
    }
}
