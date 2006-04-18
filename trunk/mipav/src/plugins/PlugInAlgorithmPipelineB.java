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
    
    private ModelImage tempImageOne = null;
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

        int xDim, yDim, zDim, sliceSize, aa, bb, cc;
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
            //	getVariables(destImageA);
            	
                xDim = destImageA.getExtents()[0];
                yDim = destImageA.getExtents()[1];
                if (destImageA.getNDims() == 3) {
                    zDim = destImageA.getExtents()[2];
                }
                destImage1 = (ModelImage) destImageA.clone();
                destImage1.setVOIs(destImageA.getVOIs());
                obMask = (ModelImage) obMaskA.clone();
                fieldImage = new ModelImage(destImageA.getType(), destImageA.getExtents(), "fieldImage", destImageA.getUserInterface());
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
				
				destImage3a = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3a", destImageA.getUserInterface());
                destImage3b = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3b", destImageA.getUserInterface());
            } else if (aa == 2) {
            	
            	
            	
                xDim = destImageB.getExtents()[0];
                yDim = destImageB.getExtents()[1];
                if (destImageB.getNDims() == 3) {
                    zDim = destImageB.getExtents()[2];
                }
                destImage1 = (ModelImage) destImageB.clone();
                destImage1.setVOIs(destImageB.getVOIs());
                obMask = (ModelImage) obMaskB.clone();
                fieldImage = new ModelImage(destImageB.getType(), destImageB.getExtents(), "fieldImage", destImageB.getUserInterface());
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

                destImage3a = new ModelImage(destImageB.getType(), destImageB.getExtents(), "destImage3a", destImageB.getUserInterface());
                destImage3b = new ModelImage(destImageB.getType(), destImageB.getExtents(), "destImage3b", destImageB.getUserInterface());
            }
            
            sliceSize = xDim * yDim;
            imgBuffer = new int[sliceSize];
            imgBuffer1 = new int[sliceSize];
            imgBuffer2 = new int[sliceSize];
            imgBuffer3 = new int[sliceSize];
            
            numObjects = new int[zDim];
            boneObject= new int[zDim];
            
            progressBar.updateValue(50 * (aa - 1) + 2, activeImage);
            
            
            
            //STEP 1: VOI to Mask 
            progressBar.setMessage("Converting VOI to Mask");
            voiMask = makeVOI(destImage1);
            progressBar.updateValue(50 * (aa - 1) + 4, activeImage);          	
            
                        
            //STEP 2: N3 inside VOI 
            progressBar.setMessage("Taking N3 inside VOI");
            N3(destImage1);
            progressBar.updateValue(50 * (aa - 1) + 30, activeImage);			
            
            
            //STEP 3: FUZZY CMEANS-WHOLE IMAGE
            progressBar.setMessage("Taking Fuzzy-C over Entire Image");	
            HardFuzzy(HardSeg1, destImage1, 3);
            progressBar.updateValue(50 * (aa - 1) + 33, activeImage);     		
            
            
            //STEP 4: FUZZY CMEANS- INSIDE MUSCLE BUNDLE
            progressBar.setMessage("Taking Fuzzy-C inside Muscle Bundle");
            convert(destImage1, voiMask, destImage1, 0, 0);  //crop
            HardFuzzy(HardSeg2, destImage1, 4);
            progressBar.updateValue(50 * (aa - 1) + 37, activeImage);      		
            destImage1.disposeLocal();            destImage1 = null;
            
 
            //STEP 5: Relabeling both segmented images (subcutaneous fat and background) 
            progressBar.setMessage("Labeling Subcutaneous Fat/ Background");
            basicRelabeling(destImage3a, destImage3b, voiMask, obMask, HardSeg1[0], HardSeg2[0]);
            progressBar.updateValue(50 * (aa - 1) + 39, activeImage);
            
            //---------destImage3a PROCESSING--------------
            //STEP 6: Isolating bone
            progressBar.setMessage("Isolating Bone");
            BoneID = isolatingBone(destImage3a, BACKGROUND);            					
            progressBar.updateValue(50 * (aa - 1) + 41, activeImage);
            

            //STEP 7: CONVERTING TO BONE
            progressBar.setMessage("Labeling Bone");
            convert(destImage3a, BoneID, destImage3a, 1, BONE);					
            progressBar.updateValue(50 * (aa - 1) + 43, activeImage);           
                           
                           
	        //STEP 8: BONE MARROW FILLING & CLEANUP
            progressBar.setMessage("Labeling Bone Marrow");
            fillBoneMarrow(destImage3a, BoneID);								
            boneMarrowCleanup(destImage3a);
	        progressBar.updateValue(50 * (aa - 1) + 44, activeImage);
	        
	        BoneID.disposeLocal();	BoneID = null;
           
            
           
	        //---------destImage3b PROCESSING--------------                
            //STEP 9: inside bundle CLEANUP 
	        progressBar.setMessage("Converting Inside Black Noise");
	        cleanUp(destImage3b, FAT, MUSCLE, 200*zDim/20);            
            progressBar.updateValue(50 * (aa - 1) + 46, activeImage);
 
            voiMask.disposeLocal();            voiMask = null;
            obMask.disposeLocal();            obMask = null;

            
            //--------------- STEP10: bringing two pieces together --------------
            progressBar.setMessage("Recreating Result Image");
            mergeImages(destImage3b, destImage3a, destImage3b);            
            
            //-------------- STEP 11: last marrow cleanup --------------------------------
            progressBar.setMessage("Tissue Type Counts/Volumes");
            for (bb = 0; bb < zDim; bb++) {
                try {
                    destImage3b.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer1);
                    for (cc = 0; cc < imgBuffer.length; cc++) {
                        if (imgBuffer[cc] == BONE_MARROW){ 
                        	if(imgBuffer[cc-1]==MUSCLE){
                        		imgBuffer[cc-1]=BONE_MARROW;
                        	}
                        	else if(imgBuffer[cc+1]==MUSCLE){
                        		imgBuffer[cc+1]=BONE_MARROW;
                        	}
                        	else if(imgBuffer[cc+xDim]==MUSCLE){
                        		imgBuffer[cc+xDim]=BONE_MARROW;
                        	}
                        	else if(imgBuffer[cc-xDim]==MUSCLE){
                        		imgBuffer[cc-xDim]=BONE_MARROW;
                        	}
                        	else if(imgBuffer[cc-1]==FAT){
                        		imgBuffer[cc-1]=BONE_MARROW;
                        	}
                        	else if(imgBuffer[cc+1]==FAT){
                        		imgBuffer[cc+1]=BONE_MARROW;
                        	}
                        	else if(imgBuffer[cc+xDim]==FAT){
                        		imgBuffer[cc+xDim]=BONE_MARROW;
                        	}
                        	else if(imgBuffer[cc-xDim]==FAT){
                        		imgBuffer[cc-xDim]=BONE_MARROW;
                        	}
                        }
                    }
                    destImage3b.importData((bb * imgBuffer.length), imgBuffer1, false);
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from destImageA in AlgorithmPipeline2-STEP7");
                }
            }    
            destImage3a.disposeLocal();
            destImage3a = null;

            progressBar.updateValue(50 * (aa - 1) + 49, activeImage);
            

            
            //------------------STEP12: counting tissue types -------------------
            for (bb = 0; bb < zDim; bb++) {
                try {
                    destImage3b.exportData((bb * imgBuffer.length),
                                           imgBuffer.length, imgBuffer);
                    for (cc = 0; cc < imgBuffer.length; cc++) {
                        if (imgBuffer[cc] == SUB_CUT_FAT) {
                            subcutfatCount++;
                            total_thighCount++;
                        } else if (imgBuffer[cc] == FAT) {
                            fatCount++;
                            total_thighCount++;
                        } else if (imgBuffer[cc] == MUSCLE) {
                            muscleCount++;
                            total_thighCount++;
                        } else if (imgBuffer[cc] == BONE) {
                            boneCount++;
                            total_thighCount++;
                        } else if (imgBuffer[cc] == BONE_MARROW) {
                            bone_marrowCount++;
                            total_thighCount++;
                        }
                    }
                    destImage3b.importData((bb * imgBuffer.length),
                                           imgBuffer, false);
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from srcImage in AlgorithmPipeline2");
                }
            }

            float[] res = destImage3b.getFileInfo()[0].getResolutions();
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
    
    
    //-------- BASIC STEPS ---------
    public ModelImage makeVOI(ModelImage destImage1){
    	progressBar.setMessage("Taking N3 inside VOI");
    	return destImage1.generateBinaryImage(false, false);
    }
    
    public void N3(ModelImage destImage1){
	    AlgorithmIHN3Correction ihn3Algo1 = null;
	    ihn3Algo1 = new AlgorithmIHN3Correction(destImage1, fieldImage,destImage1,100f, 150, 0.0001f, 33.3f, 4f, 0.2f, 0.01f, false, false, false);
	    ihn3Algo1.setProgressBarVisible(false);
	    ihn3Algo1.run();
	    progressBar.setMessage("Taking Fuzzy-C Means over entire image");
	
	    ihn3Algo1.finalize();            ihn3Algo1 = null;
	    fieldImage.disposeLocal();            fieldImage = null;
    }
    
    public ModelImage HardFuzzy(ModelImage[] HardSeg, ModelImage srcImage, int nClasses){
    	//centroid method
    	float centroid_array[] = new float[nClasses];
        int i, bb, cc;
        int zDim = 0;
    	if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }
        float percentSoFar = 0;
        float count = 0;
        int totalCount = 0;
    	for (bb = 0; bb < zDim; bb++) {
            try {
            	srcImage.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
                    for (i = 0; i < imgBuffer.length; i++) {
                        totalCount += imgBuffer[i];
                    }
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmPipeline2");
            }
        }
    	cc = 1;
    	for (bb = 0; bb < zDim; bb++) {
    		if(cc<nClasses){
	            try {
	            	srcImage.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
	                    for (i = 0; i < imgBuffer.length; i++) {
	                        count+= imgBuffer[i];
	                        percentSoFar =  count / totalCount;
	                        if ( percentSoFar >= (float)(cc / (nClasses + 1))) {
		                        centroid_array[cc - 1] = (float)(i+(bb*imgBuffer.length));
		                        cc++;
	                        }
	                    }
	            } catch (IOException ex) {
	                System.err.println(
	                        "error exporting data from srcImage in AlgorithmPipeline2");
	            }
    		}
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
    
    
    public void basicRelabeling(ModelImage destImageA,ModelImage destImageB,
    	ModelImage voiMask, ModelImage obMask, ModelImage srcImageA,ModelImage srcImageB){

        convert(destImageA, voiMask, srcImageA, 0, BACKGROUND_NEW);
        
        convert(destImageB, obMask, srcImageB, 0, BACKGROUND_NEW);
        cleanBtwn(destImageB, destImageB, voiMask, obMask, SUB_CUT_FAT);
        
        convert(destImageB, destImageB, destImageB, BACKGROUND_2, FAT);
        convert(destImageB, destImageB, destImageB, FAT_2_A, FAT);
        convert(destImageB, destImageB, destImageB, FAT_2_B, FAT);
        convert(destImageB, destImageB, destImageB, MUSCLE_2, MUSCLE);
                
        srcImageA.disposeLocal(); srcImageA = null;
        srcImageB.disposeLocal(); srcImageB = null;
    }
    

    public void cleanBtwn(ModelImage destImage, ModelImage srcImage, ModelImage voiMask, ModelImage obMask, int intensity){
    	//take difference between obMask and voiMask = btwnMask
    	//whenever btwnMask = 1, convert to 'intensity'
    	int bb, cc;
		int zDim = 0;
		if (srcImage.getNDims() == 3) {
			System.out.println("zDim of srcImage: "+srcImage.getExtents()[2]);
            zDim = srcImage.getExtents()[2];
        }
    	for (bb = 0; bb < zDim; bb++) {
	        try {
	        	srcImage.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer1);
	            obMask.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer2);
	            voiMask.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer3);
	            for (cc = 0; cc < imgBuffer.length; cc++) {
	                //clean subcutaneous FAT (on bundle fuzzy'd image used to segment muscle)
	                if (imgBuffer2[cc] == 1 && imgBuffer3[cc] == 0) {
	                    imgBuffer1[cc] = intensity;
	                }
	            }
	            destImage.importData((bb * imgBuffer.length), imgBuffer1, false); //processed bundle fuzzied image
	        } catch (IOException ex) {
	            System.err.println( "error exporting data from destImageA in AlgorithmPipeline2");
	        }
	    }
	    progressBar.setMessage("Between "+voiMask+" and "+obMask+", labeled "+intensity+
	    		". Input: "+srcImage+". Output:"+destImage);
    }
    
    
    public ModelImage isolatingBone(ModelImage srcImage, int BoneIntensity){
    	int n, bb, cc, x, y, i, xDim, yDim, zDim;
    	xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = 0;
		if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }
        BoneID = threshold(srcImage, BoneIntensity);            //ShowImage(BoneID, "thresholded for bone");
        
        IDObjects(BoneID, zDim*1000/20, zDim*30000/20);
        Open6(BoneID);
        Close24(BoneID);
        IDObjects(BoneID, zDim*1000/20, zDim*30000/20);        //ShowImage(BoneID, "thresholded IDobjected opened closed idobjected --");

        n = 1;
        for (bb = 0; bb < zDim; bb++) {
        	numObjects[bb] = 0;
        	boneObject[bb] = 1;
            try {
                BoneID.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
                //get numObjects per Slice
                for (cc = 0; cc < imgBuffer.length; cc++) {
                    if (imgBuffer[cc]>numObjects[bb]) {
                        numObjects[bb] = imgBuffer[cc];		//numObjects contains slice maximum.
                    }
                }                
                //initialize centroid variables (for particular slice)
                centroidX = new int[numObjects[bb]];
                centroidY = new int[numObjects[bb]];
                distFromCent = new float[numObjects[bb]];                    
                for (cc = 0; cc < numObjects[bb]; cc++) {
                    centroidX[cc] = 0;
                    centroidY[cc] = 0;
                }
                //obtain centroid per slice   & obtain distance between center and centroid of each object (per slice)                
                for (cc = 1; cc <= numObjects[bb]; cc++) {
                    for (x = 0; x < xDim; x++) {
                        for (y = 0; y < yDim; y++) {
                            i = x + y * xDim;
                            if (imgBuffer[i] == cc) {
							   centroidX[cc-1] = centroidX[cc-1] + x;
							   centroidY[cc-1] = centroidY[cc-1] + y;
							   n++;
                            }
                        }
                    }
                    centroidX[cc-1] = centroidX[cc-1] / n;
                    centroidY[cc-1] = centroidY[cc-1] / n;
                    distFromCent[cc-1] = Math.abs((centroidX[cc-1] - xDim / 2) ^2 + (centroidY[cc-1] - yDim / 2) ^ 2);
                }
                //using centroids to find correct bone object. object correspondent to minimum distFromCent
                for (cc = 0; cc < numObjects[bb]; cc++) {
                	if(boneObject[bb]>1){
                        if (distFromCent[cc] < distFromCent[boneObject[bb]]) {
                               boneObject[bb] = cc+1;
                               //System.out.println("boneObject["+bb+"]: "+ boneObject[bb]);
                        }
                	}
                }
                //eliminating incorrect bone objects
                for (x = 0; x < xDim; x++) {
                    for (y = 0; y < yDim; y++) {
                        i = x + y * xDim;
                        if (imgBuffer[i] == boneObject[bb]) {
                            imgBuffer[i] = 1;
                        }
                        else{
                        	imgBuffer[i]=0;
                        }
                    }
                }
                BoneID.importData((bb * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
            }
        }
        return BoneID;
    }  
    
    public void fillBoneMarrow(ModelImage destImage3a, ModelImage BoneID){
    	int bb, cc, dd, i, x, y, xDim, yDim, zDim, xmin, xmax;
    	xDim = destImage3a.getExtents()[0];
        yDim = destImage3a.getExtents()[1];
        zDim = 0;
		if (destImage3a.getNDims() == 3) {
            zDim = destImage3a.getExtents()[2];
        }
	   for (bb = 0; bb < zDim; bb++) {
	           try {
	               destImage3a.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
	               BoneID.exportData((bb * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
	               for (y = 0; y < yDim; y++) {
	        	       xmax = 0;
	        	       xmin = 0;
	                   for (x = 3; x < xDim-3; x++) {
	                       i = x + y * xDim;
	                       if (imgBuffer2[i - 3] == 0 && imgBuffer2[i - 2] == 0 && imgBuffer2[i - 1] == 0 && imgBuffer2[i] == 1 && imgBuffer2[i + 1] == 1 && imgBuffer2[i + 2] == 1) {	//if bkrd to bone boundary
	                           for(cc=0;cc<xDim-x-1;cc++){	//go up til edge of image
	                        	   if(imgBuffer2[i+cc-3]==1 && imgBuffer2[i+cc-2]==1 &&imgBuffer2[i+cc-1]==1 && imgBuffer2[i+cc]==0 && imgBuffer2[i+cc+1]==0 && imgBuffer2[i+cc+2]==0){
	                        		   xmin = cc;
	                        		   xmax = 0;
	                        		   for(dd=0;dd<xDim-x-cc-1;dd++){
	                        			   if(imgBuffer2[i+cc+dd-3]==0 &&imgBuffer2[i+cc+dd-2]==0 &&imgBuffer2[i+cc+dd-1]==0 && imgBuffer2[i+cc+dd]==1
	                        					   && imgBuffer2[i+cc+dd+1]==1&& imgBuffer2[i+cc+dd+2]==1){	//looking for another bkrd to bone boundary
		                                	   xmax = cc+dd;
		                                	   cc=xDim;
		                                	   dd=xDim;
		                                   }
	                        		   }
	                        	   }
	                           }
	                           if(xmin<xmax){
	                               for(cc=xmin;cc<xmax;cc++){
	                               	imgBuffer[i+cc]=BONE_MARROW;
	                               }
	                           }
	                       }
	                       if(imgBuffer2[i]==1 && imgBuffer[i]!=BONE){
	                    	   imgBuffer[i]=BONE_MARROW;
	                       }
	                   }
	               }
	               destImage3a.importData((bb * imgBuffer.length),imgBuffer, false);
	           }
	           catch (IOException ex) {
	           System.err.println("error exporting bone marrow data from destImageA in AlgorithmPipeline2");
	           }
	       }
    }
    
    
    public void boneMarrowCleanup(ModelImage destImage3a){
    	int zDim = 0;
 		if (destImage3a.getNDims() == 3) {
             zDim = destImage3a.getExtents()[2];
         }
        tempImageOne = threshold(destImage3a, BONE_MARROW);
        IDObjects(tempImageOne, 1, zDim*1000/20);
        convert(destImage3a, tempImageOne, destImage3a, 1, BONE);
        tempImageOne.disposeLocal();            tempImageOne = null;
        }
    
    
    public void mergeImages(ModelImage mergedImage, ModelImage boneImage, ModelImage bundleImage){
    	int zDim = 0;
 		if (boneImage.getNDims() == 3) {
             zDim = boneImage.getExtents()[2];
         }
        convert(bundleImage, boneImage, bundleImage, BONE, BONE);
        convert(mergedImage, boneImage, bundleImage, BONE_MARROW, BONE_MARROW);
        cleanUp(mergedImage, BACKGROUND_NEW, SUB_CUT_FAT, 100*zDim/20);
        cleanUp(mergedImage, SUB_CUT_FAT, BACKGROUND_NEW, 100*zDim/20);
        }

    
    
    public void cleanUp(ModelImage srcImage, int noiseIntensity, int bkrdIntensity, int sizeNoise){	
		tempImageOne = threshold(srcImage, noiseIntensity);
		IDObjects(tempImageOne, 0, sizeNoise);
		convert(srcImage, tempImageOne, srcImage, 1, bkrdIntensity);
		tempImageOne.disposeLocal();            tempImageOne = null;
    }    
    
    public void convert(ModelImage destImage, ModelImage tempImageOne, ModelImage srcImage, int tempIntensity, int newIntensity){
    	//whenever tempImageOne = tempIntensity, srcImage converted to newIntensity
    	int bb, cc;
		int zDim = 0;
    	if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }
		for (bb = 0; bb < zDim; bb++) {
		    try {
		    	srcImage.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
		        tempImageOne.exportData((bb * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
		        for (cc = 0; cc < imgBuffer.length; cc++) {
		            if (imgBuffer2[cc] == tempIntensity) { 
		                imgBuffer[cc] = newIntensity; 
		            }
		        }
		        destImage.importData((bb * imgBuffer.length), imgBuffer, false);
		    } catch (IOException ex) {
		        System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP5 black to muscle");
		    }
		}
    }
	
    public void IDObjects(ModelImage sourceImg, int min, int max) {
    	AlgorithmMorphology3D MorphIDObj = null;
    	MorphIDObj = new AlgorithmMorphology3D(sourceImg, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
    	MorphIDObj.setMinMax(min, max);
    	MorphIDObj.setProgressBarVisible(false);
    	MorphIDObj.run();
    }
    
	public void FillHole(ModelImage sourceImg){
		AlgorithmMorphology3D MorphFILL = null;
		MorphFILL = new AlgorithmMorphology3D(sourceImg, 4, 2, AlgorithmMorphology3D.FILL_HOLES, 0, 0, 0, 1, true);
	    MorphFILL.setProgressBarVisible(false);
	    MorphFILL.run();
	}
	
    public ModelImage threshold(ModelImage threshSourceImg, int intensity) {
        float thresh[] = {intensity-5, intensity+5};
    	ModelImage resultImage = null;
    	resultImage = new ModelImage(threshSourceImg.getType(), threshSourceImg.getExtents(),
    			"threshResultImg", threshSourceImg.getUserInterface());
        AlgorithmThresholdDual threshAlgo = null;
        threshAlgo = new AlgorithmThresholdDual(resultImage, threshSourceImg, thresh, 1, true, true, true);
        threshAlgo.setProgressBarVisible(false);
        threshAlgo.run();
        return resultImage;
    }
    
	public void Erode6(ModelImage sourceImg){
		AlgorithmMorphology3D MorphErode = null;
		MorphErode = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1, AlgorithmMorphology3D.ERODE, 0, 1, 0, 0, true);
		MorphErode.setProgressBarVisible(false);
		MorphErode.run();
	}
	
	public void Dilate6(ModelImage sourceImg){
		AlgorithmMorphology3D MorphErode = null;
		MorphErode = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1, AlgorithmMorphology3D.DILATE, 1, 0, 0, 0, true);
		MorphErode.setProgressBarVisible(false);
		MorphErode.run();
	}
    
	public void Open6(ModelImage sourceImg){
		AlgorithmMorphology3D MorphOpen = null;
	    MorphOpen = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1, AlgorithmMorphology3D.OPEN, 1, 1, 0, 0, true);
	    MorphOpen.setProgressBarVisible(false);
	    MorphOpen.run();
	}
	
	public void Close24(ModelImage sourceImg){
		AlgorithmMorphology3D MorphClose = null;
		MorphClose = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED24, 0.0f, AlgorithmMorphology3D.CLOSE, 1, 1, 0, 1, true);
		MorphClose.setProgressBarVisible(false);
		MorphClose.run();
	}
		
	public void ShowImage(ModelImage sourceImg, String Name){
		ModelImage cloneImg = (ModelImage) sourceImg.clone();
        cloneImg.calcMinMax();
        cloneImg.setImageName(Name);
        new ViewJFrameImage(cloneImg);
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
        
        if (obMaskA != null)
        {
        	obMaskA.disposeLocal();
        	obMaskA = null;
        }
        if (obMaskB != null)
        {
        	obMaskB.disposeLocal();
        	obMaskB = null;
        }

    }

    private void jbInit() throws Exception {
    }
}
