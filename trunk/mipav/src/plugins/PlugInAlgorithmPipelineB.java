import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import java.io.*;
import java.util.BitSet;

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
    private ModelImage destImage2 = null;
    private ModelImage destImage3a = null;
    private ModelImage destImage3b = null;
    private ModelImage obMask = null;
    private ModelImage obMaskA = null;
    private ModelImage obMaskB = null;
    private ModelImage voiMask = null;
    private ModelImage fieldImage = null;
    private ModelImage HardSeg = null;
    private ModelImage FuzzySeg2[] = null;
    
    
    private ModelImage tempImageOne = null;
    private ModelImage BoneID = null;

    private float[] buffer = null;
    private int[] imgBuffer1 = null;
    private int[] imgBuffer2 = null;
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

    public static int subcutfatCount, fatCount, muscleCount, boneCount, bone_marrowCount, total_thighCount, xDim, yDim, zDim, sliceSize, volSize;

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

        int aa, bb, cc, i;
        float realpixelSize = 1;

        xDim = 1;
        yDim = 1;
        zDim = 1;
      
        for (aa = 1; aa <= 2; aa++) {
            if (aa == 1) {
            	getVariables(destImageA, obMaskA);            	
            } else if (aa == 2) {
            	getVariables(destImageB, obMaskB);
            }
            
            sliceSize = xDim * yDim;
            volSize = xDim * yDim * zDim;
            buffer = new float[volSize];
            imgBuffer1 = new int[sliceSize];
            imgBuffer2 = new int[sliceSize];
            numObjects = new int[zDim];
            boneObject= new int[zDim];
            
            progressBar.updateValue(50 * (aa - 1) + 2, activeImage);
            
            
            
            //STEP 1: VOI to Mask 
            progressBar.setMessage("Converting VOI to Mask");
            voiMask = makeVOI(destImage1);
            progressBar.updateValue(50 * (aa - 1) + 4, activeImage);          	
            
                        
            //STEP 2: N3 inside VOI 
            progressBar.setMessage("Taking N3 inside VOI");
            destImage2 = N3(destImage1);												
            progressBar.updateValue(50 * (aa - 1) + 30, activeImage);			
            
            
            //STEP 3: FUZZY CMEANS-WHOLE IMAGE
            progressBar.setMessage("Taking Fuzzy-C over Entire Image");	
            //convert(destImage1, obMask, destImage2, 0, 0);              ShowImage(destImage1,"wholecrop");
            HardSeg = HardFuzzy(destImage2, 3);            				//ShowImage(HardSeg,"hardseg1");		
            progressBar.updateValue(50 * (aa - 1) + 33, activeImage);     		
            
            
            //STEP 4: FUZZY CMEANS- INSIDE MUSCLE BUNDLE
            progressBar.setMessage("Taking Fuzzy-C inside Muscle Bundle");
            //convert(destImage2, voiMask, destImage2, 0, 0);  //crop
            FuzzySeg2 = SoftFuzzy(destImage2, 3);
            progressBar.updateValue(50 * (aa - 1) + 37, activeImage);      		
            destImage2.disposeLocal();            destImage2 = null;

            for(i=0;i<3;i++){
     //       	ShowImage(FuzzySeg2[i], "FuzzySeg2["+i+"]");
            }
            
            //STEP 4a:  90% multiplying fat fuzzy'd image, combine with remaining
            //destImage3b = processFat(FuzzySeg2[0],FuzzySeg2[1],FuzzySeg2[2],FuzzySeg2[3]);
            destImage3b = processFat(FuzzySeg2[0],FuzzySeg2[1],FuzzySeg2[2]);
  //          ShowImage(destImage3b, "processed fat image");

            
            //---------destImage3a PROCESSING--------------
            //STEP 6: Isolating bone
            progressBar.setMessage("Isolating Bone");
            convert(destImage3a, voiMask, HardSeg, 0, BACKGROUND_NEW);
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
	        cleanUp(destImage3b, FAT, MUSCLE, 300*zDim/20);            
            progressBar.updateValue(50 * (aa - 1) + 46, activeImage);
            
    //        ShowImage(destImage3b, "bundle cleanedup fat image"); 
            
            voiMask.disposeLocal();            voiMask = null;
            obMask.disposeLocal();            obMask = null;

            
            //--------------- STEP10: bringing two pieces together --------------
            progressBar.setMessage("Recreating Result Image");
            mergeImages(destImage3b, destImage3a, destImage3b);           
            
            //-------------- STEP 11: last marrow cleanup --------------------------------
            progressBar.setMessage("Tissue Type Counts/Volumes");
            for (bb = 0; bb < zDim; bb++) {
                try {
                    destImage3b.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
                    for (cc = 0; cc < imgBuffer1.length; cc++) {
                        if (imgBuffer1[cc] == BONE_MARROW){ 
                        	if(imgBuffer1[cc-1]==MUSCLE){
                        		imgBuffer1[cc-1]=BONE_MARROW;
                        	}
                        	else if(imgBuffer1[cc+1]==MUSCLE){
                        		imgBuffer1[cc+1]=BONE_MARROW;
                        	}
                        	else if(imgBuffer1[cc+xDim]==MUSCLE){
                        		imgBuffer1[cc+xDim]=BONE_MARROW;
                        	}
                        	else if(imgBuffer1[cc-xDim]==MUSCLE){
                        		imgBuffer1[cc-xDim]=BONE_MARROW;
                        	}
                        	else if(imgBuffer1[cc-1]==FAT){
                        		imgBuffer1[cc-1]=BONE_MARROW;
                        	}
                        	else if(imgBuffer1[cc+1]==FAT){
                        		imgBuffer1[cc+1]=BONE_MARROW;
                        	}
                        	else if(imgBuffer1[cc+xDim]==FAT){
                        		imgBuffer1[cc+xDim]=BONE_MARROW;
                        	}
                        	else if(imgBuffer1[cc-xDim]==FAT){
                        		imgBuffer1[cc-xDim]=BONE_MARROW;
                        	}
                        }
                    }
                    destImage3b.importData((bb * imgBuffer1.length), imgBuffer1, false);
                } catch (IOException ex) {
                    System.err.println(
                            "error exporting data from destImageA in AlgorithmPipeline2-STEP7");
                }
            }    
            destImage3a.disposeLocal();
            destImage3a = null;

            progressBar.updateValue(50 * (aa - 1) + 49, activeImage);
            

            
            //------------------STEP12: counting tissue types -------------------
            subcutfatCount = 0;
            fatCount = 0;
            muscleCount = 0;
            boneCount = 0;
            bone_marrowCount = 0;
            total_thighCount = 0;
            AVGsubcutfatCount, AVGfatCount, AVGmuscleCount,AVGmuscleCount, AVGbone_marrowCount, AVGtotal_thighCount  = 0;
            for (bb = 0; bb < zDim; bb++) {
                try {
                    destImage3b.exportData((bb * imgBuffer1.length),
                                           imgBuffer1.length, imgBuffer1);
                    for (cc = 0; cc < imgBuffer1.length; cc++) {
                        if (imgBuffer1[cc] == SUB_CUT_FAT) {
                        	
                            subcutfatCount++;
                            total_thighCount++;
                        } else if (imgBuffer1[cc] == FAT) {
                            fatCount++;
                            total_thighCount++;
                        } else if (imgBuffer1[cc] == MUSCLE) {
                            muscleCount++;
                            total_thighCount++;
                        } else if (imgBuffer1[cc] == BONE) {
                            boneCount++;
                            total_thighCount++;
                        } else if (imgBuffer1[cc] == BONE_MARROW) {
                            bone_marrowCount++;
                            total_thighCount++;
                        }
                    }
                    destImage3b.importData((bb * imgBuffer1.length),
                                           imgBuffer1, false);
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
                                            "\n \nRight thigh ");
            } else {
                destImageB = (ModelImage) destImage3b.clone();
                destImageB.calcMinMax();
                UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID, "\n \nLeft thigh ");
            }

            UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID, "Volume: (cubic Millimeters) \n \n" +
            		"CLASS \t \tVOLUME \t \t AVG INTENSITY \n" +
            		"subcutaneousFAT \t" + subcutfatCount * realpixelSize + "\n"+ 
            		"interstitialFAT \t" + fatCount * realpixelSize + "\n" +
            		"MUSCLE \t \t" + muscleCount * realpixelSize + "\n"+
            		"BONE \t \t" + boneCount * realpixelSize + "\n"+
            		"BONE_MARROW \t" + bone_marrowCount * realpixelSize + "\n"+
            		"TOTAL THIGH \t" + total_thighCount * realpixelSize + "\n \n");

            destImage3b.disposeLocal();
            destImage3b = null;

            progressBar.updateValue(50 * (aa - 1) + 50, activeImage);
            
            

        }
        UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID, "INTENSITIES \n " +
        		"subcutaneousFAT \t" + SUB_CUT_FAT + "\n"+ 
        		"interstitialFAT \t" + FAT + "\n"+
        		"MUSCLE \t \t" + MUSCLE + "\n"+
        		"BONE \t \t " + BONE + "\n"+
        		"BONE_MARROW \t" + BONE_MARROW + "\n \n");
        System.out.println("thigh cleanup done --destImage");
        setCompleted(true);
        disposeProgressBar();

    }
    
    
    //-------- BASIC STEPS ---------
    public ModelImage makeVOI(ModelImage destImage1){
    	progressBar.setMessage("Taking N3 inside VOI");
    	return destImage1.generateBinaryImage(false, false);
    }
    
    public ModelImage N3(ModelImage destImage1){
    	ModelImage destImage2 = null;
    	destImage2 = new ModelImage(destImage1.getType(), destImage1.getExtents(),
    			"destImage2", destImage1.getUserInterface());
	    AlgorithmIHN3Correction ihn3Algo1 = null;
	    ihn3Algo1 = new AlgorithmIHN3Correction(destImage2, fieldImage,destImage1,100f, 150, 0.0001f, 33.3f, 4f, 0.2f, 0.01f, false, false, false);
	    ihn3Algo1.setProgressBarVisible(false);
	    ihn3Algo1.run();
	    progressBar.setMessage("Taking Fuzzy-C Means over entire image");
	
	    ihn3Algo1.finalize();            ihn3Algo1 = null;
	    fieldImage.disposeLocal();            fieldImage = null;
	    
	    return destImage2;
    }
    

    public ModelImage[] SoftFuzzy(ModelImage srcImage, int nClasses){
    	float centroid_array[] = new float[nClasses];
    	try {
        	srcImage.exportData(0, volSize, buffer);
        } catch (IOException ex) {
            System.err.println("error exporting data from srcImage in AlgorithmPipeline2");
        }
    	getCentroid(centroid_array, srcImage, nClasses);    	
    	
        ModelImage FuzzySeg[] = new ModelImage[nClasses];
        FileInfoBase fileInfo1;
        int i;
        for(i=0;i<nClasses;i++){
        FuzzySeg[i] = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "Hard-Fuzzy_seg", srcImage.getUserInterface());
		fileInfo1 = FuzzySeg[i].getFileInfo()[i];
		fileInfo1.setResolutions(srcImage.getFileInfo()[i].getResolutions());
		fileInfo1.setUnitsOfMeasure(srcImage.getFileInfo()[i].getUnitsOfMeasure());
		FuzzySeg[i].setFileInfo(fileInfo1, i);
        }

        firstFuzz = null;        
        firstFuzz = new AlgorithmFuzzyCMeans(FuzzySeg, srcImage, nClasses,
                4, 1, 2, 2.0f, 20000, 200000, false,
                AlgorithmFuzzyCMeans.FUZZY_ONLY, false,
                0.0f, 200, 0.01f, true);
        
        firstFuzz.setCentroids(centroid_array);
        firstFuzz.setProgressBarVisible(false);
        firstFuzz.run();
        firstFuzz.finalize();
        firstFuzz = null;

        return FuzzySeg;
        }

    public ModelImage HardFuzzy(ModelImage srcImage, int nClasses){
    	//centroid method
    	float centroid_array[] = new float[nClasses];
    	getCentroid(centroid_array, srcImage, nClasses);
    	
        ModelImage HardSeg[] = new ModelImage[1];
        FileInfoBase fileInfo1;
        HardSeg[0] = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "Hard-Fuzzy_seg", srcImage.getUserInterface());
		fileInfo1 = HardSeg[0].getFileInfo()[0];
		fileInfo1.setResolutions(srcImage.getFileInfo()[0].getResolutions());
		fileInfo1.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
		HardSeg[0].setFileInfo(fileInfo1, 0);

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
    
	public void getCentroid(float[] centroid_array, ModelImage srcImage, int nClasses){
		int dd;
		srcImage.calcMinMax();
		double min = srcImage.getMin();
		double max = srcImage.getMax();
		System.out.println("min:"+min);
		System.out.println("max:"+max);
    	for(dd=1;dd<(nClasses+1);dd++){
    		centroid_array[dd-1]=(float)(dd*max/(nClasses+1));
    		System.out.println("old centroid_array["+dd+"-1] = "+centroid_array[dd-1]);
    	}
    	System.out.println("");
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
        Dilate6(BoneID);
        Close24(BoneID);
        IDObjects(BoneID, zDim*1000/20, zDim*30000/20);        //ShowImage(BoneID, "thresholded IDobjected opened closed idobjected --");

        n = 1;
        for (bb = 0; bb < zDim; bb++) {
        	numObjects[bb] = 0;
        	boneObject[bb] = 1;
            try {
                BoneID.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
                //get numObjects per Slice
                for (cc = 0; cc < imgBuffer1.length; cc++) {
                    if (imgBuffer1[cc]>numObjects[bb]) {
                        numObjects[bb] = imgBuffer1[cc];		//numObjects contains slice maximum.
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
                            if (imgBuffer1[i] == cc) {
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
                        if (imgBuffer1[i] == boneObject[bb]) {
                            imgBuffer1[i] = 1;
                        }
                        else{
                        	imgBuffer1[i]=0;
                        }
                    }
                }
                BoneID.importData((bb * imgBuffer1.length), imgBuffer1, false);
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
	               destImage3a.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
	               BoneID.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer2);
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
	                               	imgBuffer1[i+cc]=BONE_MARROW;
	                               }
	                           }
	                       }
	                       if(imgBuffer2[i]==1 && imgBuffer1[i]!=BONE){
	                    	   imgBuffer1[i]=BONE_MARROW;
	                       }
	                   }
	               }
	               destImage3a.importData((bb * imgBuffer1.length),imgBuffer1, false);
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
    
    
//    public ModelImage processFat(ModelImage fatImage0, ModelImage fatImage1, ModelImage fatImage2, ModelImage fatImage3){
    public ModelImage processFat(ModelImage fatImage0, ModelImage fatImage1, ModelImage fatImage2){
    	ModelImage fatImage = null;
    	fatImage = new ModelImage(fatImage1.getType(), fatImage1.getExtents(),
    			"fatImage", fatImage1.getUserInterface());
    	float imgBuffer[] = null;
    	imgBuffer = new float[sliceSize];
    	int bb, cc;
		int zDim = 0;
    	if (fatImage1.getNDims() == 3) {
            zDim = fatImage1.getExtents()[2];
        }
    	
		for (bb = 0; bb < zDim; bb++) {
		    try {
		    	fatImage2.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
		        for (cc = 0; cc < imgBuffer.length; cc++) {
		        	imgBuffer[cc]=(float)(imgBuffer[cc]*0.7);
		        }
		        fatImage2.importData((bb * imgBuffer.length), imgBuffer, false);
		    } catch (IOException ex) {
		        System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP5 black to muscle");
		    }
		}
		fatImage0 = threshold1(fatImage0, (float)(0.5*fatImage2.getMax()), (float)fatImage0.getMax());
	//	ShowImage(fatImage0, "fatImage0 processed");
		fatImage2 = threshold1(fatImage2, (float)(0.2*fatImage2.getMax()), (float)fatImage2.getMax());
	//	ShowImage(fatImage2, "fatImage2 processed");
		//fatImage3 = threshold1(fatImage3, (float)(0.8*fatImage3.getMax()), (float)fatImage3.getMax());
		//ShowImage(fatImage3, "fatImage3 processed");

		convert(fatImage, voiMask, fatImage, 1, MUSCLE);
//		ShowImage(fatImage, "inside all muscle");
		convert(fatImage, fatImage0, fatImage, 1, FAT);
//		convert(fatImage, fatImage0, fatImage, 1, FAT);
	//	ShowImage(fatImage, "processed1");
		convert(fatImage, fatImage2, fatImage, 1, FAT);
	//	ShowImage(fatImage, "processed2");
//		convert(fatImage, fatImage3, fatImage, 1, FAT);
	//	ShowImage(fatImage, "fat processed fatImage");
		convert(fatImage, voiMask, fatImage, 0, SUB_CUT_FAT);
		convert(fatImage, obMask, fatImage, 0, BACKGROUND_NEW);
	//	ShowImage(fatImage, "fully processed fatImage");

		
		return fatImage;
		
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
		    	srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
		        tempImageOne.exportData((bb * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
		        for (cc = 0; cc < imgBuffer1.length; cc++) {
		            if (imgBuffer2[cc] == tempIntensity) { 
		                imgBuffer1[cc] = newIntensity; 
		            }
		        }
		        destImage.importData((bb * imgBuffer1.length), imgBuffer1, false);
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
	
    public ModelImage threshold(ModelImage threshSourceImg, float intensity) {
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
    
    public ModelImage threshold1(ModelImage threshSourceImg, float intensity1, float intensity2) {
    	float thresh[] = {intensity1, intensity2};
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
	
    public void getVariables(ModelImage destImageA, ModelImage obMaskA){
        xDim = destImageA.getExtents()[0];
        yDim = destImageA.getExtents()[1];
        if (destImageA.getNDims() == 3) {
            zDim = destImageA.getExtents()[2];
        }
        destImage1 = (ModelImage) destImageA.clone();
        destImage1.setVOIs(destImageA.getVOIs());
        obMask = (ModelImage) obMaskA.clone();
        fieldImage = new ModelImage(destImageA.getType(), destImageA.getExtents(), "fieldImage", destImageA.getUserInterface());
		destImage3a = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3a", destImageA.getUserInterface());
        destImage3b = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3b", destImageA.getUserInterface());
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
        imgBuffer1 = null;
        
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
