import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import java.io.*;
import java.text.NumberFormat;

import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.model.algorithms.AlgorithmFuzzyCMeans;
import gov.nih.mipav.model.algorithms.AlgorithmIHN3Correction;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;

/**
 * algorithm for segmenting two seperate MR thigh images 
 * 
 * @version  1.0, June 22, 2006
 * @author   Agatha Monzon, Paul Hemler, and Matthew J. McAuliffe, Ph.D.

 */

/**
 * ------------------ Static fields/initializers --------------------------
 * <p>The OAI -- Osteoarthritis Initiative – is a nationwide research study sponsored by the National Institutes of Health,
 *  that will help us better understand how to prevent and treat knee osteoarthritis, one of the most common causes of disability 
 *  in adults. It is a four-year study and will recruit men and women aged 45 and above at high risk for developing symptomatic 
 *  knee osteoarthritis. Osteoarthritis causes more health problems and medical expenses than any other form of arthritis. 
 *  Symptoms of osteoarthritis can range from stiffness and mild pain to severe joint pain and even disability. The OAI cohort 
 *  will be 5000 participants with clinically significant knee OA or at high risk for developing incident OA and obtain the 
 *  appropriate images and bio-specimens needed for investigation and validation of OA biomarkers. The large number of images 
 *  that results from the OAI is a major obstacle to overcome. Manual image segmentation is laborious and subject to inter and 
 *  intra-observer variability when performing volumetric analysis. Therefore, BIRSS has started a multistage segmentation and 
 *  quantification technique to automatically or semi-automatically process the entire cohort.</p>
 */
public class PlugInAlgorithmOAISegThighs extends AlgorithmBase {

	/** FINAL SEGMENTATION INTENSITY VALUES*/
	
	/** Interstitial fat*/
    public static int FAT = 255;    

    /** subcutaneous fat */
    public static int SUB_CUT_FAT = 225;
    
    /** Muscle*/
    public static int MUSCLE = 170;

    /** Bone */
    public static int BONE = 100;

    /** Bone marrow */
    public static int BONE_MARROW = 200;

    /** Background */
    public static int BACKGROUND_NEW = 0;


    /** INTERMEDIATE SEGMENTATION INTENSITY VALUES (4 CLASS HARD FUZZY SEGMENTATION)*/
     
    /** interstitial fat intensityA */
    public static int FAT_2_A = 189;

    /** interstitial fat intensityB */
    public static int FAT_2_B = 252;

    /** MUSCLE */
    public static int MUSCLE_2 = 126;

    /** background */
    public static int BACKGROUND_2 = 63;
    

    /** GENERAL IMAGE VARIABLES */
    public static int xDim, yDim, zDim, sliceSize, volSize, aa, bb, cc, i;
  
    /** input leg image (isn'd) */
    private ModelImage processedImage = null;

    /** result image 'right thigh' */
    private ModelImage destImageA = null;

    /** result image 'left thigh' */
    private ModelImage destImageB = null;
    
    /** input AND output 'right' image */
    private ModelImage srcImageA = null;

    /** input AND output 'left' image */
    private ModelImage srcImageB = null;
   
    
    /** DOCUMENT ME! */
    private int[] imgBuffer1 = null;

    /** DOCUMENT ME! */
    private int[] imgBuffer2 = null;

    /** outer boundary mask (obMask). --mask of individual leg boundaries
     * obMask template used in code (for left and right legs) */
    private ModelImage obMask = null;

    /** obMask input into pipelineB (right leg) */
    private ModelImage obMaskA = null;

    /** obMask input into pipelineB (left leg) */
    private ModelImage obMaskB = null;

    /** muscle bundle contour voi mask
     *  voiMask template used in code (for left and right legs)*/
    private ModelImage voiMask = null;
    
    /*use N3 checkbox*/
    private boolean useN3 = false;
    
    /** flag for bone/bonemarrow processing, to indicate whether or not bone is continuous */
    private boolean continuousBone = false; 

    /** DOCUMENT ME! */
    private ViewUserInterface UI = ViewUserInterface.getReference();


    /**
     * ~ Constructors ----------------------------------------------------------------
     * Creates a new PlugInAlgorithmPipelineB object.
     */
    public PlugInAlgorithmOAISegThighs() {
        try {
            jbInit();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Default constructor.
     *
     * @param  srcImageA  --the source image (RIGHT THIGH)
     * @param  srcImageB  --the source image (LEFT THIGH)
     * @param  obMaskA     --the source image thigh outer boundary mask(RIGHT THIGH)
     * @param  obMaskB     --the source image thigh outer boundary mask(LEFT THIGH)
     * 
     * To run algorithm --destImageA, destImageB are the input cropped left/right thighs.
     * obMaskA,obMaskB corresponding outer boundary masks
     */
    public PlugInAlgorithmOAISegThighs(ModelImage srcImageA, ModelImage srcImageB, ModelImage obMaskA,
                                    ModelImage obMaskB, boolean useN3) {
        this.obMaskA = obMaskA;
        this.obMaskB = obMaskB;
        this.srcImageA = srcImageA;
        this.srcImageB = srcImageB;
        this.useN3 = useN3;
    }
   
    
    /** ---------- METHODS ---------- */
    /**
     * GET IMAGE VARIABLES
     * 
     * 1. Initialize image dimensions(xyz).
     * 2. Makes copy of input image to put through processing.
     * 3. Initializes outer boundary mask, and intermediate processing images.
     *
     * @param  srcImage  --source image
     * @param  obMask     --source image thigh outer boundary mask
     */
    public void getVariables(ModelImage srcImage, ModelImage obMask) {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];

        if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }

        processedImage = (ModelImage) srcImage.clone();
        processedImage.setVOIs(srcImage.getVOIs());
        
        this.obMask = (ModelImage) obMask.clone();

    }
        
   /**
    * LABELING BONE
    * Loop check ending once morphpological open/close operations leave a resultant 'bone'
    * that is continuous. Continuity test done by differencing bone marrow before and after 'FillHole' operation.
    *
    * @param  SegmentedImage  --4class hard fuzzy segmented image
    */
    public ModelImage processBoneOrg(ModelImage SegmentedImage) {
	   /** residual image to check difference between image before and after 'FillHole'*/
	   ModelImage residual = new ModelImage(SegmentedImage.getType(), SegmentedImage.getExtents(), "Residual",
	    		SegmentedImage.getUserInterface());
		progressBar.setMessage("Isolating/Labeling Bone");
		continuousBone = false;
		
		/**round1 most rigorous bone isolation (open24,close24)*/
		ModelImage BoneID = threshold1(SegmentedImage, BACKGROUND_2);/**isolate bone intensity --threshold*/

		ModelImage BoneIDtemp = (ModelImage) BoneID.clone();
		Open(BoneIDtemp, 24);
		Close(BoneIDtemp, 24, 1);
    
		IDObjects(BoneIDtemp, zDim*4000/20, zDim*10000/20);  /**isolate bone size*/


		isolatingCenterObject(BoneIDtemp); /**returns binary image of center object*/

		ModelImage BoneIDtemp1 = (ModelImage)BoneIDtemp.clone(); /**BoneIDtemp retains binary image of BONE*/
		FillHole(BoneIDtemp1);

		ImgSubtract(residual,BoneIDtemp1,BoneIDtemp); /**Residual receives what's to be seen as BONE MARROW*/
		Erode(residual, 24); 	/**just to make sure residual is actually of bone marrow size. 
								(too small a residual will be eaten away with erosion)*/
		if(residual.getMax() == 0){
			/**If no hole was filled.. 
			round2 second a little less rigorous bone isolation (open6, close24)*/
			BoneIDtemp.disposeLocal();		BoneIDtemp = null;
			BoneIDtemp1.disposeLocal();		BoneIDtemp1 = null;
			System.out.println("Round1, FillBoneMarrow doesnt work");
			BoneIDtemp = (ModelImage) BoneID.clone();
			Open(BoneIDtemp,6);
			Close(BoneIDtemp,24, 1);
			
			// PFH	6/19/06
			IDObjects(BoneIDtemp, zDim*4000/20, zDim*10000/20);
			
			isolatingCenterObject(BoneIDtemp); 
			BoneIDtemp1 = (ModelImage)BoneIDtemp.clone();
			FillHole(BoneIDtemp1);
			ImgSubtract(residual,BoneIDtemp1,BoneIDtemp);
			Erode(residual, 24);			
			if(residual.getMax() == 0){
				/**round 3, no morphological operations --less rigorous BONE isolation*/
				BoneIDtemp.disposeLocal();		BoneIDtemp=null;
				BoneIDtemp1.disposeLocal();		BoneIDtemp1 = null;
				System.out.println("Round2, FillBoneMarrow doesnt work");
				BoneIDtemp = (ModelImage)BoneID.clone();
				
				// PFH	6/19/06
				IDObjects(BoneIDtemp, zDim*4000/20, zDim*10000/20);
				
				isolatingCenterObject(BoneIDtemp); 
				BoneIDtemp1 = (ModelImage)BoneIDtemp.clone();
				FillHole(BoneIDtemp1);
				ImgSubtract(residual,BoneIDtemp1,BoneIDtemp);
				Erode(residual, 24);
				if(residual.getMax() == 0){
					/**round4, if bone too faint, a simple morphological close to make it continuous --least rigorous bone isolation*/
					System.out.println("Round3, FillBoneMarrow didnt work -using bone with artificially filled marrow");
					BoneIDtemp.disposeLocal();		BoneIDtemp=null;
					BoneIDtemp1.disposeLocal();		BoneIDtemp1 = null;
					BoneIDtemp = (ModelImage)BoneID.clone();
					Close(BoneIDtemp,24, 1);
					
					// PFH	6/19/06
					IDObjects(BoneIDtemp, zDim*4000/20, zDim*10000/20);
					
					isolatingCenterObject(BoneIDtemp);
					BoneIDtemp1 = (ModelImage)BoneIDtemp.clone();
					FillHole(BoneIDtemp1);
					ImgSubtract(residual,BoneIDtemp1,BoneIDtemp);
					Erode(residual, 24);
					if(residual.getMax() == 0){
						/**cannot find continuous bone to fill with marrow. convert bone image to bone intensity.*/
						System.out.println("Round4, FillBoneMarrow unsuccessful. Bone not continuous and therefore no marrow labeled in ProcessBone. " +
								"Bone however IS labeled.");
						convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp, 1, BONE);
						BoneIDtemp.disposeLocal();		BoneIDtemp=null;
					}
					else{
						/**however if bone continuous (filled), convert filling to bonemarrow.
						 *  then on top of bone marrow convert unfilled image to bone.*/
						continuousBone = true;
						System.out.println("Round4, FillBoneMarrow successful");
						convert(BoneIDtemp1, BoneIDtemp1, BoneIDtemp1, 1, BONE_MARROW);
					    convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp1, 1, BONE);
						BoneIDtemp.disposeLocal();		BoneIDtemp=null;
					}
				}
				else{
					continuousBone = true;
					System.out.println("Round3, FillBoneMarrow successful");
					convert(BoneIDtemp1, BoneIDtemp1, BoneIDtemp1, 1, BONE_MARROW);
				    convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp1, 1, BONE);
					BoneIDtemp.disposeLocal();		BoneIDtemp=null;
				}				
			}
			else{
				continuousBone = true;
				System.out.println("Round2, FillBoneMarrow successful");
				convert(BoneIDtemp1, BoneIDtemp1, BoneIDtemp1, 1, BONE_MARROW);
			    convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp1, 1, BONE);
				BoneIDtemp.disposeLocal();		BoneIDtemp=null;
			}
		}
		else{
			continuousBone = true;
			System.out.println("Round1, FillBoneMarrow successful");
			convert(BoneIDtemp1, BoneIDtemp1, BoneIDtemp1, 1, BONE_MARROW);
		    convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp1, 1, BONE);
			BoneIDtemp.disposeLocal();		BoneIDtemp=null;
		}		
	    progressBar.updateValue(50 * (aa - 1) + 43, runningInSeparateThread);
	    residual.disposeLocal();	    residual=null;
	    BoneID.disposeLocal();			BoneID=null;
	    return BoneIDtemp1;   
	    
   }
   
    /**
     * LABELING BONE
     * Loop check ending once morphpological open/close operations leave a resultant 'bone'
     * that is continuous. Continuity test done by differencing bone marrow before and after 'FillHole' operation.
     *
     * @param  SegmentedImage  --4class hard fuzzy segmented image
     */
     public ModelImage processBoneOrg2(ModelImage SegmentedImage) {
        progressBar.setMessage("Isolating/Labeling Bone");
        continuousBone = false;
         
//       PFH        ShowImage(SegmentedImage, "processBone input image");

        /**round1 most rigorous bone isolation (open24,close24)*/
        ModelImage BoneID = threshold1(SegmentedImage, BACKGROUND_2);/**isolate bone intensity --threshold*/
//       PFH        ShowImage(BoneID, "BACKGROUND_2 Thresholded image");

        ModelImage BoneIDtemp = (ModelImage) BoneID.clone();        
//       PFH        ShowImage(BoneIDtemp, "thresholded image");

        IDObjects(BoneID, zDim*250, zDim*550);  /**isolate bone size*/
//        Close(BoneID, 24, 2);
        Dilate(BoneID, 6);
//       PFH        System.err.println("SIZE min: " + zDim*4000/20 + "  max" + zDim*10000/20);

        //isolatingCenterObject(BoneIDtemp); /**returns binary image of center object*/
//       PFH        ShowImage(BoneID, "Dilate bone");

        ModelImage BoneIDtemp1 = (ModelImage)BoneID.clone(); /**BoneIDtemp retains binary image of BONE*/
        FillHole(BoneIDtemp1);
//      PFH        ShowImage(BoneIDtemp1, "hole filled");
        Erode(BoneIDtemp1, 6);
        Erode(BoneID, 6);
//       PFH        ShowImage(BoneIDtemp1, "erode");

        int cnt = 0;
        for (int i = 0; i < xDim*yDim*zDim; i++) {
        	if (BoneIDtemp1.getInt(i) >= 1)
        		cnt++;
        }
        if (cnt < 0.5f * zDim*xDim*yDim) {
        	continuousBone = false;
            System.out.println("Round1, FillBoneMarrow successful");
            
        }
        convert(BoneIDtemp1, BoneIDtemp1, BoneIDtemp1, 1, BONE_MARROW);
        convert(BoneIDtemp1, BoneID, BoneIDtemp1, 1, BONE);
        //ShowImage(BoneIDtemp1, "bone and marrow");

        progressBar.updateValue(50 * (aa - 1) + 43, runningInSeparateThread);
        BoneID.disposeLocal();          
        BoneID=null;
        return BoneIDtemp1;   
   
    }
     
     
     
     public ModelImage extractBone(ModelImage boneMarrow, ModelImage inputThigh) {
    	 float mrBoneThreshold = 0;
    	 int cnt = 0;
  	   progressBar.setMessage("Extracting bone");

  	   ModelImage bone = new ModelImage(boneMarrow.getType(), boneMarrow.getExtents(), "boneImg", null);
  	   
  	   int xDim = inputThigh.getExtents()[0];
  	   int yDim = inputThigh.getExtents()[1];
  	   short[] boneBuffer = new short[xDim * yDim];
     	 
  	   for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
  		   try {
  			   boneMarrow.exportData((sliceNum * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
  			   inputThigh.exportData((sliceNum * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
             } catch (IOException ex) {
                 System.err.println("Error exporting data");
             }
             
             // estimate a mrBoneThreshold value for pixels next to the marrow
             // let the value be the average of the adjacent pixels
             // assign all pixels next to the marrow as bone
             // relabel all pixels next to the marrow so the image is not modified as it is processed 
             mrBoneThreshold = 0;
             cnt = 0;
        	 for (int idx = xDim; idx < boneBuffer.length - xDim; idx++) {
        		 if (imgBuffer1[idx] == 0 &&
        				 (imgBuffer1[idx+1] == 1 || imgBuffer1[idx-1] == 1 ||
        				  imgBuffer1[idx+xDim] == 1 || imgBuffer1[idx-xDim] == 1)) {
        			 
        			 mrBoneThreshold += imgBuffer2[idx];
        			 boneBuffer[idx] = 1;
    				 imgBuffer1[idx] = 2;
    				 cnt++;
        			 
        		 } // end if (....)
        	 } // end for (int idx = 0; ...)

        	 // find average mr value for pixels next to the marrow
        	 mrBoneThreshold /= cnt;
       	 
        	 // increase the marrow mask to include the pixels added as bone
        	 for (int i = 0; i < imgBuffer1.length; i++) {
        		 if (imgBuffer1[i] == 2) {
        			 imgBuffer1[i] = 1;
        		 }
        	 }

             // loop through each slice at most 8 times adding neighboring bone pixels
        	 // that are less than the mrBoneThreshold value
             for (int iter = 0; iter < 8; iter++) {
            	 for (int idx = xDim; idx < boneBuffer.length - xDim; idx++) {
            		 if (imgBuffer1[idx] == 0 &&
            				 (imgBuffer1[idx+1] == 1 || imgBuffer1[idx-1] == 1 ||
            				  imgBuffer1[idx+xDim] == 1 || imgBuffer1[idx-xDim] == 1)) {
            			 
            			 if (imgBuffer2[idx] < mrBoneThreshold) {
            				 boneBuffer[idx] = 1;
            				 imgBuffer1[idx] = 2;
            			 } // end if (...)
            			 
            		 } // end if (....)
            	 } // end for (int idx = 0; ...)
            	 
            	 for (int i = 0; i < boneBuffer.length; i++) {
            		 if (imgBuffer1[i] == 2) {
            			 imgBuffer1[i] = 1;
            		 }
            	 }
            	 
            	 // decrease the threhsold value as you get further from the marrow
            	 mrBoneThreshold *= 0.95f;
             } // end for (int iter = 0; ...)
             
             try {
            	 bone.importData((sliceNum * boneBuffer.length), boneBuffer, false);
             } catch (IOException ex) {
            	 System.err.println("error importing data");
             }

             // clean out boneBuffer for the next slice
             for (int idx = 0; idx < boneBuffer.length; idx++) {
            	 boneBuffer[idx] = 0;
             }
         } // end for (int sliceNum = 0; ...)
  
  	   bone.calcMinMax();
 	   Close(bone, 6, 2);
       return bone;
                
     } // end extractBone(...)
     
     
     /**
      * method extracts bone marrow and surrounding bone from a C-Means segmented image
      * 
      * @param destImage image to be filled with bone and marrow segmented result 
      * @param CMeansSeg CMeans segmented image of a single thigh image
      */
     public void processBoneAndMarrow(ModelImage destImage, ModelImage CMeansSeg, ModelImage thighInputImage) {
    	 progressBar.setMessage("Processing Bone and Marrow");
    	 
 //  PFH    	 ShowImage(CMeansSeg, "Segmented Image");
    	 
    	 progressBar.setMessage("Locating Bone Marrow");
    	 
    	 ModelImage boneMarrow = extractedBoneMarrow(CMeansSeg);
    	 // boneMarrow is a binary image containing only the bone marrow

//  PFH         ShowImage(boneMarrow, "bone/morrow image");
    	 
    	 // use the bone marrow and input thigh image to segment out the bone
    	 ModelImage bone = extractBone(boneMarrow, thighInputImage);

    	 // put the boneMarrow segmented image into the destImage
    	 // label value for BONE_MARROW is 200
         convert(destImage, boneMarrow, destImage, 1, BONE_MARROW);       
    	 boneMarrow.disposeLocal();
    	 boneMarrow = null;

         convert(destImage, bone, destImage, 1, BONE);       
    	 bone.disposeLocal();
    	 bone = null;

//       PFH    	 ShowImage(destImage, "bone/morrow image");
     } // end processBoneAndMarrow(...)
     
     
    
   /**
    * LABELING BONE MARROW (1. artificially, 2.filling gaps between bone)
    * case1: NO BONE. no bone marrow. (we create artificial bone)
    * case2: DISCONTINUOUS BONE. no bone marrow. (we fill in the gaps between discontinuous bone and marrow.
    * case3: CONTINUOUS BONE. bone marrow to fill. and NOISY BONE MARROW. also NOISY BONE. (processed in the end, not this method)
    * case4: CONTINUOUS BONE. CONTINUOUS BONE MARROW. (not processed at all)
    * 
    * *ALGORITHM USED ONLY IF BONE NOT CONTINUOUS* 
    *
    * @param  srcImage  --image with labeled bone
    */
    public void processBoneMarrowOrg2(ModelImage srcImage, ModelImage SegmentedImg){
	   progressBar.setMessage("Processing Bone Marrow (for discontinuous bone images)");

       ShowImage(SegmentedImg, "processed Bone Image segmented image");
	   /**First, check case 1 or case 2--is there labeled bone?*/	   
	   
       progressBar.setMessage("Is there bone at all?");
	   int bb, cc, dd, i;
	   boolean thereisBone = false;
       for (bb = 0; bb < zDim; bb++) {
           try {
               srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
               while(thereisBone == false){
	               for (cc = 0; cc < imgBuffer1.length; cc++) {	            	   
		               	if(imgBuffer1[cc] == BONE){
		               		thereisBone = true;
                            bb = zDim;
		               	}
	               }
               }
           } catch (IOException ex) {
               System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP7");
           }
       }
              
       
       
       progressBar.setMessage("Locating Bone Marrow");
       /**Second, using srcImage (HardSeg1 4class hard fuzzy segmentation),  
	   isolate pixels of intensities 160-255 to locate 'bone marrow', and label.*/
	   ModelImage BMarrow = extractedBoneMarrow(SegmentedImg); /**BMarrow should be binary image (extracted from fuzzyImg)*/
       convert(srcImage, BMarrow, srcImage, 1, BONE_MARROW);       
       BMarrow.disposeLocal();
       BMarrow=null;

       ShowImage(srcImage, "morrow processed.");
       /***LOOP FOR WHEN THERE IS BONE.*
	   Fill gaps between discontinuous bone and 'bone marrow'*/
	   if(thereisBone ==true){
		   progressBar.setMessage("Growing out Bone Marrow to Bone exterior");
		        for (bb = 0; bb < zDim; bb++) {
		            try {
		                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
		                for (cc = 0; cc < imgBuffer1.length-10*xDim; cc++) {
		                	/**gap between 'bone to marrow', fill with bonemarrow --L to R*/
		                	if(imgBuffer1[cc]==BONE && imgBuffer1[cc+1]!=BONE){
		                		for(dd=10;dd>0;dd--){
			                    	if(imgBuffer1[cc+dd]==BONE_MARROW && imgBuffer1[cc+dd-1]!=BONE_MARROW){
			                    		for(i=1;i<dd;i++){
			                    			imgBuffer1[cc+i]=BONE_MARROW;
			                    		}
			                    	}
		                		}
		                	}
		                	/** -- bone 1 row below marrow*/
		                	if(imgBuffer1[cc]==BONE && imgBuffer1[cc+xDim]!=BONE){
		                		for(dd=10;dd>0;dd--){
			                    	if(imgBuffer1[cc+dd*xDim]==BONE_MARROW && imgBuffer1[cc+(dd-1)*xDim]!=BONE_MARROW){
			                    		for(i=1;i<dd;i++){
			                    			imgBuffer1[cc+i*xDim]=BONE_MARROW;
			                    		}
			                    	}
		                		}
		                	}
		                }
		                srcImage.importData((bb * imgBuffer1.length), imgBuffer1, false);
		            } catch (IOException ex) {
		                System.err.println(
		                        "error exporting data from destImageA in AlgorithmPipeline2-STEP7");
		            }
		        }
		        //ShowImage(srcImage, "after first bone loop");
		        
		        for (bb = 0; bb < zDim; bb++) {
		            try {
		                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
		                for (cc = 10*xDim; cc < imgBuffer1.length; cc++) {
		                	/** gap between 'marrow to bone', fill with bonemarrow --L to R*/
		                	if(imgBuffer1[cc]==BONE && imgBuffer1[cc-1]!=BONE){
		                		for(dd=10;dd>0;dd--){
			                    	if(imgBuffer1[cc-dd]==BONE_MARROW && imgBuffer1[cc-dd+1]!=BONE_MARROW){
			                    		for(i=1;i<dd;i++){
			                    			imgBuffer1[cc-i]=BONE_MARROW;
			                    		}
			                    	}
		                		}
		                	}
		                	/**-- bone 1 row above marrow*/
		                	if(imgBuffer1[cc]==BONE && imgBuffer1[cc-xDim]!=BONE){
		                		for(dd=10;dd>0;dd--){
			                    	if(imgBuffer1[cc-dd*xDim]==BONE_MARROW && imgBuffer1[cc-(dd-1)*xDim]!=BONE_MARROW){
			                    		for(i=1;i<dd;i++){
			                    			imgBuffer1[cc-i*xDim]=BONE_MARROW;
			                    		}
			                    	}
		                		}
		                	}
		                }
		                srcImage.importData((bb * imgBuffer1.length), imgBuffer1, false);
		            } catch (IOException ex) {
		                System.err.println(
		                        "error exporting data from destImageA in AlgorithmPipeline2-STEP7");
		            }
		        }
		        //ShowImage(srcImage, "after second bone loop");
	   	}
	   
	   /**if there is no bone, create artificial bone around bone marrow extracted from 4class fuzzy*/
       
       /*** Needs UPGRADING - better fill!!!!! */
	   	if(thereisBone==false){
	   		progressBar.setMessage("There is no bone. Creating artificial bone around marrow.");
        	System.out.println("There is no bone");
        	dd=0;
	        for (bb = 0; bb < zDim; bb++) {
	            try {
	                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
	                	for (cc = (int)(0.1*yDim*xDim); cc < (int)(0.9*yDim*xDim); cc++) {
	                		if(imgBuffer1[cc]==BONE_MARROW){
                				if(imgBuffer1[cc-1]!=BONE_MARROW && imgBuffer1[cc-2]!=BONE_MARROW &&imgBuffer1[cc-3]!=BONE_MARROW &&
                						imgBuffer1[cc-4]!=BONE_MARROW &&imgBuffer1[cc-5]!=BONE_MARROW){
                					imgBuffer1[cc-1]=BONE;
                					imgBuffer1[cc-2]=BONE;
                					imgBuffer1[cc-3]=BONE;
            						imgBuffer1[cc-4]=BONE;
            						imgBuffer1[cc-5]=BONE;
                				}
                				if(imgBuffer1[cc+1]!=BONE_MARROW && imgBuffer1[cc+2]!=BONE_MARROW &&imgBuffer1[cc+3]!=BONE_MARROW &&
                						imgBuffer1[cc+4]!=BONE_MARROW &&imgBuffer1[cc+5]!=BONE_MARROW){
                					imgBuffer1[cc+1]=BONE;
                					imgBuffer1[cc+2]=BONE;
                					imgBuffer1[cc+3]=BONE;
            						imgBuffer1[cc+4]=BONE;
            						imgBuffer1[cc+5]=BONE;
                				}
                				if(imgBuffer1[cc-1*xDim]!=BONE_MARROW && imgBuffer1[cc-2*xDim]!=BONE_MARROW &&imgBuffer1[cc-3*xDim]!=BONE_MARROW &&
                						imgBuffer1[cc-4*xDim]!=BONE_MARROW &&imgBuffer1[cc-5*xDim]!=BONE_MARROW){
                					imgBuffer1[cc-1*xDim]=BONE;
                					imgBuffer1[cc-2*xDim]=BONE;
                					imgBuffer1[cc-3*xDim]=BONE;
            						imgBuffer1[cc-4*xDim]=BONE;
            						imgBuffer1[cc-5*xDim]=BONE;
                				}
                				if(imgBuffer1[cc+1*xDim]!=BONE_MARROW && imgBuffer1[cc+2*xDim]!=BONE_MARROW &&imgBuffer1[cc+3*xDim]!=BONE_MARROW &&
                						imgBuffer1[cc+4*xDim]!=BONE_MARROW &&imgBuffer1[cc+5*xDim]!=BONE_MARROW){
                					imgBuffer1[cc+1*xDim]=BONE;
                					imgBuffer1[cc+2*xDim]=BONE;
                					imgBuffer1[cc+3*xDim]=BONE;
            						imgBuffer1[cc+4*xDim]=BONE;
            						imgBuffer1[cc+5*xDim]=BONE;
                				}
	                		}
	                	}
	                srcImage.importData((bb * imgBuffer1.length), imgBuffer1, false);
	            } catch (IOException ex) {
	                System.err.println(
	                        "error exporting data from destImageA in AlgorithmPipeline2-STEP7");
	            }
	        }
	        //ShowImage(srcImage,"image after no BONE loop (loop3)");
	   	}
   }
   
  /**
   	* LABELING INTERSTITIAL FAT, SUBCUTANEOUS FAT, BACKGROUND AND MUSCLE
  *  1.convert everything inside voi to muscle
  *  2.convert fat1 (from 4class hard fuzzy segmentation)to fat
  *  3.convert fat2 (from 4class hard fuzzy segmentation)to fat
  *  4.convert everything outside voi to subcutaneous fat
  *  5.convert everything outside outer boundary mask (obMask) to background
  *
  * @param   Hard4Classes  --4class hard fuzzy segmented image
  *
  * @return  --image with labeled interstitial fat, subcutaneous fat, background and muscle (all but bone/bone marrow)
  */
   	public ModelImage processHardFat(ModelImage Hard4Classes){	 
   		//ShowImage(obMask,"obMask");
   		//ShowImage(voiMask,"voiMask");
	 progressBar.setMessage("Processing bundle fat");
	 	ModelImage fatImage = (ModelImage)Hard4Classes.clone();
		convert(fatImage, voiMask, fatImage, 1, MUSCLE);	/**fill voi with muscle*/
		convert(fatImage, Hard4Classes, fatImage, 189, FAT);		
		convert(fatImage, Hard4Classes, fatImage, 252, FAT);		
		convert(fatImage, voiMask, fatImage, 0, SUB_CUT_FAT);	/**all outside voi labeled subcutfat*/
		//ShowImage(fatImage,"after subcut/ before background");
		convert(fatImage, obMask, fatImage, 0, BACKGROUND_NEW);	/**all outside obMask labeled background*/
		
		cleanUp(fatImage, FAT, MUSCLE, 60*zDim/20); /**FAT FILTER (eliminates fat smaller than 60 pixels --for 20 slices)*/
		return fatImage;
 }  
    
 /**
  	* LABELING BONE MARROW (Extracts the bone marrow from 4 class hard fuzzy segmentation)
 *
 * @param   SegmentedImg  --4class hard fuzzy segmented image
 *
 * @return  --output image with bone marrow as extracted from fuzzy segmentation
 */
   	public ModelImage extractedBoneMarrow(ModelImage hardSegImg) {
 
   		ModelImage bMarrow = threshold2(hardSegImg, FAT_2_A, FAT_2_B);
   		// bMarrow contains all voxels labeled FAT_2_A or FAT_2_B in the C-Means segmented image (HardSeg)
   		// bMarrow is a binary image and remains binary through this method

   		//  PFH   		ShowImage(bMarrow, "BMarrow");

   		// find all objects in the thresholded image that have a cardinality within the specified range
   		IDObjects(bMarrow, 50*zDim, 500*zDim);

   		//  PFH   		ShowImage(bMarrow, "IDObjects");

   		// find the single object closest to the center of the image
   		isolatingCenterObject(bMarrow);
   		// bMarrow is a binary image where 1's label bone marrow and 0's are elsewhere

   		//  PFH   		ShowImage(bMarrow, "isolatingCenterObject");
   		
   		return bMarrow;
   	} // end extractedBoneMarrow(...)
  
    /**
     * Returns binary image at center object
     *
     * @param   srcImage       --image with center object to be isolated
     *
     */
    public void isolatingCenterObject(ModelImage srcImage) {
    	if(srcImage.getMax() > 1){
	        int[] numObjects = new int[zDim];
	        int[] BoneObject = new int[zDim];
	    	int bb, cc, i, n, x, y;
	    	float min;
	        for (bb = 0; bb < zDim; bb++) {
	        	numObjects[bb] = 0;
	        	BoneObject[bb] = 999999999;
	            try {
	                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
	                /**get numObjects per Slice. ~SLICE maximum.*/
	                for (cc = 0; cc < imgBuffer1.length; cc++) {
	                    if (imgBuffer1[cc]>numObjects[bb]) {
	                        numObjects[bb] = imgBuffer1[cc];		
	                    }
	                }
	                /**initialize centroid variables (for particular slice)*/
	                float[] centroidX = new float[numObjects[bb]];
	                float[] centroidY = new float[numObjects[bb]];
	                float[] distFromCent = new float[numObjects[bb]];                    
	                for (cc = 0; cc < numObjects[bb]; cc++) {
	                    centroidX[cc] = 0;
	                    centroidY[cc] = 0;
	                }
	                /**obtain centroid per slice & obtain distance between center and centroid of each object (per slice)*/                
	                for (cc = 1; cc <= numObjects[bb]; cc++) {
	                	n=1;
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
	                    distFromCent[cc-1] = Math.abs((centroidX[cc-1] - (xDim / 2))*(centroidX[cc-1] - (xDim / 2))
	                    		+ (centroidY[cc-1] - (yDim / 2))*(centroidY[cc-1] - (yDim / 2)));
	                }
	                /**use centroids to find center Bone object. (object with minimum distFromCent)*/
	                min = 10000; /**beginning distance from center. element with distance smaller than this becomes bone object.*/
	                for(cc=1;cc<=numObjects[bb];cc++){
	                	if(distFromCent[cc-1]<min && distFromCent[cc-1]<1000){
	                		/**1000: threshold min distance to ensure object close enough to center to be considered bone*/
	                		BoneObject[bb] = cc;
	                		min=distFromCent[cc-1];
	                	}
	                }
	                /**eliminate incorrect Bone objects*/
	                if(BoneObject[bb]!=999999999){
		                for(i = 0; i < imgBuffer1.length; i++){
		                        if (imgBuffer1[i] == BoneObject[bb]) {
		                            imgBuffer1[i] = 1;
		                        }
		                        else{
		                        	imgBuffer1[i]=0;
		                        }
		                }
	                }
	                srcImage.importData((bb * imgBuffer1.length), imgBuffer1, false);
	            } catch (IOException ex) {
	                System.err.println("error exporting data from destImageA in AlgorithmPipeline2");
	            }
	        }
    	}
    }
    

    /**
     * -------- BASIC STEPS ---------
     *
     * @param   destImage1  --image with user input VOI
     *
     * @return  --outputs binary mask with 1 where VOI is.
     */
    public ModelImage makeVOI(ModelImage destImage1) {
    	progressBar.setMessage("Converting VOI to Mask");
        return destImage1.generateBinaryImage(false, false);
    }
    
    /**
     * NORMALIZES INTENSITIES BETWEEN SLICES
     *
     * @param  srcImage  --source image with intensity inhomogeneities between slices 
     */
    public void ISN(ModelImage srcImage){
    	progressBar.setMessage("Executing ISN");
        PlugInAlgorithmISN isnAlgo = null;
        isnAlgo = new PlugInAlgorithmISN(srcImage, srcImage);
        isnAlgo.setProgressBarVisible(false);
        isnAlgo.run();

        isnAlgo.finalize();
        isnAlgo = null;        
        }
    
    /**
     * ELIMINATES MRI SHADING ARTIFACT - 'nonparametric intensity nonuniformity normalization' ~N3
     *
     * @param   srcImage1  source image with artifact/N3'd
     *
     */
    public ModelImage N3(ModelImage srcImage1){
    	progressBar.setMessage("Executing N3 on image");
        ModelImage fieldImage = new ModelImage(srcImage1.getType(), srcImage1.getExtents(), "fieldImage",
        		srcImage1.getUserInterface());
        ModelImage n3ProcessedImage = new ModelImage(srcImage1.getType(), srcImage1.getExtents(),
    			"n3ProcessedImage", srcImage1.getUserInterface());
	    AlgorithmIHN3Correction ihn3Algo1 = null;
	    ihn3Algo1 = new AlgorithmIHN3Correction(n3ProcessedImage, fieldImage, srcImage1,100f, 150, 0.0001f, 33.3f, 4f, 0.2f, 0.01f, true, false, false);
	    ihn3Algo1.setProgressBarVisible(false);
	    ihn3Algo1.run();
	  
	    ihn3Algo1.finalize();
	    ihn3Algo1 = null;	    
	    fieldImage.disposeLocal();
	    fieldImage = null;	    
	    return n3ProcessedImage;

    }

    /**
     * HARD FUZZY SEGMENTATION
     *
     * @param   srcImage  --regular unlabeled input image
     * @param   nClasses  --number of classes into which image will be segmented
     *
     * @return  --output Hard Fuzzy segmentation of srcImage into nClasses classes. 
     */
    public ModelImage HardFuzzy(ModelImage srcImage, int nClasses){
    	progressBar.setMessage("Calculating "+nClasses+" class Hard Fuzzy Segmentation.");

    	float centroid_array[] = new float[nClasses];
    	getCentroid(centroid_array, srcImage, nClasses);
    	
        ModelImage HardSeg[] = new ModelImage[1];
        FileInfoBase fileInfo1;
        HardSeg[0] = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "Hard-Fuzzy_seg", srcImage.getUserInterface());
		fileInfo1 = HardSeg[0].getFileInfo()[0];
		fileInfo1.setResolutions(srcImage.getFileInfo()[0].getResolutions());
		fileInfo1.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
		HardSeg[0].setFileInfo(fileInfo1, 0);

		AlgorithmFuzzyCMeans firstFuzz = new AlgorithmFuzzyCMeans(HardSeg, srcImage, nClasses,4, 1, 2, 2.0f, 20000, 200000, false,
        		AlgorithmFuzzyCMeans.HARD_ONLY, true, 0.0f, 200, 0.01f,true);
		/*   public AlgorithmFuzzyCMeans(ModelImage[] destImg, ModelImage srcImg, int _nClass, int _pyramidLevels,
                   int _jacobiIters1, int _jacobiIters2, float _q, float _smooth1, float _smooth2,
                   boolean _outputGainField, int _segmentation, boolean _cropBackground, float _threshold,
                   int _max_iter, float _tolerance, boolean _wholeImage)  ---wholeImage =true, means whole image*/

        firstFuzz.setCentroids(centroid_array);
        firstFuzz.setProgressBarVisible(false);
        firstFuzz.run();
        firstFuzz.finalize();
        firstFuzz = null;
        
        return HardSeg[0];
        }
  
    /**
     * SOFT FUZZY SEGMENTATION
     *
     * @param   srcImage  --regular unlabeled input image
     * @param   nClasses  --number of classes into which image will be segmented.
     *
     * @return  --output Soft Fuzzy segmentation of srcImage into nClasses classes.
     */
    public ModelImage[] SoftFuzzy(ModelImage srcImage, int nClasses){
    	float centroid_array[] = new float[nClasses];
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

        AlgorithmFuzzyCMeans firstFuzz = null;        
        firstFuzz = new AlgorithmFuzzyCMeans(FuzzySeg, srcImage, nClasses, 4, 1, 2, 2.0f, 20000, 200000, false,
                AlgorithmFuzzyCMeans.FUZZY_ONLY, false, 0.0f, 200, 0.01f, true);
        firstFuzz.setCentroids(centroid_array);
        firstFuzz.setProgressBarVisible(false);
        firstFuzz.run();
        firstFuzz.finalize();
        firstFuzz = null;

        return FuzzySeg;
        }
    
    /**
     * OBTAINS IMAGE CENTROIDS GIVEN NUMBER OF SEGMENTATION CLASSES
     *
     * @param  centroid_array  --array of centroids
     * @param  srcImage        --image to be segmented
     * @param  nClasses        --number of classes into which image will be segmented
     */
	public void getCentroid(float[] centroid_array, ModelImage srcImage, int nClasses){
		int dd;
		double max = srcImage.getMax();
    	for(dd = 1; dd < (nClasses+1); dd++){
    		centroid_array[dd - 1] = (float)(dd * max / (nClasses + 1));
    	}
	}
    
    /**
     * MERGING BONE/BONEMARROW IMAGE WITH BKGRD, FAT,SUBCUTFAT,MUSCLE IMAGE
     *
     * @param  mergedImage  --resultant image with labeled bone/bonemarrow, background, interstitial fat, subcutfat, muscle
     * @param  BoneImage    --image with labeled bone and bone marrow
     * @param  bundleImage  --image with labeled background, interstitial fat, subcutaneous fat, muscle
     */
    public void mergeImages(ModelImage mergedImage, ModelImage BoneImage, ModelImage bundleImage){
    	progressBar.setMessage("Merging Processed Thigh Images");
        convert(bundleImage, BoneImage, bundleImage, BONE, BONE);
        convert(mergedImage, BoneImage, bundleImage, BONE_MARROW, BONE_MARROW);

        cleanUp(mergedImage, BACKGROUND_NEW, SUB_CUT_FAT, 100*zDim/20);
        cleanUp(mergedImage, SUB_CUT_FAT, BACKGROUND_NEW, 100*zDim/20);
        }
    
    
    
    /**
     * srcImage with noiseIntensity of size up to sizeNoise converted to bkrdIntensity
     *
     * @param  srcImage        --image to be cleaned up
     * @param  noiseIntensity  --intensity classified as 'noise'
     * @param  bkrdIntensity   --intensity to which noise changed when cleaned up
     * @param  sizeNoise       --size of noise filter
     */
    public void cleanUp(ModelImage srcImage, int noiseIntensity, int bkrdIntensity, int sizeNoise) {
        ModelImage tempImage = threshold1(srcImage, noiseIntensity);
        IDObjects(tempImage, 1, sizeNoise);
        ModelImage tempImage1 = threshold2(tempImage, 1f, (float)tempImage.getMax());
        tempImage.disposeLocal();
        tempImage=null;
        convert(srcImage, tempImage1, srcImage, 1, bkrdIntensity);
        tempImage1.disposeLocal();
        tempImage1 = null;
    }
    
        /**
     * whenever templateImage = tempIntensity, srcImage converted to newDestIntensity
     *
     * @param  destImage      --destination image
     * @param  templateImage  
     * @param  srcImage       --source image
     * @param  templateIntensity 
     * @param  newDestIntensity   
     */
    public void convert(ModelImage destImage, ModelImage templateImage, ModelImage srcImage, int templateIntensity, 
            int newDestIntensity){
    	int bb, cc;
    	if(srcImage!=null){
			for (bb = 0; bb < zDim; bb++) {
			    try {
			    	srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
			        templateImage.exportData((bb * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
			        for (cc = 0; cc < imgBuffer1.length; cc++) {
			            if (imgBuffer2[cc] == templateIntensity) { 
			                imgBuffer1[cc] = newDestIntensity; 
			            }
			        }
			        destImage.importData((bb * imgBuffer1.length), imgBuffer1, false);
			    } catch (IOException ex) {
			        System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP5 black to Muscle");
			    }
			}
    	}
    	else{
    		System.out.println("srcImage in convert algorithm is null");
    	}
    }
     
    

    /**
     * morphological FILL_HOLES
     *
     * @param  srcImage  --source image
     */
    public void FillHole(ModelImage srcImage) {
        AlgorithmMorphology3D MorphFILL = null;
        MorphFILL = new AlgorithmMorphology3D(srcImage, 4, 2, AlgorithmMorphology3D.FILL_HOLES, 0, 0, 0, 1, true);
        MorphFILL.setProgressBarVisible(false);
        MorphFILL.run();
    }
    
    /**
     * morphological ID_OBJECTS
     *
     * @param  srcImage  --source image
     * @param  min       --smallest object to let through
     * @param  max       --largest object to let through
     */
    public void IDObjects(ModelImage srcImage, int min, int max) {
        AlgorithmMorphology3D MorphIDObj = null;
        MorphIDObj = new AlgorithmMorphology3D(srcImage, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        MorphIDObj.setMinMax(min, max);
        MorphIDObj.setProgressBarVisible(false);
        MorphIDObj.run();
    }
    
    /**
     * morphological OPEN
     *
     * @param  sourceImg  --source image
     */
    public void Open(ModelImage sourceImg, int kernalSize) {
    	
        AlgorithmMorphology3D MorphOpen = null;
        if(kernalSize ==6){
        MorphOpen = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1,
                                              AlgorithmMorphology3D.OPEN, 1, 1, 0, 0, true);
        }
        if(kernalSize==24){
            MorphOpen = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED24, 1,
                    AlgorithmMorphology3D.OPEN, 1, 1, 0, 0, true);        	
        }
        MorphOpen.setProgressBarVisible(false);
        MorphOpen.run();
    }
    
    /**
     * morphological CLOSE
     *
     * @param  sourceImg  --source image
     * @param  kernalSize --kernal size for closure
     */
    public void Close(ModelImage sourceImg, int kernalSize, int iters) {
    	
        AlgorithmMorphology3D MorphClose = null;
        if(kernalSize ==6){
        MorphClose = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1,
                                              AlgorithmMorphology3D.CLOSE, iters, iters, 0, 0, true);
        }
        if(kernalSize==24){
            MorphClose = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED24, 1,
                    AlgorithmMorphology3D.CLOSE, iters, iters, 0, 0, true);        	
        }
        MorphClose.setProgressBarVisible(false);
        MorphClose.run();
    }

    /**
     * morphological DILATE
     *
     * @param  srcImage  	--source image
     * @param  kernalSize	--kernal size for dilation
     */
    public void Dilate(ModelImage sourceImg, int kernalSize) {
    	
        AlgorithmMorphology3D MorphDilate = null;
        if(kernalSize ==6){
        	MorphDilate = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1,
                                              AlgorithmMorphology3D.DILATE, 1, 0, 0, 0, true);
        }
        if(kernalSize==24){
        	MorphDilate = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED24, 1,
                    AlgorithmMorphology3D.DILATE, 1, 0, 0, 0, true);        	
        }
        MorphDilate.setProgressBarVisible(false);
        MorphDilate.run();
    }
    
    /**
     * morphological ERODE
     *
     * @param  srcImage  	--source image
     * @param  kernalSize 	--kernal size for erosion
     */
    public void Erode(ModelImage sourceImg, int kernalSize) {
    	
        AlgorithmMorphology3D MorphErode = null;
        if(kernalSize ==6){
        	MorphErode = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1,
                                              AlgorithmMorphology3D.ERODE, 0, 1, 0, 0, true);
        }
        if(kernalSize==24){
        	MorphErode = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED24, 1,
                    AlgorithmMorphology3D.ERODE, 0, 1, 0, 0, true);        	
        }
        MorphErode.setProgressBarVisible(false);
        MorphErode.run();
    }
    
    
    /**
     * AlgorithmImageCalculator IMAGE SUBTRACT
     *
     * @param   srcImageA	--source image
     * @param   srcImageB	--source image by A is subtracted
     * @param 	destImage	--destination image
     */
    public void ImgSubtract(ModelImage destImage,ModelImage srcImageA,ModelImage srcImageB) {
    	AlgorithmImageCalculator MorphSubtract = null;
//    	AlgorithmImageCalculator(ModelImage destImg, ModelImage srcImgA, ModelImage srcImgB, int type, int _clipMode,
//      boolean maskFlag, String adOpString)
    	MorphSubtract = new AlgorithmImageCalculator(destImage, srcImageA, srcImageB, 4, 0,
    			  true, "SUBTRACT");
    	MorphSubtract.setProgressBarVisible(false);
    	MorphSubtract.run();
    }
    

    /**
     * THRESHOLD through given intensity
     *
     * @param   threshSourceImg  --source image
     * @param   intensity        --intensity to be let through
     *
     * @return  return image of given intensity
     */
    public ModelImage threshold1(ModelImage threshSourceImg, float intensity) {
        float[] thresh = { intensity - 5, intensity + 5 };
        ModelImage resultImage = null;
        resultImage = threshold(threshSourceImg, thresh);

        return resultImage;
    }

    /**
     * THRESHOLD through a range of intensities
     *
     * @param   threshSourceImg  --source image
     * @param   intensity1       --lower threshold
     * @param   intensity2       --upper threshold
     *
     * @return  returns image of intensities from intensity1 to intensity2
     */
    public ModelImage threshold2(ModelImage threshSourceImg, float intensity1, float intensity2) {
    	float thresh[] = {intensity1, intensity2};    	
    	ModelImage resultImage = null;
    	resultImage = threshold(threshSourceImg, thresh);
        return resultImage;
    }
    
    /**
     * THRESHOLD everything above 'thresh'
     *
     * @param   threshSourceImg		--source image
     * @param   thresh        		--lower/upper bound intensities
     *
     * @return  returns image of intensities in range 'thresh'
     */
    public ModelImage threshold(ModelImage threshSourceImg, float[] thresh) {
        ModelImage resultImage = null;
        resultImage = new ModelImage(threshSourceImg.getType(), threshSourceImg.getExtents(), "threshResultImg",
                                     threshSourceImg.getUserInterface());

        AlgorithmThresholdDual threshAlgo = null;
        threshAlgo = new AlgorithmThresholdDual(resultImage, threshSourceImg, thresh, 1, true, true, true);
        threshAlgo.setProgressBarVisible(false);
        threshAlgo.run();

        return resultImage;
    }

    
    /**
     * Creates new screen to show image
     *
     * @param  sourceImg  --source image
     * @param  Name       --name that appears in image window
     */
    public void ShowImage(ModelImage sourceImg, String Name) {
        ModelImage cloneImg = (ModelImage) sourceImg.clone();
        cloneImg.calcMinMax();
        cloneImg.setImageName(Name);
        new ViewJFrameImage(cloneImg);
    }
    
    
    /**
     * DOCUMENT ME!
     *
     * @return  returns result image A (right thigh)
     */
    public ModelImage getResultImageA() {
        return this.destImageA;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  returns result image B (left thigh)
     */
    public ModelImage getResultImageB() {
        return this.destImageB;
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  Exception  DOCUMENT ME!
     */
    private void jbInit() throws Exception { }
    
    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
        progressBar.dispose();
    }
    
    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        imgBuffer1 = null;

        if (obMaskA != null) {
            obMaskA.disposeLocal();
            obMaskA = null;
        }

        if (obMaskB != null) {
            obMaskB.disposeLocal();
            obMaskB = null;
        }

    }
    
    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {

        buildProgressBar("OAI Thigh Seg. 6/22/06", "Processing images...", 0, 100);
        initProgressBar();
        
        xDim = 1;
        yDim = 1;
        zDim = 1;
        
    	//initialize totals to zero if on first thigh, else add on
        int subcutfatIntensityTotal= 0; 
        int fatIntensityTotal= 0; 
        int muscleIntensityTotal= 0; 
        int boneIntensityTotal= 0; 
        int boneMarrowIntensityTotal= 0;
        
        int subcutfatCountTotal = 0;
        int fatCountTotal = 0;
        int muscleCountTotal = 0;
        int boneCountTotal = 0;
        int boneMarrowCountTotal = 0;
        int total_thighCount = 0;

        //aa=1 RIGHT THIGH, aa=2 LEFT THIGH
        for (aa = 1; aa <= 2; aa++) {
            if (aa == 1) {
            	// set processedImage and obMask to the RIGHT thigh image 
                getVariables(srcImageA, obMaskA);
            } else if (aa == 2) {
               	// set processedImage and obMask to the LEFT thigh image
            	getVariables(srcImageB, obMaskB);
            }

            sliceSize = xDim * yDim;
            volSize = xDim * yDim * zDim;
            imgBuffer1 = new int[sliceSize];
            imgBuffer2 = new int[sliceSize];
            
            /********************************************************
             **************** general processing ********************
             ********************************************************/            
            
            // STEP 1: VOI to Mask

            // processedImage is the right or left thigh image
            voiMask = makeVOI(processedImage);
            // voiMask: binary image where 1's are inside the VOI otherwise values are 0's
            // used in processHardFat(), should probably be made later!!
 // PFH            ShowImage(voiMask, "voi  mask");
            
            progressBar.updateValue((50 * (aa - 1)) + 4, runningInSeparateThread);


            // STEP 2: ISN and N3 inside VOI
            ISN(processedImage);
            // processedImage is right/left thigh image after ISN
// PFH            ShowImage(processedImage, "ISN");

            progressBar.updateValue((50 * (aa - 1)) + 9, runningInSeparateThread);
            
            // should take this out since we decided to segment only N3 processed images!!
            // remove the check box from the dialog (PlugInDialogPipeline) also
            if(useN3) {
            	ModelImage tmp = processedImage;
            	processedImage = N3(processedImage);
            	processedImage.setVOIs(tmp.getVOIs());
            	tmp.disposeLocal();
            	tmp = null;
            	//ViewJFrameImage dif = new ViewJFrameImage(destImage2);
            }
           	progressBar.updateValue((50 * (aa - 1)) + 30, runningInSeparateThread);
                       

            //STEP 3: FUZZY SEGMENT ENTIRE IMAGE
           	// Fuzzy C-Means for the right/left thigh image
            ModelImage HardSeg1 = HardFuzzy(processedImage, 4);
            // HardSeg1 image contains 4 different label values
            //	63 for background and bone
            //	126 muscle
            //	189 fat 1 (Bone Marrow)
            //	252 fat 2 (Bone Marrow)
// PFH            ShowImage(HardSeg1,"4 class hard fuzzy segmentation");

            progressBar.updateValue(50 * (aa - 1) + 18, runningInSeparateThread);



            // STEP 4: ISOLATE BONE AND MARROW
            
            // at the end of this step boneSeg must be filled in
            
            // make a empty bone/marrow image
            ModelImage boneSeg = new ModelImage(HardSeg1.getType(), HardSeg1.getExtents(), "Bone Seg Image", HardSeg1.getUserInterface());
            processBoneAndMarrow(boneSeg, HardSeg1, processedImage);
	            
// PFH            ShowImage(boneSeg, "bone/marrow seg");

/*
            //STEP 4: ISOLATE BONE
            //(and if bone continuous fill with bone marrow)
//            ModelImage boneSeg = processBone(HardSeg1);
// PFH            ShowImage(boneSeg, "isolated bone");
            progressBar.updateValue(50 * (aa - 1) + 27, runningInSeparateThread);

            ModelImage boneSeg = new ModelImage(HardSeg1.getType(), HardSeg1.getExtents(), "Bone Seg Image",
            		HardSeg1.getUserInterface());
            //STEP 5: PROCESS BONE MARROW --(given none found in step 4) 
            //(and if bone nonexistant, create thin artifical bone around bone marrow)
            if(continuousBone == false){
	            processBoneAndMarrow(boneSeg, HardSeg1);				//ShowImage(destImage3a,"bone with bone marrow");
	            progressBar.updateValue(50 * (aa - 1) + 36, runningInSeparateThread);
            }            
            processedImage.disposeLocal();
            processedImage = null;
*/            
            
            //STEP 6: PROCESS FAT
            ModelImage fatSeg = processHardFat(HardSeg1);        				//ShowImage(destImage3b, "bundle cleanedup fat image");
            progressBar.updateValue(50 * (aa - 1) + 46, runningInSeparateThread);
            HardSeg1.disposeLocal();
            HardSeg1=null;
            voiMask.disposeLocal();
            voiMask = null;
            obMask.disposeLocal();
            obMask = null;

            //STEP 7: MERGING PROCESSED THIGH IMAGES
            mergeImages(fatSeg, boneSeg, fatSeg);			//ShowImage(destImage3b, "after 'merge'");
            progressBar.updateValue(50 * (aa - 1) + 50, runningInSeparateThread);
            boneSeg.disposeLocal();
            boneSeg = null;
            
            //------------------STEP8: counting tissue types -------------------
            //outputData(destImage3b);
            //public void outputData(ModelImage destImage3b){
                
            int subcutfatIntensityAvg = 0; 
            int fatIntensityAvg = 0;
            int muscleIntensityAvg = 0;
            int boneIntensityAvg = 0;
            int boneMarrowIntensityAvg = 0;
            
        	int subcutfatIntensitySum = 0; 
        	int fatIntensitySum = 0;
        	int muscleIntensitySum = 0;
        	int boneIntensitySum = 0;
        	int boneMarrowIntensitySum = 0;
        	
            int subcutfatCount = 0;
            int fatCount = 0;
            int muscleCount = 0;
            int boneCount = 0;
            int boneMarrowCount = 0;
           
        
    	    for (bb = 0; bb < zDim; bb++) {
    	        try {
    	        	fatSeg.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
                    if (aa == 1){
                        srcImageA.exportData((bb * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                    }
                    else {
                        srcImageB.exportData((bb * imgBuffer2.length), imgBuffer2.length, imgBuffer2);
                    }
    	            for (cc = 0; cc < imgBuffer1.length; cc++) {
    	                if (imgBuffer1[cc] == SUB_CUT_FAT) {
                            subcutfatIntensitySum += imgBuffer2[cc];
                            subcutfatCount++;
    	                    subcutfatCountTotal++;
    	                    total_thighCount++;
    	                } else if (imgBuffer1[cc] == FAT) {
                            fatIntensitySum += imgBuffer2[cc];
                            fatCount++;
    	                    fatCountTotal++;
    	                    total_thighCount++;
    	                } else if (imgBuffer1[cc] == MUSCLE) {
                            muscleIntensitySum += imgBuffer2[cc];
                            muscleCount++;
    	                    muscleCountTotal++;
    	                    total_thighCount++;
    	                } else if (imgBuffer1[cc] == BONE) {
                            boneIntensitySum += imgBuffer2[cc];
                            boneCount++;
    	                    boneCountTotal++;
    	                    total_thighCount++;
    	                } else if (imgBuffer1[cc] == BONE_MARROW) {
                            boneMarrowIntensitySum += imgBuffer2[cc];
                            boneMarrowCount++;
    	                    boneMarrowCountTotal++;
    	                    total_thighCount++;
    	                }
    	            }
    	        } catch (IOException ex) {
    	            System.err.println(
    	                    "error exporting data from srcImage in AlgorithmPipeline2");
    	        }
    	    }
            
            subcutfatIntensityTotal     += subcutfatIntensitySum; 
            fatIntensityTotal           += fatIntensitySum;
            muscleIntensityTotal        += muscleIntensitySum;
            boneIntensityTotal          += boneIntensitySum;
            boneMarrowIntensityTotal    += boneMarrowIntensitySum;
            
    	    if(subcutfatCount!=0 && fatCount!=0 && muscleCount!=0 && boneCount!=0 && boneMarrowCount!=0){
                subcutfatIntensityAvg  = subcutfatIntensitySum/subcutfatCount;
                fatIntensityAvg        = fatIntensitySum/fatCount;
                muscleIntensityAvg     = muscleIntensitySum/muscleCount;
                boneIntensityAvg       = boneIntensitySum/boneCount;
                boneMarrowIntensityAvg = boneMarrowIntensitySum/boneMarrowCount;
    	    } 
    	    
    	    float[] res = srcImageA.getFileInfo()[0].getResolutions();
            float voxelSize = res[0] * res[1] * res[2];
            if (aa == 1) {
                destImageA = fatSeg;
                destImageA.calcMinMax();
                UI.getMessageFrame().append("Segmented Images - Results:  " +
                                            PlugInAlgorithmOAICropImage.patientID,
                                            "Right thigh ");
            } else {
                destImageB = fatSeg;
                destImageB.calcMinMax();
                UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmOAICropImage.patientID, 
                        "Left thigh ");
            }
            NumberFormat nf = NumberFormat.getInstance();
            nf.setMaximumFractionDigits(2);
            nf.setMinimumFractionDigits(0);
            
            if(aa==1){
                UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmOAICropImage.patientID, 
                        "Volume: (cubic Millimeters) \n" +
                		"CLASS \t \tVOLUME\tAVG INTENSITY \n" +
                		"SubcutaneousFat \t" + nf.format(subcutfatCount * voxelSize) + "\t"+subcutfatIntensityAvg+"\n"+ 
                		"InterstitialFat \t\t" + nf.format(fatCount * voxelSize) + "\t"+fatIntensityAvg+"\n" +
                		"Muscle \t \t" + nf.format(muscleCount * voxelSize) + "\t"+muscleIntensityAvg+"\n"+
                		"Bone \t \t" + nf.format(boneCount * voxelSize) + "\t"+boneIntensityAvg+"\n"+
                		"BoneMarrow \t\t" + nf.format(boneMarrowCount * voxelSize) + "\t"+boneMarrowIntensityAvg + "\n\n\n");
            }
            if(aa==2){
                UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmOAICropImage.patientID, 
                        "Volume: (cubic Millimeters) \n" +
                        "CLASS \t \tVOLUME\tAVG INTENSITY \n" +
                        "SubcutaneousFat \t" + nf.format(subcutfatCount * voxelSize) + "\t"+subcutfatIntensityAvg+"\n"+ 
                        "InterstitialFat \t\t" + nf.format(fatCount * voxelSize) + "\t"+fatIntensityAvg+"\n" +
                        "Muscle \t \t" + nf.format(muscleCount * voxelSize) + "\t"+muscleIntensityAvg+"\n"+
                        "Bone \t \t" + nf.format(boneCount * voxelSize) + "\t"+boneIntensityAvg+"\n"+
                        "BoneMarrow \t\t" + nf.format(boneMarrowCount * voxelSize) + "\t"+boneMarrowIntensityAvg+"\n\n"+
                        "TOTAL Thighs \t" + nf.format(total_thighCount * voxelSize) + "\n\n\n");
            	
                UI.getMessageFrame().append("Segmented Images - Results:  " + 
                		PlugInAlgorithmOAICropImage.patientID, 
        	    		"TOTAL THIGH DATA\n" +
        	    		"CLASS \t \tAVG INTENSITY \n" +
        	    		"SubcutaneousFat\t"+subcutfatIntensityTotal/subcutfatCountTotal+ "\n"+
        	    		"InterstitialFat\t\t"+fatIntensityTotal/fatCountTotal+ "\n"+
        	    		"Muscle\t\t"+muscleIntensityTotal/muscleCountTotal+ "\n"+
        	    		"Bone\t\t"+boneIntensityTotal/boneCountTotal+ "\n"+
        	    		"BoneMarrow\t\t"+boneMarrowIntensityTotal/boneMarrowCountTotal);
            	}

                progressBar.updateValue(50*aa, runningInSeparateThread);
            }
	        System.out.println("thigh cleanup done --destImage");
	        setCompleted(true);
	        disposeProgressBar();
            
        }

}