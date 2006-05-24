import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import java.io.*;

import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.model.algorithms.AlgorithmFuzzyCMeans;
import gov.nih.mipav.model.algorithms.AlgorithmIHN3Correction;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmImageCalculator;


/**
 * <p>The OAI -- Osteoarthritis Initiative – is a nationwide research study sponsored by the National Institutes of Health, that will help us better understand how to prevent and treat knee osteoarthritis, one of the most common causes of disability in adults. It is a four-year study and will recruit men and women aged 45 and above at high risk for developing symptomatic knee osteoarthritis. Osteoarthritis causes more health problems and medical expenses than any other form of arthritis. Symptoms of osteoarthritis can range from stiffness and mild pain to severe joint pain and even disability. The OAI cohort will be 5000 participants with clinically significant knee OA or at high risk for developing incident OA and obtain the appropriate images and bio-specimens needed for investigation and validation of OA biomarkers. The large number of images that results from the OAI is a major obstacle to overcome. Manual image segmentation is laborious and subject to inter and intra-observer variability when performing volumetric analysis. Therefore, BIRSS has started a multistage segmentation and quantification technique to automatically or semi-automatically process the entire cohort.</p>
 */
public class PlugInAlgorithmPipelineB extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** hardSeg1 intensities (3 class segmentation). */
    public static int BACKGROUND = 85;

    /** DOCUMENT ME! */
    public static int Muscle = 170;

    /** DOCUMENT ME! */
    public static int FAT = 255;

    /** hardSeg2 intensities (4 class segmentation). */
    public static int BACKGROUND_2 = 63;

    /** DOCUMENT ME! */
    public static int FAT_2_A = 189;

    /** DOCUMENT ME! */
    public static int FAT_2_B = 252;

    /** DOCUMENT ME! */
    public static int Muscle_2 = 126;

    /** intensity transformations (relabeled intensities). */
    public static int BACKGROUND_NEW = 0;

    /** DOCUMENT ME! */
    public static int Bone = 100;

    /** DOCUMENT ME! */
    public static int BoneMarrow = 200;

    /** DOCUMENT ME! */
    public static int SUB_CUT_FAT = 225;
    // public static int FAT = 255;
    // public static int MUSCLE = 170;

    /** DOCUMENT ME! */
    public static int xDim, yDim, zDim, sliceSize, volSize, aa, bb, cc, i;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    /** DOCUMENT ME! */
    private ModelImage BMarrow = null;

    /** DOCUMENT ME! */
    private ModelImage destImage1 = null;

    /** DOCUMENT ME! */
    private ModelImage destImage2 = null;

    /** DOCUMENT ME! */
    private ModelImage destImage3a = null;

    /** DOCUMENT ME! */
    private ModelImage destImage3b = null;

    /** DOCUMENT ME! */
    private ModelImage destImageA = null;

    /** DOCUMENT ME! */
    private ModelImage destImageB = null;

    /** DOCUMENT ME! */
    private ModelImage fieldImage = null;

    /** DOCUMENT ME! */
    private AlgorithmFuzzyCMeans firstFuzz = null;
    
    /** DOCUMENT ME! */
    private ModelImage HardSeg1 = null;

    /** DOCUMENT ME! */
    private int[] imgBuffer1 = null;

    /** DOCUMENT ME! */
    private int[] imgBuffer2 = null;

    /** DOCUMENT ME! */
    private ModelImage obMask = null;

    /** DOCUMENT ME! */
    private ModelImage obMaskA = null;

    /** DOCUMENT ME! */
    private ModelImage obMaskB = null;

    /** DOCUMENT ME! */
    private ViewUserInterface UI = ViewUserInterface.getReference();

    /** DOCUMENT ME! */
    private ModelImage voiMask = null;
    
    /** DOCUMENT ME! */
    private boolean continuousBone = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new PlugInAlgorithmPipelineB object.
     */
    public PlugInAlgorithmPipelineB() {
        try {
            jbInit();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Default constructor.
     *
     * @param  destImageA  the source image
     * @param  destImageB  DOCUMENT ME!
     * @param  obMaskA     DOCUMENT ME!
     * @param  obMaskB     DOCUMENT ME!
     */
    public PlugInAlgorithmPipelineB(ModelImage destImageA, ModelImage destImageB, ModelImage obMaskA,
                                    ModelImage obMaskB) {
        this.obMaskA = obMaskA;
        this.obMaskB = obMaskB;
        this.destImageA = destImageA;
        this.destImageB = destImageB;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    /**
     * DOCUMENT ME!
     *
     * @param  destImageA  DOCUMENT ME!
     * @param  obMaskA     DOCUMENT ME!
     */
    public void getVariables(ModelImage destImageA, ModelImage obMaskA) {
        xDim = destImageA.getExtents()[0];
        yDim = destImageA.getExtents()[1];

        if (destImageA.getNDims() == 3) {
            zDim = destImageA.getExtents()[2];
        }

        destImage1 = (ModelImage) destImageA.clone();
        destImage1.setVOIs(destImageA.getVOIs());
        obMask = (ModelImage) obMaskA.clone();
        fieldImage = new ModelImage(destImageA.getType(), destImageA.getExtents(), "fieldImage",
                                    destImageA.getUserInterface());
        destImage3a = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3a",
                                     destImageA.getUserInterface());
        destImage3b = new ModelImage(destImageA.getType(), destImageA.getExtents(), "destImage3b",
                                     destImageA.getUserInterface());
    }
    
    

   
   /**
    * DOCUMENT ME!
    *
    * @param  HardSeg  DOCUMENT ME!
    */
   // isolates bone, labels as bone if continuous (meaning, can be filled with marrow)
   public ModelImage processBone(ModelImage SegmentedImage){
   ModelImage residual = new ModelImage(SegmentedImage.getType(), SegmentedImage.getExtents(), "residual",
	    		SegmentedImage.getUserInterface());		
		progressBar.setMessage("Isolating/Labeling Bone");
		continuousBone=false;
		
		//round1 maximum bone isolation
		ModelImage BoneID = threshold1(SegmentedImage, BACKGROUND_2);
		ModelImage BoneIDtemp = (ModelImage) BoneID.clone();
		Open(BoneIDtemp, 24);
		Close(BoneIDtemp, 24);
		IDObjects(BoneIDtemp, zDim*2000/20, zDim*10000/20);  //should be on the order or 10,000
		isolatingCenterObject(BoneIDtemp); //returns binary image at center object
		ModelImage BoneIDtemp1 = (ModelImage)BoneIDtemp.clone();
		FillHole(BoneIDtemp1);
		ImgSubtract(residual,BoneIDtemp1,BoneIDtemp);
		Erode(residual, 24);
		if(residual.getMax() == 0){
			BoneIDtemp.disposeLocal();		BoneIDtemp = null;
			BoneIDtemp1.disposeLocal();		BoneIDtemp1 = null;
			System.out.println("round1, fillBoneMarrow doesnt work");
			BoneIDtemp = (ModelImage) BoneID.clone();
			Open(BoneIDtemp,6);
			Close(BoneIDtemp,24);
			IDObjects(BoneIDtemp, zDim*2000/20, zDim*10000/20);
			isolatingCenterObject(BoneIDtemp); //returns binary image at center object
			BoneIDtemp1 = (ModelImage)BoneIDtemp.clone();
			FillHole(BoneIDtemp1);
			ImgSubtract(residual,BoneIDtemp1,BoneIDtemp);
			Erode(residual, 24);
			
			if(residual.getMax() == 0){
				BoneIDtemp.disposeLocal();	BoneIDtemp=null;
				BoneIDtemp1.disposeLocal();		BoneIDtemp1 = null;
				System.out.println("round2, fillBoneMarrow doesnt work");
				BoneIDtemp = (ModelImage)BoneID.clone();
				IDObjects(BoneIDtemp, zDim*2000/20, zDim*10000/20);
				isolatingCenterObject(BoneIDtemp); //returns binary image at center object
				BoneIDtemp1 = (ModelImage)BoneIDtemp.clone();
				FillHole(BoneIDtemp1);
				ImgSubtract(residual,BoneIDtemp1,BoneIDtemp);
				Erode(residual, 24);
				if(residual.getMax() == 0){
					System.out.println("round3, fillBoneMarrow didnt work -using bone with artificially filled marrow");
					BoneIDtemp.disposeLocal();	BoneIDtemp=null;
					BoneIDtemp1.disposeLocal();		BoneIDtemp1 = null;
					BoneIDtemp = (ModelImage)BoneID.clone();
					Close(BoneIDtemp,24);
					IDObjects(BoneIDtemp, zDim*2000/20, zDim*10000/20);
					isolatingCenterObject(BoneIDtemp); //returns binary image at center object
					BoneIDtemp1 = (ModelImage)BoneIDtemp.clone();
					FillHole(BoneIDtemp1);
					ImgSubtract(residual,BoneIDtemp1,BoneIDtemp);
					Erode(residual, 24);
					if(residual.getMax() == 0){
						System.out.println("round4, fillBoneMarrow unsuccessful. no marrow labeled in processBone. bone not continuous. bone however IS labeled.");
						convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp, 1, Bone);
					}
					else{
						continuousBone = true;
						System.out.println("round4, fillBoneMarrow successful");
						convert(BoneIDtemp1, BoneIDtemp1, BoneIDtemp1, 1, BoneMarrow);
						//ShowImage(BoneIDtemp1,"boneMarrow FillHole'd");
					    convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp1, 1, Bone);
					}
				}
				else{
					continuousBone = true;
					System.out.println("round3, fillBoneMarrow successful");
					convert(BoneIDtemp1, BoneIDtemp1, BoneIDtemp1, 1, BoneMarrow);
					//ShowImage(BoneIDtemp1,"boneMarrow FillHole'd");
					//img with marrow thresholded, where pixel=1, make same img=boneMarrow
				    convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp1, 1, Bone);
//				  	//img with boneMarrow, where img has  pixel=1, make same img=boneMarrow
				}				
			}
			else{
				continuousBone = true;
				System.out.println("round2, fillBoneMarrow successful");
				convert(BoneIDtemp1, BoneIDtemp1, BoneIDtemp1, 1, BoneMarrow);
				//ShowImage(BoneIDtemp1,"boneMarrow FillHole'd");
			    convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp1, 1, Bone);
			}
		}
		else{
			continuousBone = true;
			//returns continuous bone result. and filled in bone marrow.
			System.out.println("round1, fillBoneMarrow successful");
			convert(BoneIDtemp1, BoneIDtemp1, BoneIDtemp1, 1, BoneMarrow);
			//ShowImage(BoneIDtemp1,"boneMarrow FillHole'd");
		    convert(BoneIDtemp1, BoneIDtemp, BoneIDtemp1, 1, Bone);
		    
		    //cleanUp(BoneIDtemp, BoneMarrow, Bone, zDim*1000/20);  //bonemarrow size up to 100 turn into bone -bonecleanup
		}
		
	    progressBar.updateValue(50 * (aa - 1) + 43, activeImage);   
	   	    
	    return BoneIDtemp1;
   }

   
   
   /**
    * DOCUMENT ME!
    *
    * @param  srcImage  DOCUMENT ME!
    */
   public void processBoneMarrow(ModelImage srcImage, ModelImage SegmentedImg){
	   //case1: NO BONE. no bone marrow. (we create artificial bone)
	   //case2: DISCONTINUOUS BONE. no bone marrow. (we fill in the gaps between discontinuous bone and marrow.
	   //case3: CONTINUOUS BONE. bone marrow to fill. and NOISY BONE MARROW. also NOISY BONE. (processed in the end, not this method)
	   //case4: CONTINUOUS BONE. CONTINUOUS BONE MARROW. (not processed at all)
	   
	   //*algorithm only used if bone not continuous. 
	   //so first must find whether or not it is case 1 or case 2*
	   int bb, cc, dd, i;
	   boolean thereisBone = false;
       for (bb = 0; bb < zDim; bb++) {
           try {
               srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
               for (cc = 0; cc < imgBuffer1.length; cc++) {
	               	if(imgBuffer1[cc]==Bone){
	               		thereisBone = true;
	               	}
               }
           } catch (IOException ex) {
               System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP7");
           }
       }
       
       
	   //where srcImage is HardSeg1 4class hard fuzzy segmentation, uses 
	   //160-255 intensity pixels to locate 'bone marrow', and label.
	   ModelImage BMarrow = extractedBoneMarrow(SegmentedImg); //BMarrow should be binary image (extracted from fuzzyImg)
       convert(srcImage, BMarrow, srcImage, 1, BoneMarrow);       
       BMarrow.disposeLocal();BMarrow=null;

	   //fills gaps between discontinuous bone and 4class segmentation result bone marrow
	   	//ShowImage(srcImage, "before any loop");
	   	if(thereisBone ==true){
		        for (bb = 0; bb < zDim; bb++) {
		            try {
		                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
		                //loop for when there IS bone
		                for (cc = 0; cc < imgBuffer1.length-10*xDim; cc++) {
		                	//gap between 'bone to marrow', fill with bonemarrow
		                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc+1]!=Bone){
		                		for(dd=10;dd>0;dd--){
			                    	if(imgBuffer1[cc+dd]==BoneMarrow && imgBuffer1[cc+dd-1]!=BoneMarrow){
			                    		for(i=1;i<dd;i++){
			                    			imgBuffer1[cc+i]=BoneMarrow;
			                    		}
			                    	}
		                		}
		                	}
		                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc+xDim]!=Bone){
		                		for(dd=10;dd>0;dd--){
			                    	if(imgBuffer1[cc+dd*xDim]==BoneMarrow && imgBuffer1[cc+(dd-1)*xDim]!=BoneMarrow){
			                    		for(i=1;i<dd;i++){
			                    			imgBuffer1[cc+i*xDim]=BoneMarrow;
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
		                //loop for when there IS bone
		                for (cc = 10*xDim; cc < imgBuffer1.length; cc++) {
		                	//gap between 'marrow to bone', fill with bonemarrow
		                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc-1]!=Bone){
		                		for(dd=10;dd>0;dd--){
			                    	if(imgBuffer1[cc-dd]==BoneMarrow && imgBuffer1[cc-dd+1]!=BoneMarrow){
			                    		for(i=1;i<dd;i++){
			                    			imgBuffer1[cc-i]=BoneMarrow;
			                    		}
			                    	}
		                		}
		                	}
		                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc-xDim]!=Bone){
		                		for(dd=10;dd>0;dd--){
			                    	if(imgBuffer1[cc-dd*xDim]==BoneMarrow && imgBuffer1[cc-(dd-1)*xDim]!=BoneMarrow){
			                    		for(i=1;i<dd;i++){
			                    			imgBuffer1[cc-i*xDim]=BoneMarrow;
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
	   	if(thereisBone==false){
        	System.out.println("there is no bone");
        	dd=0;
	        for (bb = 0; bb < zDim; bb++) {
	            try {
	                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
	                	for (cc = (int)(0.1*yDim*xDim); cc < (int)(0.9*yDim*xDim); cc++) {
	                		if(imgBuffer1[cc]==BoneMarrow){
                				if(imgBuffer1[cc-1]!=BoneMarrow && imgBuffer1[cc-2]!=BoneMarrow &&imgBuffer1[cc-3]!=BoneMarrow &&
                						imgBuffer1[cc-4]!=BoneMarrow &&imgBuffer1[cc-5]!=BoneMarrow){
                					imgBuffer1[cc-1]=Bone;
                					imgBuffer1[cc-2]=Bone;
                					imgBuffer1[cc-3]=Bone;
            						imgBuffer1[cc-4]=Bone;
            						imgBuffer1[cc-5]=Bone;
                				}
                				if(imgBuffer1[cc+1]!=BoneMarrow && imgBuffer1[cc+2]!=BoneMarrow &&imgBuffer1[cc+3]!=BoneMarrow &&
                						imgBuffer1[cc+4]!=BoneMarrow &&imgBuffer1[cc+5]!=BoneMarrow){
                					imgBuffer1[cc+1]=Bone;
                					imgBuffer1[cc+2]=Bone;
                					imgBuffer1[cc+3]=Bone;
            						imgBuffer1[cc+4]=Bone;
            						imgBuffer1[cc+5]=Bone;
                				}
                				if(imgBuffer1[cc-1*xDim]!=BoneMarrow && imgBuffer1[cc-2*xDim]!=BoneMarrow &&imgBuffer1[cc-3*xDim]!=BoneMarrow &&
                						imgBuffer1[cc-4*xDim]!=BoneMarrow &&imgBuffer1[cc-5*xDim]!=BoneMarrow){
                					imgBuffer1[cc-1*xDim]=Bone;
                					imgBuffer1[cc-2*xDim]=Bone;
                					imgBuffer1[cc-3*xDim]=Bone;
            						imgBuffer1[cc-4*xDim]=Bone;
            						imgBuffer1[cc-5*xDim]=Bone;
                				}
                				if(imgBuffer1[cc+1*xDim]!=BoneMarrow && imgBuffer1[cc+2*xDim]!=BoneMarrow &&imgBuffer1[cc+3*xDim]!=BoneMarrow &&
                						imgBuffer1[cc+4*xDim]!=BoneMarrow &&imgBuffer1[cc+5*xDim]!=BoneMarrow){
                					imgBuffer1[cc+1*xDim]=Bone;
                					imgBuffer1[cc+2*xDim]=Bone;
                					imgBuffer1[cc+3*xDim]=Bone;
            						imgBuffer1[cc+4*xDim]=Bone;
            						imgBuffer1[cc+5*xDim]=Bone;
                				}
	                		}
	                	}
	                srcImage.importData((bb * imgBuffer1.length), imgBuffer1, false);
	            } catch (IOException ex) {
	                System.err.println(
	                        "error exporting data from destImageA in AlgorithmPipeline2-STEP7");
	            }
	        }
	        //ShowImage(srcImage,"image after no bone loop (loop3)");
	   	}
   }
   
  /**
  *
  * @param   Hard4Classes  DOCUMENT ME!
  *
  * @return  DOCUMENT ME!
  */
 public ModelImage processHardFat(ModelImage Hard4Classes){
	 
	 if (Hard4Classes == null) {
		 System.err.println("problems");
	 } else {
		 System.err.println("not null");
	 }
	 
 	ModelImage fatImage = (ModelImage)Hard4Classes.clone();
//ShowImage(fatImage,"cloned input image to 'processHardFat'");
		convert(fatImage, voiMask, fatImage, 1, Muscle);	//fill voi with muscle
		//ShowImage(fatImage,"voi filled with muscle");
		convert(fatImage, Hard4Classes, fatImage, 189, FAT);		
		convert(fatImage, Hard4Classes, fatImage, 252, FAT);		
		convert(fatImage, voiMask, fatImage, 0, SUB_CUT_FAT);	//all outside voi labeled subcutfat
		convert(fatImage, obMask, fatImage, 0, BACKGROUND_NEW);	//all outside obMask labeled background
				
		return fatImage;
 }  
    
 
 
 
 /**
 *
 * @param   srcImage  DOCUMENT ME!
 *
 * @return  DOCUMENT ME!
 */
 public ModelImage extractedBoneMarrow(ModelImage SegmentedImg){
 	//from 4 class hard fuzzy segmentation
	 ModelImage SegmentedImg1 = (ModelImage)SegmentedImg.clone();
 	ModelImage BMarrow = threshold2(SegmentedImg1, 160f,255f);
 	IDObjects(BMarrow, 1000*zDim/20, 10000*zDim/20);
 	isolatingCenterObject(BMarrow);   	//returns binary image
 	return BMarrow;
 }
 
 
 
    /**
     * DOCUMENT ME!
     *
     * @param   srcImage       DOCUMENT ME!
     *
     */
    public void isolatingCenterObject(ModelImage srcImage){
    	//returns binary image at center object
    	if(srcImage.getMax()>1){
	        int[] numObjects = new int[zDim];
	        int[] BoneObject = new int[zDim];
	    	int bb, cc, i, n, x, y;
	    	float min;
	        for (bb = 0; bb < zDim; bb++) {
	        	numObjects[bb] = 0;
	        	BoneObject[bb] = 999999999;
	            try {
	                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
	                //get numObjects per Slice. ~SLICE maximum.
	                for (cc = 0; cc < imgBuffer1.length; cc++) {
	                    if (imgBuffer1[cc]>numObjects[bb]) {
	                        numObjects[bb] = imgBuffer1[cc];		
	                    }
	                }
	                //initialize centroid variables (for particular slice)
	                float[] centroidX = new float[numObjects[bb]];
	                float[] centroidY = new float[numObjects[bb]];
	                float[] distFromCent = new float[numObjects[bb]];                    
	                for (cc = 0; cc < numObjects[bb]; cc++) {
	                    centroidX[cc] = 0;
	                    centroidY[cc] = 0;
	                }
	                //obtain centroid per slice & obtain distance between center and centroid of each object (per slice)                
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
	                    centroidX[cc-1] = centroidX[cc-1] / n;			//System.out.println("centroidx of object: "+cc+", is: "+centroidX[cc-1]);
	                    centroidY[cc-1] = centroidY[cc-1] / n;			//System.out.println("centroidy of object: "+cc+", is: "+centroidY[cc-1]);
	                    distFromCent[cc-1] = Math.abs((centroidX[cc-1] - (xDim / 2))*(centroidX[cc-1] - (xDim / 2))
	                    		+ (centroidY[cc-1] - (yDim / 2))*(centroidY[cc-1] - (yDim / 2)));
	                }
	                //using centroids to find correct Bone object. object correspondent to minimum distFromCent
	                min = 10000; //beginning distance from center. element with distance smaller than this becomes bone object.
	                for(cc=1;cc<=numObjects[bb];cc++){
	                	if(distFromCent[cc-1]<min && distFromCent[cc-1]<1000){
	                		//less than 1000 to ensure it's close enough to center to be considered bone
	                		BoneObject[bb] = cc;
	                		min=distFromCent[cc-1];
	                	}
	                }
	                //eliminating incorrect Bone objects
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
    
    
    
    

//  **NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED**
//  **NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED**
    /**
     * DOCUMENT ME!
     *
     * @param  destImage  DOCUMENT ME!
     * @param  BoneID       DOCUMENT ME!
     */
    public void fillBoneMarrow(ModelImage destImage, ModelImage BoneID){
    	//artificial 'filling' of bone marrow ~need not be continuous
    	//requres there be labeled bone outside the marrow
        //ShowImage(BoneID, "BoneID BEFORE");
        int bb, cc, dd, i, x, y, xmin, xmax;
    	   for (bb = 1; bb < zDim; bb++) {
    	           try {
    	               destImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
    	               BoneID.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer2);
    	               for (y = 1; y < yDim; y++) {
    	        	       xmax = 0;
    	        	       xmin = 0;
    	                   for (x = 3; x < xDim-3; x++) {
    	                       i = x + y * xDim;
    	                       if (imgBuffer2[i - 3] == 0 && imgBuffer2[i - 2] == 0 && imgBuffer2[i - 1] == 0 
    	                       && imgBuffer2[i] == 1 && imgBuffer2[i + 1] == 1 && imgBuffer2[i + 2] == 1) {	//outside bkgrd to Bone boundary
    	                    	   //System.out.println("x: "+x+", y: "+y);
    	                           for(cc=0;cc<xDim-x-1;cc++){	//go up til edge of image
    	                        	   if(imgBuffer2[i+cc-3]==1 && imgBuffer2[i+cc-2]==1 &&imgBuffer2[i+cc-1]==1 
    	                        	   && imgBuffer2[i+cc]==0 && imgBuffer2[i+cc+1]==0 && imgBuffer2[i+cc+2]==0){	//inner bone to bkgrd boundary
    	                        		   xmin = cc;
    	                        		   xmax = 0;
    	                        		   //System.out.println("xmin: "+xmin);
    	                        		   for(dd=0;dd<xDim-x-cc-1;dd++){
    	                        			   if(imgBuffer2[i+cc+dd-3]==0 &&imgBuffer2[i+cc+dd-2]==0 &&imgBuffer2[i+cc+dd-1]==0 
    	                        			   && imgBuffer2[i+cc+dd]==1 && imgBuffer2[i+cc+dd+1]==1&& imgBuffer2[i+cc+dd+2]==1){	//2nd inner bkgrd to Bone boundary
    	                        				   xmax = cc+dd;
    		                                	   cc=xDim;
    		                                	   dd=xDim;
    		                                	   //System.out.println("xmax: "+xmax);
    		                                   }
    	                        		   }
    	                        	   }
    	                           }
    	                           if(xmin<xmax){
    	                               for(cc=xmin;cc<xmax;cc++){
    	                               	imgBuffer1[i+cc]=BoneMarrow;
    	                               	imgBuffer2[i+cc]=1;
    	                               }
    	                           }
    	                       }
    	                       else{
    	                    	   if(imgBuffer1[i-xDim]==BoneMarrow && imgBuffer1[i]!=Bone){
    	                    		   imgBuffer1[i]=BoneMarrow;
    	                    	   }
    	                       }
    	                       if(imgBuffer2[i]==1 && imgBuffer1[i]!=Bone){
    	                    	   imgBuffer1[i]=BoneMarrow;
    	                       }
    	                   }
    	               }
    	               destImage.importData((bb * imgBuffer1.length),imgBuffer1, false);
    	               BoneID.importData((bb * imgBuffer1.length),imgBuffer2, false);
    	           }
    	           catch (IOException ex) {
    	           System.err.println("error exporting Bone marrow data from destImageA in AlgorithmPipeline2");
    	           }
    	       }
    	   //ShowImage(BoneID, "BoneID AFTER");
        }
//  **NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED**
//  **NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED****NOT USED**
    


    /**
     * -------- BASIC STEPS ---------
     *
     * @param   destImage1  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage makeVOI(ModelImage destImage1) {
        progressBar.setMessage("Taking N3 inside VOI");

        return destImage1.generateBinaryImage(false, false);
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
     */
    //normalizes intensity between slices
    public void ISN(ModelImage srcImage){
        PlugInAlgorithmISN isnAlgo = null;
        isnAlgo = new PlugInAlgorithmISN(srcImage, srcImage);
        isnAlgo.setProgressBarVisible(false);
        isnAlgo.run();

        isnAlgo.finalize();
        isnAlgo = null;        
        }
    

    
    /**
     * DOCUMENT ME!
     *
     * @param   destImage1  DOCUMENT ME!
     *
     */
    public void N3(ModelImage destImage1){
    	ModelImage destImage2 = null;
    	destImage2 = new ModelImage(destImage1.getType(), destImage1.getExtents(),
    			"destImage2", destImage1.getUserInterface());
	    AlgorithmIHN3Correction ihn3Algo1 = null;
	    ihn3Algo1 = new AlgorithmIHN3Correction(destImage2, fieldImage,destImage1,100f, 150, 0.0001f, 33.3f, 4f, 0.2f, 0.01f, false, false, false);
	    ihn3Algo1.setProgressBarVisible(false);
	    ihn3Algo1.run();
	    progressBar.setMessage("Taking Fuzzy-C Means over entire image");
	
	    ihn3Algo1.finalize();ihn3Algo1 = null;	    fieldImage.disposeLocal();fieldImage = null;	    

    }

    
    /**
     * DOCUMENT ME!
     *
     * @param   srcImage  DOCUMENT ME!
     * @param   nClasses  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage HardFuzzy(ModelImage srcImage, int nClasses){

    	float centroid_array[] = new float[nClasses];
    	getCentroid(centroid_array, srcImage, nClasses);
    	
        ModelImage HardSeg[] = new ModelImage[1];
        FileInfoBase fileInfo1;
        HardSeg[0] = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(), "Hard-Fuzzy_seg", srcImage.getUserInterface());
		fileInfo1 = HardSeg[0].getFileInfo()[0];
		fileInfo1.setResolutions(srcImage.getFileInfo()[0].getResolutions());
		fileInfo1.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
		HardSeg[0].setFileInfo(fileInfo1, 0);

        firstFuzz = new AlgorithmFuzzyCMeans(HardSeg, srcImage, nClasses,4, 1, 2, 2.0f, 20000, 200000, false,
        		AlgorithmFuzzyCMeans.HARD_ONLY, false, 0.0f, 200, 0.01f, true);



        firstFuzz.setCentroids(centroid_array);
        firstFuzz.setProgressBarVisible(false);
        firstFuzz.run();
        firstFuzz.finalize();
        firstFuzz = null;
        
        return HardSeg[0];
        }
    

    /**
     * DOCUMENT ME!
     *
     * @param   srcImage  DOCUMENT ME!
     * @param   nClasses  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
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

        firstFuzz = null;        
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
     * DOCUMENT ME!
     *
     * @param  centroid_array  DOCUMENT ME!
     * @param  srcImage        DOCUMENT ME!
     * @param  nClasses        DOCUMENT ME!
     */
	public void getCentroid(float[] centroid_array, ModelImage srcImage, int nClasses){
		int dd;
		double max = srcImage.getMax();
    	for(dd=1;dd<(nClasses+1);dd++){
    		centroid_array[dd-1]=(float)(dd*max/(nClasses+1));
    	}
	}
    
   
    
    /**
     * DOCUMENT ME!
     *
     * @param  mergedImage  DOCUMENT ME!
     * @param  BoneImage    DOCUMENT ME!
     * @param  bundleImage  DOCUMENT ME!
     */
    public void mergeImages(ModelImage mergedImage, ModelImage BoneImage, ModelImage bundleImage){
        convert(bundleImage, BoneImage, bundleImage, Bone, Bone);
        convert(mergedImage, BoneImage, bundleImage, BoneMarrow, BoneMarrow);

        cleanUp(mergedImage, BACKGROUND_NEW, SUB_CUT_FAT, 100*zDim/20);
        cleanUp(mergedImage, SUB_CUT_FAT, BACKGROUND_NEW, 100*zDim/20);
        }
    
    
    
    /**
     * DOCUMENT ME!
     *
     * @param  srcImage        DOCUMENT ME!
     * @param  noiseIntensity  DOCUMENT ME!
     * @param  bkrdIntensity   DOCUMENT ME!
     * @param  sizeNoise       DOCUMENT ME!
     */
    //srcImage with noiseIntensity of size up to sizeNoise converted to bkrdIntensity
    public void cleanUp(ModelImage srcImage, int noiseIntensity, int bkrdIntensity, int sizeNoise) {
    	ModelImage tempImage = null;
        tempImage = threshold1(srcImage, noiseIntensity);
        IDObjects(tempImage, 1, sizeNoise);
        tempImage = threshold2(tempImage, 1f, (float)tempImage.getMax());
        convert(srcImage, tempImage, srcImage, 1, bkrdIntensity);
        tempImage.disposeLocal();
        tempImage = null;
    }
    
    
    /**
     * DOCUMENT ME!
     *
     * @param  destImage      DOCUMENT ME!
     * @param  templateImage   DOCUMENT ME!
     * @param  srcImage       DOCUMENT ME!
     * @param  templateIntensity  DOCUMENT ME!
     * @param  newDestIntensity   DOCUMENT ME!
     */
    //whenever templateImage = tempIntensity, srcImage converted to newIntensity
    public void convert(ModelImage destImage, ModelImage templateImage, ModelImage srcImage, int templateIntensity, int newDestIntensity){
    	int bb, cc;
    	if(srcImage!=null){
    		//System.out.println("srcImage in convert algorithm is NOT null");
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
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
     */
    public void FillHole(ModelImage srcImage) {
        AlgorithmMorphology3D MorphFILL = null;
        MorphFILL = new AlgorithmMorphology3D(srcImage, 4, 2, AlgorithmMorphology3D.FILL_HOLES, 0, 0, 0, 1, true);
        MorphFILL.setProgressBarVisible(false);
        MorphFILL.run();
    }
    
    
    /**
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
     * @param  min        DOCUMENT ME!
     * @param  max        DOCUMENT ME!
     */
    public void IDObjects(ModelImage srcImage, int min, int max) {
        AlgorithmMorphology3D MorphIDObj = null;
        MorphIDObj = new AlgorithmMorphology3D(srcImage, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        MorphIDObj.setMinMax(min, max);
        MorphIDObj.setProgressBarVisible(false);
        MorphIDObj.run();
    }
    
    
    /**
     * DOCUMENT ME!
     *
     * @param  sourceImg  DOCUMENT ME!
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
     * DOCUMENT ME!
     *
     * @param  sourceImg  DOCUMENT ME!
     */
    public void Close(ModelImage sourceImg, int kernalSize) {
    	
        AlgorithmMorphology3D MorphClose = null;
        if(kernalSize ==6){
        MorphClose = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1,
                                              AlgorithmMorphology3D.CLOSE, 1, 1, 0, 0, true);
        }
        if(kernalSize==24){
            MorphClose = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED24, 1,
                    AlgorithmMorphology3D.CLOSE, 1, 1, 0, 0, true);        	
        }
        MorphClose.setProgressBarVisible(false);
        MorphClose.run();
    }

    
    
    
    /**
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
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
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
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
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
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
     * DOCUMENT ME!
     *
     * @param   threshSourceImg  DOCUMENT ME!
     * @param   intensity        DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage threshold1(ModelImage threshSourceImg, float intensity) {
        float[] thresh = { intensity - 5, intensity + 5 };
        ModelImage resultImage = null;
        resultImage = threshold(threshSourceImg, thresh);

        return resultImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   threshSourceImg  DOCUMENT ME!
     * @param   intensity1       DOCUMENT ME!
     * @param   intensity2       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage threshold2(ModelImage threshSourceImg, float intensity1, float intensity2) {
    	float thresh[] = {intensity1, intensity2};    	
    	ModelImage resultImage = null;
    	resultImage = threshold(threshSourceImg, thresh);
        threshSourceImg.disposeLocal();threshSourceImg=null;
        return resultImage;
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param   threshSourceImg  DOCUMENT ME!
     * @param   thresh        DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
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
     * DOCUMENT ME!
     *
     * @param  sourceImg  DOCUMENT ME!
     * @param  Name       DOCUMENT ME!
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
     * @return  DOCUMENT ME!
     */
    public ModelImage getResultImageA() {
        return this.destImageA;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
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

        buildProgressBar("Pipeline_Segmentation", "Operating source images...", 0, 100);
        initProgressBar();

        xDim = 1;        yDim = 1;        zDim = 1;
        
    	//initialize totals to zero if on first thigh, else add on
        int AVGsubcutfatCountTOTAL= 0; 
        int AVGfatCountTOTAL= 0; 
        int AVGMuscleCountTOTAL= 0; 
        int AVGBoneCountTOTAL= 0; 
        int AVGBoneMarrowCountTOTAL= 0;

        for (aa = 1; aa <= 2; aa++) {

            if (aa == 1) {
                getVariables(destImageA, obMaskA);
            } else if (aa == 2) {
                getVariables(destImageB, obMaskB);
            }

            sliceSize = xDim * yDim;
            volSize = xDim * yDim * zDim;
            imgBuffer1 = new int[sliceSize];
            imgBuffer2 = new int[sliceSize];
            destImage2 = (ModelImage)destImage1.clone();
            
            /********************************************************
             **************** general processing ********************
             ********************************************************/            
            
            // STEP 1: VOI to Mask
            progressBar.setMessage("Converting VOI to Mask");
            voiMask = makeVOI(destImage2);
            progressBar.updateValue((50 * (aa - 1)) + 4, activeImage);


            // STEP 2: ISN and N3 inside VOI
            progressBar.setMessage("Taking ISN");
            ISN(destImage2);
            progressBar.updateValue((50 * (aa - 1)) + 9, activeImage);
            //progressBar.setMessage("Taking N3 inside VOI");
            //N3(destImage2);
            //progressBar.updateValue((50 * (aa - 1)) + 30, activeImage);


            //STEP 3: FUZZY CMEANS- WHOLE IMAGE
            progressBar.setMessage("Taking Fuzzy-C over Entire Image");
            HardSeg1 = HardFuzzy(destImage2, 4);
            progressBar.updateValue(50 * (aa - 1) + 18, activeImage);

            
            //STEP 4: BONE PROCESSING
            progressBar.setMessage("Locating Bone");
            destImage3a = processBone(HardSeg1);						//ShowImage(destImage3a,"bone");
            progressBar.updateValue(50 * (aa - 1) + 27, activeImage);

//          STEP 5: BONE_MARROW PROCESSING
            if(continuousBone==false){ //no marrow labeled in step4 processBone -->FillHole
	            progressBar.setMessage("Locating Bone Marrow");
	            processBoneMarrow(destImage3a, HardSeg1);						//ShowImage(destImage3a,"bone");
	            progressBar.updateValue(50 * (aa - 1) + 36, activeImage);
	            //ShowImage(destImage3a,"destImage3a - continuous bone =false");
            }
            
            destImage2.disposeLocal();destImage2 = null;
            
            //STEP 4A:  FAT PROCESSING (inside muscle bundle)
            progressBar.setMessage("Processing bundle fat");
            destImage3b = processHardFat(HardSeg1);
            //ShowImage(destImage3b,"destImage3b");

            //ShowImage(destImage3b,"before final fat filter");
	        cleanUp(destImage3b, FAT, Muscle, 60*zDim/20);        				//ShowImage(destImage3b, "bundle cleanedup fat image");
            //ShowImage(destImage3b,"after final fat filter");
	        //ShowImage(destImage3b,"destImage3b after fat filter");
            progressBar.updateValue(50 * (aa - 1) + 46, activeImage);
            
            voiMask.disposeLocal();voiMask = null;            obMask.disposeLocal();obMask = null;

            
            
            //--------------- STEP5: bringing two pieces together --------------
            progressBar.setMessage("Recreating Result Image");
            mergeImages(destImage3b, destImage3a, destImage3b);			//ShowImage(destImage3b, "after 'merge'");
            progressBar.updateValue(50 * (aa - 1) + 53, activeImage);
            //ShowImage(destImage3b,"destImage3b after image merge");
            destImage3a.disposeLocal();destImage3a = null;
            
            
            //------------------STEP7: counting tissue types -------------------
            //outputData(destImage3b);
            //public void outputData(ModelImage destImage3b){
                
            	int subcutfatCount = 0;
            	int fatCount = 0;
            	int MuscleCount = 0;
            	int BoneCount = 0;
            	int BoneMarrowCount = 0;
            	int total_thighCount = 0;
            
            	int AVGsubcutfatCount= 0; 
            	int AVGfatCount= 0;
            	int AVGMuscleCount= 0;
            	int AVGBoneCount=0;
            	int AVGBoneMarrowCount= 0;
            	
                int a=0;
                int b=0;
                int c=0;
                int d=0;
                int e=0;
            
        	    for (bb = 0; bb < zDim; bb++) {
        	        try {
        	            destImage3b.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
        	            destImage1.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer2);
        	            for (cc = 0; cc < imgBuffer1.length; cc++) {
        	                if (imgBuffer1[cc] == SUB_CUT_FAT) {
        	                	AVGsubcutfatCount+=imgBuffer2[cc];
        	                	a++;
        	                    subcutfatCount++;
        	                    total_thighCount++;
        	                } else if (imgBuffer1[cc] == FAT) {
        	                	AVGfatCount+=imgBuffer2[cc];
        	                	b++;
        	                    fatCount++;
        	                    total_thighCount++;
        	                } else if (imgBuffer1[cc] == Muscle) {
        	                	AVGMuscleCount+=imgBuffer2[cc];
        	                	c++;
        	                    MuscleCount++;
        	                    total_thighCount++;
        	                } else if (imgBuffer1[cc] == Bone) {
        	                	AVGBoneCount+=imgBuffer2[cc];
        	                	d++;
        	                    BoneCount++;
        	                    total_thighCount++;
        	                } else if (imgBuffer1[cc] == BoneMarrow) {
        	                	AVGBoneMarrowCount+=imgBuffer2[cc];
        	                	e++;
        	                    BoneMarrowCount++;
        	                    total_thighCount++;
        	                }
        	            }
        	        } catch (IOException ex) {
        	            System.err.println(
        	                    "error exporting data from srcImage in AlgorithmPipeline2");
        	        }
        	    }
        	    if(a!=0 && b!=0 && c!=0 && d!=0 && e!=0){
        	    AVGsubcutfatCount=AVGsubcutfatCount/a;
        	    AVGfatCount=AVGfatCount/b;
        	    AVGMuscleCount=AVGMuscleCount/c;
        	    AVGBoneCount=AVGBoneCount/d;
        	    AVGBoneMarrowCount=AVGBoneMarrowCount/e;
        	    }
        	    
        	    AVGsubcutfatCountTOTAL+= AVGsubcutfatCount/2; 
        	    AVGfatCountTOTAL+= AVGfatCount/2;
        	    AVGMuscleCountTOTAL+= AVGMuscleCount/2;
        	    AVGBoneCountTOTAL+=AVGBoneCount/2;
        	    AVGBoneMarrowCountTOTAL+= AVGBoneMarrowCount/2;
        	    
        	    float realpixelSize = 1;
        	    float[] res = destImage3b.getFileInfo()[0].getResolutions();
                float pixelSize = res[0] * res[1] * res[2];
                if (aa == 1) {
                    destImageA = (ModelImage) destImage3b.clone();
                    destImageA.calcMinMax();
                    realpixelSize = pixelSize;
                    UI.getMessageFrame().append("Segmented Images - Results:  " +
                                                PlugInAlgorithmPipeline.patientID,
                                                "Right thigh ");
                } else {
                    destImageB = (ModelImage) destImage3b.clone();
                    destImageB.calcMinMax();
                    UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID, "Left thigh ");
                }


                UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID, "Volume: (cubic Millimeters) \n" +
                		"CLASS \t \tVOLUME\tAVG INTENSITY \n" +
                		"SubcutaneousFat \t" + subcutfatCount * realpixelSize + "\t"+AVGsubcutfatCount+"\n"+ 
                		"InterstitialFat \t\t" + fatCount * realpixelSize + "\t"+AVGfatCount+"\n" +
                		"Muscle \t \t" + MuscleCount * realpixelSize + "\t"+AVGMuscleCount+"\n"+
                		"Bone \t \t" + BoneCount * realpixelSize + "\t"+AVGBoneCount+"\n"+
                		"BoneMarrow \t\t" + BoneMarrowCount * realpixelSize + "\t"+AVGBoneMarrowCount+"\n"+
                		"Total Thigh \t\t" + total_thighCount * realpixelSize + "\n\n\n");
              
                if(aa==2){
                	UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID, "SEGMENTATION INTENSITIES\n" +
        	    		"SubcutaneousFat \t" + SUB_CUT_FAT + "\n"+ 
        	    		"InterstitialFat \t\t" + FAT + "\n"+
        	    		"Muscle \t \t" + Muscle + "\n"+
        	    		"Bone \t \t" + Bone + "\n"+
        	    		"BoneMarrow \t\t" + BoneMarrow + "\n\n\n"+
        	    		"TOTAL THIGH DATA\n" +
        	    		"CLASS \t \tAVG INTENSITY \n" +
        	    		"SubcutaneousFat\t"+AVGsubcutfatCountTOTAL+ "\n"+
        	    		"InterstitialFat\t\t"+AVGfatCountTOTAL+ "\n"+
        	    		"Muscle\t\t"+AVGMuscleCountTOTAL+ "\n"+
        	    		"Bone\t\t"+AVGBoneCountTOTAL+ "\n"+
        	    		"BoneMarrow\t\t"+AVGBoneMarrowCountTOTAL);
                	}

                destImage3b.disposeLocal();
                destImage3b = null;
                progressBar.updateValue(50*aa, activeImage);
            }
        System.out.println("thigh cleanup done --destImage");
        setCompleted(true);
        disposeProgressBar();
            
        }

}
