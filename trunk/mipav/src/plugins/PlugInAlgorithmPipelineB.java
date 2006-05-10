import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;
import java.io.*;

import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.model.algorithms.AlgorithmFuzzyCMeans;
import gov.nih.mipav.model.algorithms.AlgorithmIHN3Correction;
import gov.nih.mipav.model.file.FileInfoBase;


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
    private ModelImage[] FuzzySeg2 = null;

    /** DOCUMENT ME! */
    private ModelImage HardSeg = null;
    
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
    *
    * @param   srcImage  DOCUMENT ME!
    *
    * @return  DOCUMENT ME!
    */
    public ModelImage extractedBoneMarrow(ModelImage srcImage){
    	ModelImage BMarrow = (ModelImage)srcImage.clone();	
    	
    	ShowImage(BMarrow, "before extractBoneMarrow");
     	BMarrow = threshold2(BMarrow, 160f,255f);
    	ShowImage(BMarrow,"after 160-255 thresh");
    	IDObjects(BMarrow, 1000*zDim/20, 10000*zDim/20);
    	if(BMarrow ==null){
	     	Open6(BMarrow);Close24(BMarrow);
	     	ShowImage(BMarrow,"opened and closed");
	     	IDObjects(BMarrow, 1000*zDim/20, 10000*zDim/20);
    	}
    	ShowImage(BMarrow,"objects marrow-size singled out");
    	isolatingCenterObject(BMarrow);
    	ShowImage(BMarrow,"central object isolated");
    	    	
    	return BMarrow;
    }
    
    /**
    *
    * @param   fatImage0  DOCUMENT ME!
    * @param   fatImage1  DOCUMENT ME!
    * @param   fatImage2  DOCUMENT ME!
    *
    * @return  DOCUMENT ME!
    */
   //uses soft fuzzy segmentation images 0 and 2, to label interstitial fat
   //voiMask obMask to label remaining muscle, subcutaneous fat, and background
   public ModelImage processSoftFat(ModelImage fatImage0, ModelImage fatImage1, ModelImage fatImage2){
   	ModelImage fatImage = new ModelImage(fatImage1.getType(), fatImage1.getExtents(),
   			"fatImage", fatImage1.getUserInterface());
   	float imgBuffer[] = new float[sliceSize];
   	int bb, cc;

		for (bb = 0; bb < zDim; bb++) {
		    try {
		    	fatImage2.exportData((bb * imgBuffer.length), imgBuffer.length, imgBuffer);
		        for (cc = 0; cc < imgBuffer.length; cc++) {
		        	imgBuffer[cc]=(float)(imgBuffer[cc]*0.7);
		        }
		        fatImage2.importData((bb * imgBuffer.length), imgBuffer, false);
		    } catch (IOException ex) {
		        System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP5 black to Muscle");
		    }
		}
		//takes soft fuzzy segmentations --image0 0.5-1, image2 0.2-1 (image1 ignored)
		fatImage0 = threshold2(fatImage0, (float)(0.5*fatImage2.getMax()), (float)fatImage0.getMax());
		fatImage2 = threshold2(fatImage2, (float)(0.2*fatImage2.getMax()), (float)fatImage2.getMax());

		convert(fatImage, voiMask, fatImage, 1, Muscle);	//fill voi with muscle
		convert(fatImage, fatImage0, fatImage, 1, FAT);		//fat0 all labeled as fat
		convert(fatImage, fatImage2, fatImage, 1, FAT);		//fat2 all labeled as fat
		convert(fatImage, voiMask, fatImage, 0, SUB_CUT_FAT);	//all outside voi labeled subcutfat
		convert(fatImage, obMask, fatImage, 0, BACKGROUND_NEW);	//all outside obMask labeled background

		fatImage0.disposeLocal();		fatImage0=null;
		fatImage1.disposeLocal();		fatImage1=null;
		fatImage2.disposeLocal();		fatImage2=null;
				
		return fatImage;

   }
   
   
   /**
   *
   * @param   fatImage0  DOCUMENT ME!
   * @param   fatImage1  DOCUMENT ME!
   * @param   fatImage2  DOCUMENT ME!
   *
   * @return  DOCUMENT ME!
   */
  //uses soft fuzzy segmentation images 0 and 2, to label interstitial fat
  //voiMask obMask to label remaining muscle, subcutaneous fat, and background
  public ModelImage processHardFat(ModelImage Hard4Classes){
  	ModelImage fatImage = new ModelImage(Hard4Classes.getType(), Hard4Classes.getExtents(),
  			"fatImage", Hard4Classes.getUserInterface());

		convert(fatImage, voiMask, fatImage, 1, Muscle);	//fill voi with muscle
		convert(fatImage, Hard4Classes, fatImage, 189, FAT);		
		convert(fatImage, Hard4Classes, fatImage, 252, FAT);		
		convert(fatImage, voiMask, fatImage, 0, SUB_CUT_FAT);	//all outside voi labeled subcutfat
		convert(fatImage, obMask, fatImage, 0, BACKGROUND_NEW);	//all outside obMask labeled background
				
		return fatImage;

  }
   
   /**
    * DOCUMENT ME!
    *
    * @param  HardSeg  DOCUMENT ME!
    */
   // everything outside voi labeled background, isolates bone, labels as bone
   public ModelImage processBone(ModelImage HardSeg){
	    ModelImage destImage3a = new ModelImage(HardSeg.getType(), HardSeg.getExtents(), "destImage3",
				 HardSeg.getUserInterface());
		convert(destImage3a, voiMask, HardSeg, 0, BACKGROUND_NEW);
		
		progressBar.setMessage("Isolating/Labeling Bone");
		ModelImage BoneID = threshold1(destImage3a, BACKGROUND);  
		Open6(BoneID);
		Close24(BoneID);
		IDObjects(BoneID, zDim*5000/20, zDim*20000/20);  //should be on the order or 10,000
		isolatingCenterObject(BoneID); //doesn't seem to be working
	    convert(destImage3a, BoneID, destImage3a, 1, Bone);
	    //
	    //progressBar.setMessage("Labeling Bone Marrow");
	    //fillBoneMarrow(destImage3a, tempImage);
	    //cleanUp(destImage3a, BoneMarrow, Bone, zDim*1000/20);  //bonemarrow size up to 100 turn into bone -bonecleanup
	    //progressBar.updateValue(50 * (aa - 1) + 44, activeImage);
	    //
	    progressBar.updateValue(50 * (aa - 1) + 43, activeImage);   
	   
	    BoneID.disposeLocal();	BoneID = null;
	    return destImage3a;
   }

    /**
     * DOCUMENT ME!
     *
     * @param   srcImage       DOCUMENT ME!
     * @param   BoneIntensity  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage isolatingBone(ModelImage srcImage, int BoneIntensity){
    	//thresholds bone, isolates, breaks into objects, isolates CENTER object.
        ModelImage BoneID = threshold1(srcImage, BoneIntensity);            
        //
        //IDObjects(BoneID, zDim*3000/20, zDim*30000/20);//ShowImage(BoneID, "IDObjects"); 
        //*DELETED ASOF THIGH IMM 2BG*
        //
        Open6(BoneID);
        Close24(BoneID);
        IDObjects(BoneID, zDim*5000/20, zDim*20000/20);  //should be on the order or 10,000
        isolatingCenterObject(BoneID); //doesn't seem to be working
        return BoneID;
    }  
    
    
    
    /**
     * DOCUMENT ME!
     *
     * @param   srcImage       DOCUMENT ME!
     *
     */
    public void isolatingCenterObject(ModelImage srcImage){
    	if(srcImage.getMax()>1){
	        int[] numObjects = new int[zDim];
	        int[] BoneObject = new int[zDim];
	    	int bb, cc, i, n, x, y;
	    	float min;
	        for (bb = 0; bb < zDim; bb++) {
	        	numObjects[bb] = 0;
	        	BoneObject[bb] = 1;
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
								   //System.out.println("centroidX: "+centroidX[cc-1]+", centroidY: "+centroidY[cc-1]);
								   //System.out.println("x: "+x+", y: "+y);
	                            }
	                        }
	                    }
	                    //System.out.println("n: "+n);
	                    centroidX[cc-1] = centroidX[cc-1] / n;			System.out.println("centroidx of object: "+cc+", is: "+centroidX[cc-1]);
	                    centroidY[cc-1] = centroidY[cc-1] / n;			System.out.println("centroidy of object: "+cc+", is: "+centroidY[cc-1]);
	                    distFromCent[cc-1] = Math.abs((centroidX[cc-1] - (xDim / 2))*(centroidX[cc-1] - (xDim / 2))
	                    		+ (centroidY[cc-1] - (yDim / 2))*(centroidY[cc-1] - (yDim / 2)));
	                    System.out.println("distFromCent of object: "+cc+", is: "+distFromCent[cc-1]);
	                }
	                //using centroids to find correct Bone object. object correspondent to minimum distFromCent
	                min = 10000; //beginning distance from center. element with distance smaller than this becomes bone object.
	                for(cc=1;cc<=numObjects[bb];cc++){
	                	if(distFromCent[cc-1]<min){
	                		BoneObject[bb] = cc;
	                		min=distFromCent[cc-1];
	                	}
	                }
	                //System.out.println("central object: "+BoneObject[bb]);
	                //eliminating incorrect Bone objects
	                for(i = 0; i < imgBuffer1.length; i++){
	                        if (imgBuffer1[i] == BoneObject[bb]) {
	                            imgBuffer1[i] = 1;
	                        }
	                        else{
	                        	imgBuffer1[i]=0;
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
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
     */
    //fills gaps between bone and bone marrow when there's bone
    //when there's no bone, artificial bone created around marrow
    public void marrowCleanup(ModelImage srcImage){
    	int bb, cc, dd, i;
    	boolean thereisbone = false;
    	
        for (bb = 0; bb < zDim; bb++) {
            try {
                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
                for (cc = (int)(0.1*yDim*xDim); cc < (int)(0.6*yDim*xDim); cc++) {
                	if(imgBuffer1[cc]==Bone){
                		thereisbone = true;
                	}
                }
            } catch (IOException ex) {
                System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP7");
            }
        }       
    	//ShowImage(srcImage, "before any loop");
    	if(thereisbone==true){
	        for (bb = 0; bb < zDim; bb++) {
	            try {
	                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
	                //loop for when there IS bone
	                for (cc = (int)(0.1*yDim*xDim); cc < (int)(0.6*yDim*xDim); cc++) {
	                	//gap between 'bone to marrow', fill with bonemarrow
	                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc+1]!=Bone){
	                		for(dd=5;dd>0;dd--){
		                    	if(imgBuffer1[cc+dd]==BoneMarrow && imgBuffer1[cc+dd-1]!=BoneMarrow){
		                    		for(i=1;i<dd;i++){
		                    			imgBuffer1[cc+i]=BoneMarrow;
		                    		}
		                    	}
	                		}
	                	}
	                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc+xDim]!=Bone){
	                		for(dd=5;dd>0;dd--){
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
	                for (cc = (int)(0.1*yDim*xDim); cc < (int)(0.6*yDim*xDim); cc++) {
	                	//gap between 'marrow to bone', fill with bonemarrow
	                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc-1]!=Bone){
	                		for(dd=5;dd>0;dd--){
		                    	if(imgBuffer1[cc-dd]==BoneMarrow && imgBuffer1[cc-dd+1]!=BoneMarrow){
		                    		for(i=1;i<dd;i++){
		                    			imgBuffer1[cc-i]=BoneMarrow;
		                    		}
		                    	}
	                		}
	                	}
	                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc-xDim]!=Bone){
	                		for(dd=5;dd>0;dd--){
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
    	if(thereisbone==false){
        	System.out.println("there is no bone");
        	dd=0;
	        for (bb = 0; bb < zDim; bb++) {
	            try {
	                srcImage.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
	                	for (cc = (int)(0.1*yDim*xDim); cc < (int)(0.6*yDim*xDim); cc++) {
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
    	}
    }

    /**
     * DOCUMENT ME!
     *
     * @param  destImage  DOCUMENT ME!
     * @param  BoneID       DOCUMENT ME!
     */
    public void fillBoneMarrow(ModelImage destImage, ModelImage BoneID){
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
        IDObjects(tempImage, 0, sizeNoise);
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
    public void Open6(ModelImage sourceImg) {
        AlgorithmMorphology3D MorphOpen = null;
        MorphOpen = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1,
                                              AlgorithmMorphology3D.OPEN, 1, 1, 0, 0, true);
        MorphOpen.setProgressBarVisible(false);
        MorphOpen.run();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
     */
    public void Close24(ModelImage srcImage) {
        AlgorithmMorphology3D MorphClose = null;
        MorphClose = new AlgorithmMorphology3D(srcImage, AlgorithmMorphology3D.CONNECTED24, 0.0f,
                                               AlgorithmMorphology3D.CLOSE, 1, 1, 0, 1, true);
        MorphClose.setProgressBarVisible(false);
        MorphClose.run();
    }
    
    
    
    /**
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
     */
    public void Dilate6(ModelImage srcImage) {
        AlgorithmMorphology3D MorphErode = null;
        MorphErode = new AlgorithmMorphology3D(srcImage, AlgorithmMorphology3D.CONNECTED6, 1,
                                               AlgorithmMorphology3D.DILATE, 1, 0, 0, 0, true);
        MorphErode.setProgressBarVisible(false);
        MorphErode.run();
    }
    

    /**
     * DOCUMENT ME!
     *
     * @param  srcImage  DOCUMENT ME!
     */
    public void Erode6(ModelImage srcImage) {
        AlgorithmMorphology3D MorphErode = null;
        MorphErode = new AlgorithmMorphology3D(srcImage, AlgorithmMorphology3D.CONNECTED6, 1,
                                               AlgorithmMorphology3D.ERODE, 0, 1, 0, 0, true);
        MorphErode.setProgressBarVisible(false);
        MorphErode.run();
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
            
            progressBar.updateValue((50 * (aa - 1)) + 2, activeImage);

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
            progressBar.setMessage("Taking N3 inside VOI");
            N3(destImage2);
            progressBar.updateValue((50 * (aa - 1)) + 30, activeImage);


            //STEP 3: FUZZY CMEANS- WHOLE IMAGE
            progressBar.setMessage("Taking Fuzzy-C over Entire Image");
            HardSeg = HardFuzzy(destImage2, 3);            						//ShowImage(HardSeg,"hardseg1");
            HardSeg1 = HardFuzzy(destImage2, 4);
            progressBar.updateValue(50 * (aa - 1) + 33, activeImage);

            
            //STEP 3a: BONE PROCESSING
            destImage3a = processBone(HardSeg);						//ShowImage(destImage3a,"bone");

            //STEP 4: FUZZY CMEANS- INSIDE BUNDLE
            progressBar.setMessage("Taking Fuzzy-C inside Muscle Bundle");		//convert(destImage2, voiMask, destImage2, 0, 0);  //crop
            //FuzzySeg2 = SoftFuzzy(destImage2, 3);				            	//ShowImage(FuzzySeg2[2], "FuzzySeg2["+2+"]");
            progressBar.updateValue(50 * (aa - 1) + 37, activeImage); 

            
            destImage2.disposeLocal();destImage2 = null;
            
            //STEP 4A:  FAT PROCESSING (inside muscle bundle)
    //        BMarrow = extractedBoneMarrow(FuzzySeg2[1]);						
            BMarrow = extractedBoneMarrow(HardSeg);
            progressBar.setMessage("marrow extracted,intensity will now be converted");
            convert(destImage3a, BMarrow, destImage3a, 1, BoneMarrow);
            progressBar.setMessage("Processing bundle fat");
            
    //        destImage3b = processSoftFat(FuzzySeg2[0],FuzzySeg2[1],FuzzySeg2[2]);  	//ShowImage(destImage3b, "processed fat image");
            destImage3b = processHardFat(HardSeg1);
	        cleanUp(destImage3b, FAT, Muscle, 300*zDim/20);        				//ShowImage(destImage3b, "bundle cleanedup fat image");     
            progressBar.updateValue(50 * (aa - 1) + 46, activeImage);
            
            voiMask.disposeLocal();voiMask = null;            obMask.disposeLocal();obMask = null;
            BMarrow.disposeLocal();BMarrow = null;
            
            
            //--------------- STEP5: bringing two pieces together --------------
            progressBar.setMessage("Recreating Result Image");
            mergeImages(destImage3b, destImage3a, destImage3b);			//ShowImage(destImage3b, "after 'merge'");
            progressBar.updateValue(50 * (aa - 1) + 47, activeImage);
            
            destImage3a.disposeLocal();destImage3a = null;
            
            
            //-------------- STEP6: last marrow cleanup ------------------------
            progressBar.setMessage("Tissue Type Counts/Volumes");
            marrowCleanup(destImage3b);											
            //ShowImage(destImage3b, "after 'marrowCleanup'");
            progressBar.updateValue(50 * (aa - 1) + 49, activeImage);
            
            
            
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
