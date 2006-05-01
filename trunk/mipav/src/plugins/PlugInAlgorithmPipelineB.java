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
    private ModelImage BoneID = null;
    
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
    private ModelImage tempImageOne = null;

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
     * @param  destImage3a  DOCUMENT ME!
     */
    public void BoneMarrowCleanup(ModelImage destImage3a){
        tempImageOne = threshold(destImage3a, BoneMarrow);
        IDObjects(tempImageOne, 1, zDim*1000/20);
        convert(destImage3a, tempImageOne, destImage3a, 1, Bone);
        tempImageOne.disposeLocal();            tempImageOne = null;
    }

    
    /**
     * DOCUMENT ME!
     *
     * @param  destImage3b  DOCUMENT ME!
     */
    public void marrowCleanup(ModelImage destImage3b){
    	int bb, cc, dd, x, y, i;
    	boolean thereisbone = false;
    	boolean marrowColorPicked = false;
        for (bb = 0; bb < zDim; bb++) {
            try {
                destImage3b.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
                //loop for when there IS bone
                for (cc = (int)(0.2*yDim*xDim); cc < imgBuffer1.length-(int)(0.5*yDim*xDim); cc++) {
                	//gap between 'bone to marrow', fill with bonemarrow
                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc+1]!=Bone){
                		for(dd=5;dd>0;dd--){
	                    	if(imgBuffer1[cc+dd]==BoneMarrow){
	                    		for(i=0;i<dd;i++){
	                    			imgBuffer1[cc+i]=BoneMarrow;
	                    		}
	                    	}
                		}
                	}
                	//gap between 'marrow to bone', fill with bonemarrow
                	if(imgBuffer1[cc]==Bone && imgBuffer1[cc-1]!=Bone){
                		for(dd=5;dd>0;dd--){
	                    	if(imgBuffer1[cc-dd]==BoneMarrow){
	                    		for(i=0;i<dd;i++){
	                    			imgBuffer1[cc-i]=BoneMarrow;
	                    		}
	                    	}
                		}
                	}
                	
                	//
                	//if no 'bone', look for 'bonemarrow'
                	//
                }
                //loop for when there's marrow but no bone
                for (cc = (int)(0.2*yDim*xDim); cc < imgBuffer1.length-(int)(0.5*yDim*xDim); cc++) {
                	if(imgBuffer1[cc]==Bone){
                		thereisbone = true;
                	}
                }
                if(thereisbone==false){
                	dd=0;
	                for (cc = (int)(0.2*yDim*xDim); cc < imgBuffer1.length-(int)(0.5*yDim*xDim); cc++) {
	                	if(imgBuffer1[cc]==BoneMarrow && imgBuffer1[cc+1]!=BoneMarrow){
	                		dd = imgBuffer1[cc+1];  //all pixels within 2 radius of marrow will be changed to pixel color dd
	                		marrowColorPicked = true;
	                	}
	                }
	                if(marrowColorPicked==true){
	                	if(imgBuffer1[cc]==BoneMarrow){
	                		for(x=-2;x<=2;x++){
	                			for(y=-2;y<=2;y++){
	                				i=x+y*xDim;
	                				if(imgBuffer1[cc+i]==dd){
	                					imgBuffer1[cc+i]=BoneMarrow;
	                				}
	                				else{
	                					imgBuffer1[cc+i]=Bone;
	                				}
	                			}
	                		}
	                	}
	                }
                }
                
                
                /*
                    if (imgBuffer1[cc] == BoneMarrow){ 
                    	for(dd=5;dd>0;dd--){
	                    	if(imgBuffer1[cc+dd]==Bone && imgBuffer1[cc+dd-1]!=Bone){
	                    		for(i=0;i<dd;i++){
	                    			imgBuffer1[cc+i]=BoneMarrow;
	                    		}
	                    	}
	                    	if(imgBuffer1[cc-dd]==Bone && imgBuffer1[cc-(dd-1)]!=Bone){
	                    		for(i=0;i<dd;i++){
	                    			imgBuffer1[cc-i]=BoneMarrow;
	                    		}
	                    	}
	                    	if(imgBuffer1[cc+dd*xDim]==Bone && imgBuffer1[cc+(dd-1)*xDim]!=Bone){
	                    		for(i=0;i<dd;i++){
	                    			imgBuffer1[cc+i*xDim]=BoneMarrow;
	                    		}
	                    	}
	                    	if(imgBuffer1[cc-dd*xDim]==Bone && imgBuffer1[cc-(dd-1)*xDim]!=Bone){
	                    		for(i=0;i<dd;i++){
	                    			imgBuffer1[cc-i*xDim]=BoneMarrow;
	                    		}
	                    	}
                    	}
                    	//in case no bone surrounds bone marrow
                    	// 1. eliminate noise
                    	for(x=-5;x<=5;x++){
                    		for(y=-5;y<=5;y++){
                    			i=x+y*xDim;
                    			if(imgBuffer1[cc+i]==Muscle && imgBuffer1[cc+i+1]!=Muscle && imgBuffer1[cc+i-1]!=Muscle
                    					&& imgBuffer1[cc+i+xDim]!=Muscle && imgBuffer1[cc+i-xDim]!=Muscle){
                    				imgBuffer1[cc+i]=imgBuffer1[cc+i-1];
                    			}
                    		}
                    	}
                    	// 2. extend bone marrow out to all 'fat' ~region growing algorithm (my adapatation)
                    	for(dd=3;dd>0;dd--){
	                    	if(imgBuffer1[cc+dd]==FAT && imgBuffer1[cc+dd-1]!=Bone){
	                    		for(i=0;i<dd;i++){
	                    			imgBuffer1[cc+i]=BoneMarrow;
	                    		}
	                    	}
                    	}
                    }
                    //grow out 'bone'
	                if (imgBuffer1[cc] == Bone){
	                    for(x=-2;x<=2;x++){
	                    	for(y=-2;y<=2;y++){
	                    		i = x+y*xDim;
		                    	if(imgBuffer1[cc+i]==FAT){
		                    		imgBuffer1[cc+i]=Bone;
		                    	}
	                    	}
	                    }
	                }
                }
                */
                destImage3b.importData((bb * imgBuffer1.length), imgBuffer1, false);
            } catch (IOException ex) {
                System.err.println(
                        "error exporting data from destImageA in AlgorithmPipeline2-STEP7");
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  srcImage        DOCUMENT ME!
     * @param  noiseIntensity  DOCUMENT ME!
     * @param  bkrdIntensity   DOCUMENT ME!
     * @param  sizeNoise       DOCUMENT ME!
     */
    public void cleanUp(ModelImage srcImage, int noiseIntensity, int bkrdIntensity, int sizeNoise) {
        tempImageOne = threshold(srcImage, noiseIntensity);
        IDObjects(tempImageOne, 0, sizeNoise);
        convert(srcImage, tempImageOne, srcImage, 1, bkrdIntensity);
        tempImageOne.disposeLocal();
        tempImageOne = null;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  sourceImg  DOCUMENT ME!
     */
    public void Close24(ModelImage sourceImg) {
        AlgorithmMorphology3D MorphClose = null;
        MorphClose = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED24, 0.0f,
                                               AlgorithmMorphology3D.CLOSE, 1, 1, 0, 1, true);
        MorphClose.setProgressBarVisible(false);
        MorphClose.run();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  destImage      DOCUMENT ME!
     * @param  tempImageOne   DOCUMENT ME!
     * @param  srcImage       DOCUMENT ME!
     * @param  tempIntensity  DOCUMENT ME!
     * @param  newIntensity   DOCUMENT ME!
     */
    public void convert(ModelImage destImage, ModelImage tempImageOne, ModelImage srcImage, int tempIntensity, int newIntensity){
    	//whenever tempImageOne = tempIntensity, srcImage converted to newIntensity
    	int bb, cc;
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
		        System.err.println("error exporting data from destImageA in AlgorithmPipeline2-STEP5 black to Muscle");
		    }
		}
    }
    
    
    
    /**
     * DOCUMENT ME!
     *
     * @param  sourceImg  DOCUMENT ME!
     */
    public void Dilate6(ModelImage sourceImg) {
        AlgorithmMorphology3D MorphErode = null;
        MorphErode = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1,
                                               AlgorithmMorphology3D.DILATE, 1, 0, 0, 0, true);
        MorphErode.setProgressBarVisible(false);
        MorphErode.run();
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
     *
     * @param  sourceImg  DOCUMENT ME!
     */
    public void Erode6(ModelImage sourceImg) {
        AlgorithmMorphology3D MorphErode = null;
        MorphErode = new AlgorithmMorphology3D(sourceImg, AlgorithmMorphology3D.CONNECTED6, 1,
                                               AlgorithmMorphology3D.ERODE, 0, 1, 0, 0, true);
        MorphErode.setProgressBarVisible(false);
        MorphErode.run();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  destImage3a  DOCUMENT ME!
     * @param  BoneID       DOCUMENT ME!
     */
    public void fillBoneMarrow(ModelImage destImage3a, ModelImage BoneID){
        ShowImage(BoneID, "BoneID BEFORE");
        int bb, cc, dd, i, x, y, xmin, xmax;
    	   for (bb = 1; bb < zDim; bb++) {
    	           try {
    	               destImage3a.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
    	               BoneID.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer2);
    	               for (y = 1; y < yDim; y++) {
    	        	       xmax = 0;
    	        	       xmin = 0;
    	                   for (x = 3; x < xDim-3; x++) {
    	                       i = x + y * xDim;
    	                       if (imgBuffer2[i - 3] == 0 && imgBuffer2[i - 2] == 0 && imgBuffer2[i - 1] == 0 
    	                       && imgBuffer2[i] == 1 && imgBuffer2[i + 1] == 1 && imgBuffer2[i + 2] == 1) {	//outside bkgrd to Bone boundary
    	                    	   System.out.println("x: "+x+", y: "+y);
    	                           for(cc=0;cc<xDim-x-1;cc++){	//go up til edge of image
    	                        	   if(imgBuffer2[i+cc-3]==1 && imgBuffer2[i+cc-2]==1 &&imgBuffer2[i+cc-1]==1 
    	                        	   && imgBuffer2[i+cc]==0 && imgBuffer2[i+cc+1]==0 && imgBuffer2[i+cc+2]==0){	//inner bone to bkgrd boundary
    	                        		   xmin = cc;
    	                        		   xmax = 0;
    	                        		   System.out.println("xmin: "+xmin);
    	                        		   for(dd=0;dd<xDim-x-cc-1;dd++){
    	                        			   if(imgBuffer2[i+cc+dd-3]==0 &&imgBuffer2[i+cc+dd-2]==0 &&imgBuffer2[i+cc+dd-1]==0 
    	                        			   && imgBuffer2[i+cc+dd]==1 && imgBuffer2[i+cc+dd+1]==1&& imgBuffer2[i+cc+dd+2]==1){	//2nd inner bkgrd to Bone boundary
    	                        				   xmax = cc+dd;
    		                                	   cc=xDim;
    		                                	   dd=xDim;
    		                                	   System.out.println("xmax: "+xmax);
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
    	               destImage3a.importData((bb * imgBuffer1.length),imgBuffer1, false);
    	               BoneID.importData((bb * imgBuffer1.length),imgBuffer2, false);
    	           }
    	           catch (IOException ex) {
    	           System.err.println("error exporting Bone marrow data from destImageA in AlgorithmPipeline2");
    	           }
    	       }
    	   ShowImage(BoneID, "BoneID AFTER");
        }
    
    
    /**
     * DOCUMENT ME!
     *
     * @param  sourceImg  DOCUMENT ME!
     */
    public void FillHole(ModelImage sourceImg) {
        AlgorithmMorphology3D MorphFILL = null;
        MorphFILL = new AlgorithmMorphology3D(sourceImg, 4, 2, AlgorithmMorphology3D.FILL_HOLES, 0, 0, 0, 1, true);
        MorphFILL.setProgressBarVisible(false);
        MorphFILL.run();
    }

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
     * @param  HardSeg  DOCUMENT ME!
     */
    public ModelImage processBone(ModelImage HardSeg){    	
    	progressBar.setMessage("Isolating Bone");
        convert(destImage3a, voiMask, HardSeg, 0, BACKGROUND_NEW);
        BoneID = isolatingBone(destImage3a, BACKGROUND);         
        progressBar.updateValue(50 * (aa - 1) + 41, activeImage);
        
        progressBar.setMessage("Labeling Bone");
        convert(destImage3a, BoneID, destImage3a, 1, Bone);	
        progressBar.updateValue(50 * (aa - 1) + 43, activeImage);   
 
        /*
        progressBar.setMessage("Labeling Bone Marrow");		        fillBoneMarrow(destImage3a, BoneID);								
        															BoneMarrowCleanup(destImage3a);
        progressBar.updateValue(50 * (aa - 1) + 44, activeImage);
        */
        
        BoneID.disposeLocal();	BoneID = null;
        return destImage3a;
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
     * @param  sourceImg  DOCUMENT ME!
     * @param  min        DOCUMENT ME!
     * @param  max        DOCUMENT ME!
     */
    public void IDObjects(ModelImage sourceImg, int min, int max) {
        AlgorithmMorphology3D MorphIDObj = null;
        MorphIDObj = new AlgorithmMorphology3D(sourceImg, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        MorphIDObj.setMinMax(min, max);
        MorphIDObj.setProgressBarVisible(false);
        MorphIDObj.run();
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
        int[] numObjects = new int[zDim];
        int[] BoneObject = new int[zDim];
    	int n, x, y;
    	
        BoneID = threshold(srcImage, BoneIntensity);            ShowImage(BoneID, "thresholded for Bone");



        
        IDObjects(BoneID, zDim*1000/20, zDim*30000/20);
        Open6(BoneID);

        Close24(BoneID);
        IDObjects(BoneID, zDim*1000/20, zDim*30000/20);        ShowImage(BoneID, "thresholded IDobjected opened closed idobjected --");

        n = 1;
        for (bb = 0; bb < zDim; bb++) {
        	numObjects[bb] = 0;
        	BoneObject[bb] = 1;
            try {
                BoneID.exportData((bb * imgBuffer1.length), imgBuffer1.length, imgBuffer1);
                //get numObjects per Slice
                for (cc = 0; cc < imgBuffer1.length; cc++) {
                    if (imgBuffer1[cc]>numObjects[bb]) {
                        numObjects[bb] = imgBuffer1[cc];		//numObjects contains slice maximum.
                    }
                }                
                //initialize centroid variables (for particular slice)
                int[] centroidX = new int[numObjects[bb]];
                int[] centroidY = new int[numObjects[bb]];
                float[] distFromCent = new float[numObjects[bb]];                    
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
                //using centroids to find correct Bone object. object correspondent to minimum distFromCent
                for (cc = 0; cc < numObjects[bb]; cc++) {
                	if(BoneObject[bb]>1){
                        if (distFromCent[cc] < distFromCent[BoneObject[bb]]) {
                               BoneObject[bb] = cc+1;
                               //System.out.println("BoneObject["+bb+"]: "+ BoneObject[bb]);
                        }
                	}
                }
                //eliminating incorrect Bone objects
                for (x = 0; x < xDim; x++) {
                    for (y = 0; y < yDim; y++) {
                        i = x + y * xDim;
                        if (imgBuffer1[i] == BoneObject[bb]) {
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
     *
     * @param   fatImage0  DOCUMENT ME!
     * @param   fatImage1  DOCUMENT ME!
     * @param   fatImage2  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage processFat(ModelImage fatImage0, ModelImage fatImage1, ModelImage fatImage2){
    	ModelImage fatImage = null;
    	fatImage = new ModelImage(fatImage1.getType(), fatImage1.getExtents(),
    			"fatImage", fatImage1.getUserInterface());
    	float imgBuffer[] = null;
    	imgBuffer = new float[sliceSize];
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
		fatImage0 = threshold1(fatImage0, (float)(0.5*fatImage2.getMax()), (float)fatImage0.getMax());
		fatImage2 = threshold1(fatImage2, (float)(0.2*fatImage2.getMax()), (float)fatImage2.getMax());

		convert(fatImage, voiMask, fatImage, 1, Muscle);
		convert(fatImage, fatImage0, fatImage, 1, FAT);
		convert(fatImage, fatImage2, fatImage, 1, FAT);
		convert(fatImage, voiMask, fatImage, 0, SUB_CUT_FAT);
		convert(fatImage, obMask, fatImage, 0, BACKGROUND_NEW);


		fatImage0.disposeLocal();		fatImage0=null;
		fatImage1.disposeLocal();		fatImage1=null;
		fatImage2.disposeLocal();		fatImage2=null;
				
		return fatImage;

    }
    
    
    /**
    *
    * @param   srcImage  DOCUMENT ME!
    *
    * @return  DOCUMENT ME!
    */
    public ModelImage extractedBoneMarrow(ModelImage srcImage){
    	ModelImage BMarrow = (ModelImage)srcImage.clone();
    	BMarrow = threshold1(BMarrow, (float)(0.5*BMarrow.getMax()), (float)BMarrow.getMax());
    	IDObjects(BMarrow, 1000*zDim/20, 10000*zDim/20);
    	
    	return BMarrow;
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


            // STEP 2: N3 inside VOI
            progressBar.setMessage("Taking N3 inside VOI");
            N3(destImage2);
            progressBar.updateValue((50 * (aa - 1)) + 30, activeImage);


            //STEP 3: FUZZY CMEANS- WHOLE IMAGE
            progressBar.setMessage("Taking Fuzzy-C over Entire Image");
            HardSeg = HardFuzzy(destImage2, 3);            						ShowImage(HardSeg,"hardseg1");		
            progressBar.updateValue(50 * (aa - 1) + 33, activeImage);

            
            //STEP 3a: BONE PROCESSING
            destImage3a = processBone(HardSeg);

            //STEP 4: FUZZY CMEANS- INSIDE BUNDLE
            progressBar.setMessage("Taking Fuzzy-C inside Muscle Bundle");		//convert(destImage2, voiMask, destImage2, 0, 0);  //crop
            FuzzySeg2 = SoftFuzzy(destImage2, 3);				            	ShowImage(FuzzySeg2[2], "FuzzySeg2["+2+"]");
            progressBar.updateValue(50 * (aa - 1) + 37, activeImage); 

            
            destImage2.disposeLocal();destImage2 = null;
            
            //STEP 4A:  FAT PROCESSING (inside muscle bundle)
            BMarrow = extractedBoneMarrow(FuzzySeg2[2]);						ShowImage(BMarrow, "boneMarrow");
            convert(destImage3a, BMarrow, destImage3a, 1, BoneMarrow);
            progressBar.setMessage("Processing bundle fat");
            destImage3b = processFat(FuzzySeg2[0],FuzzySeg2[1],FuzzySeg2[2]);  	//ShowImage(destImage3b, "processed fat image");
	        cleanUp(destImage3b, FAT, Muscle, 300*zDim/20);        				//ShowImage(destImage3b, "bundle cleanedup fat image");     
            progressBar.updateValue(50 * (aa - 1) + 46, activeImage);
            
            voiMask.disposeLocal();voiMask = null;            obMask.disposeLocal();obMask = null;
            BMarrow.disposeLocal();BMarrow = null;
            
            
            //--------------- STEP5: bringing two pieces together --------------
            progressBar.setMessage("Recreating Result Image");
            mergeImages(destImage3b, destImage3a, destImage3b);			ShowImage(destImage3b, "after 'merge'");
            progressBar.updateValue(50 * (aa - 1) + 47, activeImage);
            
            destImage3a.disposeLocal();destImage3a = null;
            
            
            //-------------- STEP6: last marrow cleanup ------------------------
            progressBar.setMessage("Tissue Type Counts/Volumes");
            marrowCleanup(destImage3b);											
            ShowImage(destImage3b, "after 'marrowCleanup'");
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
        	    
        	    AVGsubcutfatCountTOTAL+= AVGsubcutfatCount; 
        	    AVGfatCountTOTAL+= AVGfatCount;
        	    AVGMuscleCountTOTAL+= AVGMuscleCount;
        	    AVGBoneCountTOTAL+=AVGBoneCount;
        	    AVGBoneMarrowCountTOTAL+= AVGBoneMarrowCount;
        	    
        	    float realpixelSize = 1;
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
                		"CLASS \t \tVOLUME\tAVG INTENSITY \n" +
                		"SubcutaneousFat \t" + subcutfatCount * realpixelSize + "\t"+AVGsubcutfatCount+"\n"+ 
                		"InterstitialFat \t\t" + fatCount * realpixelSize + "\t"+AVGfatCount+"\n" +
                		"Muscle \t \t" + MuscleCount * realpixelSize + "\t"+AVGMuscleCount+"\n"+
                		"Bone \t \t" + BoneCount * realpixelSize + "\t"+AVGBoneCount+"\n"+
                		"BoneMarrow \t\t" + BoneMarrowCount * realpixelSize + "\t"+AVGBoneMarrowCount+"\n"+
                		"Total Thigh \t\t" + total_thighCount * realpixelSize + "\n \n");
              
                if(aa==2){
                	UI.getMessageFrame().append("Segmented Images - Results:  " + PlugInAlgorithmPipeline.patientID, "SEGMENTATION INTENSITIES \n " +
        	    		"SubcutaneousFat \t" + SUB_CUT_FAT + "\n"+ 
        	    		"InterstitialFat \t" + FAT + "\n"+
        	    		"Muscle \t \t" + Muscle + "\n"+
        	    		"Bone \t \t " + Bone + "\n"+
        	    		"BoneMarrow \t" + BoneMarrow + "\n \n"+
        	    		"TOTAL THIGH DATA \n\n" +
        	    		"CLASS \t \tAVG INTENSITY \n" +
        	    		"SubcutaneousFat\t"+AVGsubcutfatCountTOTAL+ "\n"+
        	    		"InterstitialFat \t"+AVGfatCountTOTAL+ "\n"+
        	    		"Muscle\t\t"+AVGMuscleCountTOTAL+ "\n"+
        	    		"Bone\t\t"+AVGBoneCountTOTAL+ "\n"+
        	    		"BoneMarrow\t"+AVGBoneMarrowCountTOTAL);
                	}

                destImage3b.disposeLocal();
                destImage3b = null;
                progressBar.updateValue(50 * (aa - 1) + 50, activeImage);
            }
        System.out.println("thigh cleanup done --destImage");
        setCompleted(true);
        disposeProgressBar();
            
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
     * @param   srcImage  DOCUMENT ME!
     * @param   nClasses  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage[] SoftFuzzy(ModelImage srcImage, int nClasses){
    	float centroid_array[] = new float[nClasses];
        float[] buffer = new float[volSize];
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
     * @param   threshSourceImg  DOCUMENT ME!
     * @param   intensity        DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage threshold(ModelImage threshSourceImg, float intensity) {
        float[] thresh = { intensity - 5, intensity + 5 };
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
     * @param   threshSourceImg  DOCUMENT ME!
     * @param   intensity1       DOCUMENT ME!
     * @param   intensity2       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage threshold1(ModelImage threshSourceImg, float intensity1, float intensity2) {
    	float thresh[] = {intensity1, intensity2};
    	ModelImage resultImage = null;
    	resultImage = new ModelImage(threshSourceImg.getType(), threshSourceImg.getExtents(),
    			"threshResultImg", threshSourceImg.getUserInterface());
        AlgorithmThresholdDual threshAlgo = null;
        threshAlgo = new AlgorithmThresholdDual(resultImage, threshSourceImg, thresh, 1, true, true, true);
        threshAlgo.setProgressBarVisible(false);
        threshAlgo.run();
        
        threshSourceImg.disposeLocal();threshSourceImg=null;
        return resultImage;
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  Exception  DOCUMENT ME!
     */
    private void jbInit() throws Exception { }
}
