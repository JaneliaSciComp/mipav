import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.*;

import java.io.*;


/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 *
 *           <p>The OAI -- Osteoarthritis Initiative – is a nationwide research study sponsored by the National Institutes of Health, that will help us better understand how to prevent and treat knee osteoarthritis, one of the most common causes of disability in adults. It is a four-year study and will recruit men and women aged 45 and above at high risk for developing symptomatic knee osteoarthritis. Osteoarthritis causes more health problems and medical expenses than any other form of arthritis. Symptoms of osteoarthritis can range from stiffness and mild pain to severe joint pain and even disability. The OAI cohort will be 5000 participants with clinically significant knee OA or at high risk for developing incident OA and obtain the appropriate images and bio-specimens needed for investigation and validation of OA biomarkers. The large number of images that results from the OAI is a major obstacle to overcome. Manual image segmentation is laborious and subject to inter and intra-observer variability when performing volumetric analysis. Therefore, BIRSS has started a multistage segmentation and quantification technique to automatically or semi-automatically process the entire cohort.</p>
 */


/*
 PROJECT ‘AUTOMATIC THIGH SEGMENTATION’ – DOCUMENTATION
2/9/06

STAGE 1
Start out with image of two thighs. Create binary mask indicative of thigh area,
 versus background. Contours of outer boundary mask (obMask) outline two thighs.
 Slice intensity normalize (ISN) 20 slice 3D thigh image.  Use mask outline of
 two thighs to crop ISN’d thigh data, tightly around thigh contours. Output are
 two tightly cropped 3D thigh images.

INTERMEDIATE:  User inputs VOI around muscle bundle, for two thigh images.

STAGE 2 (done twice)
Binary mask (‘voiMask’) created from input muscle bundle VOI. Contours of ‘voiMask’
 outline muscle bundle. Following operations are done inside muscle bundle only.
 Remaining area outside of muscle bundle yet within thigh, need only be labeled
 subcutaneous fat and requires no further processing. Therefore over the following
 operations the muscle bundle mask will be input to indicate over which area the
 algorithms should process.

• Thigh and voiMask sent to an Inhomogeneity nonparametric intensity nonuniformity
  normalization (N3), a shading correction algorithm.

• N3’d thigh and voiMask sent to Hard Fuzzy Segmentation
        o N3’d thigh segmented into 3 classes (fat, muscle, background)
               \uF0A7 Used to isolate bone and bone marrow
        o N3’d thigh segmented inside muscle bundle into 4 classes(using voiMask)
                       – 2 types of fat, muscle, background. This is to reveal more hidden fat.
               \uF0A7 Used to see inside the muscle bundle in as great detail as possible.

• ObMask, voiMask, and 4-class Fuzzy segmented muscle bundle image sent to Subcutaneous Fat Cleanup/Relabeling.
        o Taking the difference between the outer boundary mask and muscle
          bundle mask outputs a binary mask of the subcutaneous fat region.
        o Give this area different intensity on muscle bundle segmented image.
          (*eliminates issue of mis-segmentation over heavily artifacted
          subcutaneous fat region –inhomogeneous shading*)

• Subcutaneous fat relabeled 4-class segmented muscle bundle image and voiMask
  sent to muscle bundle cleanup (& bone/bone marrow relabeling)
        o Muscle Bundle Cleanup:
               \uF0A7 Eliminate ‘noise’
               \uF0A7 Convert two ‘fat’ intensities to one intensity.
               \uF0A7 Convert ‘background’ to fat.
        o Bone/Bone Marrow relabeling
               \uF0A7 IDObjects:  filter out objects with the same size and intensity
                 of expected ‘bone’. Convert intensity. This area becomes foreground
                 on binary template, and is relabeled ‘bone’ on 4-class segmented muscle bundle.
               \uF0A7 Morphological ‘close’ bone (dilation followed by erosion).
               \uF0A7 Morphological ‘fill hole’ inside ‘bone’.
               \uF0A7 Difference between area on template (now bone and bone marrow)
                 and what was labeled ‘bone’ on the image is ‘bone marrow’.  The
                 intensity over this region is then converted to ‘bone marrow’
                 on 4-class segmented muscle bundle.
 */

public class PlugInAlgorithmOAICropImage extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    /** hardSeg intensities (3 class segmentation). */
    public static int BACKGROUND = 85;

    /** DOCUMENT ME! */
    public static int MUSCLE = 170;

    /** DOCUMENT ME! */
    public static int FAT = 255;

    /** DOCUMENT ME! */
    public static String patientID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage destImageA = null;

    /** DOCUMENT ME! */
    private ModelImage destImageB = null;

    /** DOCUMENT ME! */
    private AlgorithmFuzzyCMeans firstFuzz = null;

    /** DOCUMENT ME! */
    private ModelImage HardSeg = null;

    /** DOCUMENT ME! */
    private int[] imgBuffer = null;

    /** DOCUMENT ME! */
    private int[] imgBuffer1 = null;

    /** DOCUMENT ME! */
    private int[] imgBuffer2 = null;
    
    /** DOCUMENT ME! */
    public static int xDim, yDim, zDim, sliceSize;

    /** DOCUMENT ME! */
    private ModelImage obMask = null;
    
    /** DOCUMENT ME! */
    private ModelImage obMaskA = null;

    /** DOCUMENT ME! */
    private ModelImage obMaskB = null;

    /** DOCUMENT ME! */
    private ModelImage srcImage = null;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     *
     * @param  destImageA  DOCUMENT ME!
     * @param  destImageB  DOCUMENT ME!
     * @param  obMaskA     DOCUMENT ME!
     * @param  obMaskB     DOCUMENT ME!
     * @param  srcImage    the source image
     */
    public PlugInAlgorithmOAICropImage(ModelImage destImageA, ModelImage destImageB, ModelImage obMaskA, ModelImage obMaskB,
                                   ModelImage srcImage) {
        super(null, srcImage);
        this.srcImage = srcImage;
        this.destImageA = destImageA;
        this.destImageB = destImageB;
        this.obMaskA = obMaskA;
        this.obMaskB = obMaskB;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        imgBuffer = null;
        
        if (obMask != null) {
            obMask.disposeLocal();
        }
        obMask = null;

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
     * @return  DOCUMENT ME!
     */
    public ModelImage getMaskA() {
        return this.obMaskA;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getMaskB() {
        return this.obMaskB;
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
     * @param  srcImage        DOCUMENT ME!
     * @param  nClasses        DOCUMENT ME!
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
     * Function fills the centroid_array with nClasses values equally spaced between 0
     * and the image maximum.  This is a seed array for the CMeans algorithm.  
     *
     * @param  centroid_array  DOCUMENT ME!
     * @param  srcImage        DOCUMENT ME!
     * @param  nClasses        DOCUMENT ME!
     */
	public void getCentroid(float[] centroid_array, ModelImage srcImage, int nClasses){
		int dd;
		double max = srcImage.getMax();
//		System.out.println("getCentroid()  nClasses: " + nClasses + "  max: " + max);
    	for(dd = 1; dd < (nClasses+1); dd++){
    		centroid_array[dd-1] = (float)(dd * max / (nClasses+1));
 //   		System.out.println("	centroid_array[" + (dd-1) + "]: " + centroid_array[dd-1]);
     	}
	}
	
	
    /**
     * DOCUMENT ME!
     *
     * @param  SegmentedImg  		DOCUMENT ME!
     * @param  Mask		        	DOCUMENT ME!
     */
	public ModelImage boundaryCorrect(ModelImage SegmentedImg){
    	ModelImage Mask = new ModelImage(SegmentedImg.getType(), SegmentedImg.getExtents(), "Mask", SegmentedImg.getUserInterface());
        int j, i, x, y, xx, yy, BACKGROUNDFound;
        for (j = 0; j < zDim; j++) {
            try {
                progressBar.updateValue(Math.round(10 + (30 * j / zDim)), runningInSeparateThread);

                SegmentedImg.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer);
                Mask.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer1);

                // setting the outer BACKGROUND imgBuffers on mask---------background = 0, thigh in=1
                for (i = 0; i < imgBuffer.length; i++) {
                    imgBuffer1[i] = 1;
                    imgBuffer2[i] = 1;
                }
                x = 0;
                for (y = 0; y < yDim; y++) {
                    i = x + (y * xDim);
                    if (imgBuffer[i] == BACKGROUND) {
                        imgBuffer1[i] = 0;
                        imgBuffer2[i] = 0;
                    }
                }
                x = xDim - 1;
                for (y = 0; y < yDim; y++) {
                    i = x + (y * xDim);
                    if (imgBuffer[i] == BACKGROUND) {
                        imgBuffer1[i] = 0;
                        imgBuffer2[i] = 0;
                    }
                }
                y = 0;
                for (x = 0; x < xDim; x++) {
                    i = x + (y * xDim);
                    if (imgBuffer[i] == BACKGROUND) {
                        imgBuffer1[i] = 0;
                        imgBuffer2[i] = 0;
                    }
                }
                y = yDim - 1;
                for (x = 0; x < xDim; x++) {
                    i = x + (y * xDim);
                    if (imgBuffer[i] == BACKGROUND) {
                        imgBuffer1[i] = 0;
                        imgBuffer2[i] = 0;
                    }
                }
                // setting BACKGROUND imgBuffers 4-connected to 1 of original boundary, as background.
                do {
                    BACKGROUNDFound = 0;
                    for (y = 0; y < yDim; y++) {
                        for (x = 0; x < xDim; x++) {
                            i = x + (y * xDim);
                            if (imgBuffer1[i] == 0) {
                                // checks left nearest neighbor
                                if ((x != 0) && (imgBuffer[i - 1] == BACKGROUND) && (imgBuffer1[i - 1] == 1)) {
                                    imgBuffer1[i - 1] = 0;
                                    imgBuffer2[i - 1] = 0;
                                    BACKGROUNDFound++;
                                }
                                // right nearest neighbor
                                if ((x != (xDim - 1)) && (imgBuffer[i + 1] == BACKGROUND) && (imgBuffer1[i + 1] == 1)) {
                                    imgBuffer1[i + 1] = 0;
                                    imgBuffer2[i + 1] = 0;
                                    BACKGROUNDFound++;
                                }
                                // top
                                if ((y != 0) && (imgBuffer[i - xDim] == BACKGROUND) && (imgBuffer1[i - xDim] == 1)) {
                                    imgBuffer1[i - xDim] = 0;
                                    imgBuffer2[i - xDim] = 0;
                                    BACKGROUNDFound++;
                                }
                                // bottom
                                if ((y != (yDim - 1)) && (imgBuffer[i + xDim] == BACKGROUND) &&
                                        (imgBuffer1[i + xDim] == 1)) {
                                    imgBuffer1[i + xDim] = 0;
                                    imgBuffer2[i + xDim] = 0;
                                    BACKGROUNDFound++;
                                }
                            }
                        }
                    }
                } while (BACKGROUNDFound > 0);

                // convert gray imgBuffer with outer BACKGROUND imgBuffer in its 5x5 neighborhood, into BACKGROUND
                for (y = 2; y < (yDim - 2); y++) {
                    for (x = 2; x < (xDim - 2); x++) {
                        i = x + (y * xDim);
                        if (imgBuffer[i] == MUSCLE) {
                            // check 5x5 neighborhood
                            if ((x != 0) && (y != 0) && (x != (xDim - 1)) && (y != (yDim - 1))) {
                                for (yy = -2; yy <= 2; yy++) {
                                    for (xx = -2; xx <= 2; xx++) {
                                        if (imgBuffer1[i + xx + (yy * xDim)] == 0) {
                                            imgBuffer[i] = BACKGROUND;
                                            imgBuffer2[i] = 0;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Mask.importData((j * imgBuffer.length), imgBuffer2, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmPipeline");
            }
        }

        SegmentedImg.disposeLocal();
        SegmentedImg = null;
        
        return Mask;
    }
	
    
    /**
     * DOCUMENT ME!
     *
     * @param  srcImage        DOCUMENT ME!
     *
     */
    public void crop(ModelImage destImage, ModelImage srcImage, int[] xbound, int[] ybound, int[] zbound){
    	
    	AlgorithmCrop algorithmVOICrop1 = new AlgorithmCrop(destImage, srcImage, 0, xbound, ybound, zbound);
        algorithmVOICrop1.setProgressBarVisible(false);
        algorithmVOICrop1.run();
        algorithmVOICrop1.finalize();
        algorithmVOICrop1 = null;

        destImage.calcMinMax();
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
     * morphological CLOSE
     *
     * @param  sourceImg  --source image
     * @param  kernalSize --kernal size for closure
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
     */
    public void runAlgorithm() {
    	
        buildProgressBar("OAI Thigh Seg. 6/22/06", "Initializing...", 0, 100);
        initProgressBar();

        try {
            patientID = (String) ((FileDicomTag) ((FileInfoDicom) srcImage.getFileInfo(0)).getEntry("0010,0020")).getValue(false);
            System.err.println("patient id is: " + patientID);
        }catch (Exception ex) { // do nothing
        }

        int i, j, x, y, nClasses; 
        
        zDim = 1;
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        imgBuffer  = new int[sliceSize];
        imgBuffer1 = new int[sliceSize];
        imgBuffer2 = new int[sliceSize];

        if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }

// PFH        ShowImage(srcImage, "Source Image");
        
       // --------------- STEP 1: Obtaining Background Mask --------------------
        // A) FUZZY C Means SEGMENTATION with 3 classes
        progressBar.updateValue(5, runningInSeparateThread);
        progressBar.setMessage("Fuzzy C-Means");
        nClasses = 3;
        HardSeg = HardFuzzy(srcImage, nClasses);
         
        // B) BOUNDARY CORRECTION (which works only with hard fuzzy data)
        progressBar.updateValue(15, runningInSeparateThread);
        progressBar.setMessage("Boundary Correction");
        obMask = boundaryCorrect(HardSeg);

        // To eliminate MR artifacts that appear away from the thighs, apply a connected
        // components anaylsis here and keep only the voxels in the two largest components
        // Use the resulting image in the following code, which determines the bounding
        // box of the voxels in each thigh
 
        IDObjects(obMask, zDim*2000, xDim*yDim*zDim);
        
        // make all components a label value of one
        for (i = 0; i < xDim*yDim*zDim; i++) {
        	if(obMask.getShort(i) >= 1) obMask.setShort(i, (short)1);
        }

        // ----------------------STEP 2: THIGH SEPARATION -----------------------
        progressBar.updateValue(25, runningInSeparateThread);
        progressBar.setMessage("Image Cropping");

        // CROP VOI
        
        // X min and max of the bounding box around the thigh image on the left (right thigh)
        int[] xbound = new int[2];
        xbound[0] = (int) (xDim / 2);
        xbound[1] = (int) (1);

        // Y min and max of the bounding box around the both thigh images
        int[] ybound = new int[2];
        ybound[0] = (int) (yDim / 2);
        ybound[1] = (int) (yDim / 2);

        // Z min and max of the bounding box around the both thigh images
        int[] zbound = new int[2];
        zbound[0] = 0;
        zbound[1] = zDim - 1;
        
        // X min and max of the bounding box around the thigh image on the right (left thigh)
       int[] xbound1 = new int[2];
        xbound1[0] = (int) (xDim - 1);
        xbound1[1] = (int) (xDim / 2);

        int x0, x11;
        x0 = xbound[0];
        x11 = xbound1[1];
        
        for (j = 0; j < zDim; j++) {
            try {
                progressBar.updateValue(Math.round(40 + (30 * j / zDim)), runningInSeparateThread);
                obMask.exportData((j * imgBuffer.length), imgBuffer.length, imgBuffer);
               
                for (y = 5; y < (yDim - 5); y++) {
                    for (x = 2; x < (xDim - 2); x++) {
                        i = x + (y * xDim);
                        // both legs, top y
                        if ((imgBuffer[i - (2 * xDim)] == 0) && // pixel in previous row
                        	(imgBuffer[i - xDim] == 0) && 		// pixel in previous row
                        	(imgBuffer[i] == 1) &&
                        	(imgBuffer[i + xDim] == 1) &&
                        	(imgBuffer[i + (2 * xDim)] == 1) &&
                        	(imgBuffer[i + (5 * xDim)] == 1)) {
                            if (y < ybound[0]) {
                                ybound[0] = y;
                            }
                        }
                        // both legs, bottom y
                        if ((imgBuffer[i + (2 * xDim)] == 0) && (imgBuffer[i + xDim] == 0) && (imgBuffer[i] == 1) &&
                                (imgBuffer[i - xDim] == 1) && (imgBuffer[i - (2 * xDim)] == 1) &&
                                (imgBuffer[i - (5 * xDim)] == 1)) {
                            if (y > ybound[1]) {
                                ybound[1] = y;
                            }
                        }
                    }
                }

                for (y = ybound[0]; y < ybound[1]; y++) {
                    // left leg, left x
                    for (x = 5; x < x0; x++) {
                        i = x + (y * xDim);
                        if ((imgBuffer[i - 2] == 0) && (imgBuffer[i - 1] == 0) && (imgBuffer[i] == 1) &&
                                (imgBuffer[i + 1] == 1) && (imgBuffer[i + 2] == 1) && (imgBuffer[i + 5] == 1)) {
                            if (x < xbound[0]) {
                                xbound[0] = x;
                            }
                        }
                    }
                }

                for (y = ybound[0]; y < ybound[1]; y++) {
                    for (x = x11; x < (xDim - 5); x++) {
                        i = x + (y * xDim);
                        // right leg, right x
                        if ((imgBuffer[i + 3] == 0) && (imgBuffer[i + 4] == 0) && (imgBuffer[i + 5] == 0) &&
                                (imgBuffer[i + 2] == 0) && (imgBuffer[i + 1] == 0) && (imgBuffer[i] == 1) &&
                                (imgBuffer[i - 1] == 1) && (imgBuffer[i - 2] == 1) && (imgBuffer[i - 5] == 1)) {
                            if (x > xbound1[1]) {
                                xbound1[1] = x;
                            }
                        }
                    }
                }

                for (y = ybound[0]; y < ybound[1]; y++) {
                    for (x = xbound[0]; x < (xDim / 2); x++) {
                        i = x + (y * xDim);
                        // left leg, right x
                        if ((imgBuffer[i - 5] == 1) && (imgBuffer[i - 2] == 1) && (imgBuffer[i - 1] == 1) &&
                                (imgBuffer[i] == 0) && (imgBuffer[i + 1] == 0) && (imgBuffer[i + 2] == 0) &&
                                (imgBuffer[i + 3] == 0) && (imgBuffer[i + 4] == 0) && (imgBuffer[i + 5] == 0)) {
                            if (x > xbound[1]) {
                                xbound[1] = x;
                            }
                        }
                    }
                }

                for (y = ybound[0]; y < ybound[1]; y++) {
                    for (x = xbound[1]; x < xbound1[1]; x++) {
                        i = x + (y * xDim);
                        // right leg, left x
                        if ((imgBuffer[i - 2] == 0) && (imgBuffer[i - 1] == 0) && (imgBuffer[i] == 1) &&
                                (imgBuffer[i + 1] == 1) && (imgBuffer[i + 2] == 1) && (imgBuffer[i + 5] == 1)) {
                            if (x < xbound1[0]) {
                                xbound1[0] = x;
                            }
                        }
                    }
                }

                obMask.importData((j * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmPipeline2");
            }
        }
        
        // This just smooths out the boundaries of the mask image
        Close(obMask, 24);  /*section added to eliminate results of shading artifact on obMask*/

        int[] extentA = new int[3];
        int[] extentB = new int[3];
        
        // it would be nice to leave a 5 pixel boundary around the thighs
        // requires new VOI's
//        xbound[0]  -= 5;
//        xbound1[1] += 5;
//        ybound[0]  -= 5;
//        ybound[1]  += 5;

        extentA[0] = xbound[1] - xbound[0] + 1;
        extentA[1] = ybound[1] - ybound[0] + 1;
        extentA[2] = zbound[1] - zbound[0] + 1;

        extentB[0] = xbound1[1] - xbound1[0] + 1;
        extentB[1] = extentA[1];
        extentB[2] = extentA[2];


        obMaskA = new ModelImage(srcImage.getType(), extentA, "obMaskA", srcImage.getUserInterface());
        obMaskB = new ModelImage(srcImage.getType(), extentB, "obMaskB", srcImage.getUserInterface());

        destImageA = new ModelImage(srcImage.getType(), extentA, ("cropped right leg"));
        destImageB = new ModelImage(srcImage.getType(), extentB, ("cropped left leg"));

        //ShowImage(srcImage,"srcImage");
        
        crop(destImageA, srcImage, xbound, ybound, zbound);
        progressBar.updateValue(85, runningInSeparateThread);
        //ShowImage(destImageA,"destImageA");
        
        //        System.out.println("used obMask voi to crop ISN'd imageA");
        crop(destImageB, srcImage, xbound1, ybound, zbound);
        progressBar.updateValue(90, runningInSeparateThread);
        //ShowImage(destImageB,"destImageA");
        
        //        System.out.println("used obMask voi to crop ISN'd imageB");
        crop(obMaskA, obMask, xbound, ybound, zbound);
        progressBar.updateValue(95, runningInSeparateThread);
        
        //        System.out.println("used obMask voi to crop ISN'd imageA");
        crop(obMaskB, obMask, xbound1, ybound, zbound);
        progressBar.updateValue(100, runningInSeparateThread);

        // set the first part to being done, and return to the listening dialog
        setCompleted(true);
        finalize();
        disposeProgressBar();
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

}
