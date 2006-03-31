import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import java.io.*;
import gov.nih.mipav.view.ViewUserInterface;

import gov.nih.mipav.model.algorithms.AlgorithmFuzzyCMeans;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmCrop;

import java.io.*;
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

public class PlugInAlgorithmPipeline
    extends AlgorithmBase {

    private ModelImage srcImage = null;
    private ModelImage destImageA = null;
    private ModelImage destImageB = null;
    private ModelImage destImage1 = null;
    private ModelImage[] HardSeg = null;
    private ModelImage obMask = null;
    private ModelImage obMaskA = null;
    private ModelImage obMaskB = null;

    private int[] imgBuffer = null;
    private int[] imgBuffer1 = null;
    private int[] imgBuffer2 = null;
    private float[] centroid_array = null;

    //hardSeg intensities (3 class segmentation)
    public static int BACKGROUND = 85;
    public static int MUSCLE = 170;
    public static int FAT = 255;

    public static String patientID;

    private AlgorithmFuzzyCMeans firstFuzz = null;
    private ViewUserInterface UI = ViewUserInterface.getReference();


    /**
     * Default constructor
     * @param srcImage the source image
     */
    public PlugInAlgorithmPipeline(ModelImage destImageA, ModelImage destImageB,
                                   ModelImage obMaskA, ModelImage obMaskB,
                                   ModelImage srcImage) {
        super(null, srcImage);
        this.srcImage = srcImage;
        this.destImageA = destImageA;
        this.destImageB = destImageB;
        this.obMaskA = obMaskA;
        this.obMaskB = obMaskB;
    }

    public void runAlgorithm() {

        buildProgressBar("Pipeline_Crop", "Initializing...", 0, 100);
        initProgressBar();



            try {
            	patientID = (String) ( (FileDicomTag) ( (FileInfoDicom) srcImage.getFileInfo( 0 ) ).getEntry( "0010,0020" ) ).getValue(
                        false );
                System.err.println("patient id is: " + patientID);
            } catch ( Exception ex ) {//do nothing
            }


        int i, j, x, y, xx, yy;
        double min, max;
        int BACKGROUNDFound;
        int z = 1;
        int xDim, yDim, sliceSize;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;
        imgBuffer = new int[sliceSize];
        imgBuffer1 = new int[sliceSize];
        imgBuffer2 = new int[sliceSize];

        if (srcImage.getNDims() == 3) {
            z = srcImage.getExtents()[2];
        }

        destImage1 = new ModelImage(srcImage.getType(), srcImage.getExtents(), "destImage1",
                                    srcImage.getUserInterface());
        obMask = new ModelImage(srcImage.getType(), srcImage.getExtents(), "obMask",
                                srcImage.getUserInterface());




//-------------------------------------------------------------------------------------------
//----------------------START STAGE 1:  THIGH SEPARATION-------------------------------------
//-------------------------------------------------------------------------------------------


        //--------------- STEP 1: Obtaining Background Mask --------------------
        //A) FUZZY SEGMENTATION
        progressBar.updateValue(5, activeImage);
        progressBar.setMessage("Obtaining Background Mask");
        HardSeg = new ModelImage[1];
        FileInfoBase fileInfo;
        HardSeg[0] = new ModelImage(ModelStorageBase.UBYTE, srcImage.getExtents(),
                                    "Hard-Fuzzy" + "_seg", srcImage.getUserInterface());
        fileInfo = HardSeg[0].getFileInfo()[0];
        fileInfo.setResolutions(srcImage.getFileInfo()[0].getResolutions());
        fileInfo.setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
        HardSeg[0].setFileInfo(fileInfo, 0);

        srcImage.calcMinMax();
        min = srcImage.getMin();
        max = srcImage.getMax();

        int nClasses = 3;
        centroid_array = new float[nClasses];
        for (i = 0; i < nClasses; i++) {
            centroid_array[i] = (float) (min + (max - min) * (i + 1) / (nClasses + 1));
        }

        firstFuzz = null;
        firstFuzz = new AlgorithmFuzzyCMeans(HardSeg, srcImage, nClasses, 4,
                                             1, 2, 2.0f, 20000, 200000, false, AlgorithmFuzzyCMeans.HARD_ONLY,
                                             false, 0.0f, 200, 0.01f, true);
        firstFuzz.setCentroids(centroid_array);
        firstFuzz.setProgressBarVisible(false);
        firstFuzz.run();



        firstFuzz.finalize();
        firstFuzz = null;

        //B) BOUNDARY CORRECTION (which works only with hard fuzzy data)
        for (j = 0; j < z; j++) {
            try {
              progressBar.updateValue(Math.round(10+30*j/z), activeImage);
                HardSeg[0].exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                obMask.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer1);

                //setting the outer BACKGROUND imgBuffers on mask---------background = 0, thigh in=1
                for (i = 0; i < imgBuffer.length; i++) {
                    imgBuffer1[i] = 1;
                    imgBuffer2[i] = 1;
                }
                x = 0;
                for (y = 0; y < yDim; y++) {
                    i = x + y * xDim;
                    if (imgBuffer[i] == BACKGROUND) {
                        imgBuffer1[i] = 0;
                        imgBuffer2[i] = 0;
                    }
                }
                x = xDim - 1;
                for (y = 0; y < yDim; y++) {
                    i = x + y * xDim;
                    if (imgBuffer[i] == BACKGROUND) {
                        imgBuffer1[i] = 0;
                        imgBuffer2[i] = 0;
                    }
                }
                y = 0;
                for (x = 0; x < xDim; x++) {
                    i = x + y * xDim;
                    if (imgBuffer[i] == BACKGROUND) {
                        imgBuffer1[i] = 0;
                        imgBuffer2[i] = 0;
                    }
                }
                y = yDim - 1;
                for (x = 0; x < xDim; x++) {
                    i = x + y * xDim;
                    if (imgBuffer[i] == BACKGROUND) {
                        imgBuffer1[i] = 0;
                        imgBuffer2[i] = 0;
                    }
                }
//              setProgressBarVisible(false);
                //setting BACKGROUND imgBuffers 4-connected to 1 of original boundary, as background.
                do {
                    BACKGROUNDFound = 0;
                    for (y = 0; y < yDim; y++) {
                        for (x = 0; x < xDim; x++) {
                            i = x + y * xDim;
                            if (imgBuffer1[i] == 0) {
                                //checks left nearest neighbor
                                if ( (x != 0) && (imgBuffer[i - 1] == BACKGROUND) && (imgBuffer1[i - 1] == 1)) {
                                    imgBuffer1[i - 1] = 0;
                                    imgBuffer2[i - 1] = 0;
                                    BACKGROUNDFound++;
                                }
                                //right nearest neighbor
                                if ( (x != xDim - 1) && (imgBuffer[i + 1] == BACKGROUND) && (imgBuffer1[i + 1] == 1)) {
                                    imgBuffer1[i + 1] = 0;
                                    imgBuffer2[i + 1] = 0;
                                    BACKGROUNDFound++;
                                }
                                //top
                                if ( (y != 0) && (imgBuffer[i - xDim] == BACKGROUND) && (imgBuffer1[i - xDim] == 1)) {
                                    imgBuffer1[i - xDim] = 0;
                                    imgBuffer2[i - xDim] = 0;
                                    BACKGROUNDFound++;
                                }
                                //bottom
                                if ( (y != yDim - 1) && (imgBuffer[i + xDim] == BACKGROUND) &&
                                    (imgBuffer1[i + xDim] == 1)) {
                                    imgBuffer1[i + xDim] = 0;
                                    imgBuffer2[i + xDim] = 0;
                                    BACKGROUNDFound++;
                                }
                            }
                        }
                    }
                }
                while (BACKGROUNDFound > 0);

                //convert gray imgBuffer with outer BACKGROUND imgBuffer in its 5x5 neighborhood, into BACKGROUND
                for (y = 2; y < yDim - 2; y++) {
                    for (x = 2; x < xDim - 2; x++) {
                        i = x + y * xDim;
                        if (imgBuffer[i] == MUSCLE) {
                            //check 5x5 neighborhood
                            if (x != 0 && y != 0 && x != xDim - 1 && y != yDim - 1) {
                                for (yy = -2; yy <= 2; yy++) {
                                    for (xx = -2; xx <= 2; xx++) {
                                        if (imgBuffer1[i + xx + yy * xDim] == 0) {
                                            imgBuffer[i] = BACKGROUND;
                                            imgBuffer2[i] = 0;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                obMask.importData( (j * imgBuffer.length), imgBuffer2, false);
            }
            catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmPipeline");
            }
        }

        HardSeg[0].disposeLocal();
        HardSeg[0] = null;


//        System.out.println("outer boundary mask obtained --obMask");
//        obMask.calcMinMax();
//        new ViewJFrameImage(obMask);

        //---------------------- STEP 3: ISN IMAGE------------------------------
        progressBar.updateValue(40, activeImage);
        if(z>1){
            progressBar.setMessage("Passing 3D image through ISN");
            PlugInAlgorithmISN isnAlgo = null;
            isnAlgo = new PlugInAlgorithmISN(destImage1, srcImage);
            isnAlgo.setProgressBarVisible(false);
            isnAlgo.run();

            isnAlgo.finalize();
            isnAlgo = null;
        }

//        destImage1.calcMinMax();
//        destImage1.setImageName("ISN'd input");
//        new ViewJFrameImage(destImage1);

        //----------------------STEP 4: THIGH SEPARATION -----------------------

        progressBar.setMessage("Doing Thigh Segmentation");
        //CROP VOI
        int[] xbound = new int[2];
        int[] ybound = new int[2];
        int[] zbound = new int[2];

        int[] xbound1 = new int[2];

        xbound[0] = (int) (xDim / 2);
        xbound[1] = (int) (1);
        ybound[0] = (int) (yDim / 2);
        ybound[1] = (int) (yDim / 2);
        zbound[0] = 0;
        zbound[1] = z - 1;

        xbound1[0] = (int) (xDim - 1);
        xbound1[1] = (int) (xDim / 2);

        int x0, x11;
        x0 = xbound[0];
        x11 = xbound1[1];

        for (j = 0; j < z; j++) {
            try {
                progressBar.updateValue(Math.round(40+30*j/z), activeImage);

                obMask.exportData( (j * imgBuffer.length), imgBuffer.length, imgBuffer);
                for (y = 5; y < yDim - 5; y++) {
                    for (x = 2; x < xDim - 2; x++) {
                        i = x + y * xDim;
                        //both legs, top y
                        if (imgBuffer[i - 2 * xDim] == 0 && imgBuffer[i - xDim] == 0 && imgBuffer[i] == 1 &&
                            imgBuffer[i + xDim] == 1 && imgBuffer[i + 2 * xDim] == 1 && imgBuffer[i + 5 * xDim] == 1) {
                            if (y < ybound[0]) {
                                ybound[0] = y;
                            }
                        }
                        //both legs, bottom y
                        if (imgBuffer[i + 2 * xDim] == 0 && imgBuffer[i + xDim] == 0 && imgBuffer[i] == 1 &&
                            imgBuffer[i - xDim] == 1 && imgBuffer[i - 2 * xDim] == 1 && imgBuffer[i - 5 * xDim] == 1) {
                            if (y > ybound[1]) {
                                ybound[1] = y;
                            }
                        }
                    }
                }

                for (y = ybound[0]; y < ybound[1]; y++) {
                    //left leg, left x
                    for (x = 5; x < x0; x++) {
                        i = x + y * xDim;
                        if (imgBuffer[i - 2] == 0 && imgBuffer[i - 1] == 0 && imgBuffer[i] == 1 &&
                            imgBuffer[i + 1] == 1 && imgBuffer[i + 2] == 1 && imgBuffer[i + 5] == 1) {
                            if (x < xbound[0]) {
                                xbound[0] = x;
                            }
                        }
                    }
                }
                for (y = ybound[0]; y < ybound[1]; y++) {
                    for (x = x11; x < xDim - 5; x++) {
                        i = x + y * xDim;
                        //right leg, right x
                        if (imgBuffer[i + 3] == 0 && imgBuffer[i + 4] == 0 &&
                            imgBuffer[i + 5] == 0 && imgBuffer[i + 2] == 0 && imgBuffer[i + 1] == 0 &&
                            imgBuffer[i] == 1 &&
                            imgBuffer[i - 1] == 1 && imgBuffer[i - 2] == 1 && imgBuffer[i - 5] == 1) {
                            if (x > xbound1[1]) {
                                xbound1[1] = x;
                            }
                        }
                    }
                }

                for (y = ybound[0]; y < ybound[1]; y++) {
                    for (x = xbound[0]; x < xDim / 2; x++) {
                        i = x + y * xDim;
                        //left leg, right x
                        if (imgBuffer[i - 5] == 1 && imgBuffer[i - 2] == 1 && imgBuffer[i - 1] == 1 &&
                            imgBuffer[i] == 0 &&
                            imgBuffer[i + 1] == 0 && imgBuffer[i + 2] == 0 && imgBuffer[i + 3] == 0 &&
                            imgBuffer[i + 4] == 0 &&
                            imgBuffer[i + 5] == 0) {
                            if (x > xbound[1]) {
                                xbound[1] = x;
                            }
                        }
                    }
                }
                for (y = ybound[0]; y < ybound[1]; y++) {
                    for (x = xbound[1]; x < xbound1[1]; x++) {
                        i = x + y * xDim;
                        //right leg, left x
                        if (imgBuffer[i - 2] == 0 && imgBuffer[i - 1] == 0 && imgBuffer[i] == 1 &&
                            imgBuffer[i + 1] == 1 && imgBuffer[i + 2] == 1 && imgBuffer[i + 5] == 1) {
                            if (x < xbound1[0]) {
                                xbound1[0] = x;
                            }
                        }
                    }
                }

                obMask.importData( (j * imgBuffer.length), imgBuffer, false);
            }
            catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmPipeline2");
            }
        }

        int[] extentA = new int[3];
        int[] extentB = new int[3];

        extentA[0] = xbound[1] - xbound[0] + 1;
        extentA[1] = ybound[1] - ybound[0] + 1;
        extentA[2] = zbound[1] - zbound[0] + 1;

        extentB[0] = xbound1[1] - xbound1[0] + 1;
        extentB[1] = extentA[1];
        extentB[2] = extentA[2];


        obMaskA = new ModelImage(srcImage.getType(), extentA, "obMaskA", srcImage.getUserInterface());
        obMaskB = new ModelImage(srcImage.getType(), extentB, "obMaskB", srcImage.getUserInterface());

        destImageA = new ModelImage(srcImage.getType(), extentA,
                                    ("cropped right leg"));
        destImageB = new ModelImage(srcImage.getType(), extentB,
                                    ("cropped left leg"));


        AlgorithmCrop algorithmVOICrop1 = null;
        algorithmVOICrop1 = new AlgorithmCrop(destImageA, destImage1, 0, xbound, ybound, zbound);
        algorithmVOICrop1.setProgressBarVisible(false);
        algorithmVOICrop1.run();
        algorithmVOICrop1.finalize();
        algorithmVOICrop1 = null;

        destImageA.calcMinMax();
        progressBar.updateValue(85, activeImage);



//        System.out.println("used obMask voi to crop ISN'd imageA");

        AlgorithmCrop algorithmVOICrop2 = null;
        algorithmVOICrop2 = new AlgorithmCrop(destImageB, destImage1, 0, xbound1, ybound, zbound);
        algorithmVOICrop2.setProgressBarVisible(false);
        algorithmVOICrop2.run();
        algorithmVOICrop2.finalize();
        algorithmVOICrop2 = null;

        destImageB.calcMinMax();
        progressBar.updateValue(90, activeImage);

//        System.out.println("used obMask voi to crop ISN'd imageB");

        AlgorithmCrop algorithmVOICrop3 = null;
        algorithmVOICrop3 = new AlgorithmCrop(obMaskA, obMask, 0, xbound, ybound, zbound);
        algorithmVOICrop3.setProgressBarVisible(false);
        algorithmVOICrop3.run();
        algorithmVOICrop3.finalize();
        algorithmVOICrop3 = null;

        obMaskA.calcMinMax();
        progressBar.updateValue(95, activeImage);
//        System.out.println("used obMask voi to crop ISN'd imageA");

        AlgorithmCrop algorithmVOICrop4 = null;
        algorithmVOICrop4 = new AlgorithmCrop(obMaskB, obMask, 0, xbound1, ybound, zbound);
        algorithmVOICrop4.setProgressBarVisible(false);
        algorithmVOICrop4.run();
        algorithmVOICrop4.finalize();
        algorithmVOICrop4 = null;

        obMaskB.calcMinMax();
        progressBar.updateValue(100, activeImage);
        //set the first part to being done, and return to the listening dialog
        setCompleted(true);
        finalize();
        disposeProgressBar();
    }



    public ModelImage getResultImageA() {
        return this.destImageA;
    }

    public ModelImage getResultImageB() {
        return this.destImageB;
    }

    public ModelImage getMaskA() {
        return this.obMaskA;
    }

    public ModelImage getMaskB() {
        return this.obMaskB;
    }

    public void finalize() {
        disposeLocal();
        super.finalize();
        progressBar.dispose();
    }

    public void disposeLocal() {
        imgBuffer = null;


        obMask.disposeLocal();
        destImage1.disposeLocal();

        obMask = null;
        destImage1 = null;

    }
}
