import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmRegionGrow;
import gov.nih.mipav.model.algorithms.AlgorithmSnake;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtractionPaint;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.CubeBounds;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.Point3D;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import java.awt.Point;
import java.io.IOException;
import java.util.BitSet;



public class PlugInAlgorithmNewGeneric2 extends AlgorithmBase {
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Z dimension of the CT image */
    private int zDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;

    /** Volume size for xDim*yDim*zDim */
    private int volumeSize;
    
    private ModelImage boneImage;
    private ModelImage boneMarrowImage;
    private ModelImage thighTissueImage;
    private ModelImage segmentedImage;
//  private ModelImage muscleBundleImage;
    
    // center-of-mass array for region 1 and 2 (the thresholded bone)
    private int[] x1CMs;
    private int[] y1CMs;
    private int[] x2CMs;
    private int[] y2CMs;
    
    // temp buffer to store slices.  Needed in many member functions.
    private short[] sliceBuffer;

    // unique label values for the regions of interest
    private short muscleBundleLabel = 10;
    private short thighTissueLabel = 10;
    private short boneMarrowLabel = 20;
    private short boneLabel = 3;
    
    private boolean initializedFlag = false;
    
    private BitSet volumeBitSet;

    
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmNewGeneric2(ModelImage resultImage, ModelImage srcImg) {
        super(resultImage, srcImg);
    }

    
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        // Algorithm to determine the outer thigh boundary and boundaries of the bone and bone marrow
        
        if (!initializedFlag) {
            init();
        }
        
        if (!initializedFlag) {
            return;
        }
        
        segmentImage();
    } // end runAlgorithm()
    
    

//  ~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        super.finalize();
    }
    
    
    
    /**
     * Create all the data structures that are needed by the various routines to automatically
     * segment the bone, bone marrow, and muscle bundle in 2D and 3D CT images of the thighs.
     */
    void init() {
        // simple error check up front
        if ((srcImage.getNDims() != 2) && srcImage.getNDims() != 3) {
            MipavUtil.displayError("PlugInAlgorithmNewGeneric2::init() Error image is not 2 or 3 dimensions");
            return;
        }
        
        // set and allocate know values
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];

        sliceSize = xDim * yDim;

        try {
            sliceBuffer = new short[sliceSize];
            x1CMs = new int [sliceSize];
            y1CMs = new int [sliceSize];
            x2CMs = new int [sliceSize];
            y2CMs = new int [sliceSize];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: init()");
        }

        // set values that depend on the source image being a 2D or 3D image
        if (srcImage.getNDims() == 2) {
            zDim = 1;
            volumeSize = sliceSize;
        } else if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
            volumeSize = sliceSize * zDim;
        }

        // make the label images and initialize their resolutions
        boneMarrowImage  = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "boneMarrowImage");
        thighTissueImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "thighTissueImage");
        segmentedImage   = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "segmentedImage");
//        muscleBundleImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "muscleBundleImage");
       
        // make the resolutions of the images the same as the source image
        for (int i = 0; i < zDim; i++) {
            boneMarrowImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[i].getResolutions());
            thighTissueImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[i].getResolutions());
 //           muscleBundleImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[i].getResolutions());
        }
        
        volumeBitSet = new BitSet();
               
        // set initialized flag to true so the data structures are not reallocated
        initializedFlag = true;
    } // end init()
    
    
    /**
     * Find the bone, bone marrow, and the thigh tissue
     */
    private void segmentImage() {
        long time = System.currentTimeMillis();
        // compute the bone label image
        segmentBone();
        System.out.println("Bone segmentation: "+(System.currentTimeMillis() - time));
//        ShowImage(boneImage, "CT Bone");
        
        time = System.currentTimeMillis();
        segmentBoneMarrow();
        System.out.println("Bone marrow segmentation: "+(System.currentTimeMillis() - time));

        time = System.currentTimeMillis();
        makeBoneMarrowVOI();
        makeBoneVOI();
        System.out.println("Bone/Bone marrow VOIs: "+(System.currentTimeMillis() - time));

//        ShowImage(boneMarrowImage, "CT Bone Marrow");
//        ShowImage(boneImage, "CT Bone");

        time = System.currentTimeMillis();
        segmentThighTissue();
        System.out.println("Thigh tissue segmentation: "+(System.currentTimeMillis() - time));

        time = System.currentTimeMillis();
        makeThighTissueVOI();
        System.out.println("Thigh tissue VOIs: "+(System.currentTimeMillis() - time));
//        ShowImage(thighTissueImage, "Thigh tissue");
        
        time = System.currentTimeMillis();
        combineLabelImages();
        System.out.println("combination: "+(System.currentTimeMillis() - time));
        ShowImage(segmentedImage, "Segmented Image");
        
        // save the VOI to a file(s)
        String fileName = "boneMarrowVOI.xml";
        String directory = System.getProperty("user.dir");
        System.out.println("directory: " +directory);
        FileVOI fileVOI;
        try {
            fileVOI = new FileVOI(fileName, directory, boneMarrowImage);
            fileVOI.writeVOI(boneMarrowImage.getVOIs().VOIAt(0), true);
        } catch (IOException ex) {
            System.err.println("Error segmentImage():  Opening VOI file");
            return;
        }
        
        fileName = "boneVOI.xml";
        try {
            fileVOI = new FileVOI(fileName, directory, boneImage);
            fileVOI.writeVOI(boneImage.getVOIs().VOIAt(0), true);
        } catch (IOException ex) {
            System.err.println("Error segmentImage():  Opening VOI file");
            return;
        }
        
        fileName = "thighTissueVOI.xml";
        try {
            fileVOI = new FileVOI(fileName, directory, thighTissueImage);
            fileVOI.writeVOI(thighTissueImage.getVOIs().VOIAt(0), true);
        } catch (IOException ex) {
            System.err.println("Error segmentImage():  Opening VOI file");
            return;
        }
        
       
 
        /*
        time = System.currentTimeMillis();
        // copy the bone marrow VOI into the muscleBundle image
        muscleBundleImage.setVOIs(boneMarrowImage.getVOIs());
        computeMuscleBundle();
        System.out.println("Muscle bundle segmentation: "+(System.currentTimeMillis() - time));
        
        // copy the segmented bone into the MuscleBundle image
        
        
        time = System.currentTimeMillis();
        ShowImage(muscleBundleImage, "Segmented Image");
        System.out.println("Show image: "+(System.currentTimeMillis() - time));
        time = System.currentTimeMillis();
        
        
        // here is some stuff for dealing with VOI's
        VOIVector vois = muscleBundleImage.getVOIs();
        System.out.println("Number of VOIS: " + vois.size());

        VOI theVOI;
        Vector[] curves;
        VOIContour curve;
        
        for (int voiNum = 0; voiNum < vois.size(); voiNum++) {
            theVOI = vois.get(voiNum);
            curves = theVOI.getCurves();
            System.out.println("voiNum: " +voiNum +"  voiName: " +theVOI.getName());

            for (int sliceIdx = 0; sliceIdx < zDim; sliceIdx++) {
                System.out.println("  Slice: " + sliceIdx + "   Number of curves: " + curves[sliceIdx].size());
                
                for(int curveIdx = 0; curveIdx < curves[sliceIdx].size(); curveIdx++) {
                    curve = (VOIContour)curves[sliceIdx].elementAt(curveIdx);
                    System.out.println("    Curve: " +curveIdx +"  num points: " +curve.size());
                }
            }
        } // end for (voiNum = 0; ...)
        */
   } // end segmentImage()
    
    
    // create a voi for the bone.  Assumes the boneImage has been created.
    private void makeThighTissueVOI() {
        // make the volumeBitSet for the boneImage
        int sliceByteOffset;
        for (int volumeIdx = 0, sliceNum = 0; sliceNum < zDim; sliceNum++) {
            sliceByteOffset = sliceNum * sliceSize;
            try {
                thighTissueImage.exportData(sliceByteOffset, sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("Error exporting data");
            }
            for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, volumeIdx++) {
                if (sliceBuffer[sliceIdx] > 0) {
                    volumeBitSet.set(volumeIdx);
                }
            } // end for (int sliceIdx = 0; ...)
        } // end for(int sliceNum = 0; ...)
        
        // volumeBitSet should be set for the bone
        short voiID = 0;
        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(thighTissueImage,
                volumeBitSet, xDim, yDim, zDim, voiID);

        algoPaintToVOI.setRunningInSeparateThread(false);
        algoPaintToVOI.run();
        setCompleted(true);
        
        // make sure we got one VOI composed of two curves
        VOIVector vois = thighTissueImage.getVOIs();
        if(vois.size() != 1) {
            MipavUtil.displayError("makeThighTissueVOI() Error, did not get 1 VOI");
            return;
        }
        VOI theVOI = vois.get(0);
        theVOI.setName("Thigh Tissue");
        if (theVOI.getCurvesTemp()[0].size() != 2) {
            MipavUtil.displayError("makeThighTissueVOI() Error, did not get 2 curves in the VOI");
            return;
        }

    } // end makeThighTissueVOI()
    
    
    // create a voi for the bone.  Assumes the boneImage has been created.
    private void makeBoneVOI() {
        // make the volumeBitSet for the boneImage
        int sliceByteOffset;
        for (int volumeIdx = 0, sliceNum = 0; sliceNum < zDim; sliceNum++) {
            sliceByteOffset = sliceNum * sliceSize;
            try {
                boneImage.exportData(sliceByteOffset, sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("Error exporting data");
            }
            for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, volumeIdx++) {
                if (sliceBuffer[sliceIdx] > 0) {
                    volumeBitSet.set(volumeIdx);
                }
            } // end for (int sliceIdx = 0; ...)
        } // end for(int sliceNum = 0; ...)
        
        // volumeBitSet should be set for the bone
        short voiID = 0;
        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(boneImage,
                volumeBitSet, xDim, yDim, zDim, voiID);

        algoPaintToVOI.setRunningInSeparateThread(false);
        algoPaintToVOI.run();
        setCompleted(true);
        
        // make sure we got one VOI composed of two curves
        VOIVector vois = boneImage.getVOIs();
        if(vois.size() != 1) {
            MipavUtil.displayError("makeBoneVOI() Error, did not get 1 VOI");
            return;
        }
        VOI theVOI = vois.get(0);
        theVOI.setName("Bone");
        if (theVOI.getCurvesTemp()[0].size() != 2) {
            MipavUtil.displayError("makeBoneVOI() Error, did not get 2 curves in the VOI");
            return;
        }
    } // end makeBoneVOI()
    
    
    
    // create a voi for the bone marrow.  Assumes the boneMarrowImage has been created.
    private void makeBoneMarrowVOI() {
        // make the volumeBitSet for the boneMarrowImage
        int sliceByteOffset;
        for (int volumeIdx = 0, sliceNum = 0; sliceNum < zDim; sliceNum++) {
            sliceByteOffset = sliceNum * sliceSize;
            try {
                boneMarrowImage.exportData(sliceByteOffset, sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("makeBonMarrow() Error exporting data");
            }
            for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, volumeIdx++) {
                if (sliceBuffer[sliceIdx] > 0) {
                    volumeBitSet.set(volumeIdx);
                }
            } // end for (int sliceIdx = 0; ...)
        } // end for(int sliceNum = 0; ...)
        
        // volumeBitSet should be set for the bone marrow
        short voiID = 0;
        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(boneMarrowImage,
                volumeBitSet, xDim, yDim, zDim, voiID);

        algoPaintToVOI.setRunningInSeparateThread(false);
        algoPaintToVOI.run();
        setCompleted(true);
        
        // make sure we got one VOI composed of two curves
        VOIVector vois = boneMarrowImage.getVOIs();
        if(vois.size() != 1) {
            MipavUtil.displayError("makeBoneMarrowVOI() Error, did not get 1 VOI");
            return;
        }
        VOI theVOI = vois.get(0);
        theVOI.setName("Bone Marrow");
        if (theVOI.getCurvesTemp()[0].size() != 2) {
            MipavUtil.displayError("makeBoneMarrowVOI() Error, did not get 2 curves in the VOI");
            return;
        }
    } // end makeBoneMarrowVOI()
    
    
    // put the contents of all the label images into the segmented image
    void combineLabelImages() {
        short[] boneBuffer        = new short [sliceSize];
        short[] boneMarrowBuffer  = new short [sliceSize];
        short[] thighTissueBuffer = new short [sliceSize];
        short[] segmentedBuffer   = new short [sliceSize];
        int sliceByteOffset;
        for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
            sliceByteOffset = sliceNum * sliceSize;
            try {
                boneImage.exportData(sliceByteOffset, sliceSize, boneBuffer);
                boneMarrowImage.exportData(sliceByteOffset, sliceSize, boneMarrowBuffer);
                thighTissueImage.exportData(sliceByteOffset, sliceSize, thighTissueBuffer);
            } catch (IOException ex) {
                System.err.println("Error exporting data");
            }
            
            for (int idx = 0; idx < sliceSize; idx++) {
                segmentedBuffer[idx] = 0;
                if (boneBuffer[idx] > 0)        segmentedBuffer[idx] = boneLabel;
                if (boneMarrowBuffer[idx] > 0)  segmentedBuffer[idx] = boneMarrowLabel;
                if (thighTissueBuffer[idx] > 0) segmentedBuffer[idx] = thighTissueLabel;
            } // end for(int idx = 0; ...
            
            try {
                segmentedImage.importData(sliceByteOffset, segmentedBuffer, false);
            } catch (IOException ex) {
                System.err.println("computeBoneMarrowImage(): Error importing data");
            }
        } // end for (sliceNum = 0; ...)
        
        // add the VOIs ontot he segmented image
        segmentedImage.addVOIs(boneMarrowImage.getVOIs());
        segmentedImage.addVOIs(boneImage.getVOIs());
        segmentedImage.addVOIs(thighTissueImage.getVOIs());
 
        boneBuffer = null;
        boneMarrowBuffer = null;
        thighTissueBuffer = null;
        segmentedBuffer = null;
    } // end combineLabelImages()
    
    
    
    
    void segmentThighTissue() {
         
        // let's just grow a region
        // we need a seed point
        // walk out along the x-axis from the center-of-mass of the bone on the first slice
        try {
            srcImage.exportData(0, sliceSize, sliceBuffer);
        } catch (IOException ex) {
            System.err.println("Error exporting data");
        }
        
        int xcm = x1CMs[0];
        int ycm = y1CMs[0];
        
        // the center-of-mass should be in the marrow, increment x until we get to bone
        int idx = ycm * xDim + xcm;
        while(sliceBuffer[idx] < 750) {
            xcm++;
            idx++;
        }
        // increment x until we get past the bone
        while(sliceBuffer[idx] > 750) {
            xcm++;
            idx++;
        }
        // use the seed point that is 5 more x units away from the bone
        short seedX = (short)(xcm + 5);
        short seedY = (short)ycm;
        short seedZ = 0;
        short seedVal = sliceBuffer[idx + 5];
        
        BitSet thigh1Bitmap = new BitSet();
        regionGrowMuscle(seedX, seedY, seedZ, seedVal, thigh1Bitmap);
        
        // segment the second muscle bundle
        xcm = x2CMs[0];
        ycm = y2CMs[0];
        
        // the center-of-mass should be in the marrow, increment x until we get to bone
        idx = ycm * xDim + xcm;
        while(sliceBuffer[idx] < 750) {
            xcm++;
            idx++;
        }
        // increment x until we get past the bone
        while(sliceBuffer[idx] > 750) {
            xcm++;
            idx++;
        }
        // use the seed point that is 5 more x units away from the bone
        seedX = (short)(xcm + 5);
        seedY = (short)ycm;
        seedZ = 0;
        seedVal = sliceBuffer[idx + 5];
        
        BitSet thigh2Bitmap = new BitSet();
        regionGrowMuscle(seedX, seedY, seedZ, seedVal, thigh2Bitmap);

        // make the thighTissue label image slice by slice from the 3D region grown BitSet
        // bitSetIdx is a cumulative index into the 3D BitSet
        for (int bitSetIdx = 0, sliceNum = 0; sliceNum < zDim; sliceNum++) {
            for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, bitSetIdx++) {
                if (thigh1Bitmap.get(bitSetIdx) || thigh2Bitmap.get(bitSetIdx)) {
                    sliceBuffer[sliceIdx] = thighTissueLabel;
                } else {
                    sliceBuffer[sliceIdx] = 0;
                }
            } // end for (int sliceIdx = 0; ...)
            
            // save the sliceBuffer into the boneMarrowImage
            try {
                thighTissueImage.importData(sliceNum * sliceSize, sliceBuffer, false);
            } catch (IOException ex) {
                System.err.println("segmentThighTissue(): Error importing data");
            }
        } // end for(int bitSetIdx = 0, sliceNum = 0; ...)
     } // end segmentThighTissue(...)
    
    
    
    void computeMuscleBundle() {
         
        // let's just grow a region
        // we need a seed point
        // walk out along the x-axis from the center-of-mass of the bone on the first slice
        try {
            srcImage.exportData(0, sliceSize, sliceBuffer);
        } catch (IOException ex) {
            System.err.println("Error exporting data");
        }
        
        int xcm = x1CMs[0];
        int ycm = y1CMs[0];
        
        // the center-of-mass should be in the marrow, increment x until we get to bone
        int idx = ycm * xDim + xcm;
        while(sliceBuffer[idx] < 750) {
            xcm++;
            idx++;
        }
        // increment x until we get past the bone
        while(sliceBuffer[idx] > 750) {
            xcm++;
            idx++;
        }
        // use the seed point that is 5 more x units away from the bone
        short seedX = (short)(xcm + 5);
        short seedY = (short)ycm;
        short seedZ = 0;
        short seedVal = sliceBuffer[idx + 5];
        
        BitSet muscle1Bitmap = new BitSet();
        regionGrowMuscle(seedX, seedY, seedZ, seedVal, muscle1Bitmap);
        
//        short voiID = (short) muscleBundleImage.getVOIs().size();
        
        // convert the region into a VOI
        // get the VOI of the first bone marrow region
//        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(muscleBundleImage,
//                muscle1Bitmap, xDim, yDim, zDim, voiID);

//        algoPaintToVOI.setRunningInSeparateThread(false);
//        algoPaintToVOI.run();
        setCompleted(true);


        // segment the second muscle bundle
        xcm = x2CMs[0];
        ycm = y2CMs[0];
        
        // the center-of-mass should be in the marrow, increment x until we get to bone
        idx = ycm * xDim + xcm;
        while(sliceBuffer[idx] < 750) {
            xcm++;
            idx++;
        }
        // increment x until we get past the bone
        while(sliceBuffer[idx] > 750) {
            xcm++;
            idx++;
        }
        // use the seed point that is 5 more x units away from the bone
        seedX = (short)(xcm + 5);
        seedY = (short)ycm;
        seedZ = 0;
        seedVal = sliceBuffer[idx + 5];
        
        BitSet muscle2Bitmap = new BitSet();
        regionGrowMuscle(seedX, seedY, seedZ, seedVal, muscle2Bitmap);
        
        // get the VOI of the second bone marrow region
//        algoPaintToVOI.setPaintMask(muscle2Bitmap);
//        algoPaintToVOI.run();
        setCompleted(true);

        // copy the bone and bone marrow labels into the muscle bundle label image
        short[] boneBuffer;
        try {
            boneBuffer = new short [sliceSize];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: computeMuscleBundle()");
            return;
        }
        
        int sliceByteOffset;
        for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
            sliceByteOffset = sliceNum * sliceSize;
            try {
                boneMarrowImage.exportData((sliceNum * sliceSize), sliceSize, sliceBuffer);
                boneImage.exportData((sliceNum * sliceSize), sliceSize, boneBuffer);
            } catch (IOException ex) {
                System.err.println("Error exporting data");
            }
            
            idx = sliceByteOffset;
            for (int j = 0; j < yDim; j++) {
                for (int i = 0; i < xDim; i++, idx++) {
                    // put the bone into the muscleBundleImage
                    if (boneBuffer[idx - sliceByteOffset] > 0) {
                        sliceBuffer[idx - sliceByteOffset] = boneLabel;
                    }
                    // put the muscleBundle into the muscleBundleImage
                    if (muscle1Bitmap.get(idx)) {
                        sliceBuffer[idx - sliceByteOffset] = muscleBundleLabel;
                    }
                    if (muscle2Bitmap.get(idx)) {
                        sliceBuffer[idx - sliceByteOffset] = muscleBundleLabel;
                    }
                }
            }
            
//           try {
//                muscleBundleImage.importData((sliceNum * sliceSize), sliceBuffer, false);
//            } catch (IOException ex) {
//                System.err.println("Error importing data");
//            }
        } // end for (int sliceNum = 0; ...)
    } // end computeMuscleBundle(...)
   
    
   
   void regionGrowMuscle(short sX, short sY, short sZ, short seedVal, BitSet muscleBits) {
       try {
           AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(srcImage, 1.0f, 1.0f);

           regionGrowAlgo.setRunningInSeparateThread(false);

           if (boneImage.getNDims() == 2) {
               regionGrowAlgo.regionGrow2D(muscleBits, new Point(sX, sY), -1,
                                           false, false, null, seedVal - 300,
                                           seedVal + 1000, -1, -1, false);
           } else if (boneImage.getNDims() == 3) {
               CubeBounds regionGrowBounds;
               regionGrowBounds = new CubeBounds(xDim, 0, yDim, 0, zDim, 0);
               int count = regionGrowAlgo.regionGrow3D(muscleBits, new Point3D(sX, sY, sZ), -1,
                                                   false, false, null, seedVal - 300,
                                                   seedVal + 1000, -1, -1, false,
                                                   0, regionGrowBounds);
//               System.out.println("Muscle Count: " +count);
           }
       } catch (OutOfMemoryError error) {
           System.gc();
           MipavUtil.displayError("Out of memory: regionGrowMuscle");
       }

   } // regionGrowMuscle(...)
   
   
   
   // Bone marrow in CT images is "inside" the bone
   void segmentBoneMarrow() {
       
       // Detected bone regions are likely correct, find the marrow using a seeded region grow
       BitSet boneMarrow1Bitmap = new BitSet();
       
       // seed point is the center-of-mass of the bone on the first slice
       // center-of-masses were computed when we checked to see if the segmented bones were similar on each slice 
       regionGrow((short)x1CMs[0], (short)y1CMs[0], (short)0, boneMarrow1Bitmap);
       
       BitSet boneMarrow2Bitmap = new BitSet();
       regionGrow((short)x2CMs[0], (short)y2CMs[0], (short)0, boneMarrow2Bitmap);

       // make the boneMarrow label image slice by slice from the 3D region grown BitSet
       // bitSetIdx is a cumulative index into the 3D BitSet
       for (int bitSetIdx = 0, sliceNum = 0; sliceNum < zDim; sliceNum++) {
           for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, bitSetIdx++) {
               if (boneMarrow1Bitmap.get(bitSetIdx) || boneMarrow2Bitmap.get(bitSetIdx)) {
                   sliceBuffer[sliceIdx] = boneMarrowLabel;
               } else {
                   sliceBuffer[sliceIdx] = 0;
               }
           } // end for (int sliceIdx = 0; ...)
           
           // save the sliceBuffer into the boneMarrowImage
           try {
               boneMarrowImage.importData(sliceNum * sliceSize, sliceBuffer, false);
           } catch (IOException ex) {
               System.err.println("computeBoneMarrowImage(): Error importing data");
           }
       } // end for (int bitSetIdx = 0, sliceNum = 0; ...)
   } // end segmentBoneMarrow(...)
   
   
   
   // Bone marrow in CT images is "inside" the bone
   void computeBoneMarrowImageAndVOI() {

       short voiID = (short) boneMarrowImage.getVOIs().size();
       
       // Detected bone regions are likely correct, find the marrow using a seeded region grow
       BitSet boneMarrow1Bitmap = new BitSet();
       regionGrow((short)x1CMs[0], (short)y1CMs[0], (short)0, boneMarrow1Bitmap);
       
       // get the VOI of the first bone marrow region
       AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(boneMarrowImage,
               boneMarrow1Bitmap, xDim, yDim, zDim, voiID);

       algoPaintToVOI.setRunningInSeparateThread(false);
       algoPaintToVOI.run();
       setCompleted(true);

       BitSet boneMarrow2Bitmap = new BitSet();
       regionGrow((short)x2CMs[0], (short)y2CMs[0], (short)0, boneMarrow2Bitmap);

       // get the VOI of the second bone marrow region
       algoPaintToVOI.setPaintMask(boneMarrow2Bitmap);
       algoPaintToVOI.run();
       setCompleted(true);

       int sliceByteOffset;
       for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
           sliceByteOffset = sliceNum * sliceSize;
           // This copies the bone into the bone marrow image, let's not do that now
//           try {
//               boneImage.exportData(sliceByteOffset, sliceSize, sliceBuffer);
//           } catch (IOException ex) {
//               System.err.println("computeBoneMarrowImage(): Error exporting data");
//           }

           // clear the sliceBuffer
           for (int i = 0; i < sliceSize; i++) {
               sliceBuffer[i] = 0;
           }
           int idx = sliceByteOffset;
           for (int j = 0; j < yDim; j++) {
               for (int i = 0; i < xDim; i++, idx++) {
                   if (boneMarrow1Bitmap.get(idx)) {
                       sliceBuffer[idx - sliceByteOffset] = boneMarrowLabel;
                   }
                   if (boneMarrow2Bitmap.get(idx)) {
                       sliceBuffer[idx - sliceByteOffset] = boneMarrowLabel;
                   }
               }
           }
           
           try {
               boneMarrowImage.importData(sliceByteOffset, sliceBuffer, false);
           } catch (IOException ex) {
               System.err.println("computeBoneMarrowImage(): Error importing data");
           }
       } // end for (int sliceNum = 0; ...)
   } // end computeBoneMarrowImageAndVOI(...)

    
    
    void regionGrow(short seedX, short seedY, short seedZ, BitSet seedPaintBitmap) {
        try {
            AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(boneImage, 1.0f, 1.0f);

            regionGrowAlgo.setRunningInSeparateThread(false);

            if (boneImage.getNDims() == 2) {
                regionGrowAlgo.regionGrow2D(seedPaintBitmap, new Point(seedX, seedY), -1,
                        false, false, null, 0,
                        0, -1, -1, false);
            } else if (boneImage.getNDims() == 3) {
                CubeBounds regionGrowBounds;
                regionGrowBounds = new CubeBounds(xDim, 0, yDim, 0, zDim, 0);
                int count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3D(seedX, seedY, seedZ), -1,
                                                    false, false, null, 0,
                                                    0, -1, -1, false,
                                                    0, regionGrowBounds);
//                System.out.println("Count: " +count);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }

    } // end regionGrow()
    
    
    
    // return true if the average and standard deviation of the distance between the center-of-mass
    // for the two bone regions on each slice are "close" (within 10 pixels)
    boolean boneRegionsOK() {
        if (zDim == 0) return true;
        
        // compute the distance of adjacent CMs
        float[] distances1 = new float [zDim];
        float[] distances2 = new float [zDim - 1];
        float dx, dy;
        for (int sliceNum = 0; sliceNum < zDim - 1; sliceNum++) {
            // distance between CM of region 1
            dx = x1CMs[sliceNum] - x1CMs[sliceNum + 1];
            dy = y1CMs[sliceNum] - y1CMs[sliceNum + 1];
            distances1[sliceNum] = (float)Math.sqrt(dx*dx + dy*dy);

            // distance between CM of region 2
            dx = x2CMs[sliceNum] - x2CMs[sliceNum + 1];
            dy = y2CMs[sliceNum] - y2CMs[sliceNum + 1];
            distances2[sliceNum] = (float)Math.sqrt(dx*dx + dy*dy);
        } // end for (int sliceNum = 0; ...)
/*        
        System.out.println("distances between slices\n\tRegion 1\t\t\tRegion 2");
        for (int sliceNum = 0; sliceNum < zDim - 1; sliceNum++) {
            System.out.println(sliceNum +" - " +(sliceNum+1) +"\t" +distances1[sliceNum] +"\t\t\t\t" +distances2[sliceNum]);
        }
*/
        // compute mean and standard deviation of the distances
        float sum1 = 0.0f, sum2 = 0.0f;
        for (int sliceNum = 0; sliceNum < zDim - 1; sliceNum++) {
            sum1 += distances1[sliceNum];
            sum2 += distances2[sliceNum];
        } // end for (int sliceNum = 0; ...)
        float meanDistance1 = sum1 / (zDim - 1);
        float meanDistance2 = sum2 / (zDim - 1);
        
        System.out.println("mean dist:  " +meanDistance1 +"\t\tmean dist:  " +meanDistance2);

       sum1 = sum2 = 0.0f;
       for (int sliceNum = 0; sliceNum < zDim - 1; sliceNum++) {
           dx = distances1[sliceNum] - meanDistance1;
           sum1 += (dx * dx);

           dx = distances2[sliceNum] - meanDistance2;
           sum2 += (dx * dx);
       } // end for (int sliceNum = 0; ...)
       float stdDev1 = (float)Math.sqrt(sum1 / (zDim - 1));
       float stdDev2 = (float)Math.sqrt(sum2 / (zDim - 1));
       
       System.out.println("Std Dev:  " +stdDev1 +"\t\tStd Dev:  " +stdDev2);

       if (Math.abs(meanDistance1 - meanDistance2) > 10.0f ||
           Math.abs(stdDev1 - stdDev2) > 10.0f) {
           MipavUtil.displayError("boneRegionsOK() mean distance or std dev. of bone center-of-masses is greater than 10");
           return false;
       }
       
       // see that the distance between the center-of-mass for the two bones on each slice is close
       float maxDistance = 0.0f;
       for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
           // distance between CM of region 1
           dx = x1CMs[sliceNum] - x2CMs[sliceNum];
           dy = y1CMs[sliceNum] - y2CMs[sliceNum];
           distances1[sliceNum] = (float)Math.sqrt(dx*dx + dy*dy);
           if (distances1[sliceNum] > maxDistance) {
               maxDistance = distances1[sliceNum];
           }
       } // end for (int sliceNum = 0; ...)

       System.out.println("Distance between bone regions on each slice");
       for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
           System.out.println("slice number: " +sliceNum +"  distance: " +distances1[sliceNum]);
       }
       System.out.println("Maximum distance: " +maxDistance);

       // mean distance between the bones on each slice
       sum1 = 0.0f;
       for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
           sum1 += distances1[sliceNum];
       } // end for (int sliceNum = 0; ...)
       meanDistance1 = sum1 / zDim;
       System.out.println("Mean distance: " +meanDistance1);

       // standard deviation of the distance between the bones 
       sum1 = 0.0f;
       for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
           dx = distances1[sliceNum] - meanDistance1;
           sum1 += (dx * dx);
       } // end for (int sliceNum = 0; ...)
       stdDev1 = (float)Math.sqrt(sum1 / zDim);
       System.out.println("Std dev: " +stdDev1);

       // maxDistance between the bones on each slice should be close to the mean distance, so maxDistance
       // should be close to 0
       maxDistance -= meanDistance1;
       if ((maxDistance + 3.0f * stdDev1) > 30.0f) {
           MipavUtil.displayError("boneRegionsOK() (max - mean) distance + 3 * (std dev.) of bone is greater than 30");
           return false;
       }
       
       // Detected bone regions seems reasonable
       return true;
    } // end boneRegionsOK(...)
    
    
    
    void computeBoneCMs() {

        for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
            try {
                boneImage.exportData((sliceNum * sliceSize), sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("Error exporting data");
            }
            
            // find the center-of-mass of the two bones (average x and y location)
            int idx = 0, xSum1 = 0, ySum1 = 0, xSum2 = 0, ySum2 = 0, count1 = 0, count2 = 0;
            for (int j = 0; j < yDim; j++) {
                for (int i = 0; i < xDim; i++, idx++) {
                    if (sliceBuffer[idx] == 1) {
                        xSum1 += i;
                        ySum1 += j;
                        count1++;
                    } else if (sliceBuffer[idx] == 2) {
                        xSum2 += i;
                        ySum2 += j;
                        count2++;
                    }
                } // end for (int j = 0; ...)
            } // end for (int i = 0; ...)
            
            if (count1 == 0) {
                MipavUtil.displayError("computeBoneCMs() Could NOT find any pixels in the first bone");
            }
            if (count2 == 0) {
                MipavUtil.displayError("computeBoneCMs() Could NOT find any pixels in the second bone");
            }
            
            x1CMs[sliceNum] = xSum1 / count1;
            y1CMs[sliceNum] = ySum1 / count1;
            
            x2CMs[sliceNum] = xSum2 / count2;
            y2CMs[sliceNum] = ySum2 / count2;
            
            System.out.println(x1CMs[sliceNum] +" " +y1CMs[sliceNum] +"    " +x2CMs[sliceNum] +" " +y2CMs[sliceNum]);
         } // end for (int sliceNum = 0; ...)
    } // end computeBoneCMs(...)
    
    
    
    /**
     * Uses a fixed threshold range to identify bone in CT images
     */
    void segmentBone() {
        // thresholds for bone in CT images
        float[] thresholds = { 750.0f, 2000.0f };
        boneImage = threshold(srcImage, thresholds);
        
        // make the resolutions of the bone image the same as the source image
        for (int i = 0; i < zDim; i++) {
            boneImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[i].getResolutions());
        }

        // filter by cardinality.  Keep only connected objects that are about the size of the CT bones
        // bones should be 200 to 5000 pixels per slice
        
        if (srcImage.getNDims() == 2) {
            IDObjects2D(boneImage, 200 * zDim, 5000 * zDim);
        } else if (srcImage.getNDims() == 3) {
            IDObjects3D(boneImage, 200 * zDim, 5000 * zDim);
        }
        
        // make sure we only found 2 objects
        int numBones = (int)boneImage.getMax();
        if (numBones != 2) {
            MipavUtil.displayError("computeBoneImage() Did NOT find two leg bones!!!");
        }
        
        // One bone has a label value of 1 and the other has a value of 2
 
        // test to insure we got a reasonable bone segmentation

        // compute center-of-mass for each region on each slice
        computeBoneCMs();
 
        // compute statics about the center-of-mass for each region on each slice
        // and insures that the distance and standard deviations of the distances between
        // the center-of-mass for each region between each slice are "close"
        if (!boneRegionsOK()) {
            MipavUtil.displayError("Error segmentBone(), Bone segmentation error");
            return;
        }
    } // end segmentBone(...)
    
    
    
    /**
     * Uses a fixed threshold range to identify bone in CT images
     */
    void computeBoneImage() {
        // thresholds for bone in CT images
        float[] thresholds = { 750.0f, 2000.0f };
        boneImage = threshold(srcImage, thresholds);
        
        // make the resolutions of the bone image the same as the source image
        for (int i = 0; i < zDim; i++) {
            boneImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[i].getResolutions());
        }

        // filter by cardinality.  Keep only connected objects that are about the size of the CT bones
        // bones should be 200 to 5000 pixels per slice
        
        if (srcImage.getNDims() == 2) {
            IDObjects2D(boneImage, 200 * zDim, 5000 * zDim);
        } else if (srcImage.getNDims() == 3) {
            IDObjects3D(boneImage, 200 * zDim, 5000 * zDim);
        }
        
        // make sure we only found 2 objects
        int numBones = (int)boneImage.getMax();
        if (numBones != 2) {
            MipavUtil.displayError("computeBoneImage() Did NOT find two leg bones!!!");
        }
        
        // One bone has a label value of 1 and the other has a value of 2
        
        /**********************************************************
         * Nice idea, but the different label numbers given to the different bones are 
         * used in the boneMarrowImage function, so we better not change them now!!!

        // Give the identified bone objects the correct label value
        for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
            try {
                boneImage.exportData((sliceNum * sliceSize), sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("Error extractBone():  exporting data");
            }
            
            for (int idx = 0, j = 0; j < yDim; j++) {
                for (int i = 0; i < xDim; i++, idx++) {
                    if (sliceBuffer[idx] > 0) {
                        sliceBuffer[idx] = boneLabel;
                    }
                } // end for (int i = 0; ...)
            } // end for (int idx = 0, j = 0; ...)
            
            try {
                boneImage.importData((sliceNum * sliceSize), sliceBuffer, false);
            } catch (IOException ex) {
                System.err.println("Error importing data");
            }
        } // end for (int sliceNum = 0; ...)
 
        */
        
        // make sure we get a reasonable bone segmentation

        // compute center-of-mass for each region on each slice
        computeBoneCMs();
 
        // compute statics about the center-of-mass for each region on each slice
        // and insures that the distance and standard deviations of the distances between
        // the center-of-mass for each region between each slice are "close"
        if (!boneRegionsOK()) {
            MipavUtil.displayError("Error computeBoneImage(), Bone segmentation error");
            return;
        }

        // We will get the bone VOI when doing the muscleBundle, so 
/*
        // get the VOI of the bone regions
        computeLabelVOI(1);
        computeLabelVOI(2);

        
        // here is some stuff for dealing with VOI's
        VOIVector vois = boneImage.getVOIs();
        System.out.println("Number of VOIS: " + vois.size());

        VOI theVOI = vois.get(0);
        Vector[] curves = theVOI.getCurves();
        
        for (int i = 0; i < zDim; i++) {
            System.out.println("Slice: " + i + "   Number of curves: " + curves[i].size());
        }
        
        // the first curve on the first slice
        VOIContour curve1 = (VOIContour)curves[0].elementAt(0);
        System.out.println("1) Number of points: " + curve1.size());
//        for (int i = 0; i < curve1.size(); i++) {
//            System.out.println(+i +"  " +curve1.get(i));
//        }
        
        // the second curve on the first slice
        VOIContour curve2 = (VOIContour)curves[0].elementAt(1);
        System.out.println("2) Number of points: " + curve2.size());
        
        // the third curve on the first slice
        VOIContour curve3 = (VOIContour)curves[0].elementAt(2);
        System.out.println("3) Number of points: " + curve3.size());
        
        // the fourth curve on the first slice
        VOIContour curve4 = (VOIContour)curves[0].elementAt(3);
        System.out.println("4) Number of points: " + curve4.size());
*/
    } // end computeBoneImage(...)
    
    
    
    
    void computeLabelVOI(int seedVal) {
        // Simplest thing is to do a seeded region grow to set the BitSet
        boolean seedPointFound = false;
        short sx = 0, sy = 0, sz = 0;
        for (int sliceNum = 0; sliceNum < zDim && !seedPointFound; sliceNum++) {
            try {
                boneImage.exportData((sliceNum * sliceSize), sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("Error computeBoneImage():  exporting data");
            }
            
            for (int idx = 0, j = 0; j < yDim && !seedPointFound; j++) {
                for (int i = 0; i < xDim && !seedPointFound; i++, idx++) {
                    if (sliceBuffer[idx] == seedVal) {
                        seedPointFound = true;
                        sx = (short)i;
                        sy = (short)j;
                        sz = (short)sliceNum;
                    }
                } // end for (int i = 0; ...)
            } // end for (int idx = 0, j = 0; ...)
        } // end for (int sliceNum = 0; ...)

        // run the regiongrow algorithm for region 1
        BitSet boneBits = new BitSet();
        AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(boneImage, 1.0f, 1.0f);

        if (boneImage.getNDims() == 2) {
            regionGrowAlgo.regionGrow2D(boneBits, new Point(sx, sy), -1,
                                        false, false, null, seedVal,
                                        seedVal, -1, -1, false);
        } else if (boneImage.getNDims() == 3) {
            CubeBounds regionGrowBounds;
            regionGrowBounds = new CubeBounds(xDim, 0, yDim, 0, zDim, 0);
            int count = regionGrowAlgo.regionGrow3D(boneBits, new Point3D(sx, sy, sz), -1,
                                                false, false, null, seedVal,
                                                seedVal, -1, -1, false,
                                                0, regionGrowBounds);
//            System.out.println("Bone Count: " +count);
        }

        // extract the 
        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(boneImage,
                boneBits, xDim, yDim, zDim, (short)0);
        algoPaintToVOI.setRunningInSeparateThread(false);
        
        algoPaintToVOI.run();
        setCompleted(true);

    } // end computeLabelVOI(...)
    
    
 
    public ModelImage threshold(ModelImage threshSourceImg, float[] thresh) {
        ModelImage resultImage = null;
        resultImage = new ModelImage(ModelStorageBase.UBYTE, threshSourceImg.getExtents(), "threshResultImg");

        AlgorithmThresholdDual threshAlgo = null;
        threshAlgo = new AlgorithmThresholdDual(resultImage, threshSourceImg, thresh, boneLabel, AlgorithmThresholdDual.BINARY_TYPE, true, false);
        threshAlgo.run();

        return resultImage;
    } // end threshold(...)

    /**
     * morphological ID_OBJECTS.
     *
     * @param  srcImage  --source image
     * @param  min       --smallest object to let through
     * @param  max       --largest object to let through
     */
    public void IDObjects2D(ModelImage srcImage, int min, int max) {
        AlgorithmMorphology2D MorphIDObj = null;
        MorphIDObj = new AlgorithmMorphology2D(srcImage, 4, 1, AlgorithmMorphology2D.ID_OBJECTS, 0, 0, 0, 0, true);
        MorphIDObj.setMinMax(min, max);
        MorphIDObj.run();
    }

    /**
     * morphological ID_OBJECTS.
     *
     * @param  srcImage  --source image
     * @param  min       --smallest object to let through
     * @param  max       --largest object to let through
     */
    public void IDObjects3D(ModelImage srcImage, int min, int max) {
        AlgorithmMorphology3D MorphIDObj = null;
        MorphIDObj = new AlgorithmMorphology3D(srcImage, 4, 1, AlgorithmMorphology3D.ID_OBJECTS, 0, 0, 0, 0, true);
        MorphIDObj.setMinMax(min, max);
        MorphIDObj.run();
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
     * Starts the algorithm.    OLD VERSION
     */
    public void runAlgorithm01() {

        // first version of an algorithm to find the outer thigh boundary
            
            // This seems to be where I need to put the propagation and snake code
            /*
             * From JDialogSnake
             * ModelImage image = getActiveImage();
             * float[] sigmas = { 1.0, 1.0, something that includes z resolution
             * int boundaryIterations = 50;
             * smoothness = 2;
             * VOI srcVOI = image.getVOIs();
             * int boundaryDir = AlgorithmSnake.IN_DIR; 
             * snakeAlgo = new AlgorithmSnake(image, sigmas, boundaryIterations, smoothness, srcVOI, boundaryDir);
             * 
             * int propagationType = AlgorithmSnake.PROP_ALL
             * snakeAlgo.setPropagation(propagationType);
             * 
             * snakeAlgo.addListener(this);
             * 
             * if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
             *    MipavUtil.displayError("A thread is already running on this object");
             * }
             * 
             */ 
 
/*        
            VOI goodVOI = srcImage.getVOIs().get(0);
            float xRes = srcImage.getFileInfo(0).getResolutions()[0];
            float zRes = srcImage.getFileInfo(0).getResolutions()[2];
            float normFactor = xRes / zRes; // Calculate correction factor
            float[] sigmas = new float[3];
            sigmas[0] = 1.0f;
            sigmas[1] = 1.0f;
            sigmas[2] = 1.0f * normFactor;
            int boundaryIterations = 50;
            float smoothness = 2.0f;
            int boundaryDir = AlgorithmSnake.ANY_DIR;
            AlgorithmSnake snakeAlgo = new AlgorithmSnake(srcImage, sigmas, boundaryIterations, smoothness, goodVOI, boundaryDir);
            int propagationType = AlgorithmSnake.PROP_ALL;
            snakeAlgo.setPropagation(propagationType);
            if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
*/            
        // this propagates the selected VOI to all slices
//        srcImage.getParentFrame().getComponentImage().getVOIHandler().propVOIAll();
        
        // propagate the selected VOI back one slice
//        srcImage.getParentFrame().getComponentImage().getVOIHandler().propVOI(1, false);
        
        // propagate the selected VOI forward one slice (Doesn't work)
//        srcImage.getParentFrame().getComponentImage().getVOIHandler().propVOI(-1, false);
        
        /*
         * This is close to what I want, but this puts the propagated contours in the same VOI.
         * I really want different VOI's for different slices.  Furthermore, I would like to propagate
         * the VOI to a VOI on the next slice, snake the propagated VOI to better fit the thigh on
         * that slice, smooth the VOI, and propagate this modified VOI to the next slice.  Do that
         * for each slice with an index greater than the current slice number and then do it backward
         * through the slice to the first slice in the volume. 
         */
        
        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        float normFactor = xRes / zRes; // Calculate correction factor
        float[] sigmas = new float[3];
        sigmas[0] = 1.0f;
        sigmas[1] = 1.0f;
        sigmas[2] = 1.0f * normFactor;
        int boundaryIterations = 50;
        float smoothness = 2.0f;
        int boundaryDir = AlgorithmSnake.ANY_DIR;
        
//        int propagationType = AlgorithmSnake.PROP_ALL;
//        int propagationType = AlgorithmSnake.PROP_SINGLE;
        int propagationType = AlgorithmSnake.PROP_NEXT;
        
        // Read the thigh VOI from a file 
        srcImage.getParentFrame().openVOI(false, false);
       
        // Set the read VOI as active
        VOIVector newVOI = srcImage.getVOIs();
        VOI theVOI = newVOI.get(0);
        theVOI.setActive(true);
        theVOI.getCurvesTemp()[srcImage.getParentFrame().getViewableSlice()].elementAt(0).setActive(true);
        
        // show the active VOI
        srcImage.getParentFrame().updateImages(true);

        AlgorithmSnake snakeAlgo = new AlgorithmSnake(srcImage, sigmas, boundaryIterations, smoothness, theVOI, boundaryDir);
        snakeAlgo.setPropagation(propagationType);
        if (snakeAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }
        snakeAlgo.runAlgorithm();
        VOI resultVOI = snakeAlgo.getResultVOI();
        resultVOI.setActive(true);
        
        // we also need to set the contour (curve) on the current slice to active
        resultVOI.getCurvesTemp()[srcImage.getParentFrame().getViewableSlice()].elementAt(0).setActive(true);
        
        // Smooth the resultVOI to 60 points
        AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(srcImage, resultVOI, 60, true);
        smoothAlgo.runAlgorithm();
        VOI smoothResultVOI = smoothAlgo.getResultVOI();

        newVOI.set(0, smoothResultVOI);
               
        srcImage.getParentFrame().updateImages(true);

        
    } // end runAlgorithm01()

    
    private void calc3D_01() {
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        sliceSize = xDim * yDim;
        volumeSize = sliceSize * zDim;

        float [] buffer;
        int airLabelValue = 1;
        int fatLabelValue = 2;
        int partialVolumeLabelValue = 3;
        int muscleLabelValue = 4;
        int bloodLabelValue = 5;
        int boneLabelValue = 6;
        
        try {
            buffer = new float[volumeSize];
            srcImage.exportData(0, volumeSize, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("CT Thresholding Image: Image(s) locked", true);
            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("CT Thresholding Image: Out of Memory", true);
            return;
        }
        
        // threshold out some of the known tissues
        for (int i = 0; i < volumeSize; i++) {
            if (buffer[i] > 1000) buffer[i] = boneLabelValue;
            else if (buffer[i] >= 0 && buffer[i] <= 100) buffer[i] = muscleLabelValue;
            else if (buffer[i] >= -30 && buffer[i] <= 0) buffer[i] = partialVolumeLabelValue;
            else if (buffer[i] >= -190 && buffer[i] <= -30) buffer[i] =  fatLabelValue;
            else buffer[i] = 0;
        } // end for (int i = 0; ...)
        
        ModelImage thresholdImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), "CT Threshold");

        try {
            thresholdImage.importData(0, buffer, false);
        } catch (IOException ex) {
            System.err.println("error exporting data fto thresholdImage");
        }
       
        ShowImage(thresholdImage, "CT Bone");
    } // end calc3D_01()
    



}
