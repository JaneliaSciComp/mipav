import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmRegionGrow;
import gov.nih.mipav.model.algorithms.AlgorithmSnake;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.CubeBounds;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.Point3Ds;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameTriImage;
import gov.nih.mipav.view.ViewVOIVector;
import gov.nih.mipav.view.dialogs.JDialogPaintGrow;

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
    
    ModelImage boneImage;
    ModelImage boneMarrowImage;
    ModelImage muscleBundleImage;
    
    // center-of-mass array for region 1 and 2 (the thresholded bone)
    int[] x1CMs;
    int[] y1CMs;
    int[] x2CMs;
    int[] y2CMs;
    
    // temp buffer to store slices.  Needed in many member functions.
    short[] sliceBuffer;

    // unique label values for the regions of interest
    short muscleBundleLabel = 10;
    short boneMarrowLabel = 20;
    short boneLabel = 3;

    
    
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
        // Algorithm to determine the outer thigh boundary and boundaries of the bone
        
        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() == 3) {
            calc3D();                
        }
 
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
    
    private void calc2D() {
        
        // compute the necessary fields
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = 1;

        sliceSize = xDim * yDim;
        
        sliceBuffer = new short[sliceSize];
        
        x1CMs = new int [sliceSize];
        y1CMs = new int [sliceSize];
        x2CMs = new int [sliceSize];
        y2CMs = new int [sliceSize];

        computeBoneImage();
//        ShowImage(boneImage, "CT Bone");

        computeBoneMarrowImage();
//        ShowImage(boneMarrowImage, "CT Bone Marrow");
        
        computeMuscleBundle();
        ShowImage(muscleBundleImage, "Segmented Image");
        
   }
    
    private void calc3D() {
        
        // compute the necessary fields
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];

        sliceSize = xDim * yDim;
        volumeSize = sliceSize * zDim;
        
        sliceBuffer = new short[sliceSize];
        
        x1CMs = new int [sliceSize];
        y1CMs = new int [sliceSize];
        x2CMs = new int [sliceSize];
        y2CMs = new int [sliceSize];

        computeBoneImage();
//        ShowImage(boneImage, "CT Bone");

        computeBoneMarrowImage();
//        ShowImage(boneMarrowImage, "CT Bone Marrow");
        
        computeMuscleBundle();
        ShowImage(muscleBundleImage, "Segmented Image");
        
    } // end calc3D()

    
    
   void computeMuscleBundle() {
       muscleBundleImage = new ModelImage(boneMarrowImage.getType(), boneMarrowImage.getExtents(), "MuscleBundleImg");
       
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
       
       
       int sliceByteOffset;
       for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
           sliceByteOffset = sliceNum * sliceSize;
           try {
               boneMarrowImage.exportData((sliceNum * sliceSize), sliceSize, sliceBuffer);
           } catch (IOException ex) {
               System.err.println("Error exporting data");
           }
           
           idx = sliceByteOffset;
           for (int j = 0; j < yDim; j++) {
               for (int i = 0; i < xDim; i++, idx++) {
                   if (muscle1Bitmap.get(idx)) {
                       sliceBuffer[idx - sliceByteOffset] = muscleBundleLabel;
                   }
                   if (muscle2Bitmap.get(idx)) {
                       sliceBuffer[idx - sliceByteOffset] = muscleBundleLabel;
                   }
               }
           }
           
           try {
               muscleBundleImage.importData((sliceNum * sliceSize), sliceBuffer, false);
           } catch (IOException ex) {
               System.err.println("Error importing data");
           }
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
               int count = regionGrowAlgo.regionGrow3D(muscleBits, new Point3Ds(sX, sY, sZ), -1,
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
    void computeBoneMarrowImage() {
        boneMarrowImage = new ModelImage(srcImage.getType(), srcImage.getExtents(), "boneMarrowImg");
 
        // Detected bone regions are likely correct, find the marrow using a seeded region grow
        BitSet boneMarrow1Bitmap = new BitSet();
        regionGrow((short)x1CMs[0], (short)y1CMs[0], (short)0, boneMarrow1Bitmap);

        BitSet boneMarrow2Bitmap = new BitSet();
        regionGrow((short)x2CMs[0], (short)y2CMs[0], (short)0, boneMarrow2Bitmap);
       
        int sliceByteOffset;
        for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
            sliceByteOffset = sliceNum * sliceSize;
            try {
                boneImage.exportData((sliceNum * sliceSize), sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("computeBoneMarrowImage(): Error exporting data");
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
                boneMarrowImage.importData((sliceNum * sliceSize), sliceBuffer, false);
            } catch (IOException ex) {
                System.err.println("computeBoneMarrowImage(): Error importing data");
            }
        } // end for (int sliceNum = 0; ...)
    } // end computeBoneMarrowImage(...)

    
    
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
                int count = regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3Ds(seedX, seedY, seedZ), -1,
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
        
        // compute mean and standard deviation of the distances
        float sum1 = 0.0f, sum2 = 0.0f;
        for (int sliceNum = 0; sliceNum < zDim - 1; sliceNum++) {
            sum1 += distances1[sliceNum];
            sum2 += distances2[sliceNum];
        } // end for (int sliceNum = 0; ...)
        float meanDistance1 = sum1 / (zDim - 1);
        float meanDistance2 = sum2 / (zDim - 1);
        
       sum1 = sum2 = 0.0f;
       for (int sliceNum = 0; sliceNum < zDim - 1; sliceNum++) {
           dx = distances1[sliceNum] - meanDistance1;
           sum1 += (dx * dx);

           dx = distances2[sliceNum] - meanDistance2;
           sum2 += (dx * dx);
       } // end for (int sliceNum = 0; ...)
       float stdDev1 = (float)Math.sqrt(sum1 / (zDim - 1));
       float stdDev2 = (float)Math.sqrt(sum2 / (zDim - 1));
       
       if (Math.abs(meanDistance1 - meanDistance2) > 10.0f ||
           Math.abs(stdDev1 - stdDev2) > 10.0f) {
        return false;
       }
       
       // see that the distance between the center-of-mass for the two bones is close
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

       // mean distance between the bones on each slice
       sum1 = 0.0f;
       for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
           sum1 += distances1[sliceNum];
       } // end for (int sliceNum = 0; ...)
       meanDistance1 = sum1 / zDim;

       // standard deviation of the distance between the bones 
       sum1 = 0.0f;
       for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
           dx = distances1[sliceNum] - meanDistance1;
           sum1 += (dx * dx);
       } // end for (int sliceNum = 0; ...)
       stdDev1 = (float)Math.sqrt(sum1 / zDim);

       // maxDistance between the bones on each slice should be close to the mean distance, so maxDistance
       // should be close to 0
       maxDistance -= meanDistance1;
       if ((maxDistance + 3.0f * stdDev1) > 10.0f) {
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
         } // end for (int sliceNum = 0; ...)
    } // end computeBoneCMs(...)
    
    
    
    
    void computeBoneImage() {
        // thresholds for bone in CT images
        float[] thresholds = { 750.0f, 2000.0f };
        boneImage = threshold(srcImage, thresholds);
        
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
        // the center-of-mass for each region between each slice is "close"
        if (!boneRegionsOK()) {
            MipavUtil.displayError("Error computeBoneImage(), Bone segmentation error");
            return;
        }

    } // end computeBoneImage(...)
    
    
 
    public ModelImage threshold(ModelImage threshSourceImg, float[] thresh) {
        ModelImage resultImage = null;
        resultImage = new ModelImage(threshSourceImg.getType(), threshSourceImg.getExtents(), "threshResultImg");

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
        theVOI.getCurves()[srcImage.getParentFrame().getViewableSlice()].elementAt(0).setActive(true);
        
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
        resultVOI.getCurves()[srcImage.getParentFrame().getViewableSlice()].elementAt(0).setActive(true);
        
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
