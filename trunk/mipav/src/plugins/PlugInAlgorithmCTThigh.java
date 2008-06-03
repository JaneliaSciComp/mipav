import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmRegionGrow;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtractionPaint;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.CubeBounds;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.Point3Ds;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Point;
import java.io.IOException;
import java.util.BitSet;



public class PlugInAlgorithmCTThigh extends AlgorithmBase {
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Z dimension of the CT image */
    private int zDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;

    private ModelImage boneImage;
    private ModelImage thighTissueImage;
    // center-of-mass array for region 1 and 2 (the thresholded bone)
    private int[] x1CMs;
    private int[] y1CMs;
    private int[] x2CMs;
    private int[] y2CMs;
    
    // temp buffer to store slices.  Needed in many member functions.
    private short[] sliceBuffer;

    private short thighTissueLabel = 10;
    private short boneLabel = 3;
    
    private boolean initializedFlag = false;
    
    private BitSet volumeBitSet;

    /**The final left thigh VOI*/
    private VOI leftThighVOI;
    
    /**The final right thigh VOI*/
    private VOI rightThighVOI;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmCTThigh(ModelImage resultImage, ModelImage srcImg) {
        super(resultImage, srcImg);
        
        leftThighVOI = null;
        rightThighVOI = null;
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
    private void init() {
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
        if (srcImage.getNDims() == 2) 
            zDim = 1;
        else if (srcImage.getNDims() == 3) 
            zDim = srcImage.getExtents()[2];

        // make the label images and initialize their resolutions
        thighTissueImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "thighTissueImage");
//        muscleBundleImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "muscleBundleImage");
       
        // make the resolutions of the images the same as the source image
        for (int i = 0; i < zDim; i++) {
            thighTissueImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[i].getResolutions());
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

        time = System.currentTimeMillis();
        segmentThighTissue();
        System.out.println("Thigh tissue segmentation: "+(System.currentTimeMillis() - time));

        time = System.currentTimeMillis();
        VOI totalVOI = makeThighTissueVOI();
        System.out.println("Thigh tissue VOIs: "+(System.currentTimeMillis() - time));
        ShowImage(thighTissueImage, "Thigh tissue");
        
        rightThighVOI = makeRightThighVOI(totalVOI);
        leftThighVOI = makeLeftThighVOI(totalVOI);
        
        //boneImage.unregisterAllVOIs();
        //boneImage.registerVOI(rightThighVOI);
        //boneImage.registerVOI(leftThighVOI);
        
        //boneImage.getParentFrame().updateImages(true);
        
     // save the VOI to a file(s)
        String directory = System.getProperty("user.dir");
        System.out.println("directory: " +directory);
        FileVOI fileVOI;
        
        String fileName = "Right Thigh.xml";
        try {
            fileVOI = new FileVOI(fileName, directory, boneImage);
            fileVOI.writeVOI(rightThighVOI, true);
            fileName = "Left Thigh.xml";
            fileVOI = new FileVOI(fileName, directory, boneImage);
            fileVOI.writeVOI(leftThighVOI, true);
        } catch (IOException ex) {
            System.err.println("Error segmentImage():  Opening VOI file");
            return;
        }     

   } // end segmentImage()
    
    /**
     * Produces left thigh VOI.
     * @param totalVOI VOI of both thighs (inside and outside)
     * @return multi-curve VOI
     */
    private VOI makeLeftThighVOI(VOI totalVOI) {
    	VOI tempVOI = (VOI)totalVOI.clone();
    	int size = 0;
    	for(int i=0; i<zDim; i++) {
    		size = tempVOI.getCurves()[i].size();
    		for(int j=1; j<size; j++)
    			tempVOI.removeCurve(1, i);
    	}
    	tempVOI.setName("Left Thigh");
    	return tempVOI;
    }
    
    /**
     * Produces right thigh VOI.
     * @param totalVOI VOI of both thighs (inside and outside)
     * @return multi-curve VOI
     */
    private VOI makeRightThighVOI(VOI totalVOI) {
    	VOI tempVOI = (VOI)totalVOI.clone();
    	int size = 0;
    	for(int i=0; i<zDim; i++) {
    		size = tempVOI.getCurves()[i].size();
    		for(int j=0; j<size-1; j++) {
    			if(j == 0)
    				tempVOI.removeCurve(0, i);
    			else 
    				tempVOI.removeCurve(1, i);
    		}
    	}
    	tempVOI.setName("Right Thigh");
    	return tempVOI;
    }

	    

	// create a voi for the bone.  Assumes the boneImage has been created.
    private VOI makeThighTissueVOI() {
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
            return null;
        }
        VOI theVOI = vois.get(0);
        theVOI.setName("Thigh Tissue");
        if (theVOI.getCurves()[0].size() != 4) {
            MipavUtil.displayError("makeThighTissueVOI() Error, did not get 2 curves in the VOI");
            return null;
        }
        return theVOI;

    } // end makeThighTissueVOI()
    
    
    private void segmentThighTissue() {
         
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
    
    
    
    private void regionGrowMuscle(short sX, short sY, short sZ, short seedVal, BitSet muscleBits) {
       try {
           AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(srcImage, 1.0f, 1.0f);

           regionGrowAlgo.setRunningInSeparateThread(false);

           if (srcImage.getNDims() == 2) {
               regionGrowAlgo.regionGrow2D(muscleBits, new Point(sX, sY), -1,
                                           false, false, null, seedVal - 300,
                                           seedVal + 1000, -1, -1, false);
           } else if (srcImage.getNDims() == 3) {
               CubeBounds regionGrowBounds;
               regionGrowBounds = new CubeBounds(xDim, 0, yDim, 0, zDim, 0);
               regionGrowAlgo.regionGrow3D(muscleBits, new Point3Ds(sX, sY, sZ), -1,
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
   
   
   
   private void computeBoneCMs() {

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
    
    
    
    /**
     * Uses a fixed threshold range to identify bone in CT images
     */
    private void segmentBone() {
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
        /*if (!boneRegionsOK()) {
            MipavUtil.displayError("Error segmentBone(), Bone segmentation error");
            return;
        }*/
    } // end segmentBone(...)
    
    
    
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



	public VOI getLeftThighVOI() {
		return leftThighVOI;
	}



	public VOI getRightThighVOI() {
		return rightThighVOI;
	}
    



}
