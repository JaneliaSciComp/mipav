//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtractionPaint;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;



public class PlugInAlgorithmCTBone542a extends AlgorithmBase {
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Z dimension of the CT image */
    private int zDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;

    private ModelImage boneImage;
    
    // center-of-mass array for region 1 and 2 (the thresholded bone)
    private int[] x1CMs;
    private int[] y1CMs;
    private int[] x2CMs;
    private int[] y2CMs;
    
    // temp buffer to store slices.  Needed in many member functions.
    private short[] sliceBuffer;

    private short boneLabel = 3;
    
    private boolean initializedFlag = false;
    
    private BitSet volumeBitSet;
    
    private String imageDir;

    /**The final left outside bone VOI*/
    private VOI leftBoneVOI;
    
    /**The final right outside bone VOI*/
    private VOI rightBoneVOI;
    
    private Color voiColor;
    
    private long segmentationTimeBone = 0;
    
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmCTBone542a(ModelImage resultImage, ModelImage srcImg, String imageDir, Color color) {
        super(resultImage, srcImg);
        
        this.imageDir = imageDir+File.separator;
        this.voiColor = color;
        
        leftBoneVOI = null;
        rightBoneVOI = null;
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
        
        volumeBitSet = new BitSet();
               
        // set initialized flag to true so the data structures are not reallocated
        initializedFlag = true;
    } // end init()
    
    
    /**
     * Find the bone, bone marrow, and the thigh tissue
     */
    private void segmentImage() {
        long totalTime = System.currentTimeMillis();
    	long time = System.currentTimeMillis();
        boolean doVOI = false;
        // compute the bone label image
        doVOI =  segmentBone();
        ViewUserInterface.getReference().getMessageFrame().append("Bone segmentation: "+(System.currentTimeMillis() - time)+"\n", ViewJFrameMessage.DEBUG);

        time = System.currentTimeMillis();
        VOI totalVOI = null;
        if(doVOI)
        	totalVOI = makeBoneVOI();
        ViewUserInterface.getReference().getMessageFrame().append("Bone/Bone marrow VOIs: "+(System.currentTimeMillis() - time)+"\n", ViewJFrameMessage.DEBUG);
        if(totalVOI != null) {
        	rightBoneVOI = makeRightBoneVOI(totalVOI);
        	leftBoneVOI = makeLeftBoneVOI(totalVOI);
        	
            int[] rightBoundsX = new int [2];
            int[] rightBoundsY = new int [2];
            int[] rightBoundsZ = new int [2];
            VOIContour rightCurve;
            rightCurve = ((VOIContour)rightBoneVOI.getCurves().get(0));
            rightCurve.getBounds(rightBoundsX, rightBoundsY, rightBoundsZ);
        
            int[] leftBoundsX = new int [2];
            int[] leftBoundsY = new int [2];
            int[] leftBoundsZ = new int [2];
            VOIContour leftCurve;
            leftCurve = ((VOIContour)leftBoneVOI.getCurves().get(0));
            leftCurve.getBounds(leftBoundsX, leftBoundsY, leftBoundsZ);
            
            //rightX should be to the LEFT of leftx in this orientation
            float rightX = ((VOIContour)rightBoneVOI.getCurves().get(0)).get(0).X;
            float leftX = ((VOIContour)leftBoneVOI.getCurves().get(0)).get(0).X;
            
            // the rightBoneVOI should be the leftmost
            if (rightX > leftX) {
                VOI tmp = rightBoneVOI;
                rightBoneVOI = leftBoneVOI;
                leftBoneVOI = tmp;
            }
            
            rightBoneVOI.setName("Right Bone");
            leftBoneVOI.setName("Left Bone");
            segmentationTimeBone = (System.currentTimeMillis() - totalTime);
            System.err.println("Total time for bone segmentation: "+segmentationTimeBone);
	        
	        // save the VOI to a file(s)
	        ViewUserInterface.getReference().getMessageFrame().append("directory: " +imageDir+"\n", ViewJFrameMessage.DEBUG);
	        ViewJFrameImage frame = new ViewJFrameImage(srcImage);
	    	srcImage.unregisterAllVOIs();
	    	srcImage.registerVOI(rightBoneVOI);
	    	srcImage.registerVOI(leftBoneVOI);
	    	frame.saveAllVOIsTo(imageDir);
	    	frame.dispose();
        } else
        	System.err.println("No automatic VOI created");
        
   } // end segmentImage()
    
    /**
     * Produces left outside bone VOI.
     * @param totalVOI VOI of both bones (inside and outside)
     * @return multi-curve VOI
     */
    private VOI makeRightBoneVOI(VOI totalVOI) {
    	VOI tempVOI = (VOI)totalVOI.clone();
    	Vector<VOIBase>[] curves = totalVOI.getSortedCurves(zDim);
    	for(int i=0; i<zDim; i++) {
    		for(int j=1; j<curves[i].size(); j++)
    			tempVOI.getCurves().removeElement( curves[i].elementAt(j) );
    	}
    	tempVOI.setName("Right Bone");
    	tempVOI.setColor(voiColor);
    	return tempVOI;
    }
    
    /**
     * Produces right outside bone VOI.
     * @param totalVOI VOI of both bones (inside and outside)
     * @return multi-curve VOI
     */
    private VOI makeLeftBoneVOI(VOI totalVOI) {
    	VOI tempVOI = (VOI)totalVOI.clone();
        Vector<VOIBase>[] curves = totalVOI.getSortedCurves(zDim);
    	for(int i=0; i<zDim; i++) {
            for(int j=0; j<curves[i].size(); j++) {
                if ( j != 1 )
                    tempVOI.getCurves().removeElement( curves[i].elementAt(j) );
    		}
    	}
    	tempVOI.setName("Left Bone");
    	tempVOI.setColor(voiColor);
    	return tempVOI;
    }
    
    
    // create a voi for the bone.  Assumes the boneImage has been created.
    private VOI makeBoneVOI() {
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
            System.err.println("makeBoneVOI() Error, did not get 1 VOI");
            return null;
        }
        VOI theVOI = vois.get(0);
        theVOI.setName("Bone");
        if (theVOI.getCurves().size()/zDim != 4) { //4 curves per slice expected
        	System.err.println("makeBoneVOI() Error, did not get 4 curves in the VOI.  Expected 1 outside and 1 inside for both legs.");
            return null;
        }
        
        return theVOI;
    } // end makeBoneVOI()
    
    
    
    // return true if the average and standard deviation of the distance between the center-of-mass
    // for the two bone regions on each slice are "close" (within 10 pixels)
    private boolean boneRegionsOK() {
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
       if ((maxDistance + 3.0f * stdDev1) > 30.0f) {
    	   System.err.println("boneRegionsOK() (max - mean) distance + 3 * (std dev.) of bone is greater than 30");
    	   return false;
       }
       // Detected bone regions seems reasonable
       return true;
    } // end boneRegionsOK(...)
    
    
    
    private boolean computeBoneCMs() {

        for (int sliceNum = 0; sliceNum < zDim; sliceNum++) {
            try {
                boneImage.exportData((sliceNum * sliceSize), sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("Error exporting data");
                return false;
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
                System.err.println("computeBoneCMs() Could NOT find any pixels in the first bone");
                return false;
            }
            if (count2 == 0) {
            	System.err.println("computeBoneCMs() Could NOT find any pixels in the second bone");
            	return false;
            }
            
            x1CMs[sliceNum] = xSum1 / count1;
            y1CMs[sliceNum] = ySum1 / count1;
            
            x2CMs[sliceNum] = xSum2 / count2;
            y2CMs[sliceNum] = ySum2 / count2;
         } // end for (int sliceNum = 0; ...)
        return true;
    } // end computeBoneCMs(...)
    
    
    
    /**
     * Uses a fixed threshold range to identify bone in CT images
     */
    private boolean segmentBone() {
    	
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
            System.err.println("computeBoneImage() Did NOT find two leg bones");
            return false;
        }
        
        // One bone has a label value of 1 and the other has a value of 2
        // test to insure we got a reasonable bone segmentation
        // compute center-of-mass for each region on each slice
        if(!computeBoneCMs())
        	return false;
        // compute statics about the center-of-mass for each region on each slice
        // and insures that the distance and standard deviations of the distances between
        // the center-of-mass for each region between each slice are "close"
        if (!boneRegionsOK()) {
            System.err.println("Error segmentBone(), Bone segmentation error");
            return false;
        }
        return true;
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

    
    public VOI getLeftBoneVOI() {
		return leftBoneVOI;
	}



	public VOI getRightBoneVOI() {
		return rightBoneVOI;
	}



	public long getSegmentationTimeBone() {
		return segmentationTimeBone;
	}
    



}
