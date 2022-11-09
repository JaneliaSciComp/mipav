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
import gov.nih.mipav.model.algorithms.AlgorithmRegionGrow;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtractionPaint;

import gov.nih.mipav.model.structures.CubeBounds;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.Point3D;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJFrameMessage;
import gov.nih.mipav.view.ViewUserInterface;

import java.awt.Color;
import java.awt.Point;
import java.io.File;
import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;


public class PlugInAlgorithmCTMarrow542a extends AlgorithmBase {
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Z dimension of the CT image */
    private int zDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;
    
    private ModelImage boneImage;
    private ModelImage boneMarrowImage;
    // center-of-mass array for region 1 and 2 (the thresholded bone)
    private int[] x1CMs;
    private int[] y1CMs;
    private int[] x2CMs;
    private int[] y2CMs;
    
    // temp buffer to store slices.  Needed in many member functions.
    private short[] sliceBuffer;

    private short boneMarrowLabel = 20;
    private short boneLabel = 3;
    
    private boolean initializedFlag = false;
    
    private BitSet volumeBitSet;
    
    /**The final left marrow VOI*/
    private VOI leftMarrowVOI;
    
    /**The final right marrow VOI*/
    private VOI rightMarrowVOI;

    private String imageDir;
    
    private Color voiColor;
    
    private long segmentationTimeMarrow = 0;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmCTMarrow542a(ModelImage resultImage, ModelImage srcImg, String imageDir, Color color) {
        super(resultImage, srcImg);
        
        this.imageDir = imageDir+File.separator;
        this.voiColor = color;
        
        leftMarrowVOI = null;
        rightMarrowVOI = null;
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
        boneMarrowImage  = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "boneMarrowImage");
       
        // make the resolutions of the images the same as the source image
        for (int i = 0; i < zDim; i++) {
            boneMarrowImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[i].getResolutions());
        }
        
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
        doVOI = segmentBone();
        if(doVOI)
        	doVOI = segmentBoneMarrow();
        ViewUserInterface.getReference().getMessageFrame().append("Bone marrow segmentation: "+(System.currentTimeMillis() - time)+"\n", ViewJFrameMessage.DEBUG);

        time = System.currentTimeMillis();
        VOI totalVOI = null;
        if(doVOI)
        	totalVOI = makeBoneMarrowVOI();
        
        if(totalVOI != null) {
	        //ShowImage(boneMarrowImage, "boneImage");
	        ViewUserInterface.getReference().getMessageFrame().append("Bone/Bone marrow VOIs: "+(System.currentTimeMillis() - time)+"\n", ViewJFrameMessage.DEBUG);
	
	        rightMarrowVOI = makeRightMarrowVOI(totalVOI);
	        leftMarrowVOI = makeLeftMarrowVOI(totalVOI);
	        
	        // Right leg marrow VOI is the left most in the image
            int[] rightBoundsX = new int [2];
            int[] rightBoundsY = new int [2];
            int[] rightBoundsZ = new int [2];
            VOIContour rightCurve;
            rightCurve = ((VOIContour)rightMarrowVOI.getCurves().get(0));
            rightCurve.getBounds(rightBoundsX, rightBoundsY, rightBoundsZ);
        
            int[] leftBoundsX = new int [2];
            int[] leftBoundsY = new int [2];
            int[] leftBoundsZ = new int [2];
            VOIContour leftCurve;
            leftCurve = ((VOIContour)leftMarrowVOI.getCurves().get(0));
            leftCurve.getBounds(leftBoundsX, leftBoundsY, leftBoundsZ);
            
          //rightX should be to the LEFT of leftx in this orientation
            float rightX = ((VOIContour)rightMarrowVOI.getCurves().get(0)).get(0).X;
            float leftX = ((VOIContour)leftMarrowVOI.getCurves().get(0)).get(0).X;
            
            // the rightBoneVOI should be the leftmost
            if (rightX > leftX) {
                VOI tmp = rightMarrowVOI;
                rightMarrowVOI = leftMarrowVOI;
                leftMarrowVOI = tmp;
            }
            
            rightMarrowVOI.setName("Right Marrow");
            leftMarrowVOI.setName("Left Marrow");
            segmentationTimeMarrow = (System.currentTimeMillis() - totalTime);
            System.err.println("Total time for marrow segmentation: "+segmentationTimeMarrow);
	        
	     // save the VOI to a file(s)
	        ViewUserInterface.getReference().getMessageFrame().append("directory: " +imageDir+"\n", ViewJFrameMessage.DEBUG);
	        ViewUserInterface.getReference().getMessageFrame().append("directory: " +imageDir+"\n", ViewJFrameMessage.DEBUG);
	        ViewJFrameImage frame = new ViewJFrameImage(srcImage);
	    	srcImage.unregisterAllVOIs();
	    	srcImage.registerVOI(rightMarrowVOI);
	    	srcImage.registerVOI(leftMarrowVOI);
	    	frame.saveAllVOIsTo(imageDir);
	    	frame.setVisible(false);
	    	frame.dispose();
        } else
        	System.err.println("Automatic VOIs not created");
   } // end segmentImage()
    
    /**
     * Produces left marrow VOI.
     * @param totalVOI VOI of both marrow (left and right)
     * @return multi-curve VOI
     */
    private VOI makeLeftMarrowVOI(VOI totalVOI) {
    	VOI tempVOI = (VOI)totalVOI.clone();
        Vector<VOIBase>[] curves = totalVOI.getSortedCurves(zDim);
    	for(int i=0; i<zDim; i++) 
    		tempVOI.getCurves().removeElement( curves[i].elementAt(1) );
    	tempVOI.setName("Left Marrow");
    	tempVOI.setColor(voiColor);
    	return tempVOI;
    }
    
    /**
     * Produces right marrow VOI.
     * @param totalVOI VOI of both marrow (left and right)
     * @return multi-curve VOI
     */
    private VOI makeRightMarrowVOI(VOI totalVOI) {
    	VOI tempVOI = (VOI)totalVOI.clone();
        Vector<VOIBase>[] curves = totalVOI.getSortedCurves(zDim);
    	for(int i=0; i<zDim; i++) 
            tempVOI.getCurves().removeElement( curves[i].elementAt(0) );
    	tempVOI.setName("Right Marrow");
    	tempVOI.setColor(voiColor);
    	return tempVOI;
    }
    
    
    // create a voi for the bone marrow.  Assumes the boneMarrowImage has been created.
    private VOI makeBoneMarrowVOI() {
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
            return null;
        }
        VOI theVOI = vois.get(0);
        theVOI.setName("Bone Marrow");
        if (theVOI.getCurves().size()/zDim != 2) { // 2 curves per slice expected
            MipavUtil.displayError("makeBoneMarrowVOI() Error, did not get 2 curves in the VOI");
            return null;
        }
        return theVOI;
    } // end makeBoneMarrowVOI()
    
    
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
	        System.err.println("computeBoneImage() Did NOT find two leg bones!!!");
	        return false;
	    }
	    // compute center-of-mass for each region on each slice
	    return computeBoneCMs();
	} // end segmentBone(...)



	// Bone marrow in CT images is "inside" the bone
    private boolean segmentBoneMarrow() {
	   float[] thresholds = { 750.0f, 2000.0f };
       boneImage = threshold(srcImage, thresholds);
	   
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
               return false;
           }
       } // end for (int bitSetIdx = 0, sliceNum = 0; ...)
       return true;
    } // end segmentBoneMarrow(...)
   
   
   
   private void regionGrow(short seedX, short seedY, short seedZ, BitSet seedPaintBitmap) {
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
                regionGrowAlgo.regionGrow3D(seedPaintBitmap, new Point3D(seedX, seedY, seedZ), -1,
                                                    false, false, null, 0,
                                                    0, -1, -1, false,
                                                    0, regionGrowBounds);
//                ViewUserInterface.getReference().getMessageFrame().append("Count: " +count+"\n", ViewJFrameMessage.DEBUG);
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("Out of memory: ComponentEditImage.regionGrow");
        }

    } // end regionGrow()
    
    
    
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
    
	public VOI getLeftMarrowVOI() {
		return leftMarrowVOI;
	}



	public VOI getRightMarrowVOI() {
		return rightMarrowVOI;
	}



	public long getSegmentationTimeMarrow() {
		return segmentationTimeMarrow;
	}
    



}
