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

import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmRegionGrow;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtraction;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtractionPaint;
import gov.nih.mipav.model.algorithms.AlgorithmSnake;
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
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Vector;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * PlugIn to automatically segment the exterior abdominal boundary and the internal
 * boundary of the subcutaneous fat in 2D and 3D abdominal CT images.  3D images
 * are processed as a series of 2D slices.  So they are processed as 2.5D images.
 * 
 * The algorithm implemented in this plugin is our version of an algorithm described 
 * in: 
 * "Automated Quantification of Body Fat Distribution on Volumetric Computed Tomography",
 * by Zhao B, Colville J, Kalaigian J, Curran S, Jiang L, Kijewski P, Schwartz LH.,
 * J Comput Assist Tomogr. 2006 Sep-Oct;30(5):777-83.
 * 
 * @author hemlerp
 * @date June 2008
 *
 */
public class PlugInAlgorith1mCTAbdomen542a extends AlgorithmBase implements AlgorithmInterface {
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Z dimension of the CT image */
    private int zDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;
    
    /** temporary buffer to store slice data.  A buffer like this is needed by
     * many member functions, so lets allocate it once and use it frequently.
    */
    private short[] sliceBuffer;

    /** A label image for the abdominal region */
    private ModelImage abdomenImage;
    
    /** center-of-mass array for the segmented abdomen */
    private short[] centerOfMass;
    
    private int angularResolution = 1;
    
    /** flag for finding the center of mass */
    boolean foundCenterOfMass = false;

    /** a label value for the segmented abdomen */
    private short abdomenTissueLabel = 10;
    
    /** The threshold value for muscle */
    private int muscleThresholdHU = 0;

    /** The threshold value for air */
    private short airThresholdHU = -200;
    
    /** flag to insure all buffers are allocated before they are used */
    private boolean initializedFlag = false;
    
    /** A 2D BitSet for use in the region grow algorithm */
    private BitSet volumeBitSet;

    /** The final abdomen VOI*/
    private VOI abdomenVOI;

    /** The final subcutaneous fat VOI*/
    private VOI subcutaneousVOI;

    /** Image directory */
    private String imageDir;
    
    /** An array of lists of intensity profiles */
    private ArrayList<Short>[] intensityProfiles;
    
    /** An array of lists of X locations where the intensity profiles were acquired */
    private ArrayList<Integer>[] xProfileLocs;
    
    /** An array of lists of Y locations where the intensity profiles were acquired */
    private ArrayList<Integer>[] yProfileLocs;

    /**Total segmentation time for abdomen. */
    private long segmentationTimeAbd = 0;
    
    /**Total segmentation time for subcutaneous fat.*/
    private long segmentationTimeSub = 0;
    
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     * @param  imageDir     Directory of the source image
     * @param  color        Color of the VOI
     */
    public PlugInAlgorith1mCTAbdomen542a(ModelImage resultImage, ModelImage srcImg, String imageDir, Color color) {
        super(resultImage, srcImg);
        
        this.imageDir = imageDir+File.separator;

        abdomenImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "AbdomenImage");
        abdomenVOI = null;
        subcutaneousVOI = null;
    } // end PlugInAlgorithmCTAbdomen(...)

    
    
    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        
        // Algorithm to determine the outer abdominal and inner subcutaneous boundary 
        if (!initializedFlag) {
            init();
        }
        if (!initializedFlag) {
            return;
        }
        
        if (srcImage.getNDims() == 2) {
            calc2D();
          
        } else if (srcImage.getNDims() == 3) {
            calc25D();
            
        }

        ViewUserInterface.getReference().getMessageFrame().append("directory: " +imageDir+"\n", ViewJFrameMessage.DEBUG);
        
    	ViewJFrameImage frame = new ViewJFrameImage(srcImage);
    	srcImage.unregisterAllVOIs();
    	srcImage.registerVOI(abdomenVOI);
    	srcImage.registerVOI(subcutaneousVOI);
    	frame.saveAllVOIsTo(imageDir);
    	
    	frame.dispose();
 
    } // end runAlgorithm()



    private void calc2D() {
        
    	long totalTime = System.currentTimeMillis();
    	
        labelAbdomen2D();
        makeAbdomen2DVOI();

        segmentationTimeAbd = (System.currentTimeMillis() - totalTime);
        System.err.println("Total time for abdomen segmentation: "+segmentationTimeAbd);
        
        totalTime = System.currentTimeMillis();
        // the new way to find the subcutaneous fat VOI
        // make the intensity profile for slice 0 
        makeIntensityProfiles();
        makeSubcutaneousFatVOIfromIntensityProfiles();
//        snakeSubcutaneousVOI();
        
        
        // the old way to find the subcutaneous fat VOI
//        makeSubcutaneousFat2DVOI();
//        snakeSubcutaneousVOI();

        //ShowImage(abdomenImage, "Segmented Abdomen");
        
        // the really old way to find the subcutaneous fat VOI
//        JCATsegmentSubcutaneousFat2D();
//        snakeViseralVOI();

        // associate the subcutaneousVOI with the srcImage
        srcImage.unregisterAllVOIs();
//        srcImage.registerVOI(abdomenVOI);
        srcImage.registerVOI(subcutaneousVOI);
        
        // make a binary image out of the subcutaneousVOI
        ModelImage binImage = srcImage.generateBinaryImage();
        
        // close the binaryImage with a huge structuring element to fill in all the major gaps
        
        closeImage(binImage, 61.0f);
        
        // convert the binary image into a VOI
        AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(binImage);
        VOIExtractionAlgo.setRunningInSeparateThread(false);
        VOIExtractionAlgo.run();
        VOIExtractionAlgo.setCompleted(true);
        
//        ShowImage(binImage, "After VOI extraction");

        // make sure we got one VOI with one curve
        VOIVector vois = binImage.getVOIs();
        if(vois.size() != 1) {
            System.err.println("calc2D() Error, did not get 1 VOI for the subcutaneous fat VOI");
            return;
        }
        
        // abdomenImage has one VOI, lets get it
        VOI theVOI = vois.get(0);
        theVOI.setName("Subcutaneous area");
        
        // Keep only the largest VOI as the abdomen
        int numCurves, numRemoved = 0;
        numCurves = theVOI.getCurves().size();
            
        int maxIdx = 0;
        int maxNumPoints = ((VOIContour)theVOI.getCurves().get(maxIdx)).size();
        for (int idx = 1; idx < numCurves; idx++) {
            if (((VOIContour)theVOI.getCurves().get(idx)).size() > maxNumPoints) {
                maxIdx = idx;
            }
        } // end for (int idx = 0; ...)
        for (int idx = 0; idx < numCurves; idx++) {
            if (idx != maxIdx) {
                theVOI.getCurves().remove(idx - numRemoved);
                numRemoved++;
            }
        } // end for (int idx = 0; ...)

        resampleCurve(theVOI, 0);
        
        // get the new subcutaneous VOI
        subcutaneousVOI = theVOI;
        segmentationTimeSub = (System.currentTimeMillis() - totalTime);
        System.err.println("Total time for subcutaneous fat segmentation: "+segmentationTimeSub);

        srcImage.unregisterAllVOIs();
        srcImage.registerVOI(abdomenVOI);
        srcImage.registerVOI(subcutaneousVOI);
    } // end calc2D()



    private void calc25D() {
        long totalTime = System.currentTimeMillis();
    	
    	labelAbdomen3D();
        makeAbdomen3DVOI();
        
        System.err.println("Total time for abdomen segmentation: "+(System.currentTimeMillis() - totalTime));
        
        totalTime = System.currentTimeMillis();
        
        subcutaneousVOI = new VOI((short)0, "Subcutaneous area");
        
        for (int sliceIdx = 0; sliceIdx < zDim; sliceIdx++) {
            ViewUserInterface.getReference().getMessageFrame().append("Making intensity profiles for slice: " +sliceIdx+"\n", ViewJFrameMessage.DEBUG);
            makeIntensityProfiles(sliceIdx);
            makeSubcutaneousFatVOIfromIntensityProfiles(sliceIdx);
        } // end for (sliceIdx = 0; ...)

        
        ViewUserInterface.getReference().getMessageFrame().append("Fixing the subcutaneous fat VOIs\n", ViewJFrameMessage.DEBUG);
        fixSubcutaneousFatVOIs();
        
        System.err.println("Total time for subcutaneous fat segmentation: "+(System.currentTimeMillis() - totalTime));
        
        srcImage.unregisterAllVOIs();
        srcImage.registerVOI(abdomenVOI);
        srcImage.registerVOI(subcutaneousVOI);
    } // end calc25D()

    
    


    public void algorithmPerformed(AlgorithmBase algorithm) {
        // TODO Auto-generated method stub
    } // end algorithmPerformed(...)



    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        sliceBuffer = null;
        super.finalize();
    }
    
    
    
    /**
     * Create all the data structures that are needed by the various routines to automatically
     * segment the abdominal and subcutaneous boundaries in 2D and 3D CT images of the abdomin.
     */
    private void init() {
        
        // simple error check up front
        if ((srcImage.getNDims() != 2) && srcImage.getNDims() != 3) {
            MipavUtil.displayError("PlugInAlgorithmCTAbdomen.init() Error image is not 2 or 3 dimensions");
            return;
        }
        
        // set and allocate known values
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        sliceSize = xDim * yDim;

        // allocate the slice buffer
        try {
            sliceBuffer = new short[sliceSize];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("PlugInAlgorithmCTAbdomen.init() Out of memory when making the slice buffer");
            return;
        }

        // set values that depend on the source image being a 2D or 3D image
        if (srcImage.getNDims() == 2) {
            zDim = 1;
        } else if (srcImage.getNDims() == 3) {
            zDim = srcImage.getExtents()[2];
        }

        // make the abdomenlabel image and initialize its resolutions
        abdomenImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "abdomenTissueImage");
       
        // make the resolutions of the abdomenImage the same as the source image
        for (int i = 0; i < zDim; i++) {
            abdomenImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[i].getResolutions());
        }
        
        // make the volume BitSet needed for the region grow algorithm
        try {
            volumeBitSet = new BitSet();
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("PlugInAlgorithmCTAbdomen.init() Out of memory when making the volumeBitSet");
            return;
        }
        
        // make the centerOfMass of the segmented abdomen
        try {
            centerOfMass = new short [2];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("PlugInAlgorithmCTAbdomen.init() Out of memory when making the centerOfMass array");
            return;
        }

        // all data was allocated, set initialized flag to true and return
        initializedFlag = true;
    } // end init()
    
    
    
    
    /**
     * Apply the snake algorithm to the subcutaneous VOI
     */
    @SuppressWarnings("unused")
    private void snakeSubcutaneousVOI() {
        
        // set the subcutaneous VOI as active
        subcutaneousVOI.setActive(true);
        subcutaneousVOI.getCurves().elementAt(0).setActive(true);
        
        float[] sigmas = new float[2];
        sigmas[0] = 1.0f;
        sigmas[1] = 1.0f;
        
        AlgorithmSnake snake = new AlgorithmSnake(srcImage, sigmas, 50, 2, subcutaneousVOI, AlgorithmSnake.OUT_DIR);
        snake.run();

        subcutaneousVOI = snake.getResultVOI();
        subcutaneousVOI.setName("Subcutaneous area");
        
    } // end snakeViseralVOI
    
    
    
    
    
    
    /**
     * Our adaptation of the algorithm presented in the 2006 JCAT article.  The published method
     * included the blanket under the patient as part of the segmentation.  We threshold and
     * region grow using values to "undersegment" the image.  We then clean up the segmentation
     * using mathematical morphology
     */
    private void labelAbdomen2D() {
        // fill up the slice buffer
        try {
            srcImage.exportData(0, sliceSize, sliceBuffer);
        } catch (IOException ex) {
            System.err.println("PlugInAlgorithmCTAbdomen.labelAbdomen2D(): Error exporting data");
            return;
        }

        // find a seed point inside the subcutaneous fat for a region grow
        boolean found = false;
        int seedX = 0, seedY = 0;
        short seedVal = 0;
        for (int idx = 0, y = 0; !found && y < yDim; y++) {
            for (int x = 0; x < xDim; x++, idx++) {
                // search for a fat pixel.  These are Hounsfield units
                if (sliceBuffer[idx] > -90 && sliceBuffer[idx] < -30) {
                    seedX = x;
                    seedY = y;
                    seedVal = sliceBuffer[idx];
                    found = true;
                    break;
                }
            } // end for (int x = 0; ...)
        } // end for int idx = 0, y = 0; ...)

        if (seedX < 0 || seedX >= xDim || seedY < 0 || seedY >= yDim) {
            MipavUtil.displayError("PlugInAlgorithmCTAbdomen.labelAbdomen2D(): Failed to find a seed location for the region grow");
            return;
        }

//        ViewUserInterface.getReference().getMessageFrame().append("Seed Location: " +seedX +"  " +seedY +"  intensity: " +seedVal);
        
        AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(srcImage, 1.0f, 1.0f);
        regionGrowAlgo.setRunningInSeparateThread(false);

        // under segment so that we do not get the blanket
        regionGrowAlgo.regionGrow2D(volumeBitSet, new Point(seedX, seedY), -1,
                                    false, false, null, seedVal - 75,
                                    seedVal + 1500, -1, -1, false);
        
        // make the abdominal label image from the volume BitSet determined in the region grow
        for (int idx = 0; idx < sliceSize; idx++) {
            if (volumeBitSet.get(idx)) {
                sliceBuffer[idx] = abdomenTissueLabel;
            } else {
                sliceBuffer[idx] = 0;
            }
        } // end for (int idx = 0; ...)

        // save the sliceBuffer into the abdomenImage
        try {
            abdomenImage.importData(0, sliceBuffer, false);
        } catch (IOException ex) {
            System.err.println("labelAbdomen2D(): Error importing data");
        }
//        ShowImage(abdomenImage, "Region Grown image");

        // do a mathematical morphology closing operation to fill the small gaps
        closeImage(abdomenImage);
//        ShowImage(abdomenImage, "closed image");
        
        // update the volumeBitSet to match the closed abdomenImage
        // This needs to happen if we are going to find the VOI's with a region grow technique
        // that uses the volumeBitSet

        try {
            abdomenImage.exportData(0, sliceSize, sliceBuffer);
        } catch (IOException ex) {
            System.err.println("Error exporting data");
            return;
        }
        volumeBitSet.clear();
        for (int idx = 0; idx < sliceSize; idx++) {
            if (sliceBuffer[idx] == abdomenTissueLabel) {
                volumeBitSet.set(idx);
            }
        } // end for (int idx = 0; ...)

    } // end labelAbdomen2D()
    
    
    
    
    /**
     * Our adaptation of the algorithm presented in the 2006 JCAT article.  The published method
     * included the blanket under the patient as part of the segmentation.  We threshold and
     * region grow using values to "undersegment" the image.  We then clean up the segmentation
     * using mathematical morphology
     */
     private void labelAbdomen3D() {
        
        ViewUserInterface.getReference().getMessageFrame().append("labelAbdomen3D(): Start\n", ViewJFrameMessage.DEBUG);
        
        // find a seed point inside the subcutaneous fat for a region grow
        boolean found = false;
        short seedX = 0, seedY = 0, seedZ = 0;
        short seedVal = 0;
        
        for (int idx = 0, z = 0; !found && z < zDim; z++) {
            // fill up the slice buffer
            try {
                srcImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("PlugInAlgorithmCTAbdomen.labelAbdomen3D(): Error exporting data on slice: " +z);
                return;
            }
            
            for (int y = 0; !found && y < yDim; y++) {
                for (int x = 0; x < xDim; x++, idx++) {
                    // search for a fat pixel.  These are Hounsfield units
                    if (sliceBuffer[idx] > -75 && sliceBuffer[idx] < -30) {
                        seedX = (short)x;
                        seedY = (short)y;
                        seedZ = (short)z;
                        seedVal = sliceBuffer[idx];
                        found = true;
                        break;
                    }
                } // end for (int x = 0; ...)
            } // end for int y = 0; ...)
        } // end for (int idx = 0, ...
        

        if (seedX < 0 || seedX >= xDim || seedY < 0 || seedY >= yDim || seedZ < 0 || seedZ >= zDim) {
            MipavUtil.displayError("PlugInAlgorithmCTAbdomen.labelAbdomen3D(): Failed to find a seed location for the region grow");
            return;
        }

//        ViewUserInterface.getReference().getMessageFrame().append("seed point: " +seedX +"  " +seedY +"  " +seedZ);
//        ViewUserInterface.getReference().getMessageFrame().append("seed value: " +seedVal);

        AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(srcImage, 1.0f, 1.0f);
        regionGrowAlgo.setRunningInSeparateThread(false);
        
        CubeBounds regionGrowBounds = new CubeBounds(xDim, 0, yDim, 0, zDim, 0);

        ViewUserInterface.getReference().getMessageFrame().append("  regionGrow3D: ", ViewJFrameMessage.DEBUG);
        long time = System.currentTimeMillis();

        // under segment so that we do not get the blanket
        regionGrowAlgo.regionGrow3D(volumeBitSet, new Point3D(seedX, seedY, seedZ), -1,
                                    false, false, null, seedVal - 50,
                                    seedVal + 1500, -1, -1, false, 0, regionGrowBounds);
        
        ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);

        // make the abdominal label image from the volume BitSet determined in the region grow
        for (int volIdx = 0, z = 0; z < zDim; z++) {
            for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, volIdx++) {
                if (volumeBitSet.get(volIdx)) {
                    sliceBuffer[sliceIdx] = abdomenTissueLabel;
                } else {
                    sliceBuffer[sliceIdx] = 0;
                }
            } // end for (int sliceIdx = 0; ...)
            // save the sliceBuffer into the abdomenImage
            try {
                abdomenImage.importData(z * sliceSize, sliceBuffer, false);
            } catch (IOException ex) {
                System.err.println("labelAbdomen3D(): Error importing data");
            }
        } // end for (int idx = 0; ...)

//        ShowImage(abdomenImage, "abdominal label image");

        // do a mathematical morphology closing operation to fill the small gaps
        // do it in 2D so as to not change the 3D shape
        ViewUserInterface.getReference().getMessageFrame().append("  closeImage25D: ", ViewJFrameMessage.DEBUG);
        time = System.currentTimeMillis();
        closeImage25D(abdomenImage);
        ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);
        
//        ShowImage(abdomenImage, "closed image");
        
     
        // update the volumeBitSet to match the closed abdomenImage
        // This needs to happen if we are going to find the VOI's with a region grow technique
        // that uses the volumeBitSet

        ViewUserInterface.getReference().getMessageFrame().append("  updating the volume BitSet: ", ViewJFrameMessage.DEBUG);
        time = System.currentTimeMillis();
        volumeBitSet.clear();
         for (int volIdx = 0, z = 0; z < zDim; z++) {
            // fill up the slice buffer
            try {
                abdomenImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
            } catch (IOException ex) {
                System.err.println("PlugInAlgorithmCTAbdomen.labelAbdomen3D(): Error exporting data on slice: " +z);
                return;
            }
            
            for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, volIdx++) {
                if (sliceBuffer[sliceIdx] == abdomenTissueLabel) {
                    volumeBitSet.set(volIdx);
                }
            } // end for (int sliceIdx = 0; ...)
        } // end for (volIdx = 0, ...)
         ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);

        ViewUserInterface.getReference().getMessageFrame().append("labelAbdomen3D(): End\n", ViewJFrameMessage.DEBUG);

    } // end labelAbdomen3D()
     
     
     
     
     /**
      * Our adaptation of the algorithm presented in the 2006 JCAT article.  The published method
      * included the blanket under the patient as part of the segmentation.  We threshold and
      * region grow using values to "undersegment" the image.  We then clean up the segmentation
      * using mathematical morphology
      */
      @SuppressWarnings("unused")
      private void labelAbdomen3D01() {
         
         ViewUserInterface.getReference().getMessageFrame().append("labelAbdomen3D(): Start\n", ViewJFrameMessage.DEBUG);
         
         // find a seed point inside the subcutaneous fat for a region grow
         boolean found = false;
         short seedX = 0, seedY = 0, seedZ = 0;
         short seedVal = 0;
         
         for (int idx = 0, z = 0; !found && z < zDim; z++) {
             // fill up the slice buffer
             try {
                 srcImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
             } catch (IOException ex) {
                 System.err.println("PlugInAlgorithmCTAbdomen.labelAbdomen3D(): Error exporting data on slice: " +z);
                 return;
             }
             
             for (int y = 0; !found && y < yDim; y++) {
                 for (int x = 0; x < xDim; x++, idx++) {
                     // search for a fat pixel.  These are Hounsfield units
                     if (sliceBuffer[idx] > -75 && sliceBuffer[idx] < -30) {
                         seedX = (short)x;
                         seedY = (short)y;
                         seedZ = (short)z;
                         seedVal = sliceBuffer[idx];
                         found = true;
                         break;
                     }
                 } // end for (int x = 0; ...)
             } // end for int y = 0; ...)
         } // end for (int idx = 0, ...
         

         if (seedX < 0 || seedX >= xDim || seedY < 0 || seedY >= yDim || seedZ < 0 || seedZ >= zDim) {
             MipavUtil.displayError("PlugInAlgorithmCTAbdomen.labelAbdomen3D(): Failed to find a seed location for the region grow");
             return;
         }

//         ViewUserInterface.getReference().getMessageFrame().append("seed point: " +seedX +"  " +seedY +"  " +seedZ);
//         ViewUserInterface.getReference().getMessageFrame().append("seed value: " +seedVal);

         AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(srcImage, 1.0f, 1.0f);
         regionGrowAlgo.setRunningInSeparateThread(false);
         
         CubeBounds regionGrowBounds = new CubeBounds(xDim, 0, yDim, 0, zDim, 0);

         ViewUserInterface.getReference().getMessageFrame().append("  regionGrow3D: ", ViewJFrameMessage.DEBUG);
         long time = System.currentTimeMillis();

         // under segment so that we do not get the blanket
         regionGrowAlgo.regionGrow3D(volumeBitSet, new Point3D(seedX, seedY, seedZ), -1,
                                     false, false, null, seedVal - 50,
                                     seedVal + 1500, -1, -1, false, 0, regionGrowBounds);
         
         ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);

         // make the abdominal label image from the volume BitSet determined in the region grow
         for (int volIdx = 0, z = 0; z < zDim; z++) {
             for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, volIdx++) {
                 if (volumeBitSet.get(volIdx)) {
                     sliceBuffer[sliceIdx] = abdomenTissueLabel;
                 } else {
                     sliceBuffer[sliceIdx] = 0;
                 }
             } // end for (int sliceIdx = 0; ...)
             // save the sliceBuffer into the abdomenImage
             try {
                 abdomenImage.importData(z * sliceSize, sliceBuffer, false);
             } catch (IOException ex) {
                 System.err.println("labelAbdomen3D(): Error importing data");
             }
         } // end for (int idx = 0; ...)

//         ShowImage(abdomenImage, "abdominal label image");

         // do a mathematical morphology closing operation to fill the small gaps
         ViewUserInterface.getReference().getMessageFrame().append("  closeImage3D: ", ViewJFrameMessage.DEBUG);
         time = System.currentTimeMillis();
         closeImage3D(abdomenImage);
         ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);

         //ShowImage(abdomenImage, "closed image");

         
         // update the volumeBitSet to match the closed abdomenImage
         // This needs to happen if we are going to find the VOI's with a region grow technique
         // that uses the volumeBitSet

         ViewUserInterface.getReference().getMessageFrame().append("  updating the volume BitSet: ", ViewJFrameMessage.DEBUG);
         time = System.currentTimeMillis();
         volumeBitSet.clear();
          for (int volIdx = 0, z = 0; z < zDim; z++) {
             // fill up the slice buffer
             try {
                 abdomenImage.exportData(z * sliceSize, sliceSize, sliceBuffer);
             } catch (IOException ex) {
                 System.err.println("PlugInAlgorithmCTAbdomen.labelAbdomen3D(): Error exporting data on slice: " +z);
                 return;
             }
             
             for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, volIdx++) {
                 if (sliceBuffer[sliceIdx] == abdomenTissueLabel) {
                     volumeBitSet.set(volIdx);
                 }
             } // end for (int sliceIdx = 0; ...)
         } // end for (volIdx = 0, ...)
          ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);

          ViewUserInterface.getReference().getMessageFrame().append("labelAbdomen3D(): End\n", ViewJFrameMessage.DEBUG);

     } // end labelAbdomen3D()
    
    
   
    
    /**
     * Extract a 2D VOI from the volumeBitSet that was set during the labelAbdomen2D call
     * 
     * Resample the VOI at 3 degree increments
     */
    private boolean makeAbdomen2DVOI() {
        abdomenImage.setMask(volumeBitSet);
        
        // volumeBitSet should be set for the abdomen tissue
        short voiID = 0;
        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(abdomenImage,
                volumeBitSet, xDim, yDim, zDim, voiID);

        algoPaintToVOI.setRunningInSeparateThread(false);
        algoPaintToVOI.run();
        setCompleted(true);
        
        // make sure we got one VOI with one curve
        VOIVector vois = abdomenImage.getVOIs();
        if(vois.size() != 1) {
            System.err.println("makeAbdomen2DVOI() Error, did not get 1 VOI");
            return false;
        }
        
        // abdomenImage has one VOI, lets get it
        VOI theVOI = vois.get(0);
        theVOI.setName("Abdomen");

        // Keep only the largest VOI as the abdomen
        int numCurves, numRemoved = 0;
        numCurves = theVOI.getCurves().size();
            
        int maxIdx = 0;
        int maxNumPoints = ((VOIContour)theVOI.getCurves().get(maxIdx)).size();
        for (int idx = 1; idx < numCurves; idx++) {
            if (((VOIContour)theVOI.getCurves().get(idx)).size() > maxNumPoints) {
                maxIdx = idx;
            }
        } // end for (int idx = 0; ...)
        for (int idx = 0; idx < numCurves; idx++) {
            if (idx != maxIdx) {
                theVOI.getCurves().remove(idx - numRemoved);
                numRemoved++;
            }
        } // end for (int idx = 0; ...)
        
        abdomenVOI = theVOI;
        resampleAbdomenVOI();
        return true;
    } // end makeAbdomen2DVOI()
    
    
    
   
    
    /**
     * Extract a 3D VOI from the volumeBitSet that was set during the labelAbdomen3D call
     * 
     */
    private boolean makeAbdomen3DVOI() {
        
        ViewUserInterface.getReference().getMessageFrame().append("\nmakeAbdomen3DVOI(): Start\n", ViewJFrameMessage.DEBUG);
        
        ViewUserInterface.getReference().getMessageFrame().append("  Extracting VOI from mask: ", ViewJFrameMessage.DEBUG);
        long time = System.currentTimeMillis();

        abdomenImage.setMask(volumeBitSet);
        
        // volumeBitSet should be set for the abdomen tissue
        short voiID = 0;
        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(abdomenImage,
                volumeBitSet, xDim, yDim, zDim, voiID);

        algoPaintToVOI.setRunningInSeparateThread(false);
        algoPaintToVOI.run();
        setCompleted(true);
        
        ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);
        
        // make sure we got one VOI with xDim number of curves
        VOIVector vois = abdomenImage.getVOIs();

        if(vois.size() != 1) {
            System.err.println("makeAbdomen3DVOI() Error, did not get 1 VOI");
            return false;
        }
        
        // abdomenImage has one VOI, lets get it
        VOI theVOI = vois.get(0);
        theVOI.setName("Abdomen");

        abdomenVOI = theVOI;
        
        // Keep only the largest curve on each slice
        int numCurves, maxIdx = 0, maxCurveLength;
        Vector<VOIBase>[] curves = theVOI.getSortedCurves(zDim);
        for (int sliceIdx = 0; sliceIdx < zDim; sliceIdx++) {
//            ViewUserInterface.getReference().getMessageFrame().append("  Slice: " +sliceIdx);
            numCurves = curves[sliceIdx].size();
            maxIdx = 0;
            maxCurveLength = ((VOIContour)curves[sliceIdx].get(maxIdx)).size();
            
            // find the index of the larges curve on each slice
            for (int curveIdx = 1; curveIdx < numCurves; curveIdx++) {
//                ViewUserInterface.getReference().getMessageFrame().append("    curve: " +curveIdx +"  size: " +((VOIContour)theVOI.getCurves()[sliceIdx].get(curveIdx)).size());
                if (((VOIContour)curves[sliceIdx].get(curveIdx)).size() > maxCurveLength) {
                    maxIdx = curveIdx;
                    maxCurveLength = ((VOIContour)curves[sliceIdx].get(curveIdx)).size();
                }
            } // end for (int curveIdx = 0; ...)
            
            // remove all the curves except the one with the maxIdx
            for (int curveIdx = 0; curveIdx < numCurves; curveIdx++) {
                if (curveIdx != maxIdx) {
                    theVOI.getCurves().removeElement(curves[sliceIdx].elementAt(curveIdx));
                }
            } // end for (int curveIdx = 0; ...)
            
            resampleAbdomenVOI( sliceIdx, (VOIContour)curves[sliceIdx].elementAt(0) );
        } // end for (sliceIdx = 0; ...)

        ViewUserInterface.getReference().getMessageFrame().append("makeAbdomen3DVOI(): End\n", ViewJFrameMessage.DEBUG);
        return true;
    } // end makeAbdomen3DVOI()

    
    
    
    
    
    /**
     * Resample the abdominal VOI every angularResolution degrees
     */
    private void resampleAbdomenVOI( int sliceIdx, VOIContour curve ) {
        
        // find the center of mass of the single label object in the sliceBuffer (abdomenImage)
        findAbdomenCM(sliceIdx);
        int xcm = centerOfMass[0];
        int ycm = centerOfMass[1];
//        ViewUserInterface.getReference().getMessageFrame().append("Xcm: " +xcm +"  Ycm: " +ycm);
        

        ArrayList<Integer> xValsAbdomenVOI = new ArrayList<Integer>();
        ArrayList<Integer> yValsAbdomenVOI = new ArrayList<Integer>();

        // angle in radians
        double angleRad;
       
        for (int angle = 0; angle < 360; angle += angularResolution) {
            int x = xcm;
            int y = ycm;
            double scaleFactor;      // reduces the number of trig operations that must be performed
     
            angleRad = Math.PI * angle / 180.0;
             if (angle > 315 || angle <= 45) {
                 // increment x each step
                 scaleFactor = Math.tan(angleRad);
                 while (x < xDim && curve.contains(x, y)) {
                     
                     // walk out in x and compute the value of y for the given radial line
                     x++;
                     y = ycm - (int)((x - xcm) * scaleFactor);
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
                 
             } else if (angle > 45 && angle <= 135) {
                 // decrement y each step
                 scaleFactor = (Math.tan((Math.PI / 2.0) - angleRad));
                 while (y > 0 && curve.contains(x, y)) {

                     // walk to the top of the image and compute values of x for the given radial line
                     y--;
                     x = xcm + (int)((ycm - y) * scaleFactor);
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
                                 
             } else if (angle > 135 && angle <= 225) {
                 // decrement x each step
                 scaleFactor = Math.tan(Math.PI - angleRad);
                 while (x > 0 && curve.contains(x, y)) {
                     
                     x--;
                     y = ycm - (int)((xcm - x) * scaleFactor);
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);

             } else if (angle > 225 && angle <= 315) {
                 // increment y each step
                 scaleFactor = Math.tan((3.0 * Math.PI / 2.0) - angleRad);
                 while (y < yDim && curve.contains(x, y)) {
                     
                     y++;
                     x = xcm - (int)((y - ycm) * scaleFactor);
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
             }
         } // end for (angle = 0; ...
        
//        ViewUserInterface.getReference().getMessageFrame().append("resample VOI number of points: " +xValsAbdomenVOI.size());

        curve.clear();
        for(int idx = 0; idx < xValsAbdomenVOI.size(); idx++) {
            curve.add( new Vector3f( xValsAbdomenVOI.get(idx), yValsAbdomenVOI.get(idx), sliceIdx ) );
        }
    } // end resampleAbdomenVOI(...)

    
    
    
    
    
    /**
     * Resample the abdominal VOI every angularResolution degrees
     */
    private void resampleAbdomenVOI() {
        VOIVector vois = abdomenImage.getVOIs();
        VOI theVOI = vois.get(0);

        VOIContour curve = ((VOIContour)theVOI.getCurves().get(0));
        
        // find the center of mass of the single label object in the sliceBuffer (abdomenImage)
        findAbdomenCM();
        int xcm = centerOfMass[0];
        int ycm = centerOfMass[1];
//        ViewUserInterface.getReference().getMessageFrame().append("Xcm: " +xcm +"  Ycm: " +ycm);
        

        ArrayList<Integer> xValsAbdomenVOI = new ArrayList<Integer>();
        ArrayList<Integer> yValsAbdomenVOI = new ArrayList<Integer>();

        // angle in radians
        double angleRad;
       
        for (int angle = 0; angle < 360; angle += angularResolution) {
            int x = xcm;
            int y = ycm;
            double scaleFactor;      // reduces the number of trig operations that must be performed
     
            angleRad = Math.PI * angle / 180.0;
             if (angle > 315 || angle <= 45) {
                 // increment x each step
                 scaleFactor = Math.tan(angleRad);
                 while (x < xDim && curve.contains(x, y)) {
                     
                     // walk out in x and compute the value of y for the given radial line
                     x++;
                     y = ycm - (int)((x - xcm) * scaleFactor);
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
                 
             } else if (angle > 45 && angle <= 135) {
                 // decrement y each step
                 scaleFactor = (Math.tan((Math.PI / 2.0) - angleRad));
                 while (y > 0 && curve.contains(x, y)) {

                     // walk to the top of the image and compute values of x for the given radial line
                     y--;
                     x = xcm + (int)((ycm - y) * scaleFactor);
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
                                 
             } else if (angle > 135 && angle <= 225) {
                 // decrement x each step
                 scaleFactor = Math.tan(Math.PI - angleRad);
                 while (x > 0 && curve.contains(x, y)) {
                     
                     x--;
                     y = ycm - (int)((xcm - x) * scaleFactor);
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);

             } else if (angle > 225 && angle <= 315) {
                 // increment y each step
                 scaleFactor = Math.tan((3.0 * Math.PI / 2.0) - angleRad);
                 while (y < yDim && curve.contains(x, y)) {
                     
                     y++;
                     x = xcm - (int)((y - ycm) * scaleFactor);
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
             }
         } // end for (angle = 0; ...
        
//        ViewUserInterface.getReference().getMessageFrame().append("resample VOI number of points: " +xValsAbdomenVOI.size());
        curve.clear();
        for(int idx = 0; idx < xValsAbdomenVOI.size(); idx++) {
            curve.add( new Vector3f( xValsAbdomenVOI.get(idx), yValsAbdomenVOI.get(idx), 0 ) );
        }
    } // end resampleAbdomenVOI()
    
    
    
    
    /**
     * Method fills in the intensity profile array along radial lines
     */
    @SuppressWarnings("unchecked")
    private void makeIntensityProfiles(int sliceNum) {
        
        // sanity check
        if (sliceNum < 0 || sliceNum >= zDim) return;

        // find the center of mass of the single label object in the sliceBuffer (abdomenImage)
        findAbdomenCM(sliceNum);
        int xcm = centerOfMass[0];
        int ycm = centerOfMass[1];
        
        // There should be only one VOI
        VOIVector vois = abdomenImage.getVOIs();
        VOI theVOI = vois.get(0);

        // there should be only one curve on each slice corresponding to the external abdomen boundary
        VOIContour curve = ((VOIContour)theVOI.getCurves().elementAt(sliceNum));        
        int[] xVals;
        int[] yVals;
        int[] zVals;
        try {
            xVals = new int [curve.size()];
            yVals = new int [curve.size()];
            zVals = new int [curve.size()];
        }  catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("makeIntensityProfiles(): Can NOT allocate the abdomen VOI arrays");
            return;
        }
        curve.exportArrays(xVals, yVals, zVals);
        
        // one intensity profile for each radial line.  Each radial line is 3 degrees and
        // there are 360 degrees in a circle
        try {
            intensityProfiles = new ArrayList[360 /angularResolution];
            xProfileLocs = new ArrayList[360 / angularResolution];
            yProfileLocs = new ArrayList[360 / angularResolution];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("makeIntensityProfiles(): Can NOT allocate profile arrays");
            return;
        }
        
        // load the srcImage into the slice buffer (it has the segmented abdomen image in it now)
        try {
            srcImage.exportData(sliceNum * sliceSize, sliceSize, sliceBuffer);
        } catch (IOException ex) {
            System.err.println("JCATsegmentAbdomen2D(): Error exporting data");
            return;
        }

        double angleRad;
        int count;
        int contourPointIdx = 0;
        for (int angle = 0; angle < 360; angle += angularResolution) {
            count = 0;
            int x = xcm;
            int y = ycm;
            int yOffset = y * xDim;
            double scaleFactor;      // reduces the number of trig operations that must be performed
            
            // allocate the ArrayLists for each radial line
            try {
                intensityProfiles[contourPointIdx] = new ArrayList<Short>();
                xProfileLocs[contourPointIdx] = new ArrayList<Integer>();
                yProfileLocs[contourPointIdx] = new ArrayList<Integer>();
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("makeIntensityProfiles(): Can NOT allocate profile list: " +contourPointIdx);
                return;
            }
                
            angleRad = Math.PI * angle / 180.0;
             if (angle > 315 || angle <= 45) {
                 // increment x each step
                 scaleFactor = Math.tan(angleRad);
                 while (x < xVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     intensityProfiles[contourPointIdx].add((short)sliceBuffer[yOffset + x]);
                     xProfileLocs[contourPointIdx].add(x);
                     yProfileLocs[contourPointIdx].add(y);
                     count++;
                     
                     // walk out in x and compute the value of y for the given radial line
                     x++;
                     y = ycm - (int)((x - xcm) * scaleFactor);
                     yOffset = y * xDim;
                 }
                 
             } else if (angle > 45 && angle <= 135) {
                 // decrement y each step
                 scaleFactor = (Math.tan((Math.PI / 2.0) - angleRad));
                 while (y > yVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     intensityProfiles[contourPointIdx].add((short)sliceBuffer[yOffset + x]);
                     xProfileLocs[contourPointIdx].add(x);
                     yProfileLocs[contourPointIdx].add(y);
                     count++;
                     
                     // walk to the top of the image and compute values of x for the given radial line
                     y--;
                     x = xcm + (int)((ycm - y) * scaleFactor);
                     yOffset = y * xDim;
                 }
                                 
             } else if (angle > 135 && angle <= 225) {
                 // decrement x each step
                 scaleFactor = Math.tan(Math.PI - angleRad);
                 while (x > xVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     intensityProfiles[contourPointIdx].add((short)sliceBuffer[yOffset + x]);
                     xProfileLocs[contourPointIdx].add(x);
                     yProfileLocs[contourPointIdx].add(y);
                     count++;
                     
                     x--;
                     y = ycm - (int)((xcm - x) * scaleFactor);
                     yOffset = y * xDim;
                 }

             } else if (angle > 225 && angle <= 315) {
                 // increment y each step
                 scaleFactor = Math.tan((3.0 * Math.PI / 2.0) - angleRad);
                 while (y < yVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     intensityProfiles[contourPointIdx].add((short)sliceBuffer[yOffset + x]);
                     xProfileLocs[contourPointIdx].add(x);
                     yProfileLocs[contourPointIdx].add(y);
                     count++;
                     
                     y++;
                     x = xcm - (int)((y - ycm) * scaleFactor);
                     yOffset = y * xDim;
                 }
              }
             
             contourPointIdx++;
         } // end for (angle = 0; ...

        // intensityProfiles, xProfileLocs, and yProfileLocs are set, we can find the 
        // internal boundary of the subcutaneous fat now
    } // end makeIntensityProfiles(...)
    
    
    
    
    /**
     * Method fills in the intensity profile array along radial lines
     */
    @SuppressWarnings("unchecked")
    private void makeIntensityProfiles() {

        // find the center of mass of the single label object in the sliceBuffer (abdomenImage)
        findAbdomenCM();
        int xcm = centerOfMass[0];
        int ycm = centerOfMass[1];
        
        // There should be only one VOI
        VOIVector vois = abdomenImage.getVOIs();
        VOI theVOI = vois.get(0);

        // there should be only one curve corresponding to the external abdomen boundary
        VOIContour curve = ((VOIContour)theVOI.getCurves().get(0));
        int[] xVals;
        int[] yVals;
        int[] zVals;
        try {
            xVals = new int [curve.size()];
            yVals = new int [curve.size()];
            zVals = new int [curve.size()];
        }  catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("makeIntensityProfiles(): Can NOT allocate the abdomen VOI arrays");
            return;
        }
        curve.exportArrays(xVals, yVals, zVals);
        
        // one intensity profile for each radial line.  Each radial line is 3 degrees and
        // there are 360 degrees in a circle
        try {
            intensityProfiles = new ArrayList[360 /angularResolution];
            xProfileLocs = new ArrayList[360 / angularResolution];
            yProfileLocs = new ArrayList[360 / angularResolution];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("makeIntensityProfiles(): Can NOT allocate profile arrays");
            return;
        }
        
        // load the srcImage into the slice buffer (it has the segmented abdomen image in it now)
        try {
            srcImage.exportData(0, sliceSize, sliceBuffer);
        } catch (IOException ex) {
            System.err.println("JCATsegmentAbdomen2D(): Error exporting data");
            return;
        }

        double angleRad;
        int count;
        int contourPointIdx = 0;
        for (int angle = 0; angle < 360; angle += angularResolution) {
            count = 0;
            int x = xcm;
            int y = ycm;
            int yOffset = y * xDim;
            double scaleFactor;      // reduces the number of trig operations that must be performed
            
            // allocate the ArrayLists for each radial line
            try {
                intensityProfiles[contourPointIdx] = new ArrayList<Short>();
                xProfileLocs[contourPointIdx] = new ArrayList<Integer>();
                yProfileLocs[contourPointIdx] = new ArrayList<Integer>();
            } catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("makeIntensityProfiles(): Can NOT allocate profile list: " +contourPointIdx);
                return;
            }
                
            angleRad = Math.PI * angle / 180.0;
             if (angle > 315 || angle <= 45) {
                 // increment x each step
                 scaleFactor = Math.tan(angleRad);
                 while (x < xVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     intensityProfiles[contourPointIdx].add((short)sliceBuffer[yOffset + x]);
                     xProfileLocs[contourPointIdx].add(x);
                     yProfileLocs[contourPointIdx].add(y);
                     count++;
                     
                     // walk out in x and compute the value of y for the given radial line
                     x++;
                     y = ycm - (int)((x - xcm) * scaleFactor);
                     yOffset = y * xDim;
                 }
                 
             } else if (angle > 45 && angle <= 135) {
                 // decrement y each step
                 scaleFactor = (Math.tan((Math.PI / 2.0) - angleRad));
                 while (y > yVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     intensityProfiles[contourPointIdx].add((short)sliceBuffer[yOffset + x]);
                     xProfileLocs[contourPointIdx].add(x);
                     yProfileLocs[contourPointIdx].add(y);
                     count++;
                     
                     // walk to the top of the image and compute values of x for the given radial line
                     y--;
                     x = xcm + (int)((ycm - y) * scaleFactor);
                     yOffset = y * xDim;
                 }
                                 
             } else if (angle > 135 && angle <= 225) {
                 // decrement x each step
                 scaleFactor = Math.tan(Math.PI - angleRad);
                 while (x > xVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     intensityProfiles[contourPointIdx].add((short)sliceBuffer[yOffset + x]);
                     xProfileLocs[contourPointIdx].add(x);
                     yProfileLocs[contourPointIdx].add(y);
                     count++;
                     
                     x--;
                     y = ycm - (int)((xcm - x) * scaleFactor);
                     yOffset = y * xDim;
                 }

             } else if (angle > 225 && angle <= 315) {
                 // increment y each step
                 scaleFactor = Math.tan((3.0 * Math.PI / 2.0) - angleRad);
                 while (y < yVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     intensityProfiles[contourPointIdx].add((short)sliceBuffer[yOffset + x]);
                     xProfileLocs[contourPointIdx].add(x);
                     yProfileLocs[contourPointIdx].add(y);
                     count++;
                     
                     y++;
                     x = xcm - (int)((y - ycm) * scaleFactor);
                     yOffset = y * xDim;
                 }
              }
             
             contourPointIdx++;
         } // end for (angle = 0; ...

        // intensityProfiles, xProfileLocs, and yProfileLocs are set, we can find the 
        // internal boundary of the subcutaneous fat now
    } // end makeIntensityProfiles()
    
    
    
    /**
     * find the points on the internal subcutaneous fat VOI from the intensity profiles
     * used for 2.5D images
     */
    private void makeSubcutaneousFatVOIfromIntensityProfiles(int sliceNum) {
        
        int[] xLocsSubcutaneousVOI;
        int[] yLocsSubcutaneousVOI;
        int[] zVals;
        try {
            xLocsSubcutaneousVOI = new int [360 / angularResolution];
            yLocsSubcutaneousVOI = new int [360 / angularResolution];
            zVals = new int [360 / angularResolution];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("makeSubcutaneousFatVOIfromIntensityProfiles(): Can NOT allocate xLocsSubcutaneousVOI");
            return;
        }
        
        for(int idx = 0; idx < 360 / angularResolution; idx++) {
            // fixSubcutaneiousFat2DVOI requires the zVal to be 0 (does VOI extraction on a 2D slice image)
            zVals[idx] = 0;
        }
        
        int numSamples;
        
        for(int idx = 0; idx < 360 / angularResolution; idx++) {
            numSamples = intensityProfiles[idx].size();
                        
            int sampleIdx = numSamples - 10;  // skip over the skin
            while (sampleIdx >= 0 &&
                   intensityProfiles[idx].get(sampleIdx) < muscleThresholdHU &&
                   intensityProfiles[idx].get(sampleIdx) > airThresholdHU) {
                sampleIdx--;
            }
            if (sampleIdx <= 0) {
                // could not find a valid subcutaneous fat point in the intensity profile
//                MipavUtil.displayError("findAbdomenVOI(): Can NOT find the subcutaneous fat VOI point in the intensity profile for angle: " +idx * angularResolution);
                if (idx > 0) {
                    // assign this subcutaneous fat VOI point to the previous point if one exists
                    xLocsSubcutaneousVOI[idx] = xLocsSubcutaneousVOI[idx-1];
                    yLocsSubcutaneousVOI[idx] = yLocsSubcutaneousVOI[idx-1];
                } else {
                    // assign this subcutaneous fat VOI point to the abdomen VOI point
                    xLocsSubcutaneousVOI[idx] = xProfileLocs[idx].get(numSamples - 1);
                    yLocsSubcutaneousVOI[idx] = yProfileLocs[idx].get(numSamples - 1);
                }
            } else {
                // we found a point in the intensity profile that matches subcutaneous fat
                xLocsSubcutaneousVOI[idx] = xProfileLocs[idx].get(sampleIdx);
                yLocsSubcutaneousVOI[idx] = yProfileLocs[idx].get(sampleIdx);
            }
            
       } // end for (int idx = 0; ...)

        // make the VOI's and add the points to them
        subcutaneousVOI.importCurve(xLocsSubcutaneousVOI, yLocsSubcutaneousVOI, zVals);

    } // makeSubcutaneousFatVOIfromIntensityProfiles(...)
    
    
    
    /**
     * find the points on the internal subcutaneous fat VOI from the intensity profiles
     * used for 2D images
     */
    private void makeSubcutaneousFatVOIfromIntensityProfiles() {
        
        int[] xLocsSubcutaneousVOI;
        int[] yLocsSubcutaneousVOI;
        int[] zVals;
        try {
            xLocsSubcutaneousVOI = new int [360 / angularResolution];
            yLocsSubcutaneousVOI = new int [360 / angularResolution];
            zVals = new int [360 / angularResolution];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("makeSubcutaneousFatVOIfromIntensityProfiles(): Can NOT allocate xLocsSubcutaneousVOI");
            return;
        }
        
        for(int idx = 0; idx < 360 / angularResolution; idx++) {
            zVals[idx] = 0;
        }
        
        int numSamples;
        
        for(int idx = 0; idx < 360 / angularResolution; idx++) {
            numSamples = intensityProfiles[idx].size();
                        
            int sampleIdx = numSamples - 10;  // skip over the skin
            while (sampleIdx >= 0 &&
                   intensityProfiles[idx].get(sampleIdx) < muscleThresholdHU &&
                   intensityProfiles[idx].get(sampleIdx) > airThresholdHU) {
                sampleIdx--;
            }
            if (sampleIdx <= 0) {
                // could not find a valid subcutaneous fat point in the intensity profile
//                MipavUtil.displayError("findAbdomenVOI(): Can NOT find the subcutaneous fat VOI point in the intensity profile for angle: " +idx * angularResolution);
                if (idx > 0) {
                    // assign this subcutaneous fat VOI point to the previous point if one exists
                    xLocsSubcutaneousVOI[idx] = xLocsSubcutaneousVOI[idx-1];
                    yLocsSubcutaneousVOI[idx] = yLocsSubcutaneousVOI[idx-1];
                } else {
                    // assign this subcutaneous fat VOI point to the abdomen VOI point
                    xLocsSubcutaneousVOI[idx] = xProfileLocs[idx].get(numSamples - 1);
                    yLocsSubcutaneousVOI[idx] = yProfileLocs[idx].get(numSamples - 1);
                }
            } else {
                // we found a point in the intensity profile that matches subcutaneous fat
                xLocsSubcutaneousVOI[idx] = xProfileLocs[idx].get(sampleIdx);
                yLocsSubcutaneousVOI[idx] = yProfileLocs[idx].get(sampleIdx);
            }
            
       } // end for (int idx = 0; ...)

        // make the VOI's and add the points to them
        subcutaneousVOI = new VOI((short)0, "Subcutaneous area");
        subcutaneousVOI.importCurve(xLocsSubcutaneousVOI, yLocsSubcutaneousVOI, zVals);

    } // makeSubcutaneousFatVOIfromIntensityProfiles()
    
    
    
    
    /**
     * The detected subcutaneous fat VOI may have abrupt jumps in them because intentisy
     * values were used to determined the location of VOI points.  We want to smooth
     * out the curve on each slice of the VOI
     */
    private void fixSubcutaneousFatVOIs() {
        VOI theVOI = subcutaneousVOI;
        
        if (theVOI.getName() != "Subcutaneous area") {
            MipavUtil.displayError("fixSubcutaneousFatVOIs(): SubcutaneousVOI is not properly set");
            return;
        }
        
        // make a 2D image to hold one slice and curve from the VOI
        int[] extents = new int[2];
        extents[0] = xDim;
        extents[1] = yDim;
        ModelImage sliceImage = new ModelImage(ModelStorageBase.UBYTE, extents, "tmpSliceImage");

        // There should be one curve on each slice and
        // each curve should have the same number of points since each was resampled
        int numPoints = ((VOIContour)theVOI.getCurves().get(0)).size();
        int[] xVals;
        int[] yVals;
        int[] zVals;
        try {
            xVals = new int [numPoints];
            yVals = new int [numPoints];
            zVals = new int [numPoints];
        }  catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("fixSubcutaneousFatVOIs(): Can NOT allocate the subcutaneous fat VOI arrays");
            return;
        } 

        int numCurves;
        VOIContour curve;
        for (int sliceIdx = 0; sliceIdx < zDim; sliceIdx++) {
            ViewUserInterface.getReference().getMessageFrame().append("  Extracting VOI for slice: " +sliceIdx +"  ", ViewJFrameMessage.DEBUG);
            long time = System.currentTimeMillis();

            // make a new VOI
            VOI sliceVOI = new VOI((short)0, "SliceVOI");
            
            // copy and remove a curve from the 3D image into the 2D image
            curve = ((VOIContour)theVOI.getCurves().remove(0));
            sliceVOI.importCurve(curve);
            sliceImage.registerVOI(sliceVOI);
            
            ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);

            
            // make a binary image out of the subcutaneousVOI on the 2D VOI
            ViewUserInterface.getReference().getMessageFrame().append("    Making a binary image from the VOI: ", ViewJFrameMessage.DEBUG);
            time = System.currentTimeMillis();
            ModelImage binImage = sliceImage.generateBinaryImage();   
            ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);

            ViewUserInterface.getReference().getMessageFrame().append("    Smoothing Image with "+sliceVOI.getNumPoints()+"points: ", ViewJFrameMessage.DEBUG);
            time = System.currentTimeMillis();
            new AlgorithmBSmooth(binImage, sliceVOI, sliceVOI.getNumPoints()/3, true);
            ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);
            
            // close the binaryImage with a huge structuring element to fill in all the major gaps
            ViewUserInterface.getReference().getMessageFrame().append("    Morphology to the image: ", ViewJFrameMessage.DEBUG);
            time = System.currentTimeMillis();
            closeImage(binImage, 61.0f);
            ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);
            
            // convert the binary image into a VOI
            ViewUserInterface.getReference().getMessageFrame().append("    Extracting the new VOI: ", ViewJFrameMessage.DEBUG);
            time = System.currentTimeMillis();
            AlgorithmVOIExtraction VOIExtractionAlgo = new AlgorithmVOIExtraction(binImage);
            VOIExtractionAlgo.setRunningInSeparateThread(false);
            VOIExtractionAlgo.run();
            VOIExtractionAlgo.setCompleted(true);


            // make sure we got one VOI with one curve
            VOIVector vois = binImage.getVOIs();
            if(vois.size() != 1) {
                System.err.println("fixSubcutaneousFatVOIs() Error, did not get 1 VOI for the subcutaneous fat VOI");
                return;
            }
            
            // binImage has one VOI, lets get it
            VOI theFixedVOI = vois.get(0);
            
            // Keep only the largest VOI as the subcutaneous fat
            numCurves = theFixedVOI.getCurves().size();
                
            int maxIdx = 0;
            int maxNumPoints = ((VOIContour)theFixedVOI.getCurves().get(maxIdx)).size();
            for (int idx = 1; idx < numCurves; idx++) {
                if (((VOIContour)theFixedVOI.getCurves().get(idx)).size() > maxNumPoints) {
                    maxIdx = idx;
                }
            } // end for (int idx = 0; ...)
            for (int idx = numCurves-1; idx >= 0; idx--) {
                if (idx != maxIdx) {
                    theFixedVOI.getCurves().remove(idx);
                }
            } // end for (int idx = 0; ...)
            ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);

            ViewUserInterface.getReference().getMessageFrame().append("    Copying the new VOI: ", ViewJFrameMessage.DEBUG);
            time = System.currentTimeMillis();
            // copy a curve from the 2D image into the 3D image
            // new arrays need to be made for each slice because the array size
            // is used when importing the curve to the VOI.  The extracted VOI
            // cannot be determined, unless we resample the subcutaneous VOI
            
            // resample the subcutaneous fat VOI so it has the same number of
            // sampes as the abdomen VOI. Furthermore, the samples will be on a
            // throught the center of mass of the abdomen and the abdomen VOI samples
            resampleCurve(theFixedVOI, sliceIdx);
            curve = ((VOIContour)theFixedVOI.getCurves().get(0));
/*
            int[] newXVals;
            int[] newYVals;
            int[] newZVals;
            numPoints = curve.size();        try {
                newXVals = new int [numPoints];
                newYVals = new int [numPoints];
                newZVals = new int [numPoints];
            }  catch (OutOfMemoryError error) {
                System.gc();
                MipavUtil.displayError("fixSubcutaneousFatVOIs(): Can NOT allocate the subcutaneous fat VOI arrays");
                return;
            } 
            
            curve.exportArrays(newXVals, newYVals, newZVals);
            theVOI.importCurve(newXVals, newYVals, newZVals, sliceIdx);
*/
            curve.exportArrays(xVals, yVals, zVals);
            theVOI.importCurve(xVals, yVals, zVals);
            ViewUserInterface.getReference().getMessageFrame().append(+((System.currentTimeMillis() - time)) / 1000.0f +" sec\n", ViewJFrameMessage.DEBUG);
            
            // need to remove the curve because registering a VOI makes a copy of it
            sliceVOI.removeCurves();
            sliceImage.unregisterAllVOIs();

        } // end for (int sliceIdx = 0; ...)

        //ShowImage(sliceImage, "sliceImage");

    } // fixSubcutaneousFatVOIs()
    
    
    
    
    private void resampleCurve(VOI theVOI, int sliceIdx) {
        
        VOIContour curve = ((VOIContour)theVOI.getCurves().get(0));
        
        // use the same CM as for the abdomen VOI
        findAbdomenCM(sliceIdx);
        int xcm = centerOfMass[0];
        int ycm = centerOfMass[1];
//        ViewUserInterface.getReference().getMessageFrame().append("Xcm: " +xcm +"  Ycm: " +ycm);
        

        ArrayList<Integer> xValsVOI = new ArrayList<Integer>();
        ArrayList<Integer> yValsVOI = new ArrayList<Integer>();

        // angle in radians
        double angleRad;
       
        for (int angle = 0; angle < 360; angle += angularResolution) {
            int x = xcm;
            int y = ycm;
            double scaleFactor;      // reduces the number of trig operations that must be performed
     
            angleRad = Math.PI * angle / 180.0;
             if (angle > 315 || angle <= 45) {
                 // increment x each step
                 scaleFactor = Math.tan(angleRad);
                 while (x < xDim && curve.contains(x, y)) {
                     
                     // walk out in x and compute the value of y for the given radial line
                     x++;
                     y = ycm - (int)((x - xcm) * scaleFactor);
                 }
                 xValsVOI.add(x);
                 yValsVOI.add(y);
                 
             } else if (angle > 45 && angle <= 135) {
                 // decrement y each step
                 scaleFactor = (Math.tan((Math.PI / 2.0) - angleRad));
                 while (y > 0 && curve.contains(x, y)) {

                     // walk to the top of the image and compute values of x for the given radial line
                     y--;
                     x = xcm + (int)((ycm - y) * scaleFactor);
                 }
                 xValsVOI.add(x);
                 yValsVOI.add(y);
                                 
             } else if (angle > 135 && angle <= 225) {
                 // decrement x each step
                 scaleFactor = Math.tan(Math.PI - angleRad);
                 while (x > 0 && curve.contains(x, y)) {
                     
                     x--;
                     y = ycm - (int)((xcm - x) * scaleFactor);
                 }
                 xValsVOI.add(x);
                 yValsVOI.add(y);

             } else if (angle > 225 && angle <= 315) {
                 // increment y each step
                 scaleFactor = Math.tan((3.0 * Math.PI / 2.0) - angleRad);
                 while (y < yDim && curve.contains(x, y)) {
                     
                     y++;
                     x = xcm - (int)((y - ycm) * scaleFactor);
                 }
                 xValsVOI.add(x);
                 yValsVOI.add(y);
             }
         } // end for (angle = 0; ...
        
//        ViewUserInterface.getReference().getMessageFrame().append("resample VOI number of points: " +xValsAbdomenVOI.size());
        curve.clear();
        for(int idx = 0; idx < xValsVOI.size(); idx++) {
            // sliceIdx puts the curve on the correct slice... for z-ordered VOIs
            curve.add( new Vector3f( xValsVOI.get(idx), yValsVOI.get(idx), sliceIdx ) );
        }
    } // end resampleCurve(...)

    
    
    /**
     * So I don't forget how to print the contents of a VOI
     */
    @SuppressWarnings("unused")
    private void printVOI(VOI theVOI) {
        int numCurves;
        VOIContour curve;
        Vector<VOIBase>[] curves = theVOI.getSortedCurves(zDim);
        for (int sliceIdx = 0; sliceIdx < zDim; sliceIdx++) {
            numCurves = curves[sliceIdx].size();
            ViewUserInterface.getReference().getMessageFrame().append("Slice number: " +sliceIdx +"  number of curves: " +numCurves+"\n", ViewJFrameMessage.DEBUG);
            for (int curveIdx = 0; curveIdx < numCurves; curveIdx++) {
                curve = ((VOIContour)curves[sliceIdx].get(curveIdx));
                ViewUserInterface.getReference().getMessageFrame().append("  Curve: " +curveIdx +"  number of points: " +curve.size()+"\n", ViewJFrameMessage.DEBUG);
            } // end for (int curveIdx = 0; ...)
        } // end for (int sliceIdx = 0; ...)
    } // end printVOI(...)
    
    
    
    
    /**
     * make an intensity profile along radial lines between the abdomen VOI and the
     * abdomen center-of-mass.  Use this profile to determine where the subcutaneous
     * fat ends
     */
    @SuppressWarnings("unused")
    private void makeSubcutaneousFat2DVOI() {

        // find the center of mass of the single label object in the sliceBuffer (abdomenImage)
        findAbdomenCM();
        int xcm = centerOfMass[0];
        int ycm = centerOfMass[1];
        
        VOIVector vois = abdomenImage.getVOIs();
        VOI theVOI = vois.get(0);

        // there should be only one VOI and one curve
        VOIContour curve = ((VOIContour)theVOI.getCurves().get(0));
        int[] xVals = new int [curve.size()];
        int[] yVals = new int [curve.size()];
        int[] zVals = new int [curve.size()];
        curve.exportArrays(xVals, yVals, zVals);
        
        int[] xValsSubcutaneousVOI = new int [curve.size()];
        int[] yValsSubcutaneousVOI = new int [curve.size()];
        
        
        // the intensity profile along a radial line for a given angle
        short[] profile;
        
        // the x, y location of all the pixels along a radial line for a given angle
        int[] xLocs;
        int[] yLocs;
        try {
            profile = new short[xDim];
            xLocs = new int[xDim];
            yLocs = new int[xDim];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("findAbdomenVOI(): Can NOT allocate profile");
            return;
        }

        try {
            srcImage.exportData(0, sliceSize, sliceBuffer);
        } catch (IOException ex) {
            System.err.println("JCATsegmentAbdomen2D(): Error exporting data");
            return;
        }


        // find a subcutaneous fat contour point for each abdominal contour point
        // we know the abdominal contour points are located at three degree increments
        double angleRad;
        int count;
        int contourPointIdx = 0;
        for (int angle = 0; angle < 360; angle += angularResolution) {
            count = 0;
            int x = xcm;
            int y = ycm;
            int yOffset = y * xDim;
            double scaleFactor;      // reduces the number of trig operations that must be performed
     
            angleRad = Math.PI * angle / 180.0;
             if (angle > 315 || angle <= 45) {
                 // increment x each step
                 scaleFactor = Math.tan(angleRad);
                 while (x < xVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     profile[count] = sliceBuffer[yOffset + x];
                     xLocs[count] = x;
                     yLocs[count] = y;
                     count++;
                     
                     // walk out in x and compute the value of y for the given radial line
                     x++;
                     y = ycm - (int)((x - xcm) * scaleFactor);
                     yOffset = y * xDim;
                 }

                 // profile contains all the source image intensity values along the line from
                 // the center-of-mass to the abdomen VOI point
                 // Find where the subcutaneous fat ends and the muscle starts
                 
                 // start at the end of the profile array since its order is from the
                 // center-of-mass to the abdomen voi point
                 
                 int idx = count - 5;  // skip over the skin
                 while (idx >= 0 && profile[idx] < muscleThresholdHU && profile[idx] > airThresholdHU) {
                     idx--;
                 }
                 if (idx <= 0) {
                     MipavUtil.displayError("findAbdomenVOI(): Can NOT find visceral cavity in the intensity profile");
                     break;
                 }
                 xValsSubcutaneousVOI[contourPointIdx] = xLocs[idx];
                 yValsSubcutaneousVOI[contourPointIdx] = yLocs[idx];
                 
             } else if (angle > 45 && angle <= 135) {
                 // decrement y each step
                 scaleFactor = (Math.tan((Math.PI / 2.0) - angleRad));
                 while (y > yVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     profile[count] = sliceBuffer[yOffset + x];
                     xLocs[count] = x;
                     yLocs[count] = y;
                     count++;
                     
                     // walk to the top of the image and compute values of x for the given radial line
                     y--;
                     x = xcm + (int)((ycm - y) * scaleFactor);
                     yOffset = y * xDim;
                 }
                 
                 // profile contains all the source image intensity values along the line from
                 // the center-of-mass to the newly computed abdomen VOI point
                 // Find where the subcutaneous fat ends
                 int idx = count - 5;  // skip over the skin
                 while (idx >= 0 && profile[idx] < muscleThresholdHU&& profile[idx] > airThresholdHU) {
                     idx--;
                 }
                 if (idx == 0) {
                     MipavUtil.displayError("findAbdomenVOI(): Can NOT find visceral cavity in the intensity profile");
                     return;
                 }
                 xValsSubcutaneousVOI[contourPointIdx] = xLocs[idx];
                 yValsSubcutaneousVOI[contourPointIdx] = yLocs[idx];
                                  
             } else if (angle > 135 && angle <= 225) {
                 // decrement x each step
                 scaleFactor = Math.tan(Math.PI - angleRad);
                 while (x > xVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     profile[count] = sliceBuffer[yOffset + x];
                     xLocs[count] = x;
                     yLocs[count] = y;
                     count++;
                     
                     x--;
                     y = ycm - (int)((xcm - x) * scaleFactor);
                     yOffset = y * xDim;
                 }
                 
                 // profile contains all the source image intensity values along the line from
                 // the center-of-mass to the newly computed abdomen VOI point
                 // Find where the subcutaneous fat ends
                 int idx = count - 5;  // skip over the skin
                 while (idx >= 0 && profile[idx] < muscleThresholdHU&& profile[idx] > airThresholdHU) {
                     idx--;
                 }
                 if (idx == 0) {
                     MipavUtil.displayError("findAbdomenVOI(): Can NOT find visceral cavity in the intensity profile");
                     return;
                 }
                 xValsSubcutaneousVOI[contourPointIdx] = xLocs[idx];
                 yValsSubcutaneousVOI[contourPointIdx] = yLocs[idx];
 
             } else if (angle > 225 && angle <= 315) {
                 // increment y each step
                 scaleFactor = Math.tan((3.0 * Math.PI / 2.0) - angleRad);
                 while (y < yVals[contourPointIdx]) {
                     // store the intensity and location of each point along the radial line
                     profile[count] = sliceBuffer[yOffset + x];
                     xLocs[count] = x;
                     yLocs[count] = y;
                     count++;
                     
                     y++;
                     x = xcm - (int)((y - ycm) * scaleFactor);
                     yOffset = y * xDim;
                 }
                 
                 // profile contains all the source image intensity values along the line from
                 // the center-of-mass to the newly computed abdomen VOI point
                 // Find where the subcutaneous fat ends
                 int idx = count - 5;  // skip over the skin
                 while (idx >= 0 && profile[idx] < muscleThresholdHU&& profile[idx] > airThresholdHU) {
                     idx--;
                 }
                 if (idx == 0) {
                     MipavUtil.displayError("findAbdomenVOI(): Can NOT find visceral cavity in the intensity profile");
                     return;
                 }
                 xValsSubcutaneousVOI[contourPointIdx] = xLocs[idx];
                 yValsSubcutaneousVOI[contourPointIdx] = yLocs[idx];

             }
             
             contourPointIdx++;
         } // end for (angle = 0; ...


        // make the VOI's and add the points to them
        subcutaneousVOI = new VOI((short)0, "Subcutaneous area");
        subcutaneousVOI.importCurve(xValsSubcutaneousVOI, yValsSubcutaneousVOI, zVals);

    } // end makeSubcutaneousFat2DVOI()
    
    
    
    
    /**
     * Method not based on having the abdomen VOI, but rather determining both the
     * abdominal and visceral VOI in this step using the segmented abdomenImage
     */
    @SuppressWarnings("unused")
    private void JCATsegmentSubcutaneousFat2D() {
        
        // a buffer to store a slice from the source Image
        short[] srcBuffer;
        try {
            srcBuffer = new short [sliceSize];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("JCATsegmentVisceralFat2D(): Can NOT allocate srcBuffer");
            return;
        }
        
        // get the data from the segmented abdomenImage and the srcImage
        try {
            abdomenImage.exportData(0, sliceSize, sliceBuffer);
            srcImage.exportData(0, sliceSize, srcBuffer);
        } catch (IOException ex) {
//            System.err.println("JCATsegmentVisceralFat2D(): Error exporting data");
            MipavUtil.displayError("JCATsegmentVisceralFat2D(): Error exporting data");
            return;
        }
        
        // find the center of mass of the single label object in the sliceBuffer (abdomenImage)
        findAbdomenCM();

        // Use the CM, the abdomenImage, and the srcImage to define points on the
        // abdomen and visceral VOI's
        ArrayList<Integer> xArrAbdom = new ArrayList<Integer>();
        ArrayList<Integer> yArrAbdom = new ArrayList<Integer>();
        ArrayList<Integer> xArrVis = new ArrayList<Integer>();
        ArrayList<Integer> yArrVis = new ArrayList<Integer>();
        findVOIs(centerOfMass, xArrAbdom, yArrAbdom, srcBuffer, xArrVis, yArrVis);
        
        int[] x1 = new int[xArrAbdom.size()];
        int[] y1 = new int[xArrAbdom.size()];
        int[] z1 = new int[xArrAbdom.size()];
        for(int idx = 0; idx < xArrAbdom.size(); idx++) {
            x1[idx] = xArrAbdom.get(idx);
            y1[idx] = yArrAbdom.get(idx);
            z1[idx] = 0;
        }

        // make the VOI's and add the points to them
        abdomenVOI = new VOI((short)0, "Abdomen");
        abdomenVOI.importCurve(x1, y1, z1);
        

        for(int idx = 0; idx < xArrVis.size(); idx++) {
            x1[idx] = xArrVis.get(idx);
            y1[idx] = yArrVis.get(idx);
        }

        subcutaneousVOI = new VOI((short)0, "Subcutaneous area");
        subcutaneousVOI.importCurve(x1, y1, z1);
        
/*
        ViewUserInterface.getReference().getMessageFrame().append("Xcm: " +xcm +"  Ycm: " +ycm);
        sliceBuffer[ycm * xDim + xcm] = 20;
        for (int idx = 0; idx < xArr.size(); idx++) {
            sliceBuffer[yArr.get(idx) * xDim + xArr.get(idx)] = 20;
            sliceBuffer[yArrVis.get(idx) * xDim + xArrVis.get(idx)] = 30;
        }
        // save the sliceBuffer into the abdomenImage
        try {
            abdomenImage.importData(0, sliceBuffer, false);
        } catch (IOException ex) {
            System.err.println("segmentThighTissue(): Error importing data");
        }
*/
//        ShowImage(srcImage, "Segmented Abdomen");


//        ViewUserInterface.getReference().getMessageFrame().append("Abdomen VOI points:");
//        for (int idx = 0; idx < xArr.size(); idx++) {
//            ViewUserInterface.getReference().getMessageFrame().append(xArr.get(idx) +"  " + yArr.get(idx));
//        }

    } // end JCATsegmentSubcutaneousFat2D()
    
    
    
    /**
     * Determine the VOI of the abdomen at equal angular locations, given a region grown
     * approximation that may well contain holes.  We do anticipate the external skin
     * surface has been properly detected by the region grow since a seed point from that
     * area was likely used
     * 
     */
    @SuppressWarnings("unused")
    private void findAbdomenVOI() {
        
        // find the center of mass of the single label object in the sliceBuffer (abdomenImage)
        findAbdomenCM();
        int xcm = centerOfMass[0];
        int ycm = centerOfMass[1];

        ArrayList<Integer> xValsAbdomenVOI = new ArrayList<Integer>();
        ArrayList<Integer> yValsAbdomenVOI = new ArrayList<Integer>();

        // angle in radians
        double angleRad;
       
        for (int angle = 39; angle < 45; angle += 3) {
            int x, y, yOffset;
            double scaleFactor;      // reduces the number of trig operations that must be performed
     
            angleRad = Math.PI * angle / 180.0;
             if (angle > 315 || angle <= 45) {
                 scaleFactor = Math.tan(angleRad);
                 x = xDim;
                 y = ycm;
                 yOffset = y * xDim;
                 while (x > xcm && sliceBuffer[yOffset + x] != abdomenTissueLabel) {
                     x--;
                     y = ycm - (int)((x - xcm) * scaleFactor);
                     yOffset = y * xDim;
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
                 
             } else if (angle > 45 && angle <= 135) {
                 scaleFactor = (Math.tan((Math.PI / 2.0) - angleRad));
                 x = xcm;
                 y = 0;
                 yOffset = y * xDim;
                 while (y < ycm && sliceBuffer[yOffset + x] != abdomenTissueLabel) {
                     y++;
                     x = xcm + (int)((ycm - y) * scaleFactor);
                     yOffset = y * xDim;
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
                 
             } else if (angle > 135 && angle <= 225) {
                 scaleFactor = Math.tan(Math.PI - angleRad);
                 x = 0;
                 y = ycm;
                 yOffset = y * xDim;
                 while (x < xcm && sliceBuffer[yOffset + x] != abdomenTissueLabel) {
                     x++;
                     y = ycm - (int)((xcm - x) * scaleFactor);
                     yOffset = y * xDim;
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
                 
             } else if (angle > 225 && angle <= 315) {
                 scaleFactor = Math.tan((3.0 * Math.PI / 2.0) - angleRad);
                 x = xcm;
                 y = yDim;
                 yOffset = y * xDim;
                 while (y < yDim && sliceBuffer[yOffset + x] == abdomenTissueLabel) {
                     y--;
                     x = xcm - (int)((y - ycm) * scaleFactor);
                     yOffset = y * xDim;
                 }
                 xValsAbdomenVOI.add(x);
                 yValsAbdomenVOI.add(y);
                 
             }
         } // end for (angle = 0; ...
        
        int[] x1 = new int[xValsAbdomenVOI.size()];
        int[] y1 = new int[xValsAbdomenVOI.size()];
        int[] z1 = new int[xValsAbdomenVOI.size()];
        for(int idx = 0; idx < xValsAbdomenVOI.size(); idx++) {
            x1[idx] = xValsAbdomenVOI.get(idx);
            y1[idx] = xValsAbdomenVOI.get(idx);
            z1[idx] = 0;
        }

        // make the VOI's and add the points to them
        abdomenVOI = new VOI((short)0, "Abdomen");
        abdomenVOI.importCurve(x1, y1, z1);


    } // end findAbdomenVOI(...)
    
    
    
    /**
     * Compute both the abdominal and visercal VOI and store the points in ArrayLists
     * 
     * Starting at the center of mass of the label region, form intensity profiles from
     * the source image data and use thresholds to determine where the muscle starts.
     * 
     * Make radial lines every 3 degrees
     */
    private void findVOIs(short[] cm, ArrayList<Integer> xValsAbdomenVOI, ArrayList<Integer> yValsAbdomenVOI, short[] srcBuffer, ArrayList<Integer> xValsVisceralVOI, ArrayList<Integer> yValsVisceralVOI) {
        
        // angle in radians
       double angleRad;
       
       // the intensity profile along a radial line for a given angle
       short[] profile;
       
       // the x, y location of all the pixels along a radial line for a given angle
       int[] xLocs;
       int[] yLocs;
       try {
           profile = new short[xDim];
           xLocs = new int[xDim];
           yLocs = new int[xDim];
       } catch (OutOfMemoryError error) {
           System.gc();
           MipavUtil.displayError("findAbdomenVOI(): Can NOT allocate profile");
           return;
       }
       
       // the number of pixels along the radial line for a given angle
       int count;
       
       // The threshold value for muscle as specified in the JCAT paper
       int muscleThresholdHU = 16;
       
       for (int angle = 0; angle < 360; angle += angularResolution) {
           count = 0;
           int x = cm[0];
           int y = cm[1];
           int yOffset = y * xDim;
           double scaleFactor;      // reduces the number of trig operations that must be performed
    
           angleRad = Math.PI * angle / 180.0;
            if (angle > 315 || angle <= 45) {
                // increment x each step
                scaleFactor = Math.tan(angleRad);
                while (x < xDim && sliceBuffer[yOffset + x] == abdomenTissueLabel) {
                    // store the intensity and location of each point along the radial line
                    profile[count] = srcBuffer[yOffset + x];
                    xLocs[count] = x;
                    yLocs[count] = y;
                    count++;
                    
                    // walk out in x and compute the value of y for the given radial line
                    x++;
                    y = cm[1] - (int)((x - cm[0]) * scaleFactor);
                    yOffset = y * xDim;
                }
                
                // x, y is a candidate abdomen VOI point
                // if there are more abdomenTissueLabel pixels along the radial line,
                // then we stopped prematurely
                
                xValsAbdomenVOI.add(x);
                yValsAbdomenVOI.add(y);
                
                // profile contains all the source image intensity values along the line from
                // the center-of-mass to the newly computed abdomen VOI point
                // Find where the subcutaneous fat ends and the muscle starts
                
                // start at the end of the profile array since its order is from the
                // center-of-mass to the abdomen voi point
                
                int idx = count - 5;  // skip over the skin
                while (idx >= 0 && profile[idx] < muscleThresholdHU) {
                    idx--;
                }
                if (idx <= 0) {
                    MipavUtil.displayError("findAbdomenVOI(): Can NOT find visceral cavity in the intensity profile");
                    break;
                }
                xValsVisceralVOI.add(xLocs[idx]);
                yValsVisceralVOI.add(yLocs[idx]);
                
            } else if (angle > 45 && angle <= 135) {
                // decrement y each step
                scaleFactor = (Math.tan((Math.PI / 2.0) - angleRad));
                while (y > 0 && sliceBuffer[yOffset + x] == abdomenTissueLabel) {
                    // store the intensity and location of each point along the radial line
                    profile[count] = srcBuffer[yOffset + x];
                    xLocs[count] = x;
                    yLocs[count] = y;
                    count++;
                    
                    // walk to the top of the image and compute values of x for the given radial line
                    y--;
                    x = cm[0] + (int)((cm[1] - y) * scaleFactor);
                    yOffset = y * xDim;
                }
                xValsAbdomenVOI.add(x);
                yValsAbdomenVOI.add(y);
                
                // profile contains all the source image intensity values along the line from
                // the center-of-mass to the newly computed abdomen VOI point
                // Find where the subcutaneous fat ends
                int idx = count - 5;  // skip over the skin
                while (idx >= 0 && profile[idx] < muscleThresholdHU) {
                    idx--;
                }
                if (idx == 0) {
                    MipavUtil.displayError("findAbdomenVOI(): Can NOT find visceral cavity in the intensity profile");
                    return;
                }
                xValsVisceralVOI.add(xLocs[idx]);
                yValsVisceralVOI.add(yLocs[idx]);
                                
            } else if (angle > 135 && angle <= 225) {
                // decrement x each step
                scaleFactor = Math.tan(Math.PI - angleRad);
                while (x > 0 && sliceBuffer[yOffset + x] == abdomenTissueLabel) {
                    // store the intensity and location of each point along the radial line
                    profile[count] = srcBuffer[yOffset + x];
                    xLocs[count] = x;
                    yLocs[count] = y;
                    count++;
                    
                    x--;
                    y = cm[1] - (int)((cm[0] - x) * scaleFactor);
                    yOffset = y * xDim;
                }
                xValsAbdomenVOI.add(x);
                yValsAbdomenVOI.add(y);
                
                // profile contains all the source image intensity values along the line from
                // the center-of-mass to the newly computed abdomen VOI point
                // Find where the subcutaneous fat ends
                int idx = count - 5;  // skip over the skin
                while (idx >= 0 && profile[idx] < muscleThresholdHU) {
                    idx--;
                }
                if (idx == 0) {
                    MipavUtil.displayError("findAbdomenVOI(): Can NOT find visceral cavity in the intensity profile");
                    return;
                }
                xValsVisceralVOI.add(xLocs[idx]);
                yValsVisceralVOI.add(yLocs[idx]);

            } else if (angle > 225 && angle <= 315) {
                // increment y each step
                scaleFactor = Math.tan((3.0 * Math.PI / 2.0) - angleRad);
                while (y < yDim && sliceBuffer[yOffset + x] == abdomenTissueLabel) {
                    // store the intensity and location of each point along the radial line
                    profile[count] = srcBuffer[yOffset + x];
                    xLocs[count] = x;
                    yLocs[count] = y;
                    count++;
                    
                    y++;
                    x = cm[0] - (int)((y - cm[1]) * scaleFactor);
                    yOffset = y * xDim;
                }
                xValsAbdomenVOI.add(x);
                yValsAbdomenVOI.add(y);
                
                // profile contains all the source image intensity values along the line from
                // the center-of-mass to the newly computed abdomen VOI point
                // Find where the subcutaneous fat ends
                int idx = count - 5;  // skip over the skin
                while (idx >= 0 && profile[idx] < muscleThresholdHU) {
                    idx--;
                }
                if (idx == 0) {
                    MipavUtil.displayError("findAbdomenVOI(): Can NOT find visceral cavity in the intensity profile");
                    return;
                }
                xValsVisceralVOI.add(xLocs[idx]);
                yValsVisceralVOI.add(yLocs[idx]);

            }
        } // end for (angle = 0; ...

    } // end findVOIs(...)

    
    
    /**
     * Find the center of mass of the object on sliceNum that has the abdomenTissueLabel value
      */
    private void findAbdomenCM(int sliceNum) {
        
        // load up the sliceBuffer with the appropriate slice from the abdomen label image
        try {
            abdomenImage.exportData(sliceNum * sliceSize, sliceSize, sliceBuffer);
        } catch (IOException ex) {
            System.err.println("findAbdomenCM(...) Error exporting data on slice: " +sliceNum);
            return;
        }

        int xcm = 0, ycm = 0, pixCount = 0;
        for (int idx= 0, y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++, idx++) {
                if (sliceBuffer[idx] == abdomenTissueLabel) {
                    xcm += x;
                    ycm += y;
                    pixCount++;
                }
            } // end for (int x = 0; ...)
        } // end for (int idx = 0, ...)
        
        if (pixCount == 0) {
//            System.err.println("findAbdomenCM(): No pixels with abdomenTissueLabel in segmented image");
            MipavUtil.displayError("findAbdomenCM(): No pixels with abdomenTissueLabel in segmented image");
            // set the centerOfMass to an generate an obvious error 
            centerOfMass[0] = (short)-1;
            centerOfMass[1] = (short)-1;
            return;
        }
        
        centerOfMass[0] = (short)(xcm / pixCount);
        centerOfMass[1] = (short)(ycm / pixCount);
    } // end findAbdomenCM(...)

    
    
    /**
     * Find the center of mass of the object in sliceBuffer that has the abdomenTissueLabel value
     * 
     * @param vals  Array to store the 2D center-of-mass
     */
    private void findAbdomenCM() {
        
        if (foundCenterOfMass) return;
        int xcm = 0, ycm = 0, pixCount = 0;
        for (int idx= 0, y = 0; y < yDim; y++) {
            for (int x = 0; x < xDim; x++, idx++) {
                if (sliceBuffer[idx] == abdomenTissueLabel) {
                    xcm += x;
                    ycm += y;
                    pixCount++;
                }
            } // end for (int x = 0; ...)
        } // end for (int idx = 0, ...)
        
        if (pixCount == 0) {
//            System.err.println("findAbdomenCM(): No pixels with abdomenTissueLabel in segmented image");
            MipavUtil.displayError("findAbdomenCM(): No pixels with abdomenTissueLabel in segmented image");
            return;
        }
        
        centerOfMass[0] = (short)(xcm / pixCount);
        centerOfMass[1] = (short)(ycm / pixCount);
        foundCenterOfMass = true;
    } // end findAbdomenCM(...)
    
    
    
    /**
     * segment out the visceral fat from the image, version 01
     */
    @SuppressWarnings("unused")
    private void JCATsegmentVisceralFat2D01() {
        
        // get the VOI for the external boundary of the abdomen
        VOIVector vois = abdomenImage.getVOIs();
        if(vois.size() != 1) {
            System.err.println("segmentVisceralFat2D() Error, did not get 1 VOI");
            return;
        }

        // abdomenImage has one VOI, lets get it
        VOI theVOI = vois.get(0);
 
        // find the center-of-mass of the contour
        VOIContour maxContour = ((VOIContour)theVOI.getCurves().get(0));
        int[] xVals = new int [maxContour.size()];
        int[] yVals = new int [maxContour.size()];
        int[] zVals = new int [maxContour.size()];
        maxContour.exportArrays(xVals, yVals, zVals);
        
        int xcm = 0, ycm = 0, zcm = 0;
        for (int idx = 0; idx < maxContour.size(); idx++) {
            xcm += xVals[idx];
            ycm += yVals[idx];
            zcm += zVals[idx];
        }
        
        xcm /= maxContour.size();
        ycm /= maxContour.size();
        zcm /= maxContour.size();
        
        ViewUserInterface.getReference().getMessageFrame().append("Xcm: " +xcm +"  Ycm: " +ycm +"  Zcm: " +zcm+"\n", ViewJFrameMessage.DEBUG);
        
        // This point should be inside the abdomen
        // walk right until you find the external border of the abdomen
        
        // update the volumeBitSet to match the closed abdomenImage
        short[] srcSliceBuffer = new short[sliceSize];
        short[] profile = new short[xDim];
        try {
            abdomenImage.exportData(0, sliceSize, sliceBuffer);
            srcImage.exportData(0, sliceSize, srcSliceBuffer);
                   } catch (IOException ex) {
            System.err.println("Error exporting data");
            return;
        }

        int x = xcm;
        int elementCount = 0;
        int yOffset = ycm * xDim;
        while (x < xDim && sliceBuffer[x + yOffset] == abdomenTissueLabel) {
            profile[elementCount] = srcSliceBuffer[x + yOffset];
            x++;
            elementCount++;
        } // end while(...)
        
        // profile has an intensity profile of the pixels along the ray from the 
        // contour CM to the external skin boundary. 
        
        
    } // end JCATsegmentVisceralFat2D01()
    
    
    
    
    
    
    
    
     
    /**
     * Moves seed point to ideal intensity
     * @param center image center of mass
     * @return
     */
    @SuppressWarnings("unused")
    private double[] getSeedPoint(double[] center) {
        double lowIntensity = -50;
        double highIntensity = 50;
        
        int originalX = (int)center[0], currentX = (int)center[0];
        int originalY = (int)center[1], currentY = (int)center[1];
        
        boolean pointFound = false;
        
        while(!pointFound && currentX < xDim) {
            if(srcImage.get(currentX, currentY, (int)center[2]).doubleValue() < highIntensity &&
                    srcImage.get(currentX, currentY, (int)center[2]).doubleValue() > lowIntensity) {
                pointFound = true;
                break;
            }
            if(currentX - originalX > currentY - originalY)
                currentY++;
            else
                currentX++;
        }
        
        if(pointFound) {
            center[0] = currentX;
            center[1] = currentY;
        }
        
        return center;
        
    }
    
    @SuppressWarnings("unused")
    private void regionGrowAbdomen(short sX, short sY, short sZ, BitSet muscleBits) {
       try {
           AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(srcImage, 1.0f, 1.0f);

           regionGrowAlgo.setRunningInSeparateThread(false);

           if (srcImage.getNDims() == 2) {
               regionGrowAlgo.regionGrow2D(muscleBits, new Point(sX, sY), -1,
                                           false, false, null, -100,
                                           500, -1, -1, false);
           } else if (srcImage.getNDims() == 3) {
               CubeBounds regionGrowBounds;
               regionGrowBounds = new CubeBounds(xDim, 0, yDim, 0, zDim, 0);
               regionGrowAlgo.regionGrow3D(muscleBits, new Point3D(sX, sY, sZ), -1,
                                                   false, false, null, -100,
                                                   500, -1, -1, false,
                                                   0, regionGrowBounds);
//               ViewUserInterface.getReference().getMessageFrame().append("Muscle Count: " +count);
           }
       } catch (OutOfMemoryError error) {
           System.gc();
           MipavUtil.displayError("Out of memory: regionGrowMuscle");
       }

   } // regionGrowMuscle(...)
   
   public VOI getAbdomenVOI() {
       return abdomenVOI;
   }
   
   public VOI getSubcutaneousVOI() {
       return subcutaneousVOI;
   }
   
   public ModelImage threshold(ModelImage threshSourceImg, float[] thresh) {
        ModelImage resultImage = null;
        resultImage = new ModelImage(ModelStorageBase.UBYTE, threshSourceImg.getExtents(), "threshResultImg");

        AlgorithmThresholdDual threshAlgo = null;
        threshAlgo = new AlgorithmThresholdDual(resultImage, threshSourceImg, thresh, abdomenTissueLabel, AlgorithmThresholdDual.BINARY_TYPE, true, false);
        threshAlgo.run();

        return resultImage;
    } // end threshold(...)
   
   /**
    * Mathematical Morphological closing operation
    */
   public void closeImage(ModelImage img) {
       AlgorithmMorphology2D MorphIDObj = null;
       MorphIDObj = new AlgorithmMorphology2D(img, 2, 1, AlgorithmMorphology2D.CLOSE, 4, 4, 0, 0, true);
       MorphIDObj.run();
   } // end closeImage(...)
  
   /**
    * Mathematical Morphological closing operation as a series of 2D operations
    */
   public void closeImage25D(ModelImage img) {
       
       // img is a 3D image.  break it into a series of 2D images
       int[] extents = new int[2];
       extents[0] = xDim;
       extents[1] = yDim;
       
       ModelImage img2D = new ModelImage(ModelStorageBase.USHORT, extents, "img2D");
       AlgorithmMorphology2D MorphIDObj = null;
       MorphIDObj = new AlgorithmMorphology2D(img2D, 2, 1, AlgorithmMorphology2D.CLOSE, 4, 4, 0, 0, true);

       for (int z = 0; z < zDim; z++) {
           // fill up the slice buffer
           try {
               img.exportData(z * sliceSize, sliceSize, sliceBuffer);
           } catch (IOException ex) {
               System.err.println("PlugInAlgorithmCTAbdomen.closeImage25D(): Error exporting data on slice: " +z);
               return;
           }
           // save the sliceBuffer into img2D
           try {
               img2D.importData(0, sliceBuffer, false);
           } catch (IOException ex) {
               System.err.println("PlugInAlgorithmCTAbdomen.closeImage25D(): Error importing data");
           }
           
           // run the morphology
           MorphIDObj.run();

           try {
               img2D.exportData(0, sliceSize, sliceBuffer);
           } catch (IOException ex) {
               System.err.println("PlugInAlgorithmCTAbdomen.closeImage25D(): Error exporting data on slice: " +z);
               return;
           }
           // save the sliceBuffer into the source image
           try {
               img.importData(z * sliceSize, sliceBuffer, false);
           } catch (IOException ex) {
               System.err.println("PlugInAlgorithmCTAbdomen.closeImage25D(): Error importing data");
           }
 
       } // end for (z = 0; ...)
       
   } // end closeImage25D(...)

   /**
    * Mathematical Morphological closing operation
    */
   public void closeImage3D(ModelImage img) {
       AlgorithmMorphology3D MorphIDObj = null;
       MorphIDObj = new AlgorithmMorphology3D(img, 2, 1, AlgorithmMorphology3D.CLOSE, 2, 2, 0, 0, true);
       MorphIDObj.run();
   } // end closeImage3D(...)

   /**
    * Mathematical Morphological closing operation
    */
   public void closeImage(ModelImage img, float dia) {
       AlgorithmMorphology2D MorphIDObj = null;
       //TODO: Try various diameters, construct (2D) image dimension-based method
       double idealRes = ((img.getExtents()[0]+img.getExtents()[1])/2.0+zDim)/(img.getExtents()[0]+img.getExtents()[1]+zDim)/3.0*30;
       ViewUserInterface.getReference().getMessageFrame().append("Try resolutions: "+idealRes+"\n", ViewJFrameMessage.DEBUG);
       MorphIDObj = new AlgorithmMorphology2D(img, 0, (int)dia, AlgorithmMorphology2D.CLOSE, 1, 1, 0, 0, true);
       //nia/subset uses 30
       MorphIDObj.run();
   } // end closeImageCircle(...)

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



	public long getSegmentationTimeAbd() {
		return segmentationTimeAbd;
	}



	public long getSegmentationTimeSub() {
		return segmentationTimeSub;
	}
}

