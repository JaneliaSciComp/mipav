import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCenterOfMass;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology2D;
import gov.nih.mipav.model.algorithms.AlgorithmMorphology3D;
import gov.nih.mipav.model.algorithms.AlgorithmRegionGrow;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.AlgorithmVOIExtractionPaint;
import gov.nih.mipav.model.algorithms.AlgorithmSnake;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.CubeBounds;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.Point3Ds;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIVector;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Color;
import java.awt.Point;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Vector;



public class PlugInAlgorithmCTAbdomen extends AlgorithmBase implements AlgorithmInterface {
    
    /** X dimension of the CT image */
    private int xDim;

    /** Y dimension of the CT image */
    private int yDim;

    /** Z dimension of the CT image */
    private int zDim;

    /** Slice size for xDim*yDim */
    private int sliceSize;

    private ModelImage abdomenImage;
    
    // center-of-mass array for region 1 and 2 (the thresholded bone)
    private double[] center;
    
    // temp buffer to store slices.  Needed in many member functions.
    private short[] sliceBuffer;

    private short abdomenTissueLabel = 10;
    
    private boolean initializedFlag = false;
    
    private BitSet volumeBitSet;

    /**The final abdomen VOI*/
    private VOI abdomenVOI;
    private VOI visceralVOI;
       
    private String imageDir;
    
    private Color voiColor;
    
    private AlgorithmCenterOfMass comAlgo;
    
    /**
     * Constructor.
     *
     * @param  resultImage  Result image model
     * @param  srcImg       Source image model.
     */
    public PlugInAlgorithmCTAbdomen(ModelImage resultImage, ModelImage srcImg, String imageDir, Color color) {
        super(resultImage, srcImg);
        
        this.imageDir = imageDir+File.separator;
        this.voiColor = color;

        abdomenImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "thighTissueImage");
        abdomenVOI = null;
        visceralVOI = null;
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
        
        //  This finds a center of mass in mm coordinates and it is weighted by pixel intensity
        // I don't think we want this
/*        
        getCenterOfMass();
        while(!comAlgo.isCompleted()) {}
        if(comAlgo.isCompleted()) {
        	center = comAlgo.getCenterOfMass();
            System.out.println("Found center: "+center[0]+"\t"+center[1]+"\t"+center[2]);
            segmentImage();
        }
*/
        if (srcImage.getNDims() == 2) {
            JCATsegmentAbdomen2D();
            
            // Let's not make the VOI at this point
//            JCATmakeAbdomen2DVOI();
//            ShowImage(abdomenImage, "Segmented Abdomen");
            
            JCATsegmentSubcutaneousFat2D();
            snakeViseralVOI();

            srcImage.unregisterAllVOIs();
            srcImage.registerVOI(abdomenVOI);
            srcImage.registerVOI(visceralVOI);

        }


    } // end runAlgorithm()

    public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub
    	if (algorithm instanceof AlgorithmCenterOfMass) {
            
            System.out.println("Center of mass completed");
    	}        
	}



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
        abdomenImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "thighTissueImage");
//        muscleBundleImage = new ModelImage(ModelStorageBase.USHORT, srcImage.getExtents(), "muscleBundleImage");
       
        // make the resolutions of the images the same as the source image
        for (int i = 0; i < zDim; i++) {
            abdomenImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[i].getResolutions());
        }
        
        volumeBitSet = new BitSet();
               
        // set initialized flag to true so the data structures are not reallocated
        initializedFlag = true;
    } // end init()
    
    
    /**
     * Apply the snake algorithm to the visceral VOI
     */
    private void snakeViseralVOI() {
        
        // set the visceral VOI as active
        visceralVOI.setActive(true);
        visceralVOI.getCurves()[0].elementAt(0).setActive(true);
        
        float[] sigmas = new float[2];
        sigmas[0] = 1.0f;
        sigmas[1] = 1.0f;
        
        AlgorithmSnake snake = new AlgorithmSnake(srcImage, sigmas, 50, 2, visceralVOI, AlgorithmSnake.OUT_DIR);
        snake.run();

        visceralVOI = snake.getResultVOI();
        
    } // end snakeViseralVOI
    
    
    
    
    
    
    /**
     * Our adaptation of the algorithm presented in the 2006 JCAT article.  The published method
     * included the blanket under the patient as part of the segmentation.  We threshold and
     * region grow using values to "undersegment" the image.  We then clean up the segmentation
     * using mathematical morphology
     * 
     * I have taken out updating the volumeBitSet for the properly segmented abdomen since I
     * will not be using it to find the VOI
     */
    private void JCATsegmentAbdomen2D() {
        // find a seed point inside the fat for a region grow
        try {
            srcImage.exportData(0, sliceSize, sliceBuffer);
        } catch (IOException ex) {
            System.err.println("Error exporting data");
            return;
        }
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
            MipavUtil.displayError("PlugINAlgorithmCTAbdomen::segmentThigh2D(): Failed to find a seed location for the region grow");
            return;
        }

        AlgorithmRegionGrow regionGrowAlgo = new AlgorithmRegionGrow(srcImage, 1.0f, 1.0f);
        regionGrowAlgo.setRunningInSeparateThread(false);

        // under segment so that we do not get the blanket
        regionGrowAlgo.regionGrow2D(volumeBitSet, new Point(seedX, seedY), -1,
                                    false, false, null, seedVal - 50,
                                    seedVal + 1500, -1, -1, false);
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
            System.err.println("segmentThighTissue(): Error importing data");
        }

        // do a mathematical morphology closing operation to fill the small gaps
        closeImage(abdomenImage);
        
        // update the volumeBitSet to match the closed abdomenImage
/*
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
*/
    } // end JCATsegmentAbdomen2D()  
    
    
    
    /**
     * Extract a 2D VOI from the volumeBitSet that was set during the JCATsegmentAbdomen2D call
     */
    private boolean JCATmakeAbdomen2DVOI() {
        abdomenImage.setMask(volumeBitSet);
        
        // volumeBitSet should be set for the abdomen tissue
        short voiID = 0;
        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(abdomenImage,
                volumeBitSet, xDim, yDim, zDim, voiID);

        algoPaintToVOI.setRunningInSeparateThread(false);
        algoPaintToVOI.run();
        setCompleted(true);
        
        // make sure we got one VOI composed of two curves
        VOIVector vois = abdomenImage.getVOIs();
        if(vois.size() != 1) {
            System.err.println("makeAbdomen2DVOI() Error, did not get 1 VOI");
            return false;
        }
        
        // thighTissueImage has one VOI, lets get it
        VOI theVOI = vois.get(0);
        theVOI.setName("Abdomen Tissue");

        // Remove small (10 points or less) curves from the VOI
        int numCurves, numPoints;
        VOIContour curve;
        for (int idx = 0; idx < zDim; idx++) {
            numCurves = theVOI.getCurves()[idx].size();
            
            int idx2 = 0;
            while(idx2 < numCurves) {
                curve = ((VOIContour)theVOI.getCurves()[idx].get(idx2));
                numPoints = curve.size();
                if (numPoints < 10) {
                    // remove the curve
                    theVOI.getCurves()[idx].remove(idx2);
                    numCurves--;
                } else {
                    idx2++;
                }
            } // end while(idx2 < numCurves)
         } // end for (int idx = 0; ...)
        return true;
    } // end JCATmakeAbdomen2DVOI()
    
    
    
    /**
     * Method not based on having the abdomen VOI, but rather determining both the
     * abdominal and visceral  VOI in this step using the segmented abdomenImage
     */
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
        
        // center-of-mass array, so we can pass by reference into a function
        int[] cmArray;
        try {
            cmArray = new int [2];
        } catch (OutOfMemoryError error) {
            System.gc();
            MipavUtil.displayError("JCATsegmentVisceralFat2D(): Can NOT allocate cmArray");
            return;
        }
        
        // find the center of mass of the single label object in the sliceBuffer (abdomenImage)
        findAbdomenCM(cmArray);
        int xcm = cmArray[0];
        int ycm = cmArray[1];

        // Use the CM, the abdomenImage, and the srcImage to define points on the
        // abdomen and visceral VOI's
        ArrayList<Integer> xArrAbdom = new ArrayList<Integer>();
        ArrayList<Integer> yArrAbdom = new ArrayList<Integer>();
        ArrayList<Integer> xArrVis = new ArrayList<Integer>();
        ArrayList<Integer> yArrVis = new ArrayList<Integer>();
        findVOIs(cmArray, xArrAbdom, yArrAbdom, srcBuffer, xArrVis, yArrVis);
        
        int[] x1 = new int[xArrAbdom.size()];
        int[] y1 = new int[xArrAbdom.size()];
        int[] z1 = new int[xArrAbdom.size()];
        for(int idx = 0; idx < xArrAbdom.size(); idx++) {
            x1[idx] = xArrAbdom.get(idx);
            y1[idx] = yArrAbdom.get(idx);
            z1[idx] = 0;
        }

        // make the VOI's and add the points to them
        abdomenVOI = new VOI((short)0, "Abdomen", 0);
        Vector[] v = new Vector[zDim];
        for(int idx = 0; idx < zDim; idx++) {
            v[idx] = new Vector();
        }
        abdomenVOI.setCurves(v);
        abdomenVOI.importCurve(x1, y1, z1, 0);
        

        for(int idx = 0; idx < xArrVis.size(); idx++) {
            x1[idx] = xArrVis.get(idx);
            y1[idx] = yArrVis.get(idx);
        }

        visceralVOI = new VOI((short)0, "Visceral", 1);
        visceralVOI.importCurve(x1, y1, z1, 0);
        
/*
        System.out.println("Xcm: " +xcm +"  Ycm: " +ycm);
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


//        System.out.println("Abdomen VOI points:");
//        for (int idx = 0; idx < xArr.size(); idx++) {
//            System.out.println(xArr.get(idx) +"  " + yArr.get(idx));
//        }

    } // end JCATsegmentSubcutaneousFat2D()
    
    
    
    /**
     * Compute both the abdominal and visercal VOI and store the points in ArrayLists
     * 
     * Starting at the center of mass of the label region, form intensity profiles from
     * the source image data and use thresholds to determine where the muscle starts.
     * 
     * Make radial lines every 3 degrees
     */
    private void findVOIs(int[] cm, ArrayList<Integer> xValsAbdomenVOI, ArrayList<Integer> yValsAbdomenVOI, short[] srcBuffer, ArrayList<Integer> xValsVisceralVOI, ArrayList<Integer> yValsVisceralVOI) {
        
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
       
       // The threholds value for muscle as specified in the JCAT paper
       int muscleThresholdHU = 16;
       
       for (int angle = 0; angle < 360; angle += 3) {
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
                if (idx == 0) {
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
     * Find the center of mass of the object in sliceBuffer that has the abdomenTissueLabel value
     * 
     * @param vals  Array to store the 2D center-of-mass
     */
    private void findAbdomenCM(int vals[]) {
        
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
        
        vals[0] = xcm / pixCount;
        vals[1] = ycm / pixCount;
    } // end findAbdomenCM(...)
    
    
    
    /**
     * segment out the visceral fat from the image, version 01
     */
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
        VOIContour maxContour = ((VOIContour)theVOI.getCurves()[0].get(0));
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
        
        System.out.println("Xcm: " +xcm +"  Ycm: " +ycm +"  Zcm: " +zcm);
        
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
     * Find the bone, bone marrow, and the thigh tissue
     */
    private void segmentImage() {
        long time = System.currentTimeMillis();
        boolean doVOI = true, completeVOI = false;
        // compute the bone label image

        time = System.currentTimeMillis();
        
        if(comAlgo.isCompleted())
        	doVOI = segmentAbdomenTissue(center);
        System.out.println("Abdomen tissue segmentation: "+(System.currentTimeMillis() - time));

        ShowImage(destImage, "Segmented Image");
        
        time = System.currentTimeMillis();
        if(doVOI)
        	completeVOI = makeAbdomenTissueVOI();
        System.out.println("Thigh tissue VOIs: "+(System.currentTimeMillis() - time));
        if(completeVOI) {
	        
	     // save the VOI to a file(s)
	        String directory = System.getProperty("user.dir");
	        System.out.println("directory: " +imageDir);
	        FileVOI fileVOI;
	        
	        String fileName = "Abdomen.xml";
	        try {
	            fileVOI = new FileVOI(fileName, imageDir, abdomenImage);
	            fileVOI.writeVOI(abdomenVOI, true);
	        } catch (IOException ex) {
	            System.err.println("Error segmentImage():  Opening VOI file");
	            return;
	        }     
        } else
        	System.err.println("Automatic VOIs not created");

   } // create a voi for the outside of the thigh.  Assumes the thighTissueImage has been created.
    private boolean makeAbdomenTissueVOI() {
        // make the volumeBitSet for the thigh tissue
        boolean completedThigh = true;
        
        srcImage.setMask(volumeBitSet);
        
        // volumeBitSet should be set for the thigh tissue
        short voiID = 0;
        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(srcImage,
                volumeBitSet, xDim, yDim, zDim, voiID);

        algoPaintToVOI.setRunningInSeparateThread(false);
        algoPaintToVOI.run();
        setCompleted(true);
        
        // make sure we got one VOI composed of two curves
        VOIVector vois = srcImage.getVOIs();
        if(vois.size() != 1) {
            System.err.println("makeThighTissueVOI() Error, did not get 1 VOI");
            return false;
        }
        
        // thighTissueImage has one VOI, lets get it
        VOI theVOI = vois.get(0);
        theVOI.setName("Thigh Tissue");

        // Remove small (10 points or less) curves from the VOI
        int numCurves, numPoints;
        VOIContour curve;
        for (int idx = 0; idx < zDim; idx++) {
            numCurves = theVOI.getCurves()[idx].size();
            
            int idx2 = 0;
            while(idx2 < numCurves) {
                curve = ((VOIContour)theVOI.getCurves()[idx].get(idx2));
                numPoints = curve.size();
                if (numPoints < 10) {
                    // remove the curve
                    theVOI.getCurves()[idx].remove(idx2);
                    numCurves--;
                } else {
                    idx2++;
                }
            } // end while(idx2 < numCurves)
         } // end for (int idx = 0; ...)

/*
        // print out the curves and their sizes
        for (int idx = 0; idx < zDim; idx++) {
            numCurves = theVOI.getCurves()[idx].size();
            System.out.println("slice: " +idx +"  number of curves: " +numCurves);
            
            // print out the size of each curve
            for (int idx2 = 0; idx2 < numCurves; idx2++) {
                curve = ((VOIContour)theVOI.getCurves()[idx].get(idx2));
                numPoints = curve.size();
                System.out.println("  curve: " +idx2 +"  num points: " +numPoints);
            } // end for (int idx2 = 0; ...)
        } // end for (int idx = 0; ...)
*/        

        // split the thigh curves when the legs touch (there are only 3 curves, not 4)
        for (int sliceIdx = 0; sliceIdx < zDim; sliceIdx++) {
            if (theVOI.getCurves()[sliceIdx].size() == 3) {
                
                // find the curve with the greatest area (the thigh curve)
                float maxArea = ((VOIContour)theVOI.getCurves()[sliceIdx].get(0)).area();
                int maxIdx = 0;
                for (int idx = 1; idx < theVOI.getCurves()[sliceIdx].size(); idx++) {
                    if (((VOIContour)theVOI.getCurves()[sliceIdx].get(idx)).area() > maxArea) {
                        maxIdx = idx;
                    }
                }
                
//                System.out.println("Slice num: " +sliceIdx +"   thigh curve idx: " +maxIdx);

                /*
                 The next part of the code may be error prone.  startIdx1 should be between 
                 the zeroth contour point and the minimum point on the top part of the contour,
                 the point a index upperCurveMinIdx.  endIdx2 should be after minIdx.  startIdx2
                 should come next, followed by the maximum point on the bottom part of the contour
                 (lowerCurveMaxIdx) and finally endIdx2, which should be less than the number of points
                 in the contour.  If these assumptions are not correct, this part of the code
                 will fail, and we will not be able to split a joined single thigh contour into
                 a left and right thigh contour
                 
                 */
                // split the curve through its middle with the maxIdx
                VOIContour maxContour = ((VOIContour)theVOI.getCurves()[sliceIdx].get(maxIdx));
                int[] xVals = new int [maxContour.size()];
                int[] yVals = new int [maxContour.size()];
                int[] zVals = new int [maxContour.size()];
                maxContour.exportArrays(xVals, yVals, zVals);
                
                // find the indices of an upper and lower section of the contour that is "close" to the image center
                // find the index of the first contour point whose x-component is "close" to the middle
                // startIdx1 represents the first point on the contour that is within 20 units of the image center
                int startIdx1 = 0;
                while (xVals[startIdx1] <= ((xDim / 2) - 20) || xVals[startIdx1] >= ((xDim / 2) + 20)) {
                    startIdx1++;
                }
                
                // endIdx1 is the last point on the top curve
                int endIdx1 = startIdx1 + 1;
                while (xVals[endIdx1] >= ((xDim / 2) - 20) && xVals[endIdx1] <= ((xDim / 2) + 20)) {
                    endIdx1++;
                }
                // subtract 1 since we indexed one element too far
                endIdx1--;
//                System.out.println("\nStart index 1: " +startIdx1 +"   end index 1: " +endIdx1);

                // find the index of the second contour section whose x-component is "close" to the middle
                int startIdx2 = endIdx1 + 1;
                while (xVals[startIdx2] <= ((xDim / 2) - 20) || xVals[startIdx2] >= ((xDim / 2) + 20)) {
                    startIdx2++;
                }
                
                int endIdx2 = startIdx2 + 1;
                while (xVals[endIdx2] >= ((xDim / 2) - 20) && xVals[endIdx2] <= ((xDim / 2) + 20)) {
                    endIdx2++;
                }
                // subtract 1 since we indexed one element too far
                endIdx2--;
//                System.out.println("Start index 2: " +startIdx2 +"   end index 1: " +endIdx2);

     
                // find the index of the two closest points between these two sections, this is where we will split the contour
                // one contour section goes from startIdx1 to endIdx1
                // the other goes from startIdx2 to endIdx2
                int upperCurveMinIdx = startIdx1;
                int lowerCurveMaxIdx = startIdx2;
                float dx = xVals[startIdx1] - xVals[startIdx2];
                float dy = yVals[startIdx1] - yVals[startIdx2];
                // all computations will be on the same slice, forget about the z-component
                float minDistance = dx*dx + dy*dy;
                float dist;
                
                for (int idx1 = startIdx1; idx1 <= endIdx1; idx1++) {
                    for (int idx2 = startIdx2; idx2 <= endIdx2; idx2++) {
                        dx = xVals[idx1] - xVals[idx2];
                        dy = yVals[idx1] - yVals[idx2];
                        dist = dx*dx + dy*dy;
                        if (dist < minDistance) {
                            minDistance = dist;
                            upperCurveMinIdx = idx1;
                            lowerCurveMaxIdx = idx2;
                        } // end if
                    } // end for (int idx2 = startIdx2; ...
                } // end for (int idx1 = startIdx1; ...
                
//                System.out.println("Slice: " + sliceIdx +"   Minimum distance: " +minDistance);
//                System.out.println(xVals[upperCurveMinIdx] +"  " +yVals[upperCurveMinIdx] +"   and  " +xVals[lowerCurveMaxIdx] +"  " +yVals[lowerCurveMaxIdx]);
                
                // make the two contours resulting from the split
                ArrayList<Integer> x1Arr = new ArrayList<Integer>(maxContour.size());
                ArrayList<Integer> y1Arr = new ArrayList<Integer>(maxContour.size());
                ArrayList<Integer> z1Arr = new ArrayList<Integer>(maxContour.size());
                
                ArrayList<Integer> x2Arr = new ArrayList<Integer>(maxContour.size());
                ArrayList<Integer> y2Arr = new ArrayList<Integer>(maxContour.size());
                ArrayList<Integer> z2Arr = new ArrayList<Integer>(maxContour.size());
                
                if (upperCurveMinIdx < lowerCurveMaxIdx) {
                    // the first contour (left thigh) goes from minIdx1 to minIdx2 to minIdx1
                    int newIdx = 0;
                    for (int idx = upperCurveMinIdx; idx < lowerCurveMaxIdx; idx++, newIdx++) {
                        x1Arr.add(newIdx, xVals[idx]);
                        y1Arr.add(newIdx, yVals[idx]);
                        z1Arr.add(newIdx, zVals[idx]);
                    }

                    // the second contour (right thigh) goes from minIdx2 to maxContour.size(), 0 , 1, to minIdx1 to minIdx2
                    newIdx = 0;
                    for (int idx = lowerCurveMaxIdx; idx < maxContour.size(); idx++, newIdx++) {
                        x2Arr.add(newIdx, xVals[idx]);
                        y2Arr.add(newIdx, yVals[idx]);
                        z2Arr.add(newIdx, zVals[idx]);
                    }
                    for (int idx = 0; idx < upperCurveMinIdx; idx++, newIdx++) {
                        x2Arr.add(newIdx, xVals[idx]);
                        y2Arr.add(newIdx, yVals[idx]);
                        z2Arr.add(newIdx, zVals[idx]);
                    }
                } // end if (minIdx1 < minIdx2)

                int[] x1 = new int[x1Arr.size()];
                int[] y1 = new int[x1Arr.size()];
                int[] z1 = new int[x1Arr.size()];
                for(int idx = 0; idx < x1Arr.size(); idx++) {
                    x1[idx] = x1Arr.get(idx);
                    y1[idx] = y1Arr.get(idx);
                    z1[idx] = z1Arr.get(idx);
                }

                abdomenVOI.importCurve(x1, y1, z1, sliceIdx);
                
                int[] x2 = new int[x2Arr.size()];
                int[] y2 = new int[x2Arr.size()];
                int[] z2 = new int[x2Arr.size()];
                for(int idx = 0; idx < x2Arr.size(); idx++) {
                    x2[idx] = x2Arr.get(idx);
                    y2[idx] = y2Arr.get(idx);
                    z2[idx] = z2Arr.get(idx);
                }
                                               
            } else if (theVOI.getCurves()[0].size() != 4) {
                abdomenImage.unregisterAllVOIs();
                abdomenImage.registerVOI(theVOI);            
                new ViewJFrameImage(abdomenImage).updateImages(true);
                System.err.println("makeThighTissueVOI() Error, did not get 2 curves in the VOI");
                completedThigh = false;
            } else {
                //abdomenVOI = makeAbdomenTissueVOI();
                
                // Right leg VOI is the left most in the image
                int[] rightBoundsX = new int [2];
                int[] rightBoundsY = new int [2];
                int[] rightBoundsZ = new int [2];
                VOIContour rightCurve;
                rightCurve = ((VOIContour)abdomenVOI.getCurves()[0].get(0));
                rightCurve.getBounds(rightBoundsX, rightBoundsY, rightBoundsZ);
                

            

            }

        } // end for (sliceIdx = 0; ...)
        
        
        // show the split VOIs
        //boneImage.unregisterAllVOIs();
        //boneImage.registerVOI(rightThighVOI);            
        //boneImage.registerVOI(leftThighVOI);            
        //new ViewJFrameImage(boneImage).updateImages(true);
        return completedThigh;

        
        


    } // end makeThighTissueVOI()
    
    private void getCenterOfMass() {
    	float[] thresholds = new float[2];
        thresholds[0] = -1024;
        thresholds[1] = 1103;
        
        boolean wholeImage = true;
        
        try {

            // No need to make new image space because the user has choosen to replace the source image
            // Make the algorithm class
            comAlgo = new AlgorithmCenterOfMass((ModelImage)srcImage.clone(), thresholds, wholeImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            comAlgo.addListener(this);
            
            comAlgo.join();

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (comAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Center Of Mass: unable to allocate enough memory");

            return;
        } catch(InterruptedException x) {
        	MipavUtil.displayError("Did not compute center of mass.");
        }
        
    }
    
    private BitSet expandAbdomen(BitSet abdomenSet) {
    	int firstSet = abdomenSet.nextSetBit(0);
    	//size returns the last set bit-1
    	int lastSet = abdomenSet.size() - 1;
    	int clearBit;
    	boolean included = false, foundLeft = false, foundRight = false;
    	
    	//iterate over all clear bits from firstSet to lastSet
    	for(int i=abdomenSet.nextClearBit(firstSet); i>=0 || i<lastSet; i=abdomenSet.nextClearBit(i+1)) {
    		clearBit = i;
    		included = false;
    		foundLeft = false;
    		foundRight = false;
    		
        hL: for(int j=clearBit-10; j<clearBit; j++) {
    			if(abdomenSet.get(j) == true) {
    				foundLeft = true;
    				break hL;
    			}
    		}
    		if(foundLeft) {
	    hR:		for(int j=clearBit-10; j<clearBit; j++) {
	    			if(abdomenSet.get(j) == true) {
	    				foundRight = true;
	    				break hR;
	    			}
	    		}
    		}
    		if(foundLeft && foundRight)
    			included = true;
    		if(!included) {
    			foundLeft = false;
    			foundRight = false;
      vL:		for(int j=clearBit-10*xDim; j<clearBit; j += xDim) {
    				if(abdomenSet.get(j) == true) {
        				foundLeft = true;
        				break vL;
    				}
      			}
      vR: 		for(int j=clearBit; j<clearBit+10*xDim; j += xDim) {
    				if(abdomenSet.get(j) == true) {
        				foundRight = true;
        				break vR;
        			}
    			}
      			if(foundLeft && foundRight)
      				included = true;
    		}
    		if(included)
    			abdomenSet.set(clearBit);
    	}
    	
    	return abdomenSet;
    }
    
    private boolean segmentAbdomenTissue(double[] center) {
         
    	center = getSeedPoint(center);
        BitSet abdomenSet = new BitSet();
        regionGrowAbdomen((short)center[0], (short)center[1], (short)center[2], abdomenSet);
        
        volumeBitSet = abdomenSet;//expandAbdomen(abdomenSet);
        
        // make the abdomenTissue label image slice by slice from the 3D region grown BitSet
        // bitSetIdx is a cumulative index into the 3D BitSet
        for (int bitSetIdx = 0, sliceNum = 0; sliceNum < zDim; sliceNum++) {
            for (int sliceIdx = 0; sliceIdx < sliceSize; sliceIdx++, bitSetIdx++) {
                if (volumeBitSet.get(bitSetIdx)) {
                    sliceBuffer[sliceIdx] = abdomenTissueLabel;
                } else {
                    sliceBuffer[sliceIdx] = 0;
                }
            } // end for (int sliceIdx = 0; ...)
            
            // save the sliceBuffer into the boneMarrowImage
            try {
                destImage.importData(sliceNum * sliceSize, sliceBuffer, false);
            } catch (IOException ex) {
                System.err.println("segmentThighTissue(): Error importing data");
            }
        } // end for(int bitSetIdx = 0, sliceNum = 0; ...)
        return true;
    } // end segmentAbdomenTissue(...)
    
    /**
     * Moves seed point to ideal intensity
     * @param center image center of mass
     * @return
     */
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
               regionGrowAlgo.regionGrow3D(muscleBits, new Point3Ds(sX, sY, sZ), -1,
                                                   false, false, null, -100,
                                                   500, -1, -1, false,
                                                   0, regionGrowBounds);
//               System.out.println("Muscle Count: " +count);
           }
       } catch (OutOfMemoryError error) {
           System.gc();
           MipavUtil.displayError("Out of memory: regionGrowMuscle");
       }

   } // regionGrowMuscle(...)
   
   public VOI getAbdomenVOI() {
	   return abdomenVOI;
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
    



}
