package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.Point3D;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.JDialogExtractBrain;

import java.util.*;


/**
 * Anonymize an image of a patient's head by removing the face.
 *
 * <p>This version of of the de-facer uses BET to find the brain, the brain's location is then used to calculate a
 * plane. This plane is then used to remove the patient's face from the image.</p>
 *
 * @author  mccreedy
 */
public class AlgorithmFaceAnonymizerBET extends AlgorithmBase {
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------
    
    /** Eventually won't be necessary. */
    public static final int REMOVED_INTENSITY = 0;
    
    /** May no longer be necessary. */
    public static final int MIN_RADIUS = 2;
    
    /** For indicating the sagittal image is facing right. */
    public static final int FACING_RIGHT = 1;

    /** For indicating the sagittal image is facing left. */
    public static final int FACING_LEFT = 2;

    /** For indicating the axial image is facing down. */
    public static final int FACING_DOWN = 3;

    /** For indicating the axial image is facing up. */
    public static final int FACING_UP = 4;

    /** For indicating the coronal image is facing into the screen. */
    public static final int FACING_INTO_SCREEN = 5;

    /** For indicating the coronal image is facing out of the screen. */
    public static final int FACING_OUT_OF_SCREEN = 6;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Length to buffer brain by in millimeters. */
    private int mmToPad;
    
    /** The orientation of an image. */
    private int faceOrientation;
    
    /** Parameter for brain extraction by default set to <code>false</code> */
    private boolean estimateWithSphereBET = false;

    /** Parameter for brain extraction by default set to .1 */
    private float imageInfluenceBET = 0.1f;

    /** Parameter for brain extraction by default set to .15 */
    private float stiffnessBET = 0.15f;

    /** Computed as xDim*yDim */
    private final int sliceSize;
    
    /** Computed as xDim*yDim*zDim */
    private final int volumeSize;

    /** Length of the x dimension in pixels. */
    private final int xDim;

    /** Length of the y dimension in pixels. */
    private final int yDim;

    /** Length of the z dimension in pixels. */
    private final int zDim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Construct the face anonymizer, but do not run it yet.
     *
     * @param  srcImg            The image to de-face
     * @param  faceDirection     the orientation of the patient's face, as determined by the dialog
     * @param  extraMMsToPad     the number of millimeters to buffer brain by after extraction
     */
    public AlgorithmFaceAnonymizerBET(ModelImage srcImg, int faceDirection, int extraMMsToPad) {
        srcImage = srcImg;
        faceOrientation = faceDirection;
        mmToPad = extraMMsToPad;

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        sliceSize = xDim * yDim;
        volumeSize = sliceSize * zDim;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Clean up memory used by the algorithm.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Run the de-facing algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("The source image is null");
            finalize();

            return;
        }

        if (srcImage.getNDims() != 3) {
            displayError("The source image must be 3D");
            finalize();

            return;
        }

        

        try {
            anonymizeFace();
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Face anonymizer BET: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Changes the BET algorithm parameters from their defaults (which we choose).
     *
     * @param  estimateWithSphere  whether to estimate the brain with a sphere initially
     * @param  imageInfluence      the image influence ratio
     * @param  stiffness           the mesh stiffness
     */
    public void setBETParameters(boolean estimateWithSphere, float imageInfluence, float stiffness) {
        estimateWithSphereBET = estimateWithSphere;
        imageInfluenceBET = imageInfluence;
        stiffnessBET = stiffness;
    }

    /**
     * Executes the de-facing algorithm.
     */
    private void anonymizeFace()
    {
        fireProgressStateChanged(0, null, "Anonymizing face ...");
        boolean removedFace = false;
        
        // calc plane from BET extraction
        BitSet brainMask = extractBrain();   
        
        //add mm to brainMask
        fireProgressStateChanged(58, null, "Buffering brain by "+mmToPad+" mm...");
        brainMask = bufferBrain(brainMask, mmToPad);        
        
        //define region of interest
        fireProgressStateChanged(60, null, "Defining bounds...");
        BitSet interestRegion = defineInterestRegion();

        //define small shape
        fireProgressStateChanged(62, null, "Defining initial region...");
        BitSet initialRegion = defineInitialRegion(interestRegion); 
        
        //expand to random size shape near brain to approximate face         
        fireProgressStateChanged(64, null, "Extracting face...");
        BitSet removeRegion = extractFace(interestRegion, initialRegion, brainMask);
        
        if(removeRegion.cardinality()>0) {
            removedFace = true;
        }
        
        if (!removedFace) {
            MipavUtil.displayError("The face could not be located and removed, possibly because the brain segmentation was too large.");
        }
        else {
            //smooth the dataset
            fireProgressStateChanged(95, null, "Smoothing face...");
            removeRegion = smoothFace(removeRegion); 
            for(int i=removeRegion.nextSetBit(0); i>=0; i=removeRegion.nextSetBit(i+1)) {
                srcImage.set(i, REMOVED_INTENSITY);
            }
        }
        
        srcImage.clearMask();
        
        fireProgressStateChanged(100);
        
        setCompleted(true);
    }
    
    /**
     * Performs the BET brain extraction using parameters specified from JDialogFaceAnonymizer.
     * 
     * @return  A <code>BitSet</code> of size <code>srcImage.getSize()</code> where set values are the location of brain.
     */
    
    private BitSet extractBrain()
    {
        int betOrientation;     
        if (faceOrientation == FACING_RIGHT) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
        } else if (faceOrientation == FACING_LEFT) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
        } else if (faceOrientation == FACING_DOWN) {
            betOrientation = FileInfoBase.AXIAL;
        } else if (faceOrientation == FACING_UP) {
            betOrientation = FileInfoBase.AXIAL;
        } else if (faceOrientation == FACING_INTO_SCREEN) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
        } else if (faceOrientation == FACING_OUT_OF_SCREEN) {
            betOrientation = AlgorithmBrainExtractor.SAT_COR;
        } else {
            displayError("No face orientation given.");
            finalize();
            return null;
        }

        fireProgressStateChanged(3);
        boolean showInitialEstimation = false;
        Vector3f initialCenter = JDialogExtractBrain.computeCenter(srcImage, betOrientation, estimateWithSphereBET);
        int iterations = 500;
        int maxDepth = 5;
        boolean extractBrainToPaint = true;
        AlgorithmBrainExtractor bet = new AlgorithmBrainExtractor(srcImage, betOrientation, showInitialEstimation,
                                                                  estimateWithSphereBET, initialCenter);
        bet.setIterations(iterations);
        bet.setMaxDepth(maxDepth);
        bet.setImageRatio(imageInfluenceBET);
        bet.setStiffness(stiffnessBET);
        bet.setExtractPaint(extractBrainToPaint);
        bet.setSaveBrainMesh(false);
        bet.setRunningInSeparateThread(isRunningInSeparateThread());
        linkProgressToAlgorithm(bet);
        bet.setProgressValues(generateProgressValues(3, 57));
        bet.run();
        bet.finalize();
        bet = null;
        BitSet brainMask = srcImage.getMask();        
        return brainMask;
    }
    
    /**
     * 
     * Appends the given <code>brainMaskTemp</code> by <code>mmToPad</code> in pixels in all directions.
     * TODO: Use Algorithms -> Morphological -> Dilate
     * 
     * @param brainMaskTemp     The <code>BitSet</code> returned by <code>this.extractBrain()</code>.
     * @param mmToPad           Length in millimeters that the brain's <code>BitSet</code> should be extended.
     * @return                  The original <code>BitSet</code> with <code>mmToPad</code> in pixels appended to this <code>BitSet</code>
     */
    
    private BitSet bufferBrain(BitSet brainMaskTemp, int mmToPad)   
    {
        BitSet brainMask = (BitSet)brainMaskTemp.clone();
        BitSetUtility bitUtil = new BitSetUtility();
        float xRes = srcImage.getFileInfo(0).getResolutions()[0];
        float yRes = srcImage.getFileInfo(0).getResolutions()[1];
        float zRes = srcImage.getFileInfo(0).getResolutions()[2];
        int xtoDelete = (int)(mmToPad / xRes);
        int ytoDelete = (int)(mmToPad / yRes);
        int ztoDelete = (int)(mmToPad / zRes);
        double greatestDim = xtoDelete;
        if(ytoDelete>greatestDim)
            greatestDim = ytoDelete;
        int sizeInc = (int)(greatestDim/Math.sqrt(2.0));
        for(int i=brainMaskTemp.nextSetBit(0); i>=0; i=brainMaskTemp.nextSetBit(i+1)) {
            if(bitUtil.isInScan(i+xDim+1) && !brainMaskTemp.get(i+xDim+1))
                for(int j=1; j<sizeInc; j++)
                    if(bitUtil.isInScan(i+j+j*xDim))
                        brainMask.set(i+j+j*xDim);
            if(bitUtil.isInScan(i-xDim+1) && !brainMaskTemp.get(i-xDim+1))
                for(int j=1; j<sizeInc; j++)
                    if(bitUtil.isInScan(i+j-j*xDim))
                        brainMask.set(i+j-j*xDim);
            if(bitUtil.isInScan(i-xDim-1) && !brainMaskTemp.get(i-xDim-1))
                for(int j=1; j<sizeInc; j++)
                    if(bitUtil.isInScan(i-j-j*xDim))
                        brainMask.set(i-j-j*xDim);
            if(bitUtil.isInScan(i+xDim-1) && !brainMaskTemp.get(i+xDim-1))
                for(int j=1; j<sizeInc; j++)
                    if(bitUtil.isInScan(i-j+j*xDim))
                        brainMask.set(i-j+j*xDim); 
            if(bitUtil.isInScan(i+1) && !brainMaskTemp.get(i+1))
                for(int j=1; j<=xtoDelete; j++)
                    if(bitUtil.isInScan(i+j))
                        brainMask.set(i+j);
            if(bitUtil.isInScan(i-1) && !brainMaskTemp.get(i-1))
                for(int j=1; j<=xtoDelete; j++)
                    if(bitUtil.isInScan(i-j))
                        brainMask.set(i-j);
            if(bitUtil.isInScan(i+xDim) && !brainMaskTemp.get(i+xDim))
                for(int j=1; j<=ytoDelete; j++)
                    if(bitUtil.isInScan(i+j*xDim))
                        brainMask.set(i+(j*xDim));
            if(bitUtil.isInScan(i-xDim) && !brainMaskTemp.get(i-xDim))
                for(int j=1; j<=ytoDelete; j++)
                    if(bitUtil.isInScan(i-(j*xDim)))
                        brainMask.set(i-(j*xDim));
            if(bitUtil.isInScan(i+(sliceSize)) && !brainMaskTemp.get(i+(sliceSize)))
                for(int j=1; j<=ztoDelete; j++)
                    if(bitUtil.isInScan(i+(j*(sliceSize))))
                        brainMask.set(i+(j*(sliceSize)));
            if(bitUtil.isInScan(i-(sliceSize)) && !brainMaskTemp.get(i-(sliceSize)))
                for(int j=1; j<=ztoDelete; j++)
                    if(bitUtil.isInScan(i-(j*(sliceSize))))
                        brainMask.set(i-(j*(sliceSize)));
        }
        return brainMask;
    }
    
    /**
     * Defines the bounding region for face de-identification.
     * 
     * @return          A <code>BitSet</code> of size <code>srcImage.getSize()</code> where set values are the inital shape's location.
     */
    
    private BitSet defineInterestRegion()
    {
        BitSetUtility bitUtil = new BitSetUtility();
        BitSet corner = new BitSet(srcImage.getSize());           
        if(faceOrientation == FACING_UP || faceOrientation == FACING_DOWN) {
            for(int i=0; i<xDim; i++) {
                int cornerStep = 0, cornerStart = 0, cornerJump =0, cornerRowEnd = 0;
                if(faceOrientation == FACING_DOWN) {
                    cornerStart = i + sliceSize/2;
                    cornerStep = xDim;
                    cornerJump = sliceSize;
                    cornerRowEnd = yDim-1;
                }
                if(faceOrientation == FACING_UP) {
                    cornerStart = i;
                    cornerStep = xDim;
                    cornerJump = sliceSize;
                    cornerRowEnd = yDim/2;
                }
                for (int j = cornerStart; (j/sliceSize)<(zDim/2); j += cornerJump) {
                    if(faceOrientation == FACING_DOWN) {
                        for(int k = j; k%sliceSize > xDim; k += cornerStep) { 
                            if(bitUtil.isInScan(k)) {
                                corner.set(k);
                            }
                        }
                    }
                    if(faceOrientation == FACING_UP)
                    {
                        for(int k = j; ((k % sliceSize)/xDim) <= cornerRowEnd; k += cornerStep) {  
                            if(bitUtil.isInScan(k)) {
                                corner.set(k);
                            }
                        }
                    }
                }
            }
        }   
        if(faceOrientation == FACING_INTO_SCREEN || faceOrientation == FACING_OUT_OF_SCREEN) {
            for(int i=0; i<xDim; i++) {
                int cornerStep = 0, cornerStart = 0, cornerJump =0, cornerRowEnd = 0;
                if(faceOrientation == FACING_OUT_OF_SCREEN) {
                    cornerStart = i + sliceSize/2;
                    cornerStep = sliceSize;
                    cornerJump = xDim;
                    cornerRowEnd = zDim/2;
                }
                if(faceOrientation == FACING_INTO_SCREEN) {
                    cornerStart = i + sliceSize/2 + volumeSize/2;
                    cornerStep = sliceSize;
                    cornerJump = xDim;
                    cornerRowEnd = zDim;
                }
                for (int j = cornerStart; (j % sliceSize)/xDim < yDim && (j % sliceSize)/xDim>0; j += cornerJump) {
                    for(int k = j; k / (sliceSize) < cornerRowEnd; k += cornerStep) {
                        if(bitUtil.isInScan(k)) {
                            corner.set(k);
                        }
                    }
                } 
            }
        }
        if(faceOrientation ==  FACING_LEFT || faceOrientation == FACING_RIGHT) {
            for(int i=0; i<zDim; i++)  {
                int cornerColumnEnd = 0, cornerStep = 0, cornerStart = 0, cornerJump =0;
                if(faceOrientation == FACING_LEFT) {
                    cornerStart = (sliceSize)*i + sliceSize/2 - xDim;
                    cornerColumnEnd = (sliceSize)*i + sliceSize - (xDim/2);
                    cornerStep = 1;
                    cornerJump = xDim;
                }
                if(faceOrientation == FACING_RIGHT) {
                    cornerStart = (sliceSize)*i + sliceSize/2 - xDim/2;
                    cornerColumnEnd = (sliceSize)*i + sliceSize + xDim;
                    cornerStep = 1;
                    cornerJump = xDim;
                }
                for (int j = cornerStart; j < cornerColumnEnd; j += cornerJump) {
                    for(int k = j; k<j+(cornerJump/2); k += cornerStep) {
                        if(bitUtil.isInScan(k)) {
                            corner.set(k);
                        }
                    }
                }
            }
        }
        return corner;
    }
    
    /**
     * 
     * Defines an initial region for face de-identification.
     * 
     * @param interestRegion    The bounding area of the face de-identification.
     * @return                  A <code>BitSet</code> of size <code>srcImage.getSize()</code> where set values are the inital shape's location.
     */
    
    private BitSet defineInitialRegion(BitSet interestRegion)
    {
        BitSet sphere = new BitSet(srcImage.getSize());
        BitSetUtility bitUtil = new BitSetUtility();
        int xInit = 0, yInit = 0, zInit = 0, radius = 15;
        if(faceOrientation == FACING_LEFT) {
            xInit = 0;
            yInit = yDim-1;
            zInit = zDim/2;  
        }
        else if(faceOrientation == FACING_RIGHT) {
            xInit = xDim-1;
            yInit = yDim-1;
            zInit = zDim/2;
        }
        else if(faceOrientation == FACING_DOWN) {
            xInit = xDim/2;
            yInit = yDim;
            zInit = 0;
        }
        else if(faceOrientation == FACING_UP) {
            xInit = xDim/2;
            yInit = 0;
            zInit = 0;
        }
        else if(faceOrientation == FACING_INTO_SCREEN) {
            xInit = xDim/2;
            yInit = yDim-1;
            zInit = zDim-1;
        }
        else if(faceOrientation == FACING_OUT_OF_SCREEN) {
            xInit = xDim/2;
            yInit = yDim-1;
            zInit = 0;
        }
        int[] points = CircleUtility.get1DPointsInSphere(xInit, yInit, zInit, radius, xDim, yDim);
        for(int i=0; i<points.length; i++) {
            if(bitUtil.isInScan(points[i]) && interestRegion.get(points[i])) {
                sphere.set(points[i]);
            }       
        }
        return sphere;
    }
    
    /**
     * 
     * Performs the face de-identification within the bounding area by approximating the given shape using 
     * a geometry suitable for the given shape.  For example, the approximation of a quarter cylinder is performed
     * using spheres with centers on the boundary points of the <code>initialRegion</code> with further iterations
     * on the boundary points of the resulting shape.
     * 
     * @param interestRegion    The bounding area for face de-identification.
     * @param initialRegion     Initial area.
     * @param brainMask         Brain extraction derived from BET and apended by <code>this.bufferBrain(BitSet, int)</code>
     * @return                  A <code>BitSet</code> of size <code>srcImage.getSize()</code> where set values denote face de-identification may be performed.
     */
    
    private BitSet extractFace(BitSet interestRegion, BitSet initialRegion, BitSet brainMask)
    {
        BitSet brainSlice = (BitSet)interestRegion.clone();
        BitSetUtility bitUtil = new BitSetUtility();
        brainSlice.and(brainMask);
        interestRegion.andNot(brainMask);
        BitSet removeRegion = initialRegion;
        BitSet removeInstanceRegion = new BitSet(srcImage.getSize());
        BitSet cube = defineInterestRegion(); 
        BitSet interestEdge = bitUtil.getEdgePoints(interestRegion);
        BitSet imageSet = new BitSet(srcImage.getSize());
        imageSet.set(0, srcImage.getSize()-1);
        BitSet imageSetEdge = bitUtil.getEdgePoints(imageSet);
        interestEdge.andNot(imageSetEdge);
        Vector interestEdgePoints = bitUtil.convertSetToPoints(interestEdge);
        fireProgressStateChanged(65);
        Random pointPick = new Random();
        int totalPointsRemoved = initialRegion.cardinality();
        int totalArea = (int)((cube.cardinality() - brainSlice.cardinality()) * (Math.PI/4));  //cylinder inside two octants)
        int decayedRadiusCount = 0;
        final double time = System.currentTimeMillis();
        final int minComplete = (int)(getProgressChangeListener().getProgressBar().getPercentComplete()*100);
        int progress = 0;
        while(totalPointsRemoved < totalArea && (System.currentTimeMillis()-time < 40000.0)) { //note time dependence 
            int removePoint = pointPick.nextInt(srcImage.getSize());
            if(removeRegion.get(removePoint) && bitUtil.isBoundaryPoint(removeRegion, removePoint) && !bitUtil.isBoundaryPoint(cube, removePoint)) { 
                int minDist = ((int)bitUtil.getMinDistance(removePoint, interestEdgePoints));
                if(minDist>MIN_RADIUS) {
                    int radius = pointPick.nextInt(minDist-MIN_RADIUS) + MIN_RADIUS;
                    double decayedRadius = radius * Math.exp(-((double)totalPointsRemoved) / totalArea);
                    if(((int)decayedRadius) > MIN_RADIUS) {
                        progress = (int)(minComplete+(.3*((System.currentTimeMillis()-time)/40000.0))*100); 
                        fireProgressStateChanged(progress);
                        int[] points = CircleUtility.get1DPointsInSphere(removePoint, ((int)decayedRadius), xDim, yDim);
                        for(int i=0; i<points.length; i++) {
                            if(bitUtil.isInScan(points[i]) && !brainMask.get(points[i]) && interestRegion.get(points[i])) {// && ((int)points[i]/(xDim)) == ((int)removePoint/(xDim)))
                                removeInstanceRegion.set(points[i]);
                                totalPointsRemoved++;
                            }
                        }
                        removeRegion.or(removeInstanceRegion);
                        interestRegion.andNot(removeInstanceRegion);
                        removeInstanceRegion = new BitSet(srcImage.getSize());
                    }
                    else
                        decayedRadiusCount++;
                }
                else
                    decayedRadiusCount++;
            }
        }
        return removeRegion;
    }
    
    /**
     * 
     * Connects near neighbors of set values in <code>removeRegion</code>.  Near neighbors are those values which are
     * set and are within five percent of the image resolution.
     * 
     * @param removeRegion      The original <code>BitSet</code> for face de-identification.
     * @param interestRegion    The bounding region.
     * @param brainMask         The location of the brain in this image.
     * @return                  A <code>BitSet</code> of size <code>srcImage.getSize()</code> where set values denote face de-identification may be performed.
     */
    
    private BitSet smoothFace(BitSet removeRegion)
    {
        BitSetUtility bitUtil = new BitSetUtility();
        BitSet removeEdge = bitUtil.getEdgePoints(removeRegion);
        BitSet imageSet = new BitSet(srcImage.getSize());
        imageSet.set(0, srcImage.getSize()-1);
        BitSet imageSetEdge = bitUtil.getEdgePoints(imageSet);
        removeEdge.andNot(imageSetEdge);
        BitSet removeRegionTemp = (BitSet)removeRegion.clone();
        int levelCut = ((int)(((xDim+yDim+zDim)/3)*.05));
        for(int i=removeEdge.nextSetBit(0); i>=0; i=removeEdge.nextSetBit(i+1)) {
            removeRegion = bitUtil.connectNearNeighbors(i, removeRegion, removeRegionTemp, levelCut);               
        }
        return removeRegionTemp;
    }
    
    /**
     * Private utility class for generating approximations to circles and spheres.
     * 
     * @author senseneyj
     *
     */
    
    public static class CircleUtility
    {
        
        /**
         * 
         * Calculates the points of an approximate sphere whose center is at <code>(xCenter, yCenter, zCenter)</code>
         * witha given radius.
         * 
         * @return  Array of (x,y,z) coordinates of an approximate sphere.
         */
        
        public static int[][] get3DPointsInSphere(int xCenter, int yCenter, int zCenter, int radius)
        {
            if(radius>1)
            {
                int[][] quadrant1 = getQuadrantBoundaryPoints(radius, 1);
                int[][] quadrant2 = getQuadrantBoundaryPoints(radius, 2);
                //int j = 0, k=0;
                int[][][] slices = new int[2*radius+1][][];
                int placeHolder = 0;    //shouldn't be necessary
                for(int i=0; i<quadrant1.length-1; i++) {
                   if(!(quadrant1[i][0] == quadrant1[i+1][0])) {
                       int radiusTemp =  quadrant1[i][1];
                       slices[placeHolder] = get2DPointsInCircle(yCenter, zCenter, radiusTemp);
                       placeHolder++;
                   }
                }
                slices[placeHolder] = get2DPointsInCircle(yCenter, zCenter, radius);
                placeHolder++;
                slices[placeHolder] = get2DPointsInCircle(yCenter, zCenter, radius);
                placeHolder++;
                for(int i=1; i<quadrant2.length; i++)
                {
                    if(!(quadrant2[i][0] == quadrant2[i-1][0]))
                    {
                        slices[placeHolder] = get2DPointsInCircle(yCenter, zCenter, Math.abs(quadrant2[i][1]));
                        placeHolder++;
                    }
                }
                int size = 0;
                for(int i=0; i<slices.length; i++)
                    size = size+slices[i].length;
                int[][] spherePoints = new int[size][3];
                int place = 0;
                for(int i=0; i<slices.length; i++) {
                    for(int n=0; n<slices[i].length; n++) {
                        spherePoints[place][0] = xCenter+i-radius;
                        spherePoints[place][1] = slices[i][n][0];
                        spherePoints[place][2] = slices[i][n][1];
                        place++;
                    }
                }
                return spherePoints;
            }
           
            //r <=1
            int[][] spherePoints = new int[1][3];
            spherePoints[0][0] = xCenter;
            spherePoints[0][1] = yCenter;
            spherePoints[0][2] = zCenter;
            return spherePoints;
        }
        
        /**
         * 
         * Calculates the points of an approximate circle whose center is at <code>(xCenter, yCenter)</code>
         * with a given radius.
         * 
         * @return  Array of (x,y) coordinates of an approximate circle.
         */
        
        public static int[][] get2DPointsInCircle(int xCenter, int yCenter, int r)
        {
            if(r>1) {
                ArrayList points = new ArrayList();
                int[] corner = new int[2];
                int[][] quadrant1 = getQuadrantBoundaryPoints(r, 1);
                int[][] quadrant3 = getQuadrantBoundaryPoints(r, 3);
                for(int i=0; i<quadrant1.length; i++) {
                    if((i==0) || !(quadrant1[i-1][1] == quadrant1[i][1])) {
                        corner = quadrant1[i];
                        for(int j=corner[0]; j>=-corner[0]; j--) {
                            int[] tempPoint = new int[2];
                            tempPoint[0] = xCenter + j;
                            tempPoint[1] = yCenter + corner[1];
                            points.add(tempPoint);
                        }
                    }
                }
                for(int i=1; i<quadrant3.length; i++) {
                    if(!(quadrant3[i-1][1] == quadrant3[i][1])) {
                        corner = quadrant3[i];
                        for(int j=-corner[0]; j>=corner[0]; j--) {
                            int[] tempPoint = new int[2];
                            tempPoint[0] = xCenter + j;
                            tempPoint[1] = yCenter + corner[1];
                            points.add(tempPoint);
                        }
                    }
                }
                int[][] circlePoints = new int[points.size()][2];
                for(int i=0; i<points.size(); i++) {
                    circlePoints[i] = (int[])points.get(i);
                }
                return circlePoints;
            }
            
            //r <= 1
            int[][] circlePoints = new int[1][2];
            circlePoints[0][0] = xCenter;
            circlePoints[0][1] = yCenter;
            return circlePoints;
        }
        
        /**
         * 
         * Calculates the points of an approximate sphere whose center is at <code>(xCenter, yCenter, zCenter)</code>
         * with a given radius.
         * 
         * @return  Array of points of an approximate sphere.
         */
        
        public static int[] get1DPointsInSphere(int xCenter, int yCenter, int zCenter, int radius, int xDim, int yDim)
        {
            int[][] spherePoints = get3DPointsInSphere(xCenter, yCenter, zCenter, radius);
            int[] convertedPoints = new int[spherePoints.length];
            for(int i=0; i<spherePoints.length; i++) {
                convertedPoints[i] = spherePoints[i][2]*(xDim*yDim)+spherePoints[i][1]*xDim + spherePoints[i][0];
            }
            return convertedPoints;
        }
        
        /**
         * 
         * Calculates the points of an approximate sphere whose center is at <code>value</code>
         * with a given radius.
         * 
         * @return  Array of points of an approximate sphere.
         */
        
        public static int[] get1DPointsInSphere(int value, int radius, int xDim, int yDim)
        {
            int z = value / (xDim*yDim);
            int y = (value - z*(xDim*yDim)) / xDim;
            int x = value - z*(xDim*yDim) - y*xDim;
            return get1DPointsInSphere(x, y, z, radius, xDim, yDim);
        }
        
        /**
         * 
         * Calculates the points of an approximate sphere whose center is at <code>(xCenter, yCenter)</code>
         * with a given radius.
         * 
         * @return  Array of points of an approximate sphere.
         */
        
        public static int[] get1DPointsInCircle(int xCenter, int yCenter, int zCenter, int radius, int dimX, int dimY)
        {
            int[][] circlePoints = get2DPointsInCircle(xCenter, yCenter, radius);
            int[] convertedPoints = new int[circlePoints.length];
            for(int i=0; i<circlePoints.length; i++) {
               convertedPoints[i] = zCenter*(dimX*dimY) + circlePoints[i][1]*dimX + circlePoints[i][0];
            }
            return convertedPoints;    
        }
        
        /**
         * Generates 2D data points in a given  quadrant for a specified <code>radius</code>
         * 
         * @see McIlroy, M. D.  Best approximate circles on integer grids. ACM Transactions on Graphics (TOG),  Volume 2 Issue 4. 
         * @see http://portal.acm.org/citation.cfm?id=246
         * 
         * @param radius    Radius from origin for circle.
         * @param quadrant  Allow 1 - 4 where 1: x>=0, y>0
         *                                    2: x<0, y>=0
         *                                    3: x<=0, y<0
         *                                    4: x>0, y<=0 
         *                                    
         * @author senseneyj
         */
        
        public static int[][] getQuadrantBoundaryPoints(int radius, int quadrant)
        {
            ArrayList quadrantPoints = new ArrayList();
            int x = 0, y = 0, dx = 0, dy = 0;
            if(quadrant == 1 || quadrant == 2) {
                x = radius;
                y = 0;
                dx = -1;
                dy = 1;
            }
            else if(quadrant == 3 || quadrant == 4) {
                x = -radius;
                y = 0;
                dx = 1;
                dy = -1;
            }
            else
                return null;
            int epsilon, epsilonX, epsilonY, epsilonXY;
            while(((quadrant == 1 || quadrant == 2) && x>0) || ((quadrant == 3 || quadrant == 4) && x<0))
            {
                int[] temp = new int[2];
                temp[0] = x;
                temp[1] = y;
                quadrantPoints.add(temp);
                epsilon = computeEpsilon(x, y, radius);
                epsilonX = computeEpsilonX(epsilon, x, dx);
                epsilonY = computeEpsilonY(epsilon, y, dy);
                epsilonXY = computeEpsilonXY(epsilon, x, dx, y, dy);
                if(-epsilonXY<epsilonY)
                    x = x+dx;
                if(epsilonXY<-epsilonX)
                    y = y+dy;
            }
            int[][] quadrantArray = new int[quadrantPoints.size()][2];
            if(quadrant == 1 || quadrant == 3) {
                for(int i=0; i<quadrantPoints.size(); i++) {
                    quadrantArray[i] = (int[])quadrantPoints.get(i);
                }
            }
            else if(quadrant == 2 || quadrant == 4) {
                int[] temp = new int[2];
                for(int i=0; i<quadrantPoints.size(); i++) {
                    temp = (int[])quadrantPoints.get(i);
                    quadrantArray[i][0] = temp[1];
                    quadrantArray[i][1] = -temp[0];
                }
            }
            return quadrantArray;
        }
        
        private static int computeEpsilon(int x, int y, int r)
        {
            return (int)(Math.pow(x, 2) + Math.pow(y, 2) - Math.pow(r, 2));
        }
        
        private static int computeEpsilonX(int epsilon, int x, int dx)
        {
            return epsilon + 2*x*dx + 1;
        }
        
        private static int computeEpsilonY(int epsilon, int y, int dy)
        {
            return epsilon + 2*y*dy + 1;
        }
        
        private static int computeEpsilonXY(int epsilon, int x, int dx, int y, int dy)
        {
           return epsilon + 2*x*dx + 2*y*dy + 2;
        }
    }
    
    /**
     * Private utility class for operations on BitSets
     * 
     * @author senseneyj
     *
     */
    
    private class BitSetUtility
    {
        /**
         * 
         * Calculates the distance between the closest point in this <code>BitSet</code> and the given <code>value</code>.
         */
        
        float getMinDistance(int value, BitSet set)
        {
            return getMinDistance(value, convertSetToPoints(set));
        }
        
        /**
         * 
         * Calculates the distance between the closest point in this <code>Vector</code> and the given <code>value</code>.
         */
        
        float getMinDistance(int value, Vector setPoints)
        {
            int xValue = 0, yValue = 0, zValue = 0;
            xValue = value % xDim;
            yValue = (value % sliceSize) / xDim;
            zValue = value / sliceSize;
            float diffx, diffy, diffz;
            Point3D point;
            float distance = srcImage.getSize(), dist = 0;
            for(int i=0; i<setPoints.size(); i++)
            {
                point = (Point3D)setPoints.get(i);
                
                diffx = point.x - xValue;
                diffy = point.y - yValue;
                diffz = point.z - zValue;

                dist = (diffx * diffx) + (diffy * diffy) +
                       (diffz * diffz);

                if (dist < distance) {
                    distance = dist;
                }
            }
            return ((float)Math.sqrt(distance));
        }
        
        /**
         * 
         * Finds the points of the given <code>BitSet</code> which has at least one neighbor (not including diagonals) not in the BitSet.
         */
        
        BitSet getEdgePoints(BitSet set)
        {
            BitSet edgePoints = new BitSet(srcImage.getSize());
            for(int i=set.nextSetBit(0); i>=0; i=set.nextSetBit(i+1)) {
                if(isBoundaryPoint(set, i))
                    edgePoints.set(i);
            }
            return edgePoints;
        }
        
        boolean isBoundaryPoint(BitSet set, int value)
        {
            
            if(value % xDim == 0 || value % xDim == xDim-1 || 
                    (value % sliceSize) / xDim == 0 || (value % sliceSize) / xDim == yDim-1 || 
                    value / sliceSize == 0 || value / sliceSize == zDim-1)
                return true; 
            
            if(!set.get(value-1))
                return true;
            else if(!set.get(value+1))
                return true;
            else if(!set.get(value+xDim))
                return true;
            else if(!set.get(value-xDim))
                return true;
            else if(!set.get(value+(sliceSize)))
                return true;
            else if(!set.get(value-(sliceSize)))
                return true;
            else
                return false;
            
        }
        
        static final int LEFT_EXP = 0;
        static final int RIGHT_EXP = 1;
        static final int DOWN_EXP = 2;
        static final int UP_EXP = 3;
        static final int IN_EXP = 4;
        static final int OUT_EXP = 5;
        
        int getBoundaryCode(BitSet set, int value)
        {
            int sum = 0;
            if(set.get(value-1))
                sum =+ 2^LEFT_EXP;
            if(set.get(value+1))
                sum =+ 2^RIGHT_EXP;
            if(set.get(value+xDim))
                sum =+ 2^DOWN_EXP;
            if(set.get(value-xDim))
                sum =+ 2^UP_EXP;
            if(set.get(value+(sliceSize)))
                sum =+ 2^IN_EXP;
            if(set.get(value-(sliceSize)))
                sum =+ 2^OUT_EXP;
            return sum;
        }
        
        boolean isInScan(int value)
        {
            if(value > 0 && value < volumeSize)
                return true;
            return false;
        }
        
        /**
         * 
         * Set elements of the returned BitSet meet at least one of the following criteria: 
         * <ol>
         *  <li>Element is set in <code>connectingRegion</code>.</li>
         *  <li>Walking up, down, right, and/or left from <code>loc</code> to this element requires less than or equal to <code>maxNeighborDistance</code> steps.</li>
         * </ol>
         * 
         */
        
        BitSet connectNearNeighbors(int loc, BitSet connectingRegion, int maxNeighborDistance)
        {
            BitSet connectingRegionCopy = (BitSet)connectingRegion.clone();
            connectNearNeighborsUtil(loc, connectingRegion, connectingRegionCopy, 0, 0, maxNeighborDistance);
            return connectingRegionCopy;
        }
        
        /**
         * 
         * A helper method for <code>connectNearNeighbors(int, BitSet, int)</code> that allows faster copies
         * 
         */

        BitSet connectNearNeighbors(int loc, BitSet connectingRegion, BitSet connectingRegionCopy, int maxNeighborDistance)
        {
            connectNearNeighborsUtil(loc, connectingRegion, connectingRegionCopy, 0, 0, maxNeighborDistance);
            return connectingRegionCopy;
        }
        
        private boolean connectNearNeighborsUtil(int loc, BitSet connectingRegion, BitSet connectingRegionCopy, int level, int dir, int levelCut)
        {
            final int start = 0, right = 1, left = 2, down = 3, up = 4, in = 5, out = 6;
            if(level<levelCut && isInScan(loc)) {
                if(level != 0 && connectingRegion.get(loc)) {
                    return true;
                }
                
                switch(dir) {
                    case start:
                        if(!connectingRegion.get(loc+1))
                            connectNearNeighborsUtil(loc+1, connectingRegion, connectingRegionCopy, level, right, levelCut);
                        if(!connectingRegion.get(loc-1))
                            connectNearNeighborsUtil(loc-1, connectingRegion, connectingRegionCopy, level, left, levelCut);
                        if(!connectingRegion.get(loc-(xDim)))
                            connectNearNeighborsUtil(loc-(xDim), connectingRegion, connectingRegionCopy, level, down, levelCut);
                        if(!connectingRegion.get(loc+(xDim)))
                            connectNearNeighborsUtil(loc+(xDim), connectingRegion, connectingRegionCopy, level, up, levelCut);
                        if(!connectingRegion.get(loc+(sliceSize)))
                            connectNearNeighborsUtil(loc+(sliceSize), connectingRegion, connectingRegionCopy, level, in, levelCut);
                        if(!connectingRegion.get(loc-(sliceSize)))
                            connectNearNeighborsUtil(loc-(sliceSize), connectingRegion, connectingRegionCopy, level, out, levelCut);
                        break;
                    case right:
                        if(connectNearNeighborsUtil(loc+1, connectingRegion, connectingRegionCopy, level+1, dir, levelCut)) {
                            connectingRegionCopy.set(loc);
                            return true;
                        }
                        break;
                    case left:
                        if(connectNearNeighborsUtil(loc-1, connectingRegion, connectingRegionCopy, level+1, dir, levelCut)) {
                            connectingRegionCopy.set(loc);
                            return true;
                        }
                        break;
                    case down:
                        if(connectNearNeighborsUtil(loc-(xDim), connectingRegion, connectingRegionCopy, level+1, dir, levelCut)) {
                            connectingRegionCopy.set(loc);
                            return true;
                        }
                        break;
                    case up:
                        if(connectNearNeighborsUtil(loc+(xDim), connectingRegion, connectingRegionCopy, level+1, dir, levelCut)) {
                            connectingRegionCopy.set(loc);
                            return true;
                        }
                        break;
                    case in:
                        if(connectNearNeighborsUtil(loc+(sliceSize), connectingRegion, connectingRegionCopy, level+1, dir, levelCut)) {
                            connectingRegionCopy.set(loc);
                            return true;
                        }
                        break;
                    case out:
                        if(connectNearNeighborsUtil(loc-(sliceSize), connectingRegion, connectingRegionCopy, level+1, dir, levelCut)) {
                            connectingRegionCopy.set(loc);
                            return true;
                        }
                        break;
                }
                return false;
                
            }
           
            return false;
            
        }
        
        private Vector convertSetToPoints(BitSet set)
        {
            Vector setPoints = new Vector();
            int xEdge = 0, yEdge = 0, zEdge = 0;
            for(int i=set.nextSetBit(0); i>=0; i=set.nextSetBit(i+1)) {
                xEdge = i % xDim;
                yEdge = (i % sliceSize) / xDim;
                zEdge = i / sliceSize;
                setPoints.add(new Point3D(xEdge, yEdge, zEdge));
            }
            return setPoints;
        }
    }
}
