package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * This class provides an implementation of a second method for segmentation of the brain from a 3D MRI, as opposed to
 * the BET algorithm implemented in AlgorithmBrainExtractor. The algorithm is partially based on the paper describing
 * the Brain Surface Extraction (BSE) algorithm:
 *
 * <pre>
     Magnetic Resonance Image Tissue Classification Using a Partial Volume Model<br>
     David W. Shattuck et al.<br>
     NeuroImage 13, 856-876 (2001)<br>
     available at http://www.idealibrary.com<br>
 * </pre>
 *
 * @see      gov.nih.mipav.view.dialogs.JDialogBrainSurfaceExtractor
 * @see      AlgorithmBrainExtractor
 * @version  1.0 June 3, 2004
 * @author   Evan McCreedy
 */
public class AlgorithmBrainSurfaceExtractor extends AlgorithmBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /**
     * The proportion of the total number of pixels in the volume that a region should be before we decide that it's
     * probably the brain.
     */
    private static final float brainRegionThresholdRatio = 0.01f;

    /** Used to initialize the stack size when finding the largest segmented region (i.e. the brain). */
    private static final float regionPointStackRatio = 0.4f;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The size of the region we have identified as the brain. */
    private float brainRegionSize;

    /** The number of closings to perform. */
    private int closeIterations;

    /** Use the sized circle kernel for the closing. */
    private final int closeKernel = AlgorithmMorphology25D.SIZED_CIRCLE;

    /** The size of the kernel to use in closings. */
    private float closeKernelSize;

    /** Whether to use the edge detection algorithm with a separable convolver. */
    private boolean doSeparable;

    /** The size of the edge detection kernel. */
    private float edgeKernel;

    /** Whether to erode / dilate image slices independently. */
    private boolean erosion25D;

    /** The number of erosions/dilations to perform. */
    private int erosionIterations;

    /** This flag is set when the algorithm is being used only to extract paint from the brain. */
    private boolean extractPaint;

    /** Whether to make an effort to fill in any interior holes in the final brain mask. */
    private boolean fillHolesFlag;

    /** The number of filter passes to make. */
    private int filterIterations;

    /** The size of the filter kernel to use. */
    private float filterKernel;

    /** The source image. */
    private ModelImage image;

    /** The size, in voxels, of the volume. */
    private int imgSize;

    /** The actual paint bitmap. */
    private BitSet paintMask;

    /** The stack of voxels in the image that we should try to grow to. Made a class member to increase speed. */
    private IntVector regionPointStack;

    /** The result image. */
    private ModelImage resultImage;

    /** Whether to show the intermediate images generated during the BSE algorithm. */
    private boolean showIntermediateImages;

    /** The size, in voxels, of a slice of the volume. */
    private int sliceSize;

    /** The image size in the x dimension. */
    private int xDim;

    /** The image resolution in the x dimension. */
    private float xRes;

    /** The image size in the y dimension. */
    private int yDim;

    /** The image resolution in the y dimension. */
    private float yRes;

    /** The image size in the z dimension. */
    private int zDim;

    /** The image resolution in the z dimension. */
    private float zRes;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create an extractor for segmenting the brain from a 3D magnetic resonance image.
     *
     * @param  srcImg            the source image. Should be MR image of the brain
     * @param  fIter             number of passes to make with the filter
     * @param  fKernel           gaussian kernel size to use when running the filter
     * @param  eKernel           kernel size to use with edge detection algo
     * @param  ero25D            whether to use 2.5D processing in erosion / dilation
     * @param  eroIter           number of erosions / dialations to perform
     * @param  cKernel           kernel size in mm to be used in closing
     * @param  cIter             number of closings to perform
     * @param  showIntermediate  whether to save intermediate images for display
     * @param  noHoles           whether to fill in all interior holes of the extracted brain
     * @param  doSep             whether to use separable convolver for edge detection
     * @param  extractPaint      whether to extract paint from the brain
     */
    public AlgorithmBrainSurfaceExtractor(ModelImage srcImg, int fIter, float fKernel, float eKernel, boolean ero25D,
                                          int eroIter, float cKernel, int cIter, boolean showIntermediate,
                                          boolean noHoles, boolean doSep, boolean extractPaint) {
        super(null, srcImg);
        image = srcImg;
        filterIterations = fIter;
        filterKernel = fKernel;
        edgeKernel = eKernel;
        erosion25D = ero25D;
        erosionIterations = eroIter;
        closeKernelSize = cKernel;
        closeIterations = cIter;
        showIntermediateImages = showIntermediate;
        fillHolesFlag = noHoles;
        doSeparable = doSep;
        this.extractPaint = extractPaint;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        // none of the algorithms are started asynchronously
    }

    /**
     * Isolate the brain from the rest of the MR image.
     */
    public void extractBrain() {

        // unchanging parameters and placeholders passed into algorithms
        float filterContrast = 0.05f;
        int middleSlice = image.getExtents()[2] / 2;
        xRes = image.getFileInfo(middleSlice).getResolutions()[0];
        yRes = image.getFileInfo(middleSlice).getResolutions()[1];
        zRes = image.getFileInfo(middleSlice).getResolutions()[2];

        // float normFactor = xRes / zRes;
        float normFactor = 1;
        float[] edgeSigmas = new float[] { edgeKernel, edgeKernel, edgeKernel * normFactor };

        xDim = image.getExtents()[0];
        yDim = image.getExtents()[1];
        zDim = image.getExtents()[2];
        imgSize = xDim * yDim * zDim;
        sliceSize = xDim * yDim;

        boolean do25D = false; // do full 3d
        boolean regionFlag = true; // do the whole image
        boolean clearInflectionPoints = true;

        String imgName = image.getImageName();

        fireProgressStateChanged(0);

        if (isThreadStopped()) {
            return;
        }

        /* Used to control which ratio should be used */
        float[] ratios = calculateProgressRatios(filterIterations, doSeparable, fillHolesFlag);

        // filter
        int algorithmIndex = 0;

        if (filterIterations > 0) {
            resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imgName + "_temp_results");

            int[] progressValueBounds = calculateProgressValueBoundary(algorithmIndex++, ratios, 0, 100);

            if (progressValueBounds == null) {
                MipavUtil.displayError("The boundary of progress value for the algorithm: " + (algorithmIndex - 1) +
                                       " can't be calculated!");

                return;
            }

            fireProgressStateChanged(progressValueBounds[0], null, "Filtering image ...");

            AlgorithmRegularizedIsotropicDiffusion filterAlgo = null;

            if ((progressValueBounds[1] - progressValueBounds[0]) < 2) {
                filterAlgo = new AlgorithmRegularizedIsotropicDiffusion(resultImage, image, filterIterations,
                                                                        filterKernel, filterContrast, do25D);
            } else {
                filterAlgo = new AlgorithmRegularizedIsotropicDiffusion(resultImage, image, filterIterations,
                                                                        filterKernel, filterContrast, do25D);
                linkProgressToAlgorithm(filterAlgo);
                filterAlgo.setProgressValues(generateProgressValues(progressValueBounds[0], progressValueBounds[1]));

            }

            filterAlgo.setRunningInSeparateThread(isRunningInSeparateThread());
            filterAlgo.addListener(this);
            filterAlgo.run();

            fireProgressStateChanged(progressValueBounds[1]);

            if (showIntermediateImages) {
                ModelImage tempImage = (ModelImage) resultImage.clone(imgName + "_filter");
                tempImage.calcMinMax();
                new ViewJFrameImage(tempImage);
            }

        } else {
            resultImage = (ModelImage) image.clone(imgName + "_temp_results");
        }

        if (isThreadStopped()) {
            return;
        }

        // edge detection

        ModelImage tempEdgeImage = (ModelImage) addPadding(resultImage);
        resultImage.disposeLocal();
        resultImage = (ModelImage) tempEdgeImage.clone(imgName + "_padded");

        AlgorithmBase edgeAlgo = null;

        int[] progressValueBounds = calculateProgressValueBoundary(algorithmIndex++, ratios, 0, 100);

        if (progressValueBounds == null) {
            MipavUtil.displayError("The boundary of progress value for the algorithm: " + (algorithmIndex - 1) +
                                   " can't be calculated!");

            return;
        }

        fireProgressStateChanged(progressValueBounds[0], null, "Detecting edges ...");

        if (doSeparable) {

            if ((progressValueBounds[1] - progressValueBounds[0]) < 2) {
                edgeAlgo = new AlgorithmEdgeLaplacianSep(tempEdgeImage, resultImage, edgeSigmas, regionFlag, do25D, clearInflectionPoints);
            } else {
                edgeAlgo = new AlgorithmEdgeLaplacianSep(tempEdgeImage, resultImage, edgeSigmas, regionFlag, do25D, clearInflectionPoints);
                edgeAlgo.setProgressValues(generateProgressValues(progressValueBounds[0], progressValueBounds[1]));
                linkProgressToAlgorithm(edgeAlgo);
            }

            ((AlgorithmEdgeLaplacianSep) edgeAlgo).setZeroDetectionType(AlgorithmEdgeLaplacianSep.NEGATIVE_EDGES);
            edgeAlgo.setRunningInSeparateThread(isRunningInSeparateThread());
            edgeAlgo.addListener(this);
            edgeAlgo.run();

            // get rid of filter and laplacian images
            tempEdgeImage.disposeLocal();
            resultImage.disposeLocal();
            resultImage = ((AlgorithmEdgeLaplacianSep) edgeAlgo).getZeroXMask();
        } else {

            if ((progressValueBounds[1] - progressValueBounds[0]) < 2) {
                edgeAlgo = new AlgorithmEdgeLaplacian(tempEdgeImage, resultImage, edgeSigmas, regionFlag, do25D, clearInflectionPoints);
            } else {
                edgeAlgo = new AlgorithmEdgeLaplacian(tempEdgeImage, resultImage, edgeSigmas, regionFlag, do25D, clearInflectionPoints);
                linkProgressToAlgorithm(edgeAlgo);
                edgeAlgo.setProgressValues(generateProgressValues(progressValueBounds[0], progressValueBounds[1]));

            }

            ((AlgorithmEdgeLaplacian) edgeAlgo).setZeroDetectionType(AlgorithmEdgeLaplacian.NEGATIVE_EDGES);
            edgeAlgo.setRunningInSeparateThread(isRunningInSeparateThread());
            edgeAlgo.addListener(this);
            edgeAlgo.run();

            // get rid of filter and laplacian images
            tempEdgeImage.disposeLocal();
            resultImage.disposeLocal();
            resultImage = ((AlgorithmEdgeLaplacian) edgeAlgo).getZeroXMask();
        }

        fireProgressStateChanged(progressValueBounds[1]);

        if (isThreadStopped()) {
            return;
        }

        ModelImage paddedImg = (ModelImage) resultImage.clone();
        resultImage.disposeLocal();
        resultImage = removePadding(paddedImg);
        paddedImg.disposeLocal();

        // invert (want brain to be white for erosion)
        int x, y;

        progressValueBounds = calculateProgressValueBoundary(algorithmIndex++, ratios, 0, 100);

        if (progressValueBounds == null) {
            MipavUtil.displayError("The boundary of progress value for the algorithm: " + (algorithmIndex - 1) +
                                   " can't be calculated!");

            return;
        }

        fireProgressStateChanged(progressValueBounds[0], null, "Inverting edge mask...");

        if ((progressValueBounds[1] - progressValueBounds[0]) < 2) {

            for (int i = 0; (i < imgSize) && !threadStopped; i++) {

                if (resultImage.getUByte(i) > 0) {
                    resultImage.setUByte(i, (short) 0);
                } else {
                    resultImage.setUByte(i, (short) 1);
                }

                x = i % xDim;
                y = (i % sliceSize) / xDim;

                // edge lap puts a one-pixel border along the edges of the image
                // slices
                // if ( x == xDim - 1 || y == yDim - 1 || x == 0 || y == 0 ) {
                if ((x == (xDim - 1)) || (y == (yDim - 1))) {
                    resultImage.setUByte(i, (short) 0);
                }
            }
        } else {
            int pixelsPerProgressValue = Math.round((float) imgSize) /
                                             (progressValueBounds[1] - progressValueBounds[0]);

            for (int i = 0; (i < imgSize) && !threadStopped; i++) {

                if ((i % pixelsPerProgressValue) == 0) {
                    fireProgressStateChanged(progressValueBounds[0] + (i / pixelsPerProgressValue));
                }

                if (resultImage.getUByte(i) > 0) {
                    resultImage.setUByte(i, (short) 0);
                } else {
                    resultImage.setUByte(i, (short) 1);
                }

                x = i % xDim;
                y = (i % sliceSize) / xDim;

                // edge lap puts a one-pixel border along the edges of the image
                // slices
                // if ( x == xDim - 1 || y == yDim - 1 || x == 0 || y == 0 ) {
                if ((x == (xDim - 1)) || (y == (yDim - 1))) {
                    resultImage.setUByte(i, (short) 0);
                }
            }
        }

        fireProgressStateChanged(progressValueBounds[1]);

        if (isThreadStopped()) {
            finalize();

            return;
        }

        if (showIntermediateImages) {
            ModelImage tempImage = (ModelImage) resultImage.clone(imgName + "_edge");
            tempImage.calcMinMax();
            new ViewJFrameImage(tempImage);
        }

        // erode

        AlgorithmBase erodeAlgo;

        progressValueBounds = calculateProgressValueBoundary(algorithmIndex++, ratios, 0, 100);

        if (progressValueBounds == null) {
            MipavUtil.displayError("The boundary of progress value for the algorithm: " + (algorithmIndex - 1) +
                                   " can't be calculated!");

            return;
        }

        fireProgressStateChanged(progressValueBounds[0], null, "Eroding edges...");

        if ((progressValueBounds[1] - progressValueBounds[0]) < 2) {

            if (!erosion25D) {
                erodeAlgo = new AlgorithmMorphology3D(resultImage, AlgorithmMorphology3D.CONNECTED6, 0,
                                                      AlgorithmMorphology3D.ERODE, 0, erosionIterations, 0, 0,
                                                      regionFlag);
            } else {
                erodeAlgo = new AlgorithmMorphology25D(resultImage, AlgorithmMorphology25D.CONNECTED4, 0,
                                                       AlgorithmMorphology25D.ERODE, 0, erosionIterations, 0, 0,
                                                       regionFlag);
            }
        } else {

            if (!erosion25D) {
                erodeAlgo = new AlgorithmMorphology3D(resultImage, null, AlgorithmMorphology3D.CONNECTED6, 0,
                                                      AlgorithmMorphology3D.ERODE, 0, erosionIterations, 0, 0,
                                                      regionFlag);
            } else {
                erodeAlgo = new AlgorithmMorphology25D(resultImage, AlgorithmMorphology25D.CONNECTED4, 0,
                                                       AlgorithmMorphology25D.ERODE, 0, erosionIterations, 0, 0,
                                                       regionFlag);
            }

            erodeAlgo.setProgressValues(generateProgressValues(progressValueBounds[0], progressValueBounds[1]));

            linkProgressToAlgorithm(erodeAlgo);
        }

        erodeAlgo.setRunningInSeparateThread(isRunningInSeparateThread());
        erodeAlgo.addListener(this);
        erodeAlgo.run();


        fireProgressStateChanged(progressValueBounds[1]);

        if (isThreadStopped()) {
            return;
        }

        if (showIntermediateImages) {
            ModelImage tempImage = (ModelImage) resultImage.clone(imgName + "_erode_brain");
            tempImage.calcMinMax();
            new ViewJFrameImage(tempImage);
        }

        // find largest region and remove others
        ModelImage tempMaskImage = (ModelImage) resultImage.clone(imgName + "_temp_mask");
        resultImage.disposeLocal();
        resultImage = findLargestRegion(tempMaskImage);
        tempMaskImage.disposeLocal();

        if (isThreadStopped()) {
            return;
        }

        // dilate
        AlgorithmBase dilateAlgo;

        progressValueBounds = calculateProgressValueBoundary(algorithmIndex++, ratios, 0, 100);

        if (progressValueBounds == null) {
            MipavUtil.displayError("The boundary of progress value for the algorithm: " + (algorithmIndex - 1) +
                                   " can't be calculated!");

            return;
        }

        fireProgressStateChanged(progressValueBounds[0], null, "Dilating the brain mask ...");

        if (!erosion25D) {
            int erosionKernel = AlgorithmMorphology3D.CONNECTED6;
            dilateAlgo = new AlgorithmMorphology3D(resultImage, erosionKernel, 0, AlgorithmMorphology3D.DILATE,
                                                   erosionIterations, 0, 0, 0, regionFlag);
        } else {
            int erosionKernel = AlgorithmMorphology25D.CONNECTED4;
            dilateAlgo = new AlgorithmMorphology25D(resultImage, erosionKernel, 0, AlgorithmMorphology25D.DILATE,
                                                    erosionIterations, 0, 0, 0, regionFlag);
        }

        dilateAlgo.setRunningInSeparateThread(isRunningInSeparateThread());
        dilateAlgo.addListener(this);
        dilateAlgo.run();


        fireProgressStateChanged(progressValueBounds[1]);

        if (isThreadStopped()) {
            return;
        }

        if (showIntermediateImages) {
            ModelImage tempImage = (ModelImage) resultImage.clone(imgName + "_erode_brain_dilate");
            tempImage.calcMinMax();
            new ViewJFrameImage(tempImage);
        }

        // do a closing on the mask
        progressValueBounds = calculateProgressValueBoundary(algorithmIndex++, ratios, 0, 100);

        if (progressValueBounds == null) {
            MipavUtil.displayError("The boundary of progress value for the algorithm: " + (algorithmIndex - 1) +
                                   " can't be calculated!");

            return;
        }

        fireProgressStateChanged(progressValueBounds[0], null, "Closing ...");

        AlgorithmMorphology25D closeAlgo = new AlgorithmMorphology25D(resultImage, closeKernel, closeKernelSize,
                                                                      AlgorithmMorphology25D.CLOSE, closeIterations + 1,
                                                                      closeIterations, 0, 0, regionFlag);
        closeAlgo.setRunningInSeparateThread(isRunningInSeparateThread());
        closeAlgo.addListener(this);
        closeAlgo.run();


        fireProgressStateChanged(progressValueBounds[1]);

        if (isThreadStopped()) {
            return;
        }

        if (showIntermediateImages) {
            ModelImage tempImage = (ModelImage) resultImage.clone(imgName + "_close");
            tempImage.calcMinMax();
            new ViewJFrameImage(tempImage);
        }

        // fill in interior holes of the mask
        if (fillHolesFlag) {
        	//Filling of holes is now being done by calling AlgorithmMorphogy Fill Holes
            
            AlgorithmMorphology25D idObjectsAlgo25D = new AlgorithmMorphology25D(resultImage, 0, 0,
                    AlgorithmMorphology25D.FILL_HOLES, 0, 0, 0, 0, regionFlag);
            
            idObjectsAlgo25D.run();
            
            if (showIntermediateImages) {
	            ModelImage tempImage = (ModelImage) resultImage.clone(imgName + "_filledHoles");
	            tempImage.calcMinMax();
	            new ViewJFrameImage(tempImage);
            }
            

            
            
            
            
        }

        // mask against original image
        // fireProgressStateChanged("Masking original image...");

        float[] maskData = new float[imgSize];
        float[] imgData = new float[imgSize];

        if ( resultImage == null )
        {
            setCompleted(false);
            return;
        }
        
        try {
            resultImage.exportData(0, imgSize, maskData);
            resultImage.disposeLocal();
            image.exportData(0, imgSize, imgData);

            brainRegionSize = 0;
            paintMask = new BitSet(imgSize);

            for (int i = 0; i < imgSize; i++) {

                if (extractPaint == true) {

                    // set the mask any place the computed pixel is less than zero. this is the extracted brain
                    if (maskData[i] > 0) {
                        paintMask.set(i);
                    }
                } else {
                    imgData[i] = imgData[i] * maskData[i];
                }

                if (maskData[i] != 0) {
                    brainRegionSize++;
                }
            }

            resultImage = (ModelImage) image.clone(imgName + "_brain");
            resultImage.importData(0, imgData, true);
        } catch (IOException ioe) {
            setCompleted(false);
            finalize();

            return;
        }

        System.out.println("Number of steps in the brain extraction: " + algorithmIndex);
        fireProgressStateChanged(100);

        int orient = image.getFileInfo(0).getImageOrientation();
        int[] axisOrient = image.getFileInfo(0).getAxisOrientation();

        for (int i = 0; i < zDim; i++) {
            resultImage.getFileInfo(i).setResolutions(xRes, 0);
            resultImage.getFileInfo(i).setResolutions(yRes, 1);
            resultImage.getFileInfo(i).setResolutions(zRes, 2);
            resultImage.getFileInfo(i).setSliceThickness(srcImage.getFileInfo(i).getSliceThickness());

            resultImage.getFileInfo(i).setImageOrientation(orient);
            resultImage.getFileInfo(i).setAxisOrientation(axisOrient);
        }
        


        setCompleted(true);
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        image = null;
        //resultImage = null;

        if (regionPointStack != null) {
            regionPointStack.finalize();
            regionPointStack = null;
        }

        super.finalize();
    }

    /**
     * Find the largest region within a mask image.
     *
     * @param   maskImage  the original mask image, with values of 0 or 1
     *
     * @return  ModelImage the image containing only the largest connected component from <code>maskImage</code>
     */
    public ModelImage findLargestRegion(ModelImage maskImage) {
        int index, sliceIndex;
        int regionSize, maxRegionSize = -1;
        int regionId = 100, maxRegionId = -1;
        int[] maskData = new int[imgSize];

        try {
            maskImage.exportData(0, imgSize, maskData);
        } catch (IOException ioe) {
            MipavUtil.displayError("Error exporting edge image.");

            return null;
        }

        int brainRegionThreshold = (int) (maskData.length * brainRegionThresholdRatio);
        regionPointStack = new IntVector((int) (maskData.length * regionPointStackRatio), 1000);

        // start in the middle of the volume
        int initIndex = ((zDim / 2) * sliceSize) + ((yDim / 2) * xDim) + (xDim / 2);

        // go right until we hit something big
        for (int xDisp = 0; xDisp < xDim; xDisp++) {
            index = initIndex + xDisp;

            if (maskData[index] == 1) {
                regionSize = markRegion(maskData, index, regionId);

                // System.out.println("Region Threshold = " + brainRegionThreshold + " region size = " + regionSize);
                if (regionSize > brainRegionThreshold) {
                    maxRegionSize = regionSize;
                    maxRegionId = regionId;

                    break;
                }

                regionId++;
                regionPointStack.removeAllElements();
            }
        }

        // sanity check
        if (maxRegionId == -1) {
            MipavUtil.displayError("No regions found in image. Too much erosion?");

            return null;
        }

        // save the largest region size so that we can calculate the volume later
        brainRegionSize = maxRegionSize;

        for (int iz = 0; iz < zDim; iz++) {
            sliceIndex = iz * yDim * xDim;

            for (int iy = 0; iy < yDim; iy++) {
                index = sliceIndex + (iy * xDim);

                for (int ix = 0; ix < xDim; ix++) {

                    if (maskData[index] == maxRegionId) {
                        maskData[index] = 1;
                    } else {
                        maskData[index] = 0;
                    }

                    index++;
                }
            }
        }

        ModelImage regionImage = new ModelImage(maskImage.getType(), maskImage.getExtents(), maskImage.getImageName());

        try {
            regionImage.importData(0, maskData, true);
        } catch (IOException ioe) {
            MipavUtil.displayError("Unable to export brain mask.");

            return null;
        }

        return regionImage;
    }

    /**
     * Returns the volume of the brain in units (typically millimeters).
     *
     * @return  The volume of the brain in voxel units (typically millimeters).
     */
    public final float getBrainVolume() {
        return brainRegionSize * xRes * yRes * zRes;
    }

    /**
     * Method is used when the &quot;extract paint&quot; option is set.
     *
     * @return  the paint mask bitset representing the area not removed from the image during the brain extraction
     */
    public BitSet getComputedPaintMask() {
        return paintMask;
    }

    /**
     * Get the final result image.
     *
     * @return  the final extracted brain image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Start at a point in an image and grow to all connected points, marking them and counting as we go.
     *
     * @param   imgData    the exported 3d image mask
     * @param   initIndex  the point to start growing the region from
     * @param   regionId   what to mark where the region is located
     *
     * @return  the number of voxels in the grown region
     */
    public int markRegion(int[] imgData, int initIndex, int regionId) {
        int count = 0;

        int i, idx;
        int x, y, z;

        if (imgData[initIndex] == 1) {
            regionPointStack.push(initIndex);
            count++;
        }

        while (!regionPointStack.isEmpty()) {
            i = regionPointStack.popFirstIn();

            if (imgData[i] == regionId) {

                // wrongly added this point to the stack more than once
                count--;

                continue;
            }

            x = i % xDim;
            y = (i % sliceSize) / xDim;
            z = i / sliceSize;

            imgData[i] = regionId;

            if ((x + 1) < xDim) {
                idx = i + 1;

                if (imgData[idx] == 1) {
                    regionPointStack.push(idx);
                    count++;
                }
            }

            if ((x - 1) >= 0) {
                idx = i - 1;

                if (imgData[idx] == 1) {
                    regionPointStack.push(idx);
                    count++;
                }
            }

            if ((y + 1) < yDim) {
                idx = i + xDim;

                if (imgData[idx] == 1) {
                    regionPointStack.push(idx);
                    count++;
                }
            }

            if ((y - 1) >= 0) {
                idx = i - xDim;

                if (imgData[idx] == 1) {
                    regionPointStack.push(idx);
                    count++;
                }
            }

            if ((z + 1) < zDim) {
                idx = i + sliceSize;

                if (imgData[idx] == 1) {
                    regionPointStack.push(idx);
                    count++;
                }
            }

            if ((z - 1) >= 0) {
                idx = i - sliceSize;

                if (imgData[idx] == 1) {
                    regionPointStack.push(idx);
                    count++;
                }
            }
        } // while ( !regionPointStack.isEmpty() )

        return count;
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (image == null) {
            displayError("Source Image is null");
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);

            return;
        }

        if (image.getNDims() != 3) {
            displayError("Source Image must be 3D");
            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);

            return;
        }

        

        try {
            extractBrain();
        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            fireProgressStateChanged(ViewJProgressBar.PROGRESS_WINDOW_CLOSING);

            System.gc();
            MipavUtil.displayError("Brain surface extraction: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Pad an image by copying the first and last slices 2 more times so that the laplacian algorithm behaves well in
     * edge conditions.
     *
     * @param   img  the image to pad
     *
     * @return  the padded image
     */
    private static ModelImage addPadding(ModelImage img) {
        int sliceLength = img.getSliceSize();
        float[] sliceBuffer = new float[sliceLength];
        int[] newExtents = new int[img.getNDims()];
        newExtents[0] = img.getExtents()[0];
        newExtents[1] = img.getExtents()[1];
        newExtents[2] = img.getExtents()[2] + 4;

        ModelImage paddedImg = new ModelImage(img.getType(), newExtents, img.getImageName() + "_pad");

        try {

            // move the image data
            for (int i = 0; i < img.getExtents()[2]; i++) {
                img.exportData(i * sliceLength, sliceLength, sliceBuffer);
                paddedImg.importData((i + 2) * sliceLength, sliceBuffer, false);
            }

            // add the padding to the beginning
            img.exportData(0, sliceLength, sliceBuffer);
            paddedImg.importData(0, sliceBuffer, false);
            paddedImg.importData(sliceLength, sliceBuffer, false);

            // add the padding to the end
            img.exportData(sliceLength * (img.getExtents()[2] - 1), sliceLength, sliceBuffer);
            paddedImg.importData((newExtents[2] - 2) * sliceLength, sliceBuffer, false);
            paddedImg.importData((newExtents[2] - 1) * sliceLength, sliceBuffer, false);
        } catch (IOException e) {
            e.printStackTrace();
            MipavUtil.displayError("Unable to export the first and last image slices.");

            return null;
        }

        return paddedImg;
    }

    /**
     * Remove the padding slices from the beginning and end of an image.
     *
     * @param   paddedImg  the padded image
     *
     * @return  the image without padding
     */
    private static ModelImage removePadding(ModelImage paddedImg) {
        int[] extents = new int[3];
        extents[0] = paddedImg.getExtents()[0];
        extents[1] = paddedImg.getExtents()[1];
        extents[2] = paddedImg.getExtents()[2] - 4;

        ModelImage img = new ModelImage(paddedImg.getType(), extents, paddedImg.getImageName() + "_nopad");

        boolean[] slices = new boolean[paddedImg.getExtents()[2]];
        slices[0] = true;
        slices[1] = true;
        slices[slices.length - 2] = true;
        slices[slices.length - 1] = true;

        AlgorithmRemoveSlices sliceAlgo = new AlgorithmRemoveSlices(paddedImg, img, slices);
        sliceAlgo.setRunningInSeparateThread(false);
        sliceAlgo.run();
        sliceAlgo.finalize();

        return img;
    }

    /**
     * Calculates the progress ratios for different algorithms.
     *
     * @param   filterIterations  the iteration number for filter.
     * @param   doSeparable       flag to indicate whether use separable algorithm or not.
     * @param   fillHolesFlag     DOCUMENT ME!
     *
     * @return  the progress ratios for different algorithms
     */
    private float[] calculateProgressRatios(int filterIterations, boolean doSeparable, boolean fillHolesFlag) {
        float[] ratios;

        if (filterIterations > 0) {

            if (fillHolesFlag) {
                ratios = new float[8];
                ratios[0] = 0.3f;

                if (doSeparable) {
                    ratios[1] = 0.10f;
                } else {
                    ratios[1] = 0.10f;
                }

                ratios[2] = 0.10f;
                ratios[3] = 0.10f;
                ratios[4] = 0.10f;
                ratios[5] = 0.10f;
                ratios[6] = 0.10f;
                ratios[7] = 0.10f;
            } else {
                ratios = new float[6];
                ratios[0] = 0.3f;

                if (doSeparable) {
                    ratios[1] = 0.14f;
                } else {
                    ratios[1] = 0.14f;
                }

                ratios[2] = 0.14f;
                ratios[3] = 0.14f;
                ratios[4] = 0.14f;
                ratios[5] = 0.14f;
            }
        } else {

            if (fillHolesFlag) {
                ratios = new float[7];
                ratios[0] = 0.1f;

                if (doSeparable) {
                    ratios[1] = 0.15f;
                } else {
                    ratios[1] = 0.15f;
                }

                ratios[2] = 0.15f;
                ratios[3] = 0.15f;
                ratios[4] = 0.15f;
                ratios[5] = 0.15f;
                ratios[6] = 0.15f;
            } else {
                ratios = new float[5];
                ratios[0] = 0.2f;

                if (doSeparable) {
                    ratios[1] = 0.2f;
                } else {
                    ratios[1] = 0.2f;
                }

                ratios[2] = 0.2f;
                ratios[3] = 0.2f;
                ratios[4] = 0.2f;
            }
        }

        return ratios;
    }

    /**
     * Calculates the boundary of progress value which is assigned to the <code>index</code>th algorithm.
     *
     * @param   index   the algorithm index
     * @param   ratios  the ratio array assigned to the algorithms.
     * @param   min     the minimum progress value available to be distributed
     * @param   max     the maximum progress value available to be distributed
     *
     * @return  the minimum and maximum progress value assigned to the <code>index</code>th algorithm.
     */
    private int[] calculateProgressValueBoundary(int index, float[] ratios, int min, int max) {

        if ((ratios == null) || (index < 0) || (index >= ratios.length)) {
            return null;
        }

        float accumulatedRatio = 0f;

        for (int i = 0; i < index; i++) {
            accumulatedRatio += ratios[i];
        }

        int[] range = new int[2];
        range[0] = min + Math.round(accumulatedRatio * (max - min));
        range[1] = min + Math.round((accumulatedRatio + ratios[index]) * (max - min));

        return range;
    }
}
