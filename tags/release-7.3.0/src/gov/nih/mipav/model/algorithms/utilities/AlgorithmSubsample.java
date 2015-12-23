package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Takes in an image and subsamples it to a new set of dimensions. The subsample image is a Gaussian weighted average of
 * neighboring pixels (voxels) of the original image. For 4D images only the first 3 dimensions are subsampled
 *
 * @author  Evan McCreedy
 */
public class AlgorithmSubsample extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Ratios used to translate points in the result image to points in the source image. */
    private double[] posRatios;

    /** DOCUMENT ME! */
    private boolean processIndep = true;

    /** Extents the resultImage should have. */
    private int[] resultExtents;

    /** Result image. */
    private ModelImage resultImage;
    
    /** Padded image. */
    private ModelImage paddedImage;

    /** Std deviation of the Gaussian function. */
    private float[] sigmas;

    /** Image to subsample from. */
    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private boolean transformVOI = false;

    /** DOCUMENT ME! */
    private TransMatrix transMatrix = null;

    /** Whether to perform trilinear filtering when getting intensities of points on the source image. */
    private boolean trilinearFlag = true;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Setup for a later subsampling of an image.
     *
     * @param  src           image to be subsampled
     * @param  result        image created by subsampling
     * @param  newExtents    dimensions of the result image to be sampled into
     * @param  _sigmas       x, y, and z scales for the gaussian function
     * @param  indep         process slices independently (cannot change the z dimension)
     * @param  transformVOI  if true transform the VOIs
     * @param  transMatrix   transformation matrix
     */
    public AlgorithmSubsample(ModelImage src, ModelImage result, int[] newExtents, int[] padExtents, float[] _sigmas, boolean indep,
                              boolean transformVOI, TransMatrix transMatrix, boolean doPad) {
        super(result, src);

        if (src == null) {
            MipavUtil.displayError("No source image provided to be subsampled.");
            
            return;
        }
        
        processIndep = indep;
        this.transformVOI = transformVOI;
       
        if (doPad) {
        	srcImage = padImage(src, padExtents);
        	//new ViewJFrameImage((ModelImage)(srcImage.clone()), null, new Dimension(610, 200));
        } else {
        	srcImage = src;
        }
        
        
        
        resultImage = result;
        resultExtents = newExtents;
        sigmas = _sigmas;
        this.transMatrix = transMatrix;
        
        if (processIndep && ((srcImage.getExtents().length > 2) && (srcImage.getExtents()[2] != resultExtents[2]))) {
            MipavUtil.displayError("Cannot perform 2.5D subsampling and change the number of slices");
            threadStopped = true;
            setCompleted(false);

            return;
        }

        computeRatios();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
        if (paddedImage != null) {
            paddedImage.disposeLocal();
        }
    }

    /**
     * Return the result image (or null if not created yet).
     *
     * @return  the subsampled image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Subsample the image.
     */
    public void runAlgorithm() {
        fireProgressStateChanged("Subsamping image " + srcImage.getImageName(), "Subsampling image..");


        ModelSimpleImage temp = null;

        if (srcImage.getNDims() == 2) {
            temp = subsample2D(1);
        } else if ((srcImage.getNDims() == 3) && processIndep) {
            temp = subsample2D(srcImage.getExtents()[2]);
        } else if (srcImage.getNDims() == 3) {
            temp = subsample3D();
        } else if (srcImage.getNDims() == 4) {
            temp = subsample4D();
        }

        if (resultImage == null) {
            resultImage = new ModelImage(srcImage.getType(), resultExtents, srcImage.getImageName() + "_subsampled");
        }

        try {
            resultImage.importData(0, temp.data, true);

            fireProgressStateChanged(100);
        } catch (IOException ioe) {
            MipavUtil.displayError("Error caught trying to import subsample result image.");
            setCompleted(false);

            return;
        }

        // fix the fileinfos for the resultImage
        float denom = (float) srcImage.getExtents()[0] / resultImage.getExtents()[0];

        if (srcImage.getNDims() == 4) {

            for (int i = 0; i < (resultImage.getExtents()[2] * resultImage.getExtents()[3]); i++) {
                resultImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[0].getResolution(0) * denom, 0);
                resultImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[0].getResolution(1) * denom, 1);
                resultImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[0].getResolution(2) * denom, 2);

                resultImage.getFileInfo()[i].setSliceThickness(srcImage.getFileInfo()[0].getSliceThickness() * denom);

                resultImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[0].getResolution(3) * denom, 3);

                resultImage.getFileInfo()[i].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                resultImage.getFileInfo()[i].setModality(srcImage.getFileInfo()[0].getModality());
                resultImage.getFileInfo()[i].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation());
                resultImage.getFileInfo()[i].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
                resultImage.getFileInfo()[i].setOrigin(srcImage.getFileInfo()[0].getOrigin());
            }
        } else if (srcImage.getNDims() == 3) {

            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                resultImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[0].getResolution(0) * denom, 0);
                resultImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[0].getResolution(1) * denom, 1);

                if (srcImage.getExtents()[2] == resultImage.getExtents()[2]) {

                    // the z dimension shouldn't be changing
                    resultImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[0].getResolution(2), 2);
                    resultImage.getFileInfo()[i].setSliceThickness(srcImage.getFileInfo()[0].getSliceThickness());
                } else {

                    // ratio of extents change for z dim might be different from that of the x and y dims
                    denom = (float) srcImage.getExtents()[2] / resultImage.getExtents()[2];

                    resultImage.getFileInfo()[i].setResolutions(srcImage.getFileInfo()[0].getResolution(2) * denom, 2);
                    resultImage.getFileInfo()[i].setSliceThickness(srcImage.getFileInfo()[0].getSliceThickness() *
                                                                       denom);
                }

                resultImage.getFileInfo()[i].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                resultImage.getFileInfo()[i].setModality(srcImage.getFileInfo()[0].getModality());
                resultImage.getFileInfo()[i].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation());
                resultImage.getFileInfo()[i].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
                resultImage.getFileInfo()[i].setOrigin(srcImage.getFileInfo()[0].getOrigin());
            }
        } else if (srcImage.getNDims() == 2) {
            resultImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolution(0) * denom, 0);
            resultImage.getFileInfo()[0].setResolutions(srcImage.getFileInfo()[0].getResolution(1) * denom, 1);
            resultImage.getFileInfo()[0].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());

            resultImage.getFileInfo()[0].setModality(srcImage.getFileInfo()[0].getModality());
            resultImage.getFileInfo()[0].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation());
            resultImage.getFileInfo()[0].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
            resultImage.getFileInfo()[0].setOrigin(srcImage.getFileInfo()[0].getOrigin());
        }

        resultImage.calcMinMax();

        if (transformVOI) {
            TransMatrix xfrm = null;
            int imgLength;
            float[] imgBuf;
            fireProgressStateChanged("Subsample on VOIs");
            xfrm = AlgorithmTransform.matrixtoInverseArray(transMatrix);

            if (srcImage.getNDims() >= 3) {
                imgLength = srcImage.getExtents()[0] * srcImage.getExtents()[1] * srcImage.getExtents()[2];
            } else {
                imgLength = srcImage.getExtents()[0] * srcImage.getExtents()[1];
            }


            if (srcImage.isColorImage()) {
                imgLength = imgLength * 4;
            }

            imgBuf = new float[imgLength];

            try {
                srcImage.exportData(0, imgLength, imgBuf);
            } catch (IOException error) {
                displayError("Algorithm Subsample: IOException on srcImage.exportData");

                finalize();
                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                finalize();
                System.gc();
                displayError("Algorithm Subsample: Out of memory on srcImage.exportData");

                setCompleted(false);

                return;
            }

            if (srcImage.getNDims() == 2) {
                transform25DVOI(srcImage, imgBuf, xfrm);
            } else if ((srcImage.getNDims() == 3) && (!processIndep)) {
                transform3DVOI(srcImage, imgBuf, xfrm);
            } else if ((srcImage.getNDims() == 3) && (processIndep)) {
                transform25DVOI(srcImage, imgBuf, xfrm);
            }

        } // if (transformVOI)

        setCompleted(true);


    }

    /**
     * Compute the value of the 2D gaussian at a point.
     *
     * @param   locX    x location of the value to compute
     * @param   locY    y location of the value to compute
     * @param   sigmaX  standard deviation of the guassian in the x dimension
     * @param   sigmaY  standard deviation of the guassian in the y dimension
     *
     * @return  the value of the 2D gaussian at the given point
     */
    private double computeGaussian2D(double locX, double locY, double sigmaX, double sigmaY) {
        double expX = Math.exp(-(locX * locX) / (2.0f * sigmaX * sigmaX));
        double denomX = Math.sqrt(2.0 * Math.PI) * sigmaX;

        double expY = Math.exp(-(locY * locY) / (2.0f * sigmaY * sigmaY));
        double denomY = Math.sqrt(2.0 * Math.PI) * sigmaY;

        return ((expX * expY) / (denomX * denomY));
    }

    /**
     * Computes the gaussian weights for a particular point. Written by Paul Hemler in AlgorithmVesselSegmenation.
     *
     * @param   locX    distance to go in the x dir from the point
     * @param   locY    distance to go in the y dir from the point
     * @param   locZ    distance to go in the z dir from the point
     * @param   sigmaX  stddev of the gaussian function in the x dir
     * @param   sigmaY  stddev of the gaussian function in the y dir
     * @param   sigmaZ  stddev of the gaussian function in the z dir
     *
     * @return  the gaussian weight coeff.
     */
    private double computeGaussian3D(double locX, double locY, double locZ, double sigmaX, double sigmaY,
                                     double sigmaZ) {
        double expX = Math.exp(-(locX * locX) / (2.0f * sigmaX * sigmaX));
        double denomX = Math.sqrt(2.0 * Math.PI) * sigmaX;

        double expY = Math.exp(-(locY * locY) / (2.0f * sigmaY * sigmaY));
        double denomY = Math.sqrt(2.0 * Math.PI) * sigmaY;

        double expZ = Math.exp(-(locZ * locZ) / (2.0f * sigmaZ * sigmaZ));
        double denomZ = Math.sqrt(2.0 * Math.PI) * sigmaZ;

        double rtnVal = (expX * expY * expZ) / (denomX * denomY * denomZ);

        return rtnVal;
    } // end computeGaussian3D(...)

    /**
     * Computes the image scaling ratios.
     */
    private void computeRatios() {
        posRatios = new double[srcImage.getNDims()];

        for (int i = 0; i < srcImage.getNDims(); i++) {

            // resNew[i]*(extentsNew[i]) = resOld[i]*(extentsOld[i])
            posRatios[i] = ((double) (srcImage.getExtents()[i]) / (double) (resultExtents[i]));
        }
    }

    /**
     * Translates a point from the result image to the source image.
     *
     * @param   x  x coordinate of point to be translated
     * @param   y  y coordinate of point to be translated
     *
     * @return  the translated point on the source image
     */
    private float[] getNearestNeighborPos2D(float x, float y) {
        return new float[] { (float) (x * posRatios[0]), (float) (y * posRatios[1]) };
    }

    /**
     * Translates a point from the result image to the source image.
     *
     * @param   x  x coordinate of point to be translated
     * @param   y  y coordinate of point to be translated
     * @param   z  z coordinate of point to be translated
     *
     * @return  the translated point on the source image
     */
    private float[] getNearestNeighborPos3D(float x, float y, float z) {
        return new float[] { (float) (x * posRatios[0]), (float) (y * posRatios[1]), (float) (z * posRatios[2]) };
    }

    /**
     * Version of get that performs tri-linear interpoloation. Note: does NOT perform bounds checking.
     *
     * @param   x             x coordinate
     * @param   y             y coordinate
     * @param   z             z coordinate
     * @param   channelIndex  color channel
     * @param   src           image to get intensity value from
     *
     * @return  the tri-linear interpolated value
     */
    private float getTriLinearVal3D(float x, float y, float z, int channelIndex, ModelSimpleImage src) {
        int colorOffset = 1; // no color offset for grayscale images

        if (srcImage.isColorImage()) {
            colorOffset = 4;
        }

        int xDim = src.xDim;
        int yDim = src.yDim;
        int imageSize = xDim * yDim;
        int position1, position2;
        int dataSize = src.data.length;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = (colorOffset * ((intZ * imageSize) + (intY * xDim) + intX)) + channelIndex;
        position2 = position1 + (colorOffset * imageSize);

        // if not on the last z slice, get intensities of nearest points in 2 closest planes
        if ((position1 >= 0) && (position1 < (dataSize - (colorOffset * ((xDim * yDim) + 1)))) && (position2 >= 0) &&
                (position2 < (dataSize - (colorOffset * ((xDim * yDim) + 1))))) {

            a1 = ((1 - dx) * src.data[position1]) + (dx * src.data[position1 + colorOffset]);
            a2 = ((1 - dx) * src.data[position1 + (colorOffset * xDim)]) +
                 (dx * src.data[position1 + (colorOffset * (xDim + 1))]);
            b1 = ((1 - dy) * a1) + (dy * a2);

            a1 = ((1 - dx) * src.data[position2]) + (dx * src.data[position2 + colorOffset]);
            a2 = ((1 - dx) * src.data[position2 + (colorOffset * xDim)]) +
                 (dx * src.data[position2 + (colorOffset * (xDim + 1))]);
            b2 = ((1 - dy) * a1) + (dy * a2);

            return (float) (((1 - dz) * b1) + (dz * b2));
        } // if on last z slice, just get intensities of the last slice
        else if ((position1 >= 0) && (position1 < (dataSize - (colorOffset * (xDim + 1))))) {
            a1 = ((1 - dx) * src.data[position1]) + (dx * src.data[position1 + colorOffset]);
            a2 = ((1 - dx) * src.data[position1 + (colorOffset * xDim)]) +
                 (dx * src.data[position1 + (colorOffset * (xDim + 1))]);
            b1 = ((1 - dy) * a1) + (dy * a2);

            return b1;
        } // if on last column of second to last slice
        else if ((position1 >= 0) && (position1 < (dataSize - (colorOffset * xDim)))) {
            a1 = ((1 - dx) * src.data[position1]) + (dx * src.data[position1 + colorOffset]);
            a2 = src.data[position1 + (colorOffset * xDim)];
            b1 = ((1 - dy) * a1) + (dy * a2);

            return b1;
        } // if on last row
        else if ((position1 >= 0) && (position1 < (dataSize - colorOffset))) {
            a1 = ((1 - dx) * src.data[position1]) + (dx * src.data[position1 + colorOffset]);
            b1 = a1;

            return a1;
        } // if on last column of last row
        else if ((position1 >= 0) && (position1 < dataSize)) {
            a1 = src.data[position1];

            return a1;
        } else {

            // MipavUtil.displayError("Error in bounds for subsampling ("+x+","+y+","+z+").");
            System.out.println("Error in bounds for subsampling (" + x + "," + y + "," + z + ").");

            return 0f;
        }
    }

    /**
     * Version of get that performs tri-linear interpoloation. Note: does NOT perform bounds checking.
     *
     * @param   x             x coordinate
     * @param   y             y coordinate
     * @param   z             z coordinate
     * @param   t             t coordinate
     * @param   channelIndex  color channel
     * @param   src           image to get intensity value from
     *
     * @return  the tri-linear interpolated value
     */
    private float getTriLinearVal3D(float x, float y, float z, int t, int channelIndex, ModelSimpleImage src) {
        int colorOffset = 1; // no color offset for grayscale images

        if (srcImage.isColorImage()) {
            colorOffset = 4;
        }

        int xDim = src.xDim;
        int yDim = src.yDim;
        int zDim = src.zDim;
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;
        int volDataSize = colorOffset * volSize;
        int volStart = colorOffset * t * volSize;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz;
        float a1, a2;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        position1 = volStart + (colorOffset * ((intZ * sliceSize) + (intY * xDim) + intX)) + channelIndex;
        position2 = position1 + (colorOffset * sliceSize);

        // if not on the last z slice, get intensities of nearest points in 2 closest planes
        if ((position1 >= volStart) && (position1 < (volStart + volDataSize - (colorOffset * ((xDim * yDim) + 1)))) &&
                (position2 >= volStart) &&
                (position2 < (volStart + volDataSize - (colorOffset * ((xDim * yDim) + 1))))) {

            a1 = ((1 - dx) * src.data[position1]) + (dx * src.data[position1 + colorOffset]);
            a2 = ((1 - dx) * src.data[position1 + (colorOffset * xDim)]) +
                 (dx * src.data[position1 + (colorOffset * (xDim + 1))]);
            b1 = ((1 - dy) * a1) + (dy * a2);

            a1 = ((1 - dx) * src.data[position2]) + (dx * src.data[position2 + colorOffset]);
            a2 = ((1 - dx) * src.data[position2 + (colorOffset * xDim)]) +
                 (dx * src.data[position2 + (colorOffset * (xDim + 1))]);
            b2 = ((1 - dy) * a1) + (dy * a2);

            return (float) (((1 - dz) * b1) + (dz * b2));
        } // if on last z slice, just get intensities of the last slice
        else if ((position1 >= volStart) && (position1 < (volStart + volDataSize - (colorOffset * (xDim + 1))))) {
            a1 = ((1 - dx) * src.data[position1]) + (dx * src.data[position1 + colorOffset]);
            a2 = ((1 - dx) * src.data[position1 + (colorOffset * xDim)]) +
                 (dx * src.data[position1 + (colorOffset * (xDim + 1))]);
            b1 = ((1 - dy) * a1) + (dy * a2);

            return b1;
        } // if on last column of second to last slice
        else if ((position1 >= volStart) && (position1 < (volStart + volDataSize - (colorOffset * xDim)))) {
            a1 = ((1 - dx) * src.data[position1]) + (dx * src.data[position1 + colorOffset]);
            a2 = src.data[position1 + (colorOffset * xDim)];
            b1 = ((1 - dy) * a1) + (dy * a2);

            return b1;
        } // if on last row
        else if ((position1 >= volStart) && (position1 < (volStart + volDataSize - colorOffset))) {
            a1 = ((1 - dx) * src.data[position1]) + (dx * src.data[position1 + colorOffset]);
            b1 = a1;

            return a1;
        } // if on last column of last row
        else if ((position1 >= volStart) && (position1 < (volStart + volDataSize))) {
            a1 = src.data[position1];

            return a1;
        } else {

            // MipavUtil.displayError("Error in bounds for subsampling ("+x+","+y+","+z+","+t+").");
            System.out.println("Error in bounds for subsampling (" + x + "," + y + "," + z + "," + t + ").");

            return 0f;
        }
    }

    /**
     * Get the value of a 2D point.
     *
     * @param   x             x coordinate
     * @param   y             y coordinate
     * @param   z             z coordinate (0 if really 2D)
     * @param   channelIndex  color channel
     * @param   src           image to sample from
     *
     * @return  the point's value
     */
    private float getValue2D(float x, float y, float z, int channelIndex, ModelSimpleImage src) {
        int rowSize = src.xDim;
        int sliceSize = src.xDim * src.yDim;
        int colorOffset = 1;

        if (srcImage.isColorImage()) {
            colorOffset = 4;
        }

        return src.data[(colorOffset * (((int) z * sliceSize) + ((int) y * rowSize) + (int) x)) + channelIndex];
    }

    /**
     * Get the value of a 3D point.
     *
     * @param   x             x coordinate
     * @param   y             y coordinate
     * @param   z             z coordinate
     * @param   channelIndex  color channel
     * @param   src           image to sample from
     *
     * @return  the point's value
     */
    private float getValue3D(float x, float y, float z, int channelIndex, ModelSimpleImage src) {
        int sliceSize = src.xDim * src.yDim;
        int rowSize = src.xDim;

        int colorOffset = 1; // no color offset for grayscale images

        if (srcImage.isColorImage()) {
            colorOffset = 4;
        }

        if (trilinearFlag) {
            return getTriLinearVal3D(x, y, z, channelIndex, src);
        } else {
            return src.data[(colorOffset * (((int) z * sliceSize) + ((int) y * rowSize) + (int) x)) + channelIndex];
        }
    }

    /**
     * Get the value of a 4D point.
     *
     * @param   x             x coordinate
     * @param   y             y coordinate
     * @param   z             z coordinate
     * @param   t             DOCUMENT ME!
     * @param   channelIndex  color channel
     * @param   src           image to sample from
     *
     * @return  the point's value
     */
    private float getValue3D(float x, float y, float z, int t, int channelIndex, ModelSimpleImage src) {
        int sliceSize = src.xDim * src.yDim;
        int rowSize = src.xDim;
        int volSize = sliceSize * src.zDim;

        int colorOffset = 1; // no color offset for grayscale images

        if (srcImage.isColorImage()) {
            colorOffset = 4;
        }

        if (trilinearFlag) {
            return getTriLinearVal3D(x, y, z, t, channelIndex, src);
        } else {
            return src.data[(colorOffset * ((t * volSize) + ((int) z * sliceSize) + ((int) y * rowSize) + (int) x)) +
                            channelIndex];
        }
    }

    /**
     * Subsample a 2D image.
     *
     * @param   nImages  the number of 2D images to subsample (1 if really 2D)
     *
     * @return  the subsampled image
     */
    private ModelSimpleImage subsample2D(int nImages) {

        ModelSimpleImage src = new ModelSimpleImage(srcImage.getExtents(), srcImage.getFileInfo(0).getResolutions(),
                                                    srcImage);
        float[] resolutions = new float[srcImage.getNDims()];

        for (int i = 0; i < resolutions.length; i++) {
            resolutions[i] = src.resolutions[i] * (float) posRatios[i];
        }

        ModelSimpleImage result = new ModelSimpleImage(resultExtents, resolutions, srcImage.isColorImage());

        int rowSize = result.xDim;
        int sliceSize = result.xDim * result.yDim;
        int row = src.xDim;

        int nextRow, nextCol;
        int previousRow, previousCol;

        double[] gaussian = new double[4];

        gaussian[0] = this.computeGaussian2D(0, 0, sigmas[0], sigmas[1]);
        gaussian[1] = this.computeGaussian2D(1, 0, sigmas[0], sigmas[1]);
        gaussian[2] = this.computeGaussian2D(0, 1, sigmas[0], sigmas[1]);
        gaussian[3] = this.computeGaussian2D(1, 1, sigmas[0], sigmas[1]);

        double totalGaussian = 0;

        // numbers if not near any edges
        int numAdjXPts = 2, numAdjYPts = 2, numCornerPts = 4;

        int numXEdges, numYEdges;

        int percent = 5;
        float remainder = 0;
        float inc = (90.0f / result.zDim);

        int resultOffset;
        int yOffset;
        int zOffset;

        fireProgressStateChanged(percent);

        for (int s = 0; s < nImages; s++) {
            zOffset = s * sliceSize;

            for (int y = 0; y < result.yDim; y++) {
                yOffset = y * rowSize;

                for (int x = 0; x < result.xDim; x++) {
                    resultOffset = zOffset + yOffset + x;

                    // translate point back to source image
                    float[] point = getNearestNeighborPos2D(x, y);
                    float fx, fy;
                    int ix, iy;

                    fx = ix = (int) point[0];
                    fy = iy = (int) point[1];

                    // first row of slice
                    if (iy == 0) {
                        previousRow = -1;
                        nextRow = (iy + 1) * row;
                    } // last row of slice
                    else if (iy >= (src.yDim - 1)) {
                        previousRow = (iy - 1) * row;
                        nextRow = -1;
                    } // inside row of slice
                    else {
                        previousRow = (iy - 1) * row;
                        nextRow = (iy + 1) * row;
                    }

                    // first column of slice
                    if (ix == 0) {
                        previousCol = -1;
                        nextCol = (ix + 1);
                    } // last column of slice
                    else if (ix >= (src.xDim - 1)) {
                        previousCol = (ix - 1);
                        nextCol = -1;
                    } // inside column of slice
                    else {
                        previousCol = (ix - 1);
                        nextCol = (ix + 1);
                    }

                    numXEdges = 0;
                    numYEdges = 0;
                    // count how many of the edges of the image we're on

                    if ((previousRow == -1) || (nextRow == -1)) {
                        numYEdges = 1;
                    }

                    if ((previousCol == -1) || (nextCol == -1)) {
                        numXEdges = 1;
                    }

                    numAdjXPts = 2;
                    numAdjYPts = 2;
                    numCornerPts = 4;

                    if ((numXEdges == 1) && (numYEdges == 1)) {
                        numAdjXPts = 1;
                        numAdjYPts = 1;
                        numCornerPts = 1;
                    } else if (numXEdges == 1) {
                        numAdjXPts = 1;
                        numAdjYPts = 2;
                        numCornerPts = 2;
                    } else if (numYEdges == 1) {
                        numAdjXPts = 2;
                        numAdjYPts = 1;
                        numCornerPts = 2;
                    }

                    // normalize the gaussian so that the sum of the neighbors' coeff and the point coeff is 1
                    totalGaussian = gaussian[0] + (gaussian[1] * numAdjXPts) + (gaussian[2] * numAdjYPts) +
                                    (gaussian[3] * numCornerPts);

                    double ptCoeff = gaussian[0] / totalGaussian;
                    double adjXCoeff = gaussian[1] / totalGaussian;
                    double adjYCoeff = gaussian[2] / totalGaussian;
                    double cornerCoeff = gaussian[3] / totalGaussian;

                    int numChannels = 1; // grayscale image only has one channel
                    int colorOffset = 1; // no color offset for grayscale images

                    if (srcImage.isColorImage()) {
                        numChannels = 4;
                        colorOffset = 4;
                    }

                    for (int c = 0; c < numChannels; c++) {
                        double adjXVal = 0, adjYVal = 0, cornerVal = 0;

                        // add points only if they exist (ie they aren't past a boundary of the image that we're up
                        // against)

                        if (nextRow != -1) {
                            adjYVal += getValue2D(fx, fy + 1, s, c, src);

                            if (nextCol != -1) {
                                cornerVal += getValue2D(fx + 1, fy + 1, s, c, src);
                            }

                            if (previousCol != -1) {
                                cornerVal += getValue2D(fx - 1, fy + 1, s, c, src);
                            }
                        }

                        if (previousRow != -1) {
                            adjYVal += getValue2D(fx, fy - 1, s, c, src);

                            if (nextCol != -1) {
                                cornerVal += getValue2D(fx + 1, fy - 1, s, c, src);
                            }

                            if (previousCol != -1) {
                                cornerVal += getValue2D(fx - 1, fy - 1, s, c, src);
                            }
                        }

                        if (previousCol != -1) {
                            adjXVal += getValue2D(fx - 1, fy, s, c, src);
                        }

                        if (nextCol != -1) {
                            adjXVal += getValue2D(fx + 1, fy, s, c, src);
                        }

                        result.data[(colorOffset * resultOffset) + c] = (float) ((ptCoeff *
                                                                                      (getValue2D(fx, fy, s, c, src))) +
                                                                                 (adjXCoeff * (adjXVal)) +
                                                                                 (adjYCoeff * (adjYVal)) +
                                                                                 (cornerCoeff * (cornerVal)));
                    } // for (c = 0; c <= numChannels; c++)
                }
            }
        }

        percent += (int) inc;

        fireProgressStateChanged(percent);

        remainder += inc - (int) inc;

        if (remainder >= 1) {
            percent += (int) remainder;
            remainder = remainder - (int) remainder;

            fireProgressStateChanged(percent);
        }

        return result;
    }

    /**
     * Perform the subsampling. Create a result image with the extents given into the constuctor. Then, for each point
     * in the result image, find point which corresponds to it in the source image. Add up weighted intensities of the
     * neighbors of this point in the source image and the intensity of the point itself. Store this value in the point
     * of the result image. Once all points in the result image have been visted, return the result image.
     *
     * @return  resampled image
     */
    private ModelSimpleImage subsample3D() {

        // convert the source image to make it easier to work with
        ModelSimpleImage src = new ModelSimpleImage(srcImage.getExtents(), srcImage.getFileInfo(0).getResolutions(),
                                                    srcImage);
        float[] resolutions = new float[srcImage.getNDims()];

        for (int i = 0; i < resolutions.length; i++) {
            resolutions[i] = src.resolutions[i] * (float) posRatios[i];
        }

        ModelSimpleImage result = new ModelSimpleImage(resultExtents, resolutions, srcImage.isColorImage());

        int sliceSize = result.xDim * result.yDim;
        int rowSize = result.xDim;
        int slice = src.xDim * src.yDim;
        int row = src.xDim;

        int nextRow, nextCol, nextSlice;
        int previousRow, previousCol, previousSlice;

        double[] gaussian = new double[8];

        gaussian[0] = this.computeGaussian3D(0, 0, 0, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[1] = this.computeGaussian3D(1, 0, 0, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[2] = this.computeGaussian3D(0, 1, 0, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[3] = this.computeGaussian3D(0, 0, 1, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[4] = this.computeGaussian3D(1, 1, 0, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[5] = this.computeGaussian3D(1, 0, 1, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[6] = this.computeGaussian3D(0, 1, 1, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[7] = this.computeGaussian3D(1, 1, 1, sigmas[0], sigmas[1], sigmas[2]);

        double totalGaussian = 0;

        // numbers if not near any edges
        int numAdjXPts = 2, numAdjYPts = 2, numAdjZPts = 2, numDiagXYPts = 4, numDiagXZPts = 4, numDiagYZPts = 4,
            numCornerPts = 8;

        int numXEdges, numYEdges, numZEdges;

        int percent = 5;
        float remainder = 0;
        float inc = (90.0f / result.zDim);

        fireProgressStateChanged(percent);

        for (int z = 0; z < result.zDim; z++) {

            fireProgressStateChanged("Subsampling slice " + (z + 1) + "..");

            for (int y = 0; y < result.yDim; y++) {

                for (int x = 0; x < result.xDim; x++) {

                    // translate point back to source image
                    float[] point = getNearestNeighborPos3D(x, y, z);
                    float fx, fy, fz;
                    int ix, iy, iz;

                    // need float indexes for trilinear point retrieval
                    if (trilinearFlag) {
                        fx = point[0];
                        fy = point[1];
                        fz = point[2];
                        ix = (int) point[0];
                        iy = (int) point[1];
                        iz = (int) point[2];
                    } // if not using trilinear point retrieval, just get the nearest integer points
                    else {
                        fx = ix = (int) point[0];
                        fy = iy = (int) point[1];
                        fz = iz = (int) point[2];
                    }

                    // first slice
                    if (iz == 0) {
                        previousSlice = -1;
                        nextSlice = (iz + 1) * slice;
                    } // last slice
                    else if (iz >= (src.zDim - 1)) {
                        previousSlice = (iz - 1) * slice;
                        nextSlice = -1;
                    } // any other slice
                    else {
                        previousSlice = (iz - 1) * slice;
                        nextSlice = (iz + 1) * slice;
                    }

                    // first row of slice
                    if (iy == 0) {
                        previousRow = -1;
                        nextRow = (iy + 1) * row;
                    } // last row of slice
                    else if (iy >= (src.yDim - 1)) {
                        previousRow = (iy - 1) * row;
                        nextRow = -1;
                    } // inside row of slice
                    else {
                        previousRow = (iy - 1) * row;
                        nextRow = (iy + 1) * row;
                    }

                    // first column of slice
                    if (ix == 0) {
                        previousCol = -1;
                        nextCol = (ix + 1);
                    } // last column of slice
                    else if (ix >= (src.xDim - 1)) {
                        previousCol = (ix - 1);
                        nextCol = -1;
                    } // inside column of slice
                    else {
                        previousCol = (ix - 1);
                        nextCol = (ix + 1);
                    }

                    numXEdges = 0;
                    numYEdges = 0;
                    numZEdges = 0;

                    // count how many of the edges of the image we're on
                    if ((previousSlice == -1) || (nextSlice == -1)) {
                        numZEdges = 1;
                    }

                    if ((previousRow == -1) || (nextRow == -1)) {
                        numYEdges = 1;
                    }

                    if ((previousCol == -1) || (nextCol == -1)) {
                        numXEdges = 1;
                    }

                    numAdjXPts = 2;
                    numAdjYPts = 2;
                    numAdjZPts = 2;
                    numDiagXYPts = 4;
                    numDiagXZPts = 4;
                    numDiagYZPts = 4;
                    numCornerPts = 8;

                    if ((numXEdges == 1) && (numYEdges == 1) && (numZEdges == 1)) {
                        numAdjXPts = 1;
                        numAdjYPts = 1;
                        numAdjZPts = 1;
                        numDiagXYPts = 1;
                        numDiagXZPts = 1;
                        numDiagYZPts = 1;
                        numCornerPts = 1;
                    } else if ((numXEdges == 1) && (numYEdges == 1)) {
                        numAdjXPts = 1;
                        numAdjYPts = 1;
                        numAdjZPts = 2;
                        numDiagXYPts = 1;
                        numDiagXZPts = 2;
                        numDiagYZPts = 2;
                        numCornerPts = 2;
                    } else if ((numXEdges == 1) && (numZEdges == 1)) {
                        numAdjXPts = 1;
                        numAdjYPts = 2;
                        numAdjZPts = 1;
                        numDiagXYPts = 2;
                        numDiagXZPts = 1;
                        numDiagYZPts = 2;
                        numCornerPts = 2;
                    } else if ((numYEdges == 1) && (numZEdges == 1)) {
                        numAdjXPts = 2;
                        numAdjYPts = 1;
                        numAdjZPts = 1;
                        numDiagXYPts = 2;
                        numDiagXZPts = 2;
                        numDiagYZPts = 1;
                        numCornerPts = 2;
                    } else if (numXEdges == 1) {
                        numAdjXPts = 1;
                        numAdjYPts = 2;
                        numAdjZPts = 2;
                        numDiagXYPts = 2;
                        numDiagXZPts = 2;
                        numDiagYZPts = 4;
                        numCornerPts = 4;
                    } else if (numYEdges == 1) {
                        numAdjXPts = 2;
                        numAdjYPts = 1;
                        numAdjZPts = 2;
                        numDiagXYPts = 2;
                        numDiagXZPts = 4;
                        numDiagYZPts = 2;
                        numCornerPts = 4;
                    } else if (numZEdges == 1) {
                        numAdjXPts = 2;
                        numAdjYPts = 2;
                        numAdjZPts = 1;
                        numDiagXYPts = 4;
                        numDiagXZPts = 2;
                        numDiagYZPts = 2;
                        numCornerPts = 4;
                    }

                    // normalize the gaussian so that the sum of the neighbors' coeff and the point coeff is 1
                    totalGaussian = gaussian[0] + (gaussian[1] * numAdjXPts) + (gaussian[2] * numAdjYPts) +
                                    (gaussian[3] * numAdjZPts) + (gaussian[4] * numDiagXYPts) +
                                    (gaussian[5] * numDiagXZPts) + (gaussian[6] * numDiagYZPts) +
                                    (gaussian[7] * numCornerPts);

                    double ptCoeff = gaussian[0] / totalGaussian;
                    double adjXCoeff = gaussian[1] / totalGaussian;
                    double adjYCoeff = gaussian[2] / totalGaussian;
                    double adjZCoeff = gaussian[3] / totalGaussian;
                    double diagXYCoeff = gaussian[4] / totalGaussian;
                    double diagXZCoeff = gaussian[5] / totalGaussian;
                    double diagYZCoeff = gaussian[6] / totalGaussian;
                    double cornerCoeff = gaussian[7] / totalGaussian;

                    int numChannels = 1; // grayscale image only has one channel
                    int colorOffset = 1; // no color offset for grayscale images

                    if (srcImage.isColorImage()) {
                        numChannels = 4;
                        colorOffset = 4;
                    }

                    for (int c = 0; c < numChannels; c++) {
                        double adjXVal = 0, adjYVal = 0, adjZVal = 0, diagXYVal = 0, diagXZVal = 0, diagYZVal = 0,
                               cornerVal = 0;

                        // add points only if they exist (ie they aren't past a boundary of the image that we're up
                        // against)
                        if (nextSlice != -1) {
                            adjZVal += getValue3D(fx, fy, fz + 1, c, src);

                            if (nextRow != -1) {
                                diagYZVal += getValue3D(fx, fy + 1, fz + 1, c, src);

                                if (nextCol != -1) {
                                    cornerVal += getValue3D(fx + 1, fy + 1, fz + 1, c, src);
                                }

                                if (previousCol != -1) {
                                    cornerVal += getValue3D(fx - 1, fy + 1, fz + 1, c, src);
                                }
                            }

                            if (previousRow != -1) {
                                diagYZVal += getValue3D(fx, fy - 1, fz + 1, c, src);

                                if (nextCol != -1) {
                                    cornerVal += getValue3D(fx + 1, fy - 1, fz + 1, c, src);
                                }

                                if (previousCol != -1) {
                                    cornerVal += getValue3D(fx - 1, fy - 1, fz + 1, c, src);
                                }
                            }

                            if (nextCol != -1) {
                                diagXZVal += getValue3D(fx + 1, fy, fz + 1, c, src);
                            }

                            if (previousCol != -1) {
                                diagXZVal += getValue3D(fx - 1, fy, fz + 1, c, src);
                            }
                        }

                        if (previousSlice != -1) {
                            adjZVal += getValue3D(fx, fy, fz - 1, c, src);

                            if (nextRow != -1) {
                                diagYZVal += getValue3D(fx, fy + 1, fz - 1, c, src);

                                if (nextCol != -1) {
                                    cornerVal += getValue3D(fx + 1, fy + 1, fz - 1, c, src);
                                }

                                if (previousCol != -1) {
                                    cornerVal += getValue3D(fx - 1, fy + 1, fz - 1, c, src);
                                }
                            }

                            if (previousRow != -1) {
                                diagYZVal += getValue3D(fx, fy - 1, fz - 1, c, src);

                                if (nextCol != -1) {
                                    cornerVal += getValue3D(fx + 1, fy - 1, fz - 1, c, src);
                                }

                                if (previousCol != -1) {
                                    cornerVal += getValue3D(fx - 1, fy - 1, fz - 1, c, src);
                                }
                            }

                            if (nextCol != -1) {
                                diagXZVal += getValue3D(fx + 1, fy, fz - 1, c, src);
                            }

                            if (previousCol != -1) {
                                diagXZVal += getValue3D(fx - 1, fy, fz - 1, c, src);
                            }
                        }

                        if (nextRow != -1) {
                            adjYVal += getValue3D(fx, fy + 1, fz, c, src);

                            if (nextCol != -1) {
                                diagXYVal += getValue3D(fx + 1, fy + 1, fz, c, src);
                            }

                            if (previousCol != -1) {
                                diagXYVal += getValue3D(fx - 1, fy + 1, fz, c, src);
                            }
                        }

                        if (previousRow != -1) {
                            adjYVal += getValue3D(fx, fy - 1, fz, c, src);

                            if (nextCol != -1) {
                                diagXYVal += getValue3D(fx + 1, fy - 1, fz, c, src);
                            }

                            if (previousCol != -1) {
                                diagXYVal += getValue3D(fx - 1, fy - 1, fz, c, src);
                            }
                        }

                        if (nextCol != -1) {
                            adjXVal += getValue3D(fx + 1, fy, fz, c, src);
                        }

                        if (previousCol != -1) {
                            adjXVal += getValue3D(fx - 1, fy, fz, c, src);
                        }

                        result.data[(colorOffset * ((z * sliceSize) + (y * rowSize) + x)) + c] = (float) ((ptCoeff *
                                                                                                               (getValue3D(fx,
                                                                                                                               fy,
                                                                                                                               fz,
                                                                                                                               c,
                                                                                                                               src))) +
                                                                                                          (adjXCoeff *
                                                                                                               (adjXVal)) +
                                                                                                          (adjYCoeff *
                                                                                                               (adjYVal)) +
                                                                                                          (adjZCoeff *
                                                                                                               (adjZVal)) +
                                                                                                          (diagXYCoeff *
                                                                                                               (diagXYVal)) +
                                                                                                          (diagXZCoeff *
                                                                                                               (diagXZVal)) +
                                                                                                          (diagYZCoeff *
                                                                                                               (diagYZVal)) +
                                                                                                          (cornerCoeff *
                                                                                                               (cornerVal)));
                    } // for (c = 0; c <= numChannels; c++)
                }
            }

            percent += (int) inc;

            fireProgressStateChanged(percent);

            remainder += inc - (int) inc;

            if (remainder >= 1) {
                percent += (int) remainder;
                remainder = remainder - (int) remainder;

                fireProgressStateChanged(percent);
            }

        }

        return result;
    }

    /**
     * Perform the subsampling. Create a result image with the extents given into the constuctor. Then, for each point
     * in the result image, find point which corresponds to it in the source image. Add up weighted intensities of the
     * neighbors of this point in the source image and the intensity of the point itself. Store this value in the point
     * of the result image. Once all points in the result image have been visted, return the result image. Does not
     * subsample in fourth time dimension - 3.5D operation
     *
     * @return  resampled image
     */
    private ModelSimpleImage subsample4D() {

        // convert the source image to make it easier to work with
        ModelSimpleImage src = new ModelSimpleImage(srcImage.getExtents(), srcImage.getFileInfo(0).getResolutions(),
                                                    srcImage);
        
        float[] resolutions = new float[srcImage.getNDims()];

        for (int i = 0; i < resolutions.length; i++) {
            resolutions[i] = src.resolutions[i] * (float) posRatios[i];
        }

        ModelSimpleImage result = new ModelSimpleImage(resultExtents, resolutions, srcImage.isColorImage());

        int sliceSize = result.xDim * result.yDim;
        int rowSize = result.xDim;
        int volSize = sliceSize * result.zDim;
        int slice = src.xDim * src.yDim;
        int row = src.xDim;

        int nextRow, nextCol, nextSlice;
        int previousRow, previousCol, previousSlice;

        double[] gaussian = new double[8];

        gaussian[0] = this.computeGaussian3D(0, 0, 0, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[1] = this.computeGaussian3D(1, 0, 0, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[2] = this.computeGaussian3D(0, 1, 0, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[3] = this.computeGaussian3D(0, 0, 1, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[4] = this.computeGaussian3D(1, 1, 0, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[5] = this.computeGaussian3D(1, 0, 1, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[6] = this.computeGaussian3D(0, 1, 1, sigmas[0], sigmas[1], sigmas[2]);
        gaussian[7] = this.computeGaussian3D(1, 1, 1, sigmas[0], sigmas[1], sigmas[2]);

        double totalGaussian = 0;

        // numbers if not near any edges
        int numAdjXPts = 2, numAdjYPts = 2, numAdjZPts = 2, numDiagXYPts = 4, numDiagXZPts = 4, numDiagYZPts = 4,
            numCornerPts = 8;

        int numXEdges, numYEdges, numZEdges;

        int percent = 5;
        float remainder = 0;
        float inc = (90.0f / (result.tDim * result.zDim));

        fireProgressStateChanged(percent);

        for (int t = 0; t < result.tDim; t++) {
            fireProgressStateChanged("Subsampling time " + (t + 1) + "..");

            for (int z = 0; z < result.zDim; z++) {

                for (int y = 0; y < result.yDim; y++) {

                    for (int x = 0; x < result.xDim; x++) {

                        // translate point back to source image
                        float[] point = getNearestNeighborPos3D(x, y, z);
                        float fx, fy, fz;
                        int ix, iy, iz;

                        // need float indexes for trilinear point retrieval
                        if (trilinearFlag) {
                            fx = point[0];
                            fy = point[1];
                            fz = point[2];
                            ix = (int) point[0];
                            iy = (int) point[1];
                            iz = (int) point[2];
                        } // if not using trilinear point retrieval, just get the nearest integer points
                        else {
                            fx = ix = (int) point[0];
                            fy = iy = (int) point[1];
                            fz = iz = (int) point[2];
                        }

                        // first slice
                        if (iz == 0) {
                            previousSlice = -1;
                            nextSlice = (iz + 1) * slice;
                        } // last slice
                        else if (iz >= (src.zDim - 1)) {
                            previousSlice = (iz - 1) * slice;
                            nextSlice = -1;
                        } // any other slice
                        else {
                            previousSlice = (iz - 1) * slice;
                            nextSlice = (iz + 1) * slice;
                        }

                        // first row of slice
                        if (iy == 0) {
                            previousRow = -1;
                            nextRow = (iy + 1) * row;
                        } // last row of slice
                        else if (iy >= (src.yDim - 1)) {
                            previousRow = (iy - 1) * row;
                            nextRow = -1;
                        } // inside row of slice
                        else {
                            previousRow = (iy - 1) * row;
                            nextRow = (iy + 1) * row;
                        }

                        // first column of slice
                        if (ix == 0) {
                            previousCol = -1;
                            nextCol = (ix + 1);
                        } // last column of slice
                        else if (ix >= (src.xDim - 1)) {
                            previousCol = (ix - 1);
                            nextCol = -1;
                        } // inside column of slice
                        else {
                            previousCol = (ix - 1);
                            nextCol = (ix + 1);
                        }

                        numXEdges = 0;
                        numYEdges = 0;
                        numZEdges = 0;

                        // count how many of the edges of the image we're on
                        if ((previousSlice == -1) || (nextSlice == -1)) {
                            numZEdges = 1;
                        }

                        if ((previousRow == -1) || (nextRow == -1)) {
                            numYEdges = 1;
                        }

                        if ((previousCol == -1) || (nextCol == -1)) {
                            numXEdges = 1;
                        }

                        numAdjXPts = 2;
                        numAdjYPts = 2;
                        numAdjZPts = 2;
                        numDiagXYPts = 4;
                        numDiagXZPts = 4;
                        numDiagYZPts = 4;
                        numCornerPts = 8;

                        if ((numXEdges == 1) && (numYEdges == 1) && (numZEdges == 1)) {
                            numAdjXPts = 1;
                            numAdjYPts = 1;
                            numAdjZPts = 1;
                            numDiagXYPts = 1;
                            numDiagXZPts = 1;
                            numDiagYZPts = 1;
                            numCornerPts = 1;
                        } else if ((numXEdges == 1) && (numYEdges == 1)) {
                            numAdjXPts = 1;
                            numAdjYPts = 1;
                            numAdjZPts = 2;
                            numDiagXYPts = 1;
                            numDiagXZPts = 2;
                            numDiagYZPts = 2;
                            numCornerPts = 2;
                        } else if ((numXEdges == 1) && (numZEdges == 1)) {
                            numAdjXPts = 1;
                            numAdjYPts = 2;
                            numAdjZPts = 1;
                            numDiagXYPts = 2;
                            numDiagXZPts = 1;
                            numDiagYZPts = 2;
                            numCornerPts = 2;
                        } else if ((numYEdges == 1) && (numZEdges == 1)) {
                            numAdjXPts = 2;
                            numAdjYPts = 1;
                            numAdjZPts = 1;
                            numDiagXYPts = 2;
                            numDiagXZPts = 2;
                            numDiagYZPts = 1;
                            numCornerPts = 2;
                        } else if (numXEdges == 1) {
                            numAdjXPts = 1;
                            numAdjYPts = 2;
                            numAdjZPts = 2;
                            numDiagXYPts = 2;
                            numDiagXZPts = 2;
                            numDiagYZPts = 4;
                            numCornerPts = 4;
                        } else if (numYEdges == 1) {
                            numAdjXPts = 2;
                            numAdjYPts = 1;
                            numAdjZPts = 2;
                            numDiagXYPts = 2;
                            numDiagXZPts = 4;
                            numDiagYZPts = 2;
                            numCornerPts = 4;
                        } else if (numZEdges == 1) {
                            numAdjXPts = 2;
                            numAdjYPts = 2;
                            numAdjZPts = 1;
                            numDiagXYPts = 4;
                            numDiagXZPts = 2;
                            numDiagYZPts = 2;
                            numCornerPts = 4;
                        }

                        // normalize the gaussian so that the sum of the neighbors' coeff and the point coeff is 1
                        totalGaussian = gaussian[0] + (gaussian[1] * numAdjXPts) + (gaussian[2] * numAdjYPts) +
                                        (gaussian[3] * numAdjZPts) + (gaussian[4] * numDiagXYPts) +
                                        (gaussian[5] * numDiagXZPts) + (gaussian[6] * numDiagYZPts) +
                                        (gaussian[7] * numCornerPts);

                        double ptCoeff = gaussian[0] / totalGaussian;
                        double adjXCoeff = gaussian[1] / totalGaussian;
                        double adjYCoeff = gaussian[2] / totalGaussian;
                        double adjZCoeff = gaussian[3] / totalGaussian;
                        double diagXYCoeff = gaussian[4] / totalGaussian;
                        double diagXZCoeff = gaussian[5] / totalGaussian;
                        double diagYZCoeff = gaussian[6] / totalGaussian;
                        double cornerCoeff = gaussian[7] / totalGaussian;

                        int numChannels = 1; // grayscale image only has one channel
                        int colorOffset = 1; // no color offset for grayscale images

                        if (srcImage.isColorImage()) {
                            numChannels = 4;
                            colorOffset = 4;
                        }

                        for (int c = 0; c < numChannels; c++) {
                            double adjXVal = 0, adjYVal = 0, adjZVal = 0, diagXYVal = 0, diagXZVal = 0, diagYZVal = 0,
                                   cornerVal = 0;

                            // add points only if they exist (ie they aren't past a boundary of the image that we're up
                            // against)
                            if (nextSlice != -1) {
                                adjZVal += getValue3D(fx, fy, fz + 1, t, c, src);

                                if (nextRow != -1) {
                                    diagYZVal += getValue3D(fx, fy + 1, fz + 1, t, c, src);

                                    if (nextCol != -1) {
                                        cornerVal += getValue3D(fx + 1, fy + 1, fz + 1, t, c, src);
                                    }

                                    if (previousCol != -1) {
                                        cornerVal += getValue3D(fx - 1, fy + 1, fz + 1, t, c, src);
                                    }
                                }

                                if (previousRow != -1) {
                                    diagYZVal += getValue3D(fx, fy - 1, fz + 1, t, c, src);

                                    if (nextCol != -1) {
                                        cornerVal += getValue3D(fx + 1, fy - 1, fz + 1, t, c, src);
                                    }

                                    if (previousCol != -1) {
                                        cornerVal += getValue3D(fx - 1, fy - 1, fz + 1, t, c, src);
                                    }
                                }

                                if (nextCol != -1) {
                                    diagXZVal += getValue3D(fx + 1, fy, fz + 1, t, c, src);
                                }

                                if (previousCol != -1) {
                                    diagXZVal += getValue3D(fx - 1, fy, fz + 1, t, c, src);
                                }
                            }

                            if (previousSlice != -1) {
                                adjZVal += getValue3D(fx, fy, fz - 1, t, c, src);

                                if (nextRow != -1) {
                                    diagYZVal += getValue3D(fx, fy + 1, fz - 1, t, c, src);

                                    if (nextCol != -1) {
                                        cornerVal += getValue3D(fx + 1, fy + 1, fz - 1, t, c, src);
                                    }

                                    if (previousCol != -1) {
                                        cornerVal += getValue3D(fx - 1, fy + 1, fz - 1, t, c, src);
                                    }
                                }

                                if (previousRow != -1) {
                                    diagYZVal += getValue3D(fx, fy - 1, fz - 1, t, c, src);

                                    if (nextCol != -1) {
                                        cornerVal += getValue3D(fx + 1, fy - 1, fz - 1, t, c, src);
                                    }

                                    if (previousCol != -1) {
                                        cornerVal += getValue3D(fx - 1, fy - 1, fz - 1, t, c, src);
                                    }
                                }

                                if (nextCol != -1) {
                                    diagXZVal += getValue3D(fx + 1, fy, fz - 1, t, c, src);
                                }

                                if (previousCol != -1) {
                                    diagXZVal += getValue3D(fx - 1, fy, fz - 1, t, c, src);
                                }
                            }

                            if (nextRow != -1) {
                                adjYVal += getValue3D(fx, fy + 1, fz, t, c, src);

                                if (nextCol != -1) {
                                    diagXYVal += getValue3D(fx + 1, fy + 1, fz, t, c, src);
                                }

                                if (previousCol != -1) {
                                    diagXYVal += getValue3D(fx - 1, fy + 1, fz, t, c, src);
                                }
                            }

                            if (previousRow != -1) {
                                adjYVal += getValue3D(fx, fy - 1, fz, t, c, src);

                                if (nextCol != -1) {
                                    diagXYVal += getValue3D(fx + 1, fy - 1, fz, t, c, src);
                                }

                                if (previousCol != -1) {
                                    diagXYVal += getValue3D(fx - 1, fy - 1, fz, t, c, src);
                                }
                            }

                            if (nextCol != -1) {
                                adjXVal += getValue3D(fx + 1, fy, fz, t, c, src);
                            }

                            if (previousCol != -1) {
                                adjXVal += getValue3D(fx - 1, fy, fz, t, c, src);
                            }

                            result.data[(colorOffset * ((t * volSize) + (z * sliceSize) + (y * rowSize) + x)) + c] = (float)
                                                                                                                         ((ptCoeff *
                                                                                                                               (getValue3D(fx,
                                                                                                                                               fy,
                                                                                                                                               fz,
                                                                                                                                               t,
                                                                                                                                               c,
                                                                                                                                               src))) +
                                                                                                                          (adjXCoeff *
                                                                                                                               (adjXVal)) +
                                                                                                                          (adjYCoeff *
                                                                                                                               (adjYVal)) +
                                                                                                                          (adjZCoeff *
                                                                                                                               (adjZVal)) +
                                                                                                                          (diagXYCoeff *
                                                                                                                               (diagXYVal)) +
                                                                                                                          (diagXZCoeff *
                                                                                                                               (diagXZVal)) +
                                                                                                                          (diagYZCoeff *
                                                                                                                               (diagYZVal)) +
                                                                                                                          (cornerCoeff *
                                                                                                                                 (cornerVal)));
                        } // for (c = 0; c <= numChannels; c++)
                    } // for (int x = 0; x < result.xDim; x++)
                } // for (int y = 0; y < result.yDim; y++)

                percent += (int) inc;

                fireProgressStateChanged(percent);

                remainder += inc - (int) inc;

                if (remainder >= 1) {
                    percent += (int) remainder;
                    remainder = remainder - (int) remainder;

                    fireProgressStateChanged(percent);
                } // if (remainder >= 1)

            } // for (int z = 0; z < result.zDim; z++)
        } // for (int t = 0; t < result.tDim; t++)

        return result;
    }

    /**
     * Transforms and resamples a 2D or 25D VOI using nearest neighbor interpolation.
     *
     * <ol>
     *   <li>Export VOIs as a mask image</li>
     *   <li>Transform mask</li>
     *   <li>Extract VOI contours from mask image and put in new image.</li>
     * </ol>
     *
     * @param  image      Image where VOIs are stored
     * @param  imgBuffer  Image array
     * @param  xfrm       Transformation matrix to be applied
     */
    private void transform25DVOI(ModelImage image, float[] imgBuffer, TransMatrix xfrm) {

        int i, j, z;
        int X0pos, Y0pos, Z0pos;
        float X, Y;
        float temp1, temp2;
        float value;
        float imm, jmm;
        int roundX, roundY;
        int iXdim = srcImage.getExtents()[0];
        int iYdim = srcImage.getExtents()[1];
        int iSliceSize = iXdim * iYdim;
        int iZdim = 1;
        if (srcImage.getNDims() > 2) {
            iZdim = srcImage.getExtents()[2];
        }
        float iXres = srcImage.getFileInfo()[0].getResolution(0);
        float iYres = srcImage.getFileInfo()[0].getResolution(1);
        float oXres = resultImage.getFileInfo()[0].getResolution(0);
        float oYres = resultImage.getFileInfo()[0].getResolution(1);

        float T00, T01, T02, T10, T11, T12;
        ModelImage tmpMask;
        ModelImage maskImage;

        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);

        maskImage = image.generateShortImage(1);
        tmpMask = new ModelImage(ModelImage.SHORT, resultExtents, null);

        try {
            maskImage.exportData(0, srcImage.getExtents()[0] * srcImage.getExtents()[1] * iZdim, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm VOI transform: Image(s) locked");
            setCompleted(false);

            return;
        }

        for (z = 0; (z < iZdim) && !threadStopped; z++) {
            Z0pos = z * iSliceSize;
            for (i = 0; (i < resultExtents[0]) && !threadStopped; i++) {
                imm = (float) i * oXres;
                temp1 = (imm * T00) + T02;
                temp2 = (imm * T10) + T12;
    
                for (j = 0; (j < resultExtents[1]) && !threadStopped; j++) {
    
                    // transform i,j
                    value = 0.0f; // remains zero if voxel is transformed out of bounds
                    jmm = (float) j * oYres;
                    X = (temp1 + (jmm * T01)) / iXres;
                    roundX = (int) (X + 0.5f);
    
                    if ((X >= -0.5f) && (roundX < iXdim)) {
                        Y = (temp2 + (jmm * T11)) / iYres;
                        roundY = (int) (Y + 0.5f);
    
                        if ((Y >= -0.5f) && (roundY < iYdim)) {
                            X0pos = roundX;
                            Y0pos = roundY * iXdim;
                            value = imgBuffer[Z0pos + Y0pos + X0pos];
                        } // end if Y in bounds
                    } // end if X in bounds
    
                    tmpMask.set(i, j, z, value);
                } // end for j
            } // end for i
        } // for (z = 0; (z < iZdim) && !threadStopped; z++)

        if (threadStopped) {
            return;
        }

        // ******* Make algorithm for VOI extraction.
        tmpMask.calcMinMax();

        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
        VOIExtAlgo.run();
        resultImage.setVOIs(tmpMask.getVOIs());
        tmpMask.disposeLocal();
        maskImage.disposeLocal();

    }

    /**
     * Transforms and resamples a 3D VOI using nearest neighbor interpolation.
     *
     * <ol>
     *   <li>Export VOIs as a mask image</li>
     *   <li>Transform mask</li>
     *   <li>Extract VOI contours from mask image and put in new image.</li>
     * </ol>
     *
     * @param  image      Image where VOIs are stored
     * @param  imgBuffer  Image array
     * @param  xfrm       Transformation matrix to be applied
     */
    private void transform3DVOI(ModelImage image, float[] imgBuffer, TransMatrix xfrm) {

        int i, j, k;
        int X0pos, Y0pos, Z0pos;
        float X, Y, Z;
        float value;
        int sliceSize;
        float imm, jmm, kmm;
        float k1, k2, k3, j1, j2, j3;
        int iXdim = srcImage.getExtents()[0];
        int iYdim = srcImage.getExtents()[1];
        int iZdim = srcImage.getExtents()[2];
        float iXres = srcImage.getFileInfo()[0].getResolution(0);
        float iYres = srcImage.getFileInfo()[0].getResolution(1);
        float iZres = srcImage.getFileInfo()[0].getResolution(2);
        float oXres = resultImage.getFileInfo()[0].getResolution(0);
        float oYres = resultImage.getFileInfo()[0].getResolution(1);
        float oZres = resultImage.getFileInfo()[0].getResolution(2);


        sliceSize = iXdim * iYdim;

        float T00, T01, T02, T03, T10, T11, T12, T13, T20, T21, T22, T23;
        ModelImage tmpMask;
        ModelImage maskImage;

        T00 = xfrm.get(0, 0);
        T01 = xfrm.get(0, 1);
        T02 = xfrm.get(0, 2);
        T03 = xfrm.get(0, 3);
        T10 = xfrm.get(1, 0);
        T11 = xfrm.get(1, 1);
        T12 = xfrm.get(1, 2);
        T13 = xfrm.get(1, 3);
        T20 = xfrm.get(2, 0);
        T21 = xfrm.get(2, 1);
        T22 = xfrm.get(2, 2);
        T23 = xfrm.get(2, 3);

        
        maskImage = image.generateShortImage(1);
        tmpMask = new ModelImage(ModelImage.SHORT, resultExtents, "VOI Mask");

        
        try {
            maskImage.exportData(0, iXdim * iYdim * iZdim, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            displayError("Algorithm VOI transform: Image(s) locked");
            setCompleted(false);

            return;
        }

        float invXRes = 1 / iXres;
        float invYRes = 1 / iYres;
        float invZRes = 1 / iZres;

        for (k = 0; (k < resultExtents[2]) && !threadStopped; k++) {
            kmm = k * oZres;
            k1 = (kmm * T02) + T03;
            k2 = (kmm * T12) + T13;
            k3 = (kmm * T22) + T23;

            for (j = 0; (j < resultExtents[1]) && !threadStopped; j++) {
                jmm = j * oYres;
                j1 = (jmm * T01) + k1;
                j2 = (jmm * T11) + k2;
                j3 = (jmm * T21) + k3;

                for (i = 0; (i < resultExtents[0]) && !threadStopped; i++) {

                    // transform i,j,k
                    value = 0.0f; // remains zero if voxel is transformed out of bounds
                    imm = i * oXres;
                    X = (j1 + (imm * T00)) * invXRes;

                    if ((X >= -0.5) && (X < (iXdim - 0.5f))) {
                        Y = (j2 + (imm * T10)) * invYRes;

                        if ((Y >= -0.5) && (Y < (iYdim - 0.5f))) {
                            Z = (j3 + (imm * T20)) * invZRes;

                            if ((Z >= -0.5) && (Z < (iZdim - 0.5f))) {
                                X0pos = (int) (X + 0.5f);
                                Y0pos = ((int) (Y + 0.5f)) * iXdim;
                                Z0pos = ((int) (Z + 0.5f)) * sliceSize;
                                value = imgBuffer[Z0pos + Y0pos + X0pos];
                            } // end if Z in bounds
                        } // end if Y in bounds
                    } // end if X in bounds

                    tmpMask.set(i, j, k, value);
                } // end for k
            } // end for j
        } // end for i

        if (threadStopped) {
            return;
        }

        // ******* Make algorithm for VOI extraction.
        tmpMask.calcMinMax();

        AlgorithmVOIExtraction VOIExtAlgo = new AlgorithmVOIExtraction(tmpMask);
        VOIExtAlgo.setRunningInSeparateThread(runningInSeparateThread);
        VOIExtAlgo.run();

        VOIVector resultVOIs = tmpMask.getVOIs();
        VOIVector srcVOIs = image.getVOIs();

        for (int ii = 0; ii < resultVOIs.size(); ii++) {
            int id = ((VOI) (resultVOIs.elementAt(ii))).getID();

            for (int jj = 0; jj < srcVOIs.size(); jj++) {

                if (((VOI) (srcVOIs.elementAt(jj))).getID() == id) {
                    ((VOI) (resultVOIs.elementAt(ii))).setName(((VOI) (srcVOIs.elementAt(jj))).getName());
                }
            }
        }

        resultImage.setVOIs(tmpMask.getVOIs());
        tmpMask.disposeLocal();
        maskImage.disposeLocal();
    }
    
    private ModelImage padImage(ModelImage kImage, int[] padExtents) {
    	
    	paddedImage = new ModelImage(kImage.getType(), padExtents,
                makeImageName(kImage.getImageName(), "_pad"));
        
    	int[] extents = kImage.getExtents();
    	int[] x = new int[2];
    	int[] y = new int[2];
    	int[] z = new int[2];
        // Generate bounds for pad algorithm     
        x[0] = 0;
        x[1] = padExtents[0] - extents[0];      
        y[0] = 0;
        y[1] = padExtents[1] - extents[1];      
        z[0] = 0;
        if ( extents.length > 2 )
        	z[1] = padExtents[2] - extents[2];
        else
        	z[1] = 0;
        
        AlgorithmAddMargins algoPad = new AlgorithmAddMargins(kImage, paddedImage, x, y, z);   
        algoPad.setPadValue( new float[]{0,0,0} );
        algoPad.run();
        algoPad.finalize();        
        
        if (transformVOI) {
            VOIVector VOIs = null;
            VOIVector voiVector = null;
            VOIs = kImage.getVOIs(); 
            voiVector = new VOIVector();

            int nVOI = VOIs.size();

            for (int i = 0; i < nVOI; i++) {
                voiVector.add((VOI)VOIs.VOIAt(i).clone());
            }
            paddedImage.setVOIs(voiVector);
        }
    	return paddedImage;
                        	
    }
    
    /**
     * Helper method for making the result image's name. Strips the current extension from the original name, adds the
     * given extension, and returns the new name.
     *
     * @param   image_name  the original image's name
     * @param   ext         Extension to add which gives information about what algorithm was performed on the image.
     *
     * @return  The new image name.
     */
    private static String makeImageName(String image_name, String ext) {
        String name;
        int index = image_name.indexOf(".");

        if (index == -1) {
            name = image_name;
        } else {
            name = image_name.substring(0, index);
        } // Used for setting image name

        name += ext;

        return name;
    }

}
