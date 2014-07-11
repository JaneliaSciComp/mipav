package gov.nih.mipav.model.structures;


import gov.nih.mipav.view.*;

import java.io.*;


/**
 * This is very simple class to store a float type image of up to four dimensions. For speed all class variables are
 * public. This class is used extensively in Registration algorithms where we needs many copies of the image where the
 * data does not need to be visible to the UserInterface. With the data array as public speed is achieved but at a cost
 * that the buffer is always of the type float.
 */


public class ModelSimpleImage extends ModelSerialCloneable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7953112445452821048L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** X coordinate value of the center of mass for the image. */
    public float cMassX;

    /** Y coordinate value of the center of mass for the image. */
    public float cMassY;

    /** Z coordinate value of the center of mass for the image. */
    public float cMassZ;

    /** Data buffer that is used to store the image. */
    public float[] data;

    /** Size of the data buffer (number of pixels in image). */
    public int dataSize = 1;

    /** The dimensionality of the dataset. */
    public int[] extents;

    /** Indicates whether is image is a color image or not. */
    public boolean isColor = false;

    /** Maximum intensity values of image. */
    public float max, maxR, maxG, maxB;

    /** Minimum intensity values of image. */
    public float min, minR, minG, minB;

    /** Number of dimensions of the image. */
    public int nDims = 1;

    /** Voxel resolutions (typically x,y,z(slice spacing),t). */
    public float[] resolutions;

    /** Number of volumes in the dataset. Typically t represents time. */
    public int tDim;

    /** Resolution in the time dimension. */
    public float tRes = 1.0f;

    /** Number of pixels in x dimension. */
    public int xDim;

    /** X pixel resolution. */
    public float xRes = 1.0f;

    /** Number of pixels in y dimension. */
    public int yDim;

    /** Y pixel resolution. */
    public float yRes = 1.0f;

    /** Number of pixels in z dimension. */
    public int zDim;

    /** Z voxel resolution. */
    public float zRes = 1.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a class to hold the minimum information about an image - the extents, the resolutions, and optionall, the
     * image data.
     *
     * @param  image  ModelImage structure where the input image data is stored. In this particular constructor, this
     *                parameter must not be null as the extents and resolutions are derived from the input image.
     */
    public ModelSimpleImage(ModelImage image) {
        this(image.getExtents(), image.getFileInfo(0).getResolutions(), image);
    }

    /**
     * Creates a class to hold the minimum information about an image - the extents with the resolutions defaulted to 1.
     * A data array of float type will be allocated where the size is defined by the dimExtents[].
     *
     * @param  dimExtents  Extents of the image; must be non null.
     */
    public ModelSimpleImage(int[] dimExtents) {

        nDims = dimExtents.length;
        extents = (int[]) dimExtents.clone();
        resolutions = new float[nDims];

        for (int i = 0; i < this.nDims; i++) {
            dataSize *= dimExtents[i];
            resolutions[i] = 1.0f;

            if (i == 0) {
                xDim = dimExtents[i];
                xRes = resolutions[i];
            } else if (i == 1) {
                yDim = dimExtents[i];
                yRes = resolutions[i];
            } else if (i == 2) {
                zDim = dimExtents[i];
                zRes = resolutions[i];
            } else if (i == 3) {
                tDim = dimExtents[i];
                tRes = resolutions[i];
            }
        }

        try {
            data = new float[dataSize];
        } catch (OutOfMemoryError error) {
            data = null;
            System.gc();
            MipavUtil.displayError("ModelSimpleImage: Unable to allocate float data buffer");
            throw (error);
        }

    }

    /**
     * Creates a class to hold the minimum information about an image - the extents, the resolutions, and the image
     * data. A data array of float type will be allocated where the size is defined by the ModelImage. Used for a 4D
     * image to make a 3D ModelSimpleImage.
     *
     * @param  image      ModelImage Structure where the input image data is stored. In this particular call, this
     *                    parameter must not be null as the extents and resolutions are derived from the input image.
     * @param  timeSlice  Time slice to export.
     */
    public ModelSimpleImage(ModelImage image, int timeSlice) {
        this(image.getExtents(), image.getFileInfo(0).getResolutions(), image, timeSlice);
    }

    /**
     * Creates a class to hold the minimum information about an image - the extents and the resolutions. A data array of
     * float type will be allocated where the size is defined by the dimExtents[].
     *
     * @param  dimExtents  Extents of the image; must be non null.
     * @param  voxRes      Resolutions of the image; must be non null.
     */
    public ModelSimpleImage(int[] dimExtents, float[] voxRes) {

        nDims = dimExtents.length;
        extents = (int[]) dimExtents.clone();
        resolutions = (float[]) voxRes.clone();

        for (int i = 0; i < this.nDims; i++) {
            dataSize *= dimExtents[i];

            if (i == 0) {
                xDim = dimExtents[i];
                xRes = voxRes[i];
            } else if (i == 1) {
                yDim = dimExtents[i];
                yRes = voxRes[i];
            } else if (i == 2) {
                zDim = dimExtents[i];
                zRes = voxRes[i];
            } else if (i == 3) {
                tDim = dimExtents[i];
                tRes = voxRes[i];
            }
        }

        try {
            data = new float[dataSize];
        } catch (OutOfMemoryError error) {
            data = null;
            System.gc();
            MipavUtil.displayError("ModelSimpleImage: Unable to allocate float data buffer");
            throw (error);
        }

    }

    /**
     * Creates a class to hold the minimum information about an image - the extents, the resolutions, and optionally,
     * the image data. A data array of float type will be allocated where the size is defined by the dimExtents[].
     *
     * @param  dimExtents  Extents of the image; must be non null.
     * @param  voxRes      Resolutions of the image; must be non null.
     * @param  image       Structure where image data is stored; can be null. If it is null, the buffer associated with
     *                     this class will be empty. The ModelImage is not otherwise saved or referenced within this
     *                     class.
     */
    public ModelSimpleImage(int[] dimExtents, float[] voxRes, ModelImage image) {

        nDims = dimExtents.length;
        extents = (int[]) dimExtents.clone();
        resolutions = (float[]) voxRes.clone();

        for (int i = 0; i < this.nDims; i++) {
            dataSize *= dimExtents[i];

            if (i == 0) {
                xDim = dimExtents[i];
                xRes = voxRes[i];
            } else if (i == 1) {
                yDim = dimExtents[i];
                yRes = voxRes[i];
            } else if (i == 2) {
                zDim = dimExtents[i];
                zRes = voxRes[i];
            } else if (i == 3) {
                tDim = dimExtents[i];
                tRes = voxRes[i];
            }
        }

        if (image.isColorImage()) {
            dataSize *= 4;
            isColor = true;
        }

        try {
            data = new float[dataSize];
        } catch (OutOfMemoryError error) {
            data = null;
            System.gc();
            MipavUtil.displayError("ModelSimpleImage: Unable to allocate float data buffer");
            throw (error);
        }

        if (image != null) {

            try {
                image.exportData(0, dataSize, data);
                calcMinMax();
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to get data from " + image.getImageName());
            }
        }
    }

    /**
     * Creates a class to hold the minimum information about an image - the extents, the resolutions. A data array of
     * float type will be allocated where the size is defined by the dimExtents[].
     *
     * @param  dimExtents  Extents of the image; must be non null.
     * @param  voxRes      Resolutions of the image; must be non null.
     * @param  isColor     If true, indicates that this image is a color image (i.e. ARGB).
     */
    public ModelSimpleImage(int[] dimExtents, float[] voxRes, boolean isColor) {

        nDims = dimExtents.length;
        extents = (int[]) dimExtents.clone();
        resolutions = (float[]) voxRes.clone();

        for (int i = 0; i < this.nDims; i++) {
            dataSize *= dimExtents[i];

            if (i == 0) {
                xDim = dimExtents[i];
                xRes = voxRes[i];
            } else if (i == 1) {
                yDim = dimExtents[i];
                yRes = voxRes[i];
            } else if (i == 2) {
                zDim = dimExtents[i];
                zRes = voxRes[i];
            } else if (i == 3) {
                tDim = dimExtents[i];
                tRes = voxRes[i];
            }
        }

        if (isColor) {
            dataSize *= 4;
            this.isColor = true;
        }

        try {
            data = new float[dataSize];
        } catch (OutOfMemoryError error) {
            data = null;
            System.gc();
            MipavUtil.displayError("ModelSimpleImage: Unable to allocate float data buffer");
            throw (error);
        }

    }

    /**
     * Creates a class to hold the minimum information about an image - the extents, the resolutions, and the image
     * data. A data array of float type will be allocated where the size is defined by the dimExtents[]. Used for a 4D
     * image to make a 3D ModelSimpleImage.
     *
     * @param  dimExtents  Extents of the image; must be non null.
     * @param  voxRes      Resolutions of the image; must be non null.
     * @param  image       Structure where image data is stored; can be null. If it is null, the buffer associated with
     *                     this class will be empty. The ModelImage is not otherwise saved within this class.
     * @param  timeSlice   Time slice to export into the 3D data array.
     */
    public ModelSimpleImage(int[] dimExtents, float[] voxRes, ModelImage image, int timeSlice) {
        nDims = Math.min(3, dimExtents.length);
        extents = (int[]) dimExtents.clone();
        resolutions = (float[]) voxRes.clone();

        for (int i = 0; i < this.nDims; i++) {
            dataSize *= dimExtents[i];

            if (i == 0) {
                xDim = dimExtents[i];
                xRes = voxRes[i];
            } else if (i == 1) {
                yDim = dimExtents[i];
                yRes = voxRes[i];
            } else if (i == 2) {
                zDim = dimExtents[i];
                zRes = voxRes[i];
            } else if (i == 3) {
                tDim = dimExtents[i];
                tRes = voxRes[i];
            }
        }

        if (image.isColorImage()) {
            dataSize *= 4;
            isColor = true;
        }

        try {
            data = new float[dataSize];
        } catch (OutOfMemoryError error) {
            data = null;
            System.gc();
            MipavUtil.displayError("ModelSimpleImage: Unable to allocate float data buffer");
            throw (error);
        }

        if (image != null) {

            try {
                image.exportData(timeSlice * dataSize, dataSize, data);
                calcMinMax();
            } catch (IOException error) {
                MipavUtil.displayError("Error while trying to get data from " + image.getImageName());
            }
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the min and max values for the image array.
     */
    public void calcMinMax() {

        int i;
        float value;

        if (!isColor) {
            min = Float.POSITIVE_INFINITY;
            max = Float.NEGATIVE_INFINITY;

            for (i = 0; i < dataSize; i++) {
                value = data[i];

                if (value > max) {
                    max = value;
                }

                if (value < min) {
                    min = value;
                }
            }
        } // if (!isColor)
        else { // isColor
            minR = Float.POSITIVE_INFINITY;
            maxR = Float.NEGATIVE_INFINITY;
            minG = Float.POSITIVE_INFINITY;
            maxG = Float.NEGATIVE_INFINITY;
            minB = Float.POSITIVE_INFINITY;
            maxB = Float.NEGATIVE_INFINITY;

            for (i = 0; i < dataSize; i += 4) {
                value = data[i + 1];

                if (value > maxR) {
                    maxR = value;
                }

                if (value < minR) {
                    minR = value;
                }

                value = data[i + 2];

                if (value > maxG) {
                    maxG = value;
                }

                if (value < minG) {
                    minG = value;
                }

                value = data[i + 3];

                if (value > maxB) {
                    maxB = value;
                }

                if (value < minB) {
                    minB = value;
                }
            }
        } // else isColor
    }


    /**
     * Calculates the center of mass (gravity) of a 2D image using the intensity of each pixel as a weighting value.
     *
     * @param  voxelResol  if true multiply the center of mass by voxel resolutions.
     */
    public void calculateCenterOfMass2D(boolean voxelResol) {
        int x, y;

        float voxVal = 0.0f, total = 0.0f;

        cMassX = 0;
        cMassY = 0;

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                voxVal = data[(y * xDim) + x] - min;
                cMassX += voxVal * x;
                cMassY += voxVal * y;
                total += voxVal;
            }
        }

        if (Math.abs(total) < 1e-5) {
            total = 1.0f;
        }

        cMassX /= total;
        cMassY /= total;

        if (voxelResol == true) {
            cMassX *= xRes;
            cMassY *= yRes;
        }
    }

    /**
     * Calculates the center of mass (gravity) of a 3D image. In image space where the upper left hand corner of the
     * image is 0,0,0. The x axis goes left to right, y axis goes top to bottom and z axis goes into the screen. (i.e.
     * the right hand rule). The intensity of each voxel is used as a weighting value.
     *
     * @param  voxelResol  if true multiply the center of mass by voxel resolutions.
     */
    public void calculateCenterOfMass3D(boolean voxelResol) {
        int x, y, z;
        int sliceSize = xDim * yDim;

        float voxVal = 0.0f, total = 0.0f;

        cMassX = 0;
        cMassY = 0;
        cMassZ = 0;

        for (z = 0; z < zDim; z++) {

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    voxVal = data[(z * sliceSize) + (y * xDim) + x] - min;
                    cMassX += voxVal * x;
                    cMassY += voxVal * y;
                    cMassZ += voxVal * z;
                    total += voxVal;
                }
            }
        }

        if (Math.abs(total) < 1e-5) {
            total = 1.0f;
        }

        cMassX /= total;
        cMassY /= total;
        cMassZ /= total;

        if (voxelResol == true) {
            cMassX *= xRes;
            cMassY *= yRes;
            cMassZ *= zRes;
        }
    }

    /**
     * Create a new image with the same dimensions as this image which contains the intensity value from this image if
     * it is a color image or which contains the same values if the image is already an intensity image. Scale factors
     * are equally weighted for each of the RGB channels.
     *
     * @return  ModelSimpleImage New instance created with intensity values only.
     */
    public ModelSimpleImage createIntensityImage() {
        float fOneThird = 1.0f / 3.0f;

        return createIntensityImage(fOneThird, fOneThird, fOneThird);
    }

    /**
     * Create a new image with the same dimensions as this image which contains the intensity value from this image if
     * it is a color image or which contains the same values if the image is already an intensity image. Scale factors
     * are specified for how to compute the intensity image as a linear combination of the RGB channels.
     *
     * @param   fScaleR  Scale factor to apply to the red color channel.
     * @param   fScaleG  Scale factor to apply to the green color channel.
     * @param   fScaleB  Scale factor to apply to the blue color channel.
     *
     * @return  ModelSimpleImage New instance created with intensity values only.
     */
    public ModelSimpleImage createIntensityImage(float fScaleR, float fScaleG, float fScaleB) {

        ModelSimpleImage kIntensityImage = new ModelSimpleImage(this.extents, this.resolutions);

        // If not a color image, then just copy the intensity values.
        if (!this.isColor) {

            // for (int i = 0; i < kIntensityImage.data.length; i++) {
            // kIntensityImage.data[i] = this.data[i];
            // }
            System.arraycopy(this.data, 0, kIntensityImage.data, 0, kIntensityImage.data.length);
        }

        // If is a color image, then combine each RGBA (ignoring the A)
        // from 'this' image into the intensity value for the new image.
        else {

            for (int i = 0; i < kIntensityImage.data.length; i++) {
                int iColorIndex = 4 * i;
                kIntensityImage.data[i] = (this.data[iColorIndex + 0] * fScaleR) +
                                          (this.data[iColorIndex + 1] * fScaleG) +
                                          (this.data[iColorIndex + 2] * fScaleB);
            }
        }

        return kIntensityImage;
    }

    /**
     * Create a mapping of this image. The mapping is defined by a set of floating point coordinates for each sample in
     * the image. The coordinates are actually stored in separate images, one for each dimension. The number dimensions
     * and the resolutions of these coordinate images must be the same so that either one can be used to determine the
     * dimensions and resolutions of the new image where the mapped source image values will be stored using bilinear
     * interpolation.
     *
     * @param   kImageMapX  ModelSimpleImage Image contains the x coordinate for the mapping. Each value in the image
     *                      must be in the range [0,1].
     * @param   kImageMapY  ModelSimpleImage Image contains the y coordinate for the mapping. Each value in the image
     *                      must be in the range [0,1].
     *
     * @return  ModelSimpleImage A new instance created with the mapping of this image.
     */
    public ModelSimpleImage createMappedImage2d(ModelSimpleImage kImageMapX, ModelSimpleImage kImageMapY) {

        ModelSimpleImage kMappedImage = new ModelSimpleImage(kImageMapX.extents, kImageMapX.resolutions, this.isColor);

        int iNumSamplesTrgX = kMappedImage.extents[0];
        int iNumSamplesTrgY = kMappedImage.extents[1];
        int iNumSamplesSrcX = this.extents[0];
        int iNumSamplesSrcY = this.extents[1];
        int iLimitSrcX = iNumSamplesSrcX - 1;
        int iLimitSrcY = iNumSamplesSrcY - 1;
        int iNumChannelsSrc = this.isColor ? 4 : 1;

        for (int iY = 0; iY < iNumSamplesTrgY; iY++) {

            for (int iX = 0; iX < iNumSamplesTrgX; iX++) {

                int iIndexTrg = iX + (iY * iNumSamplesTrgX);

                float fX = iLimitSrcX * kImageMapX.data[iIndexTrg];
                float fY = iLimitSrcY * kImageMapY.data[iIndexTrg];
                int iX0 = (int) fX;
                int iY0 = (int) fY;

                if (iX0 >= iLimitSrcX) {
                    iX0 = iLimitSrcX - 1;
                    fX = (float) iLimitSrcX;
                }

                if (iY0 >= iLimitSrcY) {
                    iY0 = iLimitSrcY - 1;
                    fY = (float) iLimitSrcY;
                }

                float fX1 = fX - iX0;
                float fY1 = fY - iY0;
                float fX0 = 1.0f - fX1;
                float fY0 = 1.0f - fY1;

                int iX0Y0 = iX0 + (iY0 * iNumSamplesSrcX);
                int iX1Y0 = iX0Y0 + 1;
                int iX0Y1 = iX0Y0 + iNumSamplesSrcX;
                int iX1Y1 = iX0Y1 + 1;

                iX0Y0 *= iNumChannelsSrc;
                iX1Y0 *= iNumChannelsSrc;
                iX0Y1 *= iNumChannelsSrc;
                iX1Y1 *= iNumChannelsSrc;

                // interpolate the same for each channel
                for (int iC = 0; iC < iNumChannelsSrc; iC++) {

                    // interpolate across X
                    float fSrcY0 = (this.data[iC + iX0Y0] * fX0) + (this.data[iC + iX1Y0] * fX1);
                    float fSrcY1 = (this.data[iC + iX0Y1] * fX0) + (this.data[iC + iX1Y1] * fX1);

                    // interpolate across Y
                    float fSrcValue = (fSrcY0 * fY0) + (fSrcY1 * fY1);

                    kMappedImage.data[iC + (iNumChannelsSrc * iIndexTrg)] = fSrcValue;
                }
            }
        }

        return kMappedImage;
    }

    /**
     * Create a mapping of this image. The mapping is defined by a set of floating point coordinates for each sample in
     * the image. The coordinates are actually stored in separate images, one for each dimension. The number dimensions
     * and the resolutions of these coordinate images must be the same so that either one can be used to determine the
     * dimensions and resolutions of the new image where the mapped source image values will be stored using trilinear
     * interpolation.
     *
     * @param   kImageMapX  ModelSimpleImage Image contains the x coordinate for the mapping. Each value in the image
     *                      must be in the range [0,1].
     * @param   kImageMapY  ModelSimpleImage Image contains the y coordinate for the mapping. Each value in the image
     *                      must be in the range [0,1].
     * @param   kImageMapZ  ModelSimpleImage Image contains the z coordinate for the mapping. Each value in the image
     *                      must be in the range [0,1].
     *
     * @return  ModelSimpleImage A new instance created with the mapping of this image.
     */
    public ModelSimpleImage createMappedImage3d(ModelSimpleImage kImageMapX, ModelSimpleImage kImageMapY,
                                                ModelSimpleImage kImageMapZ) {

        ModelSimpleImage kMappedImage = new ModelSimpleImage(kImageMapX.extents, kImageMapX.resolutions, this.isColor);

        int iNumSamplesTrgX = kMappedImage.extents[0];
        int iNumSamplesTrgY = kMappedImage.extents[1];
        int iNumSamplesTrgZ = kMappedImage.extents[2];
        int iNumSamplesTrgXY = iNumSamplesTrgX * iNumSamplesTrgY;
        int iNumSamplesSrcX = this.extents[0];
        int iNumSamplesSrcY = this.extents[1];
        int iNumSamplesSrcZ = this.extents[2];
        int iNumSamplesSrcXY = iNumSamplesSrcX * iNumSamplesSrcY;
        int iLimitSrcX = iNumSamplesSrcX - 1;
        int iLimitSrcY = iNumSamplesSrcY - 1;
        int iLimitSrcZ = iNumSamplesSrcZ - 1;
        int iNumChannelsSrc = this.isColor ? 4 : 1;

        for (int iZ = 0; iZ < iNumSamplesTrgZ; iZ++) {

            for (int iY = 0; iY < iNumSamplesTrgY; iY++) {

                for (int iX = 0; iX < iNumSamplesTrgX; iX++) {

                    int iIndexTrg = iX + (iY * iNumSamplesTrgX) + (iZ * iNumSamplesTrgXY);

                    float fX = iLimitSrcX * kImageMapX.data[iIndexTrg];
                    float fY = iLimitSrcY * kImageMapY.data[iIndexTrg];
                    float fZ = iLimitSrcZ * kImageMapZ.data[iIndexTrg];
                    int iX0 = (int) fX;
                    int iY0 = (int) fY;
                    int iZ0 = (int) fZ;

                    if (iX0 >= iLimitSrcX) {
                        iX0 = iLimitSrcX - 1;
                        fX = (float) iLimitSrcX;
                    }

                    if (iY0 >= iLimitSrcY) {
                        iY0 = iLimitSrcY - 1;
                        fY = (float) iLimitSrcY;
                    }

                    if (iZ0 >= iLimitSrcZ) {
                        iZ0 = iLimitSrcZ - 1;
                        fZ = (float) iLimitSrcZ;
                    }

                    float fX1 = fX - iX0;
                    float fY1 = fY - iY0;
                    float fZ1 = fZ - iZ0;
                    float fX0 = 1.0f - fX1;
                    float fY0 = 1.0f - fY1;
                    float fZ0 = 1.0f - fZ1;

                    int iX0Y0Z0 = iX0 + (iNumSamplesSrcX * iY0) + (iNumSamplesSrcXY * iZ0);
                    int iX0Y1Z0 = iX0Y0Z0 + iNumSamplesSrcX;
                    int iX1Y0Z0 = iX0Y0Z0 + 1;
                    int iX1Y1Z0 = iX0Y1Z0 + 1;
                    int iX0Y0Z1 = iX0Y0Z0 + iNumSamplesSrcXY;
                    int iX0Y1Z1 = iX0Y0Z1 + iNumSamplesSrcX;
                    int iX1Y0Z1 = iX0Y0Z1 + 1;
                    int iX1Y1Z1 = iX0Y1Z1 + 1;

                    iX0Y0Z0 *= iNumChannelsSrc;
                    iX0Y1Z0 *= iNumChannelsSrc;
                    iX1Y0Z0 *= iNumChannelsSrc;
                    iX1Y1Z0 *= iNumChannelsSrc;
                    iX0Y0Z1 *= iNumChannelsSrc;
                    iX0Y1Z1 *= iNumChannelsSrc;
                    iX1Y0Z1 *= iNumChannelsSrc;
                    iX1Y1Z1 *= iNumChannelsSrc;

                    // interpolate the same for each channel
                    for (int iC = 0; iC < iNumChannelsSrc; iC++) {

                        // interpolate across X
                        float fSrcY0Z0 = (this.data[iC + iX0Y0Z0] * fX0) + (this.data[iC + iX1Y0Z0] * fX1);
                        float fSrcY1Z0 = (this.data[iC + iX0Y1Z0] * fX0) + (this.data[iC + iX1Y1Z0] * fX1);
                        float fSrcY0Z1 = (this.data[iC + iX0Y0Z1] * fX0) + (this.data[iC + iX1Y0Z1] * fX1);
                        float fSrcY1Z1 = (this.data[iC + iX0Y1Z1] * fX0) + (this.data[iC + iX1Y1Z1] * fX1);

                        // interpolate across Y and the across Z
                        float fSrcZ0 = (fSrcY0Z0 * fY0) + (fSrcY1Z0 * fY1);
                        float fSrcZ1 = (fSrcY0Z1 * fY0) + (fSrcY1Z1 * fY1);
                        float fSrcValue = (fSrcZ0 * fZ0) + (fSrcZ1 * fZ1);

                        kMappedImage.data[iC + (iNumChannelsSrc * iIndexTrg)] = fSrcValue;
                    }
                }
            }
        }

        return kMappedImage;
    }

    /**
     * Nulls the data fields so that memory can be recovered.
     *
     * @param  gcFlag  if true the garbage collector is called.
     */
    public void disposeLocal(boolean gcFlag) {

        data = null;
        extents = null;
        resolutions = null;

        if (gcFlag == true) {
            System.gc();
        }
    }

    /**
     * Simple accessor to copy data from this class's data buffer.
     *
     * @param  buffer  destination buffer
     * @param  start   the starting pointer where data is to be copy from the source buffer
     * @param  end     the ending pointer where in the source buffer
     */
    public void exportData(float[] buffer, int start, int end) {
        System.arraycopy(data, start, buffer, 0, end - start);
    }

    /**
     * Get the value at the x, y position of the data array. 
     * @param x  input x pixel coordinate
     * @param y  input y pixel coordinate
     * @return   data value at the given position.
     */
    public final float getValue(int x, int y) {
    	return data[y * xDim + x];	
    }
    
    /**
     * Set the value at the x, y position of the data array. 
     * @param x  input x pixel coordinate. 
     * @param y  input y pixel coordinate
     * @param value  data value. 
     */
    public final void setValue(int x, int y, float value) {
    	data[y * xDim + x] = value;
    }
    
    /**
     * Version of get that performs bi-linear interpoloation. Note - does NOT perform bounds checking
     *
     * @param   x  input x coordinate
     * @param   y  input y coordinate
     *
     * @return  the bilinear interpolated value for x,y.
     */
    public final float getBiLinear(float x, float y) {

        int position;
        int intX, intY;
        float dx, dy;
        float x1, x2;

        intX = (int) x;
        intY = (int) y;

        dx = x - intX;
        dy = y - intY;
        position = (intY * xDim) + intX;

        // if ((position >= 0) && (position < size-xDim-1) ) {
        x1 = ((1 - dx) * data[position]) + (dx * data[position + 1]);
        x2 = ((1 - dx) * data[position + xDim]) + (dx * data[position + xDim + 1]);

        return (byte) (((1 - dy) * x1) + (dy * x2));
    }


    /**
     * Version of get that performs tri-linear interpoloation. <b>Note - does NOT perform bounds checking</b>
     *
     * @param   x  x coordinate
     * @param   y  y coordinate
     * @param   z  z coordinate
     *
     * @return  the trilinear interpolated value for x,y,z.
     */
    public final float getTriLinear(float x, float y, float z) {

        // if (nDims <= 2) return 0;
        int imageSize = xDim * yDim;
        int position1, position2;
        int intX, intY, intZ;
        float dx, dy, dz, dx1, dy1;
        float b1, b2;

        intX = (int) x;
        intY = (int) y;
        intZ = (int) z;

        dx = x - intX;
        dy = y - intY;
        dz = z - intZ;

        dx1 = 1 - dx;
        dy1 = 1 - dy;

        position1 = (intZ * imageSize) + (intY * xDim) + intX;
        position2 = position1 + imageSize;

        b1 = (dy1 * ((dx1 * data[position1]) + (dx * data[position1 + 1]))) +
             (dy * ((dx1 * data[position1 + xDim]) + (dx * data[position1 + xDim + 1])));

        b2 = (dy1 * ((dx1 * data[position2]) + (dx * data[position2 + 1]))) +
             (dy * ((dx1 * data[position2 + xDim]) + (dx * data[position2 + xDim + 1])));

        return (((1 - dz) * b1) + (dz * b2));
    }

    /**
     * Takes a simple image and subsamples it by 2. Linear interpolation is used.
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subSample2dBy2() {
        return subSample2dBy2(this.isColor);
    }

    /**
     * Takes a simple image and subsamples it by 2. Linear interpolation is used.
     *
     * @param   isColor  If true, indicates that this image is a color image (i.e. ARGB).
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subSample2dBy2(boolean isColor) {
        ModelSimpleImage srcImage = this;

        int[] extents = new int[srcImage.nDims];
        float[] resolutions = new float[srcImage.nDims];
        float[] distance = new float[srcImage.nDims];

        for (int i = 0; i < extents.length; i++) {
            distance[i] = srcImage.extents[i] * srcImage.resolutions[i];
            extents[i] = srcImage.extents[i] / 2;
            resolutions[i] = distance[i] / extents[i];
        }

        ModelSimpleImage resultImage = new ModelSimpleImage(extents, resolutions, isColor);

        return subSample2dBy2(resultImage, isColor);
    }

    /**
     * Takes a simple image and subsamples it by 2. Linear interpolation is used.
     *
     * @param   resultImage  The subsampled data will be stored in this object.
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subSample2dBy2(ModelSimpleImage resultImage) {
        return subSample2dBy2(resultImage, this.isColor);
    }

    /**
     * Takes a simple image and subsamples it by 2. Linear interpolation is used.
     *
     * @param   resultImage  The subsampled data will be stored in this object.
     * @param   isColor      If true, indicates that this image is a color image (i.e. ARGB).
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subSample2dBy2(ModelSimpleImage resultImage, boolean isColor) {
        ModelSimpleImage srcImage = this;

        int c;
        int rowSize = resultImage.xDim;
        int row = srcImage.xDim;
        int yStop, xStop;

        yStop = srcImage.yDim;
        xStop = srcImage.xDim;

        int nextRow, nextCol;

        for (int y = 0, by = 1; by < yStop; y++, by += 2) {

            for (int x = 0, bx = 1; bx < xStop; x++, bx += 2) {
                int currentRow = by * row;
                int previousRow = (by - 1) * row;

                if (by >= (srcImage.yDim - 1)) {
                    nextRow = (srcImage.yDim - 1) * row;
                } else {
                    nextRow = (by + 1) * row;
                }

                int currentCol = bx;
                int previousCol = (bx - 1);

                if (bx >= (srcImage.xDim - 1)) {
                    nextCol = srcImage.xDim - 1;
                } else {
                    nextCol = (bx + 1);
                }

                if (isColor) {

                    for (c = 0; c <= 3; c++) {
                        resultImage.data[(4 * ((y * rowSize) + x)) + c] = (float) ((0.208 *
                                                                                        srcImage.data[(4 *
                                                                                                           (currentRow +
                                                                                                                currentCol)) +
                                                                                                          c]) +
                                                                                   (0.122 *
                                                                                        (srcImage.data[(4 *
                                                                                                            (currentRow +
                                                                                                                 nextCol)) +
                                                                                                           c] +
                                                                                             srcImage.data[(4 *
                                                                                                                (currentRow +
                                                                                                                     previousCol)) +
                                                                                                               c] +
                                                                                             srcImage.data[(4 *
                                                                                                                (nextRow +
                                                                                                                     currentCol)) +
                                                                                                               c] +
                                                                                             srcImage.data[(4 *
                                                                                                                (previousRow +
                                                                                                                     currentCol)) +
                                                                                                               c])) +
                                                                                   (0.076 *
                                                                                        (srcImage.data[(4 *
                                                                                                            (nextRow +
                                                                                                                 nextCol)) +
                                                                                                           c] +
                                                                                             srcImage.data[(4 *
                                                                                                                (previousRow +
                                                                                                                     nextCol)) +
                                                                                                               c] +
                                                                                             srcImage.data[(4 *
                                                                                                                (nextRow +
                                                                                                                     previousCol)) +
                                                                                                               c] +
                                                                                             srcImage.data[(4 *
                                                                                                                (previousRow +
                                                                                                                     previousCol)) +
                                                                                                               c])));
                    } // for (c = 0; c <= 3; c++)
                } // if (isColor)
                else { // not color
                    resultImage.data[(y * rowSize) + x] = (float) ((0.208 * srcImage.data[currentRow + currentCol]) +
                                                                   (0.122 *
                                                                        (srcImage.data[currentRow + nextCol] +
                                                                             srcImage.data[currentRow + previousCol] +
                                                                             srcImage.data[nextRow + currentCol] +
                                                                             srcImage.data[previousRow + currentCol])) +
                                                                   (0.076 *
                                                                        (srcImage.data[nextRow + nextCol] +
                                                                             srcImage.data[previousRow + nextCol] +
                                                                             srcImage.data[nextRow + previousCol] +
                                                                             srcImage.data[previousRow + previousCol])));
                } // else not color
            }
        }

        resultImage.calcMinMax();

        return resultImage;
    }

    /**
     * Takes a simple image and subsamples it by 2. Linear interpolation is used.
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subsample3dBy2() {
        return subsample3dBy2(this.isColor);
    }

    /**
     * Takes a simple 3D image and subsamples it by 2. Linear interpolation is used.
     *
     * @param   isColor  If true, indicates that this image is a color image (i.e. ARGB).
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subsample3dBy2(boolean isColor) {
        ModelSimpleImage srcImage = this;
        int c;
        int[] extents = new int[srcImage.nDims];
        float[] resolutions = new float[srcImage.nDims];
        float[] distance = new float[srcImage.nDims];

        for (int i = 0; i < extents.length; i++) {
            distance[i] = srcImage.extents[i] * srcImage.resolutions[i];
            extents[i] = srcImage.extents[i] / 2;
            resolutions[i] = distance[i] / extents[i];
        }

        ModelSimpleImage resultImage = new ModelSimpleImage(extents, resolutions, isColor);

        int sliceSize = resultImage.xDim * resultImage.yDim;
        int rowSize = resultImage.xDim;
        int slice = srcImage.xDim * srcImage.yDim;
        int row = srcImage.xDim;
        int zStop, yStop, xStop;

        if (srcImage.nDims == 3) {
            zStop = srcImage.zDim;
        } else {
            zStop = 3;
        }

        yStop = srcImage.yDim;
        xStop = srcImage.xDim;

        int nextRow, nextCol, nextSlice;

        for (int z = 0, bz = 1; bz < zStop; z++, bz += 2) {

            for (int y = 0, by = 1; by < yStop; y++, by += 2) {

                for (int x = 0, bx = 1; bx < xStop; x++, bx += 2) {
                    int currentSlice = bz * slice;
                    int previousSlice = (bz - 1) * slice;

                    if (bz >= (srcImage.zDim - 1)) {
                        nextSlice = (srcImage.zDim - 1) * slice;
                    } else {
                        nextSlice = (bz + 1) * slice;
                    }

                    int currentRow = by * row;
                    int previousRow = (by - 1) * row;

                    if (by >= (srcImage.yDim - 1)) {
                        nextRow = (srcImage.yDim - 1) * row;
                    } else {
                        nextRow = (by + 1) * row;
                    }

                    int currentCol = bx;
                    int previousCol = (bx - 1);

                    if (bx >= (srcImage.xDim - 1)) {
                        nextCol = srcImage.xDim - 1;
                    } else {
                        nextCol = (bx + 1);
                    }

                    if (isColor) {

                        for (c = 0; c <= 3; c++) {
                            resultImage.data[(4 * ((z * sliceSize) + (y * rowSize) + x)) + c] = (float) ((0.0924 *
                                                                                                              (srcImage.data[(4 *
                                                                                                                                  (currentSlice +
                                                                                                                                       currentRow +
                                                                                                                                       currentCol)) +
                                                                                                                                 c])) +
                                                                                                         (0.0560 *
                                                                                                              (srcImage.data[(4 *
                                                                                                                                  (currentSlice +
                                                                                                                                       currentRow +
                                                                                                                                       nextCol)) +
                                                                                                                                 c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           currentRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           nextRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           previousRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           currentRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           currentRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c])) +
                                                                                                         (0.0339 *
                                                                                                              (srcImage.data[(4 *
                                                                                                                                  (currentSlice +
                                                                                                                                       nextRow +
                                                                                                                                       nextCol)) +
                                                                                                                                 c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           previousRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           nextRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (currentSlice +
                                                                                                                                           previousRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           currentRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           currentRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           currentRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           currentRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           nextRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           nextRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           previousRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           previousRow +
                                                                                                                                           currentCol)) +
                                                                                                                                     c])) +
                                                                                                         (0.0206 *
                                                                                                              (srcImage.data[(4 *
                                                                                                                                  (nextSlice +
                                                                                                                                       nextRow +
                                                                                                                                       nextCol)) +
                                                                                                                                 c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           nextRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           previousRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           previousRow +
                                                                                                                                           nextCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           nextRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           nextRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (nextSlice +
                                                                                                                                           previousRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c] +
                                                                                                                   srcImage.data[(4 *
                                                                                                                                      (previousSlice +
                                                                                                                                           previousRow +
                                                                                                                                           previousCol)) +
                                                                                                                                     c])));
                        } // for (c = 0; c <= 3; c++)
                    } // if (isColor)
                    else { // black and white
                        resultImage.data[(z * sliceSize) + (y * rowSize) + x] = (float) ((0.0924 *
                                                                                              (srcImage.data[currentSlice +
                                                                                                                 currentRow +
                                                                                                                 currentCol])) +
                                                                                         (0.0560 *
                                                                                              (srcImage.data[currentSlice +
                                                                                                                 currentRow +
                                                                                                                 nextCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     currentRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     nextRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     previousRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     currentRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     currentRow +
                                                                                                                     currentCol])) +
                                                                                         (0.0339 *
                                                                                              (srcImage.data[currentSlice +
                                                                                                                 nextRow +
                                                                                                                 nextCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     previousRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     nextRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[currentSlice +
                                                                                                                     previousRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     currentRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     currentRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     currentRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     currentRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     nextRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     nextRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     previousRow +
                                                                                                                     currentCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     previousRow +
                                                                                                                     currentCol])) +
                                                                                         (0.0206 *
                                                                                              (srcImage.data[nextSlice +
                                                                                                                 nextRow +
                                                                                                                 nextCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     nextRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     previousRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     previousRow +
                                                                                                                     nextCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     nextRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     nextRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[nextSlice +
                                                                                                                     previousRow +
                                                                                                                     previousCol] +
                                                                                                   srcImage.data[previousSlice +
                                                                                                                     previousRow +
                                                                                                                     previousCol])));
                    }
                }
            }
        }

        resultImage.calcMinMax();

        return resultImage;
    }

    /**
     * Takes a simple 3D image and subsamples each image plane (XY) by 2. Linear interpolation is used.
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subsample3dBy2XY() {
        return subsample3dBy2XY(this.isColor);
    }

    /**
     * Takes a simple 3D image and subsamples each image plane (XY) by 2. Linear interpolation is used.
     *
     * @param   isColor  If true, indicates that this image is a color image (i.e. ARGB).
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subsample3dBy2XY(boolean isColor) {
        ModelSimpleImage srcImage = this;

        int[] extents = new int[srcImage.nDims];
        float[] resolutions = new float[srcImage.nDims];
        float[] distance = new float[srcImage.nDims];

        for (int i = 0; i < 2; i++) {
            distance[i] = srcImage.extents[i] * srcImage.resolutions[i];
            extents[i] = srcImage.extents[i] / 2;
            resolutions[i] = distance[i] / extents[i];
        }

        distance[2] = srcImage.extents[2] * srcImage.resolutions[2];
        extents[2] = srcImage.extents[2];
        resolutions[2] = srcImage.resolutions[2];

        ModelSimpleImage resultImage = new ModelSimpleImage(extents, resolutions, isColor);

        return subSample3dBy2XY(resultImage, isColor);
    }

    /**
     * Takes a simple image and subsamples XY by 2, interpolating so that the new XY values are averages.
     *
     * @param   resultImage  The subsampled data will be stored in this object.
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subSample3dBy2XY(ModelSimpleImage resultImage) {
        return subSample3dBy2XY(resultImage, this.isColor);
    }

    /**
     * Takes a simple image and subsamples XY by 2, interpolating so that the new XY values are averages.
     *
     * @param   resultImage  The subsampled data will be stored in this object.
     * @param   isColor      If true, indicates that this image is a color image (i.e. ARGB).
     *
     * @return  Subsampled image.
     */
    public ModelSimpleImage subSample3dBy2XY(ModelSimpleImage resultImage, boolean isColor) {
        ModelSimpleImage srcImage = this;

        int c;

        int rowSize = resultImage.xDim;
        int row = srcImage.xDim;
        int zStop, yStop, xStop;

        zStop = srcImage.zDim;
        yStop = srcImage.yDim;
        xStop = srcImage.xDim;

        int nextRow, nextCol;
        int srcSlice = srcImage.xDim * srcImage.yDim;
        int resultSlice = resultImage.xDim * resultImage.yDim;
        int srcZ, resultZ;

        for (int z = 0; z < zStop; z++) {
            srcZ = z * srcSlice;
            resultZ = z * resultSlice;

            for (int y = 0, by = 1; by < yStop; y++, by += 2) {

                for (int x = 0, bx = 1; bx < xStop; x++, bx += 2) {
                    int currentRow = by * row;
                    int previousRow = (by - 1) * row;

                    if (by >= (srcImage.yDim - 1)) {
                        nextRow = (srcImage.yDim - 1) * row;
                    } else {
                        nextRow = (by + 1) * row;
                    }

                    int currentCol = bx;
                    int previousCol = (bx - 1);

                    if (bx >= (srcImage.xDim - 1)) {
                        nextCol = srcImage.xDim - 1;
                    } else {
                        nextCol = (bx + 1);
                    }

                    if (isColor) {

                        for (c = 0; c <= 3; c++) {
                            resultImage.data[(4 * (resultZ + (y * rowSize) + x)) + c] = (float) ((0.208 *
                                                                                                      srcImage.data[(4 *
                                                                                                                         (srcZ +
                                                                                                                              currentRow +
                                                                                                                              currentCol)) +
                                                                                                                        c]) +
                                                                                                 (0.122 *
                                                                                                      (srcImage.data[(4 *
                                                                                                                          (srcZ +
                                                                                                                               currentRow +
                                                                                                                               nextCol)) +
                                                                                                                         c] +
                                                                                                           srcImage.data[(4 *
                                                                                                                              (srcZ +
                                                                                                                                   currentRow +
                                                                                                                                   previousCol)) +
                                                                                                                             c] +
                                                                                                           srcImage.data[(4 *
                                                                                                                              (srcZ +
                                                                                                                                   nextRow +
                                                                                                                                   currentCol)) +
                                                                                                                             c] +
                                                                                                           srcImage.data[(4 *
                                                                                                                              (srcZ +
                                                                                                                                   previousRow +
                                                                                                                                   currentCol)) +
                                                                                                                             c])) +
                                                                                                 (0.076 *
                                                                                                      (srcImage.data[(4 *
                                                                                                                          (srcZ +
                                                                                                                               nextRow +
                                                                                                                               nextCol)) +
                                                                                                                         c] +
                                                                                                           srcImage.data[(4 *
                                                                                                                              (srcZ +
                                                                                                                                   previousRow +
                                                                                                                                   nextCol)) +
                                                                                                                             c] +
                                                                                                           srcImage.data[(4 *
                                                                                                                              (srcZ +
                                                                                                                                   nextRow +
                                                                                                                                   previousCol)) +
                                                                                                                             c] +
                                                                                                           srcImage.data[(4 *
                                                                                                                              (srcZ +
                                                                                                                                   previousRow +
                                                                                                                                   previousCol)) +
                                                                                                                             c])));
                        } // for (c = 0; c <= 3; c++)
                    } // if (isColor)
                    else { // not color
                        resultImage.data[resultZ + (y * rowSize) + x] = (float) ((0.208 *
                                                                                      srcImage.data[srcZ + currentRow +
                                                                                                        currentCol]) +
                                                                                 (0.122 *
                                                                                      (srcImage.data[srcZ + currentRow +
                                                                                                         nextCol] +
                                                                                           srcImage.data[srcZ +
                                                                                                             currentRow +
                                                                                                             previousCol] +
                                                                                           srcImage.data[srcZ +
                                                                                                             nextRow +
                                                                                                             currentCol] +
                                                                                           srcImage.data[srcZ +
                                                                                                             previousRow +
                                                                                                             currentCol])) +
                                                                                 (0.076 *
                                                                                      (srcImage.data[srcZ + nextRow +
                                                                                                         nextCol] +
                                                                                           srcImage.data[srcZ +
                                                                                                             previousRow +
                                                                                                             nextCol] +
                                                                                           srcImage.data[srcZ +
                                                                                                             nextRow +
                                                                                                             previousCol] +
                                                                                           srcImage.data[srcZ +
                                                                                                             previousRow +
                                                                                                             previousCol])));
                    } // else not color
                }
            }
        } // for (int z = 0; z < zStop; z++)

        resultImage.calcMinMax();

        return resultImage;
    }

    /**
     * Returns a readable representation of this simple image.
     *
     * @return  Representation containing extents and resolutions.
     */
    public String toString() {
        String s = "";
        s += "Simple Image[Extents = ";

        for (int i = 0; i < extents.length; i++) {
            s += extents[i] + "  ";
        }

        s += "; Resolutions = ";

        for (int i = 0; i < resolutions.length; i++) {
            s += resolutions[i] + "  ";
        }

        s += "]";
        s += " data length =   " + dataSize;

        s += "; Min and Max = " + min + ", " + max + "\n";

        return s;
    }

    /**
     * Calls disposeLocal of this class to ensure this class nulls the references to global class variables so that
     * memory will be recovered.
     *
     * @throws  Throwable  Throws an error if there is a problem with the finalization of this object.
     */
    protected void finalize() throws Throwable {
        this.disposeLocal(false);
        super.finalize();
    }

}
