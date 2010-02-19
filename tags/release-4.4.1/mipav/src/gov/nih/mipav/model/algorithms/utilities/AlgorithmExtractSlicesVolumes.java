package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * <p>Title: AlgorithmExtractIndividualSlices</p>
 *
 * <p>Description: Extracts individual slices from 3D or 4D images and opens each slice in its own Frame</p>
 *
 * @author   ben link
 * @version  1.0
 */
public class AlgorithmExtractSlicesVolumes extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** List of slices to remove from source image. */
    private boolean[] extract;

    /** DOCUMENT ME! */
    private Vector extractedImages;

    /** Original Z dimension of the image. */
    private int oldZdim;

    /** Area of a slice (Xdim * Ydim). */
    private int sliceArea;

    /** X dimension of the image. */
    private int Xdim;

    /** Y dimension of the image. */
    private int Ydim;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Import source and destination images into the class.
     *
     * @param  srcImage      source image (image to extract from)
     * @param  removeSlices  list of booleans for slices that should be extracted
     */
    public AlgorithmExtractSlicesVolumes(ModelImage srcImage, boolean[] removeSlices) {
        super(null, srcImage);
        extract = removeSlices;

        // get local attributes from this.srcImage
        Xdim = srcImage.getExtents()[0];
        Ydim = srcImage.getExtents()[1];
        sliceArea = Xdim * Ydim; // one slice has sliceArea number of pixels

        oldZdim = srcImage.getExtents()[2]; //

        extractedImages = new Vector();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Return the images extracted from the source image.
     *
     * @return  The extracted images.
     */
    public ModelImage[] getExtractedImages() {
        ModelImage[] array = new ModelImage[extractedImages.size()];

        for (int i = 0; i < array.length; i++) {
            array[i] = (ModelImage) extractedImages.get(i);
        }

        return array;
    }

    /**
     * Calculates the final output.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        extractSlices();
    } // end run()

    /**
     * Copy important file information to resultant image structure.
     *
     * @param  image        Source image.
     * @param  resultImage  Resultant image.
     * @param  slice        DOCUMENT ME!
     */
    public void updateFileInfo(ModelImage image, ModelImage resultImage, int slice) {
        FileInfoBase[] fileInfo;

        if (resultImage.getNDims() == 2) {
            fileInfo = resultImage.getFileInfo();

            fileInfo[0].setModality(image.getFileInfo()[slice].getModality());
            fileInfo[0].setFileDirectory(image.getFileInfo()[slice].getFileDirectory());

            // fileInfo[i].setDataType(image.getFileInfo()[j].getDataType());
            fileInfo[0].setEndianess(image.getFileInfo()[slice].getEndianess());
            fileInfo[0].setUnitsOfMeasure(image.getFileInfo()[slice].getUnitsOfMeasure());
            fileInfo[0].setResolutions(image.getFileInfo()[slice].getResolutions());
            fileInfo[0].setExtents(resultImage.getExtents());
            fileInfo[0].setMax(resultImage.getMax());
            fileInfo[0].setMin(resultImage.getMin());
            fileInfo[0].setImageOrientation(image.getImageOrientation());
            fileInfo[0].setAxisOrientation(image.getFileInfo()[slice].getAxisOrientation());
            fileInfo[0].setOrigin(image.getFileInfo()[slice].getOrigin());
            fileInfo[0].setPixelPadValue(image.getFileInfo()[slice].getPixelPadValue());
            fileInfo[0].setPhotometric(image.getFileInfo()[slice].getPhotometric());
        }

        if (resultImage.getNDims() == 3) {
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                int j = Math.min(i, image.getExtents()[2] - 1);
                fileInfo[i].setModality(image.getFileInfo()[j].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[j].getFileDirectory());

                // fileInfo[i].setDataType(image.getFileInfo()[j].getDataType());
                fileInfo[i].setEndianess(image.getFileInfo()[j].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[j].getUnitsOfMeasure());
                fileInfo[i].setResolutions(image.getFileInfo()[j].getResolutions());
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(image.getImageOrientation());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[j].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[j].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[j].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[j].getPhotometric());
            }
        }

    }

    /**
     * Calculates and returns the color factor based on the srcImage.
     *
     * @return  DOCUMENT ME!
     */
    protected int getColorFactor() {

        int colorFactor = 1;

        if (srcImage.isColorImage()) {
            colorFactor = 4;
        }

        return colorFactor;

    } // end getColorFactor()

    /**
     * Calculates the final output and stores it in the source image.
     */
    private void extractSlices() {
        int plate;
        int zSrc, zDest; // zSrc is slice-depth of srcImage; zDest is slice-depth of destination
        int axisOfChange;
        float[] imageBuffer;
        float[] resultBuffer = null;
        ModelImage resultImage = null;
        int t;
        int tDim;
        int tDestOffset, tSrcOffset;
        int colorFactor;
        int[] newExtents;
        int newNDims;

        if (srcImage.getNDims() == 4) {
            tDim = srcImage.getExtents()[3];
        } else {
            tDim = 1;
        }

        // get the colorFactor
        colorFactor = getColorFactor();

        // initialize the image buffer (will only hold a single slice at a time)
        // however, want to make sure that the src image isn't locked by someone else,
        // so set a lock --- an exception will be thrown if image is locked.
        // amount of data allocated is based on colorFactor
        try {
            imageBuffer = new float[colorFactor * sliceArea];
            fireProgressStateChanged(srcImage.getImageName(), "Removing Selected Slices...");
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();
            displayError("Algorithm extract slices or volumes reports: Out of memory");
            srcImage.releaseLock();
            setCompleted(false);


            return;
        }

        // make a location & view the progressbar; make length & increment of progressbar.

        newExtents = new int[srcImage.getExtents().length - 1];

        // System.err.println("NEW EXTENTS: " + newExtents.length);
        newExtents[0] = srcImage.getExtents()[0];
        newExtents[1] = srcImage.getExtents()[1];

        if (srcImage.getExtents().length > 3) {
            newExtents[2] = srcImage.getExtents()[3];
        }

        newNDims = newExtents.length;

        float progress = 0;
        float numToExtract = 0;

        for (int j = 0; j < extract.length; j++) {

            if (extract[j]) {
                numToExtract++;
            }
        }

        // System.err.println("tDim: " + tDim);

        // start counting the slices of the destination image at the first slice.
        // for all slices in the old image

        if (newExtents.length == 2) {

            for (zSrc = 0; (zSrc < oldZdim) && !threadStopped; zSrc++) {

                if (extract[zSrc]) {
                    progress++;

                    fireProgressStateChanged((int) ((progress / numToExtract) * 100));

                    try {

                        // try copying the zSrc-th slice out of srcImage, making it the zDest-th in destImage
                        if (srcImage.isColorImage()) {
                            srcImage.exportData(zSrc * 4 * sliceArea, 4 * sliceArea, imageBuffer);
                        } else {
                            srcImage.exportSliceXY(zSrc, imageBuffer);
                        }

                        resultImage = new ModelImage(srcImage.getType(), newExtents,
                                                     srcImage.getImageName() + "_slice" + (zSrc));

                        resultImage.importData(0, imageBuffer, false);

                        updateFileInfo(srcImage, resultImage, zSrc);

                        resultImage.calcMinMax();

                        extractedImages.add(resultImage);
                    } catch (IOException error) {
                        displayError("Algorithm Extract Individual Slices reports: " + error.getMessage());
                        error.printStackTrace();
                        setCompleted(false);


                        return;
                    }

                } // if (!extract[zSrc])
                // else {do nothing; goto next zSrc-slice;}
            } // for (zSrc = 0; zSrc < oldZdim; zSrc++)
        } else if (newExtents.length == 3) {

            for (zSrc = 0; (zSrc < oldZdim) && !threadStopped; zSrc++) {

                if (extract[zSrc]) {
                    progress++;

                    fireProgressStateChanged((int) ((progress / numToExtract) * 100));

                    resultImage = new ModelImage(srcImage.getType(), newExtents,
                                                 srcImage.getImageName() + "_slice" + (zSrc));

                    tDestOffset = 0;

                    for (t = 0; (t < tDim) && !threadStopped; t++) {
                        tSrcOffset = getOffset(srcImage, colorFactor, t);

                        // System.err.println("zSrc is: " + zSrc + " tSrcOffset is: " + tSrcOffset);
                        try {

                            if (srcImage.isColorImage()) {
                                srcImage.exportData(tSrcOffset + (zSrc * 4 * sliceArea), 4 * sliceArea, imageBuffer);
                            } else {

                                // System.err.println("exporting slice: " + (t * oldZdim + zSrc));
                                srcImage.exportSliceXY((t * oldZdim) + zSrc, imageBuffer);
                            }

                            // System.err.println("Importing data at: " + (imageBuffer.length * t));
                            resultImage.importData(imageBuffer.length * t, imageBuffer, false);
                        } catch (IOException ex) {
                            ex.printStackTrace();
                        }
                    }

                    // update file info
                    updateFileInfo(srcImage, resultImage, zSrc);

                    resultImage.calcMinMax();

                    extractedImages.add(resultImage);
                }
            }

        }

        if (threadStopped) {
            imageBuffer = null;
            resultImage = null;
            finalize();


            return;
        }

        resultImage = null; // after all, this was only temporary

        setCompleted(true);
    } // extractSlices

    /**
     * computes and returns the number of slices to extract.
     *
     * @return  DOCUMENT ME!
     */
    private int getNumRemoved() {

        int num = 0;

        // cycle through the array of slices to extract and
        // only count the ones that are set to true
        for (int i = 0; i < oldZdim; i++) {

            if (extract[i]) {
                num++;
            }
        }

        return num;

    } // end getNumRemoved()

    /**
     * Calculate and return the offset for the given image and time slice.
     *
     * @param   img          -- ModelImage whose offset is being computed
     * @param   colorFactor  -- the image's colorFactor
     * @param   t            -- current time slice
     *
     * @return  -- the computed offset into the data buffer
     */
    private int getOffset(ModelImage img, int colorFactor, int t) {

        int offset = 0;

        if (img.getNDims() > 3) {
            offset = img.getSliceSize() * img.getExtents()[2] * colorFactor * t;
        } else {
            offset = img.getSliceSize() * colorFactor * t;
        }

        return offset;

    } // end getOffset()

    /**
     * Calculate and return the offset for the given image and time slice.
     *
     * @param   ndims        -- number of dimensions
     * @param   extents      -- the extents for the image offset
     * @param   colorFactor  -- the image's colorFactor
     * @param   t            -- current time slice
     *
     * @return  -- the computed offset into the data buffer
     */
    private int getOffset(int ndims, int[] extents, int colorFactor, int t) {

        int offset = 0;

        if (ndims > 3) {
            offset = extents[0] * extents[1] * extents[2] * colorFactor * t;
        } else {
            offset = extents[0] * extents[1] * colorFactor * t;
        }

        return offset;

    } // end getOffset()

    /**
     * Build and return the fileInfo for the current slice based on the source slice, the destination slice and time
     * slice.
     *
     * @param   srcSlice    the current slice index for the src image
     * @param   destSlice   the current slice index for the result (dest) image
     * @param   t           current time slice
     * @param   newExtents  the extents for the result (dest) image
     *
     * @return  the fileInfo for destSlice
     */
    private FileInfoBase getSliceFileInfo(int srcSlice, int destSlice, int t, int[] newExtents) {

        // used for DICOM images
        // float[] nextPositionCoords = new float[3];
        // float[] imagePositionCoords = new float[3]; // image position along the XYZ-axis

        // If src is a DICOM image, then need to do some special stuff
        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
            FileInfoDicom fileInfoBuffer; // buffer of type DICOM
            fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(srcSlice).clone(); // copy into buffer

            fileInfoBuffer.setExtents(newExtents); // get the extents of this image and put it in the new filebuffer

            // change the slice number ("0020,0013"):
            // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
            fileInfoBuffer.getTagTable().setValue("0020,0013", String.valueOf(destSlice + 1),
                                                  fileInfoBuffer.getTagTable().get("0020,0013").getLength()); // Reset the image
                                                                                                              // (slice) number with
                                                                                                              // the new number
                                                                                                              // ordering

            return fileInfoBuffer;
        }

        // from this point on, image is not a DICOM image
        FileInfoBase fileInfoBuffer; // buffer of any old type

        fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * oldZdim) + srcSlice).clone();
        fileInfoBuffer.setExtents(newExtents);

        return fileInfoBuffer;

    } // end getSliceFileInfo()

}
