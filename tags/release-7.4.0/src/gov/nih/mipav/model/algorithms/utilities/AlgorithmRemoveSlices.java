package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Algorithm verifies the z-th slice should be in included in the destination image, as defined in the list, remove. It
 * copies the slice from the srcImage img to a buffer, and then from the buffer into the destination img. Copies the
 * srcImage file info to a buffer and makes it conform to the new img, then copies it into the destImage file.
 *
 * <p>Note that an image is a set of slices; each slice is XxY, with Z slices.**(as of 1 Nov, does not yet process the
 * more complicated DICOM images Completely)</p>
 *
 * @author   David Parsons (parsonsd@cbel.cit.nih.gov) (with vast help from M.McAuliffe)
 * @version  v0.11 1 Nov 1999 (processes most images)
 * @see      gov.nih.mipav.view.dialogs.JDialogRemoveSlices
 */
public class AlgorithmRemoveSlices extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Original Z dimension of the image. */
    private int oldZdim;

    /** List of slices to remove from source image. */
    private boolean[] remove;

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
     * @param  srcImage      source image (image to clip from)
     * @param  destImage     destination image (image to paste to)
     * @param  removeSlices  list of boolean indicating which slices in source should *not* be in the destination
     */
    public AlgorithmRemoveSlices(ModelImage srcImage, ModelImage destImage, boolean[] removeSlices) {
        super(destImage, srcImage);
        remove = removeSlices;

        // get local attributes from this.srcImage
        Xdim = srcImage.getExtents()[0];
        Ydim = srcImage.getExtents()[1];
        sliceArea = Xdim * Ydim; // one slice has sliceArea number of pixels

        oldZdim = srcImage.getExtents()[2]; //
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Copy important file information to resultant image structure.
     *
     * @param  image        Source image.
     * @param  resultImage  Resultant image.
     */
    public static void updateFileInfo(ModelImage image, ModelImage resultImage) {
        FileInfoBase[] fileInfo;

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
        } else if (resultImage.getNDims() == 4) {
            fileInfo = resultImage.getFileInfo();

            for (int i = 0; i < (resultImage.getExtents()[2] * resultImage.getExtents()[3]); i++) {
                fileInfo[i].setModality(image.getFileInfo()[i].getModality());
                fileInfo[i].setFileDirectory(image.getFileInfo()[i].getFileDirectory());
                fileInfo[i].setEndianess(image.getFileInfo()[i].getEndianess());
                fileInfo[i].setUnitsOfMeasure(image.getFileInfo()[i].getUnitsOfMeasure());
                fileInfo[i].setResolutions(image.getFileInfo()[i].getResolutions());
                fileInfo[i].setExtents(resultImage.getExtents());
                fileInfo[i].setMax(resultImage.getMax());
                fileInfo[i].setMin(resultImage.getMin());
                fileInfo[i].setImageOrientation(image.getImageOrientation());
                fileInfo[i].setAxisOrientation(image.getFileInfo()[i].getAxisOrientation());
                fileInfo[i].setOrigin(image.getFileInfo()[i].getOrigin());
                fileInfo[i].setPixelPadValue(image.getFileInfo()[i].getPixelPadValue());
                fileInfo[i].setPhotometric(image.getFileInfo()[i].getPhotometric());
            }
        }

    }

    /**
     * Calculates the final output and stores it in the source image.
     */
    public void calcInPlace() {
        int zSrc, zDest; // zSrc is slice-depth of srcImage; zDest is slice-depth of destination
        float[] imageBuffer;
        float[] resultBuffer = null;
        ModelImage resultImage;
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
            srcImage.setLock(ModelStorageBase.W_LOCKED);
            imageBuffer = new float[colorFactor * sliceArea];
            fireProgressStateChanged(srcImage.getImageName(), "Removing Selected Slices...");
        } catch (IOException err) {
            imageBuffer = null;
            System.gc();
            displayError("Algorithm Remove Slices: Source Image Locked.");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();
            displayError("Algorithm Remove Slices reports: Out of memory");
            srcImage.releaseLock();
            setCompleted(false);


            return;
        }

        srcImage.releaseLock();

        // need to know what the new extents will be
        // there are some cases that need to be checked which may
        // change the number of dimensions of the image
        // (i.e. if all but 1 slice of a 3D image is to be removed ... it's really
        // just a 2D image then.)
        newExtents = computeNewExtents();
        newNDims = newExtents.length;

        // create a resultImage to hold the results in temporarily.  At the end
        // of processing the resultBuffer will be exported from the resultImage and
        // imported to the srcImage.
        resultImage = new ModelImage(srcImage.getType(), newExtents, "RemoveSlicesTemp");

        for (t = 0; (t < tDim) && !threadStopped; t++) {
            tDestOffset = getOffset(newNDims, newExtents, colorFactor, t);
            tSrcOffset = getOffset(srcImage, colorFactor, t);

            zDest = 0;

            // start counting the slices of the destination image at the first slice.
            // for all slices in the old image
            for (zSrc = 0; (zSrc < oldZdim) && !threadStopped; zSrc++) {

                // let user know something is happening by updating the progressbar
                fireProgressStateChanged(Math.round((float) ((t * oldZdim) + zSrc) / ((tDim * oldZdim) - 1) * 100));

                // so long as the slice has not been marked for removal, copy it all over.
                if (!remove[zSrc]) {

                    try {

                        // try copying the zSrc-th slice out of srcImage, making it the zDest-th in destImage
                    	srcImage.exportData(tSrcOffset + (zSrc * colorFactor * sliceArea),
                    			colorFactor * sliceArea, imageBuffer);

                        resultImage.importData(tDestOffset + (zDest * colorFactor * sliceArea), imageBuffer, false);
                    } catch (IOException error) {
                        displayError("Algorithm RemoveSlices reports: " + error.getMessage());
                        error.printStackTrace();
                        setCompleted(false);


                        return;
                    }

                    // set file info for the slice.
                    // ... but do something special for DICOM images
                    if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        FileInfoDicom fileInfoBuffer = (FileInfoDicom) getSliceFileInfo(zSrc, zDest, t, newExtents);
                        resultImage.setFileInfo(fileInfoBuffer, zDest);
                    } else { // not a DICOM image

                        FileInfoBase fileInfoBuffer = getSliceFileInfo(zSrc, zDest, t, newExtents);

                        if (newNDims > 3) {
                            resultImage.setFileInfo(fileInfoBuffer, (t * newExtents[2]) + zDest);
                        } else {
                            resultImage.setFileInfo(fileInfoBuffer, t + zDest);
                        }
                    }

                    zDest++; // next slice position in the new image.

                } // if (!remove[zSrc])
                // else {do nothing; goto next zSrc-slice;}
            } // for (zSrc = 0; zSrc < oldZdim; zSrc++)
        } // for (t = 0; t < tDim; t++)

        if (threadStopped) {
            imageBuffer = null;
            resultImage = null;
            srcImage.releaseLock();
            finalize();


            return;
        }

        // reset the extents of the srcImage, then reset the dataSize
        resultImage.calcMinMax();
        srcImage.changeExtents(newExtents);

        // import the result buffer from the resultImage into the srcImage
        // do this a slice at a time to conserve memory
        int resultSize = colorFactor * sliceArea;
        int numSlices = 1;
        int numTimes = 1;

        if (newNDims > 2) {
            numSlices = newExtents[2];
        }

        if (newNDims > 3) {
            numTimes = newExtents[3];
        }

        fireProgressStateChanged("Importing Image Data...");

        try {

            if (resultBuffer != null) {
                resultBuffer = null;
                System.gc();
            }

            resultBuffer = new float[resultSize];

            int index = 0;

            for (int time = 0; time < numTimes; time++) {

                for (int slice = 0; slice < numSlices; slice++) {
                    resultImage.exportDataNoLock(index, resultSize, resultBuffer);
                    srcImage.importData(index, resultBuffer, false);

                    // increment the index by the amount of data exported/imported
                    index += resultSize;
                }
            }

            srcImage.calcMinMax();
        } catch (OutOfMemoryError e) {
            resultBuffer = null;
            resultImage = null;
            System.gc();
            displayError("Algorithm Remove Slices reports: Out of memory getting results.");
            srcImage.releaseLock();
            setCompleted(false);


            return;
        } catch (IOException e2) {
            resultBuffer = null;
            resultImage = null;
            System.gc();
            displayError(e2.getMessage());
            srcImage.releaseLock();
            setCompleted(false);


            return;
        }

        // update the fileInfo in the srcImage
        fireProgressStateChanged("Updating Image Headers...");

        
        updateFileInfo(resultImage, srcImage);
        
        // Clean up and let the calling dialog know that algorithm did its job
        srcImage.releaseLock();

        resultImage.disposeLocal(); // this was not here before... MEMORY LEAK!

        resultImage = null; // after all, this was only temporary

        setCompleted(true);
    } // end calcInPlace()

    /**
     * Calculates the final output and puts it in a destination image.
     */
    public void calcStoreInDest() {
        int zSrc, zDest; // zSrc is slice-depth of srcImage; zDest is slice-depth of destination
        float[] imageBuffer;
        int t;
        int tDim;
        int tDestOffset, tSrcOffset;
        int colorFactor;

        if (srcImage.getNDims() == 4) {
            tDim = srcImage.getExtents()[3];
        } else {
            tDim = 1;
        }

        // get the colorFactor
        colorFactor = getColorFactor();

        // create  <FileInfoBase fileInfoBuffer;> buffer; may be of type FileInfoDicom
        // amount of data allocated is based on colorFactor
        try {
            imageBuffer = new float[colorFactor * sliceArea];
            fireProgressStateChanged(srcImage.getImageName(), "Removing Selected Slices...");
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();
            displayError("Algorithm Remove Slices reports: Out of memory");
            setCompleted(false);


            return;
        }

        // make a location & view the progressbar; make length & increment of progressbar.


        // get the axis of change
        try { // axisOfChange = getAxisOfChange();
        } catch (Exception err) {
            MipavUtil.displayWarning("Failed to get axis orientation." + err.getMessage());
            setCompleted(false);


            return;
        }

        for (t = 0; (t < tDim) && !threadStopped; t++) {
            tDestOffset = getOffset(destImage, colorFactor, t);
            tSrcOffset = getOffset(srcImage, colorFactor, t);

            zDest = 0;

            // start counting the slices of the destination image at the first slice.
            // for all slices in the old image
            for (zSrc = 0; (zSrc < oldZdim) && !threadStopped; zSrc++) {

                // let user know something is happening by updating the progressbar
                fireProgressStateChanged(Math.round((float) ((t * oldZdim) + zSrc) / ((tDim * oldZdim) - 1) * 100));

                // so long as the slice has not been marked for removal, copy it all over.
                if (!remove[zSrc]) {

                    try {

                        // try copying the zSrc-th slice out of srcImage, making it the zDest-th in destImage
                    	srcImage.exportData(tSrcOffset + (zSrc * colorFactor * sliceArea), 
                    			colorFactor * sliceArea, imageBuffer);

                        destImage.importData(tDestOffset + (zDest * colorFactor * sliceArea), imageBuffer, false);
                    } catch (IOException error) {
                        displayError("Algorithm RemoveSlices reports: " + error.getMessage());
                        setCompleted(false);


                        return;
                    }

                    // set file info for the slice.
                    // ... but do something special for DICOM images
                    if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        FileInfoDicom fileInfoBuffer = (FileInfoDicom) getSliceFileInfo(zSrc, zDest, t,
                                                                                        destImage.getExtents());
                        destImage.setFileInfo(fileInfoBuffer, zDest);
                    } else { // not a DICOM image

                        FileInfoBase fileInfoBuffer = getSliceFileInfo(zSrc, zDest, t, destImage.getExtents());

                        if (destImage.getNDims() > 3) {
                            destImage.setFileInfo(fileInfoBuffer, (t * destImage.getExtents()[2]) + zDest);
                        } else {
                            destImage.setFileInfo(fileInfoBuffer, t + zDest);
                        }
                    }

                    zDest++; // next slice position in the new image.

                } // if (!remove[zSrc])
                // else {do nothing; goto next zSrc-slice;}
            } // for (zSrc = 0; zSrc < oldZdim; zSrc++)
        } // for (t = 0; t < tDim; t++)

        if (threadStopped) {
            imageBuffer = null;
            finalize();


            return;
        }

        destImage.calcMinMax(); // calculate the minimum & maximum intensity values for the destImage-image

        // Clean up and let the calling dialog know that algorithm did its job

        setCompleted(true);
    } // end calcStoreInDest()

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Calculates the final output.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        if (destImage != null) { // if there exists a destination image
            calcStoreInDest();
        } else { // there is no image but the original source.
            calcInPlace();
        }
    } // end run()

    /**
     * Calculates and returns the axis of change based on the srcImage. The default (for most images) is 2. DICOM images
     * need to be checked.
     *
     * @return  DOCUMENT ME!
     *
     * @throws  Exception  DOCUMENT ME!
     */
    protected int getAxisOfChange() throws Exception {

        int axisOfChange = 2;
        // float[] nextPositionCoords = new float[3];
        // float[] imagePositionCoords = new float[3]; // image position along the XYZ-axis

        // If not DICOM return default axis of change
        if ((srcImage.getFileInfo()[0]).getFileFormat() != FileUtility.DICOM) {
            return axisOfChange;
        }

        return axisOfChange;

    } // end getAxisOfChange()

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
        else if (srcImage.isComplexImage()) {
        	colorFactor = 2;
        }

        return colorFactor;

    } // end getColorFactor()

    /**
     * Calculates and returns the new extents based on the source image and the list of slices to be removed.
     *
     * @return  DOCUMENT ME!
     */
    private int[] computeNewExtents() {

        int[] extents = null;
        int numRemoved = getNumRemoved();

        if (srcImage.getNDims() == 3) {

            // destination image extents (length in a particular direction)
            // if user cuts all but 1 slice, make dest a 2D image:
            if ((srcImage.getExtents()[2] - numRemoved) == 1) {
                extents = new int[2];
                extents[0] = srcImage.getExtents()[0];
                extents[1] = srcImage.getExtents()[1];
            } // else dest will have volume, so make it a 3D image:
            else if ((srcImage.getExtents()[2] - numRemoved) > 1) {
                extents = new int[3];
                extents[0] = srcImage.getExtents()[0];
                extents[1] = srcImage.getExtents()[1];
                extents[2] = srcImage.getExtents()[2] - numRemoved;
            }
        } // end if (srcImage.getNDims() == 3)

        // 4D
        else {

            // destination image extents (length in a particular direction)
            // if user cuts all but 1 slice, make dest a 3D image:
            if ((srcImage.getExtents()[2] - numRemoved) == 1) {
                extents = new int[3];
                extents[0] = srcImage.getExtents()[0];
                extents[1] = srcImage.getExtents()[1];
                extents[2] = srcImage.getExtents()[3];
            } // else dest will have 4D, so make it a 4D image:
            else if ((srcImage.getExtents()[2] - numRemoved) > 1) {
                extents = new int[4];
                extents[0] = srcImage.getExtents()[0];
                extents[1] = srcImage.getExtents()[1];
                extents[2] = srcImage.getExtents()[2] - numRemoved;
                extents[3] = srcImage.getExtents()[3];
            }
        } // 4D

        return extents;

    } // end computeNewExtents()

    /**
     * computes and returns the number of slices to remove.
     *
     * @return  DOCUMENT ME!
     */
    private int getNumRemoved() {

        int num = 0;

        // cycle through the array of slices to remove and
        // only count the ones that are set to true
        for (int i = 0; i < oldZdim; i++) {

            if (remove[i]) {
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
            offset = img.getExtents()[0] * img.getExtents()[1] * img.getExtents()[2] * colorFactor * t;
        } else {
            offset = img.getExtents()[0] * img.getExtents()[1] * colorFactor * t;
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
