package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * Algorithm that extracts the slices indicated in the list from the srcImage and puts them into the destImage.
 *
 * <p>Note that an image is a set of slices; each slice is XxY, with Z slices.</p>
 *
 * @author   Lynne Pusanik based on AlgorithmRemoveSlices
 * @author   David Parsons (parsonsd@cbel.cit.nih.gov) (with vast help from M.McAuliffe)
 * @version  v0.11 1 Nov 1999 (processes most images)
 * @see      AlgorithmRemoveSlices
 */


public class AlgorithmExtractSlices extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** If true convert the selected images to 3D datasets. */
    private boolean convert4Dto3D = false;

    /** List of slices to extract from source image. */
    private Vector<String> extractList;

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
     * @param  srcImage       source image (image to clip from)
     * @param  destImage      destination image (image to paste to)
     * @param  extractSlices  list of slice numbers indicating which slices in source should be extracted to
     *                        destination.
     */
    public AlgorithmExtractSlices(ModelImage srcImage, ModelImage destImage, String[] extractSlices) {
        super(destImage, srcImage);

        // create extractList from the array
        extractList = new Vector<String>(extractSlices.length);

        for (int i = 0; i < extractSlices.length; i++) {
            extractList.addElement(extractSlices[i]);
        }

        // get local attributes from this.srcImage
        Xdim = srcImage.getExtents()[0];
        Ydim = srcImage.getExtents()[1];
        sliceArea = Xdim * Ydim; // one slice has sliceArea number of pixels

        oldZdim = srcImage.getExtents()[2];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Method to return the extract list since local copies may change while the algorithm runs.
     *
     * @return  DOCUMENT ME!
     */
    public Vector<String> getExtractList() {
        return extractList;
    }

    /**
     * Calculates the final output.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("ExtractSlices.run(): Source Image is null");

            return;
        }

        

        if (destFlag == true) { // use a destination image.
            calcStoreInDest();
        } else {
            displayError("ExtractSlices.run(): Replacing source image is not supported.");

            return;
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doConvert  DOCUMENT ME!
     */
    public void setConvert4Dto3D(boolean doConvert) {
        this.convert4Dto3D = doConvert;
    }

    /**
     * DOCUMENT ME!
     */
    private void calcStoreInDest() {

        int zSrc, zDest; // z is slice-depth of srcImage; Z is slice-depth of destination
        int axisOfChange = 2;
        float[] imageBuffer;
        float[] nextPositionCoords = new float[3];
        float[] imagePositionCoords = new float[3]; // image position along the XYZ-axis
        int t;
        int tDimSrc;
        int tDestOffset, tSrcOffset;
        int colorFactor;

        if (srcImage.getNDims() == 4) {
            tDimSrc = srcImage.getExtents()[3];
        } else {
            tDimSrc = 1;
        }

        // create  <FileInfoBase fileInfoBuffer;> buffer; may be of type FileInfoDicom
        try {

            if (srcImage.isColorImage()) {
                imageBuffer = new float[4 * sliceArea];
            } else {
                imageBuffer = new float[sliceArea];
            }

            fireProgressStateChanged(srcImage.getImageName(), "Extracting Selected Slices...");
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            System.gc();
            displayError("Algorithm Extract Slices reports: Out of memory");
            setCompleted(false);


            return;
        }

        // make a location & view the progressbar; make length & increment of progressbar.


        // No DICOM 4D images
        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

            // determine along which axis the imagePositionCoords the image varies
            if ((srcImage.getFileInfo(1) != null) && (((FileInfoDicom)srcImage.getFileInfo(0)).getTagTable().getValue("0020,0032") != null)) {
                imagePositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(0)).parseTagValue("0020,0032"));
                nextPositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(1)).parseTagValue("0020,0032"));

                // check along which axis the image coords change --- so far, only deal with orthogonal basis axis
                // to figure out which axis the slice changes in, check the first slice and the second slice for a
                // a difference along the basis.
                if ((nextPositionCoords[0] != imagePositionCoords[0]) &&
                        (nextPositionCoords[1] == imagePositionCoords[1]) &&
                        (nextPositionCoords[2] == imagePositionCoords[2])) { // change ONLY in X-axis
                    axisOfChange = 0;
                } else if ((nextPositionCoords[0] == imagePositionCoords[0]) &&
                               (nextPositionCoords[1] != imagePositionCoords[1]) &&
                               (nextPositionCoords[2] == imagePositionCoords[2])) { // change ONLY in Y-axis
                    axisOfChange = 1;
                } else if ((nextPositionCoords[0] == imagePositionCoords[0]) &&
                               (nextPositionCoords[1] == imagePositionCoords[1]) &&
                               (nextPositionCoords[2] != imagePositionCoords[2])) { // change ONLY in Z-axis
                    axisOfChange = 2;
                } else { // change ONLY in ANY OTHER axis
                    MipavUtil.displayWarning("Extract Slices does not support changes in\n" +
                                             "image position (DICOM tag 0020,0032)\n" + "in more than one dimension.");
                    setCompleted(false);


                    return;
                }
            }
        }

        if (srcImage.isColorImage()) {
            colorFactor = 4;
        } else {
            colorFactor = 1;
        }

        if (!convert4Dto3D) {

            for (t = 0; (t < tDimSrc) && !threadStopped; t++) {

                if (destImage.getNDims() > 3) {
                    tDestOffset = destImage.getSliceSize() * destImage.getExtents()[2] * colorFactor * t;
                } else {
                    tDestOffset = destImage.getSliceSize() * colorFactor * t;
                }

                tSrcOffset = srcImage.getSliceSize() * srcImage.getExtents()[2] * colorFactor * t;
                zDest = 0; // start counting the slices of the destination image at the first slice.

                for (zSrc = 0; (zSrc < oldZdim) && !threadStopped; zSrc++) { // for all slices in the src image

                    // let user know something is happening by updating the progressbar
                    fireProgressStateChanged(Math.round((float) ((t * oldZdim) + zSrc) / ((tDimSrc * oldZdim) - 1) *
                                                            100));

                    // if the slice has been marked for extraction, copy it all over.
                    if (extractList.contains(Integer.toString(zSrc))) {

                        try {

                            // try copying the zSrc slice out of srcImage, making it the zDest in destImage
                            if (srcImage.isColorImage()) {
                                srcImage.exportData(tSrcOffset + (zSrc * 4 * sliceArea), 4 * sliceArea, imageBuffer);
                                destImage.importData(tDestOffset + (zDest * 4 * sliceArea), imageBuffer, false);
                            } else {
                                srcImage.exportSliceXY((t * oldZdim) + zSrc, imageBuffer);
                                destImage.importData(tDestOffset + (zDest * sliceArea), imageBuffer, false);
                            }
                        } catch (IOException error) {
                            displayError("Algorithm ExtractSlices reports: " + error.getMessage());
                            setCompleted(false);


                            return;
                        }

                        // set file info for the slice.
                        // ... but do something special for DICOM images
                        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                            FileInfoDicom fileInfoBuffer; // buffer of type DICOM
                            fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(zSrc).clone(); // copy into buffer

                            fileInfoBuffer.setExtents(destImage.getExtents()); // get the extents of this image and put
                                                                               // it in the new filebuffer

                            // change the slice number ("0020,0013"):
                            // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                            if (fileInfoBuffer.getTagTable().get("0020,0013") != null) {
                                fileInfoBuffer.getTagTable().setValue("0020,0013", String.valueOf(zDest + 1),
                                                                  fileInfoBuffer.getTagTable().get("0020,0013").getLength()); // Reset the image (slice) number with the new number ordering
                            }
                            
                            // readjust the image position ("0020,0032"): copy the "image position (patient)" info so
                            // that axis-position left by extracting slices (zSrc) will change by sliding the next
                            // included slice forward (to the zDest position). So: the X&Y info for the zSrc (the one
                            // not removed with some large index) and we use the zDest info from the index of the
                            // copy-to slice (which has a low index).
                            if (fileInfoBuffer.getTagTable().getValue("0020,0032") != null) {
                                imagePositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(zSrc))
                                                                           .parseTagValue("0020,0032"));
                                nextPositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(zDest))
                                                                          .parseTagValue("0020,0032"));
    
                                // axis of change can only be set to X (0), Y (1), or Z (2)
                                imagePositionCoords[axisOfChange] = nextPositionCoords[axisOfChange];
    
                                // *** Only works if length of string for imagePositionCoords is the
                                // same as length for nextPositionCoords - should not need a third
                                // length parameter
                            
                                fileInfoBuffer.getTagTable().setValue("0020,0032",
                                                                  imagePositionCoords[0] + "\\" +
                                                                  imagePositionCoords[1] + "\\" +
                                                                  imagePositionCoords[2],
                                                                  fileInfoBuffer.getTagTable().get("0020,0032").getLength());
                            }

                            // readjust the slice location ("0020,1041")
                            // same change as image position above:
                            if (fileInfoBuffer.getTagTable().get("0020,1041") != null) {
                                fileInfoBuffer.getTagTable().setValue("0020,1041",
                                                                  ((FileInfoDicom) (srcImage.getFileInfo(zDest)))
                                                                      .getTagTable().getValue("0020,1041"),
                                                                  fileInfoBuffer.getTagTable().get("0020,1041").getLength());
                            }

                            destImage.setFileInfo(fileInfoBuffer, zDest);
                        } else { // not a DICOM image, so these can be processed similarly

                            FileInfoBase fileInfoBuffer; // buffer of any old type

                            fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * oldZdim) + zSrc).clone();
                            fileInfoBuffer.setExtents(destImage.getExtents());

                            if (destImage.getNDims() > 3) {
                                destImage.setFileInfo(fileInfoBuffer, (t * destImage.getExtents()[2]) + zDest);
                            } else {
                                destImage.setFileInfo(fileInfoBuffer, t + zDest);
                            }
                        }

                        zDest++; // next slice position in the new image.

                    } // if (!remove[z])

                } // for (zSrc = 0; zSrc < oldZdim; zSrc++)

                // check and see if thread stopped
                if (threadStopped) {
                    imageBuffer = null;
                    setCompleted(false);
                    finalize();


                    return;
                }

            } // for (t = 0; t < tDimSrc; t++)
        } // we are extracting certain slices out of a 4D image into a 3D image
        else {

            // System.err.println("doing 4d -> 3d");
            int destOffset = 0;
            zDest = 0;

            int numElements = extractList.size();

            for (t = 0; (t < tDimSrc) && !threadStopped; t++) {
                tSrcOffset = srcImage.getSliceSize() * srcImage.getExtents()[2] * colorFactor * t;

                for (zSrc = 0; (zSrc < oldZdim) && !threadStopped; zSrc++) { // for all slices in the src image\

                    if (extractList.contains(Integer.toString(zSrc) + "." + Integer.toString(t))) {

                        // System.err.println("Found for zSrc: " + zSrc + " and t: " + t);
                        try {

                            // try copying the zSrc slice out of srcImage, making it the zDest in destImage
                            if (srcImage.isColorImage()) {
                                srcImage.exportData(tSrcOffset + (zSrc * 4 * sliceArea), 4 * sliceArea, imageBuffer);
                                destImage.importData(destOffset, imageBuffer, false);
                                destOffset += (4 * sliceArea);
                            } else {
                                srcImage.exportSliceXY((t * oldZdim) + zSrc, imageBuffer);
                                destImage.importData(destOffset, imageBuffer, false);
                                destOffset += sliceArea;
                            }

                            fireProgressStateChanged(Math.round((float) (zDest + 1) / (numElements) * 100));

                        } catch (IOException error) {
                            displayError("Algorithm ExtractSlices reports: " + error.getMessage());
                            setCompleted(false);


                            return;
                        }

                        FileInfoBase fileInfoBuffer; // buffer of any old type

                        fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * oldZdim) + zSrc).clone();
                        fileInfoBuffer.setExtents(destImage.getExtents());

                        destImage.setFileInfo(fileInfoBuffer, zDest);
                        zDest++;
                    }
                }
            }
        }

        destImage.calcMinMax();

        setCompleted(true);

    } // end calcStoreInDest()
   

} // end class AlgorithmExtractSlices
