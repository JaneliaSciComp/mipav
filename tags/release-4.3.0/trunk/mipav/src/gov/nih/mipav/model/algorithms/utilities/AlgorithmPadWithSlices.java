package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;


/**
 * Algorithm to pad with slices to an even power of 2. In padMode = PAD_FRONT all slices are inserted in front, in
 * padMode = PAD_BACK all slices are inserted in back, and in padMode = PAD_HALF half the slices are inserted in front
 * and half the slices are inserted in back. Slices with all pixels zero are inserted. Must insert black and white with
 * black and white and color with color. Spaces between slices are maintained.
 */
public class AlgorithmPadWithSlices extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int PAD_FRONT = 0;

    /** DOCUMENT ME! */
    private static final int PAD_BACK = 1;

    /** DOCUMENT ME! */
    private static final int PAD_HALF = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Original Z dimension of the image. */
    private int oldZdim;

    /** DOCUMENT ME! */
    private int paddedSlices;

    /** DOCUMENT ME! */
    private int padMode;

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
     * @param  srcImage  source image (image to clip from)
     * @param  padMode   front, back, and half
     */
    public AlgorithmPadWithSlices(ModelImage srcImage, int padMode) {
        super(null, srcImage);
        this.padMode = padMode;

        // get local attributes from this.srcImage
        Xdim = srcImage.getExtents()[0];
        Ydim = srcImage.getExtents()[1];
        sliceArea = Xdim * Ydim; // one slice has sliceArea number of pixels

        oldZdim = srcImage.getExtents()[2];
    }

    /**
     * Import source and destination images into the class.
     *
     * @param  srcImage   source image (image to clip from)
     * @param  destImage  destination image (image to paste to)
     * @param  padMode    front, back, and half
     */
    public AlgorithmPadWithSlices(ModelImage srcImage, ModelImage destImage, int padMode) {
        super(destImage, srcImage);
        this.padMode = padMode;

        // get local attributes from this.srcImage
        Xdim = srcImage.getExtents()[0];
        Ydim = srcImage.getExtents()[1];
        sliceArea = Xdim * Ydim; // one slice has sliceArea number of pixels

        oldZdim = srcImage.getExtents()[2];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Runs algorithm.
     */
    public void runAlgorithm() {
        int i;
        int j;
        int t;
        int z, Z; // z is slice-depth of srcImage; Z is slice-depth of destination
        float[] imageBuffer;
        int tDim;
        int colorFactor;
        int tOldOffset, tNewOffset;
        int slicesToAdd = 0;
        int frontSlices = 0;
        int backSlices = 0;
        int[] destExtents = null;
        boolean calcInPlace = false;
        int currentSlices;
        float[] imagePositionCoords = new float[3]; // image position along the XYZ-axis
        float[] lastPositionCoords = new float[3];
        float[] nextPositionCoords = new float[3];
        float sliceLocation = 0.0f;
        float lastSliceLocation = 0.0f;
        float nextSliceLocation = 0.0f;
        DecimalFormat nf;

        Object value;
        String s;

        

        nf = new DecimalFormat("##0.000000");

        paddedSlices = dimPowerOfTwo(srcImage.getExtents()[2]);
        slicesToAdd = paddedSlices - srcImage.getExtents()[2];

        if (padMode == PAD_FRONT) {
            frontSlices = slicesToAdd;
        } else if (padMode == PAD_BACK) {
            backSlices = slicesToAdd;
        } else if (padMode == PAD_HALF) {
            frontSlices = slicesToAdd / 2;
            backSlices = slicesToAdd / 2;

            if ((slicesToAdd % 2) == 1) {
                backSlices++;
            }
        }

        if (destImage == null) {
            destExtents = new int[3];
            destExtents[0] = srcImage.getExtents()[0];
            destExtents[1] = srcImage.getExtents()[1];
            destExtents[2] = paddedSlices;

            // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
            destImage = new ModelImage(srcImage.getType(), destExtents, srcImage.getImageName());
            calcInPlace = true;
        }

        // create  <FileInfoBase fileInfoBuffer;> buffer; may be of type FileInfoDicom
        try {

            if (srcImage.getNDims() == 4) {
                tDim = srcImage.getExtents()[3];
            } else {
                tDim = 1;
            }

            if (srcImage.isColorImage()) {
                imageBuffer = new float[4 * sliceArea];
                colorFactor = 4;
            } else {
                imageBuffer = new float[sliceArea];
                colorFactor = 1;
            }

            if (padMode == PAD_FRONT) {
                fireProgressStateChanged(srcImage.getImageName(), "Inserting front slices...");
            } else if (padMode == PAD_BACK) {
                fireProgressStateChanged(srcImage.getImageName(), "Inserting back slices...");
            } else if (padMode == PAD_HALF) {
                fireProgressStateChanged(srcImage.getImageName(), "Inserting front and back slices...");
            }
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            errorCleanUp("Algorithm Pad With Slices reports: Out of memory", true);

            return;
        }

        // make a location & view the progressbar; make length & increment of progressbar.


        for (t = 0; (t < tDim) && !threadStopped; t++) {
            tOldOffset = Xdim * Ydim * oldZdim * colorFactor * t;
            tNewOffset = Xdim * Ydim * paddedSlices * colorFactor * t;
            Z = 0; // start counting the slices of the destination image at the first slice.

            for (z = 0; (z < (oldZdim + 1)) && !threadStopped; z++) { // for all slices in the old image

                // let user know something is happening by updating the progressbar
                fireProgressStateChanged(Math.round(((float) ((t * oldZdim) + z)) / (oldZdim * tDim) * 100));

                if (((z == 0) && (frontSlices > 0)) || ((z == oldZdim) && (backSlices > 0))) {

                    if (z == 0) {
                        currentSlices = frontSlices;

                        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                            imagePositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(z))
                                                                       .parseTagValue("0020,0032"));
                            nextPositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(z + 1))
                                                                      .parseTagValue("0020,0032"));
                        }
                    } else {
                        currentSlices = backSlices;

                        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                            imagePositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(z - 1))
                                                                       .parseTagValue("0020,0032"));
                            lastPositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(z - 2))
                                                                      .parseTagValue("0020,0032"));
                        }
                    }

                    try {

                        if (srcImage.isColorImage()) {

                            for (i = 0; i < (4 * sliceArea); i++) {
                                imageBuffer[i] = 0.0f;
                            }

                            for (j = 0; j < currentSlices; j++) {
                                destImage.importData(tNewOffset + ((Z + j) * 4 * sliceArea), imageBuffer, false);
                            }
                        } else {

                            for (i = 0; i < sliceArea; i++) {
                                imageBuffer[i] = 0.0f;
                            }

                            for (j = 0; j < currentSlices; j++) {
                                destImage.importData(tNewOffset + ((Z + j) * sliceArea), imageBuffer, false);
                            }
                        }
                    } catch (IOException error) {
                        displayError("Algorithm PadWithSlices reports: Destination image already locked.");
                        setCompleted(false);

                        return;
                    }

                    // set file info for the slice.
                    // ... but do something special for DICOM images
                    // No DICOM for 4D
                    if (threadStopped) {
                        imageBuffer = null;
                        setCompleted(false);

                        finalize();

                        return;
                    }

                    for (j = 0; j < currentSlices; j++) {

                        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                            FileInfoDicom fileInfoBuffer; // buffer of type DICOM

                            if (z != oldZdim) {
                                fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(z).clone();
                            } else { // copy into buffer
                                fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(z - 1).clone();
                            } // copy into buffer

                            fileInfoBuffer.setExtents(destImage.getExtents()); // get the extents of this image and put
                                                                               // it in the new filebuffer

                            if (z == 0) {
                                fileInfoBuffer.getTagTable().setValue("0020,0032",
                                                                      (imagePositionCoords[0] +
                                                                       ((currentSlices - j) *
                                                                            (imagePositionCoords[0] -
                                                                                 nextPositionCoords[0]))) + "\\" +
                                                                      (imagePositionCoords[1] +
                                                                       ((currentSlices - j) *
                                                                            (imagePositionCoords[1] -
                                                                                 nextPositionCoords[1]))) + "\\" +
                                                                      (imagePositionCoords[2] +
                                                                       ((currentSlices - j) *
                                                                            (imagePositionCoords[2] -
                                                                                 nextPositionCoords[2]))),
                                                                      fileInfoBuffer.getTagTable().get("0020,0032").getLength());
                            } else if (z == oldZdim) {
                                fileInfoBuffer.getTagTable().setValue("0020,0032",
                                                                      (imagePositionCoords[0] +
                                                                       ((j + 1) *
                                                                            (imagePositionCoords[0] -
                                                                                 lastPositionCoords[0]))) + "\\" +
                                                                      (imagePositionCoords[1] +
                                                                       ((j + 1) *
                                                                            (imagePositionCoords[1] -
                                                                                 lastPositionCoords[1]))) + "\\" +
                                                                      (imagePositionCoords[2] +
                                                                       ((j + 1) *
                                                                            (imagePositionCoords[2] -
                                                                                 lastPositionCoords[2]))),
                                                                      fileInfoBuffer.getTagTable().get("0020,0032").getLength());
                            }


                            float[] starts = new float[3];

                            if (fileInfoBuffer.getImageOrientation() == FileInfoBase.AXIAL) {
                                starts[0] = fileInfoBuffer.xLocation;
                                starts[1] = fileInfoBuffer.yLocation;
                                starts[2] = fileInfoBuffer.zLocation;
                            } else if (fileInfoBuffer.getImageOrientation() == FileInfoBase.SAGITTAL) {
                                starts[0] = fileInfoBuffer.yLocation;
                                starts[1] = fileInfoBuffer.zLocation;
                                starts[2] = fileInfoBuffer.xLocation;
                            } else if (fileInfoBuffer.getImageOrientation() == FileInfoBase.CORONAL) {
                                starts[0] = fileInfoBuffer.xLocation;
                                starts[1] = fileInfoBuffer.zLocation;
                                starts[2] = fileInfoBuffer.yLocation;
                            } else {
                                starts[0] = 0;
                                starts[1] = 0;
                                starts[2] = 0;
                            }

                            fileInfoBuffer.setOrigin(starts);

                            // read just the slice location ("0020,1041")
                            // same change as image position above:
                            // Do an average here
                            if (z == 0) {
                                value = ((FileInfoDicom) (srcImage.getFileInfo(z + 1))).getTagTable().getValue("0020,1041");
                                s = ((String) value).trim();

                                try {
                                    nextSliceLocation = Float.valueOf(s).floatValue();
                                } catch (NumberFormatException e) {
                                    MipavUtil.displayError("Number format error: Slice Location (a) " + (z + 1) +
                                                           " = " + s);
                                    nextSliceLocation = 0;
                                }

                                value = ((FileInfoDicom) (srcImage.getFileInfo(z))).getTagTable().getValue("0020,1041");
                                s = ((String) value).trim();

                                try {
                                    sliceLocation = Float.valueOf(s).floatValue();
                                } catch (NumberFormatException e) {
                                    MipavUtil.displayError("Number format error: Slice Location (b) " + z + " = " + s);
                                    sliceLocation = 0;
                                }

                                sliceLocation = sliceLocation +
                                                ((currentSlices - j) * (sliceLocation - nextSliceLocation));
                                s = nf.format(sliceLocation);
                                fileInfoBuffer.getTagTable().setValue("0020,1041", s, s.length());
                            } else if (z == oldZdim) {
                                value = ((FileInfoDicom) (srcImage.getFileInfo(z - 2))).getTagTable().getValue("0020,1041");
                                s = ((String) value).trim();

                                try {
                                    lastSliceLocation = Float.valueOf(s).floatValue();
                                } catch (NumberFormatException e) {
                                    MipavUtil.displayError("Number format error: Slice Location (a) " + (z - 2) +
                                                           " = " + s);
                                    lastSliceLocation = 0;
                                }

                                value = ((FileInfoDicom) (srcImage.getFileInfo(z - 1))).getTagTable().getValue("0020,1041");
                                s = ((String) value).trim();

                                try {
                                    sliceLocation = Float.valueOf(s).floatValue();
                                } catch (NumberFormatException e) {
                                    MipavUtil.displayError("Number format error: Slice Location (b) " + (z - 1) +
                                                           " = " + s);
                                    sliceLocation = 0;
                                }

                                sliceLocation = sliceLocation + ((j + 1) * (sliceLocation - lastSliceLocation));
                                s = nf.format(sliceLocation);
                                fileInfoBuffer.getTagTable().setValue("0020,1041", s, s.length());
                            }

                            // change the instance number ("0020,0013"):
                            // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                            fileInfoBuffer.getTagTable().setValue("0020,0013", String.valueOf(Z + 1),
                                                                  fileInfoBuffer.getTagTable().get("0020,0013").getLength()); // Reset the image (slice) number with the new number ordering

                            destImage.setFileInfo(fileInfoBuffer, Z);
                        } else { // not a DICOM image, so these can be processed similarly

                            FileInfoBase fileInfoBuffer; // buffer of any old type

                            if (z != oldZdim) {
                                fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z).clone();
                            } else {
                                fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z - 1).clone();
                            }

                            if (z == 0) {
                                fileInfoBuffer.setOrigin((fileInfoBuffer.getOrigin(2) +
                                                          ((currentSlices - j) *
                                                               (fileInfoBuffer.getOrigin(2) -
                                                                    ((FileInfoBase)
                                                                             srcImage.getFileInfo((t * oldZdim) + z +
                                                                                                      1)).getOrigin(2)))),
                                                         2);
                            } else if (z == oldZdim) { // if (z == 0)
                                fileInfoBuffer.setOrigin((fileInfoBuffer.getOrigin(2) +
                                                          ((j + 1) *
                                                               (fileInfoBuffer.getOrigin(2) -
                                                                    ((FileInfoBase)
                                                                             srcImage.getFileInfo((t * oldZdim) + z -
                                                                                                      2)).getOrigin(2)))),
                                                         2);
                            }

                            fileInfoBuffer.setExtents(destImage.getExtents());

                            destImage.setFileInfo(fileInfoBuffer, ((t * paddedSlices) + Z));
                        }

                        Z++;
                    } // for (j = 0; j < currentSlices; j++)
                } // if ((( z == 0) && (frontSlices > 0)) || ((z == oldZdim) && (backSlices > 0)))

                if (threadStopped) {
                    imageBuffer = null;
                    setCompleted(false);

                    finalize();

                    return;
                }

                // copy over all the original slices
                if (z != oldZdim) {

                    try {

                        if (srcImage.isColorImage()) {
                            srcImage.exportData(tOldOffset + (z * 4 * sliceArea), 4 * sliceArea, imageBuffer);
                            destImage.importData(tNewOffset + (Z * 4 * sliceArea), imageBuffer, false);
                        } else {
                            srcImage.exportSliceXY((t * oldZdim) + z, imageBuffer);
                            destImage.importData(tNewOffset + (Z * sliceArea), imageBuffer, false);
                        }
                    } catch (IOException error) {
                        errorCleanUp("Algorithm Pad With Slices reports: Destination image already locked.", false);

                        return;
                    }

                    // set file info for the slice.
                    // ... but do something special for DICOM images
                    if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        FileInfoDicom fileInfoBuffer; // buffer of type DICOM
                        fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(z).clone(); // copy into buffer

                        fileInfoBuffer.setExtents(destImage.getExtents()); // get the extents of this image and put it
                                                                           // in the new filebuffer

                        float[] starts = new float[3];

                        if (fileInfoBuffer.getImageOrientation() == FileInfoBase.AXIAL) {
                            starts[0] = fileInfoBuffer.xLocation;
                            starts[1] = fileInfoBuffer.yLocation;
                            starts[2] = fileInfoBuffer.zLocation;
                        } else if (fileInfoBuffer.getImageOrientation() == FileInfoBase.SAGITTAL) {
                            starts[0] = fileInfoBuffer.yLocation;
                            starts[1] = fileInfoBuffer.zLocation;
                            starts[2] = fileInfoBuffer.xLocation;
                        } else if (fileInfoBuffer.getImageOrientation() == FileInfoBase.CORONAL) {
                            starts[0] = fileInfoBuffer.xLocation;
                            starts[1] = fileInfoBuffer.zLocation;
                            starts[2] = fileInfoBuffer.yLocation;
                        } else {
                            starts[0] = 0;
                            starts[1] = 0;
                            starts[2] = 0;
                        }

                        fileInfoBuffer.setOrigin(starts);

                        // change the slice number ("0020,0013"):
                        // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                        fileInfoBuffer.getTagTable().setValue("0020,0013", String.valueOf(Z + 1),
                                                              fileInfoBuffer.getTagTable().get("0020,0013").getLength()); // Reset the image (slice) number with the new number ordering

                        destImage.setFileInfo(fileInfoBuffer, Z);
                    } else { // not a DICOM image, so these can be processed similarly

                        FileInfoBase fileInfoBuffer; // buffer of any old type

                        fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z).clone();
                        fileInfoBuffer.setExtents(destImage.getExtents());
                        destImage.setFileInfo(fileInfoBuffer, (t * paddedSlices) + Z);
                    }

                    Z++; // next slice position in the new image.
                } // end of if (z != oldZdim)
            } // for (z = 0; z < (oldZdim+1); z++)
        } // for (t = 0; t < tDim; t++)

        if (threadStopped) {
            imageBuffer = null;
            setCompleted(false);

            finalize();

            return;
        }

        destImage.calcMinMax(); // calculate the minimum & maximum intensity values for the destImage-image

        if (calcInPlace) {

            // reset the extents of the srcImage, then reset the dataSize
            srcImage.setExtents(destExtents);
            srcImage.recomputeDataSize(); // this destroys the existing data in srcImage

            // import the result buffer from the resultImage into the srcImage
            // do this a slice at a time to conserve memory
            int resultSize = colorFactor * sliceArea;
            int numSlices = 1;
            int numTimes = 1;

            if (srcImage.getNDims() > 2) {
                numSlices = destExtents[2];
            }

            if (srcImage.getNDims() > 3) {
                numTimes = srcImage.getExtents()[3];
            }

            fireProgressStateChanged("Importing Image Data...");

            try {

                int index = 0;

                for (int time = 0; time < numTimes; time++) {

                    for (int slice = 0; slice < numSlices; slice++) {
                        destImage.exportDataNoLock(index, resultSize, imageBuffer);
                        srcImage.importData(index, imageBuffer, false);

                        // increment the index by the amount of data exported/imported
                        index += resultSize;
                    }
                }

                FileInfoBase[] newFileInfo = new FileInfoBase[numTimes * numSlices];
                srcImage.setFileInfo(newFileInfo);

                for (t = 0; t < tDim; t++) {

                    for (z = 0; z < paddedSlices; z++) {
                        FileInfoBase fileInfoBuffer; // buffer of any old type

                        fileInfoBuffer = (FileInfoBase) destImage.getFileInfo((t * paddedSlices) + z).clone();
                        srcImage.setFileInfo(fileInfoBuffer, (t * paddedSlices) + z);
                    }
                }

                srcImage.calcMinMax();
            } catch (OutOfMemoryError e) {
                destImage = null;
                System.gc();
                displayError("Algorithm Pad With Slices reports: Out of memory getting results.");
                srcImage.releaseLock();
                setCompleted(false);


                return;
            } catch (IOException e2) {
                destImage = null;
                System.gc();
                displayError(e2.getMessage());
                srcImage.releaseLock();
                setCompleted(false);


                return;
            }

            // Clean up and let the calling dialog know that algorithm did its job
            srcImage.releaseLock();

            destImage.disposeLocal(); // this was not here before... MEMORY LEAK!

            destImage = null; // after all, this was only temporary

        }

        // Clean up and let the calling dialog know that algorithm did its job

        setCompleted(true);
    }

    /**
     * Calculate the dimension value to power of 2.
     *
     * @param   dim  dimension value.
     *
     * @return  value dimension value in power of 2
     */
    private int dimPowerOfTwo(int dim) {

        if (dim <= 4) {
            return 4;
        } else if (dim <= 8) {
            return 8;
        } else if (dim <= 16) {
            return 16;
        } else if (dim <= 32) {
            return 32;
        } else if (dim <= 64) {
            return 64;
        } else if (dim <= 128) {
            return 128;
        } else if (dim <= 256) {
            return 256;
        } else if (dim <= 512) {
            return 512;
        } else if (dim <= 1024) {
            return 1024;
        } else if (dim <= 2048) {
            return 2048;
        } else if (dim <= 4096) {
            return 4096;
        } else if (dim <= 8192) {
            return 8192;
        } else if (dim <= 16384) {
            return 16384;
        } else if (dim <= 32768) {
            return 32768;
        } else {
            return 65536;
        }
    }

}
