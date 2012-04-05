package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;


/**
 * Algorithm to insert a slice. Operates with sliceType either AVERAGE or BLANK or ORGIINAL. If AVERAGE, the inserted
 * slice is set equal to the mean of the 2 surrounding slices unless it is an new start or end. A new start or end
 * simply preserves the usual slice spacing. In BLANK mode a slice with all pixels zero is inserted. In original a 2D
 * image of the same dimensions is inserted. Must insert black and white with black and white and color with color.
 */
public class AlgorithmInsertSlice extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Weighted average slice type. */
    public static final int WEIGHTED_AVERAGE = 0;

    /** Average slice type - the inserted slice is set equal to the mean of the 2 surrounding slices. */
    public static final int AVERAGE = 1;

    /** Blank slice type - the inserted slice is blank. */
    public static final int BLANK = 2;

    /** Original slice type - a 2D image is inserted. */
    public static final int ORIGINAL = 3;

    /** Copy adjacent slice. */
    public static final int ADJACENT_DOWN = 4;

    /** Copy adjacent slice. */
    public static final int ADJACENT_UP = 5;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Image inserted for slice type == ORIGINAL. */
    private ModelImage insertedImage;

    /** Number of slice before which another slice is inserted. */
    private int insertSlice;

    /** Original Z dimension of the image. */
    private int oldZdim;

    /** Area of a slice (Xdim * Ydim). */
    private int sliceArea;

    /** Insert an AVERAGE or BLANK image slice. */
    private int sliceType;

    /** For weighted averaging. */
    private float weightPrevious = 0.5f;

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
     * @param  insertSlice    number of slice before which another slice is inserted
     * @param  sliceType      AVERAGE or BLANK or ORIGINAL
     * @param  insertedImage  image inserted for sliceType == ORIGINAL
     */
    public AlgorithmInsertSlice(ModelImage srcImage, ModelImage destImage, int insertSlice, int sliceType,
                                ModelImage insertedImage) {
        super(destImage, srcImage);
        this.insertSlice = insertSlice;
        this.sliceType = sliceType;
        this.insertedImage = insertedImage;

        // get local attributes from this.srcImage
        Xdim = srcImage.getExtents()[0];
        Ydim = srcImage.getExtents()[1];
        sliceArea = Xdim * Ydim; // one slice has sliceArea number of pixels

        oldZdim = srcImage.getExtents()[2]; //
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
        int t;
        int z, Z; // z is slice-depth of srcImage; Z is slice-depth of destination
        float[] imageBuffer;
        float[] imageBuffer2;
        float[] imagePositionCoords = new float[3]; // image position along the XYZ-axis
        float[] lastPositionCoords = new float[3];
        float[] nextPositionCoords = new float[3];
        float sliceLocation = 0.0f;
        float lastSliceLocation = 0.0f;
        float nextSliceLocation = 0.0f;
        Object value;
        String s;
        int tDim;
        int colorFactor;
        int tOldOffset, tNewOffset;
        DecimalFormat nf;

        

        // create  <FileInfoBase fileInfoBuffer;> buffer; may be of type FileInfoDicom
        try {
            nf = new DecimalFormat("##0.000000");

            if (srcImage.getNDims() == 4) {
                tDim = srcImage.getExtents()[3];
            } else {
                tDim = 1;
            }

            if (srcImage.isColorImage()) {
                imageBuffer = new float[4 * sliceArea];
                colorFactor = 4;
            } else if (srcImage.isComplexImage()) {
            	imageBuffer = new float[2 * sliceArea];
            	colorFactor = 2;
            } else {
                imageBuffer = new float[sliceArea];
                colorFactor = 1;
            }

            if (sliceType == WEIGHTED_AVERAGE) {
                fireProgressStateChanged(srcImage.getImageName(), "Inserting Weighted Averaged Slice...");
            } else if (sliceType == AVERAGE) {
                fireProgressStateChanged(srcImage.getImageName(), "Inserting Averaged Slice...");
            } else if (sliceType == BLANK) {
                fireProgressStateChanged(srcImage.getImageName(), "Inserting Blank Slice...");
            } else if (sliceType == ORIGINAL) {
                fireProgressStateChanged(srcImage.getImageName(), "Inserting Original Slice...");
            } else if (sliceType == ADJACENT_DOWN) {
                fireProgressStateChanged(srcImage.getImageName(), "Copying previous slice...");
            } else if (sliceType == ADJACENT_UP) {
                fireProgressStateChanged(srcImage.getImageName(), "Copying next slice...");
            }
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            errorCleanUp("Algorithm Insert Slice reports: Out of memory", true);

            return;
        }

        // make a location & view the progressbar; make length & increment of progressbar.


        for (t = 0; (t < tDim) && !threadStopped; t++) {
            tOldOffset = Xdim * Ydim * oldZdim * colorFactor * t;
            tNewOffset = Xdim * Ydim * (oldZdim + 1) * colorFactor * t;
            Z = 0; // start counting the slices of the destination image at the first slice.

            for (z = 0; (z < (oldZdim + 1)) && !threadStopped; z++) { // for all slices in the old image

                // let user know something is happening by updating the progressbar
                fireProgressStateChanged(Math.round(((float) ((t * oldZdim) + z)) / (oldZdim * tDim) * 100));

                if (z == insertSlice) {

                    if (sliceType == WEIGHTED_AVERAGE) {

                        if (z == 0) {

                            try {
                            	srcImage.exportData(tOldOffset, colorFactor * sliceArea, imageBuffer);
                                destImage.importData(tNewOffset, imageBuffer, false);
                            } catch (IOException error) {
                                errorCleanUp("Algorithm InsertSlice reports: Destination image already locked.", false);

                                return;
                            }
                        } // end of if (z == 0)
                        else if (z == oldZdim) {

                            try {

                                destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer, false);
                            } catch (IOException error) {
                                displayError("Algorithm InsertSlice reports: Destination image already locked.");

                                return;
                            }
                        } // end of else if (z == oldZdim)
                        else {

                            try {
                            	imageBuffer2 = new float[colorFactor * sliceArea];
                                srcImage.exportData(tOldOffset + (z * colorFactor * sliceArea), 
                                		colorFactor * sliceArea, imageBuffer2);

                                for (i = 0; i < (colorFactor * sliceArea); i++) {
                                    imageBuffer[i] = (imageBuffer[i] * weightPrevious) +
                                                     (imageBuffer2[i] * (1.0f - weightPrevious));
                                }
                                imageBuffer2 = null;

                                destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer, false);
                            } catch (IOException error) {
                                imageBuffer = null;
                                imageBuffer2 = null;
                                errorCleanUp("Algorithm InsertSlice reports: Destination image already locked.", true);

                                return;
                            } catch (OutOfMemoryError e) {
                                imageBuffer = null;
                                imageBuffer2 = null;
                                errorCleanUp("Algorithm Insert Slice reports: Out of memory", true);

                                return;
                            }
                        }
                    } // if sliceType == WEIGHTED_AVERAGE
                    else if (sliceType == AVERAGE) {

                        if (z == 0) {

                            try {
                            	srcImage.exportData(tOldOffset, colorFactor * sliceArea, imageBuffer);
                                destImage.importData(tNewOffset, imageBuffer, false);
                            } catch (IOException error) {
                                errorCleanUp("Algorithm InsertSlice reports: Destination image already locked.", false);

                                return;
                            }
                        } // end of if (z == 0)
                        else if (z == oldZdim) {

                            try {
                            	destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer, false);
                            } catch (IOException error) {
                                displayError("Algorithm InsertSlice reports: Destination image already locked.");

                                return;
                            }
                        } // end of else if (z == oldZdim)
                        else {

                            try {
                            	imageBuffer2 = new float[colorFactor * sliceArea];
                                srcImage.exportData(tOldOffset + (z * colorFactor * sliceArea), 
                                		colorFactor * sliceArea, imageBuffer2);

                                for (i = 0; i < (colorFactor * sliceArea); i++) {
                                    imageBuffer[i] = (imageBuffer[i] + imageBuffer2[i]) / 2.0f;
                                }
                                imageBuffer2 = null;

                                destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer, false);
                            } catch (IOException error) {
                                imageBuffer = null;
                                imageBuffer2 = null;
                                errorCleanUp("Algorithm InsertSlice reports: Destination image already locked.", true);

                                return;
                            } catch (OutOfMemoryError e) {
                                imageBuffer = null;
                                imageBuffer2 = null;
                                errorCleanUp("Algorithm Insert Slice reports: Out of memory", true);

                                return;
                            }
                        }
                    } // if (sliceType == AVERAGE)
                    else if (sliceType == ADJACENT_DOWN) {

                        if (z == 0) {

                            try {
                            	srcImage.exportData(tOldOffset, colorFactor * sliceArea, imageBuffer);
                                destImage.importData(tNewOffset, imageBuffer, false);
                            } catch (IOException error) {
                                errorCleanUp("Algorithm InsertSlice reports: Destination image already locked.", false);

                                return;
                            }
                        } // end of if (z == 0)
                        else if (z == oldZdim) {

                            try {
                            	destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer, false);
                            } catch (IOException error) {
                                displayError("Algorithm InsertSlice reports: Destination image already locked.");

                                return;
                            }
                        } // end of else if (z == oldZdim)
                        else {

                            try {
                            	// imageBuffer2 = new float[colorFactor*sliceArea];
                                // srcImage.exportData(tOldOffset + z*colorFactor*sliceArea,colorFactor*sliceArea,imageBuffer2);
                                // for (i = 0; i < colorFactor*sliceArea; i++) {
                                // imageBuffer[i] = imageBuffer[i];
                                // }
                                destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer, false);
                            } catch (IOException error) {
                                imageBuffer = null;
                                imageBuffer2 = null;
                                errorCleanUp("Algorithm InsertSlice reports: Destination image already locked.", true);

                                return;
                            } catch (OutOfMemoryError e) {
                                imageBuffer = null;
                                imageBuffer2 = null;
                                errorCleanUp("Algorithm Insert Slice reports: Out of memory", true);

                                return;
                            }
                        }
                    } // if (sliceType == ADJACENT_DOWN)
                    else if (sliceType == ADJACENT_UP) {

                        if (z == 0) {

                            try {
                            	srcImage.exportData(tOldOffset, colorFactor * sliceArea, imageBuffer);
                                destImage.importData(tNewOffset, imageBuffer, false);
                            } catch (IOException error) {
                                errorCleanUp("Algorithm InsertSlice reports: Destination image already locked.", false);

                                return;
                            }
                        } // end of if (z == 0)
                        else if (z == oldZdim) {

                            try {
                            	destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer, false);
                            } catch (IOException error) {
                                displayError("Algorithm InsertSlice reports: Destination image already locked.");

                                return;
                            }
                        } // end of else if (z == oldZdim)
                        else {

                            try {
                            	imageBuffer2 = new float[colorFactor * sliceArea];
                                srcImage.exportData(tOldOffset + (z * colorFactor * sliceArea), 
                                		colorFactor * sliceArea, imageBuffer2);

                                // for (i = 0; i < colorFactor*sliceArea; i++) {
                                // imageBuffer[i] = imageBuffer2[i];
                                // }
                                destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer2, false);
                                imageBuffer2 = null;
                            } catch (IOException error) {
                                imageBuffer = null;
                                imageBuffer2 = null;
                                errorCleanUp("Algorithm InsertSlice reports: Destination image already locked.", true);

                                return;
                            } catch (OutOfMemoryError e) {
                                imageBuffer = null;
                                imageBuffer2 = null;
                                errorCleanUp("Algorithm Insert Slice reports: Out of memory", true);

                                return;
                            }
                        }
                    } // if (sliceType == ADJACENT_UP)
                    else if (sliceType == BLANK) {

                        try {
                        	for (i = 0; i < (colorFactor * sliceArea); i++) {
                                imageBuffer[i] = 0.0f;
                            }

                            destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer, false);
                        } catch (IOException error) {
                            displayError("Algorithm InsertSlice reports: Destination image already locked.");
                            setCompleted(false);

                            return;
                        }
                    } // else if (sliceType == BLANK)
                    else { // sliceType == ORIGINAL

                        try {
                            insertedImage.exportData(0, imageBuffer.length, imageBuffer);
                        } catch (IOException error) {
                            displayError("Algorithm InsertSlice: Inserted image locked");
                            setCompleted(false);

                            return;
                        }

                        try {
                        	destImage.importData(tNewOffset + (z * colorFactor * sliceArea), imageBuffer, false);
                        } catch (IOException error) {
                            displayError("Algorithm InsertSlice reports: Destination image already locked.");
                            setCompleted(false);

                            return;
                        }
                    } // else sliceType == ORIGINAL

                    // set file info for the slice.
                    // ... but do something special for DICOM images
                    // No DICOM for 4D
                    if (threadStopped) {
                        imageBuffer = null;
                        imageBuffer2 = null;
                        setCompleted(false);

                        finalize();

                        return;
                    }

                    if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        FileInfoDicom fileInfoBuffer; // buffer of type DICOM

                        if (z != oldZdim) {
                            fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(z).clone();
                        } // copy into buffer
                        else {
                            fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(z - 1).clone();
                        } // copy into buffer

                        fileInfoBuffer.setExtents(destImage.getExtents()); // get the extents of this image and put it
                                                                           // in the new filebuffer

                        // change the instance number ("0020,0013"):
                        // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                        fileInfoBuffer.getTagTable().setValue("0020,0013", String.valueOf(z + 1),
                                                              fileInfoBuffer.getTagTable().get("0020,0013").getLength()); // Reset the image (slice) number with the new number ordering

                        // read just the image position ("0020,0032"): copy the "image position (patient)" info so that
                        // axis-position added by inserting slice (z) will change by sliding the next included slice
                        // backward (to the Z position). So: the X&Y info for the z (the one not removed with the equal
                        // or one lower index) and we use the Z info from the index of the copy-to slice (which has an
                        // equal or one higher index).
                        if (z != oldZdim) {
                            imagePositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(z))
                                                                       .parseTagValue("0020,0032"));
                        } else {
                            imagePositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(z - 1))
                                                                       .parseTagValue("0020,0032"));
                        }

                        if (z == 0) {
                            if (((FileInfoDicom)(srcImage.getFileInfo(z+1))).getTagTable().getValue("0020,0032") != null) {
                                nextPositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(z + 1))
                                                                          .parseTagValue("0020,0032"));
                                fileInfoBuffer.getTagTable().setValue("0020,0032",
                                                                      ((2.0f * imagePositionCoords[0]) -
                                                                       nextPositionCoords[0]) + "\\" +
                                                                      ((2.0f * imagePositionCoords[1]) -
                                                                       nextPositionCoords[1]) + "\\" +
                                                                      ((2.0f * imagePositionCoords[2]) -
                                                                       nextPositionCoords[2]),
                                                                      fileInfoBuffer.getTagTable().get("0020,0032").getLength());
                            } // if (((FileInfoDicom)(srcImage.getFileInfo(z+1))).getTagTable().getValue("0020,0032") != null
                        } else if (z == oldZdim) {
                            if (((FileInfoDicom)(srcImage.getFileInfo(z-2))).getTagTable().getValue("0020,0032") != null) {
                                lastPositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(z - 2))
                                                                          .parseTagValue("0020,0032"));
                                fileInfoBuffer.getTagTable().setValue("0020,0032",
                                                                      ((2.0f * imagePositionCoords[0]) -
                                                                       lastPositionCoords[0]) + "\\" +
                                                                      ((2.0f * imagePositionCoords[1]) -
                                                                       lastPositionCoords[1]) + "\\" +
                                                                      ((2.0f * imagePositionCoords[2]) -
                                                                       lastPositionCoords[2]),
                                                                      fileInfoBuffer.getTagTable().get("0020,0032").getLength());
                            } // if (((FileInfoDicom)(srcImage.getFileInfo(z-2))).getTagTable().getValue("0020,0032") != null)
                        } else {
                            if (fileInfoBuffer.getTagTable().getValue("0020,0032") != null) {
                                fileInfoBuffer.getTagTable().setValue("0020,0032",
                                                                      ((imagePositionCoords[0] + lastPositionCoords[0]) /
                                                                       2.0f) + "\\" +
                                                                      ((imagePositionCoords[1] + lastPositionCoords[1]) /
                                                                       2.0f) + "\\" +
                                                                      ((imagePositionCoords[2] + lastPositionCoords[2]) /
                                                                       2.0f),
                                                                      fileInfoBuffer.getTagTable().get("0020,0032").getLength());
                            } // if (fileInfoBuffer.getTagTable().getValue("0020,0032") != null)
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
                            if (((FileInfoDicom) (srcImage.getFileInfo(z + 1))).getTagTable().getValue("0020,1041") != null) {
                                value = ((FileInfoDicom) (srcImage.getFileInfo(z + 1))).getTagTable().getValue("0020,1041");
                                s = ((String) value).trim();
    
                                try {
                                    nextSliceLocation = Float.valueOf(s).floatValue();
                                } catch (NumberFormatException e) {
                                    MipavUtil.displayError("Number format error: Slice Location (a) " + (z + 1) + " = " +
                                                           s);
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
    
                                sliceLocation = (2.0f * sliceLocation) - nextSliceLocation;
                                s = nf.format(sliceLocation);
                                fileInfoBuffer.getTagTable().setValue("0020,1041", s, s.length());
                            } // if (((FileInfoDicom) (srcImage.getFileInfo(z + 1))).getTagTable().getValue("0020,1041") != null)
                        } else if (z == oldZdim) {
                            if (((FileInfoDicom)(srcImage.getFileInfo(z-2))).getTagTable().getValue("0020,1041") != null) {
                                value = ((FileInfoDicom) (srcImage.getFileInfo(z - 2))).getTagTable().getValue("0020,1041");
                                s = ((String) value).trim();
    
                                try {
                                    lastSliceLocation = Float.valueOf(s).floatValue();
                                } catch (NumberFormatException e) {
                                    MipavUtil.displayError("Number format error: Slice Location (a) " + (z - 2) + " = " +
                                                           s);
                                    lastSliceLocation = 0;
                                }
    
                                value = ((FileInfoDicom) (srcImage.getFileInfo(z - 1))).getTagTable().getValue("0020,1041");
                                s = ((String) value).trim();
    
                                try {
                                    sliceLocation = Float.valueOf(s).floatValue();
                                } catch (NumberFormatException e) {
                                    MipavUtil.displayError("Number format error: Slice Location (b) " + (z - 1) + " = " +
                                                           s);
                                    sliceLocation = 0;
                                }
    
                                sliceLocation = (2.0f * sliceLocation) - lastSliceLocation;
                                s = nf.format(sliceLocation);
                                fileInfoBuffer.getTagTable().setValue("0020,1041", s, s.length());
                            } // if (((FileInfoDicom)(srcImage.getFileInfo(z-2))).getTagTable().getValue("0020,1041") != null)
                        } else {
                            if (((FileInfoDicom)(srcImage.getFileInfo(z-1))).getTagTable().getValue("0020,1041") != null) {
                                value = ((FileInfoDicom) (srcImage.getFileInfo(z - 1))).getTagTable().getValue("0020,1041");
                                s = ((String) value).trim();
    
                                try {
                                    lastSliceLocation = Float.valueOf(s).floatValue();
                                } catch (NumberFormatException e) {
                                    MipavUtil.displayError("Number format error: Slice Location (a) " + (z - 1) + " = " +
                                                           s);
                                    lastSliceLocation = 0;
                                }
    
                                value = ((FileInfoDicom) (srcImage.getFileInfo(z))).getTagTable().getValue("0020,1041");
                                s = ((String) value).trim();
    
                                try {
                                    sliceLocation = Float.valueOf(s).floatValue();
                                } catch (NumberFormatException e) {
                                    MipavUtil.displayError("Number format error: Slice Location (b) " + z + " = " + s);
                                    sliceLocation = 0;
                                }
    
                                sliceLocation = (lastSliceLocation + sliceLocation) / 2.0f;
                                s = nf.format(sliceLocation);
                                fileInfoBuffer.getTagTable().setValue("0020,1041", s, s.length());
                            } // if (((FileInfoDicom)(srcImage.getFileInfo(z-1))).getTagTable().getValue("0020,1041") != null)
                        }

                        destImage.setFileInfo(fileInfoBuffer, z);
                    } else { // not a DICOM image, so these can be processed similarly

                        FileInfoBase fileInfoBuffer; // buffer of any old type

                        if (z != oldZdim) {
                            fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z).clone();
                        } else {
                            fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z - 1).clone();
                        }

                        fileInfoBuffer.setExtents(destImage.getExtents());

                        if (z == 0) {
                            fileInfoBuffer.setOrigin((-((FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z + 1))
                                                          .getOrigin(2) + (2.0f * fileInfoBuffer.getOrigin(2))), 2);
                        } // if (z == 0)
                        else if (z == oldZdim) {
                            fileInfoBuffer.setOrigin(((2.0f *
                                                           ((FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z - 1))
                                                           .getOrigin(2)) -
                                                      ((FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z - 2))
                                                          .getOrigin(2)), 2);

                        } else {
                            fileInfoBuffer.setOrigin((((FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z - 1))
                                                          .getOrigin(2) + fileInfoBuffer.getOrigin(2)) / 2, 2);
                        }

                        destImage.setFileInfo(fileInfoBuffer, ((t * (oldZdim + 1)) + z));
                    }

                    Z++;
                } // end of if (z == insertSlice)

                if (threadStopped) {
                    imageBuffer = null;
                    imageBuffer2 = null;
                    setCompleted(false);

                    finalize();

                    return;
                }

                // copy over all the original slices
                if (z != oldZdim) {

                    try {
                    	srcImage.exportData(tOldOffset + (z * colorFactor * sliceArea), colorFactor * sliceArea, imageBuffer);
                        destImage.importData(tNewOffset + (Z * colorFactor * sliceArea), imageBuffer, false);
                    } catch (IOException error) {
                        errorCleanUp("Algorithm InsertSlice reports: Destination image already locked.", false);

                        return;
                    }

                    // set file info for the slice.
                    // ... but do something special for DICOM images
                    if ((srcImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        FileInfoDicom fileInfoBuffer; // buffer of type DICOM
                        fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(z).clone(); // copy into buffer

                        fileInfoBuffer.setExtents(destImage.getExtents()); // get the extents of this image and put it
                                                                           // in the new filebuffer

                        // change the slice number ("0020,0013"):
                        // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                        fileInfoBuffer.getTagTable().setValue("0020,0013", String.valueOf(Z + 1),
                                                              fileInfoBuffer.getTagTable().get("0020,0013").getLength()); // Reset the image (slice) number with the new number ordering

                        // readjust the image position ("0020,0032"): copy the "image position (patient)" info so that
                        // axis-position added by inserting slice (z) will change by sliding the next included slice
                        // backward (to the Z position). So: the X&Y info for the z (the one not removed with the equal
                        // or one lower index) and we use the Z info from the index of the copy-to slice (which has an
                        // equal or one higher index).
                        if (((FileInfoDicom)(srcImage.getFileInfo(z))).getTagTable().getValue("0020,0032") != null) {
                            imagePositionCoords = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(z)).parseTagValue("0020,0032"));

                            for (i = 0; i < 3; i++) {
                                lastPositionCoords[i] = imagePositionCoords[i];
                            }
    
                            fileInfoBuffer.getTagTable().setValue("0020,0032",
                                                                  imagePositionCoords[0] + "\\" + imagePositionCoords[1] +
                                                                  "\\" + imagePositionCoords[2],
                                                                  fileInfoBuffer.getTagTable().get("0020,0032").getLength());
                        } // if (((FileInfoDicom)(srcImage.getFileInfo(z))).getTagTable().getValue("0020,0032") != null)

                        // readjust the slice location ("0020,1041")
                        // same change as image position above:
                        if (((FileInfoDicom)(srcImage.getFileInfo(z))).getTagTable().getValue("0020,1041") != null) {
                        fileInfoBuffer.getTagTable().setValue("0020,1041",
                                                              ((FileInfoDicom) (srcImage.getFileInfo(z))).getTagTable().getValue("0020,1041"),
                                                              fileInfoBuffer.getTagTable().get("0020,1041").getLength());
                        }

                        destImage.setFileInfo(fileInfoBuffer, Z);
                    } else { // not a DICOM image, so these can be processed similarly

                        FileInfoBase fileInfoBuffer; // buffer of any old type

                        fileInfoBuffer = (FileInfoBase) srcImage.getFileInfo((t * oldZdim) + z).clone();
                        fileInfoBuffer.setExtents(destImage.getExtents());
                        destImage.setFileInfo(fileInfoBuffer, (t * (oldZdim + 1)) + Z);
                    }

                    Z++; // next slice position in the new image.
                } // end of if (z != oldZdim)
                System.gc();
            } // for (z = 0; z < (oldZdim+1); z++)
        } // for (t = 0; t < tDim; t++)

        if (threadStopped) {
            imageBuffer = null;
            imageBuffer2 = null;
            setCompleted(false);

            finalize();

            return;
        }

        destImage.calcMinMax(); // calculate the minimum & maximum intensity values for the destImage-image

        // Clean up and let the calling dialog know that algorithm did its job

        setCompleted(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  wp  DOCUMENT ME!
     */
    public void setWeightPrevious(float wp) {
        this.weightPrevious = wp;
    }

}
