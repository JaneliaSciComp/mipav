package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;


/**
 * This algorithm is used to insert averaged slices where slices have been removed -If slices were removed at the
 * beginning of the original movie, these slices will not be replaced. - Slices are only inserted between the first and
 * last kept slices
 *
 * @author  Ben Link
 */
public class AlgorithmReplaceRemovedSlices extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean destFlag = false;

    /** DOCUMENT ME! */
    private float imageMin;

    /** If true, insert a blank rather than a weighted average. */
    private boolean insertBlank;

    /** DOCUMENT ME! */
    private boolean isDicom;

    /** DOCUMENT ME! */
    private boolean isSplit = false;

    /**
     * every false value in the array corresponds to a slice within the movie passed in. true values represent removed
     * slices
     */
    private boolean[] removedSlices = null; // shows which slices were removed from the original.

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for algorithm.
     *
     * @param  srcImage       the source image
     * @param  removedSlices  an array showing which slices were removed
     * @param  isSplit        DOCUMENT ME!
     * @param  destFlag       DOCUMENT ME!
     * @param  insertBlank    If true, insert a blank rather than a weighted average
     */
    public AlgorithmReplaceRemovedSlices(ModelImage srcImage, boolean[] removedSlices, boolean isSplit,
                                         boolean destFlag, boolean insertBlank) {
        super(null, srcImage);
        this.removedSlices = removedSlices;
        this.isSplit = isSplit;
        this.destFlag = destFlag;
        this.insertBlank = insertBlank;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Local cleanup.
     */
    public void disposeLocal() {

        // resultImage = null;
        removedSlices = null;
        System.gc();
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * Retrieves the result image.
     *
     * @return  resultImage the new image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Function called by thread to run the algorithm.
     */
    public void runAlgorithm() {
        int i, j, k, m, p;
        float[] newOrg2;
        int consecutiveRemove;
        float deltaOrg2;
        float[][] imagePositionCoords = null;
        float deltaPos;
        DecimalFormat nf;
        Object obj;
        String s;
        float[] sliceLocation = null;
        float deltaSliceLoc;
        boolean ignoreSliceLocation = false;
        boolean ignoreImagePositionCoords = false;

        if (srcImage == null) {
            displayError("AlgorithmReplaceRemovedSlices.run(): Source Image is null");

            return;
        } else if (srcImage.getExtents().length != 3) {
            displayError("Image must be 3 dimensional");

            return;
        }

        if (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM) {
            isDicom = true;
        } else {
            isDicom = false;
        }

        nf = new DecimalFormat("##0.000000");

        int[] destExtents = new int[3];
        destExtents[0] = srcImage.getExtents()[0];
        destExtents[1] = srcImage.getExtents()[1];
        srcImage.calcMinMax();
        imageMin = (float) srcImage.getMin();

        // System.err.println("Length of removedSlices array: " + removedSlices.length);

        if (!isSplit) {

            // for (int i = 0; i < removedSlices.length; i++) {
            // System.err.println(i + "th slice was removed: " + removedSlices[i]);

            // }

            int firstSlice = -1;
            int lastSlice = -1;
            boolean foundStart = false;

            // find the first good and last good (kept) slices
            for (i = 0; i < removedSlices.length; i++) {

                if (!foundStart && (removedSlices[i] == false)) {
                    foundStart = true;
                    firstSlice = i;
                } else if (foundStart && (removedSlices[i] == false)) {
                    lastSlice = i;
                }
            }

            if ((firstSlice == -1) || (lastSlice == -1)) {
                System.err.println("Could not find first/last slices");
                setCompleted(false);
                notifyListeners(this);

                return;
            }

            // System.err.println("first slice from removedSlices[] " + (firstSlice+1) + " last slice " + (lastSlice+1));

            // determine new # of slices:  only replace slices found after the first good (unremoved)
            // slice from the original image
            destExtents[2] = (lastSlice - firstSlice) + 1;

            // System.err.println("New extents for " + srcImage.getImageName() + ": " + destExtents[2]);

            newOrg2 = new float[destExtents[2]];

            if (isDicom) {
                imagePositionCoords = new float[destExtents[2]][3];
                sliceLocation = new float[destExtents[2]];
            }

            consecutiveRemove = 0;

            for (i = firstSlice, j = 0, m = 0; i <= lastSlice; i++, j++) {

                if (!removedSlices[i]) {
                    newOrg2[j] = srcImage.getFileInfo()[m].getOrigin()[2];

                    if (isDicom) {
                        imagePositionCoords[j] = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(m))
                                                                      .parseTagValue("0020,0032"));

                        if (imagePositionCoords[j] == null) {
                            ignoreImagePositionCoords = true;
                        }

                        obj = ((FileInfoDicom) (srcImage.getFileInfo(m))).getTagTable().getValue("0020,1041");

                        if (obj != null) {
                            s = ((String) obj).trim();

                            try {
                                sliceLocation[j] = Float.valueOf(s).floatValue();
                            } catch (NumberFormatException e) {
                                Preferences.debug("Number format error: slice location " + (i + 1) + " = " + s + "\n", 
                                		Preferences.DEBUG_ALGORITHM);
                            }
                        } // if (obj != null)
                        else {
                            ignoreSliceLocation = true;
                        }
                    }

                    m++;

                    if (consecutiveRemove > 0) {
                        deltaOrg2 = (newOrg2[j] - newOrg2[j - consecutiveRemove - 1]) / (consecutiveRemove + 1.0f);

                        for (k = 0; k < consecutiveRemove; k++) {
                            newOrg2[j - consecutiveRemove + k] = newOrg2[j - consecutiveRemove - 1] +
                                                                 ((k + 1) * deltaOrg2);
                        }

                        if (isDicom) {

                            if (!ignoreImagePositionCoords) {

                                for (p = 0; p < 3; p++) {
                                    deltaPos = (imagePositionCoords[j][p] -
                                                imagePositionCoords[j - consecutiveRemove - 1][p]) /
                                                   (consecutiveRemove + 1.0f);

                                    for (k = 0; k < consecutiveRemove; k++) {
                                        imagePositionCoords[j - consecutiveRemove + k][p] = imagePositionCoords[j -
                                                                                                                consecutiveRemove -
                                                                                                                1][p] +
                                                                                            ((k + 1) * deltaPos);
                                    }
                                }
                            } // if (!ignoreImagePositionCoords)

                            if (!ignoreSliceLocation) {
                                deltaSliceLoc = (sliceLocation[j] - sliceLocation[j - consecutiveRemove - 1]) /
                                                    (consecutiveRemove + 1.0f);

                                for (k = 0; k < consecutiveRemove; k++) {
                                    sliceLocation[j - consecutiveRemove + k] = sliceLocation[j - consecutiveRemove - 1] +
                                                                               ((k + 1) * deltaSliceLoc);
                                }
                            } // if (!ignoreLocationLocation)
                        } // (isDicom)

                        consecutiveRemove = 0;
                    } // if (consecutiveRemove > 0)
                } else {
                    consecutiveRemove++;
                }
            } // for (i = firstSlice, j = 0, m = 0; i <= lastSlice; i++, j++)

            resultImage = new ModelImage(srcImage.getType(), destExtents, srcImage.getImageName() + "_replaced_slices");

            int sliceArea = destExtents[0] * destExtents[1];
            float[] imageBuffer = null, imageBuffer2 = null;
            int colorFactor = 1;

            if (srcImage.isColorImage()) {
                colorFactor = 4;
            }
            else if (srcImage.isComplexImage()) {
            	colorFactor = 2;
            }

            // create two image buffers to hold a slice each
            imageBuffer = new float[colorFactor * sliceArea];
            imageBuffer2 = new float[imageBuffer.length];

            int srcIndex = 0;
            int resultIndex = 0;

            if (insertBlank) {
                fireProgressStateChanged(resultImage.getImageName(), "Inserting Blank Slice...");
            } else {
                fireProgressStateChanged(resultImage.getImageName(), "Inserting Weighted Averaged Slice...");
            }

            fireProgressStateChanged(0);


            int length = resultImage.getExtents()[2];

            // copy in all the unaveraged slices into their correct positions
            for (int x = firstSlice; x <= lastSlice; x++, resultIndex++) {

                if (removedSlices[x] == false) {

                    // System.err.println("Copying in from srcIndex: " + srcIndex + " to resultIndex: " + resultIndex);
                    try {
                        srcImage.exportData(srcIndex * imageBuffer.length, imageBuffer.length, imageBuffer);
                        resultImage.importData(resultIndex * imageBuffer.length, imageBuffer, false);
                    } catch (Exception ex) {
                        System.err.println("Error copying in original slices into result image: " + ex.toString());
                    }

                    srcIndex++;
                }

                fireProgressStateChanged((int) ((((float) resultIndex + 1.0f) / (float) length) * 50.0f));
            }

            // System.err.println("finished copying original slices into result image");

            foundStart = false;

            int start = 0;
            resultIndex = 0;

            // now run through and insert the averaged slices
            for (int x = firstSlice; x <= lastSlice; x++, resultIndex++) {

                if (!foundStart && (removedSlices[x] == true)) {
                    foundStart = true;
                    start = resultIndex - 1;
                    // System.err.println("start slice for insertion found: " + (start+1));
                } else if (foundStart && (removedSlices[x] == false)) {

                    // System.err.println("end slice for insertion found: " + (resultIndex+1));
                    if (insertBlank) {
                        insertBlankSlices(imageBuffer, start, resultIndex);
                    } else {
                        insertAveragedSlices(imageBuffer, imageBuffer2, start, resultIndex);
                    }

                    foundStart = false;
                }

                fireProgressStateChanged((int) ((((float) resultIndex + 1.0f) / (float) length) * 50.0f) + 50);
            }

            imageBuffer = null;
            imageBuffer2 = null;
        } else {
            // image was previously split so keep the same extents and copy first good slice into previous slices
            // and last good slice into next slices

            int firstSlice = -1;
            int lastSlice = -1;
            boolean foundStart = false;

            // find the first good and last good (kept) slices
            for (i = 0; i < removedSlices.length; i++) {

                if (!foundStart && (removedSlices[i] == false)) {
                    foundStart = true;
                    firstSlice = i;
                } else if (foundStart && (removedSlices[i] == false)) {
                    lastSlice = i;
                }
            }

            int[] extents = srcImage.getExtents();

            extents[2] = removedSlices.length;

            // System.err.println("ExtentsZ: " + extents[2]);

            newOrg2 = new float[extents[2]];

            if (isDicom) {
                imagePositionCoords = new float[extents[2]][3];
                sliceLocation = new float[extents[2]];
            }

            for (i = 0; i < firstSlice; i++) {
                newOrg2[i] = srcImage.getFileInfo()[0].getOrigin()[2];

                if (isDicom) {
                    imagePositionCoords[i] = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(0)).parseTagValue("0020,0032"));

                    if (imagePositionCoords[i] == null) {
                        ignoreImagePositionCoords = true;
                    }

                    obj = ((FileInfoDicom) (srcImage.getFileInfo(0))).getTagTable().getValue("0020,1041");

                    if (obj != null) {
                        s = ((String) obj).trim();

                        try {
                            sliceLocation[i] = Float.valueOf(s).floatValue();
                        } catch (NumberFormatException e) {
                            Preferences.debug("Number format error: slice location 1 = " + s + "\n", Preferences.DEBUG_ALGORITHM);
                        }
                    } // if (obj != null)
                    else {
                        ignoreSliceLocation = true;
                    }
                }
            }

            consecutiveRemove = 0;

            for (i = firstSlice, m = 0; i <= lastSlice; i++) {

                if (!removedSlices[i]) {
                    newOrg2[i] = srcImage.getFileInfo()[m].getOrigin()[2];

                    if (isDicom) {
                        imagePositionCoords[i] = convertIntoFloat(((FileInfoDicom) srcImage.getFileInfo(m))
                                                                      .parseTagValue("0020,0032"));

                        if (imagePositionCoords[i] == null) {
                            ignoreImagePositionCoords = true;
                        }

                        obj = ((FileInfoDicom) (srcImage.getFileInfo(m))).getTagTable().getValue("0020,1041");

                        if (obj != null) {
                            s = ((String) obj).trim();

                            try {
                                sliceLocation[i] = Float.valueOf(s).floatValue();
                            } catch (NumberFormatException e) {
                                Preferences.debug("Number format error: slice location " + (i + 1) + " = " + s + "\n", 
                                		Preferences.DEBUG_ALGORITHM);
                            }
                        } else {
                            ignoreSliceLocation = true;
                        }
                    }

                    m++;

                    if (consecutiveRemove > 0) {
                        deltaOrg2 = (newOrg2[i] - newOrg2[i - consecutiveRemove - 1]) / (consecutiveRemove + 1.0f);

                        for (k = 0; k < consecutiveRemove; k++) {
                            newOrg2[i - consecutiveRemove + k] = newOrg2[i - consecutiveRemove - 1] +
                                                                 ((k + 1) * deltaOrg2);
                        }

                        if (isDicom) {

                            if (!ignoreImagePositionCoords) {

                                for (p = 0; p < 3; p++) {
                                    deltaPos = (imagePositionCoords[i][p] -
                                                imagePositionCoords[i - consecutiveRemove - 1][p]) /
                                                   (consecutiveRemove + 1.0f);

                                    for (k = 0; k < consecutiveRemove; k++) {
                                        imagePositionCoords[i - consecutiveRemove + k][p] = imagePositionCoords[i -
                                                                                                                consecutiveRemove -
                                                                                                                1][p] +
                                                                                            ((k + 1) * deltaPos);
                                    }
                                }
                            } // if (!ignoreImagePositionCoords)

                            if (!ignoreSliceLocation) {
                                deltaSliceLoc = (sliceLocation[i] - sliceLocation[i - consecutiveRemove - 1]) /
                                                    (consecutiveRemove + 1.0f);

                                for (k = 0; k < consecutiveRemove; k++) {
                                    sliceLocation[i - consecutiveRemove + k] = sliceLocation[i - consecutiveRemove - 1] +
                                                                               ((k + 1) * deltaSliceLoc);
                                }
                            } // if (!ignoreSliceLocation)
                        } // (isDicom)

                        consecutiveRemove = 0;
                    } // if (consecutiveRemove > 0)
                } else {
                    consecutiveRemove++;
                }
            } // for (i = firstSlice, j = 0, m = 0; i <= lastSlice; i++, j++)

            for (i = lastSlice + 1; i < extents[2]; i++) {
                newOrg2[i] = srcImage.getFileInfo()[srcImage.getExtents()[2] - 1].getOrigin()[2];

                if (isDicom) {
                    imagePositionCoords[i] = convertIntoFloat(((FileInfoDicom)
                                                                   srcImage.getFileInfo(srcImage.getExtents()[2] - 1))
                                                                  .parseTagValue("0020,0032"));

                    if (imagePositionCoords[i] == null) {
                        ignoreImagePositionCoords = true;
                    }

                    obj = ((FileInfoDicom) (srcImage.getFileInfo(srcImage.getExtents()[2] - 1))).getTagTable().getValue("0020,1041");

                    if (obj != null) {
                        s = ((String) obj).trim();

                        try {
                            sliceLocation[i] = Float.valueOf(s).floatValue();
                        } catch (NumberFormatException e) {
                            Preferences.debug("Number format error: slice location " + (srcImage.getExtents()[2] - 1) +
                                              " = " + s + "\n", Preferences.DEBUG_ALGORITHM);
                        }
                    } // if (obj != null)
                    else {
                        ignoreSliceLocation = true;
                    }
                }
            }

            resultImage = new ModelImage(srcImage.getType(), extents, srcImage.getImageName() + "_replaced_slices");


            if (insertBlank) {
                fireProgressStateChanged(resultImage.getImageName(), "Inserting Blank Slice...");
            } else {
                fireProgressStateChanged(resultImage.getImageName(), "Inserting Weighted Averaged Slice...");
            }

            fireProgressStateChanged(0);


            int sliceArea = extents[0] * extents[1];
            float[] imageBuffer = null, imageBuffer2 = null;
            int colorFactor = 1;

            if (srcImage.isColorImage()) {
                colorFactor = 4;
            }
            else if (srcImage.isComplexImage()) {
            	colorFactor = 2;
            }

            // create two image buffers to hold a slice each
            imageBuffer = new float[colorFactor * sliceArea];
            imageBuffer2 = new float[imageBuffer.length];

            int length = resultImage.getExtents()[2];

            int srcIndex = 0;

            // copy in all unaveraged slices into their correct positions
            for (int x = 0; x < extents[2]; x++) {

                if ((removedSlices[x] == true) && (x < firstSlice)) {

                    // copy in the first good slice into the previous slices
                    try {
                        srcImage.exportData(srcIndex * imageBuffer.length, imageBuffer.length, imageBuffer);
                        resultImage.importData(x * imageBuffer.length, imageBuffer, false);
                    } catch (Exception ex) {
                        System.err.println("Error copying in original slices into previous slices: " + ex.toString());
                    }

                } else if (removedSlices[x] == false) {

                    try {
                        srcImage.exportData(srcIndex * imageBuffer.length, imageBuffer.length, imageBuffer);
                        resultImage.importData(x * imageBuffer.length, imageBuffer, false);
                    } catch (Exception ex) {
                        System.err.println("Error copying in original slices into result image: " + ex.toString());
                    }

                    srcIndex++;
                } else if ((removedSlices[x] == true) && (x > lastSlice)) {

                    try {
                        srcImage.exportData((srcIndex - 1) * imageBuffer.length, imageBuffer.length, imageBuffer);
                        resultImage.importData(x * imageBuffer.length, imageBuffer, false);
                    } catch (Exception ex) {
                        System.err.println("Error copying in original slices into next slices: " + ex.toString());
                    }
                }

                fireProgressStateChanged((int) ((((float) x + 1.0f) / (float) length) * 50.0f));
            }

            foundStart = false;

            int start = 0;

            // now run through and insert the averaged slices
            for (int x = firstSlice; x <= lastSlice; x++) {

                if (!foundStart && (removedSlices[x] == true)) {
                    foundStart = true;
                    start = x - 1;
                    // System.err.println("start slice for insertion found: " + (start+1));
                } else if (foundStart && (removedSlices[x] == false)) {

                    // System.err.println("end slice for insertion found: " + (resultIndex+1));
                    if (insertBlank) {
                        insertBlankSlices(imageBuffer, start, x);
                    } else {
                        insertAveragedSlices(imageBuffer, imageBuffer2, start, x);
                    }

                    foundStart = false;
                }

                fireProgressStateChanged((int) ((((float) x + 1.0f) / (float) length) * 50.0f) + 50);
            }

            imageBuffer = null;
            imageBuffer2 = null;
        }


        if (isDicom) {
            FileInfoDicom fileInfoBuffer;

            // fix the fileinfos to match

            for (i = 0; i < resultImage.getExtents()[2]; i++) {
                fileInfoBuffer = (FileInfoDicom) srcImage.getFileInfo()[0].clone();
                fileInfoBuffer.setOrigin(newOrg2[i], 2);

                if (!ignoreImagePositionCoords) {
                    fileInfoBuffer.getTagTable().setValue("0020,0032",
                                                          imagePositionCoords[i][0] + "\\" + imagePositionCoords[i][1] +
                                                          "\\" + imagePositionCoords[i][2],
                                                          fileInfoBuffer.getTagTable().get("0020,0032").getLength());
                }

                // Image slice numbers start at 1; index starts at 0, so
                // compensate by adding 1

                fileInfoBuffer.getTagTable().setValue("0020,0013", String.valueOf(i + 1),
                                                      fileInfoBuffer.getTagTable().get("0020,0013").getLength());

                if (!ignoreSliceLocation) {
                    s = nf.format(sliceLocation[i]);
                    fileInfoBuffer.getTagTable().setValue("0020,1041", s, s.length());
                }

                resultImage.setFileInfo(fileInfoBuffer, i);
            }
        } else { // not DICOM

            // fix the fileinfos to match
            for (i = 0; i < resultImage.getExtents()[2]; i++) {

                resultImage.getFileInfo()[i].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
                resultImage.getFileInfo()[i].setModality(srcImage.getFileInfo()[0].getModality());
                resultImage.getFileInfo()[i].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation());
                resultImage.getFileInfo()[i].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
                resultImage.getFileInfo()[i].setOrigin(srcImage.getFileInfo()[0].getOrigin());
                resultImage.getFileInfo()[i].setOrigin(newOrg2[i], 2);
                resultImage.getFileInfo()[i].setSliceThickness(srcImage.getFileInfo()[0].getSliceThickness());
            }
        } // else not DICOM

        resultImage.calcMinMax();


        // put it back into the srcImage if no result image wanted
        if (!destFlag) {

            // System.err.println("importing result back into srcImage (calcInPlace)");
            srcImage.changeExtents(resultImage.getExtents());
            System.err.println("new extents: " + resultImage.getExtents()[0] + ", " + resultImage.getExtents()[1] +
                               ", " + resultImage.getExtents()[2]);
            srcImage.recomputeDataSize();

            // import the result buffer from the resultImage into the srcImage
            // do this a slice at a time to conserve memory
            float[] resultBuffer = null;
            int colorFactor = 1;
            if (srcImage.isColorImage()) {
                colorFactor = 4;
            }
            else if (srcImage.isComplexImage()) {
            	colorFactor = 2;
            }
            int resultSize = resultImage.getSliceSize() * colorFactor;
            int numSlices = 1;
            int numTimes = 1;

            numSlices = destExtents[2];

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
                displayError("Algorithm Replace Removed Slices reports: Out of memory getting results.");
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

            if (isDicom) {
                FileInfoDicom fileInfoBuffer;

                // fix the fileinfos to match

                for (i = 0; i < resultImage.getExtents()[2]; i++) {
                    fileInfoBuffer = (FileInfoDicom) resultImage.getFileInfo()[i].clone();
                    srcImage.setFileInfo(fileInfoBuffer, i);
                }
            } else if (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.MINC) {
                FileInfoMinc fileInfoBuffer;

                // fix the fileinfos to match

                for (i = 0; i < resultImage.getExtents()[2]; i++) {
                    fileInfoBuffer = (FileInfoMinc) resultImage.getFileInfo()[i].clone();
                    srcImage.setFileInfo(fileInfoBuffer, i);
                }
            } else if (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.AFNI) {
                FileInfoAfni fileInfoBuffer;

                // fix the fileinfos to match

                for (i = 0; i < resultImage.getExtents()[2]; i++) {
                    fileInfoBuffer = (FileInfoAfni) resultImage.getFileInfo()[i].clone();
                    srcImage.setFileInfo(fileInfoBuffer, i);
                }
            } else if (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.NIFTI) {
                FileInfoNIFTI fileInfoBuffer;

                // fix the fileinfos to match

                for (i = 0; i < resultImage.getExtents()[2]; i++) {
                    fileInfoBuffer = (FileInfoNIFTI) resultImage.getFileInfo()[i].clone();
                    srcImage.setFileInfo(fileInfoBuffer, i);
                }
            } else if (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.ANALYZE) {
                FileInfoAnalyze fileInfoBuffer;

                // fix the fileinfos to match

                for (i = 0; i < resultImage.getExtents()[2]; i++) {
                    fileInfoBuffer = (FileInfoAnalyze) resultImage.getFileInfo()[i].clone();
                    srcImage.setFileInfo(fileInfoBuffer, i);
                }
            } else if (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.XML) {
                FileInfoXML fileInfoBuffer;

                // fix the fileinfos to match

                for (i = 0; i < resultImage.getExtents()[2]; i++) {
                    fileInfoBuffer = (FileInfoXML) resultImage.getFileInfo()[i].clone();
                    srcImage.setFileInfo(fileInfoBuffer, i);
                }
            } else {
                FileInfoBase fileInfoBuffer;

                // fix the fileinfos to match

                for (i = 0; i < resultImage.getExtents()[2]; i++) {
                    fileInfoBuffer = (FileInfoBase) resultImage.getFileInfo()[i].clone();
                    srcImage.setFileInfo(fileInfoBuffer, i);
                }
            }

            // Clean up and let the calling dialog know that algorithm did its job
            srcImage.releaseLock();

            resultImage.disposeLocal();
        }

        System.gc();
        setCompleted(true);
    }

    /**
     * Insert weighted averaged slices between the two indices.
     *
     * @param  buffer1  empty buffer to hold the first slice
     * @param  buffer2  empty buffer to hold the last slice
     * @param  start    start index
     * @param  end      end index
     */
    private void insertAveragedSlices(float[] buffer1, float[] buffer2, int start, int end) {

        float factor = 1.0f / ((float) (end - start));
        float firstFactor, secondFactor; // weights for first and last slices in the group
        float[] newSliceBuffer = new float[buffer1.length];

        int numToAdd = end - start - 1;

        // System.err.println("Adding " + numToAdd + " slices between slice " + (start+1) + " and " + (end+1));

        try {
            resultImage.exportData(start * buffer1.length, buffer1.length, buffer1);
            resultImage.exportData(end * buffer2.length, buffer2.length, buffer2);

            for (int i = 0; i < numToAdd; i++) {
                secondFactor = factor * (i + 1);
                firstFactor = 1.0f - secondFactor;

                // System.err.println("First factor is: " + firstFactor + " second factor is: " + secondFactor);

                for (int x = 0; x < buffer1.length; x++) {
                    newSliceBuffer[x] = (buffer1[x] * firstFactor) + (buffer2[x] * secondFactor);
                }

                resultImage.importData((start + i + 1) * newSliceBuffer.length, newSliceBuffer, false);
            }
        } catch (Exception ex) {
            System.err.println("Caught exception: " + ex.toString());
        }

        newSliceBuffer = null;
    }

    /**
     * Insert blank slices between the two indices.
     *
     * @param  buffer1  empty buffer to hold the minimum value
     * @param  start    start index
     * @param  end      end index
     */
    private void insertBlankSlices(float[] buffer1, int start, int end) {
        int i;

        int numToAdd = end - start - 1;

        // System.err.println("Adding " + numToAdd + " slices between slice " + (start+1) + " and " + (end+1));

        try {

            for (i = 0; i < buffer1.length; i++) {
                buffer1[i] = imageMin;
            }

            for (i = 0; i < numToAdd; i++) {

                resultImage.importData((start + i + 1) * buffer1.length, buffer1, false);
            }
        } catch (Exception ex) {
            System.err.println("Caught exception: " + ex.toString());
        }
    }
}
