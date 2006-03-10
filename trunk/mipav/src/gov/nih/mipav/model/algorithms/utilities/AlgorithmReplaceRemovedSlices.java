package gov.nih.mipav.model.algorithms.utilities;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.model.file.FileInfoBase;
import java.io.IOException;

/**
 * <p>Title: AlgorithmReplaceRemovedSlices</p>
 * <p>Description: This algorithm is used to insert averaged slices where slices have been removed
 *  -If slices were removed at the beginning of the original movie, these slices will not be replaced.
 * -  Slices are only inserted between the first and last kept slices</p>
 * <p>Copyright: Copyright (c) 2003</p>
 * <p>Company: </p>
 * @author Ben Link
 * @version 1.0
 */

public class AlgorithmReplaceRemovedSlices
    extends AlgorithmBase {

    private ModelImage resultImage;

    private boolean[] removedSlices = null; //shows which slices were removed from the original.
    //  every false value in the array corresponds to
    //  a slice within the movie passed in.  true values
    //  represent removed slices
    private boolean isSplit = false;

    private boolean destFlag = false;

    /**
     * Constructor for algorithm
     * @param srcImage the source image
     * @param removedSlices an array showing which slices were removed
     */
    public AlgorithmReplaceRemovedSlices(ModelImage srcImage, boolean[] removedSlices, boolean isSplit,
            boolean destFlag) {
        super(null, srcImage);
        this.removedSlices = removedSlices;
        this.isSplit = isSplit;
        this.destFlag = destFlag;
    }

    /**
     *  Function called by thread to run the algorithm
     */
    public void runAlgorithm() {
        if (srcImage == null) {
            displayError("AlgorithmReplaceRemovedSlices.run(): Source Image is null");
            return;
        }
        else if (srcImage.getExtents().length != 3) {
            displayError("Image must be 3 dimensional");
            return;
        }

        int[] destExtents = new int[3];
        destExtents[0] = srcImage.getExtents()[0];
        destExtents[1] = srcImage.getExtents()[1];

        if (!isSplit) {

            for (int i = 0; i < removedSlices.length; i++) {
                if (removedSlices[i] == true) {
                    //System.err.println(i + "th slice was removed...will be replaced");
                }
            }

            int firstSlice = -1;
            int lastSlice = -1;
            boolean foundStart = false;

            //find the first good and last good (kept) slices
            for (int i = 0; i < removedSlices.length; i++) {
                if (!foundStart && removedSlices[i] == false) {
                    foundStart = true;
                    firstSlice = i;
                }
                else if (foundStart && removedSlices[i] == false) {
                    lastSlice = i;
                }
            }

            if (firstSlice == -1 || lastSlice == -1) {
                System.err.println("Could not find first/last slices");
                setCompleted(false);
                notifyListeners(this);
                return;
            }

            //System.err.println("first slice from removedSlices[] " + (firstSlice+1) + " last slice " + (lastSlice+1));

            //determine new # of slices:  only replace slices found after the first good (unremoved)
            //    slice from the original image
            destExtents[2] = (lastSlice - firstSlice) + 1;

            //System.err.println("New extents for " + srcImage.getImageName() + ": " + destExtents[2]);

            resultImage = new ModelImage(srcImage.getType(), destExtents,
                                         srcImage.getImageName() + "_replaced_slices", srcImage.getUserInterface());

            int sliceArea = destExtents[0] * destExtents[1];
            float[] imageBuffer = null, imageBuffer2 = null;
            int colorFactor = 1;
            if (srcImage.isColorImage()) {
                colorFactor = 4;
            }

            //create two image buffers to hold a slice each
            imageBuffer = new float[colorFactor * sliceArea];
            imageBuffer2 = new float[imageBuffer.length];

            int srcIndex = 0;
            int resultIndex = 0;

            buildProgressBar(resultImage.getImageName(), "Inserting Weighted Averaged Slice...", 0, 100);
            progressBar.updateValue(0, activeImage);
            initProgressBar();

            int length = resultImage.getExtents()[2];

            //copy in all the unaveraged slices into their correct positions
            for (int x = firstSlice; x <= lastSlice; x++, resultIndex++) {
                if (removedSlices[x] == false) {
                    //System.err.println("Copying in from srcIndex: " + srcIndex + " to resultIndex: " + resultIndex);
                    try {
                        srcImage.exportData(srcIndex * imageBuffer.length, imageBuffer.length, imageBuffer);
                        resultImage.importData(resultIndex * imageBuffer.length, imageBuffer, false);
                    }
                    catch (Exception ex) {
                        System.err.println("Error copying in original slices into result image: " + ex.toString());
                    }
                    srcIndex++;
                }
                progressBar.updateValue( (int) ( ( ( (float) resultIndex + 1.0f) / (float) length) * 50.0f),
                                     activeImage);
            }

            //System.err.println("finished copying original slices into result image");

            foundStart = false;
            int start = 0;
            resultIndex = 0;
            //now run through and insert the averaged slices
            for (int x = firstSlice; x <= lastSlice; x++, resultIndex++) {
                if (!foundStart && removedSlices[x] == true) {
                    foundStart = true;
                    start = resultIndex - 1;
                    //System.err.println("start slice for insertion found: " + (start+1));
                }
                else if (foundStart && removedSlices[x] == false) {
                    //System.err.println("end slice for insertion found: " + (resultIndex+1));
                    insertAveragedSlices(imageBuffer, imageBuffer2, start, resultIndex);
                    foundStart = false;
                }
                progressBar.updateValue( (int) ( ( ( (float) resultIndex + 1.0f) / (float) length) * 50.0f) + 50,
                                     activeImage);
            }
            imageBuffer = null;
            imageBuffer2 = null;
        }
        else {
            // image was previously split so keep the same extents and copy first good slice into previous slices
            // and last good slice into next slices

            int firstSlice = -1;
            int lastSlice = -1;
            boolean foundStart = false;

            //find the first good and last good (kept) slices
            for (int i = 0; i < removedSlices.length; i++) {
                if (!foundStart && removedSlices[i] == false) {
                    foundStart = true;
                    firstSlice = i;
                }
                else if (foundStart && removedSlices[i] == false) {
                    lastSlice = i;
                }
            }

            int extents[] = srcImage.getExtents();

            extents[2] = removedSlices.length;

            //System.err.println("ExtentsZ: " + extents[2]);

            resultImage = new ModelImage(srcImage.getType(), extents,
                                         srcImage.getImageName() + "_replaced_slices", srcImage.getUserInterface());


            buildProgressBar(resultImage.getImageName(), "Inserting Weighted Averaged Slice...", 0, 100);
            progressBar.updateValue(0, activeImage);
            initProgressBar();

            int sliceArea = extents[0] * extents[1];
            float[] imageBuffer = null, imageBuffer2 = null;
            int colorFactor = 1;
            if (srcImage.isColorImage()) {
                colorFactor = 4;
            }

            //create two image buffers to hold a slice each
            imageBuffer = new float[colorFactor * sliceArea];
            imageBuffer2 = new float[imageBuffer.length];

            int length = resultImage.getExtents()[2];

            int srcIndex = 0;

            //copy in all unaveraged slices into their correct positions
            for (int x = 0; x < extents[2]; x++) {

                if (removedSlices[x] == true && x < firstSlice) {
                    //copy in the first good slice into the previous slices
                    try {
                        srcImage.exportData(srcIndex * imageBuffer.length, imageBuffer.length, imageBuffer);
                        resultImage.importData(x * imageBuffer.length, imageBuffer, false);
                    }
                    catch (Exception ex) {
                        System.err.println("Error copying in original slices into previous slices: " + ex.toString());
                    }

                }
                else if (removedSlices[x] == false) {
                    try {
                        srcImage.exportData(srcIndex * imageBuffer.length, imageBuffer.length, imageBuffer);
                        resultImage.importData(x * imageBuffer.length, imageBuffer, false);
                    }
                    catch (Exception ex) {
                        System.err.println("Error copying in original slices into result image: " + ex.toString());
                    }
                    srcIndex++;
                }
                else if (removedSlices[x] == true && x > lastSlice) {
                    try {
                        srcImage.exportData( (srcIndex - 1 ) * imageBuffer.length, imageBuffer.length, imageBuffer);
                        resultImage.importData(x * imageBuffer.length, imageBuffer, false);
                    }
                    catch (Exception ex) {
                        System.err.println("Error copying in original slices into next slices: " + ex.toString());
                    }
                }
                progressBar.updateValue( (int) ( ( ( (float) x + 1.0f) / (float) length) * 50.0f),
                                      activeImage);
            }

            foundStart = false;
            int start = 0;
            //now run through and insert the averaged slices
            for (int x = firstSlice; x <= lastSlice; x++) {
                if (!foundStart && removedSlices[x] == true) {
                    foundStart = true;
                    start = x - 1;
                    //System.err.println("start slice for insertion found: " + (start+1));
                }
                else if (foundStart && removedSlices[x] == false) {
                    // System.err.println("end slice for insertion found: " + (resultIndex+1));
                    insertAveragedSlices(imageBuffer, imageBuffer2, start, x);
                    foundStart = false;
                }
                progressBar.updateValue( (int) ( ( ( (float) x + 1.0f) / (float) length) * 50.0f) + 50,
                                     activeImage);
            }

            imageBuffer = null;
            imageBuffer2 = null;
        }

        float res2 = srcImage.getFileInfo()[0].getResolutions()[2];
        float start2 = srcImage.getFileInfo()[0].getOrigin(2);

        //fix the fileinfos to match
        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
            for (int j = 0; j < 3; j++)
                resultImage.getFileInfo()[i].getResolutions()[j] = srcImage.getFileInfo()[0].getResolutions()[j];
            resultImage.getFileInfo()[i].setUnitsOfMeasure(srcImage.getFileInfo()[0].getUnitsOfMeasure());
            resultImage.getFileInfo()[i].setModality(srcImage.getFileInfo()[0].getModality());
            resultImage.getFileInfo()[i].setImageOrientation(srcImage.getFileInfo()[0].getImageOrientation());
            resultImage.getFileInfo()[i].setAxisOrientation(srcImage.getFileInfo()[0].getAxisOrientation());
            resultImage.getFileInfo()[i].setOrigin(srcImage.getFileInfo()[0].getOrigin());
            resultImage.getFileInfo()[i].setOrigin(start2 - (res2 * i), 2);
            resultImage.getFileInfo()[i].setSliceSpacing(srcImage.getFileInfo()[0].getSliceSpacing());
        }


        resultImage.calcMinMax();


        //put it back into the srcImage if no result image wanted
        if (!destFlag) {
            //System.err.println("importing result back into srcImage (calcInPlace)");
            srcImage.setExtents(resultImage.getExtents());
            srcImage.recomputeDataSize();

            FileInfoBase [] fInfos = new FileInfoBase[destExtents[2]];
            for (int i = 0; i < destExtents[2]; i++) {
                fInfos[i] = (FileInfoBase)srcImage.getFileInfo()[0].clone();
            }
            srcImage.setFileInfo(fInfos);

            // import the result buffer from the resultImage into the srcImage
            // do this a slice at a time to conserve memory
            float[] resultBuffer = null;
            int resultSize = resultImage.getSliceSize();
            int numSlices = 1;
            int numTimes = 1;

            numSlices = destExtents[2];

            if (isProgressBarVisible()) {
                progressBar.setMessage("Importing Image Data...");
            }
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
                displayError(
                        "Algorithm Remove Slices reports: Out of memory getting results.");
                srcImage.releaseLock();
                setCompleted(false);
                disposeProgressBar();
                return;
            } catch (IOException e2) {
                resultBuffer = null;
                resultImage = null;
                System.gc();
                displayError(e2.getMessage());
                srcImage.releaseLock();
                setCompleted(false);
                disposeProgressBar();
                return;
            }

            AlgorithmRemoveSlices.updateFileInfo(resultImage, srcImage);

            // Clean up and let the calling dialog know that algorithm did its job
            srcImage.releaseLock();

            resultImage.disposeLocal();
        }


        progressBar.setVisible(false);
        progressBar.dispose();
        System.gc();
        setCompleted(true);
    }

    /**
     * Insert weighted averaged slices between the two indices
     * @param buffer1 empty buffer to hold the first slice
     * @param buffer2 empty buffer to hold the last slice
     * @param start start index
     * @param end end index
     */
    private void insertAveragedSlices(float[] buffer1, float[] buffer2, int start, int end) {

        float factor = 1.0f / ( (float) (end - start));
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

                //  System.err.println("First factor is: " + firstFactor + " second factor is: " + secondFactor);

                for (int x = 0; x < buffer1.length; x++) {
                    newSliceBuffer[x] = buffer1[x] * firstFactor + buffer2[x] * secondFactor;
                }
                resultImage.importData( (start + i + 1) * newSliceBuffer.length, newSliceBuffer, false);
            }
        }
        catch (Exception ex) {
            System.err.println("Caught exception: " + ex.toString());
        }
        newSliceBuffer = null;
    }

    /**
     * Retrieves the result image
     * @return resultImage the new image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Local cleanup
     */
    public void disposeLocal() {
        //resultImage = null;
        removedSlices = null;
        System.gc();
    }

    /**
     *   Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

}
