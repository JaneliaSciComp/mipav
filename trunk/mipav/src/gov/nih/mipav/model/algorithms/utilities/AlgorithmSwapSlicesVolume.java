package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.JDialogSwapSlicesVolumes.SwapMode;

import java.io.*;

import java.util.*;


/**
 * <p>Title: AlgorithmExtractIndividualSlices</p>
 *
 * <p>Description: Extracts individual slices from 3D or 4D images and opens each slice in its own Frame</p>
 *
 * @author   Justin Senseney
 * @version  1.0
 */
public class AlgorithmSwapSlicesVolume extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Swap mode, either 3D or 4D */
    private SwapMode mode;
    
    /** Number of slices in mode */
    private int nSlices; // number of slices in image
    
    /** Reordering of slices/volumes */
    private int[][] sliceRenum;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Import source and destination images into the class.
     * @param destImage 
     *
     * @param  srcImage      source image (image to extract from)
     * @param  removeSlices  list of booleans for slices that should be extracted
     */
    public AlgorithmSwapSlicesVolume(ModelImage destImage, SwapMode mode, int[][] sliceRenum, ModelImage srcImage) {
        super(destImage, srcImage);
        this.mode = mode;
        this.nSlices = srcImage.getExtents()[mode.getDim()];
        this.sliceRenum = sliceRenum;
        
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
    public ModelImage getSwappedVolume() { 
        return destImage;
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
            if (image.getFileInfo(0).getFileFormat() == FileUtility.DICOM) {
                FileInfoDicom dicomInfo = (FileInfoDicom) image.getFileInfo(slice);
                FileDicomTagTable tagTable = dicomInfo.getTagTable();
                FileInfoDicom[] fileInfoDicom = new FileInfoDicom[1];
                float[] resolutions = new float[2];
                
                resolutions[0] = image.getFileInfo(0).getResolutions()[0];
                resolutions[1] = image.getFileInfo(0).getResolutions()[1];
              
                fileInfoDicom[0] = new FileInfoDicom(dicomInfo.getFileName(), dicomInfo.getFileDirectory(),
                        dicomInfo.getFileFormat());
                ((FileInfoDicom)fileInfoDicom[0]).setVr_type(dicomInfo.getVr_type());
                ((FileInfoDicom) fileInfoDicom[0]).getTagTable().importTags((FileInfoDicom) image.getFileInfo(slice));
                fileInfoDicom[0].setExtents(resultImage.getExtents());
                fileInfoDicom[0].setResolutions(resolutions);
                fileInfoDicom[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation()[0], 0);
                fileInfoDicom[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation()[1], 1);
                fileInfoDicom[0].setAxisOrientation(image.getFileInfo()[0].getAxisOrientation()[2], 2);
                fileInfoDicom[0].setImageOrientation(image.getFileInfo()[0].getImageOrientation()); 
                
                resultImage.setFileInfo(fileInfoDicom);
            }
            
            else{
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
        else if (srcImage.isComplexImage()) {
            colorFactor = 2;
        }

        return colorFactor;

    } // end getColorFactor()

    /**
     * Calculates the final output and stores it in the source image.
     */
    private void extractSlices() {
        int zSrc; // zSrc is slice-depth of srcImage; zDest is slice-depth of destination
        float[] imageBuffer;
        ModelImage resultImage = null;
        int t;
        int tDim;
        int tSrcOffset;
        int colorFactor;
        int[] newExtents;
        boolean[] extract = null;
        int sliceArea = 0;
        
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

        int oldZdim = nSlices;
        if (newExtents.length == 2) {

            for (zSrc = 0; (zSrc < oldZdim) && !threadStopped; zSrc++) {

                if (extract[zSrc]) {
                    progress++;

                    fireProgressStateChanged((int) ((progress / numToExtract) * 100));

                    try {

                        // try copying the zSrc-th slice out of srcImage, making it the zDest-th in destImage
                        srcImage.exportData(zSrc * colorFactor * sliceArea, colorFactor * sliceArea, imageBuffer);

                        resultImage = new ModelImage(srcImage.getType(), newExtents,
                                                     srcImage.getImageName() + "_slice" + (zSrc));

                        resultImage.importData(0, imageBuffer, false);

                        updateFileInfo(srcImage, resultImage, zSrc);

                        resultImage.calcMinMax();

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

                    for (t = 0; (t < tDim) && !threadStopped; t++) {
                        tSrcOffset = getOffset(srcImage, colorFactor, t);

                        // System.err.println("zSrc is: " + zSrc + " tSrcOffset is: " + tSrcOffset);
                        try {
                            srcImage.exportData(tSrcOffset + (zSrc * colorFactor * sliceArea), 
                                    colorFactor * sliceArea, imageBuffer);

                            // System.err.println("Importing data at: " + (imageBuffer.length * t));
                            resultImage.importData(imageBuffer.length * t, imageBuffer, false);
                        } catch (IOException ex) {
                            ex.printStackTrace();
                        }
                    }

                    // update file info
                    updateFileInfo(srcImage, resultImage, zSrc);

                    resultImage.calcMinMax();

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

        }
