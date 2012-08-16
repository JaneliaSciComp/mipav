package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.JDialogSwapSlicesVolumes.SwapMode;

import ij.io.FileInfo;

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

    /** Swap mode, either 3D or 4D. */
    private SwapMode mode;
    
    /** Number of slices in mode. */
    private int nSlices; 
    
    /** Reordering of slices/volumes. */
    private int[][] sliceRenum;

    /** Number of pixels used by a slice/volume. */
    private int sliceSize;

    /** Slices that have been examined in recursive structure for swapping slices. */
    private boolean[] sliceTouched;

    /** Internal sorting variable for sorting slices on images of greater than 3 dimensions */
    private int extent = 0;
    
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
        int extent = 0;
        for(int i=0; i<sliceRenum.length; i++) {
            extent += sliceRenum[i].length;
        }
        
        this.nSlices = extent;
        this.sliceRenum = sliceRenum;
        
        sliceSize = 1;
        
        for(int i=0; i<mode.getDim(); i++) {
            sliceSize *= srcImage.getExtents()[i];
        }        
        
        sliceSize *= getColorFactor();
        
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

        swapSlices();
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

    private boolean allSlicesTouched() {
        for(int i=0; i<sliceTouched.length; i++) {
            if(!sliceTouched[i]) {
                return false;
            }
        }
        
        return true;
    }

    /**
     * Gets the fileInfos from the specified position.
     * 
     * @tDim time point to get 3D file info
     */
    private FileInfoBase[] collectFileInfos(int in, int tDim) {
        FileInfoBase[] fileOut;
        switch(mode) {
        case FourD:
            fileOut = new FileInfoBase[srcImage.getExtents()[2]];
            for(int i=0; i<srcImage.getExtents()[2]; i++) {
                if(srcImage == destImage && sliceRenum[in].length == 1) {
                    fileOut[i] = srcImage.getFileInfo(in*srcImage.getExtents()[2]+i); //only transferring file info is necessary
                } else {
                    fileOut[i] = (FileInfoBase) srcImage.getFileInfo(in*srcImage.getExtents()[2]+i).clone(); //duplicates of this file info are necessary
                }
            }
            break;
        default:
            int index = tDim*srcImage.getExtents()[2] + in; //allows 4D images to transfer single slice at given time dimension
            fileOut = new FileInfoBase[1];
            if(srcImage == destImage && sliceRenum[index].length == 1) {
                fileOut[0] = srcImage.getFileInfo(index); //only transferring file info is necessary
            } else {
                fileOut[0] = (FileInfoBase) srcImage.getFileInfo(index).clone(); //duplicates of this file info are necessary
            }
        }
        
        return fileOut;
    }

    /**
     * Inserts the fileInfos into the specified position.
     * 
     * @tDim time point to place 3D file info
     */
    private void importFileInfos(ModelImage destImage, FileInfoBase[] fileIn, int in, int tDim) {
        switch(mode) {
        case FourD:
            for(int i=0; i<destImage.getExtents()[2]; i++) {
                destImage.setFileInfo(fileIn[i], in*destImage.getExtents()[2]+i);
            }
            break;
        default:
            destImage.setFileInfo(fileIn[0], tDim*destImage.getExtents()[2]+in);
        }
    }

    /**
     * Reallocate destImage so that fileInfo and buffer equal required length 
     * 
     * @param destImage destination image of algorithm
     */
    private void reallocate(ModelImage destImage) {
        if(destImage.getExtents()[mode.getDim()] != nSlices) {
            Number[] bufferOut = new Number[destImage.getDataSize()];
            try {
                destImage.exportData(0, destImage.getDataSize(), bufferOut);
                int[] extents = Arrays.copyOf(srcImage.getExtents(), srcImage.getExtents().length);
                extents[mode.getDim()] = nSlices;
                destImage.reallocate(extents);
                
                destImage.importData(0, bufferOut, false);
            } catch (IOException e) {
                e.printStackTrace();
            }
            FileInfoBase[] fileInfo = destImage.getFileInfo();
            int nInfos = 1;
            for(int i=2; i<destImage.getExtents().length; i++) {
                nInfos *= destImage.getExtents()[i];
            }
            FileInfoBase[] newFileInfo = new FileInfoBase[nInfos];
            for(int i=0; i<fileInfo.length && i<newFileInfo.length; i++) {
                newFileInfo[i] = fileInfo[i];
            }
            destImage.setFileInfo(newFileInfo);
        }
    }

    /**
     * Calculates the final output and stores it in the source image.
     */
    private void swapSlices() {

        reallocate(destImage);
        
        sliceTouched = new boolean[sliceRenum.length];
        for(int i=0; i<sliceTouched.length; i++) {
            sliceTouched[i] = false;
        }
        
        int index = 0;
        while(!allSlicesTouched()) {
            transferIn(index);
            for(int i=index; i<sliceTouched.length; i++) {
                if(!sliceTouched[i]) {
                    index = i;
                    break;
                }
            }
        }
        
        if(srcImage != destImage) {
            for(int i=0; i<destImage.getFileInfo().length; i++) {
                destImage.getFileInfo()[i].setFileName(srcImage.getImageFileName()+"_swap"+i);
            }
        }
        
        if (threadStopped) {
            finalize();
            return;
        }

        setCompleted(true);
    } // swapSlices

    /**
     * Starts method of file/image transferring
     * 
     * @param in location to import data
     * @return whether import was successful
     */
    private boolean transferIn(int in) {
        return transferIn(null, null, in, 0);
    }
    
    /**
     * Transfers the given buffer into the given location. Also recursively transfers all
     * the old data to the location(s) where the old data is used.
     * 
     * @param bufferIn relieves the symptoms of arthritis
     * @param in location to import data
     * @return whether import was successful
     */
    private boolean transferIn(FileInfoBase[] fileIn, Number[] bufferIn, int in, int tDim) {
        try {
            if((sliceTouched.length > in && sliceRenum[in].length == 0 && bufferIn == null) || //slice has nowhere to go 
            (srcImage == destImage && sliceRenum[in].length == 1 && sliceRenum[in][0] == in)) { //slice is only going to where it already exists
                sliceTouched[in] = true;
                return true;
            }
            
            if(sliceTouched.length > in && !sliceTouched[in]) { 
                sliceTouched[in] = true; 
                
                if(sliceRenum[in].length > 0) {
                    int index = 1;
                    if(mode == SwapMode.ThreeD && srcImage.getNDims() > 3) {
                        index = srcImage.getExtents()[3];
                        extent  = srcImage.getExtents()[2]*srcImage.getExtents()[1]*srcImage.getExtents()[0];
                    }
                    
                    for(int tDimLocal=0; tDimLocal<index; tDimLocal++) {
                        
                        Number[] bufferOut = new Number[sliceSize];
                        FileInfoBase[] fileOut = collectFileInfos(in, tDimLocal);
                        srcImage.exportData(tDimLocal*extent + in*sliceSize, sliceSize, bufferOut);
                          
                        for(int i=0; i<sliceRenum[in].length; i++) {
                            transferIn(fileOut, bufferOut, sliceRenum[in][i], tDimLocal);
                        }
                    }
                }
            } 
            
            if(bufferIn != null) {
                importFileInfos(destImage, fileIn, in, tDim);
                destImage.importData(tDim*extent + in*sliceSize, bufferIn, false);
            }
    
            return true;
        } catch(IOException e) {
            e.printStackTrace();
            return false;
        }
    }

}
