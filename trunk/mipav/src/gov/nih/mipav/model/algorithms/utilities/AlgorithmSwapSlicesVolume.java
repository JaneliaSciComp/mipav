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

    /** Number of pixels used by a slice/volume, includes colorFactor. */
    private int sliceSize;

    /** Slices that have been examined in recursive structure for swapping slices. */
    private boolean[] sliceTouched;

    /** Internal sorting variables for sorting slices on images of greater than 3 dimensions */
    private int extentSrc = 0, extentDest = 0;
    
    /** Original FileInfoBase array. */
    private FileInfoBase[] srcFileInfos = null;

    /** Original extents array */
    private int[] srcExtents;
    
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
        
        srcFileInfos = Arrays.copyOf(srcImage.getFileInfo(), srcImage.getFileInfo().length);
        srcExtents = Arrays.copyOf(srcImage.getExtents(), srcImage.getExtents().length);
        
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
        
        destImage.calcMinMax();
    } // end run()

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
            fileOut = new FileInfoBase[srcExtents[2]];
            for(int i=0; i<srcExtents[2]; i++) {
                if(srcImage == destImage && sliceRenum[in].length == 1) {
                    fileOut[i] = srcFileInfos[in*srcExtents[2]+i]; //only transferring file info is necessary
                } else {
                    fileOut[i] = (FileInfoBase) srcFileInfos[in*srcExtents[2]+i].clone(); //duplicates of this file info are necessary
                }
            }
            break;
        default:
            int index = tDim*srcExtents[2] + in; //allows 4D images to transfer single slice at given time dimension
            fileOut = new FileInfoBase[1];
            if(srcImage == destImage && sliceRenum[index].length == 1) {
                fileOut[0] = srcFileInfos[index]; //only transferring file info is necessary
            } else {
                fileOut[0] = (FileInfoBase) srcFileInfos[index].clone(); //duplicates of this file info are necessary
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
     * @param image ModelImage
     */
    private void reallocate(ModelImage image) {
        if(image.getExtents()[mode.getDim()] != nSlices) {
            Number[] bufferOut = new Number[image.getDataSize()];
            try {
                image.exportData(0, image.getDataSize(), bufferOut);
                int[] extents = Arrays.copyOf(srcExtents, srcExtents.length);
                extents[mode.getDim()] = nSlices;
                image.reallocate(extents);
                
                Number[] newBuffer = null;
                if(bufferOut.length > image.getDataSize()) {
                    newBuffer = Arrays.copyOf(bufferOut, image.getDataSize());
                } else {
                    newBuffer = bufferOut;
                }
                
                image.importData(0, newBuffer, false);
            } catch (IOException e) {
                e.printStackTrace();
            }
            FileInfoBase[] fileInfo = image.getFileInfo();
            int nInfos = 1;
            for(int i=2; i<image.getExtents().length; i++) {
                nInfos *= image.getExtents()[i];
            }
            FileInfoBase[] newFileInfo = new FileInfoBase[nInfos];
            for(int i=0; i<fileInfo.length && i<newFileInfo.length; i++) {
                newFileInfo[i] = fileInfo[i];
            }
            image.setFileInfo(newFileInfo);
        }
    }

    /**
     * Calculates the final output and stores it in the source image.
     */
    private void swapSlices() {

        if(destImage.getExtents()[mode.getDim()] < nSlices) {
            reallocate(destImage);
        }
        
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
        
        if(destImage.getExtents()[mode.getDim()] > nSlices) {
            reallocate(destImage);
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
                        index = srcExtents[3];
                        extentSrc  = srcExtents[2]*srcExtents[1]*srcExtents[0]*getColorFactor();
                        extentDest   = destImage.getExtents()[2]*destImage.getExtents()[1]*destImage.getExtents()[0]*getColorFactor();
                    }
                    
                    for(int tDimLocal=0; tDimLocal<index; tDimLocal++) {
                        
                        Number[] bufferOut = new Number[sliceSize];
                        FileInfoBase[] fileOut = collectFileInfos(in, tDimLocal);
                        srcImage.exportData(tDimLocal*extentSrc + in*sliceSize, sliceSize, bufferOut);
                          
                        for(int i=0; i<sliceRenum[in].length; i++) {
                            transferIn(fileOut, bufferOut, sliceRenum[in][i], tDimLocal);
                        }
                    }
                }
            } 
            
            if(bufferIn != null) {
                importFileInfos(destImage, fileIn, in, tDim);
                destImage.importData(tDim*extentDest + in*sliceSize, bufferIn, false);
            }
    
            return true;
        } catch(IOException e) {
            e.printStackTrace();
            return false;
        }
    }

}
