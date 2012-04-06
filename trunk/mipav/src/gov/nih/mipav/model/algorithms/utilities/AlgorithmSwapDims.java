package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.io.*;
import java.util.*;


/**
 * Swaps dimensions in 4D dataset. The current image is deleted and a new one with swapped third and
 * fourth dimensions is created.
 */
public class AlgorithmSwapDims extends AlgorithmBase {

	public static final int SWAP34 = 1;
	public static final int SWAP14 = 2;
	private int swapType;
	
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new algorithm and sets source.
     *
     * @param  srcImg  source image model
     */
    public AlgorithmSwapDims(ModelImage srcImg, int sw) {
        super(null, srcImg);
		swapType = sw;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * Returns result image.
     *
     * @return  destImage
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (swapType==SWAP34) swap34();
        else if (swapType==SWAP14) swap14();
    }
    
    /**
     * Swaps the image third and fourth dimensions and replaces the source image with the swapped dimension image.
     */
    private void swap34() {

        int xy, index, t, z;
        int length;
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        int sliceSize;
        int colorFactor;
        FileInfoBase[] fileInfo, fileInfoR;
        int[] extents;
        int[] units;
        float[] resolutions;
        int tempi;
        float tempf;
        int bufferType;
        int modality;
        String fileDir;
        int dataType;
        boolean endianess;
        double min, max;
        Short pixelPadValue;
        short photometric;
        int[] axis;
        int imageOrientation;
        float[] startLocs;
        Vector<TransMatrix> mats;

        try {

            if (srcImage.isColorImage()) {
                colorFactor = 4;
            } else if (srcImage.isComplexImage()) {
            	colorFactor = 2;
            } else {
                colorFactor = 1;
            }

            sliceSize = xDim * yDim * colorFactor;
            length = colorFactor * xDim * yDim * zDim * tDim;
            buffer = new float[length];
            resultBuffer = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Swapping Dimensions 3-4 ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            System.gc();
            displayError("AlgorithmSwap34: Out of memory");
            setCompleted(false);
            

            return;
        }

        int mod = length / 100; // mod is 1 percent of length
        

        try {
            srcImage.exportData(0, length, buffer);
        } catch (IOException error) {
            displayError("AlgorithmSwapDims: Image locked");
            buffer = null;
            resultBuffer = null;
            setCompleted(false);
            

            return;
        }

        // The third and fourth dimensions are swapped in going from buffer to resultBuffer.
        // xy iterates through the first and second dimensions in one combined step.
        for (t = 0; (t < tDim) && !threadStopped; t++) {

            for (z = 0; (z < zDim) && !threadStopped; z++) {

                for (xy = 0; (xy < sliceSize) && !threadStopped; xy++) {
                    index = ((t * zDim * sliceSize) + (z * sliceSize) + xy);

                    if (((index % mod) == 0)) {
                        fireProgressStateChanged(Math.round((float) (index) / (length - 1) * 100));
                    }

                    resultBuffer[(z * tDim * sliceSize) + (t * sliceSize) + xy] = buffer[index];
                }
            }
        }

        if (threadStopped) {
            buffer = null;
            resultBuffer = null;
            finalize();

            return;
        }

        // The third and fourth dimensions are swapped in extents, units, and resolutions.
        extents = new int[] { xDim, yDim, tDim, zDim };
        fileInfo = srcImage.getFileInfo();
        mats = srcImage.getMatrixHolder().getMatrices();
        units = fileInfo[0].getUnitsOfMeasure();
        tempi = units[2];
        units[2] = units[3];
        units[3] = tempi;
        resolutions = fileInfo[0].getResolutions();
        tempf = resolutions[2];
        resolutions[2] = resolutions[3];
        resolutions[3] = tempf;
        bufferType = srcImage.getType();
        modality = fileInfo[0].getModality();
        fileDir = fileInfo[0].getFileDirectory();
        dataType = fileInfo[0].getDataType();
        endianess = fileInfo[0].getEndianess();
        max = fileInfo[0].getMax();
        min = fileInfo[0].getMin();
        pixelPadValue = fileInfo[0].getPixelPadValue();
        photometric = fileInfo[0].getPhotometric();
        axis = fileInfo[0].getAxisOrientation();
        imageOrientation = fileInfo[0].getImageOrientation();

        //BEN
        //transformID = fileInfo[0].getTransformID();

        String name = JDialogBase.makeImageName(srcImage.getImageName(), "_result");

        srcImage = null;
        destImage = new ModelImage(bufferType, extents, name);
        fileInfoR = destImage.getFileInfo();

        for (int i = 0; i < (zDim * tDim); i++) {
            startLocs = fileInfo[i].getOrigin();
            tempf = startLocs[2];
            startLocs[2] = startLocs[3];
            startLocs[3] = tempf;
            fileInfoR[i].setOrigin(startLocs);
            fileInfoR[i].setImageOrientation(imageOrientation);
            fileInfoR[i].setAxisOrientation(axis);
            fileInfoR[i].setModality(modality);
            fileInfoR[i].setFileDirectory(fileDir);
            fileInfoR[i].setDataType(dataType);
            fileInfoR[i].setEndianess(endianess);
            fileInfoR[i].setUnitsOfMeasure(units);
            fileInfoR[i].setResolutions(resolutions);
            fileInfoR[i].setSliceThickness(resolutions[2]);
            fileInfoR[i].setExtents(extents);
            fileInfoR[i].setMax(max);
            fileInfoR[i].setMin(min);
            fileInfoR[i].setPixelPadValue(pixelPadValue);
            fileInfoR[i].setPhotometric(photometric);
                       

        }

        destImage.getMatrixHolder().replaceMatrices(mats);

        try {
            destImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            displayError("AlgorithmSwap34: Image(s) locked");
            setCompleted(false);
            

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmSwap34: Out of memory");
            setCompleted(false);
            

            return;
        }

        
        setCompleted(true);
    }

    /**
     * Swaps the image first and fourth dimensions and replaces the source image with the swapped dimension image.
     */
    private void swap14() {

        int index, t, x, y, z, c;
        int length;
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        //int yzSize;
        int colorFactor;
        FileInfoBase[] fileInfo, fileInfoR;
        int[] extents;
        int[] units;
        float[] resolutions;
        int tempi;
        float tempf;
        int bufferType;
        int modality;
        String fileDir;
        int dataType;
        boolean endianess;
        double min, max;
        Short pixelPadValue;
        short photometric;
        int[] axis;
        int imageOrientation;
        float[] startLocs;
        Vector<TransMatrix> mats;

        try {

            if (srcImage.isColorImage()) {
                colorFactor = 4;
            } else if (srcImage.isComplexImage()) {
            	colorFactor = 2;
            } else {
                colorFactor = 1;
            }

            length = colorFactor * xDim * yDim * zDim * tDim;
            buffer = new float[length];
            resultBuffer = new float[length];
            fireProgressStateChanged(srcImage.getImageName(), "Swapping Dimensions 1-4 ...");
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            System.gc();
            displayError("AlgorithmSwapDims: Out of memory");
            setCompleted(false);
            

            return;
        }

        int mod = length / 100; // mod is 1 percent of length
        

        try {
            srcImage.exportData(0, length, buffer);
        } catch (IOException error) {
            displayError("AlgorithmSwapDims: Image locked");
            buffer = null;
            resultBuffer = null;
            setCompleted(false);
            

            return;
        }

        // The first and fourth dimensions are swapped in going from buffer to resultBuffer.
        // yz iterates through the second and third dimensions in one combined step.
        for (t = 0; (t < tDim) && !threadStopped; t++) {

            for (x = 0; (x < xDim) && !threadStopped; x++) {

                for (y = 0; (y < yDim) && !threadStopped; y++) {
                    
					for (z = 0; (z < zDim) && !threadStopped; z++) {
						 
						for (c = 0; (c < colorFactor) && !threadStopped; c++) {
					
							index = (c + colorFactor*( x + xDim*( y + yDim*( z + zDim*( t )))));

							if (((index % mod) == 0)) {
								fireProgressStateChanged(Math.round((float) (index) / (length - 1) * 100));
							}
		
							resultBuffer[(c + colorFactor*( t + tDim*( y + yDim*( z + zDim*( x )))))] = buffer[index];
						}
					}
                }
            }
        }

        if (threadStopped) {
            buffer = null;
            resultBuffer = null;
            finalize();

            return;
        }

        // The third and fourth dimensions are swapped in extents, units, and resolutions.
        extents = new int[] { tDim, yDim, zDim, xDim };
        fileInfo = srcImage.getFileInfo();
        mats = srcImage.getMatrixHolder().getMatrices();
        units = fileInfo[0].getUnitsOfMeasure();
        tempi = units[0];
        units[0] = units[3];
        units[3] = tempi;
        resolutions = fileInfo[0].getResolutions();
        tempf = resolutions[0];
        resolutions[0] = resolutions[3];
        resolutions[3] = tempf;
        bufferType = srcImage.getType();
        modality = fileInfo[0].getModality();
        fileDir = fileInfo[0].getFileDirectory();
        dataType = fileInfo[0].getDataType();
        endianess = fileInfo[0].getEndianess();
        max = fileInfo[0].getMax();
        min = fileInfo[0].getMin();
        pixelPadValue = fileInfo[0].getPixelPadValue();
        photometric = fileInfo[0].getPhotometric();
        axis = fileInfo[0].getAxisOrientation();
        imageOrientation = fileInfo[0].getImageOrientation();

        //BEN
        //transformID = fileInfo[0].getTransformID();

        String name = JDialogBase.makeImageName(srcImage.getImageName(), "_result");

        srcImage = null;
        destImage = new ModelImage(bufferType, extents, name);
        fileInfoR = destImage.getFileInfo();
		
        startLocs = fileInfo[0].getOrigin();
		tempf = startLocs[2];
		startLocs[2] = startLocs[3];
		startLocs[3] = tempf;
        
        for (int i = 0; i < (zDim * xDim); i++) {
        	
            fileInfoR[i].setOrigin(startLocs);
            fileInfoR[i].setImageOrientation(imageOrientation);
            fileInfoR[i].setAxisOrientation(axis);
            fileInfoR[i].setModality(modality);
            fileInfoR[i].setFileDirectory(fileDir);
            fileInfoR[i].setDataType(dataType);
            fileInfoR[i].setEndianess(endianess);
            fileInfoR[i].setUnitsOfMeasure(units);
            fileInfoR[i].setResolutions(resolutions);
            fileInfoR[i].setSliceThickness(resolutions[2]);
            fileInfoR[i].setExtents(extents);
            fileInfoR[i].setMax(max);
            fileInfoR[i].setMin(min);
            fileInfoR[i].setPixelPadValue(pixelPadValue);
            fileInfoR[i].setPhotometric(photometric);
                       

        }

        destImage.getMatrixHolder().replaceMatrices(mats);

        try {
            destImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            displayError("AlgorithmSwapDims: Image(s) locked");
            setCompleted(false);
            

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("AlgorithmSwapDims: Out of memory");
            setCompleted(false);
            

            return;
        }

        
        setCompleted(true);
    }

}
