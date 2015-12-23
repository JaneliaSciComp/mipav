package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.io.*;


/**
 * Put 3D dataset into dicom order.
 */
public class AlgorithmDicomOrder extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Axis orientations of image. */
    private int[] orient;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * In place constructor.
     *
     * @param  srcImg  source image model
     * @param  orient  3 integer codes describing the spatial orientation of the axes
     */
    public AlgorithmDicomOrder(ModelImage srcImg, int[] orient) {
        super(null, srcImg);
        this.orient = orient;

        if (srcImage.getFileInfo(0).isDicomOrdered()) {
            MipavUtil.displayWarning("Image is already dicom ordered");

            return;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Returns the dicom-ordered image.
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage returnImage() {
        return destImage;
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        

        calcInPlace();

        if (threadStopped) {
            finalize();
        }
    }

    /**
     * Calculates the dicom ordered image and replaces the original image with the dicom ordered image.
     */
    private void calcInPlace() {

        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        float[] resol = srcImage.getFileInfo(0).getResolutions();
        float[] newResol = new float[5];
        int[] resUnit = srcImage.getFileInfo(0).getUnitsOfMeasure();
        int[] newResUnit = new int[5];
        int newXDim = xDim;
        int newYDim = yDim;
        int newZDim = zDim;
        int sliceSize, newSliceSize;
        int xyzSize;
        float[] datasetBuffer;
        int[] newExtents;
        float[] dicomBuffer;
        int x, y, z;
        int newX = 0;
        int newY = 0;
        int newZ = 0;
        FileInfoBase fileInfo, newFileInfo[];
        int[] newOrient;
        double minimum, maximum;
        int i;

        switch (orient[0]) {

            case FileInfoBase.ORI_R2L_TYPE:
            case FileInfoBase.ORI_L2R_TYPE:
                newResol[0] = resol[0];
                newResUnit[0] = resUnit[0];
                newXDim = xDim;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
            case FileInfoBase.ORI_A2P_TYPE:
                newResol[1] = resol[0];
                newResUnit[1] = resUnit[0];
                newYDim = xDim;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                newResol[2] = resol[0];
                newResUnit[2] = resUnit[0];
                newZDim = xDim;
                break;
        }

        switch (orient[1]) {

            case FileInfoBase.ORI_R2L_TYPE:
            case FileInfoBase.ORI_L2R_TYPE:
                newResol[0] = resol[1];
                newResUnit[0] = resUnit[1];
                newXDim = yDim;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
            case FileInfoBase.ORI_A2P_TYPE:
                newResol[1] = resol[1];
                newResUnit[1] = resUnit[1];
                newYDim = yDim;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                newResol[2] = resol[1];
                newResUnit[2] = resUnit[1];
                newZDim = yDim;
                break;
        }

        switch (orient[2]) {

            case FileInfoBase.ORI_R2L_TYPE:
            case FileInfoBase.ORI_L2R_TYPE:
                newResol[0] = resol[2];
                newResUnit[0] = resUnit[2];
                newXDim = zDim;
                break;

            case FileInfoBase.ORI_P2A_TYPE:
            case FileInfoBase.ORI_A2P_TYPE:
                newResol[1] = resol[2];
                newResUnit[1] = resUnit[2];
                newYDim = zDim;
                break;

            case FileInfoBase.ORI_I2S_TYPE:
            case FileInfoBase.ORI_S2I_TYPE:
                newResol[2] = resol[2];
                newResUnit[2] = resUnit[2];
                newZDim = zDim;
                break;
        }

        sliceSize = xDim * yDim;
        newSliceSize = newXDim * newYDim;
        srcImage.calcMinMax();
        minimum = srcImage.getMin();
        maximum = srcImage.getMax();

        fireProgressStateChanged(srcImage.getFileInfo()[0].getFileName(), "Reordering image to dicom order ...");

        fireProgressStateChanged(0);
        xyzSize = xDim * yDim * zDim;

        try {
            datasetBuffer = new float[xyzSize];
            srcImage.exportData(0, xyzSize, datasetBuffer);
            newExtents = new int[3];
            newOrient = new int[3];
        } catch (IOException e) {
            MipavUtil.displayError("Error on srcImage.exportData(0,xyzSize,datasetBuffer)");
            setCompleted(false);


            return;
        } catch (OutOfMemoryError e) {
            datasetBuffer = null;
            System.gc();
            MipavUtil.displayError("Out of memory in Algorithm Dicom Order");
            setCompleted(false);


            return;
        }

        if (threadStopped) {
            datasetBuffer = null;

            return;
        }

        newExtents[0] = newXDim;
        newExtents[1] = newYDim;
        newExtents[2] = newZDim;
        newOrient[0] = FileInfoBase.ORI_R2L_TYPE;
        newOrient[1] = FileInfoBase.ORI_A2P_TYPE;
        newOrient[2] = FileInfoBase.ORI_I2S_TYPE;
        fileInfo = srcImage.getFileInfo(0);

        try {
            destImage = new ModelImage(srcImage.getType(), newExtents,
                                       JDialogBase.makeImageName(srcImage.getImageName(), "_dicomImage"));
            newFileInfo = new FileInfoBase[newZDim];

            for (i = 0; (i < newZDim) && !threadStopped; i++) {
                newFileInfo[i] = new FileInfoImageXML(fileInfo.getFileName(), null, 0);
                newFileInfo[i].setModality(fileInfo.getModality());
                newFileInfo[i].setFileDirectory(fileInfo.getFileDirectory());
                newFileInfo[i].setDataType(fileInfo.getDataType());
                newFileInfo[i].setEndianess(fileInfo.getEndianess());
                newFileInfo[i].setUnitsOfMeasure(newResUnit);
                newFileInfo[i].setResolutions(newResol);
                newFileInfo[i].setExtents(newExtents);
                newFileInfo[i].setAxisOrientation(newOrient);
                newFileInfo[i].setImageOrientation(FileInfoBase.AXIAL);
                newFileInfo[i].setMax(maximum);
                newFileInfo[i].setMin(minimum);
                newFileInfo[i].setPixelPadValue(fileInfo.getPixelPadValue());
                newFileInfo[i].setPhotometric(fileInfo.getPhotometric());
            }

            if (threadStopped) {
                datasetBuffer = null;

                return;
            }

            dicomBuffer = new float[xyzSize];
        } catch (OutOfMemoryError e) {
            datasetBuffer = null;
            newFileInfo = null;
            System.gc();
            MipavUtil.displayError("AlgorithmDicomOrder: Out of memory on new ModelImage");

            setCompleted(false);

            return;
        }

        destImage.setFileInfo(newFileInfo);

        for (x = 0; (x < xDim) && !threadStopped; x++) {
            fireProgressStateChanged(Math.round(100 * x / (xDim - 1)));

            switch (orient[0]) {

                case FileInfoBase.ORI_R2L_TYPE:
                    newX = x;
                    break;

                case FileInfoBase.ORI_L2R_TYPE:
                    newX = xDim - 1 - x;
                    break;

                case FileInfoBase.ORI_A2P_TYPE:
                    newY = x;
                    break;

                case FileInfoBase.ORI_P2A_TYPE:
                    newY = xDim - 1 - x;
                    break;

                case FileInfoBase.ORI_I2S_TYPE:
                    newZ = x;
                    break;

                case FileInfoBase.ORI_S2I_TYPE:
                    newZ = xDim - 1 - x;
                    break;
            }

            if (threadStopped) {
                datasetBuffer = null;
                dicomBuffer = null;

                return;
            }

            for (y = 0; (y < yDim) && !threadStopped; y++) {

                switch (orient[1]) {

                    case FileInfoBase.ORI_R2L_TYPE:
                        newX = y;
                        break;

                    case FileInfoBase.ORI_L2R_TYPE:
                        newX = yDim - 1 - y;
                        break;

                    case FileInfoBase.ORI_A2P_TYPE:
                        newY = y;
                        break;

                    case FileInfoBase.ORI_P2A_TYPE:
                        newY = yDim - 1 - y;
                        break;

                    case FileInfoBase.ORI_I2S_TYPE:
                        newZ = y;
                        break;

                    case FileInfoBase.ORI_S2I_TYPE:
                        newZ = yDim - 1 - y;
                        break;
                }

                if (threadStopped) {
                    datasetBuffer = null;
                    dicomBuffer = null;

                    return;
                }

                for (z = 0; (z < zDim) && !threadStopped; z++) {

                    switch (orient[2]) {

                        case FileInfoBase.ORI_R2L_TYPE:
                            newX = z;
                            break;

                        case FileInfoBase.ORI_L2R_TYPE:
                            newX = zDim - 1 - z;
                            break;

                        case FileInfoBase.ORI_A2P_TYPE:
                            newY = z;
                            break;

                        case FileInfoBase.ORI_P2A_TYPE:
                            newY = zDim - 1 - z;
                            break;

                        case FileInfoBase.ORI_I2S_TYPE:
                            newZ = z;
                            break;

                        case FileInfoBase.ORI_S2I_TYPE:
                            newZ = zDim - 1 - z;
                            break;
                    }

                    if (threadStopped) {
                        datasetBuffer = null;
                        dicomBuffer = null;

                        return;
                    }

                    dicomBuffer[newX + (newXDim * newY) + (newSliceSize * newZ)] = datasetBuffer[x + (xDim * y) +
                                                                                                 (sliceSize * z)];
                } // for (z = 0; z < zDim; z++)
            } // for (y = 0; y < yDim; y++)
        } // for (x = 0; x < xDim; x++)

        datasetBuffer = null;

        if (threadStopped) {
            dicomBuffer = null;

            return;
        }

        try {
            destImage.importData(0, dicomBuffer, true);
        } catch (IOException e) {
            MipavUtil.displayError("Error on resultImage.importData(0,dicomBuffer,true)");
            dicomBuffer = null;
            setCompleted(false);


            return;
        }

        dicomBuffer = null;


        setCompleted(true);
    }
}
