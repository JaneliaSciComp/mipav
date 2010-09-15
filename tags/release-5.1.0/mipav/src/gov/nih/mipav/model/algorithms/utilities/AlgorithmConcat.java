package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * Assumes the pixel resolutions are equal. Concatenates two images of compatible dimensions. Variants include
 *
 * <pre>
            1. 2D to 2D -> 3D
            2. 2D to 3D -> 3D
            3. 3D to 2D -> 3D
            4. 3D to 3D -> 3D
            5. 3D to 3D -> 4D
            6. 3D to 4D -> 4D
            7. 4D to 3D -> 4D
            8. 4D to 4D -> 4D
 *      </pre>
 *
 * @version  1.0 March 22, 2001
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmConcat extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Source image 1. */
    private ModelImage srcImage1;

    /** Source image 2. */
    private ModelImage srcImage2;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmConcat object.
     *
     * @param  srcIm1  source image model 1
     * @param  srcIm2  source image model 2
     * @param  dest    destination image
     */
    public AlgorithmConcat(ModelImage srcIm1, ModelImage srcIm2, ModelImage dest) {
        super(dest, srcIm1);
        srcImage1 = srcIm1; // Put results in destination image.
        srcImage2 = srcIm2;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage1 = null;
        srcImage2 = null;
        destImage = null;
        super.finalize();
    }

    /**
     * Accessor that returns the result image.
     *
     * @return  Result image.
     */
    public ModelImage getResultImage() {
        return destImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if ((srcImage1 == null) || (srcImage2 == null) || (destImage == null)) {
            displayError("Source Image(s) is null");
            setCompleted(false);

            return;
        }

        if (srcImage1.getType() != srcImage2.getType()) {
            displayError("Source Images must be of the same data type.");
            setCompleted(false);

            return;
        }

        float[] resols1 = srcImage1.getResolutions(0);
        float[] resols2 = srcImage2.getResolutions(0);

        if ((srcImage1.getNDims() == 2) && (srcImage2.getNDims() == 2)) {
        	if(resols1[0] == resols2[0] && resols1[1] == resols2[1]) {
        		cat2D_2D_3D();
        	}else {
        		displayError("Resolutions must match up");
                setCompleted(false);
                return;
        	}
        	
        } else if (((srcImage1.getNDims() == 2) && (srcImage2.getNDims() == 3)) ||
                       ((srcImage1.getNDims() == 3) && (srcImage2.getNDims() == 2))) {
        	if(resols1[0] == resols2[0] && resols1[1] == resols2[1]) {
        		cat2D_3D_3D();
        	}else {
        		displayError("Resolutions must match up");
                setCompleted(false);
                return;
        	}
        } else if ((srcImage1.getNDims() == 3) && (srcImage2.getNDims() == 3) && (destImage.getNDims() == 3)) {
        	if(resols1[0] == resols2[0] && resols1[1] == resols2[1] && resols1[2] == resols2[2]) {
        		cat3D_3D_3D();
        	}else {
        		displayError("Resolutions must match up");
                setCompleted(false);
                return;
        	}
        } else if ((srcImage1.getNDims() == 3) && (srcImage2.getNDims() == 3) && (destImage.getNDims() == 4)) {
        	if(resols1[0] == resols2[0] && resols1[1] == resols2[1] && resols1[2] == resols2[2]) {
        		cat3D_3D_4D();
        	}else {
        		displayError("Resolutions must match up");
                setCompleted(false);
                return;
        	}
        } else if (((srcImage1.getNDims() == 3) && (srcImage2.getNDims() == 4)) ||
                       ((srcImage1.getNDims() == 4) && (srcImage2.getNDims() == 3))) {
        	if(resols1[0] == resols2[0] && resols1[1] == resols2[1] && resols1[2] == resols2[2]) {
        		cat3D_4D_4D();
        	}else {
        		displayError("Resolutions must match up");
                setCompleted(false);
                return;
        	}
            
        } else if ((srcImage1.getNDims() == 4) && (srcImage2.getNDims() == 4)) {
        	if(resols1[0] == resols2[0] && resols1[1] == resols2[1] && resols1[2] == resols2[2] && resols1[3] == resols2[3]) {
        		cat4D_4D_4D();
        	}else {
        		displayError("Resolutions must match up");
                setCompleted(false);
                return;
        	}
        } else {
            displayError("Source Image(s) dimensionality not supported.");
        }
    }

    /**
     * This function produces a new image that has been concatenated. Two 2D-images become one 3D image.
     */
    private void cat2D_2D_3D() {

        int length;
        int xDim, yDim;
        int i;
        float[] buffer;
        int cFactor = 1;
        float[] resols = new float[3];
        FileInfoBase[] fileInfo = null;
        FileInfoDicom[] fileInfoDicom = null;

        try {
            resols = new float[3];
            xDim = srcImage1.getExtents()[0];
            yDim = srcImage1.getExtents()[1];

            if (srcImage1.isColorImage()) {
                cFactor = 4;
            }

            length = cFactor * xDim * yDim;
            buffer = new float[length];
            srcImage1.exportData(0, length, buffer);
            destImage.importData(0, buffer, false);
            srcImage2.exportData(0, length, buffer);
            destImage.importData(buffer.length, buffer, true);
        } catch (IOException error) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Out of memory", true);

            return;
        }

        resols[0] = srcImage1.getFileInfo()[0].getResolutions()[0];
        resols[1] = srcImage1.getFileInfo()[0].getResolutions()[1];
        resols[2] = 1;

        if ((srcImage1.getFileInfo()[0] instanceof FileInfoDicom) &&
                (srcImage2.getFileInfo()[0] instanceof FileInfoDicom)) {
            fileInfoDicom = new FileInfoDicom[destImage.getExtents()[2]];
            fileInfoDicom[0] = (FileInfoDicom) (((FileInfoDicom) srcImage1.getFileInfo()[0]).clone());
            fileInfoDicom[1] = (FileInfoDicom) (((FileInfoDicom) srcImage2.getFileInfo()[0]).clone());
            destImage.setFileInfo(fileInfoDicom);
        } else {
            fileInfo = destImage.getFileInfo();

            for (i = 0; (i < destImage.getExtents()[2]) && !threadStopped; i++) {
                fileInfo[i].setModality(srcImage1.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(srcImage1.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setEndianess(srcImage1.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(srcImage1.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resols);
                fileInfo[i].setExtents(destImage.getExtents());
                fileInfo[i].setMax(destImage.getMax());
                fileInfo[i].setMin(destImage.getMin());
                fileInfo[i].setImageOrientation(srcImage1.getImageOrientation());
                fileInfo[i].setPixelPadValue(srcImage1.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(srcImage1.getFileInfo()[0].getPhotometric());
                fileInfo[i].setAxisOrientation(srcImage1.getAxisOrientation());
            }

            if (srcImage1.getFileInfo()[0] instanceof FileInfoImageXML) {

                if (((FileInfoImageXML) srcImage1.getFileInfo()[0]).getPSetHashtable() != null) {
                    ((FileInfoImageXML) fileInfo[0]).setPSetHashtable(((FileInfoImageXML) srcImage1.getFileInfo()[0])
                                                                          .getPSetHashtable());
                }
            }

            if (srcImage2.getFileInfo()[0] instanceof FileInfoImageXML) {

                if (((FileInfoImageXML) srcImage2.getFileInfo()[0]).getPSetHashtable() != null) {
                    ((FileInfoImageXML) fileInfo[1]).setPSetHashtable(((FileInfoImageXML) srcImage2.getFileInfo()[0])
                                                                          .getPSetHashtable());
                }
            }

        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        setCompleted(true);

        fileInfo = null;
        fileInfoDicom = null;
    }

    /**
     * This function produces a new image that has been concatenated. One 2D- and one 3D-image become one 3D image.
     */
    private void cat2D_3D_3D() {

        int length;
        int xDim, yDim;
        float[] buffer;
        int cFactor = 1;
        int i;
        float[] resols = new float[3];
        FileInfoBase[] fileInfo = null;
        FileInfoDicom[] fileInfoDicom = null;

        try {
            fireProgressStateChanged(srcImage1.getImageName(), "Concatenating images ...");
            resols = new float[3];
            xDim = srcImage1.getExtents()[0];
            yDim = srcImage1.getExtents()[1];

            if (srcImage1.isColorImage()) {
                cFactor = 4;
            }

            length = cFactor * xDim * yDim;
            buffer = new float[length];

            int nImages;


            if (srcImage1.getNDims() > srcImage2.getNDims()) {
                nImages = srcImage1.getExtents()[2];

                for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {
                    fireProgressStateChanged(Math.round((float) (i) / (nImages - 1) * 100));

                    srcImage1.exportData(i * buffer.length, length, buffer);
                    destImage.importData(i * buffer.length, buffer, false);
                }

                if (threadStopped) {
                    buffer = null;
                    finalize();

                    return;
                }

                srcImage2.exportData(0, length, buffer);
                destImage.importData(i * buffer.length, buffer, true);
            } else {
                srcImage1.exportData(0, length, buffer);
                destImage.importData(0, buffer, false);
                nImages = srcImage2.getExtents()[2];

                for (i = 0; (i < srcImage2.getExtents()[2]) && !threadStopped; i++) {
                    fireProgressStateChanged(Math.round((float) (i) / (nImages - 1) * 100));

                    srcImage2.exportData(i * buffer.length, length, buffer);
                    destImage.importData((i + 1) * buffer.length, buffer, false);
                }

                if (threadStopped) {
                    buffer = null;
                    finalize();

                    return;
                }

                destImage.calcMinMax();
            }

        } catch (IOException error) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Out of memory", true);

            return;
        }

        resols[0] = srcImage1.getFileInfo()[0].getResolutions()[0];
        resols[1] = srcImage1.getFileInfo()[0].getResolutions()[1];
        resols[2] = 1;

        if ((srcImage1.getFileInfo()[0] instanceof FileInfoDicom) &&
                (srcImage2.getFileInfo()[0] instanceof FileInfoDicom)) {
            fileInfoDicom = new FileInfoDicom[destImage.getExtents()[2]];

            fileInfo = destImage.getFileInfo();

            if (srcImage1.getNDims() > srcImage2.getNDims()) {

                for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {
                    fileInfoDicom[i] = (FileInfoDicom) (((FileInfoDicom) srcImage1.getFileInfo()[i]).clone());
                }

                fileInfoDicom[srcImage1.getExtents()[2]] = (FileInfoDicom)
                                                               (((FileInfoDicom) srcImage2.getFileInfo()[0]).clone());
            } else {
                fileInfoDicom[0] = (FileInfoDicom) (((FileInfoDicom) srcImage1.getFileInfo()[0]).clone());

                for (i = 0; (i < srcImage2.getExtents()[2]) && !threadStopped; i++) {
                    fileInfoDicom[i + 1] = (FileInfoDicom) (((FileInfoDicom) srcImage2.getFileInfo()[i]).clone());
                }
            }

            destImage.setFileInfo(fileInfoDicom);
        } else {
            fileInfo = destImage.getFileInfo();

            for (i = 0; (i < destImage.getExtents()[2]) && !threadStopped; i++) {
                fileInfo[i].setModality(srcImage1.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(srcImage1.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setEndianess(srcImage1.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(srcImage1.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resols);
                fileInfo[i].setExtents(destImage.getExtents());
                fileInfo[i].setMax(destImage.getMax());
                fileInfo[i].setMin(destImage.getMin());
                fileInfo[i].setImageOrientation(srcImage1.getImageOrientation());
                fileInfo[i].setPixelPadValue(srcImage1.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(srcImage1.getFileInfo()[0].getPhotometric());
                fileInfo[i].setAxisOrientation(srcImage1.getAxisOrientation());
            }

            if (srcImage1.getFileInfo()[0] instanceof FileInfoImageXML) {

                if (srcImage1.getNDims() > srcImage2.getNDims()) {

                    for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {

                        if (((FileInfoImageXML) srcImage1.getFileInfo()[i]).getPSetHashtable() != null) {
                            ((FileInfoImageXML) fileInfo[i]).setPSetHashtable(((FileInfoImageXML)
                                                                                   srcImage1.getFileInfo()[i])
                                                                                  .getPSetHashtable());
                        }
                    }
                } else {

                    if (((FileInfoImageXML) srcImage1.getFileInfo()[0]).getPSetHashtable() != null) {
                        ((FileInfoImageXML) fileInfo[0]).setPSetHashtable(((FileInfoImageXML) srcImage1.getFileInfo()[0])
                                                                              .getPSetHashtable());
                    }
                }
            }

            if (srcImage2.getFileInfo()[0] instanceof FileInfoImageXML) {

                if (srcImage1.getNDims() > srcImage2.getNDims()) {

                    if (((FileInfoImageXML) srcImage2.getFileInfo()[0]).getPSetHashtable() != null) {
                        ((FileInfoImageXML) fileInfo[srcImage1.getExtents()[2]]).setPSetHashtable(((FileInfoImageXML)
                                                                                                       srcImage2.getFileInfo()[0])
                                                                                                      .getPSetHashtable());
                    }
                } else {

                    for (i = 0; (i < srcImage2.getExtents()[2]) && !threadStopped; i++) {

                        if (((FileInfoImageXML) srcImage2.getFileInfo()[i]).getPSetHashtable() != null) {
                            ((FileInfoImageXML) fileInfo[i + 1]).setPSetHashtable(((FileInfoImageXML)
                                                                                       srcImage2.getFileInfo()[i])
                                                                                      .getPSetHashtable());
                        }
                    }
                }
            }

        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        setCompleted(true);
        fileInfo = null;
        fileInfoDicom = null;
    }

    /**
     * This function produces a new image that has been concatenated. Two 3D--images become one 3D image.
     */
    private void cat3D_3D_3D() {
        int length;
        int xDim, yDim;
        float[] buffer;
        int cFactor = 1;
        int i, j;
        float[] resols;
        FileInfoBase[] fileInfo = null;
        FileInfoDicom[] fileInfoDicom = null;

        try {
            fireProgressStateChanged(srcImage1.getImageName(), "Concatenating images ...");
            resols = new float[3];
            xDim = srcImage1.getExtents()[0];
            yDim = srcImage1.getExtents()[1];

            if (srcImage1.isColorImage()) {
                cFactor = 4;
            }

            length = cFactor * xDim * yDim;
            buffer = new float[length];

            int nImages;


            nImages = srcImage1.getExtents()[2] + srcImage2.getExtents()[2];

            for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {
                fireProgressStateChanged(Math.round((float) (i) / (nImages - 1) * 100));

                srcImage1.exportData(i * buffer.length, length, buffer);
                destImage.importData(i * buffer.length, buffer, false);
            }

            if (threadStopped) {
                buffer = null;
                finalize();

                return;
            }

            int offset = i * buffer.length;

            for (j = 0; (j < srcImage2.getExtents()[2]) && !threadStopped; j++) {
                fireProgressStateChanged(Math.round((float) (i + j) / (nImages - 1) * 100));

                srcImage2.exportData(j * buffer.length, length, buffer);
                destImage.importData(offset + (j * buffer.length), buffer, false);
            }

            if (threadStopped) {
                buffer = null;
                finalize();

                return;
            }

            destImage.calcMinMax();
        } catch (IOException error) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Out of memory", true);

            return;
        }

        resols[0] = srcImage1.getFileInfo()[0].getResolutions()[0];
        resols[1] = srcImage1.getFileInfo()[0].getResolutions()[1];
        resols[2] = srcImage1.getFileInfo()[0].getResolutions()[2];

        if ((srcImage1.getFileInfo()[0] instanceof FileInfoDicom) &&
                (srcImage2.getFileInfo()[0] instanceof FileInfoDicom)) {
            fileInfoDicom = new FileInfoDicom[srcImage1.getExtents()[2] + srcImage2.getExtents()[2]];

            for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {
                fileInfoDicom[i] = (FileInfoDicom) (((FileInfoDicom) srcImage1.getFileInfo()[i]).clone());
            }

            for (i = 0; (i < srcImage2.getExtents()[2]) && !threadStopped; i++) {
                fileInfoDicom[srcImage1.getExtents()[2] + i] = (FileInfoDicom)
                                                                   (((FileInfoDicom) srcImage2.getFileInfo()[i]).clone());
            }

            destImage.setFileInfo(fileInfoDicom);
        } else {
            fileInfo = destImage.getFileInfo();

            for (i = 0; (i < destImage.getExtents()[2]) && !threadStopped; i++) {
                fileInfo[i].setModality(srcImage1.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(srcImage1.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setEndianess(srcImage1.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(srcImage1.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resols);
                fileInfo[i].setExtents(destImage.getExtents());
                fileInfo[i].setMax(destImage.getMax());
                fileInfo[i].setMin(destImage.getMin());
                fileInfo[i].setImageOrientation(srcImage1.getImageOrientation());
                fileInfo[i].setPixelPadValue(srcImage1.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(srcImage1.getFileInfo()[0].getPhotometric());
                fileInfo[i].setAxisOrientation(srcImage1.getAxisOrientation());
            }

            if (srcImage1.getFileInfo()[0] instanceof FileInfoImageXML) {

                for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {

                    if (((FileInfoImageXML) srcImage1.getFileInfo()[i]).getPSetHashtable() != null) {
                        ((FileInfoImageXML) fileInfo[i]).setPSetHashtable(((FileInfoImageXML) srcImage1.getFileInfo()[i])
                                                                              .getPSetHashtable());
                    }
                }
            }

            if (srcImage2.getFileInfo()[0] instanceof FileInfoImageXML) {

                for (i = 0; (i < srcImage2.getExtents()[2]) && !threadStopped; i++) {

                    if (((FileInfoImageXML) srcImage2.getFileInfo()[i]).getPSetHashtable() != null) {
                        ((FileInfoImageXML) fileInfo[srcImage1.getExtents()[2] + i]).setPSetHashtable(((FileInfoImageXML)
                                                                                                           srcImage2.getFileInfo()[i])
                                                                                                          .getPSetHashtable());
                    }
                }
            }

        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        setCompleted(true);
        fileInfo = null;
        fileInfoDicom = null;
    }

    /**
     * This function produces a new image that has been concatenated. Two 3D--images become one 4D image.
     */
    private void cat3D_3D_4D() {
        int length;
        int xDim, yDim;
        float[] buffer;
        int cFactor = 1;
        int i, j;
        float[] resols;
        float[] origins;
        FileInfoBase[] fileInfo = null;
        FileInfoDicom[] fileInfoDicom = null;

        try {
            fireProgressStateChanged(srcImage1.getImageName(), "Concatenating images ...");
            resols = new float[4];
            origins = new float[4];
            xDim = srcImage1.getExtents()[0];
            yDim = srcImage1.getExtents()[1];

            if (srcImage1.isColorImage()) {
                cFactor = 4;
            }

            length = cFactor * xDim * yDim;
            buffer = new float[length];

            int nImages;


            nImages = srcImage1.getExtents()[2] + srcImage2.getExtents()[2];

            for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {
                fireProgressStateChanged(Math.round((float) (i) / (nImages - 1) * 100));

                srcImage1.exportData(i * buffer.length, length, buffer);
                destImage.importData(i * buffer.length, buffer, false);
            }

            if (threadStopped) {
                buffer = null;
                finalize();

                return;
            }

            int offset = i * buffer.length;

            for (j = 0; (j < srcImage2.getExtents()[2]) && !threadStopped; j++) {
                fireProgressStateChanged(Math.round((float) (i + j) / (nImages - 1) * 100));

                srcImage2.exportData(j * buffer.length, length, buffer);
                destImage.importData(offset + (j * buffer.length), buffer, false);
            }

            if (threadStopped) {
                buffer = null;
                finalize();

                return;
            }

            destImage.calcMinMax();
        } catch (IOException error) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Out of memory", true);

            return;
        }

        resols[0] = srcImage1.getFileInfo()[0].getResolutions()[0];
        resols[1] = srcImage1.getFileInfo()[0].getResolutions()[1];
        resols[2] = srcImage1.getFileInfo()[0].getResolutions()[2];
        resols[3] = 1;
        origins[0] = srcImage1.getFileInfo()[0].getOrigin(0);
        origins[1] = srcImage1.getFileInfo()[0].getOrigin(1);
        origins[2] = srcImage1.getFileInfo()[0].getOrigin(2);
        origins[3] = 0;

        if ((srcImage1.getFileInfo()[0] instanceof FileInfoDicom) &&
                (srcImage2.getFileInfo()[0] instanceof FileInfoDicom)) {
            fileInfoDicom = new FileInfoDicom[destImage.getExtents()[2] * destImage.getExtents()[3]];

            for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {
                fileInfoDicom[i] = (FileInfoDicom) (((FileInfoDicom) srcImage1.getFileInfo()[i]).clone());
                fileInfoDicom[i].setResolutions(resols);
                fileInfoDicom[i].setOrigin(origins);
            }

            for (i = 0; (i < srcImage2.getExtents()[2]) && !threadStopped; i++) {
                fileInfoDicom[srcImage1.getExtents()[2] + i] = (FileInfoDicom)
                                                                   (((FileInfoDicom) srcImage2.getFileInfo()[i]).clone());
                fileInfoDicom[srcImage1.getExtents()[2] + i].setResolutions(resols);
                fileInfoDicom[srcImage1.getExtents()[2] + i].setOrigin(origins);
            }

            destImage.setFileInfo(fileInfoDicom);

        } else {
            fileInfo = destImage.getFileInfo();

            for (i = 0; (i < (destImage.getExtents()[2] * destImage.getExtents()[3])) && !threadStopped; i++) {
                fileInfo[i].setModality(srcImage1.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(srcImage1.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setEndianess(srcImage1.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(srcImage1.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resols);
                fileInfo[i].setExtents(destImage.getExtents());
                fileInfo[i].setMax(destImage.getMax());
                fileInfo[i].setMin(destImage.getMin());
                fileInfo[i].setImageOrientation(srcImage1.getImageOrientation());
                fileInfo[i].setPixelPadValue(srcImage1.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(srcImage1.getFileInfo()[0].getPhotometric());
                fileInfo[i].setAxisOrientation(srcImage1.getAxisOrientation());
            }

            if (srcImage1.getFileInfo()[0] instanceof FileInfoImageXML) {

                for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {

                    if (((FileInfoImageXML) srcImage1.getFileInfo()[i]).getPSetHashtable() != null) {
                        ((FileInfoImageXML) fileInfo[i]).setPSetHashtable(((FileInfoImageXML) srcImage1.getFileInfo()[i])
                                                                              .getPSetHashtable());
                    }
                }
            }

            if (srcImage2.getFileInfo()[0] instanceof FileInfoImageXML) {

                for (i = 0; (i < srcImage2.getExtents()[2]) && !threadStopped; i++) {

                    if (((FileInfoImageXML) srcImage2.getFileInfo()[i]).getPSetHashtable() != null) {
                        ((FileInfoImageXML) fileInfo[srcImage1.getExtents()[2] + i]).setPSetHashtable(((FileInfoImageXML)
                                                                                                           srcImage2.getFileInfo()[i])
                                                                                                          .getPSetHashtable());
                    }
                }
            }

        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        setCompleted(true);
        fileInfo = null;
        fileInfoDicom = null;
    }

    /**
     * cat.
     */
    private void cat3D_4D_4D() {

        int length;
        int xDim, yDim;
        float[] buffer;
        int cFactor = 1;
        int i, j;
        float[] resols = new float[3];
        float[] origins = new float[3];
        FileInfoBase[] fileInfo = null;
        FileInfoDicom[] fileInfoDicom = null;
        int srcALength, srcBLength;

        try {
            fireProgressStateChanged(srcImage1.getImageName(), "Concatenating images ...");
            resols = new float[4];
            origins = new float[4];
            xDim = srcImage1.getExtents()[0];
            yDim = srcImage1.getExtents()[1];

            if (srcImage1.isColorImage()) {
                cFactor = 4;
            }

            length = cFactor * xDim * yDim;
            buffer = new float[length];

            int nImages;


            if (srcImage1.getNDims() > srcImage2.getNDims()) {
                nImages = (srcImage1.getExtents()[2] * srcImage1.getExtents()[3]) + srcImage2.getExtents()[2];

                for (i = 0; (i < (srcImage1.getExtents()[2] * srcImage1.getExtents()[3])) && !threadStopped; i++) {
                    fireProgressStateChanged(Math.round((float) (i) / (nImages - 1) * 100));

                    srcImage1.exportData(i * length, length, buffer);
                    destImage.importData(i * length, buffer, false);
                }

                if (threadStopped) {
                    buffer = null;
                    finalize();

                    return;
                }

                for (j = 0; (j < srcImage2.getExtents()[2]) && !threadStopped; j++) {
                    fireProgressStateChanged(Math.round((float) (i + j) / (nImages - 1) * 100));

                    srcImage2.exportData(j * length, length, buffer);
                    destImage.importData((i + j) * length, buffer, false);
                }

                if (threadStopped) {
                    buffer = null;
                    finalize();

                    return;
                }

                destImage.calcMinMax();
            } else {
                nImages = (srcImage2.getExtents()[2] * srcImage2.getExtents()[3]) + srcImage1.getExtents()[2];

                for (j = 0; (j < srcImage1.getExtents()[2]) && !threadStopped; j++) {
                    fireProgressStateChanged(Math.round((float) (j) / (nImages - 1) * 100));

                    srcImage1.exportData(j * length, length, buffer);
                    destImage.importData(j * length, buffer, false);
                }

                if (threadStopped) {
                    buffer = null;
                    finalize();

                    return;
                }

                for (i = 0; (i < (srcImage2.getExtents()[2] * srcImage2.getExtents()[3])) && !threadStopped; i++) {
                    fireProgressStateChanged(Math.round((float) (i + j) / (nImages - 1) * 100));

                    srcImage2.exportData(i * buffer.length, length, buffer);
                    destImage.importData((i + j) * buffer.length, buffer, false);
                }

                if (threadStopped) {
                    buffer = null;
                    finalize();

                    return;
                }

                destImage.calcMinMax();
            }
        } catch (IOException error) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Out of memory", true);

            return;
        }

        resols[0] = srcImage1.getFileInfo()[0].getResolutions()[0];
        resols[1] = srcImage1.getFileInfo()[0].getResolutions()[1];
        resols[2] = srcImage1.getFileInfo()[0].getResolutions()[2];
        resols[3] = srcImage1.getFileInfo()[0].getResolutions()[3];
        origins[0] = srcImage1.getFileInfo()[0].getOrigin()[0];
        origins[1] = srcImage1.getFileInfo()[0].getOrigin()[1];
        origins[2] = srcImage1.getFileInfo()[0].getOrigin()[2];
        origins[3] = srcImage1.getFileInfo()[0].getOrigin()[3];

        if ((srcImage1.getFileInfo()[0] instanceof FileInfoDicom) &&
                (srcImage2.getFileInfo()[0] instanceof FileInfoDicom)) {
            fileInfoDicom = new FileInfoDicom[destImage.getExtents()[2] * destImage.getExtents()[3]];

            if (srcImage1.getNDims() > srcImage2.getNDims()) {
                srcALength = srcImage1.getExtents()[2] * srcImage1.getExtents()[3];

                for (i = 0; (i < srcALength) && !threadStopped; i++) {
                    fileInfoDicom[i] = (FileInfoDicom) (((FileInfoDicom) srcImage1.getFileInfo()[i]).clone());
                    fileInfoDicom[i].setOrigin(origins);
                }

                for (i = 0; (i < srcImage2.getExtents()[2]) && !threadStopped; i++) {
                    fileInfoDicom[srcALength + i] = (FileInfoDicom)
                                                        (((FileInfoDicom) srcImage2.getFileInfo()[i]).clone());
                    fileInfoDicom[srcALength + i].setOrigin(origins);
                }
            } else {
                srcBLength = srcImage2.getExtents()[2] * srcImage2.getExtents()[3];

                for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {
                    fileInfoDicom[i] = (FileInfoDicom) (((FileInfoDicom) srcImage1.getFileInfo()[i]).clone());
                    fileInfoDicom[i].setOrigin(origins);
                }

                for (i = 0; (i < srcBLength) && !threadStopped; i++) {
                    fileInfoDicom[srcImage1.getExtents()[2] + i] = (FileInfoDicom)
                                                                       (((FileInfoDicom) srcImage2.getFileInfo()[i])
                                                                            .clone());
                    fileInfoDicom[srcImage1.getExtents()[2] + i].setOrigin(origins);
                }

            }

            destImage.setFileInfo(fileInfoDicom);
        } else {
            fileInfo = destImage.getFileInfo();

            for (i = 0; (i < (destImage.getExtents()[2] * destImage.getExtents()[3])) && !threadStopped; i++) {
                fileInfo[i].setModality(srcImage1.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(srcImage1.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setEndianess(srcImage1.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(srcImage1.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resols);
                fileInfo[i].setExtents(destImage.getExtents());
                fileInfo[i].setMax(destImage.getMax());
                fileInfo[i].setMin(destImage.getMin());
                fileInfo[i].setImageOrientation(srcImage1.getImageOrientation());
                fileInfo[i].setPixelPadValue(srcImage1.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(srcImage1.getFileInfo()[0].getPhotometric());
                fileInfo[i].setAxisOrientation(srcImage1.getAxisOrientation());
            }

            if (srcImage1.getFileInfo()[0] instanceof FileInfoImageXML) {

                if (srcImage1.getNDims() > srcImage2.getNDims()) {
                    srcALength = srcImage1.getExtents()[2] * srcImage1.getExtents()[3];

                    for (i = 0; (i < srcALength) && !threadStopped; i++) {

                        if (((FileInfoImageXML) srcImage1.getFileInfo()[i]).getPSetHashtable() != null) {
                            ((FileInfoImageXML) fileInfo[i]).setPSetHashtable(((FileInfoImageXML)
                                                                                   srcImage1.getFileInfo()[i])
                                                                                  .getPSetHashtable());
                        }
                    }
                } else {

                    for (i = 0; (i < srcImage1.getExtents()[2]) && !threadStopped; i++) {

                        if (((FileInfoImageXML) srcImage1.getFileInfo()[i]).getPSetHashtable() != null) {
                            ((FileInfoImageXML) fileInfo[i]).setPSetHashtable(((FileInfoImageXML)
                                                                                   srcImage1.getFileInfo()[i])
                                                                                  .getPSetHashtable());
                        }
                    }
                }
            }

            if (srcImage2.getFileInfo()[0] instanceof FileInfoImageXML) {

                if (srcImage1.getNDims() > srcImage2.getNDims()) {
                    srcALength = srcImage1.getExtents()[2] * srcImage1.getExtents()[3];

                    for (i = 0; (i < srcImage2.getExtents()[2]) && !threadStopped; i++) {

                        if (((FileInfoImageXML) srcImage2.getFileInfo()[i]).getPSetHashtable() != null) {
                            ((FileInfoImageXML) fileInfo[srcALength + i]).setPSetHashtable(((FileInfoImageXML)
                                                                                                srcImage2.getFileInfo()[i])
                                                                                               .getPSetHashtable());
                        }
                    }

                } else {
                    srcBLength = srcImage2.getExtents()[2] * srcImage2.getExtents()[3];

                    for (i = 0; (i < srcBLength) && !threadStopped; i++) {

                        if (((FileInfoImageXML) srcImage2.getFileInfo()[i]).getPSetHashtable() != null) {
                            ((FileInfoImageXML) fileInfo[srcImage1.getExtents()[2] + i]).setPSetHashtable(((FileInfoImageXML)
                                                                                                               srcImage2.getFileInfo()[i])
                                                                                                              .getPSetHashtable());
                        }
                    }

                }
            }

        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        setCompleted(true);
        fileInfo = null;
        fileInfoDicom = null;
    }

    /**
     * cat.
     */
    private void cat4D_4D_4D() {

        int length;
        int xDim, yDim;
        float[] buffer;
        int cFactor = 1;
        int i, j;
        int srcALength, srcBLength;
        float[] resols;
        FileInfoBase[] fileInfo = null;
        FileInfoDicom[] fileInfoDicom = null;

        try {
            fireProgressStateChanged(srcImage1.getImageName(), "Concatenating images ...");
            resols = new float[4];
            xDim = srcImage1.getExtents()[0];
            yDim = srcImage1.getExtents()[1];

            if (srcImage1.isColorImage()) {
                cFactor = 4;
            }

            length = cFactor * xDim * yDim;
            buffer = new float[length];

            int nImages;


            nImages = (srcImage1.getExtents()[2] * srcImage1.getExtents()[3]) +
                      (srcImage2.getExtents()[2] * srcImage2.getExtents()[3]);

            for (i = 0; (i < (srcImage1.getExtents()[2] * srcImage1.getExtents()[3])) && !threadStopped; i++) {
                fireProgressStateChanged(Math.round((float) (i) / (nImages - 1) * 100));

                srcImage1.exportData(i * length, length, buffer);
                destImage.importData(i * length, buffer, false);
            }

            if (threadStopped) {
                buffer = null;
                finalize();

                return;
            }

            for (j = 0; (j < (srcImage2.getExtents()[2] * srcImage2.getExtents()[3])) && !threadStopped; j++) {
                fireProgressStateChanged(Math.round((float) (i + j) / (nImages - 1) * 100));

                srcImage2.exportData(j * length, length, buffer);
                destImage.importData((i + j) * length, buffer, false);
            }

            if (threadStopped) {
                buffer = null;
                finalize();

                return;
            }

            destImage.calcMinMax();
        } catch (IOException error) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            destImage.disposeLocal(); // Clean up memory of result image
            destImage = null;
            errorCleanUp("Algorithm Concat. Images: Out of memory", true);

            return;
        }

        resols[0] = srcImage1.getFileInfo()[0].getResolutions()[0];
        resols[1] = srcImage1.getFileInfo()[0].getResolutions()[1];
        resols[2] = srcImage1.getFileInfo()[0].getResolutions()[2];
        resols[3] = srcImage1.getFileInfo()[0].getResolutions()[3];

        if ((srcImage1.getFileInfo()[0] instanceof FileInfoDicom) &&
                (srcImage2.getFileInfo()[0] instanceof FileInfoDicom)) {
            fileInfoDicom = new FileInfoDicom[destImage.getExtents()[2] * destImage.getExtents()[3]];

            srcALength = srcImage1.getExtents()[2] * srcImage1.getExtents()[3];
            srcBLength = srcImage2.getExtents()[2] * srcImage2.getExtents()[3];

            for (i = 0; (i < srcALength) && !threadStopped; i++) {
                fileInfoDicom[i] = (FileInfoDicom) (((FileInfoDicom) srcImage1.getFileInfo()[i]).clone());
            }

            for (i = 0; (i < srcBLength) && !threadStopped; i++) {
                fileInfoDicom[srcALength + i] = (FileInfoDicom) (((FileInfoDicom) srcImage2.getFileInfo()[i]).clone());
            }

            destImage.setFileInfo(fileInfoDicom);
        } else {
            fileInfo = destImage.getFileInfo();

            for (i = 0; (i < (destImage.getExtents()[2] * destImage.getExtents()[3])) && !threadStopped; i++) {
                fileInfo[i].setModality(srcImage1.getFileInfo()[0].getModality());
                fileInfo[i].setFileDirectory(srcImage1.getFileInfo()[0].getFileDirectory());
                fileInfo[i].setEndianess(srcImage1.getFileInfo()[0].getEndianess());
                fileInfo[i].setUnitsOfMeasure(srcImage1.getFileInfo()[0].getUnitsOfMeasure());
                fileInfo[i].setResolutions(resols);
                fileInfo[i].setExtents(destImage.getExtents());
                fileInfo[i].setMax(destImage.getMax());
                fileInfo[i].setMin(destImage.getMin());
                fileInfo[i].setImageOrientation(srcImage1.getImageOrientation());
                fileInfo[i].setPixelPadValue(srcImage1.getFileInfo()[0].getPixelPadValue());
                fileInfo[i].setPhotometric(srcImage1.getFileInfo()[0].getPhotometric());
                fileInfo[i].setAxisOrientation(srcImage1.getAxisOrientation());
            }

            srcALength = srcImage1.getExtents()[2] * srcImage1.getExtents()[3];
            srcBLength = srcImage2.getExtents()[2] * srcImage2.getExtents()[3];

            if (srcImage1.getFileInfo()[0] instanceof FileInfoImageXML) {

                for (i = 0; (i < srcALength) && !threadStopped; i++) {

                    if (((FileInfoImageXML) srcImage1.getFileInfo()[i]).getPSetHashtable() != null) {
                        ((FileInfoImageXML) fileInfo[i]).setPSetHashtable(((FileInfoImageXML) srcImage1.getFileInfo()[i])
                                                                              .getPSetHashtable());
                    }
                }
            }

            if (srcImage2.getFileInfo()[0] instanceof FileInfoImageXML) {

                for (i = 0; (i < srcBLength) && !threadStopped; i++) {

                    if (((FileInfoImageXML) srcImage2.getFileInfo()[i]).getPSetHashtable() != null) {
                        ((FileInfoImageXML) fileInfo[srcALength + i]).setPSetHashtable(((FileInfoImageXML)
                                                                                            srcImage2.getFileInfo()[i])
                                                                                           .getPSetHashtable());
                    }
                }
            }

        }

        if (threadStopped) {
            buffer = null;
            finalize();

            return;
        }

        setCompleted(true);
        fileInfo = null;
        fileInfoDicom = null;
    }
}
