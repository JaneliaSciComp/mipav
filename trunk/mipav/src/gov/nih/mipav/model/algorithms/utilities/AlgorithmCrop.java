package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.structures.jama.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Crops 2D and 3D images using a supplied VOI. Crops 4D images.
 *
 * @version  1.0 June 11, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmCrop extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Extra space around VOI in x and y dimensions. */
    private int cushion = 0;

    /** Flag for color or noncolor image. */
    private boolean RGBImage;

    /** Storage for VOI. */
    private int[] x = null;

    /** Storage for VOI. */
    private int[] y = null;

    /** Storage for VOI. */
    private int[] z = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new algorithms to crop image using VOI. Stores in srcImg.
     *
     * @param  srcImg    source image model
     * @param  _cushion  extra space around VOI in x and y dimensions.
     * @param  _x        VOI xBounds: x[0] = min. bound and x[1] = max. bound
     * @param  _y        VOI yBounds: y[0] = min. bound and y[1] = max. bound
     * @param  _z        VOI zBounds: z[0] = min. bound and z[1] = max. bound
     */
    public AlgorithmCrop(ModelImage srcImg, int _cushion, int[] _x, int[] _y, int[] _z) {
        super(null, srcImg);
        cushion = _cushion;
        x = _x;
        y = _y;
        z = _z;
    }

    /**
     * Creates new algorithms to crop image using VOI. Stores in destImg.
     *
     * @param  destImg   image model where result image is to stored
     * @param  srcImg    source image model
     * @param  _cushion  extra space around VOI in x and y dimensions.
     * @param  _x        VOI xBounds: x[0] = min. bound and x[1] = max. bound
     * @param  _y        VOI yBounds: y[0] = min. bound and y[1] = max. bound
     * @param  _z        VOI zBounds: z[0] = min. bound and z[1] = max. bound
     */
    public AlgorithmCrop(ModelImage destImg, ModelImage srcImg, int _cushion, int[] _x, int[] _y, int[] _z) {

        super(destImg, srcImg);
        cushion = _cushion;
        x = _x;
        y = _y;
        z = _z;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        x = null;
        y = null;
        z = null;
        super.finalize();
    }

    /**
     * DOCUMENT ME!
     *
     * @return  ModelImage
     */
    public ModelImage getSrcImage() {
        return srcImage;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (srcImage.isColorImage()) {
            RGBImage = true;
        } else {
            RGBImage = false;
        }

        constructLog();

        if (destImage != null) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() == 3) {
                calcStoreInDest3D();
            } else if (srcImage.getNDims() == 4) {
                calcStoreInDest4D();
            }
        } // if (destImage != null)
        else { // destImage == null

            if (srcImage.getNDims() == 2) {
                calcStoreInPlace2D();
            } else if (srcImage.getNDims() == 3) {
                calcStoreInPlace3D();
            } else if (srcImage.getNDims() == 4) {
                calcStoreInPlace4D();
            }

        } // else destImage == null

        if (destImage != null) {
            updateFileInfoData();
        }

        if (threadStopped) {
            finalize();
        }
    }


    /**
     * This function produces a new image that has been cropped!
     */
    private void calcStoreInDest2D() {

        int i;
        int length;
        float[] buffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int[] dimExtents;

        if (((x != null) && (((x[0] - cushion) < 0) || ((x[1] + cushion) >= xDim))) ||
                ((y != null) && (((y[0] - cushion) < 0) || ((y[1] + cushion) >= yDim)))) {
            displayError("Algorithm Crop Image: Crop bounds exceed image bounds.");
            setCompleted(false);

            return;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            displayError(" Crop: Image(s) locked");
            buffer = null;
            setCompleted(false);

            return;
        }

        try {
            dimExtents = new int[2];
            dimExtents[0] = Math.abs(x[1] - x[0]) + 1 + (2 * cushion);
            dimExtents[1] = Math.abs(y[1] - y[0]) + 1 + (2 * cushion);

            if (RGBImage) {
                length = 4 * xDim * yDim;
            } else {
                length = xDim * yDim;
            }

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Cropping image ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Crop Image: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Crop Image:  Out of memory", true);

            return;
        }

        if (RGBImage) {
            length = 4 * dimExtents[0] * dimExtents[1];
        } else {
            length = dimExtents[0] * dimExtents[1];
        }

        int mod = length / 100; // mod is 1 percent of length

        initProgressBar();

        int j = 0;
        int k;

        if (RGBImage) {
            int offset = (4 * (((y[0] - cushion) * xDim) + x[0] - cushion));

            for (i = 0, k = 0; (i < length) && !threadStopped; i++, k++) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
                }

                if (((i % (4 * dimExtents[0])) == 0) && (i != 0)) {
                    j++;
                    k = 0;
                    offset = (4 * (((y[0] + j - cushion) * xDim) + x[0] - cushion));
                }

                destImage.set(i, buffer[offset + k]);
            }
        } // end of if (RGBImage)
        else { // not ARGB or ARGB_USHORT or ARGB_FLOAT

            int offset = (((y[0] - cushion) * xDim) + x[0] - cushion);

            for (i = 0, k = 0; (i < length) && !threadStopped; i++, k++) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
                }

                if (((i % dimExtents[0]) == 0) && (i != 0)) {
                    j++;
                    k = 0;
                    offset = (((y[0] + j - cushion) * xDim) + x[0] - cushion);
                }

                destImage.set(i, buffer[offset + k]);
            }
        } // not ARGB or ARGB_USHORT or ARGB_FLOAT

        if (threadStopped) {
            buffer = null;

            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * This function produces a new image that has been cropped.
     */
    private void calcStoreInDest3D() {

        int i;
        int length;
        int croppedLength;
        int croppedVolume;
        int slice;
        float[] buffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int[] dimExtents;
        int j, k, offset;
        int count;

        if (((x != null) && (((x[0] - cushion) < 0) || ((x[1] + cushion) >= xDim))) ||
                ((y != null) && (((y[0] - cushion) < 0) || ((y[1] + cushion) >= yDim)))) {
            displayError("Algorithm Crop Image: Crop bounds exceed image bounds.");
            setCompleted(false);

            return;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp(" Crop Image: Image(s) locked", true);

            return;
        }

        dimExtents = new int[3];
        dimExtents[0] = Math.abs(x[1] - x[0]) + 1 + (2 * cushion);
        dimExtents[1] = Math.abs(y[1] - y[0]) + 1 + (2 * cushion);

        if (Math.abs(z[1] - z[0]) == 0) {
            dimExtents[2] = 1;
        } else {
            dimExtents[2] = Math.abs(z[1] - z[0]) + 1; // + 2*cushion;
        }

        if (RGBImage) {
            croppedLength = 4 * dimExtents[0] * dimExtents[1];
        } else {
            croppedLength = dimExtents[0] * dimExtents[1];
        }

        croppedVolume = croppedLength * dimExtents[2];

        int mod = croppedVolume / 100; // mod is 1 percent of length

        if (RGBImage) {
            length = 4 * xDim * yDim;
        } else {
            length = xDim * yDim;
        }

        try {
            buffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Crop Image: Out of memory", true);

            return;
        }

        buildProgressBar(srcImage.getImageName(), "Cropping image ...", 0, 100);
        initProgressBar();

        for (slice = z[0], i = 0; slice <= z[1]; slice++) {

            try {
                srcImage.exportData(slice * length, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Crop Image: Image(s) locked", true);

                return;
            }

            j = 0;
            k = 0;

            if (RGBImage) {
                offset = (4 * (((y[0] - cushion) * xDim) + x[0] - cushion));

                for (count = 0; (count < croppedLength) && !threadStopped; i++, k++, count++) {

                    if (((i % mod) == 0) && isProgressBarVisible()) {
                        progressBar.updateValue(Math.round((float) i / (croppedVolume - 1) * 100), runningInSeparateThread);
                    }

                    if (((i % (4 * dimExtents[0])) == 0) && ((i % croppedLength) != 0)) {
                        j++;
                        k = 0;
                        offset = (4 * (((y[0] + j - cushion) * xDim) + x[0] - cushion));
                    }

                    destImage.set(i, buffer[offset + k]);
                }
            } // if (RGBImage)
            else { // not ARGB or ARGB_USHORT or ARGB_FLOAT
                offset = (((y[0] - cushion) * xDim) + x[0] - cushion);

                for (count = 0; (count < croppedLength) && !threadStopped; i++, k++, count++) {

                    if (((i % mod) == 0) && isProgressBarVisible()) {
                        progressBar.updateValue(Math.round((float) i / (croppedVolume - 1) * 100), runningInSeparateThread);
                    }

                    if (((i % dimExtents[0]) == 0) && ((i % croppedLength) != 0)) {
                        j++;
                        k = 0;
                        offset = (((y[0] + j - cushion) * xDim) + x[0] - cushion);
                    }

                    destImage.set(i, buffer[offset + k]);
                }
            } // not ARGB or ARGB_USHORT or ARGB_FLOAT
        } // for (slice = z[0], i = 0; slice <= z[1]; slice++)

        buffer = null;

        if (threadStopped) {
            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * This function produces a new image that has been cropped.
     */
    private void calcStoreInDest4D() {

        int i;
        int length;
        int volume;
        int croppedLength;
        int croppedVolume;
        int croppedSeries;
        int slice;
        float[] buffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        int[] dimExtents;
        int j, k, offset;
        int count;
        int t;

        if (((x != null) && (((x[0] - cushion) < 0) || ((x[1] + cushion) >= xDim))) ||
                ((y != null) && (((y[0] - cushion) < 0) || ((y[1] + cushion) >= yDim)))) {
            displayError("Algorithm Crop Image: Crop bounds exceed image bounds.");
            setCompleted(false);

            return;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp(" Crop Image: Image(s) locked", true);

            return;
        }

        dimExtents = new int[3];
        dimExtents[0] = Math.abs(x[1] - x[0]) + 1 + (2 * cushion);
        dimExtents[1] = Math.abs(y[1] - y[0]) + 1 + (2 * cushion);

        if (Math.abs(z[1] - z[0]) == 0) {
            dimExtents[2] = 1;
        } else {
            dimExtents[2] = Math.abs(z[1] - z[0]) + 1; // + 2*cushion;
        }

        if (RGBImage) {
            croppedLength = 4 * dimExtents[0] * dimExtents[1];
        } else {
            croppedLength = dimExtents[0] * dimExtents[1];
        }

        croppedVolume = croppedLength * dimExtents[2];
        croppedSeries = croppedVolume * tDim;

        int mod = croppedSeries / 100; // mod is 1 percent of length

        if (RGBImage) {
            length = 4 * xDim * yDim;
        } else {
            length = xDim * yDim;
        }

        volume = length * zDim;

        try {
            buffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Crop Image: Out of memory", true);

            return;
        }

        buildProgressBar(srcImage.getImageName(), "Cropping image ...", 0, 100);
        initProgressBar();

        for (t = 0, i = 0; t < tDim; t++) {

            for (slice = z[0]; slice <= z[1]; slice++) {

                try {
                    srcImage.exportData((t * volume) + (slice * length), length, buffer); // locks and releases lock
                } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Algorithm Crop Image: Image(s) locked", true);

                    return;
                }

                j = 0;
                k = 0;

                if (RGBImage) {
                    offset = (4 * (((y[0] - cushion) * xDim) + x[0] - cushion));

                    for (count = 0; (count < croppedLength) && !threadStopped; i++, k++, count++) {

                        if (((i % mod) == 0) && isProgressBarVisible()) {
                            progressBar.updateValue(Math.round((float) i / (croppedSeries - 1) * 100), runningInSeparateThread);
                        }

                        if (((i % (4 * dimExtents[0])) == 0) && ((i % croppedLength) != 0)) {
                            j++;
                            k = 0;
                            offset = (4 * (((y[0] + j - cushion) * xDim) + x[0] - cushion));
                        }

                        destImage.set(i, buffer[offset + k]);
                    }
                } // if (RGBImage)
                else { // not ARGB or ARGB_USHORT or ARGB_FLOAT
                    offset = (((y[0] - cushion) * xDim) + x[0] - cushion);

                    for (count = 0; (count < croppedLength) && !threadStopped; i++, k++, count++) {

                        if (((i % mod) == 0) && isProgressBarVisible()) {
                            progressBar.updateValue(Math.round((float) i / (croppedSeries - 1) * 100), runningInSeparateThread);
                        }

                        if (((i % dimExtents[0]) == 0) && ((i % croppedLength) != 0)) {
                            j++;
                            k = 0;
                            offset = (((y[0] + j - cushion) * xDim) + x[0] - cushion);
                        }

                        destImage.set(i, buffer[offset + k]);
                    }
                } // not ARGB or ARGB_USHORT or ARGB_FLOAT
            } // for (slice = z[0], i = 0; slice <= z[1]; slice++)
        } // for (t = 0; t < tDim; t++)

        buffer = null;

        if (threadStopped) {
            return;
        }

        destImage.calcMinMax();
        destImage.releaseLock();
        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * This function crops srcImage Must use getSrcImage after running.
     */
    private void calcStoreInPlace2D() {

        int i, n;
        int length;
        float[] buffer;
        float[] destBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int[] dimExtents;
        FileInfoBase[] fInfoBase;
        int dataType = srcImage.getType();
        String imageName = srcImage.getImageName();
        ViewUserInterface userInterface = srcImage.getUserInterface();
        float[] resols;
        int[] axisOrient;
        int nDims = 2;
        int[] direct = new int[nDims];
        float[] imgOriginLPS;
        float[] originImgOrd = new float[3];
        float[] newImgOriginLPS = new float[3];
        TransMatrix mat;
        float startPos;
        String value;


        if (((x != null) && (((x[0] - cushion) < 0) || ((x[1] + cushion) >= xDim))) ||
                ((y != null) && (((y[0] - cushion) < 0) || ((y[1] + cushion) >= yDim)))) {
            displayError("Algorithm Crop Image: Crop bounds exceed image bounds.");
            setCompleted(false);

            return;
        }

        try {
            dimExtents = new int[2];
            dimExtents[0] = Math.abs(x[1] - x[0]) + 1 + (2 * cushion);
            dimExtents[1] = Math.abs(y[1] - y[0]) + 1 + (2 * cushion);

            if (RGBImage) {
                length = 4 * xDim * yDim;
            } else {
                length = xDim * yDim;
            }

            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            buildProgressBar(srcImage.getImageName(), "Cropping image ...", 0, 100);
        } catch (IOException error) {
            buffer = null;
            errorCleanUp("Algorithm Crop Image: Image(s) locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Crop Image:  Out of memory", true);

            return;
        }

        fInfoBase = new FileInfoBase[1];
        fInfoBase[0] = (FileInfoBase) (srcImage.getFileInfo(0).clone());
        fInfoBase[0].setExtents(dimExtents);


        resols = fInfoBase[0].getResolutions();
        axisOrient = fInfoBase[0].getAxisOrientation();
        mat = srcImage.getMatrix();

        for (i = 0; i < nDims; i++) {

            if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                direct[i] = 1;
            } else {
                direct[i] = -1;
            }
        }

        imgOriginLPS = fInfoBase[0].getOrigin();
        originImgOrd = originLPS2Img(imgOriginLPS, srcImage);
        originImgOrd[0] = originImgOrd[0] + (direct[0] * (x[0] - cushion) * resols[0]);
        originImgOrd[1] = originImgOrd[1] + (direct[1] * (y[0] - cushion) * resols[1]);
        newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
        fInfoBase[0].setOrigin(newImgOriginLPS);
        fInfoBase[0].setResolutions(resols);


        if (srcImage.getParentFrame() != null) {
            srcImage.getParentFrame().close();
        }

        srcImage.disposeLocal();
        srcImage = null;


        if (RGBImage) {
            length = 4 * dimExtents[0] * dimExtents[1];
        } else {
            length = dimExtents[0] * dimExtents[1];
        }

        int mod = length / 100; // mod is 1 percent of length

        try {
            destBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            destBuffer = null;
            errorCleanUp("Algorithm Crop Image:  Out of memory", true);

            return;
        }

        initProgressBar();

        int j = 0;
        int k;

        if (RGBImage) {
            int offset = (4 * (((y[0] - cushion) * xDim) + x[0] - cushion));

            for (i = 0, k = 0; (i < length) && !threadStopped; i++, k++) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
                }

                if (((i % (4 * dimExtents[0])) == 0) && (i != 0)) {
                    j++;
                    k = 0;
                    offset = (4 * (((y[0] + j - cushion) * xDim) + x[0] - cushion));
                }

                destBuffer[i] = buffer[offset + k];
            }
        } // end of if (RGBImage)
        else { // not ARGB or ARGB_USHORT or ARGB_FLOAT

            int offset = (((y[0] - cushion) * xDim) + x[0] - cushion);

            for (i = 0, k = 0; (i < length) && !threadStopped; i++, k++) {

                if (((i % mod) == 0) && isProgressBarVisible()) {
                    progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
                }

                if (((i % dimExtents[0]) == 0) && (i != 0)) {
                    j++;
                    k = 0;
                    offset = (((y[0] + j - cushion) * xDim) + x[0] - cushion);
                }

                destBuffer[i] = buffer[offset + k];
            }
        } // not ARGB or ARGB_USHORT or ARGB_FLOAT

        if (threadStopped) {
            buffer = null;

            return;
        }

        buffer = null;

        srcImage = new ModelImage(dataType, dimExtents, imageName, userInterface);
        srcImage.setFileInfo(fInfoBase[0], 0);
        srcImage.setMatrix(mat);

        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {
            ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
            ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26);
            ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
            ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
            ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0028,0011", new Short((short) dimExtents[0]), 2); // columns
            ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0028,0010", new Short((short) dimExtents[1]), 2); // rows
            ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0020,0013", Short.toString((short) (1)),
                                                                 Short.toString((short) (1)).length()); // instance number

            int imgOrient = ((FileInfoDicom) (srcImage.getFileInfo(0))).getImageOrientation();

            startPos = originImgOrd[2];
            originImgOrd[2] = startPos + (direct[2] * z[0] * resols[2]);
            newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
            value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                    Float.toString(newImgOriginLPS[2]);

            ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0020,0032", value, value.length());

            value = String.valueOf(originImgOrd[2]);
            ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0020,1041", value, value.length());
        } // if ( ( srcImage.getFileInfo()[0] ).getFileFormat() == FileBase.DICOM )

        try {
            srcImage.importData(0, destBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm RGBtoGray: Output Image(s) locked");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * This function crops srcImage Must use getSrcImage after running.
     */
    private void calcStoreInPlace3D() {

        int i, n;
        int length;
        int croppedLength;
        int croppedVolume;
        int slice;
        float[][] buffer;
        float[] destBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int[] dimExtents;
        int[] destExtents;
        int j, k, offset;
        int count;
        int nDims;
        FileInfoBase[] fInfoBase;
        int nImages = Math.abs(z[1] - z[0]) + 1;
        int start = Math.min(z[0], z[1]);
        int dataType = srcImage.getType();
        String imageName = srcImage.getImageName();
        ViewUserInterface userInterface = srcImage.getUserInterface();
        float[] resols;
        int[] axisOrient;
        int[] direct;
        TransMatrix mat;
        float[] imgOriginLPS;
        float[] originImgOrd = new float[3];
        float[] newImgOriginLPS = new float[3];
        float startPos;
        String value;
        int slc;
        FileInfoDicom dicomInfoBuffer;


        if (((x != null) && (((x[0] - cushion) < 0) || ((x[1] + cushion) >= xDim))) ||
                ((y != null) && (((y[0] - cushion) < 0) || ((y[1] + cushion) >= yDim)))) {
            displayError("Algorithm Crop Image: Crop bounds exceed image bounds.");
            setCompleted(false);

            return;
        }

        dimExtents = new int[3];
        dimExtents[0] = Math.abs(x[1] - x[0]) + 1 + (2 * cushion);
        dimExtents[1] = Math.abs(y[1] - y[0]) + 1 + (2 * cushion);

        if (Math.abs(z[1] - z[0]) == 0) {
            dimExtents[2] = 1;
            destExtents = new int[2];
            nDims = 2;
            resols = new float[2];
        } else {
            dimExtents[2] = nImages; // + 2*cushion;
            nDims = 3;
            destExtents = new int[3];
            destExtents[2] = dimExtents[2];
            resols = new float[3];
            resols[2] = srcImage.getFileInfo(0).getResolutions()[2];
        }

        destExtents[0] = dimExtents[0];
        destExtents[1] = dimExtents[1];
        resols[0] = srcImage.getFileInfo(0).getResolutions()[0];
        resols[1] = srcImage.getFileInfo(0).getResolutions()[1];
        direct = new int[nDims];

        if (RGBImage) {
            croppedLength = 4 * dimExtents[0] * dimExtents[1];
        } else {
            croppedLength = dimExtents[0] * dimExtents[1];
        }

        croppedVolume = croppedLength * dimExtents[2];

        int mod = croppedVolume / 100; // mod is 1 percent of length

        if (RGBImage) {
            length = 4 * xDim * yDim;
        } else {
            length = xDim * yDim;
        }

        try {
            buffer = new float[nImages][length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Crop Image: Out of memory", true);

            return;
        }

        buildProgressBar(srcImage.getImageName(), "Cropping image ...", 0, 100);
        initProgressBar();

        for (slice = z[0]; slice <= z[1]; slice++) {

            try {
                srcImage.exportData(slice * length, length, buffer[slice - z[0]]); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                errorCleanUp("Algorithm Crop Image: Image(s) locked", true);

                return;
            }
        } // for (slice = z[0]; slice <= z[1]; slice++)

        fInfoBase = new FileInfoBase[nImages];

        for (n = 0; n < nImages; n++) {
            fInfoBase[n] = (FileInfoBase) (srcImage.getFileInfo(n + start).clone());
            fInfoBase[n].setExtents(destExtents);
        }

        axisOrient = fInfoBase[0].getAxisOrientation();
        mat = srcImage.getMatrix();

        for (i = 0; i < nDims; i++) {

            if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                direct[i] = 1;
            } else {
                direct[i] = -1;
            }
        }

        for (n = 0; n < nImages; n++) {
            imgOriginLPS = fInfoBase[n].getOrigin();
            originImgOrd = originLPS2Img(imgOriginLPS, srcImage);
            originImgOrd[0] = originImgOrd[0] + (direct[0] * (x[0] - cushion) * resols[0]);
            originImgOrd[1] = originImgOrd[1] + (direct[1] * (y[0] - cushion) * resols[1]);
            newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
            fInfoBase[n].setOrigin(newImgOriginLPS);
            fInfoBase[n].setResolutions(resols);
        }

        System.out.println("anything");

        if (srcImage.getParentFrame() != null) {
            srcImage.getParentFrame().close();
            System.out.println("anything more");
        }

        srcImage.disposeLocal();
        srcImage = null;

        try {
            destBuffer = new float[croppedVolume];
        } catch (OutOfMemoryError e) {
            buffer = null;
            destBuffer = null;
            errorCleanUp("Algorithm Crop Image:  Out of memory", true);

            return;
        }

        for (slice = z[0], i = 0; slice <= z[1]; slice++) {
            j = 0;
            k = 0;

            if (RGBImage) {
                offset = (4 * (((y[0] - cushion) * xDim) + x[0] - cushion));

                for (count = 0; (count < croppedLength) && !threadStopped; i++, k++, count++) {

                    if (((i % mod) == 0) && isProgressBarVisible()) {
                        progressBar.updateValue(Math.round((float) i / (croppedVolume - 1) * 100), runningInSeparateThread);
                    }

                    if (((i % (4 * dimExtents[0])) == 0) && ((i % croppedLength) != 0)) {
                        j++;
                        k = 0;
                        offset = (4 * (((y[0] + j - cushion) * xDim) + x[0] - cushion));
                    }

                    destBuffer[i] = buffer[slice - z[0]][offset + k];
                }
            } // if (RGBImage)
            else { // not ARGB or ARGB_USHORT or ARGB_FLOAT
                offset = (((y[0] - cushion) * xDim) + x[0] - cushion);

                for (count = 0; (count < croppedLength) && !threadStopped; i++, k++, count++) {

                    if (((i % mod) == 0) && isProgressBarVisible()) {
                        progressBar.updateValue(Math.round((float) i / (croppedVolume - 1) * 100), runningInSeparateThread);
                    }

                    if (((i % dimExtents[0]) == 0) && ((i % croppedLength) != 0)) {
                        j++;
                        k = 0;
                        offset = (((y[0] + j - cushion) * xDim) + x[0] - cushion);
                    }

                    destBuffer[i] = buffer[slice - z[0]][offset + k];
                }
            } // not ARGB or ARGB_USHORT or ARGB_FLOAT
        } // for (slice = z[0], i = 0; slice <= z[1]; slice++)

        for (i = 0; i < buffer.length; i++) {
            buffer[i] = null;
        }

        buffer = null;

        if (threadStopped) {
            return;
        }

        System.out.println("newing sourceImage");
        srcImage = new ModelImage(dataType, destExtents, imageName, userInterface);

        for (n = 0; n < nImages; n++) {
            srcImage.setFileInfo(fInfoBase[n], n);
        }

        srcImage.setMatrix(mat);

        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {

            if (nDims == 2) {
                ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26);
                ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
                ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0028,0011", new Short((short) destExtents[0]), 2); // columns
                ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0028,0010", new Short((short) destExtents[1]), 2); // rows
                ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0020,0013", Short.toString((short) (1)),
                                                                     Short.toString((short) (1)).length()); // instance number

                int imgOrient = ((FileInfoDicom) (srcImage.getFileInfo(0))).getImageOrientation();

                startPos = originImgOrd[2];
                originImgOrd[2] = startPos + (direct[2] * z[0] * resols[2]);
                newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
                value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                        Float.toString(newImgOriginLPS[2]);

                ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0020,0032", value, value.length());

                value = String.valueOf(originImgOrd[2]);
                ((FileInfoDicom) (srcImage.getFileInfo(0))).setValue("0020,1041", value, value.length());
            } // if (nDims = 2)
            else { // nDims == 3

                for (n = 0, slc = z[0]; slc <= z[1]; n++, slc++) {

                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26);
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0002,0013", "MIPAV--NIH", 10); //
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0028,0011", new Short((short) destExtents[0]),
                                                                         2); // columns
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0028,0010", new Short((short) destExtents[1]),
                                                                         2); // rows
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0020,0013", Short.toString((short) (n + 1)),
                                                                         Short.toString((short) (n + 1)).length()); // instance number
                    dicomInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(slc - z[0]);

                    // change the slice number ("0020,0013"):
                    // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                    value = Integer.toString(slc - (int) z[0] + 1);
                    dicomInfoBuffer.setValue("0020,0013", value, value.length());

                    // change the image start position ("0020, 0032")
                    startPos = originImgOrd[2];
                    originImgOrd[2] = startPos + (direct[2] * z[0] * resols[2]);
                    newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
                    value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                            Float.toString(newImgOriginLPS[2]);
                    dicomInfoBuffer.setValue("0020,0032", value, value.length());

                    // readjust the slice location ("0020,1041")
                    value = String.valueOf(originImgOrd[2]);
                    dicomInfoBuffer.setValue("0020,1041", value, value.length());
                } // for ( n = 0, slc = z[0]; slc <= z[1]; n++, slc++ )
            } // else nDims == 3
        } // if ( ( srcImage.getFileInfo()[0] ).getFileFormat() == FileBase.DICOM )


        try {
            srcImage.importData(0, destBuffer, true);
            System.out.println("imported data");
        } catch (IOException error) {
            displayError("Algorithm RGBtoGray: Output Image(s) locked");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * This function crops srcImage Must use getSrcImage after running.
     */
    private void calcStoreInPlace4D() {

        int i, n;
        int length;
        int volume;
        int croppedLength;
        int croppedVolume;
        int croppedSeries;
        int slice;
        float[][] buffer;
        float[] destBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        int[] dimExtents;
        int[] destExtents;
        int j, k, offset;
        int count;
        int t;
        int nDims;
        FileInfoBase[] fInfoBase;
        int nImages = Math.abs(z[1] - z[0]) + 1;
        int start = Math.min(z[0], z[1]);
        int dataType = srcImage.getType();
        String imageName = srcImage.getImageName();
        ViewUserInterface userInterface = srcImage.getUserInterface();
        float[] resols;
        int[] axisOrient;
        int[] direct;
        TransMatrix mat;
        float[] imgOriginLPS;
        float[] originImgOrd = new float[3];
        float[] newImgOriginLPS = new float[3];
        float startPos;
        String value;
        int slc;
        FileInfoDicom dicomInfoBuffer;


        if (((x != null) && (((x[0] - cushion) < 0) || ((x[1] + cushion) >= xDim))) ||
                ((y != null) && (((y[0] - cushion) < 0) || ((y[1] + cushion) >= yDim)))) {
            displayError("Algorithm Crop Image: Crop bounds exceed image bounds.");
            setCompleted(false);

            return;
        }

        dimExtents = new int[3];
        dimExtents[0] = Math.abs(x[1] - x[0]) + 1 + (2 * cushion);
        dimExtents[1] = Math.abs(y[1] - y[0]) + 1 + (2 * cushion);

        if (Math.abs(z[1] - z[0]) == 0) {
            dimExtents[2] = 1;
            destExtents = new int[3];
            destExtents[2] = tDim;
            nDims = 3;
            resols = new float[3];
            resols[2] = srcImage.getFileInfo(0).getResolutions()[3];
        } else {
            dimExtents[2] = nImages; // + 2*cushion;
            destExtents = new int[4];
            destExtents[3] = tDim;
            destExtents[2] = dimExtents[2];
            nDims = 4;
            resols = new float[4];
            resols[3] = srcImage.getFileInfo(0).getResolutions()[3];
            resols[2] = srcImage.getFileInfo(0).getResolutions()[2];
        }

        destExtents[0] = dimExtents[0];
        destExtents[1] = dimExtents[1];
        resols[0] = srcImage.getFileInfo(0).getResolutions()[0];
        resols[1] = srcImage.getFileInfo(0).getResolutions()[1];
        direct = new int[nDims];


        if (RGBImage) {
            croppedLength = 4 * dimExtents[0] * dimExtents[1];
        } else {
            croppedLength = dimExtents[0] * dimExtents[1];
        }

        croppedVolume = croppedLength * dimExtents[2];
        croppedSeries = croppedVolume * tDim;

        int mod = croppedSeries / 100; // mod is 1 percent of length

        if (RGBImage) {
            length = 4 * xDim * yDim;
        } else {
            length = xDim * yDim;
        }

        volume = length * zDim;

        try {
            buffer = new float[tDim * nImages][length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            errorCleanUp("Algorithm Crop Image: Out of memory", true);

            return;
        }

        buildProgressBar(srcImage.getImageName(), "Cropping image ...", 0, 100);
        initProgressBar();

        for (t = 0; t < tDim; t++) {

            for (slice = z[0]; slice <= z[1]; slice++) {

                try {
                    srcImage.exportData((t * volume) + (slice * length), length, buffer[(t * nImages) + slice - z[0]]); // locks and releases lock
                } catch (IOException error) {
                    buffer = null;
                    errorCleanUp("Algorithm Crop Image: Image(s) locked", true);

                    return;
                }
            } // for (slice = z[0]; slice <= z[1]; slice++)
        } // for (t = 0; t < tDim; t++)

        fInfoBase = new FileInfoBase[tDim * nImages];

        for (t = 0; t < tDim; t++) {

            for (n = 0; n < nImages; n++) {
                fInfoBase[(t * nImages) + n] = (FileInfoBase) (srcImage.getFileInfo((t * zDim) + n + start).clone());
                fInfoBase[(t * nImages) + n].setExtents(destExtents);
            }
        } // for (t = 0; t < tDim; t++)

        axisOrient = fInfoBase[0].getAxisOrientation();
        mat = srcImage.getMatrix();

        for (i = 0; i < 3; i++) {

            if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                direct[i] = 1;
            } else {
                direct[i] = -1;
            }
        }

        for (n = 0; n < (tDim * nImages); n++) {
            imgOriginLPS = fInfoBase[n].getOrigin();
            originImgOrd = originLPS2Img(imgOriginLPS, srcImage);
            originImgOrd[0] = originImgOrd[0] + (direct[0] * (x[0] - cushion) * resols[0]);
            originImgOrd[1] = originImgOrd[1] + (direct[1] * (y[0] - cushion) * resols[1]);
            newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
            fInfoBase[n].setOrigin(newImgOriginLPS);
            fInfoBase[n].setResolutions(resols);
        }


        if (srcImage.getParentFrame() != null) {
            srcImage.getParentFrame().close();
        }

        srcImage.disposeLocal();
        srcImage = null;

        try {
            destBuffer = new float[croppedSeries];
        } catch (OutOfMemoryError e) {
            buffer = null;
            destBuffer = null;
            errorCleanUp("Algorithm Crop Image:  Out of memory", true);

            return;
        }

        for (t = 0, i = 0; t < tDim; t++) {

            for (slice = z[0]; slice <= z[1]; slice++) {
                j = 0;
                k = 0;

                if (RGBImage) {
                    offset = (4 * (((y[0] - cushion) * xDim) + x[0] - cushion));

                    for (count = 0; (count < croppedLength) && !threadStopped; i++, k++, count++) {

                        if (((i % mod) == 0) && isProgressBarVisible()) {
                            progressBar.updateValue(Math.round((float) i / (croppedSeries - 1) * 100), runningInSeparateThread);
                        }

                        if (((i % (4 * dimExtents[0])) == 0) && ((i % croppedLength) != 0)) {
                            j++;
                            k = 0;
                            offset = (4 * (((y[0] + j - cushion) * xDim) + x[0] - cushion));
                        }

                        destBuffer[i] = buffer[(t * nImages) + slice - z[0]][offset + k];
                    }
                } // if (RGBImage)
                else { // not ARGB or ARGB_USHORT or ARGB_FLOAT
                    offset = (((y[0] - cushion) * xDim) + x[0] - cushion);

                    for (count = 0; (count < croppedLength) && !threadStopped; i++, k++, count++) {

                        if (((i % mod) == 0) && isProgressBarVisible()) {
                            progressBar.updateValue(Math.round((float) i / (croppedSeries - 1) * 100), runningInSeparateThread);
                        }

                        if (((i % dimExtents[0]) == 0) && ((i % croppedLength) != 0)) {
                            j++;
                            k = 0;
                            offset = (((y[0] + j - cushion) * xDim) + x[0] - cushion);
                        }

                        destBuffer[i] = buffer[(t * nImages) + slice - z[0]][offset + k];
                    }
                } // not ARGB or ARGB_USHORT or ARGB_FLOAT
            } // for (slice = z[0], i = 0; slice <= z[1]; slice++)
        } // for (t = 0; t < tDim; t++)

        for (i = 0; i < buffer.length; i++) {
            buffer[i] = null;
        }

        buffer = null;

        if (threadStopped) {
            return;
        }

        srcImage = new ModelImage(dataType, destExtents, imageName, userInterface);

        for (n = 0; n < (tDim * nImages); n++) {
            srcImage.setFileInfo(fInfoBase[n], n);
        }

        srcImage.setMatrix(mat);

        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {

            if (nDims == 3) {

                for (n = 0, slc = z[0]; slc <= z[1]; n++, slc++) {

                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26);
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0002,0013", "MIPAV--NIH", 10); //
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0028,0011", new Short((short) destExtents[0]),
                                                                         2); // columns
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0028,0010", new Short((short) destExtents[1]),
                                                                         2); // rows
                    ((FileInfoDicom) (srcImage.getFileInfo(n))).setValue("0020,0013", Short.toString((short) (n + 1)),
                                                                         Short.toString((short) (n + 1)).length()); // instance number
                    dicomInfoBuffer = (FileInfoDicom) srcImage.getFileInfo(slc - z[0]);

                    // change the slice number ("0020,0013"):
                    // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                    value = Integer.toString(slc - (int) z[0] + 1);
                    dicomInfoBuffer.setValue("0020,0013", value, value.length());

                    // change the image start position ("0020, 0032")
                    startPos = originImgOrd[2];
                    originImgOrd[2] = startPos + (direct[2] * z[0] * resols[2]);
                    newImgOriginLPS = originImg2LPS(originImgOrd, srcImage);
                    value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                            Float.toString(newImgOriginLPS[2]);
                    dicomInfoBuffer.setValue("0020,0032", value, value.length());

                    // readjust the slice location ("0020,1041")
                    value = String.valueOf(originImgOrd[2]);
                    dicomInfoBuffer.setValue("0020,1041", value, value.length());
                } // for ( n = 0, slc = z[0]; slc <= z[1]; n++, slc++ )
            } // if (nDims == 3)
        } // if ( ( srcImage.getFileInfo()[0] ).getFileFormat() == FileBase.DICOM )


        try {
            srcImage.importData(0, destBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm RGBtoGray: Output Image(s) locked");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        disposeProgressBar();
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        String paramString = "";

        paramString += "x bounds: " + x[0] + " " + x[1] + ", y bounds: " + y[0] + " " + y[1] + ", z bounds: " + z[0] +
                       " " + z[1] + ", " + cushion;

        historyString = new String("Crop image(" + paramString + ")\n");
    }


    /**
     * Switch origin order from image order to LPS order.
     *
     * @param   origImg  DOCUMENT ME!
     * @param   img      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] originImg2LPS(float[] origImg, ModelImage img) {
        float[] origLPS = new float[3];
        Matrix img2LPS = new Matrix(4, 4);

        img2LPS = (Matrix) img.getMatrix();

        for (int i = 0; i < 3; i++) { // i's are the rows

            for (int j = 0; j < 3; j++) { // j's are the columns

                if (img2LPS.get(i, j) != 0) {
                    origLPS[i] = origImg[j];
                }
            }
        }

        return origLPS;
    }

    /**
     * Switch origin order from LPS order to Img order.
     *
     * @param   origLPS  DOCUMENT ME!
     * @param   img      DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] originLPS2Img(float[] origLPS, ModelImage img) {
        float[] origImg = new float[3];
        Matrix LPS2img = new Matrix(4, 4);

        LPS2img = (Matrix) img.getMatrix().inverse();

        for (int i = 0; i < 3; i++) { // i's are the rows

            for (int j = 0; j < 3; j++) { // j's are the columns

                if (LPS2img.get(i, j) != 0) {
                    origImg[i] = origLPS[j];
                }
            }
        }

        return origImg;
    }

    /**
     * Update special case DICOM format tags.
     */
    private void updateDICOM() {
        int n;
        float startPos;
        float[] imgOriginLPS;
        float[] originImgOrd = new float[3];
        float[] newImgOriginLPS = new float[3];
        float[] resols;
        String value;
        int slc;
        int[] destExtents = destImage.getExtents();
        int[] direct = new int[3];
        FileInfoDicom dicomInfoBuffer;

        dicomInfoBuffer = (FileInfoDicom) destImage.getFileInfo(0);
        destImage.setMatrix(srcImage.getMatrix());

        imgOriginLPS = dicomInfoBuffer.getOrigin();
        originImgOrd = originLPS2Img(imgOriginLPS, destImage);

        resols = srcImage.getFileInfo(0).getResolutions();

        if ((destImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {

            if ((destImage.getNDims() == 2) || (z[0] == z[1])) {
                ((FileInfoDicom) (destImage.getFileInfo(0))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary Capture SOP UID
                ((FileInfoDicom) (destImage.getFileInfo(0))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26);
                ((FileInfoDicom) (destImage.getFileInfo(0))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                ((FileInfoDicom) (destImage.getFileInfo(0))).setValue("0002,0013", "MIPAV--NIH", 10); //
                ((FileInfoDicom) (destImage.getFileInfo(0))).setValue("0028,0011", new Short((short) destExtents[0]),
                                                                      2); // columns
                ((FileInfoDicom) (destImage.getFileInfo(0))).setValue("0028,0010", new Short((short) destExtents[1]),
                                                                      2); // rows
                ((FileInfoDicom) (destImage.getFileInfo(0))).setValue("0020,0013", Short.toString((short) (1)),
                                                                      Short.toString((short) (1)).length()); // instance number

                int imgOrient = ((FileInfoDicom) (destImage.getFileInfo(0))).getImageOrientation();

                startPos = originImgOrd[2];
                originImgOrd[2] = startPos + (direct[2] * z[0] * resols[2]);
                newImgOriginLPS = originImg2LPS(originImgOrd, destImage);
                value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                        Float.toString(newImgOriginLPS[2]);

                ((FileInfoDicom) (destImage.getFileInfo(0))).setValue("0020,0032", value, value.length());

                value = String.valueOf(originImgOrd[2]);
                ((FileInfoDicom) (destImage.getFileInfo(0))).setValue("0020,1041", value, value.length());
            } else if (destImage.getNDims() == 3) {
                int imgOrient = dicomInfoBuffer.getImageOrientation();

                for (n = 0, slc = z[0]; slc <= z[1]; n++, slc++) {

                    ((FileInfoDicom) (destImage.getFileInfo(n))).setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ",
                                                                          26); // Secondary Capture SOP UID
                    ((FileInfoDicom) (destImage.getFileInfo(n))).setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ",
                                                                          26);
                    ((FileInfoDicom) (destImage.getFileInfo(n))).setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID made up by Matt
                    ((FileInfoDicom) (destImage.getFileInfo(n))).setValue("0002,0013", "MIPAV--NIH", 10); //
                    ((FileInfoDicom) (destImage.getFileInfo(n))).setValue("0028,0011",
                                                                          new Short((short) destExtents[0]), 2); // columns
                    ((FileInfoDicom) (destImage.getFileInfo(n))).setValue("0028,0010",
                                                                          new Short((short) destExtents[1]), 2); // rows
                    ((FileInfoDicom) (destImage.getFileInfo(n))).setValue("0020,0013", Short.toString((short) (n + 1)),
                                                                          Short.toString((short) (n + 1)).length()); // instance number
                    dicomInfoBuffer = (FileInfoDicom) destImage.getFileInfo(slc - z[0]);

                    // change the slice number ("0020,0013"):
                    // Image slice numbers start at 1; index starts at 0, so compensate by adding 1
                    value = Integer.toString(slc - (int) z[0] + 1);
                    dicomInfoBuffer.setValue("0020,0013", value, value.length());

                    // change the image start position ("0020, 0032")
                    startPos = originImgOrd[2];
                    originImgOrd[2] = startPos + (direct[2] * z[0] * resols[2]);
                    newImgOriginLPS = originImg2LPS(originImgOrd, destImage);
                    value = Float.toString(newImgOriginLPS[0]) + "\\" + Float.toString(newImgOriginLPS[1]) + "\\" +
                            Float.toString(newImgOriginLPS[2]);
                    dicomInfoBuffer.setValue("0020,0032", value, value.length());

                    // readjust the slice location ("0020,1041")
                    value = String.valueOf(originImgOrd[2]);
                    dicomInfoBuffer.setValue("0020,1041", value, value.length());
                }
            }
        }
    }

    /**
     * Updates important image attributes (start locations, orientation ) for the new cropped file by modifing the
     * fileinfo fo the new ( destination ) image.
     */
    private void updateFileInfoData() {
        int i;
        float[] resols;
        float[] resolsTmp;
        float[] imgOriginLPS;
        float[] originImgOrd = new float[3];
        float[] newImgOriginLPS = new float[3];
        int[] axisOrient;
        int[] extentsTmp;
        FileInfoBase fileInfoBuffer;
        int nDims = destImage.getNDims();

        int[] direct = new int[nDims];

        if (nDims != srcImage.getNDims()) {

            if (nDims == 2) {
                destImage.setFileInfo(new FileInfoBase[1]);
            } else if (nDims == 3) {
                destImage.setFileInfo(new FileInfoBase[destImage.getExtents()[2]]);
            } else if (nDims == 4) {
                destImage.setFileInfo(new FileInfoBase[destImage.getExtents()[2] * destImage.getExtents()[3]]);
            }
        } else { // Ruida this was the problem.

            // destImage.setFileInfo( (FileInfoBase [])(srcImage.getFileInfo().clone()) );
        }

        int start = z[0];

        if (z[1] < z[0]) {
            start = z[1];
        }

        if (nDims == 2) {
            destImage.setMatrix(srcImage.getMatrix());

            // Copies the source's image file info.
            destImage.setFileInfo((FileInfoBase) (srcImage.getFileInfo()[start].clone()), 0);
            fileInfoBuffer = (FileInfoBase) destImage.getFileInfo(0);
            resols = fileInfoBuffer.getResolutions();
            axisOrient = fileInfoBuffer.getAxisOrientation();

            for (i = 0; i < nDims; i++) {

                if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                    direct[i] = 1;
                } else {
                    direct[i] = -1;
                }
            }

            imgOriginLPS = fileInfoBuffer.getOrigin();
            originImgOrd = originLPS2Img(imgOriginLPS, destImage);
            originImgOrd[0] = originImgOrd[0] + (direct[0] * (x[0] - cushion) * resols[0]);
            originImgOrd[1] = originImgOrd[1] + (direct[1] * (y[0] - cushion) * resols[1]);
            newImgOriginLPS = originImg2LPS(originImgOrd, destImage);
            fileInfoBuffer.setOrigin(newImgOriginLPS);

            if (srcImage.getNDims() == 3) {
                resolsTmp = new float[2];
                resolsTmp[0] = resols[0];
                resolsTmp[1] = resols[1];
                fileInfoBuffer.setResolutions(resolsTmp);
                extentsTmp = new int[2];
                extentsTmp[0] = destImage.getExtents()[0];
                extentsTmp[1] = destImage.getExtents()[1];
                fileInfoBuffer.setExtents(extentsTmp);
            } else {
                fileInfoBuffer.setResolutions(resols);
                fileInfoBuffer.setExtents(destImage.getExtents());
            }
        } else if (nDims == 3) {
            destImage.setMatrix(srcImage.getMatrix());

            for (int m = 0; m <= Math.abs(z[1] - z[0]); m++) {
                destImage.setFileInfo((FileInfoBase) (srcImage.getFileInfo()[start + m].clone()), m);
            }

            fileInfoBuffer = destImage.getFileInfo(0);
            resols = fileInfoBuffer.getResolutions();
            axisOrient = fileInfoBuffer.getAxisOrientation();

            if (srcImage.getNDims() == 4) { // Reallocate resolutions since we are going from 4D to 3D
                resolsTmp = new float[3];
                resolsTmp[0] = resols[0];
                resolsTmp[1] = resols[1];
                resolsTmp[2] = resols[3];

                extentsTmp = new int[3];
                extentsTmp[0] = destImage.getExtents()[0];
                extentsTmp[1] = destImage.getExtents()[1];
                extentsTmp[2] = destImage.getExtents()[3];
            } else {
                resolsTmp = resols;
                extentsTmp = destImage.getExtents();
            }

            for (i = 0; i < nDims; i++) {

                if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                    direct[i] = 1;
                } else {
                    direct[i] = -1;
                }
            }

            for (int s = 0; s < destImage.getExtents()[2]; s++) { // for each slice
                fileInfoBuffer = destImage.getFileInfo(s);

                imgOriginLPS = fileInfoBuffer.getOrigin();
                originImgOrd = originLPS2Img(imgOriginLPS, destImage);
                originImgOrd[0] = originImgOrd[0] + (direct[0] * (x[0] - cushion) * resols[0]);
                originImgOrd[1] = originImgOrd[1] + (direct[1] * (y[0] - cushion) * resols[1]);
                newImgOriginLPS = originImg2LPS(originImgOrd, destImage);

                destImage.getFileInfo(s).setExtents(extentsTmp);
                fileInfoBuffer.setOrigin(newImgOriginLPS);
            }
        } else {
            destImage.setMatrix(srcImage.getMatrix());

            int slice = 0;

            for (int t = 0; t < srcImage.getExtents()[3]; t++) {

                for (int m = 0; m <= Math.abs(z[1] - z[0]); m++, slice++) {
                    destImage.setFileInfo((FileInfoBase)
                                              (srcImage.getFileInfo()[start + m + (t * srcImage.getExtents()[2])].clone()),
                                          slice);
                }
            }

            fileInfoBuffer = destImage.getFileInfo(0);
            resols = fileInfoBuffer.getResolutions();
            axisOrient = fileInfoBuffer.getAxisOrientation();

            for (i = 0; i < 3; i++) {

                if ((axisOrient[i] == 1) || (axisOrient[i] == 4) || (axisOrient[i] == 5)) {
                    direct[i] = 1;
                } else {
                    direct[i] = -1;
                }
            }

            for (int s = 0; s < (destImage.getExtents()[2] * destImage.getExtents()[3]); s++) {
                fileInfoBuffer = destImage.getFileInfo(s);
                imgOriginLPS = fileInfoBuffer.getOrigin();
                originImgOrd = originLPS2Img(imgOriginLPS, destImage);
                destImage.getFileInfo(s).setExtents(destImage.getExtents());
                originImgOrd[0] = originImgOrd[0] + (direct[0] * (x[0] - cushion) * resols[0]);
                originImgOrd[1] = originImgOrd[1] + (direct[1] * (y[0] - cushion) * resols[1]);
                newImgOriginLPS = originImg2LPS(originImgOrd, destImage);
                fileInfoBuffer.setOrigin(newImgOriginLPS);
            }
        }

        if ((srcImage.getFileInfo()[0]).getFileFormat() == FileBase.DICOM) {
            updateDICOM();
        }

    }

}
