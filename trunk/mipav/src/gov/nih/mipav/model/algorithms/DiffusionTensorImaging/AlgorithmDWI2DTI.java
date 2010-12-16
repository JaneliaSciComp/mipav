package gov.nih.mipav.model.algorithms.DiffusionTensorImaging;


import gov.nih.mipav.util.ThreadUtil;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.Dimension;
import java.io.*;
import java.util.*;
import java.util.concurrent.CountDownLatch;

import Jama.*;
import WildMagic.LibFoundation.Mathematics.GMatrixf;
import de.jtem.numericalMethods.algebra.linear.decompose.Singularvalue;


/**
 * Algorithm calculates a Diffusion Tensor Image from a series of Diffusion Weighted Images.
 * 
 * See: Introduction to Diffusion Tensor Imaging, by Susumu Mori
 */
public class AlgorithmDWI2DTI extends AlgorithmBase implements ViewImageUpdateInterface, AlgorithmInterface {
    /** Mask Image for masking brain regions during tensor calculation: */
    private ModelImage m_kMaskImage = null;

    /** B0 weighted image: */
    private ModelImage m_kB0Image = null;

    /** ViewJFrameImage for displaying the B0 weighted image: */
    private ViewJFrameImage m_kB0Frame = null;

    /** Whether to just display the B0 weighted image and return, or to do a full calculation: */
    private boolean m_bDisplayB0 = false;

    /** Number of slices in the input images: */
    private final int m_iSlices;

    /** X Dimension of the input images: */
    private final int m_iDimX;

    /** Y Dimension of the input images: */
    private final int m_iDimY;

    /** Number of different entries in the BMatrix: */
    private final int m_iBOrig;

    /** Number of images in the weighted series: */
    private final int m_iWeights;

    /** The mean noise value: */
    private final float m_fMeanNoise;

    /** General matrix storing BMatrix values. */
    private GMatrixf m_kBMatrix = null;

    /** List of file names for the Diffusion Weighted Images, from the .path file. */
    private String[][] m_aakDWIList = null;

    /** keeps track of unique entries in the BMatrix */
    private int[] m_aiMatrixEntries;

    /** Format of the raw data: (float, int, dicom, etc.) */
    private String m_kRawImageFormat = null;

    /** Output DTI Image: */
    private ModelImage m_kDTIImage = null;

    /** handle to BSE Algorithm * */
    private AlgorithmBrainSurfaceExtractor alg;

    private final HashMap<String, float[]> m_3dbufferMap_float = new HashMap<String, float[]>();

    private final HashMap<String, float[]> m_4dbufferMap_float = new HashMap<String, float[]>();

    private final HashMap<String, int[]> m_3dbufferMap_int = new HashMap<String, int[]>();

    private final HashMap<String, int[]> m_4dbufferMap_int = new HashMap<String, int[]>();

    /**
     * Create a new AlgorithmDWI2DTI
     * 
     * @param kMaskImage mask image masking non-brain regions, if null it is calculated.
     * @param bDisplayB0 when true open and display the B0 weighted image and return.
     * @param iSlices number of slices in the raw data.
     * @param iDimX x-dimensions of the raw data.
     * @param iDimY y-dimensions of the raw data.
     * @param iBOrig number of different entries in the BMatrix.
     * @param iWeights number of weighted series.
     * @param fMeanNoise mean noise value.
     * @param aakDWIList list of file names in the weighted series.
     * @param repidx
     * @param kBMatrix BMatrix values.
     * @param kRawFormat format string for the raw data.
     */
    public AlgorithmDWI2DTI(final ModelImage kMaskImage, final boolean bDisplayB0, final int iSlices, final int iDimX,
            final int iDimY, final int iBOrig, final int iWeights, final float fMeanNoise, final String[][] aakDWIList,
            final int[] aiMatrixEntries, final GMatrixf kBMatrix, final String kRawFormat) {
        m_kMaskImage = kMaskImage;
        m_iSlices = iSlices;
        m_iDimX = iDimX;
        m_iDimY = iDimY;
        m_iBOrig = iBOrig;
        m_iWeights = iWeights;
        m_fMeanNoise = fMeanNoise;
        m_aakDWIList = aakDWIList;
        m_aiMatrixEntries = aiMatrixEntries;
        m_kBMatrix = kBMatrix;
        m_kRawImageFormat = kRawFormat;

        m_bDisplayB0 = bDisplayB0;
    }

    public void algorithmPerformed(final AlgorithmBase algorithm) {

    }

    public void disposeLocal() {
        m_kMaskImage = null;
        m_kB0Image = null;
        m_kBMatrix = null;
        m_aakDWIList = null;
        m_aiMatrixEntries = null;
        m_kRawImageFormat = null;
        m_kDTIImage = null;
    }

    /** Calculate the DTI image. If the mask image is null, calculate the mask image first. */
    public void runAlgorithm() {
        if (m_kMaskImage == null) {
            createMaskImage();
        }
        createDWIImage();

    }

    /**
     * Return the DTI Image.
     * 
     * @return the DTI Image.
     */
    public ModelImage getDTIImage() {
        return m_kDTIImage;
    }

    /**
     * Get the slice data for the image with the given slice and weight.
     * 
     * @param iSlice slice to read.
     * @param iWeight weight to read.
     * @return float[] containing the data.
     */
    private float[] readDicomWeight(final int iSlice, final int iWeight) {
        final int length = m_iDimX * m_iDimY;
        float[] buffer = new float[length];

        FileDicom fileDicom = null;
        FileInfoDicom refFileInfo = null;
        FileInfoBase fileInfo = null;

        final String kPath = m_aakDWIList[iSlice][iWeight];
        final String kDir = kPath.substring(0, kPath.lastIndexOf(File.separator)) + File.separator;
        final String kFileName = kPath.substring(kPath.lastIndexOf(File.separator) + 1, kPath.length());

        try {
            fileDicom = new FileDicom(kFileName, kDir);
            fileDicom.setQuiet(true);
            fileDicom.readHeader(true);
            refFileInfo = (FileInfoDicom) fileDicom.getFileInfo();
            fileInfo = fileDicom.getFileInfo();
            fileDicom.readImage(buffer, ModelStorageBase.SHORT, 0);
        } catch (final IOException e) {
            buffer = null;
            fileDicom = null;
        }
        fileDicom = null;
        return buffer;
    }

    /**
     * Get the slice data for the image with the given slice and weight. For 3d and 4d non-multifile images, the whole
     * buffer is read in and stored...then slice data is extracted from that buffer
     * 
     * @param iSlice slice to read.
     * @param iWeight weight to read.
     * @return float[] containing the data.
     */
    private float[] readFloatWeight(final int iSlice, final int iWeight) {
        String kPath = m_aakDWIList[iSlice][iWeight];

        if (kPath.contains("_3D_") || kPath.contains("_4D_")) {
            if (kPath.contains("_3D_")) {

                final String numSlicesString = kPath.substring(kPath.indexOf("_3D_numSlices_") + 14, kPath
                        .indexOf("_slice_"));
                final String sliceString = kPath.substring(kPath.indexOf("_slice_") + 7, kPath.length());
                final int numSlices = Integer.valueOf(numSlicesString).intValue();
                final int slice = Integer.valueOf(sliceString).intValue();
                kPath = kPath.substring(0, kPath.indexOf("_3D_"));
                if (m_3dbufferMap_float.containsKey(kPath)) {
                    float[] volBuff = m_3dbufferMap_float.get(kPath);

                    final int sliceLength = m_iDimX * m_iDimY;
                    final float[] sliceBuff = new float[sliceLength];

                    final int start = slice * sliceLength;
                    final int end = start + sliceLength;

                    for (int i = start, k = 0; i < end; i++, k++) {
                        sliceBuff[k] = volBuff[i];
                    }

                    if (iSlice == numSlices - 1) {
                        volBuff = null;
                        m_3dbufferMap_float.remove(kPath);
                    }

                    return sliceBuff;

                } else {

                    ModelImage image_3d;
                    final FileIO fileIO = new FileIO();
                    fileIO.setQuiet(true);
                    image_3d = fileIO.readImage(kPath);
                    final int m_iDimZ = numSlices;
                    final int volLength = m_iDimX * m_iDimY * m_iDimZ;
                    final float[] volBuff = new float[volLength];

                    try {
                        image_3d.exportData(0, volLength, volBuff);
                    } catch (final IOException error) {
                        System.out.println("IO exception");
                    }
                    image_3d.disposeLocal();
                    image_3d = null;

                    final int sliceLength = m_iDimX * m_iDimY;
                    final float[] sliceBuff = new float[sliceLength];

                    final int start = slice * sliceLength;
                    final int end = start + sliceLength;

                    for (int i = start, k = 0; i < end; i++, k++) {
                        sliceBuff[k] = volBuff[i];
                    }

                    m_3dbufferMap_float.put(kPath, volBuff);

                    return sliceBuff;

                }
            } else {

                final String numVolsString = kPath.substring(kPath.indexOf("_4D_numVols_") + 12, kPath
                        .indexOf("_numSlices_"));
                final String numSlicesString = kPath.substring(kPath.indexOf("_numSlices_") + 11, kPath
                        .indexOf("_vol_"));
                final String volString = kPath.substring(kPath.indexOf("_vol_") + 5, kPath.indexOf("_slice_"));
                final String sliceString = kPath.substring(kPath.indexOf("_slice_") + 7, kPath.length());
                final int numVols = Integer.valueOf(numVolsString).intValue();
                final int numSlices = Integer.valueOf(numSlicesString).intValue();
                final int vol = Integer.valueOf(volString).intValue();
                final int slice = Integer.valueOf(sliceString).intValue();
                kPath = kPath.substring(0, kPath.indexOf("_4D_"));
                if (m_4dbufferMap_float.containsKey(kPath)) {

                    float[] volsBuff = m_4dbufferMap_float.get(kPath);

                    final int sliceLength = m_iDimX * m_iDimY;
                    final float[] sliceBuff = new float[sliceLength];

                    final int start = ( (vol * numSlices) * sliceLength) + (slice * sliceLength);
                    final int end = start + sliceLength;

                    for (int i = start, k = 0; i < end; i++, k++) {
                        sliceBuff[k] = volsBuff[i];
                    }

                    if (iSlice == numSlices - 1 && iWeight == numVols - 1) {
                        volsBuff = null;
                        m_4dbufferMap_float.remove(kPath);
                    }

                    return sliceBuff;

                } else {

                    ModelImage image_4d;
                    final FileIO fileIO = new FileIO();
                    fileIO.setQuiet(true);
                    image_4d = fileIO.readImage(kPath);
                    final int m_iDimZ = numSlices;
                    final int m_iDimT = numVols;
                    final int volsLength = m_iDimX * m_iDimY * m_iDimZ * m_iDimT;
                    final float[] volsBuff = new float[volsLength];

                    try {
                        image_4d.exportData(0, volsLength, volsBuff);
                    } catch (final IOException error) {
                        System.out.println("IO exception");
                    }
                    image_4d.disposeLocal();
                    image_4d = null;

                    final int sliceLength = m_iDimX * m_iDimY;
                    final float[] sliceBuff = new float[sliceLength];

                    final int start = ( (vol * numSlices) * sliceLength) + (slice * sliceLength);
                    final int end = start + sliceLength;

                    for (int i = start, k = 0; i < end; i++, k++) {
                        sliceBuff[k] = volsBuff[i];
                    }

                    m_4dbufferMap_float.put(kPath, volsBuff);

                    return sliceBuff;

                }
            }

        } else {
            File kFile = new File(kPath);
            if ( !kFile.exists() || !kFile.canRead()) {
                MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                return null;
            }
            final int iLength = (int) kFile.length();
            if (iLength <= 0) {
                MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
                return null;
            }
            byte[] abSliceData = new byte[iLength];
            try {
                final FileInputStream kFileReader = new FileInputStream(kFile);
                kFileReader.read(abSliceData, 0, iLength);
                kFileReader.close();
                kFile = null;
            } catch (final IOException e) {}

            final int length = m_iDimX * m_iDimY;
            final float[] afResult = new float[length];
            for (int iY = 0; iY < m_iDimY; iY++) {
                for (int iX = 0; iX < m_iDimX; iX++) {
                    final int iIndex = (iY * m_iDimX) + iX;
                    afResult[iIndex] = readFloat(abSliceData, iIndex);
                }
            }
            abSliceData = null;
            return afResult;
        }

    }

    /**
     * Get the slice data for the image with the given slice and weight. For 3d and 4d non-multifile images, the whole
     * buffer is read in and stored...then slice data is extracted from that buffer
     * 
     * @param iSlice slice to read.
     * @param iWeight weight to read.
     * @return float[] containing the data.
     */
    private float[] readIntegerWeight(final int iSlice, final int iWeight) {
        String kPath = m_aakDWIList[iSlice][iWeight];

        if (kPath.contains("_3D_") || kPath.contains("_4D_")) {
            if (kPath.contains("_3D_")) {

                final String numSlicesString = kPath.substring(kPath.indexOf("_3D_numSlices_") + 14, kPath
                        .indexOf("_slice_"));
                final String sliceString = kPath.substring(kPath.indexOf("_slice_") + 7, kPath.length());
                final int numSlices = Integer.valueOf(numSlicesString).intValue();
                final int slice = Integer.valueOf(sliceString).intValue();
                kPath = kPath.substring(0, kPath.indexOf("_3D_"));
                if (m_3dbufferMap_int.containsKey(kPath)) {
                    int[] volBuff = m_3dbufferMap_int.get(kPath);

                    final int sliceLength = m_iDimX * m_iDimY;
                    final float[] sliceBuff = new float[sliceLength];

                    final int start = slice * sliceLength;
                    final int end = start + sliceLength;

                    for (int i = start, k = 0; i < end; i++, k++) {
                        sliceBuff[k] = volBuff[i];
                    }

                    if (iSlice == numSlices - 1) {
                        volBuff = null;
                        m_3dbufferMap_int.remove(kPath);
                    }

                    return sliceBuff;

                } else {

                    ModelImage image_3d;
                    final FileIO fileIO = new FileIO();
                    fileIO.setQuiet(true);
                    image_3d = fileIO.readImage(kPath);
                    final int m_iDimZ = numSlices;
                    final int volLength = m_iDimX * m_iDimY * m_iDimZ;
                    final int[] volBuff = new int[volLength];

                    try {
                        image_3d.exportData(0, volLength, volBuff);
                    } catch (final IOException error) {
                        System.out.println("IO exception");
                    }
                    image_3d.disposeLocal();
                    image_3d = null;

                    final int sliceLength = m_iDimX * m_iDimY;
                    final float[] sliceBuff = new float[sliceLength];

                    final int start = slice * sliceLength;
                    final int end = start + sliceLength;

                    for (int i = start, k = 0; i < end; i++, k++) {
                        sliceBuff[k] = volBuff[i];
                    }

                    m_3dbufferMap_int.put(kPath, volBuff);

                    return sliceBuff;

                }
            } else {

                final String numVolsString = kPath.substring(kPath.indexOf("_4D_numVols_") + 12, kPath
                        .indexOf("_numSlices_"));
                final String numSlicesString = kPath.substring(kPath.indexOf("_numSlices_") + 11, kPath
                        .indexOf("_vol_"));
                final String volString = kPath.substring(kPath.indexOf("_vol_") + 5, kPath.indexOf("_slice_"));
                final String sliceString = kPath.substring(kPath.indexOf("_slice_") + 7, kPath.length());
                final int numVols = Integer.valueOf(numVolsString).intValue();
                final int numSlices = Integer.valueOf(numSlicesString).intValue();
                final int vol = Integer.valueOf(volString).intValue();
                final int slice = Integer.valueOf(sliceString).intValue();
                kPath = kPath.substring(0, kPath.indexOf("_4D_"));
                if (m_4dbufferMap_int.containsKey(kPath)) {

                    int[] volsBuff = m_4dbufferMap_int.get(kPath);

                    final int sliceLength = m_iDimX * m_iDimY;
                    final float[] sliceBuff = new float[sliceLength];

                    final int start = ( (vol * numSlices) * sliceLength) + (slice * sliceLength);
                    final int end = start + sliceLength;

                    for (int i = start, k = 0; i < end; i++, k++) {
                        sliceBuff[k] = volsBuff[i];
                    }

                    if (iSlice == numSlices - 1 && iWeight == numVols - 1) {
                        volsBuff = null;
                        m_4dbufferMap_int.remove(kPath);
                    }

                    return sliceBuff;

                } else {

                    ModelImage image_4d;
                    final FileIO fileIO = new FileIO();
                    fileIO.setQuiet(true);
                    image_4d = fileIO.readImage(kPath);
                    final int m_iDimZ = numSlices;
                    final int m_iDimT = numVols;
                    final int volsLength = m_iDimX * m_iDimY * m_iDimZ * m_iDimT;
                    final int[] volsBuff = new int[volsLength];

                    try {
                        image_4d.exportData(0, volsLength, volsBuff);
                    } catch (final IOException error) {
                        System.out.println("IO exception");
                    }
                    image_4d.disposeLocal();
                    image_4d = null;

                    final int sliceLength = m_iDimX * m_iDimY;
                    final float[] sliceBuff = new float[sliceLength];

                    final int start = ( (vol * numSlices) * sliceLength) + (slice * sliceLength);
                    final int end = start + sliceLength;

                    for (int i = start, k = 0; i < end; i++, k++) {
                        sliceBuff[k] = volsBuff[i];
                    }

                    m_4dbufferMap_int.put(kPath, volsBuff);

                    return sliceBuff;

                }
            }

        } else {

        }

        final File kFile = new File(kPath);
        if ( !kFile.exists() || !kFile.canRead()) {
            MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
            return null;
        }
        final int iLength = (int) kFile.length();
        if (iLength <= 0) {
            MipavUtil.displayError("Error reading file: " + m_aakDWIList[iSlice][iWeight] + ".");
            return null;
        }
        final byte[] abSliceData = new byte[iLength];
        try {
            final FileInputStream kFileReader = new FileInputStream(kFile);
            kFileReader.read(abSliceData, 0, iLength);
            kFileReader.close();
        } catch (final IOException e) {}

        final int length = m_iDimX * m_iDimY;
        final float[] afResult = new float[length];
        for (int iY = 0; iY < m_iDimY; iY++) {
            for (int iX = 0; iX < m_iDimX; iX++) {
                final int iIndex = (iY * m_iDimX) + iX;
                afResult[iIndex] = readInteger(abSliceData, iIndex);
            }
        }
        return afResult;
    }

    /**
     * Get the slice data for the image with the given slice and weight.
     * 
     * @param iSlice slice to read.
     * @param iWeight weight to read.
     * @return float[] containing the data.
     */
    private float[] readSliceWeight(final int iSlice, final int iWeight) {
        float[] buffer = null;
        if (m_kRawImageFormat.equals("dicom")) {
            buffer = readDicomWeight(iSlice, iWeight);
        } else if (m_kRawImageFormat.equals("float")) {
            buffer = readFloatWeight(iSlice, iWeight);
        } else {
            buffer = readIntegerWeight(iSlice, iWeight);
        }
        return buffer;
    }

    /**
     * Create the mask image. Create an image with the B0 weighted data, if one has not already been created. Next use
     * the JDialogBrainSurfaceExtractor to extract only the brain regions.
     */
    private void createMaskImage() {
        if (m_kB0Frame == null) {
            final int iWeight = 0;
            final int[] imageExtents = new int[] {m_iDimX, m_iDimY, m_iSlices};
            m_kB0Image = new ModelImage(ModelStorageBase.FLOAT, imageExtents, new String("BrainImage"));

            final int length = m_iDimX * m_iDimY;
            for (int iSlice = 0; iSlice < m_iSlices; iSlice++) {
                final float[] buffer = readSliceWeight(iSlice, iWeight);
                try {
                    m_kB0Image.importData(length * iSlice, buffer, false);
                } catch (final IOException e) {}
            }

            m_kB0Image.addImageDisplayListener(this);
        }
        m_kB0Frame = new ViewJFrameImage(m_kB0Image, null, new Dimension(610, 200), false);

        if (m_bDisplayB0) {
            return;
        }

        try {
            // call the algorithm
            final float closeKernelSize = (Math.max(m_kB0Image.getFileInfo(0).getResolutions()[0], m_kB0Image
                    .getFileInfo(0).getResolutions()[1]) * 6) + 1;
            alg = new AlgorithmBrainSurfaceExtractor(m_kB0Image, 3, 0.5f, .62f, false, 1, closeKernelSize, 1, false,
                    false, true, true);
            alg.setRunningInSeparateThread(isRunningInSeparateThread());
            alg.addListener(this);
            alg.run();

            // now need to handle the stuff from algorithm performed
            final BitSet paintMask = alg.getComputedPaintMask();
            (m_kB0Frame).getImageA().setMask(paintMask);
            (m_kB0Frame).setActiveImage(ViewJFrameBase.IMAGE_A);

            alg.finalize();

            // necessary for paint to appear when using 'extract to paint' option
            if (m_kB0Frame != null) {
                (m_kB0Frame).getComponentImage().setPaintMask(m_kB0Image.getMask());
            }

            m_kB0Image.notifyImageDisplayListeners(null, true);
            alg = null;
        } catch (final OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Unable to allocate enough memory");

            return;
        }

    }

    /**
     * Creates the weighted data for the tensor calculation from the diffusion weighted images.
     * 
     * @return float[][][] containing the weights used in the tensor calculation.
     */
    private float[][][] createTensorWeights() {

        // ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating tensor", "creating weights mask...", 0, 100, true);

        final float[][][] norm = new float[m_iBOrig][m_iSlices][m_iDimX * m_iDimY];

        for (int i = 0; i < m_iBOrig; i++) {
            for (int j = 0; j < m_iWeights; j++) {
                if (m_aiMatrixEntries[j] == i) {
                    for (int slice = 0; slice < m_iSlices; slice++) {
                        final float[] buffer = readSliceWeight(slice, j);

                        for (int iY = 0; iY < m_iDimY; iY++) {
                            for (int iX = 0; iX < m_iDimX; iX++) {
                                final int iIndex = (iY * m_iDimX) + iX;
                                if (m_kMaskImage.getBoolean(slice * m_iDimY * m_iDimX + iIndex)) {
                                    final float fValue = buffer[iIndex];
                                    norm[i][slice][iIndex] += fValue;
                                } else {
                                    norm[i][slice][iIndex] = 0;
                                }
                            }
                        }
                    }
                }
            }
            final int iValue = (int) (100 * (float) (i + 1) / m_iBOrig);
            // kProgressBar.updateValueImmed( iValue );
        }

        // kProgressBar.dispose();
        // kProgressBar = null;
        return norm;
    }

    /**
     * Second step in processing the diffusion weighted images. This function is called once the brain extractor is
     * complete. The brain image is transformed into a mask image, which is passed to this function. The mask image
     * limits where the tensor calculations are performed. The tensor is calculated, then the eigen vectors and
     * functional anisotropy images. The DialogDTIColorDisplay is then launched.
     * 
     * @param kMaskImage mask image representing the brain.
     */
    public float[] createDWIImage() {
        final long startTime = System.currentTimeMillis();
        final float[][][] aaafWeights = createTensorWeights();

        final int iLen = m_iDimX * m_iDimY * m_iSlices;
        final float[] afTensorData = new float[iLen * 6];

        final Matrix B = new Matrix(m_kBMatrix.GetRows(), 6 + 1);
        for (int iR = 0; iR < m_kBMatrix.GetRows(); iR++) {
            for (int iC = 0; iC < 6 + 1; iC++) {
                B.set(iR, iC, m_kBMatrix.Get(iR, iC));
            }
        }
        final Matrix H = calculateMatrix(B);

        // ViewJProgressBar kProgressBar = new ViewJProgressBar("Calculating tensor", "calculating tensor...", 0, 100, true);

        if (multiThreadingEnabled) {
            final CountDownLatch doneSignal = new CountDownLatch(nthreads);
            final float step = ((float) m_iSlices) / nthreads;
            for (int i = 0; i < nthreads; i++) {
                final int start = (int) (i * step);
                final int end = (int) ( (i + 1) * step);
                final Runnable task = new Runnable() {
                    public void run() {
                        calculateDTIImage(start, end, afTensorData, aaafWeights, B, H);
                        doneSignal.countDown();
                    }
                };

                ThreadUtil.mipavThreadPool.execute(task);
            }
            try {
                doneSignal.await();
            } catch (final InterruptedException e) {
                e.printStackTrace();
            }
        } else {
            calculateDTIImage(0, m_iSlices, afTensorData, aaafWeights, B, H);
        }
        // kProgressBar.dispose();

        final int[] extents = new int[] {m_iDimX, m_iDimY, m_iSlices, 6};
        m_kDTIImage = new ModelImage(ModelStorageBase.FLOAT, extents, new String("DiffusionTensorImage"));
        try {
            m_kDTIImage.importData(0, afTensorData, true);
        } catch (final IOException e) {
            e.printStackTrace();
        }

        setCompleted(true);
        Preferences.debug("DWI->DTI:" + (System.currentTimeMillis() - startTime) + "\n");
        return afTensorData;
    }

    public void calculateDTIImage(final int start, final int end, final float[] dtiData, final float[][][] weightData,
            final Matrix B, final Matrix H) {
        for (int iSlice = start; iSlice < end; iSlice++) {
            final float[][] buffer = new float[m_iWeights][];
            for (int iWeight = 0; iWeight < m_iWeights; iWeight++) {
                buffer[iWeight] = readSliceWeight(iSlice, iWeight);
            }
            for (int iY = 0; iY < m_iDimY; iY++) {
                for (int iX = 0; iX < m_iDimX; iX++) {
                    calculateDTIVoxel(dtiData, buffer, weightData, iX, iY, iSlice, B);
                }
            }
            // int iValue = (int)(100 * (float)(iSlice+1)/(float)m_iSlices);
            // kProgressBar.updateValueImmed( iValue );
        }

    }

    public void calculateDTIVoxel(final float[] dtiData, final float[][] dwiData, final float[][][] weightData,
            final int x, final int y, final int z, final Matrix B) {
        final int vol = m_iWeights;
        final int indexInVolume = z * (m_iDimY * m_iDimX) + (y * m_iDimX) + x;
        final int indexInSlice = (y * m_iDimX) + x;
        final int volumeSize = m_iSlices * m_iDimY * m_iDimX;
        if (m_kMaskImage.getBoolean(indexInVolume)) {
            final Matrix SIGMA = Matrix.identity(vol, vol);
            final Matrix X = new Matrix(new double[vol][1]);
            final int[] r = new int[vol];
            final int[] c = new int[vol];
            int idx = 0;

            for (int iWeight = 0; iWeight < m_iWeights; iWeight++) {
                double p = dwiData[iWeight][indexInSlice];
                double w = 0; // aaafWeights[iWeight][iSlice][iIndex];
                if (weightData[m_aiMatrixEntries[iWeight]][z][indexInSlice] != 0) {
                    w = p / weightData[m_aiMatrixEntries[iWeight]][z][indexInSlice];
                }
                if (w > 0.196) {
                    r[idx] = iWeight;
                    c[idx] = iWeight;
                    idx++;
                }

                // SIGMA is a diagonal matrix and its inverse would be
                // diag(1/S(i,i))
                if (p < m_fMeanNoise) {
                    p = m_fMeanNoise;
                }

                X.set(iWeight, 0, Math.log(p));
                SIGMA.set(iWeight, iWeight, (p * p * w)); // SIGMA here
                // becomes
                // SIGMA.inverse
            }
            final Matrix B2 = B.getMatrix(r, 0, 6);
            final Matrix SIGMA2 = SIGMA.getMatrix(r, c);
            final Matrix X2 = X.getMatrix(r, 0, 0);
            final Matrix A = ( (B2.transpose()).times(SIGMA2)).times(B2);
            final Matrix Y = ( (B2.transpose()).times(SIGMA2)).times(X2);

            Matrix D = new Matrix(7, 1);

            int m = A.getRowDimension();
            int n = A.getColumnDimension();
            double[][] u = new double[m][n];
            double[][] v = new double[n][n];
            double[] s = new double[Math.min(m+1,n)];
            Singularvalue.decompose( A.getArray(), u, v, s );
            Matrix uMat = new Matrix(u);
            Matrix vMat = new Matrix(v);
            Matrix S = new Matrix(s.length, s.length);
            for (int i = 0; i < S.getRowDimension(); i++) {
                S.set(i, i, 1 / s[i]);
            }
            D = ( ( (vMat).times(S)).times(uMat.transpose())).times(Y);

            // D = [Dxx, Dxy, Dxz, Dyy, Dyz, Dzz, Amplitude]
            final float[] tensor = new float[10 + vol];
            for (int i = 0; i < 6; i++) {
                tensor[i] = (float) ( -D.get(i, 0) * 1000000); // um^2/sec
                if (i == 0 && tensor[0] < 0) {
                    tensor[0] = (float) 0.01;
                }
                if (i == 3 && tensor[3] < 0) {
                    tensor[3] = (float) 0.01;
                }
                if (i == 5 && tensor[5] < 0) {
                    tensor[5] = (float) 0.01;
                }
            }

            final float[] newTensor = new float[6];
            newTensor[0] = tensor[0];
            newTensor[1] = tensor[3];
            newTensor[2] = tensor[5];
            newTensor[3] = tensor[1];
            newTensor[4] = tensor[2];
            newTensor[5] = tensor[4];
            for (int iT = 0; iT < 6; iT++) {
                dtiData[indexInVolume + iT * volumeSize] = newTensor[iT];
            }
        } else {
            for (int iT = 0; iT < 6; iT++) {
                dtiData[indexInVolume + iT * volumeSize] = 0;
            }
        }

    }

    public void calculateDTIVoxel2(final float[] dtiData, final float[][] dwiData, final float[][][] weightData,
            final int x, final int y, final int z, final Matrix H, final int volumeSize) {
        final int indexInSlice = (y * m_iDimX) + x;
        final int indexInVolume = z * m_iDimX * m_iDimY + (y * m_iDimX) + x;
        if (m_kMaskImage.getBoolean(indexInSlice)) {
            final Matrix Y = new Matrix(m_iWeights, 1);
            for (int iWeight = 0; iWeight < m_iWeights; iWeight++) {
                Y.set(iWeight, 0, dwiData[iWeight][indexInSlice]);
            }
            final float[] tensor = solve(Y, H);
            final float[] newTensor = new float[6];
            newTensor[0] = tensor[0];
            newTensor[1] = tensor[3];
            newTensor[2] = tensor[5];
            newTensor[3] = tensor[1];
            newTensor[4] = tensor[2];
            newTensor[5] = tensor[4];
            for (int iT = 0; iT < 6; iT++) {
                dtiData[indexInVolume + iT * volumeSize] = newTensor[iT] * 1000000;
            }
        } else {
            for (int iT = 0; iT < 6; iT++) {
                dtiData[indexInSlice + iT * volumeSize] = 0;
            }
        }

    }

    public Matrix calculateMatrix(final Matrix m) {
        final int nrows = m.getRowDimension();
        final int ncols = m.getColumnDimension();
        final Matrix m2 = new Matrix(nrows, ncols);
        for (int i = 0; i < nrows; i++) {
            for (int j = 0; j < ncols - 1; j++) {
                m2.set(i, j + 1, -1 * m.get(i, j));
            }
            m2.set(i, 0, m.get(i, ncols - 1));
        }
        final Matrix m3 = m2.transpose().times(m2);
        return m3.inverse().times(m2.transpose());
    }

    public float[] solve(final Matrix y, final Matrix m) {
        final Matrix b = m.times(y);
        final float[] a = new float[b.getRowDimension()];
        for (int i = 1; i < a.length; i++) {
            a[i - 1] = (float) b.get(i, 0);
        }
        return a;
    }

    /**
     * Translates the byte[] into float values at the given indes iIndex.
     * 
     * @param abData byte[] containing float values.
     * @param iIndex index into the array to get the float from.
     * @return float value representing 4 bytes starting at abData[iIndex*4].
     */
    private float readFloat(final byte[] abData, final int iIndex) {
        final int b1 = abData[iIndex * 4 + 0] & 0xff;
        final int b2 = abData[iIndex * 4 + 1] & 0xff;
        final int b3 = abData[iIndex * 4 + 2] & 0xff;
        final int b4 = abData[iIndex * 4 + 3] & 0xff;
        final int tmpInt = ( (b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
        final float fValue = Float.intBitsToFloat(tmpInt);
        return fValue;
    }

    /**
     * Translates the byte[] into integer values at the given indes iIndex.
     * 
     * @param abData byte[] containing integer values.
     * @param iIndex index into the array to get the float from.
     * @return integer value representing 4 bytes starting at abData[iIndex*4].
     */
    private int readInteger(final byte[] abData, final int iIndex) {
        final int b1 = abData[iIndex * 4 + 0] & 0xff;
        final int b2 = abData[iIndex * 4 + 1] & 0xff;
        final int b3 = abData[iIndex * 4 + 2] & 0xff;
        final int b4 = abData[iIndex * 4 + 3] & 0xff;
        final int iValue = ( (b1 << 24) | (b2 << 16) | (b3 << 8) | b4);
        return iValue;
    }

    /** ViewImageUpdateInterface : stub */
    public void setSlice(final int slice) {}

    /** ViewImageUpdateInterface : stub */
    public void setTimeSlice(final int tSlice) {}

    /** ViewImageUpdateInterface : stub */
    public boolean updateImageExtents() {
        return false;
    }

    /** ViewImageUpdateInterface : called when the JDialogBrainSurfaceExtractor finishes. Calls processDWI. */
    public boolean updateImages() {
        if ( !m_bDisplayB0) {
            m_kMaskImage = ViewUserInterface.getReference().getRegisteredImageByName(
                    m_kB0Frame.getComponentImage().commitPaintToMask());
            m_kB0Image.removeImageDisplayListener(this);
            m_kB0Image = null;
        }
        return false;
    }

    /** ViewImageUpdateInterface : stub */
    public boolean updateImages(final boolean flag) {
        return false;
    }

    /** ViewImageUpdateInterface : stub */
    public boolean updateImages(final ModelLUT LUTa, final ModelLUT LUTb, final boolean flag, final int interpMode) {
        return false;
    }

}
