package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;


/**
 * FaceAnonymizer algorithm computes the "face" from a ModelImage based on input parameters that specify the min/max
 * voxel values for the face voxels, and the maximum skin thinkness. The algorithm also blurs the face voxels based on
 * input parameter for blur factor. The face and blured face are stored in the ModelImage so the user can see the
 * results. On getting an OK message, the blurred face is recombined with the rest of the head volume. On getting a
 * Cancel message the original volume is restored.
 *
 * @author  Alexandra Bokinsky, Magic Software. Under contract from NIH.
 */
public class AlgorithmFaceAnonymizer extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int FIND = 0;

    /** DOCUMENT ME! */
    public static final int BLUR = 1;

    /** DOCUMENT ME! */
    public static final int DELETE = 2;

    /** DOCUMENT ME! */
    public static final int OK = 3;

    /** DOCUMENT ME! */
    public static final int CANCEL = 4;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] kernXBuffer;

    /** DOCUMENT ME! */
    private float[] kernYBuffer;

    /** DOCUMENT ME! */
    private float[] kernZBuffer;

    /** face mask to keep track of which voxels are in the face:. */
    private boolean[] m_abMask;

    /** parameters for finding and bluring the face:. */
    private int[] m_aiAxis = { -1, 0, 0 };

    /** the original unblurred data:. */
    private int[] m_asOriginal;

    /** m_bFinish is set to true on an OK or Cancel message:. */
    private boolean m_bFinish;

    /**
     * m_iMinValue is used to set the minimum value in the face and blurred face images so that the image is displayed
     * "nicely" in the image window.
     */
    private double m_dMinValue;

    /** DOCUMENT ME! */
    private int m_iBlur = 1;

    /** DOCUMENT ME! */
    private int m_iFaceDeleted = 0;

    /** DOCUMENT ME! */
    private int m_iFaceFound = 0;

    /** DOCUMENT ME! */
    private int m_iMax = 0;

    /** DOCUMENT ME! */
    private int m_iMin = 0;

    /** DOCUMENT ME! */
    private int m_iQuantity;

    /** DOCUMENT ME! */
    private int m_iRange = 1;

    /** state variables to keep track of which step the algorithm is on:. */
    private int m_iState = FIND;

    /** image parameters:. */
    private int m_iXBound;

    /** DOCUMENT ME! */
    private int m_iYBound;

    /** DOCUMENT ME! */
    private int m_iZBound;

    /** DOCUMENT ME! */
    private ModelImage m_kHeadOriginal;

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private int xMaskMax;

    /** DOCUMENT ME! */
    private int xMaskMin;

    /** DOCUMENT ME! */
    private int yMaskMax;

    /** DOCUMENT ME! */
    private int yMaskMin;

    /** DOCUMENT ME! */
    private int zMaskMax;

    /** DOCUMENT ME! */
    private int zMaskMin;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Create a face Anonymizer.
     *
     * @param  srcImg  the source image
     * @param  aiAxis  the axis that the face points towards
     * @param  iMin    the minimum intensity value for the face
     * @param  iMax    the maximum intensity value for the face
     * @param  iRange  thickness in number of voxels value for the skin
     */
    public AlgorithmFaceAnonymizer(ModelImage srcImg, int[] aiAxis, int iMin, int iMax, int iRange) {

        // source image:
        m_kHeadOriginal = srcImg;

        // find face and blur face parameters:
        m_aiAxis[0] = aiAxis[0];
        m_aiAxis[1] = aiAxis[1];
        m_aiAxis[2] = aiAxis[2];
        m_iMin = iMin;
        m_iMax = iMax;
        m_iRange = iRange;
        // m_iBlur = iBlur;

        // algorithm state variables
        m_iFaceFound = 0;
        m_bFinish = false;

        // allocate memory and copy the original data into m_asOriginal:
        m_iXBound = m_kHeadOriginal.getExtents()[0];
        m_iYBound = m_kHeadOriginal.getExtents()[1];
        m_iZBound = m_kHeadOriginal.getExtents()[2];

        // Only look for a face in the anterior half of the image
        xMaskMin = 0;
        xMaskMax = m_iXBound - 1;
        yMaskMin = 0;
        yMaskMax = m_iYBound - 1;
        zMaskMin = 0;
        zMaskMax = m_iZBound - 1;

        if (m_aiAxis[0] == 1) {
            xMaskMin = (m_iXBound - 1) / 2;
        } else if (m_aiAxis[0] == -1) {
            xMaskMax = (m_iXBound - 1) / 2;
        } else if (m_aiAxis[1] == 1) {
            yMaskMin = (m_iYBound - 1) / 2;
        } else if (m_aiAxis[1] == -1) {
            yMaskMax = (m_iYBound - 1) / 2;
        } else if (m_aiAxis[2] == 1) {
            zMaskMin = (m_iZBound - 1) / 2;
        } else if (m_aiAxis[2] == -1) {
            zMaskMax = (m_iZBound - 1) / 2;
        }

        sliceSize = m_iXBound * m_iYBound;
        m_iQuantity = sliceSize * m_iZBound;

        m_asOriginal = new int[m_iQuantity];
        m_abMask = new boolean[m_iQuantity];

        for (int i = 0; i < m_iQuantity; i++) {
            m_asOriginal[i] = m_kHeadOriginal.getInt(i);
            m_abMask[i] = false;
        }

        m_dMinValue = m_kHeadOriginal.getMin();

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Find face, blur face, commit, or cancel based on which buttons in the dialog are pressed.
     */
    public void anonymizeFace() {
        fireProgressStateChanged(m_kHeadOriginal.getImageName(), "Anonymize face ...");
        

        if (m_iState == FIND) {
            findFace();
        } else if (m_iState == BLUR) {
            blurFace();
        } else if (m_iState == DELETE) {
            deleteFace();
        } else if (m_iState == OK) {
            okFace();
        } else if (m_iState == CANCEL) {
            undoFace();
        }

        fireProgressStateChanged(100);
        

        setCompleted(true);
    }

    /**
     * Blurs the face mask voxels The blurred face volume is copied into the ModelImage and displayed to the user.
     */
    public void blurFace() {
        int i;

        if (m_iFaceFound == 0) {
            findFace();
        }

        // extends the m_abMask to fit the blurred face volume
        preBlur();

        // aiBlurResult has the array variables initialized to zero.
        int[] aiBlurResult = new int[m_iQuantity];
        float[] tempImgBuffer = new float[m_iQuantity];
        float[] tempImgBuffer2 = new float[m_iQuantity];

        float sum = 0;
        float norm = 0;
        int pix, count;
        int offsetX, offsetY, offsetZ;
        int start, end;

        int kDim;
        int halfKDim;
        int step;

        // x kernel dimensions
        kDim = kernXBuffer.length;
        halfKDim = kDim / 2;

        // used for reducing repetitive calculations
        int combined = 0;

        // convolve the image with the X dimension kernel
        for (pix = 0; pix < m_iQuantity; pix++) {

            if (m_abMask[pix]) {
                offsetX = (pix % m_iXBound) - halfKDim;
                offsetY = (pix % sliceSize) / m_iXBound;
                offsetZ = (pix / sliceSize);

                combined = (offsetY * m_iXBound) + (offsetZ * sliceSize);

                count = 0;
                sum = 0;
                norm = 0;
                start = offsetX;
                end = start + (kDim - 1);

                if (start < 0) {
                    count = count - offsetX;
                    start = 0;
                }

                if (end >= m_iXBound) {
                    end = m_iXBound - 1;
                }

                for (i = start; i <= end; i++) {
                    sum += kernXBuffer[count] * m_asOriginal[i + combined];

                    if (kernXBuffer[count] >= 0) {
                        norm += kernXBuffer[count];
                    } else {
                        norm -= kernXBuffer[count];
                    }

                    count++;
                }

                tempImgBuffer[pix] = sum / norm;
            } else {
                tempImgBuffer[pix] = 0;
            }
        }

        // y kernel dimensions
        kDim = kernYBuffer.length;
        halfKDim = kDim / 2;
        step = (kDim - 1) * m_iXBound;

        // convolve the result image from above with the Y dimension kernel
        for (pix = 0; pix < m_iQuantity; pix++) {

            if (m_abMask[pix]) {
                offsetX = (pix % m_iXBound);
                offsetY = ((pix % sliceSize) / m_iXBound) - halfKDim;
                offsetZ = (pix / sliceSize);

                combined = offsetX + (offsetZ * sliceSize);

                count = 0;
                sum = 0;
                norm = 0;
                start = offsetY * m_iXBound;
                end = start + step;

                if (start < 0) {
                    count = count - offsetY;
                    start = 0;
                }

                if (end > (m_iXBound * (m_iYBound - 1))) {
                    end = m_iXBound * (m_iYBound - 1);
                }

                for (i = start; i <= end; i += m_iXBound) {
                    sum += kernYBuffer[count] * tempImgBuffer[i + combined];

                    if (kernYBuffer[count] >= 0) {
                        norm += kernYBuffer[count];
                    } else {
                        norm -= kernYBuffer[count];
                    }

                    count++;
                }

                // use imgBuffer as a temp buffer since we won't need to use it again
                tempImgBuffer2[pix] = sum / norm;
            } else {
                tempImgBuffer2[pix] = 0;
            }
        }

        // z kernel dimensions
        kDim = kernZBuffer.length;
        halfKDim = kDim / 2;
        step = (kDim - 1) * sliceSize;

        for (pix = 0; pix < m_iQuantity; pix++) {

            if (m_abMask[pix]) {
                offsetX = (pix % m_iXBound);
                offsetY = (pix % sliceSize) / m_iXBound;
                offsetZ = (pix / sliceSize) - halfKDim;

                combined = (offsetY * m_iXBound) + offsetX;

                count = 0;
                sum = 0;
                norm = 0;
                start = offsetZ * sliceSize;
                end = start + step;

                if (start < 0) {
                    count = count - offsetZ;
                    start = 0;
                }

                if (end > (sliceSize * (m_iZBound - 1))) {
                    end = sliceSize * (m_iZBound - 1);
                }

                for (i = start; i <= end; i += sliceSize) {

                    // imgBuffer now holds the result of convolving with X and Y kernels
                    sum += kernZBuffer[count] * tempImgBuffer2[i + combined];

                    if (kernZBuffer[count] >= 0) {
                        norm += kernZBuffer[count];
                    } else {
                        norm -= kernZBuffer[count];
                    }

                    count++;
                }

                aiBlurResult[pix] = (int) ((sum / norm) + 0.5f);
            } else {

                // imgBuffer now holds the result of convolving with X and Y kernels
                aiBlurResult[pix] = 0;
            }
        }

        // Copy the blurred results back into the ModelImage
        for (i = 0; i < m_iQuantity; i++) {

            if (m_abMask[i] == true) {
                m_kHeadOriginal.set(i, aiBlurResult[i]);
            } else {
                m_kHeadOriginal.set(i, m_dMinValue);
            }
        }

        // Keep track that the face has  been blurred and can be committed:
        aiBlurResult = null;
    }

    /**
     * Cancel Button has been pressed: Copy the unaltered original data into the ModelImage.
     */
    public void deleteFace() {

        if (m_iFaceFound == 0) {
            findFace();
        }

        // Otherwise copy original data minus the faceMask into the ModelImage
        for (int i = 0; i < m_iQuantity; i++) {

            if (m_abMask[i] == false) {
                m_kHeadOriginal.set(i, m_asOriginal[i]);
            } else {
                m_kHeadOriginal.set(i, m_dMinValue);
            }
        }

        m_iFaceDeleted = 1;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        m_asOriginal = null;
        m_abMask = null;

        super.finalize();
    }

    /**
     * Computes which voxels in the ModelImage are face voxels. This is done based on the min/max voxel values for the
     * face, and the maximum face thickness value, which are set by the FaceAnonymizer dialog. The face volume is copied
     * into the ModelImage and displayed to the user.
     */
    public void findFace() {

        for (int i = 0; i < m_iQuantity; i++) {
            m_kHeadOriginal.set(i, m_dMinValue);
            m_abMask[i] = false;
        }

        if ((m_aiAxis[0] == 0) && (m_aiAxis[1] == 0)) {

            // face is oriented toward Z axis:
            findFaceZ();
        } else if (m_aiAxis[0] == 0) {

            // face is oriented toward Y axis:
            findFaceY();
        } else {

            // face is oriented toward X axis:
            findFaceX();
        }

        // Keep track that the face has already been found and is ready to blur
        m_iFaceFound = 1;

        if ((m_iState == DELETE) || (m_iState == OK)) {
            fireProgressStateChanged(50);
        }
    }

    /**
     * The head points along the X axis: rays are traced in the +/- x direction:
     */
    public void findFaceX() {
        int indexZ, indexY;

        for (int z = 0; z < m_iZBound; z++) {

            if (m_iState == FIND) {
                fireProgressStateChanged(100 * z / m_iZBound);
            } else {
                fireProgressStateChanged(50 * z / m_iZBound);
            }

            indexZ = z * sliceSize;

            for (int y = 0; y < m_iYBound; y++) {
                indexY = indexZ + (y * m_iXBound);

                int num_voxels_in_range = 0;
                int x;

                if (m_aiAxis[0] == 1) {
                    x = m_iXBound - 1;
                } else {
                    x = 0;
                }

                while ((x <= xMaskMax) && (x >= xMaskMin)) {
                    int index = indexY + x;

                    // If the voxel value is within range set the mask bit
                    // and copy into the ModelImage:
                    if ((m_asOriginal[index] >= m_iMin) && (m_asOriginal[index] <= m_iMax)) {
                        m_kHeadOriginal.set(index, m_asOriginal[index]);
                        m_abMask[index] = true;
                        num_voxels_in_range++;

                    } // Face voxels have been found but the current voxel

                    // is out of range, so quit:
                    else if (num_voxels_in_range > 0) {
                        break;
                    }

                    // Face voxels have been found and we've reached the
                    // maximum thickness so quit:
                    if (num_voxels_in_range == m_iRange) {
                        break;
                    }

                    x -= m_aiAxis[0];
                }
            }
        }
    }

    /**
     * The head points along the Y axis: rays are traced in the +/- y direction:
     */
    public void findFaceY() {
        int indexZ, indexX;

        for (int z = 0; z < m_iZBound; z++) {

            if (m_iState == FIND) {
                fireProgressStateChanged(100 * z / m_iZBound);
            } else {
                fireProgressStateChanged(50 * z / m_iZBound);
            }

            indexZ = z * sliceSize;

            for (int x = 0; x < m_iXBound; x++) {

                indexX = indexZ + x;

                int num_voxels_in_range = 0;
                int y;

                if (m_aiAxis[1] == 1) {
                    y = m_iYBound - 1;
                } else {
                    y = 0;
                }

                while ((y <= yMaskMax) && (y >= yMaskMin)) {
                    int index = indexX + (y * m_iXBound);

                    // IF the voxel value is within range set the mask bit
                    // and copy into the ModelImage:
                    if ((m_asOriginal[index] >= m_iMin) && (m_asOriginal[index] <= m_iMax)) {
                        m_kHeadOriginal.set(index, m_asOriginal[index]);
                        m_abMask[index] = true;
                        num_voxels_in_range++;
                    } // Face voxels have been found but the current voxel

                    // is out of range, so quit:
                    else if (num_voxels_in_range > 0) {
                        break;
                    }

                    // Face voxels have been found and we've reached the
                    // maximum thickness so quit:
                    if (num_voxels_in_range == m_iRange) {
                        break;
                    }

                    y -= m_aiAxis[1];
                }
            }
        }
    }

    /**
     * The head points along the Z axis: rays are traced in the +/- z direction:
     */
    public void findFaceZ() {
        int indexY, indexX;

        for (int y = 0; y < m_iYBound; y++) {

            if (m_iState == FIND) {
                fireProgressStateChanged(100 * y / m_iYBound);
            } else {
                fireProgressStateChanged(50 * y / m_iYBound);
            }

            indexY = y * m_iXBound;

            for (int x = 0; x < m_iXBound; x++) {
                indexX = indexY + x;

                int num_voxels_in_range = 0;
                int z;

                if (m_aiAxis[2] == 1) {
                    z = m_iZBound - 1;
                } else {
                    z = 0;
                }

                while ((z <= zMaskMax) && (z >= zMaskMin)) {
                    int index = indexX + (z * sliceSize);

                    // IF the voxel value is within range set the mask bit
                    // and copy into the ModelImage:
                    if ((m_asOriginal[index] >= m_iMin) && (m_asOriginal[index] <= m_iMax)) {
                        m_kHeadOriginal.set(index, m_asOriginal[index]);
                        m_abMask[index] = true;
                        num_voxels_in_range++;
                    } // Face voxels have been found but the current voxel

                    // is out of range, so quit:
                    else if (num_voxels_in_range > 0) {
                        break;
                    }

                    // Face voxels have been found and we've reached the
                    // maximum thickness so quit:
                    if (num_voxels_in_range == m_iRange) {
                        break;
                    }

                    z -= m_aiAxis[2];
                }
            }
        }
    }

    /**
     * Called by the dialog.
     *
     * @return  DOCUMENT ME!
     */
    public boolean isFinished() {
        return m_bFinish;
    }

    /**
     * OK Button has been pressed: Copy the blurred face volume and the original data into the ModelImage.
     */
    public void okFace() {

        // If the face mask is deleted, we're done so return:
        if (m_iFaceDeleted == 1) {
            m_bFinish = true;

            return;
        } else {
            deleteFace();
        }

        // If the face has not been blurred yet, do so:
        for (int i = 0; i < m_iQuantity; i++) {

            // If the current voxel isn't in the faceMask
            // copy from the original data:
            if (m_abMask[i] == false) {
                m_kHeadOriginal.set(i, m_asOriginal[i]);
            }
        }

        m_bFinish = true;
    }

    /**
     * Extends the m_abMask so all the voxels in the neighborhood are blurred.
     */
    public void preBlur() {

        int indexZ, indexY, index2Z, index2Y;

        // abNewMask variables are automatically initialized to false
        boolean[] abNewMask = new boolean[m_iQuantity];

        for (int z = zMaskMin; z <= zMaskMax; z++) {
            indexZ = z * sliceSize;

            for (int y = yMaskMin; y <= yMaskMax; y++) {
                indexY = indexZ + (y * m_iXBound);

                for (int x = xMaskMin; x < xMaskMax; x++) {
                    int index = indexY + x;

                    // If the voxel is in the face
                    // turn on the mask voxels in the neighborhoor
                    if (m_abMask[index] == true) {

                        for (int i = z - m_iBlur; i <= (z + m_iBlur); i++) {

                            if ((i >= zMaskMin) && (i <= zMaskMax)) {
                                index2Z = i * sliceSize;

                                for (int j = y - m_iBlur; j <= (y + m_iBlur); j++) {

                                    if ((j >= yMaskMin) && (j <= yMaskMax)) {
                                        index2Y = index2Z + (j * m_iXBound);

                                        for (int k = x - m_iBlur; k <= (x + m_iBlur); k++) {

                                            if ((k >= xMaskMin) && (k <= xMaskMax)) {
                                                int index2 = index2Y + k;

                                                abNewMask[index2] = true;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // copy the new mask into the old one:
        m_abMask = null;
        System.gc();
        m_abMask = abNewMask;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (m_kHeadOriginal == null) {
            displayError("Source Image is null");
            finalize();

            return;
        }

        if (m_kHeadOriginal.getNDims() != 3) {
            displayError("Source Image must be 3D");
            finalize();

            return;
        }

        
        anonymizeFace();
    }

    /**
     * Set the axis toward which the face points.
     *
     * @param  aiAxis  the new axis.
     */
    public void setAxis(int[] aiAxis) {
        m_aiAxis[0] = aiAxis[0];
        m_aiAxis[1] = aiAxis[1];
        m_aiAxis[2] = aiAxis[2];
    }

    /**
     * Called when the Blur Face button is pressed. the algorithm computes the blurred face and sets the displayed image
     * to be the blurred region.
     */
    public void setBlur() {
        m_iState = BLUR;
    }

    /**
     * Sets the blur factor in number of voxels.
     *
     * @param  iBlur  the new blur factor.
     */
    public void setBlur(int iBlur) {
        m_iBlur = iBlur;
    }

    /**
     * Called when the dialog Cancel button is pressed. this sets up the Algorithm state to revert to the original copy
     * of the image and to tell the dialog to cleanup and delete this algorithm.
     */
    public void setCancel() {
        m_iState = CANCEL;
    }

    /**
     * DOCUMENT ME!
     */
    public void setDelete() {
        m_iState = DELETE;
    }

    /**
     * Called when the Find Face button is pressed. the algorithm computes the face region of the volume and sets the
     * displayed image to be that region.
     */
    public void setFind() {
        m_iState = FIND;
    }

    /**
     * Sets the maximum voxel value for the skin.
     *
     * @param  iMax  the new max value.
     */
    public void setMax(int iMax) {
        m_iMax = iMax;
    }

    /**
     * Sets the minimum voxel value for the skin.
     *
     * @param  iMin  the new min value.
     */
    public void setMin(int iMin) {
        m_iMin = iMin;
    }

    /**
     * Called when the dialog OK button is pressed. this sets up the Algorithm state to commit the changes in the image
     * and to tell the dialog to cleanup and delete this algorithm.
     */
    public void setOK() {
        m_iState = OK;
    }

    /**
     * Sets the thickness in number of voxels value for the skin.
     *
     * @param  iRange  the new thickness.
     */
    public void setRange(int iRange) {
        m_iRange = iRange;
    }

    /**
     * Cancel Button has been pressed: Copy the unaltered original data into the ModelImage.
     */
    public void undoFace() {

        // If the face has not been found and blurred, return early.
        if (m_iFaceFound == 0) {
            m_bFinish = true;

            return;
        }

        // Otherwise copy original data back into the ModelImage
        for (int i = 0; i < m_iQuantity; i++) {
            m_kHeadOriginal.set(i, m_asOriginal[i]);
        }

        m_bFinish = true;
    }
}
