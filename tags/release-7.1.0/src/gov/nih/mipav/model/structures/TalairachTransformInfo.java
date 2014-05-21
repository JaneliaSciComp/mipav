package gov.nih.mipav.model.structures;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileInfoBase;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Serializable;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This is the structure to store information needed to compute ACPC and Talairach normalization.
 * 
 * <p>
 * If you have any questions, please drop me a line. ===== Pilou Bazin MEDIC, JHU pbazin1@jhmi.edu
 * </p>
 * 
 * @version July 26, 2004
 * @author Pierre-Louis Bazin
 * @see AlgorithmBase
 * @see TextRemoval
 * @see PlugInDialogTextRemoval
 */
public class TalairachTransformInfo implements Serializable {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5791995578653926105L;

    /** acpc constants. Lateral. */
    private static final float ACPC_LATERAL = 95.0f;

    /** acpc constants. Anterior. */
    private static final float ACPC_ANTERIOR = 95.0f;

    /** acpc constants. Posterior. */
    private static final float ACPC_POSTERIOR = 140.0f;

    /** acpc constants. Inferior. */
    private static final float ACPC_INFERIOR = 70.0f;

    /** acpc constants. Superior. */
    private static final float ACPC_SUPERIOR = 100.0f;

    /** acpc constants. AC to PC. */
    public static final int TLRC_AC_TO_PC = 23;

    /** acpc constants. Most Anterior to AC. */
    public static final int TLRC_FRONT_TO_AC = 70;

    /** acpc constants. PC to most posterior. */
    public static final int TLRC_PC_TO_BACK = 79;

    /** acpc constants. most inferior to AC. */
    public static final int TLRC_BOT_TO_AC = 42;

    /** acpc constants. AC to most superior. */
    public static final int TLRC_AC_TO_TOP = 74;

    /** acpc constants. AC to left or right. */
    public static final int TLRC_AC_TO_LAT = 68;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** Anterior Comissure in acpc space. */
    private Vector3f acpcAC = new Vector3f(95.0f, 95.0f, 70.0f);

    /** Image dimensions. */
    private int[] acpcDim = {191, 236, 171};

    /** ACPC min and max extents of the brain. */
    private Vector3f acpcMax;

    /** ACPC min and max extents of the brain. */
    private Vector3f acpcMin;

    /** Posterior Comissure in acpc space. */
    private Vector3f acpcPC = new Vector3f();

    /** Voxel resolution (cubic). */
    private float acpcRes = 1.0f;

    /** True if we have the data to compute orig <-> acpc. */
    private boolean isAcpc = false;

    /** True if we ahve the data to compute acpc <-> tlrc. */
    private boolean isTlrc = false;

    /** Anterior Comissure in original space. */
    private Vector3f origAC;

    /** Original image dimensions. */
    private int[] origDim;

    /** ACPC orientation in original image. */
    private float[][] origOrient;

    /** Original image origin. */
    private float[] origOrigin;

    /** Posterior Comissure in original space. */
    private Vector3f origPC;

    /** Original image voxel resolutions. */
    private float[] origRes;

    /** Anterior Comissure in Talairach space. */
    private Vector3f tlrcAC = new Vector3f(80.0f, 80.0f, 65.0f);

    /** Image dimensions. */
    private int[] tlrcDim = {161, 191, 151};

    /** Posterior Comissure in Talairach space. */
    private Vector3f tlrcPC = new Vector3f(80.0f, 103.0f, 65.0f);

    /** Voxel resolutions for the sub-boxes. */
    private float[] tlrcRes;

    /**
     * Whether to use the old, incorrect ACPC X dim calculation, which overcounted by one (used in backwards
     * compatibility for Dr. Pierpaoli's group).
     */
    private boolean useIncorrectAcpcXDim = false;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new TalairachTransformInfo object.
     */
    public TalairachTransformInfo() {}

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
    @Override
    public void finalize() {
        int i;
        acpcAC = null;
        acpcDim = null;
        acpcMax = null;
        acpcMin = null;
        acpcPC = null;
        origAC = null;
        origDim = null;
        if (origOrient != null) {
            for (i = 0; i < origOrient.length; i++) {
                origOrient[i] = null;
            }
        }
        origOrient = null;
        origOrigin = null;
        origPC = null;
        origRes = null;
        tlrcAC = null;
        tlrcDim = null;
        tlrcPC = null;
        tlrcRes = null;
        try {
            super.finalize();
        } catch (final Throwable er) {}
    }

    /**
     * Transforms point from acpc to orig...matrix is implicitly inverted in code.
     * 
     * @param pt old point
     * @param trans new point
     */
    public void acpcToOrig(final Vector3f pt, final Vector3f trans) {

        trans.X = origAC.X + (origOrient[0][0] * (pt.X - acpcAC.X) * acpcRes / origRes[0])
                + (origOrient[1][0] * (pt.Y - acpcAC.Y) * acpcRes / origRes[0])
                + (origOrient[2][0] * (pt.Z - acpcAC.Z) * acpcRes / origRes[0]);

        trans.Y = origAC.Y + (origOrient[0][1] * (pt.X - acpcAC.X) * acpcRes / origRes[1])
                + (origOrient[1][1] * (pt.Y - acpcAC.Y) * acpcRes / origRes[1])
                + (origOrient[2][1] * (pt.Z - acpcAC.Z) * acpcRes / origRes[1]);

        trans.Z = origAC.Z + (origOrient[0][2] * (pt.X - acpcAC.X) * acpcRes / origRes[2])
                + (origOrient[1][2] * (pt.Y - acpcAC.Y) * acpcRes / origRes[2])
                + (origOrient[2][2] * (pt.Z - acpcAC.Z) * acpcRes / origRes[2]);

        return;
    }

    /**
     * Transforms point from acpc to orig...matrix is implicitly inverted in code.
     * 
     * @param x
     * @param y
     * @param z
     * @param trans new point
     */
    public void acpcToOrig(final int x, final int y, final int z, final Vector3f trans) {

        trans.X = origAC.X + (origOrient[0][0] * (x - acpcAC.X) * acpcRes / origRes[0])
                + (origOrient[1][0] * (y - acpcAC.Y) * acpcRes / origRes[0])
                + (origOrient[2][0] * (z - acpcAC.Z) * acpcRes / origRes[0]);

        trans.Y = origAC.Y + (origOrient[0][1] * (x - acpcAC.X) * acpcRes / origRes[1])
                + (origOrient[1][1] * (y - acpcAC.Y) * acpcRes / origRes[1])
                + (origOrient[2][1] * (z - acpcAC.Z) * acpcRes / origRes[1]);

        trans.Z = origAC.Z + (origOrient[0][2] * (x - acpcAC.X) * acpcRes / origRes[2])
                + (origOrient[1][2] * (y - acpcAC.Y) * acpcRes / origRes[2])
                + (origOrient[2][2] * (z - acpcAC.Z) * acpcRes / origRes[2]);

        return;
    }

    /**
     * Transforms point from acpc to tlrc.
     * 
     * @param pt old point
     * @param trans new point
     */
    public void acpcToTlrc(final Vector3f pt, final Vector3f trans) {
        float resx, resy, resz;
        boolean usePC = false;

        // cases 0,1 x; 2,3,4 y; 5,6 z
        if (pt.X < acpcAC.X) {
            resx = tlrcRes[0];
        } else {
            resx = tlrcRes[1];
        }

        if (pt.Y < acpcAC.Y) {
            resy = tlrcRes[2];
        } else if (pt.Y < acpcPC.Y) {
            resy = tlrcRes[3];
        } else {
            resy = tlrcRes[4];
            usePC = true;
        }

        if (pt.Z < acpcAC.Z) {
            resz = tlrcRes[5];
        } else {
            resz = tlrcRes[6];
        }

        if (usePC) {
            trans.X = tlrcPC.X + ( (pt.X - acpcPC.X) * acpcRes / resx);
            trans.Y = tlrcPC.Y + ( (pt.Y - acpcPC.Y) * acpcRes / resy);
            trans.Z = tlrcPC.Z + ( (pt.Z - acpcPC.Z) * acpcRes / resz);
        } else {
            trans.X = tlrcAC.X + ( (pt.X - acpcAC.X) * acpcRes / resx);
            trans.Y = tlrcAC.Y + ( (pt.Y - acpcAC.Y) * acpcRes / resy);
            trans.Z = tlrcAC.Z + ( (pt.Z - acpcAC.Z) * acpcRes / resz);
        }

        return;
    }

    /**
     * Transforms point from acpc to tlrc.
     * 
     * @param x
     * @param y
     * @param z
     * @param trans new point
     */
    public void acpcToTlrc(final int x, final int y, final int z, final Vector3f trans) {
        float resx, resy, resz;
        boolean usePC = false;

        // cases 0,1 x; 2,3,4 y; 5,6 z
        if (x < acpcAC.X) {
            resx = tlrcRes[0];
        } else {
            resx = tlrcRes[1];
        }

        if (y < acpcAC.Y) {
            resy = tlrcRes[2];
        } else if (y < acpcPC.Y) {
            resy = tlrcRes[3];
        } else {
            resy = tlrcRes[4];
            usePC = true;
        }

        if (z < acpcAC.Z) {
            resz = tlrcRes[5];
        } else {
            resz = tlrcRes[6];
        }

        // (P_tlrc-AC_tlrc)*Res_tlrc = (P_acpc-AC_acpc)*Res_acpc
        if (usePC) {
            trans.X = tlrcPC.X + ( (x - acpcPC.X) * acpcRes / resx);
            trans.Y = tlrcPC.Y + ( (y - acpcPC.Y) * acpcRes / resy);
            trans.Z = tlrcPC.Z + ( (z - acpcPC.Z) * acpcRes / resz);
        } else {
            trans.X = tlrcAC.X + ( (x - acpcAC.X) * acpcRes / resx);
            trans.Y = tlrcAC.Y + ( (y - acpcAC.Y) * acpcRes / resy);
            trans.Z = tlrcAC.Z + ( (z - acpcAC.Z) * acpcRes / resz);
        }

        return;
    }

    /**
     * For debug: return the transform as a 4x4 matrix.
     * 
     * @return 4*4 matrix
     */
    public double[][] displayAcpc() {
        final double[][] mat = new double[4][4];

        mat[0][0] = origOrient[0][0] / origRes[0] * acpcRes;
        mat[0][1] = origOrient[1][0] / origRes[1] * acpcRes;
        mat[0][2] = origOrient[2][0] / origRes[2] * acpcRes;
        mat[1][0] = origOrient[0][1] / origRes[0] * acpcRes;
        mat[1][1] = origOrient[1][1] / origRes[1] * acpcRes;
        mat[1][2] = origOrient[2][1] / origRes[2] * acpcRes;
        mat[2][0] = origOrient[0][2] / origRes[0] * acpcRes;
        mat[2][1] = origOrient[1][2] / origRes[1] * acpcRes;
        mat[2][2] = origOrient[2][2] / origRes[2] * acpcRes;

        mat[0][3] = origAC.X + (origOrient[0][0] * ( -acpcAC.X) / origRes[0] * acpcRes)
                + (origOrient[1][0] * ( -acpcAC.Y) / origRes[0] * acpcRes)
                + (origOrient[2][0] * ( -acpcAC.Z) / origRes[0] * acpcRes);

        mat[1][3] = origAC.Y + (origOrient[0][1] * ( -acpcAC.X) / origRes[1] * acpcRes)
                + (origOrient[1][1] * ( -acpcAC.Y) / origRes[1] * acpcRes)
                + (origOrient[2][1] * ( -acpcAC.Z) / origRes[1] * acpcRes);

        mat[2][3] = origAC.Z + (origOrient[0][2] * ( -acpcAC.X) / origRes[2] * acpcRes)
                + (origOrient[1][2] * ( -acpcAC.Y) / origRes[2] * acpcRes)
                + (origOrient[2][2] * ( -acpcAC.Z) / origRes[2] * acpcRes);

        mat[3][0] = 0;
        mat[3][1] = 0;
        mat[3][2] = 0;
        mat[3][3] = 1;

        return mat;
    }

    /**
     * For debug: return the transform inverse as a 4x4 matrix.
     * 
     * @return 4*4 matrix
     */
    public double[][] displayAcpcInverse() {
        final double[][] mat = new double[4][4];

        mat[0][0] = origOrient[0][0] * origRes[0] / acpcRes;
        mat[0][1] = origOrient[0][1] * origRes[0] / acpcRes;
        mat[0][2] = origOrient[0][2] * origRes[0] / acpcRes;
        mat[1][0] = origOrient[1][0] * origRes[1] / acpcRes;
        mat[1][1] = origOrient[1][1] * origRes[1] / acpcRes;
        mat[1][2] = origOrient[1][2] * origRes[1] / acpcRes;
        mat[2][0] = origOrient[2][0] * origRes[2] / acpcRes;
        mat[2][1] = origOrient[2][1] * origRes[2] / acpcRes;
        mat[2][2] = origOrient[2][2] * origRes[2] / acpcRes;

        mat[0][3] = acpcAC.X + (origOrient[0][0] * ( -origAC.X) * origRes[0] / acpcRes)
                + (origOrient[0][1] * ( -origAC.Y) * origRes[1] / acpcRes)
                + (origOrient[0][2] * ( -origAC.Z) * origRes[2] / acpcRes);

        mat[1][3] = acpcAC.Y + (origOrient[1][0] * ( -origAC.X) * origRes[0] / acpcRes)
                + (origOrient[1][1] * ( -origAC.Y) * origRes[1] / acpcRes)
                + (origOrient[1][2] * ( -origAC.Z) * origRes[2] / acpcRes);

        mat[2][3] = acpcAC.Z + (origOrient[2][0] * ( -origAC.X) * origRes[0] / acpcRes)
                + (origOrient[2][1] * ( -origAC.Y) * origRes[1] / acpcRes)
                + (origOrient[2][2] * ( -origAC.Z) * origRes[2] / acpcRes);

        mat[3][0] = 0;
        mat[3][1] = 0;
        mat[3][2] = 0;
        mat[3][3] = 1;

        return mat;
    }

    /**
     * Unknown use and currently not being referenced within MIPAV
     */
    public double[][] displayTlrc(final int id) {
        final double[][] mat = new double[4][4];
        int x = 0, y = 0, z = 0;

        switch (id) {

            case 0:
                x = 0;
                y = 2;
                z = 6;
                break;

            case 1:
                x = 1;
                y = 2;
                z = 6;
                break;

            case 2:
                x = 0;
                y = 3;
                z = 6;
                break;

            case 3:
                x = 1;
                y = 3;
                z = 6;
                break;

            case 4:
                x = 0;
                y = 4;
                z = 6;
                break;

            case 5:
                x = 1;
                y = 4;
                z = 6;
                break;

            case 6:
                x = 0;
                y = 2;
                z = 5;
                break;

            case 7:
                x = 1;
                y = 2;
                z = 5;
                break;

            case 8:
                x = 0;
                y = 3;
                z = 5;
                break;

            case 9:
                x = 1;
                y = 3;
                z = 5;
                break;

            case 10:
                x = 0;
                y = 4;
                z = 5;
                break;

            case 11:
                x = 1;
                y = 4;
                z = 5;
                break;
        }

        mat[0][0] = tlrcRes[x] / acpcRes;
        mat[0][1] = 0;
        mat[0][2] = 0;
        mat[1][0] = 0;
        mat[1][1] = tlrcRes[y] / acpcRes;
        mat[1][2] = 0;
        mat[2][0] = 0;
        mat[2][1] = 0;
        mat[2][2] = tlrcRes[z] / acpcRes;

        if (y == 4) {
            mat[0][3] = acpcPC.X - (tlrcPC.X * tlrcRes[x] / acpcRes);
            mat[1][3] = acpcPC.Y - (tlrcPC.Y * tlrcRes[y] / acpcRes);
            mat[2][3] = acpcPC.Z - (tlrcPC.Z * tlrcRes[z] / acpcRes);
        } else {
            mat[0][3] = acpcAC.X - (tlrcAC.X * tlrcRes[x] / acpcRes);
            mat[1][3] = acpcAC.Y - (tlrcAC.Y * tlrcRes[y] / acpcRes);
            mat[2][3] = acpcAC.Z - (tlrcAC.Z * tlrcRes[z] / acpcRes);
        }

        mat[3][0] = 0;
        mat[3][1] = 0;
        mat[3][2] = 0;
        mat[3][3] = id;

        return mat;
    }

    /**
     * Gets anterior commissure in acpc space.
     * 
     * @return acpcAC
     */
    public Vector3f getAcpcAC() {
        return acpcAC;
    }

    /**
     * Gets acpc image dimensions.
     * 
     * @return acpcDim
     */
    public int[] getAcpcDim() {
        return acpcDim;
    }

    /**
     * Gets acpc max extent.
     * 
     * @return acpcMax
     */
    public Vector3f getAcpcMax() {
        return acpcMax;
    }

    /**
     * Gets acpc min extent.
     * 
     * @return acpcMin
     */
    public Vector3f getAcpcMin() {
        return acpcMin;
    }

    /**
     * Gets posterior comissure in acpc space.
     * 
     * @return acpcPC
     */
    public Vector3f getAcpcPC() {
        return acpcPC;
    }

    /**
     * Gets resolution in acpc space.
     * 
     * @return acpcRes
     */
    public float getAcpcRes() {
        return acpcRes;
    }

    /**
     * Gets anterior comissure in original space.
     * 
     * @return origAC
     */
    public Vector3f getOrigAC() {
        return origAC;
    }

    /**
     * Gets original dims.
     * 
     * @return origDim
     */
    public int[] getOrigDim() {
        return origDim;
    }

    /**
     * Gets original origin.
     * 
     * @return origOrigin
     */
    public float[] getOrigOrigin() {
        return origOrigin;
    }

    /**
     * Determines the image orienation in the forward direction...orig to acpc or acpc to tlrc.
     * 
     * @return image orientation
     */
    public int getOrigImageOrientLabel() {
        int num;
        int label = 0;

        // Z: I2S
        num = 0;

        for (int n = 1; n < 3; n++) {

            if ( (origOrient[2][n] * origOrient[2][n]) > (origOrient[2][num] * origOrient[2][num])) {
                num = n;
            }
        }

        if (num == 0) {
            label = FileInfoBase.SAGITTAL;
        } else if (num == 1) {
            label = FileInfoBase.CORONAL;
        } else if (num == 2) {
            label = FileInfoBase.AXIAL;
        }

        return label;
    }

    /**
     * Gets the orienatations from original space.
     * 
     * @return origOrient
     */
    public float[][] getOrigOrient() {
        return origOrient;
    }

    /**
     * When going from acpc to orig or tlrc to orig, this method is used to determine what he new axis orienations. It
     * uses an inverted matrix for determination.
     * 
     * @return orientations
     */
    public int[] getOrigOrientLabelsInverse() {

        final TransMatrix matrix = new TransMatrix(3);
        for (int i = 0; i < 3; i++) {
            for (int k = 0; k < 3; k++) {
                matrix.set(i, k, origOrient[i][k]);
            }
        }

        matrix.Inverse();

        final float[][] origOrientINVERSE = new float[3][3];
        for (int i = 0; i < 3; i++) {
            for (int k = 0; k < 3; k++) {
                origOrientINVERSE[i][k] = matrix.get(i, k);
            }
        }

        int num;
        final int[] label = new int[3];

        // X: R2L
        num = 0;
        for (int n = 1; n < 3; n++) {
            if ( (origOrientINVERSE[0][n] * origOrientINVERSE[0][n]) > (origOrientINVERSE[0][num] * origOrientINVERSE[0][num])) {
                num = n;
            }
        }

        if ( (num == 0) && (origOrientINVERSE[0][num] > 0)) {
            label[0] = FileInfoBase.ORI_R2L_TYPE;
        } else if ( (num == 0) && (origOrientINVERSE[0][num] < 0)) {
            label[0] = FileInfoBase.ORI_L2R_TYPE;
        } else if ( (num == 1) && (origOrientINVERSE[0][num] > 0)) {
            label[0] = FileInfoBase.ORI_A2P_TYPE;
        } else if ( (num == 1) && (origOrientINVERSE[0][num] < 0)) {
            label[0] = FileInfoBase.ORI_P2A_TYPE;
        } else if ( (num == 2) && (origOrientINVERSE[0][num] > 0)) {
            label[0] = FileInfoBase.ORI_I2S_TYPE;
        } else if ( (num == 2) && (origOrientINVERSE[0][num] < 0)) {
            label[0] = FileInfoBase.ORI_S2I_TYPE;
        }

        // Y: A2P
        num = 0;
        for (int n = 1; n < 3; n++) {
            if ( (origOrientINVERSE[1][n] * origOrientINVERSE[1][n]) > (origOrientINVERSE[1][num] * origOrientINVERSE[1][num])) {
                num = n;
            }
        }

        if ( (num == 0) && (origOrientINVERSE[1][num] > 0)) {
            label[1] = FileInfoBase.ORI_R2L_TYPE;
        } else if ( (num == 0) && (origOrientINVERSE[1][num] < 0)) {
            label[1] = FileInfoBase.ORI_L2R_TYPE;
        } else if ( (num == 1) && (origOrientINVERSE[1][num] > 0)) {
            label[1] = FileInfoBase.ORI_A2P_TYPE;
        } else if ( (num == 1) && (origOrientINVERSE[1][num] < 0)) {
            label[1] = FileInfoBase.ORI_P2A_TYPE;
        } else if ( (num == 2) && (origOrientINVERSE[1][num] > 0)) {
            label[1] = FileInfoBase.ORI_I2S_TYPE;
        } else if ( (num == 2) && (origOrientINVERSE[1][num] < 0)) {
            label[1] = FileInfoBase.ORI_S2I_TYPE;
        }

        // Z: I2S
        num = 0;
        for (int n = 1; n < 3; n++) {
            if ( (origOrientINVERSE[2][n] * origOrientINVERSE[2][n]) > (origOrientINVERSE[2][num] * origOrientINVERSE[2][num])) {
                num = n;
            }
        }

        if ( (num == 0) && (origOrientINVERSE[2][num] > 0)) {
            label[2] = FileInfoBase.ORI_R2L_TYPE;
        } else if ( (num == 0) && (origOrientINVERSE[2][num] < 0)) {
            label[2] = FileInfoBase.ORI_L2R_TYPE;
        } else if ( (num == 1) && (origOrientINVERSE[2][num] > 0)) {
            label[2] = FileInfoBase.ORI_A2P_TYPE;
        } else if ( (num == 1) && (origOrientINVERSE[2][num] < 0)) {
            label[2] = FileInfoBase.ORI_P2A_TYPE;
        } else if ( (num == 2) && (origOrientINVERSE[2][num] > 0)) {
            label[2] = FileInfoBase.ORI_I2S_TYPE;
        } else if ( (num == 2) && (origOrientINVERSE[2][num] < 0)) {
            label[2] = FileInfoBase.ORI_S2I_TYPE;
        }

        return label;
    }

    /**
     * Gets posterior comissure in original space.
     * 
     * @return orgiPC
     */
    public Vector3f getOrigPC() {
        return origPC;
    }

    /**
     * Gets resolution in original space.
     * 
     * @return orgiRes
     * 
     */
    public float[] getOrigRes() {
        return origRes;
    }

    /**
     * Gets anterior comissure in tlrc space.
     * 
     * @return tlrcAC
     */
    public Vector3f getTlrcAC() {
        return tlrcAC;
    }

    /**
     * Gets dims in tlrc space.
     * 
     * @return tlrcDim
     */
    public int[] getTlrcDim() {
        return tlrcDim;
    }

    /**
     * Gets posterior comissure in tlrc space.
     * 
     * @return tlrcPC
     */
    public Vector3f getTlrcPC() {
        return tlrcPC;
    }

    /**
     * Gets resoltion in tlrc space.
     * 
     * @return tlrcRes
     */
    public float[] getTlrcRes() {
        return tlrcRes;
    }

    /**
     * Returns true if we have data to compute orig <-> acpc.
     * 
     * @return isAcpc
     */
    public boolean isAcpc() {
        return isAcpc;
    }

    /**
     * Sets isACPC flag.
     * 
     * @param boolean flag
     */
    public void isAcpc(final boolean flag) {
        isAcpc = flag;
    }

    /**
     * returns true if we have data to compute acpc <-> tlrc.
     * 
     * @return isTlrc
     */
    public boolean isTlrc() {
        return isTlrc;
    }

    /**
     * Sets isTLRC flag.
     * 
     * @param boolean flag
     */
    public void isTlrc(final boolean flag) {
        isTlrc = flag;
    }

    /**
     * Transforms point from orig to acpc.
     * 
     * @param pt old point
     * @param trans new point
     */
    public void origToAcpc(final Vector3f pt, final Vector3f trans) {

        trans.X = acpcAC.X + (origOrient[0][0] * (pt.X - origAC.X) * origRes[0] / acpcRes)
                + (origOrient[0][1] * (pt.Y - origAC.Y) * origRes[1] / acpcRes)
                + (origOrient[0][2] * (pt.Z - origAC.Z) * origRes[2] / acpcRes);

        trans.Y = acpcAC.Y + (origOrient[1][0] * (pt.X - origAC.X) * origRes[0] / acpcRes)
                + (origOrient[1][1] * (pt.Y - origAC.Y) * origRes[1] / acpcRes)
                + (origOrient[1][2] * (pt.Z - origAC.Z) * origRes[2] / acpcRes);

        trans.Z = acpcAC.Z + (origOrient[2][0] * (pt.X - origAC.X) * origRes[0] / acpcRes)
                + (origOrient[2][1] * (pt.Y - origAC.Y) * origRes[1] / acpcRes)
                + (origOrient[2][2] * (pt.Z - origAC.Z) * origRes[2] / acpcRes);

        return;
    }

    /**
     * Transforms point from orig to acpc.
     * 
     * @param x
     * @param y
     * @param z
     * @param trans new point
     */
    public void origToAcpc(final int x, final int y, final int z, final Vector3f trans) {

        trans.X = acpcAC.X + (origOrient[0][0] * (x - origAC.X) * origRes[0] / acpcRes)
                + (origOrient[0][1] * (y - origAC.Y) * origRes[1] / acpcRes)
                + (origOrient[0][2] * (z - origAC.Z) * origRes[2] / acpcRes);

        trans.Y = acpcAC.Y + (origOrient[1][0] * (x - origAC.X) * origRes[0] / acpcRes)
                + (origOrient[1][1] * (y - origAC.Y) * origRes[1] / acpcRes)
                + (origOrient[1][2] * (z - origAC.Z) * origRes[2] / acpcRes);

        trans.Z = acpcAC.Z + (origOrient[2][0] * (x - origAC.X) * origRes[0] / acpcRes)
                + (origOrient[2][1] * (y - origAC.Y) * origRes[1] / acpcRes)
                + (origOrient[2][2] * (z - origAC.Z) * origRes[2] / acpcRes);

        return;
    }

    /**
     * Transforms point from orig to tlrc.
     * 
     * @param pt old point
     * @param trans new point
     */
    public void origToTlrc(final Vector3f pt, final Vector3f trans) {
        final Vector3f tmp = new Vector3f();
        origToAcpc(pt, tmp);
        acpcToTlrc(tmp, trans);
    }

    /**
     * Transforms point from orig to tlrc.
     * 
     * @param x
     * @param y
     * @param z
     * @param trans new point
     */
    public void origToTlrc(final int x, final int y, final int z, final Vector3f trans) {
        final Vector3f tmp = new Vector3f();
        origToAcpc(x, y, z, tmp);
        acpcToTlrc(tmp, trans);
    }

    /**
     * Reads transform info file
     * 
     * @param filename name of transform info file
     */
    public void readFromFile(final String filename) {

        try {
            final File f = new File(filename);
            final FileReader fr = new FileReader(f);
            final BufferedReader br = new BufferedReader(fr);
            String line = br.readLine();
            float[] params;
            int[] dims;

            // Exact corresponding template
            if ( !line.equals("Talairach Transform Info File (do not edit)")) {
                System.out.println("not a proper Talairach Transform file");
                br.close();
                fr.close();

                return;
            }

            line = br.readLine();

            // old header
            if (line.equals("- Original Image -")) {
                isAcpc = true;
                isTlrc = true;
            } else {

                if (line.equals("ACPC aligned")) {
                    isAcpc = true;
                } else {
                    isAcpc = false;
                }

                line = br.readLine();

                if (line.equals("Talairach aligned")) {
                    isTlrc = true;
                } else {
                    isTlrc = false;
                }

                line = br.readLine();
            }

            if (isAcpc) {

                // Original Image
                // AC
                params = parseFloatParameters(br.readLine(), 3);
                setOrigAC(new Vector3f(params[0], params[1], params[2]));

                // PC
                params = parseFloatParameters(br.readLine(), 3);
                setOrigPC(new Vector3f(params[0], params[1], params[2]));

                // Res
                params = parseFloatParameters(br.readLine(), 3);
                setOrigRes(params);

                // Dim
                dims = parseIntParameters(br.readLine(), 3);
                setOrigDim(dims);

                // Origin
                boolean success = true;
                line = br.readLine();
                try {
                    params = parseFloatParameters(line, 3);
                } catch (final NumberFormatException e) {
                    success = false;
                }
                if (success) {
                    setOrigOrigin(params);

                    // Orient
                    params = parseFloatParameters(br.readLine(), 9);
                } else {
                    params = parseFloatParameters(line, 9);
                }

                final float[][] R = new float[3][3];

                for (int i = 0; i < 3; i++) {

                    for (int j = 0; j < 3; j++) {
                        R[i][j] = params[ (i * 3) + j];
                    }
                }

                setOrigOrient(R);

                // ACPC image
                line = br.readLine();

                // AC
                params = parseFloatParameters(br.readLine(), 3);

                // not used
                // PC
                params = parseFloatParameters(br.readLine(), 3);
                setAcpcPC(new Vector3f(params[0], params[1], params[2]));

                // Res
                params = parseFloatParameters(br.readLine(), 1);
                setAcpcRes(params[0]);

                // Dim
                dims = parseIntParameters(br.readLine(), 3);
                // not used

            }

            if (isTlrc) {

                // Min
                params = parseFloatParameters(br.readLine(), 3);
                setAcpcMin(new Vector3f(params[0], params[1], params[2]));

                // Max
                params = parseFloatParameters(br.readLine(), 3);
                setAcpcMax(new Vector3f(params[0], params[1], params[2]));

                // TLRC image
                line = br.readLine();

                // AC
                params = parseFloatParameters(br.readLine(), 3);

                // not used
                // PC
                params = parseFloatParameters(br.readLine(), 3);

                // not used
                // Res
                params = parseFloatParameters(br.readLine(), 7);
                setTlrcRes(params);

                // Dim
                dims = parseIntParameters(br.readLine(), 3);
                // not used
            }

            br.close();
            fr.close();
        } catch (final FileNotFoundException e) {
            System.out.println(e.getMessage());
        } catch (final IOException e) {
            System.out.println(e.getMessage());
        }

        return;
    }

    /**
     * Sets acpc max extent.
     * 
     * @param Vector3f pt
     */
    public void setAcpcMax(final Vector3f pt) {
        acpcMax = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * Sets acpc min extent.
     * 
     * @param Vector3f pt
     */
    public void setAcpcMin(final Vector3f pt) {
        acpcMin = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * Sets acpc posterior comissure.
     * 
     * @param Vector3f pt
     */
    public void setAcpcPC(final Vector3f pt) {
        acpcPC = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * Sets acps resolution.
     * 
     * @param float res
     */
    public void setAcpcRes(final float res) {
        acpcRes = res;
        final boolean revertToOldBrokenDim = false;
        if (useIncorrectAcpcXDim) {
            acpcDim[0] = Math.round( (2 * (ACPC_LATERAL + 1)) / acpcRes);
        } else {
            acpcDim[0] = Math.round( (2 * ACPC_LATERAL + 1) / acpcRes);
        }
        acpcDim[1] = Math.round( (ACPC_ANTERIOR + ACPC_POSTERIOR + 1) / acpcRes);
        acpcDim[2] = Math.round( (ACPC_INFERIOR + ACPC_SUPERIOR + 1) / acpcRes);

        acpcAC = new Vector3f(ACPC_LATERAL / acpcRes, ACPC_ANTERIOR / acpcRes, ACPC_INFERIOR / acpcRes);

        // change the overall resolution for TLRC as well
        tlrcAC = new Vector3f(80.0f / acpcRes, 80.0f / acpcRes, 65.0f / acpcRes);
        tlrcPC = new Vector3f(80.0f / acpcRes, 103.0f / acpcRes, 65.0f / acpcRes);
        tlrcDim[0] = (int) (161.0f / acpcRes);
        tlrcDim[1] = (int) (191.0f / acpcRes);
        tlrcDim[2] = (int) (151.0f / acpcRes);
    }

    /**
     * Sets the anterior comissure in original space.
     * 
     * @param Vector3f point
     */
    public void setOrigAC(final Vector3f pt) {
        origAC = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * Sets dims in original space.
     * 
     * @param int[] dim
     */
    public void setOrigDim(final int[] dim) {
        origDim = dim.clone();
    }

    /**
     * Sets origin in original space.
     * 
     * @param origin
     */
    public void setOrigOrigin(final float[] origin) {
        origOrigin = origin.clone();
    }

    /**
     * Sets orienatationsin origianl space.
     * 
     * @param float[][] orient
     */
    public void setOrigOrient(final float[][] orient) {
        origOrient = new float[3][3];

        for (int i = 0; i < 3; i++) {

            for (int j = 0; j < 3; j++) {
                origOrient[i][j] = orient[i][j];
            }
        }
    }

    /**
     * Sets posterior comissure in original space.
     * 
     * @param Vector3f point
     */
    public void setOrigPC(final Vector3f pt) {
        origPC = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * Sets resolution in original space.
     * 
     * @param float[] res
     */
    public void setOrigRes(final float[] res) {
        origRes = res.clone();
    }

    /**
     * Sets resolution in tlrc space
     * 
     * @param float[] res
     */
    public void setTlrcRes(final float[] res) {
        tlrcRes = res.clone();
    }

    /**
     * Transforms point from tlrc to acpc...matrix is implicitly inverted in code.
     * 
     * @param pt old point
     * @param trans new point
     */
    public void tlrcToAcpc(final Vector3f pt, final Vector3f trans) {
        float resx, resy, resz;
        boolean usePC = false;

        // cases 0,1 x; 2,3,4 y; 5,6 z
        if (pt.X < tlrcAC.X) {
            resx = tlrcRes[0];
        } else {
            resx = tlrcRes[1];
        }

        if (pt.Y < tlrcAC.Y) {
            resy = tlrcRes[2];
        } else if (pt.Y < tlrcPC.Y) {
            resy = tlrcRes[3];
        } else {
            resy = tlrcRes[4];
            usePC = true;
        }

        if (pt.Z < tlrcAC.Z) {
            resz = tlrcRes[5];
        } else {
            resz = tlrcRes[6];
        }

        if (usePC) {
            trans.X = acpcPC.X + ( (pt.X - tlrcPC.X) / acpcRes * resx);
            trans.Y = acpcPC.Y + ( (pt.Y - tlrcPC.Y) / acpcRes * resy);
            trans.Z = acpcPC.Z + ( (pt.Z - tlrcPC.Z) / acpcRes * resz);
        } else {
            trans.X = acpcAC.X + ( (pt.X - tlrcAC.X) / acpcRes * resx);
            trans.Y = acpcAC.Y + ( (pt.Y - tlrcAC.Y) / acpcRes * resy);
            trans.Z = acpcAC.Z + ( (pt.Z - tlrcAC.Z) / acpcRes * resz);
        }

        return;
    }

    /**
     * Transforms point from tlrc to acpc...matrix is implicitly inverted in code.
     * 
     * @param x
     * @param y
     * @param z
     * @param trans new point
     */
    public void tlrcToAcpc(final int x, final int y, final int z, final Vector3f trans) {
        float resx, resy, resz;
        boolean usePC = false;

        // cases 0,1 x; 2,3,4 y; 5,6 z
        if (x < tlrcAC.X) {
            resx = tlrcRes[0];
        } else {
            resx = tlrcRes[1];
        }

        if (y < tlrcAC.Y) {
            resy = tlrcRes[2];
        } else if (y < tlrcPC.Y) {
            resy = tlrcRes[3];
        } else {
            resy = tlrcRes[4];
            usePC = true;
        }

        if (z < tlrcAC.Z) {
            resz = tlrcRes[5];
        } else {
            resz = tlrcRes[6];
        }

        // (P_tlrc-AC_tlrc)*Res_tlrc = (P_acpc-AC_acpc)*Res_acpc
        if (usePC) {
            trans.X = acpcPC.X + ( (x - tlrcPC.X) / acpcRes * resx);
            trans.Y = acpcPC.Y + ( (y - tlrcPC.Y) / acpcRes * resy);
            trans.Z = acpcPC.Z + ( (z - tlrcPC.Z) / acpcRes * resz);
        } else {
            trans.X = acpcAC.X + ( (x - tlrcAC.X) / acpcRes * resx);
            trans.Y = acpcAC.Y + ( (y - tlrcAC.Y) / acpcRes * resy);
            trans.Z = acpcAC.Z + ( (z - tlrcAC.Z) / acpcRes * resz);
        }

        return;
    }

    /**
     * Transforms point from tlrc to orig...matrix is implicitly inverted in code.
     * 
     * @param pt old point
     * @param trans new point
     */
    public void tlrcToOrig(final Vector3f pt, final Vector3f trans) {
        final Vector3f tmp = new Vector3f();
        tlrcToAcpc(pt, tmp);
        acpcToOrig(tmp, trans);
    }

    /**
     * Transforms point from tlrc to orig...matrix is implicitly inverted in code.
     * 
     * @param x
     * @param y
     * @param z
     * @param trans new point
     */
    public void tlrcToOrig(final int x, final int y, final int z, final Vector3f trans) {
        final Vector3f tmp = new Vector3f();
        tlrcToAcpc(x, y, z, tmp);
        acpcToOrig(tmp, trans);
    }

    /**
     * writes transform information to file
     * 
     * @param filename name of transform info file
     */
    public void writeToFile(final String filename) {

        try {

            // open the file for writing
            final File f = new File(filename);
            final FileWriter fw = new FileWriter(f);
            final PrintWriter pw = new PrintWriter(fw);

            // write the parameters
            pw.println("Talairach Transform Info File (do not edit)");

            if (isAcpc) {
                pw.println("ACPC aligned");
            } else {
                pw.println("not ACPC aligned");
            }

            if (isTlrc) {
                pw.println("Talairach aligned");
            } else {
                pw.println("not Talairach aligned");
            }

            if (isAcpc) {
                pw.println("- Original Image -");
                pw.println("AC: (" + origAC.X + ", " + origAC.Y + ", " + origAC.Z + ")");
                pw.println("PC: (" + origPC.X + ", " + origPC.Y + ", " + origPC.Z + ")");
                pw.println("Res: (" + origRes[0] + ", " + origRes[1] + ", " + origRes[2] + ")");
                pw.println("Dim: (" + origDim[0] + ", " + origDim[1] + ", " + origDim[2] + ")");
                pw.println("Origin: (" + origOrigin[0] + ", " + origOrigin[1] + ", " + origOrigin[2] + ")");
                pw.println("Orient: (" + origOrient[0][0] + ", " + origOrient[0][1] + ", " + origOrient[0][2] + ", "
                        + origOrient[1][0] + ", " + origOrient[1][1] + ", " + origOrient[1][2] + ", "
                        + origOrient[2][0] + ", " + origOrient[2][1] + ", " + origOrient[2][2] + ")");
                pw.println("- AC-PC Image -");
                pw.println("AC: (" + acpcAC.X + ", " + acpcAC.Y + ", " + acpcAC.Z + ")");
                pw.println("PC: (" + acpcPC.X + ", " + acpcPC.Y + ", " + acpcPC.Z + ")");
                pw.println("Res: (" + acpcRes + ")");
                pw.println("Dim: (" + acpcDim[0] + ", " + acpcDim[1] + ", " + acpcDim[2] + ")");
            }

            if (isTlrc) {
                pw.println("Min: (" + acpcMin.X + ", " + acpcMin.Y + ", " + acpcMin.Z + ")");
                pw.println("Max: (" + acpcMax.X + ", " + acpcMax.Y + ", " + acpcMax.Z + ")");

                pw.println("- Talairach Image -");
                pw.println("AC: (" + tlrcAC.X + ", " + tlrcAC.Y + ", " + tlrcAC.Z + ")");
                pw.println("PC: (" + tlrcPC.X + ", " + tlrcPC.Y + ", " + tlrcPC.Z + ")");
                pw.println("Res: (" + tlrcRes[0] + ", " + tlrcRes[1] + ", " + tlrcRes[2] + ", " + tlrcRes[3] + ", "
                        + tlrcRes[4] + ", " + tlrcRes[5] + ", " + tlrcRes[6] + ")");
                pw.println("Dim: (" + tlrcDim[0] + ", " + tlrcDim[1] + ", " + tlrcDim[2] + ")");
            }

            // close the file
            fw.close();
        } catch (final FileNotFoundException e) {
            System.out.println(e.getMessage());
        } catch (final IOException e) {
            System.out.println(e.getMessage());
        }

    }

    /**
     * Parsing utility.
     * 
     * @param line
     * @param nb
     * 
     * @return
     */
    private float[] parseFloatParameters(final String line, final int nb) {
        int next = 0;
        final float[] x = new float[nb];

        // coordinates
        int prev = 0;
        next = line.indexOf("(", prev);
        prev = next + 1;

        if (nb > 1) {
            next = line.indexOf(", ", prev);
        } else {
            next = line.indexOf(")", prev);
        }

        System.out.print("|" + line.substring(prev, next));
        x[0] = Float.valueOf(line.substring(prev, next)).floatValue();

        for (int n = 1; n < (nb - 1); n++) {
            prev = next + 2;
            next = line.indexOf(", ", prev);
            System.out.print("|" + line.substring(prev, next));
            x[n] = Float.valueOf(line.substring(prev, next)).floatValue();
        }

        if (nb > 1) {
            prev = next + 2;
            next = line.indexOf(")", prev);
            System.out.print("|" + line.substring(prev, next));
            x[nb - 1] = Float.valueOf(line.substring(prev, next)).floatValue();
        }

        return x;
    }

    /**
     * Parsing Parameters Utility
     * 
     * @param line
     * @param nb
     * 
     * @return
     */
    private int[] parseIntParameters(final String line, final int nb) {
        int next = 0;
        final int[] x = new int[nb];

        // coordinates
        int prev = 0;
        next = line.indexOf("(", prev);
        prev = next + 1;
        next = line.indexOf(", ", prev);
        System.out.print("|" + line.substring(prev, next));
        x[0] = Integer.valueOf(line.substring(prev, next)).intValue();

        for (int n = 1; n < (nb - 1); n++) {
            prev = next + 2;
            next = line.indexOf(", ", prev);
            System.out.print("|" + line.substring(prev, next));
            x[n] = Integer.valueOf(line.substring(prev, next)).intValue();
        }

        prev = next + 2;
        next = line.indexOf(")", prev);
        System.out.print("|" + line.substring(prev, next));
        x[nb - 1] = Integer.valueOf(line.substring(prev, next)).intValue();

        return x;
    }

    /**
     * Sets whether the TalairachTransform acpcDim x value should be set up (incorrectly) like it was in previous
     * versions (for Pierpaoli's group).
     * 
     * @param flag True if the old, incorrect x acpcDim value should be used (which overcounts by one).
     */
    public void setUseIncorrectAcpcXDim(final boolean flag) {
        useIncorrectAcpcXDim = flag;
        if (useIncorrectAcpcXDim) {
            acpcDim = new int[] {192, 236, 171};
        } else {
            acpcDim = new int[] {191, 236, 171};
        }
    }
}
