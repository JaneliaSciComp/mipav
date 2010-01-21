package gov.nih.mipav.model.structures;


import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.file.*;

import java.io.*;


/**
 * This is the structure to store information needed to compute ACPC and Talairach normalization.
 *
 * <p>If you have any questions, please drop me a line. ===== Pilou Bazin MEDIC, JHU pbazin1@jhmi.edu</p>
 *
 * @version  July 26, 2004
 * @author   Pierre-Louis Bazin
 * @see      AlgorithmBase
 * @see      TextRemoval
 * @see      PlugInDialogTextRemoval
 */
public class TalairachTransformInfo implements Serializable {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5791995578653926105L;

    /** acpc constants. */
    private static final float ACPC_LATERAL = 95.0f;

    /** DOCUMENT ME! */
    private static final float ACPC_ANTERIOR = 95.0f;

    /** DOCUMENT ME! */
    private static final float ACPC_POSTERIOR = 140.0f;

    /** DOCUMENT ME! */
    private static final float ACPC_INFERIOR = 70.0f;

    /** DOCUMENT ME! */
    private static final float ACPC_SUPERIOR = 100.0f;

    /** DOCUMENT ME! */
    public static final int TLRC_AC_TO_PC = 23;

    /** DOCUMENT ME! */
    public static final int TLRC_FRONT_TO_AC = 70;

    /** DOCUMENT ME! */
    public static final int TLRC_PC_TO_BACK = 79;

    /** DOCUMENT ME! */
    public static final int TLRC_BOT_TO_AC = 42;

    /** DOCUMENT ME! */
    public static final int TLRC_AC_TO_TOP = 74;

    /** DOCUMENT ME! */
    public static final int TLRC_AC_TO_LAT = 68;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Anterior Comissure in acpc space. */
    private Vector3f acpcAC = new Vector3f(95.0f, 95.0f, 70.0f);

    /** image dimensions. */
    private int[] acpcDim = { 192, 236, 171 };

    /** DOCUMENT ME! */
    private Vector3f acpcMax;

    /** ACPC min and max extents of the brain. */
    private Vector3f acpcMin;

    /** Posterior Comissure in acpc space. */
    private Vector3f acpcPC = new Vector3f();

    /** voxel resolution (cubic). */
    private float acpcRes = 1.0f;

    // flags for computations
    /** true if we have the data to compute orig <-> acpc. */
    private boolean isAcpc = false;

    /** true if we ahve the data to compute acpc <-> tlrc. */
    private boolean isTlrc = false;

    // orig: original image space
    /** Anterior Comissure in original space. */
    private Vector3f origAC;

    /** Original image dimensions. */
    private int[] origDim;

    /** ACPC orientation in original image. */
    private float[][] origOrient;
    
    /** Original image origin */
    private float[] origOrigin;

    /* acpc: ACPC aligned space */

    /** Posterior Comissure in original space. */
    private Vector3f origPC;

    // private Vector3f  origMin;    // Original min and max extents of the brain
    // private Vector3f  origMax;

    /** Original image voxel resolutions. */
    private float[] origRes;

    // tlrc: Talairach space
    /** Anterior Comissure in Talairach space. */
    private Vector3f tlrcAC = new Vector3f(80.0f, 80.0f, 65.0f);

    /** image dimensions. */
    private int[] tlrcDim = { 161, 191, 151 };

    /** Posterior Comissure in Talairach space. */
    private Vector3f tlrcPC = new Vector3f(80.0f, 103.0f, 65.0f);

    // private Vector3f  tlrcMin = new Vector3f(12.0f,10.0f,23.0f);  // ACPC min and max extents of the brain
    // private Vector3f  tlrcMax = new Vector3f(148.0f,172.0f,139.0f);

    /** voxel resolutions for the sub-boxes. */
    private float[] tlrcRes;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new TalairachTransformInfo object.
     */
    public TalairachTransformInfo() { }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for cleanup. Calls the <code>finalize</code> method for existing elements, closes any open
     * files and sets other elements to <code>null</code>.
     */
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
        } catch (Throwable er) { }
    }
    
    /**
     * DOCUMENT ME!
     *
     * @param  pt     DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void acpcToOrig(Vector3f pt, Vector3f trans) {

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        trans.X = origAC.X + (origOrient[0][0] * (pt.X - acpcAC.X) * acpcRes / origRes[0]) +
                  (origOrient[1][0] * (pt.Y - acpcAC.Y) * acpcRes / origRes[0]) +
                  (origOrient[2][0] * (pt.Z - acpcAC.Z) * acpcRes / origRes[0]);

        trans.Y = origAC.Y + (origOrient[0][1] * (pt.X - acpcAC.X) * acpcRes / origRes[1]) +
                  (origOrient[1][1] * (pt.Y - acpcAC.Y) * acpcRes / origRes[1]) +
                  (origOrient[2][1] * (pt.Z - acpcAC.Z) * acpcRes / origRes[1]);

        trans.Z = origAC.Z + (origOrient[0][2] * (pt.X - acpcAC.X) * acpcRes / origRes[2]) +
                  (origOrient[1][2] * (pt.Y - acpcAC.Y) * acpcRes / origRes[2]) +
                  (origOrient[2][2] * (pt.Z - acpcAC.Z) * acpcRes / origRes[2]);

        /*
         *       trans.X = origAC.X + origOrient[0][0]*(pt.X-acpcAC.X)*acpcRes/origRes[0]                +
         * origOrient[1][0]*(pt.Y-acpcAC.Y)*acpcRes/origRes[1]                +
         * origOrient[2][0]*(pt.Z-acpcAC.Z)*acpcRes/origRes[2];
         *
         *    trans.Y = origAC.Y + origOrient[0][1]*(pt.X-acpcAC.X)*acpcRes/origRes[0]                +
         * origOrient[1][1]*(pt.Y-acpcAC.Y)*acpcRes/origRes[1]                +
         * origOrient[2][1]*(pt.Z-acpcAC.Z)*acpcRes/origRes[2];
         *
         *    trans.Z = origAC.Z + origOrient[0][2]*(pt.X-acpcAC.X)*acpcRes/origRes[0]                +
         * origOrient[1][2]*(pt.Y-acpcAC.Y)*acpcRes/origRes[1]                +
         * origOrient[2][2]*(pt.Z-acpcAC.Z)*acpcRes/origRes[2];
         */
        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x      DOCUMENT ME!
     * @param  y      DOCUMENT ME!
     * @param  z      DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void acpcToOrig(int x, int y, int z, Vector3f trans) {

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        trans.X = origAC.X + (origOrient[0][0] * (x - acpcAC.X) * acpcRes / origRes[0]) +
                  (origOrient[1][0] * (y - acpcAC.Y) * acpcRes / origRes[0]) +
                  (origOrient[2][0] * (z - acpcAC.Z) * acpcRes / origRes[0]);

        trans.Y = origAC.Y + (origOrient[0][1] * (x - acpcAC.X) * acpcRes / origRes[1]) +
                  (origOrient[1][1] * (y - acpcAC.Y) * acpcRes / origRes[1]) +
                  (origOrient[2][1] * (z - acpcAC.Z) * acpcRes / origRes[1]);

        trans.Z = origAC.Z + (origOrient[0][2] * (x - acpcAC.X) * acpcRes / origRes[2]) +
                  (origOrient[1][2] * (y - acpcAC.Y) * acpcRes / origRes[2]) +
                  (origOrient[2][2] * (z - acpcAC.Z) * acpcRes / origRes[2]);

        /*
         *       trans.X = origAC.X +  origOrient[0][0]*(x-acpcAC.X)*acpcRes/origRes[0]                +
         * origOrient[1][0]*(y-acpcAC.Y)*acpcRes/origRes[1]                +
         * origOrient[2][0]*(z-acpcAC.Z)*acpcRes/origRes[2];
         *
         *    trans.Y = origAC.Y +  origOrient[0][1]*(x-acpcAC.X)*acpcRes/origRes[0]                +
         * origOrient[1][1]*(y-acpcAC.Y)*acpcRes/origRes[1]                +
         * origOrient[2][1]*(z-acpcAC.Z)*acpcRes/origRes[2];
         *
         *    trans.Z = origAC.Z +  origOrient[0][2]*(x-acpcAC.X)*acpcRes/origRes[0]                +
         * origOrient[1][2]*(y-acpcAC.Y)*acpcRes/origRes[1]                +
         * origOrient[2][2]*(z-acpcAC.Z)*acpcRes/origRes[2];
         */
        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pt     DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void acpcToTlrc(Vector3f pt, Vector3f trans) {
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

        // (P_tlrc-AC_tlrc)*Res_tlrc = (P_acpc-AC_acpc)*Res_acpc
        if (usePC) {
            trans.X = tlrcPC.X + ((pt.X - acpcPC.X) * acpcRes / resx);
            trans.Y = tlrcPC.Y + ((pt.Y - acpcPC.Y) * acpcRes / resy);
            trans.Z = tlrcPC.Z + ((pt.Z - acpcPC.Z) * acpcRes / resz);
        } else {
            trans.X = tlrcAC.X + ((pt.X - acpcAC.X) * acpcRes / resx);
            trans.Y = tlrcAC.Y + ((pt.Y - acpcAC.Y) * acpcRes / resy);
            trans.Z = tlrcAC.Z + ((pt.Z - acpcAC.Z) * acpcRes / resz);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x      DOCUMENT ME!
     * @param  y      DOCUMENT ME!
     * @param  z      DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void acpcToTlrc(int x, int y, int z, Vector3f trans) {
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
            trans.X = tlrcPC.X + ((x - acpcPC.X) * acpcRes / resx);
            trans.Y = tlrcPC.Y + ((y - acpcPC.Y) * acpcRes / resy);
            trans.Z = tlrcPC.Z + ((z - acpcPC.Z) * acpcRes / resz);
        } else {
            trans.X = tlrcAC.X + ((x - acpcAC.X) * acpcRes / resx);
            trans.Y = tlrcAC.Y + ((y - acpcAC.Y) * acpcRes / resy);
            trans.Z = tlrcAC.Z + ((z - acpcAC.Z) * acpcRes / resz);
        }

        return;
    }

    /**
     * for debug: return the transform as a 4x4 matrix.*
     *
     * @return  DOCUMENT ME!
     */
    public double[][] displayAcpc() {
        double[][] mat = new double[4][4];
        Vector3f trans = new Vector3f();

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        mat[0][0] = origOrient[0][0] / origRes[0] * acpcRes;
        mat[0][1] = origOrient[1][0] / origRes[1] * acpcRes;
        mat[0][2] = origOrient[2][0] / origRes[2] * acpcRes;
        mat[1][0] = origOrient[0][1] / origRes[0] * acpcRes;
        mat[1][1] = origOrient[1][1] / origRes[1] * acpcRes;
        mat[1][2] = origOrient[2][1] / origRes[2] * acpcRes;
        mat[2][0] = origOrient[0][2] / origRes[0] * acpcRes;
        mat[2][1] = origOrient[1][2] / origRes[1] * acpcRes;
        mat[2][2] = origOrient[2][2] / origRes[2] * acpcRes;

        mat[0][3] = origAC.X + (origOrient[0][0] * (-acpcAC.X) / origRes[0] * acpcRes) +
                    (origOrient[1][0] * (-acpcAC.Y) / origRes[0] * acpcRes) +
                    (origOrient[2][0] * (-acpcAC.Z) / origRes[0] * acpcRes);

        mat[1][3] = origAC.Y + (origOrient[0][1] * (-acpcAC.X) / origRes[1] * acpcRes) +
                    (origOrient[1][1] * (-acpcAC.Y) / origRes[1] * acpcRes) +
                    (origOrient[2][1] * (-acpcAC.Z) / origRes[1] * acpcRes);

        mat[2][3] = origAC.Z + (origOrient[0][2] * (-acpcAC.X) / origRes[2] * acpcRes) +
                    (origOrient[1][2] * (-acpcAC.Y) / origRes[2] * acpcRes) +
                    (origOrient[2][2] * (-acpcAC.Z) / origRes[2] * acpcRes);

        mat[3][0] = 0;
        mat[3][1] = 0;
        mat[3][2] = 0;
        mat[3][3] = 1;

        return mat;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double[][] displayAcpcInverse() {
        double[][] mat = new double[4][4];
        Vector3f trans = new Vector3f();

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        mat[0][0] = origOrient[0][0] * origRes[0] / acpcRes;
        mat[0][1] = origOrient[0][1] * origRes[0] / acpcRes;
        mat[0][2] = origOrient[0][2] * origRes[0] / acpcRes;
        mat[1][0] = origOrient[1][0] * origRes[1] / acpcRes;
        mat[1][1] = origOrient[1][1] * origRes[1] / acpcRes;
        mat[1][2] = origOrient[1][2] * origRes[1] / acpcRes;
        mat[2][0] = origOrient[2][0] * origRes[2] / acpcRes;
        mat[2][1] = origOrient[2][1] * origRes[2] / acpcRes;
        mat[2][2] = origOrient[2][2] * origRes[2] / acpcRes;

        mat[0][3] = acpcAC.X + (origOrient[0][0] * (-origAC.X) * origRes[0] / acpcRes) +
                    (origOrient[0][1] * (-origAC.Y) * origRes[1] / acpcRes) +
                    (origOrient[0][2] * (-origAC.Z) * origRes[2] / acpcRes);

        mat[1][3] = acpcAC.Y + (origOrient[1][0] * (-origAC.X) * origRes[0] / acpcRes) +
                    (origOrient[1][1] * (-origAC.Y) * origRes[1] / acpcRes) +
                    (origOrient[1][2] * (-origAC.Z) * origRes[2] / acpcRes);

        mat[2][3] = acpcAC.Z + (origOrient[2][0] * (-origAC.X) * origRes[0] / acpcRes) +
                    (origOrient[2][1] * (-origAC.Y) * origRes[1] / acpcRes) +
                    (origOrient[2][2] * (-origAC.Z) * origRes[2] / acpcRes);

        mat[3][0] = 0;
        mat[3][1] = 0;
        mat[3][2] = 0;
        mat[3][3] = 1;

        return mat;
    }

    /**
     * DOCUMENT ME!
     *
     * @param   id  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public double[][] displayTlrc(int id) {
        double[][] mat = new double[4][4];
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

        // (P_tlrc-AC_tlrc)*Res_tlrc = (P_acpc-AC_acpc)*Res_acpc
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
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f getAcpcAC() {
        return acpcAC;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getAcpcDim() {
        return acpcDim;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f getAcpcMax() {
        return acpcMax;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f getAcpcMin() {
        return acpcMin;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f getAcpcPC() {
        return acpcPC;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float getAcpcRes() {
        return acpcRes;
    }

    /**
     * accessors to get all quantities.
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f getOrigAC() {
        return origAC;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getOrigDim() {
        return origDim;
    }
    
    public float[] getOrigOrigin() {
        return origOrigin;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int getOrigImageOrientLabel() {
        int num;
        int label = 0;

        // Z: I2S
        num = 0;

        for (int n = 1; n < 3; n++) {

            if ((origOrient[2][n] * origOrient[2][n]) > (origOrient[2][num] * origOrient[2][num])) {
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
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[][] getOrigOrient() {
        return origOrient;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getOrigOrientLabels() {
        int num;
        int[] label = new int[3];

        // X: R2L
        num = 0;

        for (int n = 1; n < 3; n++) {

            if ((origOrient[0][n] * origOrient[0][n]) > (origOrient[0][num] * origOrient[0][num])) {
                num = n;
            }
        }

        if ((num == 0) && (origOrient[0][num] > 0)) {
            label[0] = FileInfoBase.ORI_R2L_TYPE;
        } else if ((num == 0) && (origOrient[0][num] < 0)) {
            label[0] = FileInfoBase.ORI_L2R_TYPE;
        } else if ((num == 1) && (origOrient[0][num] > 0)) {
            label[0] = FileInfoBase.ORI_A2P_TYPE;
        } else if ((num == 1) && (origOrient[0][num] < 0)) {
            label[0] = FileInfoBase.ORI_P2A_TYPE;
        } else if ((num == 2) && (origOrient[0][num] > 0)) {
            label[0] = FileInfoBase.ORI_I2S_TYPE;
        } else if ((num == 2) && (origOrient[0][num] < 0)) {
            label[0] = FileInfoBase.ORI_S2I_TYPE;
        }

        // Y: A2P
        num = 0;

        for (int n = 1; n < 3; n++) {

            if ((origOrient[1][n] * origOrient[1][n]) > (origOrient[1][num] * origOrient[1][num])) {
                num = n;
            }
        }

        if ((num == 0) && (origOrient[1][num] > 0)) {
            label[1] = FileInfoBase.ORI_R2L_TYPE;
        } else if ((num == 0) && (origOrient[1][num] < 0)) {
            label[1] = FileInfoBase.ORI_L2R_TYPE;
        } else if ((num == 1) && (origOrient[1][num] > 0)) {
            label[1] = FileInfoBase.ORI_A2P_TYPE;
        } else if ((num == 1) && (origOrient[1][num] < 0)) {
            label[1] = FileInfoBase.ORI_P2A_TYPE;
        } else if ((num == 2) && (origOrient[1][num] > 0)) {
            label[1] = FileInfoBase.ORI_I2S_TYPE;
        } else if ((num == 2) && (origOrient[1][num] < 0)) {
            label[1] = FileInfoBase.ORI_S2I_TYPE;
        }

        // Z: I2S
        num = 0;

        for (int n = 1; n < 3; n++) {

            if ((origOrient[2][n] * origOrient[2][n]) > (origOrient[2][num] * origOrient[2][num])) {
                num = n;
            }
        }

        if ((num == 0) && (origOrient[2][num] > 0)) {
            label[2] = FileInfoBase.ORI_R2L_TYPE;
        } else if ((num == 0) && (origOrient[2][num] < 0)) {
            label[2] = FileInfoBase.ORI_L2R_TYPE;
        } else if ((num == 1) && (origOrient[2][num] > 0)) {
            label[2] = FileInfoBase.ORI_A2P_TYPE;
        } else if ((num == 1) && (origOrient[2][num] < 0)) {
            label[2] = FileInfoBase.ORI_P2A_TYPE;
        } else if ((num == 2) && (origOrient[2][num] > 0)) {
            label[2] = FileInfoBase.ORI_I2S_TYPE;
        } else if ((num == 2) && (origOrient[2][num] < 0)) {
            label[2] = FileInfoBase.ORI_S2I_TYPE;
        }

        return label;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f getOrigPC() {
        return origPC;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public float[] getOrigRes() {
        return origRes;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f getTlrcAC() {
        return tlrcAC;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public int[] getTlrcDim() {
        return tlrcDim;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public Vector3f getTlrcPC() {
        return tlrcPC;
    }

    /**
     * public Vector3f getTlrcMin() { return tlrcMin; } public Vector3f getTlrcMax() { return tlrcMax; }.
     *
     * @return  DOCUMENT ME!
     */
    public float[] getTlrcRes() {
        return tlrcRes;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isAcpc() {
        return isAcpc;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void isAcpc(boolean flag) {
        isAcpc = flag;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean isTlrc() {
        return isTlrc;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  flag  DOCUMENT ME!
     */
    public void isTlrc(boolean flag) {
        isTlrc = flag;
    }

    /**
     * compute transforms from one space to another one.
     *
     * @param  pt     DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void origToAcpc(Vector3f pt, Vector3f trans) {

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        trans.X = acpcAC.X + (origOrient[0][0] * (pt.X - origAC.X) * origRes[0] / acpcRes) +
                  (origOrient[0][1] * (pt.Y - origAC.Y) * origRes[1] / acpcRes) +
                  (origOrient[0][2] * (pt.Z - origAC.Z) * origRes[2] / acpcRes);

        trans.Y = acpcAC.Y + (origOrient[1][0] * (pt.X - origAC.X) * origRes[0] / acpcRes) +
                  (origOrient[1][1] * (pt.Y - origAC.Y) * origRes[1] / acpcRes) +
                  (origOrient[1][2] * (pt.Z - origAC.Z) * origRes[2] / acpcRes);

        trans.Z = acpcAC.Z + (origOrient[2][0] * (pt.X - origAC.X) * origRes[0] / acpcRes) +
                  (origOrient[2][1] * (pt.Y - origAC.Y) * origRes[1] / acpcRes) +
                  (origOrient[2][2] * (pt.Z - origAC.Z) * origRes[2] / acpcRes);

        /*
         *       trans.X = acpcAC.X + origOrient[0][0]*(pt.X-origAC.X)*origRes[0]/acpcRes                +
         * origOrient[0][1]*(pt.Y-origAC.Y)*origRes[0]/acpcRes                +
         * origOrient[0][2]*(pt.Z-origAC.Z)*origRes[0]/acpcRes;
         *
         *    trans.Y = acpcAC.Y + origOrient[1][0]*(pt.X-origAC.X)*origRes[1]/acpcRes                +
         * origOrient[1][1]*(pt.Y-origAC.Y)*origRes[1]/acpcRes                +
         * origOrient[1][2]*(pt.Z-origAC.Z)*origRes[1]/acpcRes;
         *
         *    trans.Z = acpcAC.Z + origOrient[2][0]*(pt.X-origAC.X)*origRes[2]/acpcRes                +
         * origOrient[2][1]*(pt.Y-origAC.Y)*origRes[2]/acpcRes                +
         * origOrient[2][2]*(pt.Z-origAC.Z)*origRes[2]/acpcRes;
         */
        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x      DOCUMENT ME!
     * @param  y      DOCUMENT ME!
     * @param  z      DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void origToAcpc(int x, int y, int z, Vector3f trans) {

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        trans.X = acpcAC.X + (origOrient[0][0] * (x - origAC.X) * origRes[0] / acpcRes) +
                  (origOrient[0][1] * (y - origAC.Y) * origRes[1] / acpcRes) +
                  (origOrient[0][2] * (z - origAC.Z) * origRes[2] / acpcRes);

        trans.Y = acpcAC.Y + (origOrient[1][0] * (x - origAC.X) * origRes[0] / acpcRes) +
                  (origOrient[1][1] * (y - origAC.Y) * origRes[1] / acpcRes) +
                  (origOrient[1][2] * (z - origAC.Z) * origRes[2] / acpcRes);

        trans.Z = acpcAC.Z + (origOrient[2][0] * (x - origAC.X) * origRes[0] / acpcRes) +
                  (origOrient[2][1] * (y - origAC.Y) * origRes[1] / acpcRes) +
                  (origOrient[2][2] * (z - origAC.Z) * origRes[2] / acpcRes);

        /*
         *       trans.X = acpcAC.X + origOrient[0][0]*(x-origAC.X)*origRes[0]/acpcRes                +
         * origOrient[0][1]*(y-origAC.Y)*origRes[0]/acpcRes                +
         * origOrient[0][2]*(z-origAC.Z)*origRes[0]/acpcRes;
         *
         *    trans.Y = acpcAC.Y + origOrient[1][0]*(x-origAC.X)*origRes[1]/acpcRes                +
         * origOrient[1][1]*(y-origAC.Y)*origRes[1]/acpcRes                +
         * origOrient[1][2]*(z-origAC.Z)*origRes[1]/acpcRes;
         *
         *    trans.Z = acpcAC.Z + origOrient[2][0]*(x-origAC.X)*origRes[2]/acpcRes                +
         * origOrient[2][1]*(y-origAC.Y)*origRes[2]/acpcRes                +
         * origOrient[2][2]*(z-origAC.Z)*origRes[2]/acpcRes;
         */
        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pt     DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void origToTlrc(Vector3f pt, Vector3f trans) {
        Vector3f tmp = new Vector3f();
        origToAcpc(pt, tmp);
        acpcToTlrc(tmp, trans);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x      DOCUMENT ME!
     * @param  y      DOCUMENT ME!
     * @param  z      DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void origToTlrc(int x, int y, int z, Vector3f trans) {
        Vector3f tmp = new Vector3f();
        origToAcpc(x, y, z, tmp);
        acpcToTlrc(tmp, trans);
    }

    /**
     * read/write to a text file.
     *
     * @param  filename  DOCUMENT ME!
     */
    public void readFromFile(String filename) {

        try {
            File f = new File(filename);
            FileReader fr = new FileReader(f);
            BufferedReader br = new BufferedReader(fr);
            String line = br.readLine();
            float[] params;
            int[] dims;

            // Exact corresponding template
            if (!line.equals("Talairach Transform Info File (do not edit)")) {
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
                params = parseFloatParameters(br.readLine(), 3);
                setOrigOrigin(params);

                // Orient
                params = parseFloatParameters(br.readLine(), 9);

                float[][] R = new float[3][3];

                for (int i = 0; i < 3; i++) {

                    for (int j = 0; j < 3; j++) {
                        R[i][j] = params[(i * 3) + j];
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
        } catch (FileNotFoundException e) {
            System.out.println(e.getMessage());
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pt  DOCUMENT ME!
     */
    public void setAcpcMax(Vector3f pt) {
        acpcMax = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pt  DOCUMENT ME!
     */
    public void setAcpcMin(Vector3f pt) {
        acpcMin = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pt  DOCUMENT ME!
     */
    public void setAcpcPC(Vector3f pt) {
        acpcPC = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  res  DOCUMENT ME!
     */
    public void setAcpcRes(float res) {
        acpcRes = res;
        acpcDim[0] = Math.round(2 * (ACPC_LATERAL + 1) / acpcRes);
        acpcDim[1] = Math.round((ACPC_ANTERIOR + ACPC_POSTERIOR + 1) / acpcRes);
        acpcDim[2] = Math.round((ACPC_INFERIOR + ACPC_SUPERIOR + 1) / acpcRes);
        acpcAC = new Vector3f(ACPC_LATERAL / acpcRes, ACPC_ANTERIOR / acpcRes, ACPC_INFERIOR / acpcRes);

        // change the overall resolution for TLRC as well
        tlrcAC = new Vector3f(80.0f / acpcRes, 80.0f / acpcRes, 65.0f / acpcRes);
        tlrcPC = new Vector3f(80.0f / acpcRes, 103.0f / acpcRes, 65.0f / acpcRes);
        tlrcDim[0] = (int) (161.0f / acpcRes);
        tlrcDim[1] = (int) (191.0f / acpcRes);
        tlrcDim[2] = (int) (151.0f / acpcRes);
    }

    /**
     * accessors to set the unknown quantities.
     *
     * @param  pt  DOCUMENT ME!
     */
    public void setOrigAC(Vector3f pt) {
        origAC = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dim  DOCUMENT ME!
     */
    public void setOrigDim(int[] dim) {
        origDim = (int[]) dim.clone();
    }
    
    public void setOrigOrigin(float[] origin) {
        origOrigin = (float[]) origin.clone();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  orient  DOCUMENT ME!
     */
    public void setOrigOrient(float[][] orient) {
        origOrient = new float[3][3];

        for (int i = 0; i < 3; i++) {

            for (int j = 0; j < 3; j++) {
                origOrient[i][j] = orient[i][j];
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pt  DOCUMENT ME!
     */
    public void setOrigPC(Vector3f pt) {
        origPC = new Vector3f(pt.X, pt.Y, pt.Z);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  res  DOCUMENT ME!
     */
    public void setOrigRes(float[] res) {
        origRes = (float[]) res.clone();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  res  DOCUMENT ME!
     */
    public void setTlrcRes(float[] res) {
        tlrcRes = (float[]) res.clone();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pt     DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void tlrcToAcpc(Vector3f pt, Vector3f trans) {
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

        // (P_tlrc-AC_tlrc)*Res_tlrc = (P_acpc-AC_acpc)*Res_acpc
        if (usePC) {
            trans.X = acpcPC.X + ((pt.X - tlrcPC.X) / acpcRes * resx);
            trans.Y = acpcPC.Y + ((pt.Y - tlrcPC.Y) / acpcRes * resy);
            trans.Z = acpcPC.Z + ((pt.Z - tlrcPC.Z) / acpcRes * resz);
        } else {
            trans.X = acpcAC.X + ((pt.X - tlrcAC.X) / acpcRes * resx);
            trans.Y = acpcAC.Y + ((pt.Y - tlrcAC.Y) / acpcRes * resy);
            trans.Z = acpcAC.Z + ((pt.Z - tlrcAC.Z) / acpcRes * resz);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x      DOCUMENT ME!
     * @param  y      DOCUMENT ME!
     * @param  z      DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void tlrcToAcpc(int x, int y, int z, Vector3f trans) {
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
            trans.X = acpcPC.X + ((x - tlrcPC.X) / acpcRes * resx);
            trans.Y = acpcPC.Y + ((y - tlrcPC.Y) / acpcRes * resy);
            trans.Z = acpcPC.Z + ((z - tlrcPC.Z) / acpcRes * resz);
        } else {
            trans.X = acpcAC.X + ((x - tlrcAC.X) / acpcRes * resx);
            trans.Y = acpcAC.Y + ((y - tlrcAC.Y) / acpcRes * resy);
            trans.Z = acpcAC.Z + ((z - tlrcAC.Z) / acpcRes * resz);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  pt     DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void tlrcToOrig(Vector3f pt, Vector3f trans) {
        Vector3f tmp = new Vector3f();
        tlrcToAcpc(pt, tmp);
        acpcToOrig(tmp, trans);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  x      DOCUMENT ME!
     * @param  y      DOCUMENT ME!
     * @param  z      DOCUMENT ME!
     * @param  trans  DOCUMENT ME!
     */
    public void tlrcToOrig(int x, int y, int z, Vector3f trans) {
        Vector3f tmp = new Vector3f();
        tlrcToAcpc(x, y, z, tmp);
        acpcToOrig(tmp, trans);
    }

    /**
     * read/write to a text file.
     *
     * @param  filename  DOCUMENT ME!
     */
    public void writeToFile(String filename) {

        try {

            // open the file for writing
            File f = new File(filename);
            FileWriter fw = new FileWriter(f);
            PrintWriter pw = new PrintWriter(fw);

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
                pw.println("Orient: (" + origOrient[0][0] + ", " + origOrient[0][1] + ", " + origOrient[0][2] + ", " +
                           origOrient[1][0] + ", " + origOrient[1][1] + ", " + origOrient[1][2] + ", " +
                           origOrient[2][0] + ", " + origOrient[2][1] + ", " + origOrient[2][2] + ")");
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
                pw.println("Res: (" + tlrcRes[0] + ", " + tlrcRes[1] + ", " + tlrcRes[2] + ", " + tlrcRes[3] + ", " +
                           tlrcRes[4] + ", " + tlrcRes[5] + ", " + tlrcRes[6] + ")");
                pw.println("Dim: (" + tlrcDim[0] + ", " + tlrcDim[1] + ", " + tlrcDim[2] + ")");
            }

            // close the file
            fw.close();
        } catch (FileNotFoundException e) {
            System.out.println(e.getMessage());
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }

    }

    /**
     * parsing utility.
     *
     * @param   line  DOCUMENT ME!
     * @param   nb    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] parseFloatParameters(String line, int nb) {
        int next = 0;
        float[] x = new float[nb];

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
     * DOCUMENT ME!
     *
     * @param   line  DOCUMENT ME!
     * @param   nb    DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int[] parseIntParameters(String line, int nb) {
        int next = 0;
        int[] x = new int[nb];

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

}
