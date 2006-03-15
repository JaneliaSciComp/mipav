package gov.nih.mipav.model.structures;

import gov.nih.mipav.model.file.FileInfoBase;

import java.io.*;

/**
 *
 *   This is the structure to store information needed to compute
 *  	ACPC and Talairach normalization.
 *
 * If you have any questions, please drop me a line.
 * =====
 * Pilou Bazin
 * MEDIC, JHU
 * pbazin1@jhmi.edu
 *
 *	@version    July 26, 2004
 *	@author     Pierre-Louis Bazin
 *  @see        AlgorithmBase
 *  @see        TextRemoval
 *  @see        PlugInDialogTextRemoval
 *
 *
 */
public class TalairachTransformInfo
    implements
    Serializable {

    // orig: original image space
    /** Anterior Comissure in original space */
    private Point3Df origAC;

    /** Posterior Comissure in original space */
    private Point3Df origPC;

    //private Point3Df	origMin;	// Original min and max extents of the brain
    //private Point3Df	origMax;

    /** Original image voxel resolutions  */
    private float[] origRes;

    /** Original image dimensions */
    private int[] origDim;

    /** ACPC orientation in original image  */
    private float[][] origOrient;

    /* acpc: ACPC aligned space */

    /** Anterior Comissure in acpc space   */
    private Point3Df acpcAC = new Point3Df(95.0f, 95.0f, 70.0f);

    /** Posterior Comissure in acpc space  */
    private Point3Df acpcPC = new Point3Df();

    /** ACPC min and max extents of the brain  */
    private Point3Df acpcMin;

    private Point3Df acpcMax;

    /**  voxel resolution (cubic) */
    private float acpcRes = 1.0f;

    /** image dimensions  */
    private int[] acpcDim = {
        192, 236, 171};

    // acpc constants
    private static final float ACPC_LATERAL = 95.0f;
    private static final float ACPC_ANTERIOR = 95.0f;
    private static final float ACPC_POSTERIOR = 140.0f;
    private static final float ACPC_INFERIOR = 70.0f;
    private static final float ACPC_SUPERIOR = 100.0f;

    // tlrc: Talairach space
    /** Anterior Comissure in Talairach space  */
    private Point3Df tlrcAC = new Point3Df(80.0f, 80.0f, 65.0f);

    /** Posterior Comissure in Talairach space */
    private Point3Df tlrcPC = new Point3Df(80.0f, 103.0f, 65.0f);

    //private Point3Df	tlrcMin = new Point3Df(12.0f,10.0f,23.0f);	// ACPC min and max extents of the brain
    //private Point3Df	tlrcMax = new Point3Df(148.0f,172.0f,139.0f);

    /** voxel resolutions for the sub-boxes  */
    private float[] tlrcRes;

    /** image dimensions */
    private int[] tlrcDim = {
        161, 191, 151};

    public static final int TLRC_AC_TO_PC = 23;
    public static final int TLRC_FRONT_TO_AC = 70;
    public static final int TLRC_PC_TO_BACK = 79;
    public static final int TLRC_BOT_TO_AC = 42;
    public static final int TLRC_AC_TO_TOP = 74;
    public static final int TLRC_AC_TO_LAT = 68;

    // flags for computations
    /** true if we have the data to compute orig <-> acpc */
    private boolean isAcpc = false;

    /** true if we ahve the data to compute acpc <-> tlrc */
    private boolean isTlrc = false;

    public TalairachTransformInfo() {

    }

    /**
     *  accessors to set the unknown quantities
     */
    public void setOrigAC(Point3Df pt) {
        origAC = new Point3Df(pt.x, pt.y, pt.z);
    }

    public void setOrigPC(Point3Df pt) {
        origPC = new Point3Df(pt.x, pt.y, pt.z);
    }

    public void setOrigRes(float[] res) {
        origRes = (float[]) res.clone();
    }

    public void setOrigDim(int[] dim) {
        origDim = (int[]) dim.clone();
    }

    public void setOrigOrient(float[][] orient) {
        origOrient = new float[3][3];
        for (int i = 0; i < 3; i++)for (int j = 0; j < 3; j++)
            origOrient[i][j] = orient[i][j];
    }

    public void setAcpcPC(Point3Df pt) {
        acpcPC = new Point3Df(pt.x, pt.y, pt.z);
    }

    public void setAcpcMin(Point3Df pt) {
        acpcMin = new Point3Df(pt.x, pt.y, pt.z);
    }

    public void setAcpcMax(Point3Df pt) {
        acpcMax = new Point3Df(pt.x, pt.y, pt.z);
    }

    public void setAcpcRes(float res) {
        acpcRes = res;
        acpcDim[0] = Math.round(2 * (ACPC_LATERAL + 1) / acpcRes);
        acpcDim[1] = Math.round( (ACPC_ANTERIOR + ACPC_POSTERIOR + 1) / acpcRes);
        acpcDim[2] = Math.round( (ACPC_INFERIOR + ACPC_SUPERIOR + 1) / acpcRes);
        acpcAC = new Point3Df(ACPC_LATERAL / acpcRes, ACPC_ANTERIOR / acpcRes, ACPC_INFERIOR / acpcRes);

        // change the overall resolution for TLRC as well
        tlrcAC = new Point3Df(80.0f / acpcRes, 80.0f / acpcRes, 65.0f / acpcRes);
        tlrcPC = new Point3Df(80.0f / acpcRes, 103.0f / acpcRes, 65.0f / acpcRes);
        tlrcDim[0] = (int) (161.0f / acpcRes);
        tlrcDim[1] = (int) (191.0f / acpcRes);
        tlrcDim[2] = (int) (151.0f / acpcRes);
    }

    public void setTlrcRes(float[] res) {
        tlrcRes = (float[]) res.clone();
    }

    public void isAcpc(boolean flag) {
        isAcpc = flag;
    }

    public void isTlrc(boolean flag) {
        isTlrc = flag;
    }

    /**
     *  accessors to get all quantities
     */
    public Point3Df getOrigAC() {
        return origAC;
    }

    public Point3Df getOrigPC() {
        return origPC;
    }

    public float[] getOrigRes() {
        return origRes;
    }

    public int[] getOrigDim() {
        return origDim;
    }

    public float[][] getOrigOrient() {
        return origOrient;
    }

    public Point3Df getAcpcAC() {
        return acpcAC;
    }

    public Point3Df getAcpcPC() {
        return acpcPC;
    }

    public Point3Df getAcpcMin() {
        return acpcMin;
    }

    public Point3Df getAcpcMax() {
        return acpcMax;
    }

    public float getAcpcRes() {
        return acpcRes;
    }

    public int[] getAcpcDim() {
        return acpcDim;
    }

    public Point3Df getTlrcAC() {
        return tlrcAC;
    }

    public Point3Df getTlrcPC() {
        return tlrcPC;
    }

    //public Point3Df getTlrcMin() { return tlrcMin; }
    //public Point3Df getTlrcMax() { return tlrcMax; }
    public float[] getTlrcRes() {
        return tlrcRes;
    }

    public int[] getTlrcDim() {
        return tlrcDim;
    }

    public boolean isAcpc() {
        return isAcpc;
    }

    public boolean isTlrc() {
        return isTlrc;
    }

    /**
     *	read/write to a text file
     */
    public void writeToFile(String filename) {
        try {
            // open the file for writing
            File f = new File(filename);
            FileWriter fw = new FileWriter(f);
            PrintWriter pw = new PrintWriter(fw);
            // write the parameters
            pw.println("Talairach Transform Info File (do not edit)");
            if (isAcpc) pw.println("ACPC aligned");
            else pw.println("not ACPC aligned");
            if (isTlrc) pw.println("Talairach aligned");
            else pw.println("not Talairach aligned");

            if (isAcpc) {
                pw.println("- Original Image -");
                pw.println("AC: (" + origAC.x + ", " + origAC.y + ", " + origAC.z + ")");
                pw.println("PC: (" + origPC.x + ", " + origPC.y + ", " + origPC.z + ")");
                pw.println("Res: (" + origRes[0] + ", " + origRes[1] + ", " + origRes[2] + ")");
                pw.println("Dim: (" + origDim[0] + ", " + origDim[1] + ", " + origDim[2] + ")");
                pw.println("Orient: (" + origOrient[0][0] + ", " + origOrient[0][1] + ", " + origOrient[0][2] + ", "
                           + origOrient[1][0] + ", " + origOrient[1][1] + ", " + origOrient[1][2] + ", "
                           + origOrient[2][0] + ", " + origOrient[2][1] + ", " + origOrient[2][2] + ")");
                pw.println("- AC-PC Image -");
                pw.println("AC: (" + acpcAC.x + ", " + acpcAC.y + ", " + acpcAC.z + ")");
                pw.println("PC: (" + acpcPC.x + ", " + acpcPC.y + ", " + acpcPC.z + ")");
                pw.println("Res: (" + acpcRes + ")");
                pw.println("Dim: (" + acpcDim[0] + ", " + acpcDim[1] + ", " + acpcDim[2] + ")");
            }

            if (isTlrc) {
                pw.println("Min: (" + acpcMin.x + ", " + acpcMin.y + ", " + acpcMin.z + ")");
                pw.println("Max: (" + acpcMax.x + ", " + acpcMax.y + ", " + acpcMax.z + ")");

                pw.println("- Talairach Image -");
                pw.println("AC: (" + tlrcAC.x + ", " + tlrcAC.y + ", " + tlrcAC.z + ")");
                pw.println("PC: (" + tlrcPC.x + ", " + tlrcPC.y + ", " + tlrcPC.z + ")");
                pw.println("Res: (" + tlrcRes[0] + ", " + tlrcRes[1] + ", " + tlrcRes[2] + ", "
                           + tlrcRes[3] + ", " + tlrcRes[4] + ", " + tlrcRes[5] + ", " + tlrcRes[6] + ")");
                pw.println("Dim: (" + tlrcDim[0] + ", " + tlrcDim[1] + ", " + tlrcDim[2] + ")");
            }
            // close the file
            fw.close();
        }
        catch (FileNotFoundException e) {
            System.out.println(e.getMessage());
        }
        catch (IOException e) {
            System.out.println(e.getMessage());
        }

    }

    /**
     *	read/write to a text file
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
            }
            else {
                if (line.equals("ACPC aligned")) isAcpc = true;
                else isAcpc = false;
                line = br.readLine();
                if (line.equals("Talairach aligned")) isTlrc = true;
                else isTlrc = false;
                line = br.readLine();
            }
            if (isAcpc) {
                // Original Image
                // AC
                params = parseFloatParameters(br.readLine(), 3);
                setOrigAC(new Point3Df(params[0], params[1], params[2]));
                // PC
                params = parseFloatParameters(br.readLine(), 3);
                setOrigPC(new Point3Df(params[0], params[1], params[2]));
                // Res
                params = parseFloatParameters(br.readLine(), 3);
                setOrigRes(params);
                // Dim
                dims = parseIntParameters(br.readLine(), 3);
                setOrigDim(dims);
                // Orient
                params = parseFloatParameters(br.readLine(), 9);
                float[][] R = new float[3][3];
                for (int i = 0; i < 3; i++)for (int j = 0; j < 3; j++)
                    R[i][j] = params[i * 3 + j];
                setOrigOrient(R);

                // ACPC image
                line = br.readLine();
                // AC
                params = parseFloatParameters(br.readLine(), 3);
                // not used
                // PC
                params = parseFloatParameters(br.readLine(), 3);
                setAcpcPC(new Point3Df(params[0], params[1], params[2]));
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
                setAcpcMin(new Point3Df(params[0], params[1], params[2]));
                // Max
                params = parseFloatParameters(br.readLine(), 3);
                setAcpcMax(new Point3Df(params[0], params[1], params[2]));

                // TLRC image
                line = br.readLine();
                // AC
                params = parseFloatParameters(br.readLine(), 3);
                // not used
                // PC
                params = parseFloatParameters(br.readLine(), 3);
                //not used
                // Res
                params = parseFloatParameters(br.readLine(), 7);
                setTlrcRes(params);
                // Dim
                dims = parseIntParameters(br.readLine(), 3);
                // not used
            }

            br.close();
            fr.close();
        }
        catch (FileNotFoundException e) {
            System.out.println(e.getMessage());
        }
        catch (IOException e) {
            System.out.println(e.getMessage());
        }
        return ;
    }

    /**
     *	parsing utility
     */
    private float[] parseFloatParameters(String line, int nb) {
        int next = 0;
        float[] x = new float[nb];
        // coordinates
        int prev = 0;
        next = line.indexOf("(", prev);
        prev = next + 1;
        if (nb > 1) next = line.indexOf(", ", prev);
        else next = line.indexOf(")", prev);
        System.out.print("|" + line.substring(prev, next));
        x[0] = Float.valueOf(line.substring(prev, next)).floatValue();
        for (int n = 1; n < nb - 1; n++) {
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
        for (int n = 1; n < nb - 1; n++) {
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
     *  compute transforms from one space to another one
     */
    public void origToAcpc(Point3Df pt, Point3Df trans) {

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        trans.x = acpcAC.x + origOrient[0][0] * (pt.x - origAC.x) * origRes[0] / acpcRes
            + origOrient[0][1] * (pt.y - origAC.y) * origRes[1] / acpcRes
            + origOrient[0][2] * (pt.z - origAC.z) * origRes[2] / acpcRes;

        trans.y = acpcAC.y + origOrient[1][0] * (pt.x - origAC.x) * origRes[0] / acpcRes
            + origOrient[1][1] * (pt.y - origAC.y) * origRes[1] / acpcRes
            + origOrient[1][2] * (pt.z - origAC.z) * origRes[2] / acpcRes;

        trans.z = acpcAC.z + origOrient[2][0] * (pt.x - origAC.x) * origRes[0] / acpcRes
            + origOrient[2][1] * (pt.y - origAC.y) * origRes[1] / acpcRes
            + origOrient[2][2] * (pt.z - origAC.z) * origRes[2] / acpcRes;
        /*
                 trans.x = acpcAC.x + origOrient[0][0]*(pt.x-origAC.x)*origRes[0]/acpcRes
                           + origOrient[0][1]*(pt.y-origAC.y)*origRes[0]/acpcRes
                           + origOrient[0][2]*(pt.z-origAC.z)*origRes[0]/acpcRes;

                 trans.y = acpcAC.y + origOrient[1][0]*(pt.x-origAC.x)*origRes[1]/acpcRes
                           + origOrient[1][1]*(pt.y-origAC.y)*origRes[1]/acpcRes
                           + origOrient[1][2]*(pt.z-origAC.z)*origRes[1]/acpcRes;

                 trans.z = acpcAC.z + origOrient[2][0]*(pt.x-origAC.x)*origRes[2]/acpcRes
                           + origOrient[2][1]*(pt.y-origAC.y)*origRes[2]/acpcRes
                           + origOrient[2][2]*(pt.z-origAC.z)*origRes[2]/acpcRes;
         */
        return;
    }

    public void origToAcpc(int x, int y, int z, Point3Df trans) {

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        trans.x = acpcAC.x + origOrient[0][0] * (x - origAC.x) * origRes[0] / acpcRes
            + origOrient[0][1] * (y - origAC.y) * origRes[1] / acpcRes
            + origOrient[0][2] * (z - origAC.z) * origRes[2] / acpcRes;

        trans.y = acpcAC.y + origOrient[1][0] * (x - origAC.x) * origRes[0] / acpcRes
            + origOrient[1][1] * (y - origAC.y) * origRes[1] / acpcRes
            + origOrient[1][2] * (z - origAC.z) * origRes[2] / acpcRes;

        trans.z = acpcAC.z + origOrient[2][0] * (x - origAC.x) * origRes[0] / acpcRes
            + origOrient[2][1] * (y - origAC.y) * origRes[1] / acpcRes
            + origOrient[2][2] * (z - origAC.z) * origRes[2] / acpcRes;
        /*
                 trans.x = acpcAC.x + origOrient[0][0]*(x-origAC.x)*origRes[0]/acpcRes
                           + origOrient[0][1]*(y-origAC.y)*origRes[0]/acpcRes
                           + origOrient[0][2]*(z-origAC.z)*origRes[0]/acpcRes;

                 trans.y = acpcAC.y + origOrient[1][0]*(x-origAC.x)*origRes[1]/acpcRes
                           + origOrient[1][1]*(y-origAC.y)*origRes[1]/acpcRes
                           + origOrient[1][2]*(z-origAC.z)*origRes[1]/acpcRes;

                 trans.z = acpcAC.z + origOrient[2][0]*(x-origAC.x)*origRes[2]/acpcRes
                           + origOrient[2][1]*(y-origAC.y)*origRes[2]/acpcRes
                           + origOrient[2][2]*(z-origAC.z)*origRes[2]/acpcRes;
         */
        return;
    }

    public void acpcToOrig(Point3Df pt, Point3Df trans) {

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        trans.x = origAC.x + origOrient[0][0] * (pt.x - acpcAC.x) * acpcRes / origRes[0]
            + origOrient[1][0] * (pt.y - acpcAC.y) * acpcRes / origRes[0]
            + origOrient[2][0] * (pt.z - acpcAC.z) * acpcRes / origRes[0];

        trans.y = origAC.y + origOrient[0][1] * (pt.x - acpcAC.x) * acpcRes / origRes[1]
            + origOrient[1][1] * (pt.y - acpcAC.y) * acpcRes / origRes[1]
            + origOrient[2][1] * (pt.z - acpcAC.z) * acpcRes / origRes[1];

        trans.z = origAC.z + origOrient[0][2] * (pt.x - acpcAC.x) * acpcRes / origRes[2]
            + origOrient[1][2] * (pt.y - acpcAC.y) * acpcRes / origRes[2]
            + origOrient[2][2] * (pt.z - acpcAC.z) * acpcRes / origRes[2];
        /*
                 trans.x = origAC.x + origOrient[0][0]*(pt.x-acpcAC.x)*acpcRes/origRes[0]
                           + origOrient[1][0]*(pt.y-acpcAC.y)*acpcRes/origRes[1]
                           + origOrient[2][0]*(pt.z-acpcAC.z)*acpcRes/origRes[2];

                 trans.y = origAC.y + origOrient[0][1]*(pt.x-acpcAC.x)*acpcRes/origRes[0]
                           + origOrient[1][1]*(pt.y-acpcAC.y)*acpcRes/origRes[1]
                           + origOrient[2][1]*(pt.z-acpcAC.z)*acpcRes/origRes[2];

                 trans.z = origAC.z + origOrient[0][2]*(pt.x-acpcAC.x)*acpcRes/origRes[0]
                           + origOrient[1][2]*(pt.y-acpcAC.y)*acpcRes/origRes[1]
                           + origOrient[2][2]*(pt.z-acpcAC.z)*acpcRes/origRes[2];
         */
        return;
    }

    public void acpcToOrig(int x, int y, int z, Point3Df trans) {

        // (P_acpc-AC_acpc)*Res_acpc*orientOrig = (P_orig-AC_orig)*Res_orig
        trans.x = origAC.x + origOrient[0][0] * (x - acpcAC.x) * acpcRes / origRes[0]
            + origOrient[1][0] * (y - acpcAC.y) * acpcRes / origRes[0]
            + origOrient[2][0] * (z - acpcAC.z) * acpcRes / origRes[0];

        trans.y = origAC.y + origOrient[0][1] * (x - acpcAC.x) * acpcRes / origRes[1]
            + origOrient[1][1] * (y - acpcAC.y) * acpcRes / origRes[1]
            + origOrient[2][1] * (z - acpcAC.z) * acpcRes / origRes[1];

        trans.z = origAC.z + origOrient[0][2] * (x - acpcAC.x) * acpcRes / origRes[2]
            + origOrient[1][2] * (y - acpcAC.y) * acpcRes / origRes[2]
            + origOrient[2][2] * (z - acpcAC.z) * acpcRes / origRes[2];
        /*
                 trans.x = origAC.x +  origOrient[0][0]*(x-acpcAC.x)*acpcRes/origRes[0]
                           +  origOrient[1][0]*(y-acpcAC.y)*acpcRes/origRes[1]
                           +  origOrient[2][0]*(z-acpcAC.z)*acpcRes/origRes[2];

                 trans.y = origAC.y +  origOrient[0][1]*(x-acpcAC.x)*acpcRes/origRes[0]
                           +  origOrient[1][1]*(y-acpcAC.y)*acpcRes/origRes[1]
                           +  origOrient[2][1]*(z-acpcAC.z)*acpcRes/origRes[2];

                 trans.z = origAC.z +  origOrient[0][2]*(x-acpcAC.x)*acpcRes/origRes[0]
                           +  origOrient[1][2]*(y-acpcAC.y)*acpcRes/origRes[1]
                           +  origOrient[2][2]*(z-acpcAC.z)*acpcRes/origRes[2];
         */
        return;
    }

    public void acpcToTlrc(Point3Df pt, Point3Df trans) {
        float resx, resy, resz;
        boolean usePC = false;

        // cases 0,1 x; 2,3,4 y; 5,6 z
        if (pt.x < acpcAC.x) resx = tlrcRes[0];
        else resx = tlrcRes[1];

        if (pt.y < acpcAC.y) resy = tlrcRes[2];
        else if (pt.y < acpcPC.y) resy = tlrcRes[3];
        else {
            resy = tlrcRes[4];
            usePC = true;
        }

        if (pt.z < acpcAC.z) resz = tlrcRes[5];
        else resz = tlrcRes[6];

        // (P_tlrc-AC_tlrc)*Res_tlrc = (P_acpc-AC_acpc)*Res_acpc
        if (usePC) {
            trans.x = tlrcPC.x + (pt.x - acpcPC.x) * acpcRes / resx;
            trans.y = tlrcPC.y + (pt.y - acpcPC.y) * acpcRes / resy;
            trans.z = tlrcPC.z + (pt.z - acpcPC.z) * acpcRes / resz;
        }
        else {
            trans.x = tlrcAC.x + (pt.x - acpcAC.x) * acpcRes / resx;
            trans.y = tlrcAC.y + (pt.y - acpcAC.y) * acpcRes / resy;
            trans.z = tlrcAC.z + (pt.z - acpcAC.z) * acpcRes / resz;
        }
        return;
    }

    public void acpcToTlrc(int x, int y, int z, Point3Df trans) {
        float resx, resy, resz;
        boolean usePC = false;

        // cases 0,1 x; 2,3,4 y; 5,6 z
        if (x < acpcAC.x) resx = tlrcRes[0];
        else resx = tlrcRes[1];

        if (y < acpcAC.y) resy = tlrcRes[2];
        else if (y < acpcPC.y) resy = tlrcRes[3];
        else {
            resy = tlrcRes[4];
            usePC = true;
        }

        if (z < acpcAC.z) resz = tlrcRes[5];
        else resz = tlrcRes[6];

        // (P_tlrc-AC_tlrc)*Res_tlrc = (P_acpc-AC_acpc)*Res_acpc
        if (usePC) {
            trans.x = tlrcPC.x + (x - acpcPC.x) * acpcRes / resx;
            trans.y = tlrcPC.y + (y - acpcPC.y) * acpcRes / resy;
            trans.z = tlrcPC.z + (z - acpcPC.z) * acpcRes / resz;
        }
        else {
            trans.x = tlrcAC.x + (x - acpcAC.x) * acpcRes / resx;
            trans.y = tlrcAC.y + (y - acpcAC.y) * acpcRes / resy;
            trans.z = tlrcAC.z + (z - acpcAC.z) * acpcRes / resz;
        }
        return;
    }

    public void tlrcToAcpc(Point3Df pt, Point3Df trans) {
        float resx, resy, resz;
        boolean usePC = false;

        // cases 0,1 x; 2,3,4 y; 5,6 z
        if (pt.x < tlrcAC.x) resx = tlrcRes[0];
        else resx = tlrcRes[1];

        if (pt.y < tlrcAC.y) resy = tlrcRes[2];
        else if (pt.y < tlrcPC.y) resy = tlrcRes[3];
        else {
            resy = tlrcRes[4];
            usePC = true;
        }

        if (pt.z < tlrcAC.z) resz = tlrcRes[5];
        else resz = tlrcRes[6];

        // (P_tlrc-AC_tlrc)*Res_tlrc = (P_acpc-AC_acpc)*Res_acpc
        if (usePC) {
            trans.x = acpcPC.x + (pt.x - tlrcPC.x) / acpcRes * resx;
            trans.y = acpcPC.y + (pt.y - tlrcPC.y) / acpcRes * resy;
            trans.z = acpcPC.z + (pt.z - tlrcPC.z) / acpcRes * resz;
        }
        else {
            trans.x = acpcAC.x + (pt.x - tlrcAC.x) / acpcRes * resx;
            trans.y = acpcAC.y + (pt.y - tlrcAC.y) / acpcRes * resy;
            trans.z = acpcAC.z + (pt.z - tlrcAC.z) / acpcRes * resz;
        }
        return;
    }

    public void tlrcToAcpc(int x, int y, int z, Point3Df trans) {
        float resx, resy, resz;
        boolean usePC = false;

        // cases 0,1 x; 2,3,4 y; 5,6 z
        if (x < tlrcAC.x) resx = tlrcRes[0];
        else resx = tlrcRes[1];

        if (y < tlrcAC.y) resy = tlrcRes[2];
        else if (y < tlrcPC.y) resy = tlrcRes[3];
        else {
            resy = tlrcRes[4];
            usePC = true;
        }

        if (z < tlrcAC.z) resz = tlrcRes[5];
        else resz = tlrcRes[6];

        // (P_tlrc-AC_tlrc)*Res_tlrc = (P_acpc-AC_acpc)*Res_acpc
        if (usePC) {
            trans.x = acpcPC.x + (x - tlrcPC.x) / acpcRes * resx;
            trans.y = acpcPC.y + (y - tlrcPC.y) / acpcRes * resy;
            trans.z = acpcPC.z + (z - tlrcPC.z) / acpcRes * resz;
        }
        else {
            trans.x = acpcAC.x + (x - tlrcAC.x) / acpcRes * resx;
            trans.y = acpcAC.y + (y - tlrcAC.y) / acpcRes * resy;
            trans.z = acpcAC.z + (z - tlrcAC.z) / acpcRes * resz;
        }
        return;
    }

    public void origToTlrc(Point3Df pt, Point3Df trans) {
        Point3Df tmp = new Point3Df();
        origToAcpc(pt, tmp);
        acpcToTlrc(tmp, trans);
    }

    public void origToTlrc(int x, int y, int z, Point3Df trans) {
        Point3Df tmp = new Point3Df();
        origToAcpc(x, y, z, tmp);
        acpcToTlrc(tmp, trans);
    }

    public void tlrcToOrig(Point3Df pt, Point3Df trans) {
        Point3Df tmp = new Point3Df();
        tlrcToAcpc(pt, tmp);
        acpcToOrig(tmp, trans);
    }

    public void tlrcToOrig(int x, int y, int z, Point3Df trans) {
        Point3Df tmp = new Point3Df();
        tlrcToAcpc(x, y, z, tmp);
        acpcToOrig(tmp, trans);
    }

    /** for debug: return the transform as a 4x4 matrix **/
    public double[][] displayAcpc() {
        double[][] mat = new double[4][4];
        Point3Df trans = new Point3Df();

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

        mat[0][3] = origAC.x + origOrient[0][0] * ( -acpcAC.x) / origRes[0] * acpcRes
            + origOrient[1][0] * ( -acpcAC.y) / origRes[0] * acpcRes
            + origOrient[2][0] * ( -acpcAC.z) / origRes[0] * acpcRes;

        mat[1][3] = origAC.y + origOrient[0][1] * ( -acpcAC.x) / origRes[1] * acpcRes
            + origOrient[1][1] * ( -acpcAC.y) / origRes[1] * acpcRes
            + origOrient[2][1] * ( -acpcAC.z) / origRes[1] * acpcRes;

        mat[2][3] = origAC.z + origOrient[0][2] * ( -acpcAC.x) / origRes[2] * acpcRes
            + origOrient[1][2] * ( -acpcAC.y) / origRes[2] * acpcRes
            + origOrient[2][2] * ( -acpcAC.z) / origRes[2] * acpcRes;

        mat[3][0] = 0;
        mat[3][1] = 0;
        mat[3][2] = 0;
        mat[3][3] = 1;

        return mat;
    }

    public double[][] displayAcpcInverse() {
        double[][] mat = new double[4][4];
        Point3Df trans = new Point3Df();

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

        mat[0][3] = acpcAC.x + origOrient[0][0] * ( -origAC.x) * origRes[0] / acpcRes
            + origOrient[0][1] * ( -origAC.y) * origRes[1] / acpcRes
            + origOrient[0][2] * ( -origAC.z) * origRes[2] / acpcRes;

        mat[1][3] = acpcAC.y + origOrient[1][0] * ( -origAC.x) * origRes[0] / acpcRes
            + origOrient[1][1] * ( -origAC.y) * origRes[1] / acpcRes
            + origOrient[1][2] * ( -origAC.z) * origRes[2] / acpcRes;

        mat[2][3] = acpcAC.z + origOrient[2][0] * ( -origAC.x) * origRes[0] / acpcRes
            + origOrient[2][1] * ( -origAC.y) * origRes[1] / acpcRes
            + origOrient[2][2] * ( -origAC.z) * origRes[2] / acpcRes;

        mat[3][0] = 0;
        mat[3][1] = 0;
        mat[3][2] = 0;
        mat[3][3] = 1;

        return mat;
    }

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
            mat[0][3] = acpcPC.x - tlrcPC.x * tlrcRes[x] / acpcRes;
            mat[1][3] = acpcPC.y - tlrcPC.y * tlrcRes[y] / acpcRes;
            mat[2][3] = acpcPC.z - tlrcPC.z * tlrcRes[z] / acpcRes;
        }
        else {
            mat[0][3] = acpcAC.x - tlrcAC.x * tlrcRes[x] / acpcRes;
            mat[1][3] = acpcAC.y - tlrcAC.y * tlrcRes[y] / acpcRes;
            mat[2][3] = acpcAC.z - tlrcAC.z * tlrcRes[z] / acpcRes;
        }
        mat[3][0] = 0;
        mat[3][1] = 0;
        mat[3][2] = 0;
        mat[3][3] = id;

        return mat;
    }

    public int[] getOrigOrientLabels() {
        int num;
        int[] label = new int[3];

        // X: R2L
        num = 0;
        for (int n = 1; n < 3; n++)
            if (origOrient[0][n] * origOrient[0][n] >
                origOrient[0][num] * origOrient[0][num])
                num = n;

        if ( (num == 0) && (origOrient[0][num] > 0)) label[0]
            = FileInfoBase.ORI_R2L_TYPE;
        else if ( (num == 0) && (origOrient[0][num] < 0)) label[0] = FileInfoBase.ORI_L2R_TYPE;
        else if ( (num == 1) && (origOrient[0][num] > 0)) label[0] = FileInfoBase.ORI_A2P_TYPE;
        else if ( (num == 1) && (origOrient[0][num] < 0)) label[0] = FileInfoBase.ORI_P2A_TYPE;
        else if ( (num == 2) && (origOrient[0][num] > 0)) label[0] = FileInfoBase.ORI_I2S_TYPE;
        else if ( (num == 2) && (origOrient[0][num] < 0)) label[0] = FileInfoBase.ORI_S2I_TYPE;

        // Y: A2P
        num = 0;
        for (int n = 1; n < 3; n++)
            if (origOrient[1][n] * origOrient[1][n] >
                origOrient[1][num] * origOrient[1][num])
                num = n;

        if ( (num == 0) && (origOrient[1][num] > 0)) label[1]
            = FileInfoBase.ORI_R2L_TYPE;
        else if ( (num == 0) && (origOrient[1][num] < 0)) label[1] = FileInfoBase.ORI_L2R_TYPE;
        else if ( (num == 1) && (origOrient[1][num] > 0)) label[1] = FileInfoBase.ORI_A2P_TYPE;
        else if ( (num == 1) && (origOrient[1][num] < 0)) label[1] = FileInfoBase.ORI_P2A_TYPE;
        else if ( (num == 2) && (origOrient[1][num] > 0)) label[1] = FileInfoBase.ORI_I2S_TYPE;
        else if ( (num == 2) && (origOrient[1][num] < 0)) label[1] = FileInfoBase.ORI_S2I_TYPE;

        // Z: I2S
        num = 0;
        for (int n = 1; n < 3; n++)
            if (origOrient[2][n] * origOrient[2][n] >
                origOrient[2][num] * origOrient[2][num])
                num = n;

        if ( (num == 0) && (origOrient[2][num] > 0)) label[2]
            = FileInfoBase.ORI_R2L_TYPE;
        else if ( (num == 0) && (origOrient[2][num] < 0)) label[2] = FileInfoBase.ORI_L2R_TYPE;
        else if ( (num == 1) && (origOrient[2][num] > 0)) label[2] = FileInfoBase.ORI_A2P_TYPE;
        else if ( (num == 1) && (origOrient[2][num] < 0)) label[2] = FileInfoBase.ORI_P2A_TYPE;
        else if ( (num == 2) && (origOrient[2][num] > 0)) label[2] = FileInfoBase.ORI_I2S_TYPE;
        else if ( (num == 2) && (origOrient[2][num] < 0)) label[2] = FileInfoBase.ORI_S2I_TYPE;

        return label;
    }

    public int getOrigImageOrientLabel() {
        int num;
        int label = 0;

        // Z: I2S
        num = 0;
        for (int n = 1; n < 3; n++)
            if (origOrient[2][n] * origOrient[2][n] >
                origOrient[2][num] * origOrient[2][num])
                num = n;

        if (num == 0) label = FileInfoBase.SAGITTAL;
        else if (num == 1) label = FileInfoBase.CORONAL;
        else if (num == 2) label = FileInfoBase.AXIAL;

        return label;
    }

}
