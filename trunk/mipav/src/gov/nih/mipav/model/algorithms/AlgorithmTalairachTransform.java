package gov.nih.mipav.model.algorithms;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.structures.*;

import java.io.*;

import java.util.*;


/**
 * Performs original to ACPC, original to Talairach, ACPC to original, ACPC to Talairach, Talairach to original, or
 * Talairach to ACPC transformation.
 *
 * @version  May 26, 2005
 * @author   Pilou Bazin
 */
public class AlgorithmTalairachTransform extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Trilinear interpolation. */
    public static final int TRILINEAR = 0;

    /** Biilinear interpolation. */
    public static final int BILINEAR = 1;

    /** Nearest neighbor interpolation. */
    public static final int NEAREST_NEIGHBOR = 2;

    /** Cubic bspline interpolation. */
    public static final int BSPLINE3 = 3;

    /** Quadratic bspline interpolation. */
    public static final int BSPLINE4 = 4;

    /** Cubic lagrangian interpolation. */
    public static final int CUBIC_LAGRANGIAN = 5;

    /** Quintic lagrangian interpolation. */
    public static final int QUINTIC_LAGRANGIAN = 6;

    /** Heptic lagrangian interpolation. */
    public static final int HEPTIC_LAGRANGIAN = 7;

    /** Windowed sinc interpolation. */
    public static final int WSINC = 8;

    /** DOCUMENT ME! */
    public static final int ORIG_TO_ACPC = 0;

    /** DOCUMENT ME! */
    public static final int ORIG_TO_TLRC = 1;

    /** DOCUMENT ME! */
    public static final int ACPC_TO_TLRC = 2;

    /** DOCUMENT ME! */
    public static final int TLRC_TO_ACPC = 3;

    /** DOCUMENT ME! */
    public static final int TLRC_TO_ORIG = 4;

    /** DOCUMENT ME! */
    public static final int ACPC_TO_ORIG = 5;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    boolean doClip;

    /** DOCUMENT ME! */
    boolean doVOI;

    /** DOCUMENT ME! */
    int interpolation;

    /** DOCUMENT ME! */
    String suffix;

    /** DOCUMENT ME! */
    int transformType;

    /** DOCUMENT ME! */
    String transformTypeName;

    /** DOCUMENT ME! */
    private AlgorithmBSpline Bspline;

    /** DOCUMENT ME! */
    private AlgorithmCubicLagrangian CLag;

    /** DOCUMENT ME! */
    private AlgorithmHepticLagrangian HLag;

    /** DOCUMENT ME! */
    private int[] inVolExtents = new int[3];

    /** DOCUMENT ME! */
    private int nix, niy, niz; // input image dimensions

    /** DOCUMENT ME! */
    private int nrx, nry, nrz; // result image dimensions

    /** DOCUMENT ME! */
    private AlgorithmQuinticLagrangian QLag;

    /** DOCUMENT ME! */
    private TalairachTransformInfo tInfo;

    /** DOCUMENT ME! */
    private AlgorithmWSinc WSinc;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmTalairachTransform object.
     *
     * @param  destImage      DOCUMENT ME!
     * @param  srcImage       DOCUMENT ME!
     * @param  tInfo          DOCUMENT ME!
     * @param  transformType  DOCUMENT ME!
     * @param  interpolation  DOCUMENT ME!
     * @param  doClip         DOCUMENT ME!
     * @param  doVOI          DOCUMENT ME!
     */
    public AlgorithmTalairachTransform(ModelImage destImage, ModelImage srcImage, TalairachTransformInfo tInfo,
                                       int transformType, int interpolation, boolean doClip, boolean doVOI) {
        super(destImage, srcImage);
        this.tInfo = tInfo;
        this.transformType = transformType;
        this.interpolation = interpolation;
        this.doClip = doClip;
        this.doVOI = doVOI;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {

        // System.err.println("Calling disposeLocal in algo talairach transform");
        srcImage = null;
        destImage = null;
        System.gc();
    }


    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            // notifyListeners(this);
            return;
        }

        if (destImage == null) {
            displayError("Destination Image is null");

            // notifyListeners(this);
            return;
        }

        // start the timer to compute the elapsed time
        setStartTime();

        if (destImage != null) { // if there exists a destination image

            if (srcImage.getNDims() == 2) {
                displayError("Source Image is 2D");

                // notifyListeners(this);
                return;
            }

            float[] buffer;
            float[] result;

            // find dimensions
            if (transformType == ORIG_TO_ACPC) {
                nix = tInfo.getOrigDim()[0];
                niy = tInfo.getOrigDim()[1];
                niz = tInfo.getOrigDim()[2];
                nrx = tInfo.getAcpcDim()[0];
                nry = tInfo.getAcpcDim()[1];
                nrz = tInfo.getAcpcDim()[2];
                transformTypeName = "orig to acpc";
                suffix = "_acpc";
            } else if (transformType == ACPC_TO_ORIG) {
                nix = tInfo.getAcpcDim()[0];
                niy = tInfo.getAcpcDim()[1];
                niz = tInfo.getAcpcDim()[2];
                nrx = tInfo.getOrigDim()[0];
                nry = tInfo.getOrigDim()[1];
                nrz = tInfo.getOrigDim()[2];
                transformTypeName = "acpc to orig";
                suffix = "_orig";
            } else if (transformType == ACPC_TO_TLRC) {
                nix = tInfo.getAcpcDim()[0];
                niy = tInfo.getAcpcDim()[1];
                niz = tInfo.getAcpcDim()[2];
                nrx = tInfo.getTlrcDim()[0];
                nry = tInfo.getTlrcDim()[1];
                nrz = tInfo.getTlrcDim()[2];
                transformTypeName = "acpc to tlrc";
                suffix = "_tlrc";
            } else if (transformType == TLRC_TO_ACPC) {
                nix = tInfo.getTlrcDim()[0];
                niy = tInfo.getTlrcDim()[1];
                niz = tInfo.getTlrcDim()[2];
                nrx = tInfo.getAcpcDim()[0];
                nry = tInfo.getAcpcDim()[1];
                nrz = tInfo.getAcpcDim()[2];
                transformTypeName = "tlrc to acpc";
                suffix = "_acpc";
            } else if (transformType == ORIG_TO_TLRC) {
                nix = tInfo.getOrigDim()[0];
                niy = tInfo.getOrigDim()[1];
                niz = tInfo.getOrigDim()[2];
                nrx = tInfo.getTlrcDim()[0];
                nry = tInfo.getTlrcDim()[1];
                nrz = tInfo.getTlrcDim()[2];
                transformTypeName = "orig to tlrc";
                suffix = "_tlrc";
            } else if (transformType == TLRC_TO_ORIG) {
                nix = tInfo.getTlrcDim()[0];
                niy = tInfo.getTlrcDim()[1];
                niz = tInfo.getTlrcDim()[2];
                nrx = tInfo.getOrigDim()[0];
                nry = tInfo.getOrigDim()[1];
                nrz = tInfo.getOrigDim()[2];
                transformTypeName = "tlrc to orig";
                suffix = "_orig";
            } else {
                nix = 0;
                niy = 0;
                niz = 0;
                nrx = 0;
                nry = 0;
                nrz = 0;
            }

            // There are 3 sample acpc files on the Afni server.  s14_Anat+acpc.HEAD is the
            // expected 192, 236, 171,  but EDspgr+acpc.HEAD and anat+acpc.HEAD are 
            // 191, 236, 171.
            if ((nix < srcImage.getExtents()[0]-1) || (nix > srcImage.getExtents()[0] + 1) ||
                    (niy != srcImage.getExtents()[1]) ||
                    (niz != srcImage.getExtents()[2])) {
                displayError("Source Image has wrong dimensions");

                // notifyListeners(this);
                return;
            }
            
            if (nix != srcImage.getExtents()[0]) {
                nix = srcImage.getExtents()[0];
            }

            try {
                inVolExtents[0] = nix;
                inVolExtents[1] = niy;
                inVolExtents[2] = niz;

                // image length is length in 2 dims
                int length = nix * niy * niz;
                buffer = new float[length];
                srcImage.exportData(0, length, buffer); // locks and releases lock
                result = new float[nrx * nry * nrz];
                fireProgressStateChanged(srcImage.getImageName(), "Processing image ...");
            } catch (IOException error) {
                buffer = null;
                result = null;
                errorCleanUp("Algorithm Talairach Transform reports: source image locked", true);

                return;
            } catch (OutOfMemoryError e) {
                buffer = null;
                result = null;
                errorCleanUp("Algorithm Talairach Transform reports: out of memory", true);

                return;
            }

            // main algorithm
            transformTalairachVolume(buffer, result);
        }
        
        

        if (!threadStopped) {
            setCompleted(true);
        } else {
            setCompleted(false);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  img     DOCUMENT ME!
     * @param  xi      DOCUMENT ME!
     * @param  yi      DOCUMENT ME!
     * @param  zi      DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  xr      DOCUMENT ME!
     * @param  yr      DOCUMENT ME!
     * @param  zr      DOCUMENT ME!
     */
    private void computeBSplineImage(float[] img, float xi, float yi, float zi, float[] result, int xr, int yr,
                                     int zr) {

        if ((xi >= 0) && (xi <= (nix - 1)) && (yi >= 0) && (yi <= (niy - 1)) && (zi >= 0) && (zi <= (niz - 1))) {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = Bspline.bSpline3D(0, 0, 0, xi, yi, zi);

        } else {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = 0.0f;
        }

        return;

    }

    /**
     * DOCUMENT ME!
     *
     * @param  img     DOCUMENT ME!
     * @param  xi      DOCUMENT ME!
     * @param  yi      DOCUMENT ME!
     * @param  zi      DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  xr      DOCUMENT ME!
     * @param  yr      DOCUMENT ME!
     * @param  zr      DOCUMENT ME!
     */
    private void computeCubicLagrangian(float[] img, float xi, float yi, float zi, float[] result, int xr, int yr,
                                        int zr) {

        if ((xi >= 0) && (xi <= (nix - 1)) && (yi >= 0) && (yi <= (niy - 1)) && (zi >= 0) && (zi <= (niz - 1))) {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = CLag.cubicLagrangian3D(xi, yi, zi);

        } else {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = 0.0f;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  img     DOCUMENT ME!
     * @param  xi      DOCUMENT ME!
     * @param  yi      DOCUMENT ME!
     * @param  zi      DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  xr      DOCUMENT ME!
     * @param  yr      DOCUMENT ME!
     * @param  zr      DOCUMENT ME!
     */
    private void computeHepticLagrangian(float[] img, float xi, float yi, float zi, float[] result, int xr, int yr,
                                         int zr) {

        if ((xi >= 0) && (xi <= (nix - 1)) && (yi >= 0) && (yi <= (niy - 1)) && (zi >= 0) && (zi <= (niz - 1))) {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = HLag.hepticLagrangian3D(xi, yi, zi);

        } else {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = 0.0f;
        }

        return;
    }

    /**
     * Transforms and resamples volume using nearest neighbor interpolation.
     *
     * @param  img     DOCUMENT ME!
     * @param  xi      DOCUMENT ME!
     * @param  yi      DOCUMENT ME!
     * @param  zi      DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  xr      DOCUMENT ME!
     * @param  yr      DOCUMENT ME!
     * @param  zr      DOCUMENT ME!
     */
    private void computeNearestImage(float[] img, float xi, float yi, float zi, float[] result, int xr, int yr,
                                     int zr) {
        int xa, ya, za;

        if ((xi >= 0) && (xi <= (nix - 1)) && (yi >= 0) && (yi <= (niy - 1)) && (zi >= 0) && (zi <= (niz - 1))) {
            xa = (int) Math.floor(xi);
            ya = (int) Math.floor(yi);
            za = (int) Math.floor(zi);

            if ((xi - xa) > 0.5) {
                xa = xa + 1;
            }

            if ((yi - ya) > 0.5) {
                ya = ya + 1;
            }

            if ((zi - za) > 0.5) {
                za = za + 1;
            }

            result[xr + (nrx * yr) + (nrx * nry * zr)] = img[xa + (nix * ya) + (nix * niy * za)];

        } else {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = 0.0f;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  img     DOCUMENT ME!
     * @param  xi      DOCUMENT ME!
     * @param  yi      DOCUMENT ME!
     * @param  zi      DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  xr      DOCUMENT ME!
     * @param  yr      DOCUMENT ME!
     * @param  zr      DOCUMENT ME!
     */
    private void computeQuinticLagrangian(float[] img, float xi, float yi, float zi, float[] result, int xr, int yr,
                                          int zr) {

        if ((xi >= 0) && (xi <= (nix - 1)) && (yi >= 0) && (yi <= (niy - 1)) && (zi >= 0) && (zi <= (niz - 1))) {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = QLag.quinticLagrangian3D(xi, yi, zi);

        } else {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = 0.0f;
        }

        return;
    }

    /**
     * Transforms and resamples volume using trilinear interpolation.
     *
     * @param  img     DOCUMENT ME!
     * @param  xi      DOCUMENT ME!
     * @param  yi      DOCUMENT ME!
     * @param  zi      DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  xr      DOCUMENT ME!
     * @param  yr      DOCUMENT ME!
     * @param  zr      DOCUMENT ME!
     */
    private void computeTrilinearImage(float[] img, float xi, float yi, float zi, float[] result, int xr, int yr,
                                       int zr) {
        int xa, ya, za, xb, yb, zb;
        float ax, bx, ay, by, az, bz;

        if ((xi >= 0) && (xi <= (nix - 1)) && (yi >= 0) && (yi <= (niy - 1)) && (zi >= 0) && (zi <= (niz - 1))) {
            xa = (int) Math.floor(xi);
            ya = (int) Math.floor(yi);
            za = (int) Math.floor(zi);

            if (xi < (nix - 1)) {
                xb = xa + 1;
            } else {
                xb = xa;
            }

            if (yi < (niy - 1)) {
                yb = ya + 1;
            } else {
                yb = ya;
            }

            if (zi < (niz - 1)) {
                zb = za + 1;
            } else {
                zb = za;
            }

            ax = xi - xa;
            bx = xb - xi;
            ay = yi - ya;
            by = yb - yi;
            az = zi - za;
            bz = zb - zi;

            result[xr + (nrx * yr) + (nrx * nry * zr)] = (bx * by * bz * img[xa + (nix * ya) + (nix * niy * za)]) +
                                                         (ax * by * bz * img[xb + (nix * ya) + (nix * niy * za)]) +
                                                         (bx * ay * bz * img[xa + (nix * yb) + (nix * niy * za)]) +
                                                         (bx * by * az * img[xa + (nix * ya) + (nix * niy * zb)]) +
                                                         (ax * ay * bz * img[xb + (nix * yb) + (nix * niy * za)]) +
                                                         (bx * ay * az * img[xa + (nix * yb) + (nix * niy * zb)]) +
                                                         (ax * by * az * img[xb + (nix * ya) + (nix * niy * zb)]) +
                                                         (ax * ay * az * img[xb + (nix * yb) + (nix * niy * zb)]);

        } else {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = 0.0f;
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  img     DOCUMENT ME!
     * @param  xi      DOCUMENT ME!
     * @param  yi      DOCUMENT ME!
     * @param  zi      DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  xr      DOCUMENT ME!
     * @param  yr      DOCUMENT ME!
     * @param  zr      DOCUMENT ME!
     */
    private void computeWSincImage(float[] img, float xi, float yi, float zi, float[] result, int xr, int yr, int zr) {

        if ((xi >= 0) && (xi <= (nix - 1)) && (yi >= 0) && (yi <= (niy - 1)) && (zi >= 0) && (zi <= (niz - 1))) {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = WSinc.wSinc3D(xi, yi, zi);

        } else {
            result[xr + (nrx * yr) + (nrx * nry * zr)] = 0.0f;
        }

        return;
    }

    /**
     * extract a VOI from a binary mask image.
     *
     * @param   obj   DOCUMENT ME!
     * @param   mask  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private VOI generateVOI(VOI obj, BitSet mask) {
        AlgorithmVOIExtractionPaint extract;

        destImage.resetVOIs();
        extract = new AlgorithmVOIExtractionPaint(destImage, mask, nrx, nry, nrz, (short) destImage.getVOIs().size());
        extract.runAlgorithm();
        mask = null;

        VOI objVOI = destImage.getVOIs().VOIAt(0);
        objVOI.setName(obj.getName());
        objVOI.setPolarity(obj.getPolarity());
        objVOI.setColor(obj.getColor());

        return objVOI;
    }

    /**
     * computes the selected Talairach transform.
     *
     * @param  img     the original image
     * @param  result  the transformed image
     */
    private void transformTalairachVolume(float[] img, float[] result) {
        int x, y, z;
        int i;

        int mod;
        int n;

        

        // MAIN ALGORITHM             //

        // record time
        long start_time = System.currentTimeMillis();

        fireProgressStateChanged("process volume (" + transformTypeName + ")");

        // debug: UI.setGlobalDataText("\n-- "+transformType+" --\n");

        Vector3f pt = new Vector3f();
        n = 0;
        mod = (nrx * nry * nrz) / 100;

        if ((interpolation == BSPLINE3) || (interpolation == BSPLINE4)) {
            Bspline = new AlgorithmBSpline();

            if (interpolation == BSPLINE3) {
                Bspline.setup3DBSpline(img, inVolExtents, 3);
            } else {
                Bspline.setup3DBSpline(img, inVolExtents, 4);
            }
        } // if (interpolation == BSPLINE3) || (interpolation == BSPLINE4))
        else if (interpolation == CUBIC_LAGRANGIAN) {
            CLag = new AlgorithmCubicLagrangian();
            CLag.setup3DCubicLagrangian(img, inVolExtents, doClip);
        } // else if (interpolation == CUBIC_LAGRANGIAN)
        else if (interpolation == QUINTIC_LAGRANGIAN) {
            QLag = new AlgorithmQuinticLagrangian();
            QLag.setup3DQuinticLagrangian(img, inVolExtents, doClip);
        } // else if (interpolation == QUINTIC_LAGRANGIAN)
        else if (interpolation == HEPTIC_LAGRANGIAN) {
            HLag = new AlgorithmHepticLagrangian();
            HLag.setup3DHepticLagrangian(img, inVolExtents, doClip);
        } // else if (interpolation == HEPTIC_LAGRANGIAN)
        else if (interpolation == WSINC) {
            WSinc = new AlgorithmWSinc();
            WSinc.setup3DWSinc(img, inVolExtents, doClip);
        } // else if (interpolation == WSINC)

        for (x = 0; (x < nrx) && !threadStopped; x++) {

            for (y = 0; (y < nry) && !threadStopped; y++) {

                for (z = 0; (z < nrz) && !threadStopped; z++) {

                    if ((n % mod) == 0) {
                        fireProgressStateChanged(n / mod);
                    }

                    n++;

                    // according to type
                    if (transformType == ORIG_TO_ACPC) {
                        tInfo.acpcToOrig(x, y, z, pt);
                    } else if (transformType == ACPC_TO_ORIG) {
                        tInfo.origToAcpc(x, y, z, pt);
                    } else if (transformType == ACPC_TO_TLRC) {
                        tInfo.tlrcToAcpc(x, y, z, pt);
                    } else if (transformType == TLRC_TO_ACPC) {
                        tInfo.acpcToTlrc(x, y, z, pt);
                    } else if (transformType == ORIG_TO_TLRC) {
                        tInfo.tlrcToOrig(x, y, z, pt);
                    } else if (transformType == TLRC_TO_ORIG) {
                        tInfo.origToTlrc(x, y, z, pt);
                    }

                    if (interpolation == TRILINEAR) {
                        computeTrilinearImage(img, pt.X, pt.Y, pt.Z, result, x, y, z);
                    } else if (interpolation == NEAREST_NEIGHBOR) {
                        computeNearestImage(img, pt.X, pt.Y, pt.Z, result, x, y, z);
                    } else if ((interpolation == BSPLINE3) || (interpolation == BSPLINE4)) {
                        computeBSplineImage(img, pt.X, pt.Y, pt.Z, result, x, y, z);
                    } // else if ((interpolation == BSPLINE3) || (interpolation == BSPLINE4))
                    else if (interpolation == CUBIC_LAGRANGIAN) {
                        computeCubicLagrangian(img, pt.X, pt.Y, pt.Z, result, x, y, z);
                    } // else if (interpolation == CUBIC_LAGRANGIAN)
                    else if (interpolation == QUINTIC_LAGRANGIAN) {
                        computeQuinticLagrangian(img, pt.X, pt.Y, pt.Z, result, x, y, z);
                    } // else if (interpolation == QUINTIC_LAGRANGIAN)
                    else if (interpolation == HEPTIC_LAGRANGIAN) {
                        computeHepticLagrangian(img, pt.X, pt.Y, pt.Z, result, x, y, z);
                    } // else if (interpolation == HEPTIC_LAGRANGIAN)
                    else if (interpolation == WSINC) {
                        computeWSincImage(img, pt.X, pt.Y, pt.Z, result, x, y, z);
                    } // else if (interpolation == WSINC)

                }
            }
        }

        if ((interpolation == BSPLINE3) || (interpolation == BSPLINE4)) {

            if (Bspline != null) {
                Bspline.finalize();
                Bspline = null;
            }
        } // if ((interpolation == BSPLINE3) || (interpolation == BSPLINE4))
        else if (interpolation == CUBIC_LAGRANGIAN) {

            if (CLag != null) {
                CLag.finalize();
                CLag = null;
            }
        } // else if (interpolation == CUBIC_LAGRANGIAN)
        else if (interpolation == QUINTIC_LAGRANGIAN) {

            if (QLag != null) {
                QLag.finalize();
                QLag = null;
            }
        } // else if (interpolation == QUINTIC_LAGRANGIAN)
        else if (interpolation == HEPTIC_LAGRANGIAN) {

            if (HLag != null) {
                HLag.finalize();
                HLag = null;
            }
        } // else if (interpolation == HEPTIC_LAGRANGIAN)
        else if (interpolation == WSINC) {

            if (WSinc != null) {
                WSinc.finalize();
                WSinc = null;
            }
        } // else if (interpolation == WSINC)

        if (doVOI) {

            // create a short image for voi tags
            BitSet imgMask = new BitSet(img.length);
            BitSet voiMask = new BitSet(result.length);

            VOIVector voi = srcImage.getVOIs();
            VOIVector transformedVOI = new VOIVector(voi.size());
            VOI obj;
            int xi, yi, zi;

            for (i = 0; (i < voi.size()) && !threadStopped; i++) {
                obj = voi.VOIAt(i);

                if ((obj.getCurveType() == VOI.CONTOUR) || (obj.getCurveType() == VOI.POLYLINE)) {
                    fireProgressStateChanged("process VOI (" + obj.getName() + ")");
                    fireProgressStateChanged(0);

                    // create an image with voi tags
                    imgMask.clear();
                    obj.createBinaryMask3D(imgMask, nix, niy, false, false);
                    voiMask.clear();

                    // transform it
                    n = 0;

                    for (x = 0; (x < nrx) && !threadStopped; x++) {

                        for (y = 0; (y < nry) && !threadStopped; y++) {

                            for (z = 0; (z < nrz) && !threadStopped; z++) {

                                if ((n % mod) == 0) {
                                    fireProgressStateChanged(n / mod);
                                }

                                n++;

                                // according to type
                                if (transformType == ORIG_TO_ACPC) {
                                    tInfo.acpcToOrig(x, y, z, pt);
                                } else if (transformType == ACPC_TO_ORIG) {
                                    tInfo.origToAcpc(x, y, z, pt);
                                } else if (transformType == ACPC_TO_TLRC) {
                                    tInfo.tlrcToAcpc(x, y, z, pt);
                                } else if (transformType == TLRC_TO_ACPC) {
                                    tInfo.acpcToTlrc(x, y, z, pt);
                                } else if (transformType == ORIG_TO_TLRC) {
                                    tInfo.tlrcToOrig(x, y, z, pt);
                                } else if (transformType == TLRC_TO_ORIG) {
                                    tInfo.origToTlrc(x, y, z, pt);
                                }

                                xi = Math.round(pt.X);
                                yi = Math.round(pt.Y);
                                zi = Math.round(pt.Z);

                                if ((xi > 0) && (xi < (nix - 1)) && (yi > 0) && (yi < (niy - 1)) && (zi > 0) &&
                                        (zi < (niz - 1))) {

                                    if (imgMask.get(xi + (nix * yi) + (nix * niy * zi))) {
                                        voiMask.set(x + (nrx * y) + (nrx * nry * z));
                                    }
                                }
                            }
                        }
                    }

                    // create a new VOI from it in the new image
                    obj = generateVOI(obj, voiMask);
                    transformedVOI.add(obj);
                } // if ((obj.getCurveType() == VOI.CONTOUR) ||
            } // for (i = 0; i < voi.size() && !threadStopped; i++)

            srcImage.setVOIs(voi);
            destImage.setVOIs(transformedVOI);
        } // if (doVOI)

        if (threadStopped) {
            finalize();

            return;
        }
        // debug

        System.out.print("total time: (milliseconds): " + (System.currentTimeMillis() - start_time));
        
        fireProgressStateChanged("creating result image...");

        try {
            destImage.importData(0, result, true);
            destImage.setImageName(srcImage.getImageName() + suffix);
        } catch (OutOfMemoryError e) {
            result = null;
            errorCleanUp("Algorithm: Out of memory creating result", true);
            finalize();
            
            setCompleted(false);

            return;
        } catch (IOException error) {
            errorCleanUp("Algorithm: export problem to destImage", true);
            finalize();
            
            setCompleted(false);

            return;
        }

        img = null;
        result = null;
        destImage.releaseLock();
    } // transformTalairachVolume

}
