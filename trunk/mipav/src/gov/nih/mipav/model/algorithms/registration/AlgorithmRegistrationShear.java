package gov.nih.mipav.model.algorithms.registration;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Resamples imageB to grid/position of imageA This code is ported from the AFNI software package located at
 * http://afni.nimh.nih.gov AFNI is a set of C programs for processing, analyzing, and displaying functional MRI data.
 * The AFNI registration algorithm is described in the article Real-Time 3D Image Registration for Functional MRI by
 * Robert W. Cox and Andrzej Jesmanowicz in Magnetic Resonance in Medicine 42:1014-1018(1999). A 3D image is rotated and
 * shifted using a shear factorization of the rotation matrix. Combined with gradient descent (repeated linearization)
 * on a least squares objective function, 3D image alignment for small movements can be rapidly computed.
 *
 * <p>If the imageA resolutions and dimensions do not match the imageB resolutions and dimensions, an AlgorithmTransform
 * is run on imageB to produce a resampledImageB whose resolutions and dimensions match those of imageA.</p>
 *
 * <p>Dr. Cox wrote: "The earlier (pre 2001) version of 3dvolreg was only good for small motions. In late 2000, I added
 * the -twopass options, which allow the estimation of larger movements. It is that feature that makes the small motions
 * statement somewhat dated now. However, the Fourier wraparound effect is more apparent when doing large movements,
 * which is why the Lagrangian interpolation is recommended for that case. In fact, I personally always use "-heptic
 * -clipit". "-Fourier" is the default because that has been true historically, but after a while I decided it was not
 * the best choice (although it isn't bad, either)." Hence, in this code the -twopass, -heptic, and -clipit options are
 * used.</p>
 */

public class AlgorithmRegistrationShear extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int MAX_ITER = 5;

    /** DOCUMENT ME! */
    private static final float DXY_THRESH = 0.07f; /* pixels */

    /** DOCUMENT ME! */
    private static final float PHI_THRESH = 0.21f; /* degrees */

    /** DOCUMENT ME! */
    private static final int DELTA_AFTER = 1;

    /** DOCUMENT ME! */
    private static final int DELTA_BEFORE = 2;

    /** DOCUMENT ME! */
    private static final int DELTA_FIXED = 3;

    /** DOCUMENT ME! */
    private static final double BIG_NORM = 1.0e+38;

    /** DOCUMENT ME! */
    private static final double PER0 = 1.09e-6;

    /** DOCUMENT ME! */
    private static final double PER1 = 1.22e-6;

    /** DOCUMENT ME! */
    private static final double PER2 = 1.37e-6;

    /** DOCUMENT ME! */
    private static final double DFAC = Math.PI / 180.0;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmTransform algoTrans;

    /** DOCUMENT ME! */
    private int ax1 = 2; // z

    /** DOCUMENT ME! */
    private int ax2 = 0; // x

    /** DOCUMENT ME! */
    private int ax3 = 1; // y

    /** DOCUMENT ME! */
    private float[] blurredA;

    /** DOCUMENT ME! */
    private float[] blurredB;

    /** DOCUMENT ME! */
    private ModelImage blurredImageA = null;

    /** DOCUMENT ME! */
    private ModelImage blurredImageB = null;

    /** DOCUMENT ME! */
    private ModelImage blurredImageW = null;

    /** DOCUMENT ME! */
    private float bsum, sum;

    /** DOCUMENT ME! */
    private double[] chol_fitim = null;

    /** DOCUMENT ME! */
    private boolean clipit;

    /** DOCUMENT ME! */
    private boolean clipOutput;

    /** DOCUMENT ME! */
    private int coarse_del = 10; // When doing the first pass. the first step is to do

    /** DOCUMENT ME! */
    private int coarse_num = 2; // a number of coarse shifts to find a starting point

    /** DOCUMENT ME! */
    private float[] data;

    /** DOCUMENT ME! */
    private int dcode;

    /** DOCUMENT ME! */
    private float ddx;

    /** DOCUMENT ME! */
    private float ddy;

    /** DOCUMENT ME! */
    private float ddz;

    /** del is the distance in voxel size used to compute image derivatives using finite differences. */
    private float del = 0.70f; // voxels

    /** DOCUMENT ME! */
    private float delfac;

    /** DOCUMENT ME! */
    private int[] destExtents = new int[3];

    /** DOCUMENT ME! */
    private float[] dfit = new float[7];

    /** DOCUMENT ME! */
    private float dph = 0.07f; // degrees

    /** DOCUMENT ME! */
    private float dx_1;

    /** DOCUMENT ME! */
    private float dxbar = 0.0f;

    /**
     * For twopass operation the default value is 66. For 1 pass operation the default value is 9. Iterations converge
     * when maximum movement is less than dxy voxels and maximum rotation is less than dph degrees.
     */
    private float dxy = 0.05f; // voxels

    /** DOCUMENT ME! */
    private float dxy_thresh;

    /** DOCUMENT ME! */
    private float dy_1;

    /** DOCUMENT ME! */
    private float dybar = 0.0f;

    /** DOCUMENT ME! */
    private float dz_1;

    /** DOCUMENT ME! */
    private float dzbar = 0.0f;

    /** DOCUMENT ME! */
    private int edge;

    /**
     * volume A where the default weight is set to 0. for 0 set the same thickness along each base edge to a default
     * weight of 0. The thickness is given by edging for 1 set the same percentage of each dimension around the base
     * edges. The percentage is given by edging. For example if edging is 5.0, then 5% of a 256x256x124 volume means
     * that 13 voxels on each side of the xy axes will get zero weight, and 6 along the z-axis. The largest percentage
     * value allowed is 25 percent. The default value is edgperc value is 1.
     */
    private float edging = 5.0f; // The default value is 5.0.

    /** private boolean twodup = false; // reset resultImgB origin to be the same as the imageA origin. */
    private int edperc = 1; // for -1 there is no region around the edges of the base

    /** DOCUMENT ME! */
    private int ee;

    /** DOCUMENT ME! */
    private int final_regmode;

    /** DOCUMENT ME! */
    private float[] fit = new float[7];

    /** DOCUMENT ME! */
    private float[][] fitim = null;

    /** DOCUMENT ME! */
    private float[] fj0x;

    /** DOCUMENT ME! */
    private float[] fj0y;

    /** DOCUMENT ME! */
    private float[] fj0z;

    /** DOCUMENT ME! */
    private float[] fj1y;

    /** DOCUMENT ME! */
    private float[] fj1z;

    /** DOCUMENT ME! */
    private boolean force_edging;

    /** DOCUMENT ME! */
    private AlgorithmGaussianBlur gaussianBlurAlgo;

    /** DOCUMENT ME! */
    private int i;

    /** DOCUMENT ME! */
    private int ii, jj, kk;

    /** DOCUMENT ME! */
    private ModelImage imageA, imageB, resultImgB;

    /** DOCUMENT ME! */
    private ModelImage imageW = null;

    /** DOCUMENT ME! */
    private float init_dth1 = 0.0f;

    /** DOCUMENT ME! */
    private float init_dth2 = 0.0f;

    /** DOCUMENT ME! */
    private float init_dth3 = 0.0f;

    /** DOCUMENT ME! */
    private float init_dx = 0.0f;

    /** DOCUMENT ME! */
    private float init_dy = 0.0f;

    /** DOCUMENT ME! */
    private float init_dz = 0.0f;

    /** DOCUMENT ME! */
    private int interp;

    /** DOCUMENT ME! */
    private float[] lcbuf = null;

    /** DOCUMENT ME! */
    private int maxite = 66; // The maximum number of iterations allowed for convergence.

    /** DOCUMENT ME! */
    private ModelImage mim = null;

    /**
     * private boolean tshift = false; // If image B is 3D+time and has slice-dependent time-offsets, then tshift = true
     * tells the program to time shift it to the average slice time-offset prior to doing the spatial registration.
     * private int tshift_ignore = 0; // The number of time points at the beginning to ignore in time shifting.
     */
    private int minA; // the miminum dimension in image A

    /**
     * for the iterations. coarse_del is the size of these steps in voxels and coarse_num is the number of these steps
     * along each direction (+x,-x,+y,-y, +z,-z). The default values are coarse_del = 10 and coarse num = 2. If you
     * don't want this step performed, set coarse_num = 0. Note that the amount of computation grows as coarse_num**3,
     * so don't increase coarse_num past 4, or the program will run forever. The program will prevent the coarse_del
     * parameter from being larger than 10% of the smallest dimension of image B.
     */
    private int new_coarse_del; // value that replaces coarse_del when it is too large

    /** DOCUMENT ME! */
    private int nlcbuf = 0;

    /** DOCUMENT ME! */
    private boolean noreg;

    /** DOCUMENT ME! */
    private int nref = 7; // number of volSize 3d arrays in fitim

    /** DOCUMENT ME! */
    private int numsh;

    /** DOCUMENT ME! */
    private int nx, ny, nz; // image A dimensions

    /** DOCUMENT ME! */
    private int nxB, nyB, nzB; // image B dimensions

    /** DOCUMENT ME! */
    private int nxbot, nybot, nzbot;

    /** DOCUMENT ME! */
    private int nxp, nyp, nzp;

    /** DOCUMENT ME! */
    private int nxtop, nytop, nztop;

    /** DOCUMENT ME! */
    private int old_ax1 = -99, old_ax2 = -99, old_ax3 = -99, old_dcode = -99;

    /** DOCUMENT ME! */
    private double old_th1, old_th2, old_th3, old_dx, old_dy, old_dz, old_xdel, old_ydel, old_zdel;

    /** DOCUMENT ME! */
    private float phi_thresh;

    /** DOCUMENT ME! */
    private ModelImage pim = null;

    /** DOCUMENT ME! */
    private float pitch; // rotation about the x axis in degrees CCW

    /** DOCUMENT ME! */
    private float pitch_1;

    /** DOCUMENT ME! */
    private float pitchbar = 0.0f;

    /** DOCUMENT ME! */
    private int regmode;

    /** DOCUMENT ME! */
    private ModelImage resampledImageB = null;

    /** DOCUMENT ME! */
    private float rmsnew; // RMS difference between resultImgB and image A value

    /** DOCUMENT ME! */
    private float rmsold; // RMS difference between image B and image A value

    /** DOCUMENT ME! */
    private float roll; // rotation about the z axis in degrees CCW

    /** DOCUMENT ME! */
    private float roll_1;

    /** DOCUMENT ME! */
    private float rollbar = 0.0f;

    /** DOCUMENT ME! */
    private int rotpx, rotpy, rotpz;

    /** DOCUMENT ME! */
    private double[] rr = null;

    /** DOCUMENT ME! */
    private int shift;

    /** DOCUMENT ME! */
    private shear shr;

    /** DOCUMENT ME! */
    private int shtop;

    /** DOCUMENT ME! */
    private float[] sigmas = new float[3];

    /** DOCUMENT ME! */
    private int sliceSize;

    /** DOCUMENT ME! */
    private int sqtop;

    /** DOCUMENT ME! */
    private int success;

    /** best shifts for coarse first pass. */
    private int sx = 66666;

    /** DOCUMENT ME! */
    private int sy;

    /** DOCUMENT ME! */
    private int sz;

    /** DOCUMENT ME! */
    private ModelImage tim = null;

    /**
     * private boolean clip = true; // clip resultImgB to be within range of input imageB private int zpad = 0; // zero
     * pad around the edges by zpad voxels during rotations These edge values will be stripped off in the output private
     * boolean twopass = true; // Do two passes of the registration algorithm (1) with smoothed imageA and imageB, to
     * get a crude alignment (2) with the input imageA and imageB.
     */
    private float twoblur = 2.0f; // blurring factor for pass 1 of 2 pass registration


    /** DOCUMENT ME! */
    private int VL_final = -1;
    // The original software also allows CUBIC_LAGRANGIAN, QUINTIC_LAGRANGIAN, and
    // FOURIER interpolation.  However, if the data movements are not small, FOURIER
    // interpolation can result in wraparound.

    /** DOCUMENT ME! */
    private int VL_resam = AlgorithmTransform.HEPTIC_LAGRANGIAN;

    /** DOCUMENT ME! */
    private int volSize;

    /** DOCUMENT ME! */
    private ModelImage vvv = null;

    /** DOCUMENT ME! */
    private int x, y, z;

    /** DOCUMENT ME! */
    private float xdel; // x resolution

    /** DOCUMENT ME! */
    private float xdelB; // B image x resolution

    /** Edging amounts. */
    private int xedge = 0;

    /** DOCUMENT ME! */
    private int xfade, yfade, zfade;

    /** DOCUMENT ME! */
    private TransMatrix xfrm;

    /** DOCUMENT ME! */
    private float yaw; // rotation about the y axis in degrees CCW

    /** DOCUMENT ME! */
    private float yaw_1;

    /** DOCUMENT ME! */
    private float yawbar = 0.0f;

    /** DOCUMENT ME! */
    private float ydel; // y resolution

    /** DOCUMENT ME! */
    private float ydelB; // B image y resolution

    /** DOCUMENT ME! */
    private int yedge = 0;

    /** DOCUMENT ME! */
    private float zdel; // z resolution

    /** DOCUMENT ME! */
    private float zdelB; // B image z resolution

    /** DOCUMENT ME! */
    private int zedge = 0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  imageA               image A
     * @param  imageB               image B, to be registered to imageA
     * @param  resultImgB           stores transformed imageB
     * @param  interpolationMethod  HEPTIC, QUINTIC, or CUBIC Lagrangian
     */
    public AlgorithmRegistrationShear(ModelImage imageA, ModelImage imageB, ModelImage resultImgB,
                                      int interpolationMethod) {
        this.imageA = imageA;
        this.imageB = imageB;
        this.resultImgB = resultImgB;
        VL_resam = interpolationMethod;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        cleanup();
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
        fireProgressStateChanged(imageA.getFileInfo()[0].getFileName(), "Registering to base ...");


        xdel = imageA.getFileInfo(0).getResolutions()[0];
        ydel = imageA.getFileInfo(0).getResolutions()[1];
        zdel = imageA.getFileInfo(0).getResolutions()[2];

        nx = imageA.getExtents()[0];
        ny = imageA.getExtents()[1];
        nz = imageA.getExtents()[2];

        xdelB = imageB.getFileInfo(0).getResolutions()[0];
        ydelB = imageB.getFileInfo(0).getResolutions()[1];
        zdelB = imageB.getFileInfo(0).getResolutions()[2];

        nxB = imageB.getExtents()[0];
        nyB = imageB.getExtents()[1];
        nzB = imageB.getExtents()[2];

        if ((nx != nxB) || (ny != nyB) || (nz != nzB) || (xdel != xdelB) || (ydel != ydelB) || (zdel != zdelB)) {
            xfrm = new TransMatrix(4);
            xfrm.Mult(imageB.getMatrix());

            if (VL_resam == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                interp = AlgorithmTransform.HEPTIC_LAGRANGIAN;
            } else if (VL_resam == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                interp = AlgorithmTransform.QUINTIC_LAGRANGIAN;
            } else {
                interp = AlgorithmTransform.CUBIC_LAGRANGIAN;
            }

            clipOutput = true;

            try {
                algoTrans = new AlgorithmTransform(imageB, xfrm, interp, xdel, ydel, zdel, nx, ny, nz, false,
                                                   clipOutput, false);
            } catch (OutOfMemoryError x) {
                cleanup();
                MipavUtil.displayError("Unable to allocate enough memory for AlgorithmTransform");

                setCompleted(false);

                return;
            }

            fireProgressStateChanged("Resampling imageB to match imageA dims and resolutions");
            algoTrans.run();
            resampledImageB = algoTrans.getTransformedImage();

            if (algoTrans != null) {
                algoTrans.disposeLocal();
            }

            algoTrans = null;

            if (resampledImageB == null) {
                cleanup();
                MipavUtil.displayError("Null resampledImageB returned by AlgorithmTransform");

                setCompleted(false);

                return;
            }
        } // if the 2 images are not equal in dimensions and resolutions

        sliceSize = nx * ny;
        volSize = sliceSize * nz;
        minA = Math.min(nx, ny);
        minA = Math.min(minA, nz);
        new_coarse_del = (int) ((0.1 * minA) + 0.499);

        if (coarse_del > new_coarse_del) {
            Preferences.debug("coarse_del of " + coarse_del + " was set to " + new_coarse_del);
            coarse_del = new_coarse_del;
        }

        switch (edperc) {

            case 0:
                xedge = (int) (Math.min(0.25 * nx, edging));
                yedge = (int) (Math.min(0.25 * ny, edging));
                zedge = (int) (Math.min(0.25 * nz, edging));
                break;

            case 1:
                xedge = (int) ((0.01 * edging * nx) + 0.5);
                yedge = (int) ((0.01 * edging * ny) + 0.5);
                zedge = (int) ((0.01 * edging * nz) + 0.5);
                break;
        }

        /* if in twopass mode, do the first pass */

        /* Make a blurred copy of base image A */

        try {
            fireProgressStateChanged("Blurring image A");
            fireProgressStateChanged(5);
            blurredImageA = (ModelImage) imageA.clone();
            sigmas[0] = twoblur;
            sigmas[1] = twoblur;
            sigmas[2] = twoblur;

            // Make algorithm
            gaussianBlurAlgo = new AlgorithmGaussianBlur(blurredImageA, imageA, sigmas, true, false);
            gaussianBlurAlgo.run();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for blurredImageA");

            setCompleted(false);

            return;
        }

        if (maxite <= 0) {
            maxite = MAX_ITER;
        }

        dxy_thresh = twoblur * dxy;

        if (dxy_thresh <= 0.0f) {
            dxy_thresh = DXY_THRESH;
        }

        phi_thresh = twoblur * dph;

        if (phi_thresh <= 0.0f) {
            phi_thresh = PHI_THRESH;
        }

        delfac = twoblur * del;
        dcode = -1;
        regmode = AlgorithmTransform.TRILINEAR;

        // verbose = 0;
        noreg = true;
        clipit = false;

        try {
            data = new float[volSize];
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmRegistrationShear: Out of memory creating data");

            setCompleted(false);

            return;
        }

        try {
            rr = new double[nref];
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmRegistrationShear: Out of memory creating rr");

            setCompleted(false);

            return;
        }

        try {
            fitim = new float[7][volSize];
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmRegistrationShear: Out of memory creating fitim");

            setCompleted(false);

            return;
        }

        try {
            chol_fitim = new double[nref * nref];
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmRegistrationShear: Out of memory creating chol_fitim");

            setCompleted(false);

            return;
        }

        // zero padding for rotation
        rotpx = 0;
        rotpy = 0;
        rotpz = 0;

        nxp = nx + (2 * rotpx);
        nyp = ny + (2 * rotpy);
        nzp = nz + (2 * rotpz);

        fj0x = new float[nxp];
        fj0y = new float[nyp];
        fj1y = new float[nyp];
        fj0z = new float[nzp];
        fj1z = new float[nzp];

        /* Computing first pass weight as sum of base and brick */

        /* Make a blurred copy of matching image B */

        try {
            fireProgressStateChanged("Blurring image B");
            fireProgressStateChanged(10);

            if (resampledImageB != null) {
                blurredImageB = (ModelImage) resampledImageB.clone();
                gaussianBlurAlgo = new AlgorithmGaussianBlur(blurredImageB, resampledImageB, sigmas, true, false);
            } else {
                blurredImageB = (ModelImage) imageB.clone();
                gaussianBlurAlgo = new AlgorithmGaussianBlur(blurredImageB, imageB, sigmas, true, false);
            }

            gaussianBlurAlgo.run();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for blurredImageB");


            return;
        }

        /* find shift of imageB which best overlaps with base image A */
        if ((coarse_del > 0) && (coarse_num > 0)) {

            // Getting best coarse shift
            fireProgressStateChanged("Getting best coarse shift");
            fireProgressStateChanged(15);
            sx = 0;
            sy = 0;
            sz = 0;
            shift = coarse_del;
            numsh = coarse_num;
            shtop = shift * numsh;
            edge = shtop + shift;
            sqtop = shtop * shtop;
            bsum = 0.0f;

            try {
                blurredA = new float[volSize];
                blurredImageA.exportData(0, volSize, blurredA); // locks and releases and lock
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmRegistrationShear: blurredImageA is locked");

                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                cleanup();
                displayError("AlgorithmRegistrationShear: Out of memory creating blurredA");

                setCompleted(false);

                return;
            }

            try {
                blurredB = new float[volSize];
                blurredImageB.exportData(0, volSize, blurredB); // locks and releases and lock
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmRegistrationShear: blurredImageB is locked");

                setCompleted(false);

                return;
            } catch (OutOfMemoryError e) {
                cleanup();
                displayError("AlgorithmRegistrationShear: Out of memory creating blurredB");

                setCompleted(false);

                return;
            }

            blurredImageB.disposeLocal(); // Clean up memory of blurred image B
            blurredImageB = null;

            for (x = 0; x < volSize; x++) {
                bsum += blurredA[x] * blurredA[x];
            }

            for (z = -shtop; z <= shtop; z += shift) {

                for (y = -shtop; y <= shtop; y += shift) {

                    for (x = -shtop; x <= shtop; x += shift) {

                        if (((x * x) + (y * y) + (z * z)) > sqtop) {
                            continue;
                        }

                        sum = voldif(x, y, z, edge);

                        if (sum < bsum) {
                            bsum = sum;
                            sx = x;
                            sy = y;
                            sz = z;
                        }
                    }
                }
            } // for (z = -shtop; z <= shtop; z+=shift)
        } // if ((coarse_del > 0) && (coarse_num > 0))
        else {
            sx = 0;
            sy = 0;
            sz = 0;
        }

        /* Add the shifted blurredImageB to the blurredImageA */
        ee = Math.abs(sx);
        nxbot = ee;
        nxtop = nx - ee;
        ee = Math.abs(sy);
        nybot = ee;
        nytop = ny - ee;
        ee = Math.abs(sz);
        nzbot = ee;
        nztop = nz - ee;

        for (kk = nzbot; kk < nztop; kk++) {

            for (jj = nybot; jj < nytop; jj++) {

                for (ii = nxbot; ii < nxtop; ii++) {
                    blurredA[ii + (jj * nx) + (kk * sliceSize)] += blurredB[(ii - sx) + ((jj - sy) * nx) +
                                                                            ((kk - sz) * sliceSize)];
                }
            }
        }

        blurredB = null;

        /* blur the sum to get the weight brick */

        /* blurring the first pass weight */

        fireProgressStateChanged("Getting the first pass weight");
        fireProgressStateChanged(20);

        try {
            blurredImageW = (ModelImage) imageA.clone();
            blurredImageW.importData(0, blurredA, true);
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: IOException on blurredImageW import data");

            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            cleanup();
            displayError("AlgorithmRegistrationShear: Out of memory on blurredImageW import data");

            setCompleted(false);

            return;
        }

        blurredA = null;

        gaussianBlurAlgo = new AlgorithmGaussianBlur(null, blurredImageW, sigmas, true, false);
        gaussianBlurAlgo.run();

        // Now blurredImageW = blur(blur(imageA) + shift(blur(imageB)))
        force_edging = true;

        // Calculate fitim an array of 7 volSize images and then calculate
        // chol_fitim from fitim
        fireProgressStateChanged("Creating first alignment structure");
        fireProgressStateChanged(30);
        success = mri_3dalign_setup(blurredImageA, blurredImageW);

        if (success == -1) {
            cleanup();
            MipavUtil.displayError("Failed on mri_3dalign_setup(blurredImageA, blurredImageW)");


            return;
        }

        force_edging = false;
        blurredImageW.disposeLocal();
        blurredImageW = null;

        /* do alignment on blurred copy
         * save parameters for later feed into pass #2 */
        try {
            sigmas[0] = twoblur;
            sigmas[1] = twoblur;
            sigmas[2] = twoblur;

            if (resampledImageB != null) {
                blurredImageB = (ModelImage) resampledImageB.clone();
                gaussianBlurAlgo = new AlgorithmGaussianBlur(blurredImageB, resampledImageB, sigmas, true, false);
            } else {
                blurredImageB = (ModelImage) imageB.clone();
                gaussianBlurAlgo = new AlgorithmGaussianBlur(blurredImageB, imageB, sigmas, true, false);
            }

            gaussianBlurAlgo.run();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for blurredImageB");


            return;
        }

        // degrees
        init_dth1 = 0.0f;
        init_dth2 = 0.0f;
        init_dth3 = 0.0f;

        // mm
        init_dx = sx * xdel;
        init_dy = sy * ydel;
        init_dz = sz * zdel;

        fireProgressStateChanged("Calculating first alignment parameters");
        fireProgressStateChanged(40);
        mri_3dalign_one(blurredImageB);
        blurredImageB.disposeLocal();
        blurredImageB = null;

        blurredImageA.disposeLocal();
        blurredImageA = null;
        // first pass complete

        // prepare for final alignment
        if (maxite <= 0) {
            maxite = MAX_ITER;
        }

        dxy_thresh = dxy;

        if (dxy_thresh <= 0.0f) {
            dxy_thresh = DXY_THRESH;
        }

        phi_thresh = dph;

        if (phi_thresh <= 0.0f) {
            phi_thresh = PHI_THRESH;
        }

        delfac = del;
        dcode = -1;
        regmode = VL_resam;

        // verbose = 0;
        noreg = false;
        clipit = true;

        if (VL_final < 0) {
            VL_final = VL_resam;
        }

        final_regmode = VL_final;
        fireProgressStateChanged("Creating final alignment structure");
        fireProgressStateChanged(60);
        success = mri_3dalign_setup(imageA, null);

        if (success == -1) {
            cleanup();
            MipavUtil.displayError("Failed on mri_3dalign_setup(imageA,null)");


            return;
        }

        // degrees
        init_dth1 = roll_1;
        init_dth2 = pitch_1;
        init_dth3 = yaw_1;

        // mm
        init_dx = dx_1;
        init_dy = dy_1;
        init_dz = dz_1;

        fireProgressStateChanged("Calculating final alignment parameters");
        fireProgressStateChanged(80);

        if (resampledImageB != null) {
            mri_3dalign_one_final(resampledImageB);
        } else {
            mri_3dalign_one_final(imageB);
        }

        try {
            resultImgB.importData(0, data, true);
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: IOException on resultImgB import data");

            setCompleted(false);

            return;
        }

        fireProgressStateChanged(100);
        cleanup();

        setCompleted(true);
    }

    /**
     * Apply a set of shears to a 3D array of floats. Note that we assume that the dilation factors ("f") are all 1.
     *
     * @param  shr  the shear factors to be applied
     * @param  vol  the ModelImage to which the shear is applied
     */

    void apply_3shear(shear shr, ModelImage vol) {
        int qq;
        float a, b, s;
        int[] ax;
        int flip0, flip1;
        double[][] scl;
        double[] sft;
        AlgorithmFlip flipAlgo;

        ax = shr.ax;

        if (ax[0] < 0) {
            return;
        }

        /* carry out a preliminary 180 flippo ? */

        flip0 = shr.flip0;
        flip1 = shr.flip1;
        scl = shr.scl;
        sft = shr.sft;

        if (flip0 >= 0) {

            switch (flip0 + flip1) {

                case 1:
                    flipAlgo = new AlgorithmFlip(vol, AlgorithmFlip.Z_AXIS, AlgorithmFlip.IMAGE);
                    flipAlgo.run();
                    break;

                case 2:
                    flipAlgo = new AlgorithmFlip(vol, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE);
                    flipAlgo.run();
                    break;

                case 3:
                    flipAlgo = new AlgorithmFlip(vol, AlgorithmFlip.X_AXIS, AlgorithmFlip.IMAGE);
                    flipAlgo.run();
                    break;

                default:
                    return; /* should not occur */
            }
        }

        /* apply each shear */

        try {
            vol.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: vol is locked");

            setCompleted(false);

            return;
        }

        for (qq = 0; qq < 4; qq++) {

            switch (ax[qq]) {

                case 0:
                    a = (float) scl[qq][1];
                    b = (float) scl[qq][2];
                    s = (float) sft[qq];
                    apply_xshear(a, b, s, data);
                    break;

                case 1:
                    a = (float) scl[qq][0];
                    b = (float) scl[qq][2];
                    s = (float) sft[qq];
                    apply_yshear(a, b, s, data);
                    break;

                case 2:
                    a = (float) scl[qq][0];
                    b = (float) scl[qq][1];
                    s = (float) sft[qq];
                    apply_zshear(a, b, s, data);
                    break;
            }
        }

        if ((regmode != AlgorithmTransform.TRILINEAR) &&
                ((vol.getType() == ModelStorageBase.UBYTE) || (vol.getType() == ModelStorageBase.USHORT) ||
                     (vol.getType() == ModelStorageBase.UINTEGER))) {

            for (qq = 0; qq < volSize; qq++) {

                if (data[qq] < 0) {
                    data[qq] = 0;
                }
            }
        }

        try {
            vol.importData(0, data, true);
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: IOException on vol import data");

            setCompleted(false);

            return;
        }

        for (qq = 0; qq < 4; qq++) {
            scl[qq] = null;
        }

        scl = null;
        sft = null;
        ax = null;

        return;
    }

    /**
     * Apply an x-axis shear to a 3D array: x -> x + a*y + b*z + s (dilation factor "f" assumed to be 1.0).
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  s  DOCUMENT ME!
     * @param  v  3d array to which shear is applied
     */

    void apply_xshear(float a, float b, float s, float[] v) {
        float[] fj1 = new float[nxp];
        int ny1 = nyp - 1, nz1 = nzp - 1, nxy = nxp * nyp;
        float ny2 = 0.5f * ny1, nz2 = 0.5f * nz1;
        int ii, jj, kk;
        int nup = 0;
        float a0, a1, st;

        /* don't do anything if shift is less than 0.001 pixel */

        st = (Math.abs(a) * ny2) + (Math.abs(b) * nz2) + Math.abs(s);

        if (st < 1.e-3) {
            return;
        }

        /*if( shift_method == MRI_FOURIER ){
         * nst = nx + 0.5*st ; nup = csfft_nextup_one35(nst) ; }*/

        for (kk = 0; kk < nzp; kk++) {

            for (jj = 0; jj < nyp; jj += 2) {

                for (ii = 0; ii < nxp; ii++) {
                    fj0x[ii] = v[ii + (jj * nxp) + (kk * nxy)];
                }

                if (jj < ny1) {

                    for (ii = 0; ii < nxp; ii++) {
                        fj1[ii] = v[ii + nxp + (jj * nxp) + (kk * nxy)];
                    }
                } else {
                    fj1 = null;
                }

                a0 = (a * (jj - ny2)) + (b * (kk - nz2)) + s;
                a1 = a0 + a;

                if (regmode == AlgorithmTransform.TRILINEAR) {
                    lin_shift2(nxp, nup, a0, fj0x, a1, fj1);
                } else if (regmode == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    cub_shift2(nxp, nup, a0, fj0x, a1, fj1);
                } else if (regmode == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    quint_shift2(nxp, nup, a0, fj0x, a1, fj1);
                } else if (regmode == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    hept_shift2(nxp, nup, a0, fj0x, a1, fj1);
                }

                for (ii = 0; ii < nxp; ii++) {
                    v[ii + (jj * nxp) + (kk * nxy)] = fj0x[ii];

                    if (fj1 != null) {
                        v[ii + nxp + (jj * nxp) + (kk * nxy)] = fj1[ii];
                    }
                }
            }
        }

        fj1 = null;

        return;
    }

    /**
     * Apply a y-axis shear to a 3D array: y -> y + a*x + b*z + s.
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  s  DOCUMENT ME!
     * @param  v  3d array to which shear is applied
     */

    void apply_yshear(float a, float b, float s, float[] v) {
        int nx1 = nxp - 1, nz1 = nzp - 1, nxy = nxp * nyp;
        float nx2 = 0.5f * nx1, nz2 = 0.5f * nz1;
        int ii, jj, kk;
        int nup = 0;
        float a0, st;
        float a1 = 0.0f;

        /* don't do anything if shift is less than 0.001 pixel */

        st = (Math.abs(a) * nx2) + (Math.abs(b) * nz2) + Math.abs(s);

        if (st < 1.e-3) {
            return;
        }

        /*if( shift_method == MRI_FOURIER ){
         * nst = ny + 0.5*st ; nup = csfft_nextup_one35(nst) ; }*/

        for (kk = 0; kk < nzp; kk++) {

            for (ii = 0; ii < nx1; ii += 2) {

                for (jj = 0; jj < nyp; jj++) {
                    fj0y[jj] = v[ii + (jj * nxp) + (kk * nxy)];
                    fj1y[jj] = v[ii + 1 + (jj * nxp) + (kk * nxy)];
                }

                a0 = (a * (ii - nx2)) + (b * (kk - nz2)) + s;
                a1 = a0 + a;

                if (regmode == AlgorithmTransform.TRILINEAR) {
                    lin_shift2(nyp, nup, a0, fj0y, a1, fj1y);
                } else if (regmode == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    cub_shift2(nyp, nup, a0, fj0y, a1, fj1y);
                } else if (regmode == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    quint_shift2(nyp, nup, a0, fj0y, a1, fj1y);
                } else if (regmode == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    hept_shift2(nyp, nup, a0, fj0y, a1, fj1y);
                }

                for (jj = 0; jj < nyp; jj++) {
                    v[ii + (jj * nxp) + (kk * nxy)] = fj0y[jj];
                    v[ii + 1 + (jj * nxp) + (kk * nxy)] = fj1y[jj];
                }
            }

            if (ii == nx1) { /* allow for odd nx */

                for (jj = 0; jj < nyp; jj++) {
                    fj0y[jj] = v[ii + (jj * nxp) + (kk * nxy)];
                }

                a0 = (a * (ii - nx2)) + (b * (kk - nz2)) + s;

                if (regmode == AlgorithmTransform.TRILINEAR) {
                    lin_shift2(nyp, nup, a0, fj0y, a1, null);
                } else if (regmode == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    cub_shift2(nyp, nup, a0, fj0y, a1, null);
                } else if (regmode == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    quint_shift2(nyp, nup, a0, fj0y, a1, null);
                } else if (regmode == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    hept_shift2(nyp, nup, a0, fj0y, a1, null);
                }

                for (jj = 0; jj < nyp; jj++) {
                    v[ii + (jj * nxp) + (kk * nxy)] = fj0y[jj];
                }
            }
        }

        return;
    }

    /**
     * Apply a z-axis shear to a 3D array: z -> z + a*x + b*y + s.
     *
     * @param  a  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     * @param  s  DOCUMENT ME!
     * @param  v  3d array to which shear is applied
     */

    void apply_zshear(float a, float b, float s, float[] v) {
        int nx1 = nxp - 1, ny1 = nyp - 1, nxy = nxp * nyp;
        float nx2 = 0.5f * nx1, ny2 = 0.5f * ny1;
        int ii, jj, kk;
        int nup = 0;
        float a0, st;
        float a1 = 0.0f;

        /* don't do anything if shift is less than 0.001 pixel */

        st = (Math.abs(a) * nx2) + (Math.abs(b) * ny2) + Math.abs(s);

        if (st < 1.e-3) {
            return;
        }

        /*if( shift_method == MRI_FOURIER ){
         * nst = nz + 0.5*st ; nup = csfft_nextup_one35(nst) ; }*/

        for (jj = 0; jj < nyp; jj++) {

            for (ii = 0; ii < nx1; ii += 2) {

                for (kk = 0; kk < nzp; kk++) {
                    fj0z[kk] = v[ii + (jj * nxp) + (kk * nxy)];
                    fj1z[kk] = v[ii + 1 + (jj * nxp) + (kk * nxy)];
                }

                a0 = (a * (ii - nx2)) + (b * (jj - ny2)) + s;
                a1 = a0 + a;

                if (regmode == AlgorithmTransform.TRILINEAR) {
                    lin_shift2(nzp, nup, a0, fj0z, a1, fj1z);
                } else if (regmode == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    cub_shift2(nzp, nup, a0, fj0z, a1, fj1z);
                } else if (regmode == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    quint_shift2(nzp, nup, a0, fj0z, a1, fj1z);
                } else if (regmode == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    hept_shift2(nzp, nup, a0, fj0z, a1, fj1z);
                }

                for (kk = 0; kk < nzp; kk++) {
                    v[ii + (jj * nxp) + (kk * nxy)] = fj0z[kk];
                    v[ii + 1 + (jj * nxp) + (kk * nxy)] = fj1z[kk];
                }
            }

            if (ii == nx1) { /* allow for odd nx */

                for (kk = 0; kk < nzp; kk++) {
                    fj0z[kk] = v[ii + (jj * nxp) + (kk * nxy)];
                }

                a0 = (a * (ii - nx2)) + (b * (jj - ny2)) + s;

                if (regmode == AlgorithmTransform.TRILINEAR) {
                    lin_shift2(nzp, nup, a0, fj0z, a1, null);
                } else if (regmode == AlgorithmTransform.CUBIC_LAGRANGIAN) {
                    cub_shift2(nzp, nup, a0, fj0z, a1, null);
                } else if (regmode == AlgorithmTransform.QUINTIC_LAGRANGIAN) {
                    quint_shift2(nzp, nup, a0, fj0z, a1, null);
                } else if (regmode == AlgorithmTransform.HEPTIC_LAGRANGIAN) {
                    hept_shift2(nzp, nup, a0, fj0z, a1, null);
                }

                for (kk = 0; kk < nzp; kk++) {
                    v[ii + (jj * nxp) + (kk * nxy)] = fj0z[kk];
                }
            }
        }

        return;
    }

    /**
     * Shift 1 row with with cubic Lagrange interpolation.
     *
     * @param  n   length of row to be shifted
     * @param  af  DOCUMENT ME!
     * @param  f   row to be shifted
     */

    void cub_shift(int n, float af, float[] f) {
        int ii, ia, ix;
        float wt_m1, wt_00, wt_p1, wt_p2, aa;

        // #ifdef SEPARATE_FINS
        int ibot, itop;
        float finsm1, fins, fins1, fins2;
        // #endif

        af = -af;
        ia = (int) af;

        if (af < 0) {
            ia--;
        } /* ia = floor */

        /* 15 Mar 2001: if shift is too large, return all zeros */

        if ((ia <= -n) || (ia >= n)) {

            for (ii = 0; ii < n; ii++) {
                f[ii] = 0.0f;
            }

            return;
        }

        aa = af - ia;
        wt_m1 = ((aa) * (1.0f - (aa)) * ((aa) - 2.0f) * 0.1666667f);
        wt_00 = (((aa) + 1.0f) * ((aa) - 1.0f) * ((aa) - 2.0f) * 0.5f);
        wt_p1 = ((aa) * ((aa) + 1.0f) * (2.0f - (aa)) * 0.5f);
        wt_p2 = ((aa) * ((aa) + 1.0f) * ((aa) - 1.0f) * 0.1666667f);

        if (n > nlcbuf) {

            if (lcbuf != null) {
                lcbuf = null;
            }

            lcbuf = new float[n];
            nlcbuf = n;
        }

        // #ifdef SEPARATE_FINS
        ibot = 1 - ia;

        if (ibot < 0) {
            ibot = 0;
        }

        itop = n - 3 - ia;

        if (itop > (n - 1)) {
            itop = n - 1;
        }

        for (ii = ibot; ii <= itop; ii++) {
            ix = ii + ia;
            lcbuf[ii] = (wt_m1 * f[ix - 1]) + (wt_00 * f[ix]) + (wt_p1 * f[ix + 1]) + (wt_p2 * f[ix + 2]);
        }

        if (ibot > n) {
            ibot = n;
        } /* 15 Mar 2001 */

        for (ii = 0; ii < ibot; ii++) {
            ix = ii + ia;

            if (((ix - 1) < 0) || ((ix - 1) >= n)) {
                finsm1 = 0.0f;
            } else {
                finsm1 = f[ix - 1];
            }

            if ((ix < 0) || (ix >= n)) {
                fins = 0.0f;
            } else {
                fins = f[ix];
            }

            if (((ix + 1) < 0) || ((ix + 1) >= n)) {
                fins1 = 0.0f;
            } else {
                fins1 = f[ix + 1];
            }

            if (((ix + 2) < 0) || ((ix + 2) >= n)) {
                fins2 = 0.0f;
            } else {
                fins2 = f[ix + 2];
            }

            lcbuf[ii] = (wt_m1 * finsm1) + (wt_00 * fins) + (wt_p1 * fins1) + (wt_p2 * fins2);
        }

        if (itop < 0) {
            itop = -1;
        } /* 15 Mar 2001 */

        for (ii = itop + 1; ii < n; ii++) {
            ix = ii + ia;

            if (((ix - 1) < 0) || ((ix - 1) >= n)) {
                finsm1 = 0.0f;
            } else {
                finsm1 = f[ix - 1];
            }

            if ((ix < 0) || (ix >= n)) {
                fins = 0.0f;
            } else {
                fins = f[ix];
            }

            if (((ix + 1) < 0) || ((ix + 1) >= n)) {
                fins1 = 0.0f;
            } else {
                fins1 = f[ix + 1];
            }

            if (((ix + 2) < 0) || ((ix + 2) >= n)) {
                fins2 = 0.0f;
            } else {
                fins2 = f[ix + 2];
            }

            lcbuf[ii] = (wt_m1 * finsm1) + (wt_00 * fins) + (wt_p1 * fins1) + (wt_p2 * fins2);
        }
        // #else /* not SEPARATE_FINS */
        /*for( ii=0 ; ii < n ; ii++ ){
         * ix = ii + ia ; if( ix > 0 && ix < n-2 ) lcbuf[ii] =  wt_m1 * f[ix-1] + wt_00 * f[ix] + wt_p1 * f[ix+1] +
         * wt_p2 * f[ix+2] ; else lcbuf[ii] =  wt_m1 * FINS(ix-1) + wt_00 * FINS(ix) + wt_p1 * FINS(ix+1) + wt_p2 *
         * FINS(ix+2) ; } #endif*/
        /* SEPARATE_FINS */

        for (ii = 0; ii < n; ii++) {
            f[ii] = lcbuf[ii];
        }

        return;
    }

    /**
     * Shift 2 rows with cubic Lagrangian interpolation.
     *
     * @param  n    length of rows to be shifted
     * @param  nup  DOCUMENT ME!
     * @param  af   DOCUMENT ME!
     * @param  f    first row to be shifted
     * @param  ag   DOCUMENT ME!
     * @param  g    second row to be shifted
     */
    void cub_shift2(int n, int nup, float af, float[] f, float ag, float[] g) {
        cub_shift(n, af, f);

        if (g != null) {
            cub_shift(n, ag, g);
        }

        return;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  im  im calculate initial estimate of alignment parameters
     */

    void delayed_lsqfit_dfit(ModelImage im) {
        int ii, jj;
        double sum;

        try {
            im.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: im is locked");

            setCompleted(false);

            return;
        }

        if ((nref < 1) || (volSize < nref) || (im == null) || (fitim == null) || (chol_fitim == null)) {
            return;
        }

        /*** form RHS vector into rr ***/

        for (ii = 0; ii < nref; ii++) {
            sum = 0.0;

            for (jj = 0; jj < volSize; jj++) {
                sum += fitim[ii][jj] * data[jj];
            }

            rr[ii] = sum;
        }

        /*** forward solve ***/

        for (ii = 0; ii < nref; ii++) {
            sum = rr[ii];

            for (jj = 0; jj < ii; jj++) {
                sum -= chol_fitim[ii + (jj * nref)] * rr[jj];
            }

            rr[ii] = sum / chol_fitim[ii + (ii * nref)];
        }

        /*** backward solve ***/

        for (ii = nref - 1; ii >= 0; ii--) {
            sum = rr[ii];

            for (jj = ii + 1; jj < nref; jj++) {
                sum -= chol_fitim[jj + (ii * nref)] * rr[jj];
            }

            rr[ii] = sum / chol_fitim[ii + (ii * nref)];
        }

        /*** put result into dfit ***/

        for (ii = 0; ii < nref; ii++) {
            dfit[ii] = (float) rr[ii];
        }

        /** exit ***/
    }

    /**
     * DOCUMENT ME!
     *
     * @param  im  im calculate initial estimate of alignment parameters
     */

    void delayed_lsqfit_fit(ModelImage im) {
        int ii, jj;
        double sum;

        try {
            im.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: im is locked");

            setCompleted(false);

            return;
        }

        if ((nref < 1) || (volSize < nref) || (im == null) || (fitim == null) || (chol_fitim == null)) {
            return;
        }

        /*** form RHS vector into rr ***/

        for (ii = 0; ii < nref; ii++) {
            sum = 0.0;

            for (jj = 0; jj < volSize; jj++) {
                sum += fitim[ii][jj] * data[jj];
            }

            rr[ii] = sum;
        }

        /*** forward solve ***/

        for (ii = 0; ii < nref; ii++) {
            sum = rr[ii];

            for (jj = 0; jj < ii; jj++) {
                sum -= chol_fitim[ii + (jj * nref)] * rr[jj];
            }

            rr[ii] = sum / chol_fitim[ii + (ii * nref)];
        }

        /*** backward solve ***/

        for (ii = nref - 1; ii >= 0; ii--) {
            sum = rr[ii];

            for (jj = ii + 1; jj < nref; jj++) {
                sum -= chol_fitim[jj + (ii * nref)] * rr[jj];
            }

            rr[ii] = sum / chol_fitim[ii + (ii * nref)];
        }

        /*** put result into fit ***/

        for (ii = 0; ii < nref; ii++) {
            fit[ii] = (float) rr[ii];
        }

        /** exit ***/
    }

    /**
     * Shift one row with heptic Lagrange polynomial interpolation [Nov 1998]. Note that heptic interpolation is about
     * the same as Hamming-weighted 3-sidelobe sinc interpolation.
     *
     * @param  n   length of row to be shifted
     * @param  af  DOCUMENT ME!
     * @param  f   row to be shifted
     */

    /* seventh order polynomials */

    void hept_shift(int n, float af, float[] f) {
        int ii, ia, ix;
        float wt_m1, wt_00, wt_p1, wt_p2, aa, wt_m2, wt_p3, wt_m3, wt_p4;

        // #ifdef SEPARATE_FINS
        int ibot, itop;
        float finsm3, finsm2, finsm1, fins, fins1, fins2, fins3, fins4;
        // #endif

        af = -af;
        ia = (int) af;

        if (af < 0) {
            ia--;
        } /* ia = floor */

        /* 15 Mar 2001: if shift is too large, return all zeros */

        if ((ia <= -n) || (ia >= n)) {

            for (ii = 0; ii < n; ii++) {
                f[ii] = 0.0f;
            }

            return;
        }

        aa = af - ia;
        wt_m3 = (aa * ((aa * aa) - 1.0f) * ((aa * aa) - 4.0f) * (aa - 3.0f) * (4.0f - aa) * 0.0001984126984f);
        wt_m2 = (aa * ((aa * aa) - 1.0f) * (aa - 2.0f) * ((aa * aa) - 9.0f) * (aa - 4.0f) * 0.001388888889f);
        wt_m1 = (aa * (aa - 1.0f) * ((aa * aa) - 4.0f) * ((aa * aa) - 9.0f) * (4.0f - aa) * 0.004166666667f);
        wt_00 = (((aa * aa) - 1.0f) * ((aa * aa) - 4.0f) * ((aa * aa) - 9.0f) * (aa - 4.0f) * 0.006944444444f);
        wt_p1 = (aa * (aa + 1.0f) * ((aa * aa) - 4.0f) * ((aa * aa) - 9.0f) * (4.0f - aa) * 0.006944444444f);
        wt_p2 = (aa * ((aa * aa) - 1.0f) * (aa + 2.0f) * ((aa * aa) - 9.0f) * (aa - 4.0f) * 0.004166666667f);
        wt_p3 = (aa * ((aa * aa) - 1.0f) * ((aa * aa) - 4.0f) * (aa + 3.0f) * (4.0f - aa) * 0.001388888889f);
        wt_p4 = (aa * ((aa * aa) - 1.0f) * ((aa * aa) - 4.0f) * ((aa * aa) - 9.0f) * 0.0001984126984f);

        if (n > nlcbuf) {

            if (lcbuf != null) {
                lcbuf = null;
            }

            lcbuf = new float[n];
            nlcbuf = n;
        }

        // #ifdef SEPARATE_FINS
        ibot = 3 - ia;

        if (ibot < 0) {
            ibot = 0;
        }

        itop = n - 5 - ia;

        if (itop > (n - 1)) {
            itop = n - 1;
        }

        for (ii = ibot; ii <= itop; ii++) {
            ix = ii + ia;
            lcbuf[ii] = (wt_m2 * f[ix - 2]) + (wt_m1 * f[ix - 1]) + (wt_00 * f[ix]) + (wt_p1 * f[ix + 1]) +
                        (wt_p2 * f[ix + 2]) + (wt_p3 * f[ix + 3]) + (wt_m3 * f[ix - 3]) + (wt_p4 * f[ix + 4]);
        }

        if (ibot > n) {
            ibot = n;
        } /* 15 Mar 2001 */

        for (ii = 0; ii < ibot; ii++) {
            ix = ii + ia;

            if (((ix - 3) < 0) || ((ix - 3) >= n)) {
                finsm3 = 0.0f;
            } else {
                finsm3 = f[ix - 3];
            }

            if (((ix - 2) < 0) || ((ix - 2) >= n)) {
                finsm2 = 0.0f;
            } else {
                finsm2 = f[ix - 2];
            }

            if (((ix - 1) < 0) || ((ix - 1) >= n)) {
                finsm1 = 0.0f;
            } else {
                finsm1 = f[ix - 1];
            }

            if ((ix < 0) || (ix >= n)) {
                fins = 0.0f;
            } else {
                fins = f[ix];
            }

            if (((ix + 1) < 0) || ((ix + 1) >= n)) {
                fins1 = 0.0f;
            } else {
                fins1 = f[ix + 1];
            }

            if (((ix + 2) < 0) || ((ix + 2) >= n)) {
                fins2 = 0.0f;
            } else {
                fins2 = f[ix + 2];
            }

            if (((ix + 3) < 0) || ((ix + 3) >= n)) {
                fins3 = 0.0f;
            } else {
                fins3 = f[ix + 3];
            }

            if (((ix + 4) < 0) || ((ix + 4) >= n)) {
                fins4 = 0.0f;
            } else {
                fins4 = f[ix + 4];
            }

            lcbuf[ii] = (wt_m2 * finsm2) + (wt_m1 * finsm1) + (wt_00 * fins) + (wt_p1 * fins1) + (wt_p2 * fins2) +
                        (wt_p3 * fins3) + (wt_m3 * finsm3) + (wt_p4 * fins4);
        }

        if (itop < 0) {
            itop = -1;
        } /* 15 Mar 2001 */

        for (ii = itop + 1; ii < n; ii++) {
            ix = ii + ia;

            if (((ix - 3) < 0) || ((ix - 3) >= n)) {
                finsm3 = 0.0f;
            } else {
                finsm3 = f[ix - 3];
            }

            if (((ix - 2) < 0) || ((ix - 2) >= n)) {
                finsm2 = 0.0f;
            } else {
                finsm2 = f[ix - 2];
            }

            if (((ix - 1) < 0) || ((ix - 1) >= n)) {
                finsm1 = 0.0f;
            } else {
                finsm1 = f[ix - 1];
            }

            if ((ix < 0) || (ix >= n)) {
                fins = 0.0f;
            } else {
                fins = f[ix];
            }

            if (((ix + 1) < 0) || ((ix + 1) >= n)) {
                fins1 = 0.0f;
            } else {
                fins1 = f[ix + 1];
            }

            if (((ix + 2) < 0) || ((ix + 2) >= n)) {
                fins2 = 0.0f;
            } else {
                fins2 = f[ix + 2];
            }

            if (((ix + 3) < 0) || ((ix + 3) >= n)) {
                fins3 = 0.0f;
            } else {
                fins3 = f[ix + 3];
            }

            if (((ix + 4) < 0) || ((ix + 4) >= n)) {
                fins4 = 0.0f;
            } else {
                fins4 = f[ix + 4];
            }

            lcbuf[ii] = (wt_m2 * finsm2) + (wt_m1 * finsm1) + (wt_00 * fins) + (wt_p1 * fins1) + (wt_p2 * fins2) +
                        (wt_p3 * fins3) + (wt_m3 * finsm3) + (wt_p4 * fins4);
        }
        // #else /* not SEPARATE_FINS */
        /*for( ii=0 ; ii < n ; ii++ ){
         * ix = ii + ia ; if( ix > 1 && ix < n-3 ) lcbuf[ii] =  wt_m2 * f[ix-2] + wt_m1 * f[ix-1] + wt_00 * f[ix] +
         * wt_p1 * f[ix+1] + wt_p2 * f[ix+2] + wt_p3 * f[ix+3] + wt_m3 * f[ix-3] + wt_p4 * f[ix+4] ; else lcbuf[ii] =
         * wt_m2 * FINS(ix-2) + wt_m1 * FINS(ix-1) + wt_00 * FINS(ix) + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) + wt_p3
         * FINS(ix+3) + wt_m3 * FINS(ix-3) + wt_p4 * FINS(ix+4) ; } #endif */
        /* SEPARATE_FINS */

        for (ii = 0; ii < n; ii++) {
            f[ii] = lcbuf[ii];
        }

        return;
    }

    /**
     * Shift 2 rows with heptic Lagrangian interpolation.
     *
     * @param  n    length of rows to be shifted
     * @param  nup  DOCUMENT ME!
     * @param  af   DOCUMENT ME!
     * @param  f    first row to be shifted
     * @param  ag   DOCUMENT ME!
     * @param  g    second row to be shifted
     */
    void hept_shift2(int n, int nup, float af, float[] f, float ag, float[] g) {
        hept_shift(n, af, f);

        if (g != null) {
            hept_shift(n, ag, g);
        }

        return;
    }

    /**
     * Shift one row with linear interpolation.
     *
     * @param  n   length of row to be shifted
     * @param  af  DOCUMENT ME!
     * @param  f   row to be shifted
     */

    void lin_shift(int n, float af, float[] f) {
        // #define ZFILL
        // #ifdef ZFILL
        // #  define FINS(i) ( ((i)<0 || (i)>=n) ? 0.0 : f[(i)] )
        // #else
        // #  define FINS(i) ( ((i)<0) ? f[0] : ((i)>=n) ? f[n-1] : f[(i)] )
        // #endif

        // #define SEPARATE_FINS
        int ii, ia, ix;
        float wt_00, wt_p1, aa;
        float fins, fins1;

        // #ifdef SEPARATE_FINS
        int ibot, itop;
        // #endif

        af = -af;
        ia = (int) af;

        if (af < 0) {
            ia--;
        } /* ia = floor */

        aa = af - ia;
        wt_00 = 1.0f - aa;
        wt_p1 = aa; /* linear interpolation weights */

        /* 15 Mar 2001: if shift is too large, return all zeros */

        if ((ia <= -n) || (ia >= n)) {

            for (ii = 0; ii < n; ii++) {
                f[ii] = 0.0f;
            }

            return;
        }

        if (n > nlcbuf) {

            if (lcbuf != null) {
                lcbuf = null;
            }

            lcbuf = new float[n];
            nlcbuf = n;
        }

        // #ifdef SEPARATE_FINS
        ibot = -ia;

        if (ibot < 0) {
            ibot = 0;
        }

        itop = n - 2 - ia;

        if (itop > (n - 1)) {
            itop = n - 1;
        }

        Preferences.debug("n = " + n + " ia = " + ia + " ibot = " + ibot + " itop = " + itop + "\n");

        for (ii = ibot; ii <= itop; ii++) {
            ix = ii + ia;
            lcbuf[ii] = (wt_00 * f[ix]) + (wt_p1 * f[ix + 1]);
        }

        if (ibot > n) {
            ibot = n;
        } /* 15 Mar 2001 */

        for (ii = 0; ii < ibot; ii++) {
            ix = ii + ia;

            if ((ix < 0) || (ix >= n)) {
                fins = 0.0f;
            } else {
                fins = f[ix];
            }

            if (((ix + 1) < 0) || ((ix + 1) >= n)) {
                fins1 = 0.0f;
            } else {
                fins1 = f[ix + 1];
            }

            lcbuf[ii] = (wt_00 * fins) + (wt_p1 * fins1);
        }

        if (itop < 0) {
            itop = -1;
        } /* 15 Mar 2001 */

        for (ii = itop + 1; ii < n; ii++) {
            ix = ii + ia;

            if ((ix < 0) || (ix >= n)) {
                fins = 0.0f;
            } else {
                fins = f[ix];
            }

            if (((ix + 1) < 0) || ((ix + 1) >= n)) {
                fins1 = 0.0f;
            } else {
                fins1 = f[ix + 1];
            }

            lcbuf[ii] = (wt_00 * fins) + (wt_p1 * fins1);
        }

        /*#else
         * for( ii=0 ; ii < n ; ii++ ){ ix = ii + ia ; if( ix >= 0 && ix < n-1 ) lcbuf[ii] =  wt_00 * f[ix] + wt_p1 *
         * f[ix+1] ; else lcbuf[ii] =  wt_00 * FINS(ix) + wt_p1 * FINS(ix+1) ; } #endif */
        /* SEPARATE_FINS */

        for (ii = 0; ii < n; ii++) {
            f[ii] = lcbuf[ii];
        }

        return;
    }

    /**
     * Shift 2 rows with linear interpolation.
     *
     * @param  n    length of rows to be shifted
     * @param  nup  DOCUMENT ME!
     * @param  af   DOCUMENT ME!
     * @param  f    first row to be shifted
     * @param  ag   DOCUMENT ME!
     * @param  g    second row to be shifted
     */
    void lin_shift2(int n, int nup, float af, float[] f, float ag, float[] g) {
        lin_shift(n, af, f);

        if (g != null) {
            lin_shift(n, ag, g);
        }

        return;
    }

    /**
     * "Norm" of a set of shears [maximum stretch factor].
     *
     * @param   sh  input shear
     *
     * @return  "Norm"
     */

    double norm_3shear(shear sh) {
        double top = 0.0, val;
        int ii, jj;
        int[] ax;
        double[][] scl;

        ax = sh.ax;

        if (ax[0] < 0) {
            return BIG_NORM;
        }

        scl = sh.scl;

        for (ii = 0; ii < 3; ii++) {
            jj = ax[ii];
            val = Math.abs(scl[ii][(jj + 1) % 3]);

            if (val > top) {
                top = val;
            }

            val = Math.abs(scl[ii][(jj + 2) % 3]);

            if (val > top) {
                top = val;
            }
        }

        return top;
    }

    /**
     * Shift one row with quintic Lagrange polynomial interpolation [Nov 1998]. Note that heptic interpolation is about
     * the same as Hamming-weighted 2-sidelobe sinc interpolation.
     *
     * @param  n   length of row to be shifted
     * @param  af  DOCUMENT ME!
     * @param  f   row to be shifted
     */

    void quint_shift(int n, float af, float[] f) {
        int ii, ia, ix;
        float wt_m1, wt_00, wt_p1, wt_p2, aa, wt_m2, wt_p3;

        // #ifdef SEPARATE_FINS
        int ibot, itop;
        float finsm2, finsm1, fins, fins1, fins2, fins3;
        // #endif

        af = -af;
        ia = (int) af;

        if (af < 0) {
            ia--;
        } /* ia = floor */

        /* 15 Mar 2001: if shift is too large, return all zeros */

        if ((ia <= -n) || (ia >= n)) {

            for (ii = 0; ii < n; ii++) {
                f[ii] = 0.0f;
            }

            return;
        }

        aa = af - ia;
        wt_m2 = (aa * ((aa * aa) - 1.0f) * (2.0f - aa) * (aa - 3.0f) * 0.008333333f);
        wt_m1 = (aa * ((aa * aa) - 4.0f) * (aa - 1.0f) * (aa - 3.0f) * 0.041666667f);
        wt_00 = (((aa * aa) - 4.0f) * ((aa * aa) - 1.0f) * (3.0f - aa) * 0.083333333f);
        wt_p1 = (aa * ((aa * aa) - 4.0f) * (aa + 1.0f) * (aa - 3.0f) * 0.083333333f);
        wt_p2 = (aa * ((aa * aa) - 1.0f) * (aa + 2.0f) * (3.0f - aa) * 0.041666667f);
        wt_p3 = (aa * ((aa * aa) - 1.0f) * ((aa * aa) - 4.0f) * 0.008333333f);

        if (n > nlcbuf) {

            if (lcbuf != null) {
                lcbuf = null;
            }

            lcbuf = new float[n];
            nlcbuf = n;
        }

        // #ifdef SEPARATE_FINS
        ibot = 2 - ia;

        if (ibot < 0) {
            ibot = 0;
        }

        itop = n - 4 - ia;

        if (itop > (n - 1)) {
            itop = n - 1;
        }

        for (ii = ibot; ii <= itop; ii++) {
            ix = ii + ia;
            lcbuf[ii] = (wt_m2 * f[ix - 2]) + (wt_m1 * f[ix - 1]) + (wt_00 * f[ix]) + (wt_p1 * f[ix + 1]) +
                        (wt_p2 * f[ix + 2]) + (wt_p3 * f[ix + 3]);
        }

        if (ibot > n) {
            ibot = n;
        } /* 15 Mar 2001 */

        for (ii = 0; ii < ibot; ii++) {
            ix = ii + ia;

            if (((ix - 2) < 0) || ((ix - 2) >= n)) {
                finsm2 = 0.0f;
            } else {
                finsm2 = f[ix - 2];
            }

            if (((ix - 1) < 0) || ((ix - 1) >= n)) {
                finsm1 = 0.0f;
            } else {
                finsm1 = f[ix - 1];
            }

            if ((ix < 0) || (ix >= n)) {
                fins = 0.0f;
            } else {
                fins = f[ix];
            }

            if (((ix + 1) < 0) || ((ix + 1) >= n)) {
                fins1 = 0.0f;
            } else {
                fins1 = f[ix + 1];
            }

            if (((ix + 2) < 0) || ((ix + 2) >= n)) {
                fins2 = 0.0f;
            } else {
                fins2 = f[ix + 2];
            }

            if (((ix + 3) < 0) || ((ix + 3) >= n)) {
                fins3 = 0.0f;
            } else {
                fins3 = f[ix + 3];
            }

            lcbuf[ii] = (wt_m2 * finsm2) + (wt_m1 * finsm1) + (wt_00 * fins) + (wt_p1 * fins1) + (wt_p2 * fins2) +
                        (wt_p3 * fins3);
        }

        if (itop < 0) {
            itop = -1;
        } /* 15 Mar 2001 */

        for (ii = itop + 1; ii < n; ii++) {
            ix = ii + ia;

            if (((ix - 2) < 0) || ((ix - 2) >= n)) {
                finsm2 = 0.0f;
            } else {
                finsm2 = f[ix - 2];
            }

            if (((ix - 1) < 0) || ((ix - 1) >= n)) {
                finsm1 = 0.0f;
            } else {
                finsm1 = f[ix - 1];
            }

            if ((ix < 0) || (ix >= n)) {
                fins = 0.0f;
            } else {
                fins = f[ix];
            }

            if (((ix + 1) < 0) || ((ix + 1) >= n)) {
                fins1 = 0.0f;
            } else {
                fins1 = f[ix + 1];
            }

            if (((ix + 2) < 0) || ((ix + 2) >= n)) {
                fins2 = 0.0f;
            } else {
                fins2 = f[ix + 2];
            }

            if (((ix + 3) < 0) || ((ix + 3) >= n)) {
                fins3 = 0.0f;
            } else {
                fins3 = f[ix + 3];
            }

            lcbuf[ii] = (wt_m2 * finsm2) + (wt_m1 * finsm1) + (wt_00 * fins) + (wt_p1 * fins1) + (wt_p2 * fins2) +
                        (wt_p3 * fins3);
        }
        // #else /* not SEPARATE_FINS */
        /* for( ii=0 ; ii < n ; ii++ ){
         * ix = ii + ia ; if( ix > 1 && ix < n-3 ) lcbuf[ii] =  wt_m2 * f[ix-2] + wt_m1 * f[ix-1] + wt_00 * f[ix] +
         * wt_p1 * f[ix+1] + wt_p2 * f[ix+2] + wt_p3 * f[ix+3] ; else lcbuf[ii] =  wt_m2 * FINS(ix-2) + wt_m1 *
         * FINS(ix-1) + wt_00 * FINS(ix) + wt_p1 * FINS(ix+1) + wt_p2 * FINS(ix+2) + wt_p3 * FINS(ix+3) ; } #endif */
        /* SEPARATE_FINS */

        for (ii = 0; ii < n; ii++) {
            f[ii] = lcbuf[ii];
        }

        return;
    }

    /**
     * Shift 2 rows with quintic Lagrangian interpolation.
     *
     * @param  n    length of rows to be shifted
     * @param  nup  DOCUMENT ME!
     * @param  af   DOCUMENT ME!
     * @param  f    first row to be shifted
     * @param  ag   DOCUMENT ME!
     * @param  g    second row to be shifted
     */
    void quint_shift2(int n, int nup, float af, float[] f, float ag, float[] g) {
        quint_shift(n, af, f);

        if (g != null) {
            quint_shift(n, ag, g);
        }

        return;
    }

    /**
     * Compute a rotation matrix specified by 3 angles: Q = R3 R2 R1, where Ri is rotation about axis axi by angle thi.
     *
     * @param   ax1  DOCUMENT ME!
     * @param   th1  DOCUMENT ME!
     * @param   ax2  DOCUMENT ME!
     * @param   th2  DOCUMENT ME!
     * @param   ax3  DOCUMENT ME!
     * @param   th3  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */

    double[][] rot_to_matrix(int ax1, double th1, int ax2, double th2, int ax3, double th3) {
        double[][] q = new double[3][3];
        double[][] p = new double[3][3];
        int j;

        switch (ax1) {

            case 0:
                load_rotgen_dmat(q, th1, 0, 1, 2);
                break;

            case 1:
                load_rotgen_dmat(q, th1, 1, 2, 0);
                break;

            case 2:
                load_rotgen_dmat(q, th1, 2, 0, 1);
                break;

            default:
                load_zero_dmat(q);
                break;
        }

        switch (ax2) {

            case 0:
                load_rotgen_dmat(p, th2, 0, 1, 2);
                break;

            case 1:
                load_rotgen_dmat(p, th2, 1, 2, 0);
                break;

            case 2:
                load_rotgen_dmat(p, th2, 2, 0, 1);
                break;

            default:
                load_zero_dmat(p);
                break;
        }

        q = dmat_mul(p, q);

        switch (ax3) {

            case 0:
                load_rotgen_dmat(p, th3, 0, 1, 2);
                break;

            case 1:
                load_rotgen_dmat(p, th3, 1, 2, 0);
                break;

            case 2:
                load_rotgen_dmat(p, th3, 2, 0, 1);
                break;

            default:
                load_zero_dmat(p);
                break;
        }

        q = dmat_mul(p, q);

        for (j = 0; j < 3; j++) {
            p[j] = null;
        }

        p = null;

        return q;
    }

    /**
     * Find the "best" shear decomposition (smallest stretching factors).
     *
     * @param   q       DOCUMENT ME!
     * @param   xyzdel  DOCUMENT ME!
     *
     * @return  best shear
     */

    shear shear_best(double[][] q, double[] xyzdel) {
        shear[] sh = new shear[6];
        int ii, jbest;
        double val, best;
        int i, j;
        int[] ax;
        int flip0, flip1;
        double[][] scl;
        double[] sft;

        /* compute all 6 possible factorizations (the brute force approach) */

        sh[0] = shear_arb(q, xyzdel, 0, 1, 2);
        sh[1] = shear_arb(q, xyzdel, 0, 2, 1);
        sh[2] = shear_arb(q, xyzdel, 1, 0, 2);
        sh[3] = shear_arb(q, xyzdel, 1, 2, 0);
        sh[4] = shear_arb(q, xyzdel, 2, 0, 1);
        sh[5] = shear_arb(q, xyzdel, 2, 1, 0);

        Preferences.debug("sbest-q");
        Preferences.debug("q[0][0] = " + q[0][0] + " q[0][1] = " + q[0][1] + " q[0][2] = " + q[0][2] + "\n");
        Preferences.debug("q[1][0] = " + q[1][0] + " q[1][1] = " + q[1][1] + " q[1][2] = " + q[1][2] + "\n");
        Preferences.debug("q[2][0] = " + q[2][0] + " q[2][1] = " + q[2][1] + " q[2][2] = " + q[2][2] + "\n");
        Preferences.debug("sbest-v xyzdel = " + xyzdel[0] + "  " + xyzdel[1] + "  " + xyzdel[2] + "\n");

        for (i = 0; i < 6; i++) {
            Preferences.debug("sbest-[" + i + "]\n");
            ax = sh[i].ax;
            Preferences.debug("ax = " + ax[0] + "  " + ax[1] + "  " + ax[2] + "  " + ax[3] + "\n");
            flip0 = sh[i].flip0;
            flip1 = sh[i].flip1;
            Preferences.debug("flip0 = " + flip0 + " flip1 = " + flip1 + "\n");
            scl = sh[i].scl;
            Preferences.debug("scl\n");

            for (j = 0; j < 4; j++) {
                Preferences.debug("scl[" + j + "]" + scl[j][0] + "  " + scl[j][1] + "  " + scl[j][2] + "\n");
            }

            sft = sh[i].sft;
            Preferences.debug("sft = " + sft[0] + "  " + sft[1] + "  " + sft[2] + "  " + sft[3] + "\n");
        } // for (i = 0; i < 6; i++)

        /* find the one with the smallest "norm" */

        jbest = 0;
        best = BIG_NORM;

        for (ii = 0; ii < 6; ii++) {
            val = norm_3shear(sh[ii]);
            Preferences.debug("sbest-val[" + ii + "] = " + val + "\n");

            if (val < best) {
                best = val;
                jbest = ii;
            }
        }

        Preferences.debug("sbest-jbest = " + jbest + "\n");

        return sh[jbest];
    }

    /**
     * produce the Choleski decomposition and weight scaled ref vectors for later use in delayed_lsqfit. This is to be
     * used when fitting several data vectors to the same references with the same weight factors.
     *
     * @param   veclen  = length of vectors
     * @param   wt      = array holding weight for each data point (length veclen) [if NULL, then a vector of all 1's is
     *                  used]
     * @param   nref    = number of reference vectors, each of length veclen; must have 1 <= nref <= veclen
     * @param   ref     = array of pointers to reference vectors, so that ref[i][k] is the k-th component of the i-th
     *                  reference, for i=0..nref-1, k=0..veclen-1
     *
     * @return  1 for success, -1 for failure fill array chol_fitim which points to memory for the Choleksi
     *          decomposition. It should later be passed to delayed_lsqfit. If wt != NULL, then ref[ii][jj] is scaled by
     *          wt[jj] as well.
     */

    int startup_lsqfit(int veclen, float[] wt, int nref, float[][] ref) {
        int ii, jj, kk;
        double sum;

        if ((nref < 1) || (veclen < nref) || (ref == null)) {
            cleanup();
            MipavUtil.displayError("*** Illegal inputs to startup_lsqfit\n");
            displayError("AlgorithmRegistrationShear: Out of memory creating cc");

            setCompleted(false);

            return -1;
        }

        if (wt != null) {

            for (jj = 0; jj < nref; jj++) {

                for (ii = 0; ii <= jj; ii++) {
                    sum = 0.0;

                    for (kk = 0; kk < veclen; kk++) {
                        sum += ref[ii][kk] * ref[jj][kk] * wt[kk];
                    }

                    chol_fitim[ii + (jj * nref)] = chol_fitim[jj + (ii * nref)] = sum;
                }
            }
        } else {

            for (jj = 0; jj < nref; jj++) {

                for (ii = 0; ii <= jj; ii++) {
                    sum = 0.0;

                    for (kk = 0; kk < veclen; kk++) {
                        sum += ref[ii][kk] * ref[jj][kk];
                    }

                    chol_fitim[ii + (jj * nref)] = chol_fitim[jj + (ii * nref)] = sum;
                }
            }
        }

        /*** Choleski decompose chol_fitim ***/

        for (ii = 0; ii < nref; ii++) {

            for (jj = 0; jj < ii; jj++) {
                sum = chol_fitim[ii + (jj * nref)];

                for (kk = 0; kk < jj; kk++) {
                    sum -= chol_fitim[ii + (kk * nref)] * chol_fitim[jj + (kk * nref)];
                }

                chol_fitim[ii + (jj * nref)] = sum / chol_fitim[jj + (jj * nref)];
            }

            sum = chol_fitim[ii + (ii * nref)];

            for (kk = 0; kk < ii; kk++) {
                sum -= chol_fitim[ii + (kk * nref)] * chol_fitim[ii + (kk * nref)];
            }

            if (sum <= 0.0) {
                cleanup();
                MipavUtil.displayError("Choleski factorization failure in startup_lsqfit\n");

                setCompleted(false);

                return -1;
            }

            chol_fitim[ii + (ii * nref)] = Math.sqrt(sum);
        }

        /*** scale ref by wt, if desired ***/

        if (wt != null) {

            for (ii = 0; ii < nref; ii++) {

                for (jj = 0; jj < veclen; jj++) {
                    ref[ii][jj] *= wt[jj];
                }
            }
        }

        return 1;
    }

    /**
     * DOCUMENT ME!
     */
    private void cleanup() {
        int j;

        if (blurredImageA != null) {
            blurredImageA.disposeLocal(); // Clean up memory of result image
            blurredImageA = null;
        }

        if (blurredImageB != null) {
            blurredImageB.disposeLocal(); // Clean up memory of blurred image B
            blurredImageB = null;
        }

        if (resampledImageB != null) {
            resampledImageB.disposeLocal();
            resampledImageB = null;
        }

        if (blurredImageW != null) {
            blurredImageW.disposeLocal(); // Clean up memory of blurred Image W
            blurredImageW = null;
        }

        if (pim != null) {
            pim.disposeLocal();
            pim = null;
        }

        if (mim != null) {
            mim.disposeLocal();
            mim = null;
        }

        if (imageW != null) {
            imageW.disposeLocal();
            imageW = null;
        }

        if (tim != null) {
            tim.disposeLocal();
            tim = null;
        }

        if (vvv != null) {
            vvv.disposeLocal();
            vvv = null;
        }

        data = null;
        rr = null;

        if (fitim != null) {

            for (j = 0; j < nref; j++) {
                fitim[j] = null;
            }

            fitim = null;
        }

        chol_fitim = null;
        fj0x = null;
        fj0y = null;
        fj1y = null;
        fj0z = null;
        fj1z = null;
        sigmas = null;
        blurredA = null;
        blurredB = null;
        fit = null;
        dfit = null;
        destExtents = null;
        lcbuf = null;

        if (shr != null) {

            for (j = 0; j < 4; j++) {
                shr.scl[j] = null;
            }

            shr.scl = null;
            shr.sft = null;
            shr.ax = null;
            shr = null;
        }

        if (algoTrans != null) {
            algoTrans.disposeLocal();
        }

        algoTrans = null;
        System.gc();
    }

    /**
     * multiplies two 3x3 matrices.
     *
     * @param   A  DOCUMENT ME!
     * @param   B  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     *
     * @reutrn  ouput matrix C
     */
    private double[][] dmat_mul(double[][] A, double[][] B) {
        double[][] C = new double[3][3];
        C[0][0] = (A[0][0] * B[0][0]) + (A[0][1] * B[1][0]) + (A[0][2] * B[2][0]);
        C[0][1] = (A[0][0] * B[0][1]) + (A[0][1] * B[1][1]) + (A[0][2] * B[2][1]);
        C[0][2] = (A[0][0] * B[0][2]) + (A[0][1] * B[1][2]) + (A[0][2] * B[2][2]);
        C[1][0] = (A[1][0] * B[0][0]) + (A[1][1] * B[1][0]) + (A[1][2] * B[2][0]);
        C[1][1] = (A[1][0] * B[0][1]) + (A[1][1] * B[1][1]) + (A[1][2] * B[2][1]);
        C[1][2] = (A[1][0] * B[0][2]) + (A[1][1] * B[1][2]) + (A[1][2] * B[2][2]);
        C[2][0] = (A[2][0] * B[0][0]) + (A[2][1] * B[1][0]) + (A[2][2] * B[2][0]);
        C[2][1] = (A[2][0] * B[0][1]) + (A[2][1] * B[1][1]) + (A[2][2] * B[2][1]);
        C[2][2] = (A[2][0] * B[0][2]) + (A[2][1] * B[1][2]) + (A[2][2] * B[2][2]);

        return C;
    }

    /**
     * matrix-vector multiply multiply a 3x3 matrix by a 3 element vector.
     *
     * @param   A  input 3x3 matrix
     * @param   x  input 3 element vector
     *
     * @return  DOCUMENT ME!
     *
     * @reutrn  B output 3 element vector
     */
    private double[] dmatvec(double[][] A, double[] x) {
        double[] B = new double[3];

        B[0] = (A[0][0] * x[0]) + (A[0][1] * x[1]) + (A[0][2] * x[2]);
        B[1] = (A[1][0] * x[0]) + (A[1][1] * x[1]) + (A[1][2] * x[2]);
        B[2] = (A[2][0] * x[0]) + (A[2][1] * x[1]) + (A[2][2] * x[2]);

        return B;
    }

    /**
     * load diagonal matrix.
     *
     * @param  A  input matrix
     * @param  x  [0][0] element
     * @param  y  [1][1] element
     * @param  z  [2][2] element
     */
    private void load_diag_dmat(double[][] A, double x, double y, double z) {
        A[0][0] = x;
        A[1][1] = y;
        A[2][2] = z;
        A[0][1] = A[0][2] = A[1][0] = 0.0;
        A[1][2] = A[2][0] = A[2][1] = 0.0;
    }

    /**
     * elementary rotation matrices: rotate about axis #ff, from axis #aa toward #bb, where ff, aa, and bb are a
     * permutation of {0,1,2}.
     *
     * @param  A   input matrix
     * @param  th  DOCUMENT ME!
     * @param  ff  DOCUMENT ME!
     * @param  aa  DOCUMENT ME!
     * @param  bb  DOCUMENT ME!
     */
    private void load_rotgen_dmat(double[][] A, double th, int ff, int aa, int bb) {
        A[aa][aa] = A[bb][bb] = Math.cos(th);
        A[aa][bb] = Math.sin(th);
        A[bb][aa] = -A[aa][bb];
        A[ff][ff] = 1.0;
        A[aa][ff] = A[bb][ff] = A[ff][aa] = A[ff][bb] = 0.0;
    }

    /**
     * zero matrix.
     *
     * @param  A  input matrix
     */
    private void load_zero_dmat(double[][] A) {
        A[0][0] = 0.0;
        A[1][1] = 0.0;
        A[2][2] = 0.0;
        A[0][1] = A[0][2] = A[1][0] = 0.0;
        A[1][2] = A[2][0] = A[2][1] = 0.0;
    }

    /**
     * Creates first pass alignment parameters roll_1, pitch_1, yaw_1, dx_1, dy_1, and dz_1.
     *
     * @param  im  ModelImage to align to base image
     */

    private void mri_3dalign_one(ModelImage im) {
        int iter;
        float dxt, dyt, dzt;
        boolean good;

        iter = 0;

        /* convert displacement threshold from voxels to mm in each direction */

        dxt = (xdel != 0.0) ? (Math.abs(xdel) * dxy_thresh) : dxy_thresh;
        dyt = (ydel != 0.0) ? (Math.abs(ydel) * dxy_thresh) : dxy_thresh;
        dzt = (zdel != 0.0) ? (Math.abs(zdel) * dxy_thresh) : dxy_thresh;

        if ((init_dth1 != 0.0) || (init_dth2 != 0.0) || (init_dth3 != 0.0) || (init_dx != 0.0) || (init_dy != 0.0) ||
                (init_dz != 0.0)) {
            fit[0] = 1.0f;
            fit[1] = init_dth1;
            fit[2] = init_dth2;
            fit[3] = init_dth3; /* degrees */
            fit[4] = init_dx;
            fit[5] = init_dy;
            fit[6] = init_dz; /* mm      */

            good = true;
        } else {
            delayed_lsqfit_fit(im); // calculate fit

            good = (((10.0 * Math.abs(fit[4])) > dxt) || ((10.0 * Math.abs(fit[5])) > dyt) ||
                        ((10.0 * Math.abs(fit[6])) > dzt) || ((10.0 * Math.abs(fit[1])) > phi_thresh) ||
                        ((10.0 * Math.abs(fit[2])) > phi_thresh) || ((10.0 * Math.abs(fit[3])) > phi_thresh));
        }

        Preferences.debug("First fit: " + fit[0] + "  " + fit[1] + "  " + fit[2] + "  " + fit[3] + "  " + fit[4] +
                          "  " + fit[5] + "  " + fit[6] + "\n");

        /*-- iterate fit --*/

        while (good) {

            try {
                tim = (ModelImage) im.clone();
            } catch (OutOfMemoryError x) {
                cleanup();
                MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for tim");

                setCompleted(false);

                return;
            }

            THD_rota_vol(tim, (float) (fit[1] * DFAC), (float) (fit[2] * DFAC), (float) (fit[3] * DFAC), fit[4], fit[5],
                         fit[6]);

            delayed_lsqfit_dfit(tim); /* delta angle/shift */
            tim.disposeLocal();
            tim = null;

            fit[1] += dfit[1];
            fit[2] += dfit[2];
            fit[3] += dfit[3]; /* accumulate  */
            fit[4] += dfit[4];
            fit[5] += dfit[5];
            fit[6] += dfit[6]; /* angle/shift */

            Preferences.debug("Delta fit: " + dfit[0] + "  " + dfit[1] + "  " + dfit[2] + "  " + dfit[3] + "  " +
                              dfit[4] + "  " + dfit[5] + "  " + dfit[6] + "\n");
            Preferences.debug("Total fit: " + dfit[0] + "  " + fit[1] + "  " + fit[2] + "  " + fit[3] + "  " + fit[4] +
                              "  " + fit[5] + "  " + fit[6] + "\n");

            good = (++iter < MAX_ITER) &&
                       ((Math.abs(dfit[4]) > dxt) || (Math.abs(dfit[5]) > dyt) || (Math.abs(dfit[6]) > dzt) ||
                            (Math.abs(dfit[1]) > phi_thresh) || (Math.abs(dfit[2]) > phi_thresh) ||
                            (Math.abs(dfit[3]) > phi_thresh));

        } /* end while */

        Preferences.debug("Iteration complete at " + iter + " steps\n");

        /*-- save final alignment parameters --*/

        // Leave as degrees
        roll_1 = fit[1];
        pitch_1 = fit[2];
        yaw_1 = fit[3];
        dx_1 = fit[4];
        dy_1 = fit[5];
        dz_1 = fit[6];

        return;
    }

    /**
     * Creates second pass alignment parameters roll, pitch, yaw, ddx, ddy, and ddz and performs the actual alignment.
     *
     * @param  im  ModelImage to align to base image
     */
    private void mri_3dalign_one_final(ModelImage im) {
        int iter, ii;
        float dxt, dyt, dzt, ftop, fbot;
        boolean good;
        ModelImage tim = null;

        iter = 0;

        /* convert displacement threshold from voxels to mm in each direction */

        dxt = (xdel != 0.0) ? (Math.abs(xdel) * dxy_thresh) : dxy_thresh;
        dyt = (ydel != 0.0) ? (Math.abs(ydel) * dxy_thresh) : dxy_thresh;
        dzt = (zdel != 0.0) ? (Math.abs(zdel) * dxy_thresh) : dxy_thresh;

        if ((init_dth1 != 0.0) || (init_dth2 != 0.0) || (init_dth3 != 0.0) || (init_dx != 0.0) || (init_dy != 0.0) ||
                (init_dz != 0.0)) {
            fit[0] = 1.0f;
            fit[1] = init_dth1;
            fit[2] = init_dth2;
            fit[3] = init_dth3; /* degrees */
            fit[4] = init_dx;
            fit[5] = init_dy;
            fit[6] = init_dz; /* mm      */

            good = true;
        } else {
            delayed_lsqfit_fit(im); // calculate fit

            good = (((10.0 * Math.abs(fit[4])) > dxt) || ((10.0 * Math.abs(fit[5])) > dyt) ||
                        ((10.0 * Math.abs(fit[6])) > dzt) || ((10.0 * Math.abs(fit[1])) > phi_thresh) ||
                        ((10.0 * Math.abs(fit[2])) > phi_thresh) || ((10.0 * Math.abs(fit[3])) > phi_thresh));
        }

        Preferences.debug("First fit: " + fit[0] + "  " + fit[1] + "  " + fit[2] + "  " + fit[3] + "  " + fit[4] +
                          "  " + fit[5] + "  " + fit[6] + "\n");

        /*-- iterate fit --*/

        while (good) {

            try {
                tim = (ModelImage) im.clone();
            } catch (OutOfMemoryError x) {
                cleanup();
                MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for tim");

                setCompleted(false);

                return;
            }

            THD_rota_vol(tim, (float) (fit[1] * DFAC), (float) (fit[2] * DFAC), (float) (fit[3] * DFAC), fit[4], fit[5],
                         fit[6]);

            delayed_lsqfit_dfit(tim); /* delta angle/shift */
            tim.disposeLocal();
            tim = null;

            fit[1] += dfit[1];
            fit[2] += dfit[2];
            fit[3] += dfit[3]; /* accumulate  */
            fit[4] += dfit[4];
            fit[5] += dfit[5];
            fit[6] += dfit[6]; /* angle/shift */

            Preferences.debug("Delta fit: " + dfit[0] + "  " + dfit[1] + "  " + dfit[2] + "  " + dfit[3] + "  " +
                              dfit[4] + "  " + dfit[5] + "  " + dfit[6] + "\n");
            Preferences.debug("Total fit: " + dfit[0] + "  " + fit[1] + "  " + fit[2] + "  " + fit[3] + "  " + fit[4] +
                              "  " + fit[5] + "  " + fit[6] + "\n");

            good = (++iter < MAX_ITER) &&
                       ((Math.abs(dfit[4]) > dxt) || (Math.abs(dfit[5]) > dyt) || (Math.abs(dfit[6]) > dzt) ||
                            (Math.abs(dfit[1]) > phi_thresh) || (Math.abs(dfit[2]) > phi_thresh) ||
                            (Math.abs(dfit[3]) > phi_thresh));

        } /* end while */

        Preferences.debug("Iteration complete at " + iter + " steps\n");

        /*-- save final alignment parameters --*/

        roll = (float) (fit[1] * DFAC); /* convert to radians */
        pitch = (float) (fit[2] * DFAC);
        yaw = (float) (fit[3] * DFAC);
        ddx = fit[4];
        ddy = fit[5];
        ddz = fit[6];
        Preferences.debug("roll = " + fit[1] + " pitch = " + fit[2] + " yaw = " + fit[3]);
        Preferences.debug("ddx = " + ddx + " ddy = " + ddy + " ddz = " + ddz);

        /*-- do the actual realignment --*/

        try {
            tim = (ModelImage) im.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for tim");

            setCompleted(false);

            return;
        }

        THD_rota_vol(tim, (float) (fit[1] * DFAC), (float) (fit[2] * DFAC), (float) (fit[3] * DFAC), fit[4], fit[5],
                     fit[6]);

        try {
            tim.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: tim is locked");

            setCompleted(false);

            return;
        }

        if ((tim != null) && clipit &&
                ((final_regmode == AlgorithmTransform.QUINTIC_LAGRANGIAN) ||
                     (final_regmode == AlgorithmTransform.CUBIC_LAGRANGIAN) ||
                     (final_regmode == AlgorithmTransform.HEPTIC_LAGRANGIAN))) {

            im.calcMinMax();
            ftop = (float) im.getMax();
            fbot = (float) im.getMin();

            for (ii = 0; ii < volSize; ii++) {

                if (data[ii] < fbot) {
                    data[ii] = fbot;
                } else if (data[ii] > ftop) {
                    data[ii] = ftop;
                }
            }

        }

        tim.disposeLocal();
        tim = null;

        return;
    }

    /**
     * creates fitim and chol_fitim structures used for alignment.
     *
     * @param   imbase  = base image for alignment
     * @param   imwt    DOCUMENT ME!
     *
     * @parma   imwt = image of weight factors to align to (if NULL, will generate one internally)
     *
     * @return  1 for success, -1 for failure
     */

    private int mri_3dalign_setup(ModelImage imbase, ModelImage imwt) {
        float delta;
        int ii, jj, kk, ff;
        int nxy = nx * ny;
        int cfSuccess;

        /*-- base image --*/
        // bim = mri_to_float( imbase ) ;
        // INIT_IMARR ( fitim ) ;
        // ADDTO_IMARR( fitim , bim ) ;

        try {
            imbase.exportData(0, volSize, fitim[0]); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: imbase is locked");

            setCompleted(false);

            return -1;
        }

        // case MRI_LINEAR:  shifter = lin_shift2   ; break ;

        delta = 2.0f * delfac / (nx + ny + nz);

        try {
            pim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for pim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(pim, delta, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f);

        try {
            pim.exportData(0, volSize, fitim[1]); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: pim is locked");

            setCompleted(false);

            return -1;
        }

        try {
            mim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for mim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(mim, -delta, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f);

        try {
            mim.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: mim is locked");

            setCompleted(false);

            return -1;
        }

        delta = (float) (0.5 * DFAC / delta);

        for (ii = 0; ii < volSize; ii++) {
            fitim[1][ii] = delta * (data[ii] - fitim[1][ii]);
        }

        pim.disposeLocal();
        pim = null;
        mim.disposeLocal();
        mim = null;

        /*-- d/d(th2) image --*/

        Preferences.debug("Initializing d/d(th2)\n");

        delta = (float) (2.0 * delfac / (nx + ny + nz));

        try {
            pim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for pim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(pim, 0.0f, delta, 0.0f, 0.0f, 0.0f, 0.0f);

        try {
            pim.exportData(0, volSize, fitim[2]); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: pim is locked");

            setCompleted(false);

            return -1;
        }

        try {
            mim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for mim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(mim, 0.0f, -delta, 0.0f, 0.0f, 0.0f, 0.0f);

        try {
            mim.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: mim is locked");

            setCompleted(false);

            return -1;
        }

        delta = (float) (0.5 * DFAC / delta);

        for (ii = 0; ii < volSize; ii++) {
            fitim[2][ii] = delta * (data[ii] - fitim[2][ii]);
        }

        pim.disposeLocal();
        pim = null;
        mim.disposeLocal();
        mim = null;

        /*-- d/d(th3) image --*/

        Preferences.debug("Initializing d/d(th3)\n");

        delta = (float) (2.0 * delfac / (nx + ny + nz));

        try {
            pim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for pim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(pim, 0.0f, 0.0f, delta, 0.0f, 0.0f, 0.0f);

        try {
            pim.exportData(0, volSize, fitim[3]); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: pim is locked");

            setCompleted(false);

            return -1;
        }

        try {
            mim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for mim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(mim, 0.0f, 0.0f, -delta, 0.0f, 0.0f, 0.0f);

        try {
            mim.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: mim is locked");

            setCompleted(false);

            return -1;
        }

        delta = (float) (0.5 * DFAC / delta);

        for (ii = 0; ii < volSize; ii++) {
            fitim[3][ii] = delta * (data[ii] - fitim[3][ii]);
        }

        pim.disposeLocal();
        pim = null;
        mim.disposeLocal();
        mim = null;

        /*-- d/dx image --*/

        Preferences.debug("Initializing d/dx\n");

        delta = delfac * xdel;

        try {
            pim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for pim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(pim, 0.0f, 0.0f, 0.0f, delta, 0.0f, 0.0f);

        try {
            pim.exportData(0, volSize, fitim[4]); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: pim is locked");

            setCompleted(false);

            return -1;
        }

        try {
            mim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for mim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(mim, 0.0f, 0.0f, 0.0f, -delta, 0.0f, 0.0f);

        try {
            mim.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: mim is locked");

            setCompleted(false);

            return -1;
        }

        delta = 0.5f / delta;

        for (ii = 0; ii < volSize; ii++) {
            fitim[4][ii] = delta * (data[ii] - fitim[4][ii]);
        }

        pim.disposeLocal();
        pim = null;
        mim.disposeLocal();
        mim = null;

        /*-- d/dy image --*/

        Preferences.debug("Initializing d/dy\n");

        delta = delfac * ydel;

        try {
            pim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for pim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(pim, 0.0f, 0.0f, 0.0f, 0.0f, delta, 0.0f);

        try {
            pim.exportData(0, volSize, fitim[5]); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: pim is locked");

            setCompleted(false);

            return -1;
        }

        try {
            mim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for mim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(mim, 0.0f, 0.0f, 0.0f, 0.0f, -delta, 0.0f);

        try {
            mim.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: mim is locked");

            setCompleted(false);

            return -1;
        }

        delta = 0.5f / delta;

        for (ii = 0; ii < volSize; ii++) {
            fitim[5][ii] = delta * (data[ii] - fitim[5][ii]);
        }

        pim.disposeLocal();
        pim = null;
        mim.disposeLocal();
        mim = null;

        /*-- d/dz image --*/

        Preferences.debug("Initializing d/dz\n");

        delta = delfac * zdel;

        try {
            pim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for pim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(pim, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, delta);

        try {
            pim.exportData(0, volSize, fitim[6]); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: pim is locked");

            setCompleted(false);

            return -1;
        }

        try {
            mim = (ModelImage) imbase.clone();
        } catch (OutOfMemoryError x) {
            cleanup();
            MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for mim");

            setCompleted(false);

            return -1;
        }

        THD_rota_vol(mim, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, -delta);

        try {
            mim.exportData(0, volSize, data); // locks and releases and lock
        } catch (IOException error) {
            cleanup();
            displayError("AlgorithmRegistrationShear: mim is locked");

            setCompleted(false);

            return -1;
        }

        delta = 0.5f / delta;

        for (ii = 0; ii < volSize; ii++) {
            fitim[6][ii] = delta * (data[ii] - fitim[6][ii]);
        }

        pim.disposeLocal();
        pim = null;
        mim.disposeLocal();
        mim = null;

        /*-- get the weighting image --*/

        if (imwt != null) {

            if ((imwt.getExtents()[0] != imbase.getExtents()[0]) || (imwt.getExtents()[1] != imbase.getExtents()[1]) ||
                    (imwt.getExtents()[2] != imbase.getExtents()[2])) {

                MipavUtil.displayWarning("in mri_3dalign_setup, weight image mismatch!\n");
                imwt = null;
            }
        }

        /* make weight up from the base */


        if (imwt == null) {

            Preferences.debug("Initializing weight\n");

            try {
                imageW = (ModelImage) imbase.clone();
            } catch (OutOfMemoryError x) {
                cleanup();
                MipavUtil.displayError("Algorithm Registration Shear: unable to allocate enough memory for imageW");

                setCompleted(false);

                return -1;
            }

            try {
                imageW.exportData(0, volSize, data); // locks and releases and lock
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmRegistrationShear: imageW is locked");

                setCompleted(false);

                return -1;
            }

            for (ii = 0; ii < volSize; ii++) {
                data[ii] = Math.abs(data[ii]);
            }

            try {
                imageW.importData(0, data, true);
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmRegistrationShear: IOException on imageW import data");

                setCompleted(false);

                return -1;
            }

            // #if 1
            sigmas[0] = 3.0f * xdel;
            sigmas[1] = 3.0f * ydel;
            sigmas[2] = 3.0f * zdel;

            // Make algorithm
            gaussianBlurAlgo = new AlgorithmGaussianBlur(null, imageW, sigmas, true, false);
            gaussianBlurAlgo.run();
            // #endif

            try {
                imageW.exportData(0, volSize, data); // locks and releases and lock
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmRegistrationShear: imageW is locked");

                setCompleted(false);

                return -1;
            }

            imageW.disposeLocal();
            imageW = null;

        } // if (imwt == null)
        else {

            try {
                imwt.exportData(0, volSize, data); // locks and releases and lock
            } catch (IOException error) {
                cleanup();
                displayError("AlgorithmRegistrationShear: imwt is locked");

                setCompleted(false);

                return -1;
            }
        }

        /*-- 10 Dec 2000: user-controlled fade out around the edges --*/

        if ((imwt == null) || force_edging) {

            xfade = xedge;
            yfade = yedge;
            zfade = zedge; /* static variables */

            if ((xfade < 0) || (yfade < 0) || (zfade < 0)) {

                /* the 5% solution */
                xfade = (int) ((0.05 * nx) + 0.5);
                yfade = (int) ((0.05 * ny) + 0.5);
                zfade = (int) ((0.05 * nz) + 0.5);
            }

            for (jj = 0; jj < ny; jj++) {

                for (ii = 0; ii < nx; ii++) {

                    for (ff = 0; ff < zfade; ff++) {
                        data[ii + (jj * nx) + (ff * nxy)] = data[ii + (jj * nx) + ((nz - 1 - ff) * nxy)] = 0.0f;
                    }
                }
            }

            for (kk = 0; kk < nz; kk++) {

                for (jj = 0; jj < ny; jj++) {

                    for (ff = 0; ff < xfade; ff++) {
                        data[ff + (jj * nx) + (kk * nxy)] = data[nx - 1 - ff + (jj * nx) + (kk * nxy)] = 0.0f;
                    }
                }
            }

            for (kk = 0; kk < nz; kk++) {

                for (ii = 0; ii < nx; ii++) {

                    for (ff = 0; ff < yfade; ff++) {
                        data[ii + (ff * nx) + (kk * nxy)] = data[ii + ((ny - 1 - ff) * nx) + (kk * nxy)] = 0.0f;
                    }
                }
            }
        }

        /*-- initialize linear least squares --*/

        Preferences.debug("Initializing least squares\n");

        cfSuccess = startup_lsqfit(volSize, data, nref, fitim);

        return cfSuccess;

    }

    /**
     * Permute the axes of a shear structure so that what was in order (0,1,2) is now in order (ox1,ox2,ox3). If P is
     * the 3x3 matrix [ delta(i,ox{j+1}) ] (i,j=0,1,2), then
     *
     * <p>T [output shear] = P [input shear] P</p>
     *
     * @param   shin  input shear
     * @param   ox1   DOCUMENT ME!
     * @param   ox2   DOCUMENT ME!
     * @param   ox3   DOCUMENT ME!
     *
     * @return  output shear
     */

    private shear permute_3shear(shear shin, int ox1, int ox2, int ox3) {
        shear shout;
        int ii, ain, aout;
        int[] pi = new int[3];
        int[] ax;
        int flip0 = -1;
        int flip1 = -1;
        double[][] scl = new double[4][3];
        double[][] sclsh = new double[4][3];
        double[] sft = new double[4];

        /* sanity check */

        // If shin is invalid, then invalidate shout and return
        ax = shin.ax;

        if (ax[0] < 0) {
            shout = new shear(ax, flip0, flip1, scl, sft);

            return shout;
        }

        pi[0] = ox1;
        pi[1] = ox2;
        pi[2] = ox3;

        scl = shin.scl;
        sft = shin.sft;
        flip0 = shin.flip0;
        flip1 = shin.flip1;

        for (ii = 0; ii < 4; ii++) {

            ain = ax[ii]; /* axis of input */
            aout = pi[ain]; /* axis of output */

            ax[ii] = aout; /* store new axis */
            sclsh[ii][pi[0]] = scl[ii][0]; /* permuted scalings */
            sclsh[ii][pi[1]] = scl[ii][1];
            sclsh[ii][pi[2]] = scl[ii][2];
        }

        shout = new shear(ax, flip0, flip1, sclsh, sft);
        pi = null;

        return shout;
    }

    /**
     * Permute a 3vector: T [output] = P [q].
     *
     * @param   q    input vector
     * @param   ox1  DOCUMENT ME!
     * @param   ox2  DOCUMENT ME!
     * @param   ox3  DOCUMENT ME!
     *
     * @return  permuted vector
     */

    private double[] permute_dfvec3(double[] q, int ox1, int ox2, int ox3) {
        double[] m = new double[3];
        int ii;
        int[] pi = new int[3];

        pi[0] = ox1;
        pi[1] = ox2;
        pi[2] = ox3;

        for (ii = 0; ii < 3; ii++) {
            m[ii] = q[pi[ii]];
        }

        pi = null;

        return m;
    }

    /**
     * Permute a 3x3 matrix: T [output] = P [q] P.
     *
     * @param   q    input matrix
     * @param   ox1  DOCUMENT ME!
     * @param   ox2  DOCUMENT ME!
     * @param   ox3  DOCUMENT ME!
     *
     * @return  permuted matrix
     */

    private double[][] permute_dmat33(double[][] q, int ox1, int ox2, int ox3) {
        double[][] m = new double[3][3];
        int ii, jj;
        int[] pi = new int[3];

        pi[0] = ox1;
        pi[1] = ox2;
        pi[2] = ox3;

        for (ii = 0; ii < 3; ii++) {

            for (jj = 0; jj < 3; jj++) {
                m[ii][jj] = q[pi[ii]][pi[jj]];
            }
        }

        pi = null;

        return m;
    }

    /**
     * Compute a set of shears to carry out a rotation + shift. The rotation is specified as the composition of
     * elementary rotations about 3 axes.
     *
     * @param   th1  roll in radians
     * @param   th2  pitch in radians
     * @param   th3  yaw in radians
     * @param   dx   x translation
     * @param   dy   y translation
     * @param   dz   z translation
     *
     * @return  the required shear
     */

    private shear rot_to_shear(double th1, double th2, double th3, double dx, double dy, double dz) {
        int flip0 = -1, flip1 = -1; /* no flips */
        double[][] q;
        double[][] p = new double[3][3];
        double[] d = new double[3];
        double[] c = new double[3];
        int[] ax;
        int j;
        double[][] scl;
        double[] sft;

        /* check if this is a duplicate call */

        if ((ax1 == old_ax1) && (ax2 == old_ax2) && (ax3 == old_ax3) && (dcode == old_dcode) && (th1 == old_th1) &&
                (th2 == old_th2) && (th3 == old_th3) && (dx == old_dx) && (dy == old_dy) && (dz == old_dz) &&
                (xdel == old_xdel) && (ydel == old_ydel) && (zdel == old_zdel)) {
            return shr;
        }

        old_ax1 = ax1;
        old_ax2 = ax2;
        old_ax3 = ax3;
        old_th1 = th1;
        old_th2 = th2;
        old_th3 = th3;
        old_dx = dx;
        old_dy = dy;
        old_dz = dz;
        old_xdel = xdel;
        old_ydel = ydel;
        old_zdel = zdel;
        old_dcode = dcode;

        /* compute rotation matrix */

        q = rot_to_matrix(ax1, th1, ax2, th2, ax3, th3);

        Preferences.debug("rot_to_shear: th1 = " + th1 + " th2 = " + th2 + " th3 = " + th3 + "\n");
        Preferences.debug("q[0][0] = " + q[0][0] + " q[0][1] = " + q[0][1] + " q[0][2] = " + q[0][2] + "\n");
        Preferences.debug("q[1][0] = " + q[1][0] + " q[1][1] = " + q[1][1] + " q[1][2] = " + q[1][2] + "\n");
        Preferences.debug("q[2][0] = " + q[2][0] + " q[2][1] = " + q[2][1] + " q[2][2] = " + q[2][2] + "\n");

        /* if trace too small, maybe we should flip a couple axes */

        if ((q[0][0] + q[1][1] + q[2][2]) < 1.0) {
            double top = q[0][0];
            int itop = 0, i1 = 1, i2 = 2;

            if (top < q[1][1]) {
                top = q[1][1];
                itop = 1;
            }

            if (top < q[2][2]) {
                top = q[2][2];
                itop = 2;
            }

            switch (itop) {

                case 0:
                    i1 = 1;
                    i2 = 2;
                    load_diag_dmat(p, 1, -1, -1);
                    break;

                case 1:
                    i1 = 0;
                    i2 = 2;
                    load_diag_dmat(p, -1, 1, -1);
                    break;

                case 2:
                    i1 = 0;
                    i2 = 1;
                    load_diag_dmat(p, -1, -1, 1);
                    break;
            }

            if ((q[i1][i1] + q[i2][i2]) < -0.02) {
                q = dmat_mul(q, p);
                flip0 = i1;
                flip1 = i2; /* yes flips */
                Preferences.debug("flip0 = " + flip0 + " flip1 = " + flip1 + "\n");
            }
        }

        d[0] = dx;
        d[1] = dy;
        d[2] = dz;

        switch (dcode) {

            default:
                break; /* nothing */

            case DELTA_BEFORE:
                d = dmatvec(q, d);
                break;

            case DELTA_FIXED:
                c = dmatvec(q, d);
                d = sub_dfvec3(d, c);
                break;
        }

        Preferences.debug("dcode = " + dcode + " dx = " + dx + " dy = " + dy + " dz = " + dz + "\n");

        /* scale q and d by the voxel dimensions */

        d[0] = d[0] / xdel; /* d <- inv[D] d, where D = diag[xdel,ydel,zdel] */
        d[1] = d[1] / ydel;
        d[2] = d[2] / zdel;

        q[0][1] *= (ydel / xdel); /* q <- inv[D] q D */
        q[0][2] *= (zdel / xdel);
        q[1][0] *= (xdel / ydel); /* q still has det[q]=1 after this */
        q[1][2] *= (zdel / ydel);
        q[2][0] *= (xdel / zdel);
        q[2][1] *= (ydel / zdel);

        /* compute the "best" shear for this q matrix */

        shr = shear_best(q, d);

        /* if cannot compute shear, try perturbing the matrix a little */

        ax = shr.ax;

        if (ax[0] < 0) {
            double[][] pt = new double[3][3];
            p = rot_to_matrix(0, PER0, 1, PER1, 2, PER2);
            q = dmat_mul(q, p);
            pt = transpose_dmat(p);
            q = dmat_mul(pt, q);

            shr = shear_best(q, d);
            ax = shr.ax;

            if (ax[0] < 0) {
                return shr;
            } /* give up */
        }

        shr.flip0 = flip0;
        shr.flip1 = flip1;

        Preferences.debug("rot_to_shear shr\n");
        ax = shr.ax;
        Preferences.debug("ax = " + ax[0] + "  " + ax[1] + "  " + ax[2] + "  " + ax[3] + "\n");
        Preferences.debug("flip0 = " + flip0 + " flip1 = " + flip1 + "\n");
        scl = shr.scl;
        Preferences.debug("scl\n");

        for (j = 0; j < 4; j++) {
            Preferences.debug("scl[" + j + "]" + scl[j][0] + "  " + scl[j][1] + "  " + scl[j][2] + "\n");
        }

        sft = shr.sft;
        Preferences.debug("sft = " + sft[0] + "  " + sft[1] + "  " + sft[2] + "  " + sft[3] + "\n");

        for (j = 0; j < 3; j++) {
            p[j] = null;
            q[j] = null;
        }

        p = null;
        q = null;
        d = null;
        c = null;

        return shr;
    }

    /**
     * Decompose transformation (q,xyzdel) into a set of 4 shears, whose axis order is ox1,ox2,ox3,ox1; that is, q = S S
     * S S ox1 ox3 ox2 ox1.
     *
     * @param   q       DOCUMENT ME!
     * @param   xyzdel  DOCUMENT ME!
     * @param   ox1     DOCUMENT ME!
     * @param   ox2     DOCUMENT ME!
     * @param   ox3     DOCUMENT ME!
     *
     * @return  shear
     */

    private shear shear_arb(double[][] q, double[] xyzdel, int ox1, int ox2, int ox3) {
        double[][] qq;
        double[] xx;
        shear sh_xzyx, shout;
        int[] ax;
        int j;

        /* permute the input matrix and vector to the desired order */

        qq = permute_dmat33(q, ox1, ox2, ox3);
        xx = permute_dfvec3(xyzdel, ox1, ox2, ox3);

        /* compute the Sx Sz Sy Sx factorization */

        sh_xzyx = shear_xzyx(qq, xx);
        ax = sh_xzyx.ax;

        if (ax[0] < 0) {
            return sh_xzyx;
        }

        /* permute the shear factorization back */

        shout = permute_3shear(sh_xzyx, ox1, ox2, ox3);

        for (j = 0; j < 3; j++) {
            qq[j] = null;
        }

        qq = null;
        xx = null;

        return shout;
    }

    /**
     * Function to compute the decomposition of a 3x3 matrix into a product of 4 shears:
     *
     * <p>Q = Sx[bx2,cx2,f] Sz[az,bz,f] Sy[ay,cy,f] Sx[bx1,cx1,1]</p>
     *
     * <p>where [ f b c ] [ 1 0 0 ] [ 1 0 0 ] Sx[b,c,f] = [ 0 1 0 ] Sy[a,c,f] = [ a f b ] Sz[a,b,f] = [ 0 f 0 ] [ 0 0 0
     * ] [ 0 0 1 ] [ a b 1 ]</p>
     *
     * <p>"f" is a stretching/shrinking factor applied with the 1D shear. If det[Q] = 1 , then f = 1 and these are pure
     * shears.</p>
     *
     * <p>In addition, a shift is allowed for, so that the total coordinate transformation being decomposed is really
     * </p>
     *
     * <p>[ x ] [ x ] [ xdel ] [ y ] = [Q] [ y ] + [ ydel ] [ z ] [ z ] [ zdel ] new old</p>
     *
     * <p>The output shifts are applied to the last three transformations, so that the factoring produces this sequence
     * of operations:</p>
     *
     * <p>[ x ] [ x ] [ y ] = [Sx1] [ y ] [ z ] [ z ] 1 old</p>
     *
     * <p>[ x ] [ x ] [ 0 ] [ y ] = [Sy] [ y ] + [ dy ] [ z ] [ z ] [ 0 ] 2 1</p>
     *
     * <p>[ x ] [ x ] [ 0 ] [ y ] = [Sz] [ y ] + [ 0 ] [ z ] [ z ] [ dz ] 3 2</p>
     *
     * <p>[ x ] [ x ] [ dx ] [ y ] = [Sx2] [ y ] + [ 0 ] [ z ] [ z ] [ 0 ] new 3</p>
     *
     * <p>The function returns a set of shears in the shear struct type.</p>
     *
     * <p>The C code for generating the parameters (f,bx2,cx2,az,bz,ay,cy,bx1,cx1,dx,dy,cz) was generated by the
     * following Maple V Release 4 script:</p>
     *
     * <p>with(linalg) : readlib(C) :</p>
     *
     * <p>Sx := (f,b,c,dx) -> matrix( [ [f,b,c,dx], [0,1,0,0 ], [0,0,1,0 ], [0,0,0,1] ] ) : Sy := (f,a,c,dy) -> matrix(
     * [ [1,0,0,0 ], [a,f,c,dy], [0,0,1,0 ], [0,0,0,1] ] ) : Sz := (f,a,b,dz) -> matrix( [ [1,0,0,0 ], [0,1,0,0 ],
     * [a,b,f,dz], [0,0,0,1] ] ) :</p>
     *
     * <p>SS := evalm( Sx(f,bx2,cx2,dx) &* Sz(f,az,bz,dz) &* Sy(f,ay,cy,dy) &* Sx(1,bx1,cx1,0) ) :</p>
     *
     * <p>QQ := matrix( [ [q11,q12,q13,xdel], [q21,q22,q23,ydel], [q31,q32,q33,zdel], [0,0,0,1] ] ) :</p>
     *
     * <p>ee := { seq( SS[i,1]-QQ[i,1] , i=1..3 ) , seq( SS[i,2]-QQ[i,2] , i=1..3 ) , seq( SS[i,3]-QQ[i,3] , i=1..3 ) ,
     * seq( SS[i,4]-QQ[i,4] , i=1..3 ) } :</p>
     *
     * <p>vv := { f,bx2,cx2,az,bz,ay,cy,bx1,cx1,dx,dy,dz } :</p>
     *
     * <p>s1 := solve( ee ,vv ) : s2 := map( x -> convert(x,radical) , s1 ) : ss := [ op(s2) ] : C(ss,optimized) ;</p>
     *
     * @param   q       DOCUMENT ME!
     * @param   xyzdel  DOCUMENT ME!
     *
     * @return  returned shear
     */

    private shear shear_xzyx(double[][] q, double[] xyzdel) {

        /* input variables */

        double q11, q12, q13, q21, q22, q23, q31, q32, q33, xdel, ydel, zdel;

        /* computed parameters */

        double f, bx2, cx2, az, bz, ay, cy, bx1, cx1, dx, dy, dz;

        /* output variable */

        shear shr;

        /* internals (created by Maple) */

        double t1, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t15, t16, t17, t18, t19, t20, t22, t23, t24, t25,
               t26, t27, t28, t29, t30, t32, t34, t35, t36, t37, t38, t44, t45, t47, t50, t51, t53, t54, t55, t57, t61,
               t62, t64, t66, t67, t68, t69, t70, t73, t75, t77, t78, t79, t80, t81, t84, t85, t86, t87, t89, t90, t92,
               t94, t96, t102, t107, t109, t113, t118, t119, t121, t123, t125, t127, t129, t131, t132, t134, t141, t145,
               t148, t150, t151, t157, t160, t163, t164, t167, t185, t190, t193, t194, t195, t203, t206, t207, t210,
               t220, t221, t224, t230, t233, t238, t240, t241, t252, t264, t267, t269, t275, t292;
        int[] ax = new int[4];
        int flip0 = -1;
        ;

        int flip1 = -1;
        ;

        double[][] scl = new double[4][3];
        double[] sft = new double[4];

        /* initialize output to an invalid result */

        shr = new shear(ax, flip0, flip1, scl, sft);
        ax[0] = -1;
        shr.ax = ax;

        /* load inputs into local variables */

        q11 = q[0][0];
        q12 = q[0][1];
        q13 = q[0][2];
        q21 = q[1][0];
        q22 = q[1][1];
        q23 = q[1][2];
        q31 = q[2][0];
        q32 = q[2][1];
        q33 = q[2][2];
        xdel = xyzdel[0];
        ydel = xyzdel[1];
        zdel = xyzdel[2];

        /* the code generated by Maple, slightly massaged */

        ay = q21;
        dy = ydel;
        t1 = q21 * q12;
        t3 = q13 * q22;
        t4 = t3 * q31;
        t5 = q21 * q13;
        t6 = t5 * q32;
        t7 = q23 * q11;
        t8 = t7 * q32;
        t9 = q12 * q23;
        t10 = t9 * q31;
        t11 = q22 * q11;
        t12 = t11 * q33;
        t13 = (t1 * q33) + t4 - t6 + t8 - t10 - t12;
        t15 = q32 * q32;
        t16 = t15 * q32;
        t17 = q21 * q21;
        t18 = t17 * q21;
        t19 = t16 * t18;
        t20 = q22 * q22;
        t22 = q31 * q31;
        t23 = t22 * q32;
        t24 = q21 * t20 * t23;
        t25 = t20 * q22;
        t26 = t22 * q31;
        t27 = t25 * t26;
        t28 = t15 * t17;
        t29 = q22 * q31;
        t30 = t28 * t29;
        t32 = t13 * t13;

        t34 = (-t19 - (3.0 * t24) + t27 + (3.0 * t30)) * t32;

        if (t34 > 0.0) {
            t34 = Math.pow(t34, 0.333333333333333);
        } else if (t34 < 0.0) {
            t34 = -Math.pow(-t34, 0.333333333333333);
        } else {
            t34 = 0.0;
        }

        if (t13 == 0.0) {
            return shr;
        }

        t35 = 1 / t13 * t34;
        t36 = t35 + q31;
        t37 = t36 * q21;
        t38 = q12 * q33;
        t44 = t36 * q23;
        t45 = q11 * q32;
        t47 = t36 * q12;
        t50 = t36 * q22;
        t51 = q11 * q33;
        t53 = q32 * t17;
        t54 = t53 * q12;
        t55 = q32 * q21;
        t57 = q32 * q31;
        t61 = q32 * q23 * q11 * q31;
        t62 = q31 * q21;
        t64 = q22 * q12;
        t66 = t22 * q23;
        t67 = t66 * q12;
        t68 = t22 * q13;
        t69 = t68 * q22;
        t70 = t29 * t51;
        t73 = (-t37 * t38) - (t36 * q13 * t29) + (t37 * q13 * q32) - (t44 * t45) + (t47 * q23 * q31) + (t50 * t51) +
              t54 - (t55 * t11) - (t57 * t5) + t61 + (t62 * t38) - (t62 * t64) - t67 + t69 - t70 + (q31 * t20 * q11);
        t75 = t20 * t22;

        t77 = (t28 - (2.0 * t29 * t55) + t75);

        if (t77 == 0.0) {
            return shr;
        }

        t77 = 1 / t77;

        cx2 = t73 * t77;
        t78 = t44 * q31;
        t79 = t62 * q22;
        t80 = t62 * q33;
        t81 = t37 * q33;
        t84 = t34 * t34;

        if (t84 == 0.0) {
            return shr;
        }

        t85 = 1 / t84;

        cy = (-t78 + t79 - t80 + t81 - t53 + t66) * t32 * t85;
        t86 = q21 * t22;
        t87 = t64 * t36;
        t89 = t17 * q12;
        t90 = t89 * t36;
        t92 = t51 * t22;
        t94 = t68 * q32;
        t96 = t36 * t36;
        t102 = t51 * q31;
        t107 = t11 * t36;
        t109 = t38 * t22;
        t113 = (t86 * t87) - (t57 * t90) + (2.0 * t50 * t92) + (2.0 * t37 * t94) + (t96 * t22 * t3) + (t96 * q31 * t8) -
               (3.0 * t24) - (t96 * q22 * t102) + (3.0 * t30) + t27 - (2.0 * t36 * t26 * t3) - t19 -
               (t62 * q32 * t107) - (2.0 * t37 * t109) - (t26 * q21 * t64);
        t118 = q32 * q22;
        t119 = t118 * q11;
        t121 = q11 * t36;
        t123 = t26 * q13;
        t125 = t26 * q23;
        t127 = q33 * t26;
        t129 = t96 * q12;
        t131 = t96 * q21;
        t132 = t38 * q31;
        t134 = t22 * t22;
        t141 = q31 * q13 * q32;
        t145 = (-q11 * t15 * t17 * q31) + (t23 * t89) + (t86 * t119) + (t121 * t28) - (t123 * t55) + (t125 * t45) +
               (t1 * t127) - (t129 * t66) + (t131 * t132) + (t134 * q13 * q22) - (t9 * t134) - (2.0 * t36 * t22 * t8) -
               (t131 * t141) - (t11 * t127) + (2.0 * t47 * t125);

        if (t34 == 0.0) {
            return shr;
        }

        t148 = 1 / t34;

        if (q21 == 0.0) {
            return shr;
        }

        t150 = 1 / q21;

        t151 = t148 * t77 * t150;
        bx2 = (t113 + t145) * t13 * t151;
        az = -t35;
        f = (-t29 + t55) * t13 * t148;
        t157 = ydel * q12;
        t160 = zdel * t17;
        t163 = ydel * t22;
        t164 = t163 * q21;
        t167 = ydel * t26;
        t185 = xdel * q21;
        t190 = ydel * q11;
        t193 = (-ydel * q22 * t51 * t26) - (t157 * q23 * t134) - (t160 * t129 * q33) + (t164 * t119) + (t163 * t54) -
               (t167 * q21 * q22 * q12) + (ydel * t134 * t3) + (t157 * q21 * q33 * t26) - (t167 * t6) -
               (3.0 * ydel * q21 * t75 * q32) + (t167 * t8) + (3.0 * ydel * t15 * t17 * q22 * q31) +
               (t185 * t20 * t26) - (ydel * t16 * t18) - (t190 * t28 * q31);
        t194 = zdel * q21;
        t195 = t125 * q12;
        t203 = xdel * t18;
        t206 = xdel * t17;
        t207 = q22 * t22;
        t210 = t160 * q32;
        t220 = zdel * t18;
        t221 = q32 * q12;
        t224 = t123 * q22;
        t230 = t194 * t96;
        t233 = (t194 * t195) + (t194 * t22 * t12) + (t160 * t94) - (t194 * q32 * t7 * t22) + (t203 * q31 * t15) -
               (2.0 * t206 * t207 * q32) - (t210 * t107) - (t194 * t75 * q11) + (t194 * q31 * t20 * q11 * t36) +
               (t210 * t11 * q31) - (t220 * t221 * q31) - (t194 * t224) + (t160 * t207 * q12) + (ydel * t25 * t26) -
               (t230 * t8) - (t160 * t109);
        t238 = t194 * t36;
        t240 = ydel * t96;
        t241 = t240 * q21;
        t252 = ydel * t36;
        t264 = (-t203 * t36 * t15) + (t230 * t12) + (2.0 * t238 * t61) - (t241 * t141) - (t240 * q22 * t102) +
               (t220 * t221 * t36) - (t160 * q31 * t87) + (2.0 * t206 * t36 * t29 * q32) - (2.0 * t252 * t22 * t8) +
               (t164 * t87) + (t190 * t36 * t17 * t15) - (t240 * t67) - (2.0 * t252 * t224) + (2.0 * t252 * q22 * t92) +
               (t241 * t132);
        t267 = t160 * t36;
        t269 = ydel * q31;
        t275 = t252 * q21;
        t292 = (-2.0 * t238 * t67) + (t240 * t69) + (2.0 * t267 * t132) - (t269 * q32 * t90) -
               (t185 * t36 * t20 * t22) + (2.0 * t275 * t94) + (t230 * t10) + (2.0 * t238 * t69) + (2.0 * t252 * t195) -
               (t230 * t4) - (2.0 * t275 * t109) - (2.0 * t267 * t141) - (2.0 * t238 * t70) + (t240 * q31 * t8) -
               (t269 * q21 * t118 * t121) + (t160 * t96 * q13 * q32);
        dx = -(t193 + t233 + t264 + t292) * t13 * t151;
        bz = t36 * t150;
        cx1 = -(t78 + t79 - t80 - (t96 * q23) + t81 - t53) * t150 * t32 * t85;
        dz = (-t252 + t194) * t150;
        bx1 = -(-t50 + t55) * t150 * t13 * t148;

        /* load computed values into output structure */

        ax[3] = 0;
        scl[3][0] = f;
        scl[3][1] = bx2;
        scl[3][2] = cx2;
        sft[3] = dx;
        ax[2] = 2;
        scl[2][2] = f;
        scl[2][0] = az;
        scl[2][1] = bz;
        sft[2] = dz;
        ax[1] = 1;
        scl[1][1] = f;
        scl[1][0] = ay;
        scl[1][2] = cy;
        sft[1] = dy;
        ax[0] = 0;
        scl[0][0] = 1;
        scl[0][1] = bx1;
        scl[0][2] = cx1;
        sft[0] = 0;

        flip0 = flip1 = -1; /* no flips now */
        shr = new shear(ax, flip0, flip1, scl, sft);

        return shr;
    }

    /**
     * subtraction of tow 3-element vectors.
     *
     * @param   a  DOCUMENT ME!
     * @param   b  DOCUMENT ME!
     *
     * @return  vector c = a - b
     */
    private double[] sub_dfvec3(double[] a, double[] b) {
        double[] c = new double[3];
        c[0] = a[0] - b[0];
        c[1] = a[1] - b[1];
        c[2] = a[2] - b[2];

        return c;
    }

    /**
     * Rotate and translate a 3D volume.
     *
     * @param  vol  the ModelImage to be rotated and translated
     * @param  th1  roll in radians
     * @param  th2  pitch in radians
     * @param  th3  yaw in radians
     * @param  dx   x translation
     * @param  dy   y translation
     * @param  dz   z translation
     */


    private void THD_rota_vol(ModelImage vol, float th1, float th2, float th3, float dx, float dy, float dz) {
        shear shr;

        // float bot , top ;
        // int   nxyz=nx*ny*nz, ii;
        int[] ax;

        if ((nx < 2) || (ny < 2) || (nz < 2) || (vol == null)) {
            return;
        }

        if (xdel == 0.0f) {
            xdel = 1.0f;
        }

        if (ydel == 0.0f) {
            ydel = 1.0f;
        }

        if (zdel == 0.0f) {
            zdel = 1.0f;
        }

        if ((th1 == 0.0f) && (th2 == 0.0f) && (th3 == 0.0f)) { /* nudge rotation */
            th1 = 1.e-6f;
            th2 = 1.1e-6f;
            th3 = 0.9e-6f;
        }

        Preferences.debug("THD_rota_vol:\n");
        Preferences.debug("th1 = " + th1 + " th2 = " + th2 + " th3 = " + th3 + "\n");
        Preferences.debug("dx = " + dx + " dy = " + dy + " dz = " + dz + "\n");
        Preferences.debug("xdel = " + xdel + " ydel = " + ydel + " zdel = " + zdel + "\n");

        shr = rot_to_shear(-th1, -th2, -th3, dx, dy, dz);

        ax = shr.ax;

        if (ax[0] < 0) {
            cleanup();
            MipavUtil.displayError("THD_rota_vol: can't compute shear transformation!\n");

            setCompleted(false);

            return;
        } /*#ifdef CLIPIT
           * bot = top = vol[0] ; for( ii=1 ; ii < nxyz ; ii++ ){ if( vol[ii] < bot ) bot = vol[ii] ; else if( vol[ii] >
           * top ) top = vol[ii] ; } if( bot >= top ) EXRETURN ;#endif */

        /********************************/ /* 02 Feb 2001: include padding */ {

            if ((rotpx > 0) || (rotpy > 0) || (rotpz > 0)) {

                try {
                    destExtents[0] = nxp;
                    destExtents[1] = nyp;
                    destExtents[2] = nzp;

                    vvv = new ModelImage(vol.getType(), destExtents, vol.getImageName());

                    AlgorithmAddMargins imageMarginsAlgo = new AlgorithmAddMargins(vol, vvv, 0.0, rotpx, rotpy, rotpz,
                                                                                   rotpz);

                    // when using the local-buffer method of the algorithm,  false is default
                    // imageMarginsAlgo.performCopiesWithBuffers(usingBuffer.isSelected());
                    imageMarginsAlgo.performCopiesWithBuffers(false);

                    imageMarginsAlgo.setRunningInSeparateThread(runningInSeparateThread);
                    imageMarginsAlgo.run();
                } catch (OutOfMemoryError oome) {
                    cleanup();
                    MipavUtil.displayError("AddMargins reports: unable to allocate enough memory");

                    setCompleted(false);

                    return;
                }

                apply_3shear(shr, vvv); /*-- do the actual rotation! --*/
            } else {
                apply_3shear(shr, vol); /*-- do the actual rotation! --*/
            }

            shr = null;
            ax = null;

            if (vvv != null) {

                try {
                    destExtents[0] = nx;
                    destExtents[1] = ny;
                    destExtents[2] = nz;

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    int[] xBounds = new int[2];
                    xBounds[0] = rotpx;
                    xBounds[1] = nx + rotpx - 1;

                    int[] yBounds = new int[2];
                    yBounds[0] = rotpy;
                    yBounds[1] = ny + rotpy - 1;

                    int[] zBounds = new int[2];
                    zBounds[0] = rotpz;
                    zBounds[1] = nz + rotpz - 1;

                    AlgorithmCrop cropAlgo = new AlgorithmCrop(vol, vvv, 0, xBounds, yBounds, zBounds);
                    cropAlgo.run();
                } catch (OutOfMemoryError x) {
                    cleanup();
                    MipavUtil.displayError("AlgorithmCrop: unable to allocate enough memory");

                    setCompleted(false);

                    return;
                }

                vvv.disposeLocal();
                vvv = null;
            }
        }

        /********************************/

        /*#ifdef CLIPIT
         * for( ii=0 ; ii < nxyz ; ii++ ){ if( vol[ii] < bot ) vol[ii] = bot ; else if( vol[ii] > top ) vol[ii] = top ;
         * } #endif */

        return;
    }

    /**
     * matrix transpose.
     *
     * @param   A  input matrix return B output transposed matrix
     *
     * @return  DOCUMENT ME!
     */
    private double[][] transpose_dmat(double[][] A) {
        double[][] B = new double[3][3];
        B[0][0] = A[0][0];
        B[1][0] = A[0][1];
        B[2][0] = A[0][2];
        B[0][1] = A[1][0];
        B[1][1] = A[1][1];
        B[2][1] = A[1][2];
        B[0][2] = A[2][0];
        B[1][2] = A[2][1];
        B[2][2] = A[2][2];

        return B;
    }

    /**
     * Calculate ( [ 2 ] ) min ( sum [ {c blurredB(i-dx,j-dy,k-dz) - blurredA(i,j,k)} ] ) c ( ijk [ ] ).
     *
     * <p>where the sum is taken over voxels at least 'edge' in from the edge. 'edge' must be bigger than
     * max(|dx|,|dy|,|dz|).</p>
     *
     * @param   dx    DOCUMENT ME!
     * @param   dy    DOCUMENT ME!
     * @param   dz    DOCUMENT ME!
     * @param   edge  DOCUMENT ME!
     *
     * @return  sum over voxels
     */


    private float voldif(int dx, int dy, int dz, int edge) {
        int nxtop = nx - edge, nytop = ny - edge, nztop = nz - edge, ii, jj, kk;
        float aasum = 0.0f, bbsum = 0.0f, absum = 0.0f, aa, bb;

        for (kk = edge; kk < nztop; kk++) {

            for (jj = edge; jj < nytop; jj++) {

                for (ii = edge; ii < nxtop; ii++) {
                    aa = blurredA[ii + (jj * nx) + (kk * sliceSize)];
                    bb = blurredB[(ii - dx) + ((jj - dy) * nx) + ((kk - dz) * sliceSize)];
                    aasum += aa * aa;
                    bbsum += bb * bb;
                    absum += aa * bb;
                }
            }
        }

        if (bbsum > 0.0) {
            aasum -= absum * absum / bbsum;
        }

        return aasum;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * ------------------------------------------------------------------------ Struct to contain a set of 4 3-shears,
     * used to represent an arbitrary transformation; for i=0..3: 3-shear #i is along axis # ax[i] (0=x, 1=y, 2=z), with
     * scaling parameter scl[i][j] for direction j (j=0,1,2), and shift parameter sft[i] (of course in the ax[i]
     * direction). In addition, a preliminary flipping about two axes may be present. These axes are denoted by flip0
     * and flip1, if flip0 >= 0. --------------------------------------------------------------------------
     */
    private class shear {

        /** DOCUMENT ME! */
        public int ax[], flip0, flip1;

        /** DOCUMENT ME! */
        public double scl[][], sft[];

        /**
         * constructor for shear.
         *
         * @param  ax     DOCUMENT ME!
         * @param  flip0  DOCUMENT ME!
         * @param  flip1  DOCUMENT ME!
         * @param  scl    DOCUMENT ME!
         * @param  sft    DOCUMENT ME!
         */
        public shear(int[] ax, int flip0, int flip1, double[][] scl, double[] sft) {
            this.ax = ax;
            this.flip0 = flip0;
            this.flip1 = flip1;
            this.scl = scl;
            this.sft = sft;
        }

    }

}
