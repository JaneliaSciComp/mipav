package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;


/**
 * <p>XCOSM is an algorithm for Computational Optical Sectioning Microscopy (COSM) algorithms to remove out-of-focus
 * light in 3-D volumes collected plane by plane using either widefield or confocal fluorescence microscopy.</p>
 *
 * <p>References:<br>
 * Available fro the web site: http://www.essrl.wustl.edu/~preza/xcosm/</p>
 *
 * <ol>
 *   <li># J.-A. Conchello, "Super-resolution and convergence properties of the expectation-maximization algorithm for
 *     maximum-likelihood deconvolution of incoherent images", J. Opt. Soc. Am. A, Vol 15, (10), pp. 2609-2620,
 *     1998.</li>
 *   <li>J.-A. Conchello, "Fluorescence photobleaching correction for expectation maximization algorithm" in
 *     Three-Dimensional microscopy: image acquisition and processing T. Wilson and C. J. Cogswell editors, SPIE
 *     2412-21, pp138-146, 1995.</li>
 *   <li>J.-A. Conchello and J. G. McNally "Fast regularization technique for expectation maximization algorithm for
 *     computational optical sectioning microscopy" in Three-Dimensional microscopy: image acquisition and processing C.
 *     J. Cogswell, G. S. Kino, and T. Wilson, editors, SPIE 2655, pp.199-208, 1996.</li>
 *   <li>J. Markham and J.-A. Conchello, "Tradeoffs in regularized maximum-likelihood image restoration" in 3D
 *     Microscopy: Image Acquisition and Processing IV. C. J. Cogswel, J.-A. Conchello, and T. Wilson, editors, SPIE
 *     2984-18,1997.</li>
 * </ol>
 *
 * <p>The code in this file is a direct port of the freely-available <b>C</b> code for the COSM project, linked to the
 * website identified above. The X signifies an X-Windows interface. The code in this file computes a restored volume of
 * the test data, that is also available on the website that is similar to the results of the compiled <b>C</b> code.
 * The differences are small (< 0.01) and may be because of different representations for integer and float data between
 * Java and <b>C</b>, which when iterated yield the small differeces that have been noted.</p>
 *
 * @version  1.0; 31, January 2005
 * @author   Paul F. Hemler, Ph.D.
 */
public class AlgorithmXcosmEM extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** constants. */
    float deltaX = 1.0f;

    /** DOCUMENT ME! */
    float deltaY = 1.0f;

    /** DOCUMENT ME! */
    float deltaZ = 1.0f;

    /** DOCUMENT ME! */
    float[] fwork;

    /** DOCUMENT ME! */
    float[] shiftedwork;

    /** DOCUMENT ME! */
    float[] work;


    /** Number of iterations for different resolutions. */
    private int _1xiter, _2xiter, _4xiter;

    /** Percent iterations at original, one-half, and quater size. */
    private int _1xperc, _2xperc, _4xperc;

    /** Decay constant. */
    private float decay;

    /** DOCUMENT ME! */
    private DecimalFormat df;

    /** value of zero. */
    private float epsilon = 1e-14f;

    /** Estimate decay. */
    private boolean estDecay;

    /** Number of iterations complete before making a backup. */
    private int itBack;

    /** Maximum number of iterations. */
    private int itMax;

    /** Window upper limit in Z. */
    private int origHighZ;

    /** DOCUMENT ME! */
    private ModelImage originalImage;

    /** Original image dimensions. */
    private int origInx, origIny, origInz;


    /** Window lower limit in Z. */
    private int origLowZ;


    /** PSF image dimensions. */
    private int origPx, origPy, origPz;


    /** Result image dimensions. */
    private int origRx, origRy, origRz;

    /** DOCUMENT ME! */
    private ModelImage psfImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage;

    /** Roughness penalty value. */
    private float weightOne = 0.0f;

    /** Intensity penalty value. */
    private float weightZero = 0.0f;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmXcosmEM object.
     */
    public AlgorithmXcosmEM() {

        try {
            jbInit();
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }


    /**
     * Creates a new AlgorithmXcosmEM object.
     *
     * @param  destImg                        DOCUMENT ME!
     * @param  srcImg                         DOCUMENT ME!
     * @param  psfImg                         DOCUMENT ME!
     * @param  lowZ                           DOCUMENT ME!
     * @param  estimateDecay                  DOCUMENT ME!
     * @param  decayConstant                  DOCUMENT ME!
     * @param  percentIterationsOriginalSize  DOCUMENT ME!
     * @param  percentIterationsHalfSize      DOCUMENT ME!
     * @param  percentIterationsQuaterSize    DOCUMENT ME!
     * @param  maxIterations                  DOCUMENT ME!
     * @param  backIterations                 DOCUMENT ME!
     * @param  penaltyIntensity               DOCUMENT ME!
     * @param  penaltyValue                   DOCUMENT ME!
     */
    public AlgorithmXcosmEM(ModelImage destImg, ModelImage srcImg, ModelImage psfImg, int lowZ, boolean estimateDecay,
                            float decayConstant, int percentIterationsOriginalSize, int percentIterationsHalfSize,
                            int percentIterationsQuaterSize, int maxIterations, int backIterations,
                            boolean penaltyIntensity, float penaltyValue) {

        super(destImg, srcImg);

        df = new DecimalFormat("0.00000000");

        originalImage = srcImg;
        psfImage = psfImg;
        resultImage = destImg;

        int[] imgExtents = originalImage.getExtents();
        origInx = imgExtents[0];
        origIny = imgExtents[1];
        origInz = imgExtents[2];

        int[] psfExtents = psfImage.getExtents();
        origPx = psfExtents[0];
        origPy = psfExtents[1];
        origPz = psfExtents[2];

        int[] resultExtents = resultImage.getExtents();
        origRx = resultExtents[0];
        origRy = resultExtents[1];
        origRz = resultExtents[2];

        if ((origPx < origInx) || (origPy < origIny)) {
            MipavUtil.displayError("PSF must have NX >= Input NX, and NY >= Input NY)");
        }

        if (origRz < origInz) {
            MipavUtil.displayError("Output NZ must be >= Input Nz");
        }

        if ((origPz > origRz) || (origPx > origRx) || (origPy > origRy)) {
            MipavUtil.displayError("PSF must have dimensions <= output dimensions");
        }

        origLowZ = lowZ;
        origHighZ = origLowZ + origInz - 1;

        if (origHighZ > origRz) {
            MipavUtil.displayError("Window lower limit in Z exceeds maximum");
        }

        estDecay = estimateDecay;
        decay = decayConstant;
        _1xperc = percentIterationsOriginalSize;
        _2xperc = percentIterationsHalfSize;
        _4xperc = percentIterationsQuaterSize;
        itMax = maxIterations;
        itBack = backIterations;

        if (penaltyIntensity == true) {
            weightZero = penaltyValue;
        } else {
            weightOne = penaltyValue;
        }

        if ((_1xperc + _2xperc + _4xperc) != 100) {
            MipavUtil.displayError("AlgorithmXcosmEM.  1x, 1/2x, and 1/4x percentages do NOT add up to 100");

            return;
        }

        // Calculate number of iterations for each (sub-)dimension
        _1xiter = (int) ((((float) _1xperc) / 100.0 * (float) itMax) + 0.5);
        _2xiter = (int) ((((float) _2xperc) / 100.0 * (float) itMax) + 0.5);
        _4xiter = (int) ((((float) _4xperc) / 100.0 * (float) itMax) + 0.5);

        if ((_1xiter + _2xiter + _4xiter) != itMax) {
            _1xiter += (itMax - (_1xiter + _2xiter + _4xiter));
        }

        try {
            work = new float[16383];
            fwork = new float[1026];
        } catch (OutOfMemoryError er) {
            work = fwork = null;
            errorCleanUp("AlgorithmXcosmEM: Out of memory when creating work buffers", true);

            return;
        } // end try{}-catch{}
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {
        originalImage = null;
        psfImage = null;
        resultImage = null;
        System.gc();
    }


    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }


    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (originalImage == null) {
            MipavUtil.displayError("AlgorithmXcosmEM.runAlgorithm()  Source Image is null");

            return;
        }

        if (originalImage.getNDims() == 2) {
            MipavUtil.displayError("AlgorithmXcosmEM.runAlgorithm()  Source Image must be 3D");

            return;
        }

        // 

        if (originalImage.isColorImage()) {
            MipavUtil.displayError("AlgorithmXcosmEM.runAlgorithm()  Source Image must be GrayScale");

            return;
        }

        run3D();
    }


    /**
     * AddGamma: Add 1.0 - (plane#)*decay to 'array' over (Inx,Iny,Inz = [lowz..highz]).
     *
     * @param  array   DOCUMENT ME!
     * @param  nx1     DOCUMENT ME!
     * @param  ny1     DOCUMENT ME!
     * @param  Inx     DOCUMENT ME!
     * @param  Iny     DOCUMENT ME!
     * @param  zstart  DOCUMENT ME!
     * @param  zend    DOCUMENT ME!
     */
    void addGamma(float[] array, int nx1, int ny1, int Inx, int Iny, int zstart, int zend) {
        float factor, ftmp;
        int ix, iy, iz, pl_size, deltx, idx;

        if (decay == 1.0) {
            return;
        }

        /* allocated single plane size for array */
        pl_size = nx1 * ny1;
        deltx = nx1 - Inx;

        /* loop for each plane that overlaps with raw image */
        factor = 1.0f;

        for (iz = zstart; iz <= zend; iz++) {
            idx = iz * pl_size;
            factor *= decay;
            ftmp = 1.0f - factor;

            for (iy = 0; iy < Iny; iy++, idx += deltx) {

                for (ix = 0; ix < Inx; ix++, idx++) {
                    array[idx] += ftmp;
                }
            }
        } // end for (iz = zstart; ...)
    } // end for addGamma(...)


    /**
     * DOCUMENT ME!
     *
     * @param   psf  DOCUMENT ME!
     * @param   nx   DOCUMENT ME!
     * @param   ny   DOCUMENT ME!
     * @param   nz   DOCUMENT ME!
     * @param   Rx   DOCUMENT ME!
     * @param   Ry   DOCUMENT ME!
     * @param   Rz   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    float[] calcOTF(float[] psf, int nx, int ny, int nz, int Rx, int Ry, int Rz) {
        int TwoRz = 2 * Rz;

        float[] retotf;

        try {
            retotf = new float[(Rx + 2) * (Ry + 2) * TwoRz];
        } catch (OutOfMemoryError er) {
            retotf = null;
            errorCleanUp("AlgorithmXcosmEM: Out of memory when creating otf buffer", true);

            return null;
        } // end try{}-catch{}

        int nxo2 = nx / 2; // half nx pf psf
        int nyo2 = ny / 2; // half ny of psf
        int nzo2 = nz / 2; // half nz of psf
        int iplSize = nx * ny; // psf 2D slice size
        int oplSize = (Rx + 2) * (Ry + 2); // otf 2D slice size
        int offsp = nzo2 * iplSize; // Offset to 2nd half of psf
        int offso = (TwoRz - nzo2) * oplSize; // Offset to last part of otf

        // where 2nd half of psf goes

        // copy two planes at a time (top and bottom)
        int p1, p2, o1, o2;

        for (int iz = 0; iz < nzo2; iz++) {
            p1 = iz * iplSize; // top psf plane to copy from
            o1 = iz * oplSize; // top otf plane to copy to
            p2 = p1 + offsp; // bottom psf plane to copy from
            o2 = o1 + offso; // bottom otf plane to copy to

            for (int iy = 0; iy < nyo2; iy++) {

                for (int ix = 0; ix < nxo2; ix++) {

                    // Top plane: upper left
                    retotf[o1 + ix + (iy * (Rx + 2))] = psf[p1 + ix + (iy * nx)];

                    // Top plane: upper right
                    retotf[o1 + Rx - ix - 1 + (iy * (Rx + 2))] = psf[p1 + nx - ix - 1 + (iy * nx)];

                    // Top plane: lower left
                    retotf[o1 + ix + ((Ry - iy - 1) * (Rx + 2))] = psf[p1 + ix + ((ny - iy - 1) * nx)];

                    // Top plane: lower right
                    retotf[o1 + Rx - ix - 1 + ((Ry - iy - 1) * (Rx + 2))] = psf[p1 + nx - ix - 1 + ((ny - iy - 1) * nx)];

                    // Bottom Plane: upper left
                    retotf[o2 + ix + (iy * (Rx + 2))] = psf[p2 + ix + (iy * nx)];

                    // Bottom Plane: upper right
                    retotf[o2 + Rx - ix - 1 + (iy * (Rx + 2))] = psf[p2 + nx - ix - 1 + (iy * nx)];

                    // Bottom Plane: lower left
                    retotf[o2 + ix + ((Ry - iy - 1) * (Rx + 2))] = psf[p2 + ix + ((ny - iy - 1) * nx)];

                    // Bottom Plane: lower right
                    retotf[o2 + Rx - ix - 1 + ((Ry - iy - 1) * (Rx + 2))] = psf[p2 + nx - ix - 1 + ((ny - iy - 1) * nx)];

                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)
        } // end for (iz = 0; ... )

        /*
         *                int[] exts;               exts = new int[3];               exts[0] = Rx + 2; exts[1] = Ry + 2;
         *               exts[2] = TwoRz; ModelImage otf = new ModelImage(ModelImage.FLOAT, exts, "mypsf",
         * destImage.getUserInterface());               try {                   otf.importData(0, retotf, true);
         *       } catch (IOException error) {                   retotf = null; errorCleanUp("AlgorithmXcosmEM: could
         * NOT import otf image", true);                   return null;   } // end try{}-catch{}
         *
         * FileIO fio = new FileIO(destImage.getUserInterface()); FileWriteOptions fileOptions = new
         * FileWriteOptions("mypsf.wu", "C:/", true);               fio.writeImage(otf, fileOptions);
         */

        // code correct to here (1/14/05)
        real_fft3d(retotf, Rx, Ry, TwoRz, Rx + 2, Ry + 2, 1);
        // code correct to here (1/14/05)

        /*
         *      int[] exts;     exts = new int[3];     exts[0] = Rx + 2;     exts[1] = Ry + 2;     exts[2] = TwoRz;
         * ModelImage otf = new ModelImage(ModelImage.FLOAT, exts, "mypsf", destImage.getUserInterface());     try {
         *     otf.importData(0, retotf, true);     } catch (IOException error) {         retotf = null;
         * errorCleanUp("AlgorithmXcosmEM: could NOT import otf image", true); return null;     } // end try{}-catch{}
         *
         * FileIO fio = new FileIO(destImage.getUserInterface());     FileWriteOptions fileOptions = new
         * FileWriteOptions("myfftJ.wu", "C:/Documents and Settings/phemler/My
         * Documents/Research/xcosm/myBuild/currBuild/", true);     fio.writeImage(otf, fileOptions);
         */

        // Normalize the OTF by the DC component at location (0, 0, 0)
        float cHx = retotf[0];
        float cHy = retotf[1];
        float Hooo = (float) Math.sqrt((cHx * cHx) + (cHy * cHy));

        if (Hooo > 10E-30) {

            // Divide the complex OTF array by the complex value (H000, 0.0)
            int x, y, z, plSize = (Rx + 2) * (Ry + 2), offsetZ, offset;
            float scaleVal = 1.0f / (nx * ny * nz / 2.0f);

            for (z = 0; z < nz; z++) {
                offsetZ = z * plSize;

                for (y = 0; y < ny; y++) {
                    offset = (y * (Ry + 1)) + offsetZ;

                    for (x = 0; x < nx; x++) {
                        retotf[x + offset] /= Hooo;
                    } // end for (x = 0; ...)
                } // end for (y = 0; ...)
            } // end for (z = 0; ...)
        } else {
            System.out.println("Hooo is too small to normalize the OTF");
        }

        return retotf;
    } // end calcOTF(...)


    /**
     * DOCUMENT ME!
     *
     * @param  array1  DOCUMENT ME!
     * @param  nx1     DOCUMENT ME!
     * @param  ny1     DOCUMENT ME!
     * @param  array2  DOCUMENT ME!
     * @param  nx2     DOCUMENT ME!
     * @param  ny2     DOCUMENT ME!
     * @param  nx      DOCUMENT ME!
     * @param  ny      DOCUMENT ME!
     * @param  nz      DOCUMENT ME!
     */
    void cmultiply(float[] array1, int nx1, int ny1, float[] array2, int nx2, int ny2, int nx, int ny, int nz) {
        int pl_size1 = 2 * (nx1 * ny1);
        int pl_size2 = 2 * (nx2 * ny2);
        int delt1 = 2 * (nx1 - nx); // 2 compensates for the fcomplex type in the C code
        int delt2 = 2 * (nx2 - nx); // 2 compensates for the fcomplex type in the C code
        int ix, iy, iz;
        int offset1, offset2;
        float re, im;

        for (iz = 0; iz < nz; iz++) {
            offset1 = iz * pl_size1;
            offset2 = iz * pl_size2;

            for (iy = 0; iy < ny; iy++, offset1 += delt1, offset2 += delt2) {

                for (ix = 0; ix < nx; ix++, offset1 += 2, offset2 += 2) {
                    re = (array1[offset1] * array2[offset2]) - (array1[offset1 + 1] * array2[offset2 + 1]);
                    im = (array1[offset1] * array2[offset2 + 1]) + (array1[offset1 + 1] * array2[offset2]);
                    array1[offset1] = re;
                    array1[offset1 + 1] = im;
                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)
        } // end for (iz = 0; ...)

    } // end cmultiply (...)

    /**
     * (a + jb) * (c - jd) = a*c + bd + j(bc - ad).
     *
     * @param  array1  DOCUMENT ME!
     * @param  nx1     DOCUMENT ME!
     * @param  ny1     DOCUMENT ME!
     * @param  array2  DOCUMENT ME!
     * @param  nx2     DOCUMENT ME!
     * @param  ny2     DOCUMENT ME!
     * @param  nx      DOCUMENT ME!
     * @param  ny      DOCUMENT ME!
     * @param  nz      DOCUMENT ME!
     */
    void cmultiplyConjg(float[] array1, int nx1, int ny1, float[] array2, int nx2, int ny2, int nx, int ny, int nz) {
        int pl_size1 = 2 * (nx1 * ny1);
        int pl_size2 = 2 * (nx2 * ny2);
        int delt1 = 2 * (nx1 - nx); // 2 compensates for the fcomplex type in the C code
        int delt2 = 2 * (nx2 - nx); // 2 compensates for the fcomplex type in the C code
        int ix, iy, iz;
        int offset1, offset2;
        float re, im;

        for (iz = 0; iz < nz; iz++) {
            offset1 = iz * pl_size1;
            offset2 = iz * pl_size2;

            for (iy = 0; iy < ny; iy++, offset1 += delt1, offset2 += delt2) {

                for (ix = 0; ix < nx; ix++, offset1 += 2, offset2 += 2) {

                    re = array1[offset1] * array2[offset2];
                    re += (array1[offset1 + 1] * array2[offset2 + 1]);
                    im = array1[offset1 + 1] * array2[offset2];
                    im -= (array1[offset1] * array2[offset2 + 1]);
                    array1[offset1] = re;
                    array1[offset1 + 1] = im;

                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)
        } // end for (iz = 0; ...)
    } // end cmultiplyConjg (...)


    /**
     * DOCUMENT ME!
     *
     * @param  array1     DOCUMENT ME!
     * @param  nx1        DOCUMENT ME!
     * @param  ny1        DOCUMENT ME!
     * @param  array2     DOCUMENT ME!
     * @param  array2Idx  DOCUMENT ME!
     * @param  nx2        DOCUMENT ME!
     * @param  ny2        DOCUMENT ME!
     * @param  nx         DOCUMENT ME!
     * @param  ny         DOCUMENT ME!
     * @param  nz         DOCUMENT ME!
     */
    void copyArrays(float[] array1, int nx1, int ny1, float[] array2, int array2Idx, int nx2, int ny2, int nx, int ny,
                    int nz) {
        int ix, iy, iz, pl_size1, pl_size2, deltx1, deltx2;
        int idx1, idx2;

        pl_size1 = nx1 * ny1;
        pl_size2 = nx2 * ny2;
        deltx1 = nx1 - nx;
        deltx2 = nx2 - nx;

        for (iz = 0; iz < nz; iz++) {
            idx1 = iz * pl_size1;
            idx2 = array2Idx + (iz * pl_size2);

            for (iy = 0; iy < ny; iy++, idx1 += deltx1, idx2 += deltx2) {

                for (ix = 0; ix < nx; ix++, idx1++, idx2++) {

                    if (idx2 == 0) {
                        System.out.println("idx2 is 0");
                    }

                    array2[idx2] = array1[idx1];
                    // System.out.println("idx1: " + idx1 + "  idx2: " + idx2);
                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)
        } // end for (iz = 0; ...)
    } // end copyArrays (...)


    /**
     * DOCUMENT ME!
     *
     * @param   array1     DOCUMENT ME!
     * @param   nx1        DOCUMENT ME!
     * @param   ny1        DOCUMENT ME!
     * @param   array2     DOCUMENT ME!
     * @param   array2Idx  DOCUMENT ME!
     * @param   nx2        DOCUMENT ME!
     * @param   ny2        DOCUMENT ME!
     * @param   nx         DOCUMENT ME!
     * @param   ny         DOCUMENT ME!
     * @param   nz         DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    float emError2(float[] array1, int nx1, int ny1, float[] array2, int array2Idx, int nx2, int ny2, int nx, int ny,
                   int nz) {
        int ix, iy, iz, pl_size1, pl_size2, deltx1, deltx2;
        int idx1, idx2;
        float errorVal, eVal;
        float globalError = -1e30f;

        pl_size1 = nx1 * ny1;
        pl_size2 = nx2 * ny2;
        deltx1 = nx1 - nx;
        deltx2 = nx2 - nx;

        errorVal = -1e30f;

        for (iz = 0; iz < nz; iz++) {
            idx1 = iz * pl_size1;
            idx2 = array2Idx + (iz * pl_size2);
            errorVal = -1.0f;

            for (iy = 0; iy < ny; iy++, idx1 += deltx1, idx2 += deltx2) {

                for (ix = 0; ix < nx; ix++, idx1++, idx2++) {
                    eVal = array1[idx1] - array2[idx2];

                    if (eVal < 0.0f) {
                        eVal = -eVal;
                    } // end if (eVal < 0.0f)

                    if (eVal > errorVal) {
                        errorVal = eVal;
                    } // end if (eVal < errorVal)
                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)

            if (errorVal > globalError) {
                globalError = errorVal;
            } // end if (errorVal > globalError)
        } // end for (iz = 0; ...)

        return globalError;
    } // end emError2 (...)


    /**
     * EMRatio :: Calculate ratio of arrays: array1 = array2/array1 Allocated array sizes: array1 : nx1,ny1,nz1 array2 :
     * nx2,ny2,* Perform ratio over nx,ny,(nz = [zstart,zend]) (set array1 = 1.0 where no ratio calculated).
     *
     * @param  array1  DOCUMENT ME!
     * @param  nx1     DOCUMENT ME!
     * @param  ny1     DOCUMENT ME!
     * @param  nz1     DOCUMENT ME!
     * @param  array2  DOCUMENT ME!
     * @param  nx2     DOCUMENT ME!
     * @param  ny2     DOCUMENT ME!
     * @param  nx      DOCUMENT ME!
     * @param  ny      DOCUMENT ME!
     * @param  zstart  DOCUMENT ME!
     * @param  zend    DOCUMENT ME!
     */

    void EMRatio(float[] array1, int nx1, int ny1, int nz1, float[] array2, int nx2, int ny2, int nx, int ny,
                 int zstart, int zend) {

        int k, ix, iy, iz, pl_size1, pl_size2, deltx1, deltx2;
        int idx1, idx2, idx3;

        float epsilon = (float) 1E-4;

        // Ok to here: 1/21/05
        /*
         *      int[] exts;     exts = new int[3];     exts[0] = nx1;     exts[1] = ny1;     exts[2] = 2*nz1;
         * ModelImage myModelImage = new ModelImage(ModelImage.FLOAT, exts,
         * "myarray",                                              destImage.getUserInterface());     try {
         * myModelImage.importData(0, array1, true);     } catch (IOException error) {         myModelImage = null;
         *    errorCleanUp("AlgorithmXcosmEM: could NOT import array1 image", true);         return;     } // end
         * try{}-catch{}
         *
         *   FileIO fio = new FileIO(destImage.getUserInterface());     FileWriteOptions fileOptions = new
         * FileWriteOptions("myarray1J.wu", "C:/Documents and Settings/phemler/My
         * Documents/Research/xcosm/myBuild/currBuild/", true);     fio.writeImage(myModelImage, fileOptions);
         */

        pl_size1 = nx1 * ny1;
        pl_size2 = nx2 * ny2;
        deltx1 = nx1 - nx;
        deltx2 = nx2 - nx;

        k = pl_size1 * (nz1);

        /* Copy original array1 to last half */
        copyArrays(array1, nx1, ny1, array1, k, nx1, ny1, nx1, ny1, nz1);

        // Ok to here: 1/21/05
        /*
         *     int[] exts;    exts = new int[3];    exts[0] = nx1;    exts[1] = ny1;    exts[2] = 2*nz1;    ModelImage
         * myModelImage = new ModelImage(ModelImage.FLOAT, exts,                                             "myarray",
         *                                          destImage.getUserInterface());    try { myModelImage.importData(0,
         * array1, true);    } catch (IOException error) {        myModelImage = null; errorCleanUp("AlgorithmXcosmEM:
         * could NOT import array1 image", true);        return;    } // end try{}-catch{}
         *
         * FileIO fio = new FileIO(destImage.getUserInterface());    FileWriteOptions fileOptions = new
         * FileWriteOptions("myarray1J.wu", "C:/Documents and Settings/phemler/My
         * Documents/Research/xcosm/myBuild/currBuild/", true);    fio.writeImage(myModelImage, fileOptions);
         */

        /* Set array1 to 1.0 on first half, this is for result array */
        setConstant(array1, nx1, ny1, nx1, ny1, nz1, 1.0f);

        // Ok to here: 1/21/05
        /*
         *      int[] exts;     exts = new int[3];     exts[0] = nx1;     exts[1] = ny1;     exts[2] = 2 * nz1;
         * ModelImage myModelImage = new ModelImage(ModelImage.FLOAT, exts,
         * "myarray",                                              destImage.getUserInterface());     try {
         * myModelImage.importData(0, array1, true);     } catch (IOException error) {         myModelImage = null;
         *    errorCleanUp("AlgorithmXcosmEM: could NOT import array1 image", true);         return;     } // end
         * try{}-catch{}
         *
         *   FileIO fio = new FileIO(destImage.getUserInterface());     FileWriteOptions fileOptions = new
         * FileWriteOptions("myarray1J.wu", "C:/Documents and Settings/phemler/My
         * Documents/Research/xcosm/myBuild/currBuild/", true);     fio.writeImage(myModelImage, fileOptions);
         */

        for (iz = zstart; iz <= zend; iz++) {
            idx1 = iz * pl_size1;
            idx3 = k + (iz * pl_size1);
            idx2 = (iz - zstart) * pl_size2;

            for (iy = 0; iy < ny; iy++, idx1 += deltx1, idx2 += deltx2, idx3 += deltx1) {

                for (ix = 0; ix < nx; ix++, idx1++, idx2++, idx3++) {

                    if (array1[idx3] < epsilon) {
                        array1[idx3] = epsilon;
                    }

                    array1[idx1] = array2[idx2] / array1[idx3];
                } // end for (ix = 0; ...)
            } // end for(iy = 0; ...)
        } // end for(iz = zstart; ...)

        // Ok to here: 1/21/05

        /*
         *      int[] exts;     exts = new int[3];     exts[0] = nx1;     exts[1] = ny1;     exts[2] = 2 * nz1;
         * ModelImage myModelImage = new ModelImage(ModelImage.FLOAT, exts,
         * "myarray",                                              destImage.getUserInterface());     try {
         * myModelImage.importData(0, array1, true);     } catch (IOException error) {         myModelImage = null;
         *    errorCleanUp("AlgorithmXcosmEM: could NOT import array1 image", true);         return;     } // end
         * try{}-catch{}
         *
         *   FileIO fio = new FileIO(destImage.getUserInterface());     FileWriteOptions fileOptions = new
         * FileWriteOptions("myarray1J.wu", "C:/Documents and Settings/phemler/My
         * Documents/Research/xcosm/myBuild/currBuild/", true);     fio.writeImage(myModelImage, fileOptions);
         */

    } // end EMRatio (...)


    /**
     * DOCUMENT ME!
     *
     * @param  nn     DOCUMENT ME!
     * @param  direc  DOCUMENT ME!
     */
    void fftl(int nn, int direc) {
        int m, n, j, i, mmax, istep;
        double theta, wtemp, wr, wpr, wi, wpi;
        float tempr, tempi;

        n = nn << 1;

        j = 1;

        for (i = 1; i < n; i += 2) {

            // System.out.println("i: " + i + "  j: " + j);
            if (j > i) {

                // SWAP(work[j], work[i]);
                tempr = work[j - 1];
                work[j - 1] = work[i - 1];
                work[i - 1] = tempr;

                // SWAP(work[j + 1], work[i + 1]);
                tempr = work[j];
                work[j] = work[i];
                work[i] = tempr;
            }

            m = n >> 1;

            while ((m >= 2) && (j > m)) {
                j -= m;
                m >>= 1;
            }

            j += m;
        } // end for (i = 1; ...)

        // for (i = 0; i < n; i++)
        // System.out.println("work[" + i + "]: " + shiftedwork[i]);

        mmax = 2;

        while (n > mmax) {
            istep = 2 * mmax;
            theta = 6.28318530717959 / (direc * mmax);
            wtemp = Math.sin(0.5 * theta);
            wpr = -2.0 * wtemp * wtemp;
            wpi = Math.sin(theta);
            wr = 1.0;
            wi = 0.0;

            for (m = 1; m < mmax; m += 2) {

                for (i = m; i <= n; i += istep) {
                    j = i + mmax;
                    tempr = (float) ((wr * work[j - 1]) - (wi * work[j]));
                    tempi = (float) ((wr * work[j]) + (wi * work[j - 1]));
                    work[j - 1] = work[i - 1] - tempr;
                    work[j] = work[i] - tempi;
                    work[i - 1] += tempr;
                    work[i] += tempi;
                } // end for (i = m; ...)

                wtemp = wr;
                wr = (wtemp * wpr) - (wi * wpi) + wr;
                wi = (wi * wpr) + (wtemp * wpi) + wi;
            } // end for (m = 1; ...)

            mmax = istep;
        } // end while(n > mmax)

    } // end fftl(...)


    /**
     * DOCUMENT ME!
     *
     * @param  nn     DOCUMENT ME!
     * @param  direc  DOCUMENT ME!
     */
    void fftlOrig(int nn, int direc) {
        int m, n, j, i, mmax, istep;
        double theta, wtemp, wr, wpr, wi, wpi;
        float tempr, tempi;

        n = nn << 1;

        for (i = 0; i < n; i++) {
            shiftedwork[i + 1] = work[i];
        }

        j = 1;

        for (i = 1; i < n; i += 2) {

            // System.out.println("i: " + i + "  j: " + j);
            if (j > i) {

                // SWAP(shiftedwork[j], shiftedwork[i]);
                tempr = shiftedwork[j];
                shiftedwork[j] = shiftedwork[i];
                shiftedwork[i] = tempr;

                // SWAP(shiftedwork[j + 1], shiftedwork[i + 1]);
                tempr = shiftedwork[j + 1];
                shiftedwork[j + 1] = shiftedwork[i + 1];
                shiftedwork[i + 1] = tempr;
            }

            m = n >> 1;

            while ((m >= 2) && (j > m)) {
                j -= m;
                m >>= 1;
            }

            j += m;
        } // end for (i = 1; ...)

        // for (i = 0; i < n; i++)
        // System.out.println("work[" + i + "]: " + shiftedwork[i]);

        mmax = 2;

        while (n > mmax) {
            istep = 2 * mmax;
            theta = 6.28318530717959 / (direc * mmax);
            wtemp = Math.sin(0.5 * theta);
            wpr = -2.0 * wtemp * wtemp;
            wpi = Math.sin(theta);
            wr = 1.0;
            wi = 0.0;

            for (m = 1; m < mmax; m += 2) {

                for (i = m; i <= n; i += istep) {
                    j = i + mmax;
                    tempr = (float) ((wr * shiftedwork[j]) - (wi * shiftedwork[j + 1]));
                    tempi = (float) ((wr * shiftedwork[j + 1]) + (wi * shiftedwork[j]));
                    shiftedwork[j] = shiftedwork[i] - tempr;
                    shiftedwork[j + 1] = shiftedwork[i + 1] - tempi;
                    shiftedwork[i] += tempr;
                    shiftedwork[i + 1] += tempi;
                } // end for (i = m; ...)

                wtemp = wr;
                wr = (wtemp * wpr) - (wi * wpi) + wr;
                wi = (wi * wpr) + (wtemp * wpi) + wi;
            } // end for (m = 1; ...)

            mmax = istep;
        } // end while(n > mmax)

        for (i = 0; i < n; i++) {
            work[i] = shiftedwork[i + 1];
        }

    } // end fftl(...)

    /**
     * DOCUMENT ME!
     *
     * @param  n      DOCUMENT ME!
     * @param  direc  DOCUMENT ME!
     */
    void fftr(int n, int direc) {
        float c1 = 0.5f;
        float c2;
        double theta = 3.141592653589793 / (double) n;

        if (direc == 1) {
            c2 = -0.5f;

            // for (int i1 = 0; i1 < n; i1++) {
            // System.out.println("wk[" + i1 + "]: " + work[i1]);
            // }

            fftl(n, 1);

            // for (int i1 = 0; i1 < n; i1++) {
            // System.out.println("wk[" + i1 + "]: " + work[i1]);
            // }

        } else {
            c2 = 0.5f;
            theta = -theta;
        }

        int i;
        double wtemp = Math.sin(0.5 * theta);
        double wpr = -2.0 * wtemp * wtemp;
        double wpi = Math.sin(theta);
        double wr = 1.0 + wpr;
        double wi = wpi;
        int n2p3 = (2 * n) + 3;
        double h1r, h1i, h2r, h2i;
        int i1, i2, i3, i4;

        for (i = 2; i <= (n / 2); i++) {
            i1 = i + i - 1;
            i2 = 1 + i1;
            i3 = n2p3 - i2;
            i4 = 1 + i3;

            // System.out.println("i1: " + i1 + "  i2: " + i2 + "  i3: " + i3 + "  i4: " + i4);
            h1r = c1 * (work[i1 - 1] + work[i3 - 1]);
            h1i = c1 * (work[i2 - 1] - work[i4 - 1]);
            h2r = -c2 * (work[i2 - 1] + work[i4 - 1]);
            h2i = c2 * (work[i1 - 1] - work[i3 - 1]);
            work[i1 - 1] = (float) (h1r + (wr * h2r) - (wi * h2i));
            work[i2 - 1] = (float) (h1i + (wr * h2i) + (wi * h2r));
            work[i3 - 1] = (float) (h1r - (wr * h2r) + (wi * h2i));
            work[i4 - 1] = (float) (-h1i + (wr * h2i) + (wi * h2r));
            wtemp = wr;
            wr = (wtemp * wpr) - (wi * wpi) + wr;
            wi = (wi * wpr) + (wtemp * wpi) + wi;
        } // end for (i = 2; ... )

        float tmp;

        if (direc == 1) {
            tmp = work[0];
            work[0] = tmp + work[1];
            work[1] = tmp - work[1];
        } else {
            tmp = work[0];
            work[0] = c1 * (tmp + work[1]);
            work[1] = c1 * (tmp - work[1]);
            fftl(n, -1);
        }
    } // end fftr(...)


    /**
     * DOCUMENT ME!
     *
     * @param  n      DOCUMENT ME!
     * @param  direc  DOCUMENT ME!
     */
    void fftrOrig(int n, int direc) {
        float c1 = 0.5f;
        float c2;
        double theta = 3.141592653589793 / (double) n;

        if (direc == 1) {
            c2 = -0.5f;

            // for (int i1 = 0; i1 < n; i1++) {
            // System.out.println("wk[" + i1 + "]: " + work[i1]);
            // }

            fftl(n, 1);

            // for (int i1 = 0; i1 < n; i1++) {
            // System.out.println("wk[" + i1 + "]: " + work[i1]);
            // }

        } else {
            c2 = 0.5f;
            theta = -theta;
        }

        // make the shifted array
        int i;

        for (i = 0; i < (2 * n); i++) {
            shiftedwork[i + 1] = work[i];
        }

        double wtemp = Math.sin(0.5 * theta);
        double wpr = -2.0 * wtemp * wtemp;
        double wpi = Math.sin(theta);
        double wr = 1.0 + wpr;
        double wi = wpi;
        int n2p3 = (2 * n) + 3;
        double h1r, h1i, h2r, h2i;
        int i1, i2, i3, i4;

        for (i = 2; i <= (n / 2); i++) {
            i1 = i + i - 1;
            i2 = 1 + i1;
            i3 = n2p3 - i2;
            i4 = 1 + i3;

            // System.out.println("i1: " + i1 + "  i2: " + i2 + "  i3: " + i3 + "  i4: " + i4);
            h1r = c1 * (shiftedwork[i1] + shiftedwork[i3]);
            h1i = c1 * (shiftedwork[i2] - shiftedwork[i4]);
            h2r = -c2 * (shiftedwork[i2] + shiftedwork[i4]);
            h2i = c2 * (shiftedwork[i1] - shiftedwork[i3]);
            shiftedwork[i1] = (float) (h1r + (wr * h2r) - (wi * h2i));
            shiftedwork[i2] = (float) (h1i + (wr * h2i) + (wi * h2r));
            shiftedwork[i3] = (float) (h1r - (wr * h2r) + (wi * h2i));
            shiftedwork[i4] = (float) (-h1i + (wr * h2i) + (wi * h2r));
            wtemp = wr;
            wr = (wtemp * wpr) - (wi * wpi) + wr;
            wi = (wi * wpr) + (wtemp * wpi) + wi;
        } // end for (i = 2; ... )

        float tmp;

        if (direc == 1) {
            tmp = shiftedwork[1];
            shiftedwork[1] = tmp + shiftedwork[2];
            shiftedwork[2] = tmp - shiftedwork[2];

            for (i = 0; i < (2 * n); i++) {
                work[i] = shiftedwork[i + 1];
            }
        } else {
            tmp = shiftedwork[1];
            shiftedwork[1] = c1 * (tmp + shiftedwork[2]);
            shiftedwork[2] = c1 * (tmp - shiftedwork[2]);

            for (i = 0; i < (2 * n); i++) {
                work[i] = shiftedwork[i + 1];
            }

            fftl(n, -1);
        }
    } // end fftr(...)


    /**
     * DOCUMENT ME!
     *
     * @param  ain  DOCUMENT ME!
     * @param  anx  DOCUMENT ME!
     * @param  any  DOCUMENT ME!
     * @param  sol  DOCUMENT ME!
     * @param  snx  DOCUMENT ME!
     * @param  sny  DOCUMENT ME!
     * @param  Rx   DOCUMENT ME!
     * @param  Ry   DOCUMENT ME!
     * @param  Rz   DOCUMENT ME!
     */
    void firstOrder(float[] ain, int anx, int any, float[] sol, int snx, int sny, int Rx, int Ry, int Rz) {

        float maxSol; // max value in the sol array
        float relax; // over-relaxation parameter
        float delAv; // ratio of deltaX/Y pixel sizes
        float ratDel; // ratio of delAv to deltaZ squared

        float conB0, conB1; // PDE convergence constants
        float conb, conv, con2;
        float tol;


        relax = 1.5f;
        delAv = (deltaX + deltaY) / 2.0f;
        ratDel = (delAv / deltaZ);
        ratDel *= ratDel;

        conB1 = 4.0f * weightOne;
        conB0 = 1.0f + (2.0f * conB1 * (2.0f + ratDel));
        conb = 1.0f - relax;
        conv = 0.02f;

        /*--------------------------------------------------------------------
          Set up initial estimates (get from ain)
          find max/min for convergence test
          check for small numbers which can cause precision problems
         --------------------------------------------------------------------*/
        int ndx = Rx + 2;
        int ndy = Ry + 2;
        int ndz = Rz + 2;
        int nx1 = Rx + 1;
        int ny1 = Ry + 1;
        int nz1 = Rz + 1;

        int aIdx, solIdx, solIdx2;
        int aplSize = anx * any;
        int splSize = snx * sny;
        float aa, fTmp;

        maxSol = 1.0f;

        int i, j, k;

        for (i = 1; i <= Rx; i++) {

            for (j = 1; j < Ry; j++) {

                for (k = 1; k < Rz; k++) {
                    aIdx = (i - 1) + ((j - 1) * anx) + ((k - 1) * aplSize);
                    aa = ain[aIdx];

                    if (aa == 0.0f) {
                        solIdx = i + (j * snx) + (k * splSize);
                        sol[solIdx] = 0.0f;
                    } else {

                        if (aa < epsilon) {
                            aa = epsilon;
                        }

                        fTmp = (float) Math.sqrt((double) aa);
                        solIdx = i + (j * snx) + (k * splSize);
                        sol[solIdx] = fTmp;

                        if (maxSol < fTmp) {
                            maxSol = fTmp;
                        }
                    } // end else
                } // end for (k = 0; ...)
            } // end for (j = 0; ...)
        } // end for (i = 0; ...)

        System.out.println("maxSol: " + df.format(maxSol));
        // saveWASHU(sol, Rx+2, Ry+2, 2*Rz+2, "mydiffrJ.wu");
        // correct to here:  4/1/05

        /*--------------------------------------------------------------------
         threshold for convergence check, ignore small numbers less
         than tol = maxSol/20.0;
        --------------------------------------------------------------------*/
        tol = maxSol / 20.0f;

        /*--------------------------------------------------------------------
          setup boundary conditions (replicate at boundaries)
         --------------------------------------------------------------------*/
        for (i = 2; i <= nx1; i++) {

            for (j = 2; j <= ny1; j++) {
                solIdx = (i - 1) + ((j - 1) * snx);
                solIdx2 = (i - 1) + ((j - 1) * snx) + splSize;
                sol[solIdx] = sol[solIdx2];

                solIdx = (i - 1) + ((j - 1) * snx) + ((ndz - 1) * splSize);
                solIdx2 = (i - 1) + ((j - 1) * snx) + ((nz1 - 1) * splSize);
                sol[solIdx] = sol[solIdx2];
            } // end for (j = 0; ...)
        } // end for (i = 0; ...)

        // saveWASHU(sol, Rx+2, Ry+2, 2*Rz+2, "mydiffrJ.wu");

        for (i = 2; i <= nx1; i++) {

            for (k = 2; k <= nz1; k++) {
                solIdx = (i - 1) + ((k - 1) * splSize);
                solIdx2 = (i - 1) + snx + ((k - 1) * splSize);
                sol[solIdx] = sol[solIdx2];

                solIdx = (i - 1) + ((ndy - 1) * snx) + ((k - 1) * splSize);
                solIdx2 = (i - 1) + ((ny1 - 1) * snx) + ((k - 1) * splSize);
                sol[solIdx] = sol[solIdx2];
            } // end for (k = 2; ...)
        } // end for (i = 2; ...)

        for (j = 2; j <= ny1; j++) {

            for (k = 2; k <= nz1; k++) {
                solIdx = ((j - 1) * snx) + ((k - 1) * splSize);
                solIdx2 = 1 + ((j - 1) * snx) + ((k - 1) * splSize);
                sol[solIdx] = sol[solIdx2];

                solIdx = (ndx - 1) + ((j - 1) * snx) + ((k - 1) * splSize);
                solIdx2 = (nx1 - 1) + ((j - 1) * snx) + ((k - 1) * splSize);
            } // end for (k = 2; ...)
        } // end for (j = 2; ...)

        int ainIdx, iter = 0;
        boolean done = false;
        float dev, devMax, tmpDev, tmpNew;
        float sol1, sol2, sol3, sol4, sol5, sol6;

        while (!done) {
            iter++;
            devMax = 0.0f;

            for (i = 2; i <= nx1; i++) {

                for (j = 2; j <= ny1; j++) {

                    for (k = 2; k <= nz1; k++) {
                        solIdx = (i - 1) + ((j - 1) * snx) + ((k - 1) * splSize);
                        fTmp = sol[solIdx];

                        if (fTmp == 0.0f) {
                            fTmp = 0.001f;
                        }

                        ainIdx = (i - 2) + ((j - 2) * anx) + ((k - 2) * aplSize);
                        tmpDev = -ain[ainIdx] / (fTmp * fTmp);
                        con2 = relax / (conB0 - tmpDev);
                        solIdx = i + ((j - 1) * snx) + ((k - 1) * splSize);
                        sol1 = sol[solIdx];
                        solIdx = (i - 2) + ((j - 1) * snx) + ((k - 1) * splSize);
                        sol2 = sol[solIdx];
                        solIdx = (i - 1) + ((j - 2) * snx) + ((k - 1) * splSize);
                        sol3 = sol[solIdx];
                        solIdx = (i - 1) + (j * snx) + ((k - 1) * splSize);
                        sol4 = sol[solIdx];
                        solIdx = (i - 1) + ((j - 1) * snx) + (k * splSize);
                        sol5 = sol[solIdx];
                        solIdx = (i - 1) + ((j - 1) * snx) + ((k - 2) * splSize);
                        sol6 = sol[solIdx];

                        tmpNew = (conb * fTmp) +
                                 (con2 *
                                      ((-2.0f * tmpDev * fTmp) +
                                           (conB1 * (sol1 + sol2 + sol3 + sol4 + (ratDel * (sol5 + sol6))))));
                        solIdx = (i - 1) + ((j - 1) * snx) + ((k - 1) * splSize);
                        sol[solIdx] = tmpNew;

                        if (tmpNew < tol) {
                            continue;
                        }

                        dev = (float) Math.abs(((double) tmpNew / fTmp) - 1.0f);

                        if (devMax < dev) {
                            devMax = dev;
                        }
                    } // end for (k = 2; ...)
                } // end for (j = 2; ...)
            } // end for (i = 2; ...)

            if (iter > 100) {
                MipavUtil.displayError("PDE FirstOrder() no convergence");
            }

            if (devMax <= conv) {
                done = true;
            }
        } // end while(!done)

        for (i = 2; i <= nx1; i++) {

            for (j = 2; j <= ny1; j++) {

                for (k = 2; k <= nz1; k++) {
                    ainIdx = (i - 2) + ((j - 2) * anx) + ((k - 2) * aplSize);
                    solIdx = (i - 1) + ((j - 1) * snx) + ((k - 1) * splSize);
                    fTmp = sol[solIdx];
                    ain[ainIdx] = fTmp * fTmp;
                } // end for (k = 2; ...)
            } // end for (j = 2; ...)
        } // end for (i = 2; ...)
    } // end firstOrder(...)


    /**
     * Weighted sum of array: sum = 0 for each plane sum += (plane_num * sum_of_plane) array allocated dimensions:
     * nx1,ny1,* Perform sum over nx,ny,nz.
     *
     * @param   img  DOCUMENT ME!
     * @param   nx1  DOCUMENT ME!
     * @param   ny1  DOCUMENT ME!
     * @param   nx   DOCUMENT ME!
     * @param   ny   DOCUMENT ME!
     * @param   nz   DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    float imageWeightedSum(float[] img, int nx1, int ny1, int nx, int ny, int nz) {
        int ix, iy, iz, pl_size, deltx, idx;
        double dsum, sumplane;

        pl_size = nx1 * ny1;
        deltx = nx1 - nx;
        dsum = 0.0;

        for (iz = 0; iz < nz; iz++) {
            idx = iz * pl_size;
            sumplane = 0.0;

            for (iy = 0; iy < ny; iy++, idx += deltx) {

                for (ix = 0; ix < nx; ix++, idx++) {
                    sumplane += ((double) img[idx]);
                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)

            dsum += ((double) (iz + 1) * sumplane);
        } // end for (iz = 0; ...)

        return ((float) dsum);
    }


    /**
     * DOCUMENT ME!
     *
     * @param  in   DOCUMENT ME!
     * @param  nx   DOCUMENT ME!
     * @param  ny   DOCUMENT ME!
     * @param  nz   DOCUMENT ME!
     * @param  out  DOCUMENT ME!
     * @param  ox   DOCUMENT ME!
     * @param  oy   DOCUMENT ME!
     * @param  oz   DOCUMENT ME!
     */
    void mirror(float[] in, int nx, int ny, int nz, float[] out, int ox, int oy, int oz) {

        int iplSize = nx * ny;
        int oplSize = ox * oy;

        int iz, iy, ix;
        int i1, inOffSet, o1, out1Offset, o2, out2OffSet;
        int inIdx, out1Idx, out2Idx, endIdx;
        int deltX = ox - nx;

        endIdx = (oz - 1) * oplSize;

        for (iz = 0; iz < nz; iz++) {
            inIdx = iz * iplSize;
            out1Idx = iz * oplSize;
            out2Idx = endIdx - (iz * oplSize);

            for (iy = 0; iy < ny; iy++, out1Idx += deltX, out2Idx += deltX) {

                for (ix = 0; ix < nx; ix++, inIdx++, out1Idx++, out2Idx++) {
                    out[out1Idx] = out[out2Idx] = in[inIdx];
                } // end for (iy = 0; ...)
            } // end for (iy = 0; ...)
        } // end for (iz = 0; ...)

        /*
         *              for (iz = 0; iz < nz; iz++) {                 i1 = iz * iplSize;                 o1 = iz *
         * oplSize; // start index of lower plane                 o2 = (oz - 1 - iz) * oplSize; // start index of upper
         * plane                 for (iy = 0; iy < ny; iy++) {                     inOffSet = i1 + iy * nx;
         *        out1Offset = o1 + iy * ox;                     out2OffSet = o2 + iy * ox;                     for (ix
         * = 0; ix < nx; ix++) {                         out[ix + out1Offset] = in[ix + inOffSet];
         *   out[ix + out2OffSet] = in[ix + inOffSet];                     } // end for (ix = 0, ...)                 }
         * // end for (iy = 0, ...)             } // end for (iz = 0, ...)
         */
    } // end mirror(...)

    /**
     * mirror an array onto its second half in Z (array is (nx,ny,2*nz) in size).
     *
     * @param  array  DOCUMENT ME!
     * @param  nx     DOCUMENT ME!
     * @param  ny     DOCUMENT ME!
     * @param  nz     DOCUMENT ME!
     */
    void mirror2(float[] array, int nx, int ny, int nz) {
        int ix, iz, pl_size, lastPlane, srcIdx, destIdx;
        // float *f1,*f2,*endarray;

        pl_size = nx * ny;

        /* endarray points to beginning of last plane in output array */
        lastPlane = pl_size * ((2 * nz) - 1);
        // endarray = (array + pl_size*(2*nz-1));

        for (iz = 0; iz < nz; iz++) {
            srcIdx = iz * pl_size;
            destIdx = lastPlane - (iz * pl_size);

            // f1 = array+iz*pl_size;
            // f2 = endarray-iz*pl_size;
            for (ix = 0; ix < pl_size; ix++, srcIdx++, destIdx++) {
                array[destIdx] = array[srcIdx];
            }
        } // end for (iz = 0; ...)
    } // end mirror(...)

    /**
     * array1[idx1] = array1[idx1] * array[idx2] dimensions of array2 >= dimensions of array1.
     *
     * @param  array1  DOCUMENT ME!
     * @param  nx1     DOCUMENT ME!
     * @param  ny1     DOCUMENT ME!
     * @param  array2  DOCUMENT ME!
     * @param  nx2     DOCUMENT ME!
     * @param  ny2     DOCUMENT ME!
     * @param  nx      DOCUMENT ME!
     * @param  ny      DOCUMENT ME!
     * @param  nz      DOCUMENT ME!
     */
    void mult3D(float[] array1, int nx1, int ny1, float[] array2, int nx2, int ny2, int nx, int ny, int nz) {
        int ix, iy, iz, pl_size1, pl_size2, deltx1, deltx2;
        int idx1, idx2;

        pl_size1 = nx1 * ny1;
        pl_size2 = nx2 * ny2;
        deltx1 = nx1 - nx;
        deltx2 = nx2 - nx;

        for (iz = 0; iz < nz; iz++) {
            idx1 = iz * pl_size1;
            idx2 = iz * pl_size2;

            for (iy = 0; iy < ny; iy++, idx1 += deltx1, idx2 += deltx2) {

                for (ix = 0; ix < nx; ix++, idx1++, idx2++) {
                    array1[idx1] = array1[idx1] * array2[idx2];
                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)
        } // end for (iz = 0; ...)
    } // end mult3D (...)


    /**
     * DOCUMENT ME!
     *
     * @param  image   DOCUMENT ME!
     * @param  Inx     DOCUMENT ME!
     * @param  Iny     DOCUMENT ME!
     * @param  Inz     DOCUMENT ME!
     * @param  result  DOCUMENT ME!
     * @param  Rx      DOCUMENT ME!
     * @param  Ry      DOCUMENT ME!
     * @param  Rz      DOCUMENT ME!
     * @param  psf     DOCUMENT ME!
     * @param  Px      DOCUMENT ME!
     * @param  Py      DOCUMENT ME!
     * @param  Pz      DOCUMENT ME!
     * @param  diffr   DOCUMENT ME!
     * @param  lowZ    DOCUMENT ME!
     * @param  highZ   DOCUMENT ME!
     */
    void processEM(float[] image, int Inx, int Iny, int Inz, float[] result, int Rx, int Ry, int Rz, float[] psf,
                   int Px, int Py, int Pz, float[] diffr, int lowZ, int highZ) {

        float imageSum = imageWeightedSum(image, Inx, Iny, Inx, Iny, Inz);

        // calculate the OTF
        float[] OTF;
        OTF = calcOTF(psf, Px, Py, Pz, Rx, Ry, Rz);
        // saveWASHU(OTF, Rx+2, Ry+2, 2*Rz, "myotfJ.wu");
        // saveWASHU(result, Rx, Ry, Rz, "myobjectJ.wu");
        // code correct to here (1/14/05)


        int k = (Rx + 2) * (Ry + 2) * (Rz + 2);
        int twoRz = 2 * Rz;
        int halfRx = Rx / 2;
        String mesg;

        for (int loop = 1; loop <= itMax; loop++) {

            mirror(result, Rx, Ry, Rz, diffr, Rx + 2, Ry + 2, twoRz);
            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu"); diffr is essentially the same here.  C version does
            // not initialize the allocated space for the diffr array and mirror does not copy to the last columns and
            // rows.  Therefore, an incomplete border around the right and bottom of the image differs between these
            // versions.


            mesg = "Loop: " + loop + " of " + itMax + " FFT1()";
            fireProgressStateChanged(mesg);
            fireProgressStateChanged((loop * 100) / itMax);
            real_fft3d(diffr, Rx, Ry, twoRz, Rx + 2, Ry + 2, 1);
            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu"); diffr is essentially the same here.  Difference is in
            // the bottom row for all the image planes


            cmultiply(diffr, halfRx + 1, Ry + 2, OTF, halfRx + 1, Ry + 2, halfRx + 1, Ry, twoRz);
            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu"); diffr is essentially the same here.


            mesg = "Loop: " + loop + " of " + itMax + " IFFT1()";
            fireProgressStateChanged(mesg);
            real_fft3d(diffr, Rx, Ry, twoRz, Rx + 2, Ry + 2, -1);
            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu");
            // output the same to here 1/19/05

            if (estDecay && (loop > 1)) {
                updateDecay((float) 1E-6, imageSum, diffr, Rx + 2, Ry + 2, lowZ, highZ, Inx, Iny, Inz);
                saveWASHU(diffr, Rx + 2, Ry + 2, 2 * Rz, "mydiffrJ.wu");
            }

            EMRatio(diffr, Rx + 2, Ry + 2, Rz, image, Inx, Iny, Inx, Iny, lowZ, highZ);
            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu"); output the same to here 1/19/05


            addGamma(diffr, Rx + 2, Ry + 2, Inx, Iny, lowZ, highZ);
            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu"); output the same to here 1/24/05


            /* Duplicate 1st half of diffr onto 2nd half (flipped) */
            mirror2(diffr, Rx + 2, Ry + 2, Rz);

            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu");
            mesg = "Loop: " + loop + " of " + itMax + " FFT2()";
            fireProgressStateChanged(mesg);
            real_fft3d(diffr, Rx, Ry, twoRz, Rx + 2, Ry + 2, 1);
            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu");
            // saveWASHU(OTF, Rx+2, Ry+2, 2*Rz, "myotfJ.wu");
            // output the same to here 1/24/05

            cmultiplyConjg(diffr, halfRx + 1, Ry + 2, OTF, halfRx + 1, Ry + 2, halfRx + 1, Ry, twoRz);
            // output the same to here 1/31/05
            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu");

            mesg = "Loop: " + loop + " of " + itMax + " IFFT2()";
            fireProgressStateChanged(mesg);
            real_fft3d(diffr, Rx, Ry, twoRz, Rx + 2, Ry + 2, -1);
            // saveWASHU(diffr, Rx+2, Ry+2, 2*Rz, "mydiffrJ.wu");
            // output the same to here 1/31/05


            // Copy old result to second half of diffr array to calculate error later
            copyArrays(result, Rx, Ry, diffr, k, Rx + 2, Ry + 2, Rx, Ry, Rz);
            // output the same to here 2/3/05 saveWASHU(diffr, Rx+2, Ry+2, twoRz + 2, "mydiffrJ.wu");

            // Update estimate object = object*(1st half)diffr
            mult3D(result, Rx, Ry, diffr, Rx + 2, Ry + 2, Rx, Ry, Rz);
            // output the same to here 2/3/05
            // saveWASHU(result, Rx, Ry, Rz, "myResultJ.wu");

            if (weightZero > 0.0f) {
                zeroOrder(result, Rx, Ry, Rx, Ry, Rz);
            }

            if (weightOne > 0.0f) {
                firstOrder(result, Rx, Ry, diffr, Rx + 2, Ry + 2, Rx, Ry, Rz);
                //               saveWASHU(result, Rx, Ry, Rz, "myResultJ.wu"); looks correct to here 4/5/2005
            }

            // Zero out anything below `epsilon`
            zeroOut(result, Rx, Ry, Rx, Ry, Rz);
            // output the same to here 2/3/05
            // saveWASHU(result, Rx, Ry, Rz, "myResultJ.wu");


            float error = emError2(result, Rx, Ry, diffr, k, Rx + 2, Ry + 2, Rx, Ry, Rz);

            // output the same to here 2/3/05
            System.out.println("Loop: " + loop + "  error: " + df.format(error));

        } // end for (loop = 0; ...)

        // saveWASHU(result, Rx, Ry, Rz, "myResultJ.wu");
    } // end processEM (...)

    /**
     * DOCUMENT ME!
     *
     * @param  xx    DOCUMENT ME!
     * @param  nx    DOCUMENT ME!
     * @param  ny    DOCUMENT ME!
     * @param  nz    DOCUMENT ME!
     * @param  xdim  DOCUMENT ME!
     * @param  ydim  DOCUMENT ME!
     * @param  dir   DOCUMENT ME!
     */
    void real_fft3d(float[] xx, int nx, int ny, int nz, int xdim, int ydim, int dir) {
        int halfNx = nx / 2;
        int cx = xdim;
        int cxy = cx * ydim;
        int xadd = xdim - nx;
        int pl_size = xdim * ydim;

        int i1, i2, i3, tmpI;
        final NumberFormat nf = NumberFormat.getInstance();
        nf.setMinimumFractionDigits(8);
        nf.setMaximumFractionDigits(8);

        if (dir == 1) {

            // copy the elements of each row into the work array for processing
            for (i3 = 0; i3 < nz; i3++) {

                for (i2 = 0; i2 < ny; i2++) {

                    for (i1 = 0; i1 < nx; i1++) {
                        work[i1] = xx[i1 + (i2 * cx) + (i3 * cxy)];
                    } // end for (i1 = 0; ...)

                    //                    if (i2 == (ny-1) && i3 == (nz-1)) {
                    // System.out.println("Before the first FFT");                    System.out.println("i2: " + i2 + "
                    //  i3: " + i3);                        for (i1 = 0; i1 < nx; i1 += 2) {
                    // System.out.println("wk[" + (i1/2) + "]  " + nf.format(work[i1]) + "   " +
                    // nf.format(work[(i1+1)]));                        } // end for (i1 = 0; ...)                    }
                    // // end (if (i2 == 0 && i3 == 0)

                    fftr(halfNx, 1);

                    //                    if (i2 == (ny-1) && i3 == (nz-1)) {
                    // System.out.println("After the first FFT");                        System.out.println("i2: " + i2
                    // + "  i3: " + i3);                        for (i1 = 0; i1 < nx; i1 += 2) {
                    //    System.out.println("wk[" + (i1/2) + "]  " + nf.format(work[i1]) + "   " +
                    // nf.format(work[(i1+1)]));                        } // end for (i1 = 0; ...)                    }
                    // // end (if (i2 == ? && i3 == ?)

                    i1 = (i2 * cx) + (i3 * cxy);
                    xx[i1] = work[0];
                    xx[i1 + 1] = 0.0f;

                    // System.out.println("Setting xx[" + i1 + "]: " + xx[i1]);
                    // System.out.println("Setting xx[" + (i1+1) + "]: " + xx[i1+1]);
                    i1 += nx;
                    xx[i1] = work[1];
                    xx[i1 + 1] = 0.0f;

                    // System.out.println("Setting xx[" + i1 + "]: " + xx[i1]);
                    // System.out.println("Setting xx[" + (i1+1) + "]: " + xx[i1+1]);
                    // Start at 2 since the above fills in element 0 and 1
                    for (i1 = 2; i1 < nx; i1 += 2) {
                        tmpI = i1 + (i2 * cx) + (i3 * cxy);
                        xx[tmpI] = work[i1];
                        xx[tmpI + 1] = work[i1 + 1];
                        //                        System.out.println("Setting xx[" + tmpI + "]: " + xx[tmpI]);
                        //              System.out.println("Setting xx[" + (tmpI+1) + "]: " + xx[tmpI+1]);
                    }

                    //                    if (i2 == 0 && i3 == 0) {                    if (i2 == (ny-1) && i3 == (nz-1))
                    // {                        System.out.println("After the first FFT");
                    // System.out.println("i2: " + i2 + "  i3: " + i3);                        for (i1 = 0; i1 < nx; i1
                    // += 2) {                            tmpI = i1 + i2 * cx + i3 * cxy;
                    // System.out.println("x[" + tmpI + "]  " + nf.format(xx[tmpI]) + "   " + nf.format(xx[(tmpI+1)]));
                    //                       } // end for (i1 = 0; ...)
                    // System.out.println("DONE");                    } // end (if (i2 == ? && i3 == ?)

                } // end for (i2 = 0; ...)
            } // end for (i3 = 0; ...)

            // Second pass of the FFT
            // copy the elements of each col into the work array for processing
            for (i3 = 0; i3 < nz; i3++) {

                for (i1 = 0; i1 <= nx; i1 += 2) {

                    for (i2 = 0; i2 < ny; i2++) {
                        tmpI = i1 + (i2 * cx) + (i3 * cxy);
                        work[2 * i2] = xx[tmpI];
                        work[(2 * i2) + 1] = xx[tmpI + 1];
                    } // end for (i2 = 0; ... )

                    // if (i1 == 2 && i3 == 0) {
                    // if (i1 == nx && i3 == (nz-1)) {
                    // System.out.println("Before the second FFT");
                    // System.out.println("i1: " + i1 + "  i3: " + i3);
                    // for (i2 = 0; i2 < ny; i2++) {
                    // System.out.println("wk[" + i2 + "]  " +
                    // nf.format(work[2*i2]) + "   " +
                    // nf.format(work[(2*i2 + 1)]));
                    // } // end for (i2 = 0; ...)
                    // System.out.print("DONE");
                    // } // end (if (i1 == 0 && i3 == 0)

                    fftl(ny, 1);

                    // if (i1 == 2 && i3 == 0) {
                    // if (i1 == nx && i3 == (nz-1)) {
                    // System.out.println("After the second FFT");
                    // System.out.println("i1: " + i1 + "  i3: " + i3);
                    // for (i2 = 0; i2 < ny; i2++) {
                    // System.out.println("wk[" + i2 + "]  " +
                    // nf.format(work[2*i2]) + "   " +
                    // nf.format(work[(2*i2 + 1)]));
                    // } // end for (i2 = 0; ...)
                    // System.out.print("DONE");
                    // } // end (if (i1 == 0 && i3 == 0)


                    for (i2 = 0; i2 < ny; i2++) {
                        tmpI = i1 + (i2 * cx) + (i3 * cxy);
                        xx[tmpI] = work[2 * i2];
                        xx[tmpI + 1] = work[(2 * i2) + 1];
                    } // end for (i2 = 0; ...)

                    //                    if (i1 == 0 && i3 == 0) {                    if (i1 == nx && i3 == (nz-1)) {
                    //                      System.out.println("After the second FFT");
                    // System.out.println("i1: " + i1 + "  i3: " + i3);                        for (i2 = 0; i2 < ny;
                    // i2++) {                            tmpI = i1 + i2 * cx + i3 * cxy;
                    // System.out.println("x[" + tmpI + "]  " + nf.format(xx[tmpI]) + "   " + nf.format(xx[(tmpI+1)]));
                    //                       } // end for (i1 = 0; ...)
                    // System.out.println("DONE");                    } // end (if (i2 == ? && i3 == ?)

                } // end for (i1 = 0; ...)
            } // end for (i3 = 0; ... )

            if (nz == 1) {
                return;
            }

            for (i1 = 0; i1 <= nx; i1 += 2) {

                for (i2 = 0; i2 < ny; i2++) {

                    for (i3 = 0; i3 < nz; i3++) {
                        tmpI = i1 + (i2 * cx) + (i3 * cxy);
                        work[2 * i3] = xx[tmpI];
                        work[(2 * i3) + 1] = xx[tmpI + 1];
                    } // end for (i3 = 0; ... )

                    // if (i1 == 2 && i2 == 2) {
                    // if (i1 == nx && i2 == (ny-1)) {
                    // System.out.println("Before the first FFT");
                    // System.out.println("i1: " + i1 + "  i2: " + i2);
                    // for (i3 = 0; i3 < nz; i3++) {
                    // System.out.println("wk[" + i3 + "]  " +
                    // nf.format(work[2*i3]) + "   " +
                    // nf.format(work[(2*i3 + 1)]));
                    // } // end for (i3 = 0; ...)
                    // System.out.print("DONE");
                    // } // end (if (i1 == 0 && i2 == 0)

                    fftl(nz, dir);

                    // if (i1 == 2 && i2 == 2) {
                    // if (i1 == nx && i2 == (ny-1)) {
                    // System.out.println("After the first FFT");
                    // System.out.println("i1: " + i1 + "  i2: " + i2);
                    // for (i3 = 0; i3 < nz; i3++) {
                    // System.out.println("wk[" + i3 + "]  " +
                    // nf.format(work[2*i3]) + "   " +
                    // nf.format(work[(2*i3 + 1)]));
                    // } // end for (i3 = 0; ...)
                    // System.out.print("DONE");
                    // } // end (if (i1 == 0 && i2 == 0)

                    for (i3 = 0; i3 < nz; i3++) {
                        tmpI = i1 + (i2 * cx) + (i3 * cxy);
                        xx[tmpI] = work[2 * i3];
                        xx[tmpI + 1] = work[(2 * i3) + 1];
                    } // end for (i3 = 0; ... )

                    //                    if (i1 == 0 && i2 == 0) {                    if (i1 == nx && i2 == (ny-1)) {
                    //                      System.out.println("After the first FFT");
                    // System.out.println("i1: " + i1 + "  i2: " + i2);                        for (i3 = 0; i3 < ny;
                    // i3++) {                            tmpI = i1 + i2 * cx + i3 * cxy;
                    // System.out.println("x[" + tmpI + "]  " + nf.format(xx[tmpI]) + "   " + nf.format(xx[(tmpI+1)]));
                    //                       } // end for (i1 = 0; ...)
                    // System.out.println("DONE");                    } // end (if (i1 == ? && i2 == ?)

                } // end for (i2 = 0; ... )
            } // end for (i1 = 0; ... )

        } // end if(dir == 1)
        else {

            if (nz != 1) {

                // There is more than a single plane
                for (i1 = 0; i1 <= nx; i1 += 2) {

                    for (i2 = 0; i2 < ny; i2++) {

                        for (i3 = 0; i3 < nz; i3++) {
                            tmpI = i1 + (i2 * cx) + (i3 * cxy);
                            work[2 * i3] = xx[tmpI];
                            work[(2 * i3) + 1] = xx[tmpI + 1];
                        } // end for (i3 = 0; ... )

                        /*
                         *                      if (i1 == 0 && i2 == 0) { System.out.println("Before the first IFFT");
                         * System.out.println("i2: " + i2 + "  i3: " + i3); for (i3 = 0; i3 < nx; i3 += 2) {
                         * System.out.println("wk[" + (i3 / 2) + "]  " + nf.format(work[i3]) + "   " +
                         * nf.format(work[(i3 + 1)]));                         } // end for (i3 = 0; ...) } // end if
                         * (i1 == 0 && i2 == 0)
                         */

                        fftl(nz, dir);

                        /*
                         * //                        if (i1 == 0 && i2 == 0) { if (i1 == (nx-4) && i2 == (ny-2)) {
                         * System.out.println("After the first IFFT"); System.out.println("i1: " + i1 + "  i2: " + i2);
                         * for (i3 = 0; i3 < nx; i3 += 2) { System.out.println("wk[" + (i3 / 2) + "]  " +
                         * nf.format(work[i3]) + "   " + nf.format(work[(i3 + 1)]));                         } // end
                         * for (i3 = 0; ...)                         System.out.println("Done"); } // end if (i1 == 0 &&
                         * i2 == 0)
                         */
                        // code is the same to here (1/15/2005)

                        for (i3 = 0; i3 < nz; i3++) {
                            tmpI = i1 + (i2 * cx) + (i3 * cxy);
                            xx[tmpI] = work[2 * i3];
                            xx[tmpI + 1] = work[(2 * i3) + 1];
                        } // end for (i3 = 0; ... )

                        //                   if (i1 == 0 && i2 == 5) {                    if (i1 == nx && i2 == (ny-1))
                        // {                        System.out.println("After the first FFT");
                        // System.out.println("i1: " + i1 + "  i2: " + i2);                        for (i3 = 0; i3 < ny;
                        // i3++) {                            tmpI = i1 + i2 * cx + i3 * cxy;
                        // System.out.println("x[" + tmpI + "]  " + nf.format(xx[tmpI]) + "   " +
                        // nf.format(xx[(tmpI+1)]));                        } // end for (i1 = 0; ...)
                        //      System.out.println("DONE");                    } // end (if (i1 == ? && i2 == ?)


                    } // end for (i2 = 0; ...)
                } // end for (i1 = 0; ...)

            } // end if (nz != 1)

            // Second pass of the IFFT
            for (i3 = 0; i3 < nz; i3++) {

                for (i1 = 0; i1 <= nx; i1 += 2) {

                    for (i2 = 0; i2 < ny; i2++) {
                        tmpI = i1 + (i2 * cx) + (i3 * cxy);
                        work[2 * i2] = xx[tmpI];
                        work[(2 * i2) + 1] = xx[tmpI + 1];
                    } // end for (i2 = 0; ... )

                    /*
                     *                 if (i1 == 0 && i3 == 0) { //                   if (i1 == nx && i3 == (nz-1)) {
                     *               System.out.println("Before the second IFFT");
                     * System.out.println("i1: " + i1 + "  i3: " + i3);                  for (i2 = 0; i2 < ny; i2++) {
                     *                System.out.println("wk[" + i2 + "]  " +
                     * nf.format(work[2*i2]) + "   " +                                      nf.format(work[(2*i2 +
                     * 1)]));                  } // end for (i2 = 0; ...)                  System.out.print("DONE");
                     *              } // end (if (i1 == 0 && i3 == 0)
                     */

                    fftl(ny, dir);

                    /*
                     *                  if (i1 == nx/2 && i3 == nx/2) {                     System.out.println("After
                     * the second IFFT");                     System.out.println("i1: " + i1 + "  i3: " + i3);
                     *           for (i2 = 0; i2 < ny; i2++) {                         System.out.println("wk[" + i2 +
                     * "]  " +                                            nf.format(work[2 * i2]) + "   " +
                     *                               nf.format(work[(2 * i2 + 1)]));                     } // end for
                     * (i2 = 0; ...)                     System.out.print("DONE");                 } // end (if (i1 == 0
                     * && i3 == 0)
                     */

                    for (i2 = 0; i2 < ny; i2++) {
                        tmpI = i1 + (i2 * cx) + (i3 * cxy);
                        xx[tmpI] = work[2 * i2];
                        xx[tmpI + 1] = work[(2 * i2) + 1];
                    } // end for (i2 = 0; ...)

                    /*
                     *                  if (i1 == 0 && i3 == 0) { System.out.println("After the second IFFT");
                     * System.out.println("i1: " + i1 + "  i3: " + i3);                     for (i2 = 0; i2 < ny; i2++)
                     * {                         tmpI = i1 + i2 * cx + i3 * cxy; System.out.println("x[" + tmpI + "]  "
                     * + nf.format(xx[tmpI]) + "   " + nf.format(xx[(tmpI+1)]));                   } // end for (i1 = 0;
                     * ...)                     System.out.println("DONE");            } // end (if (i2 == ? && i3 == ?)
                     */

                } // end for (i1 = 0; ...)
            } // end for (i3 = 0; ... )

            for (i3 = 0; i3 < nz; i3++) {

                for (i2 = 0; i2 < ny; i2++) {
                    work[0] = xx[(i2 * cx) + (i3 * cxy)];
                    work[1] = xx[nx + (i2 * cx) + (i3 * cxy)];

                    for (i1 = 2; i1 < nx; i1++) {
                        work[i1] = xx[i1 + (i2 * cx) + (i3 * cxy)];
                    } // end for (i1 = 0; ...)

                    /*
                     *                  if (i2 == ny/2 && i3 == nz/2) { System.out.println("Before the third IFFT");
                     * System.out.println("i2: " + i2 + "  i3: " + i3);                     for (i1 = 0; i1 < nx; i1 +=
                     * 2) {                         System.out.println("wk[" + (i1/2) + "]  " + nf.format(work[i1]) + "
                     * " + nf.format(work[(i1+1)]));                     } // end for (i1 = 0; ...)
                     * System.out.println("Done");                 } // end (if (i2 == 0 && i3 == 0)
                     */

                    fftr(halfNx, dir);

                    /*
                     *                  if (i2 == 0 && i3 == 0) { System.out.println("After the third IFFT");
                     * System.out.println("i2: " + i2 + "  i3: " + i3);                     for (i1 = 0; i1 < nx; i1 +=
                     * 2) {                         System.out.println("wk[" + (i1/2) + "]  " + nf.format(work[i1]) + "
                     * " + nf.format(work[(i1+1)]));                     } // end for (i1 = 0; ...)                 } //
                     * end (if (i2 == ? && i3 == ?)
                     */

                    for (i1 = 0; i1 < nx; i1 += 2) {
                        tmpI = i1 + (i2 * cx) + (i3 * cxy);
                        xx[tmpI] = work[i1];
                        xx[tmpI + 1] = work[i1 + 1];
                        //                        System.out.println("Setting xx[" + tmpI + "]: " + xx[tmpI]);
                        //              System.out.println("Setting xx[" + (tmpI+1) + "]: " + xx[tmpI+1]);
                    }

                    //                    if (i2 == 0 && i3 == 0) {                    if (i2 == (ny-1) && i3 == (nz-1))
                    // {                        System.out.println("After the third IFFT");
                    // System.out.println("i2: " + i2 + "  i3: " + i3);                        for (i1 = 0; i1 < nx; i1
                    // += 2) {                            tmpI = i1 + i2 * cx + i3 * cxy;
                    // System.out.println("x[" + tmpI + "]  " + nf.format(xx[tmpI]) + "   " + nf.format(xx[(tmpI+1)]));
                    //                       } // end for (i1 = 0; ...)
                    // System.out.println("DONE");                    } // end (if (i2 == ? && i3 == ?)

                } // end for (i2 = 0; ...)

            } // end for (i3 = 0; ...)

            scaleFFT(xx, nx, ny, nz, xdim, ydim);

        } // end else (dir != 1)
    } // end real_fft3d(...)


    /**
     * DOCUMENT ME!
     *
     * @param  array     DOCUMENT ME!
     * @param  rx        DOCUMENT ME!
     * @param  ry        DOCUMENT ME!
     * @param  rz        DOCUMENT ME!
     * @param  fileName  DOCUMENT ME!
     */
    void saveWASHU(float[] array, int rx, int ry, int rz, String fileName) {
        int[] exts;
        exts = new int[3];
        exts[0] = rx;
        exts[1] = ry;
        exts[2] = rz;

        ModelImage myModelImage = new ModelImage(ModelImage.FLOAT, exts, fileName);

        try {
            myModelImage.importData(0, array, true);
        } catch (IOException error) {
            myModelImage = null;
            errorCleanUp("AlgorithmXcosmEM::saveWASHU() could NOT import image", true);

            return;
        } // end try{}-catch{}

        FileIO fio = new FileIO();
        FileWriteOptions fileOptions = new FileWriteOptions(fileName,
                                                            "C:/Documents and Settings/phemler/My Documents/Research/xcosm/myBuild/currBuild/",
                                                            true);
        fio.writeImage(myModelImage, fileOptions);
    } // end saveWASHU(...)


    /**
     * DOCUMENT ME!
     *
     * @param  xx  DOCUMENT ME!
     * @param  nx  DOCUMENT ME!
     * @param  ny  DOCUMENT ME!
     * @param  nz  DOCUMENT ME!
     * @param  dx  DOCUMENT ME!
     * @param  dy  DOCUMENT ME!
     */
    void scaleFFT(float[] xx, int nx, int ny, int nz, int dx, int dy) {
        int x, y, z, plSize = dx * dy, offsetZ, offset;
        float scaleVal = 1.0f / (nx * ny * nz / 2.0f);

        for (z = 0; z < nz; z++) {
            offsetZ = z * plSize;

            for (y = 0; y < ny; y++) {
                offset = (y * dy) + offsetZ;

                for (x = 0; x < nx; x++) {
                    xx[x + offset] *= scaleVal;
                } // end for (x = 0; ...)
            } // end for (y = 0; ...)
        } // end for (z = 0; ...)
    } // end scaleFFT(...)


    /**
     * DOCUMENT ME!
     *
     * @param  array  DOCUMENT ME!
     * @param  nx1    DOCUMENT ME!
     * @param  ny1    DOCUMENT ME!
     * @param  nx     DOCUMENT ME!
     * @param  ny     DOCUMENT ME!
     * @param  nz     DOCUMENT ME!
     * @param  v      DOCUMENT ME!
     */
    void setConstant(float[] array, int nx1, int ny1, int nx, int ny, int nz, float v) {
        int idx;
        int ix, iy, iz, pl_size, deltx;

        pl_size = nx1 * ny1;
        deltx = nx1 - nx;

        for (iz = 0; iz < nz; iz++) {
            idx = iz * pl_size;

            for (iy = 0; iy < ny; iy++, idx += deltx) {

                for (ix = 0; ix < nx; ix++, idx++) {
                    array[idx] = v;
                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)
        } // end for (iz = 0; ...)
    } // end setConstant(...)


    /**
     * DOCUMENT ME!
     *
     * @param  tolerance  DOCUMENT ME!
     * @param  imagesum   DOCUMENT ME!
     * @param  diffr      DOCUMENT ME!
     * @param  nx1        DOCUMENT ME!
     * @param  ny1        DOCUMENT ME!
     * @param  lowz       DOCUMENT ME!
     * @param  highz      DOCUMENT ME!
     * @param  Inx        DOCUMENT ME!
     * @param  Iny        DOCUMENT ME!
     * @param  Inz        DOCUMENT ME!
     */
    void updateDecay(float tolerance, float imagesum, float[] diffr, int nx1, int ny1, int lowz, int highz, int Inx,
                     int Iny, int Inz) {

        int iz, itcount, maxitcount;
        float abval;
        double estimate, increment, factor, term, numerator, denominator;

        weightedSum(fwork, diffr, nx1, ny1, Inx, Iny, lowz, highz);

        maxitcount = 20;
        estimate = (double) decay;
        itcount = 0;

        while (itcount < maxitcount) {
            numerator = 0.0;
            denominator = 0.0;
            factor = 1.0;

            for (iz = 0; iz < Inz; iz++) {
                factor *= estimate;
                term = (double) (fwork[iz] * factor);
                numerator += term;
                denominator += (term * (double) (iz + 1));
            }

            numerator = (double) imagesum - numerator;
            increment = numerator / denominator;
            estimate += increment;
            itcount++;
            abval = (float) increment;

            if (abval < 0.0) {
                abval = -abval;
            }

            if (abval <= tolerance) {
                break;
            }
        }

        if (itcount >= maxitcount) {
            errorCleanUp("ERROR: In UpdateDecay(), no convergence", true);

            return;
        }

        decay = (float) estimate;
    } // end updateDecay (...)


    /**
     * WeightedSum :: Calculate sum of each plane in array weighted by plane number Loop only over [zstart,zend] planes.
     *
     * @param  sumarray  DOCUMENT ME!
     * @param  array     DOCUMENT ME!
     * @param  nx1       DOCUMENT ME!
     * @param  ny1       DOCUMENT ME!
     * @param  nx        DOCUMENT ME!
     * @param  ny        DOCUMENT ME!
     * @param  zstart    DOCUMENT ME!
     * @param  zend      DOCUMENT ME!
     */
    void weightedSum(float[] sumarray, float[] array, int nx1, int ny1, int nx, int ny, int zstart, int zend) {
        int ix, iy, iz, j, deltx, pl_size, idx;
        double sum;

        pl_size = nx1 * ny1;
        deltx = nx1 - nx;

        /* loop only over observation window */
        for (iz = zstart; iz <= zend; iz++) {

            /* image plane corresponding to iz */
            j = iz - zstart;
            idx = j * pl_size;
            sum = 0.0;

            for (iy = 0; iy < ny; iy++, idx += deltx) {

                for (ix = 0; ix < nx; ix++, idx++) {
                    sum += array[idx];
                }
            }

            sumarray[j] = (float) (sum * ((double) (j + 1)));
        } // end for (iz = zstart; ...)
    }


    /**
     * DOCUMENT ME!
     *
     * @param  arr  DOCUMENT ME!
     * @param  nx1  DOCUMENT ME!
     * @param  ny1  DOCUMENT ME!
     * @param  nx   DOCUMENT ME!
     * @param  ny   DOCUMENT ME!
     * @param  nz   DOCUMENT ME!
     */
    void zeroOrder(float[] arr, int nx1, int ny1, int nx, int ny, int nz) {

        if (weightZero == 0.0f) {
            return;
        }

        int plSize = nx1 * ny1;
        int delX = nx1 - ny1;

        float w1 = weightZero;
        float w2 = 2.0f * weightZero;

        float fTmp;
        int idx, ix, iy, iz;

        for (iz = 0; iz < nz; iz++) {
            idx = iz * plSize;

            for (iy = 0; iy < ny; iy++, idx += delX) {

                for (ix = 0; ix < nx; ix++, idx++) {
                    fTmp = (float) (Math.sqrt(((double) w2 * arr[idx]) + 1.0f));
                    arr[idx] = (fTmp - 1.0f) / w1;
                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)
        } // end for (iz = 0; ...)
    }


    /**
     * DOCUMENT ME!
     *
     * @param  arr  DOCUMENT ME!
     * @param  nx1  DOCUMENT ME!
     * @param  ny1  DOCUMENT ME!
     * @param  nx   DOCUMENT ME!
     * @param  ny   DOCUMENT ME!
     * @param  nz   DOCUMENT ME!
     */
    void zeroOut(float[] arr, int nx1, int ny1, int nx, int ny, int nz) {
        int idx, ix, iy, iz, pl_size, deltx;
        float fval;

        pl_size = nx1 * ny1;
        deltx = nx1 - nx;
        idx = 0;

        for (iz = 0; iz < nz; iz++) {

            for (iy = 0; iy < ny; iy++, idx += deltx) {

                for (ix = 0; ix < nx; ix++, idx++) {
                    fval = arr[idx];

                    if (fval < 0) {
                        fval = -fval;
                    }

                    if (fval < epsilon) {
                        arr[idx] = 0.0f;
                    } // end if (fval < eplison)
                } // end for (ix = 0; ...)
            } // end for (iy = 0; ...)
        } // end for (iz = 0; ...)
    }

    /**
     * DOCUMENT ME!
     *
     * @throws  Exception  DOCUMENT ME!
     */
    private void jbInit() throws Exception { }


    /**
     * Starts the algorithm for 3D images.
     */
    private void run3D() {

        fireProgressStateChanged(srcImage.getImageName(), "Xcosm Expectation Maximization ...");

        int iterations = 0;

        // OK, here is where the meat of the algorithm goes

        // Grap the basic image stuff
        int imageLength = origInx * origIny * origInz;
        int psfLength = origPx * origPy * origPz;
        int diffrLength = (origPx + 2) * (origPy + 2) * ((2 * origPz) + 2);
        // int diffrLength = (origPx + 2) * (origPy + 2) * (2 * origPz);

        // buffers for the image data
        float[] sourceBuffer;
        float[] psfBuffer;
        float[] resultBuffer;
        float[] diffr;

        try {
            sourceBuffer = new float[imageLength];
            resultBuffer = new float[imageLength];
            psfBuffer = new float[psfLength];
            diffr = new float[diffrLength];
        } catch (OutOfMemoryError er) {
            sourceBuffer = psfBuffer = resultBuffer = diffr = null;
            errorCleanUp("AlgorithmXcosmEM: Out of memory when creating image buffers", true);

            return;
        } // end try{}-catch{}

        // initialize image and psf with data
        try {
            originalImage.exportData(0, imageLength, sourceBuffer);
            psfImage.exportData(0, psfLength, psfBuffer);
        } catch (IOException error) {
            sourceBuffer = psfBuffer = resultBuffer = diffr = null;
            errorCleanUp("AlgorithmXcosmEM: could NOT export source image", true);

            return;
        } // end try{}-catch{}

        // intitialize guess to 1.0f
        for (int i = 0; i < imageLength; i++) {
            resultBuffer[i] = 1.0f;
        }

        processEM(sourceBuffer, origInx, origIny, origInz, resultBuffer, origRx, origRy, origRz, psfBuffer, origPx,
                  origPy, origPz, diffr, origLowZ, origHighZ);


        try {
            resultImage.importData(0, resultBuffer, true);
        } catch (IOException error) {
            resultBuffer = psfBuffer = sourceBuffer = null;
            errorCleanUp("AlgorithmXcosmEM: could NOT import dest image", true);

            return;
        } // end try{}-catch{}

        resultImage.calcMinMax();

        // The meat is done


        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);

    } // end run3D()
}
