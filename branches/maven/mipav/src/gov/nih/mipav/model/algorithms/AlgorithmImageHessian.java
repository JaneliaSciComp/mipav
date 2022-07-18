package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;

import WildMagic.LibFoundation.NumericalAnalysis.Eigenf;


/**
 * DOCUMENT ME!
 */
public class AlgorithmImageHessian extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int algorID = 0; // 2 means run on the image, 3 means run batch

    /** DOCUMENT ME! */
    private Eigenf eigenSystemAlgo = null;

    /** DOCUMENT ME! */
    private AlgorithmHessian hessianAlgo = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmImageHessian object.
     *
     * @param  srcImg  DOCUMENT ME!
     */
    public AlgorithmImageHessian(ModelImage srcImg) {
        super(null, srcImg);

        srcImage = srcImg;

        // Make algorithm
        float[] sigmas = new float[3];
        sigmas[0] = sigmas[1] = sigmas[2] = 1.0f;
        hessianAlgo = new AlgorithmHessian(srcImage, sigmas);

        // make a 3X3 eigenSolver
        eigenSystemAlgo = new Eigenf(3);

    } // end AlgorithmImageHessian(...)

    /**
     * Creates a new AlgorithmImageHessian object.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  id       DOCUMENT ME!
     */
    public AlgorithmImageHessian(ModelImage destImg, ModelImage srcImg, int id) {
        super(destImg, srcImg);

        algorID = id;

        // Make algorithm
        float[] sigmas = new float[3];
        sigmas[0] = sigmas[1] = sigmas[2] = 1.0f;
        hessianAlgo = new AlgorithmHessian(srcImage, sigmas);

        // make a 3X3 eigenSolver
        eigenSystemAlgo = new Eigenf(3);

    } // end AlgorithmImageHessian(...)

    /**
     * Creates a new AlgorithmImageHessian object.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  id       DOCUMENT ME!
     * @param  sigmas   DOCUMENT ME!
     */
    public AlgorithmImageHessian(ModelImage destImg, ModelImage srcImg, int id, float[] sigmas) {
        super(destImg, srcImg);

        algorID = id;
        hessianAlgo = new AlgorithmHessian(srcImage, sigmas);

        if (sigmas.length == 2) {
            eigenSystemAlgo = new Eigenf(2);
        } else if (sigmas.length == 3) {
            eigenSystemAlgo = new Eigenf(3);
        }

    } // end AlgorithmImageHessian(...)

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    } // end finalize()

    /**
     * DOCUMENT ME!
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            MipavUtil.displayError("AlgorithmImageHessian::run()  Source Image is null");

            return;
        }

        if (srcImage.isColorImage()) {
            MipavUtil.displayError("AlgorithmImageHessian::run()  does NOT do color images");

            return;
        }

        if (srcImage.getNDims() == 2) {

            if (algorID == 2) {

                if (destImage == null) {
                    run2D();
                } else {
                    run2D();
                }
            } else if (algorID == 3) {
                runBatch2D();
            } // end if (algorID == 2)-else if()
        } // end if (srcImage.getNDims() == 2)

        if (srcImage.getNDims() == 3) {

            if (algorID == 2) {

                if (destImage == null) {
                    runInPlace3D();
                } else {
                    runInDest3D();
                }
            } else if ((algorID == 3) && (destImage != null)) {
                runBatch3D();
            } // end if (algorID == 2)-else if()
        } // end if (srcImage.getNDims() == 3)
    } // end run()


    /**
     * DOCUMENT ME!
     */
    private void run2D() {

        fireProgressStateChanged(srcImage.getImageName(), "Hessian/Eigen System ...");

        // OK, here is where the meat of the algorithm goes

        int length, numCols, numRows;
        int[] extents = srcImage.getExtents();
        numCols = extents[0];
        numRows = extents[1];
        length = numCols * numRows;

        // buffers for the image data
        float[] sourceBuffer;
        float[] resultBuffer;

        // copy the image data into the sourceBuffer so we can access it
        try {
            sourceBuffer = new float[length];
            srcImage.exportData(0, length, sourceBuffer); // locks and releases lock
            resultBuffer = new float[length];
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: could NOT export source image", true);

            return;
        } catch (OutOfMemoryError e) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: Out of memory when creating image buffer", true);

            return;
        } // end try{}-catch{}-catch{}

        int col, row;

        // Hessian and eigen stuff
        double[][] hess = null;
        double[] evals = new double[2];
        double[] magEvals = new double[2];
        double[][] evecs = new double[2][2];
        double tmp;

        double b = 0.5f;
        double c = 50.0f;
        double expRb = 0, expS = 0;
        double blobRatio, structureness, vesselness, maxV = 0.0, maxS = 0.0;
        double maxExpRb = 0, maxExpS = 0;

        for (row = 0; row < numRows; row++) {
            fireProgressStateChanged(Math.round(((float) (row) / (numRows - 1) * 100)));

            for (col = 0; col < numCols; col++) {

                // get the Hessian at this point
                hess = hessianAlgo.hessian2D(sourceBuffer, extents, col, row);

                // fill up the eigenSolver matrix with the hessian
                eigenSystemAlgo.SetData(0, 0, (float)hess[0][0]);
                eigenSystemAlgo.SetData(0, 1, (float)hess[0][1]);

                eigenSystemAlgo.SetData(1, 0, (float)hess[1][0]);
                eigenSystemAlgo.SetData(1, 1, (float)hess[1][1]);

                // OK, solve the eigen system
                eigenSystemAlgo.IncrSortEigenStuff();

                // extract the eigenvalues from the AlgorithmEigensolver
                evals[0] = eigenSystemAlgo.GetEigenvalue(0);
                magEvals[0] = Math.abs(evals[0]);

                evals[1] = eigenSystemAlgo.GetEigenvalue(1);
                magEvals[1] = Math.abs(evals[1]);

                // put the smallest magnitude eigenvalue in element zero
                if (magEvals[1] < magEvals[0]) {
                    tmp = evals[0];
                    evals[0] = evals[1];
                    evals[1] = tmp;
                    tmp = magEvals[0];
                    magEvals[0] = magEvals[1];
                    magEvals[1] = tmp;
                    tmp = evecs[0][0];
                    evecs[0][0] = evecs[0][1];
                    evecs[0][1] = tmp;
                    tmp = evecs[1][0];
                    evecs[1][0] = evecs[1][1];
                    evecs[1][1] = tmp;
                }

                blobRatio = evals[0] / evals[1];
                structureness = Math.sqrt((evals[0] * evals[0]) + (evals[1] * evals[1]));

                if (evals[1] > 0) {
                    vesselness = 0.0f;
                } else {
                    expRb = Math.exp(-((blobRatio * blobRatio) / (2 * b * b)));
                    expS = Math.exp(-((structureness * structureness) / (2 * c * c)));
                    vesselness = expRb * (1.0 - expS);

                    if (expRb > maxExpRb) {
                        maxExpRb = expRb;
                    }

                    if (expS > maxExpS) {
                        maxExpS = expS;
                    }
                }

                if (structureness > maxS) {
                    maxS = structureness;
                }

                if (vesselness > maxV) {
                    maxV = vesselness;
                }

                // output result
                resultBuffer[(row * numCols) + col] = (float) (vesselness * 100.0);

            } // end for (col = 0; ...)
        } // end for (row = 0; ...)

        System.out.println("Max S: " + maxS);

        // OK, the resultBuffer is filled with the results of the algorithm,
        // put this data into the source image so it will be displayed in
        // in the ViewJFrameWizard
        try {

            if (destImage == null) {
                srcImage.importData(0, resultBuffer, true);
            } else {
                destImage.importData(0, resultBuffer, true);
            }
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

            return;
        } // end try{}-catch{}

        // The meat is done

        

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);

    } // end run2D()

    /**
     * DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void run3D() {

        fireProgressStateChanged(srcImage.getImageName(), "Hessian/Eigen System ...");

        // OK, here is where the meat of the algorithm goes

        int length, numCols, numRows, numPlanes = 0;
        int[] extents = srcImage.getExtents();
        numCols = extents[0];
        numRows = extents[1];
        numPlanes = extents[2];
        length = numCols * numRows * numPlanes;

        // buffers for the image data
        float[] sourceBuffer;
        float[] resultBuffer;

        // copy the image data into the sourceBuffer so we can access it
        try {
            sourceBuffer = new float[length];
            srcImage.exportData(0, length, sourceBuffer); // locks and releases lock
            resultBuffer = new float[length];
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: could NOT export source image", true);

            return;
        } catch (OutOfMemoryError e) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: Out of memory when creating image buffer", true);

            return;
        } // end try{}-catch{}-catch{}

        int col, row, plane;

        // get the center plane in array coordinates
        plane = ((numPlanes / 2) - 1);

        // Hessian and eigen stuff
        double[][] hess = null;
        double[] evals = new double[3];
        double[] magEvals = new double[3];
        int[] lut = new int[3];
        lut[0] = 0;
        lut[1] = 1;
        lut[2] = 2;

        int tmp;

        double a = 0.5f;
        double b = 0.5f;
        double c = 50.0f;
        double expRa = 0, expRb = 0, expS = 0;

        float[] sigmas = new float[3];
        double blobRatio, areaRatio, structureness, vesselness, maxV = 0.0, maxS = 0.0;
        double maxExpRa = 0, maxExpRb = 0, maxExpS = 0;

        for (int scale = 1; scale < 2; scale++) {

            System.out.println("Scale: " + scale);
            System.gc();

            sigmas[0] = sigmas[1] = sigmas[2] = 0.5f;
            hessianAlgo = new AlgorithmHessian(srcImage, sigmas);

            for (plane = 0; plane < numPlanes; plane++) {
                fireProgressStateChanged(Math.round(((float) (plane) / (numPlanes - 1) * 100)));

                for (row = 0; row < numRows; row++) {

                    for (col = 0; col < numCols; col++) {

                        // get the Hessian at this point
                        hess = hessianAlgo.hessian3D(sourceBuffer, extents, col, row, plane);

                        // fill up the eigenSolver matrix with the hessian
                        eigenSystemAlgo.SetData(0, 0, (float)hess[0][0]);
                        eigenSystemAlgo.SetData(0, 1, (float)hess[0][1]);
                        eigenSystemAlgo.SetData(0, 2, (float)hess[0][2]);

                        eigenSystemAlgo.SetData(1, 0, (float)hess[1][0]);
                        eigenSystemAlgo.SetData(1, 1, (float)hess[1][1]);
                        eigenSystemAlgo.SetData(1, 2, (float)hess[1][2]);

                        eigenSystemAlgo.SetData(2, 0, (float)hess[2][0]);
                        eigenSystemAlgo.SetData(2, 1, (float)hess[2][1]);
                        eigenSystemAlgo.SetData(2, 2, (float)hess[2][2]);

                        // OK, solve the eigen system
                        eigenSystemAlgo.IncrSortEigenStuff();

                        // extract the eigenvalues from the AlgorithmEigensolver
                        evals[0] = eigenSystemAlgo.GetEigenvalue(0);
                        magEvals[0] = Math.abs(evals[0]);

                        evals[1] = eigenSystemAlgo.GetEigenvalue(1);
                        magEvals[1] = Math.abs(evals[1]);

                        evals[2] = eigenSystemAlgo.GetEigenvalue(2);
                        magEvals[2] = Math.abs(evals[2]);

                        // reset the eigen value look-up table
                        lut[0] = 0;
                        lut[1] = 1;
                        lut[2] = 2;

                        // put the smallest eigen val in the 0 element
                        if (magEvals[lut[1]] < magEvals[lut[0]]) {
                            tmp = lut[0];
                            lut[0] = lut[1];
                            lut[1] = tmp;
                        }

                        if (magEvals[lut[2]] < magEvals[lut[0]]) {
                            tmp = lut[0];
                            lut[0] = lut[2];
                            lut[2] = tmp;
                        }

                        // put the next smallest element in the 1st element
                        if (magEvals[lut[2]] < magEvals[lut[1]]) {
                            tmp = lut[1];
                            lut[1] = lut[2];
                            lut[2] = tmp;
                        }

                        blobRatio = magEvals[lut[0]] / (Math.sqrt(magEvals[lut[1]] * magEvals[lut[2]]));
                        areaRatio = magEvals[lut[1]] / magEvals[lut[2]];
                        structureness = Math.sqrt((evals[lut[0]] * evals[lut[0]]) + (evals[lut[1]] * evals[lut[1]]) +
                                                  (evals[lut[2]] * evals[lut[2]]));

                        if ((evals[lut[1]] > 0) || (evals[lut[2]] > 0)) {
                            vesselness = 0.0f;
                        } else {
                            expRa = Math.exp(-((areaRatio * areaRatio) / (2 * a * a)));
                            expRb = Math.exp(-((blobRatio * blobRatio) / (2 * b * b)));
                            expS = Math.exp(-((structureness * structureness) / (2 * c * c)));
                            vesselness = (1.0 - expRa) * expRb * (1.0 - expS);

                            if (expRa > maxExpRa) {
                                maxExpRa = expRa;
                            }

                            if (expRb > maxExpRb) {
                                maxExpRb = expRb;
                            }

                            if (expS > maxExpS) {
                                maxExpS = expS;
                            }
                        }

                        if (structureness > maxS) {
                            maxS = structureness;
                        }

                        if (vesselness > maxV) {
                            maxV = vesselness;
                        }

                        // output result on different slices
                        resultBuffer[(plane * numRows * numCols) + (row * numCols) + col] = (float) (vesselness *
                                                                                                         100.0);

                    } // end for (col = 0; ...)
                } // end for (row = 0; ...)

            } // end for (plane = 0; ...)

        } // end for (scale = 1; ...)

        // OK, the resultBuffer is filled with the results of the algorithm,
        // put this data into the source image so it will be displayed in
        // in the ViewJFrameWizard
        try {
            srcImage.importData(0, resultBuffer, true); // locks and releases lock
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

            return;
        } // end try{}-catch{}

        // The meat is done

        

        if (threadStopped) {
            finalize();

            return;
        }

    } // end run3D()

    /**
     * Algorithm is given a scale range and scale increment and computed the 1) hessian, 2) eigenvalues, 3) eigenvectors
     * of the hessian. These values are saved in individual files for each scale
     */
    private void runBatch2D() {

        if (srcImage.getNDims() != 2) {
            return;
        }

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("AlgorithmImageHession reports: destination image locked", false);

            return;
        }

        // Scale space range used to control scale iterator and determine
        // the number of slices needed in the result image
        float startScale = 1.0f;
        float endScale = 3.0f;
        float scaleIncrement = 0.5f;
        int numImages = (int) (((endScale - startScale) / scaleIncrement) + 1.5f);

        fireProgressStateChanged(srcImage.getImageName(), "Hessian/Eigen System ...");

        // OK, here is where the meat of the algorithm goes

        int length, numCols, numRows;
        int[] extents = srcImage.getExtents();
        numCols = extents[0];
        numRows = extents[1];
        length = numCols * numRows;

        // buffers for the image data
        float[] sourceBuffer;
        float[] resultBuffer;
        float[] e1;
        float[] e2;
        float[] evec1;
        float[] evec2;
        float[] maxStruct;

        // copy the image data into the sourceBuffer so we can access it
        try {
            sourceBuffer = new float[length];
            srcImage.exportData(0, length, sourceBuffer); // locks and releases lock
            resultBuffer = new float[length];
            e1 = new float[length];
            e2 = new float[length];
            evec1 = new float[length];
            evec2 = new float[length];
            maxStruct = new float[length];
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            e1 = e2 = evec1 = evec2 = maxStruct = null;
            errorCleanUp("AlgorithmImageHessian: could NOT export source image", true);

            return;
        } catch (OutOfMemoryError e) {
            sourceBuffer = null;
            resultBuffer = null;
            e1 = e2 = evec1 = evec2 = maxStruct = null;
            errorCleanUp("AlgorithmImageHessian: Out of memory when creating image buffer", true);

            return;
        } // end try{}-catch{}-catch{}

        int col, row;

        // Hessian and eigen stuff
        double[][] hess = null;
        double[] evals = new double[2];
        double[] magEvals = new double[2];
        double[][] evecs = new double[2][2];

        double b = 0.5f;
        double c = 750.0f;
        double expRb = 0.0, expS = 0.0;

        float[] sigmas = new float[2];
        double blobRatio, structureness, vesselness, maxV = 0.0, maxS = 0.0;
        double maxExpRb = 0, maxExpS = 0;
        int index;

        float currentScale = startScale;

        for (int scaleIndex = 0; scaleIndex < numImages; scaleIndex++) {
            currentScale = startScale + (scaleIndex * scaleIncrement);

            // Show progress
            fireProgressStateChanged("Slice " + (scaleIndex + 1) + " of " + numImages + "  Scale: " + currentScale);

            sigmas[0] = sigmas[1] = currentScale;
            hessianAlgo = new AlgorithmHessian(srcImage, sigmas);

            for (row = 0; row < numRows; row++) {
                fireProgressStateChanged(Math.round(((float) (row) / (numRows - 1) * 100)));

                for (col = 0; col < numCols; col++) {

                    // get the Hessian at this point
                    hess = hessianAlgo.hessian2D(sourceBuffer, extents, col, row);

                    // fill up the eigenSolver matrix with the hessian
                    eigenSystemAlgo.SetData(0, 0, (float)hess[0][0]);
                    eigenSystemAlgo.SetData(0, 1, (float)hess[0][1]);

                    eigenSystemAlgo.SetData(1, 0, (float)hess[1][0]);
                    eigenSystemAlgo.SetData(1, 1, (float)hess[1][1]);

                    // OK, solve the eigen system
                    eigenSystemAlgo.IncrSortEigenStuff();

                    // extract the eigenvalues and vectors from the AlgorithmEigensolver
                    evals[0] = eigenSystemAlgo.GetEigenvalue(0);
                    magEvals[0] = Math.abs(evals[0]);
                    evals[1] = eigenSystemAlgo.GetEigenvalue(1);
                    magEvals[1] = Math.abs(evals[1]);

                    evecs[0][0] = eigenSystemAlgo.GetEigenvector(0, 0);
                    evecs[1][0] = eigenSystemAlgo.GetEigenvector(1, 0);

                    evecs[0][1] = eigenSystemAlgo.GetEigenvector(0, 1);
                    evecs[1][1] = eigenSystemAlgo.GetEigenvector(1, 1);

                    double tmp;

                    // put the smallest magnitude eigenvalue in element zero
                    if (magEvals[1] < magEvals[0]) {
                        tmp = evals[0];
                        evals[0] = evals[1];
                        evals[1] = tmp;
                        tmp = magEvals[0];
                        magEvals[0] = magEvals[1];
                        magEvals[1] = tmp;
                        tmp = evecs[0][0];
                        evecs[0][0] = evecs[0][1];
                        evecs[0][1] = tmp;
                        tmp = evecs[1][0];
                        evecs[1][0] = evecs[1][1];
                        evecs[1][1] = tmp;
                    }

                    blobRatio = evals[0] / evals[1];
                    structureness = Math.sqrt((evals[0] * evals[0]) + (evals[1] * evals[1]));

                    if (evals[1] > 0) {
                        vesselness = 0.0f;
                    } else {
                        expRb = Math.exp(-((blobRatio * blobRatio) / (2 * b * b)));
                        expS = Math.exp(-((structureness * structureness) / (2 * c * c)));
                        vesselness = expRb * (1.0 - expS);

                        if (expRb > maxExpRb) {
                            maxExpRb = expRb;
                        }

                        if (expS > maxExpS) {
                            maxExpS = expS;
                        }
                    }

                    if (structureness > maxS) {
                        maxS = structureness;
                    }

                    if (vesselness > maxV) {
                        maxV = vesselness;
                    }

                    // output result on different slices
                    index = (row * numCols) + col;
                    resultBuffer[index] = (float) (vesselness * 1000.0);

                    e1[index] = (float) evals[0];
                    e2[index] = (float) evals[1];
                    evec1[index] = (float) (Math.atan2(evecs[0][1], evecs[0][0]) * 180.0 / Math.PI);
                    evec2[index] = (float) (Math.atan2(evecs[1][1], evecs[1][0]) * 180.0 / Math.PI);

                    if (resultBuffer[index] > maxStruct[index]) {
                        maxStruct[index] = resultBuffer[index];
                    }

                } // end for (col = 0; ...)
            } // end for (row = 0; ...)

            maxExpRb = maxExpS = maxS = maxV = 0.0;
            destImage.releaseLock();

            try {
                destImage.importData(0, resultBuffer, true);
            } catch (IOException error) {
                sourceBuffer = null;
                resultBuffer = null;
                errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

                return;
            } // end try{}-catch{}

            destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(),
                                srcImage.getImageName() + "_vsVes" + (scaleIndex + 1), FileUtility.XML, true);

            try {
                destImage.importData(0, e1, true);
            } catch (IOException error) {
                sourceBuffer = null;
                resultBuffer = null;
                errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

                return;
            } // end try{}-catch{}

            destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(),
                                srcImage.getImageName() + "_vsE1" + (scaleIndex + 1), FileUtility.XML, true);

            try {
                destImage.importData(0, e2, true);
            } catch (IOException error) {
                sourceBuffer = null;
                resultBuffer = null;
                errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

                return;
            } // end try{}-catch{}

            destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(),
                                srcImage.getImageName() + "_vsE2" + (scaleIndex + 1), FileUtility.XML, true);

            try {
                destImage.importData(0, evec1, true);
            } catch (IOException error) {
                sourceBuffer = null;
                resultBuffer = null;
                errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

                return;
            } // end try{}-catch{}

            destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(),
                                srcImage.getImageName() + "_vsEvec1" + (scaleIndex + 1), FileUtility.XML, true);

            try {
                destImage.importData(0, evec2, true);
            } catch (IOException error) {
                sourceBuffer = null;
                resultBuffer = null;
                errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

                return;
            } // end try{}-catch{}

            destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(),
                                srcImage.getImageName() + "_vsEvec2" + (scaleIndex + 1), FileUtility.XML, true);

        } // end for (scaleIndex = 0; ...)

        // OK, the resultBuffer is filled with the results of the algorithm,
        // put this data into the source image so it will be displayed in
        // in the ViewJFrameWizard

        // Output image is the maximum structurness
        destImage.releaseLock();

        try {
            destImage.importData(0, maxStruct, true);
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: Could NOT import maxStruct to the image", true);

            return;
        } // end try{}-catch{}

        // The meat is done

        

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);

    } // end runBatch2D()

    /**
     * DOCUMENT ME!
     */
    private void runBatch3D() {

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("AlgorithmImageHession reports: destination image locked", false);

            return;
        }

        // Scale space range used to control scale iterator and determine
        // the number of slices needed in the result image
        float startScale = 1.0f;
        float endScale = 3.0f;
        float scaleIncrement = 2.0f;
        int numImages = (int) (((endScale - startScale) / scaleIncrement) + 1.5f);

        fireProgressStateChanged(srcImage.getImageName(), "Hessian/Eigen System ...");

        // OK, here is where the meat of the algorithm goes

        int length, numCols, numRows, numPlanes = 0;
        int[] extents = srcImage.getExtents();
        numCols = extents[0];
        numRows = extents[1];
        numPlanes = extents[2];
        length = numCols * numRows * numPlanes;

        // buffers for the image data
        float[] sourceBuffer;
        float[] resultBuffer;
        float[] e1;
        float[] e2;
        float[] e3;

        // copy the image data into the sourceBuffer so we can access it
        try {
            sourceBuffer = new float[length];
            srcImage.exportData(0, length, sourceBuffer); // locks and releases lock
            resultBuffer = new float[length];
            e1 = new float[length];
            e2 = new float[length];
            e3 = new float[length];

        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            e1 = e2 = e3 = null;
            errorCleanUp("AlgorithmImageHessian: could NOT export source image", true);

            return;
        } catch (OutOfMemoryError e) {
            sourceBuffer = null;
            resultBuffer = null;
            e1 = e2 = e3 = null;
            errorCleanUp("AlgorithmImageHessian: Out of memory when creating image buffer", true);

            return;
        } // end try{}-catch{}-catch{}

        int col, row, plane;

        // Hessian and eigen stuff
        double[][] hess = null;
        double[] evals = new double[3];
        double[] magEvals = new double[3];

        int[] lut = new int[3];
        lut[0] = 0;
        lut[1] = 1;
        lut[2] = 2;

        int tmp;

        double a = 0.5f;
        double b = 0.5f;
        double c = 750.0f;
        double expRa = 0.0, expRb = 0.0, expS = 0.0;

        float[] sigmas = new float[3];
        double blobRatio, areaRatio, structureness, vesselness, maxV = 0.0, maxS = 0.0;
        double maxExpRa = 0, maxExpRb = 0, maxExpS = 0;
        int index;

        float currentScale = startScale;

        for (int scaleIndex = 0; scaleIndex < numImages; scaleIndex++) {
            currentScale = startScale + (scaleIndex * scaleIncrement);

            // Show progress
            fireProgressStateChanged("Slice " + (scaleIndex + 1) + " of " + numImages + "  Scale: " + currentScale);

            sigmas[0] = sigmas[1] = sigmas[2] = currentScale;
            hessianAlgo = new AlgorithmHessian(srcImage, sigmas);

            for (plane = 0; plane < numPlanes; plane++) {
                fireProgressStateChanged(Math.round(((float) (plane) / (numPlanes - 1) * 100)));

                for (row = 0; row < numRows; row++) {

                    for (col = 0; col < numCols; col++) {

                        // get the Hessian at this point
                        hess = hessianAlgo.hessian3D(sourceBuffer, extents, col, row, plane);

                        // fill up the eigenSolver matrix with the hessian
                        eigenSystemAlgo.SetData(0, 0, (float)hess[0][0]);
                        eigenSystemAlgo.SetData(0, 1, (float)hess[0][1]);
                        eigenSystemAlgo.SetData(0, 2, (float)hess[0][2]);

                        eigenSystemAlgo.SetData(1, 0, (float)hess[1][0]);
                        eigenSystemAlgo.SetData(1, 1, (float)hess[1][1]);
                        eigenSystemAlgo.SetData(1, 2, (float)hess[1][2]);

                        eigenSystemAlgo.SetData(2, 0, (float)hess[2][0]);
                        eigenSystemAlgo.SetData(2, 1, (float)hess[2][1]);
                        eigenSystemAlgo.SetData(2, 2, (float)hess[2][2]);

                        // OK, solve the eigen system
                        eigenSystemAlgo.IncrSortEigenStuff();

                        // extract the eigenvalues from the AlgorithmEigensolver
                        evals[0] = eigenSystemAlgo.GetEigenvalue(0);
                        magEvals[0] = Math.abs(evals[0]);

                        evals[1] = eigenSystemAlgo.GetEigenvalue(1);
                        magEvals[1] = Math.abs(evals[1]);

                        evals[2] = eigenSystemAlgo.GetEigenvalue(2);
                        magEvals[2] = Math.abs(evals[2]);
                        
                        // reset the eigen value look-up table
                        lut[0] = 0;
                        lut[1] = 1;
                        lut[2] = 2;

                        // put the smallest eigen val in the 0 element
                        if (magEvals[lut[1]] < magEvals[lut[0]]) {
                            tmp = lut[0];
                            lut[0] = lut[1];
                            lut[1] = tmp;
                        }

                        if (magEvals[lut[2]] < magEvals[lut[0]]) {
                            tmp = lut[0];
                            lut[0] = lut[2];
                            lut[2] = tmp;
                        }

                        // put the next smallest element in the 1st element
                        if (magEvals[lut[2]] < magEvals[lut[1]]) {
                            tmp = lut[1];
                            lut[1] = lut[2];
                            lut[2] = tmp;
                        }

                        blobRatio = magEvals[lut[0]] / (Math.sqrt(magEvals[lut[1]] * magEvals[lut[2]]));
                        areaRatio = magEvals[lut[1]] / magEvals[lut[2]];
                        structureness = Math.sqrt((evals[lut[0]] * evals[lut[0]]) + (evals[lut[1]] * evals[lut[1]]) +
                                                  (evals[lut[2]] * evals[lut[2]]));

                        if ((evals[lut[1]] > 0) || (evals[lut[2]] > 0)) {
                            vesselness = 0.0f;
                        } else {
                            expRa = Math.exp(-((areaRatio * areaRatio) / (2 * a * a)));
                            expRb = Math.exp(-((blobRatio * blobRatio) / (2 * b * b)));
                            expS = Math.exp(-((structureness * structureness) / (2 * c * c)));
                            vesselness = (1.0 - expRa) * expRb * (1.0 - expS);

                            if (expRa > maxExpRa) {
                                maxExpRa = expRa;
                            }

                            if (expRb > maxExpRb) {
                                maxExpRb = expRb;
                            }

                            if (expS > maxExpS) {
                                maxExpS = expS;
                            }
                        }

                        if (structureness > maxS) {
                            maxS = structureness;
                        }

                        if (vesselness > maxV) {
                            maxV = vesselness;
                        }

                        // output result on different slices
                        index = (plane * numRows * numCols) + (row * numCols) + col;
                        resultBuffer[index] = (float) (vesselness * 1000.0);
                        e1[index] = (float) evals[lut[0]];
                        e2[index] = (float) evals[lut[1]];
                        e3[index] = (float) evals[lut[2]];

                    } // end for (col = 0; ...)
                } // end for (row = 0; ...)

                maxExpRa = maxExpRb = maxExpS = maxS = maxV = 0.0;

            } // end for (plane = 0; ...)

            // OK, the resultBuffer is filled with the results of the algorithm,
            // put this data into the source image so it will be displayed in
            // in the ViewJFrameWizard

            destImage.releaseLock();

            try {
                destImage.importData(0, resultBuffer, true);
            } catch (IOException error) {
                sourceBuffer = resultBuffer = null;
                e1 = e2 = e3 = null;
                errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

                return;
            } // end try{}-catch{}

            destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(),
                                srcImage.getImageName() + "_vsGauss" + scaleIndex, FileUtility.XML, true);

            try {
                destImage.importData(0, e1, true);
            } catch (IOException error) {
                sourceBuffer = resultBuffer = null;
                e1 = e2 = e3 = null;
                errorCleanUp("AlgorithmImageHessian: Could NOT import eigen value 1 to the image", true);

                return;
            } // end try{}-catch{}

            destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(),
                                srcImage.getImageName() + "_vsE1" + scaleIndex, FileUtility.XML, true);

            try {
                destImage.importData(0, e2, true);
            } catch (IOException error) {
                sourceBuffer = null;
                resultBuffer = null;
                e1 = e2 = e3 = null;
                errorCleanUp("AlgorithmImageHessian: Could NOT import eieigengrn value 2 to the image", true);

                return;
            } // end try{}-catch{}

            destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(),
                                srcImage.getImageName() + "_vsE2" + scaleIndex, FileUtility.XML, true);

            try {
                destImage.importData(0, e3, true);
            } catch (IOException error) {
                sourceBuffer = resultBuffer = null;
                e1 = e2 = e3 = null;
                errorCleanUp("AlgorithmImageHessian: Could NOT import eigen value 3 to the image", true);

                return;
            } // end try{}-catch{}

            destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(),
                                srcImage.getImageName() + "_vsE3" + scaleIndex, FileUtility.XML, true);

        } // end for (scaleIndex = 1; ...)

        destImage.releaseLock();

        try {
            destImage.importData(0, resultBuffer, true); // locks and releases lock
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            e1 = e2 = e3 = null;
            errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

            return;
        } // end try{}-catch{}

        // The meat is done

        

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);

    } // end runBatch3D()

    /**
     * DOCUMENT ME!
     */
    private void runInDest3D() {

        DecimalFormat fltFmt = new DecimalFormat("0.00");

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("AlgorithmImageHession reports: destination image locked", false);

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Hessian/Eigen System ...");

        // OK, here is where the meat of the algorithm goes

        int length, numCols, numRows, numPlanes = 0;
        int[] extents = srcImage.getExtents();
        numCols = extents[0];
        numRows = extents[1];
        numPlanes = extents[2];
        length = numCols * numRows * numPlanes;

        // buffers for the image data
        float[] sourceBuffer;
        float[] resultBuffer;

        // copy the image data into the sourceBuffer so we can access it
        try {
            sourceBuffer = new float[length];
            srcImage.exportData(0, length, sourceBuffer); // locks and releases lock
            resultBuffer = new float[length];
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: could NOT export source image", true);

            return;
        } catch (OutOfMemoryError e) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: Out of memory when creating image buffer", true);

            return;
        } // end try{}-catch{}-catch{}

        int col, row, plane;

        // Hessian and eigen stuff
        double[][] hess = null;
        double[] evals = new double[3];
        double[] magEvals = new double[3];
        int[] lut = new int[3];
        lut[0] = 0;
        lut[1] = 1;
        lut[2] = 2;

        int tmp;

        double a = 0.5f;
        double b = 0.5f;
        double c = 750.0f;
        double expRa = 0.0, expRb = 0.0, expS = 0.0;

        float[] sigmas = new float[3];
        double blobRatio, areaRatio, structureness, vesselness, maxV = 0.0, maxS = 0.0;
        double maxExpRa = 0, maxExpRb = 0, maxExpS = 0;

        for (int scale = 1; scale < 2; scale++) {

            sigmas[0] = sigmas[1] = sigmas[2] = 0.5f + ((scale - 1.0f) * 0.25f);
            hessianAlgo = new AlgorithmHessian(srcImage, sigmas);

            System.out.println("Pass: " + scale + "   sigma: " + sigmas[0]);
            System.gc();

            // get the center plane in array coordinates
            plane = ((numPlanes / 2) - 1);

            for (plane--; plane < ((numPlanes / 2) + 2); plane++) { // do 3 center slices
                fireProgressStateChanged(Math.round(((float) (plane) / (numPlanes - 1) * 100)));

                for (row = 0; row < numRows; row++) {

                    for (col = 0; col < numCols; col++) {

                        // get the Hessian at this point
                        hess = hessianAlgo.hessian3D(sourceBuffer, extents, col, row, plane);

                        // fill up the eigenSolver matrix with the hessian
                        eigenSystemAlgo.SetData(0, 0, (float)hess[0][0]);
                        eigenSystemAlgo.SetData(0, 1, (float)hess[0][1]);
                        eigenSystemAlgo.SetData(0, 2, (float)hess[0][2]);

                        eigenSystemAlgo.SetData(1, 0, (float)hess[1][0]);
                        eigenSystemAlgo.SetData(1, 1, (float)hess[1][1]);
                        eigenSystemAlgo.SetData(1, 2, (float)hess[1][2]);

                        eigenSystemAlgo.SetData(2, 0, (float)hess[2][0]);
                        eigenSystemAlgo.SetData(2, 1, (float)hess[2][1]);
                        eigenSystemAlgo.SetData(2, 2, (float)hess[2][2]);

                        // OK, solve the eigen system
                        eigenSystemAlgo.IncrSortEigenStuff();

                        // extract the eigenvalues from the AlgorithmEigensolver
                        evals[0] = eigenSystemAlgo.GetEigenvalue(0);
                        magEvals[0] = Math.abs(evals[0]);

                        evals[1] = eigenSystemAlgo.GetEigenvalue(1);
                        magEvals[1] = Math.abs(evals[1]);

                        evals[2] = eigenSystemAlgo.GetEigenvalue(2);
                        magEvals[2] = Math.abs(evals[2]);
                        
                        // reset the eigen value look-up table
                        lut[0] = 0;
                        lut[1] = 1;
                        lut[2] = 2;

                        // put the smallest eigen val in the 0 element
                        if (magEvals[lut[1]] < magEvals[lut[0]]) {
                            tmp = lut[0];
                            lut[0] = lut[1];
                            lut[1] = tmp;
                        }

                        if (magEvals[lut[2]] < magEvals[lut[0]]) {
                            tmp = lut[0];
                            lut[0] = lut[2];
                            lut[2] = tmp;
                        }

                        // put the next smallest element in the 1st element
                        if (magEvals[lut[2]] < magEvals[lut[1]]) {
                            tmp = lut[1];
                            lut[1] = lut[2];
                            lut[2] = tmp;
                        }

                        blobRatio = magEvals[lut[0]] / (Math.sqrt(magEvals[lut[1]] * magEvals[lut[2]]));
                        areaRatio = magEvals[lut[1]] / magEvals[lut[2]];
                        structureness = Math.sqrt((evals[lut[0]] * evals[lut[0]]) + (evals[lut[1]] * evals[lut[1]]) +
                                                  (evals[lut[2]] * evals[lut[2]]));

                        if ((evals[lut[1]] > 0) || (evals[lut[2]] > 0)) {
                            vesselness = 0.0f;
                        } else {
                            expRa = Math.exp(-((areaRatio * areaRatio) / (2 * a * a)));
                            expRb = Math.exp(-((blobRatio * blobRatio) / (2 * b * b)));
                            expS = Math.exp(-((structureness * structureness) / (2 * c * c)));
                            vesselness = (1.0 - expRa) * expRb * (1.0 - expS);

                            if (expRa > maxExpRa) {
                                maxExpRa = expRa;
                            }

                            if (expRb > maxExpRb) {
                                maxExpRb = expRb;
                            }

                            if (expS > maxExpS) {
                                maxExpS = expS;
                            }
                        }

                        if (structureness > maxS) {
                            maxS = structureness;
                        }

                        if (vesselness > maxV) {
                            maxV = vesselness;
                        }

                        // output result on different slices
                        resultBuffer[(plane * numRows * numCols) + (row * numCols) + col] = (float) (vesselness *
                                                                                                         1000.0);

                    } // end for (col = 0; ...)
                } // end for (row = 0; ...)

                /**/
                System.out.println("Slice Number: " + fltFmt.format(plane));
                System.out.println("Max maxExpRa: " + fltFmt.format(maxExpRa));
                System.out.println("Max maxExpRb: " + fltFmt.format(maxExpRb));
                System.out.println("Max maxExpS: " + fltFmt.format(maxExpS));
                System.out.println("Max S: " + fltFmt.format(maxS));
                System.out.println("Max V: " + fltFmt.format(maxV));

                /**/

                maxExpRa = maxExpRb = maxExpS = maxS = maxV = 0.0;

            } // end for (plane = 0; ...)

            /*      PFH: I put this here to run the algorithm with different
             * scale values in batch mode.  Thes code saves the solution for each scale value
             *
             * // OK, the resultBuffer is filled with the results of the algorithm, // put this data into the source image
             * so it will be displayed in // in the ViewJFrameWizard
             *
             * destImage.releaseLock();
             *
             * try { destImage.importData(0, resultBuffer, true); // locks and releases lock } catch (IOException error) {
             * sourceBuffer = null; resultBuffer = null; errorCleanUp("AlgorithmImageHessian: Could NOT import
             * resultBuffer to the image", true); return; } // end try{}-catch{}
             *
             * destImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), srcImage.getImageName() + "_vsGauss" +
             * scale,FileUtility.XML);
             */


        } // end for (scale = 1; ...)

        /*
         * System.out.println("Max maxExpRa: " + maxExpRa); System.out.println("Max maxExpRb: " + maxExpRb);
         * System.out.println("Max maxExpS: " + maxExpS); System.out.println("Max S: " + maxS); System.out.println("Max
         * V: " + maxV);
         */

        // OK, the resultBuffer is filled with the results of the algorithm,
        // put this data into the source image so it will be displayed in
        // in the ViewJFrameWizard

        destImage.releaseLock();

        try {
            destImage.importData(0, resultBuffer, true); // locks and releases lock
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

            return;
        } // end try{}-catch{}

        // The meat is done

        

        if (threadStopped) {
            finalize();

            return;
        }

        setCompleted(true);

    } // end runInDest3D()

    /**
     * DOCUMENT ME!
     */
    private void runInPlace3D() {
        DecimalFormat fltFmt = new DecimalFormat("0.00");

        fireProgressStateChanged(srcImage.getImageName(), "Hessian/Eigen System ...");

        // OK, here is where the meat of the algorithm goes

        int length, numCols, numRows, numPlanes = 0;
        int[] extents = srcImage.getExtents();
        numCols = extents[0];
        numRows = extents[1];
        numPlanes = extents[2];
        length = numCols * numRows * numPlanes;

        // buffers for the image data
        float[] sourceBuffer;
        float[] resultBuffer;

        // copy the image data into the sourceBuffer so we can access it
        try {
            sourceBuffer = new float[length];
            srcImage.exportData(0, length, sourceBuffer); // locks and releases lock
            resultBuffer = new float[length];
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: could NOT export source image", true);

            return;
        } catch (OutOfMemoryError e) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: Out of memory when creating image buffer", true);

            return;
        } // end try{}-catch{}-catch{}

        int col, row, plane;

        // get the center plane in array coordinates
        plane = ((numPlanes / 2) - 1);

        // Hessian and eigen stuff
        double[][] hess = null;
        double[] evals = new double[3];
        double[] magEvals = new double[3];
        int[] lut = new int[3];
        lut[0] = 0;
        lut[1] = 1;
        lut[2] = 2;

        int tmp;

        double a = 0.5f;
        double b = 0.5f;
        double c = 50.0f;
        double expRa = 0, expRb = 0, expS = 0;

        float[] sigmas = new float[3];
        double blobRatio, areaRatio, structureness, vesselness, maxV = 0.0, maxS = 0.0;
        double maxExpRa = 0, maxExpRb = 0, maxExpS = 0;

        for (int scale = 1; scale < 2; scale++) {

            System.out.println("Scale: " + scale);
            System.gc();

            sigmas[0] = sigmas[1] = sigmas[2] = 0.5f;
            hessianAlgo = new AlgorithmHessian(srcImage, sigmas);

            for (plane = 0; plane < numPlanes; plane++) {
                fireProgressStateChanged(Math.round(((float) (plane) / (numPlanes - 1) * 100)));

                for (row = 0; row < numRows; row++) {

                    for (col = 0; col < numCols; col++) {

                        // get the Hessian at this point
                        hess = hessianAlgo.hessian3D(sourceBuffer, extents, col, row, plane);

                        // fill up the eigenSolver matrix with the hessian
                        eigenSystemAlgo.SetData(0, 0, (float)hess[0][0]);
                        eigenSystemAlgo.SetData(0, 1, (float)hess[0][1]);
                        eigenSystemAlgo.SetData(0, 2, (float)hess[0][2]);

                        eigenSystemAlgo.SetData(1, 0, (float)hess[1][0]);
                        eigenSystemAlgo.SetData(1, 1, (float)hess[1][1]);
                        eigenSystemAlgo.SetData(1, 2, (float)hess[1][2]);

                        eigenSystemAlgo.SetData(2, 0, (float)hess[2][0]);
                        eigenSystemAlgo.SetData(2, 1, (float)hess[2][1]);
                        eigenSystemAlgo.SetData(2, 2, (float)hess[2][2]);

                        // OK, solve the eigen system
                        eigenSystemAlgo.IncrSortEigenStuff();

                        
                        // extract the eigenvalues from the AlgorithmEigensolver
                        evals[0] = eigenSystemAlgo.GetEigenvalue(0);
                        magEvals[0] = Math.abs(evals[0]);

                        evals[1] = eigenSystemAlgo.GetEigenvalue(1);
                        magEvals[1] = Math.abs(evals[1]);

                        evals[2] = eigenSystemAlgo.GetEigenvalue(2);
                        magEvals[2] = Math.abs(evals[2]);

                        // reset the eigen value look-up table
                        lut[0] = 0;
                        lut[1] = 1;
                        lut[2] = 2;

                        // put the smallest eigen val in the 0 element
                        if (magEvals[lut[1]] < magEvals[lut[0]]) {
                            tmp = lut[0];
                            lut[0] = lut[1];
                            lut[1] = tmp;
                        }

                        if (magEvals[lut[2]] < magEvals[lut[0]]) {
                            tmp = lut[0];
                            lut[0] = lut[2];
                            lut[2] = tmp;
                        }

                        // put the next smallest element in the 1st element
                        if (magEvals[lut[2]] < magEvals[lut[1]]) {
                            tmp = lut[1];
                            lut[1] = lut[2];
                            lut[2] = tmp;
                        }

                        blobRatio = magEvals[lut[0]] / (Math.sqrt(magEvals[lut[1]] * magEvals[lut[2]]));
                        areaRatio = magEvals[lut[1]] / magEvals[lut[2]];
                        structureness = Math.sqrt((evals[lut[0]] * evals[lut[0]]) + (evals[lut[1]] * evals[lut[1]]) +
                                                  (evals[lut[2]] * evals[lut[2]]));

                        if ((row == 36) && (col == 92)) {
                            System.out.print("blobRatio: " + fltFmt.format(magEvals[lut[0]]) + " / sqrt( " +
                                             fltFmt.format(magEvals[lut[1]]) + " * " + fltFmt.format(magEvals[lut[2]]) +
                                             " )");
                            System.out.println("   " + fltFmt.format(blobRatio));

                            System.out.print("areaRatio: " + fltFmt.format(magEvals[lut[1]]) + " / " +
                                             fltFmt.format(magEvals[lut[2]]));
                            System.out.println("   " + fltFmt.format(areaRatio));
                            System.out.println("structureness: " + fltFmt.format(structureness));
                        } // end if (row == 36 ...)

                        if ((evals[lut[1]] > 0) || (evals[lut[2]] > 0)) {
                            vesselness = 0.0f;
                        } else {
                            expRa = Math.exp(-((areaRatio * areaRatio) / (2 * a * a)));
                            expRb = Math.exp(-((blobRatio * blobRatio) / (2 * b * b)));
                            expS = Math.exp(-((structureness * structureness) / (2 * c * c)));
                            vesselness = (1.0 - expRa) * expRb * (1.0 - expS);

                            if (expRa > maxExpRa) {
                                maxExpRa = expRa;
                            }

                            if (expRb > maxExpRb) {
                                maxExpRb = expRb;
                            }

                            if (expS > maxExpS) {
                                maxExpS = expS;
                            }
                        }

                        if ((row == 36) && (col == 92)) {
                            System.out.println("expRa: " + fltFmt.format(expRa) + "  (1.0 - expRa): " +
                                               fltFmt.format((1.0 - expRa)));
                            System.out.println("expRb: " + fltFmt.format(expRb));
                            System.out.println("expS: " + fltFmt.format(expS) + "  (1.0 - expS): " +
                                               fltFmt.format((1.0 - expS)));
                            System.out.println("vesselness: " + fltFmt.format(vesselness));

                            System.out.println("");
                        } // end if (row == 36 ...)

                        if (structureness > maxS) {
                            maxS = structureness;
                        }

                        if (vesselness > maxV) {
                            maxV = vesselness;
                        }

                        // output result on different slices
                        resultBuffer[(plane * numRows * numCols) + (row * numCols) + col] = (float) (vesselness *
                                                                                                         100.0);

                    } // end for (col = 0; ...)
                } // end for (row = 0; ...)

            } // end for (plane = 0; ...)

        } // end for (scale = 1; ...)

        // OK, the resultBuffer is filled with the results of the algorithm,
        // put this data into the source image so it will be displayed in
        // in the ViewJFrameWizard
        try {
            srcImage.importData(0, resultBuffer, true); // locks and releases lock
        } catch (IOException error) {
            sourceBuffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmImageHessian: Could NOT import resultBuffer to the image", true);

            return;
        } // end try{}-catch{}

        // The meat is done

        

        if (threadStopped) {
            finalize();

            return;
        }

    } // end runInPlace3D()

} // end class AlgorithmImageHessian
