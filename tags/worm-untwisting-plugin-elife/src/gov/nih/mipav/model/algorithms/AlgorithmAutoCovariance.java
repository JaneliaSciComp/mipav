package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * let deli(x,y) = (i(x,y) - <i(x,y)>)/<i(x,y)> where the angle brackets are used to denote a spatial average. Then the
 * two-dimensional autocovariance function, g(e,n) is defined as g(e,n) = <deli(x,y)deli(x + e, y + n)> which for a M by
 * P discrete data set is calculated as g(e,n) = (num/denom) - 1 where num = (1/(M - e)(P - n))sum from j = 1 to M-e sum
 * from k = 1 to P-n for: i(j,k)*i(j+e,k+n) denom = [(1/(2(M-e)(P-n))) sum from j = 1 to M sum from k = 1 to P of i(j,k)
 * + i(j+e,k+n)]**2 Note that while some medical literature refers to this as a autocorrelation, it is actually an
 * autocovariance since means are subtracted. 3 articles in which this formula are used: 1.) "Two-photon image
 * correlation spectroscopy and image cross-correlation spectroscopy" by P. W. Wiseman, J.A. Squier, M.H. Ellisman, and
 * K.R. Wilson, Journal of Microscopy, Vol. 200. Pt. 1, October 2000, pp. 14-25. 2.) "Quantitation of Membrane Receptor
 * Distributions by Image Correlation Spectroscopy: Concept and Application" by Nils O. Petersen, Pia L. Hoddelius, Paul
 * W. Wiseman, Olle Seger, and Karl-Eric Magnusson, Biophysical Journal, Volume 65, September, 1993, pp. 1135-1146. 3.)
 * "Image cross-correlation spectroscopy: A new experimental bipohysical approach to measurement of slow diffusion of
 * fluorescent molecules" by Mamta Srivastava & Nils O. Petersen, Methods in Cell Science, Vol 18, March, 1996, pp.
 * 47-54.
 *
 * <p>The autocovariance fits to a function of the form a0 + a1*exp(-(x**2 + y**2)/w**2) = a0 + a1*exp(a2*distSqr),
 * where a2 < 0. The fitting is performed to determine the full width at half maximum of the autocovariance. For color
 * images the full width at half maximum is considered to be the minimum of the red, green, and blue values.</p>
 */
public class AlgorithmAutoCovariance extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage destImageB = null;

    /** DOCUMENT ME! */
    private ModelImage destImageG = null;

    /** DOCUMENT ME! */
    private ModelImage destImageR = null;

    /** full width at half maximum of the autocovariance. */
    private int fwhm = Integer.MAX_VALUE;

    /** DOCUMENT ME! */
    private int fwhmB = Integer.MAX_VALUE;

    /** DOCUMENT ME! */
    private int fwhmG = Integer.MAX_VALUE;

    /** DOCUMENT ME! */
    private int fwhmR = Integer.MAX_VALUE;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for black and white image in which covariance coefficients are placed in a predetermined destination
     * image.
     *
     * @param  destImg  Image model where result image is stored.
     * @param  srcImg   Source image model.
     */
    public AlgorithmAutoCovariance(ModelImage destImg, ModelImage srcImg) {
        super(destImg, srcImg);
    }

    /**
     * Constructor for color image in which covariance coefficients are placed in predetermined destination images.
     *
     * @param  destImageR  Image model where red result is stored.
     * @param  destImageG  Image model where green result is stored.
     * @param  destImageB  Image model where blue result is stored.
     * @param  srcImg      Source image model.
     */
    public AlgorithmAutoCovariance(ModelImage destImageR, ModelImage destImageG, ModelImage destImageB,
                                   ModelImage srcImg) {
        super(null, srcImg);
        this.destImageR = destImageR;
        this.destImageG = destImageG;
        this.destImageB = destImageB;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        destImageR = null;
        destImageG = null;
        destImageB = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * returns the full width at half maximum of the autocovariance.
     *
     * @return  fwhm
     */
    public int getFWHM() {
        return fwhm;
    }

    /**
     * returns the full width at half maximum of the blue autocovariance.
     *
     * @return  fwhmB
     */
    public int getFWHMB() {
        return fwhmB;
    }

    /**
     * returns the full width at half maximum of the green autocovariance.
     *
     * @return  fwhmG
     */
    public int getFWHMG() {
        return fwhmG;
    }

    /**
     * returns the full width at half maximum of the red autocovariance.
     *
     * @return  fwhmR
     */
    public int getFWHMR() {
        return fwhmR;
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if ((destImage == null) && (destImageR == null) && (destImageG == null) && (destImageB == null)) {
            displayError("Destination Image is null");

            return;
        }

        

        if (srcImage.isColorImage()) {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2DC();
            } else if (srcImage.getNDims() == 3) {
                calcStoreInDest3DC();
            } else {
                calcStoreInDest4DC();
            }
        } else {

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() == 3) {
                calcStoreInDest3D();
            } else {
                calcStoreInDest4D();
            }
        }
    }

    /**
     * This function calculates the autocovariance coefficients and places them in the destination image for black and
     * white images.
     */
    private void calcStoreInDest2D() {
        float[] buffer;
        float[] resultBuffer;
        float average;
        int averageCount;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int deltaX, deltaY;
        int i;
        int x, y;
        int oldValue = 0;
        int newValue = 0;
        int nPoints = 0;
        double[] params;
        FitCovarianceModel fcm = null;
        float[] xValues = null;
        float[] yValues = null;
        double[] initial = null;
        boolean found;
        int xLast = 0;
        int yLast = yDim - 1;
        int thisPos, nextPos, resultPos;
        int this1, next1, result1;
        int ymdelta;
        int yPos, invert;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Autocovariance reports: destination image locked", false);

            return;
        }

        try {

            // image length is length in 2 dims
            buffer = new float[sliceSize];
            resultBuffer = new float[sliceSize];
            srcImage.exportData(0, sliceSize, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance reports: out of memory", true);

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Calculating autocovariance ...");

        for (deltaY = 0; deltaY < yDim; deltaY++) {
            newValue = deltaY * 100 / yDim;

            if ((newValue > oldValue)) {
                fireProgressStateChanged(newValue);
            }

            oldValue = newValue;
            ymdelta = yDim - deltaY;
            result1 = deltaY * xDim;

            for (deltaX = 0; deltaX < xDim; deltaX++) {
                resultPos = deltaX + result1;
                average = 0.0f;
                averageCount = 0;

                for (y = 0; y < (yDim - deltaY); y++) {
                    this1 = y * xDim;
                    next1 = deltaX + ((y + deltaY) * xDim);

                    for (x = 0; x < (xDim - deltaX); x++) {
                        thisPos = x + this1;
                        nextPos = x + next1;
                        resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                        average += buffer[thisPos];
                        average += buffer[nextPos];
                        averageCount += 2;
                    }
                }

                // Normalize for the varying number of terms
                resultBuffer[resultPos] /= ((xDim - deltaX) * ymdelta);
                average /= averageCount;
                resultBuffer[resultPos] /= (average * average);
                resultBuffer[resultPos] -= 1.0f;
            }
        }

        fireProgressStateChanged(100);

        for (y = 0; y < yDim; y++) {
            yPos = y * xDim;
            invert = (yDim - 1 - y) * xDim;

            for (x = 0; x < xDim; x++) {
                buffer[x + invert] = resultBuffer[x + yPos];
            }
        }

        destImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else

        if (threadStopped) {
            finalize();

            return;
        }

        try { // but now place buffer data into the image
            destImage.importData(0, buffer, true);
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

            return;
        }

        // The autocovariance fits to a function of the form
        // a0 + a1*exp(-(x**2 + y**2)/w**2) = a0 + a1*exp(a2*distSqr),
        // where a2 < 0.

        initial = new double[3];
        initial[0] = 0.01;
        initial[1] = buffer[(yDim - 1) * xDim];
        initial[2] = -1.0;

        found = false;
        yPos = sliceSize - xDim;

        for (x = 1; (x < xDim) && !found; x++) {

            if (buffer[x + yPos] > buffer[(x - 1) + yPos]) {
                xLast = x - 1;
                found = true;
            } else if (buffer[x + yPos] < (0.1f * buffer[yPos])) {
                xLast = x - 1;
                found = true;
            }
        }

        if (!found) {
            xLast = xDim - 1;
        }

        found = false;

        for (y = yDim - 2; (y >= 0) && !found; y--) {
            yPos = y * xDim;

            if (buffer[yPos] > buffer[yPos + xDim]) {
                yLast = y + 1;
                found = true;
            } else if (buffer[yPos] < (0.1f * buffer[sliceSize - xDim])) {
                yLast = y + 1;
                found = true;
            }
        }

        if (!found) {
            yLast = 0;
        }

        nPoints = (xLast + 1) * (yDim - yLast);
        xValues = new float[nPoints];
        yValues = new float[nPoints];

        for (i = 0, y = yDim - 1; y >= yLast; y--) {
            invert = (yDim - 1 - y);
            invert *= invert;
            yPos = y * xDim;

            for (x = 0; x <= xLast; x++) {
                xValues[i] = (x * x) + invert;
                yValues[i++] = buffer[x + yPos];
            }
        }

        fcm = new FitCovarianceModel(nPoints, xValues, yValues, initial);
        fcm.driver();
        fcm.dumpResults();
        params = fcm.getParameters();

        double c1 = buffer[sliceSize - xDim] / 2.0; // a0 + a1*exp(a2*distSqr)
        double c2 = c1 - params[0]; // a1*exp(a2*distSqr)
        double c3 = c2 / params[1]; // exp(a2*distSqr)

        if (c3 > 0.0) {
            double c4 = Math.log(c3) / params[2]; // distSqr
            fwhm = (int) ((2.0 * Math.sqrt(c4)) + 0.5);

            // System.out.println("c4 = "+ c4);
            // System.out.println("fwhm = " + fwhm);
            ViewUserInterface.getReference().setDataText("Auto covariance full width at half maximum = " + fwhm + "\n");
        } else {
            ViewUserInterface.getReference().setDataText("Cannot find auto covariance full width at half maximum\n");
        }

        setCompleted(true);
    }

    /**
     * This function calculates the autocovariance coefficients and places them in the destination images for color
     * images.
     */
    private void calcStoreInDest2DC() {

        float[] buffer;
        float[] resultBuffer;
        float average;
        int averageCount;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int sliceSize = xDim * yDim;
        int deltaX, deltaY;
        int i;
        int x, y;
        int oldValue = 0;
        int newValue = 0;
        int lastValue = 0;
        int colorsPresent = 0;
        int nPoints = 0;
        double[] params;
        FitCovarianceModel fcm = null;
        float[] xValues = null;
        float[] yValues = null;
        double[] initial = null;
        boolean found;
        int xLast = 0;
        int yLast = yDim - 1;
        int thisPos, nextPos, resultPos;
        int this1, next1, result1;
        int ymdelta;
        int yPos, invert;
        int yPresent;

        if (destImageR != null) {
            colorsPresent++;
        }

        if (destImageG != null) {
            colorsPresent++;
        }

        if (destImageB != null) {
            colorsPresent++;
        }

        yPresent = yDim * colorsPresent;

        try {

            // image length is length in 2 dims
            buffer = new float[sliceSize];
            resultBuffer = new float[sliceSize];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AutoCovariance reports: out of memory", true);

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Calculating autocovariance ...");

        if (destImageR != null) {

            try {
                srcImage.exportRGBData(1, 0, sliceSize, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: source image locked", true);

                return;
            }

            Preferences.debug("Doing red autocovariance\n", Preferences.DEBUG_ALGORITHM);

            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaY = 0; deltaY < yDim; deltaY++) {
                newValue = deltaY * 100 / yPresent;

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;
                result1 = deltaY * xDim;
                ymdelta = yDim - deltaY;

                for (deltaX = 0; deltaX < xDim; deltaX++) {
                    average = 0.0f;
                    averageCount = 0;
                    resultPos = deltaX + result1;

                    for (y = 0; y < (yDim - deltaY); y++) {
                        this1 = y * xDim;
                        next1 = deltaX + ((y + deltaY) * xDim);

                        for (x = 0; x < (xDim - deltaX); x++) {
                            thisPos = x + this1;
                            nextPos = x + next1;
                            resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                            average += buffer[thisPos];
                            average += buffer[nextPos];
                            averageCount += 2;
                        }
                    }

                    // Normalize for the varying number of terms
                    resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                    average /= averageCount;
                    resultBuffer[resultPos] /= (average * average);
                    resultBuffer[resultPos] -= 1.0f;
                }
            }

            fireProgressStateChanged(100 / colorsPresent);

            for (y = 0; y < yDim; y++) {
                yPos = y * xDim;
                invert = (yDim - 1 - y) * xDim;

                for (x = 0; x < xDim; x++) {
                    buffer[x + invert] = resultBuffer[x + yPos];
                }
            }

            lastValue = 100 / colorsPresent;

            if (threadStopped) {
                finalize();

                return;
            }

            try { // but now place buffer data into the image
                destImageR.importData(0, buffer, true);
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

                return;
            }

            // The autocovariance fits to a function of the form
            // a0 + a1*exp(-(x**2 + y**2)/w**2) = a0 + a1*exp(a2*distSqr),
            // where a2 < 0.

            initial = new double[3];
            initial[0] = 0.01;
            initial[1] = buffer[sliceSize - xDim];
            initial[2] = -1.0;

            found = false;
            yPos = (yDim - 1) * xDim;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + yPos] > buffer[(x - 1) + yPos]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + yPos] < (0.1f * buffer[yPos])) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {
                yPos = y * xDim;

                if (buffer[yPos] > buffer[yPos + xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[yPos] < (0.1f * buffer[sliceSize - xDim])) {
                    yLast = y + 1;
                    found = true;
                }
            }

            if (!found) {
                yLast = 0;
            }

            nPoints = (xLast + 1) * (yDim - yLast);
            xValues = new float[nPoints];
            yValues = new float[nPoints];

            for (i = 0, y = yDim - 1; y >= yLast; y--) {
                invert = yDim - 1 - y;
                invert *= invert;
                yPos = y * xDim;

                for (x = 0; x <= xLast; x++) {
                    xValues[i] = (x * x) + invert;
                    yValues[i++] = buffer[x + yPos];
                }
            }

            fcm = new FitCovarianceModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            double c1 = buffer[sliceSize - xDim] / 2.0; // a0 + a1*exp(a2*distSqr)
            double c2 = c1 - params[0]; // a1*exp(a2*distSqr)
            double c3 = c2 / params[1]; // exp(a2*distSqr)

            if (c3 > 0.0) {
                double c4 = Math.log(c3) / params[2]; // distSqr
                fwhmR = (int) ((2.0 * Math.sqrt(c4)) + 0.5);

                // System.out.println("c4 = "+ c4);
                // System.out.println("fwhmR = " + fwhmR);
                Preferences.debug("Auto covariance full width red at half maximum = " + fwhmR + "\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Auto covariance full width red at half maximum = " +
                                                             fwhmR + "\n");
            } else {
                Preferences.debug("Cannot find auto covariance full width red at half maximum\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Cannot find auto covariance full width red at half maximum");
            }

        } // if (destImageR != null)

        if (destImageG != null) {

            try {
                srcImage.exportRGBData(2, 0, sliceSize, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: source image locked", true);

                return;
            }

            Preferences.debug("Doing green autocovariance\n", Preferences.DEBUG_ALGORITHM);

            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaY = 0; deltaY < yDim; deltaY++) {
                newValue = lastValue + (deltaY * 100 / yPresent);

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;
                result1 = deltaY * xDim;
                ymdelta = (yDim - deltaY);

                for (deltaX = 0; deltaX < xDim; deltaX++) {
                    average = 0.0f;
                    averageCount = 0;
                    resultPos = deltaX + result1;

                    for (y = 0; y < (yDim - deltaY); y++) {
                        this1 = y * xDim;
                        next1 = deltaX + ((y + deltaY) * xDim);

                        for (x = 0; x < (xDim - deltaX); x++) {
                            thisPos = x + this1;
                            nextPos = x + next1;
                            resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                            average += buffer[thisPos];
                            average += buffer[nextPos];
                            averageCount += 2;
                        }
                    }

                    // Normalize for the varying number of terms
                    resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                    average /= averageCount;
                    resultBuffer[resultPos] /= (average * average);
                    resultBuffer[resultPos] -= 1.0f;
                }
            }

            fireProgressStateChanged(lastValue + (100 / colorsPresent));


            for (y = 0; y < yDim; y++) {
                yPos = y * xDim;
                invert = (yDim - 1 - y) * xDim;

                for (x = 0; x < xDim; x++) {
                    buffer[x + invert] = resultBuffer[x + yPos];
                }
            }

            lastValue = lastValue + (100 / colorsPresent);

            if (threadStopped) {
                finalize();

                return;
            }

            try { // but now place buffer data into the image
                destImageG.importData(0, buffer, true);
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

                return;
            }

            // The autocovariance fits to a function of the form
            // a0 + a1*exp(-(x**2 + y**2)/w**2) = a0 + a1*exp(a2*distSqr),
            // where a2 < 0.

            initial = new double[3];
            initial[0] = 0.01;
            initial[1] = buffer[sliceSize - xDim];
            initial[2] = -1.0;

            found = false;
            yPos = (yDim - 1) * xDim;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + yPos] > buffer[(x - 1) + yPos]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + yPos] < (0.1f * buffer[yPos])) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {
                yPos = y * xDim;

                if (buffer[yPos] > buffer[yPos + xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[yPos] < (0.1f * buffer[sliceSize - xDim])) {
                    yLast = y + 1;
                    found = true;
                }
            }

            if (!found) {
                yLast = 0;
            }

            nPoints = (xLast + 1) * (yDim - yLast);
            xValues = new float[nPoints];
            yValues = new float[nPoints];

            for (i = 0, y = yDim - 1; y >= yLast; y--) {
                yPos = y * xDim;
                invert = (yDim - 1 - y);
                invert *= invert;

                for (x = 0; x <= xLast; x++) {
                    xValues[i] = (x * x) + invert;
                    yValues[i++] = buffer[x + yPos];
                }
            }

            fcm = new FitCovarianceModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            double c1 = buffer[sliceSize - xDim] / 2.0; // a0 + a1*exp(a2*distSqr)
            double c2 = c1 - params[0]; // a1*exp(a2*distSqr)
            double c3 = c2 / params[1]; // exp(a2*distSqr)

            if (c3 > 0.0) {
                double c4 = Math.log(c3) / params[2]; // distSqr
                fwhmG = (int) ((2.0 * Math.sqrt(c4)) + 0.5);

                // System.out.println("c4 = "+ c4);
                // System.out.println("fwhmG = " + fwhmG);
                Preferences.debug("Auto covariance full width green at half maximum = " + fwhmG + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Auto covariance full width green at half maximum = " +
                                                             fwhmG + "\n");
            } else {
                Preferences.debug("Cannot find auto covariance full width green at half maximum\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Cannot find auto covariance full width green at half maximum\n");
            }

        } // if (destImageG != null)

        if (destImageB != null) {

            try {
                srcImage.exportRGBData(3, 0, sliceSize, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: source image locked", true);

                return;
            }

            Preferences.debug("Doing blue autocovariance\n", Preferences.DEBUG_ALGORITHM);

            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaY = 0; deltaY < yDim; deltaY++) {
                newValue = lastValue + (deltaY * 100 / yPresent);

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;
                result1 = deltaY * xDim;
                ymdelta = yDim - deltaY;

                for (deltaX = 0; deltaX < xDim; deltaX++) {
                    resultPos = deltaX + result1;
                    average = 0.0f;
                    averageCount = 0;

                    for (y = 0; y < (yDim - deltaY); y++) {
                        this1 = y * xDim;
                        next1 = deltaX + ((y + deltaY) * xDim);

                        for (x = 0; x < (xDim - deltaX); x++) {
                            thisPos = x + this1;
                            nextPos = x + next1;
                            resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                            average += buffer[thisPos];
                            average += buffer[nextPos];
                            averageCount += 2;
                        }
                    }

                    // Normalize for the varying number of terms
                    resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                    average /= averageCount;
                    resultBuffer[resultPos] /= (average * average);
                    resultBuffer[resultPos] -= 1.0f;
                }
            }

            fireProgressStateChanged(100);

            for (y = 0; y < yDim; y++) {
                yPos = y * xDim;
                invert = (yDim - 1 - y) * xDim;

                for (x = 0; x < xDim; x++) {
                    buffer[x + invert] = resultBuffer[x + yPos];
                }
            }

            if (threadStopped) {
                finalize();

                return;
            }

            try { // but now place buffer data into the image
                destImageB.importData(0, buffer, true);
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

                return;
            }

            // The autocovariance fits to a function of the form
            // a0 + a1*exp(-(x**2 + y**2)/w**2) = a0 + a1*exp(a2*distSqr),
            // where a2 < 0.

            initial = new double[3];
            initial[0] = 0.01;
            initial[1] = buffer[sliceSize - xDim];
            initial[2] = -1.0;

            found = false;
            yPos = (yDim - 1) * xDim;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + yPos] > buffer[(x - 1) + yPos]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + yPos] < (0.1f * buffer[yPos])) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {
                yPos = y * xDim;

                if (buffer[yPos] > buffer[yPos + xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[yPos] < (0.1f * buffer[sliceSize - xDim])) {
                    yLast = y + 1;
                    found = true;
                }
            }

            if (!found) {
                yLast = 0;
            }

            nPoints = (xLast + 1) * (yDim - yLast);
            xValues = new float[nPoints];
            yValues = new float[nPoints];

            for (i = 0, y = yDim - 1; y >= yLast; y--) {
                yPos = y * xDim;
                invert = yDim - 1 - y;
                invert *= invert;

                for (x = 0; x <= xLast; x++) {
                    xValues[i] = (x * x) + invert;
                    yValues[i++] = buffer[x + yPos];
                }
            }

            fcm = new FitCovarianceModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            double c1 = buffer[sliceSize - xDim] / 2.0; // a0 + a1*exp(a2*distSqr)
            double c2 = c1 - params[0]; // a1*exp(a2*distSqr)
            double c3 = c2 / params[1]; // exp(a2*distSqr)

            if (c3 > 0.0) {
                double c4 = Math.log(c3) / params[2]; // distSqr
                fwhmB = (int) ((2.0 * Math.sqrt(c4)) + 0.5);

                // System.out.println("c4 = "+ c4);
                // System.out.println("fwhmB = " + fwhmB);
                Preferences.debug("Auto covariance full width blue at half maximum = " + fwhmB + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Auto covariance full width blue at half maximum = " +
                                                             fwhmB + "\n");
            } else {
                Preferences.debug("Cannot find auto covariance full width blue at half maximum\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Cannot find auto covariance full width blue at half maximum\n");
            }
        } // if (destImageB != null)

        fwhm = Math.min(fwhmR, Math.min(fwhmG, fwhmB));

        setCompleted(true);
    }

    /**
     * This function calculates the autocovariance coefficients and places them in the destination image for black and
     * white images.
     */
    private void calcStoreInDest3D() {

        float[] buffer;
        float[] resultBuffer;
        float average;
        int averageCount;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int volSize = zDim * sliceSize;
        int deltaX, deltaY, deltaZ;
        int i;
        int x, y, z;
        int oldValue = 0;
        int newValue = 0;
        int nPoints = 0;
        double[] params;
        FitCovarianceModel fcm = null;
        float[] xValues = null;
        float[] yValues = null;
        double[] initial = null;
        boolean found;
        int xLast = 0;
        int yLast = yDim - 1;
        int zLast = 0;
        int thisPos, nextPos, resultPos;
        int this1, next1, result1, this2, next2, result2;
        int ymdelta, zmdelta;
        int yPos, zPos, invert, z2;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Autocovariance reports: destination image locked", false);

            return;
        }

        try {

            // image length is length in 3 dims
            buffer = new float[volSize];
            resultBuffer = new float[volSize];
            srcImage.exportData(0, volSize, buffer); // locks and releases lock

        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance: Out of memory creating process buffer", true);

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Calculating autocovariance ...");
        
        for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
            newValue = deltaZ * 100 / zDim;

            if ((newValue > oldValue)) {
                fireProgressStateChanged(newValue);
            }

            oldValue = newValue;
            result1 = deltaZ * sliceSize;
            zmdelta = zDim - deltaZ;

            for (deltaY = 0; deltaY < yDim; deltaY++) {
                result2 = (deltaY * xDim) + result1;
                ymdelta = (yDim - deltaY) * zmdelta;

                for (deltaX = 0; deltaX < xDim; deltaX++) {
                    average = 0.0f;
                    averageCount = 0;
                    resultPos = deltaX + result2;

                    for (z = 0; z < (zDim - deltaZ); z++) {
                        this1 = z * sliceSize;
                        next1 = deltaX + ((z + deltaZ) * sliceSize);

                        for (y = 0; y < (yDim - deltaY); y++) {
                            this2 = (y * xDim) + this1;
                            next2 = ((y + deltaY) * xDim) + next1;

                            for (x = 0; x < (xDim - deltaX); x++) {
                                thisPos = x + this2;
                                nextPos = x + next2;
                                resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                                average += buffer[thisPos];
                                average += buffer[nextPos];
                                averageCount += 2;
                            }
                        }
                    }

                    // Normalize for the varying number of terms
                    resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                    average /= averageCount;
                    resultBuffer[resultPos] /= (average * average);
                    resultBuffer[resultPos] -= 1.0f;
                }
            }
        }

        fireProgressStateChanged(100);

        for (z = 0; z < zDim; z++) {
            zPos = z * sliceSize;

            for (y = 0; y < yDim; y++) {
                yPos = (y * xDim) + zPos;
                invert = ((yDim - 1 - y) * xDim) + zPos;

                for (x = 0; x < xDim; x++) {
                    buffer[x + invert] = resultBuffer[x + yPos];
                }
            }
        }

        destImage.releaseLock();

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            destImage.importData(0, buffer, true);
        } catch (IOException e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

            return;
        }

        // The autocovariance fits to a function of the form
        // a0 + a1*exp(-(x**2 + y**2 + z**2)/w**2) = a0 + a1*exp(a2*distSqr),
        // where a2 < 0.

        initial = new double[3];
        initial[0] = 0.01;
        initial[1] = buffer[(yDim - 1) * xDim];
        initial[2] = -1.0;

        found = false;
        yPos = sliceSize - xDim;

        for (x = 1; (x < xDim) && !found; x++) {

            if (buffer[x + yPos] > buffer[(x - 1) + yPos]) {
                xLast = x - 1;
                found = true;
            } else if (buffer[x + yPos] < (0.1f * buffer[yPos])) {
                xLast = x - 1;
                found = true;
            }
        }

        if (!found) {
            xLast = xDim - 1;
        }

        found = false;

        for (y = yDim - 2; (y >= 0) && !found; y--) {
            yPos = y * xDim;

            if (buffer[yPos] > buffer[yPos + xDim]) {
                yLast = y + 1;
                found = true;
            } else if (buffer[yPos] < (0.1f * buffer[sliceSize - xDim])) {
                yLast = y + 1;
                found = true;
            }
        }

        if (!found) {
            yLast = 0;
        }

        found = false;
        yPos = (yDim - 1) * xDim;

        for (z = 1; (z < zDim) && !found; z++) {
            zPos = yPos + (z * sliceSize);

            if (buffer[zPos] > buffer[zPos - sliceSize]) {
                zLast = z - 1;
                found = true;
            } else if (buffer[zPos] < (0.1f * buffer[yPos])) {
                zLast = z - 1;
                found = true;
            }
        }

        if (!found) {
            zLast = zDim - 1;
        }

        nPoints = (xLast + 1) * (yDim - yLast) * (zLast + 1);
        xValues = new float[nPoints];
        yValues = new float[nPoints];

        for (i = 0, z = 0; z <= zLast; z++) {
            z2 = z * z;
            zPos = z * sliceSize;

            for (y = yDim - 1; y >= yLast; y--) {
                invert = (yDim - 1 - y);
                invert *= invert;
                invert = invert + z2;
                yPos = (y * xDim) + zPos;

                for (x = 0; x <= xLast; x++) {
                    xValues[i] = (x * x) + invert;
                    yValues[i++] = buffer[x + yPos];
                }
            }
        }

        fcm = new FitCovarianceModel(nPoints, xValues, yValues, initial);
        fcm.driver();
        fcm.dumpResults();
        params = fcm.getParameters();

        double c1 = buffer[sliceSize - xDim] / 2.0; // a0 + a1*exp(a2*distSqr)
        double c2 = c1 - params[0]; // a1*exp(a2*distSqr)
        double c3 = c2 / params[1]; // exp(a2*distSqr)

        if (c3 > 0.0) {
            double c4 = Math.log(c3) / params[2]; // distSqr
            fwhm = (int) ((2.0 * Math.sqrt(c4)) + 0.5);

            // System.out.println("c4 = "+ c4);
            // System.out.println("fwhm = " + fwhm);
            ViewUserInterface.getReference().setDataText("Auto covariance full width at half maximum = " + fwhm + "\n");
        } else {
            ViewUserInterface.getReference().setDataText("Cannot find auto covariance full width at half maximum\n");
        }

        setCompleted(true);
    }

    /**
     * This function calculates the autocovariance coefficients and places them in the destination images for color
     * images.
     */
    private void calcStoreInDest3DC() {

        float[] buffer;
        float[] resultBuffer;
        float average;
        int averageCount;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;
        int deltaX, deltaY, deltaZ;
        int i;
        int x, y, z;
        int oldValue = 0;
        int newValue = 0;
        int lastValue = 0;
        int colorsPresent = 0;
        int nPoints = 0;
        double[] params;
        FitCovarianceModel fcm = null;
        float[] xValues = null;
        float[] yValues = null;
        double[] initial = null;
        boolean found;
        int xLast = 0;
        int yLast = yDim - 1;
        int zLast = 0;
        int thisPos, nextPos, resultPos;
        int this1, next1, result1, this2, next2, result2;
        int ymdelta, zmdelta;
        int yPos, zPos, invert, z2;
        int zPresent;


        if (destImageR != null) {
            colorsPresent++;
        }

        if (destImageG != null) {
            colorsPresent++;
        }

        if (destImageB != null) {
            colorsPresent++;
        }

        zPresent = zDim * colorsPresent;


        try {

            // image length is length in 3 dims
            buffer = new float[volSize];
            resultBuffer = new float[volSize];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance: Out of memory creating process buffer", true);

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Calculating autocovariance ...");

        if (destImageR != null) {

            try {
                srcImage.exportRGBData(1, 0, volSize, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance: source image locked", true);

                return;
            }

            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                newValue = deltaZ * 100 / zPresent;

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;
                result1 = deltaZ * sliceSize;
                zmdelta = zDim - deltaZ;

                for (deltaY = 0; deltaY < yDim; deltaY++) {
                    result2 = (deltaY * xDim) + result1;
                    ymdelta = (yDim - deltaY) * zmdelta;

                    for (deltaX = 0; deltaX < xDim; deltaX++) {
                        resultPos = deltaX + result2;
                        average = 0.0f;
                        averageCount = 0;

                        for (z = 0; z < (zDim - deltaZ); z++) {
                            this1 = z * sliceSize;
                            next1 = deltaX + ((z + deltaZ) * sliceSize);

                            for (y = 0; y < (yDim - deltaY); y++) {
                                this2 = (y * xDim) + this1;
                                next2 = ((y + deltaY) * xDim) + next1;

                                for (x = 0; x < (xDim - deltaX); x++) {
                                    thisPos = x + this2;
                                    nextPos = x + next2;
                                    resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                                    average += buffer[thisPos];
                                    average += buffer[nextPos];
                                    averageCount += 2;
                                }
                            }
                        }

                        // Normalize for the varying number of terms
                        resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                        average /= averageCount;
                        resultBuffer[resultPos] /= (average * average);
                        resultBuffer[resultPos] -= 1.0f;
                    }
                }
            }

            fireProgressStateChanged(100 / colorsPresent);

            for (z = 0; z < zDim; z++) {
                zPos = z * sliceSize;

                for (y = 0; y < yDim; y++) {
                    yPos = (y * xDim) + zPos;
                    invert = ((yDim - 1 - y) * xDim) + zPos;

                    for (x = 0; x < xDim; x++) {
                        buffer[x + invert] = resultBuffer[x + yPos];
                    }
                }
            }

            lastValue = 100 / colorsPresent;

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                destImageR.importData(0, buffer, true);
            } catch (IOException e) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

                return;
            }

            // The autocovariance fits to a function of the form
            // a0 + a1*exp(-(x**2 + y**2 + z**2)/w**2) = a0 + a1*exp(a2*distSqr),
            // where a2 < 0.

            initial = new double[3];
            initial[0] = 0.01;
            initial[1] = buffer[sliceSize - xDim];
            initial[2] = -1.0;

            found = false;
            yPos = sliceSize - xDim;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + yPos] > buffer[(x - 1) + yPos]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + yPos] < (0.1f * buffer[yPos])) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {
                yPos = y * xDim;

                if (buffer[yPos] > buffer[yPos + xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[yPos] < (0.1f * buffer[sliceSize - xDim])) {
                    yLast = y + 1;
                    found = true;
                }
            }

            if (!found) {
                yLast = 0;
            }

            found = false;
            yPos = sliceSize - xDim;

            for (z = 1; (z < zDim) && !found; z++) {
                zPos = yPos + (z * sliceSize);

                if (buffer[zPos] > buffer[zPos - sliceSize]) {
                    zLast = z - 1;
                    found = true;
                } else if (buffer[zPos] < (0.1f * buffer[yPos])) {
                    zLast = z - 1;
                    found = true;
                }
            }

            if (!found) {
                zLast = zDim - 1;
            }

            nPoints = (xLast + 1) * (yDim - yLast) * (zLast + 1);
            xValues = new float[nPoints];
            yValues = new float[nPoints];

            for (i = 0, z = 0; z <= zLast; z++) {
                z2 = z * z;
                zPos = z * sliceSize;

                for (y = yDim - 1; y >= yLast; y--) {
                    invert = yDim - 1 - y;
                    invert *= invert;
                    invert += z2;
                    yPos = (y * xDim) + zPos;

                    for (x = 0; x <= xLast; x++) {
                        xValues[i] = (x * x) + invert;
                        yValues[i++] = buffer[x + yPos];
                    }
                }
            }

            fcm = new FitCovarianceModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            double c1 = buffer[sliceSize - xDim] / 2.0; // a0 + a1*exp(a2*distSqr)
            double c2 = c1 - params[0]; // a1*exp(a2*distSqr)
            double c3 = c2 / params[1]; // exp(a2*distSqr)

            if (c3 > 0.0) {
                double c4 = Math.log(c3) / params[2]; // distSqr
                fwhmR = (int) ((2.0 * Math.sqrt(c4)) + 0.5);

                // System.out.println("c4 = "+ c4);
                // System.out.println("fwhmR = " + fwhmR);
                Preferences.debug("Auto covariance full width red at half maximum = " + fwhmR + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Auto covariance full width red at half maximum = " +
                                                             fwhmR + "\n");
            } else {
                Preferences.debug("Cannot find auto covariance full width red at half maximum\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Cannot find auto covariance full width red at half maximum\n");
            }
        } // if (destImageR != null)

        if (destImageG != null) {

            try {
                srcImage.exportRGBData(2, 0, volSize, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance: source image locked", true);

                return;
            }

            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                newValue = lastValue + (deltaZ * 100 / zPresent);

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;
                result1 = deltaZ * sliceSize;
                zmdelta = zDim - deltaZ;

                for (deltaY = 0; deltaY < yDim; deltaY++) {
                    result2 = (deltaY * xDim) + result1;
                    ymdelta = (yDim - deltaY) * zmdelta;

                    for (deltaX = 0; deltaX < xDim; deltaX++) {
                        average = 0.0f;
                        averageCount = 0;
                        resultPos = deltaX + result2;

                        for (z = 0; z < (zDim - deltaZ); z++) {
                            this1 = z * sliceSize;
                            next1 = deltaX + ((z + deltaZ) * sliceSize);

                            for (y = 0; y < (yDim - deltaY); y++) {
                                this2 = (y * xDim) + this1;
                                next2 = ((y + deltaY) * xDim) + next1;

                                for (x = 0; x < (xDim - deltaX); x++) {
                                    thisPos = x + this2;
                                    nextPos = x + next2;
                                    resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                                    average += buffer[thisPos];
                                    average += buffer[nextPos];
                                    averageCount += 2;
                                }
                            }
                        }

                        // Normalize for the varying number of terms
                        resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                        average /= averageCount;
                        resultBuffer[resultPos] /= (average * average);
                        resultBuffer[resultPos] -= 1.0f;
                    }
                }
            }

            fireProgressStateChanged(lastValue + (100 / colorsPresent));

            for (z = 0; z < zDim; z++) {
                zPos = z * sliceSize;

                for (y = 0; y < yDim; y++) {
                    yPos = (y * xDim) + zPos;
                    invert = ((yDim - 1 - y) * xDim) + zPos;

                    for (x = 0; x < xDim; x++) {
                        buffer[x + invert] = resultBuffer[x + yPos];
                    }
                }
            }

            lastValue = lastValue + (100 / colorsPresent);

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                destImageG.importData(0, buffer, true);
            } catch (IOException e) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

                return;
            }

            // The autocovariance fits to a function of the form
            // a0 + a1*exp(-(x**2 + y**2 + z**2)/w**2) = a0 + a1*exp(a2*distSqr),
            // where a2 < 0.

            initial = new double[3];
            initial[0] = 0.01;
            initial[1] = buffer[sliceSize - xDim];
            initial[2] = -1.0;

            found = false;
            yPos = sliceSize - xDim;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + yPos] > buffer[(x - 1) + yPos]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + yPos] < (0.1f * buffer[yPos])) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {
                yPos = y * xDim;

                if (buffer[yPos] > buffer[yPos + xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[yPos] < (0.1f * buffer[sliceSize - xDim])) {
                    yLast = y + 1;
                    found = true;
                }
            }

            if (!found) {
                yLast = 0;
            }

            found = false;
            yPos = sliceSize - xDim;

            for (z = 1; (z < zDim) && !found; z++) {
                zPos = yPos + (z * sliceSize);

                if (buffer[zPos] > buffer[zPos - sliceSize]) {
                    zLast = z - 1;
                    found = true;
                } else if (buffer[zPos] < (0.1f * buffer[yPos])) {
                    zLast = z - 1;
                    found = true;
                }
            }

            if (!found) {
                zLast = zDim - 1;
            }

            nPoints = (xLast + 1) * (yDim - yLast) * (zLast + 1);
            xValues = new float[nPoints];
            yValues = new float[nPoints];

            for (i = 0, z = 0; z <= zLast; z++) {
                z2 = z * z;
                zPos = z * sliceSize;

                for (y = yDim - 1; y >= yLast; y--) {
                    yPos = (y * xDim) + zPos;
                    invert = yDim - 1 - y;
                    invert *= invert;
                    invert += z2;

                    for (x = 0; x <= xLast; x++) {
                        xValues[i] = (x * x) + invert;
                        yValues[i++] = buffer[x + yPos];
                    }
                }
            }

            fcm = new FitCovarianceModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            double c1 = buffer[sliceSize - xDim] / 2.0; // a0 + a1*exp(a2*distSqr)
            double c2 = c1 - params[0]; // a1*exp(a2*distSqr)
            double c3 = c2 / params[1]; // exp(a2*distSqr)

            if (c3 > 0.0) {
                double c4 = Math.log(c3) / params[2]; // distSqr
                fwhmG = (int) ((2.0 * Math.sqrt(c4)) + 0.5);

                // System.out.println("c4 = "+ c4);
                // System.out.println("fwhmG = " + fwhmG);
                Preferences.debug("Auto covariance full width green at half maximum = " + fwhmG + "\n", 
                		Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Auto covariance full width green at half maximum = " +
                                                             fwhmG + "\n");
            } else {
                Preferences.debug("Cannot find auto covarinace full width green at half maximum\n", 
                		Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Cannot find auto covariance full width green at half maximum\n");
            }

        } // if (destImageG != null)

        if (destImageB != null) {

            try {
                srcImage.exportRGBData(3, 0, volSize, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance: source image locked", true);

                return;
            }

            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                newValue = lastValue + (deltaZ * 100 / zPresent);

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;
                result1 = deltaZ * sliceSize;
                zmdelta = zDim - deltaZ;

                for (deltaY = 0; deltaY < yDim; deltaY++) {
                    result2 = (deltaY * xDim) + result1;
                    ymdelta = (yDim - deltaY) * zmdelta;

                    for (deltaX = 0; deltaX < xDim; deltaX++) {
                        resultPos = deltaX + result2;
                        average = 0.0f;
                        averageCount = 0;

                        for (z = 0; z < (zDim - deltaZ); z++) {
                            this1 = z * sliceSize;
                            next1 = deltaX + ((z + deltaZ) * sliceSize);

                            for (y = 0; y < (yDim - deltaY); y++) {
                                this2 = (y * xDim) + this1;
                                next2 = ((y + deltaY) * xDim) + next1;

                                for (x = 0; x < (xDim - deltaX); x++) {
                                    thisPos = x + this2;
                                    nextPos = x + next2;
                                    resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                                    average += buffer[thisPos];
                                    average += buffer[nextPos];
                                    averageCount += 2;
                                }
                            }
                        }

                        // Normalize for the varying number of terms
                        resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                        average /= averageCount;
                        resultBuffer[resultPos] /= (average * average);
                        resultBuffer[resultPos] -= 1.0f;
                    }
                }
            }

            fireProgressStateChanged(100);

            for (z = 0; z < zDim; z++) {
                zPos = z * sliceSize;

                for (y = 0; y < yDim; y++) {
                    yPos = (y * xDim) + zPos;
                    invert = ((yDim - 1 - y) * xDim) + zPos;

                    for (x = 0; x < xDim; x++) {
                        buffer[x + invert] = resultBuffer[x + yPos];
                    }
                }
            }

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                destImageB.importData(0, buffer, true);
            } catch (IOException e) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

                return;
            }

            // The autocovariance fits to a function of the form
            // a0 + a1*exp(-(x**2 + y**2 + z**2)/w**2) = a0 + a1*exp(a2*distSqr),
            // where a2 < 0.

            initial = new double[3];
            initial[0] = 0.01;
            initial[1] = buffer[sliceSize - xDim];
            initial[2] = -1.0;

            found = false;
            yPos = sliceSize - xDim;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + yPos] > buffer[(x - 1) + yPos]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + yPos] < (0.1f * buffer[yPos])) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {
                yPos = y * xDim;

                if (buffer[yPos] > buffer[yPos + xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[yPos] < (0.1f * buffer[sliceSize - xDim])) {
                    yLast = y + 1;
                    found = true;
                }
            }

            if (!found) {
                yLast = 0;
            }

            found = false;
            yPos = sliceSize - xDim;

            for (z = 1; (z < zDim) && !found; z++) {
                zPos = yPos + (z * sliceSize);

                if (buffer[zPos] > buffer[zPos - sliceSize]) {
                    zLast = z - 1;
                    found = true;
                } else if (buffer[zPos] < (0.1f * buffer[yPos])) {
                    zLast = z - 1;
                    found = true;
                }
            }

            if (!found) {
                zLast = zDim - 1;
            }

            nPoints = (xLast + 1) * (yDim - yLast) * (zLast + 1);
            xValues = new float[nPoints];
            yValues = new float[nPoints];

            for (i = 0, z = 0; z <= zLast; z++) {
                z2 = z * z;
                zPos = z * sliceSize;

                for (y = yDim - 1; y >= yLast; y--) {
                    invert = yDim - 1 - y;
                    invert *= invert;
                    invert += z2;
                    yPos = (y * xDim) + zPos;

                    for (x = 0; x <= xLast; x++) {
                        xValues[i] = (x * x) + invert;
                        yValues[i++] = buffer[x + yPos];
                    }
                }
            }

            fcm = new FitCovarianceModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            double c1 = buffer[sliceSize - xDim] / 2.0; // a0 + a1*exp(a2*distSqr)
            double c2 = c1 - params[0]; // a1*exp(a2*distSqr)
            double c3 = c2 / params[1]; // exp(a2*distSqr)

            if (c3 > 0.0) {
                double c4 = Math.log(c3) / params[2]; // distSqr
                fwhmB = (int) ((2.0 * Math.sqrt(c4)) + 0.5);
                Preferences.debug("Auto covariance full width blue at half maximum = " + fwhmB + "\n", Preferences.DEBUG_ALGORITHM);

                // System.out.println("c4 = "+ c4);
                // System.out.println("fwhmB = " + fwhmB);
                ViewUserInterface.getReference().setDataText("Auto covariance full width blue at half maximum = " +
                                                             fwhmB + "\n");
            } else {
                Preferences.debug("Cannot find auto covariance full width blue at half maximum\n", Preferences.DEBUG_ALGORITHM);
                ViewUserInterface.getReference().setDataText("Cannot find auto covariance full width blue at half maximum\n");
            }

        } // if (destImageB != null)

        fwhm = Math.min(fwhmR, Math.min(fwhmG, fwhmB));

        setCompleted(true);
    }


    /**
     * This function calculates the autocovariance coefficients and places them in the destination image for black and
     * white images.
     */
    private void calcStoreInDest4D() {

        float[] buffer;
        float[] resultBuffer;
        float average;
        int averageCount;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;
        int timeSize = volSize * tDim;
        int deltaX, deltaY, deltaZ, deltaT;
        int i;
        int x, y, z, t;
        int oldValue = 0;
        int newValue = 0;
        int thisPos, nextPos, resultPos;
        int this1, next1, result1, this2, next2, result2, this3, next3, result3;
        int ymdelta, zmdelta, tmdelta;
        int yPos, zPos, tPos, invert;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Autocovariance reports: destination image locked", false);

            return;
        }

        try {

            // image length is length in 4 dims
            buffer = new float[timeSize];
            srcImage.exportData(0, timeSize, buffer); // locks and releases lock

        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance: Out of memory creating process buffer", true);

            return;
        }

        try {
            resultBuffer = new float[timeSize];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance reports: Out of memory because of resultBuffer", true);

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Calculating autocovariance ...");

        for (i = 0; i < resultBuffer.length; i++) {
            resultBuffer[i] = 0.0f;
        }

        for (deltaT = 0; deltaT < tDim; deltaT++) {
            newValue = deltaT * 100 / tDim;

            if ((newValue > oldValue)) {
                fireProgressStateChanged(newValue);
            }

            oldValue = newValue;
            result1 = deltaT * volSize;
            tmdelta = tDim - deltaT;

            for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                result2 = (deltaZ * sliceSize) + result1;
                zmdelta = (zDim - deltaZ) * tmdelta;

                for (deltaY = 0; deltaY < yDim; deltaY++) {
                    result3 = (deltaY * xDim) + result2;
                    ymdelta = (yDim - deltaY) * zmdelta;

                    for (deltaX = 0; deltaX < xDim; deltaX++) {
                        resultPos = deltaX + result3;
                        average = 0.0f;
                        averageCount = 0;

                        for (t = 0; t < (tDim - deltaT); t++) {
                            this1 = t * volSize;
                            next1 = deltaX + ((t + deltaT) * volSize);

                            for (z = 0; z < (zDim - deltaZ); z++) {
                                this2 = (z * sliceSize) + this1;
                                next2 = ((z + deltaZ) * sliceSize) + next1;

                                for (y = 0; y < (yDim - deltaY); y++) {
                                    this3 = (y * xDim) + this2;
                                    next3 = ((y + deltaY) * xDim) + next2;

                                    for (x = 0; x < (xDim - deltaX); x++) {
                                        thisPos = x + this3;
                                        nextPos = x + next3;
                                        resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                                        average += buffer[thisPos];
                                        average += buffer[nextPos];
                                        averageCount += 2;
                                    }
                                }
                            }
                        }

                        // Normalize for the varying number of terms
                        resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                        average /= averageCount;
                        resultBuffer[resultPos] /= (average * average);
                        resultBuffer[resultPos] -= 1.0f;
                    }
                }
            }
        }

        fireProgressStateChanged(100);

        for (t = 0; t < tDim; t++) {
            tPos = t * volSize;

            for (z = 0; z < zDim; z++) {
                zPos = (z * sliceSize) + tPos;

                for (y = 0; y < yDim; y++) {
                    invert = ((yDim - 1 - y) * xDim) + zPos;
                    yPos = (y * xDim) + zPos;

                    for (x = 0; x < xDim; x++) {
                        buffer[x + invert] = resultBuffer[x + yPos];
                    }
                }
            }
        }

        destImage.releaseLock();

        if (threadStopped) {
            finalize();

            return;
        }

        try {
            destImage.importData(0, buffer, true);
        } catch (IOException e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

            return;
        }

        setCompleted(true);
    }


    /**
     * This function calculates the autocovariance coefficients and places them in the destination images for color
     * images.
     */
    private void calcStoreInDest4DC() {

        float[] buffer;
        float[] resultBuffer;
        float average;
        int averageCount;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;
        int timeSize = volSize * tDim;
        int deltaX, deltaY, deltaZ, deltaT;
        int i;
        int x, y, z, t;
        int oldValue = 0;
        int newValue = 0;
        int lastValue = 0;
        int colorsPresent = 0;
        int thisPos, nextPos, resultPos;
        int this1, next1, result1, this2, next2, result2, this3, next3, result3;
        int ymdelta, zmdelta, tmdelta;
        int yPos, zPos, tPos, invert;
        int tPresent;


        if (destImageR != null) {
            colorsPresent++;
        }

        if (destImageG != null) {
            colorsPresent++;
        }

        if (destImageB != null) {
            colorsPresent++;
        }

        tPresent = tDim * colorsPresent;

        try {

            // image length is length in 4 dims
            buffer = new float[timeSize];
            resultBuffer = new float[timeSize];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocovariance: Out of memory creating process buffer", true);

            return;
        }
        
        fireProgressStateChanged(srcImage.getImageName(), "Calculating autocovariance ...");

        if (destImageR != null) {

            try {
                srcImage.exportRGBData(1, 0, timeSize, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance: source image locked", true);

                return;
            }

            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaT = 0; deltaT < tDim; deltaT++) {
                newValue = deltaT * 100 / tPresent;

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;
                result1 = deltaT * volSize;
                tmdelta = tDim - deltaT;

                for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                    result2 = (deltaZ * sliceSize) + result1;
                    zmdelta = (zDim - deltaZ) * tmdelta;

                    for (deltaY = 0; deltaY < yDim; deltaY++) {
                        result3 = (deltaY * xDim) + result2;
                        ymdelta = (yDim - deltaY) * zmdelta;

                        for (deltaX = 0; deltaX < xDim; deltaX++) {
                            average = 0.0f;
                            averageCount = 0;
                            resultPos = deltaX + result3;

                            for (t = 0; t < (tDim - deltaT); t++) {
                                this1 = t * volSize;
                                next1 = deltaX + ((t + deltaT) * volSize);

                                for (z = 0; z < (zDim - deltaZ); z++) {
                                    this2 = (z * sliceSize) + this1;
                                    next2 = ((z + deltaZ) * sliceSize) + next1;

                                    for (y = 0; y < (yDim - deltaY); y++) {
                                        this3 = (y * xDim) + this2;
                                        next3 = ((y + deltaY) * xDim) + next2;

                                        for (x = 0; x < (xDim - deltaX); x++) {
                                            thisPos = x + this3;
                                            nextPos = x + next3;
                                            resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                                            average += buffer[thisPos];
                                            average += buffer[nextPos];
                                            averageCount += 2;
                                        }
                                    }
                                }
                            }

                            // Normalize for the varying number of terms
                            resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                            average /= averageCount;
                            resultBuffer[resultPos] /= (average * average);
                            resultBuffer[resultPos] -= 1.0f;
                        }
                    }
                }
            }

            fireProgressStateChanged(100 / colorsPresent);

            for (t = 0; t < tDim; t++) {
                tPos = t * volSize;

                for (z = 0; z < zDim; z++) {
                    zPos = (z * sliceSize) + tPos;

                    for (y = 0; y < yDim; y++) {
                        yPos = (y * xDim) + zPos;
                        invert = ((yDim - 1 - y) * xDim) + zPos;

                        for (x = 0; x < xDim; x++) {
                            buffer[x + invert] = resultBuffer[x + yPos];
                        }
                    }
                }
            }

            lastValue = 100 / colorsPresent;

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                destImageR.importData(0, buffer, true);
            } catch (IOException e) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

                return;
            }
        } // if (destImageR != null)

        if (destImageG != null) {

            try {
                srcImage.exportRGBData(2, 0, timeSize, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance: source image locked", true);

                return;
            }

            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaT = 0; deltaT < tDim; deltaT++) {
                newValue = lastValue + (deltaT * 100 / (tDim * colorsPresent));

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;
                result1 = deltaT * volSize;
                tmdelta = tDim - deltaT;

                for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                    result2 = (deltaZ * sliceSize) + result1;
                    zmdelta = (zDim - deltaZ) * tmdelta;

                    for (deltaY = 0; deltaY < yDim; deltaY++) {
                        result3 = (deltaY * xDim) + result2;
                        ymdelta = (yDim - deltaY) * zmdelta;

                        for (deltaX = 0; deltaX < xDim; deltaX++) {
                            average = 0.0f;
                            averageCount = 0;
                            resultPos = deltaX + result3;

                            for (t = 0; t < (tDim - deltaT); t++) {
                                this1 = t * volSize;
                                next1 = deltaX + ((t + deltaT) * volSize);

                                for (z = 0; z < (zDim - deltaZ); z++) {
                                    this2 = (z * sliceSize) + this1;
                                    next2 = ((z + deltaZ) * sliceSize) + next1;

                                    for (y = 0; y < (yDim - deltaY); y++) {
                                        this3 = (y * xDim) + this2;
                                        next3 = ((y + deltaY) * xDim) + next2;

                                        for (x = 0; x < (xDim - deltaX); x++) {
                                            thisPos = x + this3;
                                            nextPos = x + next3;
                                            resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                                            average += buffer[thisPos];
                                            average += buffer[nextPos];
                                            averageCount += 2;
                                        }
                                    }
                                }
                            }

                            // Normalize for the varying number of terms
                            resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                            average /= averageCount;
                            resultBuffer[resultPos] /= (average * average);
                            resultBuffer[resultPos] -= 1.0f;
                        }
                    }
                }
            }

            fireProgressStateChanged(lastValue + (100 / colorsPresent));

            for (t = 0; t < tDim; t++) {
                tPos = t * volSize;

                for (z = 0; z < zDim; z++) {
                    zPos = (z * sliceSize) + tPos;

                    for (y = 0; y < yDim; y++) {
                        yPos = (y * xDim) + zPos;
                        invert = ((yDim - 1 - y) * xDim) + zPos;

                        for (x = 0; x < xDim; x++) {
                            buffer[x + invert] = resultBuffer[x + yPos];
                        }
                    }
                }
            }

            lastValue = lastValue + (100 / colorsPresent);

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                destImageG.importData(0, buffer, true);
            } catch (IOException e) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

                return;
            }
        } // if (destImageG != null)

        if (destImageB != null) {

            try {
                srcImage.exportRGBData(3, 0, timeSize, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance: source image locked", true);

                return;
            }

            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaT = 0; deltaT < tDim; deltaT++) {
                newValue = lastValue + (deltaT * 100 / tPresent);

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;
                result1 = deltaT * volSize;
                tmdelta = tDim - deltaT;

                for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                    result2 = (deltaZ * sliceSize) + result1;
                    zmdelta = (zDim - deltaZ) * tmdelta;

                    for (deltaY = 0; deltaY < yDim; deltaY++) {
                        result3 = (deltaY * xDim) + result2;
                        ymdelta = (yDim - deltaY) * zmdelta;

                        for (deltaX = 0; deltaX < xDim; deltaX++) {
                            average = 0.0f;
                            averageCount = 0;
                            resultPos = deltaX + result3;

                            for (t = 0; t < (tDim - deltaT); t++) {
                                this1 = t * volSize;
                                next1 = deltaX + ((t + deltaT) * volSize);

                                for (z = 0; z < (zDim - deltaZ); z++) {
                                    this2 = (z * sliceSize) + this1;
                                    next2 = ((z + deltaZ) * sliceSize) + next1;

                                    for (y = 0; y < (yDim - deltaY); y++) {
                                        this3 = (y * xDim) + this2;
                                        next3 = ((y + deltaY) * xDim) + next2;

                                        for (x = 0; x < (xDim - deltaX); x++) {
                                            thisPos = x + this3;
                                            nextPos = x + next3;
                                            resultBuffer[resultPos] += buffer[thisPos] * buffer[nextPos];
                                            average += buffer[thisPos];
                                            average += buffer[nextPos];
                                            averageCount += 2;
                                        }
                                    }
                                }
                            }

                            // Normalize for the varying number of terms
                            resultBuffer[resultPos] /= (xDim - deltaX) * ymdelta;
                            average /= averageCount;
                            resultBuffer[resultPos] /= (average * average);
                            resultBuffer[resultPos] -= 1.0f;
                        }
                    }
                }
            }

            fireProgressStateChanged(100);

            for (t = 0; t < tDim; t++) {
                tPos = t * volSize;

                for (z = 0; z < zDim; z++) {
                    zPos = (z * sliceSize) + tPos;

                    for (y = 0; y < yDim; y++) {
                        yPos = (y * xDim) + zPos;
                        invert = ((yDim - 1 - y) * xDim) + zPos;

                        for (x = 0; x < xDim; x++) {
                            buffer[x + invert] = resultBuffer[x + yPos];
                        }
                    }
                }
            }

            if (threadStopped) {
                finalize();

                return;
            }

            try {
                destImageB.importData(0, buffer, true);
            } catch (IOException e) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocovariance reports: destination image still locked", true);

                return;
            }
        } // if (destImageB != null)

        setCompleted(true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    class FitCovarianceModel extends NLConstrainedEngine {
    	private float[] xData;
        private float[] yData;

        /**
         * Creates a new FitCovarianceModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitCovarianceModel(int nPoints, float[] xData, float[] yData, double[] initial) {

            // nPoints data points, 3 coefficients, and exponential fitting
            super(nPoints, 3);
            this.xData = xData;
            this.yData = yData;
            
            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            
            // Constrain parameter 0
            bl[0] = -Double.MAX_VALUE;
            bu[0] = Double.MAX_VALUE;

            // Constrain parameter 1
            bl[1] = -Double.MAX_VALUE;
            bu[1] = Double.MAX_VALUE;
            
            // Constrain parameter 2
            bl[2] = -Double.MAX_VALUE;
            bu[2] = -Double.MIN_VALUE;
            
            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;

            gues[0] = initial[0];
            gues[1] = initial[1];
            gues[2] = initial[2];
        }


        /**
         * Starts the analysis.
         */
        public void driver() {
            super.driver();
        }


        /**
         * Display results of displaying exponential fitting parameters.
         */
        public void dumpResults() {
            Preferences.debug(" ******* FitCovarianceModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a2 " + String.valueOf(a[2]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /** 
         * @param a The best guess parameter values.
         * @param residuals ymodel - yData.
         * @param covarMat The derivative values of y with respect to fitting parameters.
         */
        public void fitToFunction(final double[] a, final double[] residuals, final double[][] covarMat) {
            int ctrl;
            int j;
            double ymod = 0;

            try {
                ctrl = ctrlMat[0];
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + " a[2] = " + a[2] + "\n", 
                // Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                    	ymod = a[0] + (a[1] * Math.exp(a[2] * xData[j]));
                        residuals[j] = ymod - yData[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                    	covarMat[j][0] = 1; // a0 partial derivative
                    	covarMat[j][1] = Math.exp(a[2] * xData[j]); // a1 partial derivative
                    	covarMat[j][2] = a[1] * xData[j] * Math.exp(a[2] * xData[j]); // a2 partial derivative
                    }
                }
                // Calculate the Jacobian numerically
                // else if (ctrl == 2) {
                // ctrlMat[0] = 0;
                // }
            } catch (final Exception exc) {
                Preferences.debug("function error: " + exc.getMessage() + "\n", Preferences.DEBUG_ALGORITHM);
            }

            return;
        }
    }
}
