package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Reference: Digital Image Processing, Second Edition by Rafael C. Gonzalez and Richard C. Woods, Prentice-Hall, Inc.,
 * 2002, pp. 205 - 208 and pp. 414-417. The autocorrelation coefficient = A(deltaX, deltaY)/A(0,0) where
 * A(deltaX,deltaY) = num/denom with num = sum from X = 0 to X = xDim - 1 - deltaX sum from Y = 0 to
 * Y = yDim - 1 - deltaY f(x,y)*f(x+deltaX,y+deltaY) denom = (xDim - deltaX)*(yDim - deltaY) Autocorrelation does
 * not subtract out the means while autocovariance does subtract out the means.
 *
 * <p>The autocorrelation coefficeints are fitted to a function of the form (1 - a0) + a0*exp(-(x**2 + y**2)/w**2) = (1
 * - a0) + a0*exp(a1*distSqr), where 1 >= a0 > 0, a1 < 0.</p>
 */
public class AlgorithmAutoCorrelation extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage destImageB = null;

    /** DOCUMENT ME! */
    private ModelImage destImageG = null;

    /** DOCUMENT ME! */
    private ModelImage destImageR = null;

    /** Full width at half maximum of the autocorrelation. */
    private int fwhm;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for black and white image in which correlation coefficients are placed in a predetermined destination
     * image.
     *
     * @param  destImg  Image model where result image is stored.
     * @param  srcImg   Source image model.
     */
    public AlgorithmAutoCorrelation(ModelImage destImg, ModelImage srcImg) {

        super(destImg, srcImg);

    }

    /**
     * Constructor for color image in which correlation coefficients are placed in predetermined destination images.
     *
     * @param  destImageR  Image model where red result is stored.
     * @param  destImageG  Image model where green result is stored.
     * @param  destImageB  Image model where blue result is stored.
     * @param  srcImg      Source image model.
     */
    public AlgorithmAutoCorrelation(ModelImage destImageR, ModelImage destImageG, ModelImage destImageB,
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
     * Returns the full width at half maximum of the autocorrelation.
     *
     * @return  fwhm
     */
    public int getFWHM() {
        return fwhm;
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
     * This function calculates the autocorrelation coefficients and places them in the destination image for black and
     * white images.
     */
    private void calcStoreInDest2D() {

        int length; // total number of data-elements (pixels) in image
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int deltaX, deltaY;
        int i;
        int x, y;
        float zeroCoefficient;
        int oldValue = 0;
        int newValue = 0;
        int nPoints = 0;
        double[] params;
        FitCorrelationModel fcm = null;
        float[] xValues = null;
        float[] yValues = null;
        double[] initial = null;
        boolean found;
        int xLast = 0;
        int yLast = yDim - 1;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Autocorrelation reports: destination image locked", false);

            return;
        }

        try {

            // image length is length in 2 dims
            length = xDim * yDim;

            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocorrelation reports: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocorrelation reports: out of memory", true);

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Calculating autocorrelation ..");


        for (deltaY = 0; deltaY < yDim; deltaY++) {
            newValue = deltaY * 100 / yDim;

            if ((newValue > oldValue)) {
                fireProgressStateChanged(newValue);
            }

            oldValue = newValue;

            for (deltaX = 0; deltaX < xDim; deltaX++) {

                for (y = 0; y < (yDim - deltaY); y++) {

                    for (x = 0; x < (xDim - deltaX); x++) {
                        resultBuffer[deltaX + (deltaY * xDim)] += buffer[x + (y * xDim)] *
                                                                      buffer[x + deltaX + ((y + deltaY) * xDim)];
                    }
                }

                // Normalize for the varying number of terms
                resultBuffer[deltaX + (deltaY * xDim)] /= ((xDim - deltaX) * (yDim - deltaY));
            }
        }

        fireProgressStateChanged(100);
        zeroCoefficient = resultBuffer[0];

        for (y = 0; y < yDim; y++) {

            for (x = 0; x < xDim; x++) {
                buffer[x + ((yDim - 1 - y) * xDim)] = resultBuffer[x + (y * xDim)] / zeroCoefficient;
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
            errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

            return;
        }

        // The autocorrelation fits to a function of the form
        // (1 - a0) + a0*exp(-(x**2 + y**2)/w**2) = (1 - a0) + a0*exp(a1*distSqr),
        // where 1 >= a0 > 0, a1 < 0.
        // The autocorrelation coefficients are normalized by dividing by the
        // value at zero displacement, so the autocorrelation for zero displacement
        // is always 1 by definition.

        initial = new double[2];
        initial[0] = 1.0 - destImage.getMin();
        initial[1] = -1.0;

        found = false;

        for (x = 1; (x < xDim) && !found; x++) {

            if (buffer[x + ((yDim - 1) * xDim)] >= buffer[(x - 1) + ((yDim - 1) * xDim)]) {
                xLast = x - 1;
                found = true;
            } else if (buffer[x + ((yDim - 1) * xDim)] < 0.1f) {
                xLast = x - 1;
                found = true;
            }
        }

        if (!found) {
            xLast = xDim - 1;
        }

        found = false;

        for (y = yDim - 2; (y >= 0) && !found; y--) {

            if (buffer[y * xDim] > buffer[(y + 1) * xDim]) {
                yLast = y + 1;
                found = true;
            } else if (buffer[y * xDim] < 0.1f) {
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

            for (x = 0; x <= xLast; x++) {
                xValues[i] = (x * x) + ((yDim - 1 - y) * (yDim - 1 - y));
                yValues[i++] = buffer[x + (y * xDim)];
            }
        }

        fcm = new FitCorrelationModel(nPoints, xValues, yValues, initial);
        fcm.driver();
        fcm.dumpResults();
        params = fcm.getParameters();

        if (params[0] > 0.5) {
            double c1 = Math.log((params[0] - 0.5) / params[0]);
            double c2 = c1 / params[1];
            double c3 = Math.sqrt(c2);
            fwhm = (int) ((2.0 * c3) + 0.5);

            /*System.out.println("c3 = "+ c3);
             *System.out.println("fwhm = " + fwhm);*/
            ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
            ViewUserInterface.getReference().setDataText("Auto correlation full width at half maximum = " + fwhm);
        } else {
            ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
            ViewUserInterface.getReference().setDataText("Auto correlation full width at half maximum too large to calculate");
            fwhm = Integer.MAX_VALUE;
        }

        setCompleted(true);
    }

    /**
     * This function calculates the autocorrelation coefficients and places them in the destination image for color
     * images.
     */
    private void calcStoreInDest2DC() {

        int length; // total number of data-elements (pixels) in image
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int deltaX, deltaY;
        int i;
        int x, y;
        float zeroCoefficient;
        int oldValue = 0;
        int newValue = 0;
        int lastValue = 0;
        int colorsPresent = 0;
        int nPoints = 0;
        double[] params;
        FitCorrelationModel fcm = null;
        float[] xValues = null;
        float[] yValues = null;
        double[] initial = null;
        boolean found;
        int xLast = 0;
        int yLast = yDim - 1;

        if (destImageR != null) {
            colorsPresent++;
        }

        if (destImageG != null) {
            colorsPresent++;
        }

        if (destImageB != null) {
            colorsPresent++;
        }

        try {

            // image length is length in 2 dims
            length = xDim * yDim;
            buffer = new float[length];
            resultBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AutoCorrelation reports: out of memory", true);

            return;
        }

        if (destImageR != null) {

            try {
                srcImage.exportRGBData(1, 0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocorrelation reports: source image locked", true);

                return;
            }

            fireProgressStateChanged(srcImage.getImageName(), "Calculating autocorrelation ..");


            for (deltaY = 0; deltaY < yDim; deltaY++) {
                newValue = deltaY * 100 / (yDim * colorsPresent);

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;

                for (deltaX = 0; deltaX < xDim; deltaX++) {

                    for (y = 0; y < (yDim - deltaY); y++) {

                        for (x = 0; x < (xDim - deltaX); x++) {
                            resultBuffer[deltaX + (deltaY * xDim)] += buffer[x + (y * xDim)] *
                                                                          buffer[x + deltaX + ((y + deltaY) * xDim)];
                        }
                    }

                    // Normalize for the varying number of terms
                    resultBuffer[deltaX + (deltaY * xDim)] /= (xDim - deltaX) * (yDim - deltaY);
                }
            }

            fireProgressStateChanged(100 / colorsPresent);
            zeroCoefficient = resultBuffer[0];

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    buffer[x + ((yDim - 1 - y) * xDim)] = resultBuffer[x + (y * xDim)] / zeroCoefficient;
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
                errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

                return;
            }
            
            //  The autocorrelation fits to a function of the form
            // (1 - a0) + a0*exp(-(x**2 + y**2)/w**2) = (1 - a0) + a0*exp(a1*distSqr),
            // where 1 >= a0 > 0, a1 < 0.
            // The autocorrelation coefficients are normalized by dividing by the
            // value at zero displacement, so the autocorrelation for zero displacement
            // is always 1 by definition.

            initial = new double[2];
            initial[0] = 1.0 - destImageR.getMin();
            initial[1] = -1.0;

            found = false;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + ((yDim - 1) * xDim)] >= buffer[(x - 1) + ((yDim - 1) * xDim)]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + ((yDim - 1) * xDim)] < 0.1f) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {

                if (buffer[y * xDim] > buffer[(y + 1) * xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[y * xDim] < 0.1f) {
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

                for (x = 0; x <= xLast; x++) {
                    xValues[i] = (x * x) + ((yDim - 1 - y) * (yDim - 1 - y));
                    yValues[i++] = buffer[x + (y * xDim)];
                }
            }

            fcm = new FitCorrelationModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            if (params[0] > 0.5) {
                double c1 = Math.log((params[0] - 0.5) / params[0]);
                double c2 = c1 / params[1];
                double c3 = Math.sqrt(c2);
                fwhm = (int) ((2.0 * c3) + 0.5);

                /*System.out.println("c3 = "+ c3);
                 *System.out.println("fwhm = " + fwhm);*/
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width red at half maximum = " + fwhm);
            } else {
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width red at half maximum too large to calculate");
                fwhm = Integer.MAX_VALUE;
            }
        } // if (destImageR != null)

        if (destImageG != null) {

            try {
                srcImage.exportRGBData(2, 0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocorrelation reports: source image locked", true);

                return;
            }


            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaY = 0; deltaY < yDim; deltaY++) {
                newValue = lastValue + (deltaY * 100 / (yDim * colorsPresent));

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;

                for (deltaX = 0; deltaX < xDim; deltaX++) {

                    for (y = 0; y < (yDim - deltaY); y++) {

                        for (x = 0; x < (xDim - deltaX); x++) {
                            resultBuffer[deltaX + (deltaY * xDim)] += buffer[x + (y * xDim)] *
                                                                          buffer[x + deltaX + ((y + deltaY) * xDim)];
                        }
                    }

                    // Normalize for the varying number of terms
                    resultBuffer[deltaX + (deltaY * xDim)] /= (xDim - deltaX) * (yDim - deltaY);
                }
            }

            fireProgressStateChanged(lastValue + (100 / colorsPresent));
            zeroCoefficient = resultBuffer[0];

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    buffer[x + ((yDim - 1 - y) * xDim)] = resultBuffer[x + (y * xDim)] / zeroCoefficient;
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
                errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

                return;
            }
            
            // The autocorrelation fits to a function of the form
            // (1 - a0) + a0*exp(-(x**2 + y**2)/w**2) = (1 - a0) + a0*exp(a1*distSqr),
            // where 1 >= a0 > 0, a1 < 0.
            // The autocorrelation coefficients are normalized by dividing by the
            // value at zero displacement, so the autocorrelation for zero displacement
            // is always 1 by definition.

            initial = new double[2];
            initial[0] = 1.0 - destImageG.getMin();
            initial[1] = -1.0;

            found = false;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + ((yDim - 1) * xDim)] >= buffer[(x - 1) + ((yDim - 1) * xDim)]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + ((yDim - 1) * xDim)] < 0.1f) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {

                if (buffer[y * xDim] > buffer[(y + 1) * xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[y * xDim] < 0.1f) {
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

                for (x = 0; x <= xLast; x++) {
                    xValues[i] = (x * x) + ((yDim - 1 - y) * (yDim - 1 - y));
                    yValues[i++] = buffer[x + (y * xDim)];
                }
            }

            fcm = new FitCorrelationModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            if (params[0] > 0.5) {
                double c1 = Math.log((params[0] - 0.5) / params[0]);
                double c2 = c1 / params[1];
                double c3 = Math.sqrt(c2);
                fwhm = (int) ((2.0 * c3) + 0.5);

                /*System.out.println("c3 = "+ c3);
                 *System.out.println("fwhm = " + fwhm);*/
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width green at half maximum = " + fwhm);
            } else {
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width green at half maximum too large to calculate");
                fwhm = Integer.MAX_VALUE;
            }
        } // if (destImageG != null)

        if (destImageB != null) {

            try {
                srcImage.exportRGBData(3, 0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocorrelation reports: source image locked", true);

                return;
            }


            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaY = 0; deltaY < yDim; deltaY++) {
                newValue = lastValue + (deltaY * 100 / (yDim * colorsPresent));

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;

                for (deltaX = 0; deltaX < xDim; deltaX++) {

                    for (y = 0; y < (yDim - deltaY); y++) {

                        for (x = 0; x < (xDim - deltaX); x++) {
                            resultBuffer[deltaX + (deltaY * xDim)] += buffer[x + (y * xDim)] *
                                                                          buffer[x + deltaX + ((y + deltaY) * xDim)];
                        }
                    }

                    // Normalize for the varying number of terms
                    resultBuffer[deltaX + (deltaY * xDim)] /= (xDim - deltaX) * (yDim - deltaY);
                }
            }

            fireProgressStateChanged(100);
            zeroCoefficient = resultBuffer[0];

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    buffer[x + ((yDim - 1 - y) * xDim)] = resultBuffer[x + (y * xDim)] / zeroCoefficient;
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
                errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

                return;
            }
            
            // The autocorrelation fits to a function of the form
            // (1 - a0) + a0*exp(-(x**2 + y**2)/w**2) = (1 - a0) + a0*exp(a1*distSqr),
            // where 1 >= a0 > 0, a1 < 0.
            // The autocorrelation coefficients are normalized by dividing by the
            // value at zero displacement, so the autocorrelation for zero displacement
            // is always 1 by definition.

            initial = new double[2];
            initial[0] = 1.0 - destImageB.getMin();
            initial[1] = -1.0;

            found = false;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + ((yDim - 1) * xDim)] >= buffer[(x - 1) + ((yDim - 1) * xDim)]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + ((yDim - 1) * xDim)] < 0.1f) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {

                if (buffer[y * xDim] > buffer[(y + 1) * xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[y * xDim] < 0.1f) {
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

                for (x = 0; x <= xLast; x++) {
                    xValues[i] = (x * x) + ((yDim - 1 - y) * (yDim - 1 - y));
                    yValues[i++] = buffer[x + (y * xDim)];
                }
            }

            fcm = new FitCorrelationModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            if (params[0] > 0.5) {
                double c1 = Math.log((params[0] - 0.5) / params[0]);
                double c2 = c1 / params[1];
                double c3 = Math.sqrt(c2);
                fwhm = (int) ((2.0 * c3) + 0.5);

                /*System.out.println("c3 = "+ c3);
                 *System.out.println("fwhm = " + fwhm);*/
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width blue at half maximum = " + fwhm);
            } else {
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width blue at half maximum too large to calculate");
                fwhm = Integer.MAX_VALUE;
            }
        } // if (destImageB != null)

        setCompleted(true);
    }

    /**
     * This function calculates the autocorrelation coefficients and places them in the destination image for black and
     * white images.
     */
    private void calcStoreInDest3D() {

        int length;
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int deltaX, deltaY, deltaZ;
        int i;
        int x, y, z;
        float zeroCoefficient;
        int oldValue = 0;
        int newValue = 0;
        int nPoints = 0;
        double[] params;
        FitCorrelationModel fcm = null;
        float[] xValues = null;
        float[] yValues = null;
        double[] initial = null;
        boolean found;
        int xLast = 0;
        int yLast = yDim - 1;
        int zLast = 0;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Autocorrelation reports: destination image locked", false);

            return;
        }

        try {

            // image length is length in 3 dims
            length = xDim * yDim * zDim;
            buffer = new float[length];
            resultBuffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating autocorrelation ..");
        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocorrelation: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocorrelation: Out of memory creating process buffer", true);

            return;
        }


        for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
            newValue = deltaZ * 100 / zDim;

            if ((newValue > oldValue)) {
                fireProgressStateChanged(newValue);
            }

            oldValue = newValue;

            for (deltaY = 0; deltaY < yDim; deltaY++) {

                for (deltaX = 0; deltaX < xDim; deltaX++) {
                    int idxRes = deltaX + (deltaY * xDim) + (deltaZ * sliceSize);

                    for (z = 0; z < (zDim - deltaZ); z++) {
                        int idxZ = z * sliceSize;
                        int idxZD = (z + deltaZ) * sliceSize;

                        for (y = 0; y < (yDim - deltaY); y++) {
                            int idx = (y * xDim) + idxZ;
                            int idxD = ((y + deltaY) * xDim) + idxZD;

                            for (x = 0; x < (xDim - deltaX); x++) {
                                resultBuffer[idxRes] += buffer[x + idx] * buffer[x + deltaX + idxD];
                            }
                        }
                    }

                    // Normalize for the varying number of terms
                    resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize)] /= (xDim - deltaX) * (yDim - deltaY) *
                                                                                         (zDim - deltaZ);
                }
            }
        }

        fireProgressStateChanged(100);
        zeroCoefficient = resultBuffer[0];

        for (z = 0; z < zDim; z++) {

            for (y = 0; y < yDim; y++) {

                for (x = 0; x < xDim; x++) {
                    buffer[x + ((yDim - 1 - y) * xDim) + (z * sliceSize)] = resultBuffer[x + (y * xDim) +
                                                                                         (z * sliceSize)] /
                                                                                zeroCoefficient;
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
            errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

            return;
        }

        // The autocorrelation fits to a function of the form
        // (1 - a0) + a0*exp(-(x**2 + y**2 + z**2)/w**2) =
        // (1 - a0) + a0*exp(a1*distSqr),
        // where 1 >= a0 > 0, a1 < 0.
        // The autocorrelation coefficients are normalized by dividing by the
        // value at zero displacement, so the autocorrelation for zero displacement
        // is always 1 by definition.

        initial = new double[2];
        initial[0] = 1.0 - destImage.getMin();
        initial[1] = -1.0;

        found = false;

        for (x = 1; (x < xDim) && !found; x++) {

            if (buffer[x + ((yDim - 1) * xDim)] >= buffer[(x - 1) + ((yDim - 1) * xDim)]) {
                xLast = x - 1;
                found = true;
            } else if (buffer[x + ((yDim - 1) * xDim)] < 0.1f) {
                xLast = x - 1;
                found = true;
            }
        }

        if (!found) {
            xLast = xDim - 1;
        }

        found = false;

        for (y = yDim - 2; (y >= 0) && !found; y--) {

            if (buffer[y * xDim] > buffer[(y + 1) * xDim]) {
                yLast = y + 1;
                found = true;
            } else if (buffer[y * xDim] < 0.1f) {
                yLast = y + 1;
                found = true;
            }
        }

        if (!found) {
            yLast = 0;
        }

        found = false;

        for (z = 1; (z < zDim) && !found; z++) {

            if (buffer[((yDim - 1) * xDim) + (z * sliceSize)] >= buffer[((yDim - 1) * xDim) + ((z - 1) * sliceSize)]) {
                zLast = z - 1;
                found = true;
            } else if (buffer[((yDim - 1) * xDim) + (z * sliceSize)] < 0.1f) {
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

            for (y = yDim - 1; y >= yLast; y--) {

                for (x = 0; x <= xLast; x++) {
                    xValues[i] = (x * x) + ((yDim - 1 - y) * (yDim - 1 - y)) + (z * z);
                    yValues[i++] = buffer[x + (y * xDim) + (z * sliceSize)];
                }
            }
        }

        fcm = new FitCorrelationModel(nPoints, xValues, yValues, initial);
        fcm.driver();
        fcm.dumpResults();
        params = fcm.getParameters();

        if (params[0] > 0.5) {
            double c1 = Math.log((params[0] - 0.5) / params[0]);
            double c2 = c1 / params[1];
            double c3 = Math.sqrt(c2);
            fwhm = (int) ((2.0 * c3) + 0.5);

            /*System.out.println("c3 = "+ c3);
             *System.out.println("fwhm = " + fwhm);*/
            ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
            ViewUserInterface.getReference().setDataText("Auto correlation full width at half maximum = " + fwhm);
        } else {
            ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
            ViewUserInterface.getReference().setDataText("Auto correlation full width at half maximum too large to calculate");
            fwhm = Integer.MAX_VALUE;
        }

        setCompleted(true);
    }

    /**
     * This function calculates the autocorrelation coefficients and places them in the destination image for color
     * images.
     */
    private void calcStoreInDest3DC() {

        int length;
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int deltaX, deltaY, deltaZ;
        int i;
        int x, y, z;
        float zeroCoefficient;
        int oldValue = 0;
        int newValue = 0;
        int lastValue = 0;
        int colorsPresent = 0;
        int nPoints = 0;
        double[] params;
        FitCorrelationModel fcm = null;
        float[] xValues = null;
        float[] yValues = null;
        double[] initial = null;
        boolean found;
        int xLast = 0;
        int yLast = yDim - 1;
        int zLast = 0;

        if (destImageR != null) {
            colorsPresent++;
        }

        if (destImageG != null) {
            colorsPresent++;
        }

        if (destImageB != null) {
            colorsPresent++;
        }

        try {

            // image length is length in 3 dims
            length = xDim * yDim * zDim;
            buffer = new float[length];
            resultBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocorrelation: Out of memory creating process buffer", true);

            return;
        }

        if (destImageR != null) {

            try {
                srcImage.exportRGBData(1, 0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocorrelation: source image locked", true);

                return;
            }

            fireProgressStateChanged(srcImage.getImageName(), "Calculating autocorrelation ..");


            for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                newValue = deltaZ * 100 / (zDim * colorsPresent);

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;

                for (deltaY = 0; deltaY < yDim; deltaY++) {

                    for (deltaX = 0; deltaX < xDim; deltaX++) {

                        for (z = 0; z < (zDim - deltaZ); z++) {

                            for (y = 0; y < (yDim - deltaY); y++) {

                                for (x = 0; x < (xDim - deltaX); x++) {
                                    resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize)] += buffer[x +
                                                                                                            (y * xDim) +
                                                                                                            (z *
                                                                                                                 sliceSize)] *
                                                                                                         buffer[x +
                                                                                                                    deltaX +
                                                                                                                    ((y +
                                                                                                                          deltaY) *
                                                                                                                         xDim) +
                                                                                                                    ((z +
                                                                                                                          deltaZ) *
                                                                                                                         sliceSize)];
                                }
                            }
                        }

                        // Normalize for the varying number of terms
                        resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize)] /= (xDim - deltaX) *
                                                                                             (yDim - deltaY) *
                                                                                             (zDim - deltaZ);
                    }
                }
            }

            fireProgressStateChanged(100 / colorsPresent);
            zeroCoefficient = resultBuffer[0];

            for (z = 0; z < zDim; z++) {

                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        buffer[x + ((yDim - 1 - y) * xDim) + (z * sliceSize)] = resultBuffer[x + (y * xDim) +
                                                                                             (z * sliceSize)] /
                                                                                    zeroCoefficient;
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
                errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

                return;
            }
            
            // The autocorrelation fits to a function of the form
            // (1 - a0) + a0*exp(-(x**2 + y**2 + z**2)/w**2) =
            // (1 - a0) + a0*exp(a1*distSqr),
            // where 1 >= a0 > 0, a1 < 0.
            // The autocorrelation coefficients are normalized by dividing by the
            // value at zero displacement, so the autocorrelation for zero displacement
            // is always 1 by definition.

            initial = new double[2];
            initial[0] = 1.0 - destImageR.getMin();
            initial[1] = -1.0;

            found = false;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + ((yDim - 1) * xDim)] >= buffer[(x - 1) + ((yDim - 1) * xDim)]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + ((yDim - 1) * xDim)] < 0.1f) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {

                if (buffer[y * xDim] > buffer[(y + 1) * xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[y * xDim] < 0.1f) {
                    yLast = y + 1;
                    found = true;
                }
            }

            if (!found) {
                yLast = 0;
            }

            found = false;

            for (z = 1; (z < zDim) && !found; z++) {

                if (buffer[((yDim - 1) * xDim) + (z * sliceSize)] >= buffer[((yDim - 1) * xDim) + ((z - 1) * sliceSize)]) {
                    zLast = z - 1;
                    found = true;
                } else if (buffer[((yDim - 1) * xDim) + (z * sliceSize)] < 0.1f) {
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

                for (y = yDim - 1; y >= yLast; y--) {

                    for (x = 0; x <= xLast; x++) {
                        xValues[i] = (x * x) + ((yDim - 1 - y) * (yDim - 1 - y)) + (z * z);
                        yValues[i++] = buffer[x + (y * xDim) + (z * sliceSize)];
                    }
                }
            }

            fcm = new FitCorrelationModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            if (params[0] > 0.5) {
                double c1 = Math.log((params[0] - 0.5) / params[0]);
                double c2 = c1 / params[1];
                double c3 = Math.sqrt(c2);
                fwhm = (int) ((2.0 * c3) + 0.5);

                /*System.out.println("c3 = "+ c3);
                 *System.out.println("fwhm = " + fwhm);*/
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width red at half maximum = " + fwhm);
            } else {
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width red at half maximum too large to calculate");
                fwhm = Integer.MAX_VALUE;
            }
        } // if (destImageR != null)

        if (destImageG != null) {

            try {
                srcImage.exportRGBData(2, 0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocorrelation: source image locked", true);

                return;
            }


            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                newValue = lastValue + (deltaZ * 100 / (zDim * colorsPresent));

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;

                for (deltaY = 0; deltaY < yDim; deltaY++) {

                    for (deltaX = 0; deltaX < xDim; deltaX++) {

                        for (z = 0; z < (zDim - deltaZ); z++) {

                            for (y = 0; y < (yDim - deltaY); y++) {

                                for (x = 0; x < (xDim - deltaX); x++) {
                                    resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize)] += buffer[x +
                                                                                                            (y * xDim) +
                                                                                                            (z *
                                                                                                                 sliceSize)] *
                                                                                                         buffer[x +
                                                                                                                    deltaX +
                                                                                                                    ((y +
                                                                                                                          deltaY) *
                                                                                                                         xDim) +
                                                                                                                    ((z +
                                                                                                                          deltaZ) *
                                                                                                                         sliceSize)];
                                }
                            }
                        }

                        // Normalize for the varying number of terms
                        resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize)] /= (xDim - deltaX) *
                                                                                             (yDim - deltaY) *
                                                                                             (zDim - deltaZ);
                    }
                }
            }

            fireProgressStateChanged(lastValue + (100 / colorsPresent));
            zeroCoefficient = resultBuffer[0];

            for (z = 0; z < zDim; z++) {

                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        buffer[x + ((yDim - 1 - y) * xDim) + (z * sliceSize)] = resultBuffer[x + (y * xDim) +
                                                                                             (z * sliceSize)] /
                                                                                    zeroCoefficient;
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
                errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

                return;
            }
            
            // The autocorrelation fits to a function of the form
            // (1 - a0) + a0*exp(-(x**2 + y**2 + z**2)/w**2) =
            // (1 - a0) + a0*exp(a1*distSqr),
            // where 1 >= a0 > 0, a1 < 0.
            // The autocorrelation coefficients are normalized by dividing by the
            // value at zero displacement, so the autocorrelation for zero displacement
            // is always 1 by definition.

            initial = new double[2];
            initial[0] = 1.0 - destImageG.getMin();
            initial[1] = -1.0;

            found = false;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + ((yDim - 1) * xDim)] >= buffer[(x - 1) + ((yDim - 1) * xDim)]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + ((yDim - 1) * xDim)] < 0.1f) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {

                if (buffer[y * xDim] > buffer[(y + 1) * xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[y * xDim] < 0.1f) {
                    yLast = y + 1;
                    found = true;
                }
            }

            if (!found) {
                yLast = 0;
            }

            found = false;

            for (z = 1; (z < zDim) && !found; z++) {

                if (buffer[((yDim - 1) * xDim) + (z * sliceSize)] >= buffer[((yDim - 1) * xDim) + ((z - 1) * sliceSize)]) {
                    zLast = z - 1;
                    found = true;
                } else if (buffer[((yDim - 1) * xDim) + (z * sliceSize)] < 0.1f) {
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

                for (y = yDim - 1; y >= yLast; y--) {

                    for (x = 0; x <= xLast; x++) {
                        xValues[i] = (x * x) + ((yDim - 1 - y) * (yDim - 1 - y)) + (z * z);
                        yValues[i++] = buffer[x + (y * xDim) + (z * sliceSize)];
                    }
                }
            }

            fcm = new FitCorrelationModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            if (params[0] > 0.5) {
                double c1 = Math.log((params[0] - 0.5) / params[0]);
                double c2 = c1 / params[1];
                double c3 = Math.sqrt(c2);
                fwhm = (int) ((2.0 * c3) + 0.5);

                /*System.out.println("c3 = "+ c3);
                 *System.out.println("fwhm = " + fwhm);*/
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width green at half maximum = " + fwhm);
            } else {
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width green at half maximum too large to calculate");
                fwhm = Integer.MAX_VALUE;
            }
        } // if (destImageG != null)

        if (destImageB != null) {

            try {
                srcImage.exportRGBData(3, 0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocorrelation: source image locked", true);

                return;
            }


            for (i = 0; i < resultBuffer.length; i++) {
                resultBuffer[i] = 0.0f;
            }

            for (deltaZ = 0; deltaZ < zDim; deltaZ++) {
                newValue = lastValue + (deltaZ * 100 / (zDim * colorsPresent));

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;

                for (deltaY = 0; deltaY < yDim; deltaY++) {

                    for (deltaX = 0; deltaX < xDim; deltaX++) {

                        for (z = 0; z < (zDim - deltaZ); z++) {

                            for (y = 0; y < (yDim - deltaY); y++) {

                                for (x = 0; x < (xDim - deltaX); x++) {
                                    resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize)] += buffer[x +
                                                                                                            (y * xDim) +
                                                                                                            (z *
                                                                                                                 sliceSize)] *
                                                                                                         buffer[x +
                                                                                                                    deltaX +
                                                                                                                    ((y +
                                                                                                                          deltaY) *
                                                                                                                         xDim) +
                                                                                                                    ((z +
                                                                                                                          deltaZ) *
                                                                                                                         sliceSize)];
                                }
                            }
                        }

                        // Normalize for the varying number of terms
                        resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize)] /= (xDim - deltaX) *
                                                                                             (yDim - deltaY) *
                                                                                             (zDim - deltaZ);
                    }
                }
            }

            fireProgressStateChanged(100);
            zeroCoefficient = resultBuffer[0];

            for (z = 0; z < zDim; z++) {

                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        buffer[x + ((yDim - 1 - y) * xDim) + (z * sliceSize)] = resultBuffer[x + (y * xDim) +
                                                                                             (z * sliceSize)] /
                                                                                    zeroCoefficient;
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
                errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

                return;
            }
            
            // The autocorrelation fits to a function of the form
            // (1 - a0) + a0*exp(-(x**2 + y**2 + z**2)/w**2) =
            // (1 - a0) + a0*exp(a1*distSqr),
            // where 1 >= a0 > 0, a1 < 0.
            // The autocorrelation coefficients are normalized by dividing by the
            // value at zero displacement, so the autocorrelation for zero displacement
            // is always 1 by definition.

            initial = new double[2];
            initial[0] = 1.0 - destImageB.getMin();
            initial[1] = -1.0;

            found = false;

            for (x = 1; (x < xDim) && !found; x++) {

                if (buffer[x + ((yDim - 1) * xDim)] >= buffer[(x - 1) + ((yDim - 1) * xDim)]) {
                    xLast = x - 1;
                    found = true;
                } else if (buffer[x + ((yDim - 1) * xDim)] < 0.1f) {
                    xLast = x - 1;
                    found = true;
                }
            }

            if (!found) {
                xLast = xDim - 1;
            }

            found = false;

            for (y = yDim - 2; (y >= 0) && !found; y--) {

                if (buffer[y * xDim] > buffer[(y + 1) * xDim]) {
                    yLast = y + 1;
                    found = true;
                } else if (buffer[y * xDim] < 0.1f) {
                    yLast = y + 1;
                    found = true;
                }
            }

            if (!found) {
                yLast = 0;
            }

            found = false;

            for (z = 1; (z < zDim) && !found; z++) {

                if (buffer[((yDim - 1) * xDim) + (z * sliceSize)] >= buffer[((yDim - 1) * xDim) + ((z - 1) * sliceSize)]) {
                    zLast = z - 1;
                    found = true;
                } else if (buffer[((yDim - 1) * xDim) + (z * sliceSize)] < 0.1f) {
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

                for (y = yDim - 1; y >= yLast; y--) {

                    for (x = 0; x <= xLast; x++) {
                        xValues[i] = (x * x) + ((yDim - 1 - y) * (yDim - 1 - y)) + (z * z);
                        yValues[i++] = buffer[x + (y * xDim) + (z * sliceSize)];
                    }
                }
            }

            fcm = new FitCorrelationModel(nPoints, xValues, yValues, initial);
            fcm.driver();
            fcm.dumpResults();
            params = fcm.getParameters();

            if (params[0] > 0.5) {
                double c1 = Math.log((params[0] - 0.5) / params[0]);
                double c2 = c1 / params[1];
                double c3 = Math.sqrt(c2);
                fwhm = (int) ((2.0 * c3) + 0.5);

                /*System.out.println("c3 = "+ c3);
                 *System.out.println("fwhm = " + fwhm);*/
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width blue at half maximum = " + fwhm);
            } else {
                ViewUserInterface.getReference().setDataText("\n -----------------------------------------------------------------------------\n");
                ViewUserInterface.getReference().setDataText("Auto correlation full width blue at half maximum too large to calculate");
                fwhm = Integer.MAX_VALUE;
            }
        } // if (destImageB != null)

        setCompleted(true);
    }


    /**
     * This function calculates the autocorrelation coefficients and places them in the destination image for black and
     * white images.
     */
    private void calcStoreInDest4D() {

        int length;
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;
        int deltaX, deltaY, deltaZ, deltaT;
        int x, y, z, t;
        float zeroCoefficient;
        int oldValue = 0;
        int newValue = 0;

        try {
            destImage.setLock(ModelStorageBase.RW_LOCKED);
        } catch (IOException error) {
            errorCleanUp("Algorithm Autocorrelation reports: destination image locked", false);

            return;
        }

        try {

            // image length is length in 4 dims
            length = xDim * yDim * zDim * tDim;
            resultBuffer = new float[length];
            buffer = new float[length];
            srcImage.exportData(0, length, buffer); // locks and releases lock
            fireProgressStateChanged(srcImage.getImageName(), "Calculating autocorrelation ..");

        } catch (IOException error) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocorrelation: source image locked", true);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocorrelation: Out of memory creating process buffer", true);

            return;
        }


        for (deltaT = 0; deltaT < tDim; deltaT++) {
            newValue = deltaT * 100 / tDim;

            if ((newValue > oldValue)) {
                fireProgressStateChanged(newValue);
            }

            oldValue = newValue;

            for (deltaZ = 0; deltaZ < zDim; deltaZ++) {

                for (deltaY = 0; deltaY < yDim; deltaY++) {

                    for (deltaX = 0; deltaX < xDim; deltaX++) {

                        for (t = 0; t < (tDim - deltaT); t++) {

                            for (z = 0; z < (zDim - deltaZ); z++) {

                                for (y = 0; y < (yDim - deltaY); y++) {

                                    for (x = 0; x < (xDim - deltaX); x++) {
                                        resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize) +
                                                     (deltaT * volSize)] += buffer[x + (y * xDim) + (z * sliceSize) +
                                                                                   (t * volSize)] *
                                                                                buffer[x + deltaX +
                                                                                           ((y + deltaY) * xDim) +
                                                                                           ((z + deltaZ) * sliceSize) +
                                                                                           ((t + deltaT) * volSize)];
                                    }
                                }
                            }
                        }

                        // Normalize for the varying number of terms
                        resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize) + (deltaT * volSize)] /= (xDim -
                                                                                                               deltaX) *
                                                                                                                  (yDim -
                                                                                                                       deltaY) *
                                                                                                                  (zDim -
                                                                                                                       deltaZ) *
                                                                                                                  (tDim -
                                                                                                                       deltaT);
                    }
                }
            }
        }

        fireProgressStateChanged(100);
        zeroCoefficient = resultBuffer[0];

        for (t = 0; t < tDim; t++) {

            for (z = 0; z < zDim; z++) {

                for (y = 0; y < yDim; y++) {

                    for (x = 0; x < xDim; x++) {
                        buffer[x + ((yDim - 1 - y) * xDim) + (z * sliceSize) + (t * volSize)] = resultBuffer[x +
                                                                                                             (y *
                                                                                                                  xDim) +
                                                                                                             (z *
                                                                                                                  sliceSize) +
                                                                                                             (t *
                                                                                                                  volSize)] /
                                                                                                    zeroCoefficient;
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
            errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

            return;
        }

        setCompleted(true);
    }


    /**
     * This function calculates the autocorrelation coefficients and places them in the destination image for color
     * images.
     */
    private void calcStoreInDest4DC() {

        int length;
        float[] buffer;
        float[] resultBuffer;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int tDim = srcImage.getExtents()[3];
        int sliceSize = xDim * yDim;
        int volSize = sliceSize * zDim;
        int deltaX, deltaY, deltaZ, deltaT;
        int i;
        int x, y, z, t;
        float zeroCoefficient;
        int oldValue = 0;
        int newValue = 0;
        int lastValue = 0;
        int colorsPresent = 0;

        if (destImageR != null) {
            colorsPresent++;
        }

        if (destImageG != null) {
            colorsPresent++;
        }

        if (destImageB != null) {
            colorsPresent++;
        }

        try {

            // image length is length in 4 dims
            length = xDim * yDim * zDim * tDim;
            buffer = new float[length];
            resultBuffer = new float[length];
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm Autocorrelation: Out of memory creating process buffer", true);

            return;
        }

        if (destImageR != null) {

            try {
                srcImage.exportRGBData(1, 0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocorrelation: source image locked", true);

                return;
            }

            fireProgressStateChanged(srcImage.getImageName(), "Calculating autocorrelation ..");


            for (deltaT = 0; deltaT < tDim; deltaT++) {
                newValue = deltaT * 100 / (tDim * colorsPresent);

                if ((newValue > oldValue)) {
                    fireProgressStateChanged(newValue);
                }

                oldValue = newValue;

                for (deltaZ = 0; deltaZ < zDim; deltaZ++) {

                    for (deltaY = 0; deltaY < yDim; deltaY++) {

                        for (deltaX = 0; deltaX < xDim; deltaX++) {

                            for (t = 0; t < (tDim - deltaT); t++) {

                                for (z = 0; z < (zDim - deltaZ); z++) {

                                    for (y = 0; y < (yDim - deltaY); y++) {

                                        for (x = 0; x < (xDim - deltaX); x++) {
                                            resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize) +
                                                         (deltaT * volSize)] += buffer[x + (y * xDim) +
                                                                                       (z * sliceSize) + (t * volSize)] *
                                                                                    buffer[x + deltaX +
                                                                                               ((y + deltaY) * xDim) +
                                                                                               ((z + deltaZ) *
                                                                                                    sliceSize) +
                                                                                               ((t + deltaT) * volSize)];
                                        }
                                    }
                                }
                            }

                            // Normalize for the varying number of terms
                            resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize) + (deltaT * volSize)] /= (xDim -
                                                                                                                   deltaX) *
                                                                                                                      (yDim -
                                                                                                                           deltaY) *
                                                                                                                      (zDim -
                                                                                                                           deltaZ) *
                                                                                                                      (tDim -
                                                                                                                           deltaT);
                        }
                    }
                }
            }

            fireProgressStateChanged(100 / colorsPresent);
            zeroCoefficient = resultBuffer[0];

            for (t = 0; t < tDim; t++) {

                for (z = 0; z < zDim; z++) {

                    for (y = 0; y < yDim; y++) {

                        for (x = 0; x < xDim; x++) {
                            buffer[x + ((yDim - 1 - y) * xDim) + (z * sliceSize) + (t * volSize)] = resultBuffer[x +
                                                                                                                 (y *
                                                                                                                      xDim) +
                                                                                                                 (z *
                                                                                                                      sliceSize) +
                                                                                                                 (t *
                                                                                                                      volSize)] /
                                                                                                        zeroCoefficient;
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
                errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

                return;
            }
        } // if (destImageR != null)

        if (destImageG != null) {

            try {
                srcImage.exportRGBData(2, 0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocorrelation: source image locked", true);

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

                for (deltaZ = 0; deltaZ < zDim; deltaZ++) {

                    for (deltaY = 0; deltaY < yDim; deltaY++) {

                        for (deltaX = 0; deltaX < xDim; deltaX++) {

                            for (t = 0; t < (tDim - deltaT); t++) {

                                for (z = 0; z < (zDim - deltaZ); z++) {

                                    for (y = 0; y < (yDim - deltaY); y++) {

                                        for (x = 0; x < (xDim - deltaX); x++) {
                                            resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize) +
                                                         (deltaT * volSize)] += buffer[x + (y * xDim) +
                                                                                       (z * sliceSize) + (t * volSize)] *
                                                                                    buffer[x + deltaX +
                                                                                               ((y + deltaY) * xDim) +
                                                                                               ((z + deltaZ) *
                                                                                                    sliceSize) +
                                                                                               ((t + deltaT) * volSize)];
                                        }
                                    }
                                }
                            }

                            // Normalize for the varying number of terms
                            resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize) + (deltaT * volSize)] /= (xDim -
                                                                                                                   deltaX) *
                                                                                                                      (yDim -
                                                                                                                           deltaY) *
                                                                                                                      (zDim -
                                                                                                                           deltaZ) *
                                                                                                                      (tDim -
                                                                                                                           deltaT);
                        }
                    }
                }
            }

            fireProgressStateChanged(lastValue + (100 / colorsPresent));
            zeroCoefficient = resultBuffer[0];

            for (t = 0; t < tDim; t++) {

                for (z = 0; z < zDim; z++) {

                    for (y = 0; y < yDim; y++) {

                        for (x = 0; x < xDim; x++) {
                            buffer[x + ((yDim - 1 - y) * xDim) + (z * sliceSize) + (t * volSize)] = resultBuffer[x +
                                                                                                                 (y *
                                                                                                                      xDim) +
                                                                                                                 (z *
                                                                                                                      sliceSize) +
                                                                                                                 (t *
                                                                                                                      volSize)] /
                                                                                                        zeroCoefficient;
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
                errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

                return;
            }
        } // if (destImageG != null)

        if (destImageB != null) {

            try {
                srcImage.exportRGBData(3, 0, length, buffer); // locks and releases lock
            } catch (IOException error) {
                buffer = null;
                resultBuffer = null;
                errorCleanUp("Algorithm Autocorrelation: source image locked", true);

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

                for (deltaZ = 0; deltaZ < zDim; deltaZ++) {

                    for (deltaY = 0; deltaY < yDim; deltaY++) {

                        for (deltaX = 0; deltaX < xDim; deltaX++) {

                            for (t = 0; t < (tDim - deltaT); t++) {

                                for (z = 0; z < (zDim - deltaZ); z++) {

                                    for (y = 0; y < (yDim - deltaY); y++) {

                                        for (x = 0; x < (xDim - deltaX); x++) {
                                            resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize) +
                                                         (deltaT * volSize)] += buffer[x + (y * xDim) +
                                                                                       (z * sliceSize) + (t * volSize)] *
                                                                                    buffer[x + deltaX +
                                                                                               ((y + deltaY) * xDim) +
                                                                                               ((z + deltaZ) *
                                                                                                    sliceSize) +
                                                                                               ((t + deltaT) * volSize)];
                                        }
                                    }
                                }
                            }

                            // Normalize for the varying number of terms
                            resultBuffer[deltaX + (deltaY * xDim) + (deltaZ * sliceSize) + (deltaT * volSize)] /= (xDim -
                                                                                                                   deltaX) *
                                                                                                                      (yDim -
                                                                                                                           deltaY) *
                                                                                                                      (zDim -
                                                                                                                           deltaZ) *
                                                                                                                      (tDim -
                                                                                                                           deltaT);
                        }
                    }
                }
            }

            fireProgressStateChanged(100);
            zeroCoefficient = resultBuffer[0];

            for (t = 0; t < tDim; t++) {

                for (z = 0; z < zDim; z++) {

                    for (y = 0; y < yDim; y++) {

                        for (x = 0; x < xDim; x++) {
                            buffer[x + ((yDim - 1 - y) * xDim) + (z * sliceSize) + (t * volSize)] = resultBuffer[x +
                                                                                                                 (y *
                                                                                                                      xDim) +
                                                                                                                 (z *
                                                                                                                      sliceSize) +
                                                                                                                 (t *
                                                                                                                      volSize)] /
                                                                                                        zeroCoefficient;
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
                errorCleanUp("Algorithm Autocorrelation reports: destination image still locked", true);

                return;
            }
        } // if (destImageB != null)

        setCompleted(true);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    class FitCorrelationModel extends NLConstrainedEngine {
        private float[] xData;
        private float[] yData;
        /**
         * Creates a new FitCorrelationModel object.
         *
         * @param  nPoints  DOCUMENT ME!
         * @param  xData    DOCUMENT ME!
         * @param  yData    DOCUMENT ME!
         * @param  initial  DOCUMENT ME!
         */
        public FitCorrelationModel(int nPoints, float[] xData, float[] yData, double[] initial) {

            // nPoints data points, 2 coefficients, and exponential fitting
            super(nPoints, 2);
            this.xData = xData;
            this.yData = yData;
            
            bounds = 2; // bounds = 0 means unconstrained

            // bounds = 1 means same lower and upper bounds for
            // all parameters
            // bounds = 2 means different lower and upper bounds
            // for all parameters
            
            // Constrain parameter 0
            bl[0] = Double.MIN_VALUE;
            bu[0] = 1.0;

            // Constrain parameter 1
            bl[1] = -Double.MAX_VALUE;
            bu[1] = -Double.MIN_VALUE;
            
            // The default is internalScaling = false
            // To make internalScaling = true and have the columns of the
            // Jacobian scaled to have unit length include the following line.
            // internalScaling = true;
            // Suppress diagnostic messages
            outputMes = false;

            gues[0] = initial[0];
            gues[1] = initial[1];
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
            Preferences.debug(" ******* FitCorrelationModel ********* \n\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Number of iterations: " + String.valueOf(iters) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("Chi-squared: " + String.valueOf(getChiSquared()) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a0 " + String.valueOf(a[0]) + "\n", Preferences.DEBUG_ALGORITHM);
            Preferences.debug("a1 " + String.valueOf(a[1]) + "\n", Preferences.DEBUG_ALGORITHM);
        }
        
        /**
         * 
         * 
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
                // Preferences.debug("ctrl = " + ctrl + " a[0] = " + a[0] + " a[1] = " + a[1] + "\n", 
                // Preferences.DEBUG_ALGORITHM);
                if ( (ctrl == -1) || (ctrl == 1)) {
                    
                    // evaluate the residuals[j] = ymod - yData[j]
                    for (j = 0; j < nPts; j++) {
                    	ymod = (1 - a[0]) + (a[0] * Math.exp(a[1] * xData[j]));
                        residuals[j] = ymod - yData[j];
                        // Preferences.debug("residuals["+ j + "] = " + residuals[j] + "\n", Preferences.DEBUG_ALGORITHM);
                    }
                } // if ((ctrl == -1) || (ctrl == 1))
                else if (ctrl == 2) {
                    // Calculate the Jacobian analytically
                    for (j = 0; j < nPts; j++) {
                    	covarMat[j][0] = -1 + Math.exp(a[1] * xData[j]); // a0 partial derivative
                    	covarMat[j][1] = a[0] * xData[j] * Math.exp(a[1] * xData[j]); // a1 partial derivative
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
