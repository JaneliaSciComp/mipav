package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.util.*;


/**
 * This program applies the watershed algorithm to the image. It assumes that the user has identified the starting
 * watershed regions via VOIs. Future work might be to develop a algorithm that automatically identifies initial seed
 * points and regions or possibly an automatic method that forms watersheds - see method at end of file (does not work
 * yet);
 *
 * @version  1.1 March 11, 1998
 * @author   Matthew J. McAuliffe, Ph.D. and Ray Lert, MD
 */

public class AlgorithmWatershed extends AlgorithmBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final short BOUNDARY = -4;

    /** DOCUMENT ME! */
    private static final short MASK = -3; // QUEUED

    /** DOCUMENT ME! */
    private static final short INITIAL = -2;

    /** DOCUMENT ME! */
    private static final short WSHED = -1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage energyImage = null;

    /** DOCUMENT ME! */
    private Vector seedVector = null;

    /** DOCUMENT ME! */
    private float[] sigmas;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs new watershed algorithm.
     *
     * @param  destImg  Image model where result image is to stored
     * @param  srcImg   Source image model
     * @param  gmImg    Gradient magnitude image (can be null)
     * @param  sigmas   Gaussian's standard deviations in the each dimension
     * @param  seeds    Seed points for starting watershed
     */
    public AlgorithmWatershed(ModelImage destImg, ModelImage srcImg, ModelImage gmImg, float[] sigmas, Vector seeds) {

        super(destImg, srcImg);
        energyImage = gmImg;
        this.sigmas = sigmas;
        seedVector = seeds;
        mask = srcImage.generateVOIMask();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;

        if (energyImage != null) {
            energyImage.disposeLocal();
        }

        energyImage = null;
        super.finalize();
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        if (destImage == null) {
            displayError("Destination Image is null");

            return;
        }

        constructLog();

        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() > 2) {
            calc3D();
        }
    }

    /**
     * This image is typically the gradient magnitude. Peaks in this image (function ) are the watersheds boundaries
     *
     * @param  _energyImage  the energy image (function)
     */
    public void setEnergyImage(ModelImage _energyImage) {
        energyImage = _energyImage;
    }

    /**
     * Sets the seed vector.
     *
     * @param  seedPoints  The seed points.
     */
    public void setSeedVector(Point3Df[] seedPoints) {
        int i;

        try {

            if (seedVector == null) {
                seedVector = new Vector();
            }

            seedVector.removeAllElements();

            for (i = 0; i < seedPoints.length; i++) {
                seedVector.addElement(new Seed(seedPoints[i], (short) i));
            }
        } catch (OutOfMemoryError error) {
            System.gc();
            displayError("Watershed: unable to allocate enough memory");
        }
    }

    /**
     * Calculates the watershed intialized by the VOI regions. The resultant watershed regions(basin) are labelled with
     * an integer value. In addition, each region is separated by connected (8-way) watershed identifier of 1.
     */
    private void calc2D() {
        int nVOI;
        int i, c;
        int xDim, yDim;
        int length;
        ViewVOIVector VOIs;
        Vector[] contours;
        AlgorithmGradientMagnitudeSep gradMagAlgo;
        int x, y;
        float[] xB = null, yB = null, zB = null;

        // Calculate the gradient magnitude if energyImage is null.
        if (energyImage == null) {

            try {
                energyImage = new ModelImage(ModelImage.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_gm",
                                             srcImage.getUserInterface());
                gradMagAlgo = new AlgorithmGradientMagnitudeSep(energyImage, srcImage, sigmas, true, false);
                gradMagAlgo.setRunningInSeparateThread(runningInSeparateThread);
                gradMagAlgo.run();

                if (gradMagAlgo.isCompleted() == false) {
                    setCompleted(false);

                    if (energyImage != null) {
                        energyImage.disposeLocal();
                    }

                    energyImage = null;
                    gradMagAlgo = null;
                    System.gc();

                    return;
                }

                length = energyImage.getSize();

                float max = (float) energyImage.getMax();

                // All points that are set to -1 are positions on the edge of the
                // image where the gradient magnitude could not be calculated and
                // should be set to the highest value for the watershed to work best
                for (i = 0; i < length; i++) {

                    if (energyImage.getFloat(i) == -1) {
                        energyImage.set(i, max);
                    }
                }

                energyImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), srcImage.getImageName() + "_gm",
                                      FileBase.XML, true);
            } catch (OutOfMemoryError error) {

                if (energyImage != null) {
                    energyImage.disposeLocal();
                }

                energyImage = null;
                System.gc();
                displayError("Watershed: unable to allocate enough memory");
                setCompleted(false);

                return;
            }

            gradMagAlgo = null;
        }

        System.gc(); // Free memory resources

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        length = xDim * yDim;

        float gradMagMin = (float) energyImage.getMin();

        try {
            xB = new float[3];
            yB = new float[3];
            zB = new float[3];
        } catch (OutOfMemoryError error) {
            xB = null;
            yB = null;
            zB = null;

            if (energyImage != null) {
                energyImage.disposeLocal();
            }

            energyImage = null;
            contours = null;
            displayError("Watershed: unable to allocate enough memory");

            return;
        }

        if (seedVector == null) {
            seedVector = new Vector();

            // Get VOI mask
            // Set grad. mag. to the gradient magnitude minimum where there is a VOI
            for (i = 0; i < length; i++) {

                if (mask.get(i)) {
                    energyImage.set(i, gradMagMin);
                }
            }

            x = 0;
            y = 0;

            // Set up seed vector - every VOI region should have one seed
            VOIs = (ViewVOIVector) srcImage.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    contours = VOIs.VOIAt(i).getCurves();

                    for (c = 0; c < contours[0].size(); c++) {

                        ((VOIContour) (contours[0].elementAt(c))).getBounds(xB, yB, zB);
                        ((VOIContour) (contours[0].elementAt(c))).contains(0, 0, true);

// find seed inside VOI
Found:
                        for (y = (int) yB[0]; y < yB[1]; y++) {

                            for (x = (int) xB[0]; x < xB[1]; x++) {

                                if (((VOIContour) (contours[0].elementAt(c))).contains(x, y, false)) {
                                    break Found;
                                }
                            }
                        }

                        // Add point that has been found inside the VOI to the seed vector
                        try {
                            seedVector.addElement(new Seed(new Point3Df(x, y, 0), VOIs.VOIAt(i).getWatershedID()));
                        } catch (OutOfMemoryError error) {

                            if (energyImage != null) {
                                energyImage.disposeLocal();
                            }

                            energyImage = null;
                            contours = null;
                            System.gc();
                            displayError("Watershed: unable to allocate enough memory");
                            setCompleted(false);

                            return;
                        }
                    }
                }
            }
        }

        // Prepare destination image
        int mod = length / 100; // mod is 1 percent of length

        try {
            buildProgressBar(srcImage.getImageName(), "Watershed: Filling regions ...", 0, 100);
            initProgressBar();
        } catch (OutOfMemoryError error) {

            if (energyImage != null) {
                energyImage.disposeLocal();
            }

            energyImage = null;
            contours = null;
            System.gc();
            displayError("Watershed: unable to allocate enough memory");
            setCompleted(false);

            return;
        }

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 100), runningInSeparateThread);
            }

            if (((i % xDim) == 0) || ((i % xDim) == (xDim - 1))) {
                destImage.setShort(i, BOUNDARY);
            } else if (((i / xDim) == 0) || ((i / xDim) == (yDim - 1))) {
                destImage.setShort(i, BOUNDARY);
            } else {
                destImage.setShort(i, INITIAL);
            }
        }

        if (threadStopped) {
            contours = null;
            finalize();

            return;
        }

        for (i = 0; i < seedVector.size(); i++) {
            destImage.set((int) ((Seed) seedVector.elementAt(i)).point.x,
                          (int) ((Seed) seedVector.elementAt(i)).point.y, ((Seed) seedVector.elementAt(i)).seedValue);
            energyImage.set((int) ((Seed) seedVector.elementAt(i)).point.x,
                            (int) ((Seed) seedVector.elementAt(i)).point.y, energyImage.getMin());
        }

        // Set all initial regions to watershed value defined in VOI object
        if (srcImage.getVOIs().size() > 0) {
            VOIs = (ViewVOIVector) srcImage.getVOIs();
            nVOI = VOIs.size();

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    contours = VOIs.VOIAt(i).getCurves();

                    for (int cons = 0; cons < contours[0].size(); cons++) {
                        ((VOIContour) (contours[0].elementAt(cons))).getBounds(xB, yB, zB);
                        ((VOIContour) (contours[0].elementAt(cons))).contains(0, 0, true);

                        for (y = (int) yB[0]; y < yB[1]; y++) {

                            for (x = (int) xB[0]; x < xB[1]; x++) {

                                if (((VOIContour) (contours[0].elementAt(cons))).contains(x, y, false)) {
                                    destImage.set(x, y, (short) VOIs.VOIAt(i).getWatershedID());
                                }
                            }
                        }
                    }
                }
            }
        }

        /* Find minima near user's seeds ****************************************************
         *
         *    Seed_vector contains the seeds provided by user      current_basin labels a basin corresponding to a seed
         *
         **************************************************************************************/
        Point3Df P;
        int tempP = 0;
        int pixC, pixN, pixS, pixE, pixW;
        float gmMin, gmMax;

        gmMin = (float) energyImage.getMin();
        gmMax = (float) energyImage.getMax();

        int queueSize = 4000;
        float eRange = gmMax - gmMin;

        if (eRange < 4000) {
            queueSize = 4000;
        } else if (eRange > 4000) {
            queueSize = (int) (2 * eRange);

            if (queueSize > 6000) {
                queueSize = 6000;
            }
        }

        int[] energyHisto;

        try {
            energyHisto = new int[queueSize];
        } catch (OutOfMemoryError error) {
            energyImage = null;
            System.gc();
            displayError("Watershed: unable to allocate enough memory");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        // calc histogram used to initialize the hier. queue
        float slope = (queueSize - 1) / (gmMax - gmMin);
        int index;
        int lengthGM = energyImage.getSize();

        for (i = 0; i < lengthGM; i++) {
            index = (int) (((energyImage.getFloat(i) - gmMin) * slope) + 0.5);
            energyHisto[index] = energyHisto[index] + 1;
        }

        HQueue hQueue = new HQueue(gmMin, gmMax, queueSize, energyHisto);

        for (i = 0; i < seedVector.size(); i++) {
            P = ((Seed) seedVector.elementAt(i)).point;
            pixC = (int) ((P.y * xDim) + P.x);
            hQueue.add(pixC, (float) energyImage.getMin());
        }

        // Start flooding local mins
        while (hQueue.isEmpty() == false) {
            tempP = hQueue.first();

            pixC = tempP;
            pixN = pixC - xDim;
            pixS = pixC + xDim;
            pixE = pixC + 1;
            pixW = pixC - 1;

            if (destImage.getShort(pixW) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixW)) {
                        destImage.setShort(pixC, WSHED);
                    }
                } else if (destImage.getShort(pixC) == MASK) { // West neighbor has to be basin
                    destImage.setShort(pixC, destImage.getShort(pixW));
                }
            } else {

                if (destImage.getShort(pixW) == INITIAL) {
                    destImage.setShort(pixW, MASK);
                    hQueue.add(pixW, energyImage.getFloat(pixW));
                }
            }

            if (destImage.getShort(pixE) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixE)) {
                        destImage.setShort(pixC, WSHED);
                    }
                } else if (destImage.getShort(pixC) == MASK) {
                    destImage.setShort(pixC, destImage.getShort(pixE));
                }
            } else {

                if (destImage.getShort(pixE) == INITIAL) {
                    destImage.setShort(pixE, MASK);
                    hQueue.add(pixE, energyImage.getFloat(pixE));
                }
            }


            if (destImage.getShort(pixN) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixN)) {
                        destImage.setShort(pixC, WSHED);
                    }
                } else if (destImage.getShort(pixC) == MASK) {
                    destImage.setShort(pixC, destImage.getShort(pixN));
                }
            } else {

                if (destImage.getShort(pixN) == INITIAL) {
                    destImage.setShort(pixN, MASK);
                    hQueue.add(pixN, energyImage.getFloat(pixN));
                }
            }


            if (destImage.getShort(pixS) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixS)) {
                        destImage.setShort(pixC, WSHED);
                    }

                } else if (destImage.getShort(pixC) == MASK) {
                    destImage.setShort(pixC, destImage.getShort(pixS));
                }
            } else {

                if (destImage.getShort(pixS) == INITIAL) {
                    destImage.setShort(pixS, MASK);
                    hQueue.add(pixS, energyImage.getFloat(pixS));
                }
            }

            if (destImage.getShort(pixC) == MASK) {

                /*if(currentBasin != destImage.getShort(pixN) &&
                 * currentBasin != destImage.getShort(pixE) && currentBasin != destImage.getShort(pixS) && currentBasin
                 * != destImage.getShort(pixW) ) {
                 *
                 * if (destImage.getShort(pixN) >= 0)        destImage.setShort(pixC, destImage.getShort(pixN)); else if
                 * (destImage.getShort(pixE) >= 0)   destImage.setShort(pixC, destImage.getShort(pixE)); else if
                 * (destImage.getShort(pixS) >= 0)   destImage.setShort(pixC, destImage.getShort(pixS)); else if
                 * (destImage.getShort(pixW) >= 0)   destImage.setShort(pixC, destImage.getShort(pixW)); else {     //
                 * maybe add back to queue     //destImage.setShort(pixC, (short)50);     //hQueue.add(pixC,
                 * energyImage.getFloat(pixC));     destImage.setShort(pixC, WSHED); } } else { destImage.setShort(pixC,
                 * currentBasin);} */
                destImage.setShort(pixC, WSHED);
            }
        }

        float[] imgBuffer;

        try {
            imgBuffer = new float[length];
            destImage.exportData(0, length, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            imgBuffer = null;
            contours = null;

            if (energyImage != null) {
                energyImage.disposeLocal();
            }

            energyImage = null;
            displayError("Algorithm Watershed2D: Image(s) locked");
            setCompleted(false);
            disposeProgressBar();

            return;
        } catch (OutOfMemoryError e) {
            imgBuffer = null;
            contours = null;

            if (energyImage != null) {
                energyImage.disposeLocal();
            }

            energyImage = null;
            System.gc();
            displayError("Algorithm Watershed2D: Out of memory");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        int dir;
        float max, gmC;

        for (i = 0; i < length; i++) {

            if (destImage.getShort(i) == WSHED) {
                pixC = i;
                gmC = energyImage.getFloat(pixC);
                pixN = pixC - xDim;
                pixS = pixC + xDim;
                pixE = pixC + 1;
                pixW = pixC - 1;
                dir = -1;
                max = 0;

                if (destImage.getShort(pixN) > 0) {
                    dir = 0;
                    max = energyImage.getFloat(pixN);
                }

                if ((destImage.getShort(pixE) > 0) && (energyImage.getFloat(pixE) > max)) {
                    dir = 1;
                    max = energyImage.getFloat(pixE);
                }

                if ((destImage.getShort(pixS) > 0) && (energyImage.getFloat(pixS) > max)) {
                    dir = 2;
                    max = energyImage.getFloat(pixS);
                }

                if ((destImage.getShort(pixW) > 0) && (energyImage.getFloat(pixW) > max)) {
                    dir = 3;
                    max = energyImage.getFloat(pixW);
                }

                if (dir == 0) {
                    imgBuffer[i] = destImage.getShort(pixN);
                } else if (dir == 1) {
                    imgBuffer[i] = destImage.getShort(pixE);
                } else if (dir == 2) {
                    imgBuffer[i] = destImage.getShort(pixS);
                } else if (dir == 3) {
                    imgBuffer[i] = destImage.getShort(pixW);
                } else {
                    imgBuffer[i] = 0;
                }
            }
        }

        try {
            destImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Morphology2D: Image(s) locked");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        for (i = 0; i < length; i++) {

            if (destImage.getShort(i) == BOUNDARY) {
                destImage.setShort(i, (short) 0);
            }
        }

        VOIs = (ViewVOIVector) srcImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                contours = VOIs.VOIAt(i).getCurves();

                for (int cons = 0; cons < contours[0].size(); cons++) {
                    ((VOIContour) (contours[0].elementAt(cons))).getBounds(xB, yB, zB);
                    ((VOIContour) (contours[0].elementAt(cons))).contains(0, 0, true);

                    for (y = (int) yB[0]; y < yB[1]; y++) {

                        for (x = (int) xB[0]; x < xB[1]; x++) {

                            if (((VOIContour) (contours[0].elementAt(cons))).contains(x, y, false)) {
                                destImage.set(x, y, (short) VOIs.VOIAt(i).getWatershedID());
                            }
                        }
                    }
                }
            }
        }

        destImage.calcMinMax();

        length = mask.size();

        for (i = 0; i < length; i++) {
            mask.clear(i);
        }

        energyImage.disposeLocal();
        hQueue.dispose();
        disposeProgressBar();
        setCompleted(true);
    }


    /**
     * Calculates the watershed intialized by the VOI regions. The resultant watershed regions(basin) are labelled with
     * an integer value. In addition, each region is separated by connected (8-way) watershed identifier of 1.
     */
    private void calc3D() {
        int nVOI;
        int i, slice;
        int length;
        AlgorithmGradientMagnitudeSep gradMagAlgo;
        ViewVOIVector VOIs;
        int xDim, yDim, zDim;
        Vector[] contours;
        int x, y;
        short currentBasin = 0;

        float[] xB = null, yB = null, zB = null;

        // Calculate the gradient magnitude
        if (energyImage == null) {

            try {
                energyImage = new ModelImage(ModelImage.FLOAT, srcImage.getExtents(), srcImage.getImageName() + "_gm",
                                             srcImage.getUserInterface());

                gradMagAlgo = new AlgorithmGradientMagnitudeSep(energyImage, srcImage, sigmas, true, false);
                gradMagAlgo.setRunningInSeparateThread(runningInSeparateThread);
                gradMagAlgo.run();

                if (gradMagAlgo.isCompleted() == false) {
                    setCompleted(false);
                    energyImage.disposeLocal();
                    gradMagAlgo = null;
                    System.gc();

                    return;
                }

                length = energyImage.getSize();

                float max = (float) energyImage.getMax();

                // All points that are set to -1 are positions on the edge of the
                // image where the gradient magnitude could not be calculated and
                // should be set to the highest value for the watershed to work best
                for (i = 0; i < length; i++) {

                    if (energyImage.getFloat(i) == -1) {
                        energyImage.set(i, max);
                    }
                }

                energyImage.saveImage(srcImage.getFileInfo(0).getFileDirectory(), srcImage.getImageName() + "_gm",
                                      FileBase.XML, true);
            } catch (OutOfMemoryError error) {

                if (energyImage != null) {
                    energyImage.disposeLocal();
                }

                energyImage = null;
                gradMagAlgo = null;
                System.gc();
                displayError("Watershed: unable to allocate enough memory");
                setCompleted(false);

                return;
            }

            gradMagAlgo = null;
        }

        System.gc();
        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        zDim = srcImage.getExtents()[2];
        length = xDim * yDim * zDim;

        float gmMin = (float) energyImage.getMin();
        float gmMax = (float) energyImage.getMax();

        float zero = (float) energyImage.getMin();
        VOIs = (ViewVOIVector) srcImage.getVOIs();
        nVOI = VOIs.size();
        x = 0;
        y = 0;

        try {
            xB = new float[3];
            yB = new float[3];
            zB = new float[3];
        } catch (OutOfMemoryError error) {

            if (energyImage != null) {
                energyImage.disposeLocal();
            }

            energyImage = null;
            xB = null;
            yB = null;
            zB = null;
            System.gc();
            displayError("Watershed: unable to allocate enough memory");
        }

        if (seedVector == null) {

            try {
                seedVector = new Vector();
            } catch (OutOfMemoryError error) {
                displayError("Watershed: unable to allocate enough memory");
            }

            for (i = 0; i < length; i++) {

                if (mask.get(i) == true) {
                    energyImage.set(i, zero);
                }
            }

            // Find seed points
            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    contours = VOIs.VOIAt(i).getCurves();

                    for (slice = 1; slice < (contours.length - 1); slice++) {

                        for (int cons = 0; cons < contours[slice].size(); cons++) {
                            ((VOIContour) (contours[slice].elementAt(cons))).getBounds(xB, yB, zB);
                            ((VOIContour) (contours[slice].elementAt(cons))).contains(0, 0, true);

Found:
                            for (y = (int) yB[0]; y < yB[1]; y++) {

                                for (x = (int) xB[0]; x < xB[1]; x++) {

                                    if (((VOIContour) (contours[slice].elementAt(cons))).contains(x, y, false)) {
                                        break Found;
                                    }
                                }
                            }

                            try {
                                seedVector.addElement(new Seed(new Point3Df(x, y, slice),
                                                               VOIs.VOIAt(i).getWatershedID()));
                            } catch (OutOfMemoryError error) {
                                energyImage = null;
                                System.gc();
                                displayError("Watershed: unable to allocate enough memory");
                                setCompleted(false);

                                return;
                            }
                        }
                    }
                }
            }
        }

        int imageLength = xDim * yDim;
        int lastImageIndex = (zDim - 1) * imageLength;
        int temp;
        int mod = length / 100; // mod is 1 percent of length

        try {
            buildProgressBar(srcImage.getImageName(), "Watershed: Filling regions ...", 0, 100);
        } catch (OutOfMemoryError error) {
            energyImage = null;
            System.gc();
            displayError("Watershed: unable to allocate enough memory");
            setCompleted(false);

            return;
        }

        initProgressBar();

        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 50), runningInSeparateThread);
            }

            temp = i % imageLength;

            if ((i < imageLength) || (i > lastImageIndex)) {
                destImage.setShort(i, BOUNDARY);
            } else if (((temp % xDim) == 0) || ((temp % xDim) == (xDim - 1))) {
                destImage.setShort(i, BOUNDARY);
            } else if (((temp / xDim) == 0) || ((temp / xDim) == (yDim - 1))) {
                destImage.setShort(i, BOUNDARY);
            } else {
                destImage.setShort(i, INITIAL);
            }
        }

        if (threadStopped) {
            contours = null;
            finalize();

            return;
        }

        for (i = 0; (i < seedVector.size()) && !threadStopped; i++) {
            destImage.set((int) ((Seed) seedVector.elementAt(i)).point.x,
                          (int) ((Seed) seedVector.elementAt(i)).point.y,
                          (int) ((Seed) seedVector.elementAt(i)).point.z, ((Seed) seedVector.elementAt(i)).seedValue);

            energyImage.set((int) ((Seed) seedVector.elementAt(i)).point.x,
                            (int) ((Seed) seedVector.elementAt(i)).point.y,
                            (int) ((Seed) seedVector.elementAt(i)).point.z, energyImage.getMin());
            // ((Seed)seedVector.elementAt(i)).seedValue);
        }

        if (threadStopped) {
            contours = null;
            finalize();

            return;
        }

        // Fill all watershed initial VOIs with watershedID
        if (srcImage.getVOIs().size() > 0) {

            for (i = 0; (i < nVOI) && !threadStopped; i++) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    contours = VOIs.VOIAt(i).getCurves();

                    for (slice = 1; (slice < (contours.length - 1)) && !threadStopped; slice++) {

                        for (int cons = 0; cons < contours[slice].size(); cons++) {
                            ((VOIContour) (contours[slice].elementAt(cons))).getBounds(xB, yB, zB);
                            ((VOIContour) (contours[slice].elementAt(cons))).contains(0, 0, true);

                            for (y = (int) yB[0]; (y < yB[1]) && !threadStopped; y++) {

                                for (x = (int) xB[0]; (x < xB[1]) && !threadStopped; x++) {

                                    if (((VOIContour) (contours[slice].elementAt(cons))).contains(x, y, false)) {
                                        destImage.set(x, y, slice, (short) VOIs.VOIAt(i).getWatershedID());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        if (threadStopped) {
            contours = null;
            finalize();

            return;
        }

        /**Find minima near user's seeds ****************************************************
         *
         *    Seed_vector contains the seeds provided by user      current_basin labels a basin corresponding to a seed
         *
         **************************************************************************************/
        Point3Df P = new Point3Df(0, 0, 0);
        int tempP = 0;
        int pixC, pixN, pixS, pixE, pixW, pixU, pixD;
        int imageSize;

        int queueSize = 4000;
        float eRange = gmMax - gmMin;

        if (eRange < 4000) {
            queueSize = 4000;
        } else if (eRange > 4000) {
            queueSize = (int) (2 * eRange);

            if (queueSize > 6000) {
                queueSize = 6000;
            }
        }

        int[] energyHisto;

        try {
            energyHisto = new int[queueSize];
        } catch (OutOfMemoryError error) {
            energyImage = null;
            System.gc();
            displayError("Watershed: unable to allocate enough memory");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        // calc histogram used to initialize the hier. queue
        float slope = (queueSize - 1) / (gmMax - gmMin);
        int index;
        int lengthGM = energyImage.getSize();

        for (i = 0; (i < lengthGM) && !threadStopped; i++) {
            index = (int) (((energyImage.getFloat(i) - gmMin) * slope) + 0.5);
            energyHisto[index] = energyHisto[index] + 1;
        }

        if (threadStopped) {
            contours = null;
            finalize();

            return;
        }

        HQueue hQueue = new HQueue(gmMin, gmMax, queueSize, energyHisto);

        // Put starting seeds from VOI into the queue.
        imageSize = xDim * yDim;

        for (i = 0; (i < seedVector.size()) && !threadStopped; i++) {
            P = ((Seed) seedVector.elementAt(i)).point;
            pixC = (int) ((P.z * imageSize) + (P.y * xDim) + P.x);
            hQueue.add(pixC, gmMin);
        }

        if (threadStopped) {
            contours = null;
            finalize();

            return;
        }

        // Watershed fill !!!!!
        while ((hQueue.isEmpty() == false) && !threadStopped) {
            tempP = hQueue.first();

            pixC = tempP;
            pixN = pixC - xDim;
            pixS = pixC + xDim;
            pixE = pixC + 1;
            pixW = pixC - 1;
            pixU = pixC - imageSize;
            pixD = pixC + imageSize;

            if (destImage.getShort(pixW) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixW)) {
                        destImage.setShort(pixC, WSHED);
                    }
                } else if (destImage.getShort(pixC) == MASK) { // West neighbor has to be basin
                    destImage.setShort(pixC, destImage.getShort(pixW)); // extend basin to center;
                    currentBasin = destImage.getShort(pixW);
                }
            } else {

                if (destImage.getShort(pixW) == INITIAL) {
                    destImage.setShort(pixW, MASK);
                    hQueue.add(pixW, energyImage.getFloat(pixW));
                }
            }


            if (destImage.getShort(pixE) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixE)) {
                        destImage.setShort(pixC, WSHED);
                    }
                } else if (destImage.getShort(pixC) == MASK) {
                    destImage.setShort(pixC, destImage.getShort(pixE));
                    currentBasin = destImage.getShort(pixE);
                }
            } else {

                if (destImage.getShort(pixE) == INITIAL) {
                    destImage.setShort(pixE, MASK);
                    hQueue.add(pixE, energyImage.getFloat(pixE));
                }
            }


            if (destImage.getShort(pixN) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixN)) {
                        destImage.setShort(pixC, WSHED);
                    }

                } else if (destImage.getShort(pixC) == MASK) {
                    destImage.setShort(pixC, destImage.getShort(pixN));
                    currentBasin = destImage.getShort(pixN);
                }
            } else {

                if (destImage.getShort(pixN) == INITIAL) {
                    destImage.setShort(pixN, MASK);
                    hQueue.add(pixN, energyImage.getFloat(pixN));
                }
            }


            if (destImage.getShort(pixS) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixS)) {
                        destImage.setShort(pixC, WSHED);
                    }

                } else if (destImage.getShort(pixC) == MASK) {
                    destImage.setShort(pixC, destImage.getShort(pixS));
                    currentBasin = destImage.getShort(pixS);
                }
            } else {

                if (destImage.getShort(pixS) == INITIAL) {
                    destImage.setShort(pixS, MASK);
                    hQueue.add(pixS, energyImage.getFloat(pixS));
                }
            }


            if (destImage.getShort(pixU) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixU)) {
                        destImage.setShort(pixC, WSHED);
                    }
                } else if (destImage.getShort(pixC) == MASK) {
                    destImage.setShort(pixC, destImage.getShort(pixU));
                    currentBasin = destImage.getShort(pixU);
                }
            } else {

                if (destImage.getShort(pixU) == INITIAL) {
                    destImage.setShort(pixU, MASK);
                    hQueue.add(pixU, energyImage.getFloat(pixU));
                }
            }

            if (destImage.getShort(pixD) >= 0) {

                if (destImage.getShort(pixC) >= 0) {

                    if (destImage.getShort(pixC) != destImage.getShort(pixD)) {
                        destImage.setShort(pixC, WSHED);
                    }
                } else if (destImage.getShort(pixC) == MASK) {
                    destImage.setShort(pixC, destImage.getShort(pixD));
                    currentBasin = destImage.getShort(pixD);
                }
            } else {

                if (destImage.getShort(pixD) == INITIAL) {
                    destImage.setShort(pixD, MASK);
                    hQueue.add(pixD, energyImage.getFloat(pixD));
                }
            }

            if (destImage.getShort(pixC) == MASK) {

                // destImage.setShort(pixC, currentBasin);
                destImage.setShort(pixC, WSHED);
                // hQueue.add(pixC, energyImage.getFloat(pixC));
            }
        }

        if (threadStopped) {
            contours = null;
            finalize();

            return;
        }

        // Free up some memory
        hQueue.dispose();
        energyImage.disposeLocal();

        float[] imgBuffer;

        try {
            imgBuffer = new float[length];
            destImage.exportData(0, length, imgBuffer); // locks and releases lock
        } catch (IOException error) {
            imgBuffer = null;

            if (energyImage != null) {
                energyImage.disposeLocal();
            }

            energyImage = null;
            displayError("Algorithm Watershed3D: Image(s) locked");
            setCompleted(false);
            disposeProgressBar();

            return;
        } catch (OutOfMemoryError e) {
            imgBuffer = null;

            if (energyImage != null) {
                energyImage.disposeLocal();
            }

            energyImage = null;
            System.gc();
            displayError("Algorithm Watershed3D: Out of memory");
            setCompleted(false);
            disposeProgressBar();

            return;
        }


        for (i = 0; (i < length) && !threadStopped; i++) {

            if (((i % mod) == 0) && isProgressBarVisible()) {
                progressBar.updateValue(Math.round((float) i / (length - 1) * 50) + 50, runningInSeparateThread);
            }

            if (destImage.getShort(i) == WSHED) {
                pixC = i;
                pixN = pixC - xDim;
                pixS = pixC + xDim;
                pixE = pixC + 1;
                pixW = pixC - 1;
                pixU = pixC - imageSize;
                pixD = pixC + imageSize;

                if ((destImage.getShort(pixN) == 0) || (destImage.getShort(pixE) == 0) ||
                        (destImage.getShort(pixS) == 0) || (destImage.getShort(pixW) == 0) ||
                        (destImage.getShort(pixU) == 0) || (destImage.getShort(pixD) == 0)) {
                    imgBuffer[i] = 0;
                } else if (destImage.getShort(pixN) > 0) {
                    imgBuffer[i] = destImage.getShort(pixN);
                } else if (destImage.getShort(pixE) > 0) {
                    imgBuffer[i] = destImage.getShort(pixE);
                } else if (destImage.getShort(pixS) > 0) {
                    imgBuffer[i] = destImage.getShort(pixS);
                } else if (destImage.getShort(pixW) > 0) {
                    imgBuffer[i] = destImage.getShort(pixW);
                } else if (destImage.getShort(pixU) > 0) {
                    imgBuffer[i] = destImage.getShort(pixU);
                } else if (destImage.getShort(pixD) > 0) {
                    imgBuffer[i] = destImage.getShort(pixD);
                } else {
                    imgBuffer[i] = 0;
                }
            }
        }

        if (threadStopped) {
            contours = null;
            finalize();

            return;
        }


        try {
            destImage.importData(0, imgBuffer, true);
        } catch (IOException error) {
            displayError("Algorithm Watershed3D: Image(s) locked");
            setCompleted(false);
            disposeProgressBar();

            return;
        }

        for (i = 0; i < length; i++) {

            if (destImage.getShort(i) == BOUNDARY) {
                destImage.setShort(i, (short) 0);
            }
        }

        // Make sure all watershed VOIs have there watershed ID -- fixes border voxels
        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                contours = VOIs.VOIAt(i).getCurves();

                for (slice = 1; slice < (contours.length - 1); slice++) {

                    for (int cons = 0; cons < contours[slice].size(); cons++) {
                        ((VOIContour) (contours[slice].elementAt(cons))).getBounds(xB, yB, zB);
                        ((VOIContour) (contours[slice].elementAt(cons))).contains(0, 0, true);

                        for (y = (int) yB[0]; y < yB[1]; y++) {

                            for (x = (int) xB[0]; x < xB[1]; x++) {

                                if (((VOIContour) (contours[slice].elementAt(cons))).contains(x, y, false)) {
                                    destImage.set(x, y, slice, (short) VOIs.VOIAt(i).getWatershedID());
                                }
                            }
                        }
                    }
                }
            }
        }

        destImage.calcMinMax();

        length = mask.size();

        for (i = 0; i < length; i++) {
            mask.clear(i);
        }

        setCompleted(true);
        disposeProgressBar();
        imgBuffer = null;
        System.gc();
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        historyString = new String("Watershed(");

        if ((sigmas != null) && (historyString != null)) {

            for (int i = 0; i < sigmas.length; i++) {
                historyString += sigmas[i] + ", ";
            }

            historyString += ")\n";
        }
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * autocalc2D - does not work !!!!!!! needs to be debugged !!
     */
    /*private void autocalc2D(){
     *  int i; int  xDim, yDim; int  length; short distance[];
     *
     * //gradMagImage = new ModelImage(ModelImage.FLOAT, srcImage.getExtents(), "Gradient Magnitude", null); //gradMagAlgo
     *  = new AlgorithmGradientMagnitudeSep(gradMagImage, srcImage, sigmas, true); //gradMagAlgo.run();
     *
     * //gradMagAlgo = null; //System.gc();
     *
     * xDim = srcImage.getExtents()[0]; yDim = srcImage.getExtents()[1]; length      = xDim * yDim;
     *
     * distance = new short[length];
     *
     * short    newBasin           = 0; short    currentBasin       = 1; short    current_distance   = 0; int
     * fictitiousPixel    = -1; //Point3Ds P; int      tempP = 0; int      pixC, pixN, pixS, pixE, pixW; //float
     * tempGM; //short    tempLabel;
     *
     * int      bins  = (int)(srcImage.getMax() - srcImage.getMin()); //int      bins  = (int)(gradMagImage.getMax() -
     * gradMagImage.getMin()); int      sensitivity = 75; //bins  = 1000;
     *
     * IntVector pixelsVector  = new IntVector(10000, 5000); IntVector queue         = new IntVector(); IntVector stack
     *      = new IntVector(); //HQueue   hQueue = new HQueue((float)gradMagImage.getMin(), //
     *   (float)gradMagImage.getMax(), //                             bins);
     *
     * HQueue   hQueue = new HQueue((float)srcImage.getMin(),                              (float)srcImage.getMax(),
     *                        bins);
     *
     * for (i = 0; i < length; i++){     if ( i%xDim == 0 || i%xDim == (xDim-1)){         destImage.setShort(i, BOUNDARY);
     *     }     else if (i/xDim == 0 || i/xDim == (yDim-1) ) {         destImage.setShort(i, BOUNDARY);     }     else
     * {         destImage.setShort(i, INITIAL);         hQueue.add(i, srcImage.getFloat(i));     } }
     *
     * for (int j = 0; j < length; j++){     distance[j] = 0; } //int x,y; //Start flooding local mins
     *
     * for (i=0; i < bins-sensitivity; i+=sensitivity ){
     *
     *   pixelsVector = hQueue.cloneElementsAt(i, i+sensitivity);
     *
     *   // i to i+n to MASK     while (pixelsVector.isEmpty() == false){
     *
     *       tempP = pixelsVector.lastElement();         pixelsVector.removeLastElement();
     *
     *       pixC = tempP;         pixN = pixC-xDim;         pixS = pixC+xDim;         pixE = pixC+1;         pixW =
     * pixC-1;
     *
     *       if (destImage.getShort(pixC) == BOUNDARY){         }         else {             destImage.setShort(pixC,
     * MASK);             if ((destImage.getShort(pixN) > 0) ||                 (destImage.getShort(pixS) > 0) ||
     *          (destImage.getShort(pixE) > 0) ||                 (destImage.getShort(pixW) > 0)) {
     * distance[pixC]= 1;                 // distance of a path connected pixC to the nearest flooded pixel
     *    // such that the nodes(pixels) of the path are at the same level,                 // if there exists such a
     * path.                 queue.addElement(pixC);          }        }     }
     *
     *   current_distance = 1;     queue.addElement(fictitiousPixel);     while (true) {         tempP =
     * queue.firstElement();         queue.removeElementAt(0);         if (tempP == fictitiousPixel){             if
     * (queue.isEmpty() == true){                 break;             }             else {
     * current_distance++;                 queue.addElement(fictitiousPixel);                 tempP =
     * queue.firstElement();                 queue.removeElementAt(0);             }         }         pixC = tempP;
     *     pixN = pixC-xDim;         pixS = pixC+xDim;         pixE = pixC+1;         pixW = pixC-1;
     *
     *       if (destImage.getShort(pixW) > 0 && distance[pixW] < current_distance){             if
     * (destImage.getShort(pixC) > 0 ) {                 if (destImage.getShort(pixC) != destImage.getShort(pixW)){
     *                destImage.setShort(pixC, WSHED);                 }             }             else if (
     * destImage.getShort(pixC) == MASK ){          // West neighbor has to be basin
     * destImage.setShort(pixC, destImage.getShort(pixW));                 currentBasin = destImage.getShort(pixW);
     *      // extend basin to center;             }         }         else if (destImage.getShort(pixW) == MASK &&
     * distance[pixW] == 0){             distance[pixW] =(short)(current_distance + 1);
     * queue.addElement(pixW);         }
     *
     *
     *       if (destImage.getShort(pixE) > 0 && distance[pixE] < current_distance){             if
     * (destImage.getShort(pixC) > 0 ) {                 if (destImage.getShort(pixC) != destImage.getShort(pixE)){
     *                destImage.setShort(pixC, WSHED);                 }             }             else if (
     * destImage.getShort(pixC) == MASK ){                 destImage.setShort(pixC, destImage.getShort(pixE));
     *       currentBasin = destImage.getShort(pixE);             }         }         else if (destImage.getShort(pixE)
     * == MASK && distance[pixE] == 0){             distance[pixE] = (short)(current_distance + 1);
     * queue.addElement(pixE);         }
     *
     *
     *       if (destImage.getShort(pixN) > 0 && distance[pixN] < current_distance){             if
     * (destImage.getShort(pixC) > 0 ) {                 if (destImage.getShort(pixC) != destImage.getShort(pixN)){
     *                destImage.setShort(pixC, WSHED);                 }             }             else if (
     * destImage.getShort(pixC) == MASK ){                 destImage.setShort(pixC, destImage.getShort(pixN));
     *       currentBasin = destImage.getShort(pixN);             }         }         else if (destImage.getShort(pixN)
     * == MASK && distance[pixN] == 0){             distance[pixN] = (short)(current_distance + 1);
     * queue.addElement(pixN);         }
     *
     *
     *       if (destImage.getShort(pixS) > 0 && distance[pixS] < current_distance){             if
     * (destImage.getShort(pixC) > 0 ) {                 if (destImage.getShort(pixC) != destImage.getShort(pixS)){
     *                destImage.setShort(pixC, WSHED);                 }             }             else if (
     * destImage.getShort(pixC) == MASK ){                 destImage.setShort(pixC, destImage.getShort(pixS));
     *       currentBasin = destImage.getShort(pixS);             }         }         else if (destImage.getShort(pixS)
     * == MASK && distance[pixS] == 0){             distance[pixS] = (short)(current_distance + 1);
     * queue.addElement(pixS);         }     }
     *
     *    while(hQueue.isEmptyAt(i) == false) {         tempP = hQueue.firstAt(i);         distance[tempP]=0;         if
     * (destImage.getShort(tempP) == MASK){             destImage.setShort(tempP, newBasin++);
     * stack.push(tempP);             while (stack.isEmpty() == false){                 tempP = stack.pop();
     *     pixC = tempP;                 pixN = pixC-xDim;                 pixS = pixC+xDim;                 pixE =
     * pixC+1;                 pixW = pixC-1;
     *
     *               if (destImage.getShort(pixN) == MASK) {                     stack.push(pixN);
     * destImage.setShort(pixN, newBasin);                 }                 if (destImage.getShort(pixS) == MASK) {
     *                 stack.push(pixS);                     destImage.setShort(pixS, newBasin);                 }
     *           if (destImage.getShort(pixE) == MASK) {                     stack.push(pixE);
     * destImage.setShort(pixE, newBasin);                 }                 if (destImage.getShort(pixW)  == MASK){
     *                 stack.push(pixW);                     destImage.setShort(pixW, newBasin);                 }
     *       }         }     } }
     *
     * short tempInt; for (i = 0; i < length; i++) {     if (destImage.getShort(i) == MASK){         destImage.setShort(i,
     * (short)0);     }     else if (destImage.getShort(i) == BOUNDARY){         destImage.setShort(i, (short)0);     }
     *    else if (destImage.getShort(i) == WSHED){         destImage.setShort(i, (short)100);     }     else if
     * (destImage.getShort(i) < 0){         destImage.setShort(i, (short)0);     }     else {         tempInt =
     * destImage.getShort(i);         destImage.setShort(i, (short)(tempInt+5));     } }
     *
     * destImage.calcMinMax(); //        gradMagImage.disposeLocal(); hQueue.dispose(); setCompleted(true);
     * //progressBar.dispose(); }
     */

    /**
     * Simple class to hold seed point and basin (object label) value.
     */
    private class Seed {

        /** DOCUMENT ME! */
        public Point3Df point;

        /** DOCUMENT ME! */
        public short seedValue;


        /**
         * Creates a new Seed object.
         *
         * @param  pt     DOCUMENT ME!
         * @param  value  DOCUMENT ME!
         */
        public Seed(Point3Df pt, short value) {

            point = pt;
            seedValue = value;
        }
    }

}
