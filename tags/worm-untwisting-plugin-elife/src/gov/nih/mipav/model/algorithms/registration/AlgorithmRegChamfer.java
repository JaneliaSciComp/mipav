package gov.nih.mipav.model.algorithms.registration;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import de.jtem.numericalMethods.calculus.function.RealFunctionOfSeveralVariables;
import de.jtem.numericalMethods.calculus.minimizing.NelderMead;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * AlgorithmRegChamfer First slice is template (base image) to which match image is registered. Minimize cost, measures
 * distance between surfaces in base image and match image.
 *
 * @version  1.0 April, 2000
 * @author   Delia McGarry
 */
public class AlgorithmRegChamfer extends AlgorithmBase implements RealFunctionOfSeveralVariables {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] baseBuffer;

    /** DOCUMENT ME! */
    private int DIM;

    /** DOCUMENT ME! */
    private boolean entireImage;

    /** DOCUMENT ME! */
    private boolean image25D;

    /** DOCUMENT ME! */
    private ModelImage match, volume25D, edgeImg;

    /** DOCUMENT ME! */
    private Vector3f[] matchPts; // stores mm, not pixel!

    /** DOCUMENT ME! */
    private int N; // # match pts

    /** DOCUMENT ME! */
    private int simplexDim;

    /** DOCUMENT ME! */
    private Vector3f[] TmatchPts; // stores mm, not pixel!

    /** DOCUMENT ME! */
    private TransMatrix[] tMatrixMatchtoBase;

    /** DOCUMENT ME! */
    private TransMatrix tmpXfrm;

    /** DOCUMENT ME! */
    private int volLength, sliceSize;

    /** DOCUMENT ME! */
    private int xdimB, ydimB, zdimB, xdimM, ydimM, zdimM;

    /** DOCUMENT ME! */
    private float xresB, yresB, zresB, xresM;
    
    private CostFunction func;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructs a new registration algorithm.
     *
     * @param  base        binary, edge-extracted base image
     * @param  matchImage  binary edge-extracted image to be transformed
     * @param  maskFlag    flag indicating if the whole image should be processed
     */
    public AlgorithmRegChamfer(ModelImage base, ModelImage matchImage, boolean maskFlag) {

        image25D = false;
        entireImage = maskFlag;
        match = matchImage;

        if (match.getNDims() != base.getNDims()) {
            displayError("Dimensions !=");
            System.gc();

            return;
        } else {
            DIM = match.getNDims();
        }

        try {
            base.convertToFloat();
            match.convertToFloat();

            xdimB = base.getExtents()[0];
            ydimB = base.getExtents()[1];
            xdimM = match.getExtents()[0];
            ydimM = match.getExtents()[1];
            xresB = base.getFileInfo(0).getResolutions()[0];
            yresB = base.getFileInfo(0).getResolutions()[1];
            xresM = match.getFileInfo(0).getResolutions()[0];
            volLength = xdimB * ydimB;

            if (DIM == 2) {
                simplexDim = 3;
            } else if (DIM == 3) {
                simplexDim = 6;
                zresB = base.getFileInfo(0).getResolutions()[2];
                zdimB = base.getExtents()[2];
                zdimM = match.getExtents()[2];
                volLength *= zdimB;
            }

            baseBuffer = new float[volLength];
            base.exportData(0, volLength, baseBuffer); // copy volume into 1D array
        } catch (IOException error) {
            displayError("IOException");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm: Out of memory");
            setCompleted(false);

            return;
        }

    }

    /**
     * Constructs new registration algorithm.
     *
     * @param  volume25D  volume to be registered
     * @param  edgeImg    binary, edge-extracted version of volume25D
     * @param  maskFlag   flag indicating whether to mask to image or not
     * @param  image25D   volume processed as 2.5D, slices registered to adjacent slice
     */
    public AlgorithmRegChamfer(ModelImage volume25D, ModelImage edgeImg, boolean maskFlag, boolean image25D) {
        this.image25D = image25D;
        entireImage = maskFlag;
        this.volume25D = volume25D;
        this.edgeImg = edgeImg;

        if (volume25D.getNDims() != 3) {
            displayError("Cannot process 2D image as 2.5D!");
            System.gc();

            return;
        }

        DIM = 2;
        simplexDim = 3;
        mask = volume25D.generateVOIMask();

        xdimB = volume25D.getExtents()[0];
        ydimB = volume25D.getExtents()[1];
        xdimM = volume25D.getExtents()[0];
        ydimM = volume25D.getExtents()[1];
        xresB = volume25D.getFileInfo(0).getResolutions()[0];
        yresB = volume25D.getFileInfo(0).getResolutions()[1];
        xresM = volume25D.getFileInfo(0).getResolutions()[0];
        volLength = xdimB * ydimB;
        baseBuffer = new float[volLength];
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * SQR = x^2.
     *
     * @param   x  Number to square.
     *
     * @return  DOCUMENT ME!
     */
    public static double SQR(double x) {
        x *= x;

        return x;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Accessor that returns the transformation from match to base. For 2D or 3D.
     *
     * @return  Transformation.
     */
    public TransMatrix getTransformMatchtoBase() {
        return tMatrixMatchtoBase[0];
    }

    /**
     * Accessor that returns the transformation from match to base. For 2.5D.
     *
     * @param   sliceNum  Match slice.
     *
     * @return  Transformation.
     */
    public TransMatrix getTransformMatchtoBase(int sliceNum) {
        return tMatrixMatchtoBase[sliceNum];
    }

    /**
     * Starts the algorithm.
     */
    public void runAlgorithm() {
        

        try {
            tmpXfrm = new TransMatrix(DIM + 1);

            if (image25D == true) { // for 2.5D time series

                float[] imgBuf = new float[volLength];
                float[] tImgBuf = new float[volLength];
                int baseSlice, matchSlice;
                int nSlices = volume25D.getExtents()[2];
                tMatrixMatchtoBase = new TransMatrix[nSlices];

                for (int n = 0; (n < nSlices) && !threadStopped; n++) {
                    tMatrixMatchtoBase[n] = new TransMatrix(DIM + 1);
                }

                // travel thru time series, incrementing base & match slice
                for (matchSlice = 1; (matchSlice < nSlices) && !threadStopped; matchSlice++) {

                    // for (matchSlice = 1; matchSlice < 1; matchSlice++){
                    baseSlice = matchSlice - 1;

                    // copy slice into 1D array
                    edgeImg.exportData(baseSlice * volLength, volLength, baseBuffer);
                    N = getNumberMatchPts25D(matchSlice);
                    Preferences.debug("N = " + N + "\n",Preferences.DEBUG_ALGORITHM);
                    generateCoordList25D(matchSlice);
                    distanceTransform25D();
                    search(tMatrixMatchtoBase[matchSlice]);
                    Preferences.debug("***********************************************\n",Preferences.DEBUG_ALGORITHM);
                    Preferences.debug("Transform for slice " + matchSlice + " =\n",Preferences.DEBUG_ALGORITHM);
                    //System.out.println(tMatrixMatchtoBase[matchSlice]);

                    if (matchSlice > 1) {
                        Preferences.debug("Transform for slice " + matchSlice + " after concatenation =\n",
                        		Preferences.DEBUG_ALGORITHM);

                        tMatrixMatchtoBase[matchSlice].mult(tMatrixMatchtoBase[matchSlice - 1]);
                        //System.out.println(tMatrixMatchtoBase[matchSlice]);
                    }

                    Preferences.debug("***********************************************\n",Preferences.DEBUG_ALGORITHM);
                    transform25DMatchSlice(matchSlice, tMatrixMatchtoBase[matchSlice], imgBuf, tImgBuf);
                }
            } else { // for 2D or 3D images

                /*  tMatrixMatchtoBase = new TransMatrix[1];
                 * tMatrixMatchtoBase[0] = new TransMatrix(DIM+1); N = getNumberMatchPts(); Preferences.debug("N =
                 * "+N+"\n"); generateCoordList(); distanceTransform(); search(tMatrixMatchtoBase[0]);*/
            }

            if (!threadStopped) {
                setCompleted(true);
            }
        } catch (IOException error) {
            displayError("IOException");
            setCompleted(false);
        } catch (OutOfMemoryError e) {
            System.gc();
            displayError("Algorithm: Out of memory");
            setCompleted(false);
        }
    }

    /**
     * Concatenates the matrices.
     *
     * @param  tMatrixMatchtoBase  Array of matrices to concatenate
     * @param  matchSlice          Last slice to concatenate
     */
    @SuppressWarnings("unused")
    private void concatenateMatrix(TransMatrix[] tMatrixMatchtoBase, int matchSlice) {

        for (int i = matchSlice - 1; i > 0; i--) {

            tMatrixMatchtoBase[matchSlice].mult(tMatrixMatchtoBase[i]);
        }
    }

    /**
     * Chamfer matching: Distance transformation coverts binary surface image into grey-level image where each pixel's
     * intensity = distance to nearest surface pixel.
     */
    @SuppressWarnings("unused")
    private void distanceTransform() {
        Preferences.debug("distanceTransform...\n",Preferences.DEBUG_ALGORITHM);

        int i, j, k;

        if (DIM == 2) {

            float[] sum = new float[5];

            // forward pass
            for (i = 0; i < (xdimB - 1); i++) { // forwardMask = {0,3,4,3,4 };

                for (j = 1; j < (ydimB - 1); j++) {
                    sum[0] = baseBuffer[i + (j * xdimB)];
                    sum[1] = baseBuffer[i + ((j + 1) * xdimB)] + 3;
                    sum[2] = baseBuffer[i + 1 + ((j - 1) * xdimB)] + 4;
                    sum[3] = baseBuffer[i + 1 + (j * xdimB)] + 3;
                    sum[4] = baseBuffer[i + 1 + ((j + 1) * xdimB)] + 4;
                    baseBuffer[i + (j * xdimB)] = min(sum);
                }
            }

            for (i = xdimB - 1; i > 0; i--) { // backwardMask = {4,3,4,3,0};

                for (j = ydimB - 2; j > 0; j--) {
                    sum[0] = baseBuffer[i - 1 + ((j - 1) * xdimB)] + 4;
                    sum[1] = baseBuffer[i - 1 + (j * xdimB)] + 3;
                    sum[2] = baseBuffer[i - 1 + ((j + 1) * xdimB)] + 4;
                    sum[3] = baseBuffer[i + ((j - 1) * xdimB)] + 3;
                    sum[4] = baseBuffer[i + ((j) * xdimB)];
                    baseBuffer[i + (j * xdimB)] = min(sum);
                }
            }
        } else if (DIM == 3) {
            sliceSize = xdimB * ydimB;

            float[] sum = new float[14];

            for (i = 0; i < (xdimB - 1); i++) { // forward pass

                for (j = 1; j < (ydimB - 1); j++) {

                    for (k = 1; k < (zdimB - 1); k++) {
                        sum[0] = baseBuffer[i + (j * xdimB) + (k * sliceSize)];
                        sum[1] = baseBuffer[i + 1 + ((j - 1) * xdimB) + ((k - 1) * sliceSize)] + 5;
                        sum[2] = baseBuffer[i + 1 + ((j - 1) * xdimB) + (k * sliceSize)] + 4;
                        sum[3] = baseBuffer[i + 1 + ((j - 1) * xdimB) + ((k + 1) * sliceSize)] + 5;
                        sum[4] = baseBuffer[i + 1 + (j * xdimB) + ((k - 1) * sliceSize)] + 4;
                        sum[5] = baseBuffer[i + 1 + (j * xdimB) + (k * sliceSize)] + 3;
                        sum[6] = baseBuffer[i + 1 + (j * xdimB) + ((k + 1) * sliceSize)] + 4;
                        sum[7] = baseBuffer[i + 1 + ((j + 1) * xdimB) + ((k - 1) * sliceSize)] + 5;
                        sum[8] = baseBuffer[i + 1 + ((j + 1) * xdimB) + (k * sliceSize)] + 4;
                        sum[9] = baseBuffer[i + 1 + ((j + 1) * xdimB) + ((k + 1) * sliceSize)] + 5;
                        sum[10] = baseBuffer[i + ((j + 1) * xdimB) + ((k - 1) * sliceSize)] + 4;
                        sum[11] = baseBuffer[i + ((j + 1) * xdimB) + (k * sliceSize)] + 3;
                        sum[12] = baseBuffer[i + ((j + 1) * xdimB) + ((k + 1) * sliceSize)] + 4;
                        sum[13] = baseBuffer[i + (j * xdimB) + ((k + 1) * sliceSize)] + 3;
                        baseBuffer[i + (j * xdimB) + (k * sliceSize)] = min(sum);
                    }
                }
            }

            for (i = xdimB - 1; i > 0; i--) { // backward pass

                for (j = ydimB - 2; j > 0; j--) {

                    for (k = zdimB - 2; k > 0; k--) {
                        sum[0] = baseBuffer[i - 1 + ((j - 1) * xdimB) + ((k - 1) * sliceSize)] + 5;
                        sum[1] = baseBuffer[i - 1 + ((j - 1) * xdimB) + (k * sliceSize)] + 4;
                        sum[2] = baseBuffer[i - 1 + ((j - 1) * xdimB) + ((k + 1) * sliceSize)] + 5;
                        sum[3] = baseBuffer[i - 1 + (j * xdimB) + ((k - 1) * sliceSize)] + 4;
                        sum[4] = baseBuffer[i - 1 + (j * xdimB) + (k * sliceSize)] + 3;
                        sum[5] = baseBuffer[i - 1 + (j * xdimB) + ((k + 1) * sliceSize)] + 4;
                        sum[6] = baseBuffer[i - 1 + ((j + 1) * xdimB) + ((k - 1) * sliceSize)] + 5;
                        sum[7] = baseBuffer[i - 1 + ((j + 1) * xdimB) + (k * sliceSize)] + 4;
                        sum[8] = baseBuffer[i - 1 + ((j + 1) * xdimB) + ((k + 1) * sliceSize)] + 5;
                        sum[9] = baseBuffer[i + ((j - 1) * xdimB) + ((k - 1) * sliceSize)] + 4;
                        sum[10] = baseBuffer[i + ((j - 1) * xdimB) + (k * sliceSize)] + 3;
                        sum[11] = baseBuffer[i + ((j - 1) * xdimB) + ((k + 1) * sliceSize)] + 4;
                        sum[12] = baseBuffer[i + (j * xdimB) + ((k - 1) * sliceSize)] + 3;
                        sum[13] = baseBuffer[i + (j * xdimB) + (k * sliceSize)];
                        baseBuffer[i + (j * xdimB) + (k * sliceSize)] = min(sum);
                    }
                }
            }
        }
    }

    /**
     * Chamfer matching: Distance transformation coverts binary surface image into grey-level image where each pixel's
     * intensity = distance to nearest surface pixel.
     */
    private void distanceTransform25D() {
        Preferences.debug("distanceTransform...\n",Preferences.DEBUG_ALGORITHM);

        int i, j, index;

        if (DIM == 2) {

            float[] sum = new float[5];

            // forward pass
            for (i = 0; i < (xdimB - 1); i++) { // forwardMask = {0,3,4,3,4 };

                for (j = 1; j < (ydimB - 1); j++) {
                    index = i + (j * xdimB);

                    if ((entireImage == true) || mask.get(index)) {
                        sum[0] = baseBuffer[i + (j * xdimB)];
                        sum[1] = baseBuffer[i + ((j + 1) * xdimB)] + 3;
                        sum[2] = baseBuffer[i + 1 + ((j - 1) * xdimB)] + 4;
                        sum[3] = baseBuffer[i + 1 + (j * xdimB)] + 3;
                        sum[4] = baseBuffer[i + 1 + ((j + 1) * xdimB)] + 4;
                        baseBuffer[index] = min(sum);
                    } else {
                        baseBuffer[index] = 0;
                    }
                }
            }

            for (i = xdimB - 1; i > 0; i--) { // backwardMask = {4,3,4,3,0};

                for (j = ydimB - 2; j > 0; j--) {
                    index = i + (j * xdimB);

                    if ((entireImage == true) || mask.get(index)) {
                        sum[0] = baseBuffer[i - 1 + ((j - 1) * xdimB)] + 4;
                        sum[1] = baseBuffer[i - 1 + (j * xdimB)] + 3;
                        sum[2] = baseBuffer[i - 1 + ((j + 1) * xdimB)] + 4;
                        sum[3] = baseBuffer[i + ((j - 1) * xdimB)] + 3;
                        sum[4] = baseBuffer[i + ((j) * xdimB)];
                        baseBuffer[index] = min(sum);
                    } else {
                        baseBuffer[index] = 0;
                    }
                }
            }
        } else {
            displayError("Image must be 2.5D to use distanceTransform25D");
        }

    }

    /**
     * Generates the coordinate list.
     */
    @SuppressWarnings("unused")
    private void generateCoordList() {
        Preferences.debug("generateCoordList...\n",Preferences.DEBUG_ALGORITHM);

        int i, j, k;
        int n = 0;
        matchPts = new Vector3f[N];
        TmatchPts = new Vector3f[N];

        for (i = 0; i < N; i++) {
            matchPts[i] = new Vector3f();
            TmatchPts[i] = new Vector3f();
        }

        if (DIM == 2) {

            for (i = 0; i < xdimM; i++) {

                for (j = 0; j < ydimM; j++) {

                    if (match.getFloat(i, j) != 0) { // if foreground
                        matchPts[n].X = i * xresM;
                        matchPts[n].Y = j * xresM;
                        n++;
                    }
                }
            }
        } else if (DIM == 3) {

            for (i = 0; i < xdimM; i++) {

                for (j = 0; j < ydimM; j++) {

                    for (k = 0; k < zdimM; k++) {

                        if (match.getFloat(i, j, k) != 0) { // if foreground
                            matchPts[n].X = i * xresM;
                            matchPts[n].Y = j * xresM;
                            matchPts[n].Z = k * xresM;
                            n++;
                        }
                    }
                }
            }
        }
    }

    /**
     * Generates the coordinate list 2.5 D.
     *
     * @param  matchSlice  Slice to match.
     */
    private void generateCoordList25D(int matchSlice) {
        Preferences.debug("generateCoordList25D...\n",Preferences.DEBUG_ALGORITHM);

        int i, j, index;
        int n = 0;
        matchPts = new Vector3f[N];
        TmatchPts = new Vector3f[N];

        for (i = 0; i < N; i++) {
            matchPts[i] = new Vector3f();
            TmatchPts[i] = new Vector3f();
        }

        if (image25D == true) {

            for (i = 0; i < xdimM; i++) {

                for (j = 0; j < ydimM; j++) {
                    index = i + (j * xdimM) + (matchSlice * xdimM * ydimM);

                    if (((entireImage == true) || mask.get(index)) && (edgeImg.getFloat(i, j, matchSlice) != 0)) { // if foreground
                        matchPts[n].X = i * xresM;
                        matchPts[n].Y = j * xresM;
                        n++;
                    }
                }
            }
        } else {
            displayError("image not 2.5D");

            return;
        }

    }

    /**
     * Gets the number of match points.
     *
     * @return  The number of match points.
     */
    @SuppressWarnings("unused")
    private int getNumberMatchPts() {
        Preferences.debug("getNumberMatchPts...\n",Preferences.DEBUG_ALGORITHM);

        int i, j, k;
        N = 0;

        if (DIM == 2) {

            for (i = 0; i < xdimM; i++) {

                for (j = 0; j < ydimM; j++) {

                    if (match.getFloat(i, j) != 0) { // if foreground
                        N++;
                    }
                }
            }
        } else if (DIM == 3) {

            for (i = 0; i < xdimM; i++) {

                for (j = 0; j < ydimM; j++) {

                    for (k = 0; k < zdimM; k++) {

                        if (match.getFloat(i, j, k) != 0) { // if foreground
                            N++;
                        }
                    }
                }
            }
        }

        return N;
    }

    /**
     * Get number of match points 2.5 D.
     *
     * @param   matchSlice  Slice to match.
     *
     * @return  Number of match points.
     */
    private int getNumberMatchPts25D(int matchSlice) {
        Preferences.debug("getNumberMatchPts25D...\n",Preferences.DEBUG_ALGORITHM);

        int i, j, index;
        N = 0;

        if (image25D == true) {

            for (i = 0; i < xdimM; i++) {

                for (j = 0; j < ydimM; j++) {
                    index = i + (j * xdimM) + (matchSlice * xdimM * ydimM);

                    if (((entireImage == true) || mask.get(index)) && (edgeImg.getFloat(i, j, matchSlice) != 0)) { // if foreground
                        N++;
                    }
                }
            }
        } else {
            displayError("image not 2.5D");

            return -1;
        }

        return N;
    }

    /**
     * Converts row of p into 3x3 matrix.
     *
     * @param  x     row of p[][]
     * @param  xfrm  transformation matrix.
     */
    private void getTransformFromX(double[] x, TransMatrix xfrm) {

        if (DIM == 2) {
            xfrm.setTransform(x[0], x[1], x[2]);
        } else if (DIM == 3) {
            xfrm.setTransform(x[0], x[1], x[2], x[3], x[4], x[5]);
        }
        // xfrm.print();

    }

    /*
     * private void transformSlice(int slice, TransMatrix xfrm, float imgBuf[],float tImgBuf[]) { //int interp =
     * AlgorithmTransform.NEAREST_NEIGHBOR; int interp = AlgorithmTransform.BILINEAR; float xres =
     * volume.getFileInfo(0).getResolutions()[0]; float yres = volume.getFileInfo(0).getResolutions()[1]; float tres =
     * volume.getFileInfo(0).getResolutions()[2];
     *
     * int sliceIndex = slice*sliceSize; try{ volume.exportData(sliceIndex, sliceSize, imgBuf);//copy volume into 1D array
     *
     * AlgorithmTransform.transformNearestNeighbor2D(imgBuf, tImgBuf, xfrm, xdim, ydim, xres, yres);
     * volume.importData(sliceIndex, tImgBuf, false);//copy imgBuff back into image System.gc(); } catch (IOException
     * error) { displayError("Algorithm: Image(s) locked"); setCompleted(false);  return; } catch
     * (OutOfMemoryError e){ imgBuf = null; System.gc(); displayError("Algorithm: Out of memory"); setCompleted(false);
     *  return; } volume.calcMinMax(); }*/

    /**
     */
    private void initializeNelderMead(double[][] xi, double[] initialPoint) {
        Preferences.debug("InitializePandY:\n",Preferences.DEBUG_ALGORITHM);

        int i, j;

        for (i = 0; i < simplexDim; i++) {
            initialPoint[i] = 0;
            for (j = 0; j < simplexDim; j++) {
                xi[i][j] = 0f;
            }
        }

        double scale = 1.0/0.00024;
        xi[0][0] = scale * 2 * 3f; // Tx
        xi[1][1] = scale * 2 * 2f; // Ty

        if (DIM == 2) {
            xi[2][2] = scale * 2 * 5f; // Rot about z-axis (degrees)
        } else if (DIM == 3) {
            xi[2][2] = scale * 2 * 1f; // Tz
            xi[3][3] = scale * 2 * 1f; // Rot about x-axis (degrees)
            xi[4][4] = scale * 2 * 1f; // Rot about y-axis (degrees)
            xi[5][5] = scale * 2 * 1f; // Rot about z-axis (degrees)
        }
    }

    /**
     * Finds minimum sum under mask.
     *
     * @param   x  Array to find minimum of.
     *
     * @return  DOCUMENT ME!
     */
    private float min(float[] x) {
        int i;
        int maskLength = 0;
        float min = x[0];

        if (DIM == 2) {
            maskLength = 5;
        } else if (DIM == 3) {
            maskLength = 14;
        }

        for (i = 1; i < maskLength; i++) {
            min = Math.min(x[i], min);
        }

        return min;
    }

    /**
     * Search.
     *
     * @param  xfrm  Transformation matrix.
     */
    private void search(TransMatrix xfrm) {
        // This function has not been fully tested.
        // When it is ready to test, see AlgorithmRegVOILandmark for how
        // to use the NelderMead class.
        Preferences.debug("Search:\n",Preferences.DEBUG_ALGORITHM);

        func = new CostFunction();
        double[][] xi = new double[simplexDim][simplexDim];
        double[] initialPoint = new double[simplexDim];

        initializeNelderMead(xi, initialPoint);
        NelderMead.search(initialPoint, xi,
                0.0000001, this, 50000, null);

        getTransformFromX(initialPoint, xfrm); // lowest cost row

        return;
    }

    /**
     * Transforms 2.5 D match slice.
     *
     * @param  matchSlice  Slice to match
     * @param  xfrm        Transformation matrix
     * @param  imgBuf      Image buffer
     * @param  tImgBuf     Transformed image buffer
     */
    private void transform25DMatchSlice(int matchSlice, TransMatrix xfrm, float[] imgBuf, float[] tImgBuf) {
        int sliceSize = volLength; // volLength = xdim*ydim for 2.5D
        int sliceIndex = matchSlice * sliceSize;

        // xfrm.print();
        try {
            volume25D.exportData(sliceIndex, sliceSize, imgBuf); // copy volume into 1D array

            // Does tImgBuf need to be zeroed ?
            AlgorithmTransform.transformBilinear(imgBuf, tImgBuf, xfrm, xdimM, ydimM,
                                                 volume25D.getFileInfo(0).getResolutions()[0],
                                                 volume25D.getFileInfo(0).getResolutions()[1], xdimM, xdimM,
                                                 volume25D.getFileInfo(0).getResolutions()[0],
                                                 volume25D.getFileInfo(0).getResolutions()[1], null);
            volume25D.importData(sliceIndex, tImgBuf, false); // copy imgBuff back into image

        } catch (IOException error) {
            displayError("Algorithm: Image(s) locked");
            setCompleted(false);
            

            return;
        } catch (OutOfMemoryError e) {
            imgBuf = null;
            System.gc();
            displayError("Algorithm: Out of memory");
            setCompleted(false);
            

            return;
        }

        Preferences.debug("Transforming slice " + matchSlice + " done\n",Preferences.DEBUG_ALGORITHM);
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    
    /**
     * Class for specifying optimization function for AlgorithmSimplexOpt.
     */
    private class CostFunction implements AlgorithmOptimizeFunctionBase {

        /**
         * CostFunction constructor.
         */
        public CostFunction() { }
        
        public int getCostFunction()
        {
            return 0;
        }
        /**
         * Filler to implement cost functions from AlgorithmOptimizeFunctionBase.
         *
         * @param   tMatrix  Transformation matrix.
         *
         * @return  -1
         */
        public double cost(TransMatrix tMatrix) {
            return -1;
        }
        
        /**
         * Filler to implement cost functions from AlgorithmOptimizeFunctionBase.
         *
         * @param   tMatrix  Transformation matrix.
         *
         * @return  -1
         */
        public double cost(TransMatrixd tMatrix) {
            return -1;
        }

        /**
         * Calculated cost.
         *
         * @param   x  row of p[][]
         *
         * @return  cost
         */
        public double cost(double[] x) {
            int X0pos, Y0pos, Z0pos;
            int X1pos, Y1pos, Z1pos;
            float X, Y, Z;
            float x0, y0, z0;
            float x1, y1, z1;
            float value;
            int n = 0;
            double sum = 0f;
            double cost = 0f;

            // TransMatrix xfrm;
            getTransformFromX(x, tmpXfrm);
            tmpXfrm.transformAsVector3Df(matchPts, TmatchPts);

            if (DIM == 2) {

                for (n = 0; n < N; n++) {
                    value = 0; // remains zero if voxel is transformed out of bounds
                    X = TmatchPts[n].X / xresB;

                    if ((X >= 0) && (X < (xdimB - 1))) {
                        Y = TmatchPts[n].Y / yresB;

                        if ((Y >= 0) && (Y < (ydimB - 1))) {

                            // set intensity of i,j,k to new transformed coordinate if
                            // x,y,z is w/in dimensions of image
                            x0 = X - (int) X; // bilinear interp
                            y0 = Y - (int) Y;
                            x1 = 1 - x0;
                            y1 = 1 - y0;
                            X0pos = (int) X;
                            Y0pos = (int) Y * xdimB;
                            X1pos = X0pos + 1;
                            Y1pos = Y0pos + xdimB;
                            value = (x1 * y1 * baseBuffer[Y0pos + X0pos]) + (x0 * y1 * baseBuffer[Y0pos + X1pos]) +
                                    (x1 * y0 * baseBuffer[Y1pos + X0pos]) + (x0 * y0 * baseBuffer[Y1pos + X1pos]);
                        }
                    }

                    // sum+= SQR(baseBuffer[((int)TmatchPts[n].x,
                    // (int)TmatchPts[n].y));
                    sum += SQR(value);
                }
            } else if (DIM == 3) {
                int sliceSize = xdimB * ydimB;

                for (n = 0; n < N; n++) {
                    value = 0; // remains zero if voxel is transformed out of bounds
                    X = TmatchPts[n].X / xresB;

                    if ((X >= 0) && (X < (xdimB - 1))) {
                        Y = TmatchPts[n].Y / yresB;

                        if ((Y >= 0) && (Y < (ydimB - 1))) {
                            Z = TmatchPts[n].Z / zresB;

                            if ((Z >= 0) && (Z < (zdimB - 1))) {
                                x0 = X - (int) X;
                                y0 = Y - (int) Y;
                                z0 = Z - (int) Z;
                                x1 = 1 - x0;
                                y1 = 1 - y0;
                                z1 = 1 - z0;
                                X0pos = (int) X;
                                Y0pos = (int) Y * xdimB;
                                Z0pos = (int) Z * sliceSize;
                                X1pos = X0pos + 1;
                                Y1pos = Y0pos + xdimB;
                                Z1pos = Z0pos + sliceSize;

                                value = (x1 * y1 * z1 * baseBuffer[Z0pos + Y0pos + X0pos]) +
                                        (x0 * y1 * z1 * baseBuffer[Z0pos + Y0pos + X1pos]) +
                                        (x1 * y0 * z1 * baseBuffer[Z0pos + Y1pos + X0pos]) +
                                        (x0 * y0 * z1 * baseBuffer[Z0pos + Y1pos + X1pos]) +
                                        (x1 * y1 * z0 * baseBuffer[Z1pos + Y0pos + X0pos]) +
                                        (x0 * y1 * z0 * baseBuffer[Z1pos + Y0pos + X1pos]) +
                                        (x1 * y0 * z0 * baseBuffer[Z1pos + Y1pos + X0pos]) +
                                        (x0 * y0 * z0 * baseBuffer[Z1pos + Y1pos + X1pos]);

                            }
                        }
                    }

                    sum += SQR(value);
                    // sum+= SQR(baseBuffer[((int)TmatchPts[n].x,(int)TmatchPts[n].y,
                    // (int)TmatchPts[n].z));
                }
            }

            cost = (double) Math.sqrt(sum / ((double) N)) / (double) 3;

            return cost;

        }

        
    }

    @Override
    public double eval(double[] x) {
        return func.cost(x);
    }

    @Override
    public int getNumberOfVariables() {
        return simplexDim;
    }
}
