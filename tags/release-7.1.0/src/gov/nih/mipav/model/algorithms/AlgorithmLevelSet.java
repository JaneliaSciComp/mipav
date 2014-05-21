package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.util.*;


/**
 * This algorithm iteratively expands or contracts one or more contours to a boundary.
 *
 * <p>Reference: Level Set Methods and Fast Marching Methods by J.A. Sethian, Cambridge University Press, 1999</p>
 *
 * <p>First the distance phi of every point from a contour is determined. If the point is inside a contour, the distance
 * is negative. If the point is outside all contours the distance is positive. The routine slicePointToContour(int
 * slice, int x, int y) in VOI.java is used to obtain phi for 2D. In 3D pointToContour(int x, int y, int z) is used.</p>
 *
 * <p>Equation 17.8 on page 222 of the reference is used: d(phi)/dt + gI(x,y)(1 - (epsilon)(kappa))|grad(phi)| -
 * (beta)(grad(P(x,y))) dot grad(phi) = 0 where dot represents the dot product between (beta)(grad(P(x,y))) and
 * grad(phi). Note that this equation is a development of the most basic form of the level set equation 1.6 on page 7:
 * d(phi)/dt + F|grad phi| = 0</p>
 *
 * <p>Equation 17.7 on page 220 gives: gI(x,y) = 1/(1 + |grad(Gaussian sigma * I(x,y))|) = 1/(1 - P(x,y)) An equation on
 * page 222 gives: P(x,y) = -|grad(Gaussian sigma * I(x,y))| Note that in this code the |grad(Gaussian sigma * I(x,y))|
 * is normalized to vary from 0.0 to 100.0. Equation 6.35 on page 69 gives the 2D curvature: The curvature kappa = the
 * divergence of the normalized grad(phi) = ((phixx)(phiy)**2 - 2(phiy)(phix)(phixy) + (phiyy)(phix)**2)/ ((phix)**2 +
 * (phiy)**2)**(3/2) Derivatives based on the central difference approximation give: phix = ((phi)i+1 -
 * (phi)i-1)/(2(deltaX)) phiy = ((phi)j+1 - (phi)j-1)/(2(deltaY)) phixx = ((phi)i+1 - 2(phi)i + (phi)i-1)/(deltaX**2)
 * phiyy = ((phi)j+1 - 2(phi)j + (phi)j-1)/(deltaY**2) phixy = ((phi)i+1,j+1 - (phi)i+1,j-1 - (phi)i-1,j+1 +
 * (phi)i-1,j-1)/(4(deltaX)(deltaY))</p>
 *
 * <p>The solution to this equation is based on solution 6.45 on page 74 to equation 6.44 on page 73. Equation 6.44 is
 * similar to equation 17.8. Note that the equation is currently set up to perform an expansion. The discussion on page
 * 220 says use 1 - (epsilon)(kappa) for expansion and -1 - (epsilon)(kappa) for contraction. (phi)i,j,n+1 = (phi)i,j +
 * deltaT[c1 + c2 + c3] where: c1 = -[max(gI(i,j),0)gradPlus + min(gI(i,j),0)gradMinus] for expansion c1 =
 * -[max(-gI(i,j),0)gradPlus + min(-gI(i,j),0)gradMinus] for contraction Since gI(i,j) is always between 0 and 1: c1
 * expansion = -gI(i,j)gradPlus for movement = EXPAND or EXPAND_CONTRACT c1 contraction = gI(i,j)gradMinus for movement
 * = CONTRACT gradPlus is given by equation 6.18 on page 65: gradPlus = [max(Dminusx,0)**2 + min(Dplusx,0)**2 +
 * max(Dminusy,0)**2 + min(Dplusy,0)**2]**0.5 gradMinus is given by equation 6.19 on page 65: gradMinus =
 * [max(Dplusx,0)**2 + min(Dminusx,0)**2 + max(Dplusy,0)**2 + min(Dminusy,0)**2]**0.5 Dminusx = ((phi)i,j -
 * (phi)i-1,j)/deltaX Dplusx = ((phi)i+1,j - (phi)1,j)/deltaX Dminusy = ((phi)i,j - (phi)i,j-1)/deltaY Dplusy =
 * ((phi)i,j+1 - (phi)i,j)/deltaY c2 = -[max(u(i,j),0)Dminusx + min(u(i,j),0)Dplusx + max(v(i,j),0)Dminusy +
 * min(v(i,j),0)Dplusy] where u(i,j) is the first vector component of -beta(grad(P(x,y))) and v(i,j) is the second
 * vector component of -beta(grad(P(x,y))). c3 = epsilon(kappa(i,j))(gI(i,j))(((Dzerox)**2 + (Dzeroy)**2)**(1/2)) Dzerox
 * = phix = ((phi)i+1,j - (phi)i-1,j)/(2(deltaX)) Dzeroy = phiy = ((phi)i,j+1 - (phi)i,j-1)/(2(deltaY))</p>
 *
 * <p>The user supplies 7 constants needed for the solution of the equation: 1.) epsilon 2.) sigma for the gaussian
 * derivative convolution 3.) The deltaT time step value 4.) The number of iterations 5.) movement = EXPAND,
 * EXPAND_CONTRACT, or CONTRACT EXPAND_CONTRACT is the EXPAND mechanism except that it does not prevent a value of phi
 * from transiting from a negative value to value >= 0.0. movement == EXPAND prevents a boundary from ever retreating -
 * phi is prevented from ever changing from negative to >= 0. movement == CONTRACT prevents a boundary from ever
 * expanding - phi is prevented from ever changing from >= 0.0 to negative. 6.) edgeAttract the ratio of the maximum
 * value of the absolute value of the edge attractive force to the maximum value of the absolute value of the sum of the
 * propagation and curvature forces. 7.) testIters if testIters > 0, check if the boundary is unchanged every testIters
 * iterations. On page 221 the author used time step = 0.001 and 391 iterations on a 64 by 64 grid. In the paper A Fast
 * Level Set Based Algorithm for Topology-Independent Shape Modeling by Malladi, Sethian, and Vemuri a similar scheme
 * was used. epsilons of 0.05, 0.25, and 0.75 were used. As epsilon was increased, the level set attained a smoother
 * final configuration. The (epsilon)(kappa) term smoothes out the high curvature regions and has the same regularizing
 * effect as the internal deformation energy term in thin-plate-membrane splines. Time steps ranging from 0.00025 to
 * 0.001 were used. One run on a 64 by 64 grid using 0.001 time steps took 391 iterations to complete and one run on a
 * 128 by 128 grid using 0.0005 time steps took 575 iterations to complete. a sigma = 3.25 was used. In Shape Modeling
 * with Front Propagation: A Level Set Approach by Malladi, Sethian, and Vemuri a 128 by 128 image with 0.00025 time
 * steps took 1,180 iterations.</p>
 *
 * <p>The term grad(P) dot grad(phi) denotes the projection of an (attractive) force vector on the surface normal. This
 * force results from gradient of a potential field P(x,y) = -|grad(Gaussian sigma * I(x,y)| that attracts the contours
 * to the image edges. Beta controls the strength of this attraction.</p>
 *
 * <p>When the iterations are complete, the area inside the curve has negative phi values and the area outside the curve
 * has positive phi values. Bitset a mask from the pixels with negative phi values and perform a mask to VOI conversion
 * to obtain the output curve.</p>
 *
 * <p>Extensions to 3D are very straightforward except possibly for the 3D mean curvature. Equation 6.36 on page 70 is
 * used: mean curvature = ((phiyy + phizz)phix**2 + (phixx + phizz)phiy**2 + (phixx + phiyy)phiz**2 -
 * 2(phix)(phiy)(phixy) - 2(phix)(phiz)(phixz) -2(phiy)(phiz)(phiyz))/ (phix**2 + phiy**2 + phiz**2)**1.5</p>
 */
public class AlgorithmLevelSet extends AlgorithmBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private static final int EXPAND = 1;

    /** DOCUMENT ME! */
    private static final int EXPAND_CONTRACT = 2;

    /** DOCUMENT ME! */
    private static final int CONTRACT = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Time step The defalut is 0.05 for 2D and 0.025 for 3D. */
    private float deltaT = 0.05f;

    /**
     * edgeAttract is the ratio of the maximum value of the absolute value of the edge attractive force to the maximum
     * value of the absolute value of the sum of the propagation and curvature forces.
     */
    private float edgeAttract = 20.0f;

    /**
     * epsilon is used to smooth high curvature regions. epsilon is between 0 and 1 with larger epsilon doing more
     * smoothing
     */
    private float epsilon = 0.05f;

    /** Storage location of the first derivative of the Gaussian in the X direction. */
    private float[] GxData;

    /** Storage location of the first derivative of the Gaussian in the Y direction. */
    private float[] GyData;

    /** Storage location of the first derivative of the Gaussian in the Z direction. */
    private float[] GzData;

    /** Number of iterations of the diffusion. */
    private int iterations;

    /** Dimensionality of the kernel. */
    private int[] kExtents;

    /** EXPAND, EXPAND_CONTRACT, or CONTRACT. */
    private int movement;

    /** Standard deviations of the gaussian used to calculate the kernels. */
    private float[] sigmas;

    /** If testIters > 0, check if the boundary is unchanged every testIters iterations. */
    private int testIters = 100;
    
    /** If image25D is true in 3D images, process each slice separately */
    private boolean image25D = false;
    
    /** Stores output of AlgorithmConvolver */
    private float[] outputBuffer = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * LevelSet.
     *
     * @param  srcImg       reference to the source image
     * @param  sigmas       sigmas used to describe the gaussian that is used in the calculation of the gradient
     *                      magnitude
     * @param  movement     EXPAND, EXPAND_CONTRACT, or CONTRACT
     * @param  iter         number of iterations (t) of the diffusion equation
     * @param  deltaT       time step
     * @param  epsilon      smoothing factor between 0 and 1 with higher values giving greater smoothing
     * @param  edgeAttract  the ratio of the maximum value of the absolute value of the edge attractive force to the
     *                      maximum value of the absolute value of the sum of the propagation and curvature forces
     * @param  testIters    If testIters > 0, check if the boundary is unchanged every testIters iterations
     * @param  image25D     If truein 3D images, process each slice separately
     */
    public AlgorithmLevelSet(ModelImage srcImg, float[] sigmas, int movement, int iter, float deltaT, float epsilon,
                             float edgeAttract, int testIters, boolean image25D) {
        super(null, srcImg);

        this.sigmas = sigmas;
        this.movement = movement;
        iterations = iter;
        this.deltaT = deltaT;
        this.epsilon = epsilon;
        this.edgeAttract = edgeAttract;
        this.testIters = testIters;
        this.image25D = image25D;

        if ((srcImg.getNDims() == 2) || ((srcImage.getNDims() > 2) && image25D)) {
            makeKernels2D();
        } else if (srcImg.getNDims() > 2) {
            makeKernels3D();
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void cleanUp() {
        GxData = null;
        GyData = null;
        GzData = null;
        kExtents = null;
        sigmas = null;
        srcImage = null;
    }

    /**
     * finalize - sets class storages arrays to null so that System.gc() can free the memory.
     */
    public void finalize() {
        cleanUp();
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {
        AlgorithmConvolver convolver;

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }
        
        if ((srcImage.getNDims() == 2) || image25D) {
            boolean entireImage = true;
            boolean sqrtXY = true;
            convolver = new AlgorithmConvolver(srcImage, GxData, GyData, kExtents, entireImage, sqrtXY);
        }
        else {
            convolver = new AlgorithmConvolver(srcImage, GxData, GyData, GzData, kExtents, true);
        }
        convolver.addListener(this);
        convolver.run();
        convolver.finalize();    

        if (srcImage.getNDims() == 2) {
            calc2D();
        } else if (srcImage.getNDims() > 2) {
            if (image25D) {
                calc25D();
            }
            else {
                calc3D();
            }
        }
    }

    /**
     * calc2D - Calculates level set from contours, propagates level set, and obtains new contour voi from level set.
     */
    private void calc2D() {

        float beta; // controls attraction of contours to boundaries
        int i, j, n;
        int length;
        float[] gBuffer;
        float[] uComp;
        float[] vComp;
        double[] phiImage;
        double[] phiNext;
        double[] originalPhi;
        double[] tempBuffer;
        int nVOI;
        ViewVOIVector VOIs;
        VOI contourVOI;
        short voiID;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = 1;
        int xPos, yPos;
        double dMinusX, dPlusX, dMinusY, dPlusY;
        double gradMinus, gradPlus;
        double c1, c1a, c1b, c1c, c1d;
        double c2;
        double phix, phiy, phixx, phiyy, phixy, denom;
        double possiblePhi;
        double c3;
        double c13max, c2max;
        double distance, minDistance;
        boolean[] active;
        boolean reinitializePhi = false;
        int[] boundaryX;
        int[] boundaryY;
        int boundaryLength;
        int checkIters = testIters;
        int checkIters2 = testIters;
        boolean haveChanged = true;
        float minGrad;
        float maxGrad;
        float divisor;

        try {
            length = srcImage.getSliceSize();
            gBuffer = new float[length];
            uComp = new float[length];
            vComp = new float[length];
            phiImage = new double[length];
            phiNext = new double[length];
            originalPhi = new double[length];
            active = new boolean[length];
            boundaryX = new int[length];
            boundaryY = new int[length];
            fireProgressStateChanged(srcImage.getImageName(), "Evolving the level set ...");
        } catch (OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("Level set: Out of Memory");
            setCompleted(false);

            return;
        }

        VOIs = srcImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }

        contourVOI = VOIs.VOIAt(i);

        maxGrad = 0.0f;
        minGrad = Float.MAX_VALUE;

        for (i = 0; i < length; i++) { // calculate gradient magnitude

            if (outputBuffer[i] > maxGrad) {
                maxGrad = outputBuffer[i];
            }

            if (outputBuffer[i] < minGrad) {
                minGrad = outputBuffer[i];
            }

            // Normalize the gradient magnitude to go from 0 to 100
        }

        if (maxGrad > minGrad) {
            divisor = maxGrad - minGrad;
        } else {
            divisor = 1.0f;
        }

        for (i = 0; i < length; i++) {
            outputBuffer[i] = (outputBuffer[i] - minGrad) * 100.0f / divisor;
        }

        for (i = 0; i < length; i++) {
            gBuffer[i] = 1 / (1 + outputBuffer[i]);
        }

        fireProgressStateChanged(2);

        // Adjust beta after the first trial run thru.
        beta = 1.0f;

        for (i = xDim + 1; i < (length - xDim - 1); i++) {
            xPos = i % xDim;

            if ((xPos >= 1) && (xPos < (xDim - 1))) {
                uComp[i] = beta * (outputBuffer[i + 1] - outputBuffer[i - 1]) / 2.0f;
                vComp[i] = beta * (outputBuffer[i + xDim] - outputBuffer[i - xDim]) / 2.0f;
            }
        }

        outputBuffer = null;
        System.gc();

        fireProgressStateChanged(4);

        for (i = 0; (i < length) && !threadStopped; i++) {
            yPos = i / xDim;
            xPos = i % xDim;
            phiImage[i] = contourVOI.pointToContour(xPos, yPos, 0);
            phiNext[i] = phiImage[i];
            originalPhi[i] = phiImage[i];
        }

        // For contraction runs simply only process on those pixels for which
        // phi is initially 6 or less
        // For expansion runs initially only process on those pixels for which
        // -6 <= phi <= 6.  When the contour expands to where the phi was
        // originally >= 4.0, then find the boundary and use this to create a
        // new contour.
        // Note that page 85 of Level Set Methods and Fast Marching Methods
        // states: "Our experience indicates that a narrow band width of about
        // six grid points on either side of the zero level set is a reasonable
        // balance between re-initialization costs and update costs."
        for (i = 0; i < length; i++) {
            active[i] = true;
        }

        if (movement == CONTRACT) {

            for (i = 0; i < length; i++) {

                if (phiImage[i] > 6.0) {
                    active[i] = false;
                }
            }
        } else { // EXPAND or EXPAND_CONTRACT

            for (i = 0; i < length; i++) {

                if ((phiImage[i] > 6.0) || (phiImage[i] < -6.0)) {
                    active[i] = false;
                }
            }
        }

        fireProgressStateChanged(6);

        c13max = -Double.MAX_VALUE;
        c2max = -Double.MAX_VALUE;

        // On every iteration adjust beta to make |c2|max = edgeAttract*|c1+c3|max
        // so the attractive force predominates over the other 2 forces
        // Let beta = 1.0 for the first run of this equation.
        for (i = xDim + 1; (i < (length - xDim - 1)) && !threadStopped; i++) {

            if (active[i]) {
                xPos = i % xDim;

                if ((xPos >= 1) && (xPos < (xDim - 1))) {
                    dMinusX = phiImage[i] - phiImage[i - 1];
                    dPlusX = phiImage[i + 1] - phiImage[i];
                    dMinusY = phiImage[i] - phiImage[i - xDim];
                    dPlusY = phiImage[i + xDim] - phiImage[i];

                    if ((movement == EXPAND) || (movement == EXPAND_CONTRACT)) {
                        c1a = Math.max(dMinusX, 0);
                        c1b = Math.min(dPlusX, 0);
                        c1c = Math.max(dMinusY, 0);
                        c1d = Math.min(dPlusY, 0);
                        gradPlus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                        c1 = -gBuffer[i] * gradPlus;
                    } else { // CONTRACT
                        c1a = Math.max(dPlusX, 0);
                        c1b = Math.min(dMinusX, 0);
                        c1c = Math.max(dPlusY, 0);
                        c1d = Math.min(dMinusY, 0);
                        gradMinus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                        c1 = gBuffer[i] * gradMinus;
                    }

                    c2 = -((Math.max(uComp[i], 0) * dMinusX) + (Math.min(uComp[i], 0) * dPlusX) +
                           (Math.max(vComp[i], 0) * dMinusY) + (Math.min(vComp[i], 0) * dPlusY));

                    phix = (phiImage[i + 1] - phiImage[i - 1]) * 0.5;
                    phiy = (phiImage[i + xDim] - phiImage[i - xDim]) * 0.5;
                    phixx = phiImage[i + 1] - (2 * phiImage[i]) + phiImage[i - 1];
                    phiyy = phiImage[i + xDim] - (2 * phiImage[i]) + phiImage[i - xDim];
                    phixy = (phiImage[i + xDim + 1] - phiImage[i + xDim - 1] - phiImage[i - xDim + 1] +
                             phiImage[i - xDim - 1]) * 0.25;

                    double phix2 = phix * phix;
                    double phiy2 = phiy * phiy;

                    denom = phix2 + phiy2;

                    if (denom > 0) {
                        c3 = ((phixx * phiy2) - (2 * phiy * phix * phixy) + (phiyy * phix2)) / denom;
                        c3 *= epsilon * gBuffer[i];
                    } else {
                        c3 = 0.0;
                    }

                    if (Math.abs(c1 + c3) > c13max) {
                        c13max = Math.abs(c1 + c3);
                    }

                    if (Math.abs(c2) > c2max) {
                        c2max = Math.abs(c2);
                    }
                }

            }
        }

        beta = (float) (edgeAttract * c13max / c2max);

        long startTime = System.currentTimeMillis();

        for (n = 0; (n < iterations) && !threadStopped && haveChanged; n++) {
            fireProgressStateChanged(10 + Math.round((float) n / (iterations - 1) * 90));

            c13max = -Double.MAX_VALUE;
            c2max = -Double.MAX_VALUE;
            boundaryLength = 0;

            if (checkIters == n) {
                haveChanged = false;
                checkIters2 = n;
            }

            for (i = xDim + 1; (i < (length - xDim - 1)) && !threadStopped; i++) {

                if (active[i]) {
                    xPos = i % xDim;

                    if ((xPos >= 1) && (xPos < (xDim - 1))) {
                        dMinusX = phiImage[i] - phiImage[i - 1];
                        dPlusX = phiImage[i + 1] - phiImage[i];
                        dMinusY = phiImage[i] - phiImage[i - xDim];
                        dPlusY = phiImage[i + xDim] - phiImage[i];

                        if ((movement == EXPAND) || (movement == EXPAND_CONTRACT)) {
                            c1a = Math.max(dMinusX, 0);
                            c1b = Math.min(dPlusX, 0);
                            c1c = Math.max(dMinusY, 0);
                            c1d = Math.min(dPlusY, 0);
                            gradPlus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                            c1 = -gBuffer[i] * gradPlus;
                        } else { // CONTRACT
                            c1a = Math.max(dPlusX, 0);
                            c1b = Math.min(dMinusX, 0);
                            c1c = Math.max(dPlusY, 0);
                            c1d = Math.min(dMinusY, 0);
                            gradMinus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                            c1 = gBuffer[i] * gradMinus;
                        }

                        c2 = -((Math.max(beta * uComp[i], 0) * dMinusX) + (Math.min(beta * uComp[i], 0) * dPlusX) +
                               (Math.max(beta * vComp[i], 0) * dMinusY) + (Math.min(beta * vComp[i], 0) * dPlusY));

                        phix = (phiImage[i + 1] - phiImage[i - 1]) * 0.5;
                        phiy = (phiImage[i + xDim] - phiImage[i - xDim]) * 0.5;
                        phixx = phiImage[i + 1] - (2 * phiImage[i]) + phiImage[i - 1];
                        phiyy = phiImage[i + xDim] - (2 * phiImage[i]) + phiImage[i - xDim];
                        phixy = (phiImage[i + xDim + 1] - phiImage[i + xDim - 1] - phiImage[i - xDim + 1] +
                                 phiImage[i - xDim - 1]) * 0.25;

                        double phix2 = phix * phix;
                        double phiy2 = phiy * phiy;

                        denom = phix2 + phiy2;

                        if (denom > 0) {
                            c3 = ((phixx * phiy2) - (2 * phiy * phix * phixy) + (phiyy * phix2)) / denom;
                            c3 *= epsilon * gBuffer[i];
                        } else {
                            c3 = 0.0;
                        }

                        if (Math.abs(c1 + c3) > c13max) {
                            c13max = Math.abs(c1 + c3);
                        }

                        if (Math.abs(c2) > c2max) {
                            c2max = Math.abs(c2);
                        }

                        possiblePhi = phiImage[i] + (deltaT * (c1 + c2 + c3));

                        if ((movement == EXPAND) && (possiblePhi >= 0.0) && (phiImage[i] < 0.0)) {
                            phiNext[i] = phiImage[i];
                        } else if ((movement == CONTRACT) && (possiblePhi < 0.0) && (phiImage[i] >= 0.0)) {
                            phiNext[i] = phiImage[i];
                        } else {
                            phiNext[i] = possiblePhi;
                        }

                        if (((movement == EXPAND) || (movement == EXPAND_CONTRACT)) && (phiNext[i] < 0.0) &&
                                (originalPhi[i] >= 4.0)) {
                            reinitializePhi = true;
                        } else if (checkIters2 == n) {

                            if ((phiImage[i] < 0.0) &&
                                    ((phiImage[i - 1] >= 0.0) || (phiImage[i + 1] >= 0.0) ||
                                         (phiImage[i - xDim] >= 0.0) || (phiImage[i + xDim] >= 0.0))) {
                                yPos = i / xDim;

                                if ((xPos != boundaryX[boundaryLength]) || (yPos != boundaryY[boundaryLength])) {
                                    haveChanged = true;
                                    checkIters = n + testIters;
                                }

                                boundaryX[boundaryLength] = xPos;
                                boundaryY[boundaryLength++] = yPos;
                            }
                        } // else if (checkIters2 == n)
                    } // if ((xPos >= 1) && (xPos < xDim - 1))
                } // if (active[i])
            } // for (i = xDim + 1; i < length-xDim-1 && !threadStopped; i++)

            beta = (float) (edgeAttract * c13max / c2max);
            tempBuffer = phiImage;
            phiImage = phiNext;
            phiNext = tempBuffer;

            if (reinitializePhi) {
                reinitializePhi = false;

                if (checkIters > 0) {
                    checkIters = n + testIters;
                }

                boundaryLength = 0;

                for (i = xDim + 1; (i < (length - xDim - 1)) && !threadStopped; i++) {

                    if (active[i]) {
                        xPos = i % xDim;

                        if ((xPos >= 1) && (xPos < (xDim - 1))) {

                            if ((phiImage[i] < 0.0) &&
                                    ((phiImage[i - 1] >= 0.0) || (phiImage[i + 1] >= 0.0) ||
                                         (phiImage[i - xDim] >= 0.0) || (phiImage[i + xDim] >= 0.0))) {
                                boundaryX[boundaryLength] = xPos;
                                boundaryY[boundaryLength++] = i / xDim;
                            }
                        }
                    }
                }

                for (i = 0; (i < length) && !threadStopped; i++) {
                    yPos = i / xDim;
                    xPos = i % xDim;
                    minDistance = Double.MAX_VALUE;

                    for (j = 0; j < boundaryLength; j++) {
                        distance = (((boundaryY[j] - yPos) * (boundaryY[j] - yPos)) +
                                    ((boundaryX[j] - xPos) * (boundaryX[j] - xPos)));

                        if (distance < minDistance) {
                            minDistance = distance;
                        }
                    }

                    if (phiImage[i] < 0) {
                        phiImage[i] = -Math.sqrt(minDistance);
                    } else {
                        phiImage[i] = Math.sqrt(minDistance);
                    }

                    phiNext[i] = phiImage[i];
                    originalPhi[i] = phiImage[i];

                    if ((phiImage[i] > 6.0) || (phiImage[i] < -6.0)) {
                        active[i] = false;
                    } else {
                        active[i] = true;
                    }
                }

                c13max = -Double.MAX_VALUE;
                c2max = -Double.MAX_VALUE;

                // On every iteration adjust beta to make |c2|max = edgeAttract*|c1+c3|max
                // so the attractive force predominates over the other 2 forces
                for (i = xDim + 1; (i < (length - xDim - 1)) && !threadStopped; i++) {

                    if (active[i]) {
                        xPos = i % xDim;

                        if ((xPos >= 1) && (xPos < (xDim - 1))) {
                            dMinusX = phiImage[i] - phiImage[i - 1];
                            dPlusX = phiImage[i + 1] - phiImage[i];
                            dMinusY = phiImage[i] - phiImage[i - xDim];
                            dPlusY = phiImage[i + xDim] - phiImage[i];

                            c1a = Math.max(dMinusX, 0);
                            c1b = Math.min(dPlusX, 0);
                            c1c = Math.max(dMinusY, 0);
                            c1d = Math.min(dPlusY, 0);
                            gradPlus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                            c1 = -gBuffer[i] * gradPlus;
                            c2 = -((Math.max(uComp[i], 0) * dMinusX) + (Math.min(uComp[i], 0) * dPlusX) +
                                   (Math.max(vComp[i], 0) * dMinusY) + (Math.min(vComp[i], 0) * dPlusY));

                            phix = (phiImage[i + 1] - phiImage[i - 1]) * 0.5;
                            phiy = (phiImage[i + xDim] - phiImage[i - xDim]) * 0.5;
                            phixx = phiImage[i + 1] - (2 * phiImage[i]) + phiImage[i - 1];
                            phiyy = phiImage[i + xDim] - (2 * phiImage[i]) + phiImage[i - xDim];
                            phixy = (phiImage[i + xDim + 1] - phiImage[i + xDim - 1] - phiImage[i - xDim + 1] +
                                     phiImage[i - xDim - 1]) * 0.25;

                            double phix2 = phix * phix;
                            double phiy2 = phiy * phiy;

                            denom = phix2 + phiy2;
                            denom = (phix * phix) + (phiy * phiy);

                            if (denom > 0) {
                                c3 = ((phixx * phiy2) - (2 * phiy * phix * phixy) + (phiyy * phix2)) / denom;
                                c3 *= epsilon * gBuffer[i];
                            } else {
                                c3 = 0.0;
                            }

                            if (Math.abs(c1 + c3) > c13max) {
                                c13max = Math.abs(c1 + c3);
                            }

                            if (Math.abs(c2) > c2max) {
                                c2max = Math.abs(c2);
                            }
                        }

                    }
                }

                beta = (float) (edgeAttract * c13max / c2max);

            } // if (reinitializePhi)
        } // for(n = 0; n < iterations && !threadStopped && haveChanged; n++)

        long now = System.currentTimeMillis();
        double elapsedTime = (double) (now - startTime);

        System.out.println("Algo levelset time = " + elapsedTime);

        phiNext = null;
        tempBuffer = null;
        System.gc();

        if (threadStopped) {
            
            setCompleted(false);
            finalize();

            return;
        }

        // delete the VOIs
        VOIs = srcImage.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {
            VOIs.removeElementAt(i);
        }

        voiID = 0;
        mask = new BitSet(length);

        // Note that must use phiImage[i] < 0.0 and not phiImage[i] <= 0.0
        // or the contour will expand even if phi does not change
        for (i = 0; i < length; i++) {

            if (phiImage[i] < 0.0) {
                mask.set(i);
            } else {
                mask.clear(i);
            }
        }

        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(srcImage, mask, xDim, yDim, zDim,
                                                                                     voiID);

        algoPaintToVOI.run();
        algoPaintToVOI = null;

        
        setCompleted(true);
    }
    
    /**
     * calc25D - Calculates level set from contours, propagates level set, and obtains new contour voi from level set.
     * Calculates one slice at a time in a multislice image
     */
    private void calc25D() {

        float beta; // controls attraction of contours to boundaries
        int i, j, n;
        int length;
        float[] gBuffer;
        float[] uComp;
        float[] vComp;
        double[] phiImage;
        double[] phiNext;
        double[] originalPhi;
        double[] tempBuffer;
        int nVOI;
        ViewVOIVector VOIs;
        VOI contourVOI;
        short voiID;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int xPos, yPos;
        double dMinusX, dPlusX, dMinusY, dPlusY;
        double gradMinus, gradPlus;
        double c1, c1a, c1b, c1c, c1d;
        double c2;
        double phix, phiy, phixx, phiyy, phixy, denom;
        double possiblePhi;
        double c3;
        double c13max, c2max;
        double distance, minDistance;
        boolean[] active;
        boolean reinitializePhi = false;
        int[] boundaryX;
        int[] boundaryY;
        int boundaryLength;
        int checkIters = testIters;
        int checkIters2 = testIters;
        boolean haveChanged = true;
        float minGrad;
        float maxGrad;
        float divisor;
        int z;
        Vector<VOIBase>[] curves;
        int extents2D[];
        int totalLength;
        int offset;

        try {
            length = srcImage.getSliceSize();
            gBuffer = new float[length];
            uComp = new float[length];
            vComp = new float[length];
            phiImage = new double[length];
            phiNext = new double[length];
            originalPhi = new double[length];
            active = new boolean[length];
            boundaryX = new int[length];
            boundaryY = new int[length];
            fireProgressStateChanged(srcImage.getImageName(), "Evolving the level set ...");
        } catch (OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("Level set: Out of Memory");
            setCompleted(false);

            return;
        }
        
        totalLength = length * zDim;

        VOIs = srcImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }

        contourVOI = VOIs.VOIAt(i);
        curves = contourVOI.getSortedCurves( VOIBase.ZPLANE, zDim );
        

        int[] imageExtents = srcImage.getExtents();
        extents2D = new int[2];
        extents2D[0] = imageExtents[0];
        extents2D[1] = imageExtents[1];
        
        mask = new BitSet(totalLength);
        
        long startTime = System.currentTimeMillis();
        
        for (z = 0; z < zDim; z++) {
            fireProgressStateChanged(z * 100/ zDim);
            offset = z*length;
            if (curves[z].size() == 0) {
                // If  no curves are present in this slice, don't process this slice
                continue;
            }
            
            maxGrad = 0.0f;
            minGrad = Float.MAX_VALUE;

            for (i = 0; i < length; i++) { // calculate gradient magnitude

                if (outputBuffer[offset + i] > maxGrad) {
                    maxGrad = outputBuffer[offset + i];
                }

                if (outputBuffer[offset + i] < minGrad) {
                    minGrad = outputBuffer[offset + i];
                }

                // Normalize the gradient magnitude to go from 0 to 100
            }

            if (maxGrad > minGrad) {
                divisor = maxGrad - minGrad;
            } else {
                divisor = 1.0f;
            }

            for (i = 0; i < length; i++) {
                outputBuffer[offset + i] = (outputBuffer[offset + i] - minGrad) * 100.0f / divisor;
            }

            for (i = 0; i < length; i++) {
                gBuffer[i] = 1 / (1 + outputBuffer[offset + i]);
            }
    
            
    
            // Adjust beta after the first trial run thru.
            beta = 1.0f;
    
            for (i = xDim + 1; i < (length - xDim - 1); i++) {
                xPos = i % xDim;
    
                if ((xPos >= 1) && (xPos < (xDim - 1))) {
                    uComp[i] = beta * (outputBuffer[offset + i + 1] - outputBuffer[offset + i - 1]) / 2.0f;
                    vComp[i] = beta * (outputBuffer[offset + i + xDim] - outputBuffer[offset + i - xDim]) / 2.0f;
                }
            }
    
    
            for (i = 0; (i < length) && !threadStopped; i++) {
                yPos = i / xDim;
                xPos = i % xDim;
                phiImage[i] = contourVOI.pointToContour(xPos, yPos, z);
                phiNext[i] = phiImage[i];
                originalPhi[i] = phiImage[i];
            }
    
            // For contraction runs simply only process on those pixels for which
            // phi is initially 6 or less
            // For expansion runs initially only process on those pixels for which
            // -6 <= phi <= 6.  When the contour expands to where the phi was
            // originally >= 4.0, then find the boundary and use this to create a
            // new contour.
            // Note that page 85 of Level Set Methods and Fast Marching Methods
            // states: "Our experience indicates that a narrow band width of about
            // six grid points on either side of the zero level set is a reasonable
            // balance between re-initialization costs and update costs."
            for (i = 0; i < length; i++) {
                active[i] = true;
            }
    
            if (movement == CONTRACT) {
    
                for (i = 0; i < length; i++) {
    
                    if (phiImage[i] > 6.0) {
                        active[i] = false;
                    }
                }
            } else { // EXPAND or EXPAND_CONTRACT
    
                for (i = 0; i < length; i++) {
    
                    if ((phiImage[i] > 6.0) || (phiImage[i] < -6.0)) {
                        active[i] = false;
                    }
                }
            }
    
            c13max = -Double.MAX_VALUE;
            c2max = -Double.MAX_VALUE;
    
            // On every iteration adjust beta to make |c2|max = edgeAttract*|c1+c3|max
            // so the attractive force predominates over the other 2 forces
            // Let beta = 1.0 for the first run of this equation.
            for (i = xDim + 1; (i < (length - xDim - 1)) && !threadStopped; i++) {
    
                if (active[i]) {
                    xPos = i % xDim;
    
                    if ((xPos >= 1) && (xPos < (xDim - 1))) {
                        dMinusX = phiImage[i] - phiImage[i - 1];
                        dPlusX = phiImage[i + 1] - phiImage[i];
                        dMinusY = phiImage[i] - phiImage[i - xDim];
                        dPlusY = phiImage[i + xDim] - phiImage[i];
    
                        if ((movement == EXPAND) || (movement == EXPAND_CONTRACT)) {
                            c1a = Math.max(dMinusX, 0);
                            c1b = Math.min(dPlusX, 0);
                            c1c = Math.max(dMinusY, 0);
                            c1d = Math.min(dPlusY, 0);
                            gradPlus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                            c1 = -gBuffer[i] * gradPlus;
                        } else { // CONTRACT
                            c1a = Math.max(dPlusX, 0);
                            c1b = Math.min(dMinusX, 0);
                            c1c = Math.max(dPlusY, 0);
                            c1d = Math.min(dMinusY, 0);
                            gradMinus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                            c1 = gBuffer[i] * gradMinus;
                        }
    
                        c2 = -((Math.max(uComp[i], 0) * dMinusX) + (Math.min(uComp[i], 0) * dPlusX) +
                               (Math.max(vComp[i], 0) * dMinusY) + (Math.min(vComp[i], 0) * dPlusY));
    
                        phix = (phiImage[i + 1] - phiImage[i - 1]) * 0.5;
                        phiy = (phiImage[i + xDim] - phiImage[i - xDim]) * 0.5;
                        phixx = phiImage[i + 1] - (2 * phiImage[i]) + phiImage[i - 1];
                        phiyy = phiImage[i + xDim] - (2 * phiImage[i]) + phiImage[i - xDim];
                        phixy = (phiImage[i + xDim + 1] - phiImage[i + xDim - 1] - phiImage[i - xDim + 1] +
                                 phiImage[i - xDim - 1]) * 0.25;
    
                        double phix2 = phix * phix;
                        double phiy2 = phiy * phiy;
    
                        denom = phix2 + phiy2;
    
                        if (denom > 0) {
                            c3 = ((phixx * phiy2) - (2 * phiy * phix * phixy) + (phiyy * phix2)) / denom;
                            c3 *= epsilon * gBuffer[i];
                        } else {
                            c3 = 0.0;
                        }
    
                        if (Math.abs(c1 + c3) > c13max) {
                            c13max = Math.abs(c1 + c3);
                        }
    
                        if (Math.abs(c2) > c2max) {
                            c2max = Math.abs(c2);
                        }
                    }
    
                }
            }
    
            beta = (float) (edgeAttract * c13max / c2max);
    
            for (n = 0; (n < iterations) && !threadStopped && haveChanged; n++) {
    
                c13max = -Double.MAX_VALUE;
                c2max = -Double.MAX_VALUE;
                boundaryLength = 0;
    
                if (checkIters == n) {
                    haveChanged = false;
                    checkIters2 = n;
                }
    
                for (i = xDim + 1; (i < (length - xDim - 1)) && !threadStopped; i++) {
    
                    if (active[i]) {
                        xPos = i % xDim;
    
                        if ((xPos >= 1) && (xPos < (xDim - 1))) {
                            dMinusX = phiImage[i] - phiImage[i - 1];
                            dPlusX = phiImage[i + 1] - phiImage[i];
                            dMinusY = phiImage[i] - phiImage[i - xDim];
                            dPlusY = phiImage[i + xDim] - phiImage[i];
    
                            if ((movement == EXPAND) || (movement == EXPAND_CONTRACT)) {
                                c1a = Math.max(dMinusX, 0);
                                c1b = Math.min(dPlusX, 0);
                                c1c = Math.max(dMinusY, 0);
                                c1d = Math.min(dPlusY, 0);
                                gradPlus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                                c1 = -gBuffer[i] * gradPlus;
                            } else { // CONTRACT
                                c1a = Math.max(dPlusX, 0);
                                c1b = Math.min(dMinusX, 0);
                                c1c = Math.max(dPlusY, 0);
                                c1d = Math.min(dMinusY, 0);
                                gradMinus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                                c1 = gBuffer[i] * gradMinus;
                            }
    
                            c2 = -((Math.max(beta * uComp[i], 0) * dMinusX) + (Math.min(beta * uComp[i], 0) * dPlusX) +
                                   (Math.max(beta * vComp[i], 0) * dMinusY) + (Math.min(beta * vComp[i], 0) * dPlusY));
    
                            phix = (phiImage[i + 1] - phiImage[i - 1]) * 0.5;
                            phiy = (phiImage[i + xDim] - phiImage[i - xDim]) * 0.5;
                            phixx = phiImage[i + 1] - (2 * phiImage[i]) + phiImage[i - 1];
                            phiyy = phiImage[i + xDim] - (2 * phiImage[i]) + phiImage[i - xDim];
                            phixy = (phiImage[i + xDim + 1] - phiImage[i + xDim - 1] - phiImage[i - xDim + 1] +
                                     phiImage[i - xDim - 1]) * 0.25;
    
                            double phix2 = phix * phix;
                            double phiy2 = phiy * phiy;
    
                            denom = phix2 + phiy2;
    
                            if (denom > 0) {
                                c3 = ((phixx * phiy2) - (2 * phiy * phix * phixy) + (phiyy * phix2)) / denom;
                                c3 *= epsilon * gBuffer[i];
                            } else {
                                c3 = 0.0;
                            }
    
                            if (Math.abs(c1 + c3) > c13max) {
                                c13max = Math.abs(c1 + c3);
                            }
    
                            if (Math.abs(c2) > c2max) {
                                c2max = Math.abs(c2);
                            }
    
                            possiblePhi = phiImage[i] + (deltaT * (c1 + c2 + c3));
    
                            if ((movement == EXPAND) && (possiblePhi >= 0.0) && (phiImage[i] < 0.0)) {
                                phiNext[i] = phiImage[i];
                            } else if ((movement == CONTRACT) && (possiblePhi < 0.0) && (phiImage[i] >= 0.0)) {
                                phiNext[i] = phiImage[i];
                            } else {
                                phiNext[i] = possiblePhi;
                            }
    
                            if (((movement == EXPAND) || (movement == EXPAND_CONTRACT)) && (phiNext[i] < 0.0) &&
                                    (originalPhi[i] >= 4.0)) {
                                reinitializePhi = true;
                            } else if (checkIters2 == n) {
    
                                if ((phiImage[i] < 0.0) &&
                                        ((phiImage[i - 1] >= 0.0) || (phiImage[i + 1] >= 0.0) ||
                                             (phiImage[i - xDim] >= 0.0) || (phiImage[i + xDim] >= 0.0))) {
                                    yPos = i / xDim;
    
                                    if ((xPos != boundaryX[boundaryLength]) || (yPos != boundaryY[boundaryLength])) {
                                        haveChanged = true;
                                        checkIters = n + testIters;
                                    }
    
                                    boundaryX[boundaryLength] = xPos;
                                    boundaryY[boundaryLength++] = yPos;
                                }
                            } // else if (checkIters2 == n)
                        } // if ((xPos >= 1) && (xPos < xDim - 1))
                    } // if (active[i])
                } // for (i = xDim + 1; i < length-xDim-1 && !threadStopped; i++)
    
                beta = (float) (edgeAttract * c13max / c2max);
                tempBuffer = phiImage;
                phiImage = phiNext;
                phiNext = tempBuffer;
    
                if (reinitializePhi) {
                    reinitializePhi = false;
    
                    if (checkIters > 0) {
                        checkIters = n + testIters;
                    }
    
                    boundaryLength = 0;
    
                    for (i = xDim + 1; (i < (length - xDim - 1)) && !threadStopped; i++) {
    
                        if (active[i]) {
                            xPos = i % xDim;
    
                            if ((xPos >= 1) && (xPos < (xDim - 1))) {
    
                                if ((phiImage[i] < 0.0) &&
                                        ((phiImage[i - 1] >= 0.0) || (phiImage[i + 1] >= 0.0) ||
                                             (phiImage[i - xDim] >= 0.0) || (phiImage[i + xDim] >= 0.0))) {
                                    boundaryX[boundaryLength] = xPos;
                                    boundaryY[boundaryLength++] = i / xDim;
                                }
                            }
                        }
                    }
    
                    for (i = 0; (i < length) && !threadStopped; i++) {
                        yPos = i / xDim;
                        xPos = i % xDim;
                        minDistance = Double.MAX_VALUE;
    
                        for (j = 0; j < boundaryLength; j++) {
                            distance = (((boundaryY[j] - yPos) * (boundaryY[j] - yPos)) +
                                        ((boundaryX[j] - xPos) * (boundaryX[j] - xPos)));
    
                            if (distance < minDistance) {
                                minDistance = distance;
                            }
                        }
    
                        if (phiImage[i] < 0) {
                            phiImage[i] = -Math.sqrt(minDistance);
                        } else {
                            phiImage[i] = Math.sqrt(minDistance);
                        }
    
                        phiNext[i] = phiImage[i];
                        originalPhi[i] = phiImage[i];
    
                        if ((phiImage[i] > 6.0) || (phiImage[i] < -6.0)) {
                            active[i] = false;
                        } else {
                            active[i] = true;
                        }
                    }
    
                    c13max = -Double.MAX_VALUE;
                    c2max = -Double.MAX_VALUE;
    
                    // On every iteration adjust beta to make |c2|max = edgeAttract*|c1+c3|max
                    // so the attractive force predominates over the other 2 forces
                    for (i = xDim + 1; (i < (length - xDim - 1)) && !threadStopped; i++) {
    
                        if (active[i]) {
                            xPos = i % xDim;
    
                            if ((xPos >= 1) && (xPos < (xDim - 1))) {
                                dMinusX = phiImage[i] - phiImage[i - 1];
                                dPlusX = phiImage[i + 1] - phiImage[i];
                                dMinusY = phiImage[i] - phiImage[i - xDim];
                                dPlusY = phiImage[i + xDim] - phiImage[i];
    
                                c1a = Math.max(dMinusX, 0);
                                c1b = Math.min(dPlusX, 0);
                                c1c = Math.max(dMinusY, 0);
                                c1d = Math.min(dPlusY, 0);
                                gradPlus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d));
                                c1 = -gBuffer[i] * gradPlus;
                                c2 = -((Math.max(uComp[i], 0) * dMinusX) + (Math.min(uComp[i], 0) * dPlusX) +
                                       (Math.max(vComp[i], 0) * dMinusY) + (Math.min(vComp[i], 0) * dPlusY));
    
                                phix = (phiImage[i + 1] - phiImage[i - 1]) * 0.5;
                                phiy = (phiImage[i + xDim] - phiImage[i - xDim]) * 0.5;
                                phixx = phiImage[i + 1] - (2 * phiImage[i]) + phiImage[i - 1];
                                phiyy = phiImage[i + xDim] - (2 * phiImage[i]) + phiImage[i - xDim];
                                phixy = (phiImage[i + xDim + 1] - phiImage[i + xDim - 1] - phiImage[i - xDim + 1] +
                                         phiImage[i - xDim - 1]) * 0.25;
    
                                double phix2 = phix * phix;
                                double phiy2 = phiy * phiy;
    
                                denom = phix2 + phiy2;
                                denom = (phix * phix) + (phiy * phiy);
    
                                if (denom > 0) {
                                    c3 = ((phixx * phiy2) - (2 * phiy * phix * phixy) + (phiyy * phix2)) / denom;
                                    c3 *= epsilon * gBuffer[i];
                                } else {
                                    c3 = 0.0;
                                }
    
                                if (Math.abs(c1 + c3) > c13max) {
                                    c13max = Math.abs(c1 + c3);
                                }
    
                                if (Math.abs(c2) > c2max) {
                                    c2max = Math.abs(c2);
                                }
                            }
    
                        }
                    }
    
                    beta = (float) (edgeAttract * c13max / c2max);
    
                } // if (reinitializePhi)
            } // for(n = 0; n < iterations && !threadStopped && haveChanged; n++)
            
            // Note that must use phiImage[i] < 0.0 and not phiImage[i] <= 0.0
            // or the contour will expand even if phi does not change
            for (i = 0; i < length; i++) {

                if (phiImage[i] < 0.0) {
                    mask.set(offset+i);
                } else {
                    mask.clear(offset+i);
                }
            }
    
            if (threadStopped) {
                
                setCompleted(false);
                finalize();
    
                return;
            }
        
        } // for (z = 0; z < zDim; z++)
        
        // delete the VOIs
        VOIs = srcImage.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {
            VOIs.removeElementAt(i);
        }

        voiID = 0;

        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(srcImage, mask, xDim, yDim, zDim,
                                                                                     voiID);

        algoPaintToVOI.run();
        algoPaintToVOI = null;
        
        long now = System.currentTimeMillis();
        double elapsedTime = (double) (now - startTime);

        System.out.println("Algo levelset time = " + elapsedTime);

        
        setCompleted(true);
    }


    /**
     * calc3D - Calculates level set from contours, propagates level set, and obtains new contour voi from level set.
     */
    private void calc3D() {
        float beta; // controls attraction of contours to boundaries
        int i, j, n;
        int length;
        float[] gBuffer;
        float[] uComp;
        float[] vComp;
        float[] wComp;
        double[] phiImage;
        double[] phiNext;
        double[] originalPhi;
        double[] tempBuffer;
        int nVOI;
        ViewVOIVector VOIs;
        VOI contourVOI;
        short voiID;
        int xDim = srcImage.getExtents()[0];
        int yDim = srcImage.getExtents()[1];
        int zDim = srcImage.getExtents()[2];
        int sliceSize = xDim * yDim;
        int xPos, yPos, zPos;
        double dMinusX, dPlusX, dMinusY, dPlusY, dMinusZ, dPlusZ;
        double gradMinus, gradPlus;
        double c1, c1a, c1b, c1c, c1d, c1e, c1f;
        double c2;
        double phix, phiy, phixx, phiyy, phixy, denom;
        double phiz, phizz, phixz, phiyz;
        double possiblePhi;
        double c3;
        double c2max, c13max;
        boolean[] active;
        boolean reinitializePhi = false;
        int[] boundaryX;
        int[] boundaryY;
        int[] boundaryZ;
        int boundaryLength;
        double distance, minDistance;
        int checkIters = testIters;
        int checkIters2 = testIters;
        boolean haveChanged = true;
        float minGrad;
        float maxGrad;
        float divisor;

        try {
            length = srcImage.getSliceSize() * srcImage.getExtents()[2];
            gBuffer = new float[length];
            uComp = new float[length];
            vComp = new float[length];
            wComp = new float[length];
            phiImage = new double[length];
            phiNext = new double[length];
            originalPhi = new double[length];
            active = new boolean[length];
            boundaryX = new int[length];
            boundaryY = new int[length];
            boundaryZ = new int[length];
            fireProgressStateChanged(srcImage.getImageName(), "Evolving the level set ...");
        } catch (OutOfMemoryError e) {
            cleanUp();
            System.gc();
            displayError("Level set: Out of Memory");
            setCompleted(false);

            return;
        }

        VOIs = srcImage.getVOIs();
        nVOI = VOIs.size();

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                break;
            }
        }

        contourVOI = VOIs.VOIAt(i);

        maxGrad = 0.0f;
        minGrad = Float.MAX_VALUE;

        for (i = 0; i < length; i++) { // calculate gradient magnitude

            if (outputBuffer[i] > maxGrad) {
                maxGrad = outputBuffer[i];
            }

            if (outputBuffer[i] < minGrad) {
                minGrad = outputBuffer[i];
            }
        }

        // Normalize the gradient magnitude to go from 0 to 100
        if (maxGrad > minGrad) {
            divisor = maxGrad - minGrad;
        } else {
            divisor = 1.0f;
        }

        for (i = 0; i < length; i++) {
            outputBuffer[i] = (outputBuffer[i] - minGrad) * 100.0f / divisor;
        }

        for (i = 0; i < length; i++) {
            gBuffer[i] = 1 / (1 + outputBuffer[i]);
        }
        
        fireProgressStateChanged(2);

        // Adjust beta after the first trial run thru.
        beta = 1.0f;

        for (i = sliceSize + xDim + 1; i < (length - sliceSize - xDim - 1); i++) {
            xPos = i % xDim;
            yPos = (i % sliceSize) / xDim;

            if ((xPos >= 1) && (xPos < (xDim - 1)) && (yPos >= 1) && (yPos < (yDim - 1))) {
                uComp[i] = beta * (outputBuffer[i + 1] - outputBuffer[i - 1]) * 0.5f;
                vComp[i] = beta * (outputBuffer[i + xDim] - outputBuffer[i - xDim]) * 0.5f;
                wComp[i] = beta * (outputBuffer[i + sliceSize] - outputBuffer[i - sliceSize]) * 0.5f;
            }
        }

        outputBuffer = null;
        System.gc();

        fireProgressStateChanged(4);

        for (i = 0; (i < length) && !threadStopped; i++) {
            zPos = i / sliceSize;
            yPos = (i % sliceSize) / xDim;
            xPos = i % xDim;
            phiImage[i] = contourVOI.pointToContour(xPos, yPos, zPos);
            phiNext[i] = phiImage[i];
            originalPhi[i] = phiImage[i];
        }

        // For contraction runs simply only process on those pixels for which
        // phi is initially 6 or less
        // For expansion runs initially only process on those pixels for which
        // -6 <= phi <= 6.  When the contour expands to where the phi was
        // originally >= 4.0, then find the boundary and use this to create a
        // new contour.
        // Note that page 85 of Level Set Methods and Fast Marching Methods
        // states: "Our experience indicates that a narrow band width of about
        // six grid points on either side of the zero level set is a reasonable
        // balance between re-initialization costs and update costs."
        for (i = 0; i < length; i++) {
            active[i] = true;
        }

        if (movement == CONTRACT) {

            for (i = 0; i < length; i++) {

                if (phiImage[i] > 6.0) {
                    active[i] = false;
                }
            }
        } else { // EXPAND or EXPAND_CONTRACT

            for (i = 0; i < length; i++) {

                if ((phiImage[i] > 6.0) || (phiImage[i] < -6.0)) {
                    active[i] = false;
                }
            }
        }
        
        fireProgressStateChanged(6);

        c13max = -Double.MAX_VALUE;
        c2max = -Double.MAX_VALUE;

        // On every iteration adjust beta to make |c2|max = edgeAttract*|c1+c3|max
        // so the attractive force predominates over the other 2 forces
        // Let beta = 1.0 for the first run of this equation.
        for (i = sliceSize + xDim + 1; (i < (length - sliceSize - xDim - 1)) && !threadStopped; i++) {

            if (active[i]) {
                yPos = (i % sliceSize) / xDim;
                xPos = i % xDim;

                if ((xPos >= 1) && (xPos < (xDim - 1)) && (yPos >= 1) && (yPos < (yDim - 1))) {
                    dMinusX = phiImage[i] - phiImage[i - 1];
                    dPlusX = phiImage[i + 1] - phiImage[i];
                    dMinusY = phiImage[i] - phiImage[i - xDim];
                    dPlusY = phiImage[i + xDim] - phiImage[i];
                    dMinusZ = phiImage[i] - phiImage[i - sliceSize];
                    dPlusZ = phiImage[i + sliceSize] - phiImage[i];

                    if ((movement == EXPAND) || (movement == EXPAND_CONTRACT)) {
                        c1a = Math.max(dMinusX, 0);
                        c1b = Math.min(dPlusX, 0);
                        c1c = Math.max(dMinusY, 0);
                        c1d = Math.min(dPlusY, 0);
                        c1e = Math.max(dMinusZ, 0);
                        c1f = Math.min(dPlusZ, 0);
                        gradPlus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d) + (c1e * c1e) +
                                             (c1f * c1f));
                        c1 = -gBuffer[i] * gradPlus;
                    } else { // CONTRACT
                        c1a = Math.max(dPlusX, 0);
                        c1b = Math.min(dMinusX, 0);
                        c1c = Math.max(dPlusY, 0);
                        c1d = Math.min(dMinusY, 0);
                        c1e = Math.max(dPlusZ, 0);
                        c1f = Math.min(dMinusZ, 0);
                        gradMinus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d) + (c1e * c1e) +
                                              (c1f * c1f));
                        c1 = gBuffer[i] * gradMinus;
                    }

                    c2 = -((Math.max(uComp[i], 0) * dMinusX) + (Math.min(uComp[i], 0) * dPlusX) +
                           (Math.max(vComp[i], 0) * dMinusY) + (Math.min(vComp[i], 0) * dPlusY) +
                           (Math.max(wComp[i], 0) * dMinusZ) + (Math.min(wComp[i], 0) * dPlusZ));
                    phix = (phiImage[i + 1] - phiImage[i - 1]) * 0.5;
                    phiy = (phiImage[i + xDim] - phiImage[i - xDim]) * 0.5;
                    phiz = (phiImage[i + sliceSize] - phiImage[i - sliceSize]) * 0.5;
                    phixx = phiImage[i + 1] - (2 * phiImage[i]) + phiImage[i - 1];
                    phiyy = phiImage[i + xDim] - (2 * phiImage[i]) + phiImage[i - xDim];
                    phizz = phiImage[i + sliceSize] - (2 * phiImage[i]) + phiImage[i - sliceSize];
                    phixy = (phiImage[i + xDim + 1] - phiImage[i + xDim - 1] - phiImage[i - xDim + 1] +
                             phiImage[i - xDim - 1]) * 0.25;
                    phixz = (phiImage[i + sliceSize + 1] - phiImage[i + sliceSize - 1] - phiImage[i - sliceSize + 1] +
                             phiImage[i - sliceSize - 1]) * 0.25;
                    phiyz = (phiImage[i + sliceSize + xDim] - phiImage[i + sliceSize - xDim] -
                             phiImage[i - sliceSize + xDim] + phiImage[i - sliceSize - xDim]) * 0.25;
                    denom = (phix * phix) + (phiy * phiy) + (phiz * phiz);

                    if (denom > 0) {
                        c3 = (((phixx + phizz) * phiy * phiy) - (2 * phix * phiz * phixz) +
                              ((phiyy + phizz) * phix * phix) - (2 * phiy * phiz * phiyz) +
                              ((phixx + phiyy) * phiz * phiz) - (2 * phix * phiy * phixy)) / denom;
                        c3 *= epsilon * gBuffer[i];
                    } else {
                        c3 = 0.0;
                    }

                    if (Math.abs(c1 + c3) > c13max) {
                        c13max = Math.abs(c1 + c3);
                    }

                    if (Math.abs(c2) > c2max) {
                        c2max = Math.abs(c2);
                    }
                }
            }
        }

        beta = (float) (edgeAttract * c13max / c2max);

        for (n = 0; (n < iterations) && !threadStopped && haveChanged; n++) {
            fireProgressStateChanged(10 + Math.round((float) n / (iterations - 1) * 90));

            c13max = -Double.MAX_VALUE;
            c2max = -Double.MAX_VALUE;
            boundaryLength = 0;

            if (checkIters == n) {
                haveChanged = false;
                checkIters2 = n;
            }

            for (i = sliceSize + xDim + 1; (i < (length - sliceSize - xDim - 1)) && !threadStopped; i++) {

                if (active[i]) {
                    yPos = (i % sliceSize) / xDim;
                    xPos = i % xDim;

                    if ((xPos >= 1) && (xPos < (xDim - 1)) && (yPos >= 1) && (yPos < (yDim - 1))) {
                        dMinusX = phiImage[i] - phiImage[i - 1];
                        dPlusX = phiImage[i + 1] - phiImage[i];
                        dMinusY = phiImage[i] - phiImage[i - xDim];
                        dPlusY = phiImage[i + xDim] - phiImage[i];
                        dMinusZ = phiImage[i] - phiImage[i - sliceSize];
                        dPlusZ = phiImage[i + sliceSize] - phiImage[i];

                        if ((movement == EXPAND) || (movement == EXPAND_CONTRACT)) {
                            c1a = Math.max(dMinusX, 0);
                            c1b = Math.min(dPlusX, 0);
                            c1c = Math.max(dMinusY, 0);
                            c1d = Math.min(dPlusY, 0);
                            c1e = Math.max(dMinusZ, 0);
                            c1f = Math.min(dPlusZ, 0);
                            gradPlus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d) + (c1e * c1e) +
                                                 (c1f * c1f));
                            c1 = -gBuffer[i] * gradPlus;
                        } else { // CONTRACT
                            c1a = Math.max(dPlusX, 0);
                            c1b = Math.min(dMinusX, 0);
                            c1c = Math.max(dPlusY, 0);
                            c1d = Math.min(dMinusY, 0);
                            c1e = Math.max(dPlusZ, 0);
                            c1f = Math.min(dMinusZ, 0);
                            gradMinus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d) + (c1e * c1e) +
                                                  (c1f * c1f));
                            c1 = gBuffer[i] * gradMinus;
                        }

                        c2 = -((Math.max(beta * uComp[i], 0) * dMinusX) + (Math.min(beta * uComp[i], 0) * dPlusX) +
                               (Math.max(beta * vComp[i], 0) * dMinusY) + (Math.min(beta * vComp[i], 0) * dPlusY) +
                               (Math.max(beta * wComp[i], 0) * dMinusZ) + (Math.min(beta * wComp[i], 0) * dPlusZ));
                        phix = (phiImage[i + 1] - phiImage[i - 1]) * 0.5;
                        phiy = (phiImage[i + xDim] - phiImage[i - xDim]) * 0.5;
                        phiz = (phiImage[i + sliceSize] - phiImage[i - sliceSize]) * 0.5;
                        phixx = phiImage[i + 1] - (2 * phiImage[i]) + phiImage[i - 1];
                        phiyy = phiImage[i + xDim] - (2 * phiImage[i]) + phiImage[i - xDim];
                        phizz = phiImage[i + sliceSize] - (2 * phiImage[i]) + phiImage[i - sliceSize];
                        phixy = (phiImage[i + xDim + 1] - phiImage[i + xDim - 1] - phiImage[i - xDim + 1] +
                                 phiImage[i - xDim - 1]) * 0.25;
                        phixz = (phiImage[i + sliceSize + 1] - phiImage[i + sliceSize - 1] -
                                 phiImage[i - sliceSize + 1] + phiImage[i - sliceSize - 1]) * 0.25;
                        phiyz = (phiImage[i + sliceSize + xDim] - phiImage[i + sliceSize - xDim] -
                                 phiImage[i - sliceSize + xDim] + phiImage[i - sliceSize - xDim]) * 0.25;
                        denom = (phix * phix) + (phiy * phiy) + (phiz * phiz);

                        if (denom > 0) {
                            c3 = (((phixx + phizz) * phiy * phiy) - (2 * phix * phiz * phixz) +
                                  ((phiyy + phizz) * phix * phix) - (2 * phiy * phiz * phiyz) +
                                  ((phixx + phiyy) * phiz * phiz) - (2 * phix * phiy * phixy)) / denom;
                            c3 *= epsilon * gBuffer[i];
                        } else {
                            c3 = 0.0;
                        }

                        if (Math.abs(c1 + c3) > c13max) {
                            c13max = Math.abs(c1 + c3);
                        }

                        if (Math.abs(c2) > c2max) {
                            c2max = Math.abs(c2);
                        }

                        possiblePhi = phiImage[i] + (deltaT * (c1 + c2 + c3));

                        if ((movement == EXPAND) && (possiblePhi >= 0.0) && (phiImage[i] < 0.0)) {
                            phiNext[i] = phiImage[i];
                        } else if ((movement == CONTRACT) && (possiblePhi < 0.0) && (phiImage[i] >= 0.0)) {
                            phiNext[i] = phiImage[i];
                        } else {
                            phiNext[i] = possiblePhi;
                        }

                        if (((movement == EXPAND) || (movement == EXPAND_CONTRACT)) && (phiNext[i] < 0.0) &&
                                (originalPhi[i] >= 4.0)) {
                            reinitializePhi = true;
                        } else if (checkIters2 == n) {

                            if ((phiImage[i] < 0.0) &&
                                    ((phiImage[i - 1] >= 0.0) || (phiImage[i + 1] >= 0.0) ||
                                         (phiImage[i - xDim] >= 0.0) || (phiImage[i + xDim] >= 0.0) ||
                                         (phiImage[i - sliceSize] >= 0.0) || (phiImage[i + sliceSize] >= 0.0))) {
                                zPos = i / sliceSize;

                                if ((xPos != boundaryX[boundaryLength]) || (yPos != boundaryY[boundaryLength]) ||
                                        (zPos != boundaryZ[boundaryLength])) {
                                    haveChanged = true;
                                    checkIters = n + testIters;
                                }

                                boundaryX[boundaryLength] = xPos;
                                boundaryY[boundaryLength] = yPos;
                                boundaryZ[boundaryLength++] = zPos;
                            }
                        } // else if (checkIters2 == n)
                    } // if ((xPos >= 1) && (xPos < xDim - 1) &&
                } // if (active[i])
            } // for (i = sliceSize+xDim+1; i < length-sliceSize-xDim-1 && !threadStopped; i++)

            beta = (float) (edgeAttract * c13max / c2max);
            tempBuffer = phiImage;
            phiImage = phiNext;
            phiNext = tempBuffer;

            if (reinitializePhi) {
                reinitializePhi = false;

                if (checkIters > 0) {
                    checkIters = n + testIters;
                }

                boundaryLength = 0;

                for (i = sliceSize + xDim + 1; (i < (length - sliceSize - xDim - 1)) && !threadStopped; i++) {

                    if (active[i]) {
                        yPos = (i % sliceSize) / xDim;
                        xPos = i % xDim;

                        if ((xPos >= 1) && (xPos < (xDim - 1)) && (yPos >= 1) && (yPos < (yDim - 1))) {

                            if ((phiImage[i] < 0.0) &&
                                    ((phiImage[i - 1] >= 0.0) || (phiImage[i + 1] >= 0.0) ||
                                         (phiImage[i - xDim] >= 0.0) || (phiImage[i + xDim] >= 0.0) ||
                                         (phiImage[i - sliceSize] >= 0.0) || (phiImage[i + sliceSize] >= 0.0))) {
                                boundaryX[boundaryLength] = xPos;
                                boundaryY[boundaryLength] = yPos;
                                boundaryZ[boundaryLength++] = i / sliceSize;
                            }
                        }
                    }
                }

                for (i = 0; (i < length) && !threadStopped; i++) {
                    zPos = i / sliceSize;
                    yPos = (i % sliceSize) / xDim;
                    xPos = i % xDim;
                    minDistance = Double.MAX_VALUE;

                    for (j = 0; j < boundaryLength; j++) {
                        distance = (((boundaryZ[j] - zPos) * (boundaryZ[j] - zPos)) +
                                    ((boundaryY[j] - yPos) * (boundaryY[j] - yPos)) +
                                    ((boundaryX[j] - xPos) * (boundaryX[j] - xPos)));

                        if (distance < minDistance) {
                            minDistance = distance;
                        }
                    }

                    if (phiImage[i] < 0) {
                        phiImage[i] = -Math.sqrt(minDistance);
                    } else {
                        phiImage[i] = Math.sqrt(minDistance);
                    }

                    phiNext[i] = phiImage[i];
                    originalPhi[i] = phiImage[i];

                    if ((phiImage[i] > 6.0) || (phiImage[i] < -6.0)) {
                        active[i] = false;
                    } else {
                        active[i] = true;
                    }
                }

                c13max = -Double.MAX_VALUE;
                c2max = -Double.MAX_VALUE;

                // On every iteration adjust beta to make |c2|max = edgeAttract*|c1+c3|max
                // so the attractive force predominates over the other 2 forces
                for (i = sliceSize + xDim + 1; (i < (length - sliceSize - xDim - 1)) && !threadStopped; i++) {

                    if (active[i]) {
                        yPos = (i % sliceSize) / xDim;
                        xPos = i % xDim;

                        if ((xPos >= 1) && (xPos < (xDim - 1)) && (yPos >= 1) && (yPos < (yDim - 1))) {
                            dMinusX = phiImage[i] - phiImage[i - 1];
                            dPlusX = phiImage[i + 1] - phiImage[i];
                            dMinusY = phiImage[i] - phiImage[i - xDim];
                            dPlusY = phiImage[i + xDim] - phiImage[i];
                            dMinusZ = phiImage[i] - phiImage[i - sliceSize];
                            dPlusZ = phiImage[i + sliceSize] - phiImage[i];
                            c1a = Math.max(dMinusX, 0);
                            c1b = Math.min(dPlusX, 0);
                            c1c = Math.max(dMinusY, 0);
                            c1d = Math.min(dPlusY, 0);
                            c1e = Math.max(dMinusZ, 0);
                            c1f = Math.min(dPlusZ, 0);
                            gradPlus = Math.sqrt((c1a * c1a) + (c1b * c1b) + (c1c * c1c) + (c1d * c1d) + (c1e * c1e) +
                                                 (c1f * c1f));
                            c1 = -gBuffer[i] * gradPlus;

                            c2 = -((Math.max(uComp[i], 0) * dMinusX) + (Math.min(uComp[i], 0) * dPlusX) +
                                   (Math.max(vComp[i], 0) * dMinusY) + (Math.min(vComp[i], 0) * dPlusY) +
                                   (Math.max(wComp[i], 0) * dMinusZ) + (Math.min(wComp[i], 0) * dPlusZ));
                            phix = (phiImage[i + 1] - phiImage[i - 1]) * 0.5;
                            phiy = (phiImage[i + xDim] - phiImage[i - xDim]) * 0.5;
                            phiz = (phiImage[i + sliceSize] - phiImage[i - sliceSize]) * 0.5;
                            phixx = phiImage[i + 1] - (2 * phiImage[i]) + phiImage[i - 1];
                            phiyy = phiImage[i + xDim] - (2 * phiImage[i]) + phiImage[i - xDim];
                            phizz = phiImage[i + sliceSize] - (2 * phiImage[i]) + phiImage[i - sliceSize];
                            phixy = (phiImage[i + xDim + 1] - phiImage[i + xDim - 1] - phiImage[i - xDim + 1] +
                                     phiImage[i - xDim - 1]) * 0.25;
                            phixz = (phiImage[i + sliceSize + 1] - phiImage[i + sliceSize - 1] -
                                     phiImage[i - sliceSize + 1] + phiImage[i - sliceSize - 1]) * 0.25;
                            phiyz = (phiImage[i + sliceSize + xDim] - phiImage[i + sliceSize - xDim] -
                                     phiImage[i - sliceSize + xDim] + phiImage[i - sliceSize - xDim]) * 0.25;
                            denom = (phix * phix) + (phiy * phiy) + (phiz * phiz);

                            if (denom > 0) {
                                c3 = (((phixx + phizz) * phiy * phiy) - (2 * phix * phiz * phixz) +
                                      ((phiyy + phizz) * phix * phix) - (2 * phiy * phiz * phiyz) +
                                      ((phixx + phiyy) * phiz * phiz) - (2 * phix * phiy * phixy)) / denom;
                                c3 *= epsilon * gBuffer[i];
                            } else {
                                c3 = 0.0;
                            }

                            if (Math.abs(c1 + c3) > c13max) {
                                c13max = Math.abs(c1 + c3);
                            }

                            if (Math.abs(c2) > c2max) {
                                c2max = Math.abs(c2);
                            }
                        }
                    }
                }

                beta = (float) (edgeAttract * c13max / c2max);
            } // if (reinitializePhi)
        } // for(n = 0; n < iterations && !threadStopped && haveChanged; n++)

        phiNext = null;
        tempBuffer = null;
        System.gc();

        if (threadStopped) {
            
            setCompleted(false);
            finalize();

            return;
        }

        // delete the VOIs
        VOIs = srcImage.getVOIs();

        nVOI = VOIs.size();

        if (nVOI == 0) {
            return;
        }

        for (i = (nVOI - 1); i >= 0; i--) {
            VOIs.removeElementAt(i);
        }

        voiID = 0;
        mask = new BitSet(length);

        for (i = 0; i < length; i++) {

            if (phiImage[i] < 0.0) {
                mask.set(i);
            } else {
                mask.clear(i);
            }
        }

        AlgorithmVOIExtractionPaint algoPaintToVOI = new AlgorithmVOIExtractionPaint(srcImage, mask, xDim, yDim, zDim,
                                                                                     voiID);

        algoPaintToVOI.run();
        algoPaintToVOI = null;

        
        setCompleted(true);
    }

    /**
     * makeKernals2D - creates the derivative kernels used to calculate the gradient magnitude and kernel for the
     * diffusion process.
     */
    private void makeKernels2D() {
        int xkDim, ykDim;
        int[] derivOrder = new int[2];

        kExtents = new int[2];
        derivOrder[0] = 1;
        derivOrder[1] = 0;

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        GxData = new float[xkDim * ykDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);

        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        GyData = new float[xkDim * ykDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);

        Gy.calc(true);
    }

    /**
     * makeKernals3D - creates the derivative kernels used to calculate the gradient magnitude and kernel for the
     * diffusion process.
     */
    private void makeKernels3D() {
        int xkDim, ykDim, zkDim;
        int[] derivOrder = new int[3];

        kExtents = new int[3];
        derivOrder[0] = 1;
        derivOrder[1] = 0;
        derivOrder[2] = 0;

        xkDim = Math.round(5 * sigmas[0]);

        if ((xkDim % 2) == 0) {
            xkDim++;
        }

        if (xkDim < 3) {
            xkDim = 3;
        }

        kExtents[0] = xkDim;

        ykDim = Math.round(5 * sigmas[1]);

        if ((ykDim % 2) == 0) {
            ykDim++;
        }

        if (ykDim < 3) {
            ykDim = 3;
        }

        kExtents[1] = ykDim;

        zkDim = Math.round(5 * sigmas[2]);

        if ((zkDim % 2) == 0) {
            zkDim++;
        }

        if (zkDim < 3) {
            zkDim = 3;
        }

        kExtents[2] = zkDim;

        GxData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gx = new GenerateGaussian(GxData, kExtents, sigmas, derivOrder);

        Gx.calc(false);

        derivOrder[0] = 0;
        derivOrder[1] = 1;
        derivOrder[2] = 0;
        GyData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gy = new GenerateGaussian(GyData, kExtents, sigmas, derivOrder);

        Gy.calc(true);

        derivOrder[0] = 0;
        derivOrder[1] = 0;
        derivOrder[2] = 1;
        GzData = new float[xkDim * ykDim * zkDim];

        GenerateGaussian Gz = new GenerateGaussian(GzData, kExtents, sigmas, derivOrder);

        Gz.calc(true);
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm){
        if(!algorithm.isCompleted()){
            finalize();
            return;
        }
        if (algorithm instanceof AlgorithmConvolver) {
            AlgorithmConvolver convolver = (AlgorithmConvolver) algorithm;
            outputBuffer = convolver.getOutputBuffer();
        }
    }
}
