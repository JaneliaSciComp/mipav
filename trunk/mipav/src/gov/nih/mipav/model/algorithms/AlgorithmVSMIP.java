package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;

import java.text.*;


/**
 * DOCUMENT ME!
 */
public class AlgorithmVSMIP extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    float angleX = 0.0f, angleY = 0.0f;

    /** DOCUMENT ME! */
    float[] destBuffer = null;

    /** DOCUMENT ME! */
    DecimalFormat fltFmt;

    /** DOCUMENT ME! */
    boolean iterateAngles = false;

    /** DOCUMENT ME! */
    float[] mipBuffer = null;

    /** DOCUMENT ME! */
    ModelImage mipImage = null;

    /** DOCUMENT ME! */
    int[] mipPlaneDims = null;

    /** DOCUMENT ME! */
    float[] srcBuffer = null;

    /** DOCUMENT ME! */
    float stepSize = 1.0f;

    /** DOCUMENT ME! */
    float[] volCenter = null;

    /** DOCUMENT ME! */
    int[] volDims = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------


    /**
     * Creates a new AlgorithmVSMIP object.
     *
     * @param  destImg  DOCUMENT ME!
     * @param  mipImg   DOCUMENT ME!
     * @param  srcImg   DOCUMENT ME!
     * @param  width    DOCUMENT ME!
     * @param  height   DOCUMENT ME!
     * @param  dStep    DOCUMENT ME!
     * @param  rotX     DOCUMENT ME!
     * @param  rotY     DOCUMENT ME!
     * @param  iterate  DOCUMENT ME!
     */
    public AlgorithmVSMIP(ModelImage destImg, ModelImage mipImg, ModelImage srcImg, int width, int height, float dStep,
                          float rotX, float rotY, boolean iterate) {
        super(destImg, srcImg);

        fltFmt = new DecimalFormat("0.00");

        mipImage = mipImg;

        mipPlaneDims = new int[2];
        mipPlaneDims[0] = width;
        mipPlaneDims[1] = height;

        stepSize = dStep;
        angleX = rotX;
        angleY = rotY;

        iterateAngles = iterate;
    } // end AlgorithmVSMIP(...)

    //~ Methods --------------------------------------------------------------------------------------------------------


    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        srcImage = null;
        srcBuffer = null;
        destImage = null;
        destBuffer = null;
        super.finalize();
    } // end finalize()

    /**
     * good method that works.
     */
    public void runAlgorithm() {

        // sanity checks
        if (srcImage == null) {
            MipavUtil.displayError("AlgorithmVSMIP::run()  Source Image is null");

            return;
        }

        if (srcImage.isColorImage()) {
            MipavUtil.displayError("AlgorithmVSMIP::run()  does NOT do color images");

            return;
        }

        if (srcImage.getNDims() != 3) {
            MipavUtil.displayError("AlgorithmVSMIP::run()  Source Image MUST be 3D");

            return;
        }

        if (destImage.getNDims() != 3) {
            MipavUtil.displayError("AlgorithmVSMIP::run()  Destinition Image MUST be 3D");

            return;
        }

        if (mipImage.getNDims() != 2) {
            MipavUtil.displayError("AlgorithmVSMIP::run()  MIP Image MUST be 2D");

            return;
        }

        

        if (iterateAngles == false) {
            runNoIteration();
        } else {
            runIteration();
        }
    } // end run()

    /**
     * DOCUMENT ME!
     *
     * @param   a  DOCUMENT ME!
     * @param   b  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] crossProduct(float[] a, float[] b) {
        float[] result = new float[3];
        result[0] = (a[1] * b[2]) - (a[2] * b[1]);
        result[1] = -((a[0] * b[2]) - (a[2] * b[0]));
        result[2] = (a[0] * b[1]) - (a[1] * b[0]);

        return result;
    } // end crossProduct(...)


    /**
     * DOCUMENT ME!
     *
     * @param   a  DOCUMENT ME!
     * @param   b  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float distance(float[] a, float[] b) {
        return (float) (Math.sqrt(SQR(a[0] - b[0]) + SQR(a[1] - b[1]) + SQR(a[2] - b[2])));
    } // end distance(...)


    /**
     * DOCUMENT ME!
     *
     * @param   startPt    DOCUMENT ME!
     * @param   direction  DOCUMENT ME!
     * @param   distance   DOCUMENT ME!
     * @param   mipI       DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float mipTracePoint(float[] startPt, float[] direction, float distance, MIPNode mipI) {
        float[] volCoords = new float[3];

        int volColMaxIndex = volDims[0] - 1;
        int volRowMaxIndex = volDims[1] - 1;
        int volPlaneMaxIndex = volDims[2] - 1;

        float[] currPt = new float[3];
        float currDist = 0.0f, currVal, maxVal = 0.0f;
        int numSteps = 0;

        while (currDist < distance) {
            currPt[0] = startPt[0] + (numSteps * stepSize * direction[0]);
            currPt[1] = startPt[1] + (numSteps * stepSize * direction[1]);
            currPt[2] = startPt[2] + (numSteps * stepSize * direction[2]);
            currDist = distance(startPt, currPt);

            // convert current ray point to voxel coordinates and see if it is
            // within the volume bounds
            volCoords[0] = currPt[0] + volCenter[0];

            if ((volCoords[0] > 0) && (volCoords[0] < volColMaxIndex)) {
                volCoords[1] = volRowMaxIndex - (currPt[1] + volCenter[1]);

                if ((volCoords[1] > 0) && (volCoords[1] < volRowMaxIndex)) {
                    volCoords[2] = currPt[2] + volCenter[2];

                    if ((volCoords[2] > 0) && (volCoords[2] < volPlaneMaxIndex)) {

                        // volCoords is with the volume bounds, sample it
                        currVal = sampleVolume(volCoords[0], volCoords[1], volCoords[2]);

                        if (currVal > maxVal) {
                            mipI.x = volCoords[0];
                            mipI.y = volCoords[1];
                            mipI.z = volCoords[2];
                            mipI.intensity = currVal;
                            maxVal = currVal;
                        } // end if (currVal > maxVal)

                    } // end if (volCoords[2] > 0 ...)
                } // end if (volCoords[1] > 0 ...)
            } // end if (volCoords[0] > 0 ...)

            numSteps++;
        } // end while(currDist < distance)

        setCompleted(true);

        return maxVal;
    } // end mipTracePoint(...)

    /**
     * DOCUMENT ME!
     *
     * @param  v  DOCUMENT ME!
     */
    private void normalize(float[] v) {
        float mag = (float) Math.sqrt(SQR(v[0]) + SQR(v[1]) + SQR(v[2]));

        if (mag < 0.0000001) {
            mag = 1.0f;
        }

        v[0] /= mag;
        v[1] /= mag;
        v[2] /= mag;
    } // end normalize(...)


    /**
     * DOCUMENT ME!
     *
     * @param  angleInDegrees  DOCUMENT ME!
     * @param  pt              DOCUMENT ME!
     */
    private void rotateX(float angleInDegrees, float[] pt) {
        float x = pt[0], y = pt[1], z = pt[2];
        double angleInRads = angleInDegrees * Math.PI / 180.0;
        double cTheta = Math.cos(angleInRads);
        double sTheta = Math.sin(angleInRads);

        pt[0] = (float) x;
        pt[1] = (float) ((cTheta * y) - (sTheta * z));
        pt[2] = (float) ((sTheta * y) + (cTheta * z));
    } // end rotateX(...)


    /**
     * DOCUMENT ME!
     *
     * @param  angleInDegrees  DOCUMENT ME!
     * @param  pt              DOCUMENT ME!
     */
    private void rotateY(float angleInDegrees, float[] pt) {
        float x = pt[0], y = pt[1], z = pt[2];
        double angleInRads = angleInDegrees * Math.PI / 180.0;
        double cTheta = Math.cos(angleInRads);
        double sTheta = Math.sin(angleInRads);

        pt[0] = (float) ((cTheta * x) + (sTheta * z));
        pt[1] = (float) y;
        pt[2] = (float) ((-sTheta * x) + (cTheta * z));
    } // end rotateY(...)

    /**
     * Rotate about the y-axis, then the x-axis.
     *
     * @param  angleXInDegrees  DOCUMENT ME!
     * @param  angleYInDegrees  DOCUMENT ME!
     * @param  pt               DOCUMENT ME!
     */
    private void rotateYX(float angleXInDegrees, float angleYInDegrees, float[] pt) {
        float x = pt[0], y = pt[1], z = pt[2];
        double angleXInRads = angleXInDegrees * Math.PI / 180.0;
        double cX = Math.cos(angleXInRads);
        double sX = Math.sin(angleXInRads);

        double angleYInRads = angleYInDegrees * Math.PI / 180.0;
        double cY = Math.cos(angleYInRads);
        double sY = Math.sin(angleYInRads);

        pt[0] = (float) ((cY * x) + (sY * z));
        pt[1] = (float) ((-sX * sY * x) + (cX * y) - (sX * cY * z));
        pt[2] = (float) ((-cX * sY * x) + (sX * y) + (cX * cY * z));
    } // end rotateYX(...)


    /**
     * DOCUMENT ME!
     */
    private void runIteration() {

    	fireProgressStateChanged("MIP Method", "MIP Method");
        fireProgressStateChanged("Swappin Data");

        // copy the image data into the sourceBuffer so we can access it
        volDims = srcImage.getExtents();

        int volLength = volDims[0] * volDims[1] * volDims[2];
        mipPlaneDims = mipImage.getExtents();

        int mipPlaneLength = mipPlaneDims[0] * mipPlaneDims[1];

        try {
            srcBuffer = new float[volLength];
            srcImage.exportData(0, volLength, srcBuffer);
            destBuffer = new float[volLength];
            destImage.exportData(0, volLength, destBuffer);
            mipBuffer = new float[mipPlaneLength];
        } catch (IOException error) {
            srcBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  could NOT export source image");

            return;
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  Out of memory when creating image buffer");

            return;
        } // end try{}-catch{}-catch{}


        // OK, let er rip!

        // volume center is coordinate system center
        volCenter = new float[3];
        volCenter[0] = volDims[0] / 2.0f;
        volCenter[1] = volDims[1] / 2.0f;
        volCenter[2] = volDims[2] / 2.0f;

        // diagonal of the bounding box
        float volDiagonal = (float) Math.sqrt(SQR(volDims[0]) + SQR(volDims[1]) + SQR(volDims[2]));

        // MIP plane centered with the volume, translated outside volume in Z
        float[] mipTopLeft = { -mipPlaneDims[0] / 2.0f, mipPlaneDims[1] / 2.0f, -volDiagonal / 2.0f };
        float[] mipTopRight = { mipPlaneDims[0] / 2.0f, mipPlaneDims[1] / 2.0f, -volDiagonal / 2.0f };
        float[] mipBottomRight = { mipPlaneDims[0] / 2.0f, -mipPlaneDims[1] / 2.0f, -volDiagonal / 2.0f };
        float[] mipBottomLeft = { -mipPlaneDims[0] / 2.0f, -mipPlaneDims[1] / 2.0f, -volDiagonal / 2.0f };

        // Transform MIP plane first rotate about the Y-axis then rotate about the X-axis
        float[] mipTopLeftPrime = new float[3];
        float[] mipTopRightPrime = new float[3];
        float[] mipBottomLeftPrime = new float[3];

        float[] colDirection = new float[3];
        float[] rowDirection = new float[3];

        // each step of a column moves the mipPlane location by this much
        float[] dc = new float[3];
        float[] dr = new float[3];

        float[] mipPlaneNormal = new float[3];

        // Initialize raster scan at the top left corner
        float[] leftMostPoint = new float[3];
        float[] mipPlanePt = new float[3];

        // raster scan MIP plane and trace ray through volume
        int destIndex = 0, index;
        MIPNode mipInfo = new MIPNode();

        while (angleY < 90.1f) {

            fireProgressStateChanged("Ray Trace   Angle X: " + angleX + " Y: " + angleY);

            // reset for each iteration
            destIndex = 0;
            mipTopLeftPrime[0] = mipTopLeft[0];
            mipTopLeftPrime[1] = mipTopLeft[1];
            mipTopLeftPrime[2] = mipTopLeft[2];

            mipTopRightPrime[0] = mipTopRight[0];
            mipTopRightPrime[1] = mipTopRight[1];
            mipTopRightPrime[2] = mipTopRight[2];

            mipBottomLeftPrime[0] = mipBottomLeft[0];
            mipBottomLeftPrime[1] = mipBottomLeft[1];
            mipBottomLeftPrime[2] = mipBottomLeft[2];

            rotateYX(angleX, angleY, mipTopLeftPrime);
            rotateYX(angleX, angleY, mipTopRightPrime);
            rotateYX(angleX, angleY, mipBottomLeftPrime);

            colDirection[0] = mipTopRightPrime[0] - mipTopLeftPrime[0];
            colDirection[1] = mipTopRightPrime[1] - mipTopLeftPrime[1];
            colDirection[2] = mipTopRightPrime[2] - mipTopLeftPrime[2];
            normalize(colDirection);
            dc[0] = colDirection[0];
            dc[1] = colDirection[1];
            dc[2] = colDirection[2];

            rowDirection[0] = mipBottomLeftPrime[0] - mipTopLeftPrime[0];
            rowDirection[1] = mipBottomLeftPrime[1] - mipTopLeftPrime[1];
            rowDirection[2] = mipBottomLeftPrime[2] - mipTopLeftPrime[2];
            normalize(rowDirection);
            dr[0] = rowDirection[0];
            dr[1] = rowDirection[1];
            dr[2] = rowDirection[2];

            mipPlaneNormal = crossProduct(rowDirection, colDirection);

            for (int row = 0; row < mipPlaneDims[1]; row++) {
                fireProgressStateChanged(Math.round(((float) (row) / (mipPlaneDims[1] - 1) * 100)));
                leftMostPoint[0] = mipTopLeftPrime[0] + (row * dr[0]);
                leftMostPoint[1] = mipTopLeftPrime[1] + (row * dr[1]);
                leftMostPoint[2] = mipTopLeftPrime[2] + (row * dr[2]);

                for (int col = 0; col < mipPlaneDims[0]; col++) {
                    mipPlanePt[0] = leftMostPoint[0] + (col * dc[0]);
                    mipPlanePt[1] = leftMostPoint[1] + (col * dc[1]);
                    mipPlanePt[2] = leftMostPoint[2] + (col * dc[2]);

                    mipBuffer[destIndex] = mipTracePoint(mipPlanePt, mipPlaneNormal, volDiagonal, mipInfo);

                    index = ((int) (mipInfo.z)) * volDims[0] * volDims[1];
                    index += ((int) (mipInfo.y)) * volDims[0];
                    index += (int) (mipInfo.x);
                    destBuffer[index] += mipInfo.intensity;
                    destIndex++;

                } // end for (int col = 0; ...)
            } // end for (int row = 0; ...)

            angleY += 10.0f;
        } // end while(angleY < 90.0f)

        angleY = 0.0f;
        angleX = 10.0f;

        while (angleX < 90.1f) {

            fireProgressStateChanged("Ray Trace   Angle X: " + angleX + " Y: " + angleY);

            // reset for each iteration
            destIndex = 0;
            mipTopLeftPrime[0] = mipTopLeft[0];
            mipTopLeftPrime[1] = mipTopLeft[1];
            mipTopLeftPrime[2] = mipTopLeft[2];

            mipTopRightPrime[0] = mipTopRight[0];
            mipTopRightPrime[1] = mipTopRight[1];
            mipTopRightPrime[2] = mipTopRight[2];

            mipBottomLeftPrime[0] = mipBottomLeft[0];
            mipBottomLeftPrime[1] = mipBottomLeft[1];
            mipBottomLeftPrime[2] = mipBottomLeft[2];

            rotateYX(angleX, angleY, mipTopLeftPrime);
            rotateYX(angleX, angleY, mipTopRightPrime);
            rotateYX(angleX, angleY, mipBottomLeftPrime);

            colDirection[0] = mipTopRightPrime[0] - mipTopLeftPrime[0];
            colDirection[1] = mipTopRightPrime[1] - mipTopLeftPrime[1];
            colDirection[2] = mipTopRightPrime[2] - mipTopLeftPrime[2];
            normalize(colDirection);
            dc[0] = colDirection[0];
            dc[1] = colDirection[1];
            dc[2] = colDirection[2];

            rowDirection[0] = mipBottomLeftPrime[0] - mipTopLeftPrime[0];
            rowDirection[1] = mipBottomLeftPrime[1] - mipTopLeftPrime[1];
            rowDirection[2] = mipBottomLeftPrime[2] - mipTopLeftPrime[2];
            normalize(rowDirection);
            dr[0] = rowDirection[0];
            dr[1] = rowDirection[1];
            dr[2] = rowDirection[2];

            mipPlaneNormal = crossProduct(rowDirection, colDirection);

            for (int row = 0; row < mipPlaneDims[1]; row++) {
                fireProgressStateChanged(Math.round(((float) (row) / (mipPlaneDims[1] - 1) * 100)));
                leftMostPoint[0] = mipTopLeftPrime[0] + (row * dr[0]);
                leftMostPoint[1] = mipTopLeftPrime[1] + (row * dr[1]);
                leftMostPoint[2] = mipTopLeftPrime[2] + (row * dr[2]);

                for (int col = 0; col < mipPlaneDims[0]; col++) {
                    mipPlanePt[0] = leftMostPoint[0] + (col * dc[0]);
                    mipPlanePt[1] = leftMostPoint[1] + (col * dc[1]);
                    mipPlanePt[2] = leftMostPoint[2] + (col * dc[2]);

                    mipBuffer[destIndex] = mipTracePoint(mipPlanePt, mipPlaneNormal, volDiagonal, mipInfo);

                    index = ((int) (mipInfo.z)) * volDims[0] * volDims[1];
                    index += ((int) (mipInfo.y)) * volDims[0];
                    index += (int) (mipInfo.x);
                    destBuffer[index] += mipInfo.intensity;
                    destIndex++;

                } // end for (int col = 0; ...)
            } // end for (int row = 0; ...)

            angleX += 10.0f;
        } // end while(angleX < 90.0f)

        try {
            destImage.importData(0, destBuffer, true);
        } catch (IOException error) {
            srcBuffer = null;
            mipBuffer = null;
            destBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  Could NOT import destBuffer to the image");

            return;
        } // end try{}-catch{}

        try {
            mipImage.importData(0, mipBuffer, true);
        } catch (IOException error) {
            srcBuffer = null;
            mipBuffer = null;
            destBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  Could NOT import destBuffer to the image");

            return;
        } // end try{}-catch{}

        
    } // end runIteration()


    /**
     * DOCUMENT ME!
     */
    private void runNoIteration() {

    	fireProgressStateChanged("MIP Method", "MIP Method");
        fireProgressStateChanged("Swappin Data");

        // copy the image data into the sourceBuffer so we can access it
        volDims = srcImage.getExtents();

        int volLength = volDims[0] * volDims[1] * volDims[2];
        mipPlaneDims = mipImage.getExtents();

        int mipPlaneLength = mipPlaneDims[0] * mipPlaneDims[1];

        try {
            srcBuffer = new float[volLength];
            srcImage.exportData(0, volLength, srcBuffer);
            destBuffer = new float[volLength];
            destImage.exportData(0, volLength, destBuffer);
            mipBuffer = new float[mipPlaneLength];
        } catch (IOException error) {
            srcBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  could NOT export source image");

            return;
        } catch (OutOfMemoryError e) {
            srcBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  Out of memory when creating image buffer");

            return;
        } // end try{}-catch{}-catch{}


        // OK, let er rip!

        // volume center is coordinate system center
        volCenter = new float[3];
        volCenter[0] = volDims[0] / 2.0f;
        volCenter[1] = volDims[1] / 2.0f;
        volCenter[2] = volDims[2] / 2.0f;

        // diagonal of the bounding box
        float volDiagonal = (float) Math.sqrt(SQR(volDims[0]) + SQR(volDims[1]) + SQR(volDims[2]));

        // MIP plane centered with the volume, translated outside volume in Z
        float[] mipTopLeft = { -mipPlaneDims[0] / 2.0f, mipPlaneDims[1] / 2.0f, -volDiagonal / 2.0f };
        float[] mipTopRight = { mipPlaneDims[0] / 2.0f, mipPlaneDims[1] / 2.0f, -volDiagonal / 2.0f };
        float[] mipBottomRight = { mipPlaneDims[0] / 2.0f, -mipPlaneDims[1] / 2.0f, -volDiagonal / 2.0f };
        float[] mipBottomLeft = { -mipPlaneDims[0] / 2.0f, -mipPlaneDims[1] / 2.0f, -volDiagonal / 2.0f };

        // Transform MIP plane first rotate about the Y-axis then rotate about the X-axis
        float[] mipTopLeftPrime = { mipTopLeft[0], mipTopLeft[1], mipTopLeft[2] };
        rotateYX(angleX, angleY, mipTopLeftPrime);

        float[] mipTopRightPrime = { mipTopRight[0], mipTopRight[1], mipTopRight[2] };
        rotateYX(angleX, angleY, mipTopRightPrime);

        float[] mipBottomLeftPrime = { mipBottomLeft[0], mipBottomLeft[1], mipBottomLeft[2] };
        rotateYX(angleX, angleY, mipBottomLeftPrime);

        float[] colDirection = new float[3];
        colDirection[0] = mipTopRightPrime[0] - mipTopLeftPrime[0];
        colDirection[1] = mipTopRightPrime[1] - mipTopLeftPrime[1];
        colDirection[2] = mipTopRightPrime[2] - mipTopLeftPrime[2];
        normalize(colDirection);

        // each step of a column moves the mipPlane location by this much
        float[] dc = { colDirection[0], colDirection[1], colDirection[2] };

        float[] rowDirection = new float[3];
        rowDirection[0] = mipBottomLeftPrime[0] - mipTopLeftPrime[0];
        rowDirection[1] = mipBottomLeftPrime[1] - mipTopLeftPrime[1];
        rowDirection[2] = mipBottomLeftPrime[2] - mipTopLeftPrime[2];
        normalize(rowDirection);

        // each step of a row moves the mipPlane location by this much
        float[] dr = { rowDirection[0], rowDirection[1], rowDirection[2] };

        float[] mipPlaneNormal = crossProduct(rowDirection, colDirection);

        fireProgressStateChanged("Ray Tracing");

        // Initialize raster scan at the top left corner
        float[] leftMostPoint = new float[3];
        float[] mipPlanePt = new float[3];

        // raster scan MIP plane and trace ray through volume
        int destIndex = 0, index;
        MIPNode mipInfo = new MIPNode();

        for (int row = 0; row < mipPlaneDims[1]; row++) {
            fireProgressStateChanged(Math.round(((float) (row) / (mipPlaneDims[1] - 1) * 100)));
            leftMostPoint[0] = mipTopLeftPrime[0] + (row * dr[0]);
            leftMostPoint[1] = mipTopLeftPrime[1] + (row * dr[1]);
            leftMostPoint[2] = mipTopLeftPrime[2] + (row * dr[2]);

            for (int col = 0; col < mipPlaneDims[0]; col++) {
                mipPlanePt[0] = leftMostPoint[0] + (col * dc[0]);
                mipPlanePt[1] = leftMostPoint[1] + (col * dc[1]);
                mipPlanePt[2] = leftMostPoint[2] + (col * dc[2]);

                mipBuffer[destIndex] = mipTracePoint(mipPlanePt, mipPlaneNormal, volDiagonal, mipInfo);

                index = ((int) (mipInfo.z)) * volDims[0] * volDims[1];
                index += ((int) (mipInfo.y)) * volDims[0];
                index += (int) (mipInfo.x);
                destBuffer[index] += mipInfo.intensity;
                destIndex++;

            } // end for (int col = 0; ...)
        } // end for (int row = 0; ...)

        try {
            destImage.importData(0, destBuffer, true);
        } catch (IOException error) {
            srcBuffer = null;
            mipBuffer = null;
            destBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  Could NOT import destBuffer to the image");

            return;
        } // end try{}-catch{}

        try {
            mipImage.importData(0, mipBuffer, true);
        } catch (IOException error) {
            srcBuffer = null;
            mipBuffer = null;
            destBuffer = null;
            MipavUtil.displayError("AlgorithmVSMIP::run()  Could NOT import destBuffer to the image");

            return;
        } // end try{}-catch{}

        
    } // end runNoIteration()

    /**
     * do trilinear interpolate.
     *
     * @param   x  DOCUMENT ME!
     * @param   y  DOCUMENT ME!
     * @param   z  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float sampleVolume(float x, float y, float z) {
        float val = 0.0f;

        int xLoc = (int) x, yLoc = (int) y, zLoc = (int) z;
        float a = x - xLoc, a1 = 1.0f - a;
        float b = y - yLoc, b1 = 1.0f - b;
        float c = z - zLoc, c1 = 1.0f - c;
        float coeff1 = a1 * b1;
        float coeff2 = a * b1;
        float coeff3 = a1 * b;
        float coeff4 = a * b;

        // find index for f(xLoc, yLoc, zLoc)
        int index1 = (zLoc * volDims[0] * volDims[1]) + (yLoc * volDims[0]) + xLoc;

        // index for f(xLoc+1, yLoc, zLoc)
        int index2 = index1 + 1;

        // index for f(xLoc, yLoc+1, zLoc)
        int index3 = index1 + volDims[0];

        // index for f(xLoc+1, yLoc+1, zLoc)
        int index4 = index3 + 1;

        // get first bilinear interpolation value
        float val1 = (coeff1 * srcBuffer[index1]) + (coeff2 * srcBuffer[index2]) + (coeff3 * srcBuffer[index3]) +
                     (coeff4 * srcBuffer[index4]);

        // update indices for next higher zLoc
        index1 = ((zLoc + 1) * volDims[0] * volDims[1]) + (yLoc * volDims[0]) + xLoc;
        index2 = index1 + 1;
        index3 = index1 + volDims[0];
        index4 = index3 + 1;

        // get second bilinear interpolation value
        float val2 = (coeff1 * srcBuffer[index1]) + (coeff2 * srcBuffer[index2]) + (coeff3 * srcBuffer[index3]) +
                     (coeff4 * srcBuffer[index4]);

        // linear interpolation between these values is the answer
        val = (c1 * val1) + (c * val2);

        return val;
    } // end sampleVolume(...)


    /**
     * DOCUMENT ME!
     *
     * @param   x  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float SQR(float x) {
        return x * x;
    }


} // end class AlgorithmVSMIP
