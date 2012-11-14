import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmFlip;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import gov.nih.mipav.view.dialogs.JPanelPixelExclusionSelector.RangeType;

import java.io.IOException;

import javax.swing.*;

import WildMagic.LibFoundation.Curves.BSplineBasisDiscretef;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This plugin was done for Dr Chi-hon Lee of NICHD
 * 
 * The main research problem Dr Lee had was that the z-resolution is poor compared to x and y when acquiring datsets
 * using confocal microscopy
 * 
 * The solution was to acquire 2 orthogonal datsets and combine them into a result image
 * 
 * In order to achieve this, one dataset needs to be registered to the other datset to obtain transformation files. This
 * part is done prior to the plugin by a user
 * 
 * This plugin builds a result image by transforming back to both images using the transformation files and applying a
 * certain combination of the pixels to put in the result image
 * 
 * The result image is of 512x512x512 size
 * 
 * @author pandyan
 */
public class PlugInAlgorithmDrosophilaRetinalRegistration extends AlgorithmBase {

    /** images * */
    private ModelImage imageX, imageXRegistered, imageY, resultImage, redChannelsImage, greenChannelsImage,
            imageXRegisteredTransformed, imageYTransformed;

    /** alg * */
    private AlgorithmVOIProps algoVOIProps;

    /** min and maxes of vois * */
    private float minG_X, minG_Y, maxG_X, maxG_Y;

    /** vjf to draw vois on * */
    private final ViewJFrameImage vjfX, vjfY;

    private final JTextArea outputTextArea;

    /** slope of transfer function * */
    private float slopeG;

    /** b-intercept of transfer function * */
    private float bG;

    /** transform matrices * */
    private final TransMatrix matrixGreen, matrixAffine;

    /** 2D and 3D B-Spline basis definitions. */
    private BSplineBasisDiscretef m_kBSplineBasisX;

    /** b spline */
    private BSplineBasisDiscretef m_kBSplineBasisY;

    /** b spline */
    private BSplineBasisDiscretef m_kBSplineBasisZ;

    /** b slpine */
    private BSplineLattice3Df m_kBSpline3D;

    /** alg * */
    private AlgorithmTransform algoTrans;

    /** image y res * */
    private final float[] imageYRes;

    /** image Y end * */
    private final boolean imageYEnd;

    /** dir * */
    private final String dir;

    /** image x orig * */
    private final float[] imageXOrig;

    /** extents */
    private final int[] imageYExtents;

    /** radio buttons * */
    private final JRadioButton doTrilinearRadio, doAverageRadio, doRescaleRadio, ignoreBGRadio, doSqRtRadio;

    /** textfield * */
    private final JTextField transform3FilePathTextField;

    /** num control points * */
    private final int numControlPoints;

    /** spline degree * */
    private final int splineDegree;

    /** control mat * */
    private final float[][] controlMat;
    
    private boolean createMirroredImg;

    /**
     * constructr
     * 
     * @param imageX
     * @param imageXRegistered
     * @param imageY
     * @param vjfX
     * @param vjfY
     * @param outputTextArea
     * @param matrixGreen
     * @param matrixAffine
     * @param doTrilinearRadio
     * @param doAverageRadio
     * @param doRescaleRadio
     * @param ignoreBGRadio
     * @param doSqRtRadio
     * @param transform3FilePathTextField
     * @param numControlPoints
     * @param splineDegree
     * @param controlMat
     */
    public PlugInAlgorithmDrosophilaRetinalRegistration(final ModelImage imageX, final ModelImage imageXRegistered,
            final ModelImage imageY, final ViewJFrameImage vjfX, final ViewJFrameImage vjfY,
            final JTextArea outputTextArea, final TransMatrix matrixGreen, final TransMatrix matrixAffine,
            final JRadioButton doTrilinearRadio, final JRadioButton doAverageRadio, final JRadioButton doRescaleRadio,
            final JRadioButton ignoreBGRadio, final JRadioButton doSqRtRadio,
            final JTextField transform3FilePathTextField, final int numControlPoints, final int splineDegree,
            final float[][] controlMat, boolean createMirroredImg) {
        this.imageX = imageX;
        this.imageXRegistered = imageXRegistered;
        this.imageY = imageY;
        this.vjfX = vjfX;
        this.vjfY = vjfY;
        this.outputTextArea = outputTextArea;
        this.matrixGreen = matrixGreen;
        this.matrixAffine = matrixAffine;
        this.doTrilinearRadio = doTrilinearRadio;
        this.doAverageRadio = doAverageRadio;
        this.doRescaleRadio = doRescaleRadio;
        this.ignoreBGRadio = ignoreBGRadio;
        this.doSqRtRadio = doSqRtRadio;
        this.transform3FilePathTextField = transform3FilePathTextField;
        this.numControlPoints = numControlPoints;
        this.splineDegree = splineDegree;
        this.controlMat = controlMat;
        this.createMirroredImg = createMirroredImg;

        imageYRes = imageY.getResolutions(0);
        imageYEnd = imageY.getFileInfo()[0].getEndianess();
        dir = imageY.getFileInfo()[0].getFileDirectory();
        imageXOrig = imageX.getFileInfo()[0].getOrigin();
        imageYExtents = imageY.getExtents();
    }

    /**
     * run algorithm
     */
    public void runAlgorithm() {
        outputTextArea.append("Running Algorithm v2.9" + "\n");
        final long begTime = System.currentTimeMillis();

        // rescale imageX intensity to imageY based on VOI
        if (doRescaleRadio.isSelected()) {
            outputTextArea.append("Rescaling image... \n");
            final VOIVector VOIsX = imageX.getVOIs();
            final int nVOIX = VOIsX.size();
            if (nVOIX != 1) {
                MipavUtil.displayError("Both images must contain one VOI");
                return;
            }
            final VOIVector VOIsY = imageY.getVOIs();
            final int nVOIY = VOIsY.size();
            if (nVOIY != 1) {
                MipavUtil.displayError("Both images must contain one VOI");
                return;
            }
            final VOI VOIX = VOIsX.VOIAt(0);
            VOIX.setAllActive(true);
            algoVOIProps = new AlgorithmVOIProps(imageX, AlgorithmVOIProps.PROCESS_PER_VOI,
                    RangeType.NO_RANGE, getActiveVOIs(imageX));
            algoVOIProps.run();


            minG_X = algoVOIProps.getMinIntensityGreen();
            maxG_X = algoVOIProps.getMaxIntensityGreen();



            algoVOIProps.finalize();
            algoVOIProps = null;
            final VOI VOIY = VOIsY.VOIAt(0);
            VOIY.setAllActive(true);
            algoVOIProps = new AlgorithmVOIProps(imageY, AlgorithmVOIProps.PROCESS_PER_VOI,
                    RangeType.NO_RANGE, getActiveVOIs(imageY));
            algoVOIProps.run();


            minG_Y = algoVOIProps.getMinIntensityGreen();
            maxG_Y = algoVOIProps.getMaxIntensityGreen();



            algoVOIProps.finalize();
            algoVOIProps = null;

            vjfX.setVisible(false);
            vjfY.setVisible(false);

            VOIsX.clear();
            VOIsY.clear();

            slopeG = calculateSlope(minG_Y, minG_X, maxG_Y, maxG_X);
            bG = calculateB(minG_Y, minG_X, slopeG);

            // now we go through imageX and rescale
            final int length = imageX.getExtents()[0] * imageX.getExtents()[1] * imageX.getExtents()[2] * 4;
            final float[] buffer = new float[length];

            try {
                imageX.exportData(0, length, buffer);
            } catch (final IOException error) {
                System.out.println("IO exception");
                return;
            }
            float green;
            float newGreen;

            for (int i = 0; i < buffer.length; i = i + 4) {
                /*
                 * red = buffer[i+1]; if(slopeR == 0 && bR == 0) { newRed = red; }else { newRed =
                 * getNewValue(red,slopeR,bR); if(newRed < 0) { newRed = 0; }else if(newRed > 255) { newRed = 255; } }
                 * buffer[i+1] = newRed;
                 */

                green = buffer[i + 2];
                if (slopeG == 0 && bG == 0) {
                    newGreen = green;
                } else {
                    newGreen = getNewValue(green, slopeG, bG);
                    if (newGreen < 0) {
                        newGreen = 0;
                    } else if (newGreen > 255) {
                        newGreen = 255;
                    }
                }
                buffer[i + 2] = newGreen;

                /*
                 * blue = buffer[i+3]; if(slopeB == 0 && bB == 0) { newBlue = blue; }else { newBlue =
                 * getNewValue(blue,slopeB,bB); if(newBlue < 0) { newBlue = 0; }else if(newBlue > 255) { newBlue =
                 * 255; } } buffer[i+3] = newBlue;
                 */
            }

            try {
                imageX.importData(0, buffer, true);
            } catch (final IOException error) {
                System.out.println("IO exception");
                return;
            }
            imageX.calcMinMax();
        }// done rescaling


        
        TransMatrix intermMatrix1;
        if(matrixGreen == null) {
        	intermMatrix1 = matrixAffine;
        	intermMatrix1.Inverse();
        }else {
        	intermMatrix1 = new TransMatrix(4);
            intermMatrix1.Mult(matrixAffine, matrixGreen); // pretty sure this is correct
            intermMatrix1.Inverse();
        }


        final int[] extents = {512, 512, 512};
        resultImage = new ModelImage(ModelStorageBase.ARGB, extents, "resultImage");
        final float[] resultImageResols = new float[3];
        resultImageResols[0] = imageY.getResolutions(0)[0];
        resultImageResols[1] = imageY.getResolutions(0)[1];
        resultImageResols[2] = imageY.getResolutions(0)[2] * imageY.getExtents()[2] / 512;
        for (int i = 0; i < resultImage.getExtents()[2]; i++) {
            resultImage.setResolutions(i, resultImageResols);
        }
        final byte[] resultBuffer = new byte[512 * 512 * 512 * 4];
        int index = 0; // index into resultBuffer
        final double[] tPt1 = new double[3];
        final double[] tPt2 = new double[3];

        byte[] imageXBuffer;
        final int length1 = imageX.getExtents()[0] * imageX.getExtents()[1] * imageX.getExtents()[2] * 4;
        imageXBuffer = new byte[length1];
        try {
            imageX.exportData(0, length1, imageXBuffer);
        } catch (final IOException error) {
            System.out.println("IO exception");
            return;
        }

        
		AlgorithmBSpline bSplineX = new AlgorithmBSpline();
		double[] imageXFloatBuffer = new double[length1];
        for (int c = 0; c < 4; c++) {
            for (int z = 0; z < imageX.getExtents()[2]; z++) {
                for (int y = 0; y < imageX.getExtents()[1]; y++) {
                    for (int x = 0; x < imageX.getExtents()[0]; x++) {
                    	int tempIndex = 4 * (z * imageX.getExtents()[1] * imageX.getExtents()[0] +
                    			y * imageX.getExtents()[0] + x) + c;
                    	imageXFloatBuffer[tempIndex] = (imageXBuffer[tempIndex] & 0xff);
                    }
                }
            }
        }
		bSplineX.setup3DBSplineC(imageXFloatBuffer, imageX.getExtents(), 3);

        byte[] imageYBuffer;
        final int length2 = imageY.getExtents()[0] * imageY.getExtents()[1] * imageY.getExtents()[2] * 4;
        imageYBuffer = new byte[length2];
        try {
            imageY.exportData(0, length2, imageYBuffer);
        } catch (final IOException error) {
            System.out.println("IO exception");
            return;
        }
        
		AlgorithmBSpline bSplineY = new AlgorithmBSpline();
		double[] imageYFloatBuffer = new double[length2];
        for (int c = 0; c < 4; c++) {
            for (int z = 0; z < imageY.getExtents()[2]; z++) {
                for (int y = 0; y < imageY.getExtents()[1]; y++) {
                    for (int x = 0; x < imageY.getExtents()[0]; x++) {
                    	int tempIndex = 4 * (z * imageY.getExtents()[1] * imageY.getExtents()[0] +
                    			y * imageY.getExtents()[0] + x) + c;
                    	imageYFloatBuffer[tempIndex] = (imageYBuffer[tempIndex] & 0xff);
                    }
                }
            }
        }
		bSplineY.setup3DBSplineC(imageYFloatBuffer, imageY.getExtents(), 3);

        // following is if nlt file is inputted also
        ModelSimpleImage[] akSimpleImageSourceMap = null;
        if ( !transform3FilePathTextField.getText().trim().equals("")) {
            // create the non-linear image-maps
            m_kBSplineBasisX = new BSplineBasisDiscretef(numControlPoints, splineDegree, imageYExtents[0]);
            m_kBSplineBasisY = new BSplineBasisDiscretef(numControlPoints, splineDegree, imageYExtents[1]);
            m_kBSplineBasisZ = new BSplineBasisDiscretef(numControlPoints, splineDegree, imageYExtents[2]);
            m_kBSpline3D = new BSplineLattice3Df(m_kBSplineBasisX, m_kBSplineBasisY, m_kBSplineBasisZ);

            final Vector3f kPoint = new Vector3f();

            int ind = 0;

            for (int iControlX = 0; iControlX < numControlPoints; iControlX++) {
                // System.out.println("iControlX = " + iControlX);
                for (int iControlY = 0; iControlY < numControlPoints; iControlY++) {
                    for (int iControlZ = 0; iControlZ < numControlPoints; iControlZ++) {
                        kPoint.X = controlMat[ind][0];
                        kPoint.Y = controlMat[ind][1];
                        kPoint.Z = controlMat[ind++][2];
                        m_kBSpline3D.setControlPoint(iControlX, iControlY, iControlZ, kPoint);
                    }
                }
            }

            akSimpleImageSourceMap = m_kBSpline3D.createImageMap(imageYExtents[0], imageYExtents[1], imageYExtents[2]);
        }

        // okay....now....
        float xmm, ymm, zmm;
        byte[] rgb1 = new byte[3];
        byte[] rgb2 = new byte[3];
        final short[] rgb1_short = new short[3];
        final short[] rgb2_short = new short[3];
        // loop through each point in result image
        outputTextArea.append("Combining images into result image... \n");
        for (int z = 0; z < 512; z++) {
            outputTextArea.append("z is " + z + "\n");
            for (int y = 0; y < 512; y++) {
                for (int x = 0; x < 512; x++) {
                    // first transform the point back to both spaces...results in tPt1 and tPt2
                    if ( !transform3FilePathTextField.getText().trim().equals("")) { // if nlt file is inputted
                        xmm = x * resultImage.getResolutions(0)[0];
                        ymm = y * resultImage.getResolutions(0)[1];
                        zmm = z * resultImage.getResolutions(0)[2];

                        tPt1[0] = MipavMath.round(xmm / imageY.getResolutions(0)[0]);
                        tPt1[1] = MipavMath.round(ymm / imageY.getResolutions(0)[1]);
                        tPt1[2] = MipavMath.round(zmm / imageY.getResolutions(0)[2]);

                        tPt2[0] = xmm / imageY.getResolutions(0)[0];
                        tPt2[1] = ymm / imageY.getResolutions(0)[1];
                        tPt2[2] = zmm / imageY.getResolutions(0)[2];

                        final int iIndexTrg = (int) tPt1[0] + ((int) tPt1[1] * imageYExtents[0])
                                + ((int) tPt1[2] * imageYExtents[0] * imageYExtents[1]);

                        float xMapPt = 0.0f;
                        float yMapPt = 0.0f;
                        float zMapPt = 0.0f;

                        if (iIndexTrg < akSimpleImageSourceMap[0].data.length) {
                            xMapPt = (imageYExtents[0] - 1) * akSimpleImageSourceMap[0].data[iIndexTrg];
                            yMapPt = (imageYExtents[1] - 1) * akSimpleImageSourceMap[1].data[iIndexTrg];
                            zMapPt = (imageYExtents[2] - 1) * akSimpleImageSourceMap[2].data[iIndexTrg];
                        }

                        xmm = xMapPt * imageY.getResolutions(0)[0];
                        ymm = yMapPt * imageY.getResolutions(0)[1];
                        zmm = zMapPt * imageY.getResolutions(0)[2];

                        intermMatrix1.transform(xmm, ymm, zmm, tPt1);

                        tPt1[0] = tPt1[0] / imageX.getResolutions(0)[0];
                        tPt1[1] = tPt1[1] / imageX.getResolutions(0)[1];
                        tPt1[2] = tPt1[2] / imageX.getResolutions(0)[2];
                    } else { // if nlt file is NOT inputted
                        xmm = x * resultImage.getResolutions(0)[0];
                        ymm = y * resultImage.getResolutions(0)[1];
                        zmm = z * resultImage.getResolutions(0)[2];

                        
                        //6/4/2010
                        /*intermMatrix1.transform(xmm, ymm, zmm, tPt1);
                         
                        tPt1[0] = tPt1[0] / imageY.getResolutions(0)[0];
                        tPt1[1] = tPt1[1] / imageY.getResolutions(0)[1];
                        tPt1[2] = tPt1[2] / imageY.getResolutions(0)[2];*/
                        
                        
                        
                        //6/4/2010
                        tPt1[0] = MipavMath.round(xmm / imageY.getResolutions(0)[0]);
                        tPt1[1] = MipavMath.round(ymm / imageY.getResolutions(0)[1]);
                        tPt1[2] = MipavMath.round(zmm / imageY.getResolutions(0)[2]);
                        
                        intermMatrix1.transform(xmm, ymm, zmm, tPt1);
                        
                        tPt1[0] = tPt1[0] / imageX.getResolutions(0)[0];
                        tPt1[1] = tPt1[1] / imageX.getResolutions(0)[1];
                        tPt1[2] = tPt1[2] / imageX.getResolutions(0)[2];
                        
                        
                        
                        
                        
                        

                        xmm = x * resultImage.getResolutions(0)[0];
                        ymm = y * resultImage.getResolutions(0)[1];
                        zmm = z * resultImage.getResolutions(0)[2];

                        tPt2[0] = xmm / imageY.getResolutions(0)[0];
                        tPt2[1] = ymm / imageY.getResolutions(0)[1];
                        tPt2[2] = zmm / imageY.getResolutions(0)[2];

                    }
                    // Now either do averaging or closest-Z
                    int floorPointIndex1 = 0, floorPointIndex2 = 0;
                    //if (doAverageRadio.isSelected() || doSqRtRadio.isSelected()) {
                        // get linear interpolated values from both transformed points
                        if (tPt1[0] < 0 || tPt1[1] < 0 || tPt1[2] < 0 || tPt1[0] > imageX.getExtents()[0] - 1
                                || tPt1[1] > imageX.getExtents()[1] - 1 || tPt1[2] > imageX.getExtents()[2] - 1) {
                            rgb1_short[0] = 0;
                            rgb1_short[1] = 0;
                            rgb1_short[2] = 0;

                        } else {
                            final double tX1_floor = Math.floor(tPt1[0]);
                            final double tY1_floor = Math.floor(tPt1[1]);
                            final double tZ1_floor = Math.floor(tPt1[2]);
                            final float dx1 = (float) (tPt1[0] - tX1_floor);
                            final float dy1 = (float) (tPt1[1] - tY1_floor);
                            final float dz1 = (float) (tPt1[2] - tZ1_floor);
                            final int[] extents1 = imageX.getExtents();
                            floorPointIndex1 = (int) ( ( (tZ1_floor * (extents1[0] * extents1[1]))
                                    + (tY1_floor * extents1[0]) + tX1_floor) * 4);
                            if (floorPointIndex1 < imageXBuffer.length) {
                                if (doTrilinearRadio.isSelected()) {
                                    rgb1 = AlgorithmConvolver.getTrilinearC(floorPointIndex1, dx1, dy1, dz1, extents1,
                                            imageXBuffer);
                                    rgb1_short[0] = (short) (rgb1[0] & 0xff);
                                    rgb1_short[1] = (short) (rgb1[1] & 0xff);
                                    rgb1_short[2] = (short) (rgb1[2] & 0xff);
                                } else {
                                	double[] tempValues = bSplineX.bSpline3DC(0, 0, 0, 
                                			(float)tX1_floor, (float)tY1_floor, (float)tZ1_floor);
    								for ( int c = 0; c < 4; c++ )
    								{
    									if (tempValues[c] > 255) {
    										tempValues[c] = 255;
    									} else if (tempValues[c] < 0) {
    										tempValues[c] = 0;
    									}
    								}
                                    rgb1_short[0] = (short) (tempValues[1]);
                                    rgb1_short[1] = (short) (tempValues[2]);
                                    rgb1_short[2] = (short) (tempValues[3]);
                                	/*
                                    r1_float = splineAlgX_R.interpolatedValue(imageX_R_coeff, tX1_floor, tY1_floor,
                                            tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
                                    if (r1_float > 255) {
                                        r1_float = 255;
                                    } else if (r1_float < 0) {
                                        r1_float = 0;
                                    }

                                    g1_float = splineAlgX_G.interpolatedValue(imageX_G_coeff, tX1_floor, tY1_floor,
                                            tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
                                    if (g1_float > 255) {
                                        g1_float = 255;
                                    } else if (g1_float < 0) {
                                        g1_float = 0;
                                    }

                                    b1_float = splineAlgX_B.interpolatedValue(imageX_B_coeff, tX1_floor, tY1_floor,
                                            tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
                                    if (b1_float > 255) {
                                        b1_float = 255;
                                    } else if (b1_float < 0) {
                                        b1_float = 0;
                                    }

                                    rgb1_short[0] = (short) (r1_float);
                                    rgb1_short[1] = (short) (g1_float);
                                    rgb1_short[2] = (short) (b1_float);
                                    */
                                }
                            } else {
                                rgb1_short[0] = 0;
                                rgb1_short[1] = 0;
                                rgb1_short[2] = 0;
                            }
                        }

                        if (tPt2[0] < 0 || tPt2[1] < 0 || tPt2[2] < 0 || tPt2[0] > imageY.getExtents()[0] - 1
                                || tPt2[1] > imageY.getExtents()[1] - 1 || tPt2[2] > imageY.getExtents()[2] - 1) {
                            rgb2_short[0] = 0;
                            rgb2_short[1] = 0;
                            rgb2_short[2] = 0;
                        } else {
                            final double tX2_floor = Math.floor(tPt2[0]);
                            final double tY2_floor = Math.floor(tPt2[1]);
                            final double tZ2_floor = Math.floor(tPt2[2]);
                            final float dx2 = (float) (tPt2[0] - tX2_floor);
                            final float dy2 = (float) (tPt2[1] - tY2_floor);
                            final float dz2 = (float) (tPt2[2] - tZ2_floor);
                            final int[] extents2 = imageY.getExtents();
                            floorPointIndex2 = (int) ( ( (tZ2_floor * (extents2[0] * extents2[1]))
                                    + (tY2_floor * extents2[0]) + tX2_floor) * 4);
                            if (floorPointIndex2 < imageYBuffer.length) {
                                if (doTrilinearRadio.isSelected()) {
                                    rgb2 = AlgorithmConvolver.getTrilinearC(floorPointIndex2, dx2, dy2, dz2, extents2,
                                            imageYBuffer);
                                    rgb2_short[0] = (short) (rgb2[0] & 0xff);
                                    rgb2_short[1] = (short) (rgb2[1] & 0xff);
                                    rgb2_short[2] = (short) (rgb2[2] & 0xff);
                                } else {
                                	double[] tempValues = bSplineY.bSpline3DC(0, 0, 0, 
                                			(float)tX2_floor, (float)tY2_floor, (float)tZ2_floor);
    								for ( int c = 0; c < 4; c++ )
    								{
    									if (tempValues[c] > 255) {
    										tempValues[c] = 255;
    									} else if (tempValues[c] < 0) {
    										tempValues[c] = 0;
    									}
    								}
                                    rgb2_short[0] = (short) (tempValues[1]);
                                    rgb2_short[1] = (short) (tempValues[2]);
                                    rgb2_short[2] = (short) (tempValues[3]);
                                	/*
                                    r2_float = splineAlgY_R.interpolatedValue(imageY_R_coeff, tX2_floor, tY2_floor,
                                            tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
                                    if (r2_float > 255) {
                                        r2_float = 255;
                                    } else if (r2_float < 0) {
                                        r2_float = 0;
                                    }

                                    g2_float = splineAlgY_G.interpolatedValue(imageY_G_coeff, tX2_floor, tY2_floor,
                                            tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
                                    if (g2_float > 255) {
                                        g2_float = 255;
                                    } else if (g2_float < 0) {
                                        g2_float = 0;
                                    }

                                    b2_float = splineAlgY_B.interpolatedValue(imageY_B_coeff, tX2_floor, tY2_floor,
                                            tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
                                    if (b2_float > 255) {
                                        b2_float = 255;
                                    } else if (b2_float < 0) {
                                        b2_float = 0;
                                    }

                                    rgb2_short[0] = (short) (r2_float);
                                    rgb2_short[1] = (short) (g2_float);
                                    rgb2_short[2] = (short) (b2_float);
                                    */
                                }
                            } else {
                                rgb2_short[0] = 0;
                                rgb2_short[1] = 0;
                                rgb2_short[2] = 0;
                            }
                        }

                        byte avgR, avgG, avgB;

                        if (doAverageRadio.isSelected()) {
                            // dont do combining if other point is all background
                            if (ignoreBGRadio.isSelected()) {
                                if (rgb1_short[0] == 0 && rgb1_short[1] == 0 && rgb1_short[2] == 0) {
                                    avgR = (byte) rgb2_short[0];
                                    avgG = (byte) rgb2_short[1];
                                    avgB = (byte) rgb2_short[2];
                                } else if (rgb2_short[0] == 0 && rgb2_short[1] == 0 && rgb2_short[2] == 0) {
                                    avgR = (byte) rgb1_short[0];
                                    avgG = (byte) rgb1_short[1];
                                    avgB = (byte) rgb1_short[2];
                                } else {
                                    // averaging
                                    avgR = (byte) Math.round( ( (rgb1_short[0] + rgb2_short[0]) / 2.0f));
                                    avgG = (byte) Math.round( ( (rgb1_short[1] + rgb2_short[1]) / 2.0f));
                                    avgB = (byte) Math.round( ( (rgb1_short[2] + rgb2_short[2]) / 2.0f));
                                }
                            } else {
                                // averaging
                                avgR = (byte) Math.round( ( (rgb1_short[0] + rgb2_short[0]) / 2.0f));
                                avgG = (byte) Math.round( ( (rgb1_short[1] + rgb2_short[1]) / 2.0f));
                                avgB = (byte) Math.round( ( (rgb1_short[2] + rgb2_short[2]) / 2.0f));
                            }
                        } else if (doSqRtRadio.isSelected()) {
                            // dont do combining if other point is all background
                            if (ignoreBGRadio.isSelected()) {
                                if (rgb1_short[0] == 0 && rgb1_short[1] == 0 && rgb1_short[2] == 0) {
                                    avgR = (byte) rgb2_short[0];
                                    avgG = (byte) rgb2_short[1];
                                    avgB = (byte) rgb2_short[2];
                                } else if (rgb2_short[0] == 0 && rgb2_short[1] == 0 && rgb2_short[2] == 0) {
                                    avgR = (byte) rgb1_short[0];
                                    avgG = (byte) rgb1_short[1];
                                    avgB = (byte) rgb1_short[2];
                                } else {
                                    // doing Sqrt (Intensity X * Intensity Y)
                                    avgR = (byte) Math.sqrt(rgb1_short[0] * rgb2_short[0]);
                                    avgG = (byte) Math.sqrt(rgb1_short[1] * rgb2_short[1]);
                                    avgB = (byte) Math.sqrt(rgb1_short[2] * rgb2_short[2]);
                                }
                            } else {
                                // doing Sqrt (Intensity X * Intensity Y)
                                avgR = (byte) Math.sqrt(rgb1_short[0] * rgb2_short[0]);
                                avgG = (byte) Math.sqrt(rgb1_short[1] * rgb2_short[1]);
                                avgB = (byte) Math.sqrt(rgb1_short[2] * rgb2_short[2]);
                            }
                        }else {
                        	//doing weighted
                        	if (ignoreBGRadio.isSelected()) {
                                if (rgb1_short[0] == 0 && rgb1_short[1] == 0 && rgb1_short[2] == 0) {
                                    avgR = (byte) rgb2_short[0];
                                    avgG = (byte) rgb2_short[1];
                                    avgB = (byte) rgb2_short[2];
                                } else if (rgb2_short[0] == 0 && rgb2_short[1] == 0 && rgb2_short[2] == 0) {
                                    avgR = (byte) rgb1_short[0];
                                    avgG = (byte) rgb1_short[1];
                                    avgB = (byte) rgb1_short[2];
                                } else {
                                    // doing weighted ((IntensityX/IntensityX+IntensityY)*IntensityX   +   (IntensityY/IntensityX+IntensityY)*IntensityY
                                	float weightR1;
                                	float weightR2;
                                	float weightG1;
                                	float weightG2;
                                	float weightB1;
                                	float weightB2;
                                	
                                	if(rgb1_short[0] == 0 && rgb2_short[0] == 0) {
                                		weightR1 = 0;
                                		weightR2 = 0;
                                	}else {
                                		weightR1 = rgb1_short[0] / (float)(rgb1_short[0] + rgb2_short[0]);
                                		weightR2 = rgb2_short[0] / (float)(rgb1_short[0] + rgb2_short[0]);
                                	}
                                	
                                	if(rgb1_short[1] == 0 && rgb2_short[1] == 0) {
                                		weightG1 = 0;
                                		weightG2 = 0;
                                	}else {
                                		weightG1 = rgb1_short[1] / (float)(rgb1_short[1] + rgb2_short[1]);
                                		weightG2 = rgb2_short[1] / (float)(rgb1_short[1] + rgb2_short[1]);
                                	}
                                    
                                	if(rgb1_short[2] == 0 && rgb2_short[2] == 0) {
                                		weightB1 = 0;
                                		weightB2 = 0;
                                	}else {
                                		weightB1 = rgb1_short[2] / (float)(rgb1_short[2] + rgb2_short[2]);
                                		weightB2 = rgb2_short[2] / (float)(rgb1_short[2] + rgb2_short[2]);
                                	}
                                    
                                    
                                	
                                	avgR = (byte)((weightR1 * rgb1_short[0]) + (weightR2 * rgb2_short[0]));
                                    avgG = (byte)((weightG1 * rgb1_short[1]) + (weightG2 * rgb2_short[1]));
                                    avgB = (byte)((weightB1 * rgb1_short[2]) + (weightB2 * rgb2_short[2]));
                                }
                            } else {
                            	// doing weighted ((IntensityX/IntensityX+IntensityY)*IntensityX   +   (IntensityY/IntensityX+IntensityY)*IntensityY
                            	float weightR1;
                            	float weightR2;
                            	float weightG1;
                            	float weightG2;
                            	float weightB1;
                            	float weightB2;
                            	
                            	if(rgb1_short[0] == 0 && rgb2_short[0] == 0) {
                            		weightR1 = 0;
                            		weightR2 = 0;
                            	}else {
                            		weightR1 = rgb1_short[0] / (float)(rgb1_short[0] + rgb2_short[0]);
                            		weightR2 = rgb2_short[0] / (float)(rgb1_short[0] + rgb2_short[0]);
                            	}
                            	
                            	if(rgb1_short[1] == 0 && rgb2_short[1] == 0) {
                            		weightG1 = 0;
                            		weightG2 = 0;
                            	}else {
                            		weightG1 = rgb1_short[1] / (float)(rgb1_short[1] + rgb2_short[1]);
                            		weightG2 = rgb2_short[1] / (float)(rgb1_short[1] + rgb2_short[1]);
                            	}
                                
                            	if(rgb1_short[2] == 0 && rgb2_short[2] == 0) {
                            		weightB1 = 0;
                            		weightB2 = 0;
                            	}else {
                            		weightB1 = rgb1_short[2] / (float)(rgb1_short[2] + rgb2_short[2]);
                            		weightB2 = rgb2_short[2] / (float)(rgb1_short[2] + rgb2_short[2]);
                            	}
                                
                                
                            	
                            	avgR = (byte)((weightR1 * rgb1_short[0]) + (weightR2 * rgb2_short[0]));
                                avgG = (byte)((weightG1 * rgb1_short[1]) + (weightG2 * rgb2_short[1]));
                                avgB = (byte)((weightB1 * rgb1_short[2]) + (weightB2 * rgb2_short[2]));
                            }
                        	
                        }

                        // alpha
                        resultBuffer[index] = (byte) 1;
                        // r
                        index = index + 1;
                        resultBuffer[index] = avgR;
                        // g
                        index = index + 1;
                        resultBuffer[index] = avgG;
                        // b
                        index = index + 1;
                        resultBuffer[index] = avgB;
                        index = index + 1;
                    //}
                    /* else { // CLOSEST Z
                        // look at z transformed points
                        double diff1, diff2;

                        if (tPt1[2] - Math.floor(tPt1[2]) <= .5) {
                            diff1 = tPt1[2] - Math.floor(tPt1[2]);
                        } else {
                            diff1 = Math.ceil(tPt1[2]) - tPt1[2];
                        }

                        if (tPt2[2] - Math.floor(tPt2[2]) <= .5) {
                            diff2 = tPt2[2] - Math.floor(tPt2[2]);
                        } else {
                            diff2 = Math.ceil(tPt2[2]) - tPt2[2];
                        }

                        diff1 = diff1 * imageX.getResolutions(0)[2];
                        diff2 = diff2 * imageY.getResolutions(0)[2];

                        // get linear interpolated values from both transformed points
                        if (tPt1[0] < 0 || tPt1[1] < 0 || tPt1[2] < 0 || tPt1[0] > imageX.getExtents()[0] - 1
                                || tPt1[1] > imageX.getExtents()[1] - 1 || tPt1[2] > imageX.getExtents()[2] - 1) {
                            rgb1_short[0] = 0;
                            rgb1_short[1] = 0;
                            rgb1_short[2] = 0;

                        } else {
                            final double tX1_floor = Math.floor(tPt1[0]);
                            final double tY1_floor = Math.floor(tPt1[1]);
                            final double tZ1_floor = Math.floor(tPt1[2]);
                            final float dx1 = (float) (tPt1[0] - tX1_floor);
                            final float dy1 = (float) (tPt1[1] - tY1_floor);
                            final float dz1 = (float) (tPt1[2] - tZ1_floor);
                            final int[] extents1 = imageX.getExtents();
                            floorPointIndex1 = (int) ( ( (tZ1_floor * (extents1[0] * extents1[1]))
                                    + (tY1_floor * extents1[0]) + tX1_floor) * 4);
                            if (doTrilinearRadio.isSelected()) {
                                rgb1 = AlgorithmConvolver.getTrilinearC(floorPointIndex1, dx1, dy1, dz1, extents1,
                                        imageXBuffer);
                                rgb1_short[0] = (short) (rgb1[0] & 0xff);
                                rgb1_short[1] = (short) (rgb1[1] & 0xff);
                                rgb1_short[2] = (short) (rgb1[2] & 0xff);
                            } else {
                            	float[] tempValues = bSplineY.bSpline3DC(0, 0, 0, 
                            			(float)tX1_floor, (float)tY1_floor, (float)tZ1_floor);
								for ( int c = 0; c < 4; c++ )
								{
									if (tempValues[c] > 255) {
										tempValues[c] = 255;
									} else if (tempValues[c] < 0) {
										tempValues[c] = 0;
									}
								}
                                rgb1_short[0] = (short) (tempValues[1]);
                                rgb1_short[1] = (short) (tempValues[2]);
                                rgb1_short[2] = (short) (tempValues[3]);
                            	
                                r1_float = splineAlgX_R.interpolatedValue(imageX_R_coeff, tX1_floor, tY1_floor,
                                        tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
                                if (r1_float > 255) {
                                    r1_float = 255;
                                } else if (r1_float < 0) {
                                    r1_float = 0;
                                }

                                g1_float = splineAlgX_G.interpolatedValue(imageX_G_coeff, tX1_floor, tY1_floor,
                                        tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
                                if (g1_float > 255) {
                                    g1_float = 255;
                                } else if (g1_float < 0) {
                                    g1_float = 0;
                                }

                                b1_float = splineAlgX_B.interpolatedValue(imageX_B_coeff, tX1_floor, tY1_floor,
                                        tZ1_floor, extents1[0], extents1[1], extents1[2], 3);
                                if (b1_float > 255) {
                                    b1_float = 255;
                                } else if (b1_float < 0) {
                                    b1_float = 0;
                                }

                                rgb1_short[0] = (short) (r1_float);
                                rgb1_short[1] = (short) (g1_float);
                                rgb1_short[2] = (short) (b1_float);
                                
                            }
                        }

                        if (tPt2[0] < 0 || tPt2[1] < 0 || tPt2[2] < 0 || tPt2[0] > imageY.getExtents()[0] - 1
                                || tPt2[1] > imageY.getExtents()[1] - 1 || tPt2[2] > imageY.getExtents()[2] - 1) {
                            rgb2_short[0] = 0;
                            rgb2_short[1] = 0;
                            rgb2_short[2] = 0;
                        } else {
                            final double tX2_floor = Math.floor(tPt2[0]);
                            final double tY2_floor = Math.floor(tPt2[1]);
                            final double tZ2_floor = Math.floor(tPt2[2]);
                            final float dx2 = (float) (tPt2[0] - tX2_floor);
                            final float dy2 = (float) (tPt2[1] - tY2_floor);
                            final float dz2 = (float) (tPt2[2] - tZ2_floor);
                            final int[] extents2 = imageY.getExtents();
                            floorPointIndex2 = (int) ( ( (tZ2_floor * (extents2[0] * extents2[1]))
                                    + (tY2_floor * extents2[0]) + tX2_floor) * 4);
                            if (doTrilinearRadio.isSelected()) {
                                rgb2 = AlgorithmConvolver.getTrilinearC(floorPointIndex2, dx2, dy2, dz2, extents2,
                                        imageYBuffer);
                                rgb2_short[0] = (short) (rgb2[0] & 0xff);
                                rgb2_short[1] = (short) (rgb2[1] & 0xff);
                                rgb2_short[2] = (short) (rgb2[2] & 0xff);
                            } else {
                            	float[] tempValues = bSplineY.bSpline3DC(0, 0, 0, 
                            			(float)tX2_floor, (float)tY2_floor, (float)tZ2_floor);
								for ( int c = 0; c < 4; c++ )
								{
									if (tempValues[c] > 255) {
										tempValues[c] = 255;
									} else if (tempValues[c] < 0) {
										tempValues[c] = 0;
									}
								}
                                rgb2_short[0] = (short) (tempValues[1]);
                                rgb2_short[1] = (short) (tempValues[2]);
                                rgb2_short[2] = (short) (tempValues[3]);
                                
                                r2_float = splineAlgY_R.interpolatedValue(imageY_R_coeff, tX2_floor, tY2_floor,
                                        tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
                                if (r2_float > 255) {
                                    r2_float = 255;
                                } else if (r2_float < 0) {
                                    r2_float = 0;
                                }

                                g2_float = splineAlgY_G.interpolatedValue(imageY_G_coeff, tX2_floor, tY2_floor,
                                        tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
                                if (g2_float > 255) {
                                    g2_float = 255;
                                } else if (g2_float < 0) {
                                    g2_float = 0;
                                }

                                b2_float = splineAlgY_B.interpolatedValue(imageY_B_coeff, tX2_floor, tY2_floor,
                                        tZ2_floor, extents2[0], extents2[1], extents2[2], 3);
                                if (b2_float > 255) {
                                    b2_float = 255;
                                } else if (b2_float < 0) {
                                    b2_float = 0;
                                }

                                rgb2_short[0] = (short) (r2_float);
                                rgb2_short[1] = (short) (g2_float);
                                rgb2_short[2] = (short) (b2_float);
                                
                            }
                        }

                        byte r, g, b;

                        if (diff1 < diff2) {

                            r = (byte) rgb1_short[0];
                            g = (byte) rgb1_short[1];
                            b = (byte) rgb1_short[2];
                        } else {

                            r = (byte) rgb2_short[0];
                            g = (byte) rgb2_short[1];
                            b = (byte) rgb2_short[2];
                        }

                        // alpha
                        resultBuffer[index] = (byte) 255;
                        // r
                        index = index + 1;
                        resultBuffer[index] = r;
                        // g
                        index = index + 1;
                        resultBuffer[index] = g;
                        // b
                        index = index + 1;
                        resultBuffer[index] = b;
                        index = index + 1;
                    }*/
                }
            }
        }

        outputTextArea.append("\n");

        try {
            resultImage.importData(0, resultBuffer, true);
        } catch (final IOException error) {
            System.out.println("IO exception");
            error.printStackTrace();
            return;
        }

        final FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[resultImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(resultImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(imageY.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(imageY.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(resultImageResols);
            fileInfoBases[i].setExtents(resultImage.getExtents());
            fileInfoBases[i].setImageOrientation(imageY.getFileInfo()[0].getImageOrientation());
            fileInfoBases[i].setAxisOrientation(imageY.getFileInfo()[0].getAxisOrientation());
            fileInfoBases[i].setOrigin(imageY.getFileInfo()[0].getOrigin());
            fileInfoBases[i].setPixelPadValue(imageY.getFileInfo()[0].getPixelPadValue());
            fileInfoBases[i].setPhotometric(imageY.getFileInfo()[0].getPhotometric());
            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);
            fileInfoBases[i].setFileDirectory(imageY.getFileInfo()[0].getFileDirectory());
        }

        resultImage.setFileInfo(fileInfoBases);
        resultImage.calcMinMax();

        if (imageX != null) {
            imageX.disposeLocal();
            imageX = null;
        }
        if (vjfX != null) {
            vjfX.close();
        }

        // create red and green channels image
        createRedAndGreenChannelsImages();

        if (imageY != null) {
            imageY.disposeLocal();
            imageY = null;
        }

        if (vjfY != null) {
            vjfY.close();
        }

        final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);
    }

    /**
     * creates additional red channels and green channels images
     */
    private void createRedAndGreenChannelsImages() {
        outputTextArea.append("creating red and green channel images..." + "\n");
        outputTextArea.append("\n");
        TransMatrix xfrm = new TransMatrix(4);
        xfrm.MakeIdentity();
        int interp = 0; // trilinear interp
        float oXres = imageXRegistered.getResolutions(0)[0];
        float oYres = imageXRegistered.getResolutions(0)[1];
        float oZres = imageXRegistered.getResolutions(0)[2] * (imageXRegistered.getExtents()[2] / 512f);
        int oXdim = 512;
        int oYdim = 512;
        int oZdim = 512;
        int[] units = new int[imageXRegistered.getUnitsOfMeasure().length];
        for (int i = 0; i < units.length; i++) {
            units[i] = imageXRegistered.getUnitsOfMeasure(i);
        }
        boolean doVOI = false;
        boolean doClip = true;
        boolean doPad = false;
        boolean doRotateCenter = true;
        Vector3f center = imageXRegistered.getImageCentermm(false);
        float fillValue = 0.0f;
        boolean doUpdateOrigin = true;
        boolean isSATransform = false;

        algoTrans = new AlgorithmTransform(imageXRegistered, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim,
                units, doVOI, doClip, doPad, doRotateCenter, center);
        algoTrans.setFillValue(fillValue);
        algoTrans.setUpdateOriginFlag(doUpdateOrigin);
        algoTrans.setUseScannerAnatomical(isSATransform);
        algoTrans.run();
        imageXRegisteredTransformed = algoTrans.getTransformedImage();
        imageXRegisteredTransformed.calcMinMax();

        // now we can dispose of imageXRegisterd
        if (imageXRegistered != null) {
            imageXRegistered.disposeLocal();
            imageXRegistered = null;
        }

        final int[] extents = {512, 512, 512};
        redChannelsImage = new ModelImage(ModelStorageBase.ARGB, extents, "redChannelsImage-Rxreg-Ry-Rcomp");
        final float[] redChannelsImageResols = new float[3];
        redChannelsImageResols[0] = imageYRes[0];
        redChannelsImageResols[1] = imageYRes[1];
        redChannelsImageResols[2] = imageYRes[2] * imageY.getExtents()[2] / 512;
        for (int i = 0; i < redChannelsImage.getExtents()[2]; i++) {
            redChannelsImage.setResolutions(i, redChannelsImageResols);
        }
        final byte[] redChannelsBuffer = new byte[512 * 512 * 512 * 4];

        greenChannelsImage = new ModelImage(ModelStorageBase.ARGB, extents, "greenChannelsImage-Gxreg-Gy-Gcomp");
        final float[] greenChannelsImageResols = new float[3];
        greenChannelsImageResols[0] = imageYRes[0];
        greenChannelsImageResols[1] = imageYRes[1];
        greenChannelsImageResols[2] = imageYRes[2] * imageY.getExtents()[2] / 512;
        for (int i = 0; i < greenChannelsImage.getExtents()[2]; i++) {
            greenChannelsImage.setResolutions(i, greenChannelsImageResols);
        }
        final byte[] greenChannelsBuffer = new byte[512 * 512 * 512 * 4];

        byte imageXRegisteredTransformedByteR, imageYTransformedByteR, compImageByteR, imageXRegisteredTransformedByteG, imageYTransformedByteG, compImageByteG;

        int a, r, g, b;

        for (int i = 0; i < redChannelsBuffer.length; i = i + 4) {
            a = i;
            r = i + 1;
            g = i + 2;
            b = i + 3;
            // imageYTransformedByteR = imageYTransformed.getByte(r);
            compImageByteR = resultImage.getByte(r);
            imageXRegisteredTransformedByteR = imageXRegisteredTransformed.getByte(r);

            // imageYTransformedByteG = imageYTransformed.getByte(g);
            compImageByteG = resultImage.getByte(g);
            imageXRegisteredTransformedByteG = imageXRegisteredTransformed.getByte(g);

            // alpha
            redChannelsBuffer[a] = (byte) 255;
            // channel 1
            redChannelsBuffer[r] = imageXRegisteredTransformedByteR;
            // channel 2
            // redChannelsBuffer[g] = imageYTransformedByteR;
            // channel 3
            redChannelsBuffer[b] = compImageByteR;

            // alpha
            greenChannelsBuffer[a] = (byte) 255;
            // channel 1
            greenChannelsBuffer[r] = imageXRegisteredTransformedByteG;
            // channel 2
            // greenChannelsBuffer[g] = imageYTransformedByteG;
            // channel 3
            greenChannelsBuffer[b] = compImageByteG;

        }

        // now we can save result image and then dispose it and dispose of imageXRegisteredTransformed
        if (imageXRegisteredTransformed != null) {
            imageXRegisteredTransformed.disposeLocal();
            imageXRegisteredTransformed = null;
        }

        final float[] orig = imageXOrig;

        // SAVE THE RESULT IMAGE
        String resultImageFileName = "combinedImage";
        String processString, interpString, rescaleString, bgString;
        if (doSqRtRadio.isSelected()) {
            processString = "_sqrRt";
        } else if(doAverageRadio.isSelected()){
            processString = "_avg";
        } else {
        	processString = "_weighted";
        }
        if (doTrilinearRadio.isSelected()) {
            interpString = "_trilinear";
        } else {
            interpString = "_bspline";
        }
        if (doRescaleRadio.isSelected()) {
            rescaleString = "_rescale";
        } else {
            rescaleString = "_norescale";
        }
        if (ignoreBGRadio.isSelected()) {
            bgString = "_ignoreBG";
        } else {
            bgString = "_includeBG";
        }
        FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);
        FileWriteOptions opts = new FileWriteOptions(true);
        opts.setFileType(FileUtility.ICS);
        opts.setFileDirectory(dir);
        resultImageFileName = resultImageFileName + processString + interpString + rescaleString + bgString;
        opts.setFileName(resultImageFileName + ".ics");
        opts.setBeginSlice(0);
        opts.setEndSlice(511);
        opts.setOptionsSet(true);
        fileIO.writeImage(resultImage, opts);
        outputTextArea.append("saving combined result image as: \n");
        outputTextArea.append(dir + resultImageFileName + ".ics" + "\n");
        outputTextArea.append("\n");
        
        if(createMirroredImg) {
        	ModelImage clonedResultImage = (ModelImage)resultImage.clone();
        	
        	AlgorithmFlip flipAlgo = new AlgorithmFlip(clonedResultImage, AlgorithmFlip.Y_AXIS, AlgorithmFlip.IMAGE, false);
        	flipAlgo.run();
        	
        	
        	opts = new FileWriteOptions(true);
            opts.setFileType(FileUtility.ICS);
            opts.setFileDirectory(dir);
            resultImageFileName = resultImageFileName + processString + interpString + rescaleString + bgString + "_MIRRORED";
            opts.setFileName(resultImageFileName + ".ics");
            opts.setBeginSlice(0);
            opts.setEndSlice(511);
            opts.setOptionsSet(true);
            fileIO.writeImage(clonedResultImage, opts);
            outputTextArea.append("saving mirrored result image as: \n");
            outputTextArea.append(dir + resultImageFileName + ".ics" + "\n");
            outputTextArea.append("\n");
            
            if (clonedResultImage != null) {
            	clonedResultImage.disposeLocal();
            	clonedResultImage = null;
            }
            
            flipAlgo.finalize();
            flipAlgo = null;
            
        	
        }
        
        
        
        
        

        if (resultImage != null) {
            resultImage.disposeLocal();
            resultImage = null;
        }

        xfrm = new TransMatrix(4);
        xfrm.MakeIdentity();
        interp = 0; // trilinear interp
        oXres = imageY.getResolutions(0)[0];
        oYres = imageY.getResolutions(0)[1];
        oZres = imageY.getResolutions(0)[2] * (imageY.getExtents()[2] / 512f);
        oXdim = 512;
        oYdim = 512;
        oZdim = 512;
        units = new int[imageY.getUnitsOfMeasure().length];
        for (int i = 0; i < units.length; i++) {
            units[i] = imageY.getUnitsOfMeasure(i);
        }
        doVOI = false;
        doClip = true;
        doPad = false;
        doRotateCenter = true;
        center = imageY.getImageCentermm(false);
        fillValue = 0.0f;
        doUpdateOrigin = true;
        isSATransform = false;

        // imageYTransform
        algoTrans = new AlgorithmTransform(imageY, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim, units,
                doVOI, doClip, doPad, doRotateCenter, center);
        algoTrans.setFillValue(fillValue);
        algoTrans.setUpdateOriginFlag(doUpdateOrigin);
        algoTrans.setUseScannerAnatomical(isSATransform);
        algoTrans.run();
        imageYTransformed = algoTrans.getTransformedImage();

        // new ViewJFrameImage(imageYTransformed);

        // now we can dispose of imageY
        if (imageY != null) {
            imageY.disposeLocal();
            imageY = null;
        }

        for (int i = 0; i < redChannelsBuffer.length; i = i + 4) {
            a = i;
            r = i + 1;
            g = i + 2;
            b = i + 3;
            imageYTransformedByteR = imageYTransformed.getByte(r);

            imageYTransformedByteG = imageYTransformed.getByte(g);

            // alpha
            // redChannelsBuffer[a] = (byte)255;
            // channel 1
            // redChannelsBuffer[r] = imageXRegisteredTransformedByteR;
            // channel 2
            redChannelsBuffer[g] = imageYTransformedByteR;
            // channel 3
            // redChannelsBuffer[b] = compImageByteR;

            // alpha
            // greenChannelsBuffer[a] = (byte)255;
            // channel 1
            // greenChannelsBuffer[r] = imageXRegisteredTransformedByteG;
            // channel 2
            greenChannelsBuffer[g] = imageYTransformedByteG;
            // channel 3
            // greenChannelsBuffer[b] = compImageByteG;

        }

        // now we can dispose of imageYTransformed
        if (imageYTransformed != null) {
            imageYTransformed.disposeLocal();
            imageYTransformed = null;
        }

        try {
            redChannelsImage.importData(0, redChannelsBuffer, true);
            greenChannelsImage.importData(0, greenChannelsBuffer, true);
        } catch (final IOException error) {
            System.out.println("IO exception");
            error.printStackTrace();
            // setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
            return;
        }

        FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[redChannelsImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(redChannelsImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(imageYEnd);
            fileInfoBases[i].setUnitsOfMeasure(units);
            fileInfoBases[i].setResolutions(redChannelsImageResols);
            fileInfoBases[i].setExtents(redChannelsImage.getExtents());
            fileInfoBases[i].setOrigin(orig);
            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);
            fileInfoBases[i].setFileDirectory(dir);
        }

        redChannelsImage.setFileInfo(fileInfoBases);
        redChannelsImage.calcMinMax();

        final String redImageFileName = redChannelsImage.getImageName();
        opts = new FileWriteOptions(true);
        opts.setFileType(FileUtility.ICS);
        opts.setFileDirectory(dir);
        opts.setFileName(redChannelsImage.getImageName() + ".ics");
        opts.setBeginSlice(0);
        opts.setEndSlice(511);
        opts.setOptionsSet(true);
        fileIO.writeImage(redChannelsImage, opts);

        outputTextArea.append("saving red channels image as: \n");
        outputTextArea.append(dir + redImageFileName + ".ics" + "\n");
        outputTextArea.append("\n");

        if (redChannelsImage != null) {
            redChannelsImage.disposeLocal();
            redChannelsImage = null;
        }

        fileInfoBases = new FileInfoImageXML[greenChannelsImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(greenChannelsImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(imageYEnd);
            fileInfoBases[i].setUnitsOfMeasure(units);
            fileInfoBases[i].setResolutions(redChannelsImageResols);
            fileInfoBases[i].setExtents(greenChannelsImage.getExtents());
            fileInfoBases[i].setOrigin(orig);
            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);
            fileInfoBases[i].setFileDirectory(dir);
        }

        greenChannelsImage.setFileInfo(fileInfoBases);
        greenChannelsImage.calcMinMax();

        final String greenImageFileName = greenChannelsImage.getImageName();
        opts = new FileWriteOptions(true);
        opts.setFileType(FileUtility.ICS);
        opts.setFileDirectory(dir);
        opts.setFileName(greenChannelsImage.getImageName() + ".ics");
        opts.setBeginSlice(0);
        opts.setEndSlice(511);
        opts.setOptionsSet(true);
        fileIO.writeImage(greenChannelsImage, opts);

        outputTextArea.append("saving green channels image as: \n");
        outputTextArea.append(dir + greenImageFileName + ".ics" + "\n");
        outputTextArea.append("\n");

        if (greenChannelsImage != null) {
            greenChannelsImage.disposeLocal();
            greenChannelsImage = null;
        }

    }

    /**
     * This legacy code returns all active vois for a given source image. PlugIns should explicitly identify VOIs they
     * would like to process using AlgorithmVOIProps, because the user may have already added other VOIs to srcImage, or
     * VOIs may be created by the algorithm in an unexpected way. This plugin relied on <code>AlgorithmVOIProp</code>'s
     * getActiveVOIs() code, so that code has been moved into this plugin.
     * 
     * Use of this method is discouraged, as shown by the old documentation for this method: not for use. should be
     * moved to a better location. does NOT clone the VOIs that it find to be active, and inserts into a new
     * ViewVOIVector. if no VOIs are active, the ViewVOIVector returned is <code>null</code>.
     * 
     * @return All the active VOIs for a given srcImage.
     */
    private ViewVOIVector getActiveVOIs(final ModelImage srcImage) {
        ViewVOIVector voiList;

        voiList = new ViewVOIVector();

        int i;

        try {

            for (i = 0; i < srcImage.getVOIs().size(); i++) {

                if (srcImage.getVOIs().VOIAt(i).isActive()) {

                    // voi at i is the active voi
                    voiList.addElement(srcImage.getVOIs().VOIAt(i));
                }
            }
        } catch (final ArrayIndexOutOfBoundsException indexException) {

            // got to the end of list and never found an active VOI.
            // return an empty VOI list.
            return new ViewVOIVector();
        }

        return voiList;
    }

    /**
     * calculates slop based on 4 data points
     * 
     * @param Y1
     * @param X1
     * @param Y2
     * @param X2
     * @return
     */
    private float calculateSlope(final float Y1, final float X1, final float Y2, final float X2) {
        float slope = 0;
        final float Y = Y2 - Y1;
        final float X = X2 - X1;
        if (X == 0) {
            slope = 0;
        } else {
            slope = Y / X;
        }
        return slope;
    }

    /**
     * calculates b-intercept
     * 
     * @param Y1
     * @param X1
     * @param slope
     * @return
     */
    private float calculateB(final float Y1, final float X1, final float slope) {
        float b = 0;
        final float mx = X1 * slope;
        b = Y1 - mx;
        return b;
    }

    /**
     * gets new value based on slope and b-intercept
     * 
     * @param X
     * @param slope
     * @param b
     * @return
     */
    private float getNewValue(final float X, final float slope, final float b) {
        float Y = 0;
        final float mx = slope * X;
        Y = mx + b;
        return Y;
    }

}
