import gov.nih.mipav.util.MipavMath;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegLeastSquares;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmRGBtoGray;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.*;

import javax.swing.JTextArea;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This plugin was done for Dr Chi-hon Lee of NICHD
 * 
 * This plugin basically standardizes the output from the DrosophilaRetinalRegistration to a standard model
 * 
 * It also standardizes an input surface file
 * 
 * It also allows for inverting the output surface file
 * 
 * @author pandyan
 */
public class PlugInAlgorithmDrosophilaStandardColumnRegistration extends AlgorithmBase {

    /** model images * */
    private ModelImage standardColumnImage, neuronImage, neuronImage_grey, resultImage1, finalImage, cityBlockImage;

    /** resolutions * */
    private final float[] resols;

    /** Dimensions of match image and base image. */
    private int xdimA, ydimA, zdimA;

    /** Resolutions of match image and base image. */
    private float xresA, yresA, zresA, xresB, yresB, zresB;

    /** least squared alg * */
    private AlgorithmRegLeastSquares LSMatch;

    /** num dims * */
    private final int DIM = 3;

    /** fill value * */
    private final float fillValue = 0.0f;

    /** points to remove * */
    private final ArrayList<String> removePointsLabels = new ArrayList<String>();

    /** points collection * */
    private final TreeMap<Integer, float[]> pointsMap;

    /** xSource */
    private double[] xSource;

    /** xTar */
    private double[] xTar;

    /** ySource */
    private double[] ySource;

    /** yTar */
    private double[] yTar;

    /** zSource */
    private double[] zSource;

    /** zTar! */
    private double[] zTar;

    /** tp spline alg * */
    private AlgorithmTPSpline spline;

    /** file info * */
    private FileInfoBase fInfoBase;

    /** red value weighting * */
    private final float redValue = 1.0f / 3.0f;

    /** green value weighting * */
    private final float greenValue = 1.0f / 3.0f;

    /** blue value weighting * */
    private final float blueValue = 1.0f / 3.0f;

    /** threshold average * */
    private final boolean thresholdAverage = false;

    /** intensity average * */
    private final boolean intensityAverage = false;

    /** threshold * */
    private final float threshold = 0.0f;

    /** dir where neuron image is * */
    private final String dir;

    /** least squares - rigid matrix * */
    private TransMatrix lsMatrix;

    /** coords of filament * */
    private final ArrayList<ArrayList<float[]>> allFilamentCoords;

    /** new coords of filment * */
    private final ArrayList<ArrayList<float[]>> allFilamentCoords_newCoords;

    /** tolerances used * */
    private final double tolerance, toleranceSq;

    /** old surface file * */
    private final File oldSurfaceFile;

    /** sampling rate * */
    private final float samplingRate;

    /** neuraon image extents * */
    private final int[] neuronImageExtents;

    /** r7 center coord * */
    private float[] r7_27Coord;

    /** r7 center coord transformed * */
    private float[] r7_27Coord_transformed;

    /** if r7 center point was found * */
    private boolean r7CenterPointFound = false;

    /** input points * */
    private final ArrayList<float[]> inputPointsList = new ArrayList<float[]>();

    /** transformed points * */
    private final ArrayList<float[]> transformedPointsList = new ArrayList<float[]>();

    /** points file * */
    private final File pointsFile;

    /** output text area * */
    private final JTextArea outputTextArea;

    /** whether to flip or not * */
    private final boolean flipX, flipY, flipZ;

    /**
     * constuctor
     * 
     * @param neuronImage
     * @param pointsMap
     */
    public PlugInAlgorithmDrosophilaStandardColumnRegistration(final ModelImage neuronImage,
            final TreeMap<Integer, float[]> pointsMap, final ArrayList<ArrayList<float[]>> allFilamentCoords,
            final File oldSurfaceFile, final float samplingRate, final ModelImage cityBlockImage,
            final File pointsFile, final JTextArea outputTextArea, final boolean flipX, final boolean flipY,
            final boolean flipZ) {
        this.neuronImage = neuronImage;
        this.neuronImageExtents = neuronImage.getExtents();
        dir = neuronImage.getImageDirectory();
        // create neuron grey image
        createGreyImage();
        this.pointsMap = pointsMap;
        resols = neuronImage.getResolutions(0);
        this.allFilamentCoords = allFilamentCoords;
        allFilamentCoords_newCoords = new ArrayList<ArrayList<float[]>>();
        for (int i = 0; i < allFilamentCoords.size(); i++) {
            final ArrayList<float[]> al = allFilamentCoords.get(i);
            final int size = al.size();
            final ArrayList<float[]> al_new = new ArrayList<float[]>();

            for (int k = 0; k < size; k++) {
                al_new.add(k, null);
            }

            allFilamentCoords_newCoords.add(al_new);
        }

        final double sqrRtThree = Math.sqrt(3);

        tolerance = (sqrRtThree / 2)
                * ( ( (resols[0] / resols[0]) + (resols[1] / resols[0]) + (resols[2] / resols[0])) / 3);

        toleranceSq = tolerance * tolerance;

        this.oldSurfaceFile = oldSurfaceFile;
        this.samplingRate = samplingRate;
        this.cityBlockImage = cityBlockImage;
        this.pointsFile = pointsFile;
        this.outputTextArea = outputTextArea;
        this.flipX = flipX;
        this.flipY = flipY;
        this.flipZ = flipZ;

    }

    /**
     * run algorithm
     */
    public void runAlgorithm() {
        outputTextArea.append("Running Algorithm v2.1" + "\n");

        final long begTime = System.currentTimeMillis();

        final int[] extents = {512, 512, 512};
        standardColumnImage = new ModelImage(ModelStorageBase.UBYTE, extents, "standardColumnImage");
        for (int i = 0; i < standardColumnImage.getExtents()[2]; i++) {
            standardColumnImage.setResolutions(i, resols);
        }

        final FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[standardColumnImage.getExtents()[2]];
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(standardColumnImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setEndianess(neuronImage.getFileInfo()[0].getEndianess());
            fileInfoBases[i].setUnitsOfMeasure(neuronImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(neuronImage.getFileInfo()[0].getResolutions());
            fileInfoBases[i].setExtents(neuronImage.getExtents());
            fileInfoBases[i].setOrigin(neuronImage.getFileInfo()[0].getOrigin());

        }

        standardColumnImage.setFileInfo(fileInfoBases);

        VOI newPtVOI = null;
        final float[] x = new float[1];
        final float[] y = new float[1];
        final float[] z = new float[1];

        // STANDARD COLUMN IMAGE
        newPtVOI = new VOI((short) 0, "point3D.voi", 512, VOI.POINT, -1.0f);
        newPtVOI.setUID(newPtVOI.hashCode());
        standardColumnImage.registerVOI(newPtVOI);
        Vector<VOIBase>[] curves = newPtVOI.getCurvesTemp();

        // std column vals
        // top
        // -7.637 0 0
        // -3.819 -3.819 0
        // 0 -7.637 0
        // 3.819 -3.819 0
        // 7.637 0 0
        // 3.819 3.819 0
        // 0 7.637 0
        // -3.819 3.819 0
        // 0 0 0
        // r8
        // -7.637 0 12.7
        // -3.819 -3.819 12.7
        // 0 -7.637 12.7
        // 3.819 -3.819 12.7
        // 7.637 0 12.7
        // 3.819 3.819 12.7
        // 0 7.637 12.7
        // -3.819 3.819 12.7
        // 0 0 12.7
        // r7
        // -7.637 0 19.9
        // -3.819 -3.819 19.9
        // 0 -7.637 19.9
        // 3.819 -3.819 19.9
        // 7.637 0 19.9
        // 3.819 3.819 19.9
        // 0 7.637 19.9
        // -3.819 3.819 19.9
        // 0 0 19.9

        final int neurLen = Math.round((float) (19.9 / resols[2]));
        final int diff = 512 - neurLen;
        final int zStart = Math.round(diff / 2);
        final int zEnd = zStart + neurLen;
        final int r8Len = Math.round((float) (12.7 / resols[2]));
        final int zMiddle = zStart + r8Len;

        // top
        standardColumnImage.set(188, 256, 40, 100);
        x[0] = 188;
        y[0] = 256;
        z[0] = zStart;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(222, 290, 40, 100);
        x[0] = 222;
        y[0] = 290;
        z[0] = zStart;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(256, 324, 40, 100);
        x[0] = 256;
        y[0] = 324;
        z[0] = zStart;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(290, 290, 40, 100);
        x[0] = 290;
        y[0] = 290;
        z[0] = zStart;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(324, 256, 40, 100);
        x[0] = 324;
        y[0] = 256;
        z[0] = zStart;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(290, 222, 40, 100);
        x[0] = 290;
        y[0] = 222;
        z[0] = zStart;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(256, 188, 40, 100);
        x[0] = 256;
        y[0] = 188;
        z[0] = zStart;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(222, 222, 40, 100);
        x[0] = 222;
        y[0] = 222;
        z[0] = zStart;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(256, 256, 40, 100);
        x[0] = 256;
        y[0] = 256;
        z[0] = zStart;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        // r8
        standardColumnImage.set(188, 256, 316, 100);
        x[0] = 188;
        y[0] = 256;
        z[0] = zMiddle;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(222, 290, 316, 100);
        x[0] = 222;
        y[0] = 290;
        z[0] = zMiddle;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(256, 324, 316, 100);
        x[0] = 256;
        y[0] = 324;
        z[0] = zMiddle;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(290, 290, 316, 100);
        x[0] = 290;
        y[0] = 290;
        z[0] = zMiddle;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(324, 256, 316, 100);
        x[0] = 324;
        y[0] = 256;
        z[0] = zMiddle;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(290, 222, 316, 100);
        x[0] = 290;
        y[0] = 222;
        z[0] = zMiddle;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(256, 188, 316, 100);
        x[0] = 256;
        y[0] = 188;
        z[0] = zMiddle;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(222, 222, 316, 100);
        x[0] = 222;
        y[0] = 222;
        z[0] = zMiddle;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(256, 256, 316, 100);
        x[0] = 256;
        y[0] = 256;
        z[0] = zMiddle;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        // r7
        standardColumnImage.set(188, 256, 472, 100);
        x[0] = 188;
        y[0] = 256;
        z[0] = zEnd;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(222, 290, 472, 100);
        x[0] = 222;
        y[0] = 290;
        z[0] = zEnd;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(256, 324, 472, 100);
        x[0] = 256;
        y[0] = 324;
        z[0] = zEnd;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(290, 290, 472, 100);
        x[0] = 290;
        y[0] = 290;
        z[0] = zEnd;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(324, 256, 472, 100);
        x[0] = 324;
        y[0] = 256;
        z[0] = zEnd;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(290, 222, 472, 100);
        x[0] = 290;
        y[0] = 222;
        z[0] = zEnd;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(256, 188, 472, 100);
        x[0] = 256;
        y[0] = 188;
        z[0] = zEnd;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(222, 222, 472, 100);
        x[0] = 222;
        y[0] = 222;
        z[0] = zEnd;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.set(256, 256, 472, 100);
        x[0] = 256;
        y[0] = 256;
        z[0] = zEnd;
        newPtVOI.importCurve(x, y, z, (int) z[0]);

        standardColumnImage.calcMinMax();

        // NEURON IMAGE
        newPtVOI = new VOI((short) 0, "point3D.voi", 512, VOI.POINT, -1.0f);
        newPtVOI.setUID(newPtVOI.hashCode());
        neuronImage_grey.registerVOI(newPtVOI);
        curves = newPtVOI.getCurvesTemp();
        int key;
        float[] f;
        for (int i = 0; i < pointsMap.size(); i++) {
            key = i + 1;
            if (pointsMap.get(new Integer(key)) != null) {
                x[0] = Math.round(pointsMap.get(new Integer(key))[0] / resols[0]);
                y[0] = Math.round(pointsMap.get(new Integer(key))[1] / resols[1]);
                z[0] = Math.round(pointsMap.get(new Integer(key))[2] / resols[2]);
                if (i == pointsMap.size() - 1) {
                    // this is the r7 center point!!
                    r7_27Coord = new float[3];
                    r7_27Coord[0] = x[0];
                    r7_27Coord[1] = y[0];
                    r7_27Coord[2] = z[0];
                }
                f = new float[3];
                f[0] = x[0];
                f[1] = y[0];
                f[2] = z[0];
                inputPointsList.add(f);
            } else {
                removePointsLabels.add(String.valueOf(key));
                x[0] = 10;
                y[0] = 10;
                z[0] = 10;
                inputPointsList.add(null);
            }
            transformedPointsList.add(null);
            newPtVOI.importCurve(x, y, z, (int) z[0]);
        }

        // remove whatever points from both std column and neuron image that were not available in the points file
        final VOIVector stdColumnVOIs = standardColumnImage.getVOIs();
        int nCurves;
        String label;
        VOI voi = stdColumnVOIs.elementAt(0);
        curves = voi.getCurvesTemp();
        final ArrayList<Integer> indexAL_std = new ArrayList<Integer>();
        final ArrayList<Integer> sliceAL_std = new ArrayList<Integer>();
        for (int s = 0; s < standardColumnImage.getExtents()[2]; s++) {
            nCurves = curves[s].size();
            for (int j = 0; j < nCurves; j++) {
                final VOIPoint voiPoint = (VOIPoint) (curves[s].elementAt(j));
                label = voiPoint.getLabel();
                for (int k = 0; k < removePointsLabels.size(); k++) {
                    final String removeLabel = removePointsLabels.get(k);
                    if (label.equals(removeLabel)) {
                        indexAL_std.add(new Integer(j));
                        sliceAL_std.add(new Integer(s));
                    }
                }
            }
        }
        if (indexAL_std.size() > 0) {
            for (int i = indexAL_std.size(); i > 0; i--) {
                final int ind = indexAL_std.get(i - 1);
                final int slice = sliceAL_std.get(i - 1);
                voi.removeCurve(ind, slice);
            }
        }
        standardColumnImage.notifyImageDisplayListeners();

        final VOIVector neuronVOIs = neuronImage_grey.getVOIs();
        voi = neuronVOIs.elementAt(0);
        curves = voi.getCurvesTemp();
        final ArrayList<Integer> indexAL_neuron = new ArrayList<Integer>();
        final ArrayList<Integer> sliceAL_neuron = new ArrayList<Integer>();
        for (int s = 0; s < neuronImage_grey.getExtents()[2]; s++) {
            nCurves = curves[s].size();
            for (int j = 0; j < nCurves; j++) {
                final VOIPoint voiPoint = (VOIPoint) (curves[s].elementAt(j));
                label = voiPoint.getLabel();
                for (int k = 0; k < removePointsLabels.size(); k++) {
                    final String removeLabel = removePointsLabels.get(k);
                    if (label.equals(removeLabel)) {
                        indexAL_neuron.add(new Integer(j));
                        sliceAL_neuron.add(new Integer(s));
                    }
                }

            }
        }
        if (indexAL_neuron.size() > 0) {
            for (int i = indexAL_neuron.size(); i > 0; i--) {
                final int ind = indexAL_neuron.get(i - 1);
                final int slice = sliceAL_neuron.get(i - 1);
                voi.removeCurve(ind, slice);
            }
        }

        neuronImage_grey.notifyImageDisplayListeners();

        // call rigid least squares alg
        leastSquaredAlg();
        leastSquaredAlgorithmPerformed();

        // call non linear thin plate spline alg
        thinPlateSplineAlg();
        thinPlateSplineAlgorithmPerformed();

        createFinalImage();

        if (standardColumnImage != null) {
            standardColumnImage.disposeLocal();
            standardColumnImage = null;
        }

        if (cityBlockImage != null) {
            cityBlockImage.disposeLocal();
            cityBlockImage = null;
        }

        if (neuronImage != null) {
            neuronImage.disposeLocal();
            neuronImage = null;
        }

        if (neuronImage_grey != null) {
            neuronImage_grey.disposeLocal();
            neuronImage_grey = null;
        }

        if (resultImage1 != null) {
            resultImage1.disposeLocal();
            resultImage1 = null;
        }

        final long endTime = System.currentTimeMillis();
        final long diffTime = endTime - begTime;
        final float seconds = ((float) diffTime) / 1000;

        outputTextArea.append("** Algorithm took " + seconds + " seconds \n");

        setCompleted(true);
    }

    /**
     * calls the non-linear thin plate spline alg
     */
    private void thinPlateSplineAlg() {
        outputTextArea.append("Calling Non Linear Thin Plate Spline Registration" + "\n");
        int nPtsA = 0; // = standardColumnImage.getVOIs().size();
        int nPtsB = 0; // = resultImage1.getVOIs().size()
        Vector3f[] tmpptA = null;
        Vector3f[] tmpptB = null;
        Vector3f[] ptA = null; // new Vector3f[nPtsA];
        Vector3f[] ptB = null; // new Vector3f[nPtsB];
        int i, s;
        int ptNum = 0;
        Vector[] curvesB;
        Vector[] curvesM;

        curvesB = standardColumnImage.getVOIs().VOIAt(0).getCurvesTemp(); // curves[s] holds all VOIs in slice s

        for (s = 0; s < standardColumnImage.getExtents()[2]; s++) {
            nPtsA += curvesB[s].size();
        }

        Preferences.debug("thin plate spline - nPtsA = " + nPtsA);

        curvesM = resultImage1.getVOIs().VOIAt(0).getCurvesTemp(); // curves[s] holds all VOIs in slice s

        for (s = 0; s < resultImage1.getExtents()[2]; s++) {
            nPtsB += curvesM[s].size();
        }

        Preferences.debug("thin plate spline nPtsB = " + nPtsB);

        try {
            ptA = new Vector3f[nPtsA];
            ptB = new Vector3f[nPtsB];
        } catch (final OutOfMemoryError error) {
            ptA = null;
            ptB = null;
            System.gc();
            MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on ptA");

            return;
        }

        for (s = 0; s < standardColumnImage.getExtents()[2]; s++) {
            tmpptA = standardColumnImage.getVOIs().VOIAt(0).exportPoints(s);

            for (i = 0; i < tmpptA.length; i++) {
                ptNum = (Short.valueOf( ((VOIPoint) curvesB[s].elementAt(i)).getLabel()).shortValue()) - 1;
                ptA[ptNum] = tmpptA[i];
            }
        }

        for (s = 0; s < resultImage1.getExtents()[2]; s++) {
            tmpptB = resultImage1.getVOIs().VOIAt(0).exportPoints(s);

            for (i = 0; i < tmpptB.length; i++) {
                ptNum = (Short.valueOf( ((VOIPoint) curvesM[s].elementAt(i)).getLabel()).shortValue()) - 1;
                ptB[ptNum] = tmpptB[i];
            }
        }

        // DIM == 3

        // Calculate the reverse direction to find the values of the grid positions in x',y',z' space in
        // terms of x, y, z values in the original space
        try {
            xSource = new double[nPtsA];
            ySource = new double[nPtsA];
            zSource = new double[nPtsA];

            xTar = new double[nPtsB];
            yTar = new double[nPtsB];
            zTar = new double[nPtsB];
        } catch (final OutOfMemoryError error) {
            xSource = null;
            ySource = null;
            zSource = null;

            xTar = null;
            yTar = null;
            zTar = null;

            System.gc();
            MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory.");

            return;
        }

        for (i = 0; i < nPtsA; i++) {
            xSource[i] = ptA[i].X;
            ySource[i] = ptA[i].Y;
            zSource[i] = ptA[i].Z;
        }

        for (i = 0; i < nPtsB; i++) {
            xTar[i] = ptB[i].X;
            yTar[i] = ptB[i].Y;
            zTar[i] = ptB[i].Z;
        }

        // 0.0f for no smoothing, with smoothing interpolation is not exact

        try {
            spline = new AlgorithmTPSpline(xSource, ySource, zSource, xTar, yTar, zTar, 0.0f, standardColumnImage,
                    resultImage1, true);
        } catch (final OutOfMemoryError error) {
            spline = null;
            System.gc();
            MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on spline");

            return;
        }

        spline.run();

    }

    /**
     * create a grey image from the input color image
     */
    private void createGreyImage() {
        if (neuronImage.getType() == ModelStorageBase.ARGB) {
            neuronImage_grey = new ModelImage(ModelStorageBase.UBYTE, neuronImage.getExtents(), (neuronImage
                    .getImageName() + "Gray"));
        } else if (neuronImage.getType() == ModelStorageBase.ARGB_USHORT) {
            neuronImage_grey = new ModelImage(ModelStorageBase.USHORT, neuronImage.getExtents(), (neuronImage
                    .getImageName() + "Gray"));
        } else if (neuronImage.getType() == ModelStorageBase.ARGB_FLOAT) {
            neuronImage_grey = new ModelImage(ModelStorageBase.FLOAT, neuronImage.getExtents(), (neuronImage
                    .getImageName() + "Gray"));
        }

        for (int n = 0; n < neuronImage.getFileInfo().length; n++) {
            fInfoBase = (FileInfoBase) (neuronImage.getFileInfo(n).clone());
            fInfoBase.setDataType(neuronImage_grey.getType());
            neuronImage_grey.setFileInfo(fInfoBase, n);
        }

        // Make algorithm
        final AlgorithmRGBtoGray RGBAlgo = new AlgorithmRGBtoGray(neuronImage_grey, neuronImage, redValue, greenValue,
                blueValue, thresholdAverage, threshold, intensityAverage);

        RGBAlgo.run();

    }

    /**
     * alg performed for thin plate spline
     */
    private void thinPlateSplineAlgorithmPerformed() {
        if (resultImage1 != null) {
            resultImage1.disposeLocal();
            resultImage1 = null;
        }
        if (standardColumnImage != null) {
            standardColumnImage.disposeLocal();
            standardColumnImage = null;
        }
        System.gc();
    }

    /**
     * calls rigid least squared alg
     */
    private void leastSquaredAlg() {
        outputTextArea.append("Calling Rigid Least Squared Registration" + "\n");
        int nPtsA = 0; // = standardColumnImage.getVOIs().size();
        int nPtsB = 0; // = neuronImage.getVOIs().size()
        Vector3f[] tmpptA = null;
        Vector3f[] tmpptB = null;
        Vector3f[] ptA = null; // new Vector3f[nPtsA];
        Vector3f[] ptB = null; // new Vector3f[nPtsB];
        int i, s, ptNum;
        Vector[] curves;

        curves = standardColumnImage.getVOIs().VOIAt(0).getCurvesTemp(); // curves[s] holds all VOIs in slice s

        for (s = 0; s < standardColumnImage.getExtents()[2]; s++) {
            nPtsA += curves[s].size();
        }

        Preferences.debug("nPtsA = " + nPtsA + "\n");
        ptA = new Vector3f[nPtsA];
        for (s = 0; s < standardColumnImage.getExtents()[2]; s++) {
            tmpptA = standardColumnImage.getVOIs().VOIAt(0).exportPoints(s);

            for (i = 0; i < tmpptA.length; i++) {
                ptNum = (Short.valueOf( ((VOIPoint) curves[s].elementAt(i)).getLabel()).shortValue()) - 1;
                ptA[ptNum] = tmpptA[i];
            }
        }

        curves = neuronImage_grey.getVOIs().VOIAt(0).getCurvesTemp();

        for (s = 0; s < neuronImage_grey.getExtents()[2]; s++) {
            nPtsB += curves[s].size();
        }

        if (nPtsA != nPtsB) {
            MipavUtil.displayError("Both images must have the same number of points");

            return;
        }

        Preferences.debug("nPtsB = " + nPtsB + "\n");
        ptB = new Vector3f[nPtsB];
        for (s = 0; s < neuronImage_grey.getExtents()[2]; s++) {
            tmpptB = neuronImage_grey.getVOIs().VOIAt(0).exportPoints(s);

            for (i = 0; i < tmpptB.length; i++) {
                ptNum = (Short.valueOf( ((VOIPoint) curves[s].elementAt(i)).getLabel()).shortValue()) - 1;

                // ptNum = (int)(Short.valueOf(((VOIPoint)tmpptB[i]).getLabel()).shortValue());
                ptB[ptNum] = tmpptB[i];
            }
        }

        Vector3f[] ptAmm = new Vector3f[nPtsA];
        Vector3f[] ptBmm = new Vector3f[nPtsB];
        zresA = 1;
        xresA = standardColumnImage.getFileInfo(0).getResolutions()[0];
        yresA = standardColumnImage.getFileInfo(0).getResolutions()[1];
        xresB = neuronImage_grey.getFileInfo(0).getResolutions()[0];
        yresB = neuronImage_grey.getFileInfo(0).getResolutions()[1];

        if (standardColumnImage.getNDims() == 3) {
            zresA = standardColumnImage.getFileInfo(0).getResolutions()[2];
        }
        if (neuronImage_grey.getNDims() == 3) {
            zresB = neuronImage_grey.getFileInfo(0).getResolutions()[2];
        }

        for (i = 0; i < nPtsA; i++) {
            Preferences.debug(ptA[i].X + ", " + ptA[i].Y + ", " + ptA[i].Z + "   ");
            Preferences.debug(ptB[i].X + ", " + ptB[i].Y + ", " + ptB[i].Z + "\n");
            ptAmm[i] = new Vector3f( (ptA[i].X * xresA), (ptA[i].Y * yresA), (ptA[i].Z * zresA));
            ptBmm[i] = new Vector3f( (ptB[i].X * xresB), (ptB[i].Y * yresB), (ptB[i].Z * zresB));

        }

        LSMatch = new AlgorithmRegLeastSquares(ptAmm, ptBmm, DIM);
        LSMatch.run();
        tmpptA = null;
        tmpptB = null;
        ptA = null; // new Vector3f[nPtsA];
        ptB = null;
        ptAmm = null;
        ptBmm = null;

    }

    /**
     * alg performed for least squared alg
     */
    private void leastSquaredAlgorithmPerformed() {
        lsMatrix = LSMatch.getTransformBtoA();
        LSMatch.calculateResiduals();
        xdimA = standardColumnImage.getExtents()[0];
        ydimA = standardColumnImage.getExtents()[1];
        zdimA = standardColumnImage.getExtents()[2];

        final int[] extents = new int[] {xdimA, ydimA, zdimA};
        final float[] resolutions = new float[] {xresA, yresA, zresA};
        resultImage1 = new ModelImage(neuronImage_grey.getType(), extents, "LS Transformed image");

        for (int i = 0; i < zdimA; i++) {
            resultImage1.getFileInfo(i).setResolutions(resolutions);
            resultImage1.getFileInfo(i).setUnitsOfMeasure(neuronImage_grey.getFileInfo()[0].getUnitsOfMeasure());
        }

        if (neuronImage_grey.isColorImage() == false) {
            AlgorithmTransform.transformTrilinear(neuronImage_grey, resultImage1, LSMatch.getTransformBtoA(), null,
                    true, fillValue);
        } else {
            AlgorithmTransform.transformTrilinearC(neuronImage_grey, resultImage1, LSMatch.getTransformBtoA(), xdimA,
                    ydimA, zdimA, xresA, yresA, zresA, fillValue);
        }

        LSMatch.finalize();
        LSMatch = null;

        resultImage1.calcMinMax();
        resultImage1.setImageName("LS Transformed image");

        // need to transform points if there were any
        final VOIVector srcVOIs = neuronImage_grey.getVOIs();
        VOI newVOI;
        Vector[] curves;
        Vector[] newCurves;
        int nCurves;
        Vector3f pt, tPt;

        String label = "";
        final VOIPoint point;
        ArrayList<Integer> indexAL_std = new ArrayList<Integer>();
        ArrayList<Integer> sliceAL_std = new ArrayList<Integer>();
        ArrayList<String> labelAL_std = new ArrayList<String>();
        TreeMap<Integer, AddVals> addCurvesMap = new TreeMap<Integer, AddVals>();
        VOI voi = srcVOIs.elementAt(0);
        newVOI = new VOI((short) 0, "point3D.voi", resultImage1.getExtents()[2], VOI.POINT, -1.0f);
        newVOI.setUID(newVOI.hashCode());
        curves = voi.getCurvesTemp();
        newCurves = newVOI.getCurvesTemp();

        Integer labelInt;
        for (int s = 0; s < neuronImage_grey.getExtents()[2]; s++) {
            nCurves = curves[s].size();
            for (int j = 0; j < nCurves; j++) {
                final float[] xPt = new float[1];
                final float[] yPt = new float[1];
                final float[] zPt = new float[1];
                final VOIPoint voiPoint = (VOIPoint) (curves[s].elementAt(j));
                pt = voiPoint.exportPoint();
                label = voiPoint.getLabel();
                final Vector3f pt2 = new Vector3f();
                pt2.X = pt.X * neuronImage_grey.getResolutions(0)[0];
                pt2.Y = pt.Y * neuronImage_grey.getResolutions(0)[1];
                pt2.Z = pt.Z * neuronImage_grey.getResolutions(0)[2];
                tPt = new Vector3f();
                lsMatrix.transformAsPoint3Df(pt2, tPt);
                xPt[0] = MipavMath.round(tPt.X / neuronImage_grey.getResolutions(0)[0]);
                yPt[0] = MipavMath.round(tPt.Y / neuronImage_grey.getResolutions(0)[1]);
                zPt[0] = MipavMath.round(tPt.Z / neuronImage_grey.getResolutions(0)[2]);
                if (xPt[0] < 0 || yPt[0] < 0 || zPt[0] < 0 || xPt[0] > neuronImage_grey.getExtents()[0] - 1
                        || yPt[0] > neuronImage_grey.getExtents()[1] - 1
                        || zPt[0] > neuronImage_grey.getExtents()[2] - 1) {
                    outputTextArea.append("neuron image point after rigid least squares alg is out of bounds - "
                            + label + "\n");
                    indexAL_std.add(new Integer(j));
                    sliceAL_std.add(new Integer(s));
                    labelAL_std.add(label);
                } else {
                    final AddVals vals = new AddVals(xPt, yPt, zPt);
                    labelInt = new Integer(label);
                    addCurvesMap.put(labelInt, vals);
                }
            }
        }

        // need to add curves to result image...treemap is ascending order...so should be straightforward
        final Set keySet = addCurvesMap.keySet();
        final Iterator iter = keySet.iterator();
        Integer key;
        AddVals v;
        while (iter.hasNext()) {
            key = (Integer) iter.next();
            v = addCurvesMap.get(key);
            newVOI.importCurve(v.getXPt(), v.getYPt(), v.getZPt(), (int) (v.getZPt()[0]));
        }

        // need to remove the corresponding out of bounds points from the standard column image
        if (labelAL_std.size() > 0) {
            final VOIVector stdVOIs = standardColumnImage.getVOIs();
            voi = stdVOIs.elementAt(0);
            curves = voi.getCurvesTemp();
            String removeLabel;
            for (int s = standardColumnImage.getExtents()[2] - 1; s >= 0; s--) {
                nCurves = curves[s].size();
                for (int j = nCurves - 1; j >= 0; j--) {
                    final VOIPoint voiPoint = (VOIPoint) (curves[s].elementAt(j));
                    label = voiPoint.getLabel();
                    for (int i = 0; i < labelAL_std.size(); i++) {
                        removeLabel = labelAL_std.get(i);
                        if (label.equals(removeLabel)) {
                            outputTextArea.append("removing point from standard column image - " + label + "\n");
                            voi.removeCurve(j, s);
                        }
                    }
                }
            }
        }

        standardColumnImage.notifyImageDisplayListeners();

        resultImage1.registerVOI(newVOI);

        resultImage1.notifyImageDisplayListeners();

        if (neuronImage_grey != null) {
            neuronImage_grey.disposeLocal();
            neuronImage_grey = null;
        }

        indexAL_std = null;
        sliceAL_std = null;
        labelAL_std = null;
        addCurvesMap = null;

    }

    /**
     * creates final image
     */
    private void createFinalImage() {

        // make LS MAtrix into inverse
        lsMatrix.Inverse();

        final int[] extents = {512, 512, 512};
        finalImage = new ModelImage(ModelStorageBase.ARGB, extents, "finalStandardizedImage");

        final float[] finalImageResols = new float[3];
        finalImageResols[0] = neuronImage.getResolutions(0)[0];
        finalImageResols[1] = neuronImage.getResolutions(0)[1];
        finalImageResols[2] = neuronImage.getResolutions(0)[2];
        for (int i = 0; i < finalImage.getExtents()[2]; i++) {
            finalImage.setResolutions(i, finalImageResols);
        }

        final byte[] finalBuffer = new byte[512 * 512 * 512 * 4];
        int index = 0; // index into finalBuffer

        float[] tPt1 = new float[3];
        final float[] tPt2 = new float[3];

        float xmm, ymm, zmm;

        final short[] rgb_short = new short[3];
        byte[] rgb = new byte[3];

        byte[] neuronImageBuffer;
        final int length2 = neuronImageExtents[0] * neuronImageExtents[1] * neuronImageExtents[2] * 4;
        neuronImageBuffer = new byte[length2];
        try {
            neuronImage.exportData(0, length2, neuronImageBuffer);
        } catch (final IOException error) {
            System.out.println("IO exception");
            return;
        }

        byte r, g, b;

        long begTime = 0;
        final int[] extents2 = neuronImage.getExtents();
        final float[] finalImageRes = finalImage.getResolutions(0);
        final int[] finalImageExts = finalImage.getExtents();
        float diffX, diffY, diffZ;
        float diffTotal;

        // loop through each point in result image
        for (float z = 0; z < 512; z = z + samplingRate) {
            if ((float) Math.floor(z) == z) {
                outputTextArea.append("z is " + z + "\n");
                begTime = System.currentTimeMillis();
                if (z % 5 == 0) {
                    System.gc();
                }
            }

            for (float y = 0; y < 512; y = y + samplingRate) {

                for (float x = 0; x < 512; x = x + samplingRate) {

                    final float xFloor = (float) Math.floor(x);
                    final float yFloor = (float) Math.floor(y);
                    final float zFloor = (float) Math.floor(z);

                    tPt1 = spline.getCorrespondingPoint(x, y, z);

                    xmm = tPt1[0] * finalImageRes[0];
                    ymm = tPt1[1] * finalImageRes[1];
                    zmm = tPt1[2] * finalImageRes[2];

                    lsMatrix.transform(xmm, ymm, zmm, tPt2);

                    tPt2[0] = tPt2[0] / finalImageRes[0];
                    tPt2[1] = tPt2[1] / finalImageRes[1];
                    tPt2[2] = tPt2[2] / finalImageRes[2];

                    if (tPt2[0] < 0 || tPt2[1] < 0 || tPt2[2] < 0 || tPt2[0] > finalImageExts[0] - 1
                            || tPt2[1] > finalImageExts[1] - 1 || tPt2[2] > finalImageExts[2] - 1) {
                        rgb_short[0] = 0;
                        rgb_short[1] = 0;
                        rgb_short[2] = 0;

                    } else {
                        int floorPointIndex2 = 0;
                        final double tX2_floor = Math.floor(tPt2[0]);
                        final double tY2_floor = Math.floor(tPt2[1]);
                        final double tZ2_floor = Math.floor(tPt2[2]);
                        final float dx2 = (float) (tPt2[0] - tX2_floor);
                        final float dy2 = (float) (tPt2[1] - tY2_floor);
                        final float dz2 = (float) (tPt2[2] - tZ2_floor);

                        floorPointIndex2 = (int) ( ( (tZ2_floor * (extents2[0] * extents2[1]))
                                + (tY2_floor * extents2[0]) + tX2_floor) * 4);
                        if (floorPointIndex2 < neuronImageBuffer.length) {

                            if (xFloor == x && yFloor == y && zFloor == z) {
                                rgb = AlgorithmConvolver.getTrilinearC(floorPointIndex2, dx2, dy2, dz2, extents2,
                                        neuronImageBuffer);
                                rgb_short[0] = (short) (rgb[0] & 0xff);
                                rgb_short[1] = (short) (rgb[1] & 0xff);
                                rgb_short[2] = (short) (rgb[2] & 0xff);

                            }

                            diffX = Math.abs(tPt2[0] - r7_27Coord[0]);
                            diffY = Math.abs(tPt2[1] - r7_27Coord[1]);
                            diffZ = Math.abs(tPt2[2] - r7_27Coord[2]);

                            diffTotal = (diffX * diffX) + (diffY * diffY) + (diffZ * diffZ);

                            if (diffTotal < toleranceSq) {
                                if (r7_27Coord_transformed == null
                                        || (r7_27Coord_transformed != null && r7_27Coord_transformed[3] > diffTotal)) {
                                    r7_27Coord_transformed = new float[4];
                                    r7_27Coord_transformed[0] = x * finalImage.getResolutions(0)[0];
                                    r7_27Coord_transformed[1] = y * finalImage.getResolutions(0)[1];
                                    r7_27Coord_transformed[2] = z * finalImage.getResolutions(0)[2];
                                    r7_27Coord_transformed[3] = diffTotal;
                                    r7CenterPointFound = true;
                                }
                            }

                            for (int i = 0; i < inputPointsList.size(); i++) {
                                final float[] f = inputPointsList.get(i);
                                if (f != null) {

                                    diffX = Math.abs(tPt2[0] - f[0]);
                                    diffY = Math.abs(tPt2[1] - f[1]);
                                    diffZ = Math.abs(tPt2[2] - f[2]);

                                    diffTotal = (diffX * diffX) + (diffY * diffY) + (diffZ * diffZ);

                                    if (diffTotal < toleranceSq) {
                                        final float[] ft = transformedPointsList.get(i);
                                        if (ft == null || (ft != null && ft[3] > diffTotal)) {
                                            final float[] fTrans = new float[4];
                                            fTrans[0] = x * finalImage.getResolutions(0)[0];
                                            fTrans[1] = y * finalImage.getResolutions(0)[1];
                                            fTrans[2] = z * finalImage.getResolutions(0)[2];
                                            fTrans[3] = diffTotal;
                                            transformedPointsList.set(i, fTrans);
                                        }
                                    }

                                }
                            }

                            // Calculating new surface file points!!!!!
                            if (cityBlockImage.getByte((int) (tPt2[0] + 0.5f), (int) (tPt2[1] + 0.5f),
                                    (int) (tPt2[2] + 0.5f)) != 100) {
                                ArrayList<float[]> al;
                                ArrayList<float[]> al_new;
                                float[] coords;
                                final int allFilamentsSize = allFilamentCoords.size();
                                int alSize;
                                for (int i = 0; i < allFilamentsSize; i++) {
                                    al = allFilamentCoords.get(i);
                                    alSize = al.size();
                                    for (int k = 0; k < alSize; k++) {
                                        coords = al.get(k);
                                        diffX = Math.abs(tPt2[0] - coords[0]);
                                        diffY = Math.abs(tPt2[1] - coords[1]);
                                        diffZ = Math.abs(tPt2[2] - coords[2]);

                                        diffTotal = (diffX * diffX) + (diffY * diffY) + (diffZ * diffZ);

                                        if (diffTotal < toleranceSq) {
                                            final float[] nCoords = {x, y, z, diffTotal};
                                            al_new = allFilamentCoords_newCoords.get(i);
                                            final float[] ft = al_new.get(k);
                                            if (ft == null || (ft != null && ft[3] > diffTotal)) {
                                                al_new.set(k, nCoords);
                                            }

                                            // coords[3] = 1;
                                        }
                                    }

                                }
                            }

                        } else {
                            rgb_short[0] = 0;
                            rgb_short[1] = 0;
                            rgb_short[2] = 0;
                        }
                    }

                    if (xFloor == x && yFloor == y && zFloor == z) {
                        r = (byte) rgb_short[0];
                        g = (byte) rgb_short[1];
                        b = (byte) rgb_short[2];

                        // alpha
                        finalBuffer[index] = (byte) 255;
                        // r
                        index = index + 1;
                        finalBuffer[index] = r;
                        // g
                        index = index + 1;
                        finalBuffer[index] = g;
                        // b
                        index = index + 1;
                        finalBuffer[index] = b;
                        index = index + 1;
                    }
                }
            }

        }

        outputTextArea.append("\n");

        if (cityBlockImage != null) {
            cityBlockImage.disposeLocal();
            cityBlockImage = null;
        }

        try {
            finalImage.importData(0, finalBuffer, true);
        } catch (final IOException error) {
            System.out.println("IO exception");
            error.printStackTrace();
            return;
        }

        final FileInfoImageXML[] fileInfoBases = new FileInfoImageXML[finalImage.getExtents()[2]];
        float[] f = null;
        ;
        if (r7_27Coord_transformed != null) {
            f = new float[3];
            f[0] = -r7_27Coord_transformed[0];
            f[1] = -r7_27Coord_transformed[1];
            f[2] = -r7_27Coord_transformed[2];
        }
        for (int i = 0; i < fileInfoBases.length; i++) {
            fileInfoBases[i] = new FileInfoImageXML(finalImage.getImageName(), null, FileUtility.XML);
            fileInfoBases[i].setUnitsOfMeasure(neuronImage.getFileInfo()[0].getUnitsOfMeasure());
            fileInfoBases[i].setResolutions(finalImageResols);
            fileInfoBases[i].setExtents(neuronImage.getExtents());
            if (r7_27Coord_transformed != null) {

                fileInfoBases[i].setOrigin(f);
            } else {
                fileInfoBases[i].setOrigin(neuronImage.getFileInfo()[0].getOrigin());
            }

            fileInfoBases[i].setDataType(ModelStorageBase.ARGB);

        }

        if (r7_27Coord_transformed != null) {
            outputTextArea.append("Setting new origin of standardized image to transformed r7 center point" + "\n");
            outputTextArea.append("\n");
        }

        finalImage.setFileInfo(fileInfoBases);
        finalImage.calcMinMax();

        final FileIO fileIO = new FileIO();
        fileIO.setQuiet(true);
        final FileWriteOptions opts = new FileWriteOptions(true);
        opts.setFileType(FileUtility.ICS);
        opts.setFileDirectory(dir);
        final String finalImageFileName = "finalStandardizedImage";
        opts.setFileName(finalImageFileName + ".ics");
        opts.setBeginSlice(0);
        opts.setEndSlice(511);
        opts.setOptionsSet(true);
        fileIO.writeImage(finalImage, opts);
        outputTextArea.append("Saving final standardized image as: \n");
        outputTextArea.append(dir + finalImageFileName + ".ics" + "\n");
        outputTextArea.append("\n");

        writeSurfaceFile();
        writeNewPointsFile();
    }

    /**
     * writes new points file
     * 
     * @return
     */
    private boolean writeNewPointsFile() {
        final boolean success = true;

        final String parentDir = pointsFile.getParent();
        final String newName = pointsFile.getName().substring(0, pointsFile.getName().indexOf("."))
                + "_transformedCoordinates.txt";
        outputTextArea.append("Saving new transformed coordinates file as: \n");
        outputTextArea.append(parentDir + File.separator + newName + "\n");
        outputTextArea.append("\n");

        try {

            final File newPointsCoordinatesFile = new File(parentDir + File.separator + newName);
            final FileWriter fw = new FileWriter(newPointsCoordinatesFile);
            final BufferedWriter bw = new BufferedWriter(fw);
            for (int i = 0; i < transformedPointsList.size(); i++) {
                final float[] f = transformedPointsList.get(i);
                final String ind = String.valueOf(i + 1);
                if (i == 0) {
                    bw.write("top");
                    bw.newLine();
                }

                if (i == 9) {
                    bw.write("r8");
                    bw.newLine();
                }

                if (i == 18) {
                    bw.write("r7");
                    bw.newLine();
                }

                bw.write(ind + ":");
                if (f != null) {
                    bw.write( (f[0] - r7_27Coord_transformed[0]) + "," + (f[1] - r7_27Coord_transformed[1]) + ","
                            + (f[2] - r7_27Coord_transformed[2]));
                    bw.newLine();
                } else {
                    bw.newLine();
                }
            }
            bw.close();
            fw.close();

        } catch (final Exception e) {

        }

        return true;
    }

    /**
     * writes new surface file
     * 
     * @return
     */
    private boolean writeSurfaceFile() {

        final boolean success = true;
        int index = 0;

        final String parentDir = oldSurfaceFile.getParent();
        String newName = oldSurfaceFile.getName().substring(0, oldSurfaceFile.getName().indexOf(".")) + "_standardized";
        if (flipX) {
            newName = newName + "_flipX";
        }
        if (flipY) {
            newName = newName + "_flipY";
        }
        if (flipZ) {
            newName = newName + "_flipZ";
        }
        newName = newName + ".iv";
        outputTextArea.append("Saving new filament file as: \n");
        outputTextArea.append(parentDir + File.separator + newName + "\n");
        outputTextArea.append("\n");

        try {
            final RandomAccessFile raFile = new RandomAccessFile(oldSurfaceFile, "r");
            final File newSurfaceFile = new File(parentDir + File.separator + newName);
            final FileWriter fw = new FileWriter(newSurfaceFile);
            final BufferedWriter bw = new BufferedWriter(fw);

            String line;

            while ( (line = raFile.readLine()) != null) {
                line = line.trim();

                if (line.startsWith("point [")) {
                    if (index < allFilamentCoords_newCoords.size()) {

                        final float tPt3[] = new float[3];

                        final ArrayList<float[]> filCoords = allFilamentCoords_newCoords.get(index);
                        bw.write("point [");
                        bw.newLine();
                        for (int k = 0; k < filCoords.size(); k++) {

                            final float[] filCoord = filCoords.get(k);

                            if (filCoord != null) {
                                if (r7CenterPointFound) {
                                    tPt3[0] = (filCoord[0] * finalImage.getResolutions(0)[0])
                                            - r7_27Coord_transformed[0];
                                    tPt3[1] = (filCoord[1] * finalImage.getResolutions(0)[1])
                                            - r7_27Coord_transformed[1];
                                    tPt3[2] = (filCoord[2] * finalImage.getResolutions(0)[2])
                                            - r7_27Coord_transformed[2];
                                } else {
                                    tPt3[0] = filCoord[0] * finalImage.getResolutions(0)[0];
                                    tPt3[1] = filCoord[1] * finalImage.getResolutions(0)[1];
                                    tPt3[2] = filCoord[2] * finalImage.getResolutions(0)[2];
                                }

                                if (flipX) {
                                    tPt3[0] = tPt3[0] * -1;
                                }
                                if (flipY) {
                                    tPt3[1] = tPt3[1] * -1;
                                }
                                if (flipZ) {
                                    tPt3[2] = tPt3[2] * -1;
                                }

                                bw.write(tPt3[0] + " " + tPt3[1] + " " + tPt3[2] + ",");
                                bw.newLine();
                            }

                        }

                        bw.write("]");
                        bw.newLine();

                        while ( ! (line = raFile.readLine()).endsWith("]")) {

                        }
                        raFile.readLine();
                        index = index + 1;
                    } else {
                        bw.write(line);
                        bw.newLine();
                    }

                } else {
                    bw.write(line);
                    bw.newLine();

                }

            }

            raFile.close();
            bw.close();
            fw.close();
        } catch (final Exception e) {
            e.printStackTrace();
            return false;
        }

        return success;
    }

    // /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    // Inner class that has the three coordinates for the point

    private class AddVals {
        private float[] xPt;

        private float[] yPt;

        private float[] zPt;

        private AddVals(final float[] xPt, final float[] yPt, final float[] zPt) {
            this.xPt = xPt;
            this.yPt = yPt;
            this.zPt = zPt;
        }

        public float[] getXPt() {
            return xPt;
        }

        public void setXPt(final float[] pt) {
            xPt = pt;
        }

        public float[] getYPt() {
            return yPt;
        }

        public void setYPt(final float[] pt) {
            yPt = pt;
        }

        public float[] getZPt() {
            return zPt;
        }

        public void setZPt(final float[] pt) {
            zPt = pt;
        }

    }

}
