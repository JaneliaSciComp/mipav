import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import java.io.*;


/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */

public class PlugInAlgorithmISN extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage destImage = null;

    /** DOCUMENT ME! */
    private float[] imgBuffer = null;

    /** DOCUMENT ME! */
    private ModelImage prevSliceImage2D = null;

    /** DOCUMENT ME! */
    private ModelImage prevSliceImage3D = null;

    /** DOCUMENT ME! */
    private ModelImage sliceImage2D = null;

    /** DOCUMENT ME! */
    private ModelImage sliceImage3D = null;

    /** DOCUMENT ME! */
    private ModelImage srcImage = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Default constructor.
     *
     * @param  destImage  DOCUMENT ME!
     * @param  srcImage   the source image
     */
    public PlugInAlgorithmISN(ModelImage destImage, ModelImage srcImage) {
        super(destImage, srcImage);
        this.srcImage = srcImage;
        this.destImage = destImage;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     */
    public void disposeLocal() {
        imgBuffer = null;

        sliceImage2D.disposeLocal();
        prevSliceImage2D.disposeLocal();
        sliceImage3D.disposeLocal();
        prevSliceImage3D.disposeLocal();

        sliceImage2D = null;
        prevSliceImage2D = null;
        sliceImage3D = null;
        prevSliceImage3D = null;

    }

    /**
     * DOCUMENT ME!
     */
    public void finalize() {
        disposeLocal();
        super.finalize();
    }

    /**
     * Run method for algorithm.
     */
    public void runAlgorithm() {
        runOrderRemapThenMatch();
        // runOrderMatchThenRemap();
    }

    /**
     * Run method for algorithm.
     */
    public void runOrderMatchThenRemap() {

        int i;
        int z = 1;

        imgBuffer = new float[srcImage.getExtents()[0] * srcImage.getExtents()[1]];

        if (srcImage.getNDims() == 3) {
            z = srcImage.getExtents()[2];
        }

        /* IntensitySliceNormalization algorithm here */
        int y = 0;
        int histMatchSlice = 0;
        double avg = 0;
        double a = 0;
        double min = 0;
        double max = 0;
        double[] sliceAverage = new double[z];
        double[] sliceMin = new double[z];
        double[] sliceMax = new double[z];

        double averageAVG = 0;
        //double standardevAVG = 0;
        //double CutoffBrightLow = 0;
        //double CutoffBrightHigh = 0;
        double startRange;
        double endRange;
        boolean processIndep = false;

        /* 1. calculate distribution of slice averages */
        // compute all slice intensity averages, minimums, and maximums
        for (i = 0; i < z; i++) {

            try {
                srcImage.exportData((i * imgBuffer.length), imgBuffer.length, imgBuffer);

                /* calculate min, max, and average of each slice */
                min = max = imgBuffer[0];

                for (y = 0; y < imgBuffer.length; y++) {
                    avg += imgBuffer[y];

                    if (imgBuffer[y] < min) {
                        min = imgBuffer[y];
                    } else if (imgBuffer[y] > max) {
                        max = imgBuffer[y];
                    }
                } // end for(y = 0; ...)

                avg /= imgBuffer.length;
                sliceAverage[i] = avg;
                sliceMin[i] = min;
                sliceMax[i] = max;
                destImage.importData((i * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmISN");
            }
        } // end for (i = 0; ...)

 
        /* 2. calculate mean of the slice average distribution */
        for (i = 0; i < z; i++) {
            averageAVG += sliceAverage[i];
        }

        averageAVG /= z;

 
        /* 3. calculate stddev of the slice average distribution */
        for (i = 0; i < z; i++) {
            a += (sliceAverage[i] - averageAVG) * (sliceAverage[i] - averageAVG);
        }

        a /= z;
        //standardevAVG = Math.sqrt(a);

        /* 4. cutoffs for acceptable range of averages */
        //CutoffBrightLow = averageAVG - standardevAVG;
        //CutoffBrightHigh = averageAVG + (1.0 * standardevAVG);

        boolean twoDIM = true;

        int[] extSlice3D = new int[3];
        extSlice3D[0] = srcImage.getExtents()[0];
        extSlice3D[1] = srcImage.getExtents()[1];
        extSlice3D[2] = 2;

        int[] extSlice2D = new int[2];
        extSlice2D[0] = srcImage.getExtents()[0];
        extSlice2D[1] = srcImage.getExtents()[1];

        AlgorithmChangeType changeTypeAlgo = null;
        AlgorithmHistogramMatch histoMatchAlgo = null;

        if (sliceImage2D == null) {
            sliceImage2D = new ModelImage(srcImage.getType(), extSlice2D, "temp1");
        }

        if (prevSliceImage2D == null) {
            prevSliceImage2D = new ModelImage(srcImage.getType(), extSlice2D, "temp2");
        }

        if (sliceImage3D == null) {
            sliceImage3D = new ModelImage(srcImage.getType(), extSlice3D, "temp3");
        }

        if (prevSliceImage3D == null) {
            prevSliceImage3D = new ModelImage(srcImage.getType(), extSlice3D, "temp4");
        }

        double adjAvg = 0;

        /* 5. single out and "operate" on 'bright' slices */
        for (i = 0; i < z; i++) {

            // if (sliceAverage[i] > CutoffBrightHigh) {
            // System.out.println("Image Slice = " + i);
            /* convert range of bright slice to average range of adjacent slices */
            if (i == 0) {
                startRange = sliceMin[1];
                endRange = sliceMax[1];
                histMatchSlice = 1;
                adjAvg = sliceAverage[1];
                twoDIM = true;
            } else if (i == (z - 1)) {
                startRange = sliceMin[i - 1];
                endRange = sliceMax[i - 1];
                histMatchSlice = i - 1;
                adjAvg = sliceAverage[i - 1];
                twoDIM = true;
            } else {
                adjAvg = (sliceAverage[i - 1] + sliceAverage[i + 1]) / 2.0;
                twoDIM = false;

                // obtaining lower and upper bounds of concatenated images
                if (sliceMin[i - 1] < sliceMin[i + 1]) {
                    startRange = sliceMin[i - 1];
                } else {
                    startRange = sliceMin[i + 1];
                }

                if (sliceMax[i - 1] > sliceMax[i + 1]) {
                    endRange = sliceMax[i - 1];
                } else {
                    endRange = sliceMax[i - 1];
                }
            }

            // System.out.println("diff = " + Math.abs(sliceAverage[i] - adjAvg) + " adjAvg = " + (0.15 *
            // sliceAverage[i]) ); singling out the bright slice (matt's method)
            if (Math.abs(sliceAverage[i] - adjAvg) > (0.15 * sliceAverage[i])) {

                // System.out.println("Image Slice = " + i);
                try {

                    if (twoDIM == true) {
                        srcImage.exportData((histMatchSlice * imgBuffer.length), imgBuffer.length, imgBuffer);
                        prevSliceImage2D.importData(0, imgBuffer, true);

                        srcImage.exportData((i * imgBuffer.length), imgBuffer.length, imgBuffer);
                        sliceImage2D.importData(0, imgBuffer, true);

                        // histogram matching here
                        histoMatchAlgo = new AlgorithmHistogramMatch(sliceImage2D, prevSliceImage2D);
                        histoMatchAlgo.run();

                        // intensity remapping
                        changeTypeAlgo = new AlgorithmChangeType(sliceImage2D, sliceImage2D.getType(), sliceMin[i],
                                                                 sliceMax[i], startRange, endRange, processIndep);
                        changeTypeAlgo.run();

                        sliceImage2D.exportData(0, imgBuffer.length, imgBuffer);
                        destImage.importData((i * imgBuffer.length), imgBuffer, false);
                    } else {

                        // concatenate adjacent slices
                        srcImage.exportData(((i - 1) * imgBuffer.length), imgBuffer.length, imgBuffer);
                        prevSliceImage3D.importData(0, imgBuffer, true);
                        srcImage.exportData(((i + 1) * imgBuffer.length), imgBuffer.length, imgBuffer);
                        prevSliceImage3D.importData(imgBuffer.length, imgBuffer, true);

                        // concatenate bright slice with its clone
                        srcImage.exportData((i * imgBuffer.length), imgBuffer.length, imgBuffer);
                        sliceImage3D.importData(0, imgBuffer, true);
                        sliceImage3D.importData(imgBuffer.length, imgBuffer, true);

                        // histogram matching
                        histoMatchAlgo = new AlgorithmHistogramMatch(sliceImage3D, prevSliceImage3D);
                        histoMatchAlgo.run();

                        // intensity remapping
                        changeTypeAlgo = new AlgorithmChangeType(sliceImage3D, sliceImage3D.getType(), sliceMin[i],
                                                                 sliceMax[i], startRange, endRange, processIndep);
                        changeTypeAlgo.run();

                        sliceImage3D.exportData(0, imgBuffer.length, imgBuffer);
                        destImage.importData((i * imgBuffer.length), imgBuffer, false);
                    }
                } catch (IOException ex) {
                    System.err.println("error exporting data from srcImage in AlgorithmISN");
                }
            }
        }

        destImage.calcMinMax();
        setCompleted(true);
    }

    /**
     * Run method for algorithm.
     */
    public void runOrderRemapThenMatch() {

        int i;
        int z = 1;

        imgBuffer = new float[srcImage.getExtents()[0] * srcImage.getExtents()[1]];

        if (srcImage.getNDims() == 3) {
            z = srcImage.getExtents()[2];
        }

        /* IntensitySliceNormalization algorithm here */
        int y = 0;
        int histMatchSlice = 0;
        double avg = 0;
        double a = 0;
        double min = 0;
        double max = 0;
        double[] sliceAverage = new double[z];
        double[] sliceMin = new double[z];
        double[] sliceMax = new double[z];

        double averageAVG = 0;
        //double standardevAVG = 0;
        //double CutoffBrightLow = 0;
        //double CutoffBrightHigh = 0;
        double startRange;
        double endRange;
        boolean processIndep = false;

        /* 1. calculate distribution of slice averages */
        for (i = 0; i < z; i++) {

            try {
                srcImage.exportData((i * imgBuffer.length), imgBuffer.length, imgBuffer);

                /* calculate min, max, and average of each slice */
                min = max = imgBuffer[0];

                for (y = 0; y < imgBuffer.length; y++) {
                    avg += imgBuffer[y];

                    if (imgBuffer[y] < min) {
                        min = imgBuffer[y];
                    } else if (imgBuffer[y] > max) {
                        max = imgBuffer[y];
                    }
                }

                avg /= imgBuffer.length;
                sliceAverage[i] = avg;
                sliceMin[i] = min;
                sliceMax[i] = max;
                destImage.importData((i * imgBuffer.length), imgBuffer, false);
            } catch (IOException ex) {
                System.err.println("error exporting data from srcImage in AlgorithmISN");
            }
        }

        /* 2. calculate mean of the slice average distribution */
        for (i = 0; i < z; i++) {
            averageAVG += sliceAverage[i];
        }

        averageAVG /= z;

        /* 3. calculate stddev of the slice average distribution */
        for (i = 0; i < z; i++) {
            a += (sliceAverage[i] - averageAVG) * (sliceAverage[i] - averageAVG);
        }

        a /= z;
        //standardevAVG = Math.sqrt(a);

        /* 4. cutoffs for acceptable range of averages */
        //CutoffBrightLow = averageAVG - standardevAVG;
        //CutoffBrightHigh = averageAVG + (1.0 * standardevAVG);


        boolean twoDIM = true;

        int[] extSlice3D = new int[3];
        extSlice3D[0] = srcImage.getExtents()[0];
        extSlice3D[1] = srcImage.getExtents()[1];
        extSlice3D[2] = 2;

        int[] extSlice2D = new int[2];
        extSlice2D[0] = srcImage.getExtents()[0];
        extSlice2D[1] = srcImage.getExtents()[1];

        AlgorithmChangeType changeTypeAlgo = null;
        AlgorithmHistogramMatch histoMatchAlgo = null;

        if (sliceImage2D == null) {
            sliceImage2D = new ModelImage(srcImage.getType(), extSlice2D, "temp1");
        }

        if (prevSliceImage2D == null) {
            prevSliceImage2D = new ModelImage(srcImage.getType(), extSlice2D, "temp2");
        }

        if (sliceImage3D == null) {
            sliceImage3D = new ModelImage(srcImage.getType(), extSlice3D, "temp3");
        }

        if (prevSliceImage3D == null) {
            prevSliceImage3D = new ModelImage(srcImage.getType(), extSlice3D, "temp4");
        }

        double adjAvg = 0;

        /* 5. single out and "operate" on 'bright' slices */
        for (i = 0; i < z; i++) {

            // if (sliceAverage[i] > CutoffBrightHigh) {
            // System.out.println("Image Slice = " + i);
            /* convert range of bright slice to average range of adjacent slices */
            if (i == 0) {
                startRange = sliceMin[1];
                endRange = sliceMax[1];
                histMatchSlice = 1;
                adjAvg = sliceAverage[1];
                twoDIM = true;
            } else if (i == (z - 1)) {
                startRange = sliceMin[i - 1];
                endRange = sliceMax[i - 1];
                histMatchSlice = i - 1;
                adjAvg = sliceAverage[i - 1];
                twoDIM = true;
            } else {
                startRange = (sliceMin[i - 1] + sliceMin[i + 1]) / 2.0;
                endRange = (sliceMax[i - 1] + sliceMax[i + 1]) / 2.0;
                adjAvg = (sliceAverage[i - 1] + sliceAverage[i + 1]) / 2.0;
                twoDIM = false;
            }

            // System.out.println("diff = " + Math.abs(sliceAverage[i] - adjAvg) + " adjAvg = " + (0.2 *
            // sliceAverage[i]) );
            // more than a 20% difference
            if (Math.abs(sliceAverage[i] - adjAvg) > (0.2 * sliceAverage[i])) {

                // System.out.println("Image Slice = " + i);
                try {

                    if (twoDIM == true) {
                        srcImage.exportData((histMatchSlice * imgBuffer.length), imgBuffer.length, imgBuffer);
                        prevSliceImage2D.importData(0, imgBuffer, true);

                        srcImage.exportData((i * imgBuffer.length), imgBuffer.length, imgBuffer);
                        sliceImage2D.importData(0, imgBuffer, true);

                        // intensity remapping
                        changeTypeAlgo = new AlgorithmChangeType(sliceImage2D, sliceImage2D.getType(), sliceMin[i],
                                                                 sliceMax[i], startRange, endRange, processIndep);
                        changeTypeAlgo.run();


                        // histogram matching here
                        histoMatchAlgo = new AlgorithmHistogramMatch(sliceImage2D, prevSliceImage2D);
                        histoMatchAlgo.run();

                        sliceImage2D.exportData(0, imgBuffer.length, imgBuffer);
                        destImage.importData((i * imgBuffer.length), imgBuffer, false);
                    } else {

                        // remap each original bright into average of adjacent slices * copy result. resulting in 2
                        // images of same.
                        srcImage.exportData(((i - 1) * imgBuffer.length), imgBuffer.length, imgBuffer);
                        prevSliceImage3D.importData(0, imgBuffer, true);
                        srcImage.exportData(((i + 1) * imgBuffer.length), imgBuffer.length, imgBuffer);
                        prevSliceImage3D.importData(imgBuffer.length, imgBuffer, true);

                        srcImage.exportData((i * imgBuffer.length), imgBuffer.length, imgBuffer);
                        sliceImage3D.importData(0, imgBuffer, true);

                        // srcImage.exportData( (i * imgBuffer.length), imgBuffer.length, imgBuffer);
                        sliceImage3D.importData(imgBuffer.length, imgBuffer, true);

                        // intensity remapping
                        changeTypeAlgo = new AlgorithmChangeType(sliceImage3D, sliceImage3D.getType(), sliceMin[i],
                                                                 sliceMax[i], startRange, endRange, processIndep);
                        changeTypeAlgo.run();

                        // histogram matching
                        histoMatchAlgo = new AlgorithmHistogramMatch(sliceImage3D, prevSliceImage3D);
                        histoMatchAlgo.run();

                        sliceImage3D.exportData(0, imgBuffer.length, imgBuffer);
                        destImage.importData((i * imgBuffer.length), imgBuffer, false);
                    }
                } catch (IOException ex) {
                    System.err.println("error exporting data from srcImage in AlgorithmISN");
                }
            }
        }

        destImage.calcMinMax();
        setCompleted(true);
    }
}
