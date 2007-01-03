package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;


/**
 * This algorithm creates a two dimensional histogram of the data in 2 black and white images or 1 color image. if
 * doLinearRescale is true, the range of data in the second image is rescaled to be the same as the range of data in the
 * first image.  That is the second image minimum is rescaled to the first image minimum and the second image maximum
 * is rescaled to the first image maximum.  Also, if linear rescaling is used the number of bins in the second image
 * is set equal to the number of bins in the first image.
 * 
 * The default number of bins equals the min(256,maximum value - minimum value + 1) if the data is not float or double.
 * If the data is float or double, the default number of bins is 256.
 * 
 * ch1 = 0 to ch1 = bin1 - 2 all have a width of (bin1 - 1)/range1,
 * but ch1 = bin1 - 1 has no width, it is a single value at max1.
 * bin ch1 = 0 covers min1 <= ch1 < min1 + (max1 - min1)/(bin1 - 1)
 * bin ch1 = 1 covers min1 + (max1 - min1)/(bin1 - 1) <= ch1 < min1 + 2*(max1 - min1)/(bin1 - 1)
 * bin ch1 = bin1 - 2 covers min1 + (bin1 - 2)*(max - min1)/(bin1 - 1) <= ch1 < max1
 * bin ch1 = bin1 - 1 covers only max1
 * 
 * The processing for maximum values is specially handled to prevent loss of top bin counts from rounding errors.
 */
public class AlgorithmHistogram2Dim extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage baseImage = null;

    /** DOCUMENT ME! */
    private int bin1 = 256;

    /** DOCUMENT ME! */
    private int bin2 = 256;

    /** DOCUMENT ME! */
    private boolean doLinearRescale;

    /** DOCUMENT ME! */
    private boolean useBlue = false;

    /** DOCUMENT ME! */
    private boolean useGreen = false;

    /** DOCUMENT ME! */
    private boolean useRed = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which 2D histogram is placed in a predetermined destination image. Used for 2 black and
     * white images
     *
     * @param  destImg          image model where result image is to stored
     * @param  srcImg           image for x axis of histogram
     * @param  baseImage        image for y axis of histogram
     * @param  doLinearRescale  DOCUMENT ME!
     * @param  bin1             DOCUMENT ME!
     * @param  bin2             DOCUMENT ME!
     */
    public AlgorithmHistogram2Dim(ModelImage destImg, ModelImage srcImg, ModelImage baseImage, boolean doLinearRescale,
                                  int bin1, int bin2) {

        super(destImg, srcImg);
        this.baseImage = baseImage;
        this.doLinearRescale = doLinearRescale;
        this.bin1 = bin1;
        this.bin2 = bin2;
    }

    /**
     * Constructor for images in which 2D histogram is placed in a predetermined destination image. Used for 1 color
     * image.
     *
     * @param  destImg          image model where result image is to stored
     * @param  srcImg           source image model
     * @param  doLinearRescale  DOCUMENT ME!
     * @param  bin1             DOCUMENT ME!
     * @param  bin2             DOCUMENT ME!
     * @param  useRed           DOCUMENT ME!
     * @param  useGreen         DOCUMENT ME!
     * @param  useBlue          DOCUMENT ME!
     */
    public AlgorithmHistogram2Dim(ModelImage destImg, ModelImage srcImg, boolean doLinearRescale, int bin1, int bin2,
                                  boolean useRed, boolean useGreen, boolean useBlue) {

        super(destImg, srcImg);
        this.doLinearRescale = doLinearRescale;
        this.bin1 = bin1;
        this.bin2 = bin2;
        this.useRed = useRed;
        this.useGreen = useGreen;
        this.useBlue = useBlue;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        baseImage = null;
        destImage = null;
        srcImage = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        constructLog();

        if (srcImage.isColorImage()) {
            calcStoreInDestColor();
        } else {
            calcStoreInDest();
        }
    }

    /**
     * This function produces a 2D histogram image with srcImage values represented across the x axis and baseImage
     * values represented across the y axis.
     */
    private void calcStoreInDest() {
        int length;
        int i;

        float[] buffer;
        float[] baseBuffer;
        int[] histBuffer;
        double min1, min2, max1, max2, range1, range2, scale1, scale2;
        double a, b;
        int ch1, ch2;

        // The 2 images should have equal dimensions
        length = srcImage.getExtents()[0];

        for (i = 1; i < srcImage.getNDims(); i++) {
            length *= srcImage.getExtents()[i];
        }

        try {
            buffer = new float[length];
            baseBuffer = new float[length];
            histBuffer = new int[bin1 * bin2];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            baseBuffer = null;
            histBuffer = null;
            errorCleanUp("Algorithm Histogram2Dim: Out of memory", true);

            return;
        }

        for (i = 0; i < histBuffer.length; i++) {
            histBuffer[i] = 0;
        }

        destImage.releaseLock(); // we need to be able to alter the dest image

        try {

        	fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Histogram ...");

            srcImage.exportData(0, length, buffer); // locks and releases lock
            baseImage.exportData(0, length, baseBuffer);

            try {
                fireProgressStateChanged("Processing Image");
                fireProgressStateChanged(45); // a little less than midway
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                      Preferences.DEBUG_ALGORITHM);
                }
            }

            min1 = srcImage.getMin();
            min2 = baseImage.getMin();
            max1 = srcImage.getMax();
            max2 = baseImage.getMax();
            range1 = max1 - min1;
            range2 = max2 - min2;
            scale1 = (bin1 - 1) / range1;
            scale2 = (bin2 - 1) / range2;

            // ch1 = 0 to ch1 = bin1 - 2 all have a width of (bin1 - 1)/range1,
            // but ch1 = bin1 - 1 has no width, it is a single value at max1.
            // bin ch1 = 0 covers min1 <= ch1 < min1 + (max1 - min1)/(bin1 - 1)
            // bin ch1 = 1 covers min1 + (max1 - min1)/(bin1 - 1) <= ch1 <
            //           min1 + 2*(max1 - min1)/(bin1 - 1)
            // bin ch1 = bin1 - 2 covers min1 + (bin1 - 2)*(max - min1)/(bin1 - 1) <= ch1 < max1
            // bin ch1 = bin1 - 1 covers only max1
            if (doLinearRescale) {
                // Rescale the second image data so that its min2 becomes equal to min1 and
                // max2 becomes equal to max1
                // a * max2 + b = max1
                // a * min2 + b = min1
                // a * (max2 - min2) = max1 - min1
                // a * range2 = range1
                // a = range1 / range2
                // (range1/range2) * max2 + b = max1
                // b = max1 - (range1 * max2)/range2
                // b = (max1*range2 - range1 * max2)/range2
                // b = max1*max2 - max1 * min2 - max1 * max2 + min1 * max2)/ range2
                // b = (min1*max2 - max1*min2)/range2
                a = range1 / range2;
                b = ((min1 * max2) - (max1 * min2)) / range2;

                for (i = 0; i < length; i++) {
                    // Handle the top channel separately to prevent its loss due to rounding errors
                    if (baseBuffer[i] == max2) {
                        baseBuffer[i] = (float)max1;
                    }
                    else {
                        baseBuffer[i] = (float) ((a * baseBuffer[i]) + b);
                    }
                }

                min2 = min1;
                max2 = max1;
                range2 = range1;
                scale2 = scale1;
            } // if (doLinearRescale)

            for (i = 0; i < length; i++) {
                // Handle the top channel separately to prevent its loss due to rounding errors
                if (buffer[i] == max1) {
                    ch1 = bin1 - 1;
                }
                else {
                    ch1 = (int)((buffer[i] - min1) * scale1);
                }    

                // Handle the top channel separately to prevent its loss due to rounding errors
                if (baseBuffer[i] == max2) {
                    ch2 = bin2 - 1;
                }
                else {
                    ch2 = (int) ((baseBuffer[i] - min2) * scale2);
                }

                // invert y
                histBuffer[ch1 + (bin1 * (bin2 - 1 - ch2))]++;
            }
            
            // PFH I want to zero out the entries along the main diagonal
            // I am interested in values in he image the do NOT match!!
/*
            int idx, x, y;
            for (y = 0; y < bin1; y++) {
            	for (x = 0; x < bin2; x++) {
            		idx = (bin1 - 1 - y) * bin2 + x;
           		if (idx == 0 || histBuffer[idx] > 0) 
            			System.out.println("Labels  TOSS: " + x + " MANUAL: " + y + "  histVal: " + histBuffer[idx]);
            	}
            }
*/
            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);

        } catch (IOException error) {
            displayError("Algorithm Histogram2Dim reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            baseBuffer = null;
            displayError("Algorithm Histogram2Dim reports: out of memory");
            setCompleted(false);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        
        setCompleted(true);
    }

    /**
     * This function produces a 2D histogram image with first color values represented across the x axis and second
     * color values represented across the y axis.
     */
    private void calcStoreInDestColor() {
        int length;
        int i;

        float[] buffer;
        float[] secondBuffer;
        int[] histBuffer;
        double min1, min2, max1, max2, range1, range2, scale1, scale2;
        double a, b;
        int ch1, ch2;

        length = srcImage.getExtents()[0];

        for (i = 1; i < srcImage.getNDims(); i++) {
            length *= srcImage.getExtents()[i];
        }

        try {
            buffer = new float[length];
            secondBuffer = new float[length];
            histBuffer = new int[bin1 * bin2];
        } catch (OutOfMemoryError oome) {
            buffer = null;
            secondBuffer = null;
            histBuffer = null;
            errorCleanUp("Algorithm Histogram2Dim: Out of memory", true);

            return;
        }

        for (i = 0; i < histBuffer.length; i++) {
            histBuffer[i] = 0;
        }

        destImage.releaseLock(); // we need to be able to alter the dest image

        try {

        	fireProgressStateChanged(srcImage.getImageName(), "Creating 2D Histogram ...");

            if (useRed) {
                srcImage.exportRGBData(1, 0, length, buffer); // locks and releases lock
            } else {
                srcImage.exportRGBData(2, 0, length, buffer);
            }

            if (useBlue) {
                srcImage.exportRGBData(3, 0, length, secondBuffer);
            } else {
                srcImage.exportRGBData(2, 0, length, secondBuffer);
            }

            try {
                fireProgressStateChanged("Processing Image");
                fireProgressStateChanged(45); // a little less than midway
            } catch (NullPointerException npe) {

                if (threadStopped) {
                    Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                      Preferences.DEBUG_ALGORITHM);
                }
            }

            if (useRed) {
                min1 = srcImage.getMinR();
            } else {
                min1 = srcImage.getMinG();
            }

            if (useBlue) {
                min2 = srcImage.getMinB();
            } else {
                min2 = srcImage.getMinG();
            }

            if (useRed) {
                max1 = srcImage.getMaxR();
            } else {
                max1 = srcImage.getMaxG();
            }

            if (useBlue) {
                max2 = srcImage.getMaxB();
            } else {
                max2 = srcImage.getMaxG();
            }

            range1 = max1 - min1;
            range2 = max2 - min2;
            scale1 = (bin1 - 1) / range1;
            scale2 = (bin2 - 1) / range2;

            
            // ch1 = 0 to ch1 = bin1 - 2 all have a width of (bin1 - 1)/range1,
            // but ch1 = bin1 - 1 has no width, it is a single value at max1.
            // bin ch1 = 0 covers min1 <= ch1 < min1 + (max1 - min1)/(bin1 - 1)
            // bin ch1 = 1 covers min1 + (max1 - min1)/(bin1 - 1) <= ch1 <
            //           min1 + 2*(max1 - min1)/(bin1 - 1)
            // bin ch1 = bin1 - 2 covers min1 + (bin1 - 2)*(max - min1)/(bin1 - 1) <= ch1 < max1
            // bin ch1 = bin1 - 1 covers only max1
            if (doLinearRescale) {
                // Rescale the second image data so that its min2 becomes equal to min1 and
                // max2 becomes equal to max1
                // a * max2 + b = max1
                // a * min2 + b = min1
                // a * (max2 - min2) = max1 - min1
                // a * range2 = range1
                // a = range1 / range2
                // (range1/range2) * max2 + b = max1
                // b = max1 - (range1 * max2)/range2
                // b = (max1*range2 - range1 * max2)/range2
                // b = max1*max2 - max1 * min2 - max1 * max2 + min1 * max2)/ range2
                // b = (min1*max2 - max1*min2)/range2
                a = range1 / range2;
                b = ((min1 * max2) - (max1 * min2)) / range2;

                for (i = 0; i < length; i++) {
                    // Handle the top channel separately to prevent its loss due to rounding errors
                    if (secondBuffer[i] == max2) {
                        secondBuffer[i] = (float)max1;
                    }
                    else {
                        secondBuffer[i] = (float) ((a * secondBuffer[i]) + b);
                    }
                }

                min2 = min1;
                max2 = max1;
                range2 = range1;
                scale2 = scale1;
            } // if (doLinearRescale)

            for (i = 0; i < length; i++) {
                // Handle the top channel separately to prevent its loss due to rounding errors
                if (buffer[i] == max1) {
                    ch1 = bin1 - 1;
                }
                else {
                    ch1 = (int)((buffer[i] - min1) * scale1);
                }    

                // Handle the top channel separately to prevent its loss due to rounding errors
                if (secondBuffer[i] == max2) {
                    ch2 = bin2 - 1;
                }
                else {
                    ch2 = (int) ((secondBuffer[i] - min2) * scale2);
                }

                // invert y
                histBuffer[ch1 + (bin1 * (bin2 - 1 - ch2))]++;
            }

            if (threadStopped) { // do before copying back into image
                finalize();

                return;
            }

            destImage.importData(0, histBuffer, true);

        } catch (IOException error) {
            displayError("Algorithm Histogram2Dim reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            secondBuffer = null;
            displayError("Algorithm Histogram2Dim reports: out of memory");
            setCompleted(false);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        
        setCompleted(true);
    }

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {

        if (baseImage != null) {
            historyString = new String("Histogram2Dim(" + doLinearRescale + ", " + bin1 + ", " + bin2 + ", " + "\n");
        } // if (bsaseImage != null)
        else {
            historyString = new String("Histogram2Dim(" + doLinearRescale + ", " + bin1 + ", " + bin2 + ", " + useRed +
                                       ", " + useGreen + ", " + useBlue + ", " + "\n");
        }
    }

}
