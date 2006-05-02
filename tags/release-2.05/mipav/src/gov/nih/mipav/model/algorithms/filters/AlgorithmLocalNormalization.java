package gov.nih.mipav.model.algorithms.filters;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;


/**
 * Local Normalisation equalises colour levels among pixels by removing variations due to lighting; this brings out
 * contrasts in detail.
 *
 * <p>This is done by normalizing a surface-detail version of an image with its slowly varying local average image. That
 * is to say, each pixel of the output image is the value at that location of the pixel of the high-frequency image
 * divided by the pixel at that same location of the low-frequency image.</p>
 *
 * <p>The surface-detail image is generated by using an unsharp masking algorithm, and the local average image is a
 * low-pass image generated using a general-purpose (but fast) frequency filter.</p>
 *
 * <p>References:</p>
 *
 * <ol>
 *   <li>Local Normalization. <a href="http://bigwww.epfl.ch/demo/normalize/desc.html">
 *     http://bigwww.epfl.ch/demo/normalize/desc.html</a></li>
 *   <li>Halyo, Nesim; Rahman, Zia-ur; Park, Stephen. "Information Content in Nonlinear Local Normalization Processing
 *     of Digital Images". College of William and Mary. Williamsburg, Virgiana.</li>
 * </ol>
 *
 * @see  AlgorithmUnsharpMask
 * @see  AlgorithmFrequencyFilter
 */
public class AlgorithmLocalNormalization extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The ViewUserInterface is held for debugging purposes only. May be deprecated in the future. */
    ViewUserInterface userInterface; // for debug only

    /** the AlgorithmFrequencyFilter, to lo-pass filter the image. */
    private AlgorithmFrequencyFilter FFTer;

    /** FrequencyFilter's low-pass cut-off frequency. */
    private float frequency;

    /** Images that are solely 2D images set this to true; images with other dimensionality are false. */
    private boolean is2d;

    /** Colour images set this to true; monochromatic should be false;. */
    private boolean isColourImage;

    /** Required by the low pass filter, but is unused with these options. */
    private int kernelDiameter;

    /** temporary image-data buffer to store (hi-pass)/(lo-pass) data. */
    private float[] norm; // one slice long to store [hi-pass]/[lo-pass]

    /** when true, indicates whether to process the colour channel. */
    private boolean rChannel, gChannel, bChannel;

    /** UnsharpMask sigmas, for blurring. */
    private float[] sigmas;

    /** a temporary image array. */
    private ModelImage[] tempImage;

    /** the UnsharpMask algorithm, to hi-pass filter the image. */
    private AlgorithmUnsharpMask unsharper;

    /** Indicitive of the number of colours per pixel. Monochrome is 1; Colour is 4 (alpha, red, green, blue). */
    private int valuesPerPixel = 1;

    /** UnsharpMask weighting factor. */
    private float weightingFactor;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * This constructor initialises a Local normalisation algorithm for a source and destination image, and ensures that
     * the destination image is <code>ModelStorageBase.FLOAT</code>.
     *
     * <p>Currently (8 May 2003), this algorithm does not support replacing the original data set with that of the
     * locally-normalized image.</p>
     *
     * @param  dest      DOCUMENT ME!
     * @param  src       DOCUMENT ME!
     * @param  sigmas    DOCUMENT ME!
     * @param  weight    DOCUMENT ME!
     * @param  kernSize  DOCUMENT ME!
     * @param  freq      DOCUMENT ME!
     */
    public AlgorithmLocalNormalization(ModelImage dest, ModelImage src, float[] sigmas, /* Gaussian's standard deviation
                                                                                         * in each dimension */
                                       float weight, /* weighting factor for gaussian */
                                       int kernSize, /* width of gaussian kernel */
                                       float freq /* bounding frequency for FFT-filter */) {
        super(dest, src);
        tempImage = new ModelImage[2];
        norm = new float[src.getExtents()[0] * src.getExtents()[1]];

        // tempImage[0] = tempImage[1] = (ModelImage) srcImage.clone();
        destImage.reallocate(ModelStorageBase.FLOAT);

        this.sigmas = sigmas;
        weightingFactor = weight;
        frequency = freq;
        kernelDiameter = kernSize;

        is2d = (srcImage.getNDims() == 2);

        if (src.isColorImage()) {
            isColourImage = true;
            valuesPerPixel = 4;
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();

        // clean up the tempImages!!
        if (tempImage != null) {

            for (int i = 0; i < tempImage.length; i++) {

                if (tempImage[i] != null) {
                    tempImage[i].disposeLocal();
                    tempImage[i] = null;
                }
            }
        }
    }

    /**
     * Standard algorithm run method. It will not run if the source Image is <code>null</code>. The calculation is done
     * and placed in a separate destination image if it is to be stored there.
     */
    public void runAlgorithm() {

        if (srcImage == null) {
            displayError("Source Image is null");

            return;
        }

        constructLog();

        if (destImage != null) { // if there exists a destination image
            calcStoreInDest();
        } else { // there is no image but the original source.
            calcInPlace();
        }
    }

    /**
     * RGB images are local-normalized by 'channel.' That is, each colour, red, blue and green, is filtered
     * independantly of the other two colours. This filter permits selectively filtering any combination of the three
     * channels instead of simply filtering all three. True for any of the arguments enforces filtering that channel.
     *
     * @param  r  Filter red channel.
     * @param  g  Filter green channel.
     * @param  b  Filter blue channel.
     */
    public void setRGBChannelFilter(boolean r, boolean g, boolean b) {

        if (isColourImage) { // just in case somebody called for a mono image
            rChannel = r;
            gChannel = g;
            bChannel = b;
        }
    }

    /**
     * The ViewUserInterface is set for debugging purposes only. This method may be deprecated in the future.
     *
     * @param  vui  DOCUMENT ME!
     */
    public void setUserInterface(ViewUserInterface vui) {
        userInterface = vui;
    }

    /**
     * Filters the source image. Replaces the original image with the filtered image.
     *
     * <p><em>Does not currently work.</em></p>
     */
    private void calcInPlace() {
        errorCleanUp("AlgorithmLocalNormalization: " + "Replace Image not yet implemented", false);
        finalize();

        return;
    }

    /**
     * This function produces a local-normalized image into a ModelImage that does not replace the original image-data.
     */
    private void calcStoreInDest() {
        int datalen = srcImage.getSliceSize();
        float[] imageData;

        if (srcImage.getNDims() == 3) {
            datalen *= srcImage.getExtents()[2];
        } else if (srcImage.getNDims() == 4) {
            datalen *= srcImage.getExtents()[2] * srcImage.getExtents()[3];
        }

        try {
            imageData = new float[datalen];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmLocalNormalization: " + "out of memory creating imageData", false);
            finalize();

            return;
        }

        try {
            srcImage.exportData(0, datalen, imageData);
        } catch (IOException ioe) {
            errorCleanUp("AlgorithmLocalNormalization: " + "failure to export imageData", false);
            finalize();

            return;
        }

        if (tempImage[0] == null) {
            tempImage[0] = (ModelImage) srcImage.clone();
            tempImage[0].reallocate(ModelStorageBase.FLOAT);

            try {
                tempImage[0].importData(0, imageData, true);
            } catch (IOException ioe) {
                errorCleanUp("AlgorithmLocalNormalization: " + "failure to import imageData", true);
                finalize();

                return;
            }
        }

        if (tempImage[1] == null) {
            tempImage[1] = (ModelImage) tempImage[0].clone();
        }

        unsharper = new AlgorithmUnsharpMask(tempImage[0], sigmas, weightingFactor, true, true);

        // don't set to a new Thread just yet.  let run serially.
        // (ie., in current Thread):
        unsharper.setActiveImage(activeImage);
        unsharper.run();

        // if the algorithm failed:
        if (!unsharper.isCompleted()) {
            Preferences.debug("AlgorithmLocalNormalization: " + "failure to complete unsharp mask");
            errorCleanUp(null, false);
            finalize();

            return;
        }

        /* this change is silly.  FreqFilter should know better.  but if not,
         * freqfilter either breaks for 2d or for 3d images.
         */
        FFTer = new AlgorithmFrequencyFilter(tempImage[1], !is2d, false, kernelDiameter,
                                             AlgorithmFrequencyFilter.LOWPASS, frequency, 0,
                                             AlgorithmFrequencyFilter.GAUSSIAN, 0);

        // don't set to a new Thread just yet.  let run serially.
        // (ie., in current Thread):
        FFTer.setActiveImage(activeImage);
        FFTer.run();

        // if the algorithm failed:
        if (!FFTer.isCompleted()) {
            Preferences.debug("AlgorithmLocalNormalization: " + "failure to complete FFT");
            errorCleanUp(null, false);
            finalize();

            return;
        }

        normalize(); // note, this is not a properly nice Thread method.

        //        for testing: we don't need these when normalize runs:
        //        tempImage[0].disposeLocal();
        //        tempImage[1].disposeLocal();
        //        tempImage[0]=null;
        //        tempImage[1]=null;
        imageData = null;

        if (threadStopped) {
            setCompleted(false);
            finalize();

            return;
        }

        // try cleaning the algorithms:
        unsharper = null;
        FFTer = null;
        norm = null;

        setCompleted(true);

    } // end calcStoreInDest()

    /**
     * Constructs a string of the contruction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        String sigmaStr = " [";

        for (int i = 0; i < sigmas.length; i++) {
            sigmaStr += String.valueOf(sigmas[i]);

            // append a comma when there are more sigmas:
            sigmaStr += (i < (sigmas.length - 1)) ? ", " : "";
        }

        sigmaStr += "]";

        historyString = "local normalization(" + sigmaStr + ", " + String.valueOf(kernelDiameter) + ", " +
                        String.valueOf(weightingFactor) + ", " + String.valueOf(frequency) + ")\n";
    }


    /**
     * takes the data of the two images, one unsharp'd and the other gaussian blur'd, and divides the two pixel-by-pixel
     * to find the locally normalized value for each pixel.
     *
     * @param   top  DOCUMENT ME!
     * @param   bot  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] doImageDivision(float[] top, float[] bot) {

        for (int i = 0; i < top.length; i++) {
            norm[i] = top[i] / bot[i];
        }

        return norm;
    }

    /**
     * works depending on the extent of the image and the colourisation of the source image in question.
     *
     * <p>calls to doImageDivision(...) for as many slices (and selected colour channels) as necessary to finish off the
     * image.</p>
     *
     * @see  #doImageDivision(float[], float[])
     */
    private void normalize() {

        int i;
        int nslices = (is2d) ? 1 : srcImage.getExtents()[2];

        // set up image-data buffers:
        float[] unsharpd = new float[srcImage.getExtents()[0] * srcImage.getExtents()[1]];
        float[] FFTd = new float[srcImage.getExtents()[0] * srcImage.getExtents()[1]];

        try {

            if (!isColourImage) {
                int count = 0;

                for (i = 0; i < nslices; i++) {
                    tempImage[0].exportSliceXY(i, unsharpd);
                    tempImage[1].exportSliceXY(i, FFTd);

                    // using length of unsharpd because it is the correct
                    // length, and I don't need any more variables.  we could
                    // use FFTd.length here for just the same effect.
                    destImage.importData(i * unsharpd.length, doImageDivision(unsharpd, FFTd), false);

                    // check for stopped thread
                    if ((count % 10) == 0) {

                        if (threadStopped) {
                            return;
                        }
                    }

                    count++;
                    //                    System.out.println("dest is of type: "+
                    //                            destImage.getType());
                }

                destImage.calcMinMax();
            } else if (is2d && isColourImage) {

                // System.out.println("(2d, and colour) not yet.");
                errorCleanUp("AlgorithmLocalNormalization: " + "2d and colour not yet supported", false);
                finalize();
            } else { // if (!is2d && isColourImage)

                // System.out.println("(else)not yet.");
                errorCleanUp("AlgorithmLocalNormalization: " + "colour not yet supported", false);
                finalize();
            }
        } catch (IOException ioe) {
            errorCleanUp("AlgorithmLocalNormalization: " + "IO exception: " + ioe.getMessage(), false);

            // System.out.println("throwing ioexception.  why?  dunno.");
            ioe.printStackTrace();
            finalize();
        } finally {

            // no matter what --- clean up the tempImages!!
            try {
                tempImage[0].disposeLocal();
                tempImage[1].disposeLocal();
                tempImage[0] = null;
                tempImage[1] = null;
            } catch (NullPointerException tempnull) {
                Preferences.debug("AlgorithmLocalNormalization: " + "TempImages already null.  Nothing to do, " +
                                  "so moving along.\n");
            }
        }
    }

}
