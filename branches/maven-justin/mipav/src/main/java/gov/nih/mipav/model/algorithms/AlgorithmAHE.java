package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.*;
import java.util.Arrays;


/**
 * algorithm to apply an adaptive histogram to an image, placing it in a new ModelImage, or returning the changed
 * picture to the same image. Running this algorithm with a <code>null</code> destination image causes the algorithm to
 * replace the source image with the resultant image.
 *
 * <p>This is a 'regional' algorithm, that is it divides the image buffer into (mostly) equi-sized regions (a 'window')
 * and forms the histogram from that region. The value of the cell (the brightness of the pixel at a particular
 * location) is then mapped to the region. Over the entire image, this means that all pixels will still be brighter or
 * darker than other pixels, but the brightness has been shifted making minor variations more apparent. The cumulative
 * distribution is changed so that it becomes step-wise linear, making the darker pixels use the lowest possible values
 * available to the image, the brightest use the highest possible values.</p>
 *
 * <p>This algorithm may be used to perform a contrast-limited or 'clamped' histogram equalization. It is set by
 * supplying a percentage of the maximum number of pixels in any particular brightness level. As the regional histograms
 * are tabulated, the brightness with the maximum number of pixels attributed to it is remembered. The algorithm will
 * then evenly redistibute the total number of pixels from any brightness which has a greater number than the max times
 * this fraction to all other (less populous) brightnesses. (If the supplied per-centage is 80, for Instance, the
 * maximum number of pixels to any brightness level, will be four-fifths the largest number number of pixels of any
 * shade found.</p>
 *
 * <p>The principle methodology of this algorithm, is to take each window, form a histogram; clamp so there is no
 * brightness which can have more than a certain number of pixels; scale the brightnesses so that the values are spread
 * evenly from the maximum to the minimum brightness. (ie., scale * histo(i)).</p>
 *
 * <ul>
 *   <li>Primary source: Stark, J Alex. <u>Adaptive Image Contrast Enhancement Using Generalizations of Histgram
 *     Equalization</u>.</li>
 *   <li>Background Information:
 *
 *     <ul>
 *       <li>Russ, John. <u>Image Processing HandbookM</u>.</li>
 *       <li>Tidestav, Claes. <u>Short Introduction to Adaptive Equalization</u>.</li>
 *       <li>Rabie, Tamer; Rangayan, Rangaraj; Paranjape, Raman. <u>Adaptive-Neighborhood Image Deblurring</u>.</li>
 *       <li>Lyon, Douglas. <u>Image Processing in Java</u></li>
 *     </ul>
 *   </li>
 * </ul>
 *
 * <p>According to Freedman and Diaconis as summarized in "Recent Developments in NonParametric Density Estimation" by
 * Alan Julian Izenman, Journal of the American Statistical Association, March, 1991, Vol. 86, No. 413, pp. 205 - 224:
 * The ideal histogram bin width W is given by W = 2(IQR)pow(N,-1/3) where IQR is the inrterquartile range(the 75
 * percentile minus the 25th percentile) and N is the number of available samples.</p>
 * 
 * The three coordinates of CIELAB represent the lightness of the color(L* = 0 yields black and L* = 100 indicates diffuse 
 * white; specular white may be higher), its position between red/magenta and green(a*, negative values indicate green
 * while positive values indicate magenta) and its position between yellow and blue(b*, negative values indicate blue 
 * and positive values indicate yellow).  The asterisk(*) after L, a, and b are part of the full name, since they represent 
 * L*, a*, and b*, to distinguish them from Hunter's L, a, and b.
 * 
 * The L* coordinate ranges from 0 to 100.  The possible range of a* and b* coordinates depends on the color space that one
 * is converting from.  
 * R = 0, G = 0, B = 0 => L* = 0, a* = 0, b* = 0
 * R = 255, G = 0, B = 0 => L* = 53.2, a* = 80.1, b* = 67.22
 * R = 0, G = 255, B = 0 => L* = 87.7, a* = -86.2, b* = 83.2
 * R = 0, G = 0, B = 255 => L* = 32.3, a* = 79.2, b* = -107.9
 * R = 255, G = 255, B = 0 => L* = 97.1, a* = -21.6, b* = 94.5
 * R = 255, G = 0, B = 255 => L* = 60.3, a* = 98.3, b* = -60.8
 * R = 0, G = 255, B = 255 => L* = 91.1, a* = -48.1, b* = -14.1
 * R = 255, G = 255, B = 255 => L* = 100.0, a* = 0.00525, b* = -0.0104
 * so the range of a* equals about the range of b* and the range of a* equals about twice the range of L*.
 * The simplest distance metric delta E is CIE76 = sqrt((L2* - L1*)**2 + (a2* - a1*)**2 + (b2* - b1*)**2)
 * 
 * XW, YW, and ZW (also called XN, YN, ZN or X0, Y0, Z0) are reference white tristimulus values - typically the white
 * of a perfectly reflecting diffuser under CIE standard D65 illumination(defined by x = 0.3127 and y = 0.3291 in the
 * CIE chromatcity diagram).  The 2 degrees, D65 reference tristimulus values are: XN = 95.047, YN = 100.000, and ZN = 1
 * 
 *  http://www.easyrgb.com has XYZ -> RGB, RGB -> XYZ, XYZ -> CIEL*ab, CIEL*ab -> XYZ, and
 *     XYZ(Tristimulus) Reference values of a perfect reflecting diffuser.
 *     
 *     
 *  Smoothing vs. sharpening of color images - Together or separated by Cristina Perez, Samuel Morillas, and Alberto Conejero,
 *  June, 2017.  "Histogram equalization is a non-linear process and involves intensity values of the image and not the color
 *  components  For these reasons, channel splitting and equalizing each channel separately is not the proper way for
 *  equalization of contrast.  So, the first step is to convert the color space of the image from RGB into other color
 *  space which separates intensity values from color components such as HSV, YCbCr, or Lab, and apply equalization over the
 *  H, Y, or L channel respectively."
 *  
 *
 * @version  1.01; 20 Sep 2001
 * @author   David Parsons (parsonsd)
 * @author   Matthew J. McAuliffe, Ph.D.
 */


public class AlgorithmAHE extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** If true filter the blue channel. */
    private boolean bChannel = true;

    /** DOCUMENT ME! */
    private float bufMax;

    /** DOCUMENT ME! */
    private float bufMin;


    /** Perform a "contrast limited" AHE. */
    private boolean clamped = false;

    /** DOCUMENT ME! */
    private float clipLevel = 0;

    /** If true filter the green channel. */
    private boolean gChannel = true;

    /** number of divisions to make in the height. */
    private int hDivisions;

    /** DOCUMENT ME! */
    private float imageOffset;

    /** Indicates the image being messed with is a color image. */
    private boolean isColorImage = false;

    /** If true filter the red channel. */
    private boolean rChannel = true;
    
    private boolean useCIELab = false;

    /** DOCUMENT ME! */
    private float[] sortBuffer;

    /** Number of elements in a pixel. Monochrome = 1, Color = 4. (a, R, G, B) */
    private int valuesPerPixel = 1;

    /** number of divisions to make in the width. */
    private int wDivisions;
    
    private double imageMax;
    
    // Scale factor used in RGB-CIELab conversions.  255 for ARGB, could be higher for ARGB_USHORT.
    private double scaleMax = 255.0;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor for images in which changes are returned to the source image.
     *
     * @param  srcImg  source image model
     * @param  hd      number of divisions to make of the image height
     * @param  wd      number of divisions to make of the image width
     * @param useCIELab If color, only equalize on L in CIELab space
     */
    public AlgorithmAHE(ModelImage srcImg, int hd, int wd, boolean useCIELab) {
        super(null, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
            srcImage.calcMinMax();
            imageMax = srcImage.getMax();
            scaleMax = Math.max(255.0, imageMax);
        }

        hDivisions = hd;
        wDivisions = wd;
        this.useCIELab = useCIELab;

        try { // slower if this must be done every slice.
            sortBuffer = new float[srcImage.getExtents()[0] * srcImage.getExtents()[1]];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmAHE reports: out of memory", true);
            setCompleted(false);
        }
    }

    /**
     * Constructor for images in which changes are placed in a predetermined destination image.
     *
     * @param  destImg  image model where result image is to stored
     * @param  srcImg   source image model
     * @param  hd       number of divisions to make of the image height
     * @param  wd       number of divisions to make of the image width
     * @param  useCIELab If color, only equalize on L in CIELab space
     */
    public AlgorithmAHE(ModelImage destImg, ModelImage srcImg, int hd, int wd, boolean useCIELab) {

        super(destImg, srcImg);

        if (srcImg.isColorImage()) {
            isColorImage = true;
            valuesPerPixel = 4;
            srcImage.calcMinMax();
            imageMax = srcImage.getMax();
            scaleMax = Math.max(255.0, imageMax);
        }

        hDivisions = hd;
        wDivisions = wd;
        this.useCIELab = useCIELab;

        try { // slower if this must be done every slice.
            sortBuffer = new float[srcImage.getExtents()[0] * srcImage.getExtents()[1]];
        } catch (OutOfMemoryError oome) {
            errorCleanUp("AlgorithmAHE reports: out of memory", true);
            setCompleted(false);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        destImage = null;
        srcImage = null;
        sortBuffer = null;
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

        

        // destinationImage being null sets the algo to
        // put replace the source image with the
        // resultant image.
        if (destImage != null) { // if there exists a destination image

            if (srcImage.getNDims() == 2) {
                calcStoreInDest2D();
            } else if (srcImage.getNDims() > 2) {
                calcStoreInDest3D();
            }
        } else { // the destination is the original source.

            if (srcImage.getNDims() == 2) {
                calcInPlace2D();
            } else if (srcImage.getNDims() > 2) {
                calcInPlace3D();
            }
        }
    }

    /**
     * the Clip Level is a percentage of the maximum number of pixels in any particular brightness level. This method
     * allows one to set that percentage. As the regional histograms are tabulated, the brightness with the maximum
     * number of pixels attributed to it is remembered. The algorithm will then evenly redistibute the total number of
     * pixels from any brightness which has a greater number than the max times this fraction to all other (less
     * populous) brightnesses.
     *
     * @param  sectorPercentage  DOCUMENT ME!
     */
    public void setClipLevel(int sectorPercentage) {
        clipLevel = (float) (sectorPercentage) / 100;
    }

    /**
     * Redistribute from brightnesses with at a least a specified percentage of the brightness with the maximum number
     * of pixels.
     *
     * @param  beClamped  DOCUMENT ME!
     */
    public void setContrastLimited(boolean beClamped) {
        clamped = beClamped;
    }


    /**
     * RGB images are histogram equalized by 'channel.' That is, each color, red, blue and green, is run independantly
     * of the other two colors. This permits selectively filtering any combination of the three channels instead of
     * simply trying to handle all three at once. True filters that channel.
     *
     * @param  r  DOCUMENT ME!
     * @param  g  DOCUMENT ME!
     * @param  b  DOCUMENT ME!
     */
    public void setRGBChannelFilter(boolean r, boolean g, boolean b) {

        if (isColorImage) { // just in case somebody called for a mono image
            rChannel = r;
            gChannel = g;
            bChannel = b;
        }
    }

    /**
     * gets the bottom end of the image. for any image but color images, the bottom end is defined as the buffer minimum
     * for all images except color images; there the offset is defined to be zero
     *
     * @param  type  DOCUMENT ME!
     */
    protected final void findImageOffset(int type) {
        imageOffset = bufMin;

        if (isColorImage) { // color
            imageOffset = 0.0f;
        }

        Preferences.debug("image offset is: " + imageOffset, Preferences.DEBUG_ALGORITHM);

        return;
    }

    /**
     * finds the total number of bins based on the type of image.
     *
     * @param   type   DOCUMENT ME!
     * @param   ideal  DOCUMENT ME!
     *
     * @return  the total number of bins.
     */
    protected final int findTotalBins(int type, int ideal) {
        int totalBins;

        if ((type == ModelStorageBase.UBYTE) || (type == ModelStorageBase.BYTE) || (type == ModelStorageBase.ARGB)) {
            totalBins = Math.min(ideal, 256);
        } else if ((type == ModelStorageBase.FLOAT) || (type == ModelStorageBase.DOUBLE) ||
                       (type == ModelStorageBase.ARGB_FLOAT)) {
            totalBins = ideal;
        } else {
            totalBins = Math.min(ideal, (int) (bufMax - bufMin + 1));
        }

        return totalBins;
    }

    /**
     * Histogram equalization on the source image. Replaces the original image with the filtered image.
     */
    private void calcInPlace2D() {
        int color;
        int length = srcImage.getExtents()[0] * srcImage.getExtents()[1]; // total number of data-elements (pixels) in
                                                                          // image

        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        float[] resultBuffer; // copy-to buffer (for pixel data) for image-data after filtering
        float[] L = null;
        float[] a = null;
        float[] b = null;
        float[] Leq = null;

        length = srcImage.getExtents()[0] * srcImage.getExtents()[1];

        try {
        	if (useCIELab) {
        		buffer = new float[4*length];
        		L = new float[length];
        		a = new float[length];
        		b = new float[length];
        		Leq = new float[length];
        		resultBuffer = new float[4*length];
        	}
        	else {
                buffer = new float[length];
                resultBuffer = new float[length];
        	}
       
        } catch (OutOfMemoryError oome) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmAHE reports: out of memory", true);

            return;
        }

        try {
            fireProgressStateChanged(srcImage.getImageName(), "Equalizing Histogram ...");
            

            if (!isColorImage) {
                srcImage.exportData(0, length, buffer); // locks and releases lock

                try {
                    fireProgressStateChanged("Processing Image");
                    fireProgressStateChanged(45); // not quite midway
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                this.monoSliceFilter(buffer, resultBuffer);

                if (threadStopped) { // do BEFORE buffer has been exported to Image
                    finalize();

                    return;
                }

                srcImage.importData(0, resultBuffer, true);
            } else if (useCIELab) {
            	srcImage.exportData(0, 4*length, buffer); // locks and releases lock

                try {
                    fireProgressStateChanged("Processing Image");
                    fireProgressStateChanged(45); // not quite midway
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }
                
                convertRGBtoCIELab(buffer, L, a, b);
                monoSliceFilter(L, Leq);
                convertCIELabtoRGB(Leq, a, b, resultBuffer);
                srcImage.importData(0, resultBuffer, true);
            } else { // if (isColorImage) {

                for (color = 1; (color < valuesPerPixel) && !threadStopped; color++) { // for each color

                    if (((color == 1) && rChannel) || ((color == 2) && gChannel) // process only desired channels
                            || ((color == 3) && bChannel)) { // -- and not alpha channel at all --

                       
                    	fireProgressStateChanged((int) (((float) (color) / valuesPerPixel) * 100));
    	                            fireProgressStateChanged("Processing color " + Integer.toString(color));
                     
                        srcImage.exportRGBData(color, 0, length, buffer); // get the slice

                        monoSliceFilter(buffer, resultBuffer);


                        if (threadStopped) { // do BEFORE buffer has been exported to Image
                            finalize();

                            return;
                        }

                        // then store the channel back into the destination image
                        srcImage.importRGBData(color, 0, resultBuffer, false);
                    }
                } // for (color = 1; color < valuesPerPixel && !threadStopped; color++)

                srcImage.calcMinMax();
            } // else isColorImage
        } catch (IOException error) {
            errorCleanUp("AlgorithmAHE reports: source image locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("AlgorithmAHE reports: out of memory", true);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        // srcImage.releaseLock(); // we didn't want to allow the image to be adjusted by someone else
        

        setCompleted(true);
    }

    /**
     * Histogram Equalization on the source image and replaces the source image with the processed image.
     */
    private void calcInPlace3D() {
        int i, color;
        int length;
        int nSlices = srcImage.getExtents()[2];
        float[] buffer;
        float[] resultBuffer;
        float[] L = null;
        float[] a = null;
        float[] b = null;
        float[] Leq = null;

        int numColors = 0;

        if (rChannel) {
            numColors++;
        }

        if (gChannel) {
            numColors++;
        }

        if (bChannel) {
            numColors++;
        }

        int colorUsed = 0;


        length = srcImage.getExtents()[0] * srcImage.getExtents()[1];

        try {
        	if (useCIELab) {
        		buffer = new float[4*length];
        		L = new float[length];
        		a = new float[length];
        		b = new float[length];
        		Leq = new float[length];
        		resultBuffer = new float[4*length];
        	}
        	else {
                buffer = new float[length];
                resultBuffer = new float[length];
        	}
        } catch (OutOfMemoryError oome) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHE: Out of memory", true);

            return;
        }

        try {
            fireProgressStateChanged(srcImage.getImageName(), "Equalizing Histogram ...");
            

            if (!isColorImage) {

                // image length is length in 3 dims
                for (i = 0; (i < nSlices) && !threadStopped; i++) {

                 
                	fireProgressStateChanged((int) (((float) (i) / nSlices) * 100));
                	fireProgressStateChanged("Processing slice " + Integer.toString(i + 1));
                   

                    srcImage.exportData(length * i, length, buffer); // locks and releases lock
                    monoSliceFilter(buffer, resultBuffer);

                    if (threadStopped) { // do BEFORE buffer has been exported to Image
                        finalize();

                        return;
                    }

                    srcImage.importData(length * i, resultBuffer, false);
                } // for (i = 0; i < nSlices  && !threadStopped; i++)
            } // if (!isColorImage)
            else if (useCIELab) {
            	// image length is length in 3 dims
                for (i = 0; (i < nSlices) && !threadStopped; i++) {

                 
                	fireProgressStateChanged((int) (((float) (i) / nSlices) * 100));
                	fireProgressStateChanged("Processing slice " + Integer.toString(i + 1));
                   

                    srcImage.exportData(4 * length * i, 4 * length, buffer); // locks and releases lock
                    convertRGBtoCIELab(buffer, L, a, b);
                    monoSliceFilter(L, Leq);
                    convertCIELabtoRGB(Leq, a, b, resultBuffer);

                    if (threadStopped) { // do BEFORE buffer has been exported to Image
                        finalize();

                        return;
                    }

                    srcImage.importData(4 * length * i, resultBuffer, false);
                } // for (i = 0; i < nSlices  && !threadStopped; i++)	
            } // else if (useCIELab)
            else { // if (isColorImage) {

                for (color = 1; (color < valuesPerPixel) && !threadStopped; color++) {

                    if (((color == 1) && rChannel) || ((color == 2) && gChannel) // and if it is a desired color
                            || ((color == 3) && bChannel)) { // --and not alpha--  process it// for each color
                        colorUsed++;

                        for (i = 0; (i < nSlices) && !threadStopped; i++) { // get all the slices

                    
                        	fireProgressStateChanged((int) (((float) ((Math.max(0, colorUsed - 1) * nSlices) +
                                                                             i) / (numColors * nSlices)) * 100));

                        	if ((color == 1) && rChannel) {
                        		fireProgressStateChanged("Processing red slice " + Integer.toString(i + 1));
                        	} else if ((color == 2) && gChannel) {
                        		fireProgressStateChanged("Processing green slice " + Integer.toString(i + 1));
                        	} else if ((color == 3) && bChannel) {
                        		fireProgressStateChanged("Processing blue slice " + Integer.toString(i + 1));
                        	}
                             

                            srcImage.exportRGBData(color, 4 * length * i, length, buffer); // grab the next slice

                            monoSliceFilter(buffer, resultBuffer);

                            if (threadStopped) { // do BEFORE buffer has been exported to Image
                                finalize();

                                return;
                            }

                            srcImage.importRGBData(color, 4 * length * i, resultBuffer, false);
                        } // for (i = 0; i < nSlices && !threadStopped; i++)
                    }
                } // for (color = 0; color < valuesPerPixel && !threadStopped; color++)
            } // else isColorImage

            srcImage.calcMinMax();
        } catch (IOException error) {
            errorCleanUp("Algorithm AHE:  image locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHE: Out of memory", true);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        
        setCompleted(true);
    }

    /**
     * This function produces a new image that has had itself histogram equalized. places filtered image in the
     * destination image.
     */
    private void calcStoreInDest2D() {
        int color;
        int length = srcImage.getExtents()[0] * srcImage.getExtents()[1]; // total number of data-elements (pixels) in
                                                                          // image

        float[] buffer; // data-buffer (for pixel data) which is the "heart" of the image
        float[] resultBuffer; // copy-to buffer (for pixel data) for image-data after filtering
        float[] L = null;
        float[] a = null;
        float[] b = null;
        float[] Leq = null;

        try {
        	if (useCIELab) {
        		buffer = new float[4*length];
        		L = new float[length];
        		a = new float[length];
        		b = new float[length];
        		Leq = new float[length];
        		resultBuffer = new float[4*length];
        	}
        	else {
                buffer = new float[length];
                resultBuffer = new float[length];
        	}
        } catch (OutOfMemoryError oome) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHE: Out of memory", true);

            return;
        }

        destImage.releaseLock(); // we need to be able to alter the dest image

        try {
            fireProgressStateChanged(srcImage.getImageName(), "Equalizing Histogram ...");
            

            if (!isColorImage) {
                srcImage.exportData(0, length, buffer); // locks and releases lock

                try {
                    fireProgressStateChanged("Processing Image");
                    fireProgressStateChanged(45); // a little less than midway
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                this.monoSliceFilter(buffer, resultBuffer);

                if (threadStopped) { // do before copying back into image
                    finalize();

                    return;
                }

                destImage.importData(0, resultBuffer, true);
            }
            else if (useCIELab) {
            	srcImage.exportData(0, 4 * length, buffer); // locks and releases lock

                try {
                    fireProgressStateChanged("Processing Image");
                    fireProgressStateChanged(45); // a little less than midway
                } catch (NullPointerException npe) {

                    if (threadStopped) {
                        Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                          Preferences.DEBUG_ALGORITHM);
                    }
                }

                convertRGBtoCIELab(buffer, L, a, b);
                monoSliceFilter(L, Leq);
                convertCIELabtoRGB(Leq, a, b, resultBuffer);

                if (threadStopped) { // do before copying back into image
                    finalize();

                    return;
                }

                destImage.importData(0, resultBuffer, true);
            } else { // if (isColorImage) {

                for (color = 0; (color < valuesPerPixel) && !threadStopped; color++) { // for each color

             
                	fireProgressStateChanged((int) (((float) (color) / valuesPerPixel) * 100));
                	fireProgressStateChanged("Processing color " + Integer.toString(color));
              
                    srcImage.exportRGBData(color, 0, length, buffer); // grab the slice

                    if (((color == 1) && rChannel) || ((color == 2) && gChannel) // process only desired channels
                            || ((color == 3) && bChannel)) { // -- and not alpha channel at all --
                        monoSliceFilter(buffer, resultBuffer);
                    } else { // don't process this color, just copy it as found
                        copyBuffers(resultBuffer, buffer);
                    }

                    if (threadStopped) { // do before copying back into image
                        finalize();

                        return;
                    }

                    // then store the channel back into the destination image
                    destImage.importRGBData(color, 0, resultBuffer, false);
                }

                destImage.calcMinMax();
            }
        } catch (IOException error) {
            displayError("Algorithm AHE reports: image locked");
            setCompleted(false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            displayError("Algorithm AHE reports: out of memory");
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
     * This function produces a new volume image that has been histogram equalized. The Image is changed by filtering
     * each slice individually.
     */
    private void calcStoreInDest3D() {
        int i, color;
        int length = srcImage.getExtents()[0] * srcImage.getExtents()[1];
        int nSlices = srcImage.getExtents()[2];
        float[] buffer;
        float[] resultBuffer;
        float[] L = null;
        float[] a = null;
        float[] b = null;
        float[] Leq = null;

        int numColors = 0;

        if (rChannel) {
            numColors++;
        }

        if (gChannel) {
            numColors++;
        }

        if (bChannel) {
            numColors++;
        }

        int colorUsed = 0;


        try {
        	if (useCIELab) {
        		buffer = new float[4*length];
        		L = new float[length];
        		a = new float[length];
        		b = new float[length];
        		Leq = new float[length];
        		resultBuffer = new float[4*length];
        	}
        	else {
                buffer = new float[length];
                resultBuffer = new float[length];
        	}
        } catch (OutOfMemoryError oome) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHE: Out of memory", true);

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Equalizing Histogram ...");
        

        try {

            if (!isColorImage) {

                for (i = 0; (i < nSlices) && !threadStopped; i++) { // process for all slices

                  
                	fireProgressStateChanged((int) ((float) (i) / nSlices * 100));
                	fireProgressStateChanged("Processing slice " + Integer.toString(i + 1));
   	                         

                    srcImage.exportData(length * i, length, buffer); // locks and releases lock
                    monoSliceFilter(buffer, resultBuffer);

                    if (threadStopped) { // do before copying back into image
                        finalize();

                        return;
                    }

                    destImage.importData(length * i, resultBuffer, false);
                }
            }
            else if (useCIELab) {
            	for (i = 0; (i < nSlices) && !threadStopped; i++) { // process for all slices

                    
                	fireProgressStateChanged((int) ((float) (i) / nSlices * 100));
                	fireProgressStateChanged("Processing slice " + Integer.toString(i + 1));
   	                         

                    srcImage.exportData(4 * length * i, 4 * length, buffer); // locks and releases lock
                    convertRGBtoCIELab(buffer, L, a, b);
                    monoSliceFilter(L, Leq);
                    convertCIELabtoRGB(Leq, a, b, resultBuffer);

                    if (threadStopped) { // do before copying back into image
                        finalize();

                        return;
                    }

                    destImage.importData(4 * length * i, resultBuffer, false);
                }	
            } else { // if (isColorImage) {

                for (color = 1; (color < valuesPerPixel) && !threadStopped; color++) {

                    if (((color == 1) && rChannel) || ((color == 2) && gChannel) // process only desired channels
                            || ((color == 3) && bChannel)) {
                        colorUsed++;
                    }

                    for (i = 0; (i < nSlices) && !threadStopped; i++) { // for all slices at once

                      
                    	fireProgressStateChanged((int) (((float) ((Math.max(0, colorUsed - 1) * nSlices) + i) /
                                                                    (numColors * nSlices)) * 100));

                    	if ((color == 1) && rChannel) {
                    		fireProgressStateChanged("Processing red slice " + Integer.toString(i + 1));
                    	} else if ((color == 2) && gChannel) {
                    		fireProgressStateChanged("Processing green slice " + Integer.toString(i + 1));
                    	} else if ((color == 3) && bChannel) {
                    		fireProgressStateChanged("Processing blue slice " + Integer.toString(i + 1));
                    	}
                  
                        srcImage.exportRGBData(color, 4 * length * i, length, buffer);

                        if (((color == 1) && rChannel) || ((color == 2) && gChannel) // process only desired channels
                                || ((color == 3) && bChannel)) { // -- and not alpha channel at all --
                            monoSliceFilter(buffer, resultBuffer);
                        } else { // or don't process this color and just copy it as found
                            copyBuffers(resultBuffer, buffer);
                        }

                        if (threadStopped) { // do before copying back into image
                            finalize();

                            return;
                        }

                        // but always then store the channel back into the destination image
                        destImage.importRGBData(color, 4 * length * i, resultBuffer, false);
                    }
                }
            }

            destImage.calcMinMax(); // then calculate the min/max for all colors in the image
        }
        // clean up errors....
        catch (IOException error) {
            errorCleanUp("Algorithm AHE: Source image locked", false);

            return;
        } catch (OutOfMemoryError e) {
            buffer = null;
            resultBuffer = null;
            errorCleanUp("Algorithm AHE: Out of memory", true);
            setCompleted(false);

            return;
        }

        if (threadStopped) {
            finalize();

            return;
        }

        // or the processing completed okay, so dispose of the user notification
        
        setCompleted(true);
    }

    /**
     * after generating a histogram, <code>clip</code> will reduce the number of pixels attributed to that brightness
     * level.
     *
     * <p>The idea is to limit any brightness level in the histogram to a set value; the number of pixels that are above
     * the limit are tallied. The total number of pixels that were found above the limit, is then spread evenly among
     * the brightnesses that are below limit.</p>
     *
     * @param  binNumber  the number of brightness levels available in the histogram
     * @param  histogram  array where the brightness values of all pixels in the image have been counted.
     * @param  clipLimit  no brightness may have any more than this number of pixels. Obviously, this method will not
     *                    achieve anything when <code>clipLimit</code> is as large or larger than the largest brightness
     *                    in <code>histogram</code>.
     */
    private void clip(int binNumber, int[] histogram, int clipLimit) {
        int excess = 0;
        int totalExcess = 0;
        int totalCapacity = 0;
        int minChannel = 0;
        int maxChannel = 0;
        int minHist;
        int maxHist;
        int i, j;

        // tally the excess in all bins
        for (i = 0; i < binNumber; i++) {
            excess = histogram[i] - clipLimit;

            if (excess > 0) { // if there were too many of a particular brightness
                totalExcess += excess; // then total-up the excess
            } else if (excess < 0) {
                totalCapacity -= excess;
            }
        }

        if (totalCapacity < totalExcess) {
            totalExcess = totalCapacity;
        }

        for (i = 0; i < totalExcess; i++) {
            maxHist = -Integer.MAX_VALUE;
            minHist = Integer.MAX_VALUE;

            for (j = 0; j < binNumber; j++) {

                if (histogram[j] > maxHist) {
                    maxHist = histogram[j];
                    maxChannel = j;
                }

                if (histogram[j] < minHist) {
                    minHist = histogram[j];
                    minChannel = j;
                }
            }

            histogram[maxChannel] -= 1;
            histogram[minChannel] += 1;
        }
    }

    /**
     * Copies all values from the srcbuffer to the destination buffer. the format is intended to envision the
     * organisation of dest[i] := src[i] float dest destination array float src source array
     *
     * @param  dest  DOCUMENT ME!
     * @param  src   DOCUMENT ME!
     */
    private void copyBuffers(float[] dest, float[] src) {
        int i;

        if (dest.length < src.length) {
            return; // cannot copy!!
        }

        for (i = 0; i < src.length; i++) {
            dest[i] = src[i]; // copy buffer into resultBuffer
        }
    }


    /**
     * Allows a single monochrome image slice to be filtered. Any color image may be processed in this so long as each
     * color plane is separate and provided one at a time. This means, extract <tt>[aRGB aRGB ...]</tt> buffer into 3
     * seperate buffers <tt>[RRRRRRRRRRR...] , [GGGGGG....] and [BBBBBBBB]</tt> and feed each into the sliceFilter
     * one-at-a-time.
     *
     * <p>The benefit is one sliceFilter for all image types; just break up each RGB image into these separate
     * monochrome images sliceFilter and then reassemble int aRGB.</p>
     *
     * <p>Only length of one slice (image.getExtents[0] * image.getExtents[1]) Will be copied at a time. Note that a
     * progressBar is not changed in this method.</p>
     *
     * @param  srcBuffer   source buffer
     * @param  destBuffer  destination Buffer
     */
    private void monoSliceFilter(float[] srcBuffer, float[] destBuffer) {

        int sliceLength = srcImage.getSliceSize();
        int width = srcImage.getExtents()[0]; // width of slice in number of pixels (
        int height = srcImage.getExtents()[1]; // height of slice in number of pixels

        int bin, // counter-var
            totalBins; // number of brightness-values in img.  also, sizeof(histogram)
        int brightnessLevel; // a pixel (no matter whether represented as a float or int), has a brightnessLevel,
                             // which is in the histogram as a value between 0 and totalBins-1.
        int[] hist; // (cumulative) histogram
        float unitSize; // one unit is one totalBin-th of the distance between max & min brightness.
        float[][][] mapping; // scale of histogram value per sector [totalBins][X divisions][Y divisions]
        float scale; // scale value of
                     // (max # of colors (which is 255)/(1 + largest # of pixels in sector (which is hist[totalBins-1]))
        float maxScale;

        int clipLimit = 1; // calculated to find the highest value of the pixels permitted when clipping
        float idealWidth;
        int idealBins;

        // these bounds "frame" the interior of the slice which may be filtered (&adjusted);
        // image outside the frame may not
        int yStart, yEnd, // bounds on the row
            xStart, xEnd; // bounds on the column
        int x, y, r, c;
        int w1, w2, w3, w4, w5;
        int i;

        // find the ideal bin width
        for (i = 0; i < sliceLength; i++) {
            sortBuffer[i] = srcBuffer[i];
        }

        Arrays.sort(sortBuffer);
        idealWidth = (float) (2.0f * (sortBuffer[(3 * sliceLength / 4) - 1] - sortBuffer[(sliceLength / 4) - 1]) *
                                  Math.pow(sliceLength, -1.0 / 3.0));

        bufMax = sortBuffer[sliceLength - 1];
        bufMin = sortBuffer[0];

        // findBufferMinMax(srcBuffer, 0, sliceLength); // calculates largest and smallest pixel values in a sector
        // image minimum; used as offset for images to build Histo
        if (idealWidth >= ((bufMax - bufMin)/1024.0)) {
            idealBins = (int) (((bufMax - bufMin) / idealWidth) + 0.5f);
        }
        else {
        	idealBins = 256;
        }

        Preferences.debug("idealBins = " + idealBins + "\n", Preferences.DEBUG_ALGORITHM);

        int type = srcImage.getType();
        totalBins = findTotalBins(type, idealBins);
        findImageOffset(type);

        unitSize = ((bufMax - imageOffset) / (totalBins - 1)); // one unit is the distance of one totalBin-th in
                                                               // brightness

        mapping = new float[totalBins][hDivisions][wDivisions];
        hist = new int[totalBins]; // hist is both normal histogram & cumulative histogram

        float pXsize = width / (float) wDivisions; // partition size in the X dir
        float pYsize = height / (float) hDivisions; // partition size in the Y dir

        for (r = 0; (r < hDivisions) && !threadStopped; r++) {
            yStart = (int) (r * pYsize);
            yEnd = (int) ((r + 1) * pYsize);

            if (yEnd >= height) {
                yEnd = height;
            }

            for (c = 0; (c < wDivisions) && !threadStopped; c++) {
                maxScale = -Float.MAX_VALUE;

                // calculate bounding rectangle for sector
                xStart = (int) (c * pXsize);
                xEnd = (int) ((c + 1) * pXsize);

                if (xEnd >= width) {
                    xEnd = width;
                }

                // calculate the cumulative histogram of this sector
                // clear histogram
                for (bin = 0; bin < totalBins; bin++) {
                    hist[bin] = 0;
                }

                // for each pixel in the sector of the original image,
                // find the appropriate brightness and account for it
                // in the histogram.
                for (y = yStart; y < yEnd; y++) {

                    for (x = xStart; x < xEnd; x++) {

                        try {
                            brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / (unitSize));
                            hist[brightnessLevel]++;

                            // find max brightness in the sector
                            if (srcBuffer[x + (y * width)] > maxScale) {
                                maxScale = srcBuffer[x + (y * width)];
                            }
                        } catch (ArrayIndexOutOfBoundsException aioobe) {
                            Preferences.debug("<HALT>\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("x:(x: " + x + ", y: " + y + ")-- at location: " + x + (y * width) +
                                              "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("srcbuffer: " + srcBuffer[x + (y * width)] + " bufMin: " + bufMin +
                                              " unitSize: " + unitSize + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("at hist: " + ((int) (srcBuffer[x + (y * width)] - bufMin) / unitSize) +
                                              "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("totalbins: " + totalBins + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("</HALT>\n", Preferences.DEBUG_ALGORITHM);
                            MipavUtil.displayError("Algorithm AHE reports: out of bounds");
                            setCompleted(false);

                            return;
                        }
                    }
                }

                if ((!isColorImage) || useCIELab){
                    maxScale = maxScale - bufMin;
                }

                // if user requested clamping, then redistribute the histo
                if (clamped) {

                    // find the pixel intensity with the largest count
                    int maxCount = -1;

                    for (bin = 0; bin < totalBins; bin++) {

                        if (hist[bin] > maxCount) {
                            maxCount = hist[bin]; // store the count
                        }
                    }

                    clipLimit = (int) (clipLevel * maxCount);
                    clip(totalBins, hist, clipLimit);
                }

                // make histogram cumulative
                for (bin = 1; bin < totalBins; bin++) {
                    hist[bin] += hist[bin - 1];
                }

                // scale while adding to mapping
                scale = (float) (maxScale) / (hist[totalBins - 1]); // largest scalevalue over the

                for (bin = 0; bin < totalBins; bin++) {
                    mapping[bin][r][c] = hist[bin] * scale;
                }
            }
        }

        // we have calculated the entire mapping, now we must place
        // mapping values into the internal portions of the image
        for (r = 0; (r < (hDivisions - 1)) && !threadStopped; r++) {
            yStart = (int) ((r * pYsize) + (pYsize / 2));
            yEnd = (int) (((r + 1) * pYsize) + (pYsize / 2));

            if (yEnd >= height) {
                yEnd = height;
            }

            for (c = 0; (c < (wDivisions - 1)) && !threadStopped; c++) {

                // calculate bounding recangle for sector
                xStart = (int) ((c * pXsize) + (pXsize / 2));
                xEnd = (int) (((c + 1) * pXsize) + (pXsize / 2));

                if (xEnd >= width) {
                    xEnd = width;
                }

                // interpolate histogram equalization
                for (y = yStart; y < yEnd; y++) {

                    for (x = xStart; x < xEnd; x++) {
                        brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / unitSize);

                        // find corner points
                        w1 = (xEnd - 1 - x) * (yEnd - 1 - y); // too little offset by color
                        w2 = (x - xStart) * (yEnd - 1 - y); // too much offset by color
                        w3 = (xEnd - 1 - x) * (y - yStart); // too little offset by color
                        w4 = (x - xStart) * (y - yStart); // too much offset by color
                        w5 = w1 + w2 + w3 + w4;

                        // from mapping into image data
                        try {
                            destBuffer[x + (y * width)] = ( // scales the destimage brightness by shifted version
                            (w1 * mapping[brightnessLevel][r][c]) + (w2 * mapping[brightnessLevel][r][c + 1]) +
                                                          (w3 * mapping[brightnessLevel][r + 1][c]) +
                                                          (w4 * mapping[brightnessLevel][r + 1][c + 1])) / w5;
                        } catch (ArrayIndexOutOfBoundsException aioobe) {
                            Preferences.debug("<HALT>\nslice Width = " + width + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("src buffer size = " + srcBuffer.length + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("dst buffer size = " + destBuffer.length + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("y: " + y + ", x: " + x + ", r: " + r + ", c: " + c +
                                              ", location in buffer: " + Integer.toString(x + (y * width)) + ".\n", 
                                              Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("Y: " + srcImage.getExtents()[1] + ", X: " +
                                              Integer.toString(srcImage.getExtents()[0]) + "\n", Preferences.DEBUG_ALGORITHM);
                            Preferences.debug("</HALT>\n", Preferences.DEBUG_ALGORITHM);
                            MipavUtil.displayError("Algorithm AHE reports: out of bounds in monoSliceFilter");
                            setCompleted(false);

                            return;
                        }
                    }
                }
            }
        }

        // four corner partitions of image
        // calculate bounding rectangle for sector
        xStart = (int) (pXsize / 2); // account for color elsewhere
        xEnd = (int) (((wDivisions - 1) * pXsize) + (pXsize / 2)); // account for color elsewhere

        if (xEnd >= width) {
            xEnd = width; // account for color elsewhere
        }

        yStart = (int) (pYsize / 2);
        yEnd = (int) (((hDivisions - 1) * pYsize) + (pYsize / 2));

        if (yEnd >= height) {
            yEnd = height;
        }

        for (y = 0; y < yStart; y++) { // upper-left hand corner

            for (x = 0; x < xStart; x++) {
                brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / (unitSize));

                try { // if (brightnessLevel < totalBins) {
                    destBuffer[x + (y * width)] = mapping[brightnessLevel][0][0];
                } catch (ArrayIndexOutOfBoundsException exc) { }
            }
        }

        for (y = yEnd; y < height; y++) { // lower-left hand corner

            for (x = 0; x < xStart; x++) {
                brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / (unitSize));

                try { // if (brightnessLevel < totalBins)
                    destBuffer[x + (y * width)] = mapping[brightnessLevel][hDivisions - 1][0];
                } catch (ArrayIndexOutOfBoundsException exc) { }
            }
        }

        for (y = 0; y < yStart; y++) { // upper-right hand corner

            for (x = xEnd; x < width; x++) {
                brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / (unitSize));

                try { // if (brightnessLevel < totalBins)
                    destBuffer[x + (y * width)] = mapping[brightnessLevel][0][wDivisions - 1];
                } catch (ArrayIndexOutOfBoundsException exc) { }
            }
        }

        for (y = yEnd; y < height; y++) { // lower-right hand corner

            for (x = xEnd; x < width; x++) {
                brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / (unitSize));

                try { // if (brightnessLevel < totalBins)
                    destBuffer[x + (y * width)] = mapping[brightnessLevel][hDivisions - 1][wDivisions - 1];
                } catch (ArrayIndexOutOfBoundsException exc) { }
            }
        }

        // get the top & bottom of the image
        for (c = 0; c < (wDivisions - 1); c++) {

            // calculate bounding rectangle for sector
            xStart = (int) ((c * pXsize) + (pXsize / 2));
            xEnd = (int) (((c + 1) * pXsize) + (pXsize / 2));

            if (xEnd > width) {
                xEnd = width;
            }

            for (x = xStart; x < xEnd; x++) {
                w1 = xEnd - 1 - x;
                w2 = x - xStart;
                w3 = w1 + w2;

                // top
                yStart = 0;
                yEnd = (int) (pYsize / 2);

                for (y = yStart; y < yEnd; y++) {
                    brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / (unitSize));

                    try { // if (brightnessLevel < totalBins)
                        destBuffer[x + (y * width)] = ((w1 * mapping[brightnessLevel][0][c]) +
                                                       (w2 * mapping[brightnessLevel][0][c + 1])) / w3;
                    } catch (ArrayIndexOutOfBoundsException exc) { }
                }

                // bottom
                yStart = (int) (((hDivisions - 1) * pYsize) + (pYsize / 2));
                yEnd = height;

                for (y = yStart; y < yEnd; y++) {
                    brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / (unitSize));

                    try { // if (brightnessLevel < totalBins)
                        destBuffer[x + (y * width)] = ((w1 * mapping[brightnessLevel][hDivisions - 1][c]) +
                                                       (w2 * mapping[brightnessLevel][hDivisions - 1][c + 1])) / w3;
                    } catch (ArrayIndexOutOfBoundsException exc) { }
                }
            }
        }

        // do left & right of the image
        for (r = 0; r < (hDivisions - 1); r++) {

            // calculate bounding rectangle for sector
            yStart = (int) ((r * pYsize) + (pYsize / 2));
            yEnd = (int) (((r + 1) * pYsize) + (pYsize / 2));

            for (y = yStart; y < yEnd; y++) {
                w1 = yEnd - 1 - y;
                w2 = y - yStart;
                w3 = w1 + w2;

                // left
                xStart = 0;
                xEnd = (int) (pXsize / 2);

                for (x = xStart; x < xEnd; x++) {
                    brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / (unitSize));

                    try { // if (brightnessLevel < totalBins)
                        destBuffer[x + (y * width)] = ((w1 * mapping[brightnessLevel][r][0]) +
                                                       (w2 * mapping[brightnessLevel][r + 1][0])) / w3;
                    } catch (ArrayIndexOutOfBoundsException exc) { }
                }

                // right
                xStart = (int) (((wDivisions - 1) * pXsize) + (pXsize / 2));
                xEnd = width;

                for (x = xStart; x < xEnd; x++) {
                    brightnessLevel = (int) ((srcBuffer[x + (y * width)] - imageOffset) / (unitSize));

                    try { // if (brightnessLevel < totalBins)
                        destBuffer[x + (y * width)] = ((w1 * mapping[brightnessLevel][r][wDivisions - 1]) +
                                                       (w2 * mapping[brightnessLevel][r + 1][wDivisions - 1])) / w3;
                    } catch (ArrayIndexOutOfBoundsException exc) { }
                }
            }
        }

        // restores the offset back to the buffer minimum
        if ((!isColorImage) || useCIELab) {

            for (x = 0; x < destBuffer.length; x++) {
                destBuffer[x] += bufMin;
            }
        }

    }

    /**
     * a debug tool.
     *
     * @param  histo  DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private void printhisto(int[] histo) {

        for (int i = 0; i < histo.length; i++) {
            Preferences.debug("hist[" + i + "] = " + histo[i] + "\n", Preferences.DEBUG_ALGORITHM);
        }
    }
    
    private void convertRGBtoCIELab(float buffer[], float L[], float a[], float b[]) {
        // Observer = 2 degrees, Illuminant = D65
        double XN = 95.047;
        double YN = 100.000;
        double ZN = 108.883;
        int i;
        int j;
        double varR, varG, varB;
        double X, Y, Z;
        double varX, varY, varZ;
        
        for (i = 0, j = 0; i < buffer.length; i += 4) {
            varR = buffer[i+1]/scaleMax;
            varG = buffer[i+2]/scaleMax;
            varB = buffer[i+3]/scaleMax;
            
            if (varR <= 0.04045) {
                varR = varR/12.92;
            }
            else {
                varR = Math.pow((varR + 0.055)/1.055, 2.4);
            }
            if (varG <= 0.04045) {
                varG = varG/12.92;
            }
            else {
                varG = Math.pow((varG + 0.055)/1.055, 2.4);
            }
            if (varB <= 0.04045) {
                varB = varB/12.92;
            }
            else {
                varB = Math.pow((varB + 0.055)/1.055, 2.4);
            }
            
            varR = 100.0 * varR;
            varG = 100.0 * varG;
            varB = 100.0 * varB;
            
            // Observer = 2 degrees, Illuminant = D65
            X = 0.4124*varR + 0.3576*varG + 0.1805*varB;
            Y = 0.2126*varR + 0.7152*varG + 0.0722*varB;
            Z = 0.0193*varR + 0.1192*varG + 0.9505*varB;
            
            varX = X/ XN;
            varY = Y/ YN;
            varZ = Z/ ZN;
            
            if (varX > 0.008856) {
                varX = Math.pow(varX, 1.0/3.0);
            }
            else {
                varX = (7.787 * varX) + (16.0/116.0);
            }
            if (varY > 0.008856) {
                varY = Math.pow(varY, 1.0/3.0);
            }
            else {
                varY = (7.787 * varY) + (16.0/116.0);
            }
            if (varZ > 0.008856) {
                varZ = Math.pow(varZ, 1.0/3.0);
            }
            else {
                varZ = (7.787 * varZ) + (16.0/116.0);
            }
            
            L[j] = (float)((116.0 * varY) - 16.0);
            a[j] = (float)(500.0 * (varX - varY));
            b[j++] = (float)(200.0 * (varY - varZ));
            
        } // for (i = 0; i < buffer.length; i += 4)
                
        
    } // private void convertRGBtoCIELab()
    
    private void convertCIELabtoRGB(float L[], float a[], float b[], float buffer[]) {
        // Observer = 2 degrees, Illuminant = D65
        double XN = 95.047;
        double YN = 100.000;
        double ZN = 108.883;
        int i;
        int j;
        double varX, varY, varZ;
        double varX3, varY3, varZ3;
        double X, Y, Z;
        double varR, varG, varB;
        double R, G, B;
        for (i = 0, j = 0; i < buffer.length; i += 4) {   
            
            varY = (L[j] + 16.0)/116.0;
            varX = a[j]/500.0 + varY;
            varZ = varY - b[j]/200.0;
            j++;
            
            varX3 = Math.pow(varX, 3.0);
            if (varX3 > 0.008856) {
                varX = varX3;
            }
            else {
                varX = (varX - 16.0/116.0)/7.787;
            }
            varY3 = Math.pow(varY, 3.0);
            if (varY3 > 0.008856) {
                varY = varY3;
            }
            else {
                varY = (varY - 16.0/116.0)/7.787;
            }
            varZ3 = Math.pow(varZ, 3.0);
            if (varZ3 > 0.008856) {
                varZ = varZ3;
            }
            else {
                varZ = (varZ - 16.0/116.0)/7.787;
            }
            
            X = XN * varX;
            Y = YN * varY;
            Z = ZN * varZ;
            
            varX = X / 100.0;
            varY = Y / 100.0;
            varZ = Z / 100.0;
            
            varR = 3.2406 * varX - 1.5372 * varY - 0.4986 * varZ;
            varG = -0.9689 * varX + 1.8758 * varY + 0.0415 * varZ;
            varB = 0.0557 * varX - 0.2040 * varY + 1.0570 * varZ;
            
            if (varR > 0.0031308) {
                varR = 1.055 * (Math.pow(varR, 1.0/2.4)) - 0.055;
            }
            else {
                varR = 12.92 * varR;
            }
            if (varG > 0.0031308) {
                varG = 1.055 * (Math.pow(varG, 1.0/2.4)) - 0.055;
            }
            else {
                varG = 12.92 * varG;
            }
            if (varB > 0.0031308) {
                varB = 1.055 * (Math.pow(varB, 1.0/2.4)) - 0.055;
            }
            else {
                varB = 12.92 * varB;
            }
            
            R = scaleMax * varR;
            if (R < 0) {
            	R = 0;
            }
            if (R > scaleMax) {
            	R = scaleMax;
            }
            G = scaleMax * varG;
            if (G < 0) {
            	G = 0;
            }
            if (G > scaleMax) {
            	G = scaleMax;
            }
            B = scaleMax * varB;
            if (B < 0) {
            	B = 0;
            }
            if (B > scaleMax) {
            	B = scaleMax;
            }
            
            buffer[i+1] = (float)R;
            buffer[i+2] = (float)G;
            buffer[i+3] = (float)B;
        } // for (i = 0; i < buffer.length; i += 4)
    } // private void convertCIELabtoRGB(float buffer[])

   
}
