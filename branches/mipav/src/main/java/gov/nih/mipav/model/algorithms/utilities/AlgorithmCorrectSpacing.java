package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import java.io.*;


/**
 * Algorithm to adjust image volume for cases when the slice spacing is not equal to the slice thickness. When spacing >
 * thickness: repeat images from original image set insert blank images (so that in the final image volume, all images
 * will have the same slice thickness and the image volume will be to proper scale. When spacing < thickness: set
 * thickness = spacing.
 */

public class AlgorithmCorrectSpacing extends AlgorithmBase {

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The number of dimensions in the image. */
    private int DIM;

    /** New Z dimension. */
    private int newZdim;

    /** Number of blank images, to make up gap. */
    private int numBlanks;

    /** Number of repeated images, to compensate for gap. */
    private int numRepIm;

    /** Volume for each time step. */
    private int oldTimeStepVolume, newTimeStepVolume;

    /** Original Z dimension of the image. */
    private int oldZdim;

    /** Result Image. */
    private ModelImage resultImage;

    /** Area of a slice (Xdim * Ydim). */
    private int sliceArea;

    /** Time dimension of the image. */
    private int Tdim;

    /** X dimension of the image. */
    private int Xdim;

    /** Y dimension of the image. */
    private int Ydim;
    
    private int colorFactor;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Import source image into the class.
     *
     * @param  srcImage     source image (image to clip from)
     * @param  resultImage  corrected image
     * @param  numRepIm     number of times to repeat each of original images
     * @param  numBlanks    number of blank images to insert
     */
    public AlgorithmCorrectSpacing(ModelImage srcImage, ModelImage resultImage, int numRepIm, int numBlanks) {
        super(resultImage, srcImage);
        this.resultImage = resultImage;
        this.numRepIm = numRepIm;
        this.numBlanks = numBlanks;
        
        colorFactor = 1;
        if (srcImage.isColorImage()) {
            colorFactor = 4;	
        }
        else if (srcImage.isComplexImage()) {
        	colorFactor = 2;
        }

        // set global variables for this class
        Xdim = resultImage.getExtents()[0];
        Ydim = resultImage.getExtents()[1];
        sliceArea = colorFactor * Xdim * Ydim;
        oldZdim = srcImage.getExtents()[2];
        newZdim = oldZdim * (numRepIm + numBlanks);
        DIM = srcImage.getNDims();

        if (DIM == 4) {
            Tdim = resultImage.getExtents()[3];
            oldTimeStepVolume = sliceArea * oldZdim;
            newTimeStepVolume = sliceArea * newZdim;
        } else {
            Tdim = 1;
            oldTimeStepVolume = 0;
            newTimeStepVolume = 0;
        }
        // System.out.println("In AlgorithmCorrectSpacing.  newZdim: " +newZdim);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Dispose of local variables that may be taking up lots of room.
     */
    public void disposeLocal() {
        srcImage = null;
        resultImage = null;
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        super.finalize();
    }

    /**
     * Returns corrected image.
     *
     * @return  resultImage
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Runs algorithm.
     */
    public void runAlgorithm() {
        int j, k;
        int t, z, Z; // z is slice-depth of srcImage; Z is slice-depth of destination
        float[] imageBuffer;
        

        try {
            imageBuffer = new float[sliceArea]; // assuming here 3D image set and grayscale
        } catch (OutOfMemoryError e) {
            imageBuffer = null;
            errorCleanUp("Algorithm Correct Spacing reports: Out of memory", true);

            return;
        }

        // build progressbars
        ViewJProgressBar timeBar, sliceBar;
        String title, message;
        int xPos = Toolkit.getDefaultToolkit().getScreenSize().width / 2;
        int yPos = 50;

        if (DIM == 4) {
            title = "Time Progress Bar for image " + srcImage.getImageName();
            message = "Percentage of time intervals...";
            timeBar = new ViewJProgressBar(title, message, 0, 100, true, this, this);
            timeBar.setSize(new Dimension(400, 130));
            timeBar.setLocation(xPos, yPos);
            timeBar.setVisible(true);
            yPos += 130;
        } else {
            timeBar = null;
        }

        title = "Slice Progress Bar for image " + srcImage.getImageName();
        message = "Percentage of slices processed...";
        sliceBar = new ViewJProgressBar(title, message, 0, 100, true, this, this);
        sliceBar.setSize(new Dimension(400, 130));
        sliceBar.setLocation(xPos, yPos);
        sliceBar.setVisible(true);

        // System.out.println(Thread.currentThread().getPriority());
        // main part of algorithm
        for (t = 0; (t < Tdim) && !threadStopped; t++) {

            if ((timeBar != null) && timeBar.isVisible()) {
                timeBar.updateValue(Math.round((float) (t + 1) / (float) Tdim * 100));
            }

            Z = 0; // Z is the slice number in the resultImage

            for (z = 0; (z < oldZdim) && !threadStopped; z++) { // z is the slice number in the srcImage

                if (sliceBar.isVisible()) {

                    if (DIM == 4) {
                        message = "Percentage of slices processed for time, " + (t + 1) + ". ";
                        sliceBar.setMessage(message);
                    }

                    sliceBar.updateValue(Math.round((float) (z + 1) / (float) oldZdim * 100));
                }

                try {
                    srcImage.exportData((t * oldTimeStepVolume) + (z * sliceArea), sliceArea, imageBuffer);

                    for (k = 0; k < numRepIm; k++, Z++) {
                        resultImage.importData((t * newTimeStepVolume) + (Z * sliceArea), imageBuffer, false);
                        resultImage.setFileInfo(srcImage.getFileInfo(z), Z);
                    }

                    for (j = 0; j < sliceArea; j++) {
                        imageBuffer[j] = 0.0f;
                    }

                    for (k = 0; k < numBlanks; k++, Z++) {
                        resultImage.importData((t * newTimeStepVolume) + (Z * sliceArea), imageBuffer, false);
                        resultImage.setFileInfo(srcImage.getFileInfo(z), Z);
                    }
                } catch (IOException error) {
                    System.out.println("z is " + z + ", Z is " + Z + ", and t is " + t + ".");
                    displayError("Algorithm Correct Spacing reports: Destination image already locked.");
                    setCompleted(false);

                    return;
                }
            }
        }

        if (threadStopped) {

            if (DIM == 4) {
                timeBar.dispose();
            }

            sliceBar.dispose();
            imageBuffer = null;
            setCompleted(false);
            finalize();

            return;
        }

        // Clean up and let the calling dialog know that algorithm did its job
        if (DIM == 4) {
            timeBar.dispose();
        }

        sliceBar.dispose();
        imageBuffer = null;
        setCompleted(true);
    }
   
}
