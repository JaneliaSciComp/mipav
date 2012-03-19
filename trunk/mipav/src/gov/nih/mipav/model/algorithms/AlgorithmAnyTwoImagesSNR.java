package gov.nih.mipav.model.algorithms;


import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.io.IOException;
import java.text.DecimalFormat;


/**
 
 * </p>
 */
public class AlgorithmAnyTwoImagesSNR extends AlgorithmBase {

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int cost;

    /** DOCUMENT ME! */
    private boolean createRegImage;

    /** private ModelImage srcImage;. */
    private ModelImage image2;

    /** DOCUMENT ME! */
    private boolean register;
    
    private boolean scale;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmAnyTwoImagesSNR object.
     * 
     * @param image First MRI image
     * @param image2 Second MRI image
     * @param scale if true scale image 2 so its min and max match min and max of image 1
     * @param register If true register the image2 to the image before SNR
     * @param cost Cost function used in registration
     * @param createRegImage If register = true and createRegImage = true, then create a frame with the registered image
     */
    public AlgorithmAnyTwoImagesSNR(ModelImage image, ModelImage image2, boolean scale,
            boolean register, int cost, boolean createRegImage) {

        super(null, image);
        this.image2 = image2;
        this.scale = scale;
        this.register = register;
        this.cost = cost;
        this.createRegImage = createRegImage;
        UI = ViewUserInterface.getReference();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        srcImage = null;
        super.finalize();
    }

    /**
     * starts the algorithm.
     */
    public void runAlgorithm() {
        int i;
        int xDim, yDim, sliceSize;
        int zDim;
        int tDim;
        int imageLength;
        double[] doubleBuffer;
        double[] doubleBuffer2;
        ModelImage regImage = null;

        AlgorithmRegOAR2D regAlgo = null;
        AlgorithmRegOAR3D regAlgo3D = null;
        AlgorithmTransform transform = null;

        int[] srcExtents = new int[2];

        double difference;
        double snr;
        DecimalFormat nf;
        int cFactor = 1;
        double min1;
        double max1;
        double min2;
        double max2;
        double a;
        double b;
        double numerator;
        double denominator;
        double snrdB;

        if (srcImage == null) {
            displayError("image is null");

            return;
        }

        if (image2 == null) {
            displayError("image2 is null");

            return;
        }

        fireProgressStateChanged(srcImage.getImageName(), "Performing Any Two Images SNR ...");

        xDim = srcImage.getExtents()[0];
        yDim = srcImage.getExtents()[1];
        srcExtents[0] = xDim;
        srcExtents[1] = yDim;
        sliceSize = xDim * yDim;

        zDim = 1;

        if (srcImage.getNDims() > 2) {
            zDim = srcImage.getExtents()[2];
        }
        
        tDim = 1;
        if (srcImage.getNDims() > 3) {
        	tDim = srcImage.getExtents()[3];
        }
        
        if (srcImage.isColorImage()) {
        	cFactor = 4;
        }

        imageLength = cFactor * sliceSize * zDim * tDim;
        doubleBuffer = new double[imageLength];
        doubleBuffer2 = new double[imageLength];

        nf = new DecimalFormat("0.00E0");

        if (register) {
            fireProgressStateChanged("Registering images");

            int DOF; // rigid transformation
            int interp = AlgorithmTransform.BILINEAR;
            int interp2 = AlgorithmTransform.BILINEAR;
            float rotateBegin = -3.0f;
            float rotateEnd = 3.0f;
            float coarseRate = 3.0f;
            float fineRate = 1.0f;
            float rotateBeginX = -3.0f;
            float rotateEndX = 3.0f;
            float coarseRateX = 3.0f;
            float fineRateX = 1.0f;
            float rotateBeginY = -3.0f;
            float rotateEndY = 3.0f;
            float coarseRateY = 3.0f;
            float fineRateY = 1.0f;
            float rotateBeginZ = -3.0f;
            float rotateEndZ = 3.0f;
            float coarseRateZ = 3.0f;
            float fineRateZ = 1.0f;
            boolean maxResol = true;
            boolean doSubsample;
            boolean doMultiThread;
            boolean fastMode = false;
            int maxIterations = 2;
            int numMinima = 3;
            boolean transformVOI = false;

            if (srcImage.getNDims() == 2) {
                DOF = 3;
                doSubsample = false;
                doMultiThread = true;
                interp = AlgorithmTransform.BILINEAR;
                interp2 = AlgorithmTransform.BILINEAR;
                regAlgo = new AlgorithmRegOAR2D(srcImage, image2, cost, DOF, interp, rotateBegin, rotateEnd,
                        coarseRate, fineRate, doSubsample, doMultiThread, maxIterations, numMinima);
                regAlgo.run();
                transform = new AlgorithmTransform(image2, regAlgo.getTransform(), interp2, srcImage.getFileInfo()[0]
                        .getResolutions()[0], srcImage.getFileInfo()[0].getResolutions()[1], srcImage.getExtents()[0],
                        srcImage.getExtents()[1], new int[] {srcImage.getUnitsOfMeasure(0),
                                srcImage.getUnitsOfMeasure(1)}, transformVOI, false, false);
                regAlgo.finalize();
                regAlgo = null;
            } // if (srcImage.getNDims() == 2)
            else if (srcImage.getNDims() == 3) {
                DOF = 6;
                doSubsample = true;
                doMultiThread = true;
                interp = AlgorithmTransform.TRILINEAR;
                interp2 = AlgorithmTransform.TRILINEAR;
                regAlgo3D = new AlgorithmRegOAR3D(srcImage, image2, cost, DOF, interp, rotateBeginX, rotateEndX,
                        coarseRateX, fineRateX, rotateBeginY, rotateEndY, coarseRateY, fineRateY, rotateBeginZ,
                        rotateEndZ, coarseRateZ, fineRateZ, maxResol, doSubsample, doMultiThread, fastMode,
                        maxIterations, numMinima);
                regAlgo3D.run();
                transform = new AlgorithmTransform(image2, regAlgo3D.getTransform(), interp2, srcImage.getFileInfo()[0]
                        .getResolutions()[0], srcImage.getFileInfo()[0].getResolutions()[1], srcImage.getFileInfo()[0]
                        .getResolutions()[2], srcImage.getExtents()[0], srcImage.getExtents()[1],
                        srcImage.getExtents()[2], transformVOI, false, false);
                regAlgo3D.finalize();
                regAlgo3D = null;
            } // else if (srcImagre.getNDims() == 3)

            transform.setUpdateOriginFlag(true);
            transform.run();
            regImage = transform.getTransformedImage();
            transform.finalize();

            if (createRegImage) {

                regImage.setImageName(image2.getImageName() + "_registered");
                regImage.calcMinMax();

                new ViewJFrameImage(regImage);
            } // if (createRegImage)
        } // if (register)

        try {
            srcImage.exportData(0, imageLength, doubleBuffer);
        } catch (IOException e) {
            MipavUtil.displayError("IOException " + e + " on srcImage.export(0, imageLength, doubleBuffer)");

            setCompleted(false);

            return;
        }

        if (register) {

            try {
                regImage.exportData(0, imageLength, doubleBuffer2);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on  regImage.export(0, imageLength, doubleBuffer2)");

                setCompleted(false);

                return;
            }
        } // if (register)
        else {

            try {
                image2.exportData(0, imageLength, doubleBuffer2);
            } catch (IOException e) {
                MipavUtil.displayError("IOException " + e + " on image2.export(0, imageLength, doubleBuffer2)");

                setCompleted(false);

                return;
            }
        } // else
        
        if (scale) {
        	if (srcImage.isColorImage()) {
                min1 = Math.min(srcImage.getMinR(), srcImage.getMinG());
                min1 = Math.min(min1, srcImage.getMinB());
        	}
        	else {
        		min1 = srcImage.getMin();
        	}
        	if (srcImage.isColorImage()) {
                max1 = Math.max(srcImage.getMaxR(), srcImage.getMaxG());
                max1 = Math.max(max1, srcImage.getMaxB());
        	}
        	else {
        		max1 = srcImage.getMax();
        	}
        	if (register) {
        		if (regImage.isColorImage()) {
                    min2 = Math.min(regImage.getMinR(), regImage.getMinG());
                    min2 = Math.min(min2, regImage.getMinB());
            	}
            	else {
            		min2 = regImage.getMin();
            	}
            	if (regImage.isColorImage()) {
                    max2 = Math.max(regImage.getMaxR(), regImage.getMaxG());
                    max2 = Math.max(max2, regImage.getMaxB());
            	}
            	else {
            		max2 = regImage.getMax();
            	}	
        	} // if (register)
        	else {
        		if (image2.isColorImage()) {
                    min2 = Math.min(image2.getMinR(), image2.getMinG());
                    min2 = Math.min(min2, image2.getMinB());
            	}
            	else {
            		min2 = image2.getMin();
            	}
            	if (image2.isColorImage()) {
                    max2 = Math.max(image2.getMaxR(), image2.getMaxG());
                    max2 = Math.max(max2, image2.getMaxB());
            	}
            	else {
            		max2 = image2.getMax();
            	}		
        	}
        	// a*min2 + b = min1
        	// a*max2 + b = max1
        	a = (max1 - min1)/(max2 - min2);
        	b = max1 - a*max2;
        	for (i = 0; i < imageLength; i++) {
        		doubleBuffer2[i] = a*doubleBuffer2[i] + b;
        	}
        } // if (scale)

        fireProgressStateChanged("Cacluating snr");
        
        numerator = 0.0;
        denominator = 0.0;
        if (srcImage.isColorImage()) {
            for (i = 0; i < imageLength; i+=4) {
            	numerator += doubleBuffer[i+1]*doubleBuffer[i+1];
 	            difference = doubleBuffer[i+1] - doubleBuffer2[i+1];
 	            denominator += difference*difference;	
 	            numerator += doubleBuffer[i+2]*doubleBuffer[i+2];
	            difference = doubleBuffer[i+2] - doubleBuffer2[i+2];
	            denominator += difference*difference;	
	            numerator += doubleBuffer[i+3]*doubleBuffer[i+3];
 	            difference = doubleBuffer[i+3] - doubleBuffer2[i+3];
 	            denominator += difference*difference;	
            }
        }
        else {
	        for (i = 0; i < imageLength; i++) {
	            numerator += doubleBuffer[i]*doubleBuffer[i];
	            difference = doubleBuffer[i] - doubleBuffer2[i];
	            denominator += difference*difference;
	        } // for (i = 0; i < imageLength; i++)
        }
        snr = numerator/denominator;
        snrdB = 10.0 * Math.log10(snr);

        Preferences.debug("SNR of image2 relative to image1 = " + nf.format(snr) + "\n", 
        		Preferences.DEBUG_ALGORITHM);
        UI.setDataText("SNR of image2 relative to image1 = " + nf.format(snr) + "\n");
        Preferences.debug("SNR in decibels of image2 relative to image1 = " + nf.format(snrdB) + " dB\n", 
        		Preferences.DEBUG_ALGORITHM);
        UI.setDataText("SNR in decibels of image2 relative to image1 = " + nf.format(snrdB) + " dB\n");

        setCompleted(true);

        return;
    }
}
