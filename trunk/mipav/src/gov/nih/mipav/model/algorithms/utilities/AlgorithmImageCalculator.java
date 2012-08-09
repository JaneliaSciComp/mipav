package gov.nih.mipav.model.algorithms.utilities;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.util.Stack;
import java.util.Vector;

import javax.swing.*;
import javax.swing.border.*;


/**
 * Algorithm that adds, subtracts, multiplies, or divides an image by by another image. In addition, two images can be
 * ANDed, ORed or XORed together. Also, more advanced operator expressions can be entered in a dialog text field.
 *
 * @version  1.0 Oct 1, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class AlgorithmImageCalculator extends AlgorithmBase implements ActionListener, FocusListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public static final int ADD = 0; // algorithm types

    /** DOCUMENT ME! */
    public static final int ADVANCED = 1;

    /** DOCUMENT ME! */
    public static final int AND = 2;

    /** DOCUMENT ME! */
    public static final int AVERAGE = 3;

    /** DOCUMENT ME! */
    public static final int DIFFERENCE = 4; // absolute differenece

    /** DOCUMENT ME! */
    public static final int DIVIDE = 5;

    /** DOCUMENT ME! */
    public static final int MAXIMUM = 6;
    
    public static final int MEAN_SQUARED_ERROR = 7;

    /** DOCUMENT ME! */
    public static final int MINIMUM = 8;

    /** DOCUMENT ME! */
    public static final int MULTIPLY = 9;

    /** DOCUMENT ME! */
    public static final int OR = 10;

    /** DOCUMENT ME! */
    public static final int SUBTRACT = 11;

    /** DOCUMENT ME! */
    public static final int XOR = 12;
    
    public static final int AVGERAGE_WITH_STDEV = 13;


    /** DOCUMENT ME! */
    public static final int CLIP = 0; // clamp result data to the bounds of the input image type

    /** DOCUMENT ME! */
    public static final int PROMOTE = 1; // promote image type so that the range of the result fits into
                                         // the new image type. ( ie. byte to short).


    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton aButton, bButton, backSpaceButton, ceButton, leftParButton, rightParButton;

    /** DOCUMENT ME! */
    private JDialog adOpDialog;

    /** DOCUMENT ME! */
    private String adOpString = null;

    /** DOCUMENT ME! */
    private double aVal;

    /** /**. */
    private double bestMin, bestMax;

    /** DOCUMENT ME! */
    private double bVal;

    /** DOCUMENT ME! */
    private boolean cancel = false;

    /** Minimum and maximum clipping modes. */
    private double clipMin, clipMax;

    /**
     * Clipping mode.
     *
     * <pre>
                          CLIP          = 0;   clamp result data to the bounds of the input image type
                          PROMOTE       = 1;   promote image type so that the range of the result fits into
                                               the new image type. ( ie. byte to short).
     *                   </pre>
     */
    private int clipMode = CLIP;

    /** DOCUMENT ME! */
    private int colorFactor = 1; // 1 for grayscale, 4 for color

    /**
     * Flag, if true, indicates that the whole image should be processed. If false, process the image over the mask
     * areas.
     */
    private boolean entireImage;

    /** DOCUMENT ME! */
    private JButton expButton, lnButton, sevenButton, eightButton, nineButton, divButton;

    /** DOCUMENT ME! */
    private JButton logButton, absButton;

    /** /**. */
    private double minA, maxA, minB, maxB;

    /** DOCUMENT ME! */
    private double minAB, maxAB, minBB, maxBB;

    /** DOCUMENT ME! */
    private double minAG, maxAG, minBG, maxBG;

    /** DOCUMENT ME! */
    private double minAR, maxAR, minBR, maxBR;

    /** DOCUMENT ME! */
    private boolean OK;

    /** DOCUMENT ME! */
    private JButton OKButton, cancelButton;

    /** Operation to be performed on the images (i.e. Add, ...) */
    private int opType;

    /** DOCUMENT ME! */
    private JButton powButton, commaButton, fourButton, fiveButton, sixButton, multButton;

    /** DOCUMENT ME! */
    private boolean pressedOK = false;

    /** DOCUMENT ME! */
    private JButton sinButton, cosButton, oneButton, twoButton, threeButton, subButton;

    /** Source image A. */
    private ModelImage srcImageA;

    /** Source image B. */
    private ModelImage srcImageB;

    /** DOCUMENT ME! */
    private JButton tanButton, piButton, zeroButton, decimalButton, modButton, addButton;

    /** DOCUMENT ME! */
    private JTextField textOperator;
    
    /** when performing bulk operations via Image Calculator (Bulk Images) dialog, this array is populated */
    private ModelImage[] srcImages;
    
    private String rpn;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new AlgorithmImageCalculator object using srcImgA <operator> srcImgB.
     *
     * @param  srcImgA     source image A model
     * @param  srcImgB     source image B model
     * @param  type        operation type
     * @param  _clipMode   CLIP or PROMOTE input image data
     * @param  maskFlag    Flag that indicates that the operator will be calculated for the whole image if equal to true
     * @param  adOpString  text for advanced operations
     */
    public AlgorithmImageCalculator(ModelImage srcImgA, ModelImage srcImgB, int type, int _clipMode, boolean maskFlag,
                                    String adOpString) {
        this(null, srcImgA, srcImgB, type, _clipMode, maskFlag, adOpString);
    }

    /**
     * Creates a new AlgorithmImageCalculator object.
     *
     * @param  destImg     image model where result image is to stored
     * @param  srcImgA     source image A model
     * @param  srcImgB     source image B model
     * @param  type        operation type
     * @param  _clipMode   CLIP or PROMOTE input image data
     * @param  maskFlag    Flag that indicates that the operator will be calculated for the whole image if equal to true
     * @param  adOpString  text for advanced operations
     */
    public AlgorithmImageCalculator(ModelImage destImg, ModelImage srcImgA, ModelImage srcImgB, int type, int _clipMode,
                                    boolean maskFlag, String adOpString) {
        super(destImg, srcImgA); // this algorithm will associate srcImgA with srcImage for output.
        srcImageA = srcImgA;
        srcImageB = srcImgB;
        entireImage = maskFlag;
        opType = type;
        clipMode = _clipMode;
        this.adOpString = adOpString;
        setClipValues();

        if (entireImage == false) {
            mask = srcImageA.generateVOIMask();
        }

        if (srcImageA.isColorImage()) {
            colorFactor = 4;
        }
    }
    
    
    /**
     * constructor used for bulk images
     * @param destImage
     * @param srcImages
     * @param opType
     * @param clipMode
     */
    public AlgorithmImageCalculator(ModelImage destImage, ModelImage[] srcImages, int opType, int clipMode) {
    	this.srcImages = srcImages;
    	this.destImage = destImage;
    	this.opType = opType;
    	this.clipMode = clipMode;
    	entireImage = true;
    	srcImageA = srcImages[0];
    	setClipValues();
    	if (srcImageA.isColorImage()) {
            colorFactor = 4;
        }
    	
    	
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action used by the advanced calculator.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {

        Object source = event.getSource();
        int len;

        if (event.getActionCommand().equals("Help")) {
            MipavUtil.showHelp("U4031");
        } else if (source == OKButton) {
        	this.getProgressChangeListener().setVisible(true);
            adOpString = textOperator.getText();
            adOpDialog.dispose();
            pressedOK = true;
        } else if (source == cancelButton) {
            adOpString = null;
            adOpDialog.dispose();
            pressedOK = true;
            cancel = true;
        } else if (source == aButton) {
            adOpString = textOperator.getText();
            adOpString += "A";
            textOperator.setText(adOpString);
        } else if (source == bButton) {
            adOpString = textOperator.getText();
            adOpString += "B";
            textOperator.setText(adOpString);
        } else if (source == backSpaceButton) {
            adOpString = textOperator.getText();
            len = adOpString.length();

            if (len >= 1) {
                adOpString = adOpString.substring(0, len - 1);
                textOperator.setText(adOpString);
            }
        } else if (source == ceButton) {
            adOpString = null;
            textOperator.setText(adOpString);
        } else if (source == leftParButton) {
            adOpString = textOperator.getText();
            adOpString += "(";
            textOperator.setText(adOpString);
        } else if (source == rightParButton) {
            adOpString = textOperator.getText();
            adOpString += ")";
            textOperator.setText(adOpString);
        } else if (source == expButton) {
            adOpString = textOperator.getText();
            adOpString += "exp";
            textOperator.setText(adOpString);
        } else if (source == lnButton) {
            adOpString = textOperator.getText();
            adOpString += "ln";
            textOperator.setText(adOpString);
        } else if (source == sevenButton) {
            adOpString = textOperator.getText();
            adOpString += "7";
            textOperator.setText(adOpString);
        } else if (source == eightButton) {
            adOpString = textOperator.getText();
            adOpString += "8";
            textOperator.setText(adOpString);
        } else if (source == nineButton) {
            adOpString = textOperator.getText();
            adOpString += "9";
            textOperator.setText(adOpString);
        } else if (source == divButton) {
            adOpString = textOperator.getText();
            adOpString += "/";
            textOperator.setText(adOpString);
        } else if (source == powButton) {
            adOpString = textOperator.getText();
            adOpString += "pow";
            textOperator.setText(adOpString);
        } else if (source == commaButton) {
            adOpString = textOperator.getText();
            adOpString += ",";
            textOperator.setText(adOpString);
        } else if (source == fourButton) {
            adOpString = textOperator.getText();
            adOpString += "4";
            textOperator.setText(adOpString);
        } else if (source == fiveButton) {
            adOpString = textOperator.getText();
            adOpString += "5";
            textOperator.setText(adOpString);
        } else if (source == sixButton) {
            adOpString = textOperator.getText();
            adOpString += "6";
            textOperator.setText(adOpString);
        } else if (source == multButton) {
            adOpString = textOperator.getText();
            adOpString += "*";
            textOperator.setText(adOpString);
        } else if (source == sinButton) {
            adOpString = textOperator.getText();
            adOpString += "sin";
            textOperator.setText(adOpString);
        } else if (source == cosButton) {
            adOpString = textOperator.getText();
            adOpString += "cos";
            textOperator.setText(adOpString);
        } else if (source == oneButton) {
            adOpString = textOperator.getText();
            adOpString += "1";
            textOperator.setText(adOpString);
        } else if (source == twoButton) {
            adOpString = textOperator.getText();
            adOpString += "2";
            textOperator.setText(adOpString);
        } else if (source == threeButton) {
            adOpString = textOperator.getText();
            adOpString += "3";
            textOperator.setText(adOpString);
        } else if (source == subButton) {
            adOpString = textOperator.getText();
            adOpString += "-";
            textOperator.setText(adOpString);
        } else if (source == tanButton) {
            adOpString = textOperator.getText();
            adOpString += "tan";
            textOperator.setText(adOpString);
        } else if (source == piButton) {
            adOpString = textOperator.getText();
            adOpString += "pi";
            textOperator.setText(adOpString);
        } else if (source == zeroButton) {
            adOpString = textOperator.getText();
            adOpString += "0";
            textOperator.setText(adOpString);
        } else if (source == decimalButton) {
            adOpString = textOperator.getText();
            adOpString += ".";
            textOperator.setText(adOpString);
        } else if (source == modButton) {
            adOpString = textOperator.getText();
            adOpString += "mod";
            textOperator.setText(adOpString);
        } else if (source == addButton) {
            adOpString = textOperator.getText();
            adOpString += "+";
            textOperator.setText(adOpString);
        } else if (source == logButton) {
            adOpString = textOperator.getText();
            adOpString += "log";
            textOperator.setText(adOpString);
        } else if (source == absButton) {
            adOpString = textOperator.getText();
            adOpString += "abs";
            textOperator.setText(adOpString);
        }
    }

    /**
     * Prepares this class for destruction.
     */
    public void finalize() {
        adOpString = null;
        destImage = null;
        srcImageA = null;
        srcImageB = null;
        super.finalize();
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void focusGained(FocusEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void focusLost(FocusEvent event) { }

    /**
     * Accessor to return the advanced function string.
     *
     * @return  adOpString advanced function string
     */
    public String getAdvFunction() {
        return adOpString;
    }

    /**
     * Starts the program.
     */
    public void runAlgorithm() {
    	
    	if(srcImages == null || srcImages.length == 2) {
    		//means we are operating on 2 images
    		if(srcImages != null) {
    			srcImageA = srcImages[0];
    			srcImageB = srcImages[1];
    		}
	        if ((srcImageA == null) || (srcImageB == null)) {
	            displayError("ImageCalculator.run(): Source Image A or source Image B or both are null");
	
	            return;
	        }
	
	        if ((opType == ADVANCED) && (adOpString == null)) {
	            createAdOpDialog(this);
	
	            while (!pressedOK) {
	
	                try {
	                    sleep(5L);
	                } catch (InterruptedException error) { }
	            }
	
	            if (cancel) {
	                setCompleted(false);
	                setThreadStopped(true);
	
	                return;
	            }
	
	            if (adOpString == null) {
	                displayError("AlgorithmImageCalculator: Error on createAdOpDialog");
	                setCompleted(false);
	                setThreadStopped(true);
	
	                return;
	            }
	        }
	        
	        //create progress bar
	        //progressBar = new ViewJProgressBar(srcImageA.getImageName(), "...", 0, 100, true);
	        //progressBar.setSeparateThread(true);
	        //this.addProgressChangeListener(progressBar);
	        //this.setProgressValues(0, 100);
	
	        if (destImage != null) {
	            calcStoreInDest();
            } else if (opType == AlgorithmImageCalculator.MEAN_SQUARED_ERROR) {
                calcMSE();
	        } else {
	            calcInPlace();
	        }
    	} else {
    		//means we are performing on bulk images
    		if(opType == AlgorithmImageCalculator.ADD) {
	    		performBulkAdding();
    		}
            else if(opType == AlgorithmImageCalculator.AVERAGE) {
                performBulkAveraging(false);
            }   
            else if(opType == AlgorithmImageCalculator.AVGERAGE_WITH_STDEV) {
                performBulkAveraging(true);
            }else if(opType == AlgorithmImageCalculator.MINIMUM) {
                performBulkMin();
            }else if(opType == AlgorithmImageCalculator.MAXIMUM) {
                performBulkMax();
            }   
    		
    	}
    }
    
    private void calcMSE() {
        int i, j, k, m;
        int z, t, f;
        int c;
        int offset;
        int length; // total number of data-elements (pixels) in image slice
        double[] bufferA;
        double[] bufferB;
        double[] bufferAI = null;
        double[] bufferBI = null;
        boolean doComplex = false;
        double mse = 0.0;
        double mseR = 0.0;
        double mseG = 0.0;
        double mseB = 0.0;
        ViewUserInterface ui = ViewUserInterface.getReference();
        try {
            length = srcImageA.getSliceSize() * colorFactor;
            bufferA = new double[length];
            bufferB = new double[length];

            if ((srcImageA.getType() == ModelStorageBase.COMPLEX) ||
                    (srcImageA.getType() == ModelStorageBase.DCOMPLEX)) {
                bufferAI = new double[length];
                bufferBI = new double[length];
                doComplex = true;
            }

            fireProgressStateChanged("Image Calculator", "Calculating image ...");

        } catch (OutOfMemoryError e) {
            bufferA = null;
            bufferB = null;
            System.gc();
            displayError("AlgorithmImageCalculator reports: Out of memory when creating image buffer");
            setCompleted(false);
            setThreadStopped(true);

            return;
        }
        
        int mod = length / 20;

        if (srcImageA.getNDims() == 5) {
            f = srcImageA.getExtents()[4];
        } else {
            f = 1;
        }

        if (srcImageA.getNDims() >= 4) {
            t = srcImageA.getExtents()[3];
        } else {
            t = 1;
        }

        if (srcImageA.getNDims() >= 3) {
            z = srcImageA.getExtents()[2];
        } else {
            z = 1;
        }

        // determine if we are dealing with a 3D minus a 2D
        boolean diffExtents = false;

        if ((srcImageA.getNDims() == 3) && (srcImageB.getNDims() == 2)) {
            diffExtents = true;
        }

        boolean doneOnce = false;

        int totalLength = f * t * z * length;

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * t * z * length) + (k * z * length) + (j * length);

                        if (doComplex) {
                            srcImageA.exportDComplexData(2 * offset, length, bufferA, bufferAI);
                        } else {
                            srcImageA.exportData(offset, length, bufferA); // locks and releases lock
                        }

                        if (!diffExtents || !doneOnce) {

                            if (doComplex) {
                                srcImageB.exportDComplexData(2 * offset, length, bufferB, bufferBI);
                            } else {
                                srcImageB.exportData(offset, length, bufferB); // locks and releases lock
                            }

                            doneOnce = true;
                        }
                    } catch (IOException error) {
                        displayError("Algorithm ImageCalculator : Image(s) locked");
                        setCompleted(false);
                        setThreadStopped(true);

                        return;
                    }

                    for (i = 0, c = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        // Get slice
                        if ((entireImage == true) || mask.get(i/colorFactor + offset)) {
                            bufferA[i] = bufferA[i] - bufferB[i];
                            if (doComplex) {
                                bufferAI[i] = bufferAI[i] - bufferBI[i];
                            }
                            if (srcImageA.isColorImage()) {
                               if (c == 0) {
                                   c = 1;
                               }
                               else if (c == 1) {
                                   mseR += bufferA[i] * bufferA[i];
                                   c = 2;
                               }
                               else if (c == 2) {
                                   mseG += bufferA[i] * bufferA[i];
                                   c = 3;
                               }
                               else if (c == 3) {
                                   mseB += bufferA[i] * bufferA[i];
                                   c = 0;
                               }
                            } // if (srcImageA.isColorImage())
                            else if (doComplex) {
                                mse += (bufferA[i]* bufferA[i] + bufferAI[i] * bufferAI[i]);
                            }
                            else {
                                mse += bufferA[i] * bufferA[i];
                            }
                        }
                    }
                }
            }
        }
        if (srcImageA.isColorImage()) {
            mseR = mseR/(totalLength/colorFactor);
            mseG = mseG/(totalLength/colorFactor);
            mseB = mseB/(totalLength/colorFactor);
            ui.setDataText("\nRed mean squared error = " + mseR + "\n");
            ui.setDataText("\nGreen mean squared error = " + mseG + "\n");
            ui.setDataText("\nBlue mean squared error = " + mseB + "\n");
        }
        else {
            mse = mse/totalLength;
            ui.setDataText("\nMean squared error = " + mse + "\n");
        }
        
        setCompleted(true);
        
    }

    /**
     * Runs the calculation and stores the result into the same source buffer.
     */
    private void calcInPlace() {

        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image slice
        double[] bufferA;
        double[] bufferB;
        double[] bufferAI = null;
        double[] bufferBI = null;
        double nonZeroMin;
        double nonZeroMax;
        double smallestMagnitudeNegative;
        double smallestMagnitudePositive;
        double nonZeroMinR;
        double nonZeroMaxR;
        double smallestMagnitudeNegativeR;
        double smallestMagnitudePositiveR;
        double nonZeroMinG;
        double nonZeroMaxG;
        double smallestMagnitudeNegativeG;
        double smallestMagnitudePositiveG;
        double nonZeroMinB;
        double nonZeroMaxB;
        double smallestMagnitudeNegativeB;
        double smallestMagnitudePositiveB;
        boolean doComplex = false;
        double temp;
        double denom;

        try {
            length = srcImageA.getSliceSize() * colorFactor;
            bufferA = new double[length];
            bufferB = new double[length];

            if ((srcImageA.getType() == ModelStorageBase.COMPLEX) ||
                    (srcImageA.getType() == ModelStorageBase.DCOMPLEX)) {
                bufferAI = new double[length];
                bufferBI = new double[length];
                doComplex = true;
            }

            fireProgressStateChanged("Image Calculator", "Calculating image ...");

        } catch (OutOfMemoryError e) {
            bufferA = null;
            bufferB = null;
            System.gc();
            displayError("AlgorithmImageCalculator reports: Out of memory when creating image buffer");
            setCompleted(false);
            setThreadStopped(true);

            return;
        }

        if (srcImageA.isColorImage()) {
            minAR = srcImageA.getMinR();
            minAG = srcImageA.getMinG();
            minAB = srcImageA.getMinB();
            minA = Math.min(minAR, minAG);
            minA = Math.min(minA, minAB);
            maxAR = srcImageA.getMaxR();
            maxAG = srcImageA.getMaxG();
            maxAB = srcImageA.getMaxB();
            maxA = Math.max(maxAR, maxAG);
            maxA = Math.max(maxA, maxAB);
            minBR = srcImageB.getMinR();
            minBG = srcImageB.getMinG();
            minBB = srcImageB.getMinB();
            maxBR = srcImageB.getMaxR();
            maxBG = srcImageB.getMaxG();
            maxBB = srcImageB.getMaxB();
        } else if (doComplex) {
            minA = srcImageA.getNoLogMinNonZero();
            maxA = srcImageA.getNoLogMax();
            minB = srcImageB.getNoLogMinNonZero();
            maxB = srcImageB.getNoLogMax();
        } else {
            minA = srcImageA.getMin();
            maxA = srcImageA.getMax();
            minB = srcImageB.getMin();
            maxB = srcImageB.getMax();
        }

        int newType = srcImageA.getType();

        if ((clipMode == PROMOTE) && (newType != ModelStorageBase.DOUBLE) && (newType != ModelStorageBase.ARGB_FLOAT) &&
                (newType != ModelStorageBase.DCOMPLEX)) {

            switch (opType) {

                case ADD:
                    if (srcImageA.isColorImage()) {
                        bestMax = Math.max(maxAR + maxBR, maxAG + maxBG);
                        bestMax = Math.max(maxAB + maxBB, bestMax);
                        bestMin = Math.min(minAR + minBR, minAG + minBG);
                        bestMin = Math.min(minAB + minBB, bestMin);
                    } else if (doComplex) {
                        bestMax = maxA + maxB;
                        bestMin = -maxA - maxB;
                    } else {
                        bestMax = maxA + maxB;
                        bestMin = minA + minB;
                    }

                    if ((bestMax > clipMax) || (bestMin < clipMin)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;

                case SUBTRACT:
                    if (srcImageA.isColorImage()) {
                        bestMin = Math.min(minAR - maxBR, minAG - maxBG);
                        bestMin = Math.min(minAB - maxBB, bestMin);
                        bestMax = Math.max(maxAR - minBR, maxAG - minBG);
                        bestMax = Math.max(maxAB - minBB, bestMax);
                    } else if (doComplex) {
                        bestMax = maxA + maxB;
                        bestMin = -maxA - maxB;
                    } else {
                        bestMin = minA - maxB;
                        bestMax = maxA - minB;
                    }

                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;

                case MULTIPLY:
                    if (srcImageA.isColorImage()) {
                        bestMin = java.lang.Math.min(minAR * minBR, maxAR * maxBR);
                        bestMin = java.lang.Math.min(bestMin, minAR * maxBR);
                        bestMin = java.lang.Math.min(bestMin, maxAR * minBR);
                        bestMax = java.lang.Math.max(minAR * minBR, maxAR * maxBR);
                        bestMax = java.lang.Math.max(bestMax, minAR * maxBR);
                        bestMax = java.lang.Math.max(bestMax, maxAR * minBR);
                        bestMin = java.lang.Math.min(bestMin, minAG * minBG);
                        bestMin = java.lang.Math.min(bestMin, maxAG * maxBG);
                        bestMin = java.lang.Math.min(bestMin, minAG * maxBG);
                        bestMin = java.lang.Math.min(bestMin, maxAG * minBG);
                        bestMax = java.lang.Math.max(bestMax, minAG * minBG);
                        bestMax = java.lang.Math.max(bestMax, maxAG * maxBG);
                        bestMax = java.lang.Math.max(bestMax, minAG * maxBG);
                        bestMax = java.lang.Math.max(bestMax, maxAG * minBG);
                        bestMin = java.lang.Math.min(bestMin, minAB * minBB);
                        bestMin = java.lang.Math.min(bestMin, maxAB * maxBB);
                        bestMin = java.lang.Math.min(bestMin, minAB * maxBB);
                        bestMin = java.lang.Math.min(bestMin, maxAB * minBB);
                        bestMax = java.lang.Math.max(bestMax, minAB * minBB);
                        bestMax = java.lang.Math.max(bestMax, maxAB * maxBB);
                        bestMax = java.lang.Math.max(bestMax, minAB * maxBB);
                        bestMax = java.lang.Math.max(bestMax, maxAB * minBB);
                    } else if (doComplex) {
                        bestMax = maxA * maxB;
                        bestMin = -maxA * maxB;
                    } else {
                        bestMin = java.lang.Math.min(minA * minB, maxA * maxB);
                        bestMin = java.lang.Math.min(bestMin, minA * maxB);
                        bestMin = java.lang.Math.min(bestMin, maxA * minB);
                        bestMax = java.lang.Math.max(minA * minB, maxA * maxB);
                        bestMax = java.lang.Math.max(bestMax, minA * maxB);
                        bestMax = java.lang.Math.max(bestMax, maxA * minB);
                    }

                    if ((bestMax > clipMax) || (bestMin < clipMin)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;

                case DIVIDE:
                    srcImageB.calcMinMaxNonZero();
                    if (srcImageA.isColorImage()) {
                        nonZeroMinR = srcImageB.getNonZeroMinR();
                        nonZeroMaxR = srcImageB.getNonZeroMaxR();
                        smallestMagnitudeNegativeR = srcImageB.getSmallestMagnitudeNegativeR();
                        smallestMagnitudePositiveR = srcImageB.getSmallestMagnitudePositiveR();
                        nonZeroMinG = srcImageB.getNonZeroMinG();
                        nonZeroMaxG = srcImageB.getNonZeroMaxG();
                        smallestMagnitudeNegativeG = srcImageB.getSmallestMagnitudeNegativeG();
                        smallestMagnitudePositiveG = srcImageB.getSmallestMagnitudePositiveG();
                        nonZeroMinB = srcImageB.getNonZeroMinB();
                        nonZeroMaxB = srcImageB.getNonZeroMaxB();
                        smallestMagnitudeNegativeB = srcImageB.getSmallestMagnitudeNegativeB();
                        smallestMagnitudePositiveB = srcImageB.getSmallestMagnitudePositiveB();

                        // Remember that of A and B, none, 1, or both
                        // might be positive
                        bestMin = Double.POSITIVE_INFINITY;
                        bestMax = -Double.POSITIVE_INFINITY;

                        if (!Double.isNaN(nonZeroMinR)) {
                            bestMin = java.lang.Math.min(bestMin, minAR / nonZeroMinR);
                            bestMin = java.lang.Math.min(bestMin, maxAR / nonZeroMinR);
                            bestMax = java.lang.Math.max(bestMax, minAR / nonZeroMinR);
                            bestMax = java.lang.Math.max(bestMax, maxAR / nonZeroMinR);
                        }

                        if (!Double.isNaN(nonZeroMaxR)) {
                            bestMin = java.lang.Math.min(bestMin, minAR / nonZeroMaxR);
                            bestMin = java.lang.Math.min(bestMin, maxAR / nonZeroMaxR);
                            bestMax = java.lang.Math.max(bestMax, minAR / nonZeroMaxR);
                            bestMax = java.lang.Math.max(bestMax, maxAR / nonZeroMaxR);
                        }

                        if (!Double.isNaN(smallestMagnitudeNegativeR)) {
                            bestMin = java.lang.Math.min(bestMin, minAR / smallestMagnitudeNegativeR);
                            bestMin = java.lang.Math.min(bestMin, maxAR / smallestMagnitudeNegativeR);
                            bestMax = java.lang.Math.max(bestMax, minAR / smallestMagnitudeNegativeR);
                            bestMax = java.lang.Math.max(bestMax, maxAR / smallestMagnitudeNegativeR);
                        }

                        if (!Double.isNaN(smallestMagnitudePositiveR)) {
                            bestMin = java.lang.Math.min(bestMin, minAR / smallestMagnitudePositiveR);
                            bestMin = java.lang.Math.min(bestMin, maxAR / smallestMagnitudePositiveR);
                            bestMax = java.lang.Math.max(bestMax, minAR / smallestMagnitudePositiveR);
                            bestMax = java.lang.Math.max(bestMax, maxAR / smallestMagnitudePositiveR);
                        }

                        if (!Double.isNaN(nonZeroMinG)) {
                            bestMin = java.lang.Math.min(bestMin, minAG / nonZeroMinG);
                            bestMin = java.lang.Math.min(bestMin, maxAG / nonZeroMinG);
                            bestMax = java.lang.Math.max(bestMax, minAG / nonZeroMinG);
                            bestMax = java.lang.Math.max(bestMax, maxAG / nonZeroMinG);
                        }

                        if (!Double.isNaN(nonZeroMaxG)) {
                            bestMin = java.lang.Math.min(bestMin, minAG / nonZeroMaxG);
                            bestMin = java.lang.Math.min(bestMin, maxAG / nonZeroMaxG);
                            bestMax = java.lang.Math.max(bestMax, minAG / nonZeroMaxG);
                            bestMax = java.lang.Math.max(bestMax, maxAG / nonZeroMaxG);
                        }

                        if (!Double.isNaN(smallestMagnitudeNegativeG)) {
                            bestMin = java.lang.Math.min(bestMin, minAG / smallestMagnitudeNegativeG);
                            bestMin = java.lang.Math.min(bestMin, maxAG / smallestMagnitudeNegativeG);
                            bestMax = java.lang.Math.max(bestMax, minAG / smallestMagnitudeNegativeG);
                            bestMax = java.lang.Math.max(bestMax, maxAG / smallestMagnitudeNegativeG);
                        }

                        if (!Double.isNaN(smallestMagnitudePositiveG)) {
                            bestMin = java.lang.Math.min(bestMin, minAG / smallestMagnitudePositiveG);
                            bestMin = java.lang.Math.min(bestMin, maxAG / smallestMagnitudePositiveG);
                            bestMax = java.lang.Math.max(bestMax, minAG / smallestMagnitudePositiveG);
                            bestMax = java.lang.Math.max(bestMax, maxAG / smallestMagnitudePositiveG);
                        }

                        if (!Double.isNaN(nonZeroMinB)) {
                            bestMin = java.lang.Math.min(bestMin, minAB / nonZeroMinB);
                            bestMin = java.lang.Math.min(bestMin, maxAB / nonZeroMinB);
                            bestMax = java.lang.Math.max(bestMax, minAB / nonZeroMinB);
                            bestMax = java.lang.Math.max(bestMax, maxAB / nonZeroMinB);
                        }

                        if (!Double.isNaN(nonZeroMaxB)) {
                            bestMin = java.lang.Math.min(bestMin, minAB / nonZeroMaxB);
                            bestMin = java.lang.Math.min(bestMin, maxAB / nonZeroMaxB);
                            bestMax = java.lang.Math.max(bestMax, minAB / nonZeroMaxB);
                            bestMax = java.lang.Math.max(bestMax, maxAB / nonZeroMaxB);
                        }

                        if (!Double.isNaN(smallestMagnitudeNegativeB)) {
                            bestMin = java.lang.Math.min(bestMin, minAB / smallestMagnitudeNegativeB);
                            bestMin = java.lang.Math.min(bestMin, maxAB / smallestMagnitudeNegativeB);
                            bestMax = java.lang.Math.max(bestMax, minAB / smallestMagnitudeNegativeB);
                            bestMax = java.lang.Math.max(bestMax, maxAB / smallestMagnitudeNegativeB);
                        }

                        if (!Double.isNaN(smallestMagnitudePositiveB)) {
                            bestMin = java.lang.Math.min(bestMin, minAB / smallestMagnitudePositiveB);
                            bestMin = java.lang.Math.min(bestMin, maxAB / smallestMagnitudePositiveB);
                            bestMax = java.lang.Math.max(bestMax, minAB / smallestMagnitudePositiveB);
                            bestMax = java.lang.Math.max(bestMax, maxAB / smallestMagnitudePositiveB);
                        }
                    } else if (doComplex) {
                        bestMax = maxA / minB;
                        bestMin = -maxA / minB;
                    } else { // not color
                        nonZeroMin = srcImageB.getNonZeroMin();
                        nonZeroMax = srcImageB.getNonZeroMax();
                        smallestMagnitudeNegative = srcImageB.getSmallestMagnitudeNegative();
                        smallestMagnitudePositive = srcImageB.getSmallestMagnitudePositive();

                        // Remember that of A and B, none, 1, or both
                        // might be positive
                        bestMin = Double.POSITIVE_INFINITY;
                        bestMax = -Double.POSITIVE_INFINITY;

                        if (!Double.isNaN(nonZeroMin)) {
                            bestMin = java.lang.Math.min(bestMin, minA / nonZeroMin);
                            bestMin = java.lang.Math.min(bestMin, maxA / nonZeroMin);
                            bestMax = java.lang.Math.max(bestMax, minA / nonZeroMin);
                            bestMax = java.lang.Math.max(bestMax, maxA / nonZeroMin);
                        }

                        if (!Double.isNaN(nonZeroMax)) {
                            bestMin = java.lang.Math.min(bestMin, minA / nonZeroMax);
                            bestMin = java.lang.Math.min(bestMin, maxA / nonZeroMax);
                            bestMax = java.lang.Math.max(bestMax, minA / nonZeroMax);
                            bestMax = java.lang.Math.max(bestMax, maxA / nonZeroMax);
                        }

                        if (!Double.isNaN(smallestMagnitudeNegative)) {
                            bestMin = java.lang.Math.min(bestMin, minA / smallestMagnitudeNegative);
                            bestMin = java.lang.Math.min(bestMin, maxA / smallestMagnitudeNegative);
                            bestMax = java.lang.Math.max(bestMax, minA / smallestMagnitudeNegative);
                            bestMax = java.lang.Math.max(bestMax, maxA / smallestMagnitudeNegative);
                        }

                        if (!Double.isNaN(smallestMagnitudePositive)) {
                            bestMin = java.lang.Math.min(bestMin, minA / smallestMagnitudePositive);
                            bestMin = java.lang.Math.min(bestMin, maxA / smallestMagnitudePositive);
                            bestMax = java.lang.Math.max(bestMax, minA / smallestMagnitudePositive);
                            bestMax = java.lang.Math.max(bestMax, maxA / smallestMagnitudePositive);
                        }
                    } // else not color

                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;
                    
                case AVERAGE:
                    if (srcImageA.isColorImage()) {
                        bestMax = Math.max((maxAR + maxBR)/2.0, (maxAG + maxBG)/2.0);
                        bestMax = Math.max((maxAB + maxBB)/2.0, bestMax);
                        bestMin = Math.min((minAR + minBR)/2.0, (minAG + minBG)/2.0);
                        bestMin = Math.min((minAB + minBB)/2.0, bestMin);
                    } else if (doComplex) {
                        bestMax = (maxA + maxB)/2.0;
                        bestMin = (-maxA - maxB)/2.0;
                    } else {
                        bestMax = (maxA + maxB)/2.0;
                        bestMin = (minA + minB)/2.0;
                    }

                    if ((bestMax > clipMax) || (bestMin < clipMin)) {
                        newType = findType(srcImageA.getType());
                    }
                    
                    break;
                    
                case DIFFERENCE:
                    if (srcImageA.isColorImage()) {
                        bestMin = 0;
                        bestMax = Math.max(Math.abs(minAR - maxBR), Math.abs(minAG - maxBG));
                        bestMax = Math.max(Math.abs(minAB - maxBB), bestMax);
                        bestMax = Math.max(Math.abs(maxAR - minBR), bestMax);
                        bestMax = Math.max(Math.abs(maxAG - minBG), bestMax);
                        bestMax = Math.max(Math.abs(maxAB - minBB), bestMax);
                    } else if (doComplex) {
                        bestMin = 0;
                        bestMax = Math.abs(maxA + maxB);
                    } else {
                        bestMin = 0;
                        bestMax = Math.max(Math.abs(minA - maxB),Math.abs(maxA - minB));
                    }

                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;
                    
                case MAXIMUM:
                    if (srcImageA.isColorImage()) {
                        bestMin = Math.max(minAR, minBR);
                        bestMin = Math.min(bestMin,Math.max(minAG, minBG));
                        bestMin = Math.min(bestMin, Math.max(minAB, minBB));
                        bestMax = Math.max(maxAR, maxBR);
                        bestMax = Math.max(bestMax, Math.max(maxAG, maxBG));
                        bestMax = Math.max(bestMax, Math.max(maxAB, maxBB));
                    } else if (doComplex) {
                        bestMin = Math.max(-maxA, -maxB);
                        bestMax = Math.max(maxA, maxB);
                    } else {
                        bestMin = Math.max(minA, minB);
                        bestMax = Math.max(maxA, maxB);
                    }
                    
                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;
                    
                case MINIMUM:
                    if (srcImageA.isColorImage()) {
                        bestMin = Math.min(minAR, minBR);
                        bestMin = Math.min(bestMin,Math.min(minAG, minBG));
                        bestMin = Math.min(bestMin, Math.min(minAB, minBB));
                        bestMax = Math.min(maxAR, maxBR);
                        bestMax = Math.max(bestMax, Math.min(maxAG, maxBG));
                        bestMax = Math.max(bestMax, Math.min(maxAB, maxBB));
                    } else if (doComplex) {
                        bestMin = Math.min(-maxA, -maxB);
                        bestMax = Math.min(maxA, maxB);
                    } else {
                        bestMin = Math.min(minA, minB);
                        bestMax = Math.min(maxA, maxB);
                    }
                    
                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;
                    
                case OR:
                case XOR:
                    if (srcImageA.isColorImage()) {
                        bestMin = Math.min(minAR, minBR);
                        bestMin = Math.min(bestMin,Math.min(minAG, minBG));
                        bestMin = Math.min(bestMin, Math.min(minAB, minBB));
                        bestMax = Math.max(maxAR, maxBR);
                        bestMax = Math.max(bestMax, Math.max(maxAG, maxBG));
                        bestMax = Math.max(bestMax, Math.max(maxAB, maxBB));
                    } else if (doComplex) {
                        bestMin = Math.min(-maxA, -maxB);
                        bestMax = Math.max(maxA, maxB);
                    } else {
                        bestMin = Math.min(minA, minB);
                        bestMax = Math.max(maxA, maxB);
                    }
                    
                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;
                        

                case ADVANCED:
                    newType = ModelImage.DOUBLE;
                    break;

                default:
                    break;
            }

            if (newType != srcImageA.getType()) {
                AlgorithmChangeType changeTypeAlgo = new AlgorithmChangeType(srcImageA, newType, minA, maxA, minA, maxA,
                                                                             false);
                changeTypeAlgo.setRunningInSeparateThread(runningInSeparateThread);
                changeTypeAlgo.run();

                if (!changeTypeAlgo.isCompleted()) { // if the change algo was halted,

                    // halt the rest of this processing.
                    setThreadStopped(true);
                    setCompleted(false);
                    setThreadStopped(true);

                    return;
                }
                changeTypeAlgo.finalize();
                changeTypeAlgo = null;
            }
        }

        int mod = length / 20;

        if (srcImageA.getNDims() == 5) {
            f = srcImageA.getExtents()[4];
        } else {
            f = 1;
        }

        if (srcImageA.getNDims() >= 4) {
            t = srcImageA.getExtents()[3];
        } else {
            t = 1;
        }

        if (srcImageA.getNDims() >= 3) {
            z = srcImageA.getExtents()[2];
        } else {
            z = 1;
        }

        // determine if we are dealing with an acceptable different extents case,
        // these will allow srcImageB to be applied on a per n-1 dimension basis to all "slices" of n dimension. 
        // Each "slice" has size diffExtentsFactor
        boolean diffExtents = false;
        int diffExtentsFactor = 1;

        if (srcImageA.getNDims()-1 == srcImageB.getNDims()) {
            diffExtents = true;
            for(int d=0; d<srcImageB.getNDims(); d++) {
                diffExtentsFactor *= srcImageB.getExtents()[d];
            }
        }

        
        if(opType ==  ADVANCED) {
        	rpn = evaluateToRPN(adOpString);
        	if(rpn.equals("")) {
    			MipavUtil.displayError("Error in formula expression");
    			setCompleted(false);
                setThreadStopped(true);
    			return;
    		}
        }

        int totalLength = f * t * z * length;
        //The current "slice" of srcImageA
        int pseudoSliceNum = 0;

        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {
                    
                    try {
                        offset = (m * t * z * length) + (k * z * length) + (j * length);
                        
                        if (doComplex) {
                            srcImageA.exportDComplexData(2 * offset, length, bufferA, bufferAI);
                        } else {
                            srcImageA.exportData(offset, length, bufferA); // locks and releases lock
                        }

                        if (!diffExtents) {

                            if (doComplex) {
                                srcImageB.exportDComplexData(2 * offset, length, bufferB, bufferBI);
                            } else {
                                srcImageB.exportData(offset, length, bufferB); // locks and releases lock
                            }
                        } else if(diffExtents) {
                            
                            if (doComplex) {
                                pseudoSliceNum = 2 * offset / diffExtentsFactor;
                                srcImageB.exportDComplexData(2 * offset - pseudoSliceNum*diffExtentsFactor, length, bufferB, bufferBI);
                            } else {
                                pseudoSliceNum = offset / diffExtentsFactor;
                                srcImageB.exportData(offset - pseudoSliceNum*diffExtentsFactor, length, bufferB); // locks and releases lock
                            }
                        }
                    } catch (IOException error) {
                        displayError("Algorithm ImageCalculator : Image(s) locked");
                        setCompleted(false);
                        setThreadStopped(true);

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        // Get slice
                        if ((entireImage == true) || mask.get((i + offset)/colorFactor)) {

                            switch (opType) {

                                case ADD:
                                    bufferA[i] = bufferA[i] + bufferB[i];
                                    if (doComplex) {
                                        bufferAI[i] = bufferAI[i] + bufferBI[i];
                                    }

                                    break;

                                case SUBTRACT:
                                    bufferA[i] = bufferA[i] - bufferB[i];
                                    if (doComplex) {
                                        bufferAI[i] = bufferAI[i] - bufferBI[i];
                                    }

                                    break;

                                case MULTIPLY:
                                    if (doComplex) {
                                        temp = (bufferA[i] * bufferB[i]) - (bufferAI[i] * bufferBI[i]);
                                        bufferAI[i] = (bufferA[i] * bufferBI[i]) + (bufferAI[i] * bufferB[i]);
                                        bufferA[i] = temp;
                                    } else {
                                        bufferA[i] = bufferA[i] * bufferB[i];
                                    }

                                    break;

                                case DIVIDE:
                                    if (doComplex) {
                                        denom = (bufferB[i] * bufferB[i]) + (bufferBI[i] * bufferBI[i]);
                                        temp = ((bufferA[i] * bufferB[i]) + (bufferAI[i] * bufferBI[i])) / denom;
                                        bufferAI[i] = ((-bufferA[i] * bufferBI[i]) + (bufferAI[i] * bufferB[i])) /
                                                          denom;
                                        bufferA[i] = temp;
                                    } else {
                                        bufferA[i] = bufferA[i] / bufferB[i];
                                    }

                                    break;

                                case AND:
                                    if ((bufferA[i] == 0) || (bufferB[i] == 0)) {
                                        bufferA[i] = 0;
                                    }

                                    break;

                                case OR:
                                    if (bufferA[i] == 0) {
                                        bufferA[i] = bufferB[i];
                                    }

                                    break;

                                case XOR:
                                    if ((bufferA[i] != 0) && (bufferB[i] != 0)) {
                                        bufferA[i] = 0;
                                    } else if ((bufferA[i] == 0) && (bufferB[i] != 0)) {
                                        bufferA[i] = bufferB[i];
                                    }

                                    break;
                               
                                case DIFFERENCE:
                                    if (doComplex) {
                                        bufferA[i] = Math.sqrt((bufferA[i] - bufferB[i])*(bufferA[i] - bufferB[i]) +
                                                               (bufferAI[i] - bufferBI[i])*(bufferAI[i] - bufferBI[i]));
                                        bufferAI[i] = 0;
                                    }
                                    else {
                                        bufferA[i] = Math.abs(bufferA[i] - bufferB[i]);
                                    }
                                    break;

                                case AVERAGE:
                                    bufferA[i] = (bufferA[i] + bufferB[i]) / 2.0;
                                    if (doComplex) {
                                        bufferAI[i] = (bufferAI[i] + bufferBI[i])/2.0;
                                    }
                                    break;

                                case MAXIMUM:
                                    bufferA[i] = Math.max(bufferA[i], bufferB[i]);
                                    break;

                                case MINIMUM:
                                    bufferA[i] = Math.min(bufferA[i], bufferB[i]);
                                    break;

                                case ADVANCED:
                                	double d = evaluateRPNExpression(bufferA[i], bufferB[i], rpn);
                                	if(d != Double.NaN) {
                                		bufferA[i] = d;
                                	}else {
                                		displayError("Algorithm ImageCalculator: Error evaluating expression");
                                		setCompleted(false);
                                        setThreadStopped(true);
                                        return;
                                	}
                                    if (OK == false) {
                                        displayError("Algorithm ImageCalculator: Illegal operator expression");
                                        setCompleted(false);

                                        setThreadStopped(true);

                                        return;
                                    }

                                    break;

                                default:
                                    break;
                            }
                        }
                    }

                    // clip check
                    if (clipMode == CLIP) {

                        if (srcImageA.getType() == ModelStorageBase.COMPLEX) {

                            // Keep same phase in clipping
                            for (i = 0; i < length; i++) {

                                if ((bufferA[i] > Float.MAX_VALUE) || (bufferA[i] < -Float.MAX_VALUE) ||
                                        (bufferAI[i] > Float.MAX_VALUE) || (bufferAI[i] < -Float.MAX_VALUE)) {

                                    if (Math.abs(bufferA[i]) >= Math.abs(bufferAI[i])) {

                                        if (bufferA[i] > Float.MAX_VALUE) {
                                            bufferAI[i] = bufferAI[i] * (Float.MAX_VALUE / bufferA[i]);
                                            bufferA[i] = Float.MAX_VALUE;
                                        } else {
                                            bufferAI[i] = bufferAI[i] * (-Float.MAX_VALUE / bufferA[i]);
                                            bufferA[i] = -Float.MAX_VALUE;
                                        }
                                    } else { // Math.abs(bufferA[i]) < Math.abs(bufferAI[i])

                                        if (bufferAI[i] > Float.MAX_VALUE) {
                                            bufferA[i] = bufferA[i] * (Float.MAX_VALUE / bufferAI[i]);
                                            bufferAI[i] = Float.MAX_VALUE;
                                        } else {
                                            bufferA[i] = bufferA[i] * (-Float.MAX_VALUE / bufferAI[i]);
                                            bufferAI[i] = -Float.MAX_VALUE;
                                        }
                                    }
                                }
                            }
                        } // if (srcImage.getType() == ModelStorageBase.COMPLEX)
                        else if (!doComplex) {

                            for (i = 0; i < length; i++) {

                                if (bufferA[i] > clipMax) {
                                    bufferA[i] = clipMax;
                                } else if (bufferA[i] < clipMin) {
                                    bufferA[i] = clipMin;
                                }
                            }
                        } // else if (!doComplex)
                    } // if (clipMode == clip)

                    try {

                        if (threadStopped) { // do BEFORE buffer has been exported to Image
                            setThreadStopped(true);
                            finalize();

                            return;
                        }

                        if (doComplex) {
                            srcImageA.importDComplexData(2 * offset, bufferA, bufferAI, false, true);
                        } else {
                            srcImageA.importData(offset, bufferA, false);
                        }
                    } catch (IOException error) {
                        displayError("Algorithm ImageCalculator: Image(s) locked");
                        setCompleted(false);

                        setThreadStopped(true);

                        return;
                    }
                }
            } // k loop
        } // f loop

        if (threadStopped) {
            finalize();

            return;
        }

        if (doComplex) {
            srcImageA.calcMinMaxMag(Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
        } else {
            srcImageA.calcMinMax();
        }

        setCompleted(true);
    }
    
    
    /**
     * perform bulk adidng
     *
     */
    private void performBulkAdding() {
    	srcImageA = srcImages[0];
		srcImageB = srcImages[1];
		calcStoreInDest();
		for(int i=2;i<srcImages.length;i++) {
			srcImageA = destImage;
			srcImageB = srcImages[i];
			calcStoreInDest();	
		}
    }
    
    
    
    
    /**
     * perform bulk adidng
     *
     */
    private void performBulkMin() {
    	srcImageA = srcImages[0];
		srcImageB = srcImages[1];
		calcStoreInDest();
		for(int i=2;i<srcImages.length;i++) {
			srcImageA = destImage;
			srcImageB = srcImages[i];
			calcStoreInDest();	
		}
    }
    
    
    /**
     * perform bulk adidng
     *
     */
    private void performBulkMax() {
    	srcImageA = srcImages[0];
		srcImageB = srcImages[1];
		calcStoreInDest();
		for(int i=2;i<srcImages.length;i++) {
			srcImageA = destImage;
			srcImageB = srcImages[i];
			calcStoreInDest();	
		}
    }
    
    
    
    
    /**
     * perform bulk averaging
     *
     */
    private void performBulkAveraging(boolean findSD) {
        //System.out.println(findSD);
    	int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image slice
        double[] bufferA; // data-buffer (for pixel data) which is the "heart" of the image
        double[] bufferAI = null;
        boolean doComplex = false;
        int numSrcImages = srcImages.length;
        
        //first do bulk adding..set clipMode to PROMOTE to do the bulk adding...but remember original clipMode for the averaging part
        opType = AlgorithmImageCalculator.ADD;
		performBulkAdding();
		
		//now do averaging by dividing dest image by numSrcImages
		opType = AlgorithmImageCalculator.AVERAGE;
        try {
            length = destImage.getSliceSize() * colorFactor;
            bufferA = new double[length];
            if ((srcImageA.getType() == ModelStorageBase.COMPLEX) ||
                    (srcImageA.getType() == ModelStorageBase.DCOMPLEX)) {
                bufferAI = new double[length];
                doComplex = true;
            }
        }catch (OutOfMemoryError e) {
            bufferA = null;
            System.gc();
            displayError("Algorithm ImageCalculator reports: Out of memory when creating image buffer");
            setCompleted(false);
            setThreadStopped(true);

            return;
        }
        
        if (destImage.getNDims() == 5) {
            f = destImage.getExtents()[4];
        } else {
            f = 1;
        }

        if (destImage.getNDims() >= 4) {
            t = destImage.getExtents()[3];
        } else {
            t = 1;
        }

        if (destImage.getNDims() >= 3) {
            z = destImage.getExtents()[2];
        } else {
            z = 1;
        }
        
        for (m = 0; (m < f) && !threadStopped; m++) {
            for (k = 0; (k < t) && !threadStopped; k++) {
                for (j = 0; (j < z) && !threadStopped; j++) {
                    try {
                        offset = (m * t * z * length) + (k * z * length) + (j * length);

                        if (doComplex) {
                            destImage.exportDComplexData(2 * offset, length, bufferA, bufferAI);
                        } else {
                            destImage.exportData(offset, length, bufferA); // locks and releases lock
                        }
                    } catch (IOException error) {
                        displayError("Algorithm Image Calculator : Image(s) locked");
                        setCompleted(false);
                        setThreadStopped(true);

                        return;
                    }
                    for (i = 0; (i < length) && !threadStopped; i++) {
                    	bufferA[i] = bufferA[i]/numSrcImages;
                        if (doComplex) {
                            bufferAI[i] = bufferAI[i]/numSrcImages;
                        }
                    }
                    
                    try {

                        if (threadStopped) { // do BEFORE buffer has been exported to Image
                            finalize();
                            return;
                        }

                        if (doComplex) {
                            destImage.importDComplexData(2 * offset, bufferA, bufferAI, false, true);
                        } else {
                            destImage.importData(offset, bufferA, false);
                        }
                    } catch (IOException error) {
                        displayError("Algorithm ImageCalculator: Image(s) locked");
                        setCompleted(false);
                        setThreadStopped(true);
                        return;
                    }
                }
            } // k loop
        } // f loop

        if (threadStopped) { // do BEFORE 'completed'
            finalize();

            return;
        }

        if (doComplex) {
            destImage.calcMinMaxMag(Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
            destImage.setOriginalExtents(srcImageA.getOriginalExtents());
            destImage.setOriginalMinimum(srcImageA.getOriginalMinimum());
            destImage.setOriginalMaximum(srcImageA.getOriginalMaximum());
            destImage.setOriginalDoCrop(srcImageA.getOriginalDoCrop());

            if (srcImageA.getOriginalDoCrop()) {
                destImage.setOriginalStart(srcImageA.getOriginalStart());
                destImage.setOriginalEnd(srcImageA.getOriginalEnd());
            }

            destImage.setUnequalDim(srcImageA.getUnequalDim());
            destImage.setImage25D(srcImageA.getImage25D());
            destImage.setOriginalKernelDimension(srcImageA.getOriginalKernelDimension());
            destImage.setOriginalFilterConstruction(srcImageA.getOriginalFilterConstruction());
            destImage.setHaveWindowed(srcImageA.getHaveWindowed());
        } else {
            destImage.calcMinMax();
        }
        
        if (findSD){
			if (destImage.getNDims() == 5) {
				f = destImage.getExtents()[4];
			} else {
				f = 1;
			}
	
			if (destImage.getNDims() >= 4) {
				t = destImage.getExtents()[3];
			} else {
				t = 1;
			}
	
			if (destImage.getNDims() >= 3) {
				z = destImage.getExtents()[2];
			} else {
				z = 1;
			}
			
			ModelImage stDev = new ModelImage(ModelStorageBase.DOUBLE, destImage.getExtents(), "St Dev");
            
			offset=0;
			for (m = 0; (m < f) && !threadStopped; m++) {
				for (k = 0; (k < t) && !threadStopped; k++) {
					for (j = 0; (j < z) && !threadStopped; j++) {
						double mean[] = new double[length], current[]= new double[length], total[]= new double[length];
						try {
							offset = (m * t * z * length) + (k * z * length) + (j * length);
							destImage.exportData(offset, length, mean);
						} catch (Exception e) {
							MipavUtil.displayError("I/O Problem: "+e.getMessage());
						}
						
						for (int ii = 0; ii < srcImages.length; ii++){
							try {
								srcImages[ii].exportData(offset, length, current);
							} catch (Exception e) {
								MipavUtil.displayError("I/O Problem: "+e.getMessage());
							}
							
							for (int jj = 0; jj < mean.length; jj++){
								current[jj] = current[jj] - mean[jj];
								current[jj] = current[jj] * current[jj];
								total[jj] = current[jj] + total[jj];
							}
						}
						for (int ii = 0; ii < total.length; ii++){
							total[ii] = total[ii]/srcImages.length;
							total[ii] = Math.sqrt(total[ii]);
						}
						
						try {
							stDev.importData(offset, total, false);
						} catch (Exception e) {
							MipavUtil.displayError("I/O Problem: "+e.getMessage());
						}
					}
				}
			}
			stDev.calcMinMax();
            
			try {
				new ViewJFrameImage(stDev, null, new Dimension(610, 200));
			} catch (Exception e) {
				System.gc();
				MipavUtil.displayError("Out of memory: unable to open new frame"+e.getMessage());
			}

         }

        setCompleted(true);
    }

    /**
     * Runs the calculation and stores it in a new ModelImage.
     */
    private void calcStoreInDest() {

        int i, j, k, m;
        int z, t, f;
        int offset;
        int length; // total number of data-elements (pixels) in image slice
        double[] bufferA; // data-buffer (for pixel data) which is the "heart" of the image
        double[] bufferB;
        double[] bufferAI = null;
        double[] bufferBI = null;
        double nonZeroMin;
        double nonZeroMax;
        double smallestMagnitudeNegative;
        double smallestMagnitudePositive;
        double nonZeroMinR;
        double nonZeroMaxR;
        double smallestMagnitudeNegativeR;
        double smallestMagnitudePositiveR;
        double nonZeroMinG;
        double nonZeroMaxG;
        double smallestMagnitudeNegativeG;
        double smallestMagnitudePositiveG;
        double nonZeroMinB;
        double nonZeroMaxB;
        double smallestMagnitudeNegativeB;
        double smallestMagnitudePositiveB;
        boolean doComplex = false;
        double temp;
        double denom;

        try {
            length = srcImageA.getSliceSize() * colorFactor;
            bufferA = new double[length];
            bufferB = new double[length];

            if ((srcImageA.getType() == ModelStorageBase.COMPLEX) ||
                    (srcImageA.getType() == ModelStorageBase.DCOMPLEX)) {
                bufferAI = new double[length];
                bufferBI = new double[length];
                doComplex = true;
            }

            fireProgressStateChanged("Image Calculator", "Calculating image ...");
        } catch (OutOfMemoryError e) {
            bufferA = null;
            System.gc();
            displayError("Algorithm ImageCalculator reports: Out of memory when creating image buffer");
            setCompleted(false);
            setThreadStopped(true);

            return;
        }

        if (srcImageA.isColorImage()) {
            minAR = srcImageA.getMinR();
            minAG = srcImageA.getMinG();
            minAB = srcImageA.getMinB();
            maxAR = srcImageA.getMaxR();
            maxAG = srcImageA.getMaxG();
            maxAB = srcImageA.getMaxB();
            minBR = srcImageB.getMinR();
            minBG = srcImageB.getMinG();
            minBB = srcImageB.getMinB();
            maxBR = srcImageB.getMaxR();
            maxBG = srcImageB.getMaxG();
            maxBB = srcImageB.getMaxB();
        } else if (doComplex) {
            minA = srcImageA.getNoLogMinNonZero();
            maxA = srcImageA.getNoLogMax();
            minB = srcImageB.getNoLogMinNonZero();
            maxB = srcImageB.getNoLogMax();
        } else {
            minA = srcImageA.getMin();
            maxA = srcImageA.getMax();
            minB = srcImageB.getMin();
            maxB = srcImageB.getMax();
        }

        int newType = srcImageA.getType();

        if ((clipMode == PROMOTE) && (newType != ModelStorageBase.DOUBLE) && (newType != ModelStorageBase.ARGB_FLOAT) &&
                (newType != ModelStorageBase.DCOMPLEX)) {

            switch (opType) {

                case ADD:
                    if (srcImageA.isColorImage()) {
                        bestMax = Math.max(maxAR + maxBR, maxAG + maxBG);
                        bestMax = Math.max(maxAB + maxBB, bestMax);
                        bestMin = Math.min(minAR + minBR, minAG + minBG);
                        bestMin = Math.min(minAB + minBB, bestMin);
                    } else if (doComplex) {
                        bestMax = maxA + maxB;
                        bestMin = -maxA - maxB;
                    } else {
                        bestMax = maxA + maxB;
                        bestMin = minA + minB;
                    }

                    if ((bestMax > clipMax) || (bestMin < clipMin)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;

                case SUBTRACT:
                    if (srcImageA.isColorImage()) {
                        bestMin = Math.min(minAR - maxBR, minAG - maxBG);
                        bestMin = Math.min(minAB - maxBB, bestMin);
                        bestMax = Math.max(maxAR - minBR, maxAG - minBG);
                        bestMax = Math.max(maxAB - minBB, bestMax);
                    } else if (doComplex) {
                        bestMax = maxA + maxB;
                        bestMin = -maxA - maxB;
                    } else {
                        bestMin = minA - maxB;
                        bestMax = maxA - minB;
                    }

                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;

                case MULTIPLY:
                    if (srcImageA.isColorImage()) {
                        bestMin = java.lang.Math.min(minAR * minBR, maxAR * maxBR);
                        bestMin = java.lang.Math.min(bestMin, minAR * maxBR);
                        bestMin = java.lang.Math.min(bestMin, maxAR * minBR);
                        bestMax = java.lang.Math.max(minAR * minBR, maxAR * maxBR);
                        bestMax = java.lang.Math.max(bestMax, minAR * maxBR);
                        bestMax = java.lang.Math.max(bestMax, maxAR * minBR);
                        bestMin = java.lang.Math.min(bestMin, minAG * minBG);
                        bestMin = java.lang.Math.min(bestMin, maxAG * maxBG);
                        bestMin = java.lang.Math.min(bestMin, minAG * maxBG);
                        bestMin = java.lang.Math.min(bestMin, maxAG * minBG);
                        bestMax = java.lang.Math.max(bestMax, minAG * minBG);
                        bestMax = java.lang.Math.max(bestMax, maxAG * maxBG);
                        bestMax = java.lang.Math.max(bestMax, minAG * maxBG);
                        bestMax = java.lang.Math.max(bestMax, maxAG * minBG);
                        bestMin = java.lang.Math.min(bestMin, minAB * minBB);
                        bestMin = java.lang.Math.min(bestMin, maxAB * maxBB);
                        bestMin = java.lang.Math.min(bestMin, minAB * maxBB);
                        bestMin = java.lang.Math.min(bestMin, maxAB * minBB);
                        bestMax = java.lang.Math.max(bestMax, minAB * minBB);
                        bestMax = java.lang.Math.max(bestMax, maxAB * maxBB);
                        bestMax = java.lang.Math.max(bestMax, minAB * maxBB);
                        bestMax = java.lang.Math.max(bestMax, maxAB * minBB);
                    } else if (doComplex) {
                        bestMax = maxA * maxB;
                        bestMin = -maxA * maxB;
                    } else {
                        bestMin = java.lang.Math.min(minA * minB, maxA * maxB);
                        bestMin = java.lang.Math.min(bestMin, minA * maxB);
                        bestMin = java.lang.Math.min(bestMin, maxA * minB);
                        bestMax = java.lang.Math.max(minA * minB, maxA * maxB);
                        bestMax = java.lang.Math.max(bestMax, minA * maxB);
                        bestMax = java.lang.Math.max(bestMax, maxA * minB);
                    }

                    if ((bestMax > clipMax) || (bestMin < clipMin)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;

                case DIVIDE:
                    srcImageB.calcMinMaxNonZero();
                    if (srcImageA.isColorImage()) {
                        nonZeroMinR = srcImageB.getNonZeroMinR();
                        nonZeroMaxR = srcImageB.getNonZeroMaxR();
                        smallestMagnitudeNegativeR = srcImageB.getSmallestMagnitudeNegativeR();
                        smallestMagnitudePositiveR = srcImageB.getSmallestMagnitudePositiveR();
                        nonZeroMinG = srcImageB.getNonZeroMinG();
                        nonZeroMaxG = srcImageB.getNonZeroMaxG();
                        smallestMagnitudeNegativeG = srcImageB.getSmallestMagnitudeNegativeG();
                        smallestMagnitudePositiveG = srcImageB.getSmallestMagnitudePositiveG();
                        nonZeroMinB = srcImageB.getNonZeroMinB();
                        nonZeroMaxB = srcImageB.getNonZeroMaxB();
                        smallestMagnitudeNegativeB = srcImageB.getSmallestMagnitudeNegativeB();
                        smallestMagnitudePositiveB = srcImageB.getSmallestMagnitudePositiveB();

                        // Remember that of A and B, none, 1, or both
                        // might be positive
                        bestMin = Double.POSITIVE_INFINITY;
                        bestMax = -Double.POSITIVE_INFINITY;

                        if (!Double.isNaN(nonZeroMinR)) {
                            bestMin = java.lang.Math.min(bestMin, minAR / nonZeroMinR);
                            bestMin = java.lang.Math.min(bestMin, maxAR / nonZeroMinR);
                            bestMax = java.lang.Math.max(bestMax, minAR / nonZeroMinR);
                            bestMax = java.lang.Math.max(bestMax, maxAR / nonZeroMinR);
                        }

                        if (!Double.isNaN(nonZeroMaxR)) {
                            bestMin = java.lang.Math.min(bestMin, minAR / nonZeroMaxR);
                            bestMin = java.lang.Math.min(bestMin, maxAR / nonZeroMaxR);
                            bestMax = java.lang.Math.max(bestMax, minAR / nonZeroMaxR);
                            bestMax = java.lang.Math.max(bestMax, maxAR / nonZeroMaxR);
                        }

                        if (!Double.isNaN(smallestMagnitudeNegativeR)) {
                            bestMin = java.lang.Math.min(bestMin, minAR / smallestMagnitudeNegativeR);
                            bestMin = java.lang.Math.min(bestMin, maxAR / smallestMagnitudeNegativeR);
                            bestMax = java.lang.Math.max(bestMax, minAR / smallestMagnitudeNegativeR);
                            bestMax = java.lang.Math.max(bestMax, maxAR / smallestMagnitudeNegativeR);
                        }

                        if (!Double.isNaN(smallestMagnitudePositiveR)) {
                            bestMin = java.lang.Math.min(bestMin, minAR / smallestMagnitudePositiveR);
                            bestMin = java.lang.Math.min(bestMin, maxAR / smallestMagnitudePositiveR);
                            bestMax = java.lang.Math.max(bestMax, minAR / smallestMagnitudePositiveR);
                            bestMax = java.lang.Math.max(bestMax, maxAR / smallestMagnitudePositiveR);
                        }

                        if (!Double.isNaN(nonZeroMinG)) {
                            bestMin = java.lang.Math.min(bestMin, minAG / nonZeroMinG);
                            bestMin = java.lang.Math.min(bestMin, maxAG / nonZeroMinG);
                            bestMax = java.lang.Math.max(bestMax, minAG / nonZeroMinG);
                            bestMax = java.lang.Math.max(bestMax, maxAG / nonZeroMinG);
                        }

                        if (!Double.isNaN(nonZeroMaxG)) {
                            bestMin = java.lang.Math.min(bestMin, minAG / nonZeroMaxG);
                            bestMin = java.lang.Math.min(bestMin, maxAG / nonZeroMaxG);
                            bestMax = java.lang.Math.max(bestMax, minAG / nonZeroMaxG);
                            bestMax = java.lang.Math.max(bestMax, maxAG / nonZeroMaxG);
                        }

                        if (!Double.isNaN(smallestMagnitudeNegativeG)) {
                            bestMin = java.lang.Math.min(bestMin, minAG / smallestMagnitudeNegativeG);
                            bestMin = java.lang.Math.min(bestMin, maxAG / smallestMagnitudeNegativeG);
                            bestMax = java.lang.Math.max(bestMax, minAG / smallestMagnitudeNegativeG);
                            bestMax = java.lang.Math.max(bestMax, maxAG / smallestMagnitudeNegativeG);
                        }

                        if (!Double.isNaN(smallestMagnitudePositiveG)) {
                            bestMin = java.lang.Math.min(bestMin, minAG / smallestMagnitudePositiveG);
                            bestMin = java.lang.Math.min(bestMin, maxAG / smallestMagnitudePositiveG);
                            bestMax = java.lang.Math.max(bestMax, minAG / smallestMagnitudePositiveG);
                            bestMax = java.lang.Math.max(bestMax, maxAG / smallestMagnitudePositiveG);
                        }

                        if (!Double.isNaN(nonZeroMinB)) {
                            bestMin = java.lang.Math.min(bestMin, minAB / nonZeroMinB);
                            bestMin = java.lang.Math.min(bestMin, maxAB / nonZeroMinB);
                            bestMax = java.lang.Math.max(bestMax, minAB / nonZeroMinB);
                            bestMax = java.lang.Math.max(bestMax, maxAB / nonZeroMinB);
                        }

                        if (!Double.isNaN(nonZeroMaxB)) {
                            bestMin = java.lang.Math.min(bestMin, minAB / nonZeroMaxB);
                            bestMin = java.lang.Math.min(bestMin, maxAB / nonZeroMaxB);
                            bestMax = java.lang.Math.max(bestMax, minAB / nonZeroMaxB);
                            bestMax = java.lang.Math.max(bestMax, maxAB / nonZeroMaxB);
                        }

                        if (!Double.isNaN(smallestMagnitudeNegativeB)) {
                            bestMin = java.lang.Math.min(bestMin, minAB / smallestMagnitudeNegativeB);
                            bestMin = java.lang.Math.min(bestMin, maxAB / smallestMagnitudeNegativeB);
                            bestMax = java.lang.Math.max(bestMax, minAB / smallestMagnitudeNegativeB);
                            bestMax = java.lang.Math.max(bestMax, maxAB / smallestMagnitudeNegativeB);
                        }

                        if (!Double.isNaN(smallestMagnitudePositiveB)) {
                            bestMin = java.lang.Math.min(bestMin, minAB / smallestMagnitudePositiveB);
                            bestMin = java.lang.Math.min(bestMin, maxAB / smallestMagnitudePositiveB);
                            bestMax = java.lang.Math.max(bestMax, minAB / smallestMagnitudePositiveB);
                            bestMax = java.lang.Math.max(bestMax, maxAB / smallestMagnitudePositiveB);
                        }
                    } else if (doComplex) {
                        bestMax = maxA / minB;
                        bestMin = -maxA / minB;
                    } else { // not color
                        nonZeroMin = srcImageB.getNonZeroMin();
                        nonZeroMax = srcImageB.getNonZeroMax();
                        smallestMagnitudeNegative = srcImageB.getSmallestMagnitudeNegative();
                        smallestMagnitudePositive = srcImageB.getSmallestMagnitudePositive();

                        // Remember that of A and B, none, 1, or both
                        // might be positive
                        bestMin = Double.POSITIVE_INFINITY;
                        bestMax = -Double.POSITIVE_INFINITY;

                        if (!Double.isNaN(nonZeroMin)) {
                            bestMin = java.lang.Math.min(bestMin, minA / nonZeroMin);
                            bestMin = java.lang.Math.min(bestMin, maxA / nonZeroMin);
                            bestMax = java.lang.Math.max(bestMax, minA / nonZeroMin);
                            bestMax = java.lang.Math.max(bestMax, maxA / nonZeroMin);
                        }

                        if (!Double.isNaN(nonZeroMax)) {
                            bestMin = java.lang.Math.min(bestMin, minA / nonZeroMax);
                            bestMin = java.lang.Math.min(bestMin, maxA / nonZeroMax);
                            bestMax = java.lang.Math.max(bestMax, minA / nonZeroMax);
                            bestMax = java.lang.Math.max(bestMax, maxA / nonZeroMax);
                        }

                        if (!Double.isNaN(smallestMagnitudeNegative)) {
                            bestMin = java.lang.Math.min(bestMin, minA / smallestMagnitudeNegative);
                            bestMin = java.lang.Math.min(bestMin, maxA / smallestMagnitudeNegative);
                            bestMax = java.lang.Math.max(bestMax, minA / smallestMagnitudeNegative);
                            bestMax = java.lang.Math.max(bestMax, maxA / smallestMagnitudeNegative);
                        }

                        if (!Double.isNaN(smallestMagnitudePositive)) {
                            bestMin = java.lang.Math.min(bestMin, minA / smallestMagnitudePositive);
                            bestMin = java.lang.Math.min(bestMin, maxA / smallestMagnitudePositive);
                            bestMax = java.lang.Math.max(bestMax, minA / smallestMagnitudePositive);
                            bestMax = java.lang.Math.max(bestMax, maxA / smallestMagnitudePositive);
                        }
                    } // else not color

                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;
                    
                case AVERAGE:
                    if (srcImageA.isColorImage()) {
                        bestMax = Math.max((maxAR + maxBR)/2.0, (maxAG + maxBG)/2.0);
                        bestMax = Math.max((maxAB + maxBB)/2.0, bestMax);
                        bestMin = Math.min((minAR + minBR)/2.0, (minAG + minBG)/2.0);
                        bestMin = Math.min((minAB + minBB)/2.0, bestMin);
                    } else if (doComplex) {
                        bestMax = (maxA + maxB)/2.0;
                        bestMin = (-maxA - maxB)/2.0;
                    } else {
                        bestMax = (maxA + maxB)/2.0;
                        bestMin = (minA + minB)/2.0;
                    }

                    if ((bestMax > clipMax) || (bestMin < clipMin)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;
                    
                case DIFFERENCE:
                    if (srcImageA.isColorImage()) {
                        bestMin = 0;
                        bestMax = Math.max(Math.abs(minAR - maxBR), Math.abs(minAG - maxBG));
                        bestMax = Math.max(Math.abs(minAB - maxBB), bestMax);
                        bestMax = Math.max(Math.abs(maxAR - minBR), bestMax);
                        bestMax = Math.max(Math.abs(maxAG - minBG), bestMax);
                        bestMax = Math.max(Math.abs(maxAB - minBB), bestMax);
                    } else if (doComplex) {
                        bestMin = 0;
                        bestMax = Math.abs(maxA + maxB);
                    } else {
                        bestMin = 0;
                        bestMax = Math.max(Math.abs(minA - maxB),Math.abs(maxA - minB));
                    }

                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }
                    
                    break;
                    
                case MAXIMUM:
                    if (srcImageA.isColorImage()) {
                        bestMin = Math.max(minAR, minBR);
                        bestMin = Math.min(bestMin,Math.max(minAG, minBG));
                        bestMin = Math.min(bestMin, Math.max(minAB, minBB));
                        bestMax = Math.max(maxAR, maxBR);
                        bestMax = Math.max(bestMax, Math.max(maxAG, maxBG));
                        bestMax = Math.max(bestMax, Math.max(maxAB, maxBB));
                    } else if (doComplex) {
                        bestMin = Math.max(-maxA, -maxB);
                        bestMax = Math.max(maxA, maxB);
                    } else {
                        bestMin = Math.max(minA, minB);
                        bestMax = Math.max(maxA, maxB);
                    }
                    
                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;
                    
                case MINIMUM:
                    if (srcImageA.isColorImage()) {
                        bestMin = Math.min(minAR, minBR);
                        bestMin = Math.min(bestMin,Math.min(minAG, minBG));
                        bestMin = Math.min(bestMin, Math.min(minAB, minBB));
                        bestMax = Math.min(maxAR, maxBR);
                        bestMax = Math.max(bestMax, Math.min(maxAG, maxBG));
                        bestMax = Math.max(bestMax, Math.min(maxAB, maxBB));
                    } else if (doComplex) {
                        bestMin = Math.min(-maxA, -maxB);
                        bestMax = Math.min(maxA, maxB);
                    } else {
                        bestMin = Math.min(minA, minB);
                        bestMax = Math.min(maxA, maxB);
                    }
                    
                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;
                    
                case OR:
                case XOR:
                    if (srcImageA.isColorImage()) {
                        bestMin = Math.min(minAR, minBR);
                        bestMin = Math.min(bestMin,Math.min(minAG, minBG));
                        bestMin = Math.min(bestMin, Math.min(minAB, minBB));
                        bestMax = Math.max(maxAR, maxBR);
                        bestMax = Math.max(bestMax, Math.max(maxAG, maxBG));
                        bestMax = Math.max(bestMax, Math.max(maxAB, maxBB));
                    } else if (doComplex) {
                        bestMin = Math.min(-maxA, -maxB);
                        bestMax = Math.max(maxA, maxB);
                    } else {
                        bestMin = Math.min(minA, minB);
                        bestMax = Math.max(maxA, maxB);
                    }
                    
                    if ((bestMin < clipMin) || (bestMax > clipMax)) {
                        newType = findType(srcImageA.getType());
                    }

                    break;


                case ADVANCED:
                    newType = ModelImage.DOUBLE;
                    break;

                default:
                    break;
            }

            if (newType > destImage.getType()) {
                destImage.reallocate(newType);
            }

        }


        int mod = length / 20;

        if (srcImageA.getNDims() == 5) {
            f = srcImageA.getExtents()[4];
        } else {
            f = 1;
        }

        if (srcImageA.getNDims() >= 4) {
            t = srcImageA.getExtents()[3];
        } else {
            t = 1;
        }

        if (srcImageA.getNDims() >= 3) {
            z = srcImageA.getExtents()[2];
        } else {
            z = 1;
        }

        // determine if we are dealing with an acceptable different extents case,
        // these will allow srcImageB to be applied on a per n-1 dimension basis to all "slices" of n dimension. 
        // Each "slice" has size diffExtentsFactor
        boolean diffExtents = false;
        int diffExtentsFactor = 1;

        if (srcImageA.getNDims()-1 == srcImageB.getNDims()) {
            diffExtents = true;
            for(int d=0; d<srcImageB.getNDims(); d++) {
                diffExtentsFactor *= srcImageB.getExtents()[d];
            }
        }

        int totalLength = f * t * z * length;
        //The current "slice" of srcImageA
        int pseudoSliceNum = 0;

        if(opType ==  ADVANCED) {
        	rpn = evaluateToRPN(adOpString);
        	if(rpn.equals("")) {
    			MipavUtil.displayError("Error in formula expression");
    			setCompleted(false);
                setThreadStopped(true);
    			return;
    		}
        }

        // System.err.println("Length is: " + length + " totalLength is: " + totalLength);
        // System.err.println("Dest image type is: " + destImage.getType());
        for (m = 0; (m < f) && !threadStopped; m++) {

            for (k = 0; (k < t) && !threadStopped; k++) {

                for (j = 0; (j < z) && !threadStopped; j++) {

                    try {
                        offset = (m * t * z * length) + (k * z * length) + (j * length);

                        if (doComplex) {
                            srcImageA.exportDComplexData(2 * offset, length, bufferA, bufferAI);
                        } else {
                            srcImageA.exportData(offset, length, bufferA); // locks and releases lock
                        }

                        if (!diffExtents) {

                            if (doComplex) {
                                srcImageB.exportDComplexData(2 * offset, length, bufferB, bufferBI);
                            } else {
                                srcImageB.exportData(offset, length, bufferB); // locks and releases lock
                            }
                        } else if(diffExtents) {
                            
                            if (doComplex) {
                                pseudoSliceNum = 2 * offset / diffExtentsFactor;
                                srcImageB.exportDComplexData(2 * offset - pseudoSliceNum*diffExtentsFactor, length, bufferB, bufferBI);
                            } else {
                                pseudoSliceNum = offset / diffExtentsFactor;
                                srcImageB.exportData(offset - pseudoSliceNum*diffExtentsFactor, length, bufferB); // locks and releases lock
                            }
                        }
                    } catch (IOException error) {
                        displayError("Algorithm Image Calculator : Image(s) locked");
                        setCompleted(false);
                        setThreadStopped(true);

                        return;
                    }

                    for (i = 0; (i < length) && !threadStopped; i++) {

                        try {

                            if (((i % mod) == 0)) {
                                fireProgressStateChanged(Math.round((float) (i + offset) / (totalLength - 1) * 100));
                            }
                        } catch (NullPointerException npe) {

                            if (threadStopped) {
                                Preferences.debug("somehow you managed to cancel the algorithm and dispose the progressbar between checking for threadStopping and using it.",
                                                  Preferences.DEBUG_ALGORITHM);
                            }
                        }

                        // Get slice
                        if ((entireImage == true) || mask.get((i + offset)/colorFactor)) {

                            // if (i < 300) { for testing     System.err.println(i + " Buffer A is: " + bufferA[i] + ",
                            // Buffer B is: "+ bufferB[i]); }
                            switch (opType) {

                                case ADD:
                                    bufferA[i] = bufferA[i] + bufferB[i];
                                    if (doComplex) {
                                        bufferAI[i] = bufferAI[i] + bufferBI[i];
                                    }

                                    break;

                                case SUBTRACT:
                                    bufferA[i] = bufferA[i] - bufferB[i];
                                    if (doComplex) {
                                        bufferAI[i] = bufferAI[i] - bufferBI[i];
                                    }

                                    break;

                                case MULTIPLY:
                                    if (doComplex) {
                                        temp = (bufferA[i] * bufferB[i]) - (bufferAI[i] * bufferBI[i]);
                                        bufferAI[i] = (bufferA[i] * bufferBI[i]) + (bufferAI[i] * bufferB[i]);
                                        bufferA[i] = temp;
                                    } else {
                                        bufferA[i] = bufferA[i] * bufferB[i];
                                    }

                                    break;

                                case DIVIDE:
                                    if (doComplex) {
                                        denom = (bufferB[i] * bufferB[i]) + (bufferBI[i] * bufferBI[i]);
                                        temp = ((bufferA[i] * bufferB[i]) + (bufferAI[i] * bufferBI[i])) / denom;
                                        bufferAI[i] = ((-bufferA[i] * bufferBI[i]) + (bufferAI[i] * bufferB[i])) /
                                                          denom;
                                        bufferA[i] = temp;
                                    } else {
                                        bufferA[i] = bufferA[i] / bufferB[i];
                                    }

                                    break;

                                case AND:
                                    if ((bufferA[i] == 0) || (bufferB[i] == 0)) {
                                        bufferA[i] = 0;
                                    }

                                    break;

                                case OR:
                                    if (bufferA[i] == 0) {
                                        bufferA[i] = bufferB[i];
                                    }

                                    break;

                                case XOR:
                                    if ((bufferA[i] != 0) && (bufferB[i] != 0)) {
                                        bufferA[i] = 0;
                                    } else if ((bufferA[i] == 0) && (bufferB[i] != 0)) {
                                        bufferA[i] = bufferB[i];
                                    }

                                    break;

                                case DIFFERENCE:
                                    if (doComplex) {
                                        bufferA[i] = Math.sqrt((bufferA[i] - bufferB[i])*(bufferA[i] - bufferB[i]) +
                                                               (bufferAI[i] - bufferBI[i])*(bufferAI[i] - bufferBI[i]));
                                        bufferAI[i] = 0;
                                    }
                                    else {
                                        bufferA[i] = Math.abs(bufferA[i] - bufferB[i]);
                                    }
                                    break;

                                case AVERAGE:
                                    bufferA[i] = (bufferA[i] + bufferB[i]) / 2.0;
                                    if (doComplex) {
                                        bufferAI[i] = (bufferAI[i] + bufferBI[i])/2.0;
                                    }
                                    break;

                                case MAXIMUM:
                                    bufferA[i] = Math.max(bufferA[i], bufferB[i]);
                                    break;

                                case MINIMUM:
                                    bufferA[i] = Math.min(bufferA[i], bufferB[i]);
                                    break;

                                case ADVANCED:
                                	double d = evaluateRPNExpression(bufferA[i], bufferB[i], rpn);
                                	if(d != Double.NaN) {
                                		bufferA[i] = d;
                                	}else {
                                		displayError("Algorithm ImageCalculator: Error evaluating expression");
                                		setCompleted(false);
                                        setThreadStopped(true);
                                        return;
                                	}
                                    if (OK == false) {
                                        displayError("Algorithm ImageCalculator: Illegal operator expression");
                                        setCompleted(false);

                                        setThreadStopped(true);

                                        return;
                                    }

                                    break;

                                default:
                                    break;
                            }
                        }
                        // if (i < 300 ) {
                        // System.err.println(i + " new buffer: " + bufferA[i]);
                        // }
                    }

                    // clip check
                    if (clipMode == CLIP) {

                        if (srcImageA.getType() == ModelStorageBase.COMPLEX) {

                            // Keep same phase in clipping
                            for (i = 0; i < length; i++) {

                                if ((bufferA[i] > Float.MAX_VALUE) || (bufferA[i] < -Float.MAX_VALUE) ||
                                        (bufferAI[i] > Float.MAX_VALUE) || (bufferAI[i] < -Float.MAX_VALUE)) {

                                    if (Math.abs(bufferA[i]) >= Math.abs(bufferAI[i])) {

                                        if (bufferA[i] > Float.MAX_VALUE) {
                                            bufferAI[i] = bufferAI[i] * (Float.MAX_VALUE / bufferA[i]);
                                            bufferA[i] = Float.MAX_VALUE;
                                        } else {
                                            bufferAI[i] = bufferAI[i] * (-Float.MAX_VALUE / bufferA[i]);
                                            bufferA[i] = -Float.MAX_VALUE;
                                        }
                                    } else { // Math.abs(bufferA[i]) < Math.abs(bufferAI[i])

                                        if (bufferAI[i] > Float.MAX_VALUE) {
                                            bufferA[i] = bufferA[i] * (Float.MAX_VALUE / bufferAI[i]);
                                            bufferAI[i] = Float.MAX_VALUE;
                                        } else {
                                            bufferA[i] = bufferA[i] * (-Float.MAX_VALUE / bufferAI[i]);
                                            bufferAI[i] = -Float.MAX_VALUE;
                                        }
                                    }
                                }
                            }
                        } // if (srcImage.getType() == ModelStorageBase.COMPLEX)
                        else if (!doComplex) {

                            for (i = 0; i < length; i++) {

                                if (bufferA[i] > clipMax) {
                                    bufferA[i] = clipMax;
                                } else if (bufferA[i] < clipMin) {
                                    bufferA[i] = clipMin;
                                }
                            }
                        } // else if (!doComplex)
                    } // if (clipMode == clip)


                    try {

                        if (threadStopped) { // do BEFORE buffer has been exported to Image
                            finalize();

                            return;
                        }

                        if (doComplex) {
                            destImage.importDComplexData(2 * offset, bufferA, bufferAI, false, true);
                        } else {
                            destImage.importData(offset, bufferA, false);
                        }
                    } catch (IOException error) {
                        displayError("Algorithm ImageCalculator: Image(s) locked");
                        setCompleted(false);

                        setThreadStopped(true);

                        return;
                    }

                }
            } // k loop
        } // f loop

        if (threadStopped) { // do BEFORE 'completed'
            finalize();

            return;
        }

        if (doComplex) {
            destImage.calcMinMaxMag(Preferences.is(Preferences.PREF_LOGMAG_DISPLAY));
            destImage.setOriginalExtents(srcImageA.getOriginalExtents());
            destImage.setOriginalMinimum(srcImageA.getOriginalMinimum());
            destImage.setOriginalMaximum(srcImageA.getOriginalMaximum());
            destImage.setOriginalDoCrop(srcImageA.getOriginalDoCrop());

            if (srcImageA.getOriginalDoCrop()) {
                destImage.setOriginalStart(srcImageA.getOriginalStart());
                destImage.setOriginalEnd(srcImageA.getOriginalEnd());
            }

            destImage.setUnequalDim(srcImageA.getUnequalDim());
            destImage.setImage25D(srcImageA.getImage25D());
            destImage.setOriginalKernelDimension(srcImageA.getOriginalKernelDimension());
            destImage.setOriginalFilterConstruction(srcImageA.getOriginalFilterConstruction());
            destImage.setHaveWindowed(srcImageA.getHaveWindowed());
        } else {
            destImage.calcMinMax();
        }

        setCompleted(true);
    }

    /**
     * Builds the advanced function dialog - looks like a calculator.
     *
     * @param  al  DOCUMENT ME!
     */
    private void createAdOpDialog(ActionListener al) {
        JPanel panel;
        TitledBorder border;
        Font serif12, serif12B;
        JLabel labelA, labelB, labelP;

        adOpDialog = new JDialog(ViewUserInterface.getReference().getActiveImageFrame(), "Enter advanced operator",
                                 false);
        adOpDialog.setSize(400, 450);
        adOpDialog.setLocation((Toolkit.getDefaultToolkit().getScreenSize().width / 2) -
                               (adOpDialog.getBounds().width / 2),
                               (Toolkit.getDefaultToolkit().getScreenSize().height / 2) -
                               (adOpDialog.getBounds().height / 2));
        adOpDialog.getContentPane().setLayout(new GridBagLayout());

        serif12 = MipavUtil.font12;
        serif12B = MipavUtil.font12B;

        panel = new JPanel();
        panel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        panel.setLayout(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 6;
        gbc.gridheight = 4;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 0;
        panel.setForeground(Color.black);
        border = new TitledBorder("Advanced operator");
        border.setTitleColor(Color.black);
        border.setBorder(new EtchedBorder());
        border.setTitleFont(serif12B);
        panel.setBorder(border);
        adOpDialog.getContentPane().add(panel, gbc);

        gbc.gridwidth = 6;
        gbc.gridheight = 1;
        textOperator = new JTextField();
        textOperator.setText(" ");
        textOperator.setFont(serif12);
        textOperator.addFocusListener(this);
        panel.add(textOperator, gbc);

        gbc.gridy = 1;
        labelA = new JLabel("A = " + srcImageA.getImageName());
        labelA.setForeground(Color.black);
        labelA.setFont(serif12);
        panel.add(labelA, gbc);

        gbc.gridy = 2;
        labelB = new JLabel("B = " + srcImageB.getImageName());
        labelB.setForeground(Color.black);
        labelB.setFont(serif12);
        panel.add(labelB, gbc);

        gbc.gridy = 3;
        labelP = new JLabel("pow(x,y) gives value of x raised to the y power");
        labelP.setForeground(Color.black);
        labelP.setFont(serif12);
        panel.add(labelP, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;

        JPanel clearPanel = new JPanel();
        clearPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 0, 3));
        clearPanel.setLayout(new GridBagLayout());
        adOpDialog.getContentPane().add(clearPanel, gbc);

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.fill = GridBagConstraints.NONE;
        backSpaceButton = new JButton("Backspace");
        backSpaceButton.setFont(serif12B);
        backSpaceButton.setMinimumSize(MipavUtil.defaultButtonSize);
        backSpaceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        backSpaceButton.addActionListener(al);
        clearPanel.add(backSpaceButton, gbc);

        gbc.gridx = 3;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        ceButton = new JButton("CE");
        ceButton.setFont(serif12B);
        ceButton.setMinimumSize(MipavUtil.defaultButtonSize);
        ceButton.setPreferredSize(MipavUtil.defaultButtonSize);
        ceButton.addActionListener(al);
        clearPanel.add(ceButton, gbc);

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;

        JPanel buttonPanel = new JPanel();
        buttonPanel.setBorder(BorderFactory.createEmptyBorder(0, 3, 3, 3));
        buttonPanel.setLayout(new GridBagLayout());
        absButton = new JButton("abs");
        absButton.setFont(serif12B);
        absButton.addActionListener(al);
        buttonPanel.add(absButton, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        logButton = new JButton("log");
        logButton.setFont(serif12B);
        logButton.addActionListener(al);
        buttonPanel.add(logButton, gbc);

        gbc.gridx = 2;
        gbc.gridy = 0;
        aButton = new JButton("A");
        aButton.setFont(serif12B);
        aButton.addActionListener(al);
        aButton.setToolTipText(srcImageA.getImageName());
        buttonPanel.add(aButton, gbc);

        gbc.gridx = 3;
        gbc.gridy = 0;
        bButton = new JButton("B");
        bButton.setFont(serif12B);
        bButton.setToolTipText(srcImageB.getImageName());
        bButton.addActionListener(al);
        buttonPanel.add(bButton, gbc);

        gbc.gridx = 4;
        gbc.gridy = 0;
        leftParButton = new JButton("(");
        leftParButton.setFont(serif12B);
        leftParButton.addActionListener(al);
        buttonPanel.add(leftParButton, gbc);

        gbc.gridx = 5;
        gbc.gridy = 0;
        rightParButton = new JButton(")");
        rightParButton.setFont(serif12B);
        rightParButton.addActionListener(al);
        buttonPanel.add(rightParButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        expButton = new JButton("exp");
        expButton.setFont(serif12B);
        expButton.addActionListener(al);
        buttonPanel.add(expButton, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        lnButton = new JButton("ln");
        lnButton.setFont(serif12B);
        lnButton.addActionListener(al);
        buttonPanel.add(lnButton, gbc);

        gbc.gridx = 2;
        gbc.gridy = 1;
        sevenButton = new JButton("7");
        sevenButton.setFont(serif12B);
        sevenButton.setForeground(Color.blue);
        sevenButton.addActionListener(al);
        buttonPanel.add(sevenButton, gbc);

        gbc.gridx = 3;
        gbc.gridy = 1;
        eightButton = new JButton("8");
        eightButton.setFont(serif12B);
        eightButton.setForeground(Color.blue);
        eightButton.addActionListener(al);
        buttonPanel.add(eightButton, gbc);

        gbc.gridx = 4;
        gbc.gridy = 1;
        nineButton = new JButton("9");
        nineButton.setFont(serif12B);
        nineButton.setForeground(Color.blue);
        nineButton.addActionListener(al);
        buttonPanel.add(nineButton, gbc);

        gbc.gridx = 5;
        gbc.gridy = 1;
        divButton = new JButton("/");
        divButton.setFont(serif12B);
        divButton.addActionListener(al);
        buttonPanel.add(divButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        powButton = new JButton("pow");
        powButton.setFont(serif12B);
        powButton.addActionListener(al);
        buttonPanel.add(powButton, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        commaButton = new JButton(",");
        commaButton.setFont(serif12B);
        commaButton.addActionListener(al);
        buttonPanel.add(commaButton, gbc);

        gbc.gridx = 2;
        gbc.gridy = 2;
        fourButton = new JButton("4");
        fourButton.setFont(serif12B);
        fourButton.setForeground(Color.blue);
        fourButton.addActionListener(al);
        buttonPanel.add(fourButton, gbc);

        gbc.gridx = 3;
        gbc.gridy = 2;
        fiveButton = new JButton("5");
        fiveButton.setFont(serif12B);
        fiveButton.setForeground(Color.blue);
        fiveButton.addActionListener(al);
        buttonPanel.add(fiveButton, gbc);

        gbc.gridx = 4;
        gbc.gridy = 2;
        sixButton = new JButton("6");
        sixButton.setFont(serif12B);
        sixButton.setForeground(Color.blue);
        sixButton.addActionListener(al);
        buttonPanel.add(sixButton, gbc);

        gbc.gridx = 5;
        gbc.gridy = 2;
        multButton = new JButton("*");
        multButton.setFont(serif12B);
        multButton.addActionListener(al);
        buttonPanel.add(multButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        sinButton = new JButton("sin");
        sinButton.setFont(serif12B);
        sinButton.addActionListener(al);
        buttonPanel.add(sinButton, gbc);

        gbc.gridx = 1;
        gbc.gridy = 3;
        cosButton = new JButton("cos");
        cosButton.setFont(serif12B);
        cosButton.addActionListener(al);
        buttonPanel.add(cosButton, gbc);

        gbc.gridx = 2;
        gbc.gridy = 3;
        oneButton = new JButton("1");
        oneButton.setFont(serif12B);
        oneButton.setForeground(Color.blue);
        oneButton.addActionListener(al);
        buttonPanel.add(oneButton, gbc);

        gbc.gridx = 3;
        gbc.gridy = 3;
        twoButton = new JButton("2");
        twoButton.setFont(serif12B);
        twoButton.setForeground(Color.blue);
        twoButton.addActionListener(al);
        buttonPanel.add(twoButton, gbc);

        gbc.gridx = 4;
        gbc.gridy = 3;
        threeButton = new JButton("3");
        threeButton.setFont(serif12B);
        threeButton.setForeground(Color.blue);
        threeButton.addActionListener(al);
        buttonPanel.add(threeButton, gbc);

        gbc.gridx = 5;
        gbc.gridy = 3;
        subButton = new JButton("-");
        subButton.setFont(serif12B);
        subButton.addActionListener(al);
        buttonPanel.add(subButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        tanButton = new JButton("tan");
        tanButton.setFont(serif12B);
        tanButton.addActionListener(al);
        buttonPanel.add(tanButton, gbc);

        gbc.gridx = 1;
        gbc.gridy = 4;
        piButton = new JButton("pi");
        piButton.setFont(serif12B);
        piButton.setForeground(Color.blue);
        piButton.addActionListener(al);
        buttonPanel.add(piButton, gbc);

        gbc.gridx = 2;
        gbc.gridy = 4;
        zeroButton = new JButton("0");
        zeroButton.setFont(serif12B);
        zeroButton.setForeground(Color.blue);
        zeroButton.addActionListener(al);
        buttonPanel.add(zeroButton, gbc);

        gbc.gridx = 3;
        gbc.gridy = 4;
        decimalButton = new JButton(".");
        decimalButton.setFont(serif12B);
        decimalButton.setForeground(Color.blue);
        decimalButton.addActionListener(al);
        buttonPanel.add(decimalButton, gbc);

        gbc.gridx = 4;
        gbc.gridy = 4;
        modButton = new JButton("mod");
        modButton.setFont(serif12B);
        modButton.addActionListener(al);
        buttonPanel.add(modButton, gbc);

        gbc.gridx = 5;
        gbc.gridy = 4;
        addButton = new JButton("+");
        addButton.setFont(serif12B);
        addButton.addActionListener(al);
        buttonPanel.add(addButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.gridwidth = 6;
        gbc.gridheight = 5;
        adOpDialog.getContentPane().add(buttonPanel, gbc);

        JPanel OKPanel = new JPanel();
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.gridwidth = 6;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        adOpDialog.getContentPane().add(OKPanel, gbc);

        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 2;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        OKPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
        OKPanel.setLayout(new GridBagLayout());
        OKButton = new JButton("OK");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);
        OKButton.addActionListener(al);
        OKPanel.add(OKButton, gbc);
        gbc.gridx = 3;
        gbc.anchor = GridBagConstraints.WEST;
        cancelButton = new JButton("Cancel");
        cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
        cancelButton.setFont(serif12B);
        cancelButton.addActionListener(al);
        OKPanel.add(cancelButton, gbc);

        gbc.gridx = 4;
        JButton helpButton = new JButton("Help");
        helpButton.setMinimumSize(MipavUtil.defaultButtonSize);
        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
        helpButton.setFont(serif12B);
        helpButton.addActionListener(al);
        OKPanel.add(helpButton, gbc);
        
        adOpDialog.setResizable(true);
        adOpDialog.setVisible(true);

    }

    /**
     * Method that evaluates the RPN expression and returns the value
     */
    public double evaluateRPNExpression(double a, double b, String rpn) {
    	this.aVal = a;
        this.bVal = b;
        OK = true;
		double finalAnswer = Double.NaN;
		try {
			Double result;
			String[] rpnTokens = rpn.split("\\|");
			Stack<Double> rpnStack = new Stack<Double>();
			for(int i=0;i<rpnTokens.length;i++) {
				String token = rpnTokens[i];
				if(isOperator(token)) {
					//pop 2 items from stack
					Double operand2 = rpnStack.pop();
					Double operand1 = rpnStack.pop();

					result = new Double(0);
					double d = 0;
					if(token.equals("+")) {
						d = operand1.doubleValue() + operand2.doubleValue();
					}else if(token.equals("-")) {
						d = operand1.doubleValue() - operand2.doubleValue();
					}else if(token.equals("*")) {
						d = operand1.doubleValue() * operand2.doubleValue();
					}else if(token.equals("/")) {
						d = operand1.doubleValue() / operand2.doubleValue();
					}else if(token.equals("mod")) {
						d = operand1.doubleValue()%operand2.doubleValue();
					}
					result = new Double(d);
					
					//push answer on stack
					rpnStack.push(result);
				}else if(isFunction(token)) {
					if(token.equals("pow")) {
						//pop 2 items from stack
						Double operand2 = rpnStack.pop();
						Double operand1 = rpnStack.pop();
						
						result = new Double(0);
						double d = 0;
						d = Math.pow(operand1.doubleValue(), operand2.doubleValue());
						result = new Double(d);
						
						//push answer on stack
						rpnStack.push(result);
            } else {
						//pop 1 item from stack
						Double operand1 = rpnStack.pop();
						
						result = new Double(0);
						double d = 0;
						if(token.equals("abs")) {
							d = Math.abs(operand1);
						}else if(token.equals("log")) {
							d = Math.log10(operand1);
						}else if(token.equals("exp")) {
							d = Math.exp(operand1);
						}else if(token.equals("ln")) {
							d = Math.log(operand1);
						}else if(token.equals("sin")) {
							d = Math.sin(operand1);
						}else if(token.equals("cos")) {
							d = Math.cos(operand1);
						}else if(token.equals("tan")) {
							d = Math.tan(operand1);
            }
						result = new Double(d);
						
						//push answer on stack
						rpnStack.push(result);
        }
				}else {
					if(token.equals("A")) {
						Double doubleA = new Double(this.aVal);
						rpnStack.push(doubleA);
					}else if(token.equals("B")) {
						Double doubleB = new Double(this.bVal);
						rpnStack.push(doubleB);
					}else if(token.equals("pi")) {
						Double doublePi = new Double(Math.PI);
						rpnStack.push(doublePi);
					}else {
						Double operandValue = new Double(token);
						rpnStack.push(operandValue);
					}
    }
        }
			finalAnswer = rpnStack.pop().doubleValue();
		}catch(Exception e) {
			e.printStackTrace();
			return Double.NaN;
        }
		return finalAnswer;
        }

    
    
    /**
     * method that determines the RPN
     * @param expression
     * @return
     */
    public  String evaluateToRPN(String expression) {
		String rpnString = "";
		Vector<String> tokens = new Vector<String>();
		boolean success;
		success = initialValidate(expression);
		if(!success) {
			return "";
        }
		success = tokenize(expression,tokens);
		if(!success) {
			return "";
		}

		//check that there are tokens where there are operators back to back or digits back to back or right and left paren
		//like A + + B
		//like 7 + 8 4
		//like (A+B)(C-D)
		for(int i=0;i<tokens.size();i++) {
			String t = tokens.get(i);
			if(i+1 < tokens.size()) {
				String t2 = tokens.get(i+1);
				if(isOperator(t) && isOperator(t2)) {
					return "";
            }
				if((isValidNumber(t)||t.equals("A")||t.equals("B")) && (isValidNumber(t2)||t2.equals("A")||t2.equals("B"))) {
					return "";
				}
				if(t.equals(")") && t2.equals("(")) {
					return "";
				}
			}

        }


		rpnString = convertToRPN(tokens);
		return rpnString;

            }

    /**
     * method that does some initial validation of the expression
     * @param expression
     * @return
     */
    public boolean initialValidate(String expression){
		boolean success = true;
		// validate num of parentheses
		int numOccurencesOfLeftParen = 0;
		int numOccurencesOfRightParen = 0;
		for(int i=0;i<expression.length();i++) {
			if(expression.charAt(i) == '(') {
				numOccurencesOfLeftParen++;
			}else if(expression.charAt(i) == ')') {
				numOccurencesOfRightParen++;
            }
		}
		if(numOccurencesOfLeftParen != numOccurencesOfRightParen) {
			return false;
		}
		//validate that first occurrence of right paren does not come before left paren
		if(numOccurencesOfLeftParen > 0) {
			if(expression.indexOf(")") < expression.indexOf("(")) {
				return false;
			}
		}
		if(expression.contains("abs")) {
			int index = 0;
			do{
				index = expression.indexOf("abs",index);
				if(index != -1) {
					index = index + 3;
					if(index < expression.length()) {
						char c = expression.charAt(index);
						if(c != '(') {
							MipavUtil.displayError("abs function must be followed by a (");
							return false;
                }
            } else {
						MipavUtil.displayError("abs function must be followed by a (");
						return false;
            }
				}else {
					break;
				}
			}while(index < expression.length());
		}
		if(expression.contains("log")) {
			int index = 0;
			do{
				index = expression.indexOf("log",index);
				if(index != -1) {
					index = index + 3;
					if(index < expression.length()) {
						char c = expression.charAt(index);
						if(c != '(') {
							MipavUtil.displayError("log function must be followed by a (");
							return false;
            }
					}else {
						MipavUtil.displayError("log function must be followed by a (");
						return false;
					}
				}else {
					break;
				}
			}while(index < expression.length());
		}
		if(expression.contains("exp")) {
			int index = 0;
			do{
				index = expression.indexOf("exp",index);
				if(index != -1) {
					index = index + 3;
					if(index < expression.length()) {
						char c = expression.charAt(index);
						if(c != '(') {
							MipavUtil.displayError("exp function must be followed by a (");
							return false;
        }
					}else {
						MipavUtil.displayError("exp function must be followed by a (");
						return false;
					}
				}else {
					break;
				}
			}while(index < expression.length());
		}
		if(expression.contains("ln")) {
			int index = 0;
			do{
				index = expression.indexOf("ln",index);
				if(index != -1) {
					index = index + 2;
					if(index < expression.length()) {
						char c = expression.charAt(index);
						if(c != '(') {
							MipavUtil.displayError("ln function must be followed by a (");
							return false;
        }
					}else {
						MipavUtil.displayError("ln function must be followed by a (");
						return false;
					}
				}else {
					break;
				}
			}while(index < expression.length());
		}
		if(expression.contains("pow")) {
			int index = 0;
			do{
				index = expression.indexOf("pow",index);
				if(index != -1) {
					index = index + 3;
					if(index < expression.length()) {
						char c = expression.charAt(index);
						if(c != '(') {
							MipavUtil.displayError("pow function must be followed by a (");
							return false;
    }
					}else {
						MipavUtil.displayError("pow function must be followed by a (");
						return false;
					}
				}else {
					break;
				}
			}while(index < expression.length());
		}
		if(expression.contains("sin")) {
			int index = 0;
			do{
				index = expression.indexOf("sin",index);
				if(index != -1) {
					index = index + 3;
					if(index < expression.length()) {
						char c = expression.charAt(index);
						if(c != '(') {
							MipavUtil.displayError("sin function must be followed by a (");
							return false;
        }
					}else {
						MipavUtil.displayError("sin function must be followed by a (");
						return false;
					}
				}else {
					break;
				}
			}while(index < expression.length());
		}
		if(expression.contains("cos")) {
			int index = 0;
			do{
				index = expression.indexOf("cos",index);
				if(index != -1) {
					index = index + 3;
					if(index < expression.length()) {
						char c = expression.charAt(index);
						if(c != '(') {
							MipavUtil.displayError("cos function must be followed by a (");
							return false;
                        }
                    } else {
						MipavUtil.displayError("cos function must be followed by a (");
						return false;
                    }
				}else {
					break;
                }
			}while(index < expression.length());
		}
		if(expression.contains("tan")) {
			int index = 0;
			do{
				index = expression.indexOf("tan",index);
				if(index != -1) {
					index = index + 3;
					if(index < expression.length()) {
						char c = expression.charAt(index);
						if(c != '(') {
							MipavUtil.displayError("tan function must be followed by a (");
							return false;
						}
					}else {
						MipavUtil.displayError("tan function must be followed by a (");
						return false;
					}
				}else {
                break;
        }
			}while(index < expression.length());
		}
		return success;
    }

    
    
    
    /**
     * method that tokenizes the expression
     * @param expression
     * @param tokens
     * @return
     */
    public boolean tokenize(String expression, Vector<String> tokens){
		int pos = 0;
		boolean success = true;
		try{
			char c;
			String s;
			StringBuffer sb;
			while(pos < expression.length()) {
				c = readChar(expression,pos);
				if(c == ' ') {
					pos++;
				} else if(c == '(') {
					s = String.valueOf(c);
					tokens.add(s);
					pos++;
				}else if(c == ')') {
					s = String.valueOf(c);
					tokens.add(s);
					pos++;
				}else if(c == '/') {
					s = String.valueOf(c);
					tokens.add(s);
					pos++;
				}
				else if(c == '*') {
					s = String.valueOf(c);
					tokens.add(s);
					pos++;
				}
				else if(c == '-') {
					s = String.valueOf(c);
					tokens.add(s);
					pos++;
				}
				else if(c == '+') {
					s = String.valueOf(c);
					tokens.add(s);
					pos++;
				}else if(c == '.') {
					sb = new StringBuffer();
					s = String.valueOf(c);
					sb.append(s);
					pos++;
					if(pos < expression.length()) {
						char c2;
						c2 = readChar(expression,pos);
						if(isDigit(c2)) {
							while(isDigit(c2)) {
								s = String.valueOf(c2);
								sb.append(s);
								pos++;
								if(pos < expression.length()) {
									c2 = readChar(expression,pos);
								}else {
									break;
								}
							}
						}else {
							return false;
						}
						tokens.add(sb.toString());
					}else {
						return false;
					}
				}else if(c == ',') {
					s = String.valueOf(c);
					tokens.add(s);
					pos++;
				}else if(c == 'A') {
					s = String.valueOf(c);
					tokens.add(s);
					pos++;
					//check to make sure next char is not a letter
					char c2;
					if(pos < expression.length()) {
						c2 = readChar(expression,pos);
						if(Character.isLetter(c2)) {
							return false;
						}
					}
				}else if(c == 'B') {
					s = String.valueOf(c);
					tokens.add(s);
					pos++;
					//check to make sure next char is not a letter
					char c2;
					if(pos < expression.length()) {
						c2 = readChar(expression,pos);
						if(Character.isLetter(c2)) {
							return false;
						}
					}
				}else if(isDigit(c)) {
					sb = new StringBuffer();
					while(isDigit(c) || c == '.') {
						s = String.valueOf(c);
						sb.append(s);
						pos++;
						if(pos < expression.length()) {
							c = readChar(expression,pos);
						}else {
							break;
						}
					}
					tokens.add(sb.toString());
				}else if(c == 'a') {
					//check if word is "abs"
					char c2,c3;
					sb = new StringBuffer();
					s = String.valueOf(c);
					sb.append(s);
					pos++;
					if(pos+1 < expression.length()) {
						c2 = readChar(expression,pos);
						pos++;
						c3 = readChar(expression,pos);
						pos++;
						if(c2 == 'b' && c3 == 's') {
							s = String.valueOf(c2);
							sb.append(s);
							s = String.valueOf(c3);
							sb.append(s);
							tokens.add(sb.toString());
						}else {
							return false;
						}
					}else {
						return false;
					}
				}else if(c == 'e') {
					//check if word is "exp"
					char c2,c3;
					sb = new StringBuffer();
					s = String.valueOf(c);
					sb.append(s);
					pos++;
					if(pos+1 < expression.length()) {
						c2 = readChar(expression,pos);
						pos++;
						c3 = readChar(expression,pos);
						pos++;
						if(c2 == 'x' && c3 == 'p') {
							s = String.valueOf(c2);
							sb.append(s);
							s = String.valueOf(c3);
							sb.append(s);
							tokens.add(sb.toString());
						}else {
							return false;
						}
					}else {
						return false;
					}
				}else if(c == 's') {
					//check if word is "sin"
					char c2,c3;
					sb = new StringBuffer();
					s = String.valueOf(c);
					sb.append(s);
					pos++;
					if(pos+1 < expression.length()) {
						c2 = readChar(expression,pos);
						pos++;
						c3 = readChar(expression,pos);
						pos++;
						if(c2 == 'i' && c3 == 'n') {
							s = String.valueOf(c2);
							sb.append(s);
							s = String.valueOf(c3);
							sb.append(s);
							tokens.add(sb.toString());
						}else {
							return false;
						}
					}else {
						return false;
					}
				}else if(c == 'c') {
					//check if word is "cos"
					char c2,c3;
					sb = new StringBuffer();
					s = String.valueOf(c);
					sb.append(s);
					pos++;
					if(pos+1 < expression.length()) {
						c2 = readChar(expression,pos);
						pos++;
						c3 = readChar(expression,pos);
						pos++;
						if(c2 == 'o' && c3 == 's') {
							s = String.valueOf(c2);
							sb.append(s);
							s = String.valueOf(c3);
							sb.append(s);
							tokens.add(sb.toString());
						}else {
							return false;
						}
					}else {
						return false;
					}
				}else if(c == 't') {
					//check if word is "tan"
					char c2,c3;
					sb = new StringBuffer();
					s = String.valueOf(c);
					sb.append(s);
					pos++;
					if(pos+1 < expression.length()) {
						c2 = readChar(expression,pos);
						pos++;
						c3 = readChar(expression,pos);
						pos++;
						if(c2 == 'a' && c3 == 'n') {
							s = String.valueOf(c2);
							sb.append(s);
							s = String.valueOf(c3);
							sb.append(s);
							tokens.add(sb.toString());
						}else {
							return false;
						}
					}else {
						return false;
					}
				}else if(c == 'm') {
					//check if word is "mod"
					char c2,c3;
					sb = new StringBuffer();
					s = String.valueOf(c);
					sb.append(s);
					pos++;
					if(pos+1 < expression.length()) {
						c2 = readChar(expression,pos);
						pos++;
						c3 = readChar(expression,pos);
						pos++;
						if(c2 == 'o' && c3 == 'd') {
							s = String.valueOf(c2);
							sb.append(s);
							s = String.valueOf(c3);
							sb.append(s);
							tokens.add(sb.toString());
						}else {
							return false;
						}
					}else {
						return false;
					}
				}else if(c == 'p') {
					//check if word is pow or pi
					char c2,c3;
					sb = new StringBuffer();
					s = String.valueOf(c);
					sb.append(s);
					pos++;
					if(pos < expression.length()) {
						c2 = readChar(expression,pos);
						if(c2 == 'i') {
							s = String.valueOf(c2);
							sb.append(s);
							pos++;
							tokens.add(sb.toString());
						}else if(c2 == 'o') {
							s = String.valueOf(c2);
							sb.append(s);
							pos++;
							if(pos < expression.length()) {
								c3 = readChar(expression,pos);
								if(c3 == 'w') {
									s = String.valueOf(c3);
									sb.append(s);
									pos++;
									tokens.add(sb.toString());
								}else {
									return false;
								}
							}else {
								return false;
							}
						}else {
							return false;
						}
					}else {
						return false;
					}		
				}else if(c == 'l') {
					//check if word is log or ln
					char c2,c3;
					sb = new StringBuffer();
					s = String.valueOf(c);
					sb.append(s);
					pos++;
					if(pos < expression.length()) {
						c2 = readChar(expression,pos);
						if(c2 == 'n') {
							s = String.valueOf(c2);
							sb.append(s);
							pos++;
							tokens.add(sb.toString());
						}else if(c2 == 'o') {
							s = String.valueOf(c2);
							sb.append(s);
							pos++;
							if(pos < expression.length()) {
								c3 = readChar(expression,pos);
								if(c3 == 'g') {
									s = String.valueOf(c3);
									sb.append(s);
									pos++;
									tokens.add(sb.toString());
								}else {
									return false;
								}
							}else {
								return false;
							}
						}else {
							return false;
						}
					}else {
						return false;
					}		
				}else {
					return false;
				}
        }
		}catch(Exception e) {
			e.printStackTrace();
			success = false;
			return success;
    }
		return success;
	}
    
    
    /**
     * method that converts the tokens to RPN
     * @param tokens
     * @return
     */
    public String convertToRPN(Vector<String> tokens) {
		String rpnString = "";
		Stack<String> operatorStack = new Stack<String>();
		try{
			for(int i=0;i<tokens.size();i++) {
				String token = tokens.get(i);
				if(token.equals("(")) {
					operatorStack.push(token);
				}else if (token.equals(")")) {
					while(!operatorStack.peek().equals("(")) {
						String pop = operatorStack.pop();
						rpnString = rpnString + "|" + pop;
					}
					if(!operatorStack.isEmpty() && operatorStack.peek().equals("(")) {
						operatorStack.pop();
					}
					if(!operatorStack.isEmpty() && isFunction(operatorStack.peek())) {
						String pop = operatorStack.pop();
						rpnString = rpnString + "|" + pop;
					}

				}else if(isOperator(token)) {
					String peek1 = "";
					if(!operatorStack.isEmpty()) {
						peek1 = operatorStack.peek();
					}
					if(operatorStack.isEmpty() || peek1.equals("(") || (precedence(peek1) < precedence(token))) {
						operatorStack.push(token);
					}else {
						String peek2 = "";
        do {
							String pop = operatorStack.pop();
							rpnString = rpnString + "|" + pop;
							if(!operatorStack.isEmpty()) {
								peek2 = operatorStack.peek();
							}
						}while(!operatorStack.isEmpty() && !peek2.equals("(") || (precedence(token) < precedence(peek2)));
						operatorStack.push(token);
            }
				}else if(isFunction(token)) {
					operatorStack.push(token);
				}else if(token.equals(",")) {
					if(!operatorStack.isEmpty() && isOperator(operatorStack.peek())) {
						String pop = operatorStack.pop();
						rpnString = rpnString + "|" + pop;
					}
				}else {
					rpnString = rpnString + "|" + token;
				}
			}
			while(!operatorStack.isEmpty()) {
				String pop = operatorStack.pop();
				rpnString = rpnString + "|" + pop;
			}
			rpnString = rpnString.trim();

			//take away first pipe
			rpnString = rpnString.substring(1, rpnString.length());

			return rpnString;
			
		}catch(Exception e) {
			e.printStackTrace();
			rpnString = "";
			return rpnString;
                }

		
            }

    /**
     * method that determines precedence
     * @param op
     * @return
     */
    public int precedence(String op) {
		int precedence = 0;
		if(op.equals("mod")) {
			precedence = 2;
		}else if (op.equals("*") || op.equals("/")){
			precedence = 1;
		}else {
			precedence = 0;
		}
		return precedence;
	}


    /**
     * method determining if token is an operator
     * @param token
     * @return
     */
    public boolean isOperator(String token) {
		boolean isOperator = false;
		if(token.equals("+") || token.equals("-") || token.equals("*") || token.equals("/") || token.equals("mod")) {
			isOperator = true;
            }

		return isOperator;
        }


	/**
	 * method determining if token is function
	 * @param token
	 * @return
	 */
	public boolean isFunction(String token) {
		boolean isSingleFunction = false;
		if(token.equals("abs") || token.equals("log") || token.equals("exp") || token.equals("ln") || token.equals("sin") || token.equals("cos") || token.equals("tan") || token.equals("pow") ) {
			isSingleFunction = true;
        }
		return isSingleFunction;
	}

	/**
	 * reads char
	 * @param exp
	 * @param pos
	 * @return
	 */
	public char readChar(String exp,int pos) {
		char c;
		c = exp.charAt(pos);
		return c;
	}


	/**
	 * determines if char is a digit
	 * @param c
	 * @return
	 */
	public boolean isDigit(char c) {
		boolean isDigit = false;
		if(c == '0' || c == '1' || c == '2' || c == '3' || c == '4' || c == '5' || c == '6' || c == '7' || c == '8' || c == '9') {
			isDigit = true;
                }
		return isDigit;
	}

    
    
	public boolean isValidNumber(String s) {
    
		try{
			Double.parseDouble(s);
		}catch(Exception e) {
			return false;
		}
		
		return true;
	}

	
	
	
    
    
    
    //////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    

    /**
     * Find the type able to contain the full range of the data.
     *
     * @param   stType  starting type of image. Image will be promoted above this type if needed.
     *
     * @return  type capable of storing full range of data. See ModelStorageBase for image types.
     */
    private int findType(int stType) {
        boolean loop = true;
        int endType;

        endType = stType;

        if ((endType == ModelStorageBase.DOUBLE) || (endType == ModelStorageBase.ARGB_FLOAT) ||
                (endType == ModelStorageBase.DCOMPLEX)) {
            return endType;
        }

        switch (opType) {

            case ADD:
            case SUBTRACT:
            case MULTIPLY:
            case DIVIDE:
            case AVERAGE:
            case DIFFERENCE:
            case MAXIMUM:
            case MINIMUM:
            case OR:
            case XOR:
                while (loop == true) {

                    if (testType(endType, bestMin, bestMax) == false) {
                        endType = promoteType(endType);

                        if ((endType == ModelStorageBase.DOUBLE) || (endType == ModelStorageBase.ARGB_FLOAT) ||
                                (endType == ModelStorageBase.DCOMPLEX)) {
                            loop = false;
                        }
            } else {
                        loop = false;
                    }
                }

                break;

            default:
                break;
            }

        return endType;
        }



    /**
     * DOCUMENT ME!
     *
     * @param   presentType  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private int promoteType(int presentType) {

        switch (presentType) {

            case ModelStorageBase.BOOLEAN:
                return ModelStorageBase.BYTE;

            case ModelStorageBase.BYTE:
                return ModelStorageBase.UBYTE;

            case ModelStorageBase.UBYTE:
                return ModelStorageBase.SHORT;

            case ModelStorageBase.SHORT:
                return ModelStorageBase.USHORT;

            case ModelStorageBase.USHORT:
                return ModelStorageBase.INTEGER;

            case ModelStorageBase.INTEGER:
                return ModelStorageBase.UINTEGER;

            case ModelStorageBase.UINTEGER:
                return ModelStorageBase.LONG;

            case ModelStorageBase.LONG:
                return ModelStorageBase.FLOAT;

            case ModelStorageBase.FLOAT:
                return ModelStorageBase.DOUBLE;

            case ModelStorageBase.DOUBLE:
                return ModelStorageBase.DOUBLE;

            case ModelStorageBase.ARGB:
                return ModelStorageBase.ARGB_USHORT;

            case ModelStorageBase.ARGB_USHORT:
                return ModelStorageBase.ARGB_FLOAT;

            case ModelStorageBase.ARGB_FLOAT:
                return ModelStorageBase.ARGB_FLOAT;

            case ModelStorageBase.COMPLEX:
                return ModelStorageBase.DCOMPLEX;

            case ModelStorageBase.DCOMPLEX:
                return ModelStorageBase.DCOMPLEX;

            default:
                return ModelStorageBase.DOUBLE;
        }
    }

    /*******************************************************************************************/
    //
    // Next methods are used to parse the equation input from the advanced calculator.
    //
    /*******************************************************************************************/

    /**
     * Sets the minimum and maximum clipping values.
     */
    private void setClipValues() {

        if (srcImageA.getType() == ModelStorageBase.BOOLEAN) {
            clipMin = 0;
            clipMax = 1;
        } else if (srcImageA.getType() == ModelStorageBase.BYTE) {
            clipMin = -128;
            clipMax = 127;
        } else if (srcImageA.getType() == ModelStorageBase.UBYTE) {
            clipMin = 0;
            clipMax = 255;
        } else if (srcImageA.getType() == ModelStorageBase.SHORT) {
            clipMin = -32768;
            clipMax = 32767;
        } else if (srcImageA.getType() == ModelStorageBase.USHORT) {
            clipMin = 0;
            clipMax = 65535;
        } else if (srcImageA.getType() == ModelStorageBase.INTEGER) {
            clipMin = Integer.MIN_VALUE;
            clipMax = Integer.MAX_VALUE;
        } else if (srcImageA.getType() == ModelStorageBase.UINTEGER) {
            clipMin = 0;
            clipMax = 4294967295L;
        } else if (srcImageA.getType() == ModelStorageBase.LONG) {
            clipMin = Long.MIN_VALUE;
            clipMax = Long.MAX_VALUE;
        } else if (srcImageA.getType() == ModelStorageBase.FLOAT) {
            clipMin = -Float.MAX_VALUE;
            clipMax = Float.MAX_VALUE;
        } else if (srcImageA.getType() == ModelStorageBase.DOUBLE) {
            clipMin = -Double.MAX_VALUE;
            clipMax = Double.MAX_VALUE;
        } else if (srcImageA.getType() == ModelStorageBase.ARGB) {
            clipMin = 0;
            clipMax = 255;
        } else if (srcImageA.getType() == ModelStorageBase.ARGB_USHORT) {
            clipMin = 0;
            clipMax = 65535;
        } else if (srcImageA.getType() == ModelStorageBase.ARGB_FLOAT) {
            clipMin = -Float.MAX_VALUE;
            clipMax = Float.MAX_VALUE;
        } else if (srcImageA.getType() == ModelStorageBase.COMPLEX) {
            clipMin = -Float.MAX_VALUE;
            clipMax = Float.MAX_VALUE;
        }
    }


    /**
     * Determine if the min and max values are in the image types range.
     *
     * @param   type    image type
     * @param   minVal  min value of the image
     * @param   maxVal  max value of the image
     *
     * @return  true if min and max are within the image type specified
     */
    private boolean testType(int type, double minVal, double maxVal) {

        if (type == ModelStorageBase.BOOLEAN) {

            if ((minVal < 0) || (maxVal > 1)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.BYTE) {

            if ((minVal < -128) || (maxVal > 127)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UBYTE) {

            if ((minVal < 0) || (maxVal > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.SHORT) {

            if ((minVal < -32768) || (maxVal > 32767)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.USHORT) {

            if ((minVal < 0) || (maxVal > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.INTEGER) {

            if ((minVal < Integer.MIN_VALUE) || (maxVal > Integer.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UINTEGER) {

            if ((minVal < 0) || (maxVal > 4294967295L)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.LONG) {

            if ((minVal < Long.MIN_VALUE) || (maxVal > Long.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.FLOAT) {

            if ((minVal < -Float.MAX_VALUE) || (maxVal > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.DOUBLE) {

            if ((minVal < -Double.MAX_VALUE) || (maxVal > Double.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB) {

            if ((minVal < 0) || (maxVal > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_USHORT) {

            if ((minVal < 0) || (maxVal > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_FLOAT) {

            if ((minVal < -Float.MAX_VALUE) || (maxVal > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.COMPLEX) {

            if ((minVal < -Float.MAX_VALUE) || (maxVal > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.DCOMPLEX) {

            if ((minVal < -Double.MAX_VALUE) || (maxVal > Double.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

}
